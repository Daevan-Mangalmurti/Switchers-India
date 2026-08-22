# SHRUG SECC 2012 and SECC-consumption context at 2008 AC geography.

build_secc_context <- function(paths, dirs, geography) {
  message("Building SHRUG SECC context")

  source_paths <- c(
    paths$secc_rural_ac08,
    paths$secc_urban_ac08,
    paths$secc_cons_rural_ac08,
    paths$secc_cons_urban_ac08
  )
  purrr::walk(source_paths, assert_file_exists)

  read_shrug_ac08 <- function(path, variables, label) {
    data <- haven::read_dta(path) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          plain_col
        )
      )

    assert_has_columns(
      data,
      c("ac08_id", variables),
      label
    )

    data <- data |>
      dplyr::transmute(
        ac08_id = stringr::str_squish(
          as.character(ac08_id)
        ),
        dplyr::across(
          dplyr::all_of(variables),
          ~ suppressWarnings(as.numeric(.x))
        )
      )

    assert_unique_rows(data, "ac08_id", label)
    data
  }

  # Use the project's SHRUG AC-name key rather than reconstructing or
  # spatially reassigning constituency IDs.
  ac_key <- readr::read_csv(
    paths$ac_name_key,
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      ac08_id = stringr::str_squish(
        as.character(ac08_id)
      ),
      state_no = as.integer(
        stringr::str_match(
          ac08_id,
          "^2008-(\\d{2})-\\d{3}$"
        )[, 2]
      ),
      ac = as.integer(
        stringr::str_extract(
          ac08_id,
          "\\d{3}$"
        )
      ),
      ac_uid = make_ac_uid(state_no, ac),
      shrug_ac_name = as.character(ac08_name)
    ) |>
    dplyr::semi_join(
      geography$ac_reference |>
        dplyr::select(ac_uid),
      by = "ac_uid"
    ) |>
    dplyr::distinct(ac_uid, .keep_all = TRUE)

  assert_unique_rows(ac_key, "ac08_id", "SHRUG AC08 key")
  assert_unique_rows(ac_key, "ac_uid", "SHRUG AC08 key")

  secc_rural <- read_shrug_ac08(
    paths$secc_rural_ac08,
    c("ed_sec_share", "secc_hh", "house_tax1", "tot_p"),
    "SHRUG rural SECC AC08"
  ) |>
    dplyr::rename(
      secc_ed_sec_share_rural = ed_sec_share,
      secc_hh_rural = secc_hh,
      house_tax1_rural_n = house_tax1,
      secc_population_rural = tot_p
    )

  secc_urban <- read_shrug_ac08(
    paths$secc_urban_ac08,
    c("ed_sec_share", "secc_hh", "tot_p"),
    "SHRUG urban SECC AC08"
  ) |>
    dplyr::rename(
      secc_ed_sec_share_urban = ed_sec_share,
      secc_hh_urban = secc_hh,
      secc_population_urban = tot_p
    )

  secc_cons_rural <- read_shrug_ac08(
    paths$secc_cons_rural_ac08,
    c("secc_cons_pc_rural", "secc_cons_rural"),
    "SHRUG rural SECC consumption AC08"
  )

  secc_cons_urban <- read_shrug_ac08(
    paths$secc_cons_urban_ac08,
    c("secc_cons_pc_urban", "secc_cons_urban"),
    "SHRUG urban SECC consumption AC08"
  )

  weighted_available <- function(
    rural_value,
    rural_weight,
    urban_value,
    urban_weight
  ) {
    rural_valid <- is.finite(rural_value) &
      is.finite(rural_weight) &
      rural_weight > 0
    urban_valid <- is.finite(urban_value) &
      is.finite(urban_weight) &
      urban_weight > 0

    numerator <-
      dplyr::if_else(
        rural_valid,
        rural_value * rural_weight,
        0
      ) +
      dplyr::if_else(
        urban_valid,
        urban_value * urban_weight,
        0
      )

    denominator <-
      dplyr::if_else(
        rural_valid,
        rural_weight,
        0
      ) +
      dplyr::if_else(
        urban_valid,
        urban_weight,
        0
      )

    dplyr::if_else(
      denominator > 0,
      numerator / denominator,
      NA_real_
    )
  }

  sum_available <- function(rural_value, urban_value) {
    rural_valid <- is.finite(rural_value)
    urban_valid <- is.finite(urban_value)

    dplyr::if_else(
      rural_valid | urban_valid,
      dplyr::if_else(
        rural_valid,
        rural_value,
        0
      ) +
        dplyr::if_else(
          urban_valid,
          urban_value,
          0
        ),
      NA_real_
    )
  }


  # Strict weighted combination:
  # - mixed rural/urban ACs require both sector estimates;
  # - single-sector ACs may use the one relevant estimate;
  # - missing sector weights are treated as unknown, not zero.
  weighted_complete <- function(
    rural_value,
    rural_weight,
    urban_value,
    urban_weight
  ) {
    rural_present <- is.finite(rural_weight) & rural_weight > 0
    urban_present <- is.finite(urban_weight) & urban_weight > 0
    rural_valid <- rural_present & is.finite(rural_value)
    urban_valid <- urban_present & is.finite(urban_value)

    dplyr::case_when(
      rural_present & urban_present &
        rural_valid & urban_valid ~
        (
          rural_value * rural_weight +
            urban_value * urban_weight
        ) /
          (rural_weight + urban_weight),

      rural_present & !urban_present &
        rural_valid ~ rural_value,

      urban_present & !rural_present &
        urban_valid ~ urban_value,

      TRUE ~ NA_real_
    )
  }

  secc_ac <- ac_key |>
    dplyr::left_join(
      secc_rural,
      by = "ac08_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      secc_urban,
      by = "ac08_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      secc_cons_rural,
      by = "ac08_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      secc_cons_urban,
      by = "ac08_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      # AC-wide secondary-or-above share. Rural and urban source
      # shares are recombined using their corresponding populations.
      ed_sec_share = weighted_available(
        secc_ed_sec_share_rural,
        secc_population_rural,
        secc_ed_sec_share_urban,
        secc_population_urban
      ),

      # AC-wide household count from the available rural and urban
      # household-count components.
      secc_hh = sum_available(
        secc_hh_rural,
        secc_hh_urban
      ),

      # AC-wide annual per-capita consumption, in 2012 INR.
      # The broad version uses whichever valid rural/urban components
      # are available and weights them by the corresponding populations.
      secc_cons_pc = weighted_available(
        secc_cons_pc_rural,
        secc_population_rural,
        secc_cons_pc_urban,
        secc_population_urban
      ),

      # Preferred complete-case version. Mixed ACs require both rural
      # and urban estimates; purely rural or urban ACs use the relevant
      # single-sector estimate.
      secc_cons_pc_complete = weighted_complete(
        secc_cons_pc_rural,
        secc_population_rural,
        secc_cons_pc_urban,
        secc_population_urban
      ),

      # AC-wide annual household consumption, in 2012 INR, weighted by
      # rural and urban SECC household counts.
      secc_cons_hh = weighted_available(
        secc_cons_rural,
        secc_hh_rural,
        secc_cons_urban,
        secc_hh_urban
      ),

      secc_cons_hh_complete = weighted_complete(
        secc_cons_rural,
        secc_hh_rural,
        secc_cons_urban,
        secc_hh_urban
      ),

      log_secc_cons_pc = safe_log(secc_cons_pc),
      log_secc_cons_pc_complete =
        safe_log(secc_cons_pc_complete),
      log_secc_cons_hh = safe_log(secc_cons_hh),
      log_secc_cons_hh_complete =
        safe_log(secc_cons_hh_complete),

      # The source variable exists only in the rural SECC table.
      # Keep the scope explicit rather than treating it as an
      # all-AC household count.
      house_tax1_rural_share = safe_share(
        house_tax1_rural_n,
        secc_hh_rural
      ),

      secc_ed_sec_components_n =
        as.integer(
          is.finite(secc_ed_sec_share_rural) &
            is.finite(secc_population_rural) &
            secc_population_rural > 0
        ) +
        as.integer(
          is.finite(secc_ed_sec_share_urban) &
            is.finite(secc_population_urban) &
            secc_population_urban > 0
        ),

      secc_hh_components_n =
        as.integer(is.finite(secc_hh_rural)) +
        as.integer(is.finite(secc_hh_urban)),

      secc_cons_pc_components_n =
        as.integer(
          is.finite(secc_cons_pc_rural) &
            is.finite(secc_population_rural) &
            secc_population_rural > 0
        ) +
        as.integer(
          is.finite(secc_cons_pc_urban) &
            is.finite(secc_population_urban) &
            secc_population_urban > 0
        ),

      secc_cons_hh_components_n =
        as.integer(
          is.finite(secc_cons_rural) &
            is.finite(secc_hh_rural) &
            secc_hh_rural > 0
        ) +
        as.integer(
          is.finite(secc_cons_urban) &
            is.finite(secc_hh_urban) &
            secc_hh_urban > 0
        ),

      secc_cons_pc_complete_available =
        is.finite(secc_cons_pc_complete),

      secc_cons_hh_complete_available =
        is.finite(secc_cons_hh_complete),

      secc_sector_coverage = dplyr::case_when(
        secc_ed_sec_components_n == 2L ~
          "rural_and_urban",
        secc_ed_sec_components_n == 1L &
          is.finite(secc_ed_sec_share_rural) ~
          "rural_component_only",
        secc_ed_sec_components_n == 1L &
          is.finite(secc_ed_sec_share_urban) ~
          "urban_component_only",
        TRUE ~
          "no_valid_component"
      ),

      secc_context_year = 2012L
    ) |>
    dplyr::select(
      ac_uid,
      state_no,
      ac,
      ac08_id,
      shrug_ac_name,

      ed_sec_share,
      secc_ed_sec_share_rural,
      secc_ed_sec_share_urban,

      secc_hh,
      secc_hh_rural,
      secc_hh_urban,

      house_tax1_rural_n,
      house_tax1_rural_share,

      secc_cons_pc,
      secc_cons_pc_complete,
      log_secc_cons_pc,
      log_secc_cons_pc_complete,

      secc_cons_hh,
      secc_cons_hh_complete,
      log_secc_cons_hh,
      log_secc_cons_hh_complete,

      secc_cons_pc_rural,
      secc_cons_rural,
      secc_cons_pc_urban,
      secc_cons_urban,

      secc_population_rural,
      secc_population_urban,
      secc_ed_sec_components_n,
      secc_hh_components_n,
      secc_cons_pc_components_n,
      secc_cons_hh_components_n,
      secc_cons_pc_complete_available,
      secc_cons_hh_complete_available,
      secc_sector_coverage,
      secc_context_year
    ) |>
    dplyr::arrange(state_no, ac)

  assert_unique_rows(
    secc_ac,
    "ac_uid",
    "SHRUG SECC AC context"
  )

  secc_diagnostics <- dplyr::bind_rows(
    secc_ac |>
      dplyr::summarise(
        module = "SECC",
        check = "AC key match",
        n = sum(!is.na(ac08_id)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = NA_real_,
        median = NA_real_,
        maximum = NA_real_,
        passed = n == denominator,
        details = paste(
          "Direct join to SHRUG 2008 AC identifier;",
          "no district allocation or spatial re-crosswalk."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC",
        check = "Valid combined secondary education share",
        n = sum(!is.na(ed_sec_share)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(ed_sec_share),
        median = safe_median(ed_sec_share),
        maximum = safe_max(ed_sec_share),
        passed = all(
          is.na(ed_sec_share) |
            dplyr::between(ed_sec_share, 0, 1)
        ),
        details = paste(
          "Population-weighted combination of available",
          "rural and urban SECC components."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC",
        check = "Rural tax-paying household count valid",
        n = sum(!is.na(house_tax1_rural_n)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(house_tax1_rural_n),
        median = safe_median(house_tax1_rural_n),
        maximum = safe_max(house_tax1_rural_n),
        passed = all(
          is.na(house_tax1_rural_n) |
            house_tax1_rural_n >= 0
        ),
        details = paste(
          "Rural-only source variable;",
          "urban SECC has no direct house_tax1 analogue."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC consumption",
        check = "Combined per-capita consumption available",
        n = sum(!is.na(secc_cons_pc)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(secc_cons_pc),
        median = safe_median(secc_cons_pc),
        maximum = safe_max(secc_cons_pc),
        passed = all(
          is.na(secc_cons_pc) |
            secc_cons_pc >= 0
        ),
        details = paste(
          "Population-weighted rural/urban annual per-capita",
          "consumption in 2012 INR; uses available components."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC consumption",
        check = "Complete combined per-capita consumption available",
        n = sum(!is.na(secc_cons_pc_complete)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(secc_cons_pc_complete),
        median = safe_median(secc_cons_pc_complete),
        maximum = safe_max(secc_cons_pc_complete),
        passed = all(
          is.na(secc_cons_pc_complete) |
            secc_cons_pc_complete >= 0
        ),
        details = paste(
          "Mixed ACs require both rural and urban estimates;",
          "single-sector ACs use the relevant component."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC consumption",
        check = "Combined household consumption available",
        n = sum(!is.na(secc_cons_hh)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(secc_cons_hh),
        median = safe_median(secc_cons_hh),
        maximum = safe_max(secc_cons_hh),
        passed = all(
          is.na(secc_cons_hh) |
            secc_cons_hh >= 0
        ),
        details = paste(
          "Household-count-weighted rural/urban annual household",
          "consumption in 2012 INR; uses available components."
        )
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC consumption",
        check = "Rural per-capita consumption available",
        n = sum(!is.na(secc_cons_pc_rural)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(secc_cons_pc_rural),
        median = safe_median(secc_cons_pc_rural),
        maximum = safe_max(secc_cons_pc_rural),
        passed = all(
          is.na(secc_cons_pc_rural) |
            secc_cons_pc_rural >= 0
        ),
        details = "Annual per-capita rural consumption in 2012 INR."
      ),

    secc_ac |>
      dplyr::summarise(
        module = "SECC consumption",
        check = "Urban per-capita consumption available",
        n = sum(!is.na(secc_cons_pc_urban)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(secc_cons_pc_urban),
        median = safe_median(secc_cons_pc_urban),
        maximum = safe_max(secc_cons_pc_urban),
        passed = all(
          is.na(secc_cons_pc_urban) |
            secc_cons_pc_urban >= 0
        ),
        details = "Annual per-capita urban consumption in 2012 INR."
      ),

    secc_ac |>
      dplyr::count(
        secc_sector_coverage,
        name = "n"
      ) |>
      dplyr::mutate(
        module = "SECC",
        check = "Rural/urban education-component coverage",
        denominator = sum(n),
        pct = 100 * n / denominator,
        minimum = NA_real_,
        median = NA_real_,
        maximum = NA_real_,
        passed = TRUE,
        details = secc_sector_coverage
      ) |>
      dplyr::select(
        module,
        check,
        n,
        denominator,
        pct,
        minimum,
        median,
        maximum,
        passed,
        details
      )
  )

  write_csv_checked(
    secc_ac,
    file.path(
      dirs$intermediate_dir,
      "shrug_secc_ac_context.csv"
    ),
    "ac_uid"
  )

  write_csv_checked(
    secc_diagnostics,
    file.path(
      dirs$diagnostic_dir,
      "shrug_secc_diagnostics.csv"
    )
  )

  list(
    ac_context = secc_ac,
    diagnostics = secc_diagnostics
  )
}

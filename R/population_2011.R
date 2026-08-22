PC11_PCA_AC08_SHA256 <-
  "bcd3127f75d5cacb2fdd70001d17f34987f647240904d9f5805a5079ba0fdcd5"

read_pc11_pca_ac08 <- function(paths) {
  assert_file_exists(
    paths$ac_population_2011,
    "2011 Census PCA aggregated to AC08"
  )

  actual_sha <- digest::digest(
    file = paths$ac_population_2011,
    algo = "sha256"
  )

  if (!identical(
    tolower(actual_sha),
    tolower(PC11_PCA_AC08_SHA256)
  )) {
    stop(
      "pc11_pca_clean_con08.dta SHA-256 mismatch. Expected ",
      PC11_PCA_AC08_SHA256,
      "; found ",
      actual_sha
    )
  }

  raw <- haven::read_dta(
    paths$ac_population_2011
  )

  assert_has_columns(
    raw,
    c(
      "ac08_id",
      "pc11_pca_tot_p",
      "pc11_pca_p_sc",
      "pc11_pca_p_st"
    ),
    "2011 PCA AC08 source"
  )

  out <- raw |>
    dplyr::transmute(
      ac08_id =
        as.character(ac08_id),

      state_no =
        as.integer(
          stringr::str_match(
            ac08_id,
            "^2008-(\\d{2})-(\\d{3})$"
          )[, 2]
        ),

      ac =
        as.integer(
          stringr::str_match(
            ac08_id,
            "^2008-(\\d{2})-(\\d{3})$"
          )[, 3]
        ),

      ac_population_2011_direct =
        as.numeric(pc11_pca_tot_p),

      sc_population_2011_direct =
        as.numeric(pc11_pca_p_sc),

      st_population_2011_direct =
        as.numeric(pc11_pca_p_st)
    )

  assert_unique_rows(
    out,
    c("state_no", "ac"),
    "2011 PCA AC08 source"
  )

  if (any(
    !is.na(out$ac_population_2011_direct) &
      out$ac_population_2011_direct <= 0
  )) {
    stop(
      "Direct 2011 AC population contains a nonpositive value."
    )
  }

  if (any(
    !is.na(out$sc_population_2011_direct) &
      out$sc_population_2011_direct < 0
  )) {
    stop(
      "Direct 2011 AC SC population contains a negative value."
    )
  }

  if (any(
    !is.na(out$st_population_2011_direct) &
      out$st_population_2011_direct < 0
  )) {
    stop(
      "Direct 2011 AC ST population contains a negative value."
    )
  }

  out
}

read_2011_state_population <- function(path) {
  read_excel_raw(path) |>
    dplyr::select(1:9) |>
    rlang::set_names(
      c(
        "state_code",
        "district_code",
        "district_name",
        "census_year",
        "persons",
        "abs_change",
        "pct_change",
        "males",
        "females"
      )
    ) |>
    tidyr::fill(
      state_code,
      district_code,
      district_name,
      .direction = "down"
    ) |>
    dplyr::filter(
      stringr::str_detect(
        census_year,
        "^2011\\*?$"
      ),
      stringr::str_detect(
        as.character(
          district_code
        ),
        "^0+$"
      )
    ) |>
    dplyr::transmute(
      state_no =
        as.integer(state_code),

      state_pop_2011 =
        clean_num(persons)
    ) |>
    dplyr::filter(
      !is.na(state_no),
      !is.na(state_pop_2011),
      state_pop_2011 > 0
    )
}

read_2011_state_group_population <- function(
  path,
  group = c("sc", "st")
) {
  group <- match.arg(group)

  keep_code <-
    if (group == "sc") {
      "000"
    } else {
      "500"
    }

  read_excel_raw(path) |>
    dplyr::select(2:10) |>
    rlang::set_names(
      c(
        "state_code",
        "district_code",
        "area_name",
        "group_code",
        "group_name",
        "area_type",
        "persons",
        "males",
        "females"
      )
    ) |>
    dplyr::filter(
      area_type == "Total",
      group_code == keep_code,
      stringr::str_detect(
        as.character(
          district_code
        ),
        "^0+$"
      )
    ) |>
    dplyr::transmute(
      state_no =
        as.integer(state_code),

      population =
        clean_num(persons)
    ) |>
    dplyr::filter(
      !is.na(state_no),
      !is.na(population),
      population >= 0
    )
}

build_2011_population_allocation <- function(
  ac_reference,
  geometry_land_area,
  pca11_ac,
  ac_land_area,
  district_pop,
  state_pop
) {
  assert_unique_rows(
    state_pop,
    "state_no",
    "2011 state population source"
  )

  allocation <- ac_reference |>
    dplyr::left_join(
      ac_land_area,
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      geometry_land_area,
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      pca11_ac,
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      district_pop,
      by = c(
        "state_no",
        "district_code_2011"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      con08_land_area =
        dplyr::coalesce(
          con08_land_area,
          geom_land_area
        )
    ) |>
    dplyr::group_by(
      state_no,
      district_code_2011
    ) |>
    dplyr::mutate(
      n_ac_in_district =
        dplyr::n(),

      direct_population_available =
        !is.na(
          ac_population_2011_direct
        ) &
        ac_population_2011_direct > 0,

      n_direct_population =
        sum(
          direct_population_available
        ),

      n_missing_population =
        sum(
          !direct_population_available
        ),

      direct_population_sum =
        sum(
          ac_population_2011_direct[
            direct_population_available
          ],
          na.rm = TRUE
        ),

      district_population_residual =
        district_pop_2011 -
        direct_population_sum,

      district_population_residual_valid =
        !is.na(district_pop_2011) &
        n_missing_population > 0 &
        district_population_residual > 0,

      proxy_ac_pop_district =
        dplyr::case_when(
          direct_population_available ~
            ac_population_2011_direct,

          !is.na(district_pop_2011) &
            n_direct_population == 0 ~
            district_pop_2011 /
            n_ac_in_district,

          district_population_residual_valid ~
            district_population_residual /
            n_missing_population,

          !is.na(district_pop_2011) ~
            district_pop_2011 /
            n_ac_in_district,

          TRUE ~
            NA_real_
        ),

      proxy_ac_pop_source_district =
        dplyr::case_when(
          direct_population_available ~
            "direct_pc11_pca_ac08",

          !is.na(district_pop_2011) &
            n_direct_population == 0 ~
            "district_2011_equal_allocation_no_direct_ac",

          district_population_residual_valid ~
            "district_2011_residual_equal_allocation",

          !is.na(district_pop_2011) ~
            "district_2011_mean_fallback_invalid_residual",

          TRUE ~
            NA_character_
        ),

      direct_sum =
        direct_population_sum,

      direct_inconsistent =
        !is.na(district_pop_2011) &
        direct_population_sum >
          district_pop_2011
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      state_pop,
      by = "state_no",
      relationship = "many-to-one"
    ) |>
    dplyr::group_by(
      state_no
    ) |>
    dplyr::mutate(
      n_ac_in_state =
        dplyr::n(),

      proxy_ac_pop =
        dplyr::case_when(
          !is.na(
            proxy_ac_pop_district
          ) &
            proxy_ac_pop_district > 0 ~
            proxy_ac_pop_district,

          !is.na(
            state_pop_2011
          ) &
            state_pop_2011 > 0 ~
            state_pop_2011 /
            n_ac_in_state,

          TRUE ~
            NA_real_
        ),

      proxy_ac_pop_source =
        dplyr::case_when(
          !is.na(
            proxy_ac_pop_district
          ) &
            proxy_ac_pop_district > 0 ~
            proxy_ac_pop_source_district,

          !is.na(
            state_pop_2011
          ) &
            state_pop_2011 > 0 ~
            "state_2011_mean_fallback_no_district_population",

          TRUE ~
            "missing_no_2011_population_source"
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      allocation_group =
        dplyr::if_else(
          !is.na(
            district_code_2011
          ),
          as.character(
            district_code_2011
          ),
          paste0(
            "UNMATCHED_",
            ac_uid
          )
        )
    ) |>
    dplyr::group_by(
      state_no,
      allocation_group
    ) |>
    dplyr::mutate(
      proxy_sum =
        sum(
          proxy_ac_pop,
          na.rm = TRUE
        ),

      all_proxy_available =
        all(
          !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0
        ),

      ac_alloc_share =
        dplyr::if_else(
          all_proxy_available &
            proxy_sum > 0,
          proxy_ac_pop /
            proxy_sum,
          NA_real_
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      state,
      state_no,
      pc,
      ac,
      ac_uid,
      district_code_2011,
      district_name_2011,
      district_harmonization_group_id,
      ac_population_2011_direct,
      sc_population_2011_direct,
      st_population_2011_direct,
      district_pop_2011,
      state_pop_2011,
      proxy_ac_pop,
      proxy_ac_pop_source,
      con08_land_area,
      ac_alloc_share,
      n_ac_in_district,
      n_ac_in_state,
      direct_sum,
      direct_inconsistent,
      district_population_residual,
      district_population_residual_valid
    )

  allocation
}

impute_2011_group_population <- function(
  allocation,
  district_data,
  state_data,
  direct_column,
  district_column,
  state_column,
  output_column,
  source_column,
  structural_zero_states = integer()
) {
  assert_unique_rows(
    district_data,
    c(
      "state_no",
      "district_code_2011"
    ),
    paste0(
      district_column,
      " district source"
    )
  )

  assert_unique_rows(
    state_data,
    "state_no",
    paste0(
      state_column,
      " state source"
    )
  )

  x <- allocation |>
    dplyr::select(
      state_no,
      ac,
      ac_uid,
      district_code_2011,
      district_pop_2011,
      state_pop_2011,
      proxy_ac_pop,
      dplyr::all_of(
        direct_column
      )
    ) |>
    dplyr::mutate(
      direct_group_population =
        .data[[direct_column]]
    ) |>
    dplyr::left_join(
      district_data |>
        dplyr::select(
          state_no,
          district_code_2011,
          dplyr::all_of(
            district_column
          )
        ),
      by = c(
        "state_no",
        "district_code_2011"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      district_group_population =
        .data[[district_column]]
    ) |>
    dplyr::left_join(
      state_data |>
        dplyr::select(
          state_no,
          dplyr::all_of(
            state_column
          )
        ),
      by = "state_no",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      state_group_population =
        .data[[state_column]]
    ) |>
    dplyr::group_by(
      state_no,
      district_code_2011
    ) |>
    dplyr::mutate(
      direct_group_available =
        !is.na(
          direct_group_population
        ) &
        direct_group_population >= 0,

      direct_group_sum =
        sum(
          direct_group_population[
            direct_group_available
          ],
          na.rm = TRUE
        ),

      n_missing_group =
        sum(
          !direct_group_available
        ),

      district_group_residual =
        district_group_population -
        direct_group_sum,

      missing_population_base =
        sum(
          proxy_ac_pop[
            !direct_group_available &
            !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0
          ],
          na.rm = TRUE
        ),

      district_group_residual_valid =
        !is.na(
          district_group_population
        ) &
        n_missing_group > 0 &
        district_group_residual >= 0 &
        missing_population_base > 0 &
        district_group_residual <=
          missing_population_base,

      group_population_district =
        dplyr::case_when(
          direct_group_available ~
            direct_group_population,

          state_no %in%
            structural_zero_states ~
            0,

          district_group_residual_valid ~
            district_group_residual *
            proxy_ac_pop /
            missing_population_base,

          !is.na(
            district_group_population
          ) &
            !is.na(
              district_pop_2011
            ) &
            district_pop_2011 > 0 &
            !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0 ~
            proxy_ac_pop *
            district_group_population /
            district_pop_2011,

          TRUE ~
            NA_real_
        ),

      group_population_source_district =
        dplyr::case_when(
          direct_group_available ~
            "direct_pc11_pca_ac08",

          state_no %in%
            structural_zero_states ~
            "structural_zero_2011",

          district_group_residual_valid ~
            "district_2011_residual_population_weighted",

          !is.na(
            district_group_population
          ) &
            !is.na(
              district_pop_2011
            ) &
            district_pop_2011 > 0 &
            !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0 ~
            "district_2011_share_fallback",

          TRUE ~
            NA_character_
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      group_population =
        dplyr::case_when(
          !is.na(
            group_population_district
          ) ~
            group_population_district,

          state_no %in%
            structural_zero_states ~
            0,

          !is.na(
            state_group_population
          ) &
            !is.na(
              state_pop_2011
            ) &
            state_pop_2011 > 0 &
            !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0 ~
            proxy_ac_pop *
            state_group_population /
            state_pop_2011,

          TRUE ~
            NA_real_
        ),

      group_population_source =
        dplyr::case_when(
          !is.na(
            group_population_district
          ) ~
            group_population_source_district,

          state_no %in%
            structural_zero_states ~
            "structural_zero_2011",

          !is.na(
            state_group_population
          ) &
            !is.na(
              state_pop_2011
            ) &
            state_pop_2011 > 0 &
            !is.na(proxy_ac_pop) &
            proxy_ac_pop > 0 ~
            "state_2011_share_fallback_no_district_source",

          TRUE ~
            "missing_no_2011_group_source"
        )
    ) |>
    dplyr::select(
      state_no,
      ac,
      ac_uid,
      proxy_ac_pop,
      group_population,
      group_population_source
    )

  names(x)[
    names(x) ==
      "group_population"
  ] <- output_column

  names(x)[
    names(x) ==
      "group_population_source"
  ] <- source_column

  x
}

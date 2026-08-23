# FDI project cleaning, spatial exposure, and AC-year aggregation.

build_fdi <- function(paths, dirs, geography, elections, demographics) {
  message("Building FDI exposure")
  purrr::walk(
    c(paths$fdi, paths$fdi_sector_config, paths$fdi_status_config),
    assert_file_exists
  )

  sector_key <- readr::read_csv(paths$fdi_sector_config, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      source_activity = stringr::str_squish(source_activity)
    )

  assert_has_columns(
    sector_key,
    c(
      "source_activity",
      "standardized_sector",
      "included_in_total",
      "included_in_manufacturing",
      "included_in_services"
    ),
    "FDI sector taxonomy"
  )

  assert_unique_rows(
    sector_key,
    "source_activity",
    "FDI sector taxonomy"
  )

  if (
    any(
      is.na(sector_key$standardized_sector)
    ) ||
    any(
      !sector_key$standardized_sector %in%
        c(
          "manufacturing",
          "services"
        )
    )
  ) {
    stop(
      "FDI sector taxonomy must classify every activity as manufacturing or services."
    )
  }

  taxonomy_flag_columns <- c(
    "included_in_total",
    "included_in_manufacturing",
    "included_in_services"
  )

  if (
    any(
      is.na(
        as.matrix(
          sector_key[
            taxonomy_flag_columns
          ]
        )
      )
    )
  ) {
    stop(
      "FDI sector taxonomy inclusion flags may not be missing."
    )
  }

  if (
    any(
      !sector_key$included_in_total
    )
  ) {
    stop(
      "Every activity in the approved FDI taxonomy must be included in total FDI."
    )
  }

  if (
    any(
      sector_key$included_in_manufacturing ==
        sector_key$included_in_services
    )
  ) {
    stop(
      "Each FDI activity must belong to exactly one of manufacturing or services."
    )
  }

  expected_manufacturing_flag <-
    sector_key$standardized_sector ==
      "manufacturing"

  if (
    any(
      sector_key$included_in_manufacturing !=
        expected_manufacturing_flag
    ) ||
    any(
      sector_key$included_in_services !=
        !expected_manufacturing_flag
    )
  ) {
    stop(
      "FDI taxonomy sector labels and inclusion flags disagree."
    )
  }

  sector_taxonomy_sha256 <-
    digest::digest(
      file = paths$fdi_sector_config,
      algo = "sha256"
    )

  status_key <- readr::read_csv(paths$fdi_status_config, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(source_status = stringr::str_squish(source_status))

  raw <- readr::read_csv(paths$fdi, show_col_types = FALSE) |>
    janitor::clean_names()
  source_id_var <- intersect(c("x", "project_id", "id"), names(raw))[1]
  if (is.na(source_id_var)) raw$source_project_id_input <- NA_character_
  assert_has_columns(
    raw,
    c("project_date", "coordinates", "activity", "project_status"),
    "FDI project file"
  )

  periods <- tibble::tribble(
    ~year, ~period_start, ~period_end,
    2009L, as.Date("2004-04-01"), as.Date("2009-04-01"),
    2014L, as.Date("2009-04-01"), as.Date("2014-04-01")
  )

  projects <- raw |>
    dplyr::mutate(
      source_row = dplyr::row_number(),
      source_project_id = if (!is.na(source_id_var)) {
        as.character(.data[[source_id_var]])
      } else {
        as.character(source_project_id_input)
      },
      fdi_project_uid = dplyr::if_else(
        !is.na(source_project_id) & source_project_id != "",
        paste0("FDI_", source_project_id),
        sprintf("FDI_ROW_%05d", source_row)
      ),
      project_month = as.Date(lubridate::parse_date_time(project_date, orders = c("b Y", "B Y"))),
      source_activity = stringr::str_squish(as.character(activity)),
      source_status = stringr::str_squish(as.character(project_status))
    ) |>
    tidyr::extract(
      coordinates,
      into = c("lat", "lon"),
      regex = "^\\s*(-?\\d+(?:\\.\\d+)?)\\s*,\\s*(-?\\d+(?:\\.\\d+)?)\\s*$",
      remove = FALSE,
      convert = TRUE
    ) |>
    dplyr::left_join(sector_key, by = "source_activity", relationship = "many-to-one") |>
    dplyr::left_join(status_key, by = "source_status", relationship = "many-to-one") |>
    dplyr::mutate(
      standardized_status = dplyr::coalesce(standardized_status, "other_or_unknown"),
      coordinate_valid = is.finite(lat) & is.finite(lon) & dplyr::between(lat, 5, 38) & dplyr::between(lon, 65, 100),
      year = dplyr::case_when(
        project_month >= periods$period_start[1] & project_month < periods$period_end[1] ~ 2009L,
        project_month >= periods$period_start[2] & project_month < periods$period_end[2] ~ 2014L,
        TRUE ~ NA_integer_
      )
    )

  unmapped_activities <- projects |>
    dplyr::filter(
      is.na(
        standardized_sector
      )
    ) |>
    dplyr::count(
      source_activity,
      sort = TRUE
    )

  if (
    nrow(
      unmapped_activities
    ) > 0
  ) {
    stop(
      "Unmapped FDI source activities: ",
      paste(
        unmapped_activities$source_activity,
        collapse = ", "
      )
    )
  }

  duplicate_project_ids <- projects |>
    dplyr::count(fdi_project_uid, name = "n_source_rows") |>
    dplyr::filter(n_source_rows > 1)
  if (nrow(duplicate_project_ids) > 0) {
    # Keep source rows distinct when a source ID is reused.
    projects <- projects |>
      dplyr::group_by(fdi_project_uid) |>
      dplyr::mutate(
        fdi_project_uid = dplyr::if_else(
          dplyr::n() > 1,
          paste0(fdi_project_uid, "_ROW_", source_row),
          fdi_project_uid
        )
      ) |>
      dplyr::ungroup()
  }

  points <- projects |>
    dplyr::filter(coordinate_valid) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(sf::st_crs(geography$ac_reference_sf))

  ac_spatial <- geography$ac_reference_sf |>
    dplyr::select(
      ac_uid,
      state_no,
      ac,
      geometry
    )

  spatial_support <- ac_spatial |>
    sf::st_drop_geometry() |>
    dplyr::distinct(
      ac_uid,
      state_no,
      ac
    ) |>
    dplyr::left_join(
      geography$ac_neighbor_pairs |>
        dplyr::filter(
          !is.na(ac_uid)
        ) |>
        dplyr::count(
          ac_uid,
          name = "fdi_n_touching_neighbors"
        ),
      by = "ac_uid",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      fdi_spatial_support = TRUE,
      fdi_n_touching_neighbors =
        dplyr::coalesce(
          fdi_n_touching_neighbors,
          0L
        )
    )

  fdi_geography_lookup <- dplyr::bind_rows(
    geography$ac_reference |>
      dplyr::select(
        ac_uid,
        state_no,
        ac
      ),
    spatial_support |>
      dplyr::select(
        ac_uid,
        state_no,
        ac
      )
  ) |>
    dplyr::filter(
      !is.na(ac_uid)
    ) |>
    dplyr::distinct(
      ac_uid,
      .keep_all = TRUE
    )

  containing <- sf::st_join(points, ac_spatial, join = sf::st_within, left = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::select(fdi_project_uid, containing_ac_uid = ac_uid)

  multiple_containing <- containing |>
    dplyr::filter(!is.na(containing_ac_uid)) |>
    dplyr::count(fdi_project_uid, name = "n") |>
    dplyr::filter(n > 1)
  if (nrow(multiple_containing) > 0) {
    stop("Some FDI projects fall in multiple containing ACs. Review geometry before continuing.")
  }

  own <- containing |>
    dplyr::filter(!is.na(containing_ac_uid)) |>
    dplyr::transmute(
      fdi_project_uid,
      exposed_ac_uid = containing_ac_uid,
      exposure_scope = "own"
    )

  adjacent <- containing |>
    dplyr::filter(!is.na(containing_ac_uid)) |>
    dplyr::left_join(
      geography$ac_neighbor_pairs |>
        dplyr::transmute(
          containing_ac_uid = ac_uid,
          exposed_ac_uid = neighbor_ac_uid
        ),
      by = "containing_ac_uid",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(!is.na(exposed_ac_uid), exposed_ac_uid != containing_ac_uid) |>
    dplyr::transmute(fdi_project_uid, exposed_ac_uid, exposure_scope = "adjacent")

  exposure_base <- dplyr::bind_rows(own, adjacent) |>
    dplyr::distinct(fdi_project_uid, exposed_ac_uid, exposure_scope)

  exposure <- dplyr::bind_rows(
    exposure_base,
    exposure_base |>
      dplyr::transmute(fdi_project_uid, exposed_ac_uid, exposure_scope = "local")
  ) |>
    dplyr::distinct(fdi_project_uid, exposed_ac_uid, exposure_scope) |>
    dplyr::left_join(
      projects |>
        dplyr::select(
          fdi_project_uid, project_month, year, source_activity,
          standardized_sector, source_status, standardized_status,
          coordinate_valid
        ),
      by = "fdi_project_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      geography$ac_reference |>
        dplyr::select(exposed_ac_uid = ac_uid, state_no, ac),
      by = "exposed_ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(spatial_match_valid = !is.na(exposed_ac_uid))

  sector_levels <- c("total", "mfg", "services")
  scope_levels <- c("own", "adjacent", "local")
  status_levels <- c("all", "announced", "opened")

  exposure_expanded <- exposure |>
    dplyr::filter(
      spatial_match_valid,
      !is.na(year),
      standardized_status %in% c("announced", "opened")
    ) |>
    dplyr::mutate(
      sector_list = purrr::map(
        standardized_sector,
        ~ c("total", dplyr::case_when(
          .x == "manufacturing" ~ "mfg",
          .x == "services" ~ "services",
          TRUE ~ NA_character_
        ))
      ),
      status_list = purrr::map(standardized_status, ~ c("all", .x))
    ) |>
    tidyr::unnest_longer(sector_list, values_to = "sector") |>
    tidyr::unnest_longer(status_list, values_to = "status") |>
    dplyr::filter(!is.na(sector)) |>
    dplyr::distinct(fdi_project_uid, exposed_ac_uid, year, sector, exposure_scope, status)

  counts_long <- exposure_expanded |>
    dplyr::summarise(
      projects_n = dplyr::n_distinct(fdi_project_uid),
      .by = c(exposed_ac_uid, year, sector, exposure_scope, status)
    )

  full_grid <- elections$ac_year |>
    dplyr::distinct(
      exposed_ac_uid = ac_uid,
      year
    ) |>
    dplyr::left_join(
      spatial_support |>
        dplyr::select(
          exposed_ac_uid = ac_uid,
          fdi_spatial_support,
          fdi_n_touching_neighbors
        ),
      by = "exposed_ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      fdi_spatial_support =
        dplyr::coalesce(
          fdi_spatial_support,
          FALSE
        ),

      fdi_n_touching_neighbors =
        dplyr::if_else(
          fdi_spatial_support,
          dplyr::coalesce(
            fdi_n_touching_neighbors,
            0L
          ),
          NA_integer_
        )
    ) |>
    tidyr::crossing(
      sector = sector_levels,
      exposure_scope = scope_levels,
      status = status_levels
    ) |>
    dplyr::left_join(
      counts_long,
      by = c(
        "exposed_ac_uid",
        "year",
        "sector",
        "exposure_scope",
        "status"
      )
    ) |>
    dplyr::mutate(
      projects_n =
        dplyr::if_else(
          fdi_spatial_support,
          tidyr::replace_na(
            projects_n,
            0L
          ),
          NA_integer_
        )
    ) |>
    dplyr::left_join(
      demographics |>
        dplyr::select(
          exposed_ac_uid = ac_uid,
          proxy_ac_pop
        ),
      by = "exposed_ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      projects_pc100k =
        per_100k(
          projects_n,
          proxy_ac_pop
        ),

      log1p_projects_pc100k =
        log1p(
          projects_pc100k
        ),

      stem =
        paste(
          "fdi",
          sector,
          exposure_scope,
          status,
          sep = "_"
        )
    )

  fdi_ac_year <- full_grid |>
    dplyr::select(
      exposed_ac_uid,
      year,
      fdi_spatial_support,
      fdi_n_touching_neighbors,
      stem,
      projects_n,
      projects_pc100k,
      log1p_projects_pc100k
    ) |>
    tidyr::pivot_wider(
      names_from = stem,
      values_from = c(
        projects_n,
        projects_pc100k,
        log1p_projects_pc100k
      ),
      names_glue =
        "{stem}_{.value}"
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(
        .x,
        "_projects_n$",
        "_n"
      )
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(
        .x,
        "_projects_pc100k$",
        "_pc100k"
      )
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(
        .x,
        "_log1p_projects_pc100k$",
        "_log1p_pc100k"
      )
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(
        .x,
        "^(fdi_.+)_log1p_pc100k$",
        "log1p_\\1_pc100k"
      )
    ) |>
    dplyr::mutate(
      any_fdi_total_own_all =
        dplyr::if_else(
          fdi_spatial_support,
          as.integer(
            fdi_total_own_all_n > 0
          ),
          NA_integer_
        ),

      any_fdi_total_adjacent_all =
        dplyr::if_else(
          fdi_spatial_support,
          as.integer(
            fdi_total_adjacent_all_n > 0
          ),
          NA_integer_
        ),

      any_fdi_total_local_all =
        dplyr::if_else(
          fdi_spatial_support,
          as.integer(
            fdi_total_local_all_n > 0
          ),
          NA_integer_
        )
    ) |>
    dplyr::rename(
      ac_uid =
        exposed_ac_uid
    ) |>
    dplyr::left_join(
      fdi_geography_lookup,
      by = "ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(
      state_no,
      ac,
      year
    )

  for (
    scope in scope_levels
  ) {
    for (
      status in status_levels
    ) {
      total_col <-
        paste0(
          "fdi_total_",
          scope,
          "_",
          status,
          "_n"
        )

      mfg_col <-
        paste0(
          "fdi_mfg_",
          scope,
          "_",
          status,
          "_n"
        )

      services_col <-
        paste0(
          "fdi_services_",
          scope,
          "_",
          status,
          "_n"
        )

      identity_failed <-
        !is.na(
          fdi_ac_year[[total_col]]
        ) &
        (
          is.na(
            fdi_ac_year[[mfg_col]]
          ) |
          is.na(
            fdi_ac_year[[services_col]]
          ) |
          fdi_ac_year[[total_col]] !=
            fdi_ac_year[[mfg_col]] +
            fdi_ac_year[[services_col]]
        )

      if (
        any(
          identity_failed
        )
      ) {
        stop(
          "FDI sector identity failed for scope=",
          scope,
          ", status=",
          status,
          ": total must equal manufacturing plus services."
        )
      }
    }
  }

  for (
    sector in sector_levels
  ) {
    for (
      status in status_levels
    ) {
      own_col <-
        paste0(
          "fdi_",
          sector,
          "_own_",
          status,
          "_n"
        )

      adjacent_col <-
        paste0(
          "fdi_",
          sector,
          "_adjacent_",
          status,
          "_n"
        )

      local_col <-
        paste0(
          "fdi_",
          sector,
          "_local_",
          status,
          "_n"
        )

      spatial_identity_failed <-
        !is.na(
          fdi_ac_year[[local_col]]
        ) &
        (
          is.na(
            fdi_ac_year[[own_col]]
          ) |
          is.na(
            fdi_ac_year[[adjacent_col]]
          ) |
          fdi_ac_year[[local_col]] !=
            fdi_ac_year[[own_col]] +
            fdi_ac_year[[adjacent_col]]
        )

      if (
        any(
          spatial_identity_failed
        )
      ) {
        stop(
          "FDI spatial identity failed for sector=",
          sector,
          ", status=",
          status,
          ": local must equal own plus adjacent."
        )
      }
    }
  }

  assert_unique_rows(fdi_ac_year, c("ac_uid", "year"), "FDI AC-year data")

  project_diagnostics <- projects |>
    dplyr::summarise(
      n_projects = dplyr::n_distinct(fdi_project_uid),
      n_coordinate_valid = dplyr::n_distinct(fdi_project_uid[coordinate_valid]),
      n_coordinate_invalid = dplyr::n_distinct(fdi_project_uid[!coordinate_valid]),
      n_in_election_period = dplyr::n_distinct(fdi_project_uid[!is.na(year)]),
      n_unknown_sector = dplyr::n_distinct(fdi_project_uid[is.na(standardized_sector)]),
      n_unknown_status = dplyr::n_distinct(fdi_project_uid[standardized_status == "other_or_unknown"]),
      .by = c(standardized_status, standardized_sector)
    )

  exposure_diagnostics <- full_grid |>
    dplyr::summarise(
      n_acs =
        dplyr::n(),

      n_spatially_supported_acs =
        sum(
          fdi_spatial_support
        ),

      n_spatially_unsupported_acs =
        sum(
          !fdi_spatial_support
        ),

      n_exposed_acs =
        sum(
          projects_n > 0,
          na.rm = TRUE
        ),

      n_zero_exposure_acs =
        sum(
          projects_n == 0,
          na.rm = TRUE
        ),

      pct_zero_exposure_acs =
        dplyr::if_else(
          n_spatially_supported_acs > 0,
          100 *
            n_zero_exposure_acs /
            n_spatially_supported_acs,
          NA_real_
        ),

      mean_projects =
        mean(
          projects_n,
          na.rm = TRUE
        ),

      median_projects =
        median(
          projects_n,
          na.rm = TRUE
        ),

      max_projects =
        max(
          projects_n,
          na.rm = TRUE
        ),

      .by = c(
        year,
        sector,
        exposure_scope,
        status
      )
    )

  spatial_support_diagnostics <- elections$ac_year |>
    dplyr::distinct(
      ac_uid,
      year
    ) |>
    dplyr::left_join(
      spatial_support |>
        dplyr::select(
          ac_uid,
          fdi_spatial_support,
          fdi_n_touching_neighbors
        ),
      by = "ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      fdi_spatial_support =
        dplyr::coalesce(
          fdi_spatial_support,
          FALSE
        )
    ) |>
    dplyr::summarise(
      n_election_acs =
        dplyr::n(),

      n_spatially_supported =
        sum(
          fdi_spatial_support
        ),

      n_spatially_unsupported =
        sum(
          !fdi_spatial_support
        ),

      n_supported_zero_touching_neighbors =
        sum(
          fdi_spatial_support &
            fdi_n_touching_neighbors == 0,
          na.rm = TRUE
        ),

      .by = year
    )

  fdi_source_sha256 <-
    digest::digest(
      file = paths$fdi,
      algo = "sha256"
    )

  build_provenance <- dplyr::bind_rows(
    tibble::tibble(
      artifact =
        "fdi_raw_source",
      source_file =
        paths$fdi,
      sha256 =
        fdi_source_sha256,
      n_rows =
        nrow(
          raw
        ),
      n_activities =
        NA_integer_,
      classification_rule =
        NA_character_
    ),

    tibble::tibble(
      artifact =
        "fdi_sector_taxonomy",
      source_file =
        paths$fdi_sector_config,
      sha256 =
        sector_taxonomy_sha256,
      n_rows =
        NA_integer_,
      n_activities =
        nrow(
          sector_key
        ),
      classification_rule =
        paste(
          "Manufacturing family = Manufacturing, Extraction,",
          "Electricity, Recycling; services = all remaining",
          "approved fDi Markets activities"
        )
    )
  )

  write_csv_checked(projects, file.path(dirs$intermediate_dir, "fdi_projects_clean.csv"), "fdi_project_uid")
  write_csv_checked(exposure, file.path(dirs$intermediate_dir, "fdi_project_exposure.csv"), c("fdi_project_uid", "exposed_ac_uid", "exposure_scope"))
  write_csv_checked(fdi_ac_year, file.path(dirs$intermediate_dir, "fdi_ac_year.csv"), c("ac_uid", "year"))
  write_csv_checked(project_diagnostics, file.path(dirs$diagnostic_dir, "fdi_project_diagnostics.csv"))
  write_csv_checked(
    exposure_diagnostics,
    file.path(
      dirs$diagnostic_dir,
      "fdi_exposure_diagnostics.csv"
    )
  )
  write_csv_checked(
    spatial_support_diagnostics,
    file.path(
      dirs$diagnostic_dir,
      "fdi_spatial_support_diagnostics.csv"
    )
  )
  write_csv_checked(
    build_provenance,
    file.path(
      dirs$diagnostic_dir,
      "fdi_build_provenance.csv"
    )
  )
  readr::write_csv(sector_key, file.path(dirs$final_dir, "fdi_sector_taxonomy.csv"))
  readr::write_csv(status_key, file.path(dirs$final_dir, "fdi_status_taxonomy.csv"))

  list(
    projects = projects,
    exposure = exposure,
    ac_year = fdi_ac_year,
    diagnostics = list(
      projects = project_diagnostics,
      exposure = exposure_diagnostics,
      spatial_support = spatial_support_diagnostics,
      provenance = build_provenance
    )
  )
}

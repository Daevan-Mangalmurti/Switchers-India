# FDI project cleaning, spatial exposure, and AC-year aggregation.

build_fdi <- function(paths, dirs, geography, elections, demographics) {
  message("Building FDI exposure")
  purrr::walk(
    c(paths$fdi, paths$fdi_sector_config, paths$fdi_status_config),
    assert_file_exists
  )

  sector_key <- readr::read_csv(paths$fdi_sector_config, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(source_activity = stringr::str_squish(source_activity))
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
      standardized_sector = dplyr::coalesce(standardized_sector, "other"),
      standardized_status = dplyr::coalesce(standardized_status, "other_or_unknown"),
      coordinate_valid = is.finite(lat) & is.finite(lon) & dplyr::between(lat, 5, 38) & dplyr::between(lon, 65, 100),
      year = dplyr::case_when(
        project_month >= periods$period_start[1] & project_month < periods$period_end[1] ~ 2009L,
        project_month >= periods$period_start[2] & project_month < periods$period_end[2] ~ 2014L,
        TRUE ~ NA_integer_
      )
    )

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
    dplyr::select(ac_uid, state_no, ac, geometry)

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
    dplyr::distinct(exposed_ac_uid = ac_uid, year) |>
    tidyr::crossing(
      sector = sector_levels,
      exposure_scope = scope_levels,
      status = status_levels
    ) |>
    dplyr::left_join(
      counts_long,
      by = c("exposed_ac_uid", "year", "sector", "exposure_scope", "status")
    ) |>
    dplyr::mutate(projects_n = tidyr::replace_na(projects_n, 0L)) |>
    dplyr::left_join(
      demographics |>
        dplyr::select(exposed_ac_uid = ac_uid, proxy_ac_pop),
      by = "exposed_ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      projects_pc100k = per_100k(projects_n, proxy_ac_pop),
      log1p_projects_pc100k = log1p(projects_pc100k),
      stem = paste("fdi", sector, exposure_scope, status, sep = "_")
    )

  fdi_ac_year <- full_grid |>
    dplyr::select(exposed_ac_uid, year, stem, projects_n, projects_pc100k, log1p_projects_pc100k) |>
    tidyr::pivot_wider(
      names_from = stem,
      values_from = c(projects_n, projects_pc100k, log1p_projects_pc100k),
      names_glue = "{stem}_{.value}"
    ) |>
    dplyr::rename_with(~ stringr::str_replace(.x, "_projects_n$", "_n")) |>
    dplyr::rename_with(~ stringr::str_replace(.x, "_projects_pc100k$", "_pc100k")) |>
    dplyr::rename_with(~ stringr::str_replace(.x, "_log1p_projects_pc100k$", "_log1p_pc100k")) |>
    # Restore the approved prefix order: log1p_fdi_...
    dplyr::rename_with(
      ~ stringr::str_replace(.x, "^(fdi_.+)_log1p_pc100k$", "log1p_\\1_pc100k")
    ) |>
    dplyr::mutate(
      any_fdi_total_own_all =
        as.integer(fdi_total_own_all_n > 0),
      any_fdi_total_adjacent_all =
        as.integer(fdi_total_adjacent_all_n > 0),
      any_fdi_total_local_all =
        as.integer(fdi_total_local_all_n > 0)
    ) |>
    dplyr::rename(ac_uid = exposed_ac_uid) |>
    dplyr::left_join(
      geography$ac_reference |>
        dplyr::select(ac_uid, state_no, ac),
      by = "ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(state_no, ac, year)

  assert_unique_rows(fdi_ac_year, c("ac_uid", "year"), "FDI AC-year data")

  project_diagnostics <- projects |>
    dplyr::summarise(
      n_projects = dplyr::n_distinct(fdi_project_uid),
      n_coordinate_valid = dplyr::n_distinct(fdi_project_uid[coordinate_valid]),
      n_coordinate_invalid = dplyr::n_distinct(fdi_project_uid[!coordinate_valid]),
      n_in_election_period = dplyr::n_distinct(fdi_project_uid[!is.na(year)]),
      n_unknown_sector = dplyr::n_distinct(fdi_project_uid[standardized_sector == "other"]),
      n_unknown_status = dplyr::n_distinct(fdi_project_uid[standardized_status == "other_or_unknown"]),
      .by = c(standardized_status, standardized_sector)
    )

  exposure_diagnostics <- full_grid |>
    dplyr::summarise(
      n_exposed_acs = sum(projects_n > 0),
      n_zero_exposure_acs = sum(projects_n == 0),
      pct_zero_exposure_acs = 100 * n_zero_exposure_acs / dplyr::n(),
      mean_projects = mean(projects_n),
      median_projects = median(projects_n),
      max_projects = max(projects_n),
      .by = c(year, sector, exposure_scope, status)
    )

  write_csv_checked(projects, file.path(dirs$intermediate_dir, "fdi_projects_clean.csv"), "fdi_project_uid")
  write_csv_checked(exposure, file.path(dirs$intermediate_dir, "fdi_project_exposure.csv"), c("fdi_project_uid", "exposed_ac_uid", "exposure_scope"))
  write_csv_checked(fdi_ac_year, file.path(dirs$intermediate_dir, "fdi_ac_year.csv"), c("ac_uid", "year"))
  write_csv_checked(project_diagnostics, file.path(dirs$diagnostic_dir, "fdi_project_diagnostics.csv"))
  write_csv_checked(exposure_diagnostics, file.path(dirs$diagnostic_dir, "fdi_exposure_diagnostics.csv"))
  readr::write_csv(sector_key, file.path(dirs$final_dir, "fdi_sector_taxonomy.csv"))
  readr::write_csv(status_key, file.path(dirs$final_dir, "fdi_status_taxonomy.csv"))

  list(
    projects = projects,
    exposure = exposure,
    ac_year = fdi_ac_year,
    diagnostics = list(projects = project_diagnostics, exposure = exposure_diagnostics)
  )
}

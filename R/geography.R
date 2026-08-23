# Geographic reference, AC adjacency, and 2001-2011 district lineage.

build_district_lineage <- function(paths, dirs) {
  assert_file_exists(paths$district_lineage_config, "district lineage configuration")

  raw <- readr::read_csv(paths$district_lineage_config, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(district_2001), !is.na(district_2011)) |>
    dplyr::mutate(
      state_no_2001 = state_name_to_no(state_2001),
      state_no_2011 = state_name_to_no(state_2011),
      district_name_2001_norm = norm_name(district_2001),
      district_name_2011_norm = norm_name(district_2011),
      node_2001 = paste("2001", state_no_2001, district_name_2001_norm, sep = "|"),
      node_2011 = paste("2011", state_no_2011, district_name_2011_norm, sep = "|")
    )

  component_lookup <- connected_component_ids(raw$node_2001, raw$node_2011)

  lineage_pairs <- raw |>
    dplyr::mutate(
      district_harmonization_group_id = unname(component_lookup[node_2001]),
      relationship_type = dplyr::case_when(
        stringr::str_detect(relationship_type, "many_to_many") ~ "many_to_many",
        stringr::str_detect(relationship_type, "one_to_many") ~ "split",
        stringr::str_detect(relationship_type, "many_to_one") ~ "merge",
        stringr::str_detect(relationship_type, "renamed") ~ "renamed",
        TRUE ~ "stable"
      ),
      change_comparable = !stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(change_usable, "")),
        "no|unresolved"
      ),
      harmonization_method = dplyr::coalesce(harmonization_method, "common lineage aggregation")
    ) |>
    dplyr::select(
      state_no_2001,
      state_2001,
      district_2001,
      district_name_2001_norm,
      state_no_2011,
      state_2011,
      district_2011,
      district_name_2011_norm,
      district_harmonization_group_id,
      relationship_type,
      change_comparable,
      harmonization_method,
      status,
      source_url
    )

  group_summary <- lineage_pairs |>
    dplyr::summarise(
      n_2001_districts = dplyr::n_distinct(paste(state_no_2001, district_name_2001_norm)),
      n_2011_districts = dplyr::n_distinct(paste(state_no_2011, district_name_2011_norm)),
      relationship_type = dplyr::case_when(
        n_2001_districts > 1 & n_2011_districts > 1 ~ "many_to_many",
        n_2001_districts == 1 & n_2011_districts > 1 ~ "split",
        n_2001_districts > 1 & n_2011_districts == 1 ~ "merge",
        any(relationship_type == "renamed") ~ "renamed",
        TRUE ~ "stable"
      ),
      change_comparable = all(change_comparable),
      harmonization_method = paste(unique(harmonization_method), collapse = "; "),
      .by = district_harmonization_group_id
    )

  lineage_pairs <- lineage_pairs |>
    dplyr::select(-relationship_type, -change_comparable, -harmonization_method) |>
    dplyr::left_join(group_summary, by = "district_harmonization_group_id")

  write_csv_checked(
    lineage_pairs,
    file.path(dirs$final_dir, "district_harmonization_crosswalk.csv")
  )

  diagnostics <- group_summary |>
    dplyr::count(relationship_type, change_comparable, name = "n_harmonization_groups")
  write_csv_checked(
    diagnostics,
    file.path(dirs$diagnostic_dir, "district_harmonization_diagnostics.csv")
  )

  district_overlap_review <- lineage_pairs |>
    dplyr::filter(relationship_type == "many_to_many") |>
    dplyr::transmute(
      district_2001_uid = paste0(state_no_2001, "_", district_name_2001_norm),
      district_2011_uid = paste0(state_no_2011, "_", district_name_2011_norm),
      overlap_area_sqkm = NA_real_,
      overlap_share_of_2001 = NA_real_,
      overlap_share_of_2011 = NA_real_,
      sliver_flag = NA,
      substantive_link = change_comparable,
      manual_review = TRUE,
      review_note = paste0(
        "Lineage link comes from the reviewed crosswalk. Polygon overlap values are not ",
        "fabricated; populate them only after adding validated 2001 and 2011 district layers."
      )
    )
  write_csv_checked(
    district_overlap_review,
    file.path(dirs$diagnostic_dir, "district_overlap_review.csv")
  )

  list(
    pairs = lineage_pairs,
    groups = group_summary,
    overlap_review = district_overlap_review
  )
}

build_geography <- function(paths, dirs) {
  message("Building geographic references")

  purrr::walk(
    c(paths$ac_name_key, paths$ac_district_xwalk, paths$delhi_districts, paths$district_codes),
    assert_file_exists
  )
  assert_directory_exists(paths$ac_map_dir)

  district_lineage <- build_district_lineage(paths, dirs)

  district_codes_raw <- readxl::read_xlsx(paths$district_codes, col_types = "text")
  assert_has_columns(
    district_codes_raw,
    c("State Code", "District Code", "Sub District Code", "Town-Village Name"),
    "district code file"
  )

  district_reference <- district_codes_raw |>
    dplyr::mutate(
      state_no = as.integer(`State Code`),
      district_code_2011 = as.integer(`District Code`),
      subdistrict_code = stringr::str_pad(`Sub District Code`, 5, pad = "0")
    ) |>
    dplyr::filter(district_code_2011 != 0, subdistrict_code == "00000") |>
    dplyr::transmute(
      state_no,
      district_code_2011,
      district_name_2011 = stringr::str_squish(`Town-Village Name`),
      district_name_2011_norm = norm_name(`Town-Village Name`),
      district_join_key_name = district_name_key(`Town-Village Name`)
    ) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)

  assert_unique_rows(district_reference, c("state_no", "district_code_2011"), "district reference")

  assembly_files <- list.files(
    paths$ac_map_dir,
    pattern = "\\.assembly\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(assembly_files) == 0) stop("No assembly shapefiles found in ", paths$ac_map_dir)

  ac_map_clean <- purrr::map(assembly_files, sf::read_sf, quiet = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      state_no = state_name_to_no(state),
      pc = as.integer(pc),
      ac = as.integer(ac)
    ) |>
    dplyr::filter(!is.na(state_no), !is.na(ac), ac > 0) |>
    sf::st_make_valid()

  assert_unique_rows(sf::st_drop_geometry(ac_map_clean), c("state_no", "ac"), "AC map")

  ac_name_key <- readr::read_csv(paths$ac_name_key, show_col_types = FALSE) |>
    dplyr::transmute(
      state_no = as.integer(stringr::str_match(ac08_id, "^\\d{4}-(\\d{2})-\\d{3}$")[, 2]),
      ac = as.integer(stringr::str_extract(ac08_id, "\\d{3}$")),
      ac_name_key = ac08_name,
      district_code_key = as.integer(pc01_district_id),
      district_name_key_value = pc01_district_name,
      district_join_key_name_key = district_name_key(pc01_district_name)
    ) |>
    dplyr::distinct(state_no, ac, .keep_all = TRUE)

  delhi_manual <- tibble::tribble(
    ~state_no, ~ac, ~district_name_manual,
    7, 63, "North East", 7, 64, "North East", 7, 67, "North East", 7, 68, "North East",
    7, 41, "South", 7, 54, "South", 7, 61, "East", 7, 62, "East",
    7, 42, "South", 7, 50, "South", 7, 49, "South", 7, 51, "South",
    7, 52, "South", 7, 53, "South"
  )

  manual_key <- tibble::tribble(
    ~state_no, ~ac, ~district_manual,
    23, 205, "Indore", 23, 206, "Indore", 23, 207, "Indore", 23, 208, "Indore",
    24, 45, "Ahmadabad", 24, 46, "Ahmadabad", 24, 47, "Ahmadabad", 24, 48, "Ahmadabad",
    24, 49, "Ahmadabad", 24, 50, "Ahmadabad", 24, 51, "Ahmadabad", 24, 52, "Ahmadabad",
    24, 53, "Ahmadabad", 24, 54, "Ahmadabad", 24, 55, "Ahmadabad", 24, 56, "Ahmadabad",
    24, 161, "Surat", 24, 162, "Surat", 24, 163, "Surat", 24, 164, "Surat",
    24, 165, "Surat", 24, 166, "Surat", 24, 167, "Surat",
    18, 33, "Bongaigaon", 18, 42, "Barpeta", 20, 57, "Saraikela Kharsawan",
    20, 59, "Ranchi", 20, 70, "Simdega"
  ) |>
    dplyr::mutate(district_join_key_name_manual = district_name_key(district_manual))

  # Spatial Delhi assignment, with the existing hand-coded fallback retained.
  delhi_districts <- sf::read_sf(paths$delhi_districts, quiet = TRUE) |>
    janitor::clean_names() |>
    sf::st_make_valid()
  delhi_name_col <- names(delhi_districts)[stringr::str_detect(names(delhi_districts), "district|dist|name")][1]
  if (is.na(delhi_name_col)) stop("Could not identify the Delhi district-name column")

  norm_delhi <- function(x) {
    norm_name(x) |>
      stringr::str_remove("\\s+DISTRICT$") |>
      stringr::str_remove("\\s+DELHI$") |>
      stringr::str_remove("^DELHI\\s+")
  }

  delhi_codes <- district_reference |>
    dplyr::filter(state_no == 7) |>
    dplyr::transmute(
      district_name_norm = norm_delhi(district_name_2011),
      district_code_2011,
      district_name_2011
    )

  delhi_polygons <- delhi_districts |>
    dplyr::mutate(district_name_norm = norm_delhi(.data[[delhi_name_col]])) |>
    dplyr::left_join(delhi_codes, by = "district_name_norm")

  delhi_spatial <- suppressWarnings(
    ac_map_clean |>
      dplyr::filter(state_no == 7) |>
      dplyr::select(state_no, ac, geometry) |>
      sf::st_transform(6933) |>
      sf::st_intersection(
        delhi_polygons |>
          dplyr::select(district_code_2011, district_name_2011, geometry) |>
          sf::st_transform(6933)
      )
  ) |>
    dplyr::mutate(overlap_area = as.numeric(sf::st_area(geometry))) |>
    sf::st_drop_geometry() |>
    dplyr::slice_max(overlap_area, by = c(state_no, ac), n = 1, with_ties = FALSE)

  delhi_fallback <- delhi_manual |>
    dplyr::mutate(district_name_norm = norm_delhi(district_name_manual)) |>
    dplyr::left_join(delhi_codes, by = "district_name_norm")

  xwalk_raw <- sf::read_sf(paths$ac_district_xwalk, quiet = TRUE)
  assert_has_columns(xwalk_raw, c("ST_CODE", "PC_NO", "AC_NO", "DIST_NAME", "DT_CODE"), "AC-district crosswalk")

  ac_dist_raw <- xwalk_raw |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      state_no = as.integer(ST_CODE),
      xwalk_pc = as.integer(PC_NO),
      ac = as.integer(AC_NO),
      district = as.character(DIST_NAME),
      district_code_xwalk = as.integer(DT_CODE),
      district_join_key_name = district_name_key(DIST_NAME),
      manual_xwalk = FALSE
    ) |>
    dplyr::filter(!is.na(ac), ac > 0) |>
    dplyr::left_join(
      delhi_spatial |>
        dplyr::select(state_no, ac, district_code_2011_spatial = district_code_2011, district_name_2011_spatial = district_name_2011),
      by = c("state_no", "ac")
    ) |>
    dplyr::left_join(
      delhi_fallback |>
        dplyr::select(state_no, ac, district_code_2011_manual = district_code_2011, district_name_2011_manual = district_name_2011),
      by = c("state_no", "ac")
    ) |>
    dplyr::left_join(manual_key, by = c("state_no", "ac")) |>
    dplyr::mutate(
      district = dplyr::case_when(
        !is.na(district_manual) ~ district_manual,
        state_no == 7 & !is.na(district_name_2011_spatial) ~ district_name_2011_spatial,
        state_no == 7 & !is.na(district_name_2011_manual) ~ district_name_2011_manual,
        TRUE ~ district
      ),
      district_code_xwalk = dplyr::coalesce(
        dplyr::if_else(state_no == 7, district_code_2011_spatial, NA_integer_),
        dplyr::if_else(state_no == 7, district_code_2011_manual, NA_integer_),
        district_code_xwalk
      ),
      district_join_key_name = dplyr::coalesce(
        district_join_key_name_manual,
        district_name_key(district)
      ),
      manual_xwalk = !is.na(district_manual) |
        (state_no == 7 & is.na(district_name_2011_spatial) & !is.na(district_name_2011_manual))
    ) |>
    dplyr::select(state_no, xwalk_pc, ac, district, district_code_xwalk, district_join_key_name, manual_xwalk)

  ambiguous <- ac_dist_raw |>
    dplyr::summarise(n_districts = dplyr::n_distinct(district_join_key_name), .by = c(state_no, ac)) |>
    dplyr::filter(n_districts > 1)

  ac_dist <- ac_dist_raw |>
    dplyr::left_join(ambiguous |>
      dplyr::mutate(ambiguous_ac = TRUE), by = c("state_no", "ac")) |>
    dplyr::left_join(ac_name_key, by = c("state_no", "ac")) |>
    dplyr::mutate(
      ambiguous_ac = dplyr::coalesce(ambiguous_ac, FALSE),
      matches_key = district_join_key_name == district_join_key_name_key
    ) |>
    dplyr::group_by(state_no, ac) |>
    dplyr::mutate(use_key = ambiguous_ac & any(matches_key, na.rm = TRUE)) |>
    dplyr::filter(!use_key | matches_key) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      district = dplyr::if_else(use_key, district_name_key_value, district),
      district_code_xwalk = dplyr::if_else(use_key, district_code_key, district_code_xwalk),
      district_join_key_name = dplyr::if_else(use_key, district_join_key_name_key, district_join_key_name)
    ) |>
    dplyr::distinct(state_no, ac, .keep_all = TRUE)

  manual_missing <- manual_key |>
    dplyr::anti_join(ac_dist, by = c("state_no", "ac")) |>
    dplyr::transmute(
      state_no,
      xwalk_pc = NA_integer_,
      ac,
      district = district_manual,
      district_code_xwalk = NA_integer_,
      district_join_key_name = district_join_key_name_manual,
      manual_xwalk = TRUE
    )

  lineage_source_name <- function(x) {
    x_norm <- norm_name(x)

    dplyr::recode(
      x_norm,
      "PURBA MEDINAPUR" = "PURBA MEDINIPUR",
      "PASCHIM MEDINAPUR" = "PASCHIM MEDINIPUR",
      "NORTH 24 PARGANAS" = "NORTH TWENTY FOUR PARGANAS",
      "SOUTH 24 PARGANAS" = "SOUTH TWENTY FOUR PARGANAS",
      "SARAIKELA" = "SARAIKELA KHARSAWAN",
      .default = x_norm
    )
  }

  historical_lineage_name_lookup <- dplyr::bind_rows(
    district_lineage$pairs |>
      dplyr::transmute(
        state_no = state_no_2001,
        district_source_norm = district_name_2001_norm,
        district_harmonization_group_id
      ),
    district_lineage$pairs |>
      dplyr::transmute(
        state_no = state_no_2011,
        district_source_norm = district_name_2011_norm,
        district_harmonization_group_id
      )
  ) |>
    dplyr::filter(
      !is.na(state_no),
      !is.na(district_source_norm),
      district_source_norm != "",
      !is.na(district_harmonization_group_id)
    ) |>
    dplyr::distinct() |>
    dplyr::add_count(
      state_no,
      district_source_norm,
      name = "n_lineage_groups"
    ) |>
    dplyr::filter(n_lineage_groups == 1L) |>
    dplyr::select(
      state_no,
      district_source_norm,
      district_harmonization_group_id
    ) |>
    dplyr::left_join(
      district_lineage$groups |>
        dplyr::select(
          district_harmonization_group_id,
          relationship_type,
          change_comparable
        ),
      by = "district_harmonization_group_id",
      relationship = "many-to-one"
    )

  ac_lineage_fallback <- dplyr::bind_rows(
    ac_dist,
    manual_missing
  ) |>
    dplyr::mutate(
      lineage_fallback_allowed =
        dplyr::coalesce(manual_xwalk, FALSE) |
        !dplyr::coalesce(ambiguous_ac, FALSE) |
        dplyr::coalesce(use_key, FALSE)
    ) |>
    dplyr::filter(lineage_fallback_allowed) |>
    dplyr::transmute(
      state_no,
      ac,
      district_source_name = district,
      district_source_norm_original = norm_name(district),
      district_source_norm = lineage_source_name(district),
      district_lineage_alias_used =
        district_source_norm_original != district_source_norm
    ) |>
    dplyr::left_join(
      historical_lineage_name_lookup |>
        dplyr::rename(
          district_harmonization_group_id_fallback =
            district_harmonization_group_id,
          relationship_type_fallback =
            relationship_type,
          change_comparable_fallback =
            change_comparable
        ),
      by = c(
        "state_no",
        "district_source_norm"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::select(
      state_no,
      ac,
      district_source_name,
      district_lineage_alias_used,
      district_harmonization_group_id_fallback,
      relationship_type_fallback,
      change_comparable_fallback
    )

  ac_reference <- dplyr::bind_rows(ac_dist, manual_missing) |>
    dplyr::left_join(
      district_reference,
      by = c("state_no", "district_join_key_name"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      ac_map_clean |>
        sf::st_drop_geometry() |>
        dplyr::transmute(state_no, ac, map_pc = pc, map_state = state),
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(state_lookup, by = "state_no", relationship = "many-to-one") |>
    dplyr::mutate(
      state = dplyr::coalesce(state, stringr::str_replace_all(map_state, "_", " ")),
      pc = dplyr::coalesce(map_pc, xwalk_pc),
      ac_uid = make_ac_uid(state_no, ac),
      pc_cluster_id = make_pc_uid(state_no, pc),
      district_name_2011_norm = norm_name(district_name_2011)
    ) |>
    dplyr::left_join(
      district_lineage$pairs |>
        dplyr::distinct(
          state_no_2011,
          district_name_2011_norm,
          district_harmonization_group_id,
          relationship_type,
          change_comparable
        ),
      by = c("state_no" = "state_no_2011", "district_name_2011_norm")
    ) |>
    dplyr::left_join(
      ac_lineage_fallback,
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      district_lineage_source = dplyr::case_when(
        !is.na(district_harmonization_group_id) ~
          "2011_district_exact",
        is.na(district_harmonization_group_id) &
          !is.na(district_harmonization_group_id_fallback) ~
          "historical_lineage_exact",
        TRUE ~
          "unresolved"
      ),
      district_lineage_alias_used = dplyr::if_else(
        district_lineage_source == "historical_lineage_exact",
        dplyr::coalesce(district_lineage_alias_used, FALSE),
        FALSE
      ),
      district_harmonization_group_id = dplyr::coalesce(
        district_harmonization_group_id,
        district_harmonization_group_id_fallback
      ),
      relationship_type = dplyr::coalesce(
        relationship_type,
        relationship_type_fallback
      ),
      change_comparable = dplyr::coalesce(
        change_comparable,
        change_comparable_fallback
      )
    ) |>
    dplyr::transmute(
      state,
      state_no,
      pc,
      ac,
      ac_uid,
      pc_cluster_id,
      district_code_2011,
      district_name_2011,
      district_harmonization_group_id,
      district_relationship_type = relationship_type,
      district_change_comparable = change_comparable,
      district_lineage_source,
      district_lineage_alias_used,
      manual_xwalk,
      district_join_success = !is.na(district_code_2011)
    ) |>
    dplyr::distinct(state_no, ac, .keep_all = TRUE)

  assert_unique_rows(ac_reference, c("state_no", "ac"), "AC reference")

  ac_reference_sf <- ac_map_clean |>
    dplyr::select(state_no, ac, geometry) |>
    dplyr::left_join(
      ac_reference,
      by = c("state_no", "ac"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      ac_uid = dplyr::coalesce(
        ac_uid,
        make_ac_uid(state_no, ac)
      )
    )

  touches <- sf::st_touches(ac_reference_sf)
  ac_neighbor_pairs <- tibble::tibble(
    focal_row = seq_len(nrow(ac_reference_sf)),
    neighbor_row = touches
  ) |>
    tidyr::unnest_longer(neighbor_row) |>
    dplyr::transmute(
      ac_uid = ac_reference_sf$ac_uid[focal_row],
      state_no = ac_reference_sf$state_no[focal_row],
      ac = ac_reference_sf$ac[focal_row],
      neighbor_ac_uid = ac_reference_sf$ac_uid[neighbor_row],
      neighbor_state_no = ac_reference_sf$state_no[neighbor_row],
      neighbor_ac = ac_reference_sf$ac[neighbor_row]
    ) |>
    dplyr::filter(ac_uid != neighbor_ac_uid) |>
    dplyr::distinct()

  geography_diagnostics <- dplyr::bind_rows(
    tibble::tibble(
      check = c(
        "AC rows", "missing district matches", "manual AC-district matches",
        "ACs with no touching neighbors", "district lineage unmatched"
      ),
      n = c(
        nrow(ac_reference),
        sum(!ac_reference$district_join_success),
        sum(ac_reference$manual_xwalk, na.rm = TRUE),
        sum(!ac_reference$ac_uid %in% ac_neighbor_pairs$ac_uid),
        sum(is.na(ac_reference$district_harmonization_group_id))
      ),
      passed = c(TRUE, NA, NA, NA, NA),
      details = NA_character_
    )
  )

  write_csv_checked(ac_reference, file.path(dirs$intermediate_dir, "ac_reference.csv"), c("state_no", "ac"))
  saveRDS(ac_reference_sf, file.path(dirs$intermediate_dir, "ac_reference_sf.rds"))
  write_csv_checked(ac_neighbor_pairs, file.path(dirs$intermediate_dir, "ac_neighbor_pairs.csv"), c("ac_uid", "neighbor_ac_uid"))
  write_csv_checked(geography_diagnostics, file.path(dirs$diagnostic_dir, "geography_diagnostics.csv"))

  list(
    ac_reference = ac_reference,
    ac_reference_sf = ac_reference_sf,
    ac_neighbor_pairs = ac_neighbor_pairs,
    district_reference = district_reference,
    district_lineage = district_lineage
  )
}

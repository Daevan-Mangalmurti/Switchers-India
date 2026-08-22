# Population, employment, migration, religion, language, SC/ST education,
# and work-migrant composition.

allocate_district_counts <- function(district_data, allocation_weights, value_columns) {
  assert_unique_rows(
    district_data,
    c("state_no", "district_code_2011"),
    "district data supplied for AC allocation"
  )
  
  allocation_weights |>
    dplyr::select(state_no, ac, ac_uid, district_code_2011, proxy_ac_pop, ac_alloc_share) |>
    dplyr::left_join(
      district_data,
      by = c("state_no", "district_code_2011"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(value_columns),
        ~ dplyr::if_else(!is.na(.x) & !is.na(ac_alloc_share), .x * ac_alloc_share, NA_real_),
        .names = "{.col}_ac"
      )
    ) |>
    dplyr::select(state_no, ac, ac_uid, proxy_ac_pop, dplyr::all_of(paste0(value_columns, "_ac")))
}

clean_pop_change_file <- function(path) {
  read_excel_raw(path) |>
    dplyr::select(1:9) |>
    rlang::set_names(c(
      "state_code", "district_code", "district_name", "census_year", "persons",
      "abs_change", "pct_change", "males", "females"
    )) |>
    dplyr::filter(stringr::str_detect(census_year, "^\\d{4}\\*?$")) |>
    tidyr::fill(state_code, district_code, district_name, .direction = "down") |>
    dplyr::filter(district_code != "000") |>
    dplyr::mutate(
      census_year = as.integer(stringr::str_remove(census_year, "\\*")),
      dplyr::across(c(persons, abs_change, pct_change, males, females), clean_num)
    )
}

extract_age_lower <- function(x) {
  suppressWarnings(
    as.numeric(
      stringr::str_extract(
        stringr::str_squish(as.character(x)),
        "^\\d+"
      )
    )
  )
}

clean_group_population_file <- function(path, group = c("sc", "st")) {
  group <- match.arg(group)
  keep_code <- if (group == "sc") "000" else "500"
  
  read_excel_raw(path) |>
    dplyr::select(2:10) |>
    rlang::set_names(c(
      "state_code", "district_code", "area_name", "group_code", "group_name",
      "area_type", "persons", "males", "females"
    )) |>
    dplyr::filter(
      area_type == "Total",
      district_code != "00",
      group_code == keep_code
    ) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(stringr::str_extract(area_name, "\\d{3}$")),
      district_name = area_name |>
        stringr::str_remove("^District\\s*-\\s*") |>
        stringr::str_remove("\\s+\\d{3}$") |>
        stringr::str_squish(),
      population = clean_num(persons)
    )
}

clean_c13_age_file <- function(path) {
  raw <- read_excel_raw(path)
  if (ncol(raw) < 9) stop("Unexpected C-13 structure: ", path)
  
  raw |>
    dplyr::select(1:9) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "area_name",
      "residence", "age", "persons", "males", "females"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      stringr::str_detect(district_code, "^\\d{3}$"),
      district_code != "000",
      residence == "Total"
    ) |>
    dplyr::mutate(
      age_numeric = extract_age_lower(age),
      persons = clean_num(persons)
    ) |>
    dplyr::filter(!is.na(age_numeric)) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(district_code),
      age = age_numeric,
      persons
    )
}

build_population_demographics <- function(paths, dirs, geography) {
  message("Building population, employment, and SC/ST controls")
  
  purrr::walk(
    c(paths$ac_population, paths$economic_census),
    assert_file_exists
  )
  purrr::walk(
    c(paths$population_change_dir, paths$sc_population_dir, paths$st_population_dir),
    assert_directory_exists
  )
  
  ac_population <- readr::read_csv(paths$ac_population, show_col_types = FALSE) |>
    dplyr::transmute(
      state_no = as.integer(stringr::str_match(ac08_id, "^\\d{4}-(\\d{2})-\\d{3}$")[, 2]),
      ac = as.integer(stringr::str_extract(ac08_id, "\\d{3}$")),
      con08_pop = clean_num(con08_pc01_pca_tot_p),
      con08_land_area = clean_num(con08_land_area)
    ) |>
    dplyr::distinct(state_no, ac, .keep_all = TRUE)
  
  district_pop <- read_many_excel(
    paths$population_change_dir,
    "\\.xlsx?$",
    clean_pop_change_file
  ) |>
    dplyr::filter(census_year == 2011) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(district_code),
      district_pop_2011 = persons
    ) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  
  allocation <- geography$ac_reference_sf |>
    dplyr::left_join(ac_population, by = c("state_no", "ac"), relationship = "one-to-one") |>
    dplyr::left_join(district_pop, by = c("state_no", "district_code_2011"), relationship = "many-to-one") |>
    dplyr::mutate(
      geom_land_area = as.numeric(sf::st_area(sf::st_transform(geometry, 6933))) / 1e6,
      con08_land_area = dplyr::coalesce(con08_land_area, geom_land_area),
      allocation_group = dplyr::coalesce(
        as.character(district_code_2011),
        paste0("UNMATCHED_", ac_uid)
      )
    ) |>
    dplyr::group_by(state_no, allocation_group) |>
    dplyr::mutate(
      n_ac_in_district = dplyr::n(),
      direct_available = !is.na(con08_pop) & con08_pop > 0,
      n_direct = sum(direct_available),
      n_missing_direct = sum(!direct_available),
      direct_sum = sum(con08_pop[direct_available], na.rm = TRUE),
      direct_inconsistent = !is.na(district_pop_2011) & direct_sum > 1.05 * district_pop_2011,
      remaining_population = dplyr::if_else(
        !is.na(district_pop_2011),
        pmax(district_pop_2011 - direct_sum, 0),
        NA_real_
      ),
      proxy_ac_pop = dplyr::case_when(
        direct_inconsistent & !is.na(district_pop_2011) ~ district_pop_2011 / n_ac_in_district,
        direct_available ~ con08_pop,
        !is.na(district_pop_2011) & n_direct == 0 ~ district_pop_2011 / n_ac_in_district,
        !is.na(district_pop_2011) & n_direct > 0 & n_missing_direct > 0 ~ remaining_population / n_missing_direct,
        TRUE ~ NA_real_
      ),
      proxy_ac_pop_source = dplyr::case_when(
        direct_inconsistent & !is.na(district_pop_2011) ~ "equal_allocation_inconsistent_direct_population",
        direct_available ~ "direct_ac_population",
        !is.na(district_pop_2011) & n_direct == 0 ~ "equal_allocation_no_direct_population",
        !is.na(district_pop_2011) & n_direct > 0 & n_missing_direct > 0 ~ "remaining_population_equal_allocation",
        TRUE ~ "missing"
      ),
      proxy_sum = sum(proxy_ac_pop, na.rm = TRUE),
      all_proxy_available = all(!is.na(proxy_ac_pop)),
      ac_alloc_share = dplyr::if_else(all_proxy_available & proxy_sum > 0, proxy_ac_pop / proxy_sum, NA_real_)
    ) |>
    dplyr::ungroup() |>
    sf::st_drop_geometry() |>
    dplyr::select(
      state, state_no, pc, ac, ac_uid, district_code_2011, district_name_2011,
      district_harmonization_group_id, proxy_ac_pop, proxy_ac_pop_source,
      con08_land_area, ac_alloc_share, n_ac_in_district, direct_sum,
      direct_inconsistent
    )
  
  allocation_diagnostics <- allocation |>
    dplyr::filter(!is.na(district_code_2011)) |>
    dplyr::summarise(
      n_ac = dplyr::n(),
      share_sum = sum(ac_alloc_share, na.rm = TRUE),
      n_missing_share = sum(is.na(ac_alloc_share)),
      passed = n_missing_share == 0 & abs(share_sum - 1) < 1e-8,
      .by = c(state_no, district_code_2011)
    )
  
  employment_district <- readr::read_csv(paths$economic_census, show_col_types = FALSE) |>
    dplyr::transmute(
      state_no = as.integer(pc11_state_id),
      district_code_2011 = as.integer(pc11_district_id),
      employment_total_district = clean_num(ec13_emp_all),
      employment_manufacturing_district = clean_num(ec13_emp_manuf),
      employment_services_district = clean_num(ec13_emp_services)
    ) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  
  employment_ac <- allocate_district_counts(
    employment_district,
    allocation,
    c(
      "employment_total_district",
      "employment_manufacturing_district",
      "employment_services_district"
    )
  ) |>
    dplyr::rename(
      employment_total_ac = employment_total_district_ac,
      employment_manufacturing_ac = employment_manufacturing_district_ac,
      employment_services_ac = employment_services_district_ac
    )
  
  sc_district <- read_many_excel(
    paths$sc_population_dir,
    "-PCA-A10-APPENDIX\\.xlsx$",
    clean_group_population_file,
    group = "sc"
  ) |>
    dplyr::rename(sc_population_district = population) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  
  st_district <- read_many_excel(
    paths$st_population_dir,
    "-PCA-A11-APPENDIX\\.xlsx$",
    clean_group_population_file,
    group = "st"
  ) |>
    dplyr::rename(st_population_district = population) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  
  sc_ac <- allocate_district_counts(sc_district, allocation, "sc_population_district") |>
    dplyr::rename(sc_population_ac = sc_population_district_ac)
  st_ac <- allocate_district_counts(st_district, allocation, "st_population_district") |>
    dplyr::rename(st_population_ac = st_population_district_ac)
  
  # C-13 is optional because it is not currently in the pasted directory tree.
  age_available <- dir.exists(paths$age_2011_dir) &&
    length(list.files(paths$age_2011_dir, pattern = "C-13", ignore.case = TRUE)) > 0
  
  if (age_available) {
    age_district <- read_many_excel(
      paths$age_2011_dir,
      "C-13.*\\.xls[x]?$",
      clean_c13_age_file
    ) |>
      dplyr::summarise(
        population_15plus_district = sum(persons[age >= 15], na.rm = TRUE),
        population_15_64_district = sum(persons[age >= 15 & age <= 64], na.rm = TRUE),
        .by = c(state_no, district_code_2011)
      )
    
    age_ac <- allocate_district_counts(
      age_district,
      allocation,
      c("population_15plus_district", "population_15_64_district")
    ) |>
      dplyr::rename(
        working_age_population_15plus_ac = population_15plus_district_ac,
        working_age_population_15_64_ac = population_15_64_district_ac
      )
  } else {
    age_ac <- allocation |>
      dplyr::transmute(
        state_no, ac, ac_uid, proxy_ac_pop,
        working_age_population_15plus_ac = NA_real_,
        working_age_population_15_64_ac = NA_real_
      )
  }
  
  zero_st_states <- c(3L, 6L, 7L, 34L)
  zero_sc_states <- c(12L, 13L)
  
  demographics <- allocation |>
    dplyr::select(
      state, state_no, pc, ac, ac_uid, district_code_2011,
      district_name_2011, district_harmonization_group_id,
      proxy_ac_pop, proxy_ac_pop_source, con08_land_area
    ) |>
    dplyr::left_join(employment_ac |>
                       dplyr::select(-proxy_ac_pop), by = c("state_no", "ac", "ac_uid")) |>
    dplyr::left_join(sc_ac |>
                       dplyr::select(-proxy_ac_pop), by = c("state_no", "ac", "ac_uid")) |>
    dplyr::left_join(st_ac |>
                       dplyr::select(-proxy_ac_pop), by = c("state_no", "ac", "ac_uid")) |>
    dplyr::left_join(age_ac |>
                       dplyr::select(-proxy_ac_pop), by = c("state_no", "ac", "ac_uid")) |>
    dplyr::mutate(
      sc_population_ac = dplyr::if_else(
        state_no %in% zero_sc_states & is.na(sc_population_ac) & !is.na(proxy_ac_pop),
        0,
        sc_population_ac
      ),
      st_population_ac = dplyr::if_else(
        state_no %in% zero_st_states & is.na(st_population_ac) & !is.na(proxy_ac_pop),
        0,
        st_population_ac
      ),
      ac_pop_density_sqkm = safe_divide(proxy_ac_pop, con08_land_area),
      log_ac_pop_density_sqkm = safe_log(ac_pop_density_sqkm),
      employment_per_total_population = safe_divide(employment_total_ac, proxy_ac_pop),
      employment_per_population_15plus = safe_divide(employment_total_ac, working_age_population_15plus_ac),
      employment_per_population_15_64 = safe_divide(employment_total_ac, working_age_population_15_64_ac),
      sc_pop_share = safe_share(sc_population_ac, proxy_ac_pop),
      st_pop_share = safe_share(st_population_ac, proxy_ac_pop)
    )
  
  population_allocation_diagnostics <- dplyr::bind_rows(
    employment_district |>
      dplyr::select(state_no, district_code_2011, source_total = employment_total_district) |>
      dplyr::left_join(
        employment_ac |>
          dplyr::left_join(allocation |>
                             dplyr::select(state_no, ac, district_code_2011), by = c("state_no", "ac")) |>
          dplyr::summarise(allocated_ac_total = sum(employment_total_ac, na.rm = TRUE), .by = c(state_no, district_code_2011)),
        by = c("state_no", "district_code_2011")
      ) |>
      dplyr::mutate(source = "Economic Census employment"),
    sc_district |>
      dplyr::select(state_no, district_code_2011, source_total = sc_population_district) |>
      dplyr::left_join(
        sc_ac |>
          dplyr::left_join(allocation |>
                             dplyr::select(state_no, ac, district_code_2011), by = c("state_no", "ac")) |>
          dplyr::summarise(allocated_ac_total = sum(sc_population_ac, na.rm = TRUE), .by = c(state_no, district_code_2011)),
        by = c("state_no", "district_code_2011")
      ) |>
      dplyr::mutate(source = "SC population"),
    st_district |>
      dplyr::select(state_no, district_code_2011, source_total = st_population_district) |>
      dplyr::left_join(
        st_ac |>
          dplyr::left_join(allocation |>
                             dplyr::select(state_no, ac, district_code_2011), by = c("state_no", "ac")) |>
          dplyr::summarise(allocated_ac_total = sum(st_population_ac, na.rm = TRUE), .by = c(state_no, district_code_2011)),
        by = c("state_no", "district_code_2011")
      ) |>
      dplyr::mutate(source = "ST population")
  ) |>
    dplyr::mutate(
      difference = allocated_ac_total - source_total,
      relative_difference = safe_divide(difference, source_total),
      passed = is.na(source_total) | abs(difference) < 1e-6
    )
  
  write_csv_checked(allocation, file.path(dirs$intermediate_dir, "ac_allocation_weights.csv"), c("state_no", "ac"))
  write_csv_checked(demographics, file.path(dirs$intermediate_dir, "demographics_ac.csv"), c("state_no", "ac"))
  write_csv_checked(allocation_diagnostics, file.path(dirs$diagnostic_dir, "allocation_share_diagnostics.csv"))
  write_csv_checked(population_allocation_diagnostics, file.path(dirs$diagnostic_dir, "population_allocation_diagnostics.csv"))
  write_csv_checked(
    tibble::tibble(
      check = "C-13 working-age source available",
      n = as.integer(age_available),
      denominator = 1L,
      pct = 100 * as.integer(age_available),
      passed = age_available,
      details = if (age_available) {
        "Working-age employment-intensity measures constructed"
      } else {
        paste0(
          "No C-13 files found in ", paths$age_2011_dir,
          "; employment_per_population_15plus and _15_64 are NA"
        )
      }
    ),
    file.path(dirs$diagnostic_dir, "working_age_population_diagnostics.csv")
  )
  
  list(
    allocation = allocation,
    demographics = demographics,
    employment_district = employment_district,
    sc_district = sc_district,
    st_district = st_district,
    age_available = age_available
  )
}

migration_source_columns <- c(
  "state_code", "district_code", "area_name", "last_residence",
  "last_residence_type", "place_of_enumeration",
  "total_persons", "total_males", "total_females",
  "persons_lt1", "males_lt1", "females_lt1",
  "persons_1_4", "males_1_4", "females_1_4",
  "persons_5_9", "males_5_9", "females_5_9",
  "persons_10_19", "males_10_19", "females_10_19",
  "persons_gt20", "males_gt20", "females_gt20",
  "persons_unknown", "males_unknown", "females_unknown"
)

clean_migration_file <- function(path) {
  read_excel_raw(path) |>
    dplyr::select(2:28) |>
    rlang::set_names(migration_source_columns) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      stringr::str_detect(district_code, "^\\d{3}$"),
      district_code != "000"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::matches("persons|males|females"), clean_num))
}

build_migration <- function(paths, dirs, geography, population) {
  message("Building migration exposures")
  assert_directory_exists(paths$migration_dir)
  
  source_rows <- read_many_excel(paths$migration_dir, "\\.xlsx?$", clean_migration_file) |>
    dplyr::mutate(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(district_code),
      last_residence_norm = norm_name(last_residence),
      migration_origin = dplyr::case_when(
        stringr::str_detect(last_residence_norm, "BEYOND THE STATE OF ENUMERATION") ~ "interstate",
        stringr::str_detect(last_residence_norm, "OUTSIDE INDIA|BEYOND INDIA|OTHER COUNTRIES") ~ "international",
        TRUE ~ "excluded"
      )
    )
  
  excluded_origins <- source_rows |>
    dplyr::filter(migration_origin == "excluded") |>
    dplyr::count(last_residence, sort = TRUE, name = "n_rows")
  
  district <- source_rows |>
    dplyr::filter(
      migration_origin %in% c("interstate", "international"),
      last_residence_type == "Total",
      place_of_enumeration == "Total"
    ) |>
    dplyr::summarise(
      mig_total_district = sum_or_na(total_persons),
      male_mig_total_district = sum_or_na(total_males),
      mig_lt1_district = sum_or_na(persons_lt1),
      male_mig_lt1_district = sum_or_na(males_lt1),
      mig_1_4_district = sum_or_na(persons_1_4),
      male_mig_1_4_district = sum_or_na(males_1_4),
      mig_5_9_district = sum_or_na(persons_5_9),
      male_mig_5_9_district = sum_or_na(males_5_9),
      mig_10_19_district = sum_or_na(persons_10_19),
      male_mig_10_19_district = sum_or_na(males_10_19),
      mig_gt20_district = sum_or_na(persons_gt20),
      male_mig_gt20_district = sum_or_na(males_gt20),
      .by = c(state_no, district_code_2011)
    )
  
  interstate_district <- source_rows |>
    dplyr::filter(
      migration_origin == "interstate",
      last_residence_type == "Total",
      place_of_enumeration == "Total"
    ) |>
    dplyr::summarise(
      interstate_migrants_0_9_n_2011 = sum_or_na(persons_lt1 + persons_1_4 + persons_5_9),
      .by = c(state_no, district_code_2011)
    )
  
  values <- setdiff(names(district), c("state_no", "district_code_2011"))
  bins <- allocate_district_counts(district, population$allocation, values) |>
    dplyr::rename_with(~ stringr::str_remove(.x, "_district_ac$"), dplyr::ends_with("_district_ac"))
  
  bin_spec <- tibble::tribble(
    ~source_col, ~male_source_col, ~first_year, ~last_year, ~bin_years,
    "mig_10_19", "male_mig_10_19", 1992L, 2001L, 10,
    "mig_5_9", "male_mig_5_9", 2002L, 2006L, 5,
    "mig_1_4", "male_mig_1_4", 2007L, 2010L, 4,
    "mig_lt1", "male_mig_lt1", 2011L, 2011L, 1
  )
  
  annual_observed <- purrr::map_dfr(seq_len(nrow(bin_spec)), function(i) {
    spec <- bin_spec[i, ]
    years <- seq(spec$first_year, spec$last_year)
    bins |>
      dplyr::transmute(
        state_no, ac, ac_uid, proxy_ac_pop,
        persons_total = .data[[spec$source_col]],
        males_total = .data[[spec$male_source_col]]
      ) |>
      tidyr::crossing(migration_year = years) |>
      dplyr::mutate(
        annual_migrants_observed = persons_total / spec$bin_years,
        annual_male_migrants_observed = males_total / spec$bin_years,
        migration_bin = spec$source_col
      )
  })
  
  annual <- bins |>
    dplyr::distinct(state_no, ac, ac_uid, proxy_ac_pop) |>
    tidyr::crossing(migration_year = 1992L:2013L) |>
    dplyr::left_join(
      annual_observed |>
        dplyr::select(
          state_no, ac, ac_uid, migration_year, migration_bin,
          annual_migrants_observed, annual_male_migrants_observed
        ),
      by = c("state_no", "ac", "ac_uid", "migration_year"),
      relationship = "one-to-one"
    ) |>
    dplyr::group_by(state_no, ac, ac_uid) |>
    dplyr::mutate(
      imputation_mean_2009_2011 = safe_mean(annual_migrants_observed[migration_year %in% 2009:2011]),
      male_imputation_mean_2009_2011 = safe_mean(annual_male_migrants_observed[migration_year %in% 2009:2011]),
      annual_migrants = dplyr::case_when(
        migration_year <= 2011 ~ annual_migrants_observed,
        migration_year %in% 2012:2013 ~ imputation_mean_2009_2011,
        TRUE ~ NA_real_
      ),
      annual_male_migrants = dplyr::case_when(
        migration_year <= 2011 ~ annual_male_migrants_observed,
        migration_year %in% 2012:2013 ~ male_imputation_mean_2009_2011,
        TRUE ~ NA_real_
      ),
      migration_imputed = migration_year %in% 2012:2013 & !is.na(annual_migrants),
      male_migration_imputed = migration_year %in% 2012:2013 & !is.na(annual_male_migrants)
    ) |>
    dplyr::ungroup()
  
  windows <- tibble::tribble(
    ~year, ~period, ~window_start, ~window_end,
    2009L, "recent", 2004L, 2008L,
    2009L, "prior", 1999L, 2003L,
    2009L, "baseline", 1994L, 1998L,
    2014L, "recent", 2009L, 2013L,
    2014L, "prior", 2004L, 2008L,
    2014L, "baseline", 1999L, 2003L
  )
  
  totals <- bins |>
    dplyr::distinct(state_no, ac, ac_uid, proxy_ac_pop) |>
    tidyr::crossing(windows) |>
    dplyr::mutate(migration_year = purrr::map2(window_start, window_end, seq)) |>
    tidyr::unnest_longer(migration_year) |>
    dplyr::left_join(
      annual |>
        dplyr::select(state_no, ac, ac_uid, migration_year, annual_migrants, annual_male_migrants),
      by = c("state_no", "ac", "ac_uid", "migration_year"),
      relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      migration_total = dplyr::if_else(all(!is.na(annual_migrants)), sum(annual_migrants), NA_real_),
      male_migration_total = dplyr::if_else(all(!is.na(annual_male_migrants)), sum(annual_male_migrants), NA_real_),
      .by = c(state_no, ac, ac_uid, proxy_ac_pop, year, period)
    ) |>
    tidyr::pivot_wider(
      names_from = period,
      values_from = c(migration_total, male_migration_total),
      names_glue = "{.value}_{period}"
    ) |>
    dplyr::rename(
      mig_recent_5yr_total = migration_total_recent,
      mig_prior_5yr_total = migration_total_prior,
      mig_baseline_5yr_total = migration_total_baseline,
      male_mig_recent_5yr_total = male_migration_total_recent,
      male_mig_prior_5yr_total = male_migration_total_prior,
      male_mig_baseline_5yr_total = male_migration_total_baseline
    )
  
  base <- totals |>
    dplyr::left_join(
      bins |>
        dplyr::transmute(
          state_no, ac, ac_uid,
          mig_total_upto_2001 = mig_10_19 + mig_gt20,
          male_mig_total_upto_2001 = male_mig_10_19 + male_mig_gt20
        ),
      by = c("state_no", "ac", "ac_uid"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      population$demographics |>
        dplyr::select(ac_uid, con08_land_area),
      by = "ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      mig_prior_5_15yr_total = mig_prior_5yr_total + mig_baseline_5yr_total,
      male_mig_prior_5_15yr_total = male_mig_prior_5yr_total + male_mig_baseline_5yr_total,
      
      dplyr::across(
        c(
          mig_recent_5yr_total, mig_prior_5yr_total, mig_prior_5_15yr_total,
          mig_baseline_5yr_total, mig_total_upto_2001,
          male_mig_recent_5yr_total, male_mig_prior_5yr_total,
          male_mig_prior_5_15yr_total, male_mig_baseline_5yr_total,
          male_mig_total_upto_2001
        ),
        log1p,
        .names = "log1p_{.col}"
      ),
      
      mig_recent_5yr_share_ac_pop = safe_share(mig_recent_5yr_total, proxy_ac_pop),
      mig_prior_5yr_share_ac_pop = safe_share(mig_prior_5yr_total, proxy_ac_pop),
      mig_prior_5_15yr_share_ac_pop = safe_share(mig_prior_5_15yr_total, proxy_ac_pop),
      mig_baseline_5yr_share_ac_pop = safe_share(mig_baseline_5yr_total, proxy_ac_pop),
      mig_total_upto_2001_share_ac_pop = safe_share(mig_total_upto_2001, proxy_ac_pop),
      
      male_mig_recent_5yr_share_ac_pop = safe_share(male_mig_recent_5yr_total, proxy_ac_pop),
      male_mig_prior_5yr_share_ac_pop = safe_share(male_mig_prior_5yr_total, proxy_ac_pop),
      male_mig_prior_5_15yr_share_ac_pop = safe_share(male_mig_prior_5_15yr_total, proxy_ac_pop),
      male_mig_baseline_5yr_share_ac_pop = safe_share(male_mig_baseline_5yr_total, proxy_ac_pop),
      male_mig_total_upto_2001_share_ac_pop = safe_share(male_mig_total_upto_2001, proxy_ac_pop),
      
      dplyr::across(
        c(
          mig_recent_5yr_share_ac_pop, mig_prior_5yr_share_ac_pop,
          mig_prior_5_15yr_share_ac_pop, mig_baseline_5yr_share_ac_pop,
          mig_total_upto_2001_share_ac_pop
        ),
        safe_log,
        .names = "log_{.col}"
      ),
      
      mig_recent_5yr_density_sqkm = safe_divide(mig_recent_5yr_total, con08_land_area),
      mig_prior_5yr_density_sqkm = safe_divide(mig_prior_5yr_total, con08_land_area),
      mig_prior_5_15yr_density_sqkm = safe_divide(mig_prior_5_15yr_total, con08_land_area),
      mig_baseline_5yr_density_sqkm = safe_divide(mig_baseline_5yr_total, con08_land_area),
      mig_total_upto_2001_density_sqkm = safe_divide(mig_total_upto_2001, con08_land_area),
      male_mig_total_upto_2001_density_sqkm = safe_divide(male_mig_total_upto_2001, con08_land_area),
      
      dplyr::across(
        c(
          mig_recent_5yr_density_sqkm, mig_prior_5yr_density_sqkm,
          mig_prior_5_15yr_density_sqkm, mig_baseline_5yr_density_sqkm,
          mig_total_upto_2001_density_sqkm
        ),
        log1p,
        .names = "log1p_{.col}"
      ),
      
      mig_recent_5yr_male_share = safe_share(male_mig_recent_5yr_total, mig_recent_5yr_total),
      mig_prior_5yr_male_share = safe_share(male_mig_prior_5yr_total, mig_prior_5yr_total),
      mig_prior_5_15yr_male_share = safe_share(male_mig_prior_5_15yr_total, mig_prior_5_15yr_total),
      mig_baseline_5yr_male_share = safe_share(male_mig_baseline_5yr_total, mig_baseline_5yr_total),
      mig_total_upto_2001_male_share = safe_share(male_mig_total_upto_2001, mig_total_upto_2001),
      
      mig_recent_5yr_over_total_upto_2001 = safe_ratio(mig_recent_5yr_total, mig_total_upto_2001),
      mig_prior_5yr_over_total_upto_2001 = safe_ratio(mig_prior_5yr_total, mig_total_upto_2001),
      male_mig_recent_5yr_over_total_upto_2001 = safe_ratio(male_mig_recent_5yr_total, male_mig_total_upto_2001),
      male_mig_prior_5yr_over_total_upto_2001 = safe_ratio(male_mig_prior_5yr_total, male_mig_total_upto_2001),
      
      mig_accel_recent_vs_prior5_ratio = safe_ratio(mig_recent_5yr_total, mig_prior_5yr_total),
      mig_accel_recent_vs_prior5_pct_change = safe_pct_change(mig_recent_5yr_total, mig_prior_5yr_total),
      mig_accel_recent_vs_prior5_log = safe_log_ratio(mig_recent_5yr_total, mig_prior_5yr_total),
      mig_accel_recent_vs_prior5_log1p = log1p(mig_recent_5yr_total) - log1p(mig_prior_5yr_total),
      
      mig_accel_prior5_vs_baseline5_ratio = safe_ratio(mig_prior_5yr_total, mig_baseline_5yr_total),
      mig_accel_prior5_vs_baseline5_pct_change = safe_pct_change(mig_prior_5yr_total, mig_baseline_5yr_total),
      mig_accel_prior5_vs_baseline5_log = safe_log_ratio(mig_prior_5yr_total, mig_baseline_5yr_total),
      mig_accel_prior5_vs_baseline5_log1p = log1p(mig_prior_5yr_total) - log1p(mig_baseline_5yr_total),
      
      mig_accel_recent5_vs_prior10_annual_ratio = safe_ratio(
        mig_recent_5yr_total / 5,
        mig_prior_5_15yr_total / 10
      ),
      mig_accel_recent5_vs_prior10_annual_pct_change = safe_pct_change(
        mig_recent_5yr_total / 5,
        mig_prior_5_15yr_total / 10
      ),
      mig_accel_recent5_vs_prior10_annual_log = safe_log_ratio(
        mig_recent_5yr_total / 5,
        mig_prior_5_15yr_total / 10
      )
    )
  
  neighbor <- geography$ac_neighbor_pairs |>
    dplyr::left_join(
      base |>
        dplyr::transmute(
          neighbor_ac_uid = ac_uid,
          year,
          neighbor_recent_share = mig_recent_5yr_share_ac_pop,
          neighbor_prior_share = mig_prior_5yr_share_ac_pop,
          neighbor_acceleration = mig_accel_recent_vs_prior5_log
        ),
      by = "neighbor_ac_uid",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      mig_neighbor_n = dplyr::n_distinct(neighbor_ac_uid),
      mig_neighbor_recent_5yr_share_ac_pop = safe_mean(neighbor_recent_share),
      mig_neighbor_prior_5yr_share_ac_pop = safe_mean(neighbor_prior_share),
      mig_neighbor_accel_recent_vs_prior5_log = safe_mean(neighbor_acceleration),
      .by = c(ac_uid, year)
    )
  
  ac_year <- base |>
    dplyr::left_join(neighbor, by = c("ac_uid", "year"), relationship = "one-to-one") |>
    dplyr::mutate(
      mig_neighbor_n = tidyr::replace_na(mig_neighbor_n, 0L),
      mig_local_vs_neighbor_recent_share_log = safe_log_ratio(
        mig_recent_5yr_share_ac_pop,
        mig_neighbor_recent_5yr_share_ac_pop
      ),
      mig_local_minus_neighbor_acceleration =
        mig_accel_recent_vs_prior5_log - mig_neighbor_accel_recent_vs_prior5_log
    ) |>
    dplyr::arrange(state_no, ac, year)
  
  assert_unique_rows(ac_year, c("ac_uid", "year"), "migration AC-year data")
  
  imputation_diagnostics <- annual |>
    dplyr::filter(migration_year %in% 2009:2013) |>
    dplyr::transmute(
      state_no, ac, ac_uid, migration_year,
      annual_migrants_observed,
      annual_migrants_used = annual_migrants,
      imputed = migration_imputed,
      imputation_mean_2009_2011,
      annual_male_migrants_observed,
      annual_male_migrants_used = annual_male_migrants,
      male_imputed = male_migration_imputed,
      male_imputation_mean_2009_2011
    )
  
  migration_diagnostics <- dplyr::bind_rows(
    tibble::tibble(
      year = NA_integer_,
      variable = "excluded migration-origin labels",
      check = "Source labels excluded from interstate + international universe",
      n = nrow(excluded_origins),
      denominator = dplyr::n_distinct(source_rows$last_residence),
      pct = 100 * safe_divide(n, denominator),
      minimum = NA_real_, median = NA_real_, maximum = NA_real_,
      passed = NA,
      details = paste(excluded_origins$last_residence, collapse = "; ")
    ),
    ac_year |>
      dplyr::summarise(
        variable = "mig_recent_5yr_total",
        check = "Missing recent migration window",
        n = sum(is.na(mig_recent_5yr_total)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(mig_recent_5yr_total),
        median = safe_median(mig_recent_5yr_total),
        maximum = safe_max(mig_recent_5yr_total),
        passed = n == 0,
        details = NA_character_,
        .by = year
      ),
    ac_year |>
      dplyr::summarise(
        variable = "migration male shares",
        check = "Male share outside [0,1]",
        n = sum(
          dplyr::if_any(
            dplyr::ends_with("_male_share"),
            ~ !is.na(.x) & (.x < 0 | .x > 1)
          )
        ),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = NA_real_, median = NA_real_, maximum = NA_real_,
        passed = n == 0,
        details = NA_character_,
        .by = year
      )
  )
  
  write_csv_checked(district, file.path(dirs$intermediate_dir, "migration_district_clean.csv"), c("state_no", "district_code_2011"))
  write_csv_checked(bins, file.path(dirs$intermediate_dir, "migration_ac_bins.csv"), c("state_no", "ac"))
  write_csv_checked(annual, file.path(dirs$intermediate_dir, "migration_ac_annual.csv"), c("ac_uid", "migration_year"))
  saveRDS(annual, file.path(dirs$intermediate_dir, "migration_ac_annual.rds"))
  write_csv_checked(ac_year, file.path(dirs$intermediate_dir, "migration_ac_year.csv"), c("ac_uid", "year"))
  write_csv_checked(imputation_diagnostics, file.path(dirs$diagnostic_dir, "migration_imputation_diagnostics.csv"), c("ac_uid", "migration_year"))
  write_csv_checked(migration_diagnostics, file.path(dirs$diagnostic_dir, "migration_diagnostics.csv"))
  
  list(
    source_rows = source_rows,
    district = district,
    interstate_district = interstate_district,
    bins = bins,
    annual = annual,
    ac_year = ac_year,
    diagnostics = migration_diagnostics
  )
}

add_district_lineage <- function(data, year, district_name_col, lineage) {
  data <- data |>
    dplyr::mutate(district_name_norm_join = norm_name(.data[[district_name_col]]))
  
  if (year == 2001) {
    data |>
      dplyr::left_join(
        lineage$pairs |>
          dplyr::distinct(
            state_no = state_no_2001,
            district_name_norm_join = district_name_2001_norm,
            district_harmonization_group_id,
            district_relationship_type = relationship_type,
            district_change_comparable = change_comparable
          ),
        by = c("state_no", "district_name_norm_join"),
        relationship = "many-to-one"
      )
  } else {
    data |>
      dplyr::left_join(
        lineage$pairs |>
          dplyr::distinct(
            state_no = state_no_2011,
            district_name_norm_join = district_name_2011_norm,
            district_harmonization_group_id,
            district_relationship_type = relationship_type,
            district_change_comparable = change_comparable
          ),
        by = c("state_no", "district_name_norm_join"),
        relationship = "many-to-one"
      )
  }
}

clean_religion_file <- function(path, year) {
  raw <- read_excel_raw(path)
  raw |>
    dplyr::select(1:16) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "tehsil_code", "town_code",
      "area_name", "residence", "total_persons", "total_males", "total_females",
      "hindu_persons", "hindu_males", "hindu_females",
      "muslim_persons", "muslim_males", "muslim_females"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      district_code != if (year == 2001) "00" else "000",
      tehsil_code %in% c("0000", "00000"),
      town_code %in% c("00000000", "000000"),
      residence == "Total"
    ) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_source = district_code,
      district_name = area_name |>
        stringr::str_remove("^District\\s*-\\s*") |>
        stringr::str_squish(),
      total_population = clean_num(total_persons),
      hindu_population = clean_num(hindu_persons),
      muslim_population = clean_num(muslim_persons)
    )
}

read_all_excel_sheets <- function(path) {
  sheets <- readxl::excel_sheets(path)
  purrr::map_dfr(sheets, ~ read_excel_raw(path, sheet = .x))
}

clean_language_file <- function(path, year) {
  raw <- read_all_excel_sheets(path)
  raw |>
    dplyr::select(1:8) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "subdistrict_code",
      "area_name", "mother_tongue_code", "mother_tongue_name", "persons"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      district_code != if (year == 2001) "00" else "000",
      subdistrict_code %in% c("0000", "00000"),
      stringr::str_detect(mother_tongue_code, "^\\d{6}$")
    ) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_source = district_code,
      district_name = area_name |>
        stringr::str_remove("^District\\s*-\\s*") |>
        stringr::str_remove("\\s+\\d{2,3}$") |>
        stringr::str_squish(),
      mother_tongue_code = stringr::str_pad(mother_tongue_code, 6, pad = "0"),
      mother_tongue_name = stringr::str_squish(mother_tongue_name),
      persons = clean_num(persons)
    )
}

clean_c08_2011_file <- function(path, group) {
  raw <- read_excel_raw(path)
  raw |>
    dplyr::select(1:45) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "area_name", "residence", "age_group",
      "total_p", "total_m", "total_f", "illiterate_p", "illiterate_m", "illiterate_f",
      "literate_p", "literate_m", "literate_f", "no_level_p", "no_level_m", "no_level_f",
      "below_primary_p", "below_primary_m", "below_primary_f", "primary_p", "primary_m", "primary_f",
      "middle_p", "middle_m", "middle_f", "secondary_p", "secondary_m", "secondary_f",
      "higher_secondary_p", "higher_secondary_m", "higher_secondary_f",
      "nontechnical_diploma_p", "nontechnical_diploma_m", "nontechnical_diploma_f",
      "technical_diploma_p", "technical_diploma_m", "technical_diploma_f",
      "graduate_plus_p", "graduate_plus_m", "graduate_plus_f",
      "unclassified_p", "unclassified_m", "unclassified_f"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      district_code != "000",
      residence == "Total"
    ) |>
    dplyr::mutate(
      # C-08 reports single years through age 19 and grouped ages thereafter
      # (20-24, 25-29, ..., 80+). The lower bound identifies whether the
      # complete source group belongs in the approved 20+ or 25+ universe.
      age_lower = extract_age_lower(age_group),
      dplyr::across(dplyr::ends_with("_p"), clean_num)
    ) |>
    dplyr::filter(!is.na(age_lower)) |>
    dplyr::mutate(
      state_no = as.integer(state_code),
      district_code_source = district_code,
      district_name = area_name |>
        stringr::str_remove("^District\\s*-\\s*") |>
        stringr::str_remove("\\s+\\d{2,3}$") |>
        stringr::str_squish()
    ) |>
    dplyr::summarise(
      higher_secondary_plus_n_age7plus =
        sum_or_na(higher_secondary_p[age_lower >= 7]) +
        sum_or_na(nontechnical_diploma_p[age_lower >= 7]) +
        sum_or_na(technical_diploma_p[age_lower >= 7]) +
        sum_or_na(graduate_plus_p[age_lower >= 7]),
      population_age7plus = sum_or_na(total_p[age_lower >= 7]),
      graduate_plus_n_age7plus =
        sum_or_na(graduate_plus_p[age_lower >= 7]),
      higher_secondary_plus_n_age20plus =
        sum_or_na(higher_secondary_p[age_lower >= 20]) +
        sum_or_na(nontechnical_diploma_p[age_lower >= 20]) +
        sum_or_na(technical_diploma_p[age_lower >= 20]) +
        sum_or_na(graduate_plus_p[age_lower >= 20]),
      population_age20plus = sum_or_na(total_p[age_lower >= 20]),
      graduate_plus_n_age25plus = sum_or_na(graduate_plus_p[age_lower >= 25]),
      population_age25plus = sum_or_na(total_p[age_lower >= 25]),
      .by = c(state_no, district_code_source, district_name)
    ) |>
    dplyr::mutate(group = group)
}

clean_c08_2001_appendix_file <- function(path, group) {
  raw <- read_excel_raw(path)
  raw |>
    dplyr::select(1:43) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "group_code", "area_name", "group_name", "residence",
      "total_p", "total_m", "total_f", "illiterate_p", "illiterate_m", "illiterate_f",
      "literate_p", "literate_m", "literate_f", "no_level_p", "no_level_m", "no_level_f",
      "below_primary_p", "below_primary_m", "below_primary_f", "primary_p", "primary_m", "primary_f",
      "middle_p", "middle_m", "middle_f", "secondary_p", "secondary_m", "secondary_f",
      "higher_secondary_p", "higher_secondary_m", "higher_secondary_f",
      "nontechnical_diploma_p", "nontechnical_diploma_m", "nontechnical_diploma_f",
      "technical_diploma_p", "technical_diploma_m", "technical_diploma_f",
      "graduate_plus_p", "graduate_plus_m", "graduate_plus_f"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      district_code != "00",
      group_code == "000",
      residence == "Total"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), clean_num)) |>
    dplyr::transmute(
      state_no = as.integer(state_code),
      district_code_source = district_code,
      district_name = area_name |>
        stringr::str_remove("^District\\s*-\\s*") |>
        stringr::str_remove("\\s+\\d{2}$") |>
        stringr::str_squish(),
      higher_secondary_plus_n_age7plus =
        higher_secondary_p + nontechnical_diploma_p + technical_diploma_p + graduate_plus_p,
      population_age7plus = total_p,
      graduate_plus_n_age7plus = graduate_plus_p,
      group
    )
}

clean_d07_file <- function(path) {
  raw <- read_excel_raw(path)
  raw |>
    dplyr::select(1:10) |>
    rlang::set_names(c(
      "table", "state_code", "district_code", "area_name", "place_of_enumeration",
      "moved_from", "age_group", "work_persons", "work_males", "work_females"
    )) |>
    dplyr::filter(
      stringr::str_detect(state_code, "^\\d+$"),
      district_code != "000",
      place_of_enumeration == "Total",
      age_group == "All ages",
      stringr::str_detect(norm_name(moved_from), "OUTSIDE THE STATE")
    ) |>
    dplyr::mutate(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(district_code),
      district_name = area_name
    ) |>
    dplyr::summarise(
      interstate_work_migrants_0_9_n_2011 = sum(clean_num(work_persons), na.rm = TRUE),
      .by = c(state_no, district_code_2011, district_name)
    )
}

build_census_context <- function(paths, dirs, geography, migration) {
  message("Building religion, language, SC/ST education, and work-migrant context")
  
  required_dirs <- c(
    paths$religion_2001_dir, paths$religion_2011_dir,
    paths$language_2001_dir, paths$language_2011_dir,
    paths$sc_education_2001_dir, paths$sc_education_2011_dir,
    paths$st_education_2001_dir, paths$st_education_2011_dir,
    paths$migration_employment_2011_dir
  )
  purrr::walk(required_dirs, assert_directory_exists)
  assert_file_exists(paths$local_language_config)
  
  religion_2001 <- read_many_excel(paths$religion_2001_dir, "\\.xls[x]?$", clean_religion_file, year = 2001) |>
    dplyr::distinct(state_no, district_code_source, .keep_all = TRUE) |>
    add_district_lineage(2001, "district_name", geography$district_lineage)
  religion_2011 <- read_many_excel(paths$religion_2011_dir, "\\.xls[x]?$", clean_religion_file, year = 2011) |>
    dplyr::distinct(state_no, district_code_source, .keep_all = TRUE) |>
    add_district_lineage(2011, "district_name", geography$district_lineage)
  
  aggregate_religion <- function(data, year) {
    data |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::summarise(
        "total_population_{year}" := sum_or_na(total_population),
        "hindu_population_{year}" := sum_or_na(hindu_population),
        "muslim_population_{year}" := sum_or_na(muslim_population),
        .by = district_harmonization_group_id
      )
  }
  
  lineage_group_status <- geography$district_lineage$groups |>
    dplyr::select(
      district_harmonization_group_id,
      lineage_change_comparable = change_comparable
    )
  
  religion_groups <- aggregate_religion(religion_2001, 2001) |>
    dplyr::full_join(aggregate_religion(religion_2011, 2011), by = "district_harmonization_group_id") |>
    dplyr::left_join(lineage_group_status, by = "district_harmonization_group_id") |>
    dplyr::mutate(
      muslim_share_2001_dist_proxy = safe_share(muslim_population_2001, total_population_2001),
      muslim_share_2011_dist_proxy = safe_share(muslim_population_2011, total_population_2011),
      hindu_muslim_ratio_2001_dist_proxy = safe_ratio(hindu_population_2001, muslim_population_2001),
      hindu_muslim_ratio_2011_dist_proxy = safe_ratio(hindu_population_2011, muslim_population_2011),
      log_hindu_muslim_ratio_2001_dist_proxy = safe_log(hindu_muslim_ratio_2001_dist_proxy),
      log_hindu_muslim_ratio_2011_dist_proxy = safe_log(hindu_muslim_ratio_2011_dist_proxy),
      d_muslim_population_2001_2011_n = muslim_population_2011 - muslim_population_2001,
      pct_change_muslim_population_2001_2011 = safe_pct_change(muslim_population_2011, muslim_population_2001),
      d_log1p_muslim_population_2001_2011 = log1p(muslim_population_2011) - log1p(muslim_population_2001),
      d_muslim_share_2001_2011_pp = safe_pp_change(muslim_share_2011_dist_proxy, muslim_share_2001_dist_proxy),
      d_hindu_muslim_ratio_2001_2011_ratio_points =
        hindu_muslim_ratio_2011_dist_proxy - hindu_muslim_ratio_2001_dist_proxy,
      d_log_hindu_muslim_ratio_2001_2011 =
        log_hindu_muslim_ratio_2011_dist_proxy - log_hindu_muslim_ratio_2001_dist_proxy,
      dplyr::across(
        c(
          d_muslim_population_2001_2011_n,
          pct_change_muslim_population_2001_2011,
          d_log1p_muslim_population_2001_2011,
          d_muslim_share_2001_2011_pp,
          d_hindu_muslim_ratio_2001_2011_ratio_points,
          d_log_hindu_muslim_ratio_2001_2011
        ),
        ~ dplyr::if_else(lineage_change_comparable %in% TRUE, .x, NA_real_)
      )
    ) |>
    dplyr::select(-lineage_change_comparable)
  
  religion_2011_district <- religion_2011 |>
    dplyr::transmute(
      state_no,
      district_code_2011 = as.integer(district_code_source),
      total_population_2011 = total_population,
      hindu_population_2011 = hindu_population,
      muslim_population_2011 = muslim_population,
      muslim_share_2011_dist_proxy = safe_share(muslim_population_2011, total_population_2011),
      hindu_muslim_ratio_2011_dist_proxy = safe_ratio(hindu_population_2011, muslim_population_2011),
      log_hindu_muslim_ratio_2011_dist_proxy = safe_log(hindu_muslim_ratio_2011_dist_proxy)
    ) |>
    dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  
  language_key_full <- readr::read_csv(paths$local_language_config, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      state_no = as.integer(state_no),
      include_common = stringr::str_to_lower(as.character(include_common_2001_2011_change)) %in%
        c("true", "t", "1", "yes", "y"),
      code_2001 = dplyr::if_else(
        !is.na(clean_num(mother_tongue_code_2001)),
        sprintf("%06d", as.integer(clean_num(mother_tongue_code_2001))),
        NA_character_
      ),
      code_2011 = dplyr::if_else(
        !is.na(clean_num(mother_tongue_code_2011)),
        sprintf("%06d", as.integer(clean_num(mother_tongue_code_2011))),
        NA_character_
      ),
      included_in_analysis = include_common & !is.na(code_2001) & !is.na(code_2011)
    )
  
  language_key <- language_key_full |>
    dplyr::filter(included_in_analysis)
  
  duplicate_language_codes <- dplyr::bind_rows(
    language_key |>
      dplyr::count(state_no, code = code_2001, name = "n") |>
      dplyr::filter(n > 1) |>
      dplyr::mutate(year = 2001L),
    language_key |>
      dplyr::count(state_no, code = code_2011, name = "n") |>
      dplyr::filter(n > 1) |>
      dplyr::mutate(year = 2011L)
  )
  if (nrow(duplicate_language_codes) > 0) {
    readr::write_csv(
      duplicate_language_codes,
      file.path(dirs$diagnostic_dir, "local_language_duplicate_codes.csv")
    )
    stop("The common-language key counts at least one state-year source code more than once.")
  }
  
  language_2001_raw <- read_many_excel(
    paths$language_2001_dir,
    "\\.xls[x]?$",
    clean_language_file,
    year = 2001
  ) |>
    add_district_lineage(2001, "district_name", geography$district_lineage)

  language_2011_raw <- read_many_excel(
    paths$language_2011_dir,
    "\\.xls[x]?$",
    clean_language_file,
    year = 2011
  ) |>
    add_district_lineage(2011, "district_name", geography$district_lineage)

  language_2001 <- language_2001_raw |>
    dplyr::inner_join(
      language_key |>
        dplyr::select(
          state_no,
          mother_tongue_code = code_2001,
          canonical_language
        ),
      by = c("state_no", "mother_tongue_code"),
      relationship = "many-to-one"
    )

  language_2011 <- language_2011_raw |>
    dplyr::inner_join(
      language_key |>
        dplyr::select(
          state_no,
          mother_tongue_code = code_2011,
          canonical_language
        ),
      by = c("state_no", "mother_tongue_code"),
      relationship = "many-to-one"
    )

  aggregate_local_language <- function(data, year) {
    data |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(
        state_no, district_harmonization_group_id, district_name,
        mother_tongue_code, canonical_language, .keep_all = TRUE
      ) |>
      dplyr::summarise(
        "local_language_population_common_{year}" := sum_or_na(persons),
        .by = district_harmonization_group_id
      )
  }
  
  language_groups <- aggregate_local_language(language_2001, 2001) |>
    dplyr::full_join(aggregate_local_language(language_2011, 2011), by = "district_harmonization_group_id") |>
    dplyr::left_join(
      religion_groups |>
        dplyr::select(
          district_harmonization_group_id,
          total_population_2001,
          total_population_2011
        ),
      by = "district_harmonization_group_id"
    ) |>
    dplyr::left_join(lineage_group_status, by = "district_harmonization_group_id") |>
    dplyr::mutate(
      local_language_share_common_2001_dist_proxy = safe_share(
        local_language_population_common_2001,
        total_population_2001
      ),
      local_language_share_common_2011_dist_proxy = safe_share(
        local_language_population_common_2011,
        total_population_2011
      ),
      nonlocal_language_population_common_2001 = pmax(
        total_population_2001 - local_language_population_common_2001,
        0
      ),
      nonlocal_language_population_common_2011 = pmax(
        total_population_2011 - local_language_population_common_2011,
        0
      ),
      nonlocal_language_share_common_2001_dist_proxy = 1 - local_language_share_common_2001_dist_proxy,
      nonlocal_language_share_common_2011_dist_proxy = 1 - local_language_share_common_2011_dist_proxy,
      d_nonlocal_language_share_common_2001_2011_pp = safe_pp_change(
        nonlocal_language_share_common_2011_dist_proxy,
        nonlocal_language_share_common_2001_dist_proxy
      ),
      d_nonlocal_language_share_common_2001_2011_pp = dplyr::if_else(
        lineage_change_comparable %in% TRUE,
        d_nonlocal_language_share_common_2001_2011_pp,
        NA_real_
      )
    ) |>
    dplyr::select(
      district_harmonization_group_id,
      dplyr::starts_with("local_language_"),
      dplyr::starts_with("nonlocal_language_"),
      d_nonlocal_language_share_common_2001_2011_pp
    )
  
  language_2011_district <- language_2011 |>
    dplyr::distinct(
      state_no, district_code_source, mother_tongue_code, canonical_language,
      .keep_all = TRUE
    ) |>
    dplyr::mutate(
      district_code_2011 = as.integer(district_code_source)
    ) |>
    dplyr::summarise(
      local_language_population_common_2011 = sum_or_na(persons),
      .by = c(state_no, district_code_2011)
    ) |>
    dplyr::left_join(
      religion_2011_district |>
        dplyr::select(state_no, district_code_2011, total_population_2011),
      by = c("state_no", "district_code_2011"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      local_language_share_common_2011_dist_proxy = safe_share(
        local_language_population_common_2011, total_population_2011
      ),
      nonlocal_language_population_common_2011 = pmax(
        total_population_2011 - local_language_population_common_2011, 0
      ),
      nonlocal_language_share_common_2011_dist_proxy =
        1 - local_language_share_common_2011_dist_proxy
    ) |>
    dplyr::select(-total_population_2011)
  

  # Targeted Bengali/Bhojpuri outsider-language measure ---------------------
  #
  # Census mother-tongue codes are national rather than state-specific. This
  # codebook therefore captures Bengali and Bhojpuri even in states where they
  # are not included in the local-language crosswalk used above.
  target_language_codebook <- tibble::tribble(
    ~canonical_language, ~code_2001, ~code_2011,
    "Bengali",           "002004",   "002007",
    "Bhojpuri",          "006045",   "006102"
  )

  add_target_language_rules <- function(data) {
    data |>
      dplyr::mutate(
        target_include_bengali = dplyr::case_when(
          is.na(state_no) ~ NA,
          state_no %in% c(16L, 19L) ~ FALSE,
          TRUE ~ TRUE
        ),
        target_include_bhojpuri = dplyr::case_when(
          is.na(state_no) ~ NA,
          state_no %in% c(9L, 10L) ~ FALSE,
          TRUE ~ TRUE
        ),
        target_language_rule = dplyr::case_when(
          is.na(state_no) ~ NA_character_,
          state_no %in% c(9L, 10L) ~
            "Bengali only: Bihar or Uttar Pradesh",
          state_no == 18L ~
            "Bengali and Bhojpuri: Assam exception",
          state_no %in% c(16L, 19L) ~
            "Bhojpuri only: Tripura or West Bengal",
          TRUE ~ "Bengali and Bhojpuri"
        )
      )
  }

  map_target_language <- function(data, year) {
    code_column <- paste0("code_", year)
    year_key <- target_language_codebook |>
      dplyr::select(
        canonical_language,
        dplyr::all_of(code_column)
      ) |>
      rlang::set_names(
        c("canonical_language", "mother_tongue_code")
      )

    data |>
      dplyr::inner_join(
        year_key,
        by = "mother_tongue_code",
        relationship = "many-to-one"
      )
  }

  language_2001_target <- map_target_language(language_2001_raw, 2001)
  language_2011_target <- map_target_language(language_2011_raw, 2011)

  group_state_lookup <- dplyr::bind_rows(
    language_2001_raw |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(state_no, district_harmonization_group_id),
    language_2011_raw |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(state_no, district_harmonization_group_id)
  ) |>
    dplyr::distinct()

  target_language_group_state_conflicts <- group_state_lookup |>
    dplyr::count(
      district_harmonization_group_id,
      name = "n_states"
    ) |>
    dplyr::filter(n_states > 1)

  if (nrow(target_language_group_state_conflicts) > 0) {
    readr::write_csv(
      target_language_group_state_conflicts,
      file.path(
        dirs$diagnostic_dir,
        "target_language_group_state_conflicts.csv"
      )
    )
    stop(
      "At least one district harmonization group crosses state boundaries; ",
      "the targeted language rule cannot be assigned uniquely."
    )
  }

  aggregate_target_language_group <- function(data, year) {
    data |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(
        state_no,
        district_harmonization_group_id,
        district_name,
        mother_tongue_code,
        canonical_language,
        .keep_all = TRUE
      ) |>
      dplyr::summarise(
        persons = sum_or_na(persons),
        .by = c(
          state_no,
          district_harmonization_group_id,
          canonical_language
        )
      ) |>
      tidyr::complete(
        tidyr::nesting(
          state_no,
          district_harmonization_group_id
        ),
        canonical_language = c("Bengali", "Bhojpuri"),
        fill = list(persons = 0)
      ) |>
      tidyr::pivot_wider(
        names_from = canonical_language,
        values_from = persons,
        values_fill = 0
      ) |>
      dplyr::rename(
        "bengali_population_{year}" := Bengali,
        "bhojpuri_population_{year}" := Bhojpuri
      )
  }

  target_group_scaffold <- dplyr::full_join(
    language_2001_raw |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(state_no, district_harmonization_group_id) |>
      dplyr::mutate(language_source_2001_available = TRUE),
    language_2011_raw |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::distinct(state_no, district_harmonization_group_id) |>
      dplyr::mutate(language_source_2011_available = TRUE),
    by = c("state_no", "district_harmonization_group_id"),
    relationship = "one-to-one"
  )

  target_language_groups <- target_group_scaffold |>
    dplyr::left_join(
      aggregate_target_language_group(language_2001_target, 2001),
      by = c("state_no", "district_harmonization_group_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      aggregate_target_language_group(language_2011_target, 2011),
      by = c("state_no", "district_harmonization_group_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      language_source_2001_available =
        tidyr::replace_na(language_source_2001_available, FALSE),
      language_source_2011_available =
        tidyr::replace_na(language_source_2011_available, FALSE),
      bengali_population_2001 = dplyr::if_else(
        language_source_2001_available,
        tidyr::replace_na(bengali_population_2001, 0),
        NA_real_
      ),
      bhojpuri_population_2001 = dplyr::if_else(
        language_source_2001_available,
        tidyr::replace_na(bhojpuri_population_2001, 0),
        NA_real_
      ),
      bengali_population_2011 = dplyr::if_else(
        language_source_2011_available,
        tidyr::replace_na(bengali_population_2011, 0),
        NA_real_
      ),
      bhojpuri_population_2011 = dplyr::if_else(
        language_source_2011_available,
        tidyr::replace_na(bhojpuri_population_2011, 0),
        NA_real_
      )
    ) |>
    add_target_language_rules() |>
    dplyr::left_join(
      religion_groups |>
        dplyr::select(
          district_harmonization_group_id,
          total_population_2001,
          total_population_2011
        ),
      by = "district_harmonization_group_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      lineage_group_status,
      by = "district_harmonization_group_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      target_bengali_bhojpuri_population_2001 =
        dplyr::if_else(
          target_include_bengali,
          bengali_population_2001,
          0
        ) +
        dplyr::if_else(
          target_include_bhojpuri,
          bhojpuri_population_2001,
          0
        ),
      target_bengali_bhojpuri_population_2011 =
        dplyr::if_else(
          target_include_bengali,
          bengali_population_2011,
          0
        ) +
        dplyr::if_else(
          target_include_bhojpuri,
          bhojpuri_population_2011,
          0
        ),
      target_bengali_bhojpuri_share_2001_dist_proxy = safe_share(
        target_bengali_bhojpuri_population_2001,
        total_population_2001
      ),
      target_bengali_bhojpuri_share_2011_dist_proxy = safe_share(
        target_bengali_bhojpuri_population_2011,
        total_population_2011
      ),
      d_target_bengali_bhojpuri_population_2001_2011_n =
        target_bengali_bhojpuri_population_2011 -
        target_bengali_bhojpuri_population_2001,
      d_target_bengali_bhojpuri_share_2001_2011_pp = safe_pp_change(
        target_bengali_bhojpuri_share_2011_dist_proxy,
        target_bengali_bhojpuri_share_2001_dist_proxy
      ),
      d_target_bengali_bhojpuri_population_2001_2011_n =
        dplyr::if_else(
          lineage_change_comparable %in% TRUE,
          d_target_bengali_bhojpuri_population_2001_2011_n,
          NA_real_
        ),
      d_target_bengali_bhojpuri_share_2001_2011_pp =
        dplyr::if_else(
          lineage_change_comparable %in% TRUE,
          d_target_bengali_bhojpuri_share_2001_2011_pp,
          NA_real_
        )
    )

  aggregate_target_language_district_2011 <- function(data) {
    data |>
      dplyr::distinct(
        state_no,
        district_code_source,
        district_name,
        mother_tongue_code,
        canonical_language,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        district_code_2011 = as.integer(district_code_source)
      ) |>
      dplyr::summarise(
        persons = sum_or_na(persons),
        .by = c(
          state_no,
          district_code_2011,
          canonical_language
        )
      ) |>
      tidyr::complete(
        tidyr::nesting(state_no, district_code_2011),
        canonical_language = c("Bengali", "Bhojpuri"),
        fill = list(persons = 0)
      ) |>
      tidyr::pivot_wider(
        names_from = canonical_language,
        values_from = persons,
        values_fill = 0
      ) |>
      dplyr::rename(
        bengali_population_2011 = Bengali,
        bhojpuri_population_2011 = Bhojpuri
      )
  }

  target_language_2011_district <- language_2011_raw |>
    dplyr::mutate(
      district_code_2011 = as.integer(district_code_source)
    ) |>
    dplyr::distinct(state_no, district_code_2011) |>
    dplyr::left_join(
      aggregate_target_language_district_2011(language_2011_target),
      by = c("state_no", "district_code_2011"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      bengali_population_2011 =
        tidyr::replace_na(bengali_population_2011, 0),
      bhojpuri_population_2011 =
        tidyr::replace_na(bhojpuri_population_2011, 0)
    ) |>
    add_target_language_rules() |>
    dplyr::left_join(
      religion_2011_district |>
        dplyr::select(
          state_no,
          district_code_2011,
          total_population_2011
        ),
      by = c("state_no", "district_code_2011"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      target_bengali_bhojpuri_population_2011 =
        dplyr::if_else(
          target_include_bengali,
          bengali_population_2011,
          0
        ) +
        dplyr::if_else(
          target_include_bhojpuri,
          bhojpuri_population_2011,
          0
        ),
      target_bengali_bhojpuri_share_2011_dist_proxy = safe_share(
        target_bengali_bhojpuri_population_2011,
        total_population_2011
      )
    ) |>
    dplyr::select(
      state_no,
      district_code_2011,
      bengali_population_2011,
      bhojpuri_population_2011,
      target_bengali_bhojpuri_population_2011,
      target_bengali_bhojpuri_share_2011_dist_proxy
    )

  language_groups <- language_groups |>
    dplyr::full_join(
      target_language_groups |>
        dplyr::select(
          district_harmonization_group_id,
          bengali_population_2001,
          bhojpuri_population_2001,
          bengali_population_2011,
          bhojpuri_population_2011,
          target_bengali_bhojpuri_population_2001,
          target_bengali_bhojpuri_population_2011,
          target_bengali_bhojpuri_share_2001_dist_proxy,
          target_bengali_bhojpuri_share_2011_dist_proxy,
          d_target_bengali_bhojpuri_population_2001_2011_n,
          d_target_bengali_bhojpuri_share_2001_2011_pp
        ),
      by = "district_harmonization_group_id",
      relationship = "one-to-one"
    )

  language_2011_district <- language_2011_district |>
    dplyr::full_join(
      target_language_2011_district,
      by = c("state_no", "district_code_2011"),
      relationship = "one-to-one"
    )

  sc_2011 <- read_many_excel(
    paths$sc_education_2011_dir,
    "C-08SC\\.xlsx$",
    clean_c08_2011_file,
    group = "sc"
  ) |>
    dplyr::distinct(state_no, district_code_source, .keep_all = TRUE) |>
    add_district_lineage(2011, "district_name", geography$district_lineage)
  
  st_2011 <- read_many_excel(
    paths$st_education_2011_dir,
    "C-08ST\\.xlsx$",
    clean_c08_2011_file,
    group = "st"
  ) |>
    dplyr::distinct(state_no, district_code_source, .keep_all = TRUE) |>
    add_district_lineage(2011, "district_name", geography$district_lineage)
  
  sc_2001 <- read_many_excel(
    paths$sc_education_2001_dir,
    "SC08_APPENDIX.*\\.xls$",
    clean_c08_2001_appendix_file,
    group = "sc"
  ) |>
    add_district_lineage(2001, "district_name", geography$district_lineage)
  
  st_2001 <- read_many_excel(
    paths$st_education_2001_dir,
    "ST08_APPENDIX.*\\.xls$",
    clean_c08_2001_appendix_file,
    group = "st"
  ) |>
    add_district_lineage(2001, "district_name", geography$district_lineage)
  
  education_2011_district <- function(data, prefix) {
    data |>
      dplyr::transmute(
        state_no,
        district_code_2011 = as.integer(district_code_source),
        "{prefix}_higher_secondary_plus_share_age7plus_2011_dist_proxy" := safe_share(
          higher_secondary_plus_n_age7plus,
          population_age7plus
        ),
        "{prefix}_graduate_plus_share_age7plus_2011_dist_proxy" := safe_share(
          graduate_plus_n_age7plus,
          population_age7plus
        ),
        "{prefix}_higher_secondary_plus_share_age20plus_2011_dist_proxy" := safe_share(
          higher_secondary_plus_n_age20plus,
          population_age20plus
        ),
        "{prefix}_graduate_plus_share_age25plus_2011_dist_proxy" := safe_share(
          graduate_plus_n_age25plus,
          population_age25plus
        )
      ) |>
      dplyr::distinct(state_no, district_code_2011, .keep_all = TRUE)
  }
  
  education_2011_direct <- education_2011_district(sc_2011, "sc") |>
    dplyr::full_join(
      education_2011_district(st_2011, "st"),
      by = c("state_no", "district_code_2011")
    )
  
  aggregate_education_2011 <- function(data, prefix) {
    data |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::summarise(
        higher_secondary_plus_n_age7plus =
          sum_or_na(higher_secondary_plus_n_age7plus),
        population_age7plus = sum_or_na(population_age7plus),
        graduate_plus_n_age7plus =
          sum_or_na(graduate_plus_n_age7plus),
        higher_secondary_plus_n_age20plus =
          sum_or_na(higher_secondary_plus_n_age20plus),
        population_age20plus = sum_or_na(population_age20plus),
        graduate_plus_n_age25plus =
          sum_or_na(graduate_plus_n_age25plus),
        population_age25plus = sum_or_na(population_age25plus),
        .by = district_harmonization_group_id
      ) |>
      dplyr::transmute(
        district_harmonization_group_id,
        "{prefix}_higher_secondary_plus_share_age7plus_2011_dist_proxy" := safe_share(
          higher_secondary_plus_n_age7plus,
          population_age7plus
        ),
        "{prefix}_graduate_plus_share_age7plus_2011_dist_proxy" := safe_share(
          graduate_plus_n_age7plus,
          population_age7plus
        ),
        "{prefix}_higher_secondary_plus_share_age20plus_2011_dist_proxy" := safe_share(
          higher_secondary_plus_n_age20plus,
          population_age20plus
        ),
        "{prefix}_graduate_plus_share_age25plus_2011_dist_proxy" := safe_share(
          graduate_plus_n_age25plus,
          population_age25plus
        )
      )
  }
  
  aggregate_education_2001 <- function(data, prefix) {
    data |>
      dplyr::filter(!is.na(district_harmonization_group_id)) |>
      dplyr::summarise(
        higher_secondary_plus_n_age7plus = sum_or_na(higher_secondary_plus_n_age7plus),
        population_age7plus = sum_or_na(population_age7plus),
        graduate_plus_n_age7plus = sum_or_na(graduate_plus_n_age7plus),
        .by = district_harmonization_group_id
      ) |>
      dplyr::transmute(
        district_harmonization_group_id,
        "{prefix}_higher_secondary_plus_share_age7plus_2001_dist_proxy" := safe_share(
          higher_secondary_plus_n_age7plus,
          population_age7plus
        ),
        "{prefix}_graduate_plus_share_age7plus_2001_dist_proxy" := safe_share(
          graduate_plus_n_age7plus,
          population_age7plus
        )
      )
  }
  
  education_groups <- aggregate_education_2011(sc_2011, "sc") |>
    dplyr::full_join(
      aggregate_education_2011(st_2011, "st"),
      by = "district_harmonization_group_id"
    ) |>
    dplyr::full_join(
      aggregate_education_2001(sc_2001, "sc"),
      by = "district_harmonization_group_id"
    ) |>
    dplyr::full_join(
      aggregate_education_2001(st_2001, "st"),
      by = "district_harmonization_group_id"
    ) |>
    dplyr::left_join(
      lineage_group_status,
      by = "district_harmonization_group_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp =
        safe_pp_change(
          sc_higher_secondary_plus_share_age7plus_2011_dist_proxy,
          sc_higher_secondary_plus_share_age7plus_2001_dist_proxy
        ),
      d_sc_graduate_plus_share_age7plus_2001_2011_pp =
        safe_pp_change(
          sc_graduate_plus_share_age7plus_2011_dist_proxy,
          sc_graduate_plus_share_age7plus_2001_dist_proxy
        ),
      d_st_higher_secondary_plus_share_age7plus_2001_2011_pp =
        safe_pp_change(
          st_higher_secondary_plus_share_age7plus_2011_dist_proxy,
          st_higher_secondary_plus_share_age7plus_2001_dist_proxy
        ),
      d_st_graduate_plus_share_age7plus_2001_2011_pp =
        safe_pp_change(
          st_graduate_plus_share_age7plus_2011_dist_proxy,
          st_graduate_plus_share_age7plus_2001_dist_proxy
        ),
      dplyr::across(
        c(
          d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp,
          d_sc_graduate_plus_share_age7plus_2001_2011_pp,
          d_st_higher_secondary_plus_share_age7plus_2001_2011_pp,
          d_st_graduate_plus_share_age7plus_2001_2011_pp
        ),
        ~ dplyr::if_else(
          lineage_change_comparable %in% TRUE,
          .x,
          NA_real_
        )
      ),
      education_age7plus_change_comparable =
        lineage_change_comparable %in% TRUE,
      sc_higher_secondary_plus_share_age20plus_2001_dist_proxy = NA_real_,
      sc_graduate_plus_share_age25plus_2001_dist_proxy = NA_real_,
      st_higher_secondary_plus_share_age20plus_2001_dist_proxy = NA_real_,
      st_graduate_plus_share_age25plus_2001_dist_proxy = NA_real_,
      d_sc_higher_secondary_plus_share_age20plus_2001_2011_pp = NA_real_,
      d_sc_graduate_plus_share_age25plus_2001_2011_pp = NA_real_,
      d_st_higher_secondary_plus_share_age20plus_2001_2011_pp = NA_real_,
      d_st_graduate_plus_share_age25plus_2001_2011_pp = NA_real_,
      education_change_comparable = FALSE
    )
  
  # The supplied 2001 C-08 appendices contain age-7-plus totals. The code
  # therefore constructs comparable age-7-plus changes as robustness measures,
  # while leaving preferred age-20-plus and age-25-plus changes unavailable.
  education_source_diagnostic <- dplyr::bind_rows(
    tibble::tibble(
      module = c("SC education", "ST education"),
      year = 2001L,
      check = "Comparable age-7-plus education change constructed",
      n = c(
        sum(
          !is.na(
            education_groups$
              d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp
          )
        ),
        sum(
          !is.na(
            education_groups$
              d_st_higher_secondary_plus_share_age7plus_2001_2011_pp
          )
        )
      ),
      denominator = nrow(education_groups),
      pct = 100 * safe_divide(n, denominator),
      minimum = c(
        safe_min(
          education_groups$
            d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp
        ),
        safe_min(
          education_groups$
            d_st_higher_secondary_plus_share_age7plus_2001_2011_pp
        )
      ),
      median = c(
        safe_median(
          education_groups$
            d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp
        ),
        safe_median(
          education_groups$
            d_st_higher_secondary_plus_share_age7plus_2001_2011_pp
        )
      ),
      maximum = c(
        safe_max(
          education_groups$
            d_sc_higher_secondary_plus_share_age7plus_2001_2011_pp
        ),
        safe_max(
          education_groups$
            d_st_higher_secondary_plus_share_age7plus_2001_2011_pp
        )
      ),
      passed = n > 0,
      details = paste(
        "Uses the same age-7-plus denominator in 2001 and 2011;",
        "retained only for comparable district lineages."
      )
    ),
    tibble::tibble(
      module = c("SC education", "ST education"),
      year = 2001L,
      check = "Preferred age-20-plus or age-25-plus denominator available",
      n = 0L,
      denominator = 1L,
      pct = 0,
      minimum = NA_real_,
      median = NA_real_,
      maximum = NA_real_,
      passed = FALSE,
      details = paste(
        "The supplied 2001 C-08 appendix contains only population age 7+ totals.",
        "Preferred age-20-plus and age-25-plus levels and changes remain NA."
      )
    )
  )
  
  work_district <- read_many_excel(
    paths$migration_employment_2011_dir,
    "D07.*\\.xlsx$",
    clean_d07_file
  ) |>
    dplyr::summarise(
      interstate_work_migrants_0_9_n_2011 = first_nonmissing(interstate_work_migrants_0_9_n_2011),
      district_name = first_nonmissing(district_name),
      .by = c(state_no, district_code_2011)
    ) |>
    dplyr::left_join(
      migration$interstate_district,
      by = c("state_no", "district_code_2011"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      interstate_work_migrant_share_0_9_2011_dist_proxy = safe_share(
        interstate_work_migrants_0_9_n_2011,
        interstate_migrants_0_9_n_2011
      )
    )
  
  # Baseline levels and changes use common district-lineage groups. Current
  # 2011 levels remain the actual 2011 district values attached to each AC.
  religion_group_context <- religion_groups |>
    dplyr::select(
      district_harmonization_group_id,
      total_population_2001, hindu_population_2001, muslim_population_2001,
      muslim_share_2001_dist_proxy, hindu_muslim_ratio_2001_dist_proxy,
      log_hindu_muslim_ratio_2001_dist_proxy,
      d_muslim_population_2001_2011_n,
      pct_change_muslim_population_2001_2011,
      d_log1p_muslim_population_2001_2011,
      d_muslim_share_2001_2011_pp,
      d_hindu_muslim_ratio_2001_2011_ratio_points,
      d_log_hindu_muslim_ratio_2001_2011
    )
  
  language_group_context <- language_groups |>
    dplyr::select(
      district_harmonization_group_id,
      local_language_population_common_2001,
      local_language_share_common_2001_dist_proxy,
      nonlocal_language_population_common_2001,
      nonlocal_language_share_common_2001_dist_proxy,
      d_nonlocal_language_share_common_2001_2011_pp,
      bengali_population_2001,
      bhojpuri_population_2001,
      target_bengali_bhojpuri_population_2001,
      target_bengali_bhojpuri_share_2001_dist_proxy,
      d_target_bengali_bhojpuri_population_2001_2011_n,
      d_target_bengali_bhojpuri_share_2001_2011_pp
    )
  
  education_group_context <- education_groups |>
    dplyr::select(
      district_harmonization_group_id,
      dplyr::contains("age7plus_2001"),
      dplyr::contains("age20plus_2001"),
      dplyr::contains("age25plus_2001"),
      dplyr::starts_with("d_sc_"),
      dplyr::starts_with("d_st_"),
      education_age7plus_change_comparable,
      education_change_comparable
    )
  
  group_context <- religion_group_context |>
    dplyr::full_join(language_group_context, by = "district_harmonization_group_id") |>
    dplyr::full_join(education_group_context, by = "district_harmonization_group_id")
  
  district_2011_context <- religion_2011_district |>
    dplyr::full_join(
      language_2011_district,
      by = c("state_no", "district_code_2011")
    ) |>
    dplyr::full_join(
      education_2011_direct,
      by = c("state_no", "district_code_2011")
    )
  
  ac_context <- geography$ac_reference |>
    dplyr::select(
      state, state_no, pc, ac, ac_uid, district_code_2011,
      district_name_2011, district_harmonization_group_id,
      district_relationship_type, district_change_comparable
    ) |>
    dplyr::left_join(group_context, by = "district_harmonization_group_id", relationship = "many-to-one") |>
    dplyr::left_join(
      district_2011_context,
      by = c("state_no", "district_code_2011"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      work_district |>
        dplyr::select(
          state_no, district_code_2011,
          interstate_work_migrants_0_9_n_2011,
          interstate_migrants_0_9_n_2011,
          interstate_work_migrant_share_0_9_2011_dist_proxy
        ),
      by = c("state_no", "district_code_2011"),
      relationship = "many-to-one"
    ) |>
    add_target_language_rules()

  target_language_rule_assignments <- ac_context |>
    dplyr::count(
      state,
      state_no,
      target_include_bengali,
      target_include_bhojpuri,
      target_language_rule,
      name = "n_ac"
    ) |>
    dplyr::arrange(state_no)

  target_language_source_code_diagnostics <- dplyr::bind_rows(
    target_language_codebook |>
      dplyr::transmute(
        canonical_language,
        year = 2001L,
        mother_tongue_code = code_2001
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        n_source_rows = sum(
          language_2001_raw$mother_tongue_code == mother_tongue_code,
          na.rm = TRUE
        ),
        n_states = dplyr::n_distinct(
          language_2001_raw$state_no[
            language_2001_raw$mother_tongue_code == mother_tongue_code
          ],
          na.rm = TRUE
        ),
        passed = n_source_rows > 0
      ) |>
      dplyr::ungroup(),
    target_language_codebook |>
      dplyr::transmute(
        canonical_language,
        year = 2011L,
        mother_tongue_code = code_2011
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        n_source_rows = sum(
          language_2011_raw$mother_tongue_code == mother_tongue_code,
          na.rm = TRUE
        ),
        n_states = dplyr::n_distinct(
          language_2011_raw$state_no[
            language_2011_raw$mother_tongue_code == mother_tongue_code
          ],
          na.rm = TRUE
        ),
        passed = n_source_rows > 0
      ) |>
      dplyr::ungroup()
  )

  language_code_duplicates <- duplicate_language_codes
  
  census_diagnostics <- dplyr::bind_rows(
    education_source_diagnostic,
    tibble::tibble(
      module = "Language",
      year = NA_integer_,
      check = "Duplicate common-language source-code pair within state",
      n = nrow(language_code_duplicates),
      denominator = nrow(language_key),
      pct = 100 * safe_divide(n, denominator),
      minimum = NA_real_, median = NA_real_, maximum = NA_real_,
      passed = n == 0,
      details = NA_character_
    ),
    target_language_groups |>
      dplyr::summarise(
        module = "Target language",
        year = 2001L,
        check = "Target Bengali/Bhojpuri share outside [0,1]",
        n = sum(
          !is.na(target_bengali_bhojpuri_share_2001_dist_proxy) &
            (
              target_bengali_bhojpuri_share_2001_dist_proxy < 0 |
                target_bengali_bhojpuri_share_2001_dist_proxy > 1
            )
        ),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(
          target_bengali_bhojpuri_share_2001_dist_proxy
        ),
        median = safe_median(
          target_bengali_bhojpuri_share_2001_dist_proxy
        ),
        maximum = safe_max(
          target_bengali_bhojpuri_share_2001_dist_proxy
        ),
        passed = n == 0,
        details = "Harmonized district-lineage baseline"
      ),
    target_language_2011_district |>
      dplyr::summarise(
        module = "Target language",
        year = 2011L,
        check = "Target Bengali/Bhojpuri share outside [0,1]",
        n = sum(
          !is.na(target_bengali_bhojpuri_share_2011_dist_proxy) &
            (
              target_bengali_bhojpuri_share_2011_dist_proxy < 0 |
                target_bengali_bhojpuri_share_2011_dist_proxy > 1
            )
        ),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(
          target_bengali_bhojpuri_share_2011_dist_proxy
        ),
        median = safe_median(
          target_bengali_bhojpuri_share_2011_dist_proxy
        ),
        maximum = safe_max(
          target_bengali_bhojpuri_share_2011_dist_proxy
        ),
        passed = n == 0,
        details = "Actual 2011 district level"
      ),
    target_language_source_code_diagnostics |>
      dplyr::transmute(
        module = "Target language",
        year,
        check = paste(canonical_language, "source code found"),
        n = as.integer(n_source_rows == 0),
        denominator = 1L,
        pct = 100 * n,
        minimum = as.numeric(n_source_rows),
        median = as.numeric(n_source_rows),
        maximum = as.numeric(n_source_rows),
        passed,
        details = paste0(
          "Code ", mother_tongue_code,
          "; source rows=", n_source_rows,
          "; states represented=", n_states
        )
      ),
    ac_context |>
      dplyr::summarise(
        module = "Religion",
        year = 2011L,
        check = "Muslim share outside [0,1]",
        n = sum(!is.na(muslim_share_2011_dist_proxy) &
                  (muslim_share_2011_dist_proxy < 0 | muslim_share_2011_dist_proxy > 1)),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(muslim_share_2011_dist_proxy),
        median = safe_median(muslim_share_2011_dist_proxy),
        maximum = safe_max(muslim_share_2011_dist_proxy),
        passed = n == 0,
        details = NA_character_
      ),
    work_district |>
      dplyr::summarise(
        module = "Work migration",
        year = 2011L,
        check = "Work migrants exceed all interstate migrants",
        n = sum(interstate_work_migrants_0_9_n_2011 > interstate_migrants_0_9_n_2011, na.rm = TRUE),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(interstate_work_migrant_share_0_9_2011_dist_proxy),
        median = safe_median(interstate_work_migrant_share_0_9_2011_dist_proxy),
        maximum = safe_max(interstate_work_migrant_share_0_9_2011_dist_proxy),
        passed = n == 0,
        details = NA_character_
      )
  )
  
  readr::write_csv(
    language_key_full,
    file.path(dirs$final_dir, "local_language_crosswalk_final.csv")
  )
  readr::write_csv(
    target_language_codebook,
    file.path(dirs$final_dir, "target_language_codebook.csv")
  )
  write_csv_checked(
    target_language_groups,
    file.path(
      dirs$intermediate_dir,
      "target_bengali_bhojpuri_language_groups.csv"
    ),
    "district_harmonization_group_id"
  )
  write_csv_checked(
    target_language_2011_district,
    file.path(
      dirs$intermediate_dir,
      "target_bengali_bhojpuri_language_2011_district.csv"
    ),
    c("state_no", "district_code_2011")
  )
  write_csv_checked(
    target_language_rule_assignments,
    file.path(
      dirs$diagnostic_dir,
      "target_language_rule_assignments.csv"
    ),
    "state_no"
  )
  write_csv_checked(
    target_language_source_code_diagnostics,
    file.path(
      dirs$diagnostic_dir,
      "target_language_source_code_diagnostics.csv"
    ),
    c("canonical_language", "year")
  )
  write_csv_checked(
    education_groups,
    file.path(
      dirs$intermediate_dir,
      "sc_st_education_harmonized_groups.csv"
    ),
    "district_harmonization_group_id"
  )
  write_csv_checked(
    education_2011_direct,
    file.path(
      dirs$intermediate_dir,
      "sc_st_education_2011_district.csv"
    ),
    c("state_no", "district_code_2011")
  )
  write_csv_checked(
    ac_context,
    file.path(dirs$intermediate_dir, "census_context_ac.csv"),
    "ac_uid"
  )
  write_csv_checked(
    census_diagnostics,
    file.path(dirs$diagnostic_dir, "census_module_diagnostics.csv")
  )

  list(
    religion_groups = religion_groups,
    language_groups = language_groups,
    target_language_groups = target_language_groups,
    target_language_2011_district = target_language_2011_district,
    target_language_rule_assignments = target_language_rule_assignments,
    education_groups = education_groups,
    work_district = work_district,
    ac_context = ac_context,
    diagnostics = census_diagnostics
  )
}

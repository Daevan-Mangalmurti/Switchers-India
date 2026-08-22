# ============================================================
# 01_build_data.R
# Build all final analysis datasets from the existing source tree.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages(include_models = FALSE)

paths <- build_project_paths(project_root)
create_output_directories(paths)

purrr::walk(
  c("geography.R", "elections.R", "fdi.R", "census.R", "secc.R", "nes.R"),
  ~ source(file.path(paths$r_dir, .x))
)

message("1/8 Geography")
geography <- build_geography(paths, paths)

message("2/8 Elections")
elections <- build_elections(paths, paths, geography)

message("3/8 Population and fixed AC controls")
population <- build_population_demographics(paths, paths, geography)

message("4/8 FDI")
fdi <- build_fdi(paths, paths, geography, elections, population$demographics)

message("5/8 Migration")
migration <- build_migration(paths, paths, geography, population)

message("6/8 Census context")
census <- build_census_context(paths, paths, geography, migration)

message("7/8 SHRUG SECC context")
secc <- build_secc_context(paths, paths, geography)

message("8/8 NES respondents")
nes <- build_nes(paths, paths, geography)

# ------------------------------------------------------------
# Final AC-year panel
# ------------------------------------------------------------

population_join_columns <- setdiff(
  names(population$demographics),
  names(elections$ac_year)
)

ac_year <- elections$ac_year |>
  dplyr::left_join(
    population$demographics |>
      dplyr::select(
        state_no, ac, ac_uid, district_code_2011,
        dplyr::all_of(population_join_columns)
      ),
    by = c("state_no", "ac", "ac_uid", "district_code_2011"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    fdi$ac_year |>
      dplyr::select(-dplyr::any_of(c("state_no", "ac"))),
    by = c("ac_uid", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    migration$ac_year |>
      dplyr::select(-dplyr::any_of(c("state_no", "ac", "proxy_ac_pop", "con08_land_area"))),
    by = c("ac_uid", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    census$ac_context |>
      dplyr::select(
        -dplyr::any_of(c(
          "state", "state_no", "pc", "ac", "district_code_2011",
          "district_name_2011", "district_harmonization_group_id",
          "district_relationship_type", "district_change_comparable"
        ))
      ),
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    secc$ac_context |>
      dplyr::select(
        -dplyr::any_of(
          c("state_no", "ac", "ac08_id", "shrug_ac_name")
        )
      ),
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    nes$ac_year |>
      dplyr::select(-dplyr::any_of(c("state_no", "ac"))),
    by = c("ac_uid", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    ac_year_uid = paste(ac_uid, year, sep = "_"),
    census_context_year = 2011L,
    nes_pc_matches_election_pc = dplyr::if_else(
      !is.na(nes_pc) & !is.na(pc),
      nes_pc == pc,
      NA
    )
  ) |>
  dplyr::relocate(
    state, state_no, pc_name, pc, ac_name, ac, year,
    ac_uid, ac_year_uid, pc_cluster_id,
    district_code_2011, district_name_2011,
    district_harmonization_group_id,
    district_relationship_type,
    district_change_comparable,
    manual_xwalk, district_join_success
  ) |>
  dplyr::arrange(state_no, ac, year)

assert_unique_rows(ac_year, c("ac_uid", "year"), "final AC-year panel")

# ------------------------------------------------------------
# AC change file: only quantities that truly vary by election year
# are widened and differenced. Census-2011 and other fixed controls
# appear once.
# ------------------------------------------------------------

id_columns <- c(
  "state", "state_no", "pc_name", "pc", "ac_name", "ac", "ac_uid",
  "pc_cluster_id", "district_code_2011", "district_name_2011",
  "district_harmonization_group_id", "district_relationship_type",
  "district_change_comparable", "manual_xwalk", "district_join_success"
)

fixed_columns <- c(
  "proxy_ac_pop", "proxy_ac_pop_source", "con08_land_area",
  "ac_pop_density_sqkm", "log_ac_pop_density_sqkm",
  "employment_total_ac", "employment_manufacturing_ac", "employment_services_ac",
  "employment_per_total_population", "working_age_population_15plus_ac",
  "working_age_population_15_64_ac", "employment_per_population_15plus",
  "employment_per_population_15_64", "sc_population_ac", "st_population_ac",
  "sc_pop_share", "st_pop_share",
  "mig_total_upto_2001", "log1p_mig_total_upto_2001",
  "mig_total_upto_2001_share_ac_pop", "mig_total_upto_2001_density_sqkm",
  "log1p_mig_total_upto_2001_density_sqkm",
  "log_mig_total_upto_2001_share_ac_pop",
  "mig_total_upto_2001_male_share",
  "male_mig_total_upto_2001", "log1p_male_mig_total_upto_2001",
  "male_mig_total_upto_2001_share_ac_pop", "male_mig_total_upto_2001_density_sqkm",
  "interstate_work_migrants_0_9_n_2011", "interstate_migrants_0_9_n_2011",
  "interstate_work_migrant_share_0_9_2011_dist_proxy",
  "census_context_year"
)

secc_fixed_columns <- names(ac_year)[
  stringr::str_detect(
    names(ac_year),
    paste0(
      "^(secc_|ed_sec_share$|house_tax1_rural_|",
      "log_secc_cons_)"
    )
  )
]

fixed_columns <- unique(
  c(fixed_columns, secc_fixed_columns)
)

census_dated_columns <- names(ac_year)[
  stringr::str_detect(
    names(ac_year),
    "(_2001_|_2011_|_2001$|_2011$|2001_2011)"
  ) &
    !stringr::str_detect(
      names(ac_year),
      "^(mig_|male_mig_|log1p_mig_|log_mig_)"
    )
]

never_change_columns <- unique(c(id_columns, fixed_columns, census_dated_columns))
never_change_columns <- intersect(never_change_columns, names(ac_year))

varying_candidates <- names(ac_year)[
  stringr::str_detect(
    names(ac_year),
    paste0(
      "^(valid_votes|bjp_votes|shs_votes|mns_votes|fr_party_votes|",
      "bjp_vote_share|shs_vote_share|mns_vote_share|fr_party_vote_share|",
      "bjp_candidate_present|shs_candidate_present|mns_candidate_present|",
      "fr_candidate_present|fr_candidate_n|fdi_|any_fdi_|log1p_fdi_|mig_|male_mig_|",
      "log1p_mig_|log_mig_|log1p_male_mig_|nes_.*share)"
    )
  )
]

varying_columns <- setdiff(varying_candidates, c(never_change_columns, "nes_pc_matches_election_pc"))
varying_columns <- varying_columns[vapply(ac_year[varying_columns], is.numeric, logical(1)) |
  vapply(ac_year[varying_columns], is.logical, logical(1))]

wide_levels <- ac_year |>
  dplyr::select(ac_uid, year, dplyr::all_of(varying_columns)) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = dplyr::all_of(varying_columns),
    names_glue = "{.value}_{year}"
  )

change_base <- ac_year |>
  dplyr::filter(year == 2014) |>
  dplyr::select(dplyr::all_of(never_change_columns)) |>
  dplyr::distinct(ac_uid, .keep_all = TRUE)

ac_change <- change_base |>
  dplyr::left_join(wide_levels, by = "ac_uid", relationship = "one-to-one")

for (variable in varying_columns) {
  old_name <- paste0(variable, "_2009")
  new_name <- paste0(variable, "_2014")
  if (!all(c(old_name, new_name) %in% names(ac_change))) next

  is_vote_share <- variable %in% c(
    "bjp_vote_share", "shs_vote_share", "mns_vote_share", "fr_party_vote_share"
  )
  is_proportion <- stringr::str_detect(variable, "share") &&
    !is_vote_share &&
    !stringr::str_detect(variable, "(^log|log1p|_log$|ratio|pct_change)")
  delta_name <- paste0(
    "d_", variable, "_2009_2014",
    if (is_vote_share || is_proportion) "_pp" else ""
  )

  ac_change[[delta_name]] <- if (is_vote_share) {
    # Election vote-share levels are already stored on a 0-100 scale.
    as.numeric(ac_change[[new_name]]) - as.numeric(ac_change[[old_name]])
  } else if (is_proportion) {
    # Other shares are stored on a 0-1 scale and are converted to percentage points.
    safe_pp_change(ac_change[[new_name]], ac_change[[old_name]])
  } else {
    as.numeric(ac_change[[new_name]]) - as.numeric(ac_change[[old_name]])
  }
}

ac_change <- ac_change |>
  dplyr::relocate(dplyr::all_of(id_columns)) |>
  dplyr::arrange(state_no, ac)

assert_unique_rows(ac_change, "ac_uid", "final AC change data")

# ------------------------------------------------------------
# Respondent-level analysis file. No aggregation: each row remains
# one voter with AC-year and district context attached.
# ------------------------------------------------------------

nes_context_exclusions <- c(
  id_columns, "year", "ac_year_uid",
  names(ac_year)[stringr::str_detect(names(ac_year), "^nes_")],
  "survey_weight_validated", "nes_pc_matches_election_pc"
)

context_columns <- setdiff(names(ac_year), nes_context_exclusions)
respondent_context <- ac_year |>
  dplyr::select(ac_uid, year, dplyr::all_of(context_columns))

nes_respondent_analysis <- nes$respondents |>
  dplyr::left_join(
    respondent_context,
    by = c("ac_uid", "year"),
    relationship = "many-to-one",
    suffix = c("", "_context")
  ) |>
  dplyr::left_join(
    ac_year |>
      dplyr::select(ac_uid, year, state, pc_name, ac_name),
    by = c("ac_uid", "year"),
    relationship = "many-to-one"
  ) |>
  dplyr::relocate(
    year, respondent_uid, state, state_no, pc_name, pc, ac_name, ac,
    ac_uid, ac_year_uid, pc_cluster_id, polling_station_id, psu_uid,
    district_code_2011, district_name_2011,
    district_harmonization_group_id,
    district_change_comparable, district_join_success
  )

assert_unique_rows(nes_respondent_analysis, "respondent_uid", "respondent analysis data")

# ------------------------------------------------------------
# Secondary AC-year-ideology summary, joined to contextual exposure.
# ------------------------------------------------------------

ideology_context_columns <- setdiff(
  names(ac_year),
  c(
    names(nes$ideology_cells),
    names(ac_year)[stringr::str_detect(names(ac_year), "^nes_")],
    "survey_weight_validated", "nes_pc_matches_election_pc"
  )
)

ideology_context <- ac_year |>
  dplyr::select(ac_uid, year, dplyr::all_of(ideology_context_columns))

ac_year_ideology_summary <- nes$ideology_cells |>
  dplyr::left_join(
    ideology_context,
    by = c("ac_uid", "year"),
    relationship = "many-to-one",
    suffix = c("", "_context")
  ) |>
  dplyr::arrange(state_no, ac, year, ideology)

assert_unique_rows(
  ac_year_ideology_summary,
  c("ac_uid", "year", "ideology"),
  "AC-year-ideology summary"
)

# ------------------------------------------------------------
# Final exports
# ------------------------------------------------------------

write_rds_csv(ac_year, file.path(paths$final_dir, "ac_year"), c("ac_uid", "year"))
write_rds_csv(ac_change, file.path(paths$final_dir, "ac_change"), "ac_uid")
write_rds_csv(
  nes_respondent_analysis,
  file.path(paths$final_dir, "nes_respondent_analysis"),
  "respondent_uid"
)
write_rds_csv(
  ac_year_ideology_summary,
  file.path(paths$final_dir, "ac_year_ideology_summary"),
  c("ac_uid", "year", "ideology")
)

# ------------------------------------------------------------
# Final diagnostics
# ------------------------------------------------------------

final_key_diagnostics <- tibble::tribble(
  ~file, ~expected_key, ~rows, ~distinct_keys, ~missing_key_rows,
  "ac_year.csv", "ac_uid + year", nrow(ac_year), dplyr::n_distinct(paste(ac_year$ac_uid, ac_year$year)), sum(is.na(ac_year$ac_uid) | is.na(ac_year$year)),
  "ac_change.csv", "ac_uid", nrow(ac_change), dplyr::n_distinct(ac_change$ac_uid), sum(is.na(ac_change$ac_uid)),
  "nes_respondent_analysis.csv", "respondent_uid", nrow(nes_respondent_analysis), dplyr::n_distinct(nes_respondent_analysis$respondent_uid), sum(is.na(nes_respondent_analysis$respondent_uid)),
  "ac_year_ideology_summary.csv", "ac_uid + year + ideology", nrow(ac_year_ideology_summary), dplyr::n_distinct(paste(ac_year_ideology_summary$ac_uid, ac_year_ideology_summary$year, ac_year_ideology_summary$ideology)), sum(is.na(ac_year_ideology_summary$ac_uid) | is.na(ac_year_ideology_summary$year) | is.na(ac_year_ideology_summary$ideology))
) |>
  dplyr::mutate(
    duplicate_rows = rows - distinct_keys,
    passed = duplicate_rows == 0 & missing_key_rows == 0
  )

final_join_diagnostics <- tibble::tribble(
  ~target_file, ~source_module, ~join_key, ~rows_before, ~rows_after, ~matched_rows,
  "ac_year.csv", "FDI", "ac_uid + year", nrow(elections$ac_year), nrow(ac_year), sum(!is.na(ac_year$fdi_total_local_all_n)),
  "ac_year.csv", "Migration", "ac_uid + year", nrow(elections$ac_year), nrow(ac_year), sum(!is.na(ac_year$mig_recent_5yr_total)),
  "ac_year.csv", "Census context", "ac_uid", nrow(elections$ac_year), nrow(ac_year), sum(!is.na(ac_year$muslim_share_2011_dist_proxy)),
  "ac_year.csv", "SHRUG SECC", "ac_uid", nrow(elections$ac_year), nrow(ac_year), sum(!is.na(ac_year$secc_context_year)),
  "ac_year.csv", "NES AC summaries", "ac_uid + year", nrow(elections$ac_year), nrow(ac_year), sum(!is.na(ac_year$nes_n_respondents)),
  "nes_respondent_analysis.csv", "AC-year context", "ac_uid + year", nrow(nes$respondents), nrow(nes_respondent_analysis), sum(!is.na(nes_respondent_analysis$fr_party_vote_share))
) |>
  dplyr::mutate(
    unmatched_rows = rows_after - matched_rows,
    match_rate = safe_share(matched_rows, rows_after),
    duplicate_key_rows = rows_after - rows_before,
    passed = rows_before == rows_after
  )

final_missingness <- dplyr::bind_rows(
  summarize_missingness(ac_year, "ac_year.csv", "year"),
  summarize_missingness(ac_change, "ac_change.csv"),
  summarize_missingness(nes_respondent_analysis, "nes_respondent_analysis.csv", "year"),
  summarize_missingness(ac_year_ideology_summary, "ac_year_ideology_summary.csv", "year")
)

sample_coverage_by_state_year <- dplyr::bind_rows(
  ac_year |>
    dplyr::summarise(
      file = "ac_year.csv", n_rows = dplyr::n(), n_acs = dplyr::n_distinct(ac_uid),
      n_pcs = dplyr::n_distinct(pc_cluster_id),
      n_district_groups = dplyr::n_distinct(district_harmonization_group_id, na.rm = TRUE),
      n_respondents = sum(nes_n_respondents, na.rm = TRUE),
      .by = c(year, state_no, state)
    ),
  nes_respondent_analysis |>
    dplyr::summarise(
      file = "nes_respondent_analysis.csv", n_rows = dplyr::n(), n_acs = dplyr::n_distinct(ac_uid),
      n_pcs = dplyr::n_distinct(pc_cluster_id),
      n_district_groups = dplyr::n_distinct(district_harmonization_group_id, na.rm = TRUE),
      n_respondents = dplyr::n(),
      .by = c(year, state_no, state)
    )
)

write_csv_checked(final_key_diagnostics, file.path(paths$diagnostic_dir, "final_key_diagnostics.csv"))
write_csv_checked(final_join_diagnostics, file.path(paths$diagnostic_dir, "final_join_diagnostics.csv"))
write_csv_checked(final_missingness, file.path(paths$diagnostic_dir, "final_missingness.csv"))
write_csv_checked(sample_coverage_by_state_year, file.path(paths$diagnostic_dir, "sample_coverage_by_state_year.csv"))

# ------------------------------------------------------------
# Variable dictionary and source-geography dictionary
# ------------------------------------------------------------

infer_role <- function(variable) {
  dplyr::case_when(
    stringr::str_detect(variable, "(^|_)uid$|^state$|^state_no$|^pc$|^ac$|^year$") ~ "identifier",
    stringr::str_detect(variable, "fr_party_vote_share|voted_fr|close_any_fr|fr_candidate_present") ~ "outcome",
    stringr::str_detect(variable, "fdi_") ~ "economic exposure",
    stringr::str_detect(variable, "mig_|muslim|nonlocal_language") ~ "demographic exposure",
    stringr::str_detect(variable, "male_share|work_migrant|education|ed_sec_share") ~ "composition",
    stringr::str_detect(variable, "^secc_|house_tax1|log_secc_cons") ~ "socioeconomic control",
    stringr::str_detect(variable, "ideology") ~ "effect modifier",
    stringr::str_detect(variable, "candidate|join|validated|missing|manual") ~ "diagnostic",
    TRUE ~ "control"
  )
}

infer_unit <- function(variable) {
  dplyr::case_when(
    stringr::str_ends(variable, "_pp") ~ "percentage points",
    stringr::str_detect(variable, "pct_change") ~ "percent change",
    stringr::str_detect(variable, "ratio_points") ~ "ratio points",
    stringr::str_detect(variable, "pc100k") ~ "per 100,000 residents",
    stringr::str_detect(variable, "density_sqkm") ~ "per square kilometre",
    stringr::str_detect(variable, "vote_share") ~ "percent",
    stringr::str_detect(variable, "log") ~ "log units",
    stringr::str_detect(variable, "share|rate|per_population") ~ "proportion",
    stringr::str_detect(variable, "_n$|population|votes|respondents|migrants|employment_total") ~ "count",
    TRUE ~ "source units"
  )
}

infer_source_geography <- function(variable) {
  dplyr::case_when(
    stringr::str_detect(variable, "^respondent_|^reported_vote|^vote_valid$|^voted_|^close_|^never_vote|^education_|^income_|^caste_|^religion_|^survey_weight|ideology") ~ "survey respondent or NES AC sample",
    stringr::str_detect(variable, "fdi_|election|vote_share|candidate") ~ "assembly constituency",
    stringr::str_detect(variable, "^secc_|^ed_sec_share$|house_tax1|log_secc_cons") ~
      "SHRUG 2008 assembly constituency",
    stringr::str_detect(variable, "muslim|hindu|language|higher_secondary|graduate_plus|work_migrant") ~ dplyr::if_else(
      stringr::str_detect(variable, "2001_2011|_2001"),
      "district lineage group attached to AC",
      "2011 district attached to AC"
    ),
    stringr::str_detect(variable, "migration|mig_") ~ "district counts allocated to AC",
    TRUE ~ "assembly constituency or allocated district source"
  )
}

infer_source_file <- function(variable) {
  dplyr::case_when(
    stringr::str_detect(variable, "fdi_") ~ "data/IN_FDI_2004_2014.csv",
    stringr::str_detect(variable, "^secc_|^ed_sec_share$|house_tax1|log_secc_cons") ~
      "data/shrug/secc_*_con08.dta",
    stringr::str_detect(variable, "vote_share|votes|candidate_present|candidate_n") ~ "data/election/lok_dhaba_ge.csv",
    stringr::str_detect(variable, "^mig_|^male_mig_|^log1p_mig_|^log_mig_") ~ "data/pop/migration/ (Census D-02)",
    stringr::str_detect(variable, "work_migrant|interstate_migrants_0_9") ~ "data/pop/migration_employment_2011/ (Census D-07) and D-02",
    stringr::str_detect(variable, "muslim|hindu") ~ "data/pop/religion_2001/ and religion_2011/ (Census C-01)",
    stringr::str_detect(variable, "language") ~ "data/pop/language_2001/ and language_2011/ (Census C-16)",
    stringr::str_detect(variable, "^sc_.*education|sc_.*higher|sc_.*graduate") ~ "data/pop/sc_education_2001/ and sc_education_2011/ (Census C-08SC)",
    stringr::str_detect(variable, "^st_.*education|st_.*higher|st_.*graduate") ~ "data/pop/st_education_2001/ and st_education_2011/ (Census C-08ST)",
    stringr::str_detect(variable, "employment") ~ "data/shrug/ec13_pc11dist.csv",
    stringr::str_detect(variable, "proxy_ac_pop|land_area|pop_density") ~ "data/shrug/con08_pop_area_key.csv and 2011 Census population tables",
    stringr::str_detect(variable, "voter_ideology|voted_|close_|never_vote|education_|income_|caste_|religion_|survey_weight") ~ "data/lokniti/nes_2009.sav and nes_2014.sav",
    TRUE ~ NA_character_
  )
}

infer_definition <- function(variable) {
  dplyr::case_when(
    variable == "fr_party_vote_share" ~ "100 × (BJP + SHS + MNS votes) / valid votes; zero when no far-right candidate runs",
    variable == "any_fdi_total_own_all" ~ "1 when at least one FDI project is located inside the AC in the election exposure window",
    variable == "any_fdi_total_adjacent_all" ~ "1 when at least one FDI project is located in a touching AC in the election exposure window",
    variable == "any_fdi_total_local_all" ~ "1 when at least one FDI project is located inside or adjacent to the AC in the election exposure window",
    variable == "ed_sec_share" ~ "Population-weighted combination of rural and urban SECC secondary-school-or-above shares",
    variable == "secc_hh" ~ "Sum of available rural and urban SECC household counts",
    variable == "house_tax1_rural_n" ~ "Number of rural households paying income tax or professional tax",
    variable == "house_tax1_rural_share" ~ "Rural tax-paying households divided by rural SECC households",
    variable == "secc_cons_pc" ~ "Population-weighted rural and urban annual per-capita consumption in 2012 INR, using available components",
    variable == "secc_cons_pc_complete" ~ "Population-weighted rural and urban annual per-capita consumption; mixed ACs require both sector estimates",
    variable == "secc_cons_hh" ~ "Household-count-weighted rural and urban annual household consumption in 2012 INR, using available components",
    variable == "secc_cons_hh_complete" ~ "Household-count-weighted annual household consumption; mixed ACs require both sector estimates",
    variable == "voted_fr" ~ "1 when the respondent reports voting BJP, SHS, or MNS; 0 for another valid party response",
    variable == "close_any_fr" ~ "1 when the respondent reports closeness to BJP, SHS, or MNS; 0 for a valid non-far-right closeness response",
    stringr::str_detect(variable, "^fdi_") & stringr::str_ends(variable, "_n") ~ "Number of distinct FDI projects in the named sector, scope, status, and election exposure window",
    stringr::str_detect(variable, "^fdi_.*_pc100k$") ~ "FDI project count per 100,000 AC residents",
    stringr::str_detect(variable, "^log1p_fdi_") ~ "log(1 + FDI projects per 100,000 AC residents)",
    variable == "mig_total_upto_2001" ~ "Interstate plus international migrants with 10–19 or 20+ years residence in the 2011 Census",
    stringr::str_detect(variable, "mig_accel_.*_ratio") ~ "Ratio of the migration windows named in the variable; missing when the denominator is zero",
    stringr::str_detect(variable, "mig_accel_.*_pct_change") ~ "100 × (migration-window ratio − 1); missing when the denominator is zero",
    stringr::str_detect(variable, "mig_accel_.*_log1p") ~ "Difference between log(1 + migration counts) in the named windows",
    stringr::str_detect(variable, "mig_accel_.*_log$") ~ "Log ratio of the migration windows named in the variable",
    variable == "d_muslim_share_2001_2011_pp" ~ "100 × (2011 Muslim population share − 2001 Muslim population share), after common-district aggregation",
    variable == "d_nonlocal_language_share_common_2001_2011_pp" ~ "100 × (2011 − 2001) non-local-language share using the same valid language set in both years",
    variable == "employment_per_total_population" ~ "Allocated 2013 Economic Census employment divided by 2011 AC population",
    variable == "employment_per_population_15plus" ~ "Allocated 2013 Economic Census employment divided by allocated 2011 population age 15+; available only with C-13",
    stringr::str_starts(variable, "d_") & stringr::str_ends(variable, "_pp") ~ "Later share minus earlier share, expressed in percentage points",
    stringr::str_starts(variable, "d_") ~ "Later value minus earlier value in the units of the underlying variable",
    TRUE ~ NA_character_
  )
}

make_dictionary <- function(data, file_name) {
  tibble::tibble(variable = names(data)) |>
    dplyr::mutate(
      file = file_name,
      label = stringr::str_to_sentence(stringr::str_replace_all(variable, "_", " ")),
      type = vapply(data, function(x) class(x)[1], character(1)),
      unit = infer_unit(variable),
      level = dplyr::case_when(
        file_name == "nes_respondent_analysis.csv" ~ "respondent",
        file_name == "ac_year_ideology_summary.csv" ~ "AC-year-ideology",
        file_name == "ac_change.csv" ~ "AC",
        TRUE ~ "AC-year"
      ),
      source_file = infer_source_file(variable),
      source_table = dplyr::case_when(
        stringr::str_detect(variable, "muslim|hindu") ~ "C-01",
        stringr::str_detect(variable, "language") ~ "C-16",
        stringr::str_detect(variable, "work_migrant") ~ "D-07",
        stringr::str_detect(variable, "^mig_|^male_mig_|^log1p_mig_|^log_mig_") ~ "D-02",
        stringr::str_detect(variable, "higher_secondary|graduate_plus") ~ "C-08SC/C-08ST",
        stringr::str_detect(variable, "^secc_|^ed_sec_share$|house_tax1|log_secc_cons") ~
          "SHRUG SECC 2012 / SECC consumption",
        TRUE ~ NA_character_
      ),
      source_geography = infer_source_geography(variable),
      time_reference = dplyr::case_when(
        stringr::str_detect(variable, "2001_2011") ~ "2001 to 2011",
        stringr::str_detect(variable, "_2001") ~ "2001",
        stringr::str_detect(variable, "_2011") ~ "2011",
        TRUE ~ "row year or fixed source year documented in notes"
      ),
      formula_or_definition = infer_definition(variable),
      missing_meaning = "Unavailable, inapplicable, or failed source/geographic match; see diagnostics",
      main_role = infer_role(variable),
      notes = dplyr::case_when(
        stringr::str_detect(variable, "sc_.*age20plus_2001|st_.*age20plus_2001|sc_.*age25plus_2001|st_.*age25plus_2001") ~
          "Unavailable from supplied 2001 C-08 appendix files because they lack age-specific rows",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      file, variable, label, type, unit, level, source_file, source_table,
      source_geography, time_reference, formula_or_definition,
      missing_meaning, main_role, notes
    )
}

data_dictionary <- dplyr::bind_rows(
  make_dictionary(ac_year, "ac_year.csv"),
  make_dictionary(ac_change, "ac_change.csv"),
  make_dictionary(nes_respondent_analysis, "nes_respondent_analysis.csv"),
  make_dictionary(ac_year_ideology_summary, "ac_year_ideology_summary.csv")
)

source_geography_dictionary <- tibble::tribble(
  ~variable_family, ~source_geography, ~analysis_geography, ~allocation_method, ~harmonization_method, ~repeated_within, ~interpretation_note,
  "Election outcomes", "AC segment", "AC-year", "none", "post-2008 boundaries", "none", "Direct AC electoral outcome",
  "FDI", "project coordinates", "AC-year", "point-in-polygon plus touching ACs", "post-2008 AC geography", "projects may expose multiple neighboring ACs", "Own, adjacent, and local project exposure",
  "Migration", "2011 district", "AC-year", "district counts allocated using AC population shares", "2011 districts", "district", "AC proxy exposure, not directly observed AC migration",
  "Religion/language/SC-ST education", "2001 and 2011 districts", "AC", "2011 district levels attached directly; 2001 baselines and changes attached after group aggregation", "common district-lineage groups for change only", "2011 district or district lineage group", "Current levels use actual 2011 districts; historical baselines and changes use comparable lineage groups",
  "Employment", "2013 district", "AC", "district counts allocated using AC population shares", "2011 districts", "district", "Establishment employment intensity, not conventional resident employment rate",
  "SHRUG SECC", "SHRUG 2008 AC", "AC", "direct join on ac08_id", "post-2008 assembly constituency geography", "AC", "Rural and urban components retained; combined consumption uses population or household weights",
  "NES", "respondent", "respondent", "none", "joined by AC and year", "AC-year context", "Individual outcome and ideology with contextual exposures"
)

write_csv_checked(data_dictionary, file.path(paths$final_dir, "data_dictionary.csv"), c("file", "variable"))
write_csv_checked(source_geography_dictionary, file.path(paths$final_dir, "source_geography_dictionary.csv"), "variable_family")

# ------------------------------------------------------------
# Diagnostic figures for contextual distributions
# ------------------------------------------------------------

save_distribution_plot <- function(data, variable, title, file_name, facet_year = TRUE) {
  if (!variable %in% names(data)) return(invisible(NULL))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(bins = 40, na.rm = TRUE) +
    ggplot2::labs(title = title, x = variable, y = "AC-years") +
    ggplot2::theme_minimal()
  if (facet_year && "year" %in% names(data)) plot <- plot + ggplot2::facet_wrap(~year, scales = "free_y")
  ggplot2::ggsave(file.path(paths$diagnostic_dir, file_name), plot, width = 9, height = 5, dpi = 300)
}

save_distribution_plot(
  ac_year,
  "log1p_fdi_total_local_all_pc100k",
  "Local FDI exposure by election year",
  "fdi_distribution_by_year_scope_status.png"
)
save_distribution_plot(
  ac_year,
  "mig_recent_5yr_total",
  "Recent migration exposure by election year",
  "migration_distribution_by_year.png"
)
save_distribution_plot(
  ac_year,
  "mig_accel_recent_vs_prior5_pct_change",
  "Migration acceleration by election year",
  "migration_acceleration_distribution_by_year.png"
)

fdi_zero_plot <- ac_year |>
  dplyr::summarise(
    zero_share = mean(fdi_total_local_all_n == 0, na.rm = TRUE),
    .by = year
  ) |>
  ggplot2::ggplot(ggplot2::aes(factor(year), zero_share)) +
  ggplot2::geom_col() +
  ggplot2::scale_y_continuous(labels = scales::label_percent()) +
  ggplot2::labs(title = "ACs with zero local FDI exposure", x = NULL, y = "Share of ACs") +
  ggplot2::theme_minimal()
ggplot2::ggsave(
  file.path(paths$diagnostic_dir, "fdi_zero_exposure_by_year.png"),
  fdi_zero_plot,
  width = 7,
  height = 5,
  dpi = 300
)

census_change_variables <- intersect(
  c(
    "d_muslim_share_2001_2011_pp",
    "d_nonlocal_language_share_common_2001_2011_pp",
    "d_sc_higher_secondary_plus_share_age20plus_2001_2011_pp",
    "d_st_higher_secondary_plus_share_age20plus_2001_2011_pp"
  ),
  names(ac_year)
)
if (length(census_change_variables) > 0) {
  census_change_plot_data <- ac_year |>
    dplyr::filter(year == 2014) |>
    dplyr::select(dplyr::all_of(census_change_variables)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value")
  census_change_plot <- ggplot2::ggplot(census_change_plot_data, ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = 35, na.rm = TRUE) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(title = "Census contextual change variables", x = NULL, y = "ACs") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(
    file.path(paths$diagnostic_dir, "census_change_variable_distributions.png"),
    census_change_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
}

missingness_plot_data <- final_missingness |>
  dplyr::filter(file %in% c("ac_year.csv", "nes_respondent_analysis.csv")) |>
  dplyr::mutate(variable = forcats::fct_reorder(variable, pct_missing))
missingness_plot <- ggplot2::ggplot(
  missingness_plot_data,
  ggplot2::aes(year, variable, fill = pct_missing)
) +
  ggplot2::geom_tile() +
  ggplot2::facet_wrap(~file, scales = "free_y") +
  ggplot2::labs(title = "Final-data missingness", x = "Year", y = NULL, fill = "% missing") +
  ggplot2::theme_minimal()
ggplot2::ggsave(
  file.path(paths$diagnostic_dir, "final_missingness_heatmap.png"),
  missingness_plot,
  width = 12,
  height = 14,
  dpi = 300,
  limitsize = FALSE
)

# ------------------------------------------------------------
# Build manifest
# ------------------------------------------------------------

output_files <- list.files(paths$derived_dir, recursive = TRUE, full.names = TRUE)
manifest <- make_output_manifest(output_files, project_root) |>
  dplyr::mutate(
    display_file = basename(file),
    rows = purrr::map_int(file, function(path) {
      if (!stringr::str_ends(path, "\\.csv$")) return(NA_integer_)
      tryCatch(nrow(readr::read_csv(path, show_col_types = FALSE)), error = function(e) NA_integer_)
    }),
    columns = purrr::map_int(file, function(path) {
      if (!stringr::str_ends(path, "\\.csv$")) return(NA_integer_)
      tryCatch(ncol(readr::read_csv(path, n_max = 1, show_col_types = FALSE)), error = function(e) NA_integer_)
    })
  ) |>
  dplyr::mutate(key = NA_character_) |>
  dplyr::select(file = display_file, relative_path, rows, columns, key, generated_at, file_size_bytes, file_hash)

write_csv_checked(manifest, file.path(paths$derived_dir, "output_manifest.csv"), "relative_path")

message("Build complete. Final datasets are in: ", paths$final_dir)

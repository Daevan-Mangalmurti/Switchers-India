suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

intermediate_dir <-
  file.path(
    project_root,
    "data",
    "derived",
    "switchers_rewrite",
    "intermediate"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_district_allocation_audit_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

allocation <-
  read_csv(
    file.path(
      intermediate_dir,
      "ac_allocation_weights.csv"
    ),
    show_col_types = FALSE
  )

migration_district <-
  read_csv(
    file.path(
      intermediate_dir,
      "migration_district_clean.csv"
    ),
    show_col_types = FALSE
  )

migration_ac_year <-
  read_csv(
    file.path(
      intermediate_dir,
      "migration_ac_year.csv"
    ),
    show_col_types = FALSE
  )

demographics <-
  read_csv(
    file.path(
      intermediate_dir,
      "demographics_ac.csv"
    ),
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

if (
  !"AC01" %in%
    names(
      ac_samples
    )
) {
  stop(
    "Canonical AC01 sample is unavailable."
  )
}

required_allocation <-
  c(
    "ac_uid",
    "state_no",
    "district_code_2011",
    "proxy_ac_pop",
    "district_pop_2011",
    "ac_alloc_share"
  )

required_migration_district <-
  c(
    "state_no",
    "district_code_2011",
    "mig_total_district",
    "mig_10_19_district",
    "mig_gt20_district",
    "male_mig_10_19_district",
    "male_mig_gt20_district"
  )

required_migration_year <-
  c(
    "ac_uid",
    "year",
    "mig_prior_5yr_total",
    "male_mig_prior_5yr_total"
  )

required_change <-
  c(
    "ac_uid",
    "mig_total_upto_2001_share_ac_pop",
    "male_mig_total_upto_2001_share_ac_pop",
    "d_mig_prior_5yr_share_ac_pop_2009_2014_pp",
    "d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp",
    "employment_per_total_population"
  )

checks <-
  list(
    allocation =
      setdiff(
        required_allocation,
        names(
          allocation
        )
      ),
    migration_district =
      setdiff(
        required_migration_district,
        names(
          migration_district
        )
      ),
    migration_ac_year =
      setdiff(
        required_migration_year,
        names(
          migration_ac_year
        )
      ),
    ac_change =
      setdiff(
        required_change,
        names(
          ac_change
        )
      )
  )

for (
  nm in
    names(
      checks
    )
) {
  if (
    length(
      checks[[
        nm
      ]]
    ) >
      0L
  ) {
    stop(
      nm,
      " is missing: ",
      paste(
        checks[[
          nm
        ]],
        collapse = ", "
      )
    )
  }
}

if (
  anyDuplicated(
    allocation$ac_uid
  ) >
    0L
) {
  stop(
    "Allocation is not unique by ac_uid."
  )
}

district_population <-
  allocation |>
  filter(
    !is.na(
      district_code_2011
    )
  ) |>
  group_by(
    state_no,
    district_code_2011
  ) |>
  summarise(
    n_assigned_ac =
      n(),

    sum_assigned_ac_proxy_pop =
      sum(
        proxy_ac_pop,
        na.rm = TRUE
      ),

    district_pop_2011 =
      first(
        district_pop_2011[
          is.finite(
            district_pop_2011
          )
        ]
      ),

    allocation_share_sum =
      sum(
        ac_alloc_share,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  ) |>
  mutate(
    assigned_population_coverage =
      sum_assigned_ac_proxy_pop /
      district_pop_2011,

    denominator_inflation_factor =
      district_pop_2011 /
      sum_assigned_ac_proxy_pop
  )

coverage_summary <-
  district_population |>
  filter(
    is.finite(
      assigned_population_coverage
    )
  ) |>
  summarise(
    n_districts =
      n(),

    coverage_min =
      min(
        assigned_population_coverage
      ),

    coverage_p01 =
      quantile(
        assigned_population_coverage,
        .01,
        names = FALSE,
        type = 8
      ),

    coverage_p05 =
      quantile(
        assigned_population_coverage,
        .05,
        names = FALSE,
        type = 8
      ),

    coverage_p25 =
      quantile(
        assigned_population_coverage,
        .25,
        names = FALSE,
        type = 8
      ),

    coverage_median =
      median(
        assigned_population_coverage
      ),

    coverage_p75 =
      quantile(
        assigned_population_coverage,
        .75,
        names = FALSE,
        type = 8
      ),

    coverage_p95 =
      quantile(
        assigned_population_coverage,
        .95,
        names = FALSE,
        type = 8
      ),

    coverage_p99 =
      quantile(
        assigned_population_coverage,
        .99,
        names = FALSE,
        type = 8
      ),

    coverage_max =
      max(
        assigned_population_coverage
      ),

    n_below_050 =
      sum(
        assigned_population_coverage <
          .50
      ),

    n_below_080 =
      sum(
        assigned_population_coverage <
          .80
      ),

    n_outside_080_120 =
      sum(
        assigned_population_coverage <
          .80 |
          assigned_population_coverage >
            1.20
      ),

    n_outside_090_110 =
      sum(
        assigned_population_coverage <
          .90 |
          assigned_population_coverage >
            1.10
      ),

    n_above_120 =
      sum(
        assigned_population_coverage >
          1.20
      ),

    n_above_150 =
      sum(
        assigned_population_coverage >
          1.50
      )
  )

district_established <-
  migration_district |>
  transmute(
    state_no,
    district_code_2011,

    established_migrants_district =
      mig_10_19_district +
      mig_gt20_district,

    established_male_migrants_district =
      male_mig_10_19_district +
      male_mig_gt20_district,

    all_migrants_district =
      mig_total_district
  ) |>
  left_join(
    district_population,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    established_migrant_share_district =
      established_migrants_district /
      district_pop_2011,

    established_male_migrant_share_district =
      established_male_migrants_district /
      district_pop_2011,

    all_migrant_share_district =
      all_migrants_district /
      district_pop_2011
  )

if (
  any(
    district_established$established_migrant_share_district >
      1,
    na.rm = TRUE
  ) ||
    any(
      district_established$established_migrant_share_district <
        0,
      na.rm = TRUE
    )
) {
  stop(
    "District-consistent established migrant share lies outside [0,1]."
  )
}

migration_window_district <-
  migration_ac_year |>
  left_join(
    allocation |>
      select(
        ac_uid,
        state_no_allocation =
          state_no,
        district_code_2011
      ),
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  )

if (
  any(
    migration_window_district$state_no !=
      migration_window_district$state_no_allocation,
    na.rm = TRUE
  )
) {
  stop(
    "State mismatch between migration data and allocation."
  )
}

migration_window_district <-
  migration_window_district |>
  group_by(
    state_no,
    district_code_2011,
    year
  ) |>
  summarise(
    district_prior_5yr_migrants =
      if (
        all(
          is.na(
            mig_prior_5yr_total
          )
        )
      ) {
        NA_real_
      } else {
        sum(
          mig_prior_5yr_total,
          na.rm = TRUE
        )
      },

    district_prior_5yr_male_migrants =
      if (
        all(
          is.na(
            male_mig_prior_5yr_total
          )
        )
      ) {
        NA_real_
      } else {
        sum(
          male_mig_prior_5yr_total,
          na.rm = TRUE
        )
      },

    .groups =
      "drop"
  ) |>
  left_join(
    district_population |>
      select(
        state_no,
        district_code_2011,
        district_pop_2011
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  ) |>
  mutate(
    district_prior_5yr_share =
      district_prior_5yr_migrants /
      district_pop_2011,

    district_prior_5yr_male_share =
      district_prior_5yr_male_migrants /
      district_pop_2011
  )

window_wide <-
  migration_window_district |>
  filter(
    year %in%
      c(
        2009,
        2014
      )
  ) |>
  select(
    state_no,
    district_code_2011,
    year,
    district_prior_5yr_share,
    district_prior_5yr_male_share
  ) |>
  pivot_wider(
    names_from =
      year,
    values_from =
      c(
        district_prior_5yr_share,
        district_prior_5yr_male_share
      ),
    names_glue =
      "{.value}_{year}"
  ) |>
  mutate(
    d_district_prior_5yr_share_2009_2014_pp =
      100 *
      (
        district_prior_5yr_share_2014 -
          district_prior_5yr_share_2009
      ),

    d_district_prior_5yr_male_share_2009_2014_pp =
      100 *
      (
        district_prior_5yr_male_share_2014 -
          district_prior_5yr_male_share_2009
      )
  )

corrected_context_ac <-
  allocation |>
  select(
    ac_uid,
    state_no,
    district_code_2011,
    proxy_ac_pop,
    district_pop_2011,
    ac_alloc_share
  ) |>
  left_join(
    district_established |>
      select(
        state_no,
        district_code_2011,
        established_migrant_share_district,
        established_male_migrant_share_district,
        all_migrant_share_district
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  ) |>
  left_join(
    window_wide,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  ) |>
  left_join(
    ac_change |>
      select(
        ac_uid,
        old_established_ac_share =
          mig_total_upto_2001_share_ac_pop,
        old_established_male_ac_share =
          male_mig_total_upto_2001_share_ac_pop,
        old_prior5_change_pp =
          d_mig_prior_5yr_share_ac_pop_2009_2014_pp,
        old_male_prior5_change_pp =
          d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp,
        old_employment_rate =
          employment_per_total_population
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

migration_comparison_summary <-
  corrected_context_ac |>
  summarise(
    n_ac =
      n(),

    n_old_established_gt1 =
      sum(
        old_established_ac_share >
          1,
        na.rm = TRUE
      ),

    n_new_established_gt1 =
      sum(
        established_migrant_share_district >
          1,
        na.rm = TRUE
      ),

    correlation_old_new_established =
      cor(
        old_established_ac_share,
        established_migrant_share_district,
        use =
          "complete.obs"
      ),

    correlation_old_new_established_male =
      cor(
        old_established_male_ac_share,
        established_male_migrant_share_district,
        use =
          "complete.obs"
      ),

    correlation_old_new_prior5_change =
      cor(
        old_prior5_change_pp,
        d_district_prior_5yr_share_2009_2014_pp,
        use =
          "complete.obs"
      ),

    correlation_old_new_male_prior5_change =
      cor(
        old_male_prior5_change_pp,
        d_district_prior_5yr_male_share_2009_2014_pp,
        use =
          "complete.obs"
      ),

    max_abs_established_difference =
      max(
        abs(
          old_established_ac_share -
            established_migrant_share_district
        ),
        na.rm = TRUE
      ),

    max_abs_prior5_change_difference =
      max(
        abs(
          old_prior5_change_pp -
            d_district_prior_5yr_share_2009_2014_pp
        ),
        na.rm = TRUE
      )
  )

employment_path <-
  file.path(
    project_root,
    "data",
    "shrug",
    "ec13_pc11dist.csv"
  )

if (
  !file.exists(
    employment_path
  )
) {
  stop(
    "Economic Census file is missing."
  )
}

employment_district <-
  read_csv(
    employment_path,
    show_col_types = FALSE
  ) |>
  transmute(
    state_no =
      as.integer(
        pc11_state_id
      ),

    district_code_2011 =
      as.integer(
        pc11_district_id
      ),

    employment_total_district =
      as.numeric(
        ec13_emp_all
      )
  ) |>
  distinct(
    state_no,
    district_code_2011,
    .keep_all = TRUE
  ) |>
  left_join(
    district_population |>
      select(
        state_no,
        district_code_2011,
        district_pop_2011,
        assigned_population_coverage
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    district_consistent_employment_rate =
      employment_total_district /
      district_pop_2011
  )

employment_comparison <-
  corrected_context_ac |>
  select(
    ac_uid,
    state_no,
    district_code_2011,
    old_employment_rate
  ) |>
  left_join(
    employment_district |>
      select(
        state_no,
        district_code_2011,
        district_consistent_employment_rate,
        assigned_population_coverage
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  )

employment_summary <-
  employment_comparison |>
  summarise(
    n_complete =
      sum(
        is.finite(
          old_employment_rate
        ) &
          is.finite(
            district_consistent_employment_rate
          )
      ),

    old_min =
      min(
        old_employment_rate,
        na.rm = TRUE
      ),

    old_median =
      median(
        old_employment_rate,
        na.rm = TRUE
      ),

    old_max =
      max(
        old_employment_rate,
        na.rm = TRUE
      ),

    new_min =
      min(
        district_consistent_employment_rate,
        na.rm = TRUE
      ),

    new_median =
      median(
        district_consistent_employment_rate,
        na.rm = TRUE
      ),

    new_max =
      max(
        district_consistent_employment_rate,
        na.rm = TRUE
      ),

    n_old_gt1 =
      sum(
        old_employment_rate >
          1,
        na.rm = TRUE
      ),

    n_new_gt1 =
      sum(
        district_consistent_employment_rate >
          1,
        na.rm = TRUE
      ),

    correlation_old_new =
      cor(
        old_employment_rate,
        district_consistent_employment_rate,
        use =
          "complete.obs"
      ),

    max_abs_difference =
      max(
        abs(
          old_employment_rate -
            district_consistent_employment_rate
        ),
        na.rm = TRUE
      )
  )

source_columns <-
  intersect(
    c(
      "sc_population_source",
      "st_population_source"
    ),
    names(
      demographics
    )
  )

if (
  length(
    source_columns
  ) >
    0L
) {
  sc_st_source_overall <-
    demographics |>
    select(
      ac_uid,
      all_of(
        source_columns
      )
    ) |>
    pivot_longer(
      cols =
        all_of(
          source_columns
        ),
      names_to =
        "measure",
      values_to =
        "source"
    ) |>
    count(
      measure,
      source,
      name =
        "n_ac"
    )
} else {
  sc_st_source_overall <-
    tibble(
      measure =
        character(),
      source =
        character(),
      n_ac =
        integer()
    )
}

canonical_ids <-
  ac_samples[[
    "AC01"
  ]] |>
  distinct(
    ac_uid
  )

if (
  length(
    source_columns
  ) >
    0L
) {
  sc_st_source_ac01 <-
    demographics |>
    semi_join(
      canonical_ids,
      by =
        "ac_uid"
    ) |>
    select(
      ac_uid,
      all_of(
        source_columns
      )
    ) |>
    pivot_longer(
      cols =
        all_of(
          source_columns
        ),
      names_to =
        "measure",
      values_to =
        "source"
    ) |>
    count(
      measure,
      source,
      name =
        "n_ac"
    )
} else {
  sc_st_source_ac01 <-
    tibble(
      measure =
        character(),
      source =
        character(),
      n_ac =
        integer()
    )
}

write_csv(
  district_population,
  file.path(
    output_dir,
    "01_district_population_coverage.csv"
  )
)

write_csv(
  coverage_summary,
  file.path(
    output_dir,
    "02_population_coverage_summary.csv"
  )
)

write_csv(
  district_established,
  file.path(
    output_dir,
    "03_district_consistent_established_migration.csv"
  )
)

write_csv(
  window_wide,
  file.path(
    output_dir,
    "04_district_consistent_migration_change.csv"
  )
)

write_csv(
  corrected_context_ac,
  file.path(
    output_dir,
    "05_diagnostic_corrected_migration_context_ac.csv"
  )
)

write_csv(
  migration_comparison_summary,
  file.path(
    output_dir,
    "06_old_vs_district_consistent_migration_summary.csv"
  )
)

write_csv(
  employment_comparison,
  file.path(
    output_dir,
    "07_employment_denominator_comparison.csv"
  )
)

write_csv(
  employment_summary,
  file.path(
    output_dir,
    "08_employment_denominator_summary.csv"
  )
)

write_csv(
  sc_st_source_overall,
  file.path(
    output_dir,
    "09_sc_st_source_overall.csv"
  )
)

write_csv(
  sc_st_source_ac01,
  file.path(
    output_dir,
    "10_sc_st_source_ac01.csv"
  )
)

notes <-
  c(
    "R31a2 DISTRICT ALLOCATION AND DENOMINATOR AUDIT",
    "",
    "No regression model is estimated and no canonical derived dataset is modified.",
    "",
    "The audit tests whether district-level counts divided by AC proxy populations create denominator mismatch.",
    "",
    "Migration diagnostic correction:",
    "Established migration = district residents observed in the 2011 Census whose duration-of-residence bin implies arrival by 2001.",
    "The district-consistent rate divides this district numerator by direct 2011 district population.",
    "The same denominator is used for prior-five-year migration-window rates before calculating 2009-to-2014 percentage-point changes.",
    "",
    "These diagnostic variables should be described as district-derived contextual migration rates, not literal AC-level population shares.",
    "",
    "The Economic Census employment control is audited because it is constructed through the same district-count allocation machinery.",
    "",
    "SC/ST source labels are reported overall and within the canonical AC01 sample to identify reliance on direct versus fallback population sources.",
    "",
    "R31b should not be estimated until this audit is reviewed."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "11_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "12_session_info.txt"
  )
)

cat(
  "\n===== NATIONAL DISTRICT POPULATION COVERAGE =====\n"
)

print(
  coverage_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== WORST DISTRICT POPULATION MISMATCHES =====\n"
)

print(
  district_population |>
    filter(
      is.finite(
        assigned_population_coverage
      )
    ) |>
    arrange(
      assigned_population_coverage
    ) |>
    slice_head(
      n =
        20
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== OLD VS DISTRICT-CONSISTENT MIGRATION =====\n"
)

print(
  migration_comparison_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== EMPLOYMENT DENOMINATOR AUDIT =====\n"
)

print(
  employment_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== SC/ST SOURCES IN CANONICAL AC01 SAMPLE =====\n"
)

print(
  sc_st_source_ac01,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A2_DISTRICT_ALLOCATION_AUDIT_COMPLETE\n"
)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
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

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_full_lineage_migration_context_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

migration <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/migration_district_clean.csv",
    show_col_types = FALSE
  )

lineage <-
  read_csv(
    "data/derived/switchers_rewrite/final/district_harmonization_crosswalk.csv",
    show_col_types = FALSE
  )

group_population <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/target_bengali_bhojpuri_language_groups.csv",
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

old_context <-
  readRDS(
    "outputs/r31_2001_denominator_migration_context_v1_0/08_established_migration_context_by_ac.rds"
  )

ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

voter_samples <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
  )

norm_name_local <- function(
  x
) {
  x |>
    as.character() |>
    str_remove_all("\\*") |>
    str_to_upper() |>
    str_replace_all("&", "AND") |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

required_lineage <-
  c(
    "state_no_2011",
    "district_2011",
    "district_name_2011_norm",
    "district_harmonization_group_id",
    "change_comparable"
  )

required_group_population <-
  c(
    "state_no",
    "district_harmonization_group_id",
    "total_population_2001",
    "total_population_2011",
    "lineage_change_comparable"
  )

required_migration <-
  c(
    "state_no",
    "district_code_2011",
    "mig_total_district",
    "male_mig_total_district",
    "mig_lt1_district",
    "male_mig_lt1_district",
    "mig_1_4_district",
    "male_mig_1_4_district",
    "mig_5_9_district",
    "male_mig_5_9_district",
    "mig_10_19_district",
    "male_mig_10_19_district",
    "mig_gt20_district",
    "male_mig_gt20_district"
  )

for (
  check in
    list(
      lineage =
        setdiff(
          required_lineage,
          names(
            lineage
          )
        ),

      group_population =
        setdiff(
          required_group_population,
          names(
            group_population
          )
        ),

      migration =
        setdiff(
          required_migration,
          names(
            migration
          )
        )
    )
) {
  if (
    length(
      check
    ) >
      0L
  ) {
    stop(
      "Required variables missing: ",
      paste(
        check,
        collapse = ", "
      )
    )
  }
}

district_codes_path <-
  file.path(
    project_root,
    "data",
    "dist_codes.xlsx"
  )

if (
  !file.exists(
    district_codes_path
  )
) {
  stop(
    "Canonical 2011 district-code reference is missing: ",
    district_codes_path
  )
}

district_codes_raw <-
  readxl::read_xlsx(
    district_codes_path,
    col_types =
      "text"
  )

required_district_code_columns <-
  c(
    "State Code",
    "District Code",
    "Sub District Code",
    "Town-Village Name"
  )

missing_district_code_columns <-
  setdiff(
    required_district_code_columns,
    names(
      district_codes_raw
    )
  )

if (
  length(
    missing_district_code_columns
  ) >
    0L
) {
  stop(
    "District-code reference missing columns: ",
    paste(
      missing_district_code_columns,
      collapse = ", "
    )
  )
}

district_reference <-
  district_codes_raw |>
  mutate(
    state_no =
      as.integer(
        `State Code`
      ),

    district_code_2011 =
      as.integer(
        `District Code`
      ),

    subdistrict_code =
      str_pad(
        `Sub District Code`,
        5,
        pad = "0"
      )
  ) |>
  filter(
    district_code_2011 !=
      0,
    subdistrict_code ==
      "00000"
  ) |>
  transmute(
    state_no,
    district_code_2011,

    district_name_source =
      str_squish(
        `Town-Village Name`
      ),

    district_name_norm =
      norm_name_local(
        `Town-Village Name`
      )
  ) |>
  distinct(
    state_no,
    district_code_2011,
    .keep_all =
      TRUE
  )

district_reference_duplicates <-
  district_reference |>
  count(
    state_no,
    district_code_2011,
    name =
      "n"
  ) |>
  filter(
    n >
      1L
  )

if (
  nrow(
    district_reference_duplicates
  ) >
    0L
) {
  print(
    district_reference_duplicates,
    n = Inf,
    width = Inf
  )

  stop(
    "Canonical district-code reference is not unique by state/district code."
  )
}

cat(
  "\nCANONICAL DISTRICT-CODE REFERENCE ROWS: ",
  nrow(
    district_reference
  ),
  "\n",
  sep = ""
)

lineage_2011 <-
  lineage |>
  transmute(
    state_no =
      as.integer(
        state_no_2011
      ),

    district_harmonization_group_id,

    district_2011,

    district_name_norm =
      district_name_2011_norm,

    crosswalk_change_comparable =
      as.logical(
        change_comparable
      )
  ) |>
  distinct()

lineage_mapping_conflicts <-
  lineage_2011 |>
  summarise(
    n_groups =
      n_distinct(
        district_harmonization_group_id
      ),

    .by =
      c(
        state_no,
        district_name_norm
      )
  ) |>
  filter(
    n_groups >
      1L
  )

if (
  nrow(
    lineage_mapping_conflicts
  ) >
    0L
) {
  print(
    lineage_mapping_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "A 2011 district maps to multiple harmonization groups."
  )
}

lineage_2011 <-
  lineage_2011 |>
  distinct(
    state_no,
    district_name_norm,
    district_harmonization_group_id,
    crosswalk_change_comparable,
    .keep_all = TRUE
  )

lineage_group_status <-
  lineage_2011 |>
  summarise(
    n_change_values =
      n_distinct(
        crosswalk_change_comparable[
          !is.na(
            crosswalk_change_comparable
          )
        ]
      ),

    crosswalk_change_comparable =
      if (
        all(
          is.na(
            crosswalk_change_comparable
          )
        )
      ) {
        NA
      } else {
        first(
          crosswalk_change_comparable[
            !is.na(
              crosswalk_change_comparable
            )
          ]
        )
      },

    n_expected_2011_districts =
      n_distinct(
        district_name_norm
      ),

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

if (
  any(
    lineage_group_status$n_change_values >
      1L
  )
) {
  stop(
    "Crosswalk change-comparability flag varies within a harmonization group."
  )
}

group_population_key <-
  group_population |>
  select(
    state_no,
    district_harmonization_group_id,
    total_population_2001,
    total_population_2011,
    lineage_change_comparable
  )

if (
  anyDuplicated(
    group_population_key[
      c(
        "state_no",
        "district_harmonization_group_id"
      )
    ]
  ) >
    0L
) {
  stop(
    "Authoritative population artifact is not unique by state/group."
  )
}

status_check <-
  lineage_group_status |>
  inner_join(
    group_population_key |>
      select(
        state_no,
        district_harmonization_group_id,
        lineage_change_comparable
      ),
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "one-to-one"
  ) |>
  filter(
    !is.na(
      crosswalk_change_comparable
    ),
    !is.na(
      lineage_change_comparable
    ),
    crosswalk_change_comparable !=
      lineage_change_comparable
  )

if (
  nrow(
    status_check
  ) >
    0L
) {
  print(
    status_check,
    n = Inf,
    width = Inf
  )

  stop(
    "Crosswalk and canonical group artifact disagree on change comparability."
  )
}

migration_keyed <-
  migration |>
  left_join(
    district_reference,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  )

migration_codes_without_reference <-
  migration_keyed |>
  filter(
    is.na(
      district_name_norm
    )
  ) |>
  select(
    state_no,
    district_code_2011,
    everything()
  )

if (
  nrow(
    migration_codes_without_reference
  ) >
    0L
) {
  print(
    migration_codes_without_reference,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one migration district code is absent from the canonical 2011 district-code reference."
  )
}

migration_name_conflicts <-
  migration_keyed |>
  summarise(
    n_district_codes =
      n_distinct(
        district_code_2011
      ),

    .by =
      c(
        state_no,
        district_name_norm
      )
  ) |>
  filter(
    n_district_codes >
      1L
  )

if (
  nrow(
    migration_name_conflicts
  ) >
    0L
) {
  print(
    migration_name_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "Migration district names are not unique within state."
  )
}

migration_mapped <-
  migration_keyed |>
  left_join(
    lineage_2011 |>
      select(
        state_no,
        district_name_norm,
        district_harmonization_group_id
      ),
    by =
      c(
        "state_no",
        "district_name_norm"
      ),
    relationship =
      "many-to-one"
  )

unmatched_migration <-
  migration_mapped |>
  filter(
    is.na(
      district_harmonization_group_id
    )
  )

lineage_district_coverage <-
  lineage_2011 |>
  select(
    state_no,
    district_name_norm,
    district_2011,
    district_harmonization_group_id
  ) |>
  left_join(
    migration_keyed |>
      select(
        state_no,
        district_name_norm,
        district_code_2011
      ) |>
      distinct(),
    by =
      c(
        "state_no",
        "district_name_norm"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    migration_present =
      !is.na(
        district_code_2011
      )
  )

group_district_coverage <-
  lineage_district_coverage |>
  summarise(
    n_expected_2011_districts =
      n(),

    n_migration_districts_present =
      sum(
        migration_present
      ),

    full_migration_coverage =
      n_expected_2011_districts ==
        n_migration_districts_present,

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

ac_group_map <-
  ac_change |>
  select(
    ac_uid,
    state_no,
    district_harmonization_group_id
  ) |>
  distinct()

if (
  anyDuplicated(
    ac_group_map$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change is not unique by ac_uid for harmonization group."
  )
}

ac01_groups <-
  ac_samples[[
    "AC01"
  ]] |>
  distinct(
    ac_uid
  ) |>
  left_join(
    ac_group_map,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  distinct(
    state_no,
    district_harmonization_group_id
  )

v01_groups <-
  voter_samples[[
    "V01"
  ]] |>
  distinct(
    ac_uid
  ) |>
  left_join(
    ac_group_map,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  distinct(
    state_no,
    district_harmonization_group_id
  )

group_district_coverage <-
  group_district_coverage |>
  mutate(
    used_in_AC01 =
      paste(
        state_no,
        district_harmonization_group_id
      ) %in%
        paste(
          ac01_groups$state_no,
          ac01_groups$district_harmonization_group_id
        ),

    used_in_V01 =
      paste(
        state_no,
        district_harmonization_group_id
      ) %in%
        paste(
          v01_groups$state_no,
          v01_groups$district_harmonization_group_id
        )
  )

incomplete_sample_groups <-
  group_district_coverage |>
  filter(
    (
      used_in_AC01 |
        used_in_V01
    ) &
      !full_migration_coverage
  )

if (
  nrow(
    incomplete_sample_groups
  ) >
    0L
) {
  print(
    incomplete_sample_groups,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one canonical-sample lineage lacks migration data for a required 2011 district."
  )
}

sum_complete <- function(
  x
) {
  x <-
    as.numeric(
      x
    )

  if (
    length(
      x
    ) ==
      0L ||
      any(
        !is.finite(
          x
        )
      )
  ) {
    NA_real_
  } else {
    sum(
      x
    )
  }
}

migration_mapped <-
  migration_mapped |>
  mutate(
    migrant_known_duration_district =
      mig_lt1_district +
      mig_1_4_district +
      mig_5_9_district +
      mig_10_19_district +
      mig_gt20_district,

    male_migrant_known_duration_district =
      male_mig_lt1_district +
      male_mig_1_4_district +
      male_mig_5_9_district +
      male_mig_10_19_district +
      male_mig_gt20_district,

    migrant_established_by2001_district =
      mig_10_19_district +
      mig_gt20_district,

    male_migrant_established_by2001_district =
      male_mig_10_19_district +
      male_mig_gt20_district,

    migrant_unknown_duration_district =
      mig_total_district -
      migrant_known_duration_district,

    male_migrant_unknown_duration_district =
      male_mig_total_district -
      male_migrant_known_duration_district
  )

if (
  any(
    migration_mapped$migrant_unknown_duration_district <
      -1e-6,
    na.rm = TRUE
  ) ||
    any(
      migration_mapped$male_migrant_unknown_duration_district <
        -1e-6,
      na.rm = TRUE
    )
) {
  stop(
    "Known migration-duration bins exceed a reported migrant total."
  )
}

migration_group <-
  migration_mapped |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  summarise(
    n_migration_districts =
      n_distinct(
        district_name_norm
      ),

    migrant_established_by2001 =
      sum_complete(
        migrant_established_by2001_district
      ),

    male_migrant_established_by2001 =
      sum_complete(
        male_migrant_established_by2001_district
      ),

    migrant_known_duration_2011 =
      sum_complete(
        migrant_known_duration_district
      ),

    male_migrant_known_duration_2011 =
      sum_complete(
        male_migrant_known_duration_district
      ),

    migrant_reported_total_2011 =
      sum_complete(
        mig_total_district
      ),

    male_migrant_reported_total_2011 =
      sum_complete(
        male_mig_total_district
      ),

    migrant_unknown_duration_2011 =
      sum_complete(
        migrant_unknown_duration_district
      ),

    male_migrant_unknown_duration_2011 =
      sum_complete(
        male_migrant_unknown_duration_district
      ),

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

context_group <-
  group_population_key |>
  left_join(
    lineage_group_status |>
      select(
        state_no,
        district_harmonization_group_id,
        n_expected_2011_districts
      ),
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "one-to-one"
  ) |>
  left_join(
    migration_group,
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    full_numerator_coverage =
      is.finite(
        n_expected_2011_districts
      ) &
        is.finite(
          n_migration_districts
        ) &
        n_expected_2011_districts ==
          n_migration_districts,

    migrant_share_2001_proxy =
      if_else(
        full_numerator_coverage,
        migrant_established_by2001 /
          total_population_2001,
        NA_real_
      ),

    male_migrant_share_2001_proxy =
      if_else(
        full_numerator_coverage,
        male_migrant_established_by2001 /
          total_population_2001,
        NA_real_
      ),

    migrant_share_2011_known_duration =
      if_else(
        full_numerator_coverage,
        migrant_known_duration_2011 /
          total_population_2011,
        NA_real_
      ),

    male_migrant_share_2011_known_duration =
      if_else(
        full_numerator_coverage,
        male_migrant_known_duration_2011 /
          total_population_2011,
        NA_real_
      ),

    migrant_share_2011_reported_total =
      if_else(
        full_numerator_coverage,
        migrant_reported_total_2011 /
          total_population_2011,
        NA_real_
      ),

    d_migrant_share_2001_2011_pp =
      if_else(
        full_numerator_coverage &
          lineage_change_comparable %in%
            TRUE,
        100 *
          (
            migrant_share_2011_known_duration -
              migrant_share_2001_proxy
          ),
        NA_real_
      ),

    d_male_migrant_share_2001_2011_pp =
      if_else(
        full_numerator_coverage &
          lineage_change_comparable %in%
            TRUE,
        100 *
          (
            male_migrant_share_2011_known_duration -
              male_migrant_share_2001_proxy
          ),
        NA_real_
      ),

    d_migrant_share_2001_2011_reported_total_pp =
      if_else(
        full_numerator_coverage &
          lineage_change_comparable %in%
            TRUE,
        100 *
          (
            migrant_share_2011_reported_total -
              migrant_share_2001_proxy
          ),
        NA_real_
      ),

    unknown_duration_share_of_reported_migrants =
      migrant_unknown_duration_2011 /
        migrant_reported_total_2011
  )

range_audit <-
  context_group |>
  summarise(
    n_groups =
      n(),

    n_complete_baseline =
      sum(
        is.finite(
          migrant_share_2001_proxy
        )
      ),

    n_baseline_below_zero =
      sum(
        migrant_share_2001_proxy <
          0,
        na.rm = TRUE
      ),

    n_baseline_above_one =
      sum(
        migrant_share_2001_proxy >
          1,
        na.rm = TRUE
      ),

    baseline_max =
      max(
        migrant_share_2001_proxy,
        na.rm = TRUE
      ),

    n_complete_2011 =
      sum(
        is.finite(
          migrant_share_2011_known_duration
        )
      ),

    n_2011_below_zero =
      sum(
        migrant_share_2011_known_duration <
          0,
        na.rm = TRUE
      ),

    n_2011_above_one =
      sum(
        migrant_share_2011_known_duration >
          1,
        na.rm = TRUE
      ),

    current_max =
      max(
        migrant_share_2011_known_duration,
        na.rm = TRUE
      ),

    n_complete_change =
      sum(
        is.finite(
          d_migrant_share_2001_2011_pp
        )
      )
  )

if (
  range_audit$n_baseline_below_zero >
    0L ||
    range_audit$n_baseline_above_one >
      0L ||
    range_audit$n_2011_below_zero >
      0L ||
    range_audit$n_2011_above_one >
      0L
) {
  print(
    range_audit,
    width = Inf
  )

  stop(
    "A proposed migration-stock share lies outside [0,1]; review before freezing."
  )
}

context_ac <-
  ac_group_map |>
  left_join(
    context_group,
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "many-to-one"
  )

if (
  anyDuplicated(
    context_ac$ac_uid
  ) >
    0L
) {
  stop(
    "Final migration context is not unique by ac_uid."
  )
}

baseline_comparison <-
  context_ac |>
  inner_join(
    old_context |>
      select(
        ac_uid,
        r31a4_baseline =
          migrant_stock_by2001_share_2001pop
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  filter(
    is.finite(
      migrant_share_2001_proxy
    ),
    is.finite(
      r31a4_baseline
    )
  ) |>
  summarise(
    n_ac =
      n(),

    correlation =
      cor(
        migrant_share_2001_proxy,
        r31a4_baseline
      ),

    mean_absolute_difference =
      mean(
        abs(
          migrant_share_2001_proxy -
            r31a4_baseline
        )
      ),

    max_absolute_difference =
      max(
        abs(
          migrant_share_2001_proxy -
            r31a4_baseline
        )
      ),

    n_changed_above_1e_12 =
      sum(
        abs(
          migrant_share_2001_proxy -
            r31a4_baseline
        ) >
          1e-12
      )
  )

change_comparison <-
  context_ac |>
  inner_join(
    ac_change |>
      select(
        ac_uid,
        old_change_pp =
          d_mig_prior_5yr_share_ac_pop_2009_2014_pp,
        old_male_change_pp =
          d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  summarise(
    n_total =
      sum(
        is.finite(
          d_migrant_share_2001_2011_pp
        ) &
          is.finite(
            old_change_pp
          )
      ),

    correlation_total =
      cor(
        d_migrant_share_2001_2011_pp,
        old_change_pp,
        use = "complete.obs"
      ),

    n_male =
      sum(
        is.finite(
          d_male_migrant_share_2001_2011_pp
        ) &
          is.finite(
            old_male_change_pp
          )
      ),

    correlation_male =
      cor(
        d_male_migrant_share_2001_2011_pp,
        old_male_change_pp,
        use = "complete.obs"
      )
  )

unknown_duration_summary <-
  context_group |>
  summarise(
    n_complete =
      sum(
        is.finite(
          unknown_duration_share_of_reported_migrants
        )
      ),

    median =
      median(
        unknown_duration_share_of_reported_migrants,
        na.rm = TRUE
      ),

    p90 =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .90,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p95 =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .95,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p99 =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .99,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    max =
      max(
        unknown_duration_share_of_reported_migrants,
        na.rm = TRUE
      ),

    correlation_known_vs_reported_change =
      cor(
        d_migrant_share_2001_2011_pp,
        d_migrant_share_2001_2011_reported_total_pp,
        use = "complete.obs"
      )
  )

ac01 <-
  ac_samples[[
    "AC01"
  ]]

v01 <-
  voter_samples[[
    "V01"
  ]]

sample_coverage <-
  bind_rows(
    ac01 |>
      select(
        ac_uid
      ) |>
      left_join(
        context_ac |>
          select(
            ac_uid,
            migrant_share_2001_proxy,
            d_migrant_share_2001_2011_pp
          ),
        by =
          "ac_uid",
        relationship =
          "one-to-one"
      ) |>
      summarise(
        sample =
          "AC01",

        n =
          n(),

        n_baseline =
          sum(
            is.finite(
              migrant_share_2001_proxy
            )
          ),

        n_change =
          sum(
            is.finite(
              d_migrant_share_2001_2011_pp
            )
          )
      ),

    v01 |>
      select(
        respondent_uid,
        ac_uid
      ) |>
      left_join(
        context_ac |>
          select(
            ac_uid,
            migrant_share_2001_proxy,
            d_migrant_share_2001_2011_pp
          ),
        by =
          "ac_uid",
        relationship =
          "many-to-one"
      ) |>
      summarise(
        sample =
          "V01 respondents",

        n =
          n(),

        n_baseline =
          sum(
            is.finite(
              migrant_share_2001_proxy
            )
          ),

        n_change =
          sum(
            is.finite(
              d_migrant_share_2001_2011_pp
            )
          )
      )
  )

definitions <-
  tribble(
    ~variable, ~preferred_label, ~definition,

    "migrant_share_2001_proxy",
    "Established migrant stock by 2001 relative to 2001 population",
    "Interstate/international migrants enumerated in 2011 with 10+ years residence, summed over every 2011 district in the full historical harmonization lineage, divided by the lineage's Census-2001 population",

    "male_migrant_share_2001_proxy",
    "Established male migrant stock by 2001 relative to 2001 population",
    "Male analogue of migrant_share_2001_proxy using the same full lineage and Census-2001 population denominator",

    "migrant_share_2011_known_duration",
    "Migrant stock share in 2011",
    "Interstate/international migrants in all known duration-of-residence bins, summed over every 2011 district in the full lineage, divided by the lineage's Census-2011 population",

    "d_migrant_share_2001_2011_pp",
    "Proxy change in migrant-stock share, 2001-2011",
    "Percentage-point difference between the lineage-level 2011 known-duration migrant-stock share and retrospective 2001 migrant-stock proxy; retained only for canonically comparable lineages",

    "d_male_migrant_share_2001_2011_pp",
    "Proxy change in male migrant-stock share, 2001-2011",
    "Male analogue of the 2001-2011 proxy change, retained only for canonically comparable lineages"
  )

write_csv(
  lineage_district_coverage,
  file.path(
    output_dir,
    "01_full_lineage_district_migration_coverage.csv"
  )
)

write_csv(
  unmatched_migration,
  file.path(
    output_dir,
    "02_unmatched_migration_districts.csv"
  )
)

write_csv(
  group_district_coverage,
  file.path(
    output_dir,
    "03_group_numerator_coverage.csv"
  )
)

write_csv(
  context_group,
  file.path(
    output_dir,
    "04_full_lineage_migration_context_by_group.csv"
  )
)

write_csv(
  context_ac,
  file.path(
    output_dir,
    "05_full_lineage_migration_context_by_ac.csv"
  )
)

saveRDS(
  context_ac,
  file.path(
    output_dir,
    "06_full_lineage_migration_context_by_ac.rds"
  )
)

write_csv(
  range_audit,
  file.path(
    output_dir,
    "07_range_audit.csv"
  )
)

write_csv(
  baseline_comparison,
  file.path(
    output_dir,
    "08_r31a4_baseline_comparison.csv"
  )
)

write_csv(
  change_comparison,
  file.path(
    output_dir,
    "09_old_change_comparison.csv"
  )
)

write_csv(
  unknown_duration_summary,
  file.path(
    output_dir,
    "10_unknown_duration_summary.csv"
  )
)

write_csv(
  sample_coverage,
  file.path(
    output_dir,
    "11_canonical_sample_coverage.csv"
  )
)

write_csv(
  definitions,
  file.path(
    output_dir,
    "12_variable_definitions.csv"
  )
)

notes <-
  c(
    "R31a7 FULL-LINEAGE MIGRATION CONTEXT",
    "",
    "No regression model is estimated and no canonical final analysis dataset is overwritten.",
    "",
    "This script supersedes the migration-context construction in R31a4 and the attempted R31a6 lineage reconstruction.",
    "",
    "Migration numerators are assigned using the full historical district_harmonization_crosswalk.csv, not the AC-facing district mapping.",
    "",
    "Census-2001 and Census-2011 population denominators are taken from the canonical harmonized-group population artifact used by the Bengali/Bhojpuri change construction.",
    "",
    "Baseline migration uses all 2011 districts belonging to the lineage and migrants with 10+ years residence.",
    "",
    "Migration change compares that retrospective 2001 proxy with the 2011 known-duration migrant stock share and is retained only for canonically comparable district lineages.",
    "",
    "Reported migrant totals including unknown-duration observations are retained only as a measurement sensitivity."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "13_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "14_session_info.txt"
  )
)

cat(
  "\n===== FULL CROSSWALK DISTRICT COVERAGE =====\n"
)

print(
  tibble(
    n_crosswalk_2011_districts =
      nrow(
        lineage_2011
      ),

    n_migration_districts =
      nrow(
        migration_keyed
      ),

    n_crosswalk_districts_without_migration =
      sum(
        !lineage_district_coverage$migration_present
      ),

    n_migration_districts_without_crosswalk =
      nrow(
        unmatched_migration
      ),

    n_incomplete_AC01_or_V01_groups =
      nrow(
        incomplete_sample_groups
      )
  ),
  width = Inf
)

cat(
  "\n===== RANGE AUDIT =====\n"
)

print(
  range_audit,
  width = Inf
)

cat(
  "\n===== R31a4 BASELINE COMPARISON =====\n"
)

print(
  baseline_comparison,
  width = Inf
)

cat(
  "\n===== OLD VS NEW CHANGE =====\n"
)

print(
  change_comparison,
  width = Inf
)

cat(
  "\n===== UNKNOWN DURATION =====\n"
)

print(
  unknown_duration_summary,
  width = Inf
)

cat(
  "\n===== CANONICAL SAMPLE COVERAGE =====\n"
)

print(
  sample_coverage,
  n = Inf,
  width = Inf
)

cat(
  "\n===== DEFINITIONS =====\n"
)

print(
  definitions,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A7_FULL_LINEAGE_MIGRATION_CONTEXT_COMPLETE\n"
)

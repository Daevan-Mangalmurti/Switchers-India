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

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_migration_change_2001_2011_audit_v1_0"
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

allocation <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/ac_allocation_weights.csv",
    show_col_types = FALSE
  )

census_ac <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/census_context_ac.csv",
    show_col_types = FALSE
  )

old_context <-
  readRDS(
    "outputs/r31_2001_denominator_migration_context_v1_0/08_established_migration_context_by_ac.rds"
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

voter_samples <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
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

required_census <-
  c(
    "state_no",
    "district_code_2011",
    "district_harmonization_group_id",
    "total_population_2001",
    "total_population_2011",
    "muslim_population_2001",
    "muslim_population_2011",
    "district_change_comparable"
  )

required_allocation <-
  c(
    "ac_uid",
    "state_no",
    "district_code_2011",
    "district_harmonization_group_id"
  )

checks <-
  list(
    migration =
      setdiff(
        required_migration,
        names(
          migration
        )
      ),

    census =
      setdiff(
        required_census,
        names(
          census_ac
        )
      ),

    allocation =
      setdiff(
        required_allocation,
        names(
          allocation
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
      " missing required variables: ",
      paste(
        checks[[
          nm
        ]],
        collapse = ", "
      )
    )
  }
}

allocation_map <-
  allocation |>
  filter(
    !is.na(
      district_code_2011
    ),
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  distinct(
    state_no,
    district_code_2011,
    district_harmonization_group_id
  ) |>
  mutate(
    mapping_source =
      "allocation"
  )

census_map <-
  census_ac |>
  filter(
    !is.na(
      district_code_2011
    ),
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  distinct(
    state_no,
    district_code_2011,
    district_harmonization_group_id
  ) |>
  mutate(
    mapping_source =
      "census_context"
  )

map_union_long <-
  bind_rows(
    allocation_map,
    census_map
  )

map_conflicts <-
  map_union_long |>
  summarise(
    n_groups =
      n_distinct(
        district_harmonization_group_id
      ),

    groups =
      paste(
        sort(
          unique(
            district_harmonization_group_id
          )
        ),
        collapse = "; "
      ),

    .by =
      c(
        state_no,
        district_code_2011
      )
  ) |>
  filter(
    n_groups >
      1L
  )

if (
  nrow(
    map_conflicts
  ) >
    0L
) {
  print(
    map_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "Conflicting district-to-harmonization-group mappings found."
  )
}

district_to_group <-
  map_union_long |>
  distinct(
    state_no,
    district_code_2011,
    district_harmonization_group_id
  )

mapping_comparison <-
  full_join(
    allocation_map |>
      select(
        state_no,
        district_code_2011,
        allocation_group =
          district_harmonization_group_id
      ),

    census_map |>
      select(
        state_no,
        district_code_2011,
        census_group =
          district_harmonization_group_id
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
    status =
      case_when(
        !is.na(
          allocation_group
        ) &
          !is.na(
            census_group
          ) ~
          "Present in both",

        is.na(
          allocation_group
        ) &
          !is.na(
            census_group
          ) ~
          "Census context only",

        !is.na(
          allocation_group
        ) &
          is.na(
            census_group
          ) ~
          "Allocation only",

        TRUE ~
          "Unexpected"
      )
  )

first_finite <- function(
  x
) {
  x <-
    suppressWarnings(
      as.numeric(
        x
      )
    )

  x <-
    x[
      is.finite(
        x
      )
    ]

  if (
    length(
      x
    ) ==
      0L
  ) {
    NA_real_
  } else {
    x[[1]]
  }
}

population_2001_group <-
  census_ac |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  summarise(
    n_pop2001 =
      n_distinct(
        total_population_2001[
          is.finite(
            total_population_2001
          )
        ]
      ),

    n_muslim2001 =
      n_distinct(
        muslim_population_2001[
          is.finite(
            muslim_population_2001
          )
        ]
      ),

    total_population_2001 =
      first_finite(
        total_population_2001
      ),

    muslim_population_2001 =
      first_finite(
        muslim_population_2001
      ),

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

population_2001_conflicts <-
  population_2001_group |>
  filter(
    n_pop2001 >
      1L |
      n_muslim2001 >
        1L
  )

if (
  nrow(
    population_2001_conflicts
  ) >
    0L
) {
  print(
    population_2001_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "2001 population values are inconsistent within a harmonization group."
  )
}

population_2011_district <-
  census_ac |>
  filter(
    !is.na(
      district_harmonization_group_id
    ),
    !is.na(
      district_code_2011
    )
  ) |>
  summarise(
    n_pop2011_values =
      n_distinct(
        total_population_2011[
          is.finite(
            total_population_2011
          )
        ]
      ),

    n_muslim2011_values =
      n_distinct(
        muslim_population_2011[
          is.finite(
            muslim_population_2011
          )
        ]
      ),

    total_population_2011_district =
      first_finite(
        total_population_2011
      ),

    muslim_population_2011_district =
      first_finite(
        muslim_population_2011
      ),

    .by =
      c(
        state_no,
        district_harmonization_group_id,
        district_code_2011
      )
  )

population_2011_district_conflicts <-
  population_2011_district |>
  filter(
    n_pop2011_values >
      1L |
      n_muslim2011_values >
        1L
  )

if (
  nrow(
    population_2011_district_conflicts
  ) >
    0L
) {
  print(
    population_2011_district_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "2011 population values are inconsistent within a 2011 district."
  )
}

population_2011_group <-
  population_2011_district |>
  summarise(
    n_2011_districts =
      n_distinct(
        district_code_2011
      ),

    n_2011_districts_with_population =
      sum(
        is.finite(
          total_population_2011_district
        )
      ),

    n_2011_districts_with_muslim_population =
      sum(
        is.finite(
          muslim_population_2011_district
        )
      ),

    total_population_2011 =
      if (
        all(
          is.finite(
            total_population_2011_district
          )
        )
      ) {
        sum(
          total_population_2011_district
        )
      } else {
        NA_real_
      },

    muslim_population_2011 =
      if (
        all(
          is.finite(
            muslim_population_2011_district
          )
        )
      ) {
        sum(
          muslim_population_2011_district
        )
      } else {
        NA_real_
      },

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

population_group <-
  population_2001_group |>
  select(
    state_no,
    district_harmonization_group_id,
    total_population_2001,
    muslim_population_2001
  ) |>
  full_join(
    population_2011_group,
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "one-to-one"
  )

lineage_status <-
  census_ac |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  summarise(
    n_comparability_values =
      n_distinct(
        district_change_comparable[
          !is.na(
            district_change_comparable
          )
        ]
      ),

    lineage_change_comparable =
      if (
        all(
          is.na(
            district_change_comparable
          )
        )
      ) {
        NA
      } else {
        first(
          district_change_comparable[
            !is.na(
              district_change_comparable
            )
          ]
        )
      },

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

lineage_status_conflicts <-
  lineage_status |>
  filter(
    n_comparability_values >
      1L
  )

if (
  nrow(
    lineage_status_conflicts
  ) >
    0L
) {
  print(
    lineage_status_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "District-lineage change-comparability status is inconsistent within a harmonization group."
  )
}

population_group <-
  population_group |>
  left_join(
    lineage_status |>
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
  )

population_group_validity <-
  population_group |>
  summarise(
    n_groups =
      n(),

    n_missing_2001_population =
      sum(
        !is.finite(
          total_population_2001
        )
      ),

    n_missing_2011_population =
      sum(
        !is.finite(
          total_population_2011
        )
      ),

    n_nonpositive_2001_population =
      sum(
        is.finite(
          total_population_2001
        ) &
          total_population_2001 <=
            0
      ),

    n_nonpositive_2011_population =
      sum(
        is.finite(
          total_population_2011
        ) &
          total_population_2011 <=
            0
      )
  )

if (
  population_group_validity$n_nonpositive_2001_population >
    0L ||
    population_group_validity$n_nonpositive_2011_population >
      0L
) {
  print(
    population_group_validity,
    width = Inf
  )

  stop(
    "A harmonized Census population denominator is nonpositive."
  )
}

write_csv(
  population_2001_group,
  file.path(
    output_dir,
    "00a_population_2001_group_audit.csv"
  )
)

write_csv(
  population_2011_district,
  file.path(
    output_dir,
    "00b_population_2011_component_district_audit.csv"
  )
)

write_csv(
  population_2011_group,
  file.path(
    output_dir,
    "00c_population_2011_group_aggregation.csv"
  )
)

write_csv(
  population_group_validity,
  file.path(
    output_dir,
    "00d_population_group_validity.csv"
  )
)

sum_complete <- function(
  x
) {
  x <-
    suppressWarnings(
      as.numeric(
        x
      )
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

migration_group <-
  migration |>
  inner_join(
    district_to_group,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    known_duration_migrants_district =
      mig_lt1_district +
      mig_1_4_district +
      mig_5_9_district +
      mig_10_19_district +
      mig_gt20_district,

    known_duration_male_migrants_district =
      male_mig_lt1_district +
      male_mig_1_4_district +
      male_mig_5_9_district +
      male_mig_10_19_district +
      male_mig_gt20_district,

    established_migrants_district =
      mig_10_19_district +
      mig_gt20_district,

    established_male_migrants_district =
      male_mig_10_19_district +
      male_mig_gt20_district,

    unknown_duration_migrants_district =
      mig_total_district -
      known_duration_migrants_district,

    unknown_duration_male_migrants_district =
      male_mig_total_district -
      known_duration_male_migrants_district
  )

if (
  any(
    migration_group$unknown_duration_migrants_district <
      -1e-6,
    na.rm = TRUE
  ) ||
    any(
      migration_group$unknown_duration_male_migrants_district <
        -1e-6,
      na.rm = TRUE
    )
) {
  stop(
    "Known duration-bin counts exceed reported migrant totals."
  )
}

migration_group <-
  migration_group |>
  summarise(
    n_2011_districts =
      n_distinct(
        district_code_2011
      ),

    migrant_total_reported =
      sum_complete(
        mig_total_district
      ),

    male_migrant_total_reported =
      sum_complete(
        male_mig_total_district
      ),

    migrant_total_known_duration =
      sum_complete(
        known_duration_migrants_district
      ),

    male_migrant_total_known_duration =
      sum_complete(
        known_duration_male_migrants_district
      ),

    migrant_established_by2001 =
      sum_complete(
        established_migrants_district
      ),

    male_migrant_established_by2001 =
      sum_complete(
        established_male_migrants_district
      ),

    migrant_unknown_duration =
      sum_complete(
        unknown_duration_migrants_district
      ),

    male_migrant_unknown_duration =
      sum_complete(
        unknown_duration_male_migrants_district
      ),

    .by =
      c(
        state_no,
        district_harmonization_group_id
      )
  )

candidate_group <-
  migration_group |>
  left_join(
    population_group,
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    migrant_share_2001_proxy =
      migrant_established_by2001 /
      total_population_2001,

    male_migrant_share_2001_proxy =
      male_migrant_established_by2001 /
      total_population_2001,

    migrant_share_2011_known_duration =
      migrant_total_known_duration /
      total_population_2011,

    male_migrant_share_2011_known_duration =
      male_migrant_total_known_duration /
      total_population_2011,

    migrant_share_2011_reported_total =
      migrant_total_reported /
      total_population_2011,

    male_migrant_share_2011_reported_total =
      male_migrant_total_reported /
      total_population_2011,

    d_migrant_share_2001_2011_pp =
      100 *
      (
        migrant_share_2011_known_duration -
          migrant_share_2001_proxy
      ),

    d_male_migrant_share_2001_2011_pp =
      100 *
      (
        male_migrant_share_2011_known_duration -
          male_migrant_share_2001_proxy
      ),

    d_migrant_share_2001_2011_reported_total_pp =
      100 *
      (
        migrant_share_2011_reported_total -
          migrant_share_2001_proxy
      ),

    d_male_migrant_share_2001_2011_reported_total_pp =
      100 *
      (
        male_migrant_share_2011_reported_total -
          male_migrant_share_2001_proxy
      ),

    unknown_duration_share_of_reported_migrants =
      migrant_unknown_duration /
      migrant_total_reported,

    male_unknown_duration_share_of_reported_migrants =
      male_migrant_unknown_duration /
      male_migrant_total_reported,

    muslim_share_2001_reconstructed =
      muslim_population_2001 /
      total_population_2001,

    muslim_share_2011_reconstructed =
      muslim_population_2011 /
      total_population_2011,

    d_muslim_share_2001_2011_reconstructed_pp =
      100 *
      (
        muslim_share_2011_reconstructed -
          muslim_share_2001_reconstructed
      ),

    d_migrant_share_2001_2011_pp =
      if_else(
        lineage_change_comparable %in%
          TRUE,
        d_migrant_share_2001_2011_pp,
        NA_real_
      ),

    d_male_migrant_share_2001_2011_pp =
      if_else(
        lineage_change_comparable %in%
          TRUE,
        d_male_migrant_share_2001_2011_pp,
        NA_real_
      ),

    d_migrant_share_2001_2011_reported_total_pp =
      if_else(
        lineage_change_comparable %in%
          TRUE,
        d_migrant_share_2001_2011_reported_total_pp,
        NA_real_
      ),

    d_male_migrant_share_2001_2011_reported_total_pp =
      if_else(
        lineage_change_comparable %in%
          TRUE,
        d_male_migrant_share_2001_2011_reported_total_pp,
        NA_real_
      ),

    d_muslim_share_2001_2011_reconstructed_pp =
      if_else(
        lineage_change_comparable %in%
          TRUE,
        d_muslim_share_2001_2011_reconstructed_pp,
        NA_real_
      )
  )

invalid_population_share <-
  candidate_group |>
  filter(
    migrant_share_2001_proxy <
      0 |
      migrant_share_2001_proxy >
        1 |
      migrant_share_2011_known_duration <
        0 |
      migrant_share_2011_known_duration >
        1 |
      migrant_share_2011_reported_total <
        0 |
      migrant_share_2011_reported_total >
        1
  )

if (
  nrow(
    invalid_population_share
  ) >
    0L
) {
  print(
    invalid_population_share,
    n = Inf,
    width = Inf
  )

  stop(
    "A district-consistent migration population share lies outside [0,1]."
  )
}

ac_group <-
  allocation |>
  select(
    ac_uid,
    state_no,
    district_code_2011,
    district_harmonization_group_id
  ) |>
  distinct()

candidate_ac <-
  ac_group |>
  left_join(
    candidate_group,
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
    candidate_ac$ac_uid
  ) >
    0L
) {
  stop(
    "Candidate AC migration artifact is not unique by ac_uid."
  )
}

baseline_reproduction <-
  candidate_ac |>
  inner_join(
    old_context |>
      select(
        ac_uid,
        old_frozen_baseline =
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
      old_frozen_baseline
    )
  ) |>
  summarise(
    n_ac =
      n(),

    max_absolute_difference =
      max(
        abs(
          migrant_share_2001_proxy -
            old_frozen_baseline
        )
      )
  )

if (
  nrow(
    baseline_reproduction
  ) !=
    1L ||
    baseline_reproduction$max_absolute_difference >
      1e-12
) {
  print(
    baseline_reproduction,
    width = Inf
  )

  stop(
    "Existing R31a4 baseline migration values changed for overlapping ACs."
  )
}

muslim_lineage <-
  candidate_ac |>
  left_join(
    ac_change |>
      select(
        ac_uid,
        canonical_muslim_2001 =
          muslim_share_2001_dist_proxy,
        canonical_muslim_change_pp =
          d_muslim_share_2001_2011_pp
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  summarise(
    n_2001 =
      sum(
        is.finite(
          muslim_share_2001_reconstructed
        ) &
          is.finite(
            canonical_muslim_2001
          )
      ),

    max_abs_difference_2001 =
      max(
        abs(
          muslim_share_2001_reconstructed -
            canonical_muslim_2001
        ),
        na.rm = TRUE
      ),

    n_change =
      sum(
        is.finite(
          d_muslim_share_2001_2011_reconstructed_pp
        ) &
          is.finite(
            canonical_muslim_change_pp
          )
      ),

    max_abs_difference_change_pp =
      max(
        abs(
          d_muslim_share_2001_2011_reconstructed_pp -
            canonical_muslim_change_pp
        ),
        na.rm = TRUE
      )
  )

if (
  muslim_lineage$max_abs_difference_2001 >
    1e-12 ||
    muslim_lineage$max_abs_difference_change_pp >
      1e-10
) {
  print(
    muslim_lineage,
    width = Inf
  )

  stop(
    "Muslim baseline/change lineage reproduction failed."
  )
}

old_new_change <-
  candidate_ac |>
  left_join(
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
        use =
          "complete.obs"
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
        use =
          "complete.obs"
      )
  )

unknown_duration_summary <-
  candidate_group |>
  summarise(
    n_groups =
      n(),

    n_complete =
      sum(
        is.finite(
          unknown_duration_share_of_reported_migrants
        )
      ),

    median_unknown_share =
      median(
        unknown_duration_share_of_reported_migrants,
        na.rm = TRUE
      ),

    p90_unknown_share =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .90,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p95_unknown_share =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .95,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p99_unknown_share =
      quantile(
        unknown_duration_share_of_reported_migrants,
        .99,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    max_unknown_share =
      max(
        unknown_duration_share_of_reported_migrants,
        na.rm = TRUE
      )
  )

change_distribution <-
  candidate_group |>
  summarise(
    n_complete =
      sum(
        is.finite(
          d_migrant_share_2001_2011_pp
        )
      ),

    min_pp =
      min(
        d_migrant_share_2001_2011_pp,
        na.rm = TRUE
      ),

    p01_pp =
      quantile(
        d_migrant_share_2001_2011_pp,
        .01,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p10_pp =
      quantile(
        d_migrant_share_2001_2011_pp,
        .10,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    median_pp =
      median(
        d_migrant_share_2001_2011_pp,
        na.rm = TRUE
      ),

    p90_pp =
      quantile(
        d_migrant_share_2001_2011_pp,
        .90,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    p99_pp =
      quantile(
        d_migrant_share_2001_2011_pp,
        .99,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),

    max_pp =
      max(
        d_migrant_share_2001_2011_pp,
        na.rm = TRUE
      ),

    correlation_known_vs_reported_total_change =
      cor(
        d_migrant_share_2001_2011_pp,
        d_migrant_share_2001_2011_reported_total_pp,
        use =
          "complete.obs"
      )
  )

ac01_ids <-
  unique(
    ac_samples[[
      "AC01"
    ]]$ac_uid
  )

v01 <-
  voter_samples[[
    "V01"
  ]]

coverage_ac01 <-
  tibble(
    ac_uid =
      ac01_ids
  ) |>
  left_join(
    candidate_ac |>
      select(
        ac_uid,
        district_code_2011,
        district_harmonization_group_id,
        migrant_share_2001_proxy,
        migrant_share_2011_known_duration,
        d_migrant_share_2001_2011_pp
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    baseline_available =
      is.finite(
        migrant_share_2001_proxy
      ),

    current_available =
      is.finite(
        migrant_share_2011_known_duration
      ),

    change_available =
      is.finite(
        d_migrant_share_2001_2011_pp
      )
  )

coverage_summary <-
  bind_rows(
    tibble(
      sample =
        "AC01",
      n =
        length(
          ac01_ids
        ),
      n_baseline_available =
        sum(
          coverage_ac01$baseline_available
        ),
      n_current_available =
        sum(
          coverage_ac01$current_available
        ),
      n_change_available =
        sum(
          coverage_ac01$change_available
        )
    ),

    tibble(
      sample =
        "V01 respondents",
      n =
        nrow(
          v01
        ),
      n_baseline_available =
        sum(
          v01$ac_uid %in%
            candidate_ac$ac_uid[
              is.finite(
                candidate_ac$migrant_share_2001_proxy
              )
            ]
        ),
      n_current_available =
        sum(
          v01$ac_uid %in%
            candidate_ac$ac_uid[
              is.finite(
                candidate_ac$migrant_share_2011_known_duration
              )
            ]
        ),
      n_change_available =
        sum(
          v01$ac_uid %in%
            candidate_ac$ac_uid[
              is.finite(
                candidate_ac$d_migrant_share_2001_2011_pp
              )
            ]
        )
    )
  )

previous_missing <-
  tibble(
    ac_uid =
      unique(
        ac01_ids[
          !ac01_ids %in%
            old_context$ac_uid[
              is.finite(
                old_context$migrant_stock_by2001_share_2001pop
              )
            ]
        ]
      )
  )

recovery_audit <-
  previous_missing |>
  left_join(
    candidate_ac |>
      select(
        ac_uid,
        state_no,
        district_code_2011,
        district_harmonization_group_id,
        migrant_share_2001_proxy,
        migrant_share_2011_known_duration,
        d_migrant_share_2001_2011_pp
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    recovered_baseline =
      is.finite(
        migrant_share_2001_proxy
      ),

    recovered_change =
      is.finite(
        d_migrant_share_2001_2011_pp
      )
  )

write_csv(
  mapping_comparison,
  file.path(
    output_dir,
    "01_district_harmonization_mapping_comparison.csv"
  )
)

write_csv(
  candidate_group,
  file.path(
    output_dir,
    "02_candidate_migration_context_by_group.csv"
  )
)

write_csv(
  candidate_ac,
  file.path(
    output_dir,
    "03_candidate_migration_context_by_ac.csv"
  )
)

saveRDS(
  candidate_ac,
  file.path(
    output_dir,
    "04_candidate_migration_context_by_ac.rds"
  )
)

write_csv(
  baseline_reproduction,
  file.path(
    output_dir,
    "05_baseline_reproduction_check.csv"
  )
)

write_csv(
  muslim_lineage,
  file.path(
    output_dir,
    "06_muslim_2001_2011_lineage_check.csv"
  )
)

write_csv(
  unknown_duration_summary,
  file.path(
    output_dir,
    "07_unknown_duration_summary.csv"
  )
)

write_csv(
  change_distribution,
  file.path(
    output_dir,
    "08_change_distribution.csv"
  )
)

write_csv(
  old_new_change,
  file.path(
    output_dir,
    "09_old_vs_candidate_change.csv"
  )
)

write_csv(
  coverage_summary,
  file.path(
    output_dir,
    "10_canonical_sample_coverage.csv"
  )
)

write_csv(
  recovery_audit,
  file.path(
    output_dir,
    "11_previous_missing_ac_recovery_audit.csv"
  )
)

definitions <-
  tribble(
    ~variable, ~preferred_label, ~definition,

    "migrant_share_2001_proxy",
    "Established migrant stock by 2001 relative to 2001 population",
    "2011-observed interstate/international migrants with 10+ years residence divided by Census-2001 population of the same harmonized district lineage",

    "migrant_share_2011_known_duration",
    "Migrant stock share in 2011",
    "2011-observed interstate/international migrants in all known duration-of-residence bins divided by Census-2011 population of the same harmonized district lineage",

    "d_migrant_share_2001_2011_pp",
    "Proxy change in migrant-stock share, 2001-2011",
    "100 times migrant_share_2011_known_duration minus migrant_share_2001_proxy",

    "d_male_migrant_share_2001_2011_pp",
    "Proxy change in male migrant-stock share, 2001-2011",
    "Male analogue of the proxy migrant-stock share change using the same 2001 and 2011 total-population denominators"
  )

write_csv(
  definitions,
  file.path(
    output_dir,
    "12_candidate_variable_definitions.csv"
  )
)

notes <-
  c(
    "R31a6 CANDIDATE 2001-2011 MIGRATION-STOCK CHANGE",
    "",
    "No regression model is estimated and no canonical analysis dataset is modified.",
    "",
    "The prior election-relative migration-change variable is not reused.",
    "Its 2009 prior window is 1999-2003 and its 2014 prior window is 2004-2008.",
    "",
    "Candidate baseline:",
    "2011-observed migrants with 10+ years residence / Census-2001 population.",
    "",
    "Candidate 2011 level:",
    "2011-observed migrants summed over all known duration-of-residence bins / Census-2011 population.",
    "",
    "Candidate change:",
    "100 x (2011 known-duration migrant share - retrospective 2001 migrant-share proxy).",
    "",
    "Reported total migrant stock including unknown-duration migrants is retained only as a measurement sensitivity.",
    "",
    "The script also combines district-to-harmonization mappings from the population allocation and Census-context objects, after requiring that they do not conflict."
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
  "\n===== DISTRICT MAPPING SOURCES =====\n"
)

print(
  mapping_comparison |>
    count(
      status,
      name =
        "n_districts"
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== BASELINE REPRODUCTION =====\n"
)

print(
  baseline_reproduction,
  width = Inf
)

cat(
  "\n===== MUSLIM 2001/2011 LINEAGE =====\n"
)

print(
  muslim_lineage,
  width = Inf
)

cat(
  "\n===== UNKNOWN DURATION AUDIT =====\n"
)

print(
  unknown_duration_summary,
  width = Inf
)

cat(
  "\n===== CANDIDATE CHANGE DISTRIBUTION =====\n"
)

print(
  change_distribution,
  width = Inf
)

cat(
  "\n===== OLD VS CANDIDATE CHANGE =====\n"
)

print(
  old_new_change,
  width = Inf
)

cat(
  "\n===== CANONICAL SAMPLE COVERAGE =====\n"
)

print(
  coverage_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== RECOVERY OF PREVIOUSLY MISSING ACs =====\n"
)

print(
  recovery_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A6_MIGRATION_CHANGE_2001_2011_AUDIT_COMPLETE\n"
)

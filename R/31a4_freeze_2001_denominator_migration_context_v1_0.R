suppressPackageStartupMessages({
  library(dplyr)
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

audit_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_2001_population_migration_lineage_audit_v1_0"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_2001_denominator_migration_context_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

allocation <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/ac_allocation_weights.csv",
    show_col_types = FALSE
  )

migration <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/migration_district_clean.csv",
    show_col_types = FALSE
  )

candidate_group <-
  read_csv(
    file.path(
      audit_dir,
      "05_established_migrant_2001_denominator_candidate_by_group.csv"
    ),
    show_col_types = FALSE
  )

unmatched_migration <-
  read_csv(
    file.path(
      audit_dir,
      "04_unmatched_migration_districts.csv"
    ),
    show_col_types = FALSE
  )

candidate_ac_audit <-
  read_csv(
    file.path(
      audit_dir,
      "08_candidate_measure_mapped_to_ac.csv"
    ),
    show_col_types = FALSE
  )

ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

voter_samples <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
  )

if (
  !"AC01" %in%
    names(
      ac_samples
    ) ||
    !"V01" %in%
      names(
        voter_samples
      )
) {
  stop(
    "Canonical AC01/V01 samples are unavailable."
  )
}

district_mapping <-
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
  )

migration_keys <-
  migration |>
  distinct(
    state_no,
    district_code_2011
  )

mapped_district_coverage <-
  district_mapping |>
  left_join(
    migration_keys |>
      mutate(
        migration_row_present =
          TRUE
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  )

missing_migration_for_mapped_district <-
  mapped_district_coverage |>
  filter(
    is.na(
      migration_row_present
    )
  )

if (
  nrow(
    missing_migration_for_mapped_district
  ) >
    0L
) {
  print(
    missing_migration_for_mapped_district,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one AC-mapped 2011 district lacks a migration observation."
  )
}

unmatched_overlap <-
  unmatched_migration |>
  semi_join(
    district_mapping,
    by =
      c(
        "state_no",
        "district_code_2011"
      )
  )

if (
  nrow(
    unmatched_overlap
  ) >
    0L
) {
  stop(
    "A migration row classified as unmatched nevertheless belongs to the AC mapping."
  )
}

missing_population_groups <-
  candidate_group |>
  filter(
    !is.finite(
      total_population_2001
    )
  )

missing_population_ac <-
  allocation |>
  semi_join(
    missing_population_groups,
    by =
      c(
        "state_no",
        "district_harmonization_group_id"
      )
  ) |>
  distinct(
    ac_uid,
    state_no,
    district_code_2011,
    district_harmonization_group_id
  ) |>
  mutate(
    in_AC01 =
      ac_uid %in%
        ac_samples[[
          "AC01"
        ]]$ac_uid,

    in_V01 =
      ac_uid %in%
        voter_samples[[
          "V01"
        ]]$ac_uid
  )

missing_population_sample_summary <-
  tibble(
    n_missing_population_groups =
      nrow(
        missing_population_groups
      ),

    n_ac_mapped_to_missing_population_group =
      nrow(
        missing_population_ac
      ),

    n_ac01_affected =
      sum(
        missing_population_ac$in_AC01
      ),

    n_v01_ac_affected =
      sum(
        missing_population_ac$in_V01
      )
  )

candidate_validity <-
  candidate_group |>
  summarise(
    n_groups =
      n(),

    n_complete =
      sum(
        is.finite(
          established_migrant_stock_over_2001_population
        )
      ),

    n_negative =
      sum(
        established_migrant_stock_over_2001_population <
          0,
        na.rm =
          TRUE
      ),

    n_above_one =
      sum(
        established_migrant_stock_over_2001_population >
          1,
        na.rm =
          TRUE
      ),

    maximum =
      max(
        established_migrant_stock_over_2001_population,
        na.rm =
          TRUE
      )
  )

if (
  candidate_validity$n_negative !=
    0L ||
    candidate_validity$n_above_one !=
      0L
) {
  stop(
    "Frozen candidate migration measure failed range validation."
  )
}

first_finite <- function(
  x
) {
  x <-
    as.numeric(
      x
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

census_ac <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/census_context_ac.csv",
    show_col_types = FALSE
  )

population_group <-
  census_ac |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  group_by(
    state_no,
    district_harmonization_group_id
  ) |>
  summarise(
    total_population_2001 =
      first_finite(
        total_population_2001
      ),

    muslim_population_2001 =
      first_finite(
        muslim_population_2001
      ),

    .groups =
      "drop"
  )

migration_group_raw <-
  migration |>
  inner_join(
    district_mapping,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  ) |>
  group_by(
    state_no,
    district_harmonization_group_id
  ) |>
  summarise(
    established_migrants_observed_2011_10plus =
      if (
        all(
          is.finite(
            mig_10_19_district
          ),
          is.finite(
            mig_gt20_district
          )
        )
      ) {
        sum(
          mig_10_19_district +
            mig_gt20_district
        )
      } else {
        NA_real_
      },

    established_male_migrants_observed_2011_10plus =
      if (
        all(
          is.finite(
            male_mig_10_19_district
          ),
          is.finite(
            male_mig_gt20_district
          )
        )
      ) {
        sum(
          male_mig_10_19_district +
            male_mig_gt20_district
        )
      } else {
        NA_real_
      },

    n_2011_districts =
      n_distinct(
        district_code_2011
      ),

    .groups =
      "drop"
  )

context_group <-
  migration_group_raw |>
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
    migrant_stock_by2001_share_2001pop =
      established_migrants_observed_2011_10plus /
      total_population_2001,

    male_migrant_stock_by2001_share_2001pop =
      established_male_migrants_observed_2011_10plus /
      total_population_2001,

    muslim_share_2001_reconstructed =
      muslim_population_2001 /
      total_population_2001
  )

context_ac <-
  allocation |>
  select(
    ac_uid,
    state_no,
    district_code_2011,
    district_harmonization_group_id
  ) |>
  distinct() |>
  left_join(
    context_group |>
      select(
        state_no,
        district_harmonization_group_id,
        total_population_2001,
        established_migrants_observed_2011_10plus,
        established_male_migrants_observed_2011_10plus,
        migrant_stock_by2001_share_2001pop,
        male_migrant_stock_by2001_share_2001pop,
        muslim_share_2001_reconstructed
      ),
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
    "Corrected migration-context artifact is not unique by ac_uid."
  )
}

canonical_muslim <-
  candidate_ac_audit |>
  select(
    ac_uid,
    canonical_muslim_share_2001
  )

muslim_check <-
  context_ac |>
  inner_join(
    canonical_muslim,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  filter(
    is.finite(
      muslim_share_2001_reconstructed
    ),
    is.finite(
      canonical_muslim_share_2001
    )
  ) |>
  summarise(
    n_ac =
      n(),

    max_absolute_difference =
      max(
        abs(
          muslim_share_2001_reconstructed -
            canonical_muslim_share_2001
        )
      )
  )

if (
  nrow(
    muslim_check
  ) !=
    1L ||
    muslim_check$max_absolute_difference >
      1e-12
) {
  print(
    muslim_check,
    width = Inf
  )

  stop(
    "2001 Muslim-share lineage reproduction failed."
  )
}

sample_availability <-
  bind_rows(
    tibble(
      sample =
        "AC01",
      n =
        nrow(
          ac_samples[[
            "AC01"
          ]]
        ),
      n_complete_new_migrant_measure =
        sum(
          ac_samples[[
            "AC01"
          ]]$ac_uid %in%
            context_ac$ac_uid[
              is.finite(
                context_ac$migrant_stock_by2001_share_2001pop
              )
            ]
        )
    ),

    tibble(
      sample =
        "V01 respondents",
      n =
        nrow(
          voter_samples[[
            "V01"
          ]]
        ),
      n_complete_new_migrant_measure =
        sum(
          voter_samples[[
            "V01"
          ]]$ac_uid %in%
            context_ac$ac_uid[
              is.finite(
                context_ac$migrant_stock_by2001_share_2001pop
              )
            ]
        )
    )
  )

write_csv(
  mapped_district_coverage,
  file.path(
    output_dir,
    "01_mapped_district_migration_coverage.csv"
  )
)

write_csv(
  unmatched_migration,
  file.path(
    output_dir,
    "02_unmatched_migration_districts_outside_ac_mapping.csv"
  )
)

write_csv(
  missing_population_groups,
  file.path(
    output_dir,
    "03_missing_2001_population_groups.csv"
  )
)

write_csv(
  missing_population_ac,
  file.path(
    output_dir,
    "04_ac_implications_of_missing_2001_population.csv"
  )
)

write_csv(
  missing_population_sample_summary,
  file.path(
    output_dir,
    "05_missing_population_sample_summary.csv"
  )
)

write_csv(
  context_group,
  file.path(
    output_dir,
    "06_established_migration_context_by_harmonization_group.csv"
  )
)

write_csv(
  context_ac,
  file.path(
    output_dir,
    "07_established_migration_context_by_ac.csv"
  )
)

saveRDS(
  context_ac,
  file.path(
    output_dir,
    "08_established_migration_context_by_ac.rds"
  )
)

write_csv(
  muslim_check,
  file.path(
    output_dir,
    "09_muslim_lineage_reproduction.csv"
  )
)

write_csv(
  sample_availability,
  file.path(
    output_dir,
    "10_canonical_sample_availability.csv"
  )
)

definition <-
  tribble(
    ~variable, ~definition, ~numerator_source, ~denominator_source, ~preferred_label,

    "migrant_stock_by2001_share_2001pop",
    "2011-observed interstate/international migrants with 10-19 or 20+ years residence divided by 2001 population of the same harmonized district lineage",
    "Census 2011 migration duration-of-residence data",
    "Census 2001 total population in district_harmonization_group_id",
    "Established migrant stock by 2001 relative to 2001 population",

    "male_migrant_stock_by2001_share_2001pop",
    "2011-observed male interstate/international migrants with 10-19 or 20+ years residence divided by 2001 population of the same harmonized district lineage",
    "Census 2011 migration duration-of-residence data",
    "Census 2001 total population in district_harmonization_group_id",
    "Established male migrant stock by 2001 relative to 2001 population"
  )

write_csv(
  definition,
  file.path(
    output_dir,
    "11_variable_definitions.csv"
  )
)

notes <-
  c(
    "R31a4 FROZEN 2001-DENOMINATOR MIGRATION CONTEXT",
    "",
    "No regression model is estimated and no canonical analysis dataset is overwritten.",
    "",
    "Preferred baseline migration moderator:",
    "2011-observed interstate/international migrants with 10+ years residence divided by Census-2001 population of the same district harmonization lineage.",
    "",
    "The numerator therefore proxies migrants established by 2001 among people still observed at the destination in 2011.",
    "The denominator is explicitly Census-2001 population.",
    "",
    "This is not described as a literal contemporaneous Census-2001 migrant share.",
    "",
    "The old AC allocation / proxy population migration measure is retired for subsequent R31 robustness analyses.",
    "",
    "Male established-migration robustness uses the same 2001 denominator.",
    "",
    "The 2001 population lineage is validated by exact reproduction of the canonical 2001 Muslim-share measure."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "12_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "13_session_info.txt"
  )
)

cat(
  "\n===== MAPPED DISTRICT MIGRATION COVERAGE =====\n"
)

print(
  tibble(
    n_ac_mapped_districts =
      nrow(
        mapped_district_coverage
      ),

    n_missing_migration_rows =
      nrow(
        missing_migration_for_mapped_district
      ),

    n_extra_migration_districts_outside_ac_mapping =
      nrow(
        unmatched_migration
      )
  ),
  width = Inf
)

cat(
  "\n===== MISSING 2001 POPULATION GROUP IMPACT =====\n"
)

print(
  missing_population_sample_summary,
  width = Inf
)

if (
  nrow(
    missing_population_ac
  ) >
    0L
) {
  print(
    missing_population_ac,
    n = Inf,
    width = Inf
  )
}

cat(
  "\n===== MUSLIM LINEAGE REPRODUCTION =====\n"
)

print(
  muslim_check,
  width = Inf
)

cat(
  "\n===== CANONICAL SAMPLE AVAILABILITY =====\n"
)

print(
  sample_availability,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FROZEN VARIABLE DEFINITIONS =====\n"
)

print(
  definition,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A4_2001_DENOMINATOR_MIGRATION_CONTEXT_COMPLETE\n"
)

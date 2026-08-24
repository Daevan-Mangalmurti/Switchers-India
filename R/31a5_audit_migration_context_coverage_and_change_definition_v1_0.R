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
    "r31_migration_context_coverage_and_change_audit_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

context_ac <-
  readRDS(
    "outputs/r31_2001_denominator_migration_context_v1_0/08_established_migration_context_by_ac.rds"
  )

allocation <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/ac_allocation_weights.csv",
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

migration_ac_year <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/migration_ac_year.csv",
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

if (
  anyDuplicated(
    context_ac$ac_uid
  ) >
    0L
) {
  stop(
    "Migration-context artifact is not unique by ac_uid."
  )
}

allocation_payload <-
  allocation |>
  select(
    ac_uid,
    state_no_allocation =
      state_no,
    district_code_2011,
    district_harmonization_group_id
  )

context_payload <-
  context_ac |>
  select(
    ac_uid,
    total_population_2001,
    established_migrants_observed_2011_10plus,
    established_male_migrants_observed_2011_10plus,
    migrant_stock_by2001_share_2001pop,
    male_migrant_stock_by2001_share_2001pop
  )

old_payload <-
  ac_change |>
  select(
    ac_uid,
    muslim_share_2001_dist_proxy,
    mig_total_upto_2001_share_ac_pop,
    male_mig_total_upto_2001_share_ac_pop,
    d_mig_prior_5yr_share_ac_pop_2009_2014_pp,
    d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp
  )

classify_coverage <- function(
  ids
) {
  tibble(
    ac_uid =
      unique(
        as.character(
          ids
        )
      )
  ) |>
    left_join(
      allocation_payload,
      by =
        "ac_uid",
      relationship =
        "one-to-one"
    ) |>
    left_join(
      context_payload,
      by =
        "ac_uid",
      relationship =
        "one-to-one"
    ) |>
    left_join(
      old_payload,
      by =
        "ac_uid",
      relationship =
        "one-to-one"
    ) |>
    mutate(
      coverage_status =
        case_when(
          is.na(
            state_no_allocation
          ) ~
            "No AC allocation row",

          is.na(
            district_code_2011
          ) ~
            "Missing 2011 district code",

          is.na(
            district_harmonization_group_id
          ) ~
            "Missing harmonization group",

          !is.finite(
            total_population_2001
          ) ~
            "Missing 2001 population",

          !is.finite(
            established_migrants_observed_2011_10plus
          ) ~
            "Missing established-migration numerator",

          !is.finite(
            migrant_stock_by2001_share_2001pop
          ) ~
            "Other nonfinite new migration measure",

          TRUE ~
            "Complete"
        )
    )
}

ac01_coverage <-
  classify_coverage(
    ac_samples[[
      "AC01"
    ]]$ac_uid
  )

v01_ac_coverage <-
  classify_coverage(
    voter_samples[[
      "V01"
    ]]$ac_uid
  )

v01_respondent_coverage <-
  voter_samples[[
    "V01"
  ]] |>
  select(
    respondent_uid,
    ac_uid
  ) |>
  left_join(
    v01_ac_coverage |>
      select(
        ac_uid,
        coverage_status
      ),
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  )

coverage_summary <-
  bind_rows(
    ac01_coverage |>
      count(
        coverage_status,
        name =
          "n"
      ) |>
      mutate(
        sample =
          "AC01 constituencies"
      ),

    v01_ac_coverage |>
      count(
        coverage_status,
        name =
          "n"
      ) |>
      mutate(
        sample =
          "V01 constituencies"
      ),

    v01_respondent_coverage |>
      count(
        coverage_status,
        name =
          "n"
      ) |>
      mutate(
        sample =
          "V01 respondents"
      )
  ) |>
  select(
    sample,
    coverage_status,
    n
  )

missing_ac01 <-
  ac01_coverage |>
  filter(
    coverage_status !=
      "Complete"
  ) |>
  arrange(
    state_no_allocation,
    district_code_2011,
    ac_uid
  )

missing_v01_ac <-
  v01_ac_coverage |>
  filter(
    coverage_status !=
      "Complete"
  ) |>
  mutate(
    n_v01_respondents =
      vapply(
        ac_uid,
        function(
          id
        ) {
          sum(
            voter_samples[[
              "V01"
            ]]$ac_uid ==
              id
          )
        },
        integer(
          1
        )
      )
  ) |>
  arrange(
    state_no_allocation,
    district_code_2011,
    ac_uid
  )

migration_year_columns <-
  tibble(
    column =
      names(
        migration_ac_year
      )
  )

migration_years <-
  migration_ac_year |>
  summarise(
    n_rows =
      n(),
    n_ac =
      n_distinct(
        ac_uid
      ),
    min_year =
      min(
        year,
        na.rm =
          TRUE
      ),
    max_year =
      max(
        year,
        na.rm =
          TRUE
      ),
    years =
      paste(
        sort(
          unique(
            year
          )
        ),
        collapse =
          ", "
      )
  )

relevant_migration_columns <-
  names(
    migration_ac_year
  )[
    grepl(
      "mig|year|pop|share|duration|prior",
      names(
        migration_ac_year
      ),
      ignore.case =
        TRUE
    )
  ]

migration_year_summary <-
  migration_ac_year |>
  select(
    any_of(
      c(
        "ac_uid",
        "state_no",
        "year",
        relevant_migration_columns
      )
    )
  ) |>
  distinct()

numeric_relevant <-
  relevant_migration_columns[
    vapply(
      migration_ac_year[
        relevant_migration_columns
      ],
      is.numeric,
      logical(
        1
      )
    )
  ]

numeric_summary <-
  bind_rows(
    lapply(
      numeric_relevant,
      function(
        variable
      ) {
        x <-
          migration_ac_year[[
            variable
          ]]

        tibble(
          variable =
            variable,
          n_nonmissing =
            sum(
              is.finite(
                x
              )
            ),
          min =
            if (
              any(
                is.finite(
                  x
                )
              )
            ) {
              min(
                x[
                  is.finite(
                    x
                  )
                ]
              )
            } else {
              NA_real_
            },
          median =
            if (
              any(
                is.finite(
                  x
                )
              )
            ) {
              median(
                x[
                  is.finite(
                    x
                  )
                ]
              )
            } else {
              NA_real_
            },
          max =
            if (
              any(
                is.finite(
                  x
                )
              )
            ) {
              max(
                x[
                  is.finite(
                    x
                  )
                ]
              )
            } else {
              NA_real_
            }
        )
      }
    )
  )

old_change_payload <-
  old_payload |>
  select(
    ac_uid,
    d_mig_prior_5yr_share_ac_pop_2009_2014_pp,
    d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp
  )

ac01_change_audit <-
  ac_samples[[
    "AC01"
  ]] |>
  select(
    ac_uid
  ) |>
  left_join(
    old_change_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

v01_change_audit <-
  voter_samples[[
    "V01"
  ]] |>
  select(
    respondent_uid,
    ac_uid
  ) |>
  left_join(
    old_change_payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  )

old_change_availability <-
  bind_rows(
    ac01_change_audit |>
      summarise(
        sample =
          "AC01",

        n =
          n(),

        n_old_change_complete =
          sum(
            is.finite(
              d_mig_prior_5yr_share_ac_pop_2009_2014_pp
            )
          ),

        n_old_male_change_complete =
          sum(
            is.finite(
              d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp
            )
          )
      ),

    v01_change_audit |>
      summarise(
        sample =
          "V01 respondents",

        n =
          n(),

        n_old_change_complete =
          sum(
            is.finite(
              d_mig_prior_5yr_share_ac_pop_2009_2014_pp
            )
          ),

        n_old_male_change_complete =
          sum(
            is.finite(
              d_male_mig_prior_5yr_share_ac_pop_2009_2014_pp
            )
          )
      )
  )

write_csv(
  coverage_summary,
  file.path(
    output_dir,
    "01_baseline_migration_coverage_summary.csv"
  )
)

write_csv(
  missing_ac01,
  file.path(
    output_dir,
    "02_ac01_missing_new_baseline_migration.csv"
  )
)

write_csv(
  missing_v01_ac,
  file.path(
    output_dir,
    "03_v01_ac_missing_new_baseline_migration.csv"
  )
)

write_csv(
  migration_year_columns,
  file.path(
    output_dir,
    "04_migration_ac_year_columns.csv"
  )
)

write_csv(
  migration_years,
  file.path(
    output_dir,
    "05_migration_ac_year_years.csv"
  )
)

write_csv(
  migration_year_summary,
  file.path(
    output_dir,
    "06_migration_ac_year_relevant_fields.csv"
  )
)

write_csv(
  numeric_summary,
  file.path(
    output_dir,
    "07_migration_ac_year_numeric_summary.csv"
  )
)

write_csv(
  old_change_availability,
  file.path(
    output_dir,
    "08_old_change_measure_availability.csv"
  )
)

notes <-
  c(
    "R31a5 MIGRATION CONTEXT COVERAGE AND CHANGE-DEFINITION AUDIT",
    "",
    "No regression is estimated and no analysis data are modified.",
    "",
    "Part 1 classifies why the frozen 2001-denominator established-migration measure is missing for some canonical AC01/V01 observations.",
    "",
    "Part 2 inventories the year-specific migration intermediate and the existing migration-change variables.",
    "",
    "No replacement change-in-migration moderator is defined in this script.",
    "That estimand will be frozen only after the source-year construction is reviewed."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "09_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "10_session_info.txt"
  )
)

cat(
  "\n===== BASELINE MIGRATION COVERAGE SUMMARY =====\n"
)

print(
  coverage_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== AC01 MISSING BASELINE MIGRATION =====\n"
)

print(
  missing_ac01,
  n = Inf,
  width = Inf
)

cat(
  "\n===== V01 ACs MISSING BASELINE MIGRATION =====\n"
)

print(
  missing_v01_ac,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MIGRATION AC-YEAR STRUCTURE =====\n"
)

print(
  migration_years,
  width = Inf
)

cat(
  "\n===== MIGRATION AC-YEAR COLUMNS =====\n"
)

print(
  migration_year_columns,
  n = Inf,
  width = Inf
)

cat(
  "\n===== OLD CHANGE-MEASURE AVAILABILITY =====\n"
)

print(
  old_change_availability,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A5_MIGRATION_CONTEXT_COVERAGE_AND_CHANGE_AUDIT_COMPLETE\n"
)

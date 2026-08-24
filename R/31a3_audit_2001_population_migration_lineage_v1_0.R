suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
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

final_dir <-
  file.path(
    project_root,
    "data",
    "derived",
    "switchers_rewrite",
    "final"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_2001_population_migration_lineage_audit_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

migration_path <-
  file.path(
    intermediate_dir,
    "migration_district_clean.csv"
  )

allocation_path <-
  file.path(
    intermediate_dir,
    "ac_allocation_weights.csv"
  )

ac_change_path <-
  file.path(
    final_dir,
    "ac_change.rds"
  )

required_files <-
  c(
    migration_path,
    allocation_path,
    ac_change_path
  )

missing_files <-
  required_files[
    !file.exists(
      required_files
    )
  ]

if (
  length(
    missing_files
  ) >
    0L
) {
  stop(
    "Missing required files: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

read_object_names <- function(
  path
) {
  extension <-
    tolower(
      tools::file_ext(
        path
      )
    )

  tryCatch(
    {
      if (
        extension ==
          "csv"
      ) {
        names(
          read_csv(
            path,
            n_max =
              0,
            show_col_types =
              FALSE,
            progress =
              FALSE
          )
        )
      } else if (
        extension ==
          "rds"
      ) {
        file_size <-
          file.info(
            path
          )$size

        if (
          is.na(
            file_size
          ) ||
            file_size >
              200000000
        ) {
          character()
        } else {
          object <-
            readRDS(
              path
            )

          names(
            object
          )
        }
      } else {
        character()
      }
    },
    error =
      function(
        e
      ) {
        character()
      }
  )
}

candidate_files <-
  c(
    list.files(
      intermediate_dir,
      pattern =
        "\\.(csv|rds)$",
      full.names =
        TRUE,
      ignore.case =
        TRUE
    ),
    list.files(
      final_dir,
      pattern =
        "\\.(csv|rds)$",
      full.names =
        TRUE,
      ignore.case =
        TRUE
    )
  ) |>
  unique()

candidate_manifest <-
  map_dfr(
    candidate_files,
    function(
      path
    ) {
      object_names <-
        read_object_names(
          path
        )

      tibble(
        path =
          path,

        file =
          basename(
            path
          ),

        has_group_id =
          "district_harmonization_group_id" %in%
            object_names,

        has_state_no =
          "state_no" %in%
            object_names,

        has_total_population_2001 =
          "total_population_2001" %in%
            object_names,

        has_muslim_population_2001 =
          "muslim_population_2001" %in%
            object_names,

        has_total_population_2011 =
          "total_population_2011" %in%
            object_names,

        has_muslim_population_2011 =
          "muslim_population_2011" %in%
            object_names,

        has_district_code_2011 =
          "district_code_2011" %in%
            object_names,

        n_columns =
          length(
            object_names
          )
      )
    }
  )

population_candidates <-
  candidate_manifest |>
  filter(
    has_group_id,
    has_total_population_2001,
    has_muslim_population_2001
  )

write_csv(
  candidate_manifest,
  file.path(
    output_dir,
    "01_candidate_file_manifest.csv"
  )
)

cat(
  "\n===== 2001 POPULATION SOURCE CANDIDATES =====\n"
)

print(
  population_candidates,
  n = Inf,
  width = Inf
)

if (
  nrow(
    population_candidates
  ) ==
    0L
) {
  stop(
    "No object contains district_harmonization_group_id, total_population_2001, and muslim_population_2001 together."
  )
}

population_source_path <-
  file.path(
    intermediate_dir,
    "census_context_ac.csv"
  )

if (
  !file.exists(
    population_source_path
  )
) {
  stop(
    "Canonical intermediate Census context artifact is missing: ",
    population_source_path
  )
}

canonical_population_candidate <-
  population_candidates |>
  filter(
    normalizePath(
      path,
      mustWork =
        TRUE
    ) ==
      normalizePath(
        population_source_path,
        mustWork =
          TRUE
      )
  )

if (
  nrow(
    canonical_population_candidate
  ) !=
    1L
) {
  stop(
    "census_context_ac.csv was not uniquely identified among the valid 2001 population-source candidates."
  )
}

population_candidates <-
  canonical_population_candidate

cat(
  "\nSELECTED 2001 POPULATION SOURCE:\n",
  population_source_path,
  "\n"
)

extension <-
  tolower(
    tools::file_ext(
      population_source_path
    )
  )

if (
  extension ==
    "csv"
) {
  population_source <-
    read_csv(
      population_source_path,
      show_col_types =
        FALSE
    )
} else if (
  extension ==
    "rds"
) {
  population_source <-
    readRDS(
      population_source_path
    )
} else {
  stop(
    "Unsupported selected population-source format."
  )
}

allocation <-
  read_csv(
    allocation_path,
    show_col_types =
      FALSE
  )

migration <-
  read_csv(
    migration_path,
    show_col_types =
      FALSE
  )

ac_change <-
  readRDS(
    ac_change_path
  )

required_allocation_columns <-
  c(
    "ac_uid",
    "state_no",
    "district_code_2011",
    "district_harmonization_group_id"
  )

required_migration_columns <-
  c(
    "state_no",
    "district_code_2011",
    "mig_10_19_district",
    "mig_gt20_district"
  )

required_population_columns <-
  c(
    "district_harmonization_group_id",
    "total_population_2001",
    "muslim_population_2001"
  )

for (
  specification in
    list(
      allocation =
        list(
          data =
            allocation,
          required =
            required_allocation_columns
        ),

      migration =
        list(
          data =
            migration,
          required =
            required_migration_columns
        ),

      population =
        list(
          data =
            population_source,
          required =
            required_population_columns
        )
    )
) {
  missing <-
    setdiff(
      specification$required,
      names(
        specification$data
      )
    )

  if (
    length(
      missing
    ) >
      0L
  ) {
    stop(
      "Required columns missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
}

district_to_group <-
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

district_mapping_audit <-
  district_to_group |>
  summarise(
    n_harmonization_groups =
      n_distinct(
        district_harmonization_group_id
      ),
    .by =
      c(
        state_no,
        district_code_2011
      )
  ) |>
  arrange(
    desc(
      n_harmonization_groups
    ),
    state_no,
    district_code_2011
  )

ambiguous_districts <-
  district_mapping_audit |>
  filter(
    n_harmonization_groups !=
      1L
  )

write_csv(
  district_mapping_audit,
  file.path(
    output_dir,
    "02_2011_district_to_harmonization_group_audit.csv"
  )
)

if (
  nrow(
    ambiguous_districts
  ) >
    0L
) {
  print(
    ambiguous_districts,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one 2011 district maps to multiple harmonization groups."
  )
}

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

population_keys <-
  "district_harmonization_group_id"

if (
  "state_no" %in%
    names(
      population_source
    )
) {
  population_keys <-
    c(
      "state_no",
      "district_harmonization_group_id"
    )
}

population_consistency <-
  population_source |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  group_by(
    across(
      all_of(
        population_keys
      )
    )
  ) |>
  summarise(
    n_total_population_2001_values =
      n_distinct(
        total_population_2001[
          is.finite(
            total_population_2001
          )
        ]
      ),

    n_muslim_population_2001_values =
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

    total_population_2011 =
      if (
        "total_population_2011" %in%
          names(
            population_source
          )
      ) {
        first_finite(
          total_population_2011
        )
      } else {
        NA_real_
      },

    .groups =
      "drop"
  )

write_csv(
  population_consistency,
  file.path(
    output_dir,
    "03_2001_population_group_consistency.csv"
  )
)

inconsistent_population_groups <-
  population_consistency |>
  filter(
    n_total_population_2001_values >
      1L |
      n_muslim_population_2001_values >
        1L
  )

if (
  nrow(
    inconsistent_population_groups
  ) >
    0L
) {
  print(
    inconsistent_population_groups,
    n = Inf,
    width = Inf
  )

  stop(
    "2001 population source is not internally constant within harmonization group."
  )
}

if (
  any(
    !is.na(
      population_consistency$total_population_2001
    ) &
      population_consistency$total_population_2001 <=
        0
  )
) {
  stop(
    "At least one harmonization group has nonpositive 2001 population."
  )
}

migration_with_group <-
  migration |>
  left_join(
    district_to_group,
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  ) |>
  mutate(
    established_migrants_district =
      as.numeric(
        mig_10_19_district
      ) +
      as.numeric(
        mig_gt20_district
      )
  )

unmatched_migration_districts <-
  migration_with_group |>
  filter(
    is.na(
      district_harmonization_group_id
    )
  ) |>
  select(
    state_no,
    district_code_2011,
    everything()
  )

write_csv(
  unmatched_migration_districts,
  file.path(
    output_dir,
    "04_unmatched_migration_districts.csv"
  )
)

group_keys <-
  c(
    "state_no",
    "district_harmonization_group_id"
  )

if (
  !"state_no" %in%
    population_keys
) {
  group_keys <-
    "district_harmonization_group_id"

  group_state_audit <-
    district_to_group |>
    summarise(
      n_states =
        n_distinct(
          state_no
        ),
      .by =
        district_harmonization_group_id
    )

  if (
    any(
      group_state_audit$n_states >
        1L
    )
  ) {
    stop(
      "Harmonization group IDs are not globally unique across states."
    )
  }
}

migration_group <-
  migration_with_group |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  group_by(
    across(
      all_of(
        c(
          "state_no",
          "district_harmonization_group_id"
        )
      )
    )
  ) |>
  summarise(
    n_2011_districts =
      n_distinct(
        district_code_2011
      ),

    n_districts_with_complete_established_count =
      sum(
        is.finite(
          established_migrants_district
        )
      ),

    established_migrants_observed_2011_10plus =
      if (
        all(
          is.finite(
            established_migrants_district
          )
        )
      ) {
        sum(
          established_migrants_district
        )
      } else {
        NA_real_
      },

    .groups =
      "drop"
  )

if (
  identical(
    group_keys,
    "district_harmonization_group_id"
  )
) {
  migration_group <-
    migration_group |>
    select(
      -state_no
    )
}

candidate_group <-
  migration_group |>
  left_join(
    population_consistency |>
      select(
        all_of(
          population_keys
        ),
        total_population_2001,
        muslim_population_2001,
        total_population_2011
      ),
    by =
      group_keys,
    relationship =
      "one-to-one"
  ) |>
  mutate(
    established_migrant_stock_over_2001_population =
      established_migrants_observed_2011_10plus /
      total_population_2001,

    established_migrant_stock_over_2011_population =
      established_migrants_observed_2011_10plus /
      total_population_2011,

    muslim_share_2001_reconstructed =
      muslim_population_2001 /
      total_population_2001
  )

write_csv(
  candidate_group,
  file.path(
    output_dir,
    "05_established_migrant_2001_denominator_candidate_by_group.csv"
  )
)

candidate_validity <-
  candidate_group |>
  summarise(
    n_harmonization_groups =
      n(),

    n_complete_2001_denominator =
      sum(
        is.finite(
          established_migrant_stock_over_2001_population
        )
      ),

    n_missing_2001_denominator =
      sum(
        !is.finite(
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

    min =
      min(
        established_migrant_stock_over_2001_population,
        na.rm =
          TRUE
      ),

    p01 =
      quantile(
        established_migrant_stock_over_2001_population,
        .01,
        na.rm =
          TRUE,
        names =
          FALSE,
        type =
          8
      ),

    p10 =
      quantile(
        established_migrant_stock_over_2001_population,
        .10,
        na.rm =
          TRUE,
        names =
          FALSE,
        type =
          8
      ),

    median =
      median(
        established_migrant_stock_over_2001_population,
        na.rm =
          TRUE
      ),

    p90 =
      quantile(
        established_migrant_stock_over_2001_population,
        .90,
        na.rm =
          TRUE,
        names =
          FALSE,
        type =
          8
      ),

    p99 =
      quantile(
        established_migrant_stock_over_2001_population,
        .99,
        na.rm =
          TRUE,
        names =
          FALSE,
        type =
          8
      ),

    max =
      max(
        established_migrant_stock_over_2001_population,
        na.rm =
          TRUE
      ),

    correlation_2001_vs_2011_denominator =
      cor(
        established_migrant_stock_over_2001_population,
        established_migrant_stock_over_2011_population,
        use =
          "complete.obs"
      )
  )

write_csv(
  candidate_validity,
  file.path(
    output_dir,
    "06_candidate_migrant_share_validity_summary.csv"
  )
)

candidate_extremes <-
  candidate_group |>
  filter(
    is.finite(
      established_migrant_stock_over_2001_population
    )
  ) |>
  arrange(
    desc(
      established_migrant_stock_over_2001_population
    )
  ) |>
  slice_head(
    n =
      25
  )

write_csv(
  candidate_extremes,
  file.path(
    output_dir,
    "07_highest_candidate_migrant_shares.csv"
  )
)

allocation_group <-
  allocation |>
  select(
    ac_uid,
    state_no,
    district_harmonization_group_id
  ) |>
  filter(
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  distinct()

candidate_ac <-
  allocation_group

if (
  identical(
    group_keys,
    "district_harmonization_group_id"
  )
) {
  candidate_ac <-
    candidate_ac |>
    left_join(
      candidate_group |>
        select(
          district_harmonization_group_id,
          established_migrant_stock_over_2001_population,
          muslim_share_2001_reconstructed
        ),
      by =
        "district_harmonization_group_id",
      relationship =
        "many-to-one"
    )
} else {
  candidate_ac <-
    candidate_ac |>
    left_join(
      candidate_group |>
        select(
          state_no,
          district_harmonization_group_id,
          established_migrant_stock_over_2001_population,
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
}

candidate_ac <-
  candidate_ac |>
  left_join(
    ac_change |>
      select(
        ac_uid,
        canonical_muslim_share_2001 =
          muslim_share_2001_dist_proxy,
        old_migration_measure =
          mig_total_upto_2001_share_ac_pop
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

write_csv(
  candidate_ac,
  file.path(
    output_dir,
    "08_candidate_measure_mapped_to_ac.csv"
  )
)

muslim_reconstruction_audit <-
  candidate_ac |>
  filter(
    is.finite(
      muslim_share_2001_reconstructed
    ),
    is.finite(
      canonical_muslim_share_2001
    )
  ) |>
  summarise(
    n_ac_compared =
      n(),

    correlation =
      cor(
        muslim_share_2001_reconstructed,
        canonical_muslim_share_2001
      ),

    mean_absolute_difference =
      mean(
        abs(
          muslim_share_2001_reconstructed -
            canonical_muslim_share_2001
        )
      ),

    max_absolute_difference =
      max(
        abs(
          muslim_share_2001_reconstructed -
            canonical_muslim_share_2001
        )
      )
  )

write_csv(
  muslim_reconstruction_audit,
  file.path(
    output_dir,
    "09_muslim_2001_reconstruction_audit.csv"
  )
)

migration_old_new_audit <-
  candidate_ac |>
  filter(
    is.finite(
      established_migrant_stock_over_2001_population
    ),
    is.finite(
      old_migration_measure
    )
  ) |>
  summarise(
    n_ac_compared =
      n(),

    old_measure_n_above_one =
      sum(
        old_migration_measure >
          1
      ),

    new_measure_n_above_one =
      sum(
        established_migrant_stock_over_2001_population >
          1
      ),

    correlation_old_new =
      cor(
        old_migration_measure,
        established_migrant_stock_over_2001_population
      ),

    mean_absolute_difference =
      mean(
        abs(
          old_migration_measure -
            established_migrant_stock_over_2001_population
        )
      ),

    max_absolute_difference =
      max(
        abs(
          old_migration_measure -
            established_migrant_stock_over_2001_population
        )
      )
  )

write_csv(
  migration_old_new_audit,
  file.path(
    output_dir,
    "10_old_vs_2001_denominator_migration_audit.csv"
  )
)

mapping_summary <-
  tibble(
    n_2011_district_mappings =
      nrow(
        district_mapping_audit
      ),

    n_ambiguous_2011_districts =
      nrow(
        ambiguous_districts
      ),

    n_migration_district_rows =
      nrow(
        migration
      ),

    n_unmatched_migration_districts =
      nrow(
        unmatched_migration_districts
      ),

    n_harmonization_groups_with_migration =
      nrow(
        migration_group
      ),

    n_harmonization_groups_with_2001_population =
      sum(
        is.finite(
          candidate_group$total_population_2001
        )
      )
  )

write_csv(
  mapping_summary,
  file.path(
    output_dir,
    "11_lineage_mapping_summary.csv"
  )
)

notes <-
  c(
    "R31a3 2001 POPULATION / MIGRATION LINEAGE PREFLIGHT",
    "",
    "No canonical derived dataset is modified and no regression is estimated.",
    "",
    "Target baseline migration measure:",
    "2011-observed interstate/international migrants with 10+ years residence divided by 2001 population of the same district harmonization group.",
    "",
    "The numerator is aggregated from 2011 districts to district_harmonization_group_id before division.",
    "The denominator is total_population_2001 for the same harmonization group.",
    "",
    "This mirrors the temporal denominator logic of the 2001 Muslim population share.",
    "",
    "The proposed migration measure should not be described as a literal Census-2001 migrant share.",
    "A more accurate description is established migrant stock relative to 2001 population, or a proxy for migrant population share by 2001.",
    "",
    "The script independently reconstructs Muslim population share as muslim_population_2001 / total_population_2001 and compares it with the canonical muslim_share_2001_dist_proxy.",
    "",
    "The old migration measure using allocated district migrant counts divided by proxy_ac_pop is retained only for comparison and is not modified here."
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
  "\n===== LINEAGE MAPPING SUMMARY =====\n"
)

print(
  mapping_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CANDIDATE MIGRANT SHARE VALIDITY =====\n"
)

print(
  candidate_validity,
  n = Inf,
  width = Inf
)

cat(
  "\n===== HIGHEST 25 CANDIDATE VALUES =====\n"
)

print(
  candidate_extremes,
  n = Inf,
  width = Inf
)

cat(
  "\n===== 2001 MUSLIM SHARE RECONSTRUCTION =====\n"
)

print(
  muslim_reconstruction_audit,
  n = Inf,
  width = Inf
)

cat(
  "\n===== OLD VS NEW MIGRATION MEASURE =====\n"
)

print(
  migration_old_new_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A3_2001_POPULATION_MIGRATION_LINEAGE_AUDIT_COMPLETE\n"
)

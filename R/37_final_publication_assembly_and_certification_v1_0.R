suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
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

registry_path <-
  "config/paper_artifacts_v1_3.csv"

if (
  !file.exists(
    registry_path
  )
) {
  stop(
    "Missing authoritative v1.3 paper artifact registry."
  )
}

registry <-
  read_csv(
    registry_path,
    show_col_types = FALSE
  )

output_root <-
  file.path(
    "outputs",
    "paper_outputs_final_v1_0"
  )

main_dir <-
  file.path(
    output_root,
    "main"
  )

appendix_dir <-
  file.path(
    output_root,
    "appendix"
  )

source_dir <-
  file.path(
    output_root,
    "frozen_sources"
  )

cert_dir <-
  file.path(
    output_root,
    "certification"
  )

config_dir <-
  file.path(
    output_root,
    "config"
  )

for (
  d in
    c(
      output_root,
      main_dir,
      appendix_dir,
      source_dir,
      cert_dir,
      config_dir
    )
) {
  dir.create(
    d,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

certification <- tibble(
  check_id =
    character(),
  check =
    character(),
  passed =
    logical(),
  detail =
    character()
)

add_check <- function(
  check_id,
  check,
  passed,
  detail
) {
  certification <<-
    bind_rows(
      certification,
      tibble(
        check_id =
          check_id,
        check =
          check,
        passed =
          isTRUE(
            passed
          ),
        detail =
          as.character(
            detail
          )
      )
    )
}

all_built <-
  all(
    registry$publication_status ==
      "Built"
  )

add_check(
  "C001",
  "Every registered artifact is Built",
  all_built,
  paste0(
    "Unbuilt = ",
    sum(
      registry$publication_status !=
        "Built"
    )
  )
)

artifact_exists <-
  !is.na(
    registry$publication_artifact
  ) &
    file.exists(
      registry$publication_artifact
    )

add_check(
  "C002",
  "Every registered publication artifact exists",
  all(
    artifact_exists
  ),
  paste0(
    sum(
      artifact_exists
    ),
    "/",
    nrow(
      registry
    ),
    " publication artifacts exist"
  )
)

source_exists <-
  !is.na(
    registry$source_artifact
  ) &
    file.exists(
      registry$source_artifact
    )

add_check(
  "C003",
  "Every registered source artifact exists",
  all(
    source_exists
  ),
  paste0(
    sum(
      source_exists
    ),
    "/",
    nrow(
      registry
    ),
    " source artifacts exist"
  )
)

main_architecture <-
  registry |>
  filter(
    placement ==
      "Main"
  ) |>
  summarise(
    n =
      n(),
    figures =
      sum(
        artifact_type ==
          "Figure"
      ),
    tables =
      sum(
        artifact_type ==
          "Table"
      )
  )

add_check(
  "C004",
  "Main architecture is exactly six figures and two tables",
  main_architecture$n ==
    8L &&
    main_architecture$figures ==
      6L &&
    main_architecture$tables ==
      2L,
  paste0(
    "n=",
    main_architecture$n,
    "; figures=",
    main_architecture$figures,
    "; tables=",
    main_architecture$tables
  )
)

figure2_prob <-
  read_csv(
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_2_adjusted_probabilities.csv",
    show_col_types = FALSE
  )

figure2_contrasts <-
  read_csv(
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_2_ideology_contrasts.csv",
    show_col_types = FALSE
  )

f2_mod_left <-
  figure2_contrasts |>
  filter(
    comparison ==
      "Moderate - Left"
  )

f2_right_mod <-
  figure2_contrasts |>
  filter(
    comparison ==
      "Right - Moderate"
  )

add_check(
  "C005",
  "Figure 2 Moderate-minus-Left contrast reproduces frozen result",
  nrow(
    f2_mod_left
  ) ==
    1L &&
    abs(
      f2_mod_left$estimate_pp[[1]] -
        4.26
    ) <
      0.02,
  if (
    nrow(
      f2_mod_left
    ) ==
      1L
  ) {
    paste0(
      "Observed=",
      signif(
        f2_mod_left$estimate_pp[[1]],
        8
      ),
      " pp"
    )
  } else {
    "Contrast not uniquely found"
  }
)

add_check(
  "C006",
  "Figure 2 Right-minus-Moderate contrast reproduces frozen result",
  nrow(
    f2_right_mod
  ) ==
    1L &&
    abs(
      f2_right_mod$estimate_pp[[1]] -
        6.34
    ) <
      0.02,
  if (
    nrow(
      f2_right_mod
    ) ==
      1L
  ) {
    paste0(
      "Observed=",
      signif(
        f2_right_mod$estimate_pp[[1]],
        8
      ),
      " pp"
    )
  } else {
    "Contrast not uniquely found"
  }
)

nes <-
  read_csv(
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_3_source_data.csv",
    show_col_types = FALSE
  ) |>
  arrange(
    year,
    ideology
  )

nes_expected <-
  tibble(
    year =
      c(
        2009,
        2009,
        2009,
        2014,
        2014,
        2014
      ),

    ideology =
      c(
        "Left",
        "Center",
        "Right",
        "Left",
        "Center",
        "Right"
      ),

    expected =
      c(
        13.071868031833597,
        21.48717030949682,
        34.632395956920995,
        23.600840469812322,
        37.210055480478616,
        49.00711683512926
      )
  )

nes_check <-
  nes |>
  mutate(
    ideology =
      as.character(
        ideology
      ),
    year =
      as.numeric(
        as.character(
          year
        )
      )
  ) |>
  select(
    year,
    ideology,
    observed =
      bjp_vote_pct_weighted
  ) |>
  left_join(
    nes_expected,
    by =
      c(
        "year",
        "ideology"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    diff =
      abs(
        observed -
          expected
      )
  )

add_check(
  "C007",
  "Figure 3 reproduces all six audited NES BJP vote shares",
  nrow(
    nes_check
  ) ==
    6L &&
    all(
      nes_check$diff <
        1e-10
    ),
  paste0(
    "Max absolute difference=",
    format(
      max(
        nes_check$diff
      ),
      scientific = TRUE
    )
  )
)

table1_summary <-
  read_csv(
    "outputs/main_regression_table_models_v1_0/09_main_ac_table_summary.csv",
    show_col_types = FALSE
  )

table2_summary <-
  read_csv(
    "outputs/main_regression_table_models_v1_0/10_main_voter_table_summary.csv",
    show_col_types = FALSE
  )

ac_total <-
  table1_summary |>
  filter(
    model_id ==
      "total_raw__C3"
  )

ac_mfg <-
  table1_summary |>
  filter(
    model_id ==
      "manufacturing_raw__C3"
  )

v_total <-
  table2_summary |>
  filter(
    model_id ==
      "total_raw__C3"
  )

v_mfg <-
  table2_summary |>
  filter(
    model_id ==
      "manufacturing_raw__C3"
  )

add_check(
  "C008",
  "Table 1 controlled Total interaction reproduces 0.630",
  nrow(
    ac_total
  ) ==
    1L &&
    abs(
      ac_total$focal_estimate[[1]] -
        0.630
    ) <
      0.001,
  paste0(
    "Observed=",
    ifelse(
      nrow(
        ac_total
      ) ==
        1L,
      signif(
        ac_total$focal_estimate[[1]],
        8
      ),
      NA
    )
  )
)

add_check(
  "C009",
  "Table 1 controlled Manufacturing interaction reproduces 2.131",
  nrow(
    ac_mfg
  ) ==
    1L &&
    abs(
      ac_mfg$focal_estimate[[1]] -
        2.131
    ) <
      0.001,
  paste0(
    "Observed=",
    ifelse(
      nrow(
        ac_mfg
      ) ==
        1L,
      signif(
        ac_mfg$focal_estimate[[1]],
        8
      ),
      NA
    )
  )
)

add_check(
  "C010",
  "Table 2 controlled Total interaction reproduces 0.346",
  nrow(
    v_total
  ) ==
    1L &&
    abs(
      v_total$focal_estimate[[1]] -
        0.346
    ) <
      0.001,
  paste0(
    "Observed=",
    ifelse(
      nrow(
        v_total
      ) ==
        1L,
      signif(
        v_total$focal_estimate[[1]],
        8
      ),
      NA
    )
  )
)

add_check(
  "C011",
  "Table 2 controlled Manufacturing interaction reproduces 1.545",
  nrow(
    v_mfg
  ) ==
    1L &&
    abs(
      v_mfg$focal_estimate[[1]] -
        1.545
    ) <
      0.001,
  paste0(
    "Observed=",
    ifelse(
      nrow(
        v_mfg
      ) ==
        1L,
      signif(
        v_mfg$focal_estimate[[1]],
        8
      ),
      NA
    )
  )
)

r30 <-
  read_csv(
    "outputs/r30_core_specification_curve_v1_0/01_core_specification_results.csv",
    show_col_types = FALSE
  )

r30_counts <-
  r30 |>
  count(
    level,
    control_set,
    name =
      "n"
  )

add_check(
  "C012",
  "R30 contains exactly 40 registered core specifications",
  nrow(
    r30
  ) ==
    40L,
  paste0(
    "Rows=",
    nrow(
      r30
    )
  )
)

add_check(
  "C013",
  "R30 contains 20 AC and 20 voter specifications",
  sum(
    r30$level ==
      "AC"
  ) ==
    20L &&
    sum(
      r30$level ==
        "Voter"
    ) ==
      20L,
  paste0(
    "AC=",
    sum(
      r30$level ==
        "AC"
    ),
    "; Voter=",
    sum(
      r30$level ==
        "Voter"
    )
  )
)

r31_registry <-
  read_csv(
    "config/r31_demographic_context_registry_v1_0.csv",
    show_col_types = FALSE
  )

add_check(
  "C014",
  "R31 authoritative demographic-context registry contains eight moderators",
  nrow(
    r31_registry
  ) ==
    8L,
  paste0(
    "Rows=",
    nrow(
      r31_registry
    )
  )
)

r31_measurement <-
  read_csv(
    "config/r31_measurement_decisions_v1_0.csv",
    show_col_types = FALSE
  )

add_check(
  "C015",
  "R31 measurement-decision registry exists and is nonempty",
  nrow(
    r31_measurement
  ) >
    0L,
  paste0(
    "Rows=",
    nrow(
      r31_measurement
    )
  )
)

a12_summary <-
  read_csv(
    "outputs/paper_appendix_final_diagnostics_v1_0/A12_influence_support/tables/A12a_influence_summary.csv",
    show_col_types = FALSE
  )

get_diag <- function(
  label
) {
  x <-
    a12_summary |>
    filter(
      diagnostic ==
        label
    )

  if (
    nrow(
      x
    ) !=
      1L
  ) {
    return(
      NA_real_
    )
  }

  x$value[[1]]
}

add_check(
  "C016",
  "AC01 leave-one-AC refits have zero sign reversals",
  identical(
    get_diag(
      "Leave-one-AC sign reversals"
    ),
    0
  ),
  paste0(
    "Sign reversals=",
    get_diag(
      "Leave-one-AC sign reversals"
    )
  )
)

add_check(
  "C017",
  "AC01 leave-one-state refits have zero sign reversals",
  identical(
    get_diag(
      "Leave-one-state sign reversals"
    ),
    0
  ),
  paste0(
    "Sign reversals=",
    get_diag(
      "Leave-one-state sign reversals"
    )
  )
)

taxonomy <-
  read_csv(
    "config/fdi_sector_taxonomy.csv",
    show_col_types = FALSE
  )

taxonomy_valid <-
  all(
    taxonomy$included_in_total
  ) &&
    all(
      xor(
        taxonomy$included_in_manufacturing,
        taxonomy$included_in_services
      )
    )

add_check(
  "C018",
  "FDI taxonomy partitions every registered activity into exactly one sector",
  taxonomy_valid,
  paste0(
    "Activities=",
    nrow(
      taxonomy
    )
  )
)

fdi_identity <-
  read_csv(
    "outputs/paper_appendix_descriptives_v1_0/figure_data/03_fdi_sector_identity_audit.csv",
    show_col_types = FALSE
  )

add_check(
  "C019",
  "FDI Total equals Manufacturing plus Services to numerical precision",
  all(
    fdi_identity$max_absolute_difference <
      1e-10
  ),
  paste0(
    "Max difference=",
    format(
      max(
        fdi_identity$max_absolute_difference
      ),
      scientific = TRUE
    )
  )
)

exclude_registry <-
  read_csv(
    "config/paper_artifact_exclusions_v1_0.csv",
    show_col_types = FALSE
  )

r31d_excluded <-
  any(
    exclude_registry$item ==
      "R/31d_unknown_duration_migration_sensitivity_v1_0.R" &
      exclude_registry$status ==
        "Do not use"
  )

add_check(
  "C020",
  "Unused R31d sensitivity is explicitly excluded",
  r31d_excluded,
  ifelse(
    r31d_excluded,
    "R31d marked Do not use",
    "R31d exclusion missing"
  )
)

live_registry_mentions_r31d <-
  any(
    grepl(
      "31d_unknown_duration",
      apply(
        registry,
        1,
        paste,
        collapse =
          " "
      )
    )
  )

add_check(
  "C021",
  "Authoritative v1.3 paper registry does not reference R31d",
  !live_registry_mentions_r31d,
  paste0(
    "Live references=",
    as.integer(
      live_registry_mentions_r31d
    )
  )
)

live_registry_mentions_stale_r30_demog <-
  any(
    grepl(
      "r30_demographic_moderator_registry",
      apply(
        registry,
        1,
        paste,
        collapse =
          " "
      )
    )
  )

add_check(
  "C022",
  "Authoritative v1.3 paper registry does not use stale R30 demographic registry",
  !live_registry_mentions_stale_r30_demog,
  paste0(
    "Live references=",
    as.integer(
      live_registry_mentions_stale_r30_demog
    )
  )
)

scripts_to_parse <-
  c(
    "R/25_ac_centrist_bjp_fdi_canonical_v1_0.R",
    "R/26_voter_centrist_bjp_fdi_canonical_v1_0.R",
    "R/27_post_primary_wald_diagnostics_v1_0.R",
    "R/27b_ac_ideology_outcome_heterogeneity_v1_0.R",
    "R/27c_ac_ideology_pairwise_wald_refinement_v1_0.R",
    "R/28_main_regression_table_models_v1_0.R",
    "R/29_manufacturing_marginal_effects_v1_0.R",
    "R/29b_manufacturing_marginal_effects_display_refinement_v1_0.R",
    "R/30a_construct_fdi_12m_temporal_robustness_v1_0.R",
    "R/30b_freeze_specification_curve_registry_v1_0.R",
    "R/30c_estimate_core_specification_curve_v1_0.R",
    "R/30d_refine_specification_curve_display_v1_0.R",
    "R/31a_audit_demographic_context_moderators_v1_0.R",
    "R/31a2_audit_district_allocation_and_construct_context_rates_v1_0.R",
    "R/31a3_audit_2001_population_migration_lineage_v1_0.R",
    "R/31a4_freeze_2001_denominator_migration_context_v1_0.R",
    "R/31a5_audit_migration_context_coverage_and_change_definition_v1_0.R",
    "R/31a6_construct_and_audit_2001_2011_migration_change_v1_0.R",
    "R/31a7_freeze_full_lineage_migration_context_v1_0.R",
    "R/31a8_freeze_corrected_employment_control_v1_0.R",
    "R/31b_estimate_demographic_context_robustness_v1_0.R",
    "R/31c_refine_demographic_context_display_v1_0.R",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "R/33_freeze_paper_artifact_registry_v1_0.R",
    "R/34_render_publication_tables_v1_0.R",
    "R/35_build_appendix_distributions_and_taxonomy_v1_0.R",
    "R/36_build_final_nes_and_influence_appendix_v1_0.R",
    "status_threat_puzzle_pipeline_party_rewrite_v6_1.R"
  )

missing_scripts <-
  scripts_to_parse[
    !file.exists(
      scripts_to_parse
    )
  ]

add_check(
  "C023",
  "Every current publication-analysis script exists",
  length(
    missing_scripts
  ) ==
    0L,
  if (
    length(
      missing_scripts
    ) ==
      0L
  ) {
    paste0(
      length(
        scripts_to_parse
      ),
      " scripts found"
    )
  } else {
    paste(
      missing_scripts,
      collapse =
        " | "
    )
  }
)

parse_results <-
  tibble(
    script =
      scripts_to_parse,
    parse_ok =
      vapply(
        scripts_to_parse,
        function(path) {
          tryCatch(
            {
              invisible(
                parse(
                  file =
                    path
                )
              )
              TRUE
            },
            error =
              function(e) {
                FALSE
              }
          )
        },
        logical(1)
      )
  )

add_check(
  "C024",
  "Every current publication-analysis script parses",
  all(
    parse_results$parse_ok
  ),
  paste0(
    sum(
      parse_results$parse_ok
    ),
    "/",
    nrow(
      parse_results
    ),
    " parse"
  )
)

copy_registry_artifact <- function(
  row
) {
  destination_root <-
    if (
      row$placement ==
        "Main"
    ) {
      main_dir
    } else {
      appendix_dir
    }

  destination <-
    file.path(
      destination_root,
      paste0(
        gsub(
          "[^A-Za-z0-9]+",
          "_",
          row$paper_id
        ),
        "__",
        basename(
          row$publication_artifact
        )
      )
    )

  ok <-
    file.copy(
      row$publication_artifact,
      destination,
      overwrite =
        TRUE
    )

  if (
    !ok
  ) {
    stop(
      "Failed to copy registered publication artifact: ",
      row$paper_id
    )
  }

  destination
}

publication_copy_registry <-
  bind_rows(
    lapply(
      seq_len(
        nrow(
          registry
        )
      ),
      function(i) {
        row <-
          registry[
            i,
            ,
            drop =
              FALSE
          ]

        destination <-
          copy_registry_artifact(
            row
          )

        tibble(
          paper_id =
            row$paper_id,
          placement =
            row$placement,
          artifact_type =
            row$artifact_type,
          source_path =
            row$publication_artifact,
          final_path =
            destination
        )
      }
    )
  )

unique_source_paths <-
  unique(
    registry$source_artifact
  )

source_copy_registry <-
  bind_rows(
    lapply(
      seq_along(
        unique_source_paths
      ),
      function(i) {
        source_path <-
          unique_source_paths[[i]]

        destination <-
          file.path(
            source_dir,
            paste0(
              sprintf(
                "%02d",
                i
              ),
              "__",
              basename(
                source_path
              )
            )
          )

        if (
          dir.exists(
            source_path
          )
        ) {
          stop(
            "Registry source artifact unexpectedly resolves to directory: ",
            source_path
          )
        }

        ok <-
          file.copy(
            source_path,
            destination,
            overwrite =
              TRUE
          )

        if (
          !ok
        ) {
          stop(
            "Failed to copy frozen source artifact: ",
            source_path
          )
        }

        tibble(
          source_path =
            source_path,
          final_path =
            destination
        )
      }
    )
  )

config_files <-
  c(
    "config/paper_artifacts_v1_3.csv",
    "config/paper_artifact_exclusions_v1_0.csv",
    "config/paper_display_decisions_v1_0.csv",
    "config/r30_control_set_registry_v1_0.csv",
    "config/r30_core_fdi_specification_registry_v1_0.csv",
    "config/r30_excluded_dimensions_v1_0.csv",
    "config/r31_demographic_context_registry_v1_0.csv",
    "config/r31_measurement_decisions_v1_0.csv",
    "config/fdi_sector_taxonomy.csv"
  )

config_copy_registry <-
  bind_rows(
    lapply(
      config_files,
      function(path) {
        if (
          !file.exists(
            path
          )
        ) {
          stop(
            "Missing required final config: ",
            path
          )
        }

        destination <-
          file.path(
            config_dir,
            basename(
              path
            )
          )

        ok <-
          file.copy(
            path,
            destination,
            overwrite =
              TRUE
          )

        if (
          !ok
        ) {
          stop(
            "Failed to copy final config: ",
            path
          )
        }

        tibble(
          source_path =
            path,
          final_path =
            destination
        )
      }
    )
  )

write_csv(
  certification,
  file.path(
    cert_dir,
    "01_certification_checks.csv"
  )
)

write_csv(
  parse_results,
  file.path(
    cert_dir,
    "02_script_parse_audit.csv"
  )
)

write_csv(
  publication_copy_registry,
  file.path(
    cert_dir,
    "03_publication_artifact_copy_registry.csv"
  )
)

write_csv(
  source_copy_registry,
  file.path(
    cert_dir,
    "04_frozen_source_copy_registry.csv"
  )
)

write_csv(
  config_copy_registry,
  file.path(
    cert_dir,
    "05_config_copy_registry.csv"
  )
)

write_csv(
  nes_check,
  file.path(
    cert_dir,
    "06_figure3_numerical_reproduction.csv"
  )
)

write_csv(
  fdi_identity,
  file.path(
    cert_dir,
    "07_fdi_sector_identity_audit.csv"
  )
)

all_final_files <-
  list.files(
    output_root,
    recursive =
      TRUE,
    full.names =
      TRUE
  )

final_manifest <-
  tibble(
    path =
      all_final_files,
    bytes =
      file.info(
        all_final_files
      )$size,
    md5 =
      unname(
        tools::md5sum(
          all_final_files
        )
      )
  )

write_csv(
  final_manifest,
  file.path(
    cert_dir,
    "08_final_output_manifest.csv"
  )
)

git_head <-
  system2(
    "git",
    c(
      "rev-parse",
      "HEAD"
    ),
    stdout =
      TRUE
  )

git_branch <-
  system2(
    "git",
    c(
      "branch",
      "--show-current"
    ),
    stdout =
      TRUE
  )

build_metadata <-
  tibble(
    field =
      c(
        "build_date",
        "git_head_before_final_commit",
        "git_branch",
        "registry",
        "registered_artifacts",
        "certification_checks",
        "failed_checks"
      ),

    value =
      c(
        as.character(
          Sys.Date()
        ),
        git_head[[1]],
        git_branch[[1]],
        registry_path,
        as.character(
          nrow(
            registry
          )
        ),
        as.character(
          nrow(
            certification
          )
        ),
        as.character(
          sum(
            !certification$passed
          )
        )
      )
  )

write_csv(
  build_metadata,
  file.path(
    cert_dir,
    "09_build_metadata.csv"
  )
)

readme <-
  c(
    "SWITCHERS-INDIA FINAL PAPER OUTPUTS V1.0",
    "",
    "This directory is assembled from config/paper_artifacts_v1_3.csv.",
    "",
    "main/ contains the six registered main figures and two main tables.",
    "",
    "appendix/ contains every registered appendix figure, table, mixed NES diagnostic package, and influence/support diagnostic package.",
    "",
    "frozen_sources/ contains one copy of every source artifact registered for the publication displays.",
    "",
    "config/ contains the authoritative publication, specification, measurement, and FDI taxonomy registries.",
    "",
    "certification/ contains numerical, lineage, parse, and checksum audits.",
    "",
    "No statistical model is estimated by R37.",
    "",
    "The git commit recorded in build metadata is the pre-final-commit HEAD. The final commit hash will therefore differ after these publication scripts and registries are committed."
  )

writeLines(
  readme,
  file.path(
    output_root,
    "README.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    cert_dir,
    "10_session_info.txt"
  )
)

failed <-
  certification |>
  filter(
    !passed
  )

cat(
  "\n===== FINAL CERTIFICATION =====\n"
)

print(
  certification,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FINAL ARTIFACT COUNTS =====\n"
)

print(
  registry |>
    count(
      placement,
      artifact_type,
      publication_status,
      name =
        "n"
    ) |>
    arrange(
      placement,
      artifact_type
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== PARSE AUDIT =====\n"
)

print(
  parse_results,
  n = Inf,
  width = Inf
)

cat(
  "\n===== BUILD METADATA =====\n"
)

print(
  build_metadata,
  n = Inf,
  width = Inf
)

if (
  nrow(
    failed
  ) >
    0L
) {
  cat(
    "\n===== FAILED CHECKS =====\n"
  )

  print(
    failed,
    n = Inf,
    width = Inf
  )

  stop(
    "R37 final certification failed."
  )
}

cat(
  "\nR37_FINAL_PUBLICATION_CERTIFICATION_COMPLETE\n"
)

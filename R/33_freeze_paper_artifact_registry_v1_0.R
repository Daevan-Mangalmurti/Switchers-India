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

output_dir <-
  file.path(
    "outputs",
    "paper_artifact_registry_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

r32_captions <-
  read_csv(
    "outputs/paper_descriptive_figures_v1_0/01_main_figure_captions_and_provenance.csv",
    show_col_types = FALSE
  )

if (
  nrow(
    r32_captions
  ) !=
    4L
) {
  stop(
    "Expected exactly four R32 main descriptive captions."
  )
}

r32_caption <- function(
  id
) {
  x <-
    r32_captions |>
    filter(
      figure_id ==
        id
    )

  if (
    nrow(
      x
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify ",
      id,
      " in R32 caption registry."
    )
  }

  x$caption[[1]]
}

main_registry <-
  tribble(
    ~paper_id,
    ~placement,
    ~artifact_type,
    ~publication_status,
    ~title,
    ~publication_artifact,
    ~source_artifact,
    ~generating_script,
    ~estimand_or_content,
    ~caption,

    "Figure 1",
    "Main",
    "Figure",
    "Built",
    "Far-right support over time by ideology",
    "outputs/paper_descriptive_figures_v1_0/main/Figure_1_aid_lmic_far_right_support_over_time.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_1_source_data.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Equal-country survey-wave mean far-right support by ideology in AIDs and LMICs; broad ideology definition",
    r32_caption(
      "Figure 1"
    ),

    "Figure 2",
    "Main",
    "Figure",
    "Built",
    "Adjusted probability of far-right support by ideology",
    "outputs/paper_descriptive_figures_v1_0/main/Figure_2_adjusted_far_right_support_by_ideology.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_2_adjusted_probabilities.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Country-fixed-effect LPM counterfactual-standardized probability of far-right support by ideology",
    r32_caption(
      "Figure 2"
    ),

    "Figure 3",
    "Main",
    "Figure",
    "Built",
    "BJP vote share by voter ideology, 2009 and 2014",
    "outputs/paper_descriptive_figures_v1_0/main/Figure_3_nes_bjp_vote_share_by_ideology.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_3_source_data.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Survey-weighted BJP vote share among audited Left, Center, and Right NES respondents in 2009 and 2014",
    r32_caption(
      "Figure 3"
    ),

    "Figure 4",
    "Main",
    "Figure",
    "Built",
    "Far-right support in India by ideology",
    "outputs/paper_descriptive_figures_v1_0/main/Figure_4_india_wvs_far_right_support_by_ideology.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Figure_4_source_data.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Observed India WVS far-right vote share by ideology; broad ideology definition",
    r32_caption(
      "Figure 4"
    ),

    "Table 1",
    "Main",
    "Table",
    "Source frozen; rendering pending",
    "Assembly-constituency models of centrist BJP support",
    NA_character_,
    "outputs/main_regression_table_models_v1_0/09_main_ac_table_summary.csv",
    "R/28_main_regression_table_models_v1_0.R",
    "Raw Total and Manufacturing FDI: additive, interaction without primary controls, and interaction with primary controls; state fixed effects and baseline FDI retained",
    paste0(
      "Assembly-constituency estimates of centrist BJP support in 2014. ",
      "Columns 1-3 use Total local FDI and columns 4-6 use Manufacturing local FDI. ",
      "Within each exposure family, the first column is additive, the second adds the Muslim-share interaction, ",
      "and the third adds the primary constituency controls. All specifications retain baseline FDI and state fixed effects. ",
      "Standard errors are clustered by parliamentary constituency."
    ),

    "Table 2",
    "Main",
    "Table",
    "Source frozen; rendering pending",
    "Voter-level models of centrist BJP support",
    NA_character_,
    "outputs/main_regression_table_models_v1_0/10_main_voter_table_summary.csv",
    "R/28_main_regression_table_models_v1_0.R",
    "Unweighted centrist-voter LPMs for raw Total and Manufacturing FDI with AC random intercepts and state fixed effects",
    paste0(
      "Voter-level linear probability models of BJP voting among 2014 centrist respondents. ",
      "Columns 1-3 use Total local FDI and columns 4-6 use Manufacturing local FDI. ",
      "Within each exposure family, the first column is additive, the second adds the Muslim-share interaction, ",
      "and the third adds the primary individual and constituency controls. ",
      "All specifications retain baseline FDI, state fixed effects, and an assembly-constituency random intercept."
    ),

    "Figure 5",
    "Main",
    "Figure",
    "Built",
    "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/07_main_figure_manufacturing_raw_1pp.pdf",
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/02_primary_1pp_curve_with_support_metrics.csv",
    "R/29b_manufacturing_marginal_effects_display_refinement_v1_0.R",
    "Change in BJP support associated with a +1 percentage-point Muslim population share across current raw Manufacturing FDI",
    paste0(
      "Marginal effect of a one-percentage-point increase in Muslim population share across current Manufacturing FDI exposure. ",
      "Panel A reports the assembly-constituency model and Panel B the voter-level model. ",
      "The calculation incorporates the fitted current-FDI interaction while averaging the baseline-FDI contribution over the corresponding estimation sample. ",
      "Shaded intervals are 95% confidence intervals. The x-axis reports raw Manufacturing FDI projects per 100,000 population."
    ),

    "Figure 6",
    "Main",
    "Figure",
    "Built",
    "Robustness of the FDI × Muslim-share relationship",
    "outputs/r30_specification_curve_display_refinement_v1_0/11_main_figure_6_ac_specification_robustness_final.pdf",
    "outputs/r30_specification_curve_display_refinement_v1_0/01_final_figure_data_reference_sd.csv",
    "R/30d_refine_specification_curve_display_v1_0.R",
    "Reference-standardized change in the +1-pp Muslim-share BJP-support gradient associated with a one-SD increase in registered Total or Manufacturing FDI exposure",
    paste0(
      "Assembly-constituency robustness of the FDI × Muslim-share interaction across registered Total and Manufacturing FDI definitions. ",
      "Effects are placed on a common scale using the Primary-sample reference standard deviation for each exposure family. ",
      "Primary and Expanded control sets are shown separately. ",
      "The 12-month temporal-window specifications are post-estimation robustness checks rather than part of the original primary specification."
    )
  )

appendix_registry <-
  tribble(
    ~paper_id,
    ~placement,
    ~artifact_type,
    ~publication_status,
    ~title,
    ~publication_artifact,
    ~source_artifact,
    ~generating_script,
    ~estimand_or_content,
    ~caption,

    "Appendix Figure A1",
    "Appendix",
    "Figure",
    "Built",
    "Far-right support over time using the narrow ideology definition",
    "outputs/paper_descriptive_figures_v1_0/appendix/Appendix_narrow_aid_lmic_far_right_support_over_time.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Appendix_narrow_wave_source_data.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Figure 1 analogue using Left 1-2, Moderate 5-6, Right 9-10",
    "Alternative ideology-definition version of Figure 1.",

    "Appendix Figure A2",
    "Appendix",
    "Figure",
    "Built",
    "Adjusted far-right support using the narrow ideology definition",
    "outputs/paper_descriptive_figures_v1_0/appendix/Appendix_narrow_adjusted_far_right_support_by_ideology.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Appendix_narrow_adjusted_probabilities.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Figure 2 analogue using Left 1-2, Moderate 5-6, Right 9-10",
    "Alternative ideology-definition version of Figure 2.",

    "Appendix Figure A3",
    "Appendix",
    "Figure",
    "Built",
    "India WVS far-right support using the narrow ideology definition",
    "outputs/paper_descriptive_figures_v1_0/appendix/Appendix_narrow_india_wvs_far_right_support_by_ideology.pdf",
    "outputs/paper_descriptive_figures_v1_0/figure_data/Appendix_narrow_india_source_data.csv",
    "R/32_build_main_descriptive_figures_v1_0.R",
    "Figure 4 analogue using Left 1-2, Moderate 5-6, Right 9-10",
    "Alternative ideology-definition version of Figure 4.",

    "Appendix Figure A4",
    "Appendix",
    "Figure",
    "Built",
    "Logged Manufacturing FDI and the Muslim-share gradient",
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/08_appendix_figure_manufacturing_log1p_1pp.pdf",
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/02_primary_1pp_curve_with_support_metrics.csv",
    "R/29b_manufacturing_marginal_effects_display_refinement_v1_0.R",
    "Logged-Manufacturing analogue of Figure 5",
    "Marginal-effect analogue of Figure 5 using log1p Manufacturing FDI.",

    "Appendix Figure A5",
    "Appendix",
    "Figure",
    "Built",
    "Marginal effect of Manufacturing FDI across Muslim population share",
    "outputs/manufacturing_marginal_effects_v1_0/09_appendix_reverse_manufacturing_plus1_project.pdf",
    "outputs/manufacturing_marginal_effects_v1_0/03_appendix_reverse_plus1_project_grid.csv",
    "R/29_manufacturing_marginal_effects_v1_0.R",
    "Average discrete effect of +1 Manufacturing FDI project per 100k across Muslim population share",
    "Reverse-derivative Manufacturing FDI marginal-effect display.",

    "Appendix Figure A6",
    "Appendix",
    "Figure",
    "Built",
    "Voter-level specification robustness",
    "outputs/r30_specification_curve_display_refinement_v1_0/12_appendix_voter_specification_robustness_final.pdf",
    "outputs/r30_specification_curve_display_refinement_v1_0/01_final_figure_data_reference_sd.csv",
    "R/30d_refine_specification_curve_display_v1_0.R",
    "Voter-level analogue of Figure 6",
    "Voter-level robustness of the FDI × Muslim-share relationship.",

    "Appendix Figure A7",
    "Appendix",
    "Figure",
    "Built",
    "Alternative demographic-context moderators",
    "outputs/r31_demographic_context_display_v1_0/01_appendix_demographic_context_robustness.pdf",
    "outputs/r31_demographic_context_display_v1_0/03_appendix_figure_data.csv",
    "R/31c_refine_demographic_context_display_v1_0.R",
    "FDI interaction across Muslim, migration, male-migration, and Bengali/Bhojpuri demographic-context moderators",
    paste0(
      "Alternative demographic-context interactions. Native model coefficients remain the inferential results; ",
      "the figure uses fixed reference scaling only to make differently measured moderators visually comparable. ",
      "Measures of 2001-2011 demographic change overlap part of the current FDI period and are interpreted as post-baseline contextual robustness checks."
    ),

    "Appendix Table A1",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Assembly-constituency log1p Total and Manufacturing FDI models",
    NA_character_,
    "outputs/main_regression_table_models_v1_0/11_appendix_ac_log_table_summary.csv",
    "R/28_main_regression_table_models_v1_0.R",
    "Log1p AC analogues of the main Total and Manufacturing models",
    "Assembly-constituency models using log1p Total and Manufacturing FDI.",

    "Appendix Table A2",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Voter-level log1p Total and Manufacturing FDI models",
    NA_character_,
    "outputs/main_regression_table_models_v1_0/12_appendix_voter_log_table_summary.csv",
    "R/28_main_regression_table_models_v1_0.R",
    "Log1p voter analogues of the main Total and Manufacturing models",
    "Voter-level models using log1p Total and Manufacturing FDI.",

    "Appendix Table A3",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Post-estimation Wald diagnostics",
    NA_character_,
    "outputs/post_primary_wald_diagnostics_v1_0/02_appendix_wald_table.csv",
    "R/27_post_primary_wald_diagnostics_v1_0.R",
    "Joint Wald tests motivated by observed correlation between baseline and current FDI",
    "Post-estimation Wald diagnostics motivated by the observed correlation between baseline and current exposure.",

    "Appendix Table A4",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Ideology-outcome heterogeneity",
    NA_character_,
    "outputs/ac_ideology_outcome_heterogeneity_v1_0/05_appendix_ideology_heterogeneity_table.csv",
    "R/27b_ac_ideology_outcome_heterogeneity_v1_0.R",
    "Native Center, Left, and Right BJP-share interaction estimates",
    "Assembly-constituency estimates using Center, Left, and Right BJP vote shares as alternative outcomes.",

    "Appendix Table A5",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Pairwise ideology heterogeneity Wald tests",
    NA_character_,
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/01_pairwise_common_sample_wald_tests.csv",
    "R/27c_ac_ideology_pairwise_wald_refinement_v1_0.R",
    "Maximal-overlap pairwise Wald comparisons of Center, Left, and Right interaction coefficients",
    "Pairwise Wald tests using maximal pair-specific common samples.",

    "Appendix Table A6",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Three-ideology omnibus Wald tests",
    NA_character_,
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/02_three_ideology_omnibus_wald_tests.csv",
    "R/27c_ac_ideology_pairwise_wald_refinement_v1_0.R",
    "Three-way omnibus equality tests; fragile high-correlation Manufacturing result treated as secondary",
    "Three-ideology omnibus Wald tests on the common three-outcome sample.",

    "Appendix Table A7",
    "Appendix",
    "Table",
    "Source frozen; rendering pending",
    "Core temporal and exposure-definition robustness",
    NA_character_,
    "outputs/r30_core_specification_curve_v1_0/01_core_specification_results.csv",
    "R/30c_estimate_core_specification_curve_v1_0.R",
    "Registered Total and Manufacturing 60m local raw/log, own-only, 21m, and post-estimation 12m specifications",
    "Full numerical results underlying the specification-robustness figures."
  )

planned_registry <-
  tribble(
    ~paper_id,
    ~placement,
    ~artifact_type,
    ~publication_status,
    ~title,
    ~publication_artifact,
    ~source_artifact,
    ~generating_script,
    ~estimand_or_content,
    ~caption,

    "Appendix Table A8",
    "Appendix",
    "Table",
    "Planned; not yet rendered",
    "Services FDI models",
    NA_character_,
    NA_character_,
    NA_character_,
    "Parallel Services FDI AC and voter specifications",
    "Services FDI robustness models.",

    "Appendix Table A9",
    "Appendix",
    "Table",
    "Planned; not yet rendered",
    "Official-vote contextual triple interactions",
    NA_character_,
    NA_character_,
    NA_character_,
    "Restricted and temporally saturated official BJP vote-share triple-interaction specifications",
    "Official-vote contextual triple-interaction robustness.",

    "Appendix Figure A8",
    "Appendix",
    "Figure",
    "Planned; not yet built",
    "FDI exposure distributions",
    NA_character_,
    NA_character_,
    NA_character_,
    "Total, Manufacturing, and Services FDI distributions and temporal-change distributions",
    "Distribution of constituency FDI exposure.",

    "Appendix Figure A9",
    "Appendix",
    "Figure",
    "Planned; not yet built",
    "Muslim population share distribution",
    NA_character_,
    NA_character_,
    NA_character_,
    "Distribution of 2001 Muslim population share across constituencies",
    "Distribution of constituency Muslim population share in 2001.",

    "Appendix Table A10",
    "Appendix",
    "Table",
    "Planned; not yet rendered",
    "FDI sector taxonomy",
    NA_character_,
    "config/fdi_sector_taxonomy.csv",
    NA_character_,
    "Authoritative mapping of FDI project industries into Manufacturing and Services",
    "FDI sector-classification taxonomy.",

    "Appendix Figure/Table A11",
    "Appendix",
    "Mixed",
    "Planned; not yet assembled",
    "NES classification and composition diagnostics",
    NA_character_,
    NA_character_,
    NA_character_,
    "NES ideology item coding, classification composition, party choice, income, and education diagnostics",
    "NES classification and descriptive robustness materials.",

    "Appendix Diagnostic A12",
    "Appendix",
    "Diagnostic",
    "Planned; not yet assembled",
    "Influence and support diagnostics",
    NA_character_,
    NA_character_,
    NA_character_,
    "AC influence diagnostics and FDI support diagnostics",
    "Influence and empirical-support diagnostics."
  )

artifact_registry <-
  bind_rows(
    main_registry,
    appendix_registry,
    planned_registry
  ) |>
  mutate(
    architecture_version =
      "v1.0",
    frozen_date =
      as.character(
        Sys.Date()
      )
  )

if (
  anyDuplicated(
    artifact_registry$paper_id
  ) >
    0L
) {
  stop(
    "Duplicate paper IDs in artifact registry."
  )
}

built_or_source_frozen <-
  artifact_registry |>
  filter(
    publication_status %in%
      c(
        "Built",
        "Source frozen; rendering pending"
      )
  )

required_source_paths <-
  built_or_source_frozen |>
  filter(
    !is.na(
      source_artifact
    ),
    nzchar(
      source_artifact
    )
  )

missing_sources <-
  required_source_paths |>
  filter(
    !file.exists(
      source_artifact
    )
  )

if (
  nrow(
    missing_sources
  ) >
    0L
) {
  print(
    missing_sources,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one frozen source artifact is missing."
  )
}

built_publication <-
  artifact_registry |>
  filter(
    publication_status ==
      "Built"
  )

missing_publication <-
  built_publication |>
  filter(
    is.na(
      publication_artifact
    ) |
      !file.exists(
        publication_artifact
      )
  )

if (
  nrow(
    missing_publication
  ) >
    0L
) {
  print(
    missing_publication,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one Built publication artifact is missing."
  )
}

main_check <-
  artifact_registry |>
  filter(
    placement ==
      "Main"
  ) |>
  summarise(
    n_main =
      n(),
    n_figures =
      sum(
        artifact_type ==
          "Figure"
      ),
    n_tables =
      sum(
        artifact_type ==
          "Table"
      ),
    n_built_figures =
      sum(
        artifact_type ==
          "Figure" &
          publication_status ==
            "Built"
      ),
    n_table_sources_frozen =
      sum(
        artifact_type ==
          "Table" &
          publication_status ==
            "Source frozen; rendering pending"
      )
  )

if (
  main_check$n_main !=
    8L ||
    main_check$n_figures !=
      6L ||
    main_check$n_tables !=
      2L ||
    main_check$n_built_figures !=
      6L ||
    main_check$n_table_sources_frozen !=
      2L
) {
  print(
    main_check,
    width = Inf
  )

  stop(
    "Main display architecture is not exactly six figures plus two tables."
  )
}

provenance_paths <-
  artifact_registry |>
  transmute(
    paper_id,
    publication_status,
    publication_artifact,
    source_artifact
  ) |>
  tidyr::pivot_longer(
    cols =
      c(
        publication_artifact,
        source_artifact
      ),
    names_to =
      "path_role",
    values_to =
      "path"
  ) |>
  filter(
    !is.na(
      path
    ),
    nzchar(
      path
    ),
    file.exists(
      path
    )
  ) |>
  mutate(
    bytes =
      file.info(
        path
      )$size,
    md5 =
      unname(
        tools::md5sum(
          path
        )
      )
  )

exclusions <-
  tribble(
    ~item,
    ~status,
    ~replacement_or_decision,
    ~reason,

    "R/31d_unknown_duration_migration_sensitivity_v1_0.R",
    "Do not use",
    "No replacement analysis requested",
    "Authors explicitly chose not to pursue the unknown-duration migration sensitivity",

    "outputs/manufacturing_marginal_effects_v1_0/07_main_candidate_manufacturing_marginal_effects_1pp.pdf",
    "Superseded",
    "Figure 5 from R29b",
    "Four-panel raw-plus-log candidate superseded by raw-only two-panel main figure",

    "config/r30_demographic_moderator_registry_v1_0.csv",
    "Superseded",
    "config/r31_demographic_context_registry_v1_0.csv",
    "R30 registry predates corrected migration lineage and demographic-context freeze",

    "outputs/r30_specification_registry_freeze_v1_0/02_frozen_demographic_context_registry.csv",
    "Superseded",
    "config/r31_demographic_context_registry_v1_0.csv",
    "R31 is authoritative for demographic-context moderators",

    "outputs/r30_pre_employment_regeneration_snapshot_v1_0",
    "Internal QA only",
    "Current regenerated R30 outputs",
    "Snapshot exists solely to certify employment-correction invariance",

    "outputs/r31_2001_denominator_migration_context_v1_0",
    "Superseded analytical branch",
    "outputs/r31_full_lineage_migration_context_v1_0",
    "Full-lineage R31a7 construction supersedes the incomplete earlier migration mapping",

    "R/31a6_construct_and_audit_2001_2011_migration_change_v1_0.R",
    "Superseded analytical branch",
    "R/31a7_freeze_full_lineage_migration_context_v1_0.R",
    "R31a7 supplies the authoritative corrected lineage-based migration context"
  )

write_csv(
  artifact_registry,
  "config/paper_artifacts_v1_0.csv"
)

write_csv(
  exclusions,
  "config/paper_artifact_exclusions_v1_0.csv"
)

write_csv(
  provenance_paths,
  file.path(
    output_dir,
    "01_frozen_artifact_provenance.csv"
  )
)

write_csv(
  artifact_registry |>
    filter(
      publication_status %in%
        c(
          "Planned; not yet rendered",
          "Planned; not yet built",
          "Planned; not yet assembled"
        )
    ),
  file.path(
    output_dir,
    "02_remaining_publication_artifacts.csv"
  )
)

write_csv(
  main_registry,
  file.path(
    output_dir,
    "03_main_display_registry.csv"
  )
)

write_csv(
  appendix_registry,
  file.path(
    output_dir,
    "04_built_or_source_frozen_appendix_registry.csv"
  )
)

write_csv(
  main_check,
  file.path(
    output_dir,
    "05_main_architecture_check.csv"
  )
)

notes <-
  c(
    "R33 PAPER ARTIFACT REGISTRY FREEZE",
    "",
    "The main empirical architecture is frozen at six figures and two tables.",
    "",
    "Figures 1-4 are generated by R32.",
    "Figure 5 is the raw Manufacturing marginal-effect figure generated by R29b.",
    "Figure 6 is the final AC specification-robustness figure generated by R30d.",
    "",
    "Tables 1 and 2 have frozen R28 source data but still require publication rendering.",
    "",
    "The appendix registry distinguishes already-built figures, frozen table sources awaiting rendering, and planned artifacts not yet assembled.",
    "",
    "The R31 demographic-context registry supersedes the stale R30 demographic-moderator registry.",
    "",
    "R31d is explicitly excluded from the publication pipeline because the authors chose not to pursue that sensitivity analysis.",
    "",
    "The R30 pre-employment snapshot is retained only as internal QA evidence and must not be used as a publication source.",
    "",
    "No statistical model is estimated or altered by this script."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "06_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "07_session_info.txt"
  )
)

cat(
  "\n===== MAIN DISPLAY ARCHITECTURE =====\n"
)

print(
  main_registry |>
    select(
      paper_id,
      artifact_type,
      publication_status,
      title,
      publication_artifact,
      source_artifact
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== MAIN ARCHITECTURE CHECK =====\n"
)

print(
  main_check,
  width = Inf
)

cat(
  "\n===== BUILT / SOURCE-FROZEN APPENDIX =====\n"
)

print(
  appendix_registry |>
    select(
      paper_id,
      artifact_type,
      publication_status,
      title
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== REMAINING PUBLICATION ARTIFACTS =====\n"
)

print(
  planned_registry |>
    select(
      paper_id,
      artifact_type,
      publication_status,
      title
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== EXCLUSIONS / SUPERSESSIONS =====\n"
)

print(
  exclusions,
  n = Inf,
  width = Inf
)

cat(
  "\nR33_PAPER_ARTIFACT_REGISTRY_FREEZE_COMPLETE\n"
)

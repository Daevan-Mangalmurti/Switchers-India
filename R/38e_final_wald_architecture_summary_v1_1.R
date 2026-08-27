suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# =============================================================================
# R38E v1.1
# Final heterogeneity / Wald architecture after R38A3 and R38C4
#
# IMPORTANT CONCEPTUAL CHANGE FROM v1.0
#
# R38A3 is NOT a four-category ideology-composition model.
# It is an AC-level contextual triple interaction in which the third variable is
# one continuous constituency moderator:
#
#   share of 2014 ideology-complete NES respondents in the AC who are Center.
#
# Therefore R38A3 is reported separately from the three genuine pairwise
# ideology-comparison families:
#
#   1. R38B  : AC ideology-specific outcomes
#   2. R38C3 : separately fitted voter ideology models + AC-cluster bootstrap
#   3. R38C4 : one pooled voter model, CENTER reference
#
# This script performs NO model estimation.
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

output_dir <- file.path(
  project_root,
  "outputs",
  "r38e_final_wald_architecture_summary_v1_1"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  ac_native = file.path(
    project_root,
    "outputs",
    "r38b_ac_four_ideology_heterogeneity_v1_1",
    "04_PRIMARY_union_native_pairwise_wald_tests.csv"
  ),

  voter_native = file.path(
    project_root,
    "outputs",
    "r38c3_native_voter_cluster_bootstrap_wald_v1_0",
    "06_PRIMARY_native_model_pairwise_cluster_bootstrap_wald_tests.csv"
  ),

  voter_pooled_pairwise = file.path(
    project_root,
    "outputs",
    "r38c4_voter_center_reference_four_ideology_wald_v1_0",
    "03_pairwise_wald_tests.csv"
  ),

  voter_pooled_direct = file.path(
    project_root,
    "outputs",
    "r38c4_voter_center_reference_four_ideology_wald_v1_0",
    "01_center_reference_direct_coefficients.csv"
  ),

  voter_pooled_implied = file.path(
    project_root,
    "outputs",
    "r38c4_voter_center_reference_four_ideology_wald_v1_0",
    "02_implied_group_fdi_muslim_interactions.csv"
  ),

  voter_pooled_omnibus = file.path(
    project_root,
    "outputs",
    "r38c4_voter_center_reference_four_ideology_wald_v1_0",
    "04_four_group_omnibus_wald_tests.csv"
  ),

  voter_reference_equivalence = file.path(
    project_root,
    "outputs",
    "r38c4_voter_center_reference_four_ideology_wald_v1_0",
    "07_optional_left_reference_equivalence_check.csv"
  ),

  ac_contextual_focal = file.path(
    project_root,
    "outputs",
    "r38a3_ac_2014_centrist_share_triple_v1_1",
    "02_focal_current_and_baseline_triple_coefficients.csv"
  ),

  ac_contextual_wald = file.path(
    project_root,
    "outputs",
    "r38a3_ac_2014_centrist_share_triple_v1_1",
    "03_wald_tests.csv"
  ),

  ac_contextual_sample = file.path(
    project_root,
    "outputs",
    "r38a3_ac_2014_centrist_share_triple_v1_1",
    "05_sample_summary.csv"
  )
)

required_paths <- unlist(paths, use.names = FALSE)
missing_files <- required_paths[!file.exists(required_paths)]

if (length(missing_files) > 0L) {
  stop(
    "Missing required completed R38 output(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

ac_native <- read_csv(paths$ac_native, show_col_types = FALSE)
voter_native <- read_csv(paths$voter_native, show_col_types = FALSE)
voter_pooled_pairwise <- read_csv(
  paths$voter_pooled_pairwise,
  show_col_types = FALSE
)
voter_pooled_direct <- read_csv(
  paths$voter_pooled_direct,
  show_col_types = FALSE
)
voter_pooled_implied <- read_csv(
  paths$voter_pooled_implied,
  show_col_types = FALSE
)
voter_pooled_omnibus <- read_csv(
  paths$voter_pooled_omnibus,
  show_col_types = FALSE
)
voter_reference_equivalence <- read_csv(
  paths$voter_reference_equivalence,
  show_col_types = FALSE
)
ac_contextual_focal <- read_csv(
  paths$ac_contextual_focal,
  show_col_types = FALSE
)
ac_contextual_wald <- read_csv(
  paths$ac_contextual_wald,
  show_col_types = FALSE
)
ac_contextual_sample <- read_csv(
  paths$ac_contextual_sample,
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 1. Three genuine pairwise ideology-comparison families
# -----------------------------------------------------------------------------

ac_pairwise_clean <- ac_native |>
  filter(
    functional_form == "Raw",
    sector %in% c("Total", "Manufacturing")
  ) |>
  transmute(
    analysis_level = "AC",
    model_family = "Ideology-specific BJP-share outcomes",
    dependent_variable =
      "BJP vote share among 2014 NES respondents in ideology group",
    comparison_method =
      "Union-stacked Wald of native group-specific FDI x Muslim coefficients",
    sector,
    contrast_id,
    ideology_a,
    ideology_b,
    estimate = difference,
    std_error,
    p_value = cluster_df_F_p,
    inference_note = paste0(
      "PC-clustered F test; native AC samples A=",
      native_n_ac_a,
      ", B=",
      native_n_ac_b,
      "; union=",
      n_union_ac,
      "; overlap=",
      n_overlap_ac
    )
  )

voter_native_clean <- voter_native |>
  filter(
    sector %in% c("Total", "Manufacturing")
  ) |>
  transmute(
    analysis_level = "Voter",
    model_family = "Separately fitted ideology-specific mixed LPMs",
    dependent_variable = "Whether 2014 respondent voted BJP",
    comparison_method =
      "AC-cluster bootstrap covariance + Wald comparison of native coefficients",
    sector,
    contrast_id,
    ideology_a,
    ideology_b,
    estimate = observed_difference,
    std_error = bootstrap_se_difference,
    p_value = wald_p_normal,
    inference_note = paste0(
      "Complete bootstrap reps=",
      complete_bootstrap_replicates,
      "; centered-bootstrap p diagnostic=",
      signif(centered_bootstrap_p_diagnostic, 4)
    )
  )

voter_pooled_clean <- voter_pooled_pairwise |>
  filter(
    period == "current",
    sector %in% c("Total", "Manufacturing")
  ) |>
  transmute(
    analysis_level = "Voter",
    model_family =
      "Pooled four-ideology triple-interaction mixed LPM",
    dependent_variable = "Whether 2014 respondent voted BJP",
    comparison_method =
      "Wald contrast within one pooled CENTER-reference model",
    sector,
    contrast_id,
    ideology_a,
    ideology_b,
    estimate,
    std_error,
    p_value,
    inference_note =
      "Center is the explicit reference category; all six pairwise contrasts are linear combinations of the same fitted model"
  )

pairwise_summary <- bind_rows(
  ac_pairwise_clean,
  voter_native_clean,
  voter_pooled_clean
) |>
  arrange(
    sector,
    contrast_id,
    analysis_level,
    model_family
  )

write_csv(
  pairwise_summary,
  file.path(
    output_dir,
    "01_three_pairwise_ideology_heterogeneity_families.csv"
  )
)

center_focus <- pairwise_summary |>
  filter(
    contrast_id %in% c(
      "center_vs_left",
      "center_vs_right",
      "center_vs_mixed"
    )
  )

write_csv(
  center_focus,
  file.path(
    output_dir,
    "02_center_focused_pairwise_ideology_heterogeneity.csv"
  )
)

# -----------------------------------------------------------------------------
# 2. R38A3 AC contextual triple
#    Not a pairwise ideology-category Wald family.
# -----------------------------------------------------------------------------

contextual_focal <- ac_contextual_focal |>
  transmute(
    analysis_level = "AC",
    model_family =
      "Official BJP vote share x continuous 2014 Center-share contextual triple",
    dependent_variable = "Official 2014 BJP AC vote share",
    sector,
    period,
    term,
    estimate,
    std_error,
    coefficient_statistic = statistic,
    coefficient_p_value = p_value,
    contextual_moderator =
      "Unweighted Center / all ideology-complete 2014 NES respondents in AC",
    hierarchy =
      "Full conventional main effects + two-way interactions + triple interaction"
  )

contextual_wald <- ac_contextual_wald |>
  transmute(
    analysis_level = "AC",
    sector,
    test_family,
    restriction,
    estimate,
    std_error,
    wald_chisq,
    chi_square_df,
    chi_square_p,
    wald_F,
    F_df1,
    F_df2,
    cluster_df_F_p
  )

write_csv(
  contextual_focal,
  file.path(
    output_dir,
    "03_ac_contextual_center_share_triple_coefficients.csv"
  )
)

write_csv(
  contextual_wald,
  file.path(
    output_dir,
    "04_ac_contextual_center_share_triple_wald_tests.csv"
  )
)

write_csv(
  ac_contextual_sample,
  file.path(
    output_dir,
    "05_ac_contextual_center_share_sample_summary.csv"
  )
)

# -----------------------------------------------------------------------------
# 3. R38C4 Center-reference pooled model summaries
# -----------------------------------------------------------------------------

voter_direct_current <- voter_pooled_direct |>
  filter(
    period == "current",
    sector %in% c("Total", "Manufacturing")
  ) |>
  arrange(
    factor(sector, levels = c("Total", "Manufacturing")),
    factor(
      ideology_comparison,
      levels = c("Center", "Left", "Right", "Mixed")
    )
  )

voter_implied_current <- voter_pooled_implied |>
  filter(
    period == "current",
    sector %in% c("Total", "Manufacturing")
  ) |>
  arrange(
    factor(sector, levels = c("Total", "Manufacturing")),
    factor(
      ideology,
      levels = c("Center", "Left", "Right", "Mixed")
    )
  )

voter_omnibus_current <- voter_pooled_omnibus |>
  filter(
    period == "current",
    sector %in% c("Total", "Manufacturing")
  )

write_csv(
  voter_direct_current,
  file.path(
    output_dir,
    "06_voter_center_reference_direct_coefficients_current.csv"
  )
)

write_csv(
  voter_implied_current,
  file.path(
    output_dir,
    "07_voter_implied_group_fdi_muslim_interactions_current.csv"
  )
)

write_csv(
  voter_omnibus_current,
  file.path(
    output_dir,
    "08_voter_four_group_omnibus_current.csv"
  )
)

write_csv(
  voter_reference_equivalence,
  file.path(
    output_dir,
    "09_center_vs_left_reference_equivalence_audit.csv"
  )
)

# -----------------------------------------------------------------------------
# 4. Architecture registry / notes
# -----------------------------------------------------------------------------

architecture <- tribble(
  ~component, ~script, ~estimand, ~comparison,
  "A3",
  "R/38a3_ac_2014_centrist_share_triple_v1_1.R",
  paste0(
    "Official 2014 BJP AC vote share; current/baseline FDI x Muslim share x ",
    "continuous 2014 AC Center share"
  ),
  "Direct contextual triple coefficient; no omitted ideology category",

  "B",
  "R/38b_ac_four_ideology_heterogeneity_v1_1.R",
  "Ideology-specific AC BJP-share outcomes",
  "Pairwise native FDI x Muslim coefficients via union-stacked Wald",

  "C3",
  "R/38c3_native_voter_cluster_bootstrap_wald_v1_0.R",
  "Separately fitted ideology-specific voter mixed LPMs",
  "Pairwise native FDI x Muslim coefficients via AC-cluster bootstrap covariance",

  "C4",
  "R/38c4_voter_center_reference_four_ideology_wald_v1_0.R",
  "One pooled voter mixed LPM with Center reference",
  "Direct and linear-combination pairwise differences plus four-group omnibus"
)

write_csv(
  architecture,
  file.path(
    output_dir,
    "10_final_r38_inference_architecture_registry.csv"
  )
)

notes <- c(
  "R38E v1.1 — FINAL HETEROGENEITY / WALD ARCHITECTURE",
  "",
  "R38A2 and R38C2 are intentionally no longer inputs.",
  "",
  "R38A3 is not a compositional-category comparison.",
  "Its third variable is one continuous constituency-level moderator:",
  "the share of 2014 ideology-complete NES respondents in the AC who are Center.",
  "There is no omitted ideology composition category.",
  "",
  "Accordingly, R38A3 is reported separately from the three genuine pairwise ideology-comparison families.",
  "",
  "R38C4 uses Center as the explicit reference category.",
  "A pure reference-category change must not alter fitted values, N, or log likelihood;",
  "the stored equivalence audit is carried forward here.",
  "",
  "No statistical model is fitted by R38E v1.1."
)

writeLines(
  notes,
  file.path(
    output_dir,
    "11_notes.txt"
  )
)

cat("\n===== R38E v1.1 CENTER-FOCUSED PAIRWISE SUMMARY =====\n\n")
print(center_focus, n = Inf, width = Inf)

cat("\n===== R38E v1.1 AC CONTEXTUAL TRIPLE =====\n\n")
print(contextual_focal, n = Inf, width = Inf)

cat("\n===== R38E v1.1 AC CONTEXTUAL WALD TESTS =====\n\n")
print(contextual_wald, n = Inf, width = Inf)

cat("\n===== R38E v1.1 VOTER CENTER-REFERENCE DIRECT COEFFICIENTS =====\n\n")
print(voter_direct_current, n = Inf, width = Inf)

cat("\n===== R38E v1.1 VOTER IMPLIED GROUP INTERACTIONS =====\n\n")
print(voter_implied_current, n = Inf, width = Inf)

cat("\n===== R38E v1.1 VOTER OMNIBUS =====\n\n")
print(voter_omnibus_current, n = Inf, width = Inf)

cat("\nOUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38E_V1_1_COMPLETE\n")

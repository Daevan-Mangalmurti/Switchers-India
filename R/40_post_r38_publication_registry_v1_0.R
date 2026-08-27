suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

output_dir <- file.path("outputs", "r40_post_r38_publication_registry_v1_0")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_file <- function(path) {
  if (!file.exists(path)) stop("Missing required R40 input: ", path)
  path
}

registry <- read_csv(
  required_file("config/paper_artifacts_v1_3.csv"),
  show_col_types = FALSE
)

display_decisions <- read_csv(
  required_file("config/paper_display_decisions_v1_0.csv"),
  show_col_types = FALSE
)

figure5_a <- "outputs/r38d_fig5_display_patch_v1_4/06_fig5_option_A_p90_p95_FINAL.pdf"
figure5_d <- "outputs/r38d_fig5_display_patch_v1_4/09_fig5_option_D_aligned_hist_below_FINAL.pdf"
figure5_source <- "outputs/r38d_fig5_display_patch_v1_4/00_figure5_curve_with_90_95_ci.csv"

a4_a <- "outputs/r39_marginal_effect_display_standard_v1_1/appendix_A4_log1p_1pp__option_A_p90_p95.pdf"
a4_d <- "outputs/r39_marginal_effect_display_standard_v1_1/appendix_A4_log1p_1pp__option_D_aligned_hist_below.pdf"
a5_a <- "outputs/r39_marginal_effect_display_standard_v1_1/appendix_A5_reverse_plus1__option_A_p90_p95.pdf"
a5_d <- "outputs/r39_marginal_effect_display_standard_v1_1/appendix_A5_reverse_plus1__option_D_aligned_hist_below.pdf"

r39_source <- "outputs/r39_marginal_effect_display_standard_v1_1/00_all_harmonized_curve_data_90_95_ci.csv"

required_promoted <- c(
  figure5_a, figure5_d, figure5_source,
  a4_a, a4_d, a5_a, a5_d, r39_source,
  "R/38d_fig5_display_patch_v1_4.R",
  "R/39_marginal_effect_display_standard_v1_1.R"
)

missing_promoted <- required_promoted[!file.exists(required_promoted)]
if (length(missing_promoted) > 0L) {
  stop(
    "Missing promoted R38/R39 publication file(s):\n",
    paste(missing_promoted, collapse = "\n")
  )
}

target_ids <- c("Figure 5", "Appendix Figure A4", "Appendix Figure A5")
if (!all(target_ids %in% registry$paper_id) || anyDuplicated(registry$paper_id) > 0L) {
  stop("Unexpected paper_artifacts_v1_3 registry structure.")
}

updates <- tribble(
  ~paper_id, ~publication_artifact, ~source_artifact, ~generating_script, ~estimand_or_content, ~caption,

  "Figure 5",
  figure5_a,
  figure5_source,
  "R/38d_fig5_display_patch_v1_4.R",
  "Change in BJP support associated with a +1 percentage-point increase in Muslim population share across current raw Manufacturing FDI",
  paste0(
    "Marginal effect of a one-percentage-point increase in Muslim population share across current raw Manufacturing FDI exposure. ",
    "Panel A reports the assembly-constituency model and Panel B the voter-level model. ",
    "The black line is the point estimate; inner and outer shaded bands are 90% and 95% confidence intervals; ",
    "the dashed horizontal line marks zero effect. Option A is the compact registered rendering; ",
    "Option D is retained as a paper-ready alternate with an aligned empirical-support histogram. ",
    "No model was re-estimated for the display."
  ),

  "Appendix Figure A4",
  a4_a,
  r39_source,
  "R/39_marginal_effect_display_standard_v1_1.R",
  "Logged-Manufacturing analogue of Figure 5 using the completed log1p specification",
  paste0(
    "Logged-Manufacturing analogue of Figure 5. The black line is the point estimate; ",
    "inner and outer bands are 90% and 95% confidence intervals; the dashed line marks zero effect. ",
    "Option A is the compact registered rendering and Option D is retained as an alternate with aligned empirical support. ",
    "No model was re-estimated."
  ),

  "Appendix Figure A5",
  a5_a,
  r39_source,
  "R/39_marginal_effect_display_standard_v1_1.R",
  "Average discrete effect of +1 Manufacturing FDI project per 100,000 across 2001 Muslim population share",
  paste0(
    "Reverse-derivative Manufacturing FDI marginal-effect display. The x-axis is 2001 Muslim population share. ",
    "The black line is the point estimate; inner and outer bands are 90% and 95% confidence intervals; ",
    "the dashed line marks zero effect. Option A is the compact registered rendering and Option D is retained as an alternate. ",
    "No model was re-estimated."
  )
)

registry_v1_4 <- registry |>
  select(-any_of(c("architecture_version", "frozen_date"))) |>
  left_join(
    updates |>
      rename(
        new_publication_artifact = publication_artifact,
        new_source_artifact = source_artifact,
        new_generating_script = generating_script,
        new_estimand_or_content = estimand_or_content,
        new_caption = caption
      ),
    by = "paper_id",
    relationship = "one-to-one"
  ) |>
  mutate(
    publication_artifact = coalesce(new_publication_artifact, publication_artifact),
    source_artifact = coalesce(new_source_artifact, source_artifact),
    generating_script = coalesce(new_generating_script, generating_script),
    estimand_or_content = coalesce(new_estimand_or_content, estimand_or_content),
    caption = coalesce(new_caption, caption),
    architecture_version = "v1.4",
    frozen_date = as.character(Sys.Date())
  ) |>
  select(
    -new_publication_artifact,
    -new_source_artifact,
    -new_generating_script,
    -new_estimand_or_content,
    -new_caption
  )

missing_built <- registry_v1_4 |>
  filter(
    publication_status == "Built",
    is.na(publication_artifact) | !file.exists(publication_artifact)
  )

if (nrow(missing_built) > 0L) {
  print(missing_built, n = Inf, width = Inf)
  stop("At least one Built artifact is missing after R40.")
}

missing_sources <- registry_v1_4 |>
  filter(!is.na(source_artifact), nzchar(source_artifact), !file.exists(source_artifact))

if (nrow(missing_sources) > 0L) {
  print(missing_sources, n = Inf, width = Inf)
  stop("At least one registered source artifact is missing after R40.")
}

write_csv(registry_v1_4, "config/paper_artifacts_v1_4.csv")

alternate_registry <- tribble(
  ~paper_id, ~placement, ~artifact_type, ~variant_id, ~publication_status, ~title,
  ~publication_artifact, ~source_artifact, ~generating_script, ~notes,

  "Figure 5", "Main", "Figure", "Option D", "Built",
  "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
  figure5_d, figure5_source, "R/38d_fig5_display_patch_v1_4.R",
  "Paper-ready alternate with aligned empirical-support histogram below the marginal-effect curve.",

  "Appendix Figure A4", "Appendix", "Figure", "Option D", "Built",
  "Logged Manufacturing FDI and the Muslim-share gradient",
  a4_d, r39_source, "R/39_marginal_effect_display_standard_v1_1.R",
  "Paper-ready alternate with aligned empirical-support histogram below the marginal-effect curve.",

  "Appendix Figure A5", "Appendix", "Figure", "Option D", "Built",
  "Marginal effect of Manufacturing FDI across Muslim population share",
  a5_d, r39_source, "R/39_marginal_effect_display_standard_v1_1.R",
  "Paper-ready alternate with aligned Muslim-share support histogram below the marginal-effect curve."
)

if (
  any(!file.exists(alternate_registry$publication_artifact)) ||
  any(!file.exists(alternate_registry$source_artifact))
) {
  stop("At least one retained Option-D alternate or source is missing.")
}

write_csv(alternate_registry, "config/paper_artifact_alternates_v1_0.csv")

display_v1_1 <- display_decisions |>
  mutate(
    generating_script = case_when(
      decision_id == "MFG-ME-RAW-1PP" ~ "R/38d_fig5_display_patch_v1_4.R",
      decision_id %in% c("MFG-ME-LOG-1PP", "MFG-ME-RAW-10PP", "MFG-ME-LOG-10PP", "MFG-ME-REVERSE") ~
        "R/39_marginal_effect_display_standard_v1_1.R",
      TRUE ~ generating_script
    ),
    output_stub = case_when(
      decision_id == "MFG-ME-RAW-1PP" ~
        "06_fig5_option_A_p90_p95_FINAL [default]; 09_fig5_option_D_aligned_hist_below_FINAL [alternate]",
      decision_id == "MFG-ME-LOG-1PP" ~
        "appendix_A4_log1p_1pp__option_A_p90_p95 [default]; appendix_A4_log1p_1pp__option_D_aligned_hist_below [alternate]",
      decision_id == "MFG-ME-RAW-10PP" ~
        "review_10pp__option_A_p90_p95 / review_10pp__option_D_aligned_hist_below",
      decision_id == "MFG-ME-LOG-10PP" ~
        "review-only legacy log1p 10pp display; not a registered paper artifact",
      decision_id == "MFG-ME-REVERSE" ~
        "appendix_A5_reverse_plus1__option_A_p90_p95 [default]; appendix_A5_reverse_plus1__option_D_aligned_hist_below [alternate]",
      TRUE ~ output_stub
    ),
    notes = case_when(
      decision_id == "MFG-ME-RAW-1PP" ~
        "Final R38 grammar: point estimate, 90% CI, 95% CI, zero line, key, empirical support. Retain A and D.",
      decision_id == "MFG-ME-LOG-1PP" ~
        "Appendix analogue under final R39 grammar. Retain A and D; B/C review-only.",
      decision_id == "MFG-ME-RAW-10PP" ~
        "Internal review scale; A/D retained if useful, not required as a paper artifact.",
      decision_id == "MFG-ME-LOG-10PP" ~
        "Internal review only; not part of the current registered paper architecture.",
      decision_id == "MFG-ME-REVERSE" ~
        "Appendix reverse derivative under final R39 grammar. Retain A and D; B/C review-only.",
      TRUE ~ notes
    )
  )

write_csv(display_v1_1, "config/paper_display_decisions_v1_1.csv")

promotion_check <- registry_v1_4 |>
  filter(paper_id %in% target_ids) |>
  select(paper_id, publication_artifact, source_artifact, generating_script, architecture_version)

write_csv(
  promotion_check,
  file.path(output_dir, "01_promoted_artifact_registry_rows.csv")
)
write_csv(
  alternate_registry,
  file.path(output_dir, "02_retained_option_d_alternates.csv")
)

writeLines(
  c(
    "R40 POST-R38/R39 PUBLICATION REGISTRY REFRESH",
    "",
    "paper_artifacts_v1_3.csv is preserved as the historical pre-R38 freeze.",
    "paper_artifacts_v1_4.csv is the current post-R38/R39 authoritative registry.",
    "",
    "Option A is the compact/default publication artifact for Figure 5, A4, and A5.",
    "Option D is retained explicitly in paper_artifact_alternates_v1_0.csv.",
    "Options B/C are review-only.",
    "",
    "No statistical model is fitted or altered by R40."
  ),
  file.path(output_dir, "03_readme.txt")
)

cat("\n===== R40 PROMOTED DEFAULT ARTIFACTS =====\n\n")
print(promotion_check, n = Inf, width = Inf)
cat("\n===== R40 RETAINED OPTION-D ALTERNATES =====\n\n")
print(alternate_registry, n = Inf, width = Inf)
cat("\nR40_POST_R38_REGISTRY_REFRESH_COMPLETE\n")

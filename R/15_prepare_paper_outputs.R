# ============================================================
# 15_prepare_paper_outputs.R
# Shared preparation for the Switchers paper output pipeline.
# Revision: 2026-08-13-v1.0.1
#
# This script is sourced by 16_generate_paper_figures.R and
# 17_generate_paper_tables.R. It does not estimate political models by itself.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(scales)
  library(fixest)
  library(haven)
  library(tibble)
})

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

helper_path <- file.path(project_root, "R", "helpers.R")
if (!file.exists(helper_path)) {
  stop("Cannot find R/helpers.R under SWITCHERS_ROOT: ", project_root)
}
source(helper_path)
paths <- build_project_paths(project_root)

PAPER_OUTPUT_REVISION <- "2026-08-13-v1.0.1-paper-output-pipeline"
paper_output_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  PAPER_OUTPUT_REVISION
)

paper_dirs <- list(
  root = paper_output_root,
  main_figures = file.path(paper_output_root, "main", "figures"),
  main_tables = file.path(paper_output_root, "main", "tables"),
  appendix_figures = file.path(paper_output_root, "appendix", "figures"),
  appendix_tables = file.path(paper_output_root, "appendix", "tables"),
  audit = file.path(paper_output_root, "audit"),
  data = file.path(paper_output_root, "data")
)
walk(paper_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Output-status ledger
# ------------------------------------------------------------
.paper_status <- tibble(
  item = character(),
  artifact = character(),
  path = character(),
  status = character(),
  message = character()
)

register_output <- function(item, artifact, path, status = "CREATED", message = "") {
  .paper_status <<- bind_rows(
    .paper_status,
    tibble(
      item = as.character(item),
      artifact = artifact,
      path = path,
      status = status,
      message = message
    )
  )
  invisible(path)
}

write_status_ledger <- function() {
  write_csv(
    .paper_status,
    file.path(paper_dirs$audit, "00_paper_output_status.csv")
  )
}

# ------------------------------------------------------------
# Generic helpers
# ------------------------------------------------------------
first_existing <- function(x) {
  hit <- x[file.exists(x)][1]
  if (length(hit) == 0 || is.na(hit)) NA_character_ else hit
}

save_plot_pair <- function(plot, stem, directory, width = 9, height = 6, item = NA_character_) {
  png_path <- file.path(directory, paste0(stem, ".png"))
  pdf_path <- file.path(directory, paste0(stem, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, dpi = 400, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  register_output(item, "figure_png", png_path)
  register_output(item, "figure_pdf", pdf_path)
  invisible(c(png_path, pdf_path))
}

weighted_share <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(w) & w >= 0
  if (!any(ok) || sum(w[ok]) <= 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

safe_weight_var <- function(data) {
  if ("survey_weight_norm_year" %in% names(data)) return("survey_weight_norm_year")
  if ("survey_weight" %in% names(data)) return("survey_weight")
  stop("No survey weight variable found in respondent data.")
}

safe_median <- function(x, positive = FALSE) {
  x <- x[is.finite(x)]
  if (positive) x <- x[x > 0]
  if (!length(x)) return(NA_real_)
  median(x, na.rm = TRUE)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\textbackslash{}")
  x <- str_replace_all(x, "([&_#%$])", "\\\\\\1")
  x <- str_replace_all(x, "~", "\\\\textasciitilde{}")
  x <- str_replace_all(x, "\\^", "\\\\textasciicircum{}")
  x
}

# ------------------------------------------------------------
# Load final analysis data and frozen manifests
# ------------------------------------------------------------
ac_year <- readRDS(file.path(paths$final_dir, "ac_year.rds"))
ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))
respondents <- readRDS(file.path(paths$final_dir, "nes_respondent_analysis.rds"))
ideology_items_long <- readRDS(file.path(paths$intermediate_dir, "ideology_item_responses_long.rds"))

manifest_candidates <- function(filename) c(
  file.path(paths$derived_dir, "model_exploration", "specification_curves", "manifests", filename),
  file.path(paths$derived_dir, "model_exploration", "respondent_specification_curves", "manifests", filename),
  file.path(project_root, "derived", "model_exploration", "specification_curves", "manifests", filename),
  file.path(project_root, "derived", "model_exploration", "respondent_specification_curves", "manifests", filename)
)

read_manifest_or_fallback <- function(filename, fallback = NULL) {
  p <- first_existing(manifest_candidates(filename))
  if (!is.na(p)) return(read_csv(p, show_col_types = FALSE))
  if (!is.null(fallback) && file.exists(fallback)) return(read_csv(fallback, show_col_types = FALSE))
  NULL
}

fdi_meta <- read_manifest_or_fallback("fdi_specifications.csv", file.path(project_root, "derived", "model_exploration", "specification_curves", "manifests", "fdi_specifications.csv"))
if (is.null(fdi_meta)) {
  # Last fallback for project copies placed in the project root.
  root_copy <- file.path(project_root, "fdi_specifications.csv")
  if (file.exists(root_copy)) fdi_meta <- read_csv(root_copy, show_col_types = FALSE)
}

control_meta <- read_manifest_or_fallback("control_sets.csv", file.path(project_root, "control_sets.csv"))
muslim_meta <- read_manifest_or_fallback("muslim_moderators.csv", file.path(project_root, "muslim_moderators.csv"))
migration_meta <- read_manifest_or_fallback("migration_moderators.csv", file.path(project_root, "migration_moderators.csv"))
center_meta <- read_manifest_or_fallback("center_moderators.csv", file.path(project_root, "center_moderators.csv"))
design_meta <- read_manifest_or_fallback("designs.csv", file.path(project_root, "designs.csv"))

# ------------------------------------------------------------
# Ensure respondent baseline/context variables and weights exist
# ------------------------------------------------------------
if (!"respondent_sample_candidate_present" %in% names(respondents)) {
  respondents <- respondents |>
    mutate(
      respondent_sample_candidate_present =
        vote_valid & !is.na(voted_bjp) &
        !is.na(bjp_candidate_present) & bjp_candidate_present == 1
    )
}

if (!"log1p_employment_per_total_population" %in% names(respondents) &&
    "employment_per_total_population" %in% names(respondents)) {
  respondents <- respondents |>
    mutate(log1p_employment_per_total_population = log1p(employment_per_total_population))
}
if (!"log1p_employment_per_total_population" %in% names(ac_change) &&
    "employment_per_total_population" %in% names(ac_change)) {
  ac_change <- ac_change |>
    mutate(log1p_employment_per_total_population = log1p(employment_per_total_population))
}
if (!"log1p_employment_per_total_population" %in% names(ac_year) &&
    "employment_per_total_population" %in% names(ac_year)) {
  ac_year <- ac_year |>
    mutate(log1p_employment_per_total_population = log1p(employment_per_total_population))
}

baseline_payload_names <- c(
  "bjp_vote_share_2009",
  "log1p_fdi_mfg_local_all_pc100k_2009"
)
missing_baseline <- setdiff(baseline_payload_names, names(respondents))
if (length(missing_baseline)) {
  payload <- ac_change |>
    select(ac_uid, any_of(missing_baseline)) |>
    distinct(ac_uid, .keep_all = TRUE)
  respondents <- respondents |>
    left_join(payload, by = "ac_uid", relationship = "many-to-one")
}

RESP_WEIGHT_VAR <- safe_weight_var(respondents)

# ------------------------------------------------------------
# Reattach ideology-item buckets to respondent rows.
# ------------------------------------------------------------
classification_items <- c("a4b", "a4c", "a4d", "a4g", "q26a", "q10b", "q10e", "q23c")

bucket_col <- case_when(
  "response_bucket" %in% names(ideology_items_long) ~ "response_bucket",
  "item_bucket" %in% names(ideology_items_long) ~ "item_bucket",
  TRUE ~ NA_character_
)
if (is.na(bucket_col)) stop("No response_bucket/item_bucket column in ideology_item_responses_long.rds")

ideology_buckets_wide <- ideology_items_long |>
  filter(item %in% classification_items) |>
  transmute(
    respondent_uid,
    year,
    item,
    item_bucket = as.character(.data[[bucket_col]])
  ) |>
  distinct(respondent_uid, year, item, .keep_all = TRUE) |>
  pivot_wider(
    names_from = item,
    values_from = item_bucket,
    names_glue = "ideology_{item}_bucket"
  )

bucket_cols_needed <- paste0("ideology_", classification_items, "_bucket")
if (!all(bucket_cols_needed %in% names(respondents))) {
  respondents <- respondents |>
    left_join(
      ideology_buckets_wide,
      by = c("respondent_uid", "year"),
      relationship = "one-to-one"
    )
}

# ------------------------------------------------------------
# Full four-bucket harmonization.
# User decision: apply the same 2009 relaxation to Left, Center and Right.
# 2009: 2/2 recognition + >=2/3 statism in the same bucket.
# 2014: 2/2 recognition + 1/1 statism in the same bucket.
# Complete respondents not satisfying a pure bucket are Mixed.
# ------------------------------------------------------------
count_bucket <- function(...) {
  mats <- list(...)
  Reduce(`+`, lapply(mats, function(x) as.integer(x)))
}

for (bucket in c("Left", "Center", "Right")) {
  nm_rec <- paste0("recognition_", tolower(bucket), "_n")
  nm_stat <- paste0("statism_", tolower(bucket), "_n")

  respondents[[nm_rec]] <- case_when(
    respondents$year == 2009 ~
      count_bucket(
        respondents$ideology_a4b_bucket == bucket,
        respondents$ideology_a4c_bucket == bucket
      ),
    respondents$year == 2014 ~
      count_bucket(
        respondents$ideology_q10b_bucket == bucket,
        respondents$ideology_q10e_bucket == bucket
      ),
    TRUE ~ NA_integer_
  )

  respondents[[nm_stat]] <- case_when(
    respondents$year == 2009 ~
      count_bucket(
        respondents$ideology_a4d_bucket == bucket,
        respondents$ideology_a4g_bucket == bucket,
        respondents$ideology_q26a_bucket == bucket
      ),
    respondents$year == 2014 ~
      as.integer(respondents$ideology_q23c_bucket == bucket),
    TRUE ~ NA_integer_
  )
}

respondents <- respondents |>
  mutate(
    ideology_complete_pipeline = coalesce(ideology_complete, FALSE),
    voter_ideology_harmonized = case_when(
      !ideology_complete_pipeline ~ NA_character_,
      year == 2009 & recognition_left_n == 2 & statism_left_n >= 2 ~ "Left",
      year == 2009 & recognition_center_n == 2 & statism_center_n >= 2 ~ "Center",
      year == 2009 & recognition_right_n == 2 & statism_right_n >= 2 ~ "Right",
      year == 2014 & recognition_left_n == 2 & statism_left_n == 1 ~ "Left",
      year == 2014 & recognition_center_n == 2 & statism_center_n == 1 ~ "Center",
      year == 2014 & recognition_right_n == 2 & statism_right_n == 1 ~ "Right",
      TRUE ~ "Mixed"
    ),
    voter_ideology_harmonized = factor(
      voter_ideology_harmonized,
      levels = c("Left", "Center", "Right", "Mixed")
    ),
    voter_ideology_harmonized_center_ref = relevel(
      voter_ideology_harmonized,
      ref = "Center"
    ),
    center_harmonized = case_when(
      is.na(voter_ideology_harmonized) ~ NA_real_,
      voter_ideology_harmonized == "Center" ~ 1,
      TRUE ~ 0
    )
  )

# Guard: 2014 harmonization must reproduce the existing strict four-bucket classification.
harm_2014_guard <- respondents |>
  filter(year == 2014, ideology_complete_pipeline, !is.na(voter_ideology)) |>
  summarise(
    n = n(),
    n_mismatch = sum(as.character(voter_ideology_harmonized) != as.character(voter_ideology), na.rm = TRUE)
  )
write_csv(harm_2014_guard, file.path(paper_dirs$audit, "01_2014_harmonized_vs_strict_guard.csv"))
if (nrow(harm_2014_guard) == 1 && harm_2014_guard$n_mismatch > 0) {
  stop("2014 harmonized ideology does not reproduce strict voter_ideology. Audit before proceeding.")
}

harmonization_audit <- respondents |>
  filter(ideology_complete_pipeline) |>
  count(
    year,
    strict = as.character(voter_ideology),
    harmonized = as.character(voter_ideology_harmonized),
    name = "n_unweighted"
  ) |>
  group_by(year) |>
  mutate(share_unweighted = n_unweighted / sum(n_unweighted)) |>
  ungroup()
write_csv(harmonization_audit, file.path(paper_dirs$audit, "02_ideology_harmonization_transition.csv"))

harmonization_weighted <- respondents |>
  filter(ideology_complete_pipeline, !is.na(.data[[RESP_WEIGHT_VAR]])) |>
  group_by(year, strict = as.character(voter_ideology), harmonized = as.character(voter_ideology_harmonized)) |>
  summarise(weight = sum(.data[[RESP_WEIGHT_VAR]], na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(weighted_share = weight / sum(weight)) |>
  ungroup()
write_csv(harmonization_weighted, file.path(paper_dirs$audit, "03_ideology_harmonization_transition_weighted.csv"))

# ------------------------------------------------------------
# Baseline 2009 constituency Center share for aggregate triples.
# Keep the previously preferred survey-weighted ideology-complete share and N>=5.
# ------------------------------------------------------------
center_share_var <- "nes_weighted_share_center_among_ideology_complete"
center_n_var <- "nes_n_ideology_complete"

if (!all(c(center_share_var, center_n_var) %in% names(ac_year))) {
  stop("ac_year is missing preferred 2009 constituency Center-share variables.")
}

center2009 <- ac_year |>
  filter(year == 2009) |>
  transmute(
    ac_uid,
    center_share_2009 = .data[[center_share_var]],
    center_n_2009 = .data[[center_n_var]]
  ) |>
  distinct(ac_uid, .keep_all = TRUE)

ac_year <- ac_year |>
  select(-any_of(c("center_share_2009", "center_n_2009"))) |>
  left_join(center2009, by = "ac_uid", relationship = "many-to-one")

if (!"center_share_2009" %in% names(ac_change)) {
  ac_change <- ac_change |>
    left_join(center2009, by = "ac_uid", relationship = "one-to-one")
}

# ------------------------------------------------------------
# Fixed common informative-unit rulers.
# User decision: 0 -> median for FDI, Muslim share, migrant share, and AC Center share.
# FDI uses median POSITIVE 2014 exposure to avoid a zero median in a sparse treatment.
# These same constants are reused across AC and voter models.
# ------------------------------------------------------------
preferred_fdi_pooled <- "log1p_fdi_mfg_local_all_pc100k"
preferred_fdi_2014 <- "log1p_fdi_mfg_local_all_pc100k_2014"
preferred_fdi_2009 <- "log1p_fdi_mfg_local_all_pc100k_2009"
preferred_muslim <- "muslim_share_2001_dist_proxy"
preferred_migration <- "mig_total_upto_2001_share_ac_pop"

required_reference_vars <- c(
  preferred_fdi_pooled, preferred_muslim, preferred_migration,
  "center_share_2009"
)
missing_ref <- setdiff(required_reference_vars, names(ac_year))
if (length(missing_ref)) stop("Missing informative-ruler variables in ac_year: ", paste(missing_ref, collapse = ", "))

reference_ac <- ac_year |>
  filter(year == 2014) |>
  distinct(ac_uid, .keep_all = TRUE)

# Audit the full reference distributions BEFORE selecting the informative ruler.
# This prevents a hard stop from hiding which quantity caused the problem.
reference_distributions <- tibble(
  quantity = c(
    "FDI",
    "Muslim share 2001",
    "Established migrant share 2001",
    "Weighted AC Center share 2009"
  ),
  values = list(
    reference_ac[[preferred_fdi_pooled]],
    reference_ac[[preferred_muslim]],
    reference_ac[[preferred_migration]],
    reference_ac$center_share_2009[reference_ac$center_n_2009 >= 5]
  )
) |>
  mutate(
    n_nonmissing = map_int(values, ~sum(is.finite(.x))),
    share_zero = map_dbl(
      values,
      ~{
        z <- .x[is.finite(.x)]
        if (!length(z)) return(NA_real_)
        mean(z == 0)
      }
    ),
    median_all = map_dbl(values, ~safe_median(.x, positive = FALSE)),
    median_positive = map_dbl(values, ~safe_median(.x, positive = TRUE)),
    q25 = map_dbl(
      values,
      ~{
        z <- .x[is.finite(.x)]
        if (!length(z)) return(NA_real_)
        as.numeric(quantile(z, 0.25, na.rm = TRUE, names = FALSE))
      }
    ),
    q75 = map_dbl(
      values,
      ~{
        z <- .x[is.finite(.x)]
        if (!length(z)) return(NA_real_)
        as.numeric(quantile(z, 0.75, na.rm = TRUE, names = FALSE))
      }
    )
  ) |>
  select(-values)

write_csv(
  reference_distributions,
  file.path(
    paper_dirs$audit,
    "04a_informative_unit_reference_distribution_audit.csv"
  )
)

# Informative-unit rule:
#   * FDI is zero-inflated by construction, so use 0 -> median POSITIVE FDI.
#   * Muslim and migrant shares use 0 -> ordinary median when that median is > 0.
#   * The constituency Center share can also be zero-inflated because many
#     eligible ACs have no Center respondents even when N_ideology_complete >= 5.
#     If its ordinary median is zero, use 0 -> median POSITIVE Center share,
#     exactly parallel to the FDI treatment rather than substituting an arbitrary
#     quantile such as q75.
#   * The same fallback is available for a demographic share only if its ordinary
#     median is zero; the audit records when this occurs.
choose_zero_to_typical <- function(x, always_positive_median = FALSE) {
  med_all <- safe_median(x, positive = FALSE)
  med_pos <- safe_median(x, positive = TRUE)

  if (always_positive_median) {
    return(list(
      high = med_pos,
      rule = "0 to median positive"
    ))
  }

  if (is.finite(med_all) && med_all > 0) {
    return(list(
      high = med_all,
      rule = "0 to median"
    ))
  }

  if (is.finite(med_pos) && med_pos > 0) {
    return(list(
      high = med_pos,
      rule = "0 to median positive (ordinary median = 0)"
    ))
  }

  list(
    high = NA_real_,
    rule = "FAILED: no positive finite reference value"
  )
}

ref_fdi_choice <- choose_zero_to_typical(
  reference_ac[[preferred_fdi_pooled]],
  always_positive_median = TRUE
)
ref_muslim_choice <- choose_zero_to_typical(
  reference_ac[[preferred_muslim]],
  always_positive_median = FALSE
)
ref_migration_choice <- choose_zero_to_typical(
  reference_ac[[preferred_migration]],
  always_positive_median = FALSE
)
ref_center_choice <- choose_zero_to_typical(
  reference_ac$center_share_2009[reference_ac$center_n_2009 >= 5],
  always_positive_median = FALSE
)

informative_reference <- tibble(
  quantity = c(
    "FDI",
    "Muslim share 2001",
    "Established migrant share 2001",
    "Weighted AC Center share 2009"
  ),
  low = 0,
  high = c(
    ref_fdi_choice$high,
    ref_muslim_choice$high,
    ref_migration_choice$high,
    ref_center_choice$high
  ),
  rule = c(
    paste0(ref_fdi_choice$rule, " among 2014 ACs"),
    paste0(ref_muslim_choice$rule, " among 2014 ACs"),
    paste0(ref_migration_choice$rule, " among 2014 ACs"),
    paste0(ref_center_choice$rule, " among 2009 N>=5 Center-share ACs")
  )
)

write_csv(
  informative_reference,
  file.path(
    paper_dirs$audit,
    "04_informative_unit_reference_values.csv"
  )
)

bad_reference <- informative_reference |>
  filter(!is.finite(high) | high <= 0)

if (nrow(bad_reference) > 0) {
  stop(
    "Could not construct a positive informative-unit ruler for: ",
    paste(bad_reference$quantity, collapse = ", "),
    ". See audit/04a_informative_unit_reference_distribution_audit.csv ",
    "and audit/04_informative_unit_reference_values.csv."
  )
}

message("Informative-unit rulers:")
purrr::pwalk(
  informative_reference,
  function(quantity, low, high, rule) {
    message(
      "  ",
      quantity,
      ": ",
      sprintf("%.6f", low),
      " -> ",
      sprintf("%.6f", high),
      " [",
      rule,
      "]"
    )
  }
)

REF_FDI <- informative_reference$high[informative_reference$quantity == "FDI"]
REF_MUSLIM <- informative_reference$high[informative_reference$quantity == "Muslim share 2001"]
REF_MIGRATION <- informative_reference$high[informative_reference$quantity == "Established migrant share 2001"]
REF_CENTER <- informative_reference$high[informative_reference$quantity == "Weighted AC Center share 2009"]

# ------------------------------------------------------------
# Detect which C2 controls actually vary within AC across the two panel rows.
# Static controls are absorbed by AC fixed effects and are not estimable there.
# ------------------------------------------------------------
C1_VARS <- c("proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share")
C2_EXTRA_CANDIDATES <- c("log1p_employment_per_total_population", "employment_per_population_15_64", "ed_sec_share")
C2_EXTRA_CANDIDATES <- C2_EXTRA_CANDIDATES[C2_EXTRA_CANDIDATES %in% names(ac_year)]

within_variation <- function(data, var) {
  tmp <- data |>
    filter(year %in% c(2009, 2014)) |>
    group_by(ac_uid) |>
    summarise(
      n_nonmissing = sum(!is.na(.data[[var]])),
      n_unique = n_distinct(.data[[var]][!is.na(.data[[var]])]),
      .groups = "drop"
    )
  sum(tmp$n_unique > 1, na.rm = TRUE)
}

control_variation_audit <- tibble(
  variable = unique(c(C1_VARS, C2_EXTRA_CANDIDATES))
) |>
  mutate(
    n_ac_with_within_change = map_int(variable, ~within_variation(ac_year, .x)),
    estimable_with_ac_fe = n_ac_with_within_change > 0
  )
write_csv(control_variation_audit, file.path(paper_dirs$audit, "05_ac_fe_control_variation_audit.csv"))

AC_FE_C2_VARS <- control_variation_audit |>
  filter(variable %in% C2_EXTRA_CANDIDATES, estimable_with_ac_fe) |>
  pull(variable)

# ------------------------------------------------------------
# Common theme and labels
# ------------------------------------------------------------
ideology_levels <- c("Left", "Center", "Right", "Mixed")
ideology_labels <- setNames(ideology_levels, ideology_levels)

theme_paper <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      plot.title.position = "plot",
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      strip.text = element_text(face = "bold")
    )
}

message("Prepared shared paper-output objects under: ", paper_output_root)

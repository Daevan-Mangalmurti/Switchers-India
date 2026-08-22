# ============================================================
# 22_ac_ideology_bjp_fdi_interactions_v1_4.R
#
# Assembly-constituency models of BJP voting by ideological group.
# Revision: 2026-08-21-v1.4
#
# Core question:
#   Among Left, Center, and Right NES respondents, is BJP support more strongly
#   related to FDI exposure in ACs with larger pre-existing Muslim shares?
#
# MAIN MODEL (repeated for Left, Center, and Right):
#   Ideology-group BJP share ~ Muslim share (2001) * FDI exposure
#                      + population + SC share + ST share + land area
#                      + secondary education share + fixed effects
#
# This script:
#   1. rebuilds the 2009 and 2014 centrist classifications from RAW NES files
#      using the current audited cross-year coding rules;
#   2. constructs weighted and unweighted AC-year BJP vote shares among Left,
#      Center, and Right respondents, retaining underlying counts/weighted totals;
#   3. summarizes mean calendar-year AC FDI exposure per 100,000 for 2004-2014;
#   4. constructs election-to-election FDI exposure using April election-month
#      cutoffs and the existing spatial FDI exposure file;
#   5. constructs two FDI-change definitions, raw and signed-log transformed;
#   6. prepares pooled, 2014 lagged-outcome, and matched-NES datasets;
#   7. estimates the 27-model design for each ideology where estimable;
#      sparse matched-NES specifications are audited and skipped rather than
#      allowed to crash the pipeline.
#
# DEFAULTS USED IN EACH 27-MODEL IDEOLOGY SET:
#   - DV: survey-weighted BJP share within the relevant ideology group
#   - FDI scope: local (own AC + touching ACs)
#   - FDI status: all announced/opened projects
#   - level FDI: log1p(projects per 100,000)
#   - change FDI: sign(x) * log1p(abs(x))
#   - default controls: population + SC share + ST share
#   - robustness-only controls retained in prepared data: land area +
#     secondary-education share
#   - Muslim moderator: 2001 Muslim population share
#   - inference: parliamentary-constituency clustered SEs
#   - FE: state + year for pooled models; state only otherwise
#
# IMPORTANT CHANGE-1 NOTE:
#   The requested default is literal: average annual FDI over
#   Apr-2009--Mar-2014 minus observed FDI in calendar 2004. Because the source
#   extract starts in April 2004, that baseline is Apr-Dec 2004. The script
#   ALSO preserves an annualized Apr-Dec-2004 alternative (x 12/9) so the
#   unequal-duration issue can be tested without rebuilding the data.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(haven)
  library(stringr)
  library(lubridate)
  library(tibble)
  library(fixest)
  library(modelsummary)
})

# ============================================================
# 0. PROJECT + SETTINGS
# ============================================================

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

SCRIPT_REVISION <- "2026-08-21-v1.3-ideology-bjp-ac-fdi-composition-audit"
message("Running: ", SCRIPT_REVISION)

# Easy switches for later hand-edits.
FDI_SCOPE <- "local"       # change to "own" if desired
FDI_ALLOWED_STATUSES <- c("announced", "opened")

# Election-month windows. Project dates are month-level, so boundaries use
# the first day of the election month and are left-closed/right-open.
WINDOW_0409_START <- as.Date("2004-04-01")
WINDOW_0409_END   <- as.Date("2009-04-01")
WINDOW_0914_START <- as.Date("2009-04-01")
WINDOW_0914_END   <- as.Date("2014-04-01")

# The raw FDI extract starts in April 2004. This period is retained as the
# observed 2004 baseline for Change Definition 1.
BASELINE_2004_START <- as.Date("2004-04-01")
BASELINE_2004_END   <- as.Date("2005-01-01")
BASELINE_2004_MONTHS <- 9

# Requested Change-1 default: use observed calendar-2004 exposure literally.
# Set TRUE to use the preserved annualized Apr-Dec 2004 sensitivity instead.
ANNUALIZE_2004_BASELINE <- FALSE

out_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "ac_ideology_bjp_fdi_interactions_v1_4"
)
out_data_dir <- file.path(out_root, "data")
out_table_dir <- file.path(out_root, "tables")
out_audit_dir <- file.path(out_root, "audits")
dir.create(out_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_audit_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. SMALL HELPERS
# ============================================================

assert_has_columns_local <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(data)
}

positive_weight <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x) & x > 0, x, NA_real_)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

signed_log1p <- function(x) {
  sign(x) * log1p(abs(x))
}

# Sparse-sample-safe model fitting. This is especially important for the
# exact same-AC 2009/2014 matched-NES analyses, where strict ideology cells can
# leave too few complete observations for a fully controlled state-FE model.
# Failed/non-identifiable models return NULL and are recorded rather than
# stopping the entire 81-model pipeline.
model_fit_log <- list()

record_model_fit <- function(ideology, model, status, n_input, n_complete,
                             n_states_complete, n_clusters_complete,
                             focal_retained = NA, error = NA_character_) {
  model_fit_log[[length(model_fit_log) + 1L]] <<- tibble::tibble(
    ideology = ideology,
    model = model,
    status = status,
    n_input = n_input,
    n_complete = n_complete,
    n_states_complete = n_states_complete,
    n_clusters_complete = n_clusters_complete,
    focal_retained = focal_retained,
    error = error
  )
}

safe_feols <- function(fml, data, ideology, model_label,
                       cluster_fml = ~pc_cluster_id, require_focal = TRUE) {
  vars_needed <- unique(c(all.vars(fml), all.vars(cluster_fml)))
  missing_vars <- setdiff(vars_needed, names(data))

  if (length(missing_vars) > 0L) {
    record_model_fit(
      ideology, model_label, "missing_variables", nrow(data), 0L, 0L, 0L,
      FALSE, paste("Missing:", paste(missing_vars, collapse = ", "))
    )
    return(NULL)
  }

  cc <- stats::complete.cases(data[, vars_needed, drop = FALSE])
  fit_data <- data[cc, , drop = FALSE]
  n_complete <- nrow(fit_data)
  n_states <- dplyr::n_distinct(fit_data$state_no)
  n_clusters <- dplyr::n_distinct(fit_data$pc_cluster_id)

  if (n_complete == 0L) {
    record_model_fit(
      ideology, model_label, "no_complete_cases", nrow(data), 0L, 0L, 0L,
      FALSE, "No complete observations for model variables."
    )
    return(NULL)
  }

  fit <- tryCatch(
    fixest::feols(fml, data = fit_data, cluster = cluster_fml),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    record_model_fit(
      ideology, model_label, "feols_error", nrow(data), n_complete,
      n_states, n_clusters, FALSE, conditionMessage(fit)
    )
    return(NULL)
  }

  coef_names <- names(stats::coef(fit))
  focal_names <- coef_names[
    stringr::str_detect(coef_names, "muslim_share_2001_dist_proxy") &
      stringr::str_detect(coef_names, ":") &
      stringr::str_detect(coef_names, "fdi_")
  ]
  focal_ok <- length(focal_names) == 1L

  # Approximate residual support after slopes and absorbed state effects.
  # This is intentionally conservative for the sparse matched-NES models.
  approx_resid_df <- stats::nobs(fit) - length(coef_names) - max(n_states - 1L, 0L)

  if (require_focal && !focal_ok) {
    record_model_fit(
      ideology, model_label, "focal_interaction_not_identified", nrow(data),
      n_complete, n_states, n_clusters, FALSE,
      paste0("Focal Muslim-share x FDI interaction was dropped; retained coefficients: ",
             paste(coef_names, collapse = ", "))
    )
    return(NULL)
  }

  if (approx_resid_df <= 0L) {
    record_model_fit(
      ideology, model_label, "insufficient_residual_df", nrow(data),
      n_complete, n_states, n_clusters, focal_ok,
      paste0("Approximate residual df = ", approx_resid_df, ".")
    )
    return(NULL)
  }

  if (n_clusters < 2L) {
    record_model_fit(
      ideology, model_label, "insufficient_clusters", nrow(data),
      n_complete, n_states, n_clusters, focal_ok,
      "Fewer than two PC clusters remain after complete-case filtering."
    )
    return(NULL)
  }

  record_model_fit(
    ideology, model_label, "estimated", nrow(data), n_complete, n_states,
    n_clusters, focal_ok, NA_character_
  )
  fit
}

per_100k_local <- function(n, pop) {
  ifelse(is.finite(pop) & pop > 0, 100000 * n / pop, NA_real_)
}

# Use the project's AC-ID helper when available. The fallback reproduces the
# legacy state_no_ac convention used by the project.
make_ac_uid_local <- function(state_no, ac) {
  if (exists("make_ac_uid", mode = "function", inherits = TRUE)) {
    return(make_ac_uid(state_no, ac))
  }
  paste(as.integer(state_no), as.integer(ac), sep = "_")
}

nes_state_recode_local <- function(x) {
  dplyr::recode(
    as.numeric(x),
    `1` = 28, `2` = 12, `3` = 18, `4` = 10, `5` = 30, `6` = 24,
    `7` = 6, `8` = 2, `9` = 1, `10` = 29, `11` = 32, `12` = 23,
    `13` = 27, `14` = 14, `15` = 17, `16` = 15, `17` = 13,
    `18` = 21, `19` = 3, `20` = 8, `21` = 11, `22` = 33,
    `23` = 16, `24` = 9, `25` = 19, `26` = 35, `27` = 4,
    `28` = 26, `29` = 25, `30` = 7, `31` = 31, `32` = 34,
    `33` = 20, `34` = 22, `35` = 5, `36` = 28,
    .default = NA_real_
  )
}

bucket_from_oriented <- function(x) {
  dplyr::case_when(
    x == -2 ~ "Left",
    x %in% c(-1, 1) ~ "Center",
    x == 2 ~ "Right",
    TRUE ~ NA_character_
  )
}

# ============================================================
# 2. LOAD FINAL AC-YEAR DATA + RAW NES + FDI EXPOSURE
# ============================================================

ac_year_path <- file.path(paths$final_dir, "ac_year.rds")
if (!file.exists(ac_year_path)) stop("Missing final AC-year file: ", ac_year_path)
ac_year <- readRDS(ac_year_path)

# Land area and secondary education remain required here because they are retained
# in the prepared analysis files for robustness checks, even though they are no
# longer part of the default regression control set.
required_ac <- c(
  "ac_uid", "state_no", "ac", "pc_cluster_id", "year",
  "bjp_vote_share", "proxy_ac_pop", "con08_land_area",
  "sc_pop_share", "st_pop_share", "ed_sec_share",
  "muslim_share_2001_dist_proxy"
)
assert_has_columns_local(ac_year, required_ac, "ac_year.rds")

# Raw NES files. Prefer paths returned by helpers; fall back to canonical tree.
nes_2009_path <- if (!is.null(paths$nes_2009)) {
  paths$nes_2009
} else {
  file.path(project_root, "data", "lokniti", "nes_2009.sav")
}
nes_2014_path <- if (!is.null(paths$nes_2014)) {
  paths$nes_2014
} else {
  file.path(project_root, "data", "lokniti", "nes_2014.sav")
}

if (!file.exists(nes_2009_path)) stop("Missing NES 2009 file: ", nes_2009_path)
if (!file.exists(nes_2014_path)) stop("Missing NES 2014 file: ", nes_2014_path)

raw09 <- haven::read_sav(nes_2009_path)
raw14 <- haven::read_sav(nes_2014_path)

required09 <- c(
  "st_id", "pc_id", "ac_id", "q1a", "stpop1",
  "a4b", "a4c", "a4d", "a4g", "q26a"
)
required14 <- c(
  "state_id", "pc_id", "ac_id", "q1a", "stpop",
  "q10b", "q10e", "q23c"
)
assert_has_columns_local(raw09, required09, "raw NES 2009")
assert_has_columns_local(raw14, required14, "raw NES 2014")

fdi_exposure_path <- file.path(paths$intermediate_dir, "fdi_project_exposure.csv")
if (!file.exists(fdi_exposure_path)) {
  stop(
    "Missing spatial FDI exposure file: ", fdi_exposure_path, "\n",
    "Run the project data-build pipeline first so FDI projects are spatially mapped."
  )
}

fdi_exposure <- readr::read_csv(
  fdi_exposure_path,
  show_col_types = FALSE,
  progress = FALSE
)

required_fdi_exposure <- c(
  "fdi_project_uid", "exposed_ac_uid", "exposure_scope",
  "project_month", "standardized_sector", "standardized_status"
)
assert_has_columns_local(
  fdi_exposure,
  required_fdi_exposure,
  "fdi_project_exposure.csv"
)

# ============================================================
# 3. FRESH NES IDEOLOGY + BJP RECONSTRUCTION
# ============================================================

# ------------------------------------------------------------
# 3A. 2009
# Main rule: 2/2 recognition + >=2/3 statism in same bucket.
# ------------------------------------------------------------

d09 <- raw09 |>
  dplyr::mutate(
    year = 2009L,
    respondent_uid = paste0("2009_", dplyr::row_number()),
    state_no = as.integer(nes_state_recode_local(st_id)),
    pc = as.integer(pc_id),
    ac = as.integer(ac_id),
    ac_uid = make_ac_uid_local(state_no, ac),

    a4b_raw = as.numeric(a4b),
    a4c_raw = as.numeric(a4c),
    a4d_raw = as.numeric(a4d),
    a4g_raw = as.numeric(a4g),
    q26a_raw = as.numeric(q26a),

    a4b_oriented = dplyr::case_when(
      a4b_raw == 1 ~ -2, a4b_raw == 2 ~ -1,
      a4b_raw == 3 ~  1, a4b_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),
    a4c_oriented = dplyr::case_when(
      a4c_raw == 1 ~  2, a4c_raw == 2 ~  1,
      a4c_raw == 3 ~ -1, a4c_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),
    a4d_oriented = dplyr::case_when(
      a4d_raw == 1 ~  2, a4d_raw == 2 ~  1,
      a4d_raw == 3 ~ -1, a4d_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),
    a4g_oriented = dplyr::case_when(
      a4g_raw == 1 ~ -2, a4g_raw == 2 ~ -1,
      a4g_raw == 3 ~  1, a4g_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),
    q26a_oriented = dplyr::case_when(
      q26a_raw == 1 ~  2, q26a_raw == 2 ~  1,
      q26a_raw == 3 ~ -1, q26a_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    a4b_bucket = bucket_from_oriented(a4b_oriented),
    a4c_bucket = bucket_from_oriented(a4c_oriented),
    a4d_bucket = bucket_from_oriented(a4d_oriented),
    a4g_bucket = bucket_from_oriented(a4g_oriented),
    q26a_bucket = bucket_from_oriented(q26a_oriented),

    ideology_complete =
      !is.na(a4b_bucket) & !is.na(a4c_bucket) &
      !is.na(a4d_bucket) & !is.na(a4g_bucket) & !is.na(q26a_bucket),

    recognition_left_n =
      as.integer(a4b_bucket == "Left") + as.integer(a4c_bucket == "Left"),
    recognition_center_n =
      as.integer(a4b_bucket == "Center") + as.integer(a4c_bucket == "Center"),
    recognition_right_n =
      as.integer(a4b_bucket == "Right") + as.integer(a4c_bucket == "Right"),

    statism_left_n =
      as.integer(a4d_bucket == "Left") + as.integer(a4g_bucket == "Left") +
      as.integer(q26a_bucket == "Left"),
    statism_center_n =
      as.integer(a4d_bucket == "Center") + as.integer(a4g_bucket == "Center") +
      as.integer(q26a_bucket == "Center"),
    statism_right_n =
      as.integer(a4d_bucket == "Right") + as.integer(a4g_bucket == "Right") +
      as.integer(q26a_bucket == "Right"),

    ideology_main = dplyr::case_when(
      !ideology_complete ~ NA_character_,
      recognition_left_n == 2 & statism_left_n >= 2 ~ "Left",
      recognition_center_n == 2 & statism_center_n >= 2 ~ "Center",
      recognition_right_n == 2 & statism_right_n >= 2 ~ "Right",
      TRUE ~ "Mixed"
    ),

    vote_code = as.numeric(q1a),
    vote_valid = !is.na(vote_code) & !vote_code %in% c(98, 99),
    voted_bjp = dplyr::case_when(
      !vote_valid ~ NA_real_,
      vote_code == 2 ~ 1,
      TRUE ~ 0
    ),
    survey_weight = positive_weight(stpop1)
  ) |>
  dplyr::select(
    year, respondent_uid, state_no, pc, ac, ac_uid,
    ideology_complete, ideology_main, vote_valid, voted_bjp, survey_weight
  )

# ------------------------------------------------------------
# 3B. 2014
# Main rule: Q10b + Q10e + corrected Q23c all in same bucket.
# ------------------------------------------------------------

d14 <- raw14 |>
  dplyr::mutate(
    year = 2014L,
    respondent_uid = paste0("2014_", dplyr::row_number()),
    state_no = as.integer(nes_state_recode_local(state_id)),
    pc = as.integer(pc_id),
    ac = as.integer(ac_id),
    ac_uid = make_ac_uid_local(state_no, ac),

    q10b_raw = as.numeric(q10b),
    q10e_raw = as.numeric(q10e),
    q23c_raw = as.numeric(q23c),

    q10b_oriented = dplyr::case_when(
      q10b_raw == 1 ~  2, q10b_raw == 2 ~  1,
      q10b_raw == 3 ~ -1, q10b_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),
    q10e_oriented = dplyr::case_when(
      q10e_raw == 1 ~ -2, q10e_raw == 2 ~ -1,
      q10e_raw == 3 ~  1, q10e_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),
    # Corrected orientation from the current cross-year audit.
    q23c_oriented = dplyr::case_when(
      q23c_raw == 1 ~  2, q23c_raw == 2 ~  1,
      q23c_raw == 3 ~ -1, q23c_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    q10b_bucket = bucket_from_oriented(q10b_oriented),
    q10e_bucket = bucket_from_oriented(q10e_oriented),
    q23c_bucket = bucket_from_oriented(q23c_oriented),

    ideology_complete =
      !is.na(q10b_bucket) & !is.na(q10e_bucket) & !is.na(q23c_bucket),

    ideology_main = dplyr::case_when(
      !ideology_complete ~ NA_character_,
      q10b_bucket == "Left" & q10e_bucket == "Left" & q23c_bucket == "Left" ~ "Left",
      q10b_bucket == "Center" & q10e_bucket == "Center" & q23c_bucket == "Center" ~ "Center",
      q10b_bucket == "Right" & q10e_bucket == "Right" & q23c_bucket == "Right" ~ "Right",
      TRUE ~ "Mixed"
    ),

    vote_code = as.numeric(q1a),
    vote_valid = !is.na(vote_code) & !vote_code %in% c(96, 98, 99),
    voted_bjp = dplyr::case_when(
      !vote_valid ~ NA_real_,
      vote_code == 2 ~ 1,
      TRUE ~ 0
    ),
    survey_weight = positive_weight(stpop)
  ) |>
  dplyr::select(
    year, respondent_uid, state_no, pc, ac, ac_uid,
    ideology_complete, ideology_main, vote_valid, voted_bjp, survey_weight
  )

nes_fresh <- dplyr::bind_rows(d09, d14)

# ============================================================
# IDEOLOGY COMPLETION + CLASSIFICATION DISTRIBUTION BY YEAR
# ============================================================

# ------------------------------------------------------------
# A. What share of all survey respondents could be classified?
# ------------------------------------------------------------

ideology_completion_by_year <- nes_fresh |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n_total_respondents = dplyr::n(),
    
    n_ideology_complete =
      sum(ideology_complete, na.rm = TRUE),
    
    pct_ideology_complete =
      100 * n_ideology_complete / n_total_respondents,
    
    n_ideology_incomplete =
      sum(!ideology_complete, na.rm = TRUE),
    
    pct_ideology_incomplete =
      100 * n_ideology_incomplete / n_total_respondents,
    
    .groups = "drop"
  )

print(ideology_completion_by_year, n = Inf)


# ------------------------------------------------------------
# B. Among respondents who answered the ideology questions,
#    what share were Left / Center / Right / Mixed?
# ------------------------------------------------------------

ideology_distribution_by_year <- nes_fresh |>
  dplyr::filter(ideology_complete) |>
  dplyr::count(
    year,
    ideology_main,
    name = "n_respondents"
  ) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    n_ideology_complete = sum(n_respondents),
    
    pct_among_ideology_complete =
      100 * n_respondents / n_ideology_complete
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    year,
    factor(
      ideology_main,
      levels = c("Left", "Center", "Right", "Mixed")
    )
  )

print(ideology_distribution_by_year, n = Inf)

ideology_summary_wide <- ideology_distribution_by_year |>
  dplyr::select(
    year,
    ideology_main,
    n_respondents,
    pct_among_ideology_complete
  ) |>
  tidyr::pivot_wider(
    names_from = ideology_main,
    values_from = c(
      n_respondents,
      pct_among_ideology_complete
    ),
    names_glue = "{.value}_{ideology_main}"
  ) |>
  dplyr::left_join(
    ideology_completion_by_year,
    by = "year"
  )

print(ideology_summary_wide, n = Inf, width = Inf)

ideology_distribution_weighted <- nes_fresh |>
  dplyr::filter(
    ideology_complete,
    !is.na(survey_weight),
    survey_weight > 0
  ) |>
  dplyr::group_by(year, ideology_main) |>
  dplyr::summarise(
    weighted_n = sum(survey_weight),
    .groups = "drop"
  ) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    weighted_pct_among_ideology_complete =
      100 * weighted_n / sum(weighted_n)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    year,
    factor(
      ideology_main,
      levels = c("Left", "Center", "Right", "Mixed")
    )
  )

print(ideology_distribution_weighted, n = Inf)

# ============================================================
# 4. AC-YEAR BJP OUTCOMES BY IDEOLOGICAL GROUP + COUNTS
# ============================================================

# Build parallel AC-year outcomes for Left, Center, and Right respondents.
# "centrist" is retained in variable names for the Center category so existing
# centrist formulas remain easy to recognize and edit.
make_ideology_ac_year <- function(data, ideology_value, prefix) {
  out <- data |>
    dplyr::filter(ideology_main == ideology_value, !is.na(ac_uid)) |>
    dplyr::group_by(ac_uid, state_no, ac, year) |>
    dplyr::summarise(
      n_group = dplyr::n(),
      n_group_valid_vote = sum(vote_valid, na.rm = TRUE),
      n_group_bjp = sum(voted_bjp == 1, na.rm = TRUE),

      wt_group = sum(survey_weight, na.rm = TRUE),
      wt_group_valid_vote = sum(
        survey_weight[vote_valid & !is.na(survey_weight)],
        na.rm = TRUE
      ),
      wt_group_bjp = sum(
        survey_weight[vote_valid & voted_bjp == 1 & !is.na(survey_weight)],
        na.rm = TRUE
      ),

      bjp_share_unweighted = dplyr::if_else(
        n_group_valid_vote > 0,
        n_group_bjp / n_group_valid_vote,
        NA_real_
      ),
      bjp_share_weighted = weighted_mean_safe(
        voted_bjp[vote_valid],
        survey_weight[vote_valid]
      ),
      .groups = "drop"
    )

  rename_map <- c(
    n_group = paste0("n_", prefix),
    n_group_valid_vote = paste0("n_", prefix, "_valid_vote"),
    n_group_bjp = paste0("n_", prefix, "_bjp"),
    wt_group = paste0("wt_", prefix),
    wt_group_valid_vote = paste0("wt_", prefix, "_valid_vote"),
    wt_group_bjp = paste0("wt_", prefix, "_bjp"),
    bjp_share_unweighted = paste0(prefix, "_bjp_share_unweighted"),
    bjp_share_weighted = paste0(prefix, "_bjp_share_weighted")
  )
  idx <- match(names(rename_map), names(out))
  names(out)[idx] <- unname(rename_map)
  out
}

left_ac_year <- make_ideology_ac_year(nes_fresh, "Left", "left")
centrist_ac_year <- make_ideology_ac_year(nes_fresh, "Center", "centrist")
right_ac_year <- make_ideology_ac_year(nes_fresh, "Right", "right")

ideology_ac_year <- left_ac_year |>
  dplyr::full_join(
    centrist_ac_year,
    by = c("ac_uid", "state_no", "ac", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::full_join(
    right_ac_year,
    by = c("ac_uid", "state_no", "ac", "year"),
    relationship = "one-to-one"
  )

# Explicit 2009 and 2014 wide outcomes/counts for all three ideological groups.
ideology_wide <- ideology_ac_year |>
  dplyr::select(
    ac_uid, year,
    dplyr::starts_with("n_left"), dplyr::starts_with("wt_left"),
    dplyr::starts_with("left_bjp_share"),
    dplyr::starts_with("n_centrist"), dplyr::starts_with("wt_centrist"),
    dplyr::starts_with("centrist_bjp_share"),
    dplyr::starts_with("n_right"), dplyr::starts_with("wt_right"),
    dplyr::starts_with("right_bjp_share")
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = -c(ac_uid, year),
    names_glue = "{.value}_{year}"
  )

# Backward-compatible alias used by the original centrist model block.
centrist_wide <- ideology_wide

readr::write_csv(
  ideology_completion_by_year,
  file.path(
    out_audit_dir,
    "00b_ideology_completion_by_year.csv"
  )
)

readr::write_csv(
  ideology_distribution_by_year,
  file.path(
    out_audit_dir,
    "00c_ideology_distribution_unweighted_by_year.csv"
  )
)

readr::write_csv(
  ideology_distribution_weighted,
  file.path(
    out_audit_dir,
    "00d_ideology_distribution_weighted_by_year.csv"
  )
)

readr::write_csv(
  ideology_summary_wide,
  file.path(
    out_audit_dir,
    "00e_ideology_completion_and_distribution_wide.csv"
  )
)

# ============================================================
# 5. CUSTOM FDI WINDOWS FROM THE EXISTING SPATIAL EXPOSURE MAP
# ============================================================

fdi_base <- fdi_exposure |>
  dplyr::mutate(
    project_month = as.Date(project_month),
    exposed_ac_uid = as.character(exposed_ac_uid),
    exposure_scope = as.character(exposure_scope),
    standardized_sector = as.character(standardized_sector),
    standardized_status = as.character(standardized_status)
  ) |>
  dplyr::filter(
    exposure_scope == FDI_SCOPE,
    standardized_status %in% FDI_ALLOWED_STATUSES,
    !is.na(project_month),
    !is.na(exposed_ac_uid)
  ) |>
  dplyr::distinct(
    fdi_project_uid,
    exposed_ac_uid,
    exposure_scope,
    project_month,
    standardized_sector,
    standardized_status
  )

ac_population <- ac_year |>
  dplyr::select(ac_uid, proxy_ac_pop) |>
  dplyr::distinct(ac_uid, .keep_all = TRUE)

all_ac_ids <- ac_population |>
  dplyr::select(ac_uid)

# ------------------------------------------------------------
# 5A. CALENDAR-YEAR FDI PER 100K, 2004-2014
# ------------------------------------------------------------
# Means are across all ACs in the population frame, including AC-years with
# zero mapped FDI. 2004 is a partial observed year because the FDI source begins
# in April 2004; this is flagged explicitly in the output.
annual_fdi_observed <- fdi_base |>
  dplyr::mutate(calendar_year = lubridate::year(project_month)) |>
  dplyr::filter(calendar_year >= 2004L, calendar_year <= 2014L) |>
  dplyr::group_by(exposed_ac_uid, calendar_year) |>
  dplyr::summarise(
    fdi_total_n = dplyr::n_distinct(fdi_project_uid),
    fdi_mfg_n = dplyr::n_distinct(
      fdi_project_uid[standardized_sector == "manufacturing"]
    ),
    fdi_services_n = dplyr::n_distinct(
      fdi_project_uid[standardized_sector == "services"]
    ),
    .groups = "drop"
  ) |>
  dplyr::rename(ac_uid = exposed_ac_uid)

annual_fdi_ac_year <- tidyr::crossing(
  ac_uid = all_ac_ids$ac_uid,
  calendar_year = 2004:2014
) |>
  dplyr::left_join(
    annual_fdi_observed,
    by = c("ac_uid", "calendar_year"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    fdi_total_n = tidyr::replace_na(fdi_total_n, 0L),
    fdi_mfg_n = tidyr::replace_na(fdi_mfg_n, 0L),
    fdi_services_n = tidyr::replace_na(fdi_services_n, 0L)
  ) |>
  dplyr::left_join(
    ac_population,
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    fdi_total_pc100k = per_100k_local(fdi_total_n, proxy_ac_pop),
    fdi_mfg_pc100k = per_100k_local(fdi_mfg_n, proxy_ac_pop),
    fdi_services_pc100k = per_100k_local(fdi_services_n, proxy_ac_pop)
  )

annual_fdi_summary <- annual_fdi_ac_year |>
  dplyr::group_by(calendar_year) |>
  dplyr::summarise(
    mean_total_fdi_pc100k = mean(fdi_total_pc100k, na.rm = TRUE),
    mean_mfg_fdi_pc100k = mean(fdi_mfg_pc100k, na.rm = TRUE),
    mean_services_fdi_pc100k = mean(fdi_services_pc100k, na.rm = TRUE),
    median_total_fdi_pc100k = stats::median(fdi_total_pc100k, na.rm = TRUE),
    median_mfg_fdi_pc100k = stats::median(fdi_mfg_pc100k, na.rm = TRUE),
    median_services_fdi_pc100k = stats::median(fdi_services_pc100k, na.rm = TRUE),
    share_acs_zero_total_fdi = mean(fdi_total_n == 0, na.rm = TRUE),
    n_acs_with_population = sum(!is.na(proxy_ac_pop) & proxy_ac_pop > 0),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    source_period_note = dplyr::case_when(
      calendar_year == lubridate::year(min(fdi_base$project_month, na.rm = TRUE)) ~
        paste0(
          "Partial calendar year: source begins ",
          format(min(fdi_base$project_month, na.rm = TRUE), "%b %Y")
        ),
      calendar_year == lubridate::year(max(fdi_base$project_month, na.rm = TRUE)) &
        lubridate::month(max(fdi_base$project_month, na.rm = TRUE)) < 12L ~
        paste0(
          "Partial calendar year: source ends ",
          format(max(fdi_base$project_month, na.rm = TRUE), "%b %Y")
        ),
      TRUE ~ "Full calendar year in source range"
    )
  )

readr::write_csv(
  annual_fdi_summary,
  file.path(out_audit_dir, "00_calendar_year_fdi_pc100k_summary_2004_2014.csv")
)
readr::write_csv(
  annual_fdi_ac_year,
  file.path(out_data_dir, "calendar_year_ac_fdi_pc100k_2004_2014.csv")
)

count_fdi_window <- function(start_date, end_date, suffix) {
  observed <- fdi_base |>
    dplyr::filter(project_month >= start_date, project_month < end_date) |>
    dplyr::group_by(exposed_ac_uid) |>
    dplyr::summarise(
      total_n = dplyr::n_distinct(fdi_project_uid),
      mfg_n = dplyr::n_distinct(
        fdi_project_uid[standardized_sector == "manufacturing"]
      ),
      services_n = dplyr::n_distinct(
        fdi_project_uid[standardized_sector == "services"]
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(ac_uid = exposed_ac_uid)

  out <- all_ac_ids |>
    dplyr::left_join(observed, by = "ac_uid", relationship = "one-to-one") |>
    dplyr::mutate(
      total_n = tidyr::replace_na(total_n, 0L),
      mfg_n = tidyr::replace_na(mfg_n, 0L),
      services_n = tidyr::replace_na(services_n, 0L)
    ) |>
    dplyr::left_join(ac_population, by = "ac_uid", relationship = "one-to-one") |>
    dplyr::mutate(
      total_pc100k = per_100k_local(total_n, proxy_ac_pop),
      mfg_pc100k = per_100k_local(mfg_n, proxy_ac_pop),
      services_pc100k = per_100k_local(services_n, proxy_ac_pop)
    ) |>
    dplyr::select(-proxy_ac_pop)

  names(out)[names(out) != "ac_uid"] <- paste0(
    "fdi_",
    names(out)[names(out) != "ac_uid"],
    "_",
    suffix
  )
  out
}

fdi_0409 <- count_fdi_window(WINDOW_0409_START, WINDOW_0409_END, "0409")
fdi_0914 <- count_fdi_window(WINDOW_0914_START, WINDOW_0914_END, "0914")
fdi_2004 <- count_fdi_window(BASELINE_2004_START, BASELINE_2004_END, "2004_observed")

fdi_ac <- all_ac_ids |>
  dplyr::left_join(fdi_0409, by = "ac_uid", relationship = "one-to-one") |>
  dplyr::left_join(fdi_0914, by = "ac_uid", relationship = "one-to-one") |>
  dplyr::left_join(fdi_2004, by = "ac_uid", relationship = "one-to-one")

# Standardize names so families read naturally below.
for (family in c("total", "mfg", "services")) {
  # count_fdi_window generated, e.g., fdi_total_pc100k_0409.
  pre_pc <- paste0("fdi_", family, "_pc100k_0409")
  post_pc <- paste0("fdi_", family, "_pc100k_0914")
  base_obs_pc <- paste0("fdi_", family, "_pc100k_2004_observed")

  log_pre <- paste0("log1p_fdi_", family, "_pc100k_0409")
  log_post <- paste0("log1p_fdi_", family, "_pc100k_0914")
  post_avg <- paste0("fdi_", family, "_avg_annual_pc100k_0914")
  base_ann <- paste0("fdi_", family, "_pc100k_2004_annualized")

  delta1 <- paste0("delta1_fdi_", family, "_pc100k")
  delta1_literal <- paste0("delta1_literal_observed_fdi_", family, "_pc100k")
  delta2 <- paste0("delta2_fdi_", family, "_pc100k")
  slog_delta1 <- paste0("slog_delta1_fdi_", family, "_pc100k")
  slog_delta1_literal <- paste0("slog_delta1_literal_observed_fdi_", family, "_pc100k")
  slog_delta2 <- paste0("slog_delta2_fdi_", family, "_pc100k")

  fdi_ac[[log_pre]] <- log1p(fdi_ac[[pre_pc]])
  fdi_ac[[log_post]] <- log1p(fdi_ac[[post_pc]])

  # Five exact years = 60 months.
  fdi_ac[[post_avg]] <- fdi_ac[[post_pc]] / 5

  # Annualized Apr-Dec 2004 baseline. The observed-Apr-Dec value itself is also
  # retained so the literal alternative can be substituted later.
  fdi_ac[[base_ann]] <- fdi_ac[[base_obs_pc]] * (12 / BASELINE_2004_MONTHS)

  # Change Definition 1: average annual FDI in Apr-2009--Mar-2014 minus 2004.
  # The requested default uses observed Apr-Dec 2004 literally; an annualized
  # sensitivity is available via ANNUALIZE_2004_BASELINE.
  baseline_for_delta1 <- if (ANNUALIZE_2004_BASELINE) {
    fdi_ac[[base_ann]]
  } else {
    fdi_ac[[base_obs_pc]]
  }
  fdi_ac[[delta1]] <- fdi_ac[[post_avg]] - baseline_for_delta1

  # Literal requested alternative retained irrespective of default switch.
  fdi_ac[[delta1_literal]] <- fdi_ac[[post_avg]] - fdi_ac[[base_obs_pc]]

  # Change Definition 2: post-election-window exposure minus pre-election-window
  # exposure, both measured over exact five-year windows.
  fdi_ac[[delta2]] <- fdi_ac[[post_pc]] - fdi_ac[[pre_pc]]

  fdi_ac[[slog_delta1]] <- signed_log1p(fdi_ac[[delta1]])
  fdi_ac[[slog_delta1_literal]] <- signed_log1p(fdi_ac[[delta1_literal]])
  fdi_ac[[slog_delta2]] <- signed_log1p(fdi_ac[[delta2]])
}

# ============================================================
# 6. ASSEMBLE REGRESSION DATASETS
# ============================================================

ac_ideology_year <- ac_year |>
  dplyr::filter(year %in% c(2009L, 2014L)) |>
  dplyr::left_join(
    ideology_ac_year,
    by = c("ac_uid", "state_no", "ac", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    fdi_ac,
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    # Election-specific level exposure for pooled/year-specific models.
    log1p_fdi_total_election_pc100k = dplyr::if_else(
      year == 2009L,
      log1p_fdi_total_pc100k_0409,
      log1p_fdi_total_pc100k_0914
    ),
    log1p_fdi_mfg_election_pc100k = dplyr::if_else(
      year == 2009L,
      log1p_fdi_mfg_pc100k_0409,
      log1p_fdi_mfg_pc100k_0914
    ),
    log1p_fdi_services_election_pc100k = dplyr::if_else(
      year == 2009L,
      log1p_fdi_services_pc100k_0409,
      log1p_fdi_services_pc100k_0914
    ),
    fdi_total_election_pc100k = dplyr::if_else(
      year == 2009L, fdi_total_pc100k_0409, fdi_total_pc100k_0914
    ),
    fdi_mfg_election_pc100k = dplyr::if_else(
      year == 2009L, fdi_mfg_pc100k_0409, fdi_mfg_pc100k_0914
    ),
    fdi_services_election_pc100k = dplyr::if_else(
      year == 2009L, fdi_services_pc100k_0409, fdi_services_pc100k_0914
    )
  )

# Backward-compatible name for the explicit Center formulas below. Restrict to
# AC-years that actually have a Center outcome so fixest does not print thousands
# of irrelevant LHS-NA removals from the full national AC frame.
ac_centrist_year <- ac_ideology_year |>
  dplyr::filter(!is.na(centrist_bjp_share_weighted))

# Explicit official 2009 and 2014 BJP vote-share variables.
official_bjp_wide <- ac_year |>
  dplyr::filter(year %in% c(2009L, 2014L)) |>
  dplyr::select(ac_uid, year, bjp_vote_share) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = bjp_vote_share,
    names_glue = "bjp_vote_share_{year}"
  )

make_group_2014_data <- function(group_prefix) {
  dv <- paste0(group_prefix, "_bjp_share_weighted")

  ac_ideology_year |>
    dplyr::filter(year == 2014L, !is.na(.data[[dv]])) |>
    dplyr::left_join(
      official_bjp_wide,
      by = "ac_uid",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      ideology_wide,
      by = "ac_uid",
      relationship = "many-to-one"
    )
}

make_group_matched_data <- function(data_2014, group_prefix) {
  dv09 <- paste0(group_prefix, "_bjp_share_weighted_2009")
  dv14 <- paste0(group_prefix, "_bjp_share_weighted_2014")

  data_2014 |>
    dplyr::filter(!is.na(.data[[dv09]]), !is.na(.data[[dv14]]))
}

ac_left_2014 <- make_group_2014_data("left")
ac_centrist_2014 <- make_group_2014_data("centrist")
ac_right_2014 <- make_group_2014_data("right")

ac_left_matched <- make_group_matched_data(ac_left_2014, "left")
ac_centrist_matched <- make_group_matched_data(ac_centrist_2014, "centrist")
ac_right_matched <- make_group_matched_data(ac_right_2014, "right")

# ============================================================
# 7. SAMPLE / CELL-SIZE AUDITS
# ============================================================

make_group_cell_audit <- function(data, group_prefix, ideology_label) {
  dv <- paste0(group_prefix, "_bjp_share_weighted")
  n_total <- paste0("n_", group_prefix)
  n_valid <- paste0("n_", group_prefix, "_valid_vote")
  n_bjp <- paste0("n_", group_prefix, "_bjp")

  data |>
    dplyr::filter(!is.na(.data[[dv]])) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      ideology = ideology_label,
      n_ac_years_with_group_dv = dplyr::n(),
      n_unique_acs = dplyr::n_distinct(ac_uid),
      n_states = dplyr::n_distinct(state_no),
      total_group_respondents = sum(.data[[n_total]], na.rm = TRUE),
      total_group_valid_votes = sum(.data[[n_valid]], na.rm = TRUE),
      total_group_bjp_voters = sum(.data[[n_bjp]], na.rm = TRUE),
      min_group_valid_votes = min(.data[[n_valid]], na.rm = TRUE),
      median_group_valid_votes = stats::median(.data[[n_valid]], na.rm = TRUE),
      max_group_valid_votes = max(.data[[n_valid]], na.rm = TRUE),
      acs_n_ge_1 = sum(.data[[n_valid]] >= 1, na.rm = TRUE),
      acs_n_ge_3 = sum(.data[[n_valid]] >= 3, na.rm = TRUE),
      acs_n_ge_5 = sum(.data[[n_valid]] >= 5, na.rm = TRUE),
      acs_n_ge_10 = sum(.data[[n_valid]] >= 10, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(ideology, dplyr::everything())
}

ideology_cell_audit <- dplyr::bind_rows(
  make_group_cell_audit(ac_ideology_year, "left", "Left"),
  make_group_cell_audit(ac_ideology_year, "centrist", "Center"),
  make_group_cell_audit(ac_ideology_year, "right", "Right")
)

make_matched_audit <- function(data, group_prefix, ideology_label) {
  n09 <- paste0("n_", group_prefix, "_2009")
  n14 <- paste0("n_", group_prefix, "_2014")
  valid09 <- paste0("n_", group_prefix, "_valid_vote_2009")
  valid14 <- paste0("n_", group_prefix, "_valid_vote_2014")

  tibble::tibble(
    ideology = ideology_label,
    n_matched_acs = nrow(data),
    n_states = dplyr::n_distinct(data$state_no),
    total_group_respondents_2009 = sum(data[[n09]], na.rm = TRUE),
    total_group_respondents_2014 = sum(data[[n14]], na.rm = TRUE),
    total_valid_group_votes_2009 = sum(data[[valid09]], na.rm = TRUE),
    total_valid_group_votes_2014 = sum(data[[valid14]], na.rm = TRUE),
    median_valid_group_votes_2009 = stats::median(data[[valid09]], na.rm = TRUE),
    median_valid_group_votes_2014 = stats::median(data[[valid14]], na.rm = TRUE),
    matched_acs_2009_n_ge_3 = sum(data[[valid09]] >= 3, na.rm = TRUE),
    matched_acs_2014_n_ge_3 = sum(data[[valid14]] >= 3, na.rm = TRUE),
    matched_acs_2009_n_ge_5 = sum(data[[valid09]] >= 5, na.rm = TRUE),
    matched_acs_2014_n_ge_5 = sum(data[[valid14]] >= 5, na.rm = TRUE),
    matched_acs_2009_n_ge_10 = sum(data[[valid09]] >= 10, na.rm = TRUE),
    matched_acs_2014_n_ge_10 = sum(data[[valid14]] >= 10, na.rm = TRUE)
  )
}

matched_ideology_audit <- dplyr::bind_rows(
  make_matched_audit(ac_left_matched, "left", "Left"),
  make_matched_audit(ac_centrist_matched, "centrist", "Center"),
  make_matched_audit(ac_right_matched, "right", "Right")
)

readr::write_csv(
  ideology_cell_audit,
  file.path(out_audit_dir, "01_ac_year_ideology_cell_size_audit.csv")
)
readr::write_csv(
  matched_ideology_audit,
  file.path(out_audit_dir, "02_matched_ac_ideology_cell_size_audit.csv")
)

# Save prepared datasets with Left/Center/Right weighted + unweighted outcomes,
# 2009/2014 group-specific shares and counts, official BJP vote shares, and all
# raw/transformed FDI variables.
saveRDS(
  ac_ideology_year,
  file.path(out_data_dir, "ac_ideology_year_prepared.rds")
)
readr::write_csv(
  ac_ideology_year,
  file.path(out_data_dir, "ac_ideology_year_prepared.csv")
)

saveRDS(
  ac_left_2014,
  file.path(out_data_dir, "ac_left_2014_lagged_prepared.rds")
)
saveRDS(
  ac_centrist_2014,
  file.path(out_data_dir, "ac_centrist_2014_lagged_prepared.rds")
)
saveRDS(
  ac_right_2014,
  file.path(out_data_dir, "ac_right_2014_lagged_prepared.rds")
)

saveRDS(
  ac_left_matched,
  file.path(out_data_dir, "ac_left_matched_prepared.rds")
)
saveRDS(
  ac_centrist_matched,
  file.path(out_data_dir, "ac_centrist_matched_prepared.rds")
)
saveRDS(
  ac_right_matched,
  file.path(out_data_dir, "ac_right_matched_prepared.rds")
)

# CSV versions for convenient inspection.
readr::write_csv(ac_left_2014, file.path(out_data_dir, "ac_left_2014_lagged_prepared.csv"))
readr::write_csv(ac_centrist_2014, file.path(out_data_dir, "ac_centrist_2014_lagged_prepared.csv"))
readr::write_csv(ac_right_2014, file.path(out_data_dir, "ac_right_2014_lagged_prepared.csv"))
readr::write_csv(ac_left_matched, file.path(out_data_dir, "ac_left_matched_prepared.csv"))
readr::write_csv(ac_centrist_matched, file.path(out_data_dir, "ac_centrist_matched_prepared.csv"))
readr::write_csv(ac_right_matched, file.path(out_data_dir, "ac_right_matched_prepared.csv"))

# ============================================================
# 8. THE 27 FULL CENTER MODELS
# ============================================================
#
# Default full controls in every model:
#   proxy_ac_pop + sc_pop_share + st_pop_share
#
# Land area (`con08_land_area`) and secondary education (`ed_sec_share`) are
# retained in the prepared datasets for explicit robustness specifications, but
# they are not included in the default 27-model design.
# These 27 explicit formulas are the readable Center reference set. Left and
# Right are estimated in parallel in Section 9 with the same exact design.
# To use the unweighted Center DV, replace centrist_bjp_share_weighted with
# centrist_bjp_share_unweighted (and in matched models replace the 2009 lag too).
# To use raw FDI, replace the log/signed-log exposure with its raw counterpart.
# ============================================================

# ------------------------------------------------------------
# A. Pooled + year-specific level models (Models 1-9)
# ------------------------------------------------------------

m01_pooled_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m02_pooled_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m03_pooled_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m04_2009_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m05_2009_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m06_2009_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m07_2014_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2014L),
  cluster = ~pc_cluster_id
)

m08_2014_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2014L),
  cluster = ~pc_cluster_id
)

m09_2014_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2014L),
  cluster = ~pc_cluster_id
)

# ------------------------------------------------------------
# B. Broad 2014 lagged-outcome models (Models 10-18)
# Baseline = official AC BJP vote share in 2009.
# ------------------------------------------------------------

m10_lagged_total_level <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_pc100k_0914 +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m11_lagged_mfg_level <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_pc100k_0914 +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m12_lagged_services_level <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_pc100k_0914 +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m13_lagged_total_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_total_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m14_lagged_mfg_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_mfg_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m15_lagged_services_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_services_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m16_lagged_total_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_total_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m17_lagged_mfg_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_mfg_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m18_lagged_services_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_services_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

# ------------------------------------------------------------
# C. Matched-NES lagged-centrist-outcome models (Models 19-27)
# Baseline = weighted share of centrists voting BJP in that AC in 2009.
# ------------------------------------------------------------

m19_matched_total_level <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "19 Level: total"
)

m20_matched_mfg_level <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "20 Level: manufacturing"
)

m21_matched_services_level <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "21 Level: services"
)

m22_matched_total_change1 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_total_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "22 Change 1: total"
)

m23_matched_mfg_change1 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_mfg_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "23 Change 1: manufacturing"
)

m24_matched_services_change1 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_services_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "24 Change 1: services"
)

m25_matched_total_change2 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_total_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "25 Change 2: total"
)

m26_matched_mfg_change2 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_mfg_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "26 Change 2: manufacturing"
)

m27_matched_services_change2 <- safe_feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_services_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share |
    state_no,
  data = ac_centrist_matched,
  ideology = "Center",
  model_label = "27 Change 2: services"
)

# ============================================================
# 9. PARALLEL LEFT/RIGHT MODEL SETS + MODEL LISTS
# ============================================================

# Center model lists preserve the explicit formulas above.
models_center_A <- list(
  "1 Pooled: total" = m01_pooled_total,
  "2 Pooled: manufacturing" = m02_pooled_mfg,
  "3 Pooled: services" = m03_pooled_services,
  "4 2009: total" = m04_2009_total,
  "5 2009: manufacturing" = m05_2009_mfg,
  "6 2009: services" = m06_2009_services,
  "7 2014: total" = m07_2014_total,
  "8 2014: manufacturing" = m08_2014_mfg,
  "9 2014: services" = m09_2014_services
)

models_center_B <- list(
  "10 Level: total" = m10_lagged_total_level,
  "11 Level: manufacturing" = m11_lagged_mfg_level,
  "12 Level: services" = m12_lagged_services_level,
  "13 Change 1: total" = m13_lagged_total_change1,
  "14 Change 1: manufacturing" = m14_lagged_mfg_change1,
  "15 Change 1: services" = m15_lagged_services_change1,
  "16 Change 2: total" = m16_lagged_total_change2,
  "17 Change 2: manufacturing" = m17_lagged_mfg_change2,
  "18 Change 2: services" = m18_lagged_services_change2
)

models_center_C <- list(
  "19 Level: total" = m19_matched_total_level,
  "20 Level: manufacturing" = m20_matched_mfg_level,
  "21 Level: services" = m21_matched_services_level,
  "22 Change 1: total" = m22_matched_total_change1,
  "23 Change 1: manufacturing" = m23_matched_mfg_change1,
  "24 Change 1: services" = m24_matched_services_change1,
  "25 Change 2: total" = m25_matched_total_change2,
  "26 Change 2: manufacturing" = m26_matched_mfg_change2,
  "27 Change 2: services" = m27_matched_services_change2
)
models_center_27 <- c(models_center_A, models_center_B, models_center_C)

MODEL_LABELS_27 <- c(
  "1 Pooled: total", "2 Pooled: manufacturing", "3 Pooled: services",
  "4 2009: total", "5 2009: manufacturing", "6 2009: services",
  "7 2014: total", "8 2014: manufacturing", "9 2014: services",
  "10 Level: total", "11 Level: manufacturing", "12 Level: services",
  "13 Change 1: total", "14 Change 1: manufacturing", "15 Change 1: services",
  "16 Change 2: total", "17 Change 2: manufacturing", "18 Change 2: services",
  "19 Level: total", "20 Level: manufacturing", "21 Level: services",
  "22 Change 1: total", "23 Change 1: manufacturing", "24 Change 1: services",
  "25 Change 2: total", "26 Change 2: manufacturing", "27 Change 2: services"
)

# Compact helper for exact Left/Right replications of the Center design.
# Every fit is complete-case filtered and failure-safe so a sparse matched sample
# cannot terminate the rest of the pipeline.
fit_parallel_ideology_models <- function(group_prefix, data_2014, data_matched) {
  dv <- paste0(group_prefix, "_bjp_share_weighted")
  dv09 <- paste0(group_prefix, "_bjp_share_weighted_2009")
  dv14 <- paste0(group_prefix, "_bjp_share_weighted_2014")
  # Keep this identical to the Center reference models above.
  controls <- paste(
    "proxy_ac_pop + sc_pop_share + st_pop_share"
  )

  model_counter <- 0L
  ideology_label <- dplyr::recode(
    group_prefix,
    left = "Left",
    centrist = "Center",
    right = "Right",
    .default = group_prefix
  )

  fit <- function(lhs, exposure, data, fe, baseline = NULL, year_filter = NULL) {
    model_counter <<- model_counter + 1L
    model_label <- MODEL_LABELS_27[[model_counter]]

    rhs <- paste0(
      "muslim_share_2001_dist_proxy * ", exposure,
      if (!is.null(baseline)) paste0(" + ", baseline) else "",
      " + ", controls
    )
    fml <- stats::as.formula(paste0(lhs, " ~ ", rhs, " | ", fe))
    fit_data <- if (is.null(year_filter)) data else dplyr::filter(data, year == year_filter)
    fit_data <- fit_data |> dplyr::filter(!is.na(.data[[lhs]]))

    safe_feols(
      fml = fml,
      data = fit_data,
      ideology = ideology_label,
      model_label = model_label
    )
  }

  list(
    "1 Pooled: total" = fit(dv, "log1p_fdi_total_election_pc100k", ac_ideology_year, "state_no + year"),
    "2 Pooled: manufacturing" = fit(dv, "log1p_fdi_mfg_election_pc100k", ac_ideology_year, "state_no + year"),
    "3 Pooled: services" = fit(dv, "log1p_fdi_services_election_pc100k", ac_ideology_year, "state_no + year"),
    "4 2009: total" = fit(dv, "log1p_fdi_total_election_pc100k", ac_ideology_year, "state_no", year_filter = 2009L),
    "5 2009: manufacturing" = fit(dv, "log1p_fdi_mfg_election_pc100k", ac_ideology_year, "state_no", year_filter = 2009L),
    "6 2009: services" = fit(dv, "log1p_fdi_services_election_pc100k", ac_ideology_year, "state_no", year_filter = 2009L),
    "7 2014: total" = fit(dv, "log1p_fdi_total_election_pc100k", ac_ideology_year, "state_no", year_filter = 2014L),
    "8 2014: manufacturing" = fit(dv, "log1p_fdi_mfg_election_pc100k", ac_ideology_year, "state_no", year_filter = 2014L),
    "9 2014: services" = fit(dv, "log1p_fdi_services_election_pc100k", ac_ideology_year, "state_no", year_filter = 2014L),

    "10 Level: total" = fit(dv, "log1p_fdi_total_pc100k_0914", data_2014, "state_no", "bjp_vote_share_2009"),
    "11 Level: manufacturing" = fit(dv, "log1p_fdi_mfg_pc100k_0914", data_2014, "state_no", "bjp_vote_share_2009"),
    "12 Level: services" = fit(dv, "log1p_fdi_services_pc100k_0914", data_2014, "state_no", "bjp_vote_share_2009"),
    "13 Change 1: total" = fit(dv, "slog_delta1_fdi_total_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),
    "14 Change 1: manufacturing" = fit(dv, "slog_delta1_fdi_mfg_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),
    "15 Change 1: services" = fit(dv, "slog_delta1_fdi_services_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),
    "16 Change 2: total" = fit(dv, "slog_delta2_fdi_total_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),
    "17 Change 2: manufacturing" = fit(dv, "slog_delta2_fdi_mfg_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),
    "18 Change 2: services" = fit(dv, "slog_delta2_fdi_services_pc100k", data_2014, "state_no", "bjp_vote_share_2009"),

    "19 Level: total" = fit(dv14, "log1p_fdi_total_pc100k_0914", data_matched, "state_no", dv09),
    "20 Level: manufacturing" = fit(dv14, "log1p_fdi_mfg_pc100k_0914", data_matched, "state_no", dv09),
    "21 Level: services" = fit(dv14, "log1p_fdi_services_pc100k_0914", data_matched, "state_no", dv09),
    "22 Change 1: total" = fit(dv14, "slog_delta1_fdi_total_pc100k", data_matched, "state_no", dv09),
    "23 Change 1: manufacturing" = fit(dv14, "slog_delta1_fdi_mfg_pc100k", data_matched, "state_no", dv09),
    "24 Change 1: services" = fit(dv14, "slog_delta1_fdi_services_pc100k", data_matched, "state_no", dv09),
    "25 Change 2: total" = fit(dv14, "slog_delta2_fdi_total_pc100k", data_matched, "state_no", dv09),
    "26 Change 2: manufacturing" = fit(dv14, "slog_delta2_fdi_mfg_pc100k", data_matched, "state_no", dv09),
    "27 Change 2: services" = fit(dv14, "slog_delta2_fdi_services_pc100k", data_matched, "state_no", dv09)
  )
}

models_left_27 <- fit_parallel_ideology_models("left", ac_left_2014, ac_left_matched)
models_right_27 <- fit_parallel_ideology_models("right", ac_right_2014, ac_right_matched)

models_left_A <- models_left_27[1:9]
models_left_B <- models_left_27[10:18]
models_left_C <- models_left_27[19:27]
models_right_A <- models_right_27[1:9]
models_right_B <- models_right_27[10:18]
models_right_C <- models_right_27[19:27]

models_81 <- c(
  stats::setNames(models_left_27, paste0("Left | ", names(models_left_27))),
  stats::setNames(models_center_27, paste0("Center | ", names(models_center_27))),
  stats::setNames(models_right_27, paste0("Right | ", names(models_right_27)))
) |>
  purrr::compact()

# Preserve the full 27-slot ideology lists, including NULL placeholders for
# non-estimable matched models. The all-ideology object contains successful fits
# only and is therefore safe for downstream modelsummary/broom-style operations.
saveRDS(models_center_27, file.path(out_data_dir, "models_center_27.rds"))
saveRDS(models_left_27, file.path(out_data_dir, "models_left_27.rds"))
saveRDS(models_right_27, file.path(out_data_dir, "models_right_27.rds"))
saveRDS(models_81, file.path(out_data_dir, "models_all_ideologies_successful.rds"))

# ============================================================
# 10. TABLES + MODEL / INTERACTION AUDITS
# ============================================================

write_model_family <- function(models, filename) {
  valid_models <- purrr::compact(models)

  if (length(valid_models) == 0L) {
    message("Skipping empty model table: ", filename)
    return(invisible(NULL))
  }

  modelsummary::modelsummary(
    valid_models,
    stars = TRUE,
    output = file.path(out_table_dir, filename)
  )
}

write_model_family(models_center_A, "01_center_pooled_and_year_specific_models.html")
write_model_family(models_center_B, "02_center_2014_official_bjp_lagged_models.html")
write_model_family(models_center_C, "03_center_matched_nes_lagged_models.html")
write_model_family(models_left_A, "04_left_pooled_and_year_specific_models.html")
write_model_family(models_left_B, "05_left_2014_official_bjp_lagged_models.html")
write_model_family(models_left_C, "06_left_matched_nes_lagged_models.html")
write_model_family(models_right_A, "07_right_pooled_and_year_specific_models.html")
write_model_family(models_right_B, "08_right_2014_official_bjp_lagged_models.html")
write_model_family(models_right_C, "09_right_matched_nes_lagged_models.html")

successful_model_audit <- purrr::imap_dfr(
  models_81,
  function(fit, model_label) {
    parts <- stringr::str_split_fixed(model_label, " \\| ", 2)
    tibble::tibble(
      ideology = parts[1],
      model = parts[2],
      nobs = stats::nobs(fit),
      r2 = as.numeric(fixest::fitstat(fit, "r2")$r2),
      within_r2 = tryCatch(
        as.numeric(fixest::fitstat(fit, "wr2")$wr2),
        error = function(e) NA_real_
      ),
      formula = paste(deparse(stats::formula(fit)), collapse = " ")
    )
  }
)

fit_log <- if (length(model_fit_log) == 0L) {
  tibble::tibble(
    ideology = character(), model = character(), status = character(),
    n_input = integer(), n_complete = integer(), n_states_complete = integer(),
    n_clusters_complete = integer(), focal_retained = logical(), error = character()
  )
} else {
  dplyr::bind_rows(model_fit_log)
}

expected_model_grid <- tidyr::crossing(
  ideology = c("Left", "Center", "Right"),
  model = MODEL_LABELS_27
)

model_audit <- expected_model_grid |>
  dplyr::left_join(
    successful_model_audit,
    by = c("ideology", "model"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    fit_log |>
      dplyr::select(
        ideology, model, logged_status = status, n_input, n_complete,
        n_states_complete, n_clusters_complete, focal_retained, error
      ),
    by = c("ideology", "model"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    status = dplyr::case_when(
      !is.na(nobs) ~ "estimated",
      !is.na(logged_status) ~ logged_status,
      TRUE ~ "not_estimated"
    )
  ) |>
  dplyr::select(
    ideology, model, status, nobs, n_input, n_complete,
    n_states_complete, n_clusters_complete, focal_retained,
    r2, within_r2, formula, error
  )

readr::write_csv(
  model_audit,
  file.path(out_audit_dir, "03_model_sample_formula_and_estimability_audit_all_ideologies.csv")
)
readr::write_csv(
  fit_log,
  file.path(out_audit_dir, "03b_safe_fit_log.csv")
)

# Extract the focal Muslim-share x FDI interaction from each model so the same
# specification can be compared directly across Left, Center, and Right.
extract_focal_interaction <- function(fit, ideology, model_label) {
  ct <- as.data.frame(fixest::coeftable(fit))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL

  hit <- ct |>
    dplyr::filter(
      stringr::str_detect(term, "muslim_share_2001_dist_proxy") &
        stringr::str_detect(term, ":") &
        stringr::str_detect(term, "fdi_")
    )

  if (nrow(hit) != 1L) {
    return(tibble::tibble(
      ideology = ideology,
      model = model_label,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      nobs = stats::nobs(fit)
    ))
  }

  tibble::tibble(
    ideology = ideology,
    model = model_label,
    term = hit$term[1],
    estimate = as.numeric(hit[[1]][1]),
    std_error = as.numeric(hit[[2]][1]),
    p_value = as.numeric(hit[[4]][1]),
    nobs = stats::nobs(fit)
  )
}

interaction_comparison_estimated <- dplyr::bind_rows(
  purrr::imap_dfr(purrr::compact(models_left_27), ~extract_focal_interaction(.x, "Left", .y)),
  purrr::imap_dfr(purrr::compact(models_center_27), ~extract_focal_interaction(.x, "Center", .y)),
  purrr::imap_dfr(purrr::compact(models_right_27), ~extract_focal_interaction(.x, "Right", .y))
)

# Keep all 81 planned ideology x design cells in the comparison output.
# Non-estimable models appear explicitly with NA coefficients and their status.
interaction_comparison <- expected_model_grid |>
  dplyr::left_join(
    interaction_comparison_estimated,
    by = c("ideology", "model"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    model_audit |> dplyr::select(ideology, model, status),
    by = c("ideology", "model"),
    relationship = "one-to-one"
  )

interaction_comparison_wide <- interaction_comparison |>
  dplyr::select(ideology, model, status, estimate, std_error, p_value, nobs) |>
  tidyr::pivot_wider(
    names_from = ideology,
    values_from = c(status, estimate, std_error, p_value, nobs),
    names_glue = "{ideology}_{.value}"
  )


readr::write_csv(
  interaction_comparison,
  file.path(out_audit_dir, "04_focal_interaction_comparison_long.csv")
)
readr::write_csv(
  interaction_comparison_wide,
  file.path(out_audit_dir, "05_focal_interaction_comparison_wide.csv")
)

# ============================================================
# 11. CONSOLE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("CALENDAR-YEAR FDI PER 100K SUMMARY, 2004-2014\n")
cat("============================================================\n")
print(annual_fdi_summary, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("AC-YEAR IDEOLOGY CELL-SIZE AUDIT\n")
cat("============================================================\n")
print(ideology_cell_audit, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("MATCHED-NES IDEOLOGY CELL-SIZE AUDIT\n")
cat("============================================================\n")
print(matched_ideology_audit, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("FOCAL MUSLIM-SHARE x FDI INTERACTIONS BY IDEOLOGY\n")
cat("============================================================\n")
print(interaction_comparison, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("81-MODEL AUDIT (27 DESIGNS x 3 IDEOLOGIES)\n")
cat("============================================================\n")
print(model_audit, n = Inf, width = Inf)

message("Complete.")
message("Prepared data: ", out_data_dir)
message("Model tables: ", out_table_dir)
message("Audits: ", out_audit_dir)

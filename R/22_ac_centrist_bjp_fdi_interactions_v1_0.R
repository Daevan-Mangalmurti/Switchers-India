# ============================================================
# 22_ac_centrist_bjp_fdi_interactions_v1_0.R
#
# Assembly-constituency models of BJP voting among centrists.
# Revision: 2026-08-20-v1.0
#
# Core question:
#   Among centrist NES respondents, is BJP support more strongly related to
#   FDI exposure in ACs with larger pre-existing Muslim population shares?
#
# MAIN MODEL:
#   Centrist BJP share ~ Muslim share (2001) * FDI exposure
#                      + population + SC share + ST share + land area
#                      + secondary education share + fixed effects
#
# This script:
#   1. rebuilds the 2009 and 2014 centrist classifications from RAW NES files
#      using the current audited cross-year coding rules;
#   2. constructs weighted and unweighted AC-year BJP vote shares among
#      centrists, retaining all underlying respondent counts/weighted totals;
#   3. constructs election-to-election FDI exposure using April election-month
#      cutoffs and the existing spatial FDI exposure file;
#   4. constructs two FDI-change definitions, raw and signed-log transformed;
#   5. prepares pooled, 2014 lagged-outcome, and matched-NES datasets;
#   6. estimates exactly 27 full models.
#
# DEFAULTS USED IN THE 27 MODELS:
#   - DV: survey-weighted centrist BJP share
#   - FDI scope: local (own AC + touching ACs)
#   - FDI status: all announced/opened projects
#   - level FDI: log1p(projects per 100,000)
#   - change FDI: sign(x) * log1p(abs(x))
#   - controls: population + SC share + ST share + land area
#               + secondary-education share
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

SCRIPT_REVISION <- "2026-08-20-v1.0-centrist-bjp-ac-fdi"
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
  "ac_centrist_bjp_fdi_interactions_v1_0"
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
    ideology_main, vote_valid, voted_bjp, survey_weight
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
    ideology_main, vote_valid, voted_bjp, survey_weight
  )

nes_fresh <- dplyr::bind_rows(d09, d14)

# ============================================================
# 4. AC-YEAR CENTRIST BJP OUTCOMES + RESPONDENT COUNTS
# ============================================================

centrist_ac_year <- nes_fresh |>
  dplyr::filter(ideology_main == "Center", !is.na(ac_uid)) |>
  dplyr::group_by(ac_uid, state_no, ac, year) |>
  dplyr::summarise(
    n_centrist = dplyr::n(),
    n_centrist_valid_vote = sum(vote_valid, na.rm = TRUE),
    n_centrist_bjp = sum(voted_bjp == 1, na.rm = TRUE),

    wt_centrist = sum(survey_weight, na.rm = TRUE),
    wt_centrist_valid_vote = sum(
      survey_weight[vote_valid & !is.na(survey_weight)],
      na.rm = TRUE
    ),
    wt_centrist_bjp = sum(
      survey_weight[vote_valid & voted_bjp == 1 & !is.na(survey_weight)],
      na.rm = TRUE
    ),

    centrist_bjp_share_unweighted = dplyr::if_else(
      n_centrist_valid_vote > 0,
      n_centrist_bjp / n_centrist_valid_vote,
      NA_real_
    ),
    centrist_bjp_share_weighted = weighted_mean_safe(
      voted_bjp[vote_valid],
      survey_weight[vote_valid]
    ),
    .groups = "drop"
  )

# Explicit 2009 and 2014 wide outcomes/counts, including the 2014 variables
# requested for downstream auditing and matched-sample construction.
centrist_wide <- centrist_ac_year |>
  dplyr::select(
    ac_uid, year,
    n_centrist, n_centrist_valid_vote, n_centrist_bjp,
    wt_centrist, wt_centrist_valid_vote, wt_centrist_bjp,
    centrist_bjp_share_unweighted,
    centrist_bjp_share_weighted
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(
      n_centrist, n_centrist_valid_vote, n_centrist_bjp,
      wt_centrist, wt_centrist_valid_vote, wt_centrist_bjp,
      centrist_bjp_share_unweighted,
      centrist_bjp_share_weighted
    ),
    names_glue = "{.value}_{year}"
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

ac_centrist_year <- ac_year |>
  dplyr::filter(year %in% c(2009L, 2014L)) |>
  dplyr::left_join(
    centrist_ac_year,
    by = c("ac_uid", "state_no", "ac", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    fdi_ac,
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    # Election-specific level exposure for the pooled/year-specific models.
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

# Explicit official 2009 and 2014 BJP vote-share variables.
official_bjp_wide <- ac_year |>
  dplyr::filter(year %in% c(2009L, 2014L)) |>
  dplyr::select(ac_uid, year, bjp_vote_share) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = bjp_vote_share,
    names_glue = "bjp_vote_share_{year}"
  )

# Broad 2014 data: every 2014 AC with a centrist vote-share outcome can enter;
# individual models then drop rows missing required covariates/baseline BJP.
ac_centrist_2014 <- ac_centrist_year |>
  dplyr::filter(year == 2014L, !is.na(centrist_bjp_share_weighted)) |>
  dplyr::left_join(
    official_bjp_wide,
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    centrist_wide,
    by = "ac_uid",
    relationship = "many-to-one"
  )

# Matched NES data: require an estimable centrist BJP share in both waves.
ac_centrist_matched <- ac_centrist_2014 |>
  dplyr::filter(
    !is.na(centrist_bjp_share_weighted_2009),
    !is.na(centrist_bjp_share_weighted_2014)
  )

# ============================================================
# 7. SAMPLE / CELL-SIZE AUDITS
# ============================================================

ac_year_cell_audit <- ac_centrist_year |>
  dplyr::filter(!is.na(centrist_bjp_share_weighted)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n_ac_years_with_centrist_dv = dplyr::n(),
    n_unique_acs = dplyr::n_distinct(ac_uid),
    n_states = dplyr::n_distinct(state_no),
    total_centrists = sum(n_centrist, na.rm = TRUE),
    total_centrist_valid_votes = sum(n_centrist_valid_vote, na.rm = TRUE),
    total_centrist_bjp_voters = sum(n_centrist_bjp, na.rm = TRUE),
    min_centrist_valid_votes = min(n_centrist_valid_vote, na.rm = TRUE),
    median_centrist_valid_votes = stats::median(n_centrist_valid_vote, na.rm = TRUE),
    max_centrist_valid_votes = max(n_centrist_valid_vote, na.rm = TRUE),
    acs_n_ge_1 = sum(n_centrist_valid_vote >= 1, na.rm = TRUE),
    acs_n_ge_3 = sum(n_centrist_valid_vote >= 3, na.rm = TRUE),
    acs_n_ge_5 = sum(n_centrist_valid_vote >= 5, na.rm = TRUE),
    acs_n_ge_10 = sum(n_centrist_valid_vote >= 10, na.rm = TRUE),
    .groups = "drop"
  )

matched_cell_audit <- tibble::tibble(
  n_matched_acs = nrow(ac_centrist_matched),
  n_states = dplyr::n_distinct(ac_centrist_matched$state_no),
  total_centrists_2009 = sum(ac_centrist_matched$n_centrist_2009, na.rm = TRUE),
  total_centrists_2014 = sum(ac_centrist_matched$n_centrist_2014, na.rm = TRUE),
  total_valid_centrist_votes_2009 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2009,
    na.rm = TRUE
  ),
  total_valid_centrist_votes_2014 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2014,
    na.rm = TRUE
  ),
  median_valid_centrist_votes_2009 = stats::median(
    ac_centrist_matched$n_centrist_valid_vote_2009,
    na.rm = TRUE
  ),
  median_valid_centrist_votes_2014 = stats::median(
    ac_centrist_matched$n_centrist_valid_vote_2014,
    na.rm = TRUE
  ),
  matched_acs_2009_n_ge_3 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2009 >= 3,
    na.rm = TRUE
  ),
  matched_acs_2014_n_ge_3 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2014 >= 3,
    na.rm = TRUE
  ),
  matched_acs_2009_n_ge_5 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2009 >= 5,
    na.rm = TRUE
  ),
  matched_acs_2014_n_ge_5 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2014 >= 5,
    na.rm = TRUE
  ),
  matched_acs_2009_n_ge_10 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2009 >= 10,
    na.rm = TRUE
  ),
  matched_acs_2014_n_ge_10 = sum(
    ac_centrist_matched$n_centrist_valid_vote_2014 >= 10,
    na.rm = TRUE
  )
)

readr::write_csv(
  ac_year_cell_audit,
  file.path(out_audit_dir, "01_ac_year_centrist_cell_size_audit.csv")
)
readr::write_csv(
  matched_cell_audit,
  file.path(out_audit_dir, "02_matched_ac_centrist_cell_size_audit.csv")
)

# Save prepared datasets with BOTH weighted/unweighted DVs, BOTH election-year
# vote shares, respondent counts, raw/log FDI, and raw/signed-log changes.
saveRDS(
  ac_centrist_year,
  file.path(out_data_dir, "ac_centrist_year_prepared.rds")
)
readr::write_csv(
  ac_centrist_year,
  file.path(out_data_dir, "ac_centrist_year_prepared.csv")
)

saveRDS(
  ac_centrist_2014,
  file.path(out_data_dir, "ac_centrist_2014_lagged_prepared.rds")
)
readr::write_csv(
  ac_centrist_2014,
  file.path(out_data_dir, "ac_centrist_2014_lagged_prepared.csv")
)

saveRDS(
  ac_centrist_matched,
  file.path(out_data_dir, "ac_centrist_matched_prepared.rds")
)
readr::write_csv(
  ac_centrist_matched,
  file.path(out_data_dir, "ac_centrist_matched_prepared.csv")
)

# ============================================================
# 8. THE 27 FULL MODELS
# ============================================================
#
# Full controls in every model:
#   proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share
#
# To run the basic controls only, remove `+ ed_sec_share`.
# To use the unweighted DV, replace centrist_bjp_share_weighted with
# centrist_bjp_share_unweighted (and in matched models replace the 2009 lag too).
# To use raw FDI, replace the log/signed-log exposure with its raw counterpart.
# ============================================================

# ------------------------------------------------------------
# A. Pooled + year-specific level models (Models 1-9)
# ------------------------------------------------------------

m01_pooled_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m02_pooled_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m03_pooled_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no + year,
  data = ac_centrist_year,
  cluster = ~pc_cluster_id
)

m04_2009_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m05_2009_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m06_2009_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2009L),
  cluster = ~pc_cluster_id
)

m07_2014_total <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2014L),
  cluster = ~pc_cluster_id
)

m08_2014_mfg <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = dplyr::filter(ac_centrist_year, year == 2014L),
  cluster = ~pc_cluster_id
)

m09_2014_services <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_election_pc100k +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
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
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m11_lagged_mfg_level <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_pc100k_0914 +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m12_lagged_services_level <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_pc100k_0914 +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m13_lagged_total_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_total_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m14_lagged_mfg_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_mfg_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m15_lagged_services_change1 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_services_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m16_lagged_total_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_total_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m17_lagged_mfg_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_mfg_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

m18_lagged_services_change2 <- fixest::feols(
  centrist_bjp_share_weighted ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_services_pc100k +
    bjp_vote_share_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_2014,
  cluster = ~pc_cluster_id
)

# ------------------------------------------------------------
# C. Matched-NES lagged-centrist-outcome models (Models 19-27)
# Baseline = weighted share of centrists voting BJP in that AC in 2009.
# ------------------------------------------------------------

m19_matched_total_level <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_total_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m20_matched_mfg_level <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_mfg_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m21_matched_services_level <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * log1p_fdi_services_pc100k_0914 +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m22_matched_total_change1 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_total_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m23_matched_mfg_change1 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_mfg_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m24_matched_services_change1 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta1_fdi_services_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m25_matched_total_change2 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_total_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m26_matched_mfg_change2 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_mfg_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

m27_matched_services_change2 <- fixest::feols(
  centrist_bjp_share_weighted_2014 ~
    muslim_share_2001_dist_proxy * slog_delta2_fdi_services_pc100k +
    centrist_bjp_share_weighted_2009 +
    proxy_ac_pop + sc_pop_share + st_pop_share + con08_land_area + ed_sec_share |
    state_no,
  data = ac_centrist_matched,
  cluster = ~pc_cluster_id
)

# ============================================================
# 9. MODEL LISTS + TABLES + MODEL AUDIT
# ============================================================

models_A <- list(
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

models_B <- list(
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

models_C <- list(
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

models_27 <- c(models_A, models_B, models_C)
saveRDS(models_27, file.path(out_data_dir, "models_27.rds"))

# HTML files keep the console manageable while preserving complete tables.
modelsummary::modelsummary(
  models_A,
  stars = TRUE,
  output = file.path(out_table_dir, "01_pooled_and_year_specific_models.html")
)
modelsummary::modelsummary(
  models_B,
  stars = TRUE,
  output = file.path(out_table_dir, "02_2014_official_bjp_lagged_models.html")
)
modelsummary::modelsummary(
  models_C,
  stars = TRUE,
  output = file.path(out_table_dir, "03_matched_nes_lagged_centrist_models.html")
)

model_audit <- purrr::imap_dfr(
  models_27,
  function(fit, model_label) {
    tibble::tibble(
      model = model_label,
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
readr::write_csv(
  model_audit,
  file.path(out_audit_dir, "03_model_sample_and_formula_audit.csv")
)

# ============================================================
# 10. CONSOLE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("AC-YEAR CENTRIST CELL-SIZE AUDIT\n")
cat("============================================================\n")
print(ac_year_cell_audit, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("MATCHED-NES CELL-SIZE AUDIT\n")
cat("============================================================\n")
print(matched_cell_audit, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("27-MODEL AUDIT\n")
cat("============================================================\n")
print(model_audit, n = Inf, width = Inf)

message("Complete.")
message("Prepared data: ", out_data_dir)
message("Model tables: ", out_table_dir)
message("Audits: ", out_audit_dir)

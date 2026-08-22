# ============================================================
# 02_explore_models.R
# Specification-curve analysis for BJP vote share.
#
# This replaces the former interactive single-model exploration script.
# It estimates and plots specification curves for:
#   1. pooled BJP vote share x Muslim exposure
#   2. pooled BJP vote share x migration/compositional exposure
#   3. first-difference BJP vote share x Muslim exposure
#   4. lagged-outcome BJP vote share x Muslim exposure
#   5. first-difference BJP vote share x migration/compositional exposure
#   6. lagged-outcome BJP vote share x migration/compositional exposure
#
# Each design is estimated separately for total, manufacturing, and services
# FDI, and separately as a two-way interaction and a triple interaction with
# constituency-level centrist share.
#
# Main design decisions encoded here:
#   * FDI scope: own and local only. Adjacent-only exposure is excluded.
#   * FDI status: all, announced, opened.
#   * FDI form: count, per 100k, log1p(per 100k).
#   * Recent_5yr migration measures are excluded.
#   * recent_vs_prior migration-acceleration measures are excluded.
#   * General non-local-language measures are excluded.
#   * Bengali/Bhojpuri targeted outsider measures are retained.
#   * Male prior5-vs-baseline5 acceleration variables are constructed here.
#   * Triple interactions use ideology-complete centrist shares only.
#   * Pooled triple models use contemporaneous AC-year centrist share;
#     change-design triple models use 2009 centrist share, not 2014.
#   * Triple-interaction sample rules vary minimum ideology-complete NES N:
#     N >= 1, N >= 5, and N >= 10; N >= 5 is the preferred reliability rule.
#   * Hindu/Muslim-ratio contrasts are reversed so that "high" always means
#     greater Muslim exposure.
#   * First-difference and lagged-outcome models are separate curves with
#     different dependent variables.
#   * Change-design FDI treatment uses the existing *_2014 exposure columns;
#     these are the April 2009-March 2014 FDI exposure window built upstream.
#   * PC-clustered standard errors are fixed across the primary curves.
#
# Full-grid note:
#   With the currently approved variable universe this script requests approximately
#   217,728 regressions. Results are cached curve-by-curve and checkpointed so
#   an interrupted run can resume. Set SWITCHERS_SPEC_MODE=pilot for a smoke
#   test before a full run.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages()

paths <- build_project_paths(project_root)

ac_year <- readRDS(file.path(paths$final_dir, "ac_year.rds"))
ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))

SCRIPT_REVISION <- "2026-08-07-v5.0-control-repair-preferred-models"
message("Loading 02_explore_models.R revision: ", SCRIPT_REVISION)

# ac_change widens NES share variables but not the underlying respondent counts.
# Join the 2009 ideology-complete denominator here so change-design triple
# interactions can enforce the agreed baseline reliability thresholds.
center_quality_2009 <- ac_year |>
  dplyr::filter(year == 2009) |>
  dplyr::select(
    ac_uid,
    nes_n_ideology_complete_2009 = nes_n_ideology_complete
  ) |>
  dplyr::distinct(ac_uid, .keep_all = TRUE)

ac_change <- ac_change |>
  dplyr::left_join(
    center_quality_2009,
    by = "ac_uid",
    relationship = "one-to-one"
  )

# ============================================================
# 0. RUN SETTINGS
# ============================================================

# Default to a small smoke test so sourcing this file does not accidentally
# launch the full ~217,728-regression multiverse. Set SWITCHERS_SPEC_MODE=full
# explicitly when the pilot succeeds.
RUN_MODE <- tolower(Sys.getenv("SWITCHERS_SPEC_MODE", unset = "pilot"))
if (!RUN_MODE %in% c("full", "pilot")) {
  stop("SWITCHERS_SPEC_MODE must be either 'full' or 'pilot'.")
}

OVERWRITE_EXISTING <- FALSE
CHECKPOINT_EVERY <- 100L
CONFIDENCE_LEVEL <- 0.95

RUN_DESIGNS <- c(
  "pooled_muslim",
  "pooled_migration",
  "first_difference_muslim",
  "lagged_outcome_muslim",
  "first_difference_migration",
  "lagged_outcome_migration"
)

RUN_FDI_FAMILIES <- c("total", "mfg", "services")
RUN_INTERACTION_ORDERS <- c("two_way", "triple")

# In pilot mode, use one representative FDI definition, two moderators, all
# four control sets, and one centrist-share definition per curve. This is only
# a syntax/data smoke test; it is not an inferential analysis.
PILOT_FDI_SCOPE <- "local"
PILOT_FDI_STATUS <- "all"
PILOT_FDI_FORM <- "log1p_pc100k"
PILOT_MODERATORS_PER_DOMAIN <- 2L
PILOT_CENTER_MIN_N <- 5L
# Keep all four control blocks in the smoke test. This raises the pilot from
# 72 to 288 curve regressions but ensures the repaired C2/C3 specifications
# are actually exercised before a full run.
PILOT_CONTROL_SETS <- c("C0", "C1", "C2", "C3")

# ============================================================
# 1. OUTPUT DIRECTORIES
# ============================================================

spec_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "specification_curves"
)

spec_result_dir <- file.path(spec_root, "results")
spec_figure_dir <- file.path(spec_root, "figures")
spec_manifest_dir <- file.path(spec_root, "manifests")
spec_summary_dir <- file.path(spec_root, "summaries")
spec_log_dir <- file.path(spec_root, "logs")

purrr::walk(
  c(
    spec_root,
    spec_result_dir,
    spec_figure_dir,
    spec_manifest_dir,
    spec_summary_dir,
    spec_log_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 2. ANALYSIS-ONLY DERIVED VARIABLES
# ============================================================

# Logged Muslim-population levels were part of the planned specification
# universe but are not stored as final-data columns.
add_logged_muslim_levels <- function(data) {
  required <- c("muslim_population_2001", "muslim_population_2011")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "Cannot construct logged Muslim population variables. Missing: ",
      paste(missing, collapse = ", ")
    )
  }
  
  data |>
    dplyr::mutate(
      log1p_muslim_population_2001 = log1p(muslim_population_2001),
      log1p_muslim_population_2011 = log1p(muslim_population_2011)
    )
}

# Construct male migration acceleration using exactly the same formulas used
# upstream for all-migrant prior5-vs-baseline5 acceleration.
add_male_prior5_acceleration <- function(data, suffix = "") {
  prior_var <- paste0("male_mig_prior_5yr_total", suffix)
  baseline_var <- paste0("male_mig_baseline_5yr_total", suffix)
  
  required <- c(prior_var, baseline_var)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "Cannot construct male prior5-vs-baseline5 acceleration for suffix '",
      suffix,
      "'. Missing: ",
      paste(missing, collapse = ", ")
    )
  }
  
  ratio_name <- paste0("male_mig_accel_prior5_vs_baseline5_ratio", suffix)
  pct_name <- paste0("male_mig_accel_prior5_vs_baseline5_pct_change", suffix)
  log_name <- paste0("male_mig_accel_prior5_vs_baseline5_log", suffix)
  log1p_name <- paste0("male_mig_accel_prior5_vs_baseline5_log1p", suffix)
  
  prior <- data[[prior_var]]
  baseline <- data[[baseline_var]]
  
  data[[ratio_name]] <- safe_ratio(prior, baseline)
  data[[pct_name]] <- safe_pct_change(prior, baseline)
  data[[log_name]] <- safe_log_ratio(prior, baseline)
  data[[log1p_name]] <- log1p(prior) - log1p(baseline)
  
  data
}

ac_year <- ac_year |>
  add_logged_muslim_levels() |>
  add_male_prior5_acceleration("")

ac_change <- ac_change |>
  add_logged_muslim_levels() |>
  add_male_prior5_acceleration("_2009") |>
  add_male_prior5_acceleration("_2014")

# The intended working-age employment-rate control is unavailable because the
# Census C-13 working-age denominator was not available upstream. The Economic
# Census employment numerator itself is usable. The existing
# employment_per_total_population variable is therefore retained as a broader
# district employment-intensity proxy attached to ACs. Log it for C2/C3 to
# reduce leverage from its small, right-skewed upper tail (concentrated in Delhi).
add_logged_employment_intensity <- function(data) {
  required <- "employment_per_total_population"
  missing <- setdiff(required, names(data))
  
  if (length(missing) > 0) {
    stop(
      "Cannot construct logged employment-intensity proxy. Missing: ",
      paste(missing, collapse = ", ")
    )
  }
  
  if (any(data$employment_per_total_population < 0, na.rm = TRUE)) {
    stop(
      "employment_per_total_population contains negative values; ",
      "inspect the upstream employment construction before modelling."
    )
  }
  
  data |>
    dplyr::mutate(
      log1p_employment_per_total_population =
        log1p(employment_per_total_population)
    )
}

ac_year <- add_logged_employment_intensity(ac_year)
ac_change <- add_logged_employment_intensity(ac_change)

# ============================================================
# 3. VARIABLE METADATA
# ============================================================

# ------------------------------------------------------------
# 3A. FDI metadata
# Own and local only. Adjacent-only exposure is intentionally excluded.
# ------------------------------------------------------------

fdi_meta <- tidyr::crossing(
  fdi_family = c("total", "mfg", "services"),
  fdi_scope = c("own", "local"),
  fdi_status = c("all", "announced", "opened"),
  fdi_form = c("count", "pc100k", "log1p_pc100k")
) |>
  dplyr::mutate(
    fdi_family_label = dplyr::recode(
      fdi_family,
      total = "Total FDI",
      mfg = "Manufacturing FDI",
      services = "Services FDI"
    ),
    fdi_scope_label = dplyr::recode(
      fdi_scope,
      own = "Own AC",
      local = "Local: own + neighbors"
    ),
    fdi_status_label = dplyr::recode(
      fdi_status,
      all = "All announced/opened",
      announced = "Announced",
      opened = "Opened"
    ),
    fdi_form_label = dplyr::recode(
      fdi_form,
      count = "Project count",
      pc100k = "Projects per 100k",
      log1p_pc100k = "log1p projects per 100k"
    ),
    pooled_var = dplyr::case_when(
      fdi_form == "count" ~ paste0(
        "fdi_", fdi_family, "_", fdi_scope, "_", fdi_status, "_n"
      ),
      fdi_form == "pc100k" ~ paste0(
        "fdi_", fdi_family, "_", fdi_scope, "_", fdi_status, "_pc100k"
      ),
      fdi_form == "log1p_pc100k" ~ paste0(
        "log1p_fdi_", fdi_family, "_", fdi_scope, "_", fdi_status, "_pc100k"
      )
    ),
    change_var = paste0(pooled_var, "_2014"),
    baseline_var = paste0(pooled_var, "_2009"),
    fdi_preferred =
      fdi_family == "mfg" &
      fdi_scope == "local" &
      fdi_status == "all" &
      fdi_form == "log1p_pc100k"
  )

# ------------------------------------------------------------
# 3B. Muslim moderator metadata
# orientation = -1 for Hindu/Muslim ratio measures, so a move from low to
# high moderator exposure always means a move toward greater Muslim exposure.
# ------------------------------------------------------------

muslim_meta <- tibble::tribble(
  ~pooled_var, ~moderator_family, ~moderator_form, ~orientation,
  "muslim_population_2001", "Muslim level: 2001", "Population count", 1,
  "log1p_muslim_population_2001", "Muslim level: 2001", "log1p population", 1,
  "muslim_share_2001_dist_proxy", "Muslim level: 2001", "Population share", 1,
  "muslim_population_2011", "Muslim level: 2011", "Population count", 1,
  "log1p_muslim_population_2011", "Muslim level: 2011", "log1p population", 1,
  "muslim_share_2011_dist_proxy", "Muslim level: 2011", "Population share", 1,
  "d_muslim_population_2001_2011_n", "Muslim change: 2001-2011", "Absolute population change", 1,
  "pct_change_muslim_population_2001_2011", "Muslim change: 2001-2011", "Percent population change", 1,
  "d_log1p_muslim_population_2001_2011", "Muslim change: 2001-2011", "log1p population change", 1,
  "d_muslim_share_2001_2011_pp", "Muslim change: 2001-2011", "Share change, pp", 1,
  "hindu_muslim_ratio_2001_dist_proxy", "Hindu/Muslim relative composition", "Hindu/Muslim ratio: 2001", -1,
  "log_hindu_muslim_ratio_2001_dist_proxy", "Hindu/Muslim relative composition", "Log Hindu/Muslim ratio: 2001", -1,
  "hindu_muslim_ratio_2011_dist_proxy", "Hindu/Muslim relative composition", "Hindu/Muslim ratio: 2011", -1,
  "log_hindu_muslim_ratio_2011_dist_proxy", "Hindu/Muslim relative composition", "Log Hindu/Muslim ratio: 2011", -1,
  "d_hindu_muslim_ratio_2001_2011_ratio_points", "Hindu/Muslim relative composition", "Hindu/Muslim ratio change", -1,
  "d_log_hindu_muslim_ratio_2001_2011", "Hindu/Muslim relative composition", "Log Hindu/Muslim ratio change", -1
) |>
  dplyr::mutate(
    change_var = pooled_var,
    moderator_domain = "muslim",
    moderator_preferred =
      pooled_var == "muslim_share_2001_dist_proxy"
  )

# ------------------------------------------------------------
# 3C. Migration/compositional moderator metadata
# Explicitly excluded:
#   * every recent_5yr count/log/share
#   * every recent_vs_prior acceleration measure
#   * recent5_vs_prior10 acceleration
#   * general non-local-language levels/shares/changes
#   * density, neighboring-AC, male-share, and prior-over-stock measures
# ------------------------------------------------------------

migration_meta <- tibble::tribble(
  ~pooled_var, ~change_var, ~moderator_family, ~moderator_form,
  
  "mig_prior_5yr_total", "mig_prior_5yr_total_2014",
  "All migrants: prior 5 years", "Count",
  "log1p_mig_prior_5yr_total", "log1p_mig_prior_5yr_total_2014",
  "All migrants: prior 5 years", "log1p count",
  "mig_prior_5yr_share_ac_pop", "mig_prior_5yr_share_ac_pop_2014",
  "All migrants: prior 5 years", "Share of AC population",
  
  "mig_prior_5_15yr_total", "mig_prior_5_15yr_total_2014",
  "All migrants: prior 5-15 years", "Count",
  "log1p_mig_prior_5_15yr_total", "log1p_mig_prior_5_15yr_total_2014",
  "All migrants: prior 5-15 years", "log1p count",
  "mig_prior_5_15yr_share_ac_pop", "mig_prior_5_15yr_share_ac_pop_2014",
  "All migrants: prior 5-15 years", "Share of AC population",
  
  "mig_total_upto_2001", "mig_total_upto_2001",
  "All migrants: established stock", "Count",
  "log1p_mig_total_upto_2001", "log1p_mig_total_upto_2001",
  "All migrants: established stock", "log1p count",
  "mig_total_upto_2001_share_ac_pop", "mig_total_upto_2001_share_ac_pop",
  "All migrants: established stock", "Share of AC population",
  
  "male_mig_prior_5yr_total", "male_mig_prior_5yr_total_2014",
  "Male migrants: prior 5 years", "Count",
  "log1p_male_mig_prior_5yr_total", "log1p_male_mig_prior_5yr_total_2014",
  "Male migrants: prior 5 years", "log1p count",
  "male_mig_prior_5yr_share_ac_pop", "male_mig_prior_5yr_share_ac_pop_2014",
  "Male migrants: prior 5 years", "Share of AC population",
  
  "male_mig_prior_5_15yr_total", "male_mig_prior_5_15yr_total_2014",
  "Male migrants: prior 5-15 years", "Count",
  "log1p_male_mig_prior_5_15yr_total", "log1p_male_mig_prior_5_15yr_total_2014",
  "Male migrants: prior 5-15 years", "log1p count",
  "male_mig_prior_5_15yr_share_ac_pop", "male_mig_prior_5_15yr_share_ac_pop_2014",
  "Male migrants: prior 5-15 years", "Share of AC population",
  
  "male_mig_total_upto_2001", "male_mig_total_upto_2001",
  "Male migrants: established stock", "Count",
  "log1p_male_mig_total_upto_2001", "log1p_male_mig_total_upto_2001",
  "Male migrants: established stock", "log1p count",
  "male_mig_total_upto_2001_share_ac_pop", "male_mig_total_upto_2001_share_ac_pop",
  "Male migrants: established stock", "Share of AC population",
  
  "mig_accel_prior5_vs_baseline5_ratio", "mig_accel_prior5_vs_baseline5_ratio_2014",
  "All migrants: prior5 vs baseline5 acceleration", "Ratio",
  "mig_accel_prior5_vs_baseline5_pct_change", "mig_accel_prior5_vs_baseline5_pct_change_2014",
  "All migrants: prior5 vs baseline5 acceleration", "Percent change",
  "mig_accel_prior5_vs_baseline5_log", "mig_accel_prior5_vs_baseline5_log_2014",
  "All migrants: prior5 vs baseline5 acceleration", "Log ratio",
  "mig_accel_prior5_vs_baseline5_log1p", "mig_accel_prior5_vs_baseline5_log1p_2014",
  "All migrants: prior5 vs baseline5 acceleration", "Difference in log1p counts",
  
  "male_mig_accel_prior5_vs_baseline5_ratio", "male_mig_accel_prior5_vs_baseline5_ratio_2014",
  "Male migrants: prior5 vs baseline5 acceleration", "Ratio",
  "male_mig_accel_prior5_vs_baseline5_pct_change", "male_mig_accel_prior5_vs_baseline5_pct_change_2014",
  "Male migrants: prior5 vs baseline5 acceleration", "Percent change",
  "male_mig_accel_prior5_vs_baseline5_log", "male_mig_accel_prior5_vs_baseline5_log_2014",
  "Male migrants: prior5 vs baseline5 acceleration", "Log ratio",
  "male_mig_accel_prior5_vs_baseline5_log1p", "male_mig_accel_prior5_vs_baseline5_log1p_2014",
  "Male migrants: prior5 vs baseline5 acceleration", "Difference in log1p counts",
  
  "target_bengali_bhojpuri_population_2001", "target_bengali_bhojpuri_population_2001",
  "Target Bengali/Bhojpuri exposure", "Population count: 2001",
  "target_bengali_bhojpuri_share_2001_dist_proxy", "target_bengali_bhojpuri_share_2001_dist_proxy",
  "Target Bengali/Bhojpuri exposure", "Population share: 2001",
  "target_bengali_bhojpuri_population_2011", "target_bengali_bhojpuri_population_2011",
  "Target Bengali/Bhojpuri exposure", "Population count: 2011",
  "target_bengali_bhojpuri_share_2011_dist_proxy", "target_bengali_bhojpuri_share_2011_dist_proxy",
  "Target Bengali/Bhojpuri exposure", "Population share: 2011",
  "d_target_bengali_bhojpuri_population_2001_2011_n", "d_target_bengali_bhojpuri_population_2001_2011_n",
  "Target Bengali/Bhojpuri exposure", "Population change: 2001-2011",
  "d_target_bengali_bhojpuri_share_2001_2011_pp", "d_target_bengali_bhojpuri_share_2001_2011_pp",
  "Target Bengali/Bhojpuri exposure", "Share change, pp: 2001-2011"
) |>
  dplyr::mutate(
    orientation = 1,
    moderator_domain = "migration",
    moderator_preferred = FALSE
  )

# ------------------------------------------------------------
# 3D. Centrist-share metadata
# Only ideology-complete denominators are retained. The all-respondent versions
# are excluded because missing ideology was effectively counted as non-centrist,
# which made the 2009 measure partly a data-completeness measure.
# Pooled models use contemporaneous AC-year share.
# Change models use the corresponding 2009 baseline share.
# ------------------------------------------------------------

center_meta <- tibble::tribble(
  ~pooled_var, ~change_var, ~center_weighting, ~center_denominator,
  
  "nes_share_center_among_ideology_complete",
  "nes_share_center_among_ideology_complete_2009",
  "Unweighted", "Ideology-complete respondents",
  
  "nes_weighted_share_center_among_ideology_complete",
  "nes_weighted_share_center_among_ideology_complete_2009",
  "Survey weighted", "Ideology-complete respondents"
) |>
  dplyr::mutate(
    center_preferred =
      center_weighting == "Survey weighted"
  )

# Sample-reliability rules are a specification dimension only for triple
# interactions. N >= 5 is the preferred reliability rule for both pooled and
# change designs; N >= 1 and N >= 10 are unrestricted/stricter sensitivities.
center_sample_rules <- tibble::tribble(
  ~center_min_n, ~center_sample_label, ~center_sample_role,
  1L,  "N >= 1",  "Unrestricted sensitivity",
  5L,  "N >= 5",  "Preferred reliability rule",
  10L, "N >= 10", "Stricter sensitivity"
)

# ------------------------------------------------------------
# 3E. Coherent control sets
# ------------------------------------------------------------

control_sets <- tibble::tribble(
  ~control_set, ~control_label, ~control_string,
  "C0", "Size/geography",
  "proxy_ac_pop + con08_land_area",
  
  "C1", "Size/geography + SC/ST composition",
  "proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share",
  
  "C2", "C1 + district employment intensity + education",
  paste(
    "proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share",
    "log1p_employment_per_total_population", "ed_sec_share",
    sep = " + "
  ),
  
  "C3", "C2 + logged per-capita consumption",
  paste(
    "proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share",
    "log1p_employment_per_total_population", "ed_sec_share", "log_secc_cons_pc",
    sep = " + "
  )
) |>
  dplyr::mutate(
    control_preferred =
      control_set == "C1",
    control_role = dplyr::case_when(
      control_set == "C1" ~
        "Preferred baseline adjustment",
      control_set == "C0" ~
        "Minimal adjustment sensitivity",
      control_set == "C2" ~
        "Expanded socioeconomic adjustment",
      control_set == "C3" ~
        "Expanded socioeconomic + consumption adjustment",
      TRUE ~ "Alternative"
    )
  )

# ------------------------------------------------------------
# 3F. Design metadata
# ------------------------------------------------------------

design_meta <- tibble::tribble(
  ~design_id, ~design_label, ~data_name, ~outcome, ~moderator_domain,
  ~fixed_effects, ~design_type, ~outcome_label,
  
  "pooled_muslim",
  "Pooled 2009/2014: Muslim exposure",
  "ac_year", "bjp_vote_share", "muslim",
  "state_no + year", "pooled",
  "BJP vote share",
  
  "pooled_migration",
  "Pooled 2009/2014: Migration/compositional exposure",
  "ac_year", "bjp_vote_share", "migration",
  "state_no + year", "pooled",
  "BJP vote share",
  
  "first_difference_muslim",
  "First difference 2009-2014: Muslim exposure",
  "ac_change", "d_bjp_vote_share_2009_2014_pp", "muslim",
  "state_no", "first_difference",
  "Change in BJP vote share, 2009-2014",
  
  "lagged_outcome_muslim",
  "Lagged outcome 2014: Muslim exposure",
  "ac_change", "bjp_vote_share_2014", "muslim",
  "state_no", "lagged_outcome",
  "BJP vote share in 2014",
  
  "first_difference_migration",
  "First difference 2009-2014: Migration/compositional exposure",
  "ac_change", "d_bjp_vote_share_2009_2014_pp", "migration",
  "state_no", "first_difference",
  "Change in BJP vote share, 2009-2014",
  
  "lagged_outcome_migration",
  "Lagged outcome 2014: Migration/compositional exposure",
  "ac_change", "bjp_vote_share_2014", "migration",
  "state_no", "lagged_outcome",
  "BJP vote share in 2014"
) |>
  dplyr::mutate(
    design_preferred =
      design_type == "first_difference",
    design_role = dplyr::case_when(
      design_type == "first_difference" ~
        "Primary change design",
      design_type == "lagged_outcome" ~
        "Key alternative change design",
      design_type == "pooled" ~
        "Secondary pooled-level design",
      TRUE ~ "Alternative"
    )
  )

# ============================================================
# 4. VALIDATION HELPERS
# ============================================================

assert_has_columns_local <- function(data, columns, label) {
  columns <- unique(columns[!is.na(columns) & nzchar(columns)])
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(data)
}

# Validate the full planned universe before fitting anything.
assert_has_columns_local(
  ac_year,
  c(
    "bjp_vote_share", "state_no", "year", "pc_cluster_id",
    fdi_meta$pooled_var,
    muslim_meta$pooled_var,
    migration_meta$pooled_var,
    center_meta$pooled_var,
    "nes_n_ideology_complete",
    unlist(strsplit(paste(control_sets$control_string, collapse = " + "), " \\+ "))
  ),
  "ac_year"
)

assert_has_columns_local(
  ac_change,
  c(
    "d_bjp_vote_share_2009_2014_pp", "bjp_vote_share_2014",
    "bjp_vote_share_2009", "state_no", "pc_cluster_id",
    fdi_meta$change_var, fdi_meta$baseline_var,
    muslim_meta$change_var,
    migration_meta$change_var,
    center_meta$change_var,
    "nes_n_ideology_complete_2009",
    unlist(strsplit(paste(control_sets$control_string, collapse = " + "), " \\+ "))
  ),
  "ac_change"
)

# ------------------------------------------------------------
# 4A. Control-block completeness audit
# Fail before launching the multiverse if an entire planned control block has
# no complete observations. This guards against the C2/C3 failure encountered
# when employment_per_population_15_64 was entirely missing.
# ------------------------------------------------------------

control_vars_from_string <- function(control_string) {
  vars <- unlist(
    strsplit(
      control_string,
      " \\+ "
    )
  )
  
  unique(vars[nzchar(vars)])
}

audit_control_completeness <- function(data, data_label) {
  purrr::map_dfr(
    seq_len(nrow(control_sets)),
    function(i) {
      vars <- control_vars_from_string(
        control_sets$control_string[[i]]
      )
      
      complete <- stats::complete.cases(
        data[, vars, drop = FALSE]
      )
      
      tibble::tibble(
        data = data_label,
        control_set = control_sets$control_set[[i]],
        control_label = control_sets$control_label[[i]],
        control_preferred =
          control_sets$control_preferred[[i]],
        n_total = nrow(data),
        n_complete = sum(complete),
        pct_complete = 100 * mean(complete)
      )
    }
  )
}

control_block_diagnostics <- dplyr::bind_rows(
  audit_control_completeness(
    ac_year |>
      dplyr::filter(
        year %in% c(2009, 2014)
      ),
    "ac_year_2009_2014"
  ),
  audit_control_completeness(
    ac_change,
    "ac_change"
  )
)

readr::write_csv(
  control_block_diagnostics,
  file.path(
    spec_manifest_dir,
    "control_block_completeness.csv"
  )
)

print(
  control_block_diagnostics,
  n = Inf
)

if (any(control_block_diagnostics$n_complete == 0)) {
  bad_blocks <- control_block_diagnostics |>
    dplyr::filter(
      n_complete == 0
    ) |>
    dplyr::transmute(
      label = paste0(
        data,
        ": ",
        control_set,
        " (",
        control_label,
        ")"
      )
    ) |>
    dplyr::pull(label)
  
  stop(
    "At least one planned control set has zero complete observations: ",
    paste(bad_blocks, collapse = "; "),
    ". Inspect control_block_completeness.csv before running the multiverse."
  )
}

# ============================================================
# 5. CONTRAST REFERENCE VALUES
# ============================================================

finite_values <- function(x) {
  x <- as.numeric(x)
  x[is.finite(x)]
}

fdi_reference <- function(x) {
  x <- finite_values(x)
  positive <- x[x > 0]
  
  if (length(positive) == 0) {
    return(tibble::tibble(
      low = NA_real_, high = NA_real_, delta = NA_real_,
      reference_method = "No positive FDI exposure"
    ))
  }
  
  high <- stats::median(positive, na.rm = TRUE)
  
  tibble::tibble(
    low = 0,
    high = high,
    delta = high,
    reference_method = "0 to median positive FDI"
  )
}

moderator_reference <- function(x, orientation = 1) {
  x <- finite_values(x)
  
  if (length(unique(x)) < 2) {
    return(tibble::tibble(
      low_exposure = NA_real_, high_exposure = NA_real_, delta = NA_real_,
      reference_method = "Insufficient moderator variation"
    ))
  }
  
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  q25 <- as.numeric(q[1])
  q75 <- as.numeric(q[2])
  method <- "25th to 75th percentile"
  
  # If quartiles collapse, use a zero-to-typical-positive contrast when that is
  # meaningful; otherwise use the observed range.
  if (!is.finite(q25) || !is.finite(q75) || q25 == q75) {
    positive <- x[x > 0]
    if (orientation == 1 && any(x == 0) && length(positive) > 0) {
      q25 <- 0
      q75 <- stats::median(positive, na.rm = TRUE)
      method <- "0 to median positive moderator"
    } else {
      q25 <- min(x, na.rm = TRUE)
      q75 <- max(x, na.rm = TRUE)
      method <- "Observed minimum to maximum"
    }
  }
  
  if (orientation == 1) {
    low_exposure <- q25
    high_exposure <- q75
  } else {
    # For Hindu/Muslim ratios, a lower numerical ratio represents greater
    # Muslim exposure. Reverse the numerical endpoints while preserving the
    # substantive low-to-high exposure interpretation.
    low_exposure <- q75
    high_exposure <- q25
    method <- paste0(method, "; reversed for Muslim exposure")
  }
  
  tibble::tibble(
    low_exposure = low_exposure,
    high_exposure = high_exposure,
    delta = high_exposure - low_exposure,
    reference_method = method
  )
}

center_reference <- function(x) {
  x <- finite_values(x)
  
  if (length(unique(x)) < 2) {
    return(tibble::tibble(
      low_center = NA_real_, high_center = NA_real_, delta_center = NA_real_,
      reference_method = "Insufficient centrist-share variation"
    ))
  }
  
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  low <- as.numeric(q[1])
  high <- as.numeric(q[2])
  method <- "25th to 75th percentile"
  
  if (!is.finite(low) || !is.finite(high) || low == high) {
    low <- min(x, na.rm = TRUE)
    high <- max(x, na.rm = TRUE)
    method <- "Observed minimum to maximum"
  }
  
  tibble::tibble(
    low_center = low,
    high_center = high,
    delta_center = high - low,
    reference_method = method
  )
}

# ============================================================
# 6. SPECIFICATION-GRID CONSTRUCTION
# ============================================================

get_design_data <- function(design_row) {
  if (design_row$data_name == "ac_year") {
    ac_year |>
      dplyr::filter(year %in% c(2009, 2014))
  } else {
    ac_change
  }
}

get_moderator_meta <- function(domain) {
  if (domain == "muslim") muslim_meta else migration_meta
}

make_curve_grid <- function(design_row, fdi_family, interaction_order) {
  data <- get_design_data(design_row)
  moderator_meta <- get_moderator_meta(design_row$moderator_domain)
  
  fdi_rows <- fdi_meta |>
    dplyr::filter(.data$fdi_family == .env$fdi_family)
  
  if (RUN_MODE == "pilot") {
    fdi_rows <- fdi_rows |>
      dplyr::filter(
        fdi_scope == PILOT_FDI_SCOPE,
        fdi_status == PILOT_FDI_STATUS,
        fdi_form == PILOT_FDI_FORM
      )
    moderator_meta <- moderator_meta |>
      dplyr::slice_head(n = PILOT_MODERATORS_PER_DOMAIN)
  }
  
  pooled_design <- design_row$design_type == "pooled"
  
  fdi_rows <- fdi_rows |>
    dplyr::mutate(
      exposure_var = if (pooled_design) pooled_var else change_var,
      baseline_fdi_var = if (pooled_design) NA_character_ else baseline_var
    )
  
  moderator_rows <- moderator_meta |>
    dplyr::mutate(
      moderator_var = if (pooled_design) pooled_var else change_var
    )
  
  if (interaction_order == "triple") {
    center_rows <- tidyr::crossing(
      center_meta,
      center_sample_rules
    ) |>
      dplyr::mutate(
        center_var = if (pooled_design) pooled_var else change_var,
        center_n_var = if (
          pooled_design
        ) {
          "nes_n_ideology_complete"
        } else {
          "nes_n_ideology_complete_2009"
        },
        center_rule_primary = center_min_n == 5L
      )
    
    if (RUN_MODE == "pilot") {
      center_rows <- center_rows |>
        dplyr::filter(center_min_n == PILOT_CENTER_MIN_N) |>
        dplyr::slice_head(n = 1)
    }
  } else {
    center_rows <- tibble::tibble(
      pooled_var = NA_character_,
      change_var = NA_character_,
      center_weighting = NA_character_,
      center_denominator = NA_character_,
      center_preferred = FALSE,
      center_min_n = NA_integer_,
      center_sample_label = NA_character_,
      center_sample_role = NA_character_,
      center_var = NA_character_,
      center_n_var = NA_character_,
      center_rule_primary = FALSE
    )
  }
  
  controls <- control_sets
  if (RUN_MODE == "pilot") {
    controls <- controls |>
      dplyr::filter(
        control_set %in% PILOT_CONTROL_SETS
      )
  }
  
  # Reference values are calculated before control-set and center-N variation.
  # Two-way curves use the broad outcome-valid sample. Triple curves use the
  # primary-quality NES sample (ideology-complete N >= 5), so reference values
  # stay within the support of the centrist-share analysis while remaining fixed
  # across the N >= 1 / 5 / 10 sensitivity rules.
  reference_base <- data |>
    dplyr::filter(is.finite(.data[[design_row$outcome]]))
  
  if (interaction_order == "triple") {
    primary_center_n_var <- if (
      pooled_design
    ) {
      "nes_n_ideology_complete"
    } else {
      "nes_n_ideology_complete_2009"
    }
    
    primary_center_var <- if (
      pooled_design
    ) {
      center_meta$pooled_var[[1]]
    } else {
      center_meta$change_var[[1]]
    }
    
    reference_base <- reference_base |>
      dplyr::filter(
        !is.na(.data[[primary_center_var]]),
        !is.na(.data[[primary_center_n_var]]),
        .data[[primary_center_n_var]] >= 5
      )
  }
  
  fdi_refs <- fdi_rows |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ref = list(
        fdi_reference(reference_base[[exposure_var]])
      )
    ) |>
    tidyr::unnest_wider(ref, names_sep = "_fdi_") |>
    dplyr::ungroup() |>
    dplyr::rename(
      fdi_low = ref_fdi_low,
      fdi_high = ref_fdi_high,
      delta_fdi = ref_fdi_delta,
      fdi_reference_method = ref_fdi_reference_method
    ) |>
    dplyr::select(-pooled_var, -change_var, -baseline_var)
  
  moderator_refs <- moderator_rows |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ref = list(
        moderator_reference(
          reference_base[[moderator_var]],
          orientation = orientation
        )
      )
    ) |>
    tidyr::unnest_wider(ref, names_sep = "_mod_") |>
    dplyr::ungroup() |>
    dplyr::rename(
      moderator_low = ref_mod_low_exposure,
      moderator_high = ref_mod_high_exposure,
      delta_moderator = ref_mod_delta,
      moderator_reference_method = ref_mod_reference_method
    ) |>
    dplyr::select(-pooled_var, -change_var)
  
  if (interaction_order == "triple") {
    center_refs <- center_rows |>
      dplyr::rowwise() |>
      dplyr::mutate(
        ref = list(
          center_reference(reference_base[[center_var]])
        )
      ) |>
      tidyr::unnest_wider(ref, names_sep = "_center_") |>
      dplyr::ungroup() |>
      dplyr::rename(
        center_low = ref_center_low_center,
        center_high = ref_center_high_center,
        delta_center = ref_center_delta_center,
        center_reference_method = ref_center_reference_method
      ) |>
      dplyr::select(-pooled_var, -change_var)
  } else {
    center_refs <- center_rows |>
      dplyr::mutate(
        center_low = NA_real_,
        center_high = NA_real_,
        delta_center = 1,
        center_reference_method = NA_character_
      ) |>
      dplyr::select(-pooled_var, -change_var)
  }
  
  grid <- tidyr::crossing(
    fdi_refs,
    moderator_refs,
    center_refs,
    controls
  ) |>
    dplyr::mutate(
      design_id = design_row$design_id,
      design_label = design_row$design_label,
      outcome = design_row$outcome,
      outcome_label = design_row$outcome_label,
      fixed_effects = design_row$fixed_effects,
      design_type = design_row$design_type,
      interaction_order = interaction_order,
      moderator_domain = design_row$moderator_domain,
      cluster_var = "pc_cluster_id",
      spec_key = paste(
        exposure_var,
        moderator_var,
        dplyr::coalesce(center_var, "no_center"),
        dplyr::coalesce(as.character(center_min_n), "no_center_n_rule"),
        control_set,
        sep = "__"
      ),
      reference_valid =
        is.finite(delta_fdi) & delta_fdi != 0 &
        is.finite(delta_moderator) & delta_moderator != 0 &
        is.finite(delta_center) & delta_center != 0,
      contrast_multiplier =
        delta_fdi * delta_moderator * delta_center,
      preferred_model_role = dplyr::case_when(
        interaction_order == "triple" &
          moderator_domain == "muslim" &
          dplyr::coalesce(fdi_preferred, FALSE) &
          dplyr::coalesce(moderator_preferred, FALSE) &
          dplyr::coalesce(center_preferred, FALSE) &
          center_min_n == 5L &
          dplyr::coalesce(control_preferred, FALSE) &
          design_id == "first_difference_muslim" ~
          "Preferred primary model",
        interaction_order == "triple" &
          moderator_domain == "muslim" &
          dplyr::coalesce(fdi_preferred, FALSE) &
          dplyr::coalesce(moderator_preferred, FALSE) &
          dplyr::coalesce(center_preferred, FALSE) &
          center_min_n == 5L &
          dplyr::coalesce(control_preferred, FALSE) &
          design_id == "lagged_outcome_muslim" ~
          "Preferred lagged-outcome alternative",
        TRUE ~
          "Specification-curve alternative"
      ),
      spec_preferred =
        preferred_model_role !=
        "Specification-curve alternative"
    )
  
  if (anyDuplicated(grid$spec_key)) {
    stop(
      "Specification keys are not unique for ",
      design_row$design_id, " / ", fdi_family, " / ", interaction_order
    )
  }
  
  grid
}

# ============================================================
# 7. MODEL-FITTING HELPERS
# ============================================================

interaction_term_name <- function(fit, variables) {
  coef_names <- names(stats::coef(fit))
  target <- paste(sort(variables), collapse = ":")
  
  normalized <- vapply(
    strsplit(coef_names, ":", fixed = TRUE),
    function(parts) {
      parts <- gsub("`", "", parts, fixed = TRUE)
      paste(sort(parts), collapse = ":")
    },
    character(1)
  )
  
  matches <- coef_names[normalized == target]
  
  if (length(matches) != 1) {
    return(NA_character_)
  }
  
  matches[[1]]
}

make_model_formula <- function(spec) {
  interaction_term <- if (spec$interaction_order == "two_way") {
    paste0(spec$exposure_var, " * ", spec$moderator_var)
  } else {
    paste0(
      spec$exposure_var, " * ", spec$moderator_var, " * ", spec$center_var
    )
  }
  
  always_controls <- character(0)
  
  if (spec$design_type %in% c("first_difference", "lagged_outcome")) {
    always_controls <- c(always_controls, spec$baseline_fdi_var)
  }
  
  if (spec$design_type == "lagged_outcome") {
    always_controls <- c(always_controls, "bjp_vote_share_2009")
  }
  
  rhs <- c(
    interaction_term,
    always_controls,
    spec$control_string
  )
  
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  
  stats::as.formula(
    paste0(
      spec$outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | ",
      spec$fixed_effects
    )
  )
}

extract_model_r2 <- function(fit) {
  value <- tryCatch(
    fixest::fitstat(fit, "r2")[[1]],
    error = function(e) NA_real_
  )
  suppressWarnings(as.numeric(value))
}

fit_one_specification <- function(spec, data) {
  formula <- make_model_formula(spec)
  formula_text <- paste(deparse(formula), collapse = " ")
  
  if (!isTRUE(spec$reference_valid)) {
    return(tibble::tibble(
      spec_key = spec$spec_key,
      fit_ok = FALSE,
      interaction_term = NA_character_,
      interaction_estimate = NA_real_,
      interaction_se = NA_real_,
      interaction_p = NA_real_,
      interaction_conf_low = NA_real_,
      interaction_conf_high = NA_real_,
      nobs = NA_integer_,
      r2 = NA_real_,
      formula = formula_text,
      error = "Invalid or zero-width substantive contrast reference"
    ))
  }
  
  fit_data <- data
  
  # Triple-interaction sample rules are based on the number of ideology-complete
  # NES respondents underlying the centrist-share estimate. Pooled models apply
  # the threshold to each AC-year; change models apply it to the 2009 baseline.
  if (
    spec$interaction_order == "triple" &&
    !is.na(spec$center_min_n) &&
    !is.na(spec$center_n_var)
  ) {
    center_n_var <- spec$center_n_var
    center_min_n <- as.integer(spec$center_min_n)
    
    fit_data <- fit_data |>
      dplyr::filter(
        !is.na(.data[[center_n_var]]),
        .data[[center_n_var]] >= center_min_n
      )
  }
  
  fit <- tryCatch(
    fixest::feols(
      formula,
      data = fit_data,
      vcov = stats::as.formula(paste0("~", spec$cluster_var)),
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(tibble::tibble(
      spec_key = spec$spec_key,
      fit_ok = FALSE,
      interaction_term = NA_character_,
      interaction_estimate = NA_real_,
      interaction_se = NA_real_,
      interaction_p = NA_real_,
      interaction_conf_low = NA_real_,
      interaction_conf_high = NA_real_,
      nobs = NA_integer_,
      r2 = NA_real_,
      formula = formula_text,
      error = conditionMessage(fit)
    ))
  }
  
  target_variables <- if (spec$interaction_order == "two_way") {
    c(spec$exposure_var, spec$moderator_var)
  } else {
    c(spec$exposure_var, spec$moderator_var, spec$center_var)
  }
  
  term <- interaction_term_name(fit, target_variables)
  
  if (is.na(term)) {
    return(tibble::tibble(
      spec_key = spec$spec_key,
      fit_ok = FALSE,
      interaction_term = NA_character_,
      interaction_estimate = NA_real_,
      interaction_se = NA_real_,
      interaction_p = NA_real_,
      interaction_conf_low = NA_real_,
      interaction_conf_high = NA_real_,
      nobs = stats::nobs(fit),
      r2 = extract_model_r2(fit),
      formula = formula_text,
      error = "Target interaction term was absent, usually because of collinearity"
    ))
  }
  
  estimate <- unname(stats::coef(fit)[term])
  standard_error <- unname(fixest::se(fit)[term])
  p_value <- unname(fixest::pvalue(fit)[term])
  
  ci <- tryCatch(
    stats::confint(fit, parm = term, level = CONFIDENCE_LEVEL),
    error = function(e) NULL
  )
  
  if (is.null(ci)) {
    z <- stats::qnorm(1 - (1 - CONFIDENCE_LEVEL) / 2)
    conf_low <- estimate - z * standard_error
    conf_high <- estimate + z * standard_error
  } else {
    conf_low <- as.numeric(ci[1, 1])
    conf_high <- as.numeric(ci[1, 2])
  }
  
  tibble::tibble(
    spec_key = spec$spec_key,
    fit_ok = TRUE,
    interaction_term = term,
    interaction_estimate = estimate,
    interaction_se = standard_error,
    interaction_p = p_value,
    interaction_conf_low = conf_low,
    interaction_conf_high = conf_high,
    nobs = stats::nobs(fit),
    r2 = extract_model_r2(fit),
    formula = formula_text,
    error = NA_character_
  )
}

run_curve <- function(design_row, fdi_family, interaction_order) {
  curve_id <- paste(
    design_row$design_id,
    fdi_family,
    interaction_order,
    RUN_MODE,
    SCRIPT_REVISION,
    sep = "__"
  )
  
  final_path <- file.path(spec_result_dir, paste0(curve_id, ".rds"))
  csv_path <- file.path(spec_result_dir, paste0(curve_id, ".csv"))
  partial_path <- file.path(spec_result_dir, paste0(curve_id, "__partial.rds"))
  
  grid <- make_curve_grid(design_row, fdi_family, interaction_order)
  data <- get_design_data(design_row)
  
  if (file.exists(final_path) && !OVERWRITE_EXISTING) {
    message("Loading cached curve: ", curve_id)
    return(readRDS(final_path))
  }
  
  existing <- tibble::tibble()
  if (file.exists(partial_path) && !OVERWRITE_EXISTING) {
    existing <- readRDS(partial_path)
    message(
      "Resuming ", curve_id, " from ", nrow(existing),
      " completed specifications."
    )
  }
  
  completed_keys <- if (nrow(existing) > 0) existing$spec_key else character(0)
  remaining <- grid |>
    dplyr::filter(!spec_key %in% completed_keys)
  
  message(
    "Running ", curve_id, ": ", nrow(remaining),
    " remaining of ", nrow(grid), " specifications."
  )
  
  new_results <- vector("list", nrow(remaining))
  
  if (nrow(remaining) > 0) {
    for (i in seq_len(nrow(remaining))) {
      spec <- remaining[i, , drop = FALSE]
      new_results[[i]] <- fit_one_specification(spec, data)
      
      if (i %% CHECKPOINT_EVERY == 0L || i == nrow(remaining)) {
        checkpoint <- dplyr::bind_rows(
          existing,
          dplyr::bind_rows(new_results[seq_len(i)])
        ) |>
          dplyr::distinct(spec_key, .keep_all = TRUE)
        
        saveRDS(checkpoint, partial_path)
        
        message(
          "  ", curve_id, ": ", nrow(checkpoint), "/", nrow(grid),
          " complete"
        )
      }
    }
  }
  
  fitted <- dplyr::bind_rows(
    existing,
    dplyr::bind_rows(new_results)
  ) |>
    dplyr::distinct(spec_key, .keep_all = TRUE)
  
  result <- grid |>
    dplyr::left_join(fitted, by = "spec_key") |>
    dplyr::mutate(
      contrast_estimate = interaction_estimate * contrast_multiplier,
      contrast_conf_low_raw = interaction_conf_low * contrast_multiplier,
      contrast_conf_high_raw = interaction_conf_high * contrast_multiplier,
      contrast_conf_low = pmin(
        contrast_conf_low_raw,
        contrast_conf_high_raw,
        na.rm = FALSE
      ),
      contrast_conf_high = pmax(
        contrast_conf_low_raw,
        contrast_conf_high_raw,
        na.rm = FALSE
      ),
      contrast_se = abs(contrast_multiplier) * interaction_se,
      contrast_positive = contrast_estimate > 0,
      ci_excludes_zero =
        contrast_conf_low > 0 | contrast_conf_high < 0,
      ci_positive = contrast_conf_low > 0,
      ci_negative = contrast_conf_high < 0
    )
  
  saveRDS(result, final_path)
  readr::write_csv(result, csv_path)
  
  if (file.exists(partial_path)) unlink(partial_path)
  
  result
}

# ============================================================
# 8. CURVE SUMMARIES
# ============================================================

summarize_curve <- function(results) {
  valid <- results |>
    dplyr::filter(fit_ok, is.finite(contrast_estimate))
  
  tibble::tibble(
    design_id = dplyr::first(results$design_id),
    fdi_family = dplyr::first(results$fdi_family),
    interaction_order = dplyr::first(results$interaction_order),
    requested_specs = nrow(results),
    fitted_specs = nrow(valid),
    failed_specs = sum(!results$fit_ok | !is.finite(results$contrast_estimate), na.rm = TRUE),
    median_contrast = if (nrow(valid) > 0) stats::median(valid$contrast_estimate) else NA_real_,
    q25_contrast = if (nrow(valid) > 0) stats::quantile(valid$contrast_estimate, 0.25) else NA_real_,
    q75_contrast = if (nrow(valid) > 0) stats::quantile(valid$contrast_estimate, 0.75) else NA_real_,
    share_positive = if (nrow(valid) > 0) mean(valid$contrast_estimate > 0, na.rm = TRUE) else NA_real_,
    share_ci_excludes_zero = if (nrow(valid) > 0) mean(valid$ci_excludes_zero, na.rm = TRUE) else NA_real_,
    share_ci_positive = if (nrow(valid) > 0) mean(valid$ci_positive, na.rm = TRUE) else NA_real_,
    share_ci_negative = if (nrow(valid) > 0) mean(valid$ci_negative, na.rm = TRUE) else NA_real_,
    min_n = if (nrow(valid) > 0) min(valid$nobs, na.rm = TRUE) else NA_real_,
    median_n = if (nrow(valid) > 0) stats::median(valid$nobs, na.rm = TRUE) else NA_real_,
    max_n = if (nrow(valid) > 0) max(valid$nobs, na.rm = TRUE) else NA_real_
  )
}


summarize_triple_by_center_rule <- function(results) {
  valid <- results |>
    dplyr::filter(
      interaction_order == "triple",
      fit_ok,
      is.finite(contrast_estimate)
    )
  
  if (nrow(valid) == 0) {
    return(tibble::tibble())
  }
  
  valid |>
    dplyr::summarise(
      fitted_specs = dplyr::n(),
      median_contrast =
        stats::median(contrast_estimate, na.rm = TRUE),
      q25_contrast =
        stats::quantile(contrast_estimate, 0.25, na.rm = TRUE),
      q75_contrast =
        stats::quantile(contrast_estimate, 0.75, na.rm = TRUE),
      share_positive =
        mean(contrast_estimate > 0, na.rm = TRUE),
      share_ci_excludes_zero =
        mean(ci_excludes_zero, na.rm = TRUE),
      median_n =
        stats::median(nobs, na.rm = TRUE),
      .by = c(
        design_id,
        fdi_family,
        center_weighting,
        center_min_n,
        center_sample_label,
        center_sample_role
      )
    )
}

# ============================================================
# 9. SPECIFICATION-CURVE PLOTTING
# ============================================================

placeholder_plot <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

prepare_curve_results <- function(results) {
  out <- results |>
    dplyr::filter(fit_ok, is.finite(contrast_estimate)) |>
    dplyr::arrange(contrast_estimate, spec_key) |>
    dplyr::mutate(curve_order = dplyr::row_number())
  
  if (nrow(out) == 0) {
    return(out |>
             dplyr::mutate(curve_percentile = numeric(0)))
  }
  
  # dplyr::if_else() is vectorised and requires its TRUE/FALSE branches to
  # conform to the size of the condition. Here dplyr::n() == 1L is a single
  # condition, while the multi-specification branch is a vector. Use ordinary
  # scalar `if` to choose which vector to return.
  out |>
    dplyr::mutate(
      curve_percentile = if (dplyr::n() == 1L) {
        rep(50, dplyr::n())
      } else {
        100 * (curve_order - 1) / (dplyr::n() - 1)
      }
    )
}

curve_domain_label <- function(results) {
  if (dplyr::first(results$moderator_domain) == "muslim") {
    "Muslim exposure"
  } else {
    "migration/compositional exposure"
  }
}

curve_y_label <- function(results) {
  domain <- curve_domain_label(results)
  order <- dplyr::first(results$interaction_order)
  design_type <- dplyr::first(results$design_type)
  
  if (order == "two_way") {
    if (design_type == "first_difference") {
      paste0(
        "Difference in FDI effect: high vs low ", domain,
        "\nChange in BJP vote share, 2009-2014 (pp)"
      )
    } else {
      paste0(
        "Difference in FDI effect: high vs low ", domain,
        "\nBJP vote share (pp)"
      )
    }
  } else {
    if (design_type == "first_difference") {
      paste0(
        "Increase in FDI x ", domain, " contrast",
        "\nfrom low- to high-centrist-share ACs (pp of BJP vote-share change)"
      )
    } else {
      paste0(
        "Increase in FDI x ", domain, " contrast",
        "\nfrom low- to high-centrist-share ACs (pp of BJP vote share)"
      )
    }
  }
}

curve_subtitle <- function(results) {
  if (dplyr::first(results$interaction_order) == "two_way") {
    paste0(
      "FDI moves from zero to the median positive exposure; ",
      "demographic exposure moves from its low to high reference value."
    )
  } else {
    paste0(
      "Difference between the FDI x demographic contrast at the 75th vs 25th ",
      "percentile of centrist share."
    )
  }
}

curve_summary_text <- function(results) {
  valid <- results |>
    dplyr::filter(fit_ok, is.finite(contrast_estimate))
  
  if (nrow(valid) == 0) {
    return("No estimable specifications")
  }
  
  paste0(
    format(nrow(valid), big.mark = ","), " fitted specifications",
    "  |  median contrast = ",
    sprintf("%.2f", stats::median(valid$contrast_estimate, na.rm = TRUE)),
    " pp",
    "  |  ",
    sprintf("%.0f", 100 * mean(valid$contrast_estimate > 0, na.rm = TRUE)),
    "% positive",
    "  |  ",
    sprintf("%.0f", 100 * mean(valid$ci_excludes_zero, na.rm = TRUE)),
    "% with 95% CI excluding zero",
    "  |  median N = ",
    format(round(stats::median(valid$nobs, na.rm = TRUE)), big.mark = ",")
  )
}

make_estimate_plot <- function(results) {
  plot_data <- prepare_curve_results(results)
  
  if (nrow(plot_data) == 0) {
    return(placeholder_plot("No estimable specifications"))
  }
  
  median_estimate <- stats::median(
    plot_data$contrast_estimate,
    na.rm = TRUE
  )
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = curve_percentile, y = contrast_estimate)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = median_estimate,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin = contrast_conf_low,
        ymax = contrast_conf_high
      ),
      linewidth = 0.12,
      alpha = 0.16
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = ci_excludes_zero),
      size = 0.65,
      stroke = 0.25
    ) +
    ggplot2::scale_shape_manual(
      values = c(`FALSE` = 1, `TRUE` = 16),
      breaks = c(FALSE, TRUE),
      labels = c(
        "95% CI includes zero",
        "95% CI excludes zero"
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100)
    ) +
    ggplot2::labs(
      subtitle = paste(
        curve_subtitle(results),
        curve_summary_text(results),
        sep = "\n"
      ),
      x = NULL,
      y = curve_y_label(results),
      shape = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 8.5)
    )
}

matrix_rows_for_curve <- function(plot_data) {
  domain <- dplyr::first(plot_data$moderator_domain)
  triple <- dplyr::first(plot_data$interaction_order) == "triple"
  
  rows <- dplyr::bind_rows(
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("FDI geography: ", fdi_scope_label),
        matrix_group = "FDI geography"
      ),
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("FDI status: ", fdi_status_label),
        matrix_group = "FDI status"
      ),
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("FDI form: ", fdi_form_label),
        matrix_group = "FDI form"
      ),
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("Moderator family: ", moderator_family),
        matrix_group = "Moderator family"
      ),
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("Moderator form: ", moderator_form),
        matrix_group = "Moderator form"
      ),
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row = paste0("Controls: ", control_set),
        matrix_group = "Controls"
      )
  )
  
  if (triple) {
    rows <- dplyr::bind_rows(
      rows,
      plot_data |>
        dplyr::transmute(
          curve_percentile,
          matrix_row = paste0("Center weighting: ", center_weighting),
          matrix_group = "Center weighting"
        ),
      plot_data |>
        dplyr::transmute(
          curve_percentile,
          matrix_row = paste0(
            "Minimum ideology-complete N: ",
            center_sample_label,
            dplyr::if_else(
              center_rule_primary,
              " [preferred]",
              ""
            )
          ),
          matrix_group = "Center sample rule"
        )
    )
  }
  
  geography_levels <- c(
    "FDI geography: Own AC",
    "FDI geography: Local: own + neighbors"
  )
  
  status_levels <- c(
    "FDI status: All announced/opened",
    "FDI status: Announced",
    "FDI status: Opened"
  )
  
  form_levels <- c(
    "FDI form: Project count",
    "FDI form: Projects per 100k",
    "FDI form: log1p projects per 100k"
  )
  
  moderator_family_levels <- if (domain == "muslim") {
    paste0(
      "Moderator family: ",
      c(
        "Muslim level: 2001",
        "Muslim level: 2011",
        "Muslim change: 2001-2011",
        "Hindu/Muslim relative composition"
      )
    )
  } else {
    paste0(
      "Moderator family: ",
      c(
        "All migrants: prior 5 years",
        "All migrants: prior 5-15 years",
        "All migrants: established stock",
        "Male migrants: prior 5 years",
        "Male migrants: prior 5-15 years",
        "Male migrants: established stock",
        "All migrants: prior5 vs baseline5 acceleration",
        "Male migrants: prior5 vs baseline5 acceleration",
        "Target Bengali/Bhojpuri exposure"
      )
    )
  }
  
  moderator_form_levels <- unique(
    paste0("Moderator form: ", plot_data$moderator_form)
  )
  
  control_levels <- paste0(
    "Controls: ",
    c("C0", "C1", "C2", "C3")
  )
  
  center_levels <- if (triple) {
    c(
      "Center weighting: Unweighted",
      "Center weighting: Survey weighted",
      "Minimum ideology-complete N: N >= 1",
      "Minimum ideology-complete N: N >= 5 [preferred]",
      "Minimum ideology-complete N: N >= 10"
    )
  } else {
    character(0)
  }
  
  row_levels <- c(
    geography_levels,
    status_levels,
    form_levels,
    moderator_family_levels,
    moderator_form_levels,
    center_levels,
    control_levels
  )
  
  row_levels <- row_levels[
    row_levels %in% rows$matrix_row
  ]
  
  rows |>
    dplyr::mutate(
      matrix_row = factor(
        matrix_row,
        levels = rev(row_levels)
      )
    )
}

make_matrix_plot <- function(results) {
  plot_data <- prepare_curve_results(results)
  
  if (nrow(plot_data) == 0) {
    return(placeholder_plot("No specification matrix"))
  }
  
  matrix_data <- matrix_rows_for_curve(plot_data)
  
  ggplot2::ggplot(
    matrix_data,
    ggplot2::aes(x = curve_percentile, y = matrix_row)
  ) +
    ggplot2::geom_point(size = 0.34, alpha = 0.72) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100")
    ) +
    ggplot2::labs(
      x = "Specification rank percentile (lowest to highest interaction contrast)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 8.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 7),
      axis.text.x = ggplot2::element_text(size = 7)
    )
}

curve_page_note <- function(results, design_row) {
  pieces <- character(0)
  
  if (design_row$moderator_domain == "muslim") {
    pieces <- c(
      pieces,
      paste0(
        "Hindu/Muslim-ratio contrasts are reoriented so that higher ",
        "substantive exposure always means greater Muslim exposure."
      )
    )
  }
  
  if (dplyr::first(results$interaction_order) == "triple") {
    pieces <- c(
      pieces,
      paste0(
        "Centrist share is calculated only among ideology-complete NES respondents; ",
        "weighted and unweighted versions are varied."
      )
    )
    
    if (design_row$design_type == "pooled") {
      pieces <- c(
        pieces,
        paste0(
          "The sample-rule dimension uses minimum ideology-complete N of ",
          "1, 5, and 10 within each AC-year. N >= 5 is the preferred ",
          "reliability rule; N >= 1 and N >= 10 are sensitivity analyses."
        )
      )
    } else {
      pieces <- c(
        pieces,
        paste0(
          "The moderator and sample threshold use 2009 NES composition. ",
          "N >= 5 is the preferred reliability rule, with N >= 1 as the ",
          "unrestricted sensitivity and N >= 10 as the stricter sensitivity."
        )
      )
    }
  }
  
  paste(pieces, collapse = " ")
}

wrap_curve_note <- function(x, width = 145L) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) {
    return("")
  }
  
  paste(
    base::strwrap(
      x,
      width = width,
      simplify = TRUE
    ),
    collapse = "\n"
  )
}

draw_curve_page <- function(results, design_row, fdi_label) {
  order_label <- if (
    dplyr::first(results$interaction_order) == "two_way"
  ) {
    "Two-way FDI x demographic interaction"
  } else {
    "Triple interaction with centrist share"
  }
  
  title <- paste0(
    design_row$design_label,
    " | ",
    fdi_label,
    " | ",
    order_label
  )
  
  note <- wrap_curve_note(
    curve_page_note(results, design_row),
    width = 145L
  )
  
  p_top <- make_estimate_plot(results)
  p_bottom <- make_matrix_plot(results)
  
  grid::grid.newpage()
  
  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 1,
    heights = grid::unit(
      c(0.15, 0.57, 0.28),
      "null"
    )
  )
  
  grid::pushViewport(
    grid::viewport(layout = layout)
  )
  
  grid::grid.text(
    title,
    vp = grid::viewport(
      layout.pos.row = 1,
      layout.pos.col = 1
    ),
    gp = grid::gpar(
      fontsize = 14,
      fontface = "bold"
    ),
    y = 0.82
  )
  
  if (nzchar(note)) {
    grid::grid.text(
      note,
      vp = grid::viewport(
        layout.pos.row = 1,
        layout.pos.col = 1
      ),
      gp = grid::gpar(
        fontsize = 7.5,
        lineheight = 1.05
      ),
      y = 0.28
    )
  }
  
  print(
    p_top,
    vp = grid::viewport(
      layout.pos.row = 2,
      layout.pos.col = 1
    )
  )
  
  print(
    p_bottom,
    vp = grid::viewport(
      layout.pos.row = 3,
      layout.pos.col = 1
    )
  )
  
  grid::popViewport()
}

save_combined_figure <- function(two_way, triple, design_row, fdi_family) {
  if (
    nrow(two_way) == 0 &&
    nrow(triple) == 0
  ) {
    return(invisible(NULL))
  }
  
  fdi_label <- fdi_meta |>
    dplyr::filter(
      .data$fdi_family == .env$fdi_family
    ) |>
    dplyr::pull(fdi_family_label) |>
    dplyr::first()
  
  file_name <- paste0(
    design_row$design_id,
    "__",
    fdi_family,
    "__",
    RUN_MODE,
    "__",
    SCRIPT_REVISION,
    "__specification_curves.pdf"
  )
  
  file_path <- file.path(
    spec_figure_dir,
    file_name
  )
  
  # Two pages per PDF: two-way curve first, triple-interaction curve second.
  grDevices::pdf(
    file_path,
    width = 12,
    height = 9,
    onefile = TRUE
  )
  
  if (nrow(two_way) > 0) {
    draw_curve_page(
      two_way,
      design_row,
      fdi_label
    )
  }
  
  if (nrow(triple) > 0) {
    draw_curve_page(
      triple,
      design_row,
      fdi_label
    )
  }
  
  grDevices::dev.off()
  
  invisible(file_path)
}

# ============================================================
# 10. NES CENTRIST-MODERATOR DIAGNOSTICS
# These run before the multiverse so the centrist-share sample can be audited
# before a full specification run.
# ============================================================

voters <- readRDS(
  file.path(paths$final_dir, "nes_respondent_analysis.rds")
)

weighted_ess <- function(w) {
  w <- as.numeric(w)
  w <- w[is.finite(w) & w > 0]
  
  if (length(w) == 0) {
    return(NA_real_)
  }
  
  sum(w)^2 / sum(w^2)
}

# ------------------------------------------------------------
# 10A. Effective weighted sample size by AC-year
# ------------------------------------------------------------

nes_ess_ac_year <- voters |>
  dplyr::filter(
    year %in% c(2009, 2014),
    !is.na(ac_uid)
  ) |>
  dplyr::summarise(
    weighted_ess_ideology_complete =
      weighted_ess(
        survey_weight_norm_year[!is.na(voter_ideology)]
      ),
    .by = c(ac_uid, year)
  )

# ------------------------------------------------------------
# 10B. Retained centrist-share measures in long form
# ------------------------------------------------------------

center_coverage_long <- ac_year |>
  dplyr::filter(year %in% c(2009, 2014)) |>
  dplyr::left_join(
    nes_ess_ac_year,
    by = c("ac_uid", "year")
  ) |>
  dplyr::select(
    state,
    state_no,
    pc_cluster_id,
    ac_uid,
    year,
    nes_n_ideology_complete,
    weighted_ess_ideology_complete,
    dplyr::all_of(center_meta$pooled_var)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(center_meta$pooled_var),
    names_to = "center_var",
    values_to = "center_share"
  ) |>
  dplyr::left_join(
    center_meta |>
      dplyr::select(
        pooled_var,
        center_weighting,
        center_denominator
      ),
    by = c("center_var" = "pooled_var")
  ) |>
  dplyr::mutate(
    raw_denominator_n = nes_n_ideology_complete,
    weighted_effective_n =
      weighted_ess_ideology_complete
  )

# ------------------------------------------------------------
# 10C. Overall coverage and denominator-size diagnostics
# ------------------------------------------------------------

center_coverage_summary <- center_coverage_long |>
  dplyr::summarise(
    total_ac_years = dplyr::n(),
    covered_ac_years =
      sum(!is.na(center_share)),
    coverage_share =
      mean(!is.na(center_share)),
    median_n =
      stats::median(
        raw_denominator_n[!is.na(center_share)],
        na.rm = TRUE
      ),
    p10_n =
      stats::quantile(
        raw_denominator_n[!is.na(center_share)],
        0.10,
        na.rm = TRUE
      ),
    p25_n =
      stats::quantile(
        raw_denominator_n[!is.na(center_share)],
        0.25,
        na.rm = TRUE
      ),
    p75_n =
      stats::quantile(
        raw_denominator_n[!is.na(center_share)],
        0.75,
        na.rm = TRUE
      ),
    p90_n =
      stats::quantile(
        raw_denominator_n[!is.na(center_share)],
        0.90,
        na.rm = TRUE
      ),
    share_n_lt5 =
      mean(
        raw_denominator_n[!is.na(center_share)] < 5,
        na.rm = TRUE
      ),
    share_n_lt10 =
      mean(
        raw_denominator_n[!is.na(center_share)] < 10,
        na.rm = TRUE
      ),
    share_n_lt20 =
      mean(
        raw_denominator_n[!is.na(center_share)] < 20,
        na.rm = TRUE
      ),
    median_weighted_ess =
      stats::median(
        weighted_effective_n[!is.na(center_share)],
        na.rm = TRUE
      ),
    .by = c(
      year,
      center_var,
      center_weighting
    )
  )

readr::write_csv(
  center_coverage_summary,
  file.path(
    spec_manifest_dir,
    "nes_center_coverage_summary.csv"
  )
)

# ------------------------------------------------------------
# 10D. State coverage rates and concentration
# This now reports both the state's share of the covered sample and the share
# of all ACs in that state for which a centrist estimate is available.
# ------------------------------------------------------------

state_ac_totals <- ac_year |>
  dplyr::filter(year %in% c(2009, 2014)) |>
  dplyr::summarise(
    total_acs = dplyr::n_distinct(ac_uid),
    .by = c(year, state_no, state)
  )

center_state_concentration <- center_coverage_long |>
  dplyr::summarise(
    covered_acs =
      dplyr::n_distinct(
        ac_uid[!is.na(center_share)]
      ),
    respondents =
      sum(
        raw_denominator_n[!is.na(center_share)],
        na.rm = TRUE
      ),
    .by = c(
      year,
      center_var,
      center_weighting,
      state_no,
      state
    )
  ) |>
  dplyr::left_join(
    state_ac_totals,
    by = c(
      "year",
      "state_no",
      "state"
    )
  ) |>
  dplyr::mutate(
    state_coverage_rate =
      covered_acs / total_acs
  ) |>
  dplyr::group_by(
    year,
    center_var
  ) |>
  dplyr::mutate(
    share_covered_acs =
      covered_acs / sum(covered_acs),
    share_respondents =
      respondents / sum(respondents)
  ) |>
  dplyr::ungroup()

readr::write_csv(
  center_state_concentration,
  file.path(
    spec_manifest_dir,
    "nes_center_state_concentration.csv"
  )
)

# ------------------------------------------------------------
# 10E. PC concentration
# ------------------------------------------------------------

center_pc_concentration <- center_coverage_long |>
  dplyr::filter(
    !is.na(center_share),
    !is.na(pc_cluster_id)
  ) |>
  dplyr::summarise(
    respondents =
      sum(
        raw_denominator_n,
        na.rm = TRUE
      ),
    covered_acs =
      dplyr::n_distinct(ac_uid),
    .by = c(
      year,
      center_var,
      center_weighting,
      pc_cluster_id
    )
  ) |>
  dplyr::group_by(
    year,
    center_var
  ) |>
  dplyr::mutate(
    respondent_share =
      respondents / sum(respondents)
  ) |>
  dplyr::ungroup()

pc_concentration_summary <-
  center_pc_concentration |>
  dplyr::arrange(
    year,
    center_var,
    dplyr::desc(respondent_share)
  ) |>
  dplyr::summarise(
    n_pcs = dplyr::n(),
    top5_pc_share =
      sum(
        head(
          sort(
            respondent_share,
            decreasing = TRUE
          ),
          5
        )
      ),
    top10_pc_share =
      sum(
        head(
          sort(
            respondent_share,
            decreasing = TRUE
          ),
          10
        )
      ),
    hhi =
      sum(respondent_share^2),
    effective_n_pcs =
      1 / hhi,
    .by = c(
      year,
      center_var,
      center_weighting
    )
  )

readr::write_csv(
  pc_concentration_summary,
  file.path(
    spec_manifest_dir,
    "nes_center_pc_concentration_summary.csv"
  )
)

# ------------------------------------------------------------
# 10F. Are low-N centrist estimates extreme?
# ------------------------------------------------------------

center_share_by_n_plot <-
  center_coverage_long |>
  dplyr::filter(
    !is.na(center_share),
    !is.na(raw_denominator_n)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = raw_denominator_n,
      y = center_share
    )
  ) +
  ggplot2::geom_point(
    alpha = 0.35,
    size = 0.8
  ) +
  ggplot2::facet_grid(
    year ~ center_weighting,
    scales = "free_x"
  ) +
  ggplot2::labs(
    title =
      "Centrist-share estimates versus underlying NES sample size",
    subtitle =
      "Only ideology-complete centrist-share measures are retained.",
    x =
      "Ideology-complete respondents underlying AC-year centrist-share estimate",
    y =
      "Estimated centrist share"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  file.path(
    spec_figure_dir,
    "nes_center_share_vs_sample_size.pdf"
  ),
  center_share_by_n_plot,
  width = 11,
  height = 7
)

# ------------------------------------------------------------
# 10G. Threshold-specific NES-covered versus uncovered balance audits
#
# We audit coverage separately for N >= 1, N >= 5, and N >= 10.
# The pooled audit applies the threshold within each AC-year.
# The change-design audit applies the threshold to the 2009 NES denominator,
# matching the first-difference and lagged-outcome triple-interaction designs.
#
# Standardized mean difference is covered minus uncovered, divided by the
# pooled within-group standard deviation.
# ------------------------------------------------------------

standardized_balance_from_long <- function(balance_long) {
  balance_long |>
    tidyr::pivot_wider(
      names_from = coverage_group,
      values_from = c(
        n_nonmissing,
        mean,
        median,
        sd
      ),
      names_glue = "{.value}_{coverage_group}"
    ) |>
    dplyr::mutate(
      pooled_sd = sqrt(
        (
          sd_covered^2 +
            sd_uncovered^2
        ) / 2
      ),
      standardized_mean_difference =
        dplyr::if_else(
          is.finite(pooled_sd) &
            pooled_sd > 0,
          (mean_covered - mean_uncovered) /
            pooled_sd,
          NA_real_
        ),
      abs_standardized_mean_difference =
        abs(standardized_mean_difference)
    )
}

# ------------------------------------------------------------
# 10G.1 Pooled AC-year balance by threshold
# ------------------------------------------------------------

coverage_balance_vars <- intersect(
  c(
    "bjp_vote_share",
    "log1p_fdi_total_local_all_pc100k",
    "muslim_share_2001_dist_proxy",
    "muslim_share_2011_dist_proxy",
    "mig_prior_5yr_share_ac_pop",
    "mig_prior_5_15yr_share_ac_pop",
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    "log_secc_cons_pc"
  ),
  names(ac_year)
)

coverage_balance_long <- purrr::map_dfr(
  center_meta$pooled_var,
  function(center_var_current) {
    purrr::map_dfr(
      center_sample_rules$center_min_n,
      function(center_min_n_current) {
        ac_year |>
          dplyr::filter(
            year %in% c(2009, 2014)
          ) |>
          dplyr::mutate(
            center_var = center_var_current,
            center_min_n = as.integer(center_min_n_current),
            center_sample_label = paste0(
              "N >= ",
              center_min_n_current
            ),
            coverage_group = dplyr::if_else(
              !is.na(.data[[center_var_current]]) &
                !is.na(nes_n_ideology_complete) &
                nes_n_ideology_complete >= center_min_n_current,
              "covered",
              "uncovered"
            )
          ) |>
          dplyr::select(
            year,
            center_var,
            center_min_n,
            center_sample_label,
            coverage_group,
            dplyr::all_of(coverage_balance_vars)
          ) |>
          tidyr::pivot_longer(
            cols = dplyr::all_of(coverage_balance_vars),
            names_to = "variable",
            values_to = "value"
          ) |>
          dplyr::summarise(
            n_nonmissing = sum(is.finite(value)),
            mean = safe_mean(value),
            median = safe_median(value),
            sd = safe_sd(value),
            .by = c(
              year,
              center_var,
              center_min_n,
              center_sample_label,
              variable,
              coverage_group
            )
          )
      }
    )
  }
)

coverage_balance <- standardized_balance_from_long(
  coverage_balance_long
)

readr::write_csv(
  coverage_balance,
  file.path(
    spec_manifest_dir,
    "nes_center_covered_vs_uncovered_balance_by_threshold.csv"
  )
)

coverage_balance_plot <- coverage_balance |>
  dplyr::filter(
    is.finite(standardized_mean_difference)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = standardized_mean_difference,
      y = stats::reorder(
        variable,
        standardized_mean_difference
      )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(
    interaction(
      year,
      center_sample_label,
      sep = " | "
    ) ~ center_var,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "NES-covered versus uncovered AC-years by minimum ideology-complete N",
    subtitle =
      paste0(
        "Positive standardized differences indicate higher values in covered AC-years. ",
        "Coverage is defined separately for N >= 1, N >= 5, and N >= 10."
      ),
    x =
      "Standardized mean difference: covered - uncovered",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 8)

ggplot2::ggsave(
  file.path(
    spec_figure_dir,
    "nes_center_covered_vs_uncovered_balance_by_threshold.pdf"
  ),
  coverage_balance_plot,
  width = 13,
  height = 15
)

# ------------------------------------------------------------
# 10G.2 Change-design balance by 2009 threshold
# ------------------------------------------------------------

change_balance_vars <- intersect(
  c(
    "d_bjp_vote_share_2009_2014_pp",
    "bjp_vote_share_2009",
    "bjp_vote_share_2014",
    "log1p_fdi_total_local_all_pc100k_2009",
    "log1p_fdi_total_local_all_pc100k_2014",
    "muslim_share_2001_dist_proxy",
    "muslim_share_2011_dist_proxy",
    "d_muslim_share_2001_2011_pp",
    "mig_prior_5yr_share_ac_pop_2014",
    "mig_prior_5_15yr_share_ac_pop_2014",
    "mig_total_upto_2001_share_ac_pop",
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    "log_secc_cons_pc"
  ),
  names(ac_change)
)

change_coverage_balance_long <- purrr::map_dfr(
  center_meta$change_var,
  function(center_var_current) {
    purrr::map_dfr(
      center_sample_rules$center_min_n,
      function(center_min_n_current) {
        ac_change |>
          dplyr::mutate(
            center_var = center_var_current,
            center_min_n = as.integer(center_min_n_current),
            center_sample_label = paste0(
              "N >= ",
              center_min_n_current
            ),
            coverage_group = dplyr::if_else(
              !is.na(.data[[center_var_current]]) &
                !is.na(nes_n_ideology_complete_2009) &
                nes_n_ideology_complete_2009 >= center_min_n_current,
              "covered",
              "uncovered"
            )
          ) |>
          dplyr::select(
            center_var,
            center_min_n,
            center_sample_label,
            coverage_group,
            dplyr::all_of(change_balance_vars)
          ) |>
          tidyr::pivot_longer(
            cols = dplyr::all_of(change_balance_vars),
            names_to = "variable",
            values_to = "value"
          ) |>
          dplyr::summarise(
            n_nonmissing = sum(is.finite(value)),
            mean = safe_mean(value),
            median = safe_median(value),
            sd = safe_sd(value),
            .by = c(
              center_var,
              center_min_n,
              center_sample_label,
              variable,
              coverage_group
            )
          )
      }
    )
  }
)

change_coverage_balance <- standardized_balance_from_long(
  change_coverage_balance_long
)

readr::write_csv(
  change_coverage_balance,
  file.path(
    spec_manifest_dir,
    "nes_center_change_design_balance_by_threshold.csv"
  )
)

change_coverage_balance_plot <- change_coverage_balance |>
  dplyr::filter(
    is.finite(standardized_mean_difference)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = standardized_mean_difference,
      y = stats::reorder(
        variable,
        standardized_mean_difference
      )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(
    center_sample_label ~ center_var,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Change-design NES coverage balance by 2009 ideology-complete N threshold",
    subtitle =
      paste0(
        "Coverage uses the 2009 centrist-share measure and 2009 NES denominator, ",
        "matching the first-difference and lagged-outcome triple-interaction designs."
      ),
    x =
      "Standardized mean difference: covered - uncovered",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 8)

ggplot2::ggsave(
  file.path(
    spec_figure_dir,
    "nes_center_change_design_balance_by_threshold.pdf"
  ),
  change_coverage_balance_plot,
  width = 13,
  height = 11
)

# ============================================================
# 11. THEORY-PREFERRED SPECIFICATION MANIFEST
# The preferred model is chosen on theoretical/temporal grounds, not on the
# largest coefficient in the multiverse. The broad specification universe is
# retained unchanged apart from repairing the unusable employment control.
# ============================================================

preferred_specification <- tibble::tribble(
  ~dimension, ~preferred_choice, ~role_or_rationale,
  "FDI family", "Manufacturing", "Economic-disruption mechanism",
  "FDI geography", "Local: own + neighbors", "Local economic exposure can cross AC boundaries",
  "FDI status", "All announced/opened", "Captures realized and anticipated foreign-investment exposure",
  "FDI form", "log1p projects per 100k", "Population-scaled exposure with reduced leverage from extreme values",
  "Muslim moderator", "2001 Muslim population share", "Pre-treatment, pre-existing demographic context",
  "Centrist moderator", "Survey weighted, ideology-complete", "Preferred NES composition measure",
  "NES reliability", "N >= 5", "Preferred reliability/coverage tradeoff",
  "Temporal design", "First difference", "Primary 2009-2014 change design",
  "Alternative temporal design", "Lagged outcome", "Key corroborating change design",
  "Control set", "C1", "Preferred baseline adjustment; C2/C3 are expanded socioeconomic robustness"
)

readr::write_csv(
  preferred_specification,
  file.path(
    spec_manifest_dir,
    "preferred_specification.csv"
  )
)

# ============================================================
# 11. WRITE SPECIFICATION MANIFESTS AND PLANNED COUNTS
# ============================================================

readr::write_csv(fdi_meta, file.path(spec_manifest_dir, "fdi_specifications.csv"))
readr::write_csv(muslim_meta, file.path(spec_manifest_dir, "muslim_moderators.csv"))
readr::write_csv(migration_meta, file.path(spec_manifest_dir, "migration_moderators.csv"))
readr::write_csv(center_meta, file.path(spec_manifest_dir, "center_moderators.csv"))
readr::write_csv(
  center_sample_rules,
  file.path(spec_manifest_dir, "center_sample_rules.csv")
)
readr::write_csv(control_sets, file.path(spec_manifest_dir, "control_sets.csv"))
readr::write_csv(design_meta, file.path(spec_manifest_dir, "designs.csv"))

curve_plan <- tidyr::crossing(
  design_meta |>
    dplyr::filter(design_id %in% RUN_DESIGNS),
  fdi_family = RUN_FDI_FAMILIES,
  interaction_order = RUN_INTERACTION_ORDERS
) |>
  dplyr::mutate(
    n_fdi = if (RUN_MODE == "pilot") 1L else 18L,
    n_moderators = if (RUN_MODE == "pilot") {
      PILOT_MODERATORS_PER_DOMAIN
    } else {
      dplyr::if_else(
        moderator_domain == "muslim",
        nrow(muslim_meta),
        nrow(migration_meta)
      )
    },
    n_controls = if (RUN_MODE == "pilot") {
      length(PILOT_CONTROL_SETS)
    } else {
      nrow(control_sets)
    },
    n_center = dplyr::if_else(
      interaction_order == "triple",
      if (RUN_MODE == "pilot") 1L else nrow(center_meta),
      1L
    ),
    n_center_sample_rules = dplyr::if_else(
      interaction_order == "triple",
      if (RUN_MODE == "pilot") 1L else nrow(center_sample_rules),
      1L
    ),
    requested_specs =
      n_fdi * n_moderators * n_controls *
      n_center * n_center_sample_rules
  )

readr::write_csv(
  curve_plan,
  file.path(spec_manifest_dir, paste0("curve_plan_", RUN_MODE, ".csv"))
)

message(
  "Planned regressions in ", RUN_MODE, " mode: ",
  format(sum(curve_plan$requested_specs), big.mark = ",")
)

# ============================================================
# 12. RUN ALL REQUESTED CURVES
# ============================================================

all_summaries <- list()
summary_index <- 0L

all_center_rule_summaries <- list()
center_rule_summary_index <- 0L

for (design_id in RUN_DESIGNS) {
  design_row <- design_meta |>
    dplyr::filter(.data$design_id == .env$design_id)
  
  if (nrow(design_row) != 1) {
    stop("Unknown or duplicated design id: ", design_id)
  }
  
  for (fdi_family in RUN_FDI_FAMILIES) {
    curve_results <- list()
    
    for (interaction_order in RUN_INTERACTION_ORDERS) {
      results <- run_curve(
        design_row = design_row,
        fdi_family = fdi_family,
        interaction_order = interaction_order
      )
      
      curve_results[[interaction_order]] <- results
      
      summary_index <- summary_index + 1L
      all_summaries[[summary_index]] <- summarize_curve(results)
      
      # Keep a separate diagnostic summary of triple-interaction results by
      # centrist-share weighting and minimum ideology-complete NES N rule.
      if (interaction_order == "triple") {
        center_rule_summary_index <- center_rule_summary_index + 1L
        all_center_rule_summaries[[center_rule_summary_index]] <-
          summarize_triple_by_center_rule(results)
      }
      
      failures <- results
      failures <- results |>
        dplyr::filter(!fit_ok | !is.finite(contrast_estimate)) |>
        dplyr::select(
          design_id, fdi_family, interaction_order, spec_key,
          exposure_var, moderator_var, center_var,
          center_min_n, center_sample_label, center_weighting,
          control_set, nobs, formula, error
        )
      
      if (nrow(failures) > 0) {
        readr::write_csv(
          failures,
          file.path(
            spec_log_dir,
            paste0(
              design_id, "__", fdi_family, "__", interaction_order,
              "__", RUN_MODE, "__failures.csv"
            )
          )
        )
      }
    }
    
    if (all(c("two_way", "triple") %in% names(curve_results))) {
      save_combined_figure(
        two_way = curve_results$two_way,
        triple = curve_results$triple,
        design_row = design_row,
        fdi_family = fdi_family
      )
    }
  }
}

curve_summaries <- dplyr::bind_rows(all_summaries)

readr::write_csv(
  curve_summaries,
  file.path(spec_summary_dir, paste0("curve_summaries_", RUN_MODE, ".csv"))
)

center_rule_summaries <- dplyr::bind_rows(
  all_center_rule_summaries
)

readr::write_csv(
  center_rule_summaries,
  file.path(
    spec_summary_dir,
    paste0(
      "triple_center_rule_summaries_",
      RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 13. THEORY-PREFERRED MODEL ROBUSTNESS
# These targeted models do NOT add new dimensions to the multiverse. They make
# the theory-first benchmark easier to interpret and distinguish changes due to
# controls from changes due to sample composition.
# ============================================================

PREFERRED_FDI_VAR <-
  "log1p_fdi_mfg_local_all_pc100k_2014"
PREFERRED_BASELINE_FDI_VAR <-
  "log1p_fdi_mfg_local_all_pc100k_2009"
PREFERRED_MUSLIM_VAR <-
  "muslim_share_2001_dist_proxy"
PREFERRED_CENTER_VAR <-
  "nes_weighted_share_center_among_ideology_complete_2009"
PREFERRED_CENTER_N_VAR <-
  "nes_n_ideology_complete_2009"
PREFERRED_CENTER_MIN_N <- 5L
PREFERRED_CLUSTER_VAR <- "pc_cluster_id"
DELHI_STATE_NO <- 7

preferred_design_rows <- design_meta |>
  dplyr::filter(
    design_id %in% c(
      "first_difference_muslim",
      "lagged_outcome_muslim"
    )
  )

preferred_control_string <- function(
    control_set_name
) {
  value <- control_sets |>
    dplyr::filter(
      control_set == control_set_name
    ) |>
    dplyr::pull(control_string)
  
  if (length(value) != 1) {
    stop(
      "Could not uniquely resolve preferred-model control set: ",
      control_set_name
    )
  }
  
  value[[1]]
}

raw_employment_control_string <- function(
    control_set_name
) {
  if (!control_set_name %in% c("C2", "C3")) {
    stop(
      "Raw-employment sensitivity is defined only for C2 and C3."
    )
  }
  
  sub(
    "log1p_employment_per_total_population",
    "employment_per_total_population",
    preferred_control_string(
      control_set_name
    ),
    fixed = TRUE
  )
}

make_preferred_formula <- function(
    design_row,
    control_string
) {
  always_controls <- c(
    PREFERRED_BASELINE_FDI_VAR
  )
  
  if (
    design_row$design_type ==
    "lagged_outcome"
  ) {
    always_controls <- c(
      always_controls,
      "bjp_vote_share_2009"
    )
  }
  
  rhs <- c(
    paste0(
      PREFERRED_FDI_VAR,
      " * ",
      PREFERRED_MUSLIM_VAR,
      " * ",
      PREFERRED_CENTER_VAR
    ),
    always_controls,
    control_string
  )
  
  stats::as.formula(
    paste0(
      design_row$outcome,
      " ~ ",
      paste(
        rhs,
        collapse = " + "
      ),
      " | ",
      design_row$fixed_effects
    )
  )
}

preferred_base_sample <- function(
    exclude_delhi = FALSE
) {
  data <- ac_change |>
    dplyr::filter(
      !is.na(
        .data[[
          PREFERRED_CENTER_N_VAR
        ]]
      ),
      .data[[
        PREFERRED_CENTER_N_VAR
      ]] >=
        PREFERRED_CENTER_MIN_N
    )
  
  if (exclude_delhi) {
    data <- data |>
      dplyr::filter(
        is.na(state_no) |
          state_no !=
          DELHI_STATE_NO
      )
  }
  
  data
}

preferred_model_required_vars <- function(
    design_row,
    control_string
) {
  vars <- c(
    design_row$outcome,
    PREFERRED_FDI_VAR,
    PREFERRED_BASELINE_FDI_VAR,
    PREFERRED_MUSLIM_VAR,
    PREFERRED_CENTER_VAR,
    PREFERRED_CENTER_N_VAR,
    "state_no",
    PREFERRED_CLUSTER_VAR,
    control_vars_from_string(
      control_string
    )
  )
  
  if (
    design_row$design_type ==
    "lagged_outcome"
  ) {
    vars <- c(
      vars,
      "bjp_vote_share_2009"
    )
  }
  
  unique(vars)
}

preferred_reference_values <- function(
    design_row
) {
  data <- preferred_base_sample(
    exclude_delhi = FALSE
  ) |>
    dplyr::filter(
      is.finite(
        .data[[
          design_row$outcome
        ]]
      ),
      !is.na(
        nes_share_center_among_ideology_complete_2009
      )
    )
  
  # Match the specification-curve reference-value logic: references are fixed
  # on the outcome-valid N >= 5 centrist-analysis support, then each variable's
  # finite values determine its own reference points. Do not change references
  # when controls, Delhi inclusion, or employment transformation changes.
  fdi_ref <- fdi_reference(
    data[[
      PREFERRED_FDI_VAR
    ]]
  )
  moderator_ref <- moderator_reference(
    data[[
      PREFERRED_MUSLIM_VAR
    ]],
    orientation = 1
  )
  center_ref <- center_reference(
    data[[
      PREFERRED_CENTER_VAR
    ]]
  )
  
  tibble::tibble(
    design_id = design_row$design_id,
    fdi_low = fdi_ref$low,
    fdi_high = fdi_ref$high,
    delta_fdi = fdi_ref$delta,
    muslim_low =
      moderator_ref$low_exposure,
    muslim_high =
      moderator_ref$high_exposure,
    delta_muslim =
      moderator_ref$delta,
    center_low =
      center_ref$low_center,
    center_high =
      center_ref$high_center,
    delta_center =
      center_ref$delta_center,
    fdi_reference_method =
      fdi_ref$reference_method,
    muslim_reference_method =
      moderator_ref$reference_method,
    center_reference_method =
      center_ref$reference_method
  )
}

extract_preferred_fit_summary <- function(
    fit,
    design_row,
    reference_values,
    robustness_dimension,
    robustness_variant,
    control_set,
    sample_rule,
    employment_form,
    exclude_delhi
) {
  formula_text <- paste(
    deparse(
      stats::formula(fit)
    ),
    collapse = " "
  )
  
  triple_term <- interaction_term_name(
    fit,
    c(
      PREFERRED_FDI_VAR,
      PREFERRED_MUSLIM_VAR,
      PREFERRED_CENTER_VAR
    )
  )
  
  if (is.na(triple_term)) {
    return(
      tibble::tibble(
        design_id =
          design_row$design_id,
        design_label =
          design_row$design_label,
        robustness_dimension =
          robustness_dimension,
        robustness_variant =
          robustness_variant,
        control_set =
          control_set,
        sample_rule =
          sample_rule,
        employment_form =
          employment_form,
        exclude_delhi =
          exclude_delhi,
        fit_ok = FALSE,
        triple_term = NA_character_,
        triple_estimate = NA_real_,
        triple_se = NA_real_,
        triple_p = NA_real_,
        triple_conf_low = NA_real_,
        triple_conf_high = NA_real_,
        substantive_contrast = NA_real_,
        substantive_conf_low = NA_real_,
        substantive_conf_high = NA_real_,
        nobs = stats::nobs(fit),
        r2 = extract_model_r2(fit),
        formula = formula_text,
        error =
          "Preferred triple-interaction term absent"
      )
    )
  }
  
  estimate <- unname(
    stats::coef(fit)[
      triple_term
    ]
  )
  standard_error <- unname(
    fixest::se(fit)[
      triple_term
    ]
  )
  p_value <- unname(
    fixest::pvalue(fit)[
      triple_term
    ]
  )
  
  ci <- tryCatch(
    stats::confint(
      fit,
      parm = triple_term,
      level = CONFIDENCE_LEVEL
    ),
    error = function(e) NULL
  )
  
  if (is.null(ci)) {
    z <- stats::qnorm(
      1 -
        (1 - CONFIDENCE_LEVEL) /
        2
    )
    conf_low <-
      estimate -
      z * standard_error
    conf_high <-
      estimate +
      z * standard_error
  } else {
    conf_low <-
      as.numeric(ci[1, 1])
    conf_high <-
      as.numeric(ci[1, 2])
  }
  
  multiplier <-
    reference_values$delta_fdi *
    reference_values$delta_muslim *
    reference_values$delta_center
  
  contrast <-
    estimate *
    multiplier
  contrast_low_raw <-
    conf_low *
    multiplier
  contrast_high_raw <-
    conf_high *
    multiplier
  
  tibble::tibble(
    design_id =
      design_row$design_id,
    design_label =
      design_row$design_label,
    robustness_dimension =
      robustness_dimension,
    robustness_variant =
      robustness_variant,
    control_set =
      control_set,
    sample_rule =
      sample_rule,
    employment_form =
      employment_form,
    exclude_delhi =
      exclude_delhi,
    fit_ok = TRUE,
    triple_term =
      triple_term,
    triple_estimate =
      estimate,
    triple_se =
      standard_error,
    triple_p =
      p_value,
    triple_conf_low =
      conf_low,
    triple_conf_high =
      conf_high,
    substantive_contrast =
      contrast,
    substantive_conf_low =
      pmin(
        contrast_low_raw,
        contrast_high_raw
      ),
    substantive_conf_high =
      pmax(
        contrast_low_raw,
        contrast_high_raw
      ),
    nobs = stats::nobs(fit),
    r2 = extract_model_r2(fit),
    formula = formula_text,
    error = NA_character_
  )
}

fit_preferred_variant <- function(
    design_row,
    reference_values,
    robustness_dimension,
    robustness_variant,
    control_set,
    control_string,
    common_c3_sample = FALSE,
    exclude_delhi = FALSE,
    employment_form =
      "Logged employment intensity"
) {
  data <- preferred_base_sample(
    exclude_delhi =
      exclude_delhi
  )
  
  if (common_c3_sample) {
    c3_vars <-
      preferred_model_required_vars(
        design_row,
        preferred_control_string(
          "C3"
        )
      )
    
    data <- data[
      stats::complete.cases(
        data[
          ,
          c3_vars,
          drop = FALSE
        ]
      ),
      ,
      drop = FALSE
    ]
  }
  
  formula <- make_preferred_formula(
    design_row,
    control_string
  )
  
  fit <- tryCatch(
    fixest::feols(
      formula,
      data = data,
      vcov =
        stats::as.formula(
          paste0(
            "~",
            PREFERRED_CLUSTER_VAR
          )
        ),
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) e
  )
  
  sample_rule <- if (
    common_c3_sample
  ) {
    "Common C3-complete sample"
  } else if (
    exclude_delhi
  ) {
    "Available cases, Delhi excluded"
  } else {
    "Available cases"
  }
  
  if (inherits(fit, "error")) {
    return(
      tibble::tibble(
        design_id =
          design_row$design_id,
        design_label =
          design_row$design_label,
        robustness_dimension =
          robustness_dimension,
        robustness_variant =
          robustness_variant,
        control_set =
          control_set,
        sample_rule =
          sample_rule,
        employment_form =
          employment_form,
        exclude_delhi =
          exclude_delhi,
        fit_ok = FALSE,
        triple_term =
          NA_character_,
        triple_estimate =
          NA_real_,
        triple_se =
          NA_real_,
        triple_p =
          NA_real_,
        triple_conf_low =
          NA_real_,
        triple_conf_high =
          NA_real_,
        substantive_contrast =
          NA_real_,
        substantive_conf_low =
          NA_real_,
        substantive_conf_high =
          NA_real_,
        nobs = NA_integer_,
        r2 = NA_real_,
        formula = paste(
          deparse(formula),
          collapse = " "
        ),
        error =
          conditionMessage(fit)
      )
    )
  }
  
  extract_preferred_fit_summary(
    fit = fit,
    design_row = design_row,
    reference_values =
      reference_values,
    robustness_dimension =
      robustness_dimension,
    robustness_variant =
      robustness_variant,
    control_set = control_set,
    sample_rule = sample_rule,
    employment_form =
      employment_form,
    exclude_delhi =
      exclude_delhi
  )
}

preferred_robustness_results <- list()
preferred_robustness_index <- 0L
preferred_reference_rows <- list()
preferred_reference_index <- 0L

for (
  i in seq_len(
    nrow(preferred_design_rows)
  )
) {
  design_row <-
    preferred_design_rows[
      i,
      ,
      drop = FALSE
    ]
  
  refs <- preferred_reference_values(
    design_row
  )
  
  preferred_reference_index <-
    preferred_reference_index +
    1L
  preferred_reference_rows[[
    preferred_reference_index
  ]] <- refs
  
  # A. Normal available-case C0-C3 comparison.
  for (
    control_set_name in
    control_sets$control_set
  ) {
    preferred_robustness_index <-
      preferred_robustness_index +
      1L
    
    preferred_robustness_results[[
      preferred_robustness_index
    ]] <- fit_preferred_variant(
      design_row = design_row,
      reference_values = refs,
      robustness_dimension =
        "Control-set comparison",
      robustness_variant =
        paste0(
          control_set_name,
          " available-case"
        ),
      control_set =
        control_set_name,
      control_string =
        preferred_control_string(
          control_set_name
        ),
      common_c3_sample = FALSE,
      exclude_delhi = FALSE,
      employment_form =
        if (
          control_set_name %in%
          c("C2", "C3")
        ) {
          "Logged employment intensity"
        } else {
          "Not included"
        }
    )
  }
  
  # B. C0-C3 on the exact same C3-complete observations.
  for (
    control_set_name in
    control_sets$control_set
  ) {
    preferred_robustness_index <-
      preferred_robustness_index +
      1L
    
    preferred_robustness_results[[
      preferred_robustness_index
    ]] <- fit_preferred_variant(
      design_row = design_row,
      reference_values = refs,
      robustness_dimension =
        "Common-sample control comparison",
      robustness_variant =
        paste0(
          control_set_name,
          " on C3-complete sample"
        ),
      control_set =
        control_set_name,
      control_string =
        preferred_control_string(
          control_set_name
        ),
      common_c3_sample = TRUE,
      exclude_delhi = FALSE,
      employment_form =
        if (
          control_set_name %in%
          c("C2", "C3")
        ) {
          "Logged employment intensity"
        } else {
          "Not included"
        }
    )
  }
  
  # C. Preferred C1 model excluding Delhi.
  preferred_robustness_index <-
    preferred_robustness_index +
    1L
  
  preferred_robustness_results[[
    preferred_robustness_index
  ]] <- fit_preferred_variant(
    design_row = design_row,
    reference_values = refs,
    robustness_dimension =
      "Delhi sensitivity",
    robustness_variant =
      "C1 excluding Delhi",
    control_set = "C1",
    control_string =
      preferred_control_string(
        "C1"
      ),
    common_c3_sample = FALSE,
    exclude_delhi = TRUE,
    employment_form =
      "Not included"
  )
  
  # D. Raw versus logged employment intensity in the expanded controls.
  for (
    control_set_name in
    c("C2", "C3")
  ) {
    preferred_robustness_index <-
      preferred_robustness_index +
      1L
    
    preferred_robustness_results[[
      preferred_robustness_index
    ]] <- fit_preferred_variant(
      design_row = design_row,
      reference_values = refs,
      robustness_dimension =
        "Employment-form sensitivity",
      robustness_variant =
        paste0(
          control_set_name,
          " with raw employment intensity"
        ),
      control_set =
        control_set_name,
      control_string =
        raw_employment_control_string(
          control_set_name
        ),
      common_c3_sample = FALSE,
      exclude_delhi = FALSE,
      employment_form =
        "Raw employment intensity"
    )
  }
}

preferred_robustness <-
  dplyr::bind_rows(
    preferred_robustness_results
  )

preferred_references <-
  dplyr::bind_rows(
    preferred_reference_rows
  )

readr::write_csv(
  preferred_references,
  file.path(
    spec_summary_dir,
    paste0(
      "preferred_model_reference_values_",
      RUN_MODE,
      ".csv"
    )
  )
)

readr::write_csv(
  preferred_robustness,
  file.path(
    spec_summary_dir,
    paste0(
      "preferred_model_robustness_",
      RUN_MODE,
      ".csv"
    )
  )
)

preferred_robustness_plot <-
  preferred_robustness |>
  dplyr::filter(
    fit_ok,
    is.finite(
      substantive_contrast
    )
  ) |>
  dplyr::mutate(
    plot_label = paste0(
      robustness_dimension,
      ": ",
      robustness_variant
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = stats::reorder(
        plot_label,
        substantive_contrast
      ),
      y = substantive_contrast
    )
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = substantive_conf_low,
      ymax = substantive_conf_high
    ),
    width = 0.15
  ) +
  ggplot2::geom_point(
    size = 1.5
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(
    ~design_label
  ) +
  ggplot2::labs(
    title =
      "Theory-preferred manufacturing-FDI triple interaction: targeted robustness",
    subtitle =
      paste0(
        "Manufacturing FDI = local, all announced/opened, log1p projects per 100k; ",
        "Muslim exposure = 2001 share; center = survey-weighted 2009 share; N >= 5."
      ),
    x = NULL,
    y =
      "Substantive triple-interaction contrast (percentage points)"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  )

ggplot2::ggsave(
  file.path(
    spec_figure_dir,
    paste0(
      "preferred_model_robustness_",
      RUN_MODE,
      ".pdf"
    )
  ),
  preferred_robustness_plot,
  width = 12,
  height = 8.5
)

# ============================================================
# 14. CONDITIONAL-EFFECT PLOTS FOR THE PREFERRED C1 MODELS
# The plotted quantity is the FDI x Muslim substantive contrast at low versus
# high 2009 centrist share. This makes the triple interaction interpretable
# rather than relying on the three-way coefficient alone.
# ============================================================

conditional_effect_rows <- list()
conditional_effect_index <- 0L

for (
  i in seq_len(
    nrow(preferred_design_rows)
  )
) {
  design_row <-
    preferred_design_rows[
      i,
      ,
      drop = FALSE
    ]
  
  refs <- preferred_reference_values(
    design_row
  )
  
  data <- preferred_base_sample(
    exclude_delhi = FALSE
  )
  
  formula <- make_preferred_formula(
    design_row,
    preferred_control_string(
      "C1"
    )
  )
  
  fit <- tryCatch(
    fixest::feols(
      formula,
      data = data,
      vcov =
        stats::as.formula(
          paste0(
            "~",
            PREFERRED_CLUSTER_VAR
          )
        ),
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    warning(
      "Could not fit preferred C1 conditional-effect model for ",
      design_row$design_id,
      ": ",
      conditionMessage(fit)
    )
    next
  }
  
  fd_term <- interaction_term_name(
    fit,
    c(
      PREFERRED_FDI_VAR,
      PREFERRED_MUSLIM_VAR
    )
  )
  
  triple_term <- interaction_term_name(
    fit,
    c(
      PREFERRED_FDI_VAR,
      PREFERRED_MUSLIM_VAR,
      PREFERRED_CENTER_VAR
    )
  )
  
  if (
    is.na(fd_term) ||
    is.na(triple_term)
  ) {
    warning(
      "Could not identify preferred conditional-effect terms for ",
      design_row$design_id
    )
    next
  }
  
  beta <- stats::coef(fit)
  vc <- stats::vcov(fit)
  
  scale_fd_muslim <-
    refs$delta_fdi *
    refs$delta_muslim
  
  center_points <- tibble::tibble(
    center_reference = c(
      "Low centrist share (25th percentile)",
      "High centrist share (75th percentile)"
    ),
    center_value = c(
      refs$center_low,
      refs$center_high
    )
  )
  
  for (
    j in seq_len(
      nrow(center_points)
    )
  ) {
    center_value <-
      center_points$center_value[[j]]
    
    weights <- c(
      scale_fd_muslim,
      scale_fd_muslim *
        center_value
    )
    names(weights) <- c(
      fd_term,
      triple_term
    )
    
    coefficient_vector <-
      beta[
        names(weights)
      ]
    vc_sub <-
      vc[
        names(weights),
        names(weights),
        drop = FALSE
      ]
    
    estimate <- sum(
      weights *
        coefficient_vector
    )
    
    variance <- as.numeric(
      t(weights) %*%
        vc_sub %*%
        weights
    )
    
    standard_error <- if (
      is.finite(variance) &&
      variance >= 0
    ) {
      sqrt(variance)
    } else {
      NA_real_
    }
    
    z <- stats::qnorm(
      1 -
        (1 - CONFIDENCE_LEVEL) /
        2
    )
    
    conditional_effect_index <-
      conditional_effect_index +
      1L
    
    conditional_effect_rows[[
      conditional_effect_index
    ]] <- tibble::tibble(
      design_id =
        design_row$design_id,
      design_label =
        design_row$design_label,
      center_reference =
        center_points$center_reference[[j]],
      center_value =
        center_value,
      fdi_low =
        refs$fdi_low,
      fdi_high =
        refs$fdi_high,
      muslim_low =
        refs$muslim_low,
      muslim_high =
        refs$muslim_high,
      estimate = estimate,
      standard_error =
        standard_error,
      conf_low =
        estimate -
        z * standard_error,
      conf_high =
        estimate +
        z * standard_error,
      nobs =
        stats::nobs(fit)
    )
  }
}

preferred_conditional_effects <-
  dplyr::bind_rows(
    conditional_effect_rows
  )

readr::write_csv(
  preferred_conditional_effects,
  file.path(
    spec_summary_dir,
    paste0(
      "preferred_conditional_effects_",
      RUN_MODE,
      ".csv"
    )
  )
)

if (
  nrow(
    preferred_conditional_effects
  ) > 0
) {
  preferred_conditional_effect_plot <-
    preferred_conditional_effects |>
    dplyr::mutate(
      center_reference =
        factor(
          center_reference,
          levels = c(
            "Low centrist share (25th percentile)",
            "High centrist share (75th percentile)"
          )
        )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = center_reference,
        y = estimate,
        group = design_id
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = conf_low,
        ymax = conf_high
      ),
      width = 0.12
    ) +
    ggplot2::geom_line(
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      size = 2
    ) +
    ggplot2::facet_wrap(
      ~design_label
    ) +
    ggplot2::labs(
      title =
        "Conditional manufacturing-FDI x Muslim-exposure contrast",
      subtitle =
        paste0(
          "Preferred C1 models. Each point compares the effect of moving FDI from zero ",
          "to median-positive exposure at the 75th versus 25th percentile of 2001 ",
          "Muslim share, evaluated at low or high 2009 centrist share."
        ),
      x = NULL,
      y =
        "Difference in FDI effect: high vs low 2001 Muslim share (percentage points)"
    ) +
    ggplot2::theme_minimal(
      base_size = 10
    ) +
    ggplot2::theme(
      axis.text.x =
        ggplot2::element_text(
          angle = 15,
          hjust = 1
        )
    )
  
  ggplot2::ggsave(
    file.path(
      spec_figure_dir,
      paste0(
        "preferred_conditional_effects_",
        RUN_MODE,
        ".pdf"
      )
    ),
    preferred_conditional_effect_plot,
    width = 11,
    height = 6.5
  )
}

# ============================================================
# 15. CONSOLE SUMMARY
# ============================================================

print(
  curve_summaries |>
    dplyr::arrange(design_id, fdi_family, interaction_order),
  n = Inf
)

message("Specification-curve run complete.")
message("Results: ", spec_result_dir)
message("Figures: ", spec_figure_dir)
message("Summaries: ", spec_summary_dir)
message("Manifests: ", spec_manifest_dir)
message(
  "Preferred-model robustness: ",
  file.path(
    spec_summary_dir,
    paste0(
      "preferred_model_robustness_",
      RUN_MODE,
      ".csv"
    )
  )
)
message(
  "Preferred conditional effects: ",
  file.path(
    spec_summary_dir,
    paste0(
      "preferred_conditional_effects_",
      RUN_MODE,
      ".csv"
    )
  )
)
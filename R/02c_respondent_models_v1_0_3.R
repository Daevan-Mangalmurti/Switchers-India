# ============================================================
# 02c_respondent_models.R
# Respondent-level specification curves for individual BJP voting.
#
# FROZEN DESIGN (2026-08-08)
# --------------------------
# Primary outcome:
#   voted_bjp among valid voters in constituencies where BJP fielded a candidate.
#
# Primary estimator:
#   survey-weighted linear probability model (LPM).
#
# Primary designs:
#   R1. pooled 2009/2014 repeated cross-section with state x year fixed effects.
#   R2. 2014 baseline-adjusted model with state fixed effects, baseline 2009 BJP
#       vote share, and matched baseline 2009 FDI exposure.
#
# Primary inference:
#   two-way clustering by parliamentary constituency and district harmonization
#   group.
#
# Primary control blocks:
#   V2 voter controls (religion + caste + education)
#   C1 contextual controls (population + area + SC/ST composition)
#
# Primary triple moderator:
#   center_harmonized vs other ideology-complete respondents.
#   2009: recognition 2/2 Center AND statism >= 2/3 Center.
#   2014: recognition 2/2 Center AND statism 1/1 Center.
#
# Main multiverse:
#   54 FDI definitions x 48 demographic moderators x
#   4 voter-control blocks x 4 contextual-control blocks x
#   2 respondent designs x 2 interaction orders
#   = 165,888 primary weighted-LPM specifications.
#
# Parallel robustness families:
#   - weighted logit, V2+C1, probability-scale contrasts
#   - unweighted LPM, V2+C1
#   - all-valid-voter outcome, weighted LPM, V2+C1
#   - pooled additive state + year FE sensitivity, V2+C1
#   - pooled strict-Center triple sensitivity, V2+C1
#
# IMPORTANT:
#   This script is intentionally separate from the AC specification-curve code
#   while it is being smoke-tested. Once the respondent pilot passes, it can be
#   sourced from 02_explore_models.R without changing the AC model universe.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(
  file.path(
    project_root,
    "R",
    "helpers.R"
  )
)

load_switchers_packages()

paths <- build_project_paths(
  project_root
)

RESPONDENT_SCRIPT_REVISION <-
  "2026-08-08-v1.0.3-targeted-robustness-hotfix"

message(
  "Loading respondent-model revision: ",
  RESPONDENT_SCRIPT_REVISION
)

message(
  "Verified grid scalar hotfix active: .env$interaction_order / .env$center_var."
)

# ============================================================
# 0. RUN SETTINGS
# ============================================================

RESPONDENT_RUN_MODE <- tolower(
  Sys.getenv(
    "SWITCHERS_RESPONDENT_SPEC_MODE",
    unset = "pilot"
  )
)

if (
  !RESPONDENT_RUN_MODE %in%
  c(
    "pilot",
    "full"
  )
) {
  stop(
    "SWITCHERS_RESPONDENT_SPEC_MODE must be 'pilot' or 'full'."
  )
}

# Which parallel families should run?
# Examples:
#   Sys.setenv(SWITCHERS_RESPONDENT_FAMILIES = "primary")
#   Sys.setenv(SWITCHERS_RESPONDENT_FAMILIES = "primary,logit")
#   Sys.setenv(SWITCHERS_RESPONDENT_FAMILIES = "all")
family_env <- tolower(
  Sys.getenv(
    "SWITCHERS_RESPONDENT_FAMILIES",
    unset = if (
      RESPONDENT_RUN_MODE == "pilot"
    ) {
      "all"
    } else {
      "primary"
    }
  )
)

all_analysis_families <- c(
  "primary",
  "logit",
  "unweighted",
  "all_valid",
  "pooled_additive_fe",
  "strict_center"
)

if (
  family_env == "all"
) {
  RUN_ANALYSIS_FAMILIES <-
    all_analysis_families
} else {
  RUN_ANALYSIS_FAMILIES <-
    unique(
      trimws(
        unlist(
          strsplit(
            family_env,
            ",",
            fixed = TRUE
          )
        )
      )
    )
}

bad_families <- setdiff(
  RUN_ANALYSIS_FAMILIES,
  all_analysis_families
)

if (
  length(bad_families) > 0
) {
  stop(
    "Unknown respondent analysis families: ",
    paste(
      bad_families,
      collapse = ", "
    )
  )
}

RESPONDENT_OVERWRITE_EXISTING <- FALSE
RESPONDENT_CHECKPOINT_EVERY <- 100L
RESPONDENT_CONFIDENCE_LEVEL <- 0.95

# Pilot deliberately exercises all V x C blocks for the primary LPM while
# restricting substantive dimensions to a small set.
RESPONDENT_PILOT_FDI_FAMILY <- "mfg"
RESPONDENT_PILOT_FDI_SCOPE <- "local"
RESPONDENT_PILOT_FDI_STATUS <- "all"
RESPONDENT_PILOT_FDI_FORM <- "log1p_pc100k"

RESPONDENT_PILOT_MUSLIM_VARS <- c(
  "muslim_share_2001_dist_proxy",
  "hindu_muslim_ratio_2001_dist_proxy"
)

RESPONDENT_PILOT_MIGRATION_VARS <- c(
  "mig_prior_5yr_share_ac_pop",
  "mig_total_upto_2001_share_ac_pop"
)

# ============================================================
# 1. OUTPUT DIRECTORIES
# ============================================================

respondent_spec_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "respondent_specification_curves"
)

respondent_result_dir <- file.path(
  respondent_spec_root,
  "results"
)

respondent_figure_dir <- file.path(
  respondent_spec_root,
  "figures"
)

respondent_manifest_dir <- file.path(
  respondent_spec_root,
  "manifests"
)

respondent_summary_dir <- file.path(
  respondent_spec_root,
  "summaries"
)

respondent_log_dir <- file.path(
  respondent_spec_root,
  "logs"
)

respondent_preferred_dir <- file.path(
  respondent_spec_root,
  "preferred_models"
)

purrr::walk(
  c(
    respondent_spec_root,
    respondent_result_dir,
    respondent_figure_dir,
    respondent_manifest_dir,
    respondent_summary_dir,
    respondent_log_dir,
    respondent_preferred_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 2. LOAD RESPONDENT AND BASELINE AC DATA
# ============================================================

respondents <- readRDS(
  file.path(
    paths$final_dir,
    "nes_respondent_analysis.rds"
  )
)

ac_change_respondent <- readRDS(
  file.path(
    paths$final_dir,
    "ac_change.rds"
  )
)

ideology_items_long <- readRDS(
  file.path(
    paths$intermediate_dir,
    "ideology_item_responses_long.rds"
  )
)

# ============================================================
# 3. ANALYSIS-ONLY DERIVED VARIABLES
# ============================================================

# ------------------------------------------------------------
# 3A. Muslim logged levels
# ------------------------------------------------------------

add_respondent_logged_muslim_levels <- function(
    data
) {
  required <- c(
    "muslim_population_2001",
    "muslim_population_2011"
  )

  missing <- setdiff(
    required,
    names(data)
  )

  if (
    length(missing) > 0
  ) {
    stop(
      "Cannot create respondent logged Muslim levels. Missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  data |>
    dplyr::mutate(
      log1p_muslim_population_2001 =
        log1p(
          muslim_population_2001
        ),
      log1p_muslim_population_2011 =
        log1p(
          muslim_population_2011
        )
    )
}

# ------------------------------------------------------------
# 3B. Male prior5-vs-baseline5 acceleration
# ------------------------------------------------------------

add_respondent_male_acceleration <- function(
    data
) {
  required <- c(
    "male_mig_prior_5yr_total",
    "male_mig_baseline_5yr_total"
  )

  missing <- setdiff(
    required,
    names(data)
  )

  if (
    length(missing) > 0
  ) {
    stop(
      "Cannot create respondent male migration acceleration. Missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  prior <-
    data$male_mig_prior_5yr_total
  baseline <-
    data$male_mig_baseline_5yr_total

  data |>
    dplyr::mutate(
      male_mig_accel_prior5_vs_baseline5_ratio =
        safe_ratio(
          prior,
          baseline
        ),
      male_mig_accel_prior5_vs_baseline5_pct_change =
        safe_pct_change(
          prior,
          baseline
        ),
      male_mig_accel_prior5_vs_baseline5_log =
        safe_log_ratio(
          prior,
          baseline
        ),
      male_mig_accel_prior5_vs_baseline5_log1p =
        log1p(prior) -
        log1p(baseline)
    )
}

# ------------------------------------------------------------
# 3C. Logged district employment-intensity proxy
# ------------------------------------------------------------

add_respondent_logged_employment_intensity <- function(
    data
) {
  required <-
    "employment_per_total_population"

  if (
    !required %in%
    names(data)
  ) {
    stop(
      "Respondent data are missing employment_per_total_population."
    )
  }

  if (
    any(
      data$employment_per_total_population < 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "employment_per_total_population contains negative values."
    )
  }

  data |>
    dplyr::mutate(
      log1p_employment_per_total_population =
        log1p(
          employment_per_total_population
        )
    )
}

respondents <- respondents |>
  add_respondent_logged_muslim_levels() |>
  add_respondent_male_acceleration() |>
  add_respondent_logged_employment_intensity()

# ------------------------------------------------------------
# 3D. Reattach ideology item buckets
# ------------------------------------------------------------

classification_items <- c(
  "a4b",
  "a4c",
  "a4d",
  "a4g",
  "q26a",
  "q10b",
  "q10e",
  "q23c"
)

bucket_col <- dplyr::case_when(
  "response_bucket" %in%
    names(ideology_items_long) ~
    "response_bucket",
  "item_bucket" %in%
    names(ideology_items_long) ~
    "item_bucket",
  TRUE ~
    NA_character_
)

if (
  is.na(bucket_col)
) {
  stop(
    "Could not find response_bucket/item_bucket in ideology_item_responses_long.rds."
  )
}

duplicate_item_rows <-
  ideology_items_long |>
  dplyr::filter(
    item %in%
      classification_items
  ) |>
  dplyr::count(
    respondent_uid,
    year,
    item,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

if (
  nrow(
    duplicate_item_rows
  ) > 0
) {
  stop(
    "Duplicate respondent x ideology-item rows found."
  )
}

ideology_buckets_wide <-
  ideology_items_long |>
  dplyr::filter(
    item %in%
      classification_items
  ) |>
  dplyr::transmute(
    respondent_uid,
    year,
    item,
    item_bucket =
      as.character(
        .data[[
          bucket_col
        ]]
      )
  ) |>
  tidyr::pivot_wider(
    names_from = item,
    values_from = item_bucket,
    names_glue =
      "ideology_{item}_bucket"
  )

n_respondents_before_ideology_join <-
  nrow(respondents)

respondents <- respondents |>
  dplyr::left_join(
    ideology_buckets_wide,
    by = c(
      "respondent_uid",
      "year"
    ),
    relationship =
      "one-to-one"
  )

if (
  nrow(respondents) !=
  n_respondents_before_ideology_join
) {
  stop(
    "Ideology-item join changed respondent row count."
  )
}

# ------------------------------------------------------------
# 3E. Strict, harmonized, and 80%-item Center definitions
# ------------------------------------------------------------

respondents <- respondents |>
  dplyr::mutate(
    recognition_center_n =
      dplyr::case_when(
        year == 2009 ~
          rowSums(
            cbind(
              ideology_a4b_bucket ==
                "Center",
              ideology_a4c_bucket ==
                "Center"
            ),
            na.rm = TRUE
          ),
        year == 2014 ~
          rowSums(
            cbind(
              ideology_q10b_bucket ==
                "Center",
              ideology_q10e_bucket ==
                "Center"
            ),
            na.rm = TRUE
          ),
        TRUE ~
          NA_real_
      ),

    statism_center_n =
      dplyr::case_when(
        year == 2009 ~
          rowSums(
            cbind(
              ideology_a4d_bucket ==
                "Center",
              ideology_a4g_bucket ==
                "Center",
              ideology_q26a_bucket ==
                "Center"
            ),
            na.rm = TRUE
          ),
        year == 2014 ~
          as.numeric(
            ideology_q23c_bucket ==
              "Center"
          ),
        TRUE ~
          NA_real_
      ),

    required_ideology_item_n =
      dplyr::case_when(
        year == 2009 ~
          5L,
        year == 2014 ~
          3L,
        TRUE ~
          NA_integer_
      ),

    center_item_n =
      recognition_center_n +
      statism_center_n,

    center_item_share =
      center_item_n /
      required_ideology_item_n,

    center_strict =
      dplyr::case_when(
        !ideology_complete ~
          NA_real_,
        voter_ideology ==
          "Center" ~
          1,
        TRUE ~
          0
      ),

    center_harmonized =
      dplyr::case_when(
        !ideology_complete ~
          NA_real_,

        year == 2009 &
          recognition_center_n == 2 &
          statism_center_n >= 2 ~
          1,

        year == 2014 &
          recognition_center_n == 2 &
          statism_center_n == 1 ~
          1,

        TRUE ~
          0
      ),

    center_relaxed_80 =
      dplyr::case_when(
        !ideology_complete ~
          NA_real_,
        center_item_share >=
          0.80 ~
          1,
        TRUE ~
          0
      )
  )

# Guard the frozen harmonization decision: every 2009 person newly admitted by
# center_harmonized should be exactly one statism answer away from strict Center.
center_harmonization_audit <-
  respondents |>
  dplyr::filter(
    year == 2009,
    ideology_complete,
    center_strict == 0,
    center_harmonized == 1
  ) |>
  dplyr::mutate(
    n_noncenter_statism =
      rowSums(
        cbind(
          ideology_a4d_bucket !=
            "Center",
          ideology_a4g_bucket !=
            "Center",
          ideology_q26a_bucket !=
            "Center"
        ),
        na.rm = TRUE
      )
  ) |>
  dplyr::summarise(
    n_switchers =
      dplyr::n(),
    n_exactly_one_noncenter =
      sum(
        n_noncenter_statism == 1
      ),
    share_exactly_one_noncenter =
      mean(
        n_noncenter_statism == 1
      )
  )

readr::write_csv(
  center_harmonization_audit,
  file.path(
    respondent_manifest_dir,
    "center_harmonization_audit.csv"
  )
)

center_harmonization_switcher_distribution <-
  respondents |>
  dplyr::filter(
    year == 2009,
    ideology_complete,
    center_strict == 0,
    center_harmonized == 1
  ) |>
  dplyr::mutate(
    a4d_noncenter =
      ideology_a4d_bucket !=
      "Center",
    a4g_noncenter =
      ideology_a4g_bucket !=
      "Center",
    q26a_noncenter =
      ideology_q26a_bucket !=
      "Center",

    dissenting_statism_item =
      dplyr::case_when(
        a4d_noncenter ~
          "a4d",
        a4g_noncenter ~
          "a4g",
        q26a_noncenter ~
          "q26a",
        TRUE ~
          NA_character_
      ),

    dissenting_statism_bucket =
      dplyr::case_when(
        a4d_noncenter ~
          as.character(
            ideology_a4d_bucket
          ),
        a4g_noncenter ~
          as.character(
            ideology_a4g_bucket
          ),
        q26a_noncenter ~
          as.character(
            ideology_q26a_bucket
          ),
        TRUE ~
          NA_character_
      )
  ) |>
  dplyr::count(
    dissenting_statism_item,
    dissenting_statism_bucket,
    name = "n"
  ) |>
  dplyr::mutate(
    share_of_switchers =
      n /
      sum(n)
  ) |>
  dplyr::arrange(
    dplyr::desc(n)
  )

readr::write_csv(
  center_harmonization_switcher_distribution,
  file.path(
    respondent_manifest_dir,
    "center_harmonization_switcher_item_distribution.csv"
  )
)

if (
  nrow(center_harmonization_audit) == 1 &&
  center_harmonization_audit$n_switchers > 0 &&
  center_harmonization_audit$share_exactly_one_noncenter != 1
) {
  stop(
    "Frozen Center harmonization audit failed: not every newly admitted 2009 Center is exactly one statism answer away from strict Center."
  )
}

# ------------------------------------------------------------
# 3F. Primary/alternative outcome-sample flags
# ------------------------------------------------------------

respondents <- respondents |>
  dplyr::mutate(
    respondent_sample_all_valid =
      vote_valid &
      !is.na(voted_bjp),

    respondent_sample_candidate_present =
      respondent_sample_all_valid &
      !is.na(
        bjp_candidate_present
      ) &
      bjp_candidate_present == 1
  )

# ------------------------------------------------------------
# 3G. Attach baseline 2009 BJP share and baseline FDI exposures
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

baseline_join_vars <- unique(
  c(
    "ac_uid",
    "bjp_vote_share_2009",
    fdi_meta$baseline_var
  )
)

missing_baseline_join_vars <- setdiff(
  baseline_join_vars,
  names(
    ac_change_respondent
  )
)

if (
  length(
    missing_baseline_join_vars
  ) > 0
) {
  stop(
    "ac_change is missing respondent baseline variables: ",
    paste(
      missing_baseline_join_vars,
      collapse = ", "
    )
  )
}

baseline_payload_vars <- setdiff(
  baseline_join_vars,
  c(
    "ac_uid",
    names(respondents)
  )
)

if (
  length(
    baseline_payload_vars
  ) > 0
) {
  baseline_context <-
    ac_change_respondent |>
    dplyr::select(
      ac_uid,
      dplyr::all_of(
        baseline_payload_vars
      )
    ) |>
    dplyr::distinct(
      ac_uid,
      .keep_all = TRUE
    )

  n_respondents_before_baseline_join <-
    nrow(respondents)

  respondents <- respondents |>
    dplyr::left_join(
      baseline_context,
      by = "ac_uid",
      relationship =
        "many-to-one"
    )

  if (
    nrow(respondents) !=
    n_respondents_before_baseline_join
  ) {
    stop(
      "Baseline-context join changed respondent row count."
    )
  }
}

# ------------------------------------------------------------
# 3H. Equal-election pooled survey weights
# ------------------------------------------------------------

make_equal_election_weight <- function(
    data,
    base_sample
) {
  if (
    length(base_sample) !=
    nrow(data)
  ) {
    stop(
      "base_sample must have one logical value per respondent."
    )
  }

  out <- rep(
    NA_real_,
    nrow(data)
  )

  years <- sort(
    unique(
      data$year[
        base_sample &
          !is.na(data$year)
      ]
    )
  )

  if (
    length(years) == 0
  ) {
    return(out)
  }

  target_total <-
    sum(
      base_sample,
      na.rm = TRUE
    ) /
    length(years)

  for (
    yr in years
  ) {
    idx <-
      base_sample &
      data$year == yr &
      is.finite(
        data$survey_weight_norm_year
      ) &
      data$survey_weight_norm_year > 0

    total_weight <-
      sum(
        data$survey_weight_norm_year[
          idx
        ],
        na.rm = TRUE
      )

    if (
      total_weight > 0
    ) {
      out[idx] <-
        data$survey_weight_norm_year[
          idx
        ] *
        target_total /
        total_weight
    }
  }

  out
}

weight_equal_candidate_two_way <-
  make_equal_election_weight(
    data = respondents,
    base_sample =
      respondents$respondent_sample_candidate_present
  )

weight_equal_candidate_triple <-
  make_equal_election_weight(
    data = respondents,
    base_sample =
      respondents$respondent_sample_candidate_present &
      respondents$ideology_complete
  )

weight_equal_all_valid_two_way <-
  make_equal_election_weight(
    data = respondents,
    base_sample =
      respondents$respondent_sample_all_valid
  )

weight_equal_all_valid_triple <-
  make_equal_election_weight(
    data = respondents,
    base_sample =
      respondents$respondent_sample_all_valid &
      respondents$ideology_complete
  )

respondents <-
  respondents |>
  dplyr::mutate(
    respondent_weight_equal_candidate_two_way =
      weight_equal_candidate_two_way,

    respondent_weight_equal_candidate_triple =
      weight_equal_candidate_triple,

    respondent_weight_equal_all_valid_two_way =
      weight_equal_all_valid_two_way,

    respondent_weight_equal_all_valid_triple =
      weight_equal_all_valid_triple
  )

# ============================================================
# 4. FROZEN SPECIFICATION METADATA
# ============================================================

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

voter_control_sets <- tibble::tribble(
  ~voter_control_set,
  ~voter_control_label,
  ~voter_control_string,
  ~voter_control_preferred,

  "V0",
  "No voter-level controls",
  "",
  FALSE,

  "V1",
  "Religion + caste",
  "religion_group + caste_group",
  FALSE,

  "V2",
  "Religion + caste + education",
  "religion_group + caste_group + education_harmonized",
  TRUE,

  "V3",
  "V2 + contemporaneous income",
  "religion_group + caste_group + education_harmonized + income_harmonized",
  FALSE
)

context_control_sets <- tibble::tribble(
  ~context_control_set,
  ~context_control_label,
  ~context_control_string,
  ~context_control_preferred,

  "C0",
  "Size/geography",
  "proxy_ac_pop + con08_land_area",
  FALSE,

  "C1",
  "Size/geography + SC/ST composition",
  "proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share",
  TRUE,

  "C2",
  "C1 + district employment intensity + education",
  paste(
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    sep = " + "
  ),
  FALSE,

  "C3",
  "C2 + logged per-capita consumption",
  paste(
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    "log_secc_cons_pc",
    sep = " + "
  ),
  FALSE
)

respondent_design_meta <- tibble::tribble(
  ~design_id,
  ~design_type,
  ~design_label,
  ~moderator_domain,
  ~fixed_effects_primary,
  ~design_preferred,

  "respondent_pooled_muslim",
  "pooled",
  "Pooled 2009/2014 respondent choice: Muslim exposure",
  "muslim",
  "state_no^year",
  FALSE,

  "respondent_pooled_migration",
  "pooled",
  "Pooled 2009/2014 respondent choice: Migration/compositional exposure",
  "migration",
  "state_no^year",
  FALSE,

  "respondent_2014_muslim",
  "baseline_2014",
  "2014 baseline-adjusted respondent choice: Muslim exposure",
  "muslim",
  "state_no",
  TRUE,

  "respondent_2014_migration",
  "baseline_2014",
  "2014 baseline-adjusted respondent choice: Migration/compositional exposure",
  "migration",
  "state_no",
  TRUE
)

respondent_analysis_family_meta <- tibble::tribble(
  ~analysis_family,
  ~analysis_family_label,
  ~estimator,
  ~outcome_sample,
  ~weighting_rule,
  ~voter_control_mode,
  ~context_control_mode,
  ~fixed_effect_mode,
  ~center_definition_mode,

  "primary",
  "Primary weighted LPM",
  "lpm",
  "candidate_present",
  "primary_weighted",
  "vary",
  "vary",
  "primary",
  "harmonized",

  "logit",
  "Weighted logit, preferred controls",
  "logit",
  "candidate_present",
  "primary_weighted",
  "preferred",
  "preferred",
  "primary",
  "harmonized",

  "unweighted",
  "Unweighted LPM, preferred controls",
  "lpm",
  "candidate_present",
  "unweighted",
  "preferred",
  "preferred",
  "primary",
  "harmonized",

  "all_valid",
  "All-valid-voter weighted LPM, preferred controls",
  "lpm",
  "all_valid",
  "primary_weighted",
  "preferred",
  "preferred",
  "primary",
  "harmonized",

  "pooled_additive_fe",
  "Pooled additive state + year FE sensitivity",
  "lpm",
  "candidate_present",
  "primary_weighted",
  "preferred",
  "preferred",
  "pooled_additive",
  "harmonized",

  "strict_center",
  "Pooled strict-Center triple sensitivity",
  "lpm",
  "candidate_present",
  "primary_weighted",
  "preferred",
  "preferred",
  "primary",
  "strict"
)

preferred_respondent_manifest <- tibble::tribble(
  ~dimension,
  ~preferred_choice,
  ~role_or_rationale,

  "Outcome sample",
  "Valid voters; BJP candidate present",
  "Conditional BJP choice when BJP is actually available",

  "Estimator",
  "Survey-weighted LPM",
  "Primary probability-scale estimator",

  "Respondent design",
  "2014 baseline-adjusted",
  "Preferred individual-level temporal design",

  "Voter controls",
  "V2: religion + caste + education",
  "Predetermined/compositional individual adjustment",

  "Context controls",
  "C1",
  "Preferred baseline contextual adjustment",

  "FDI family",
  "Manufacturing",
  "Economic-disruption mechanism",

  "FDI geography",
  "Local: own + neighbors",
  "Local economic exposure can cross AC boundaries",

  "FDI status",
  "All announced/opened",
  "Captures anticipated and realized FDI exposure",

  "FDI form",
  "log1p projects per 100k",
  "Population-scaled with reduced leverage",

  "Muslim moderator",
  "2001 Muslim population share",
  "Pre-treatment contextual demographic exposure",

  "Triple moderator",
  "center_harmonized",
  "2009 axis-aware harmonization; identical to strict Center in 2014",

  "2014 fixed effects",
  "State FE",
  "Within-state identification",

  "Pooled fixed effects",
  "State x year FE",
  "Within-state-election identification",

  "Inference",
  "PC + district multiway clustering",
  "Allows dependence along electoral and district-context dimensions"
)

readr::write_csv(
  preferred_respondent_manifest,
  file.path(
    respondent_manifest_dir,
    "preferred_respondent_specification.csv"
  )
)

readr::write_csv(
  voter_control_sets,
  file.path(
    respondent_manifest_dir,
    "voter_control_sets.csv"
  )
)

readr::write_csv(
  context_control_sets,
  file.path(
    respondent_manifest_dir,
    "context_control_sets.csv"
  )
)

readr::write_csv(
  fdi_meta,
  file.path(
    respondent_manifest_dir,
    "respondent_fdi_specifications.csv"
  )
)

readr::write_csv(
  muslim_meta,
  file.path(
    respondent_manifest_dir,
    "respondent_muslim_moderator_specifications.csv"
  )
)

readr::write_csv(
  migration_meta,
  file.path(
    respondent_manifest_dir,
    "respondent_migration_moderator_specifications.csv"
  )
)

# ============================================================
# 5. VALIDATION
# ============================================================

assert_respondent_model_columns <- function(
    data,
    columns,
    label = "respondents"
) {
  columns <- unique(
    columns[
      !is.na(columns) &
        nzchar(columns)
    ]
  )

  missing <- setdiff(
    columns,
    names(data)
  )

  if (
    length(missing) > 0
  ) {
    stop(
      label,
      " is missing required columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  invisible(data)
}

all_control_vars <- unique(
  c(
    unlist(
      strsplit(
        paste(
          voter_control_sets$voter_control_string,
          collapse = " + "
        ),
        " \\+ "
      )
    ),
    unlist(
      strsplit(
        paste(
          context_control_sets$context_control_string,
          collapse = " + "
        ),
        " \\+ "
      )
    )
  )
)

all_control_vars <-
  all_control_vars[
    nzchar(
      all_control_vars
    )
  ]

required_respondent_model_columns <- unique(
  c(
    "respondent_uid",
    "year",
    "voted_bjp",
    "vote_valid",
    "bjp_candidate_present",
    "survey_weight_norm_year",
    "respondent_sample_candidate_present",
    "respondent_sample_all_valid",
    "ideology_complete",
    "center_strict",
    "center_harmonized",
    "center_relaxed_80",
    "state_no",
    "ac_uid",
    "pc_cluster_id",
    "district_harmonization_group_id",
    "bjp_vote_share_2009",
    "respondent_weight_equal_candidate_two_way",
    "respondent_weight_equal_candidate_triple",
    "respondent_weight_equal_all_valid_two_way",
    "respondent_weight_equal_all_valid_triple",
    fdi_meta$pooled_var,
    fdi_meta$baseline_var,
    muslim_meta$pooled_var,
    migration_meta$pooled_var,
    all_control_vars
  )
)

assert_respondent_model_columns(
  respondents,
  required_respondent_model_columns
)

# Outcome must be binary wherever defined.
observed_bjp_values <- sort(
  unique(
    respondents$voted_bjp[
      !is.na(
        respondents$voted_bjp
      )
    ]
  )
)

if (
  !all(
    observed_bjp_values %in%
      c(
        0,
        1
      )
  )
) {
  stop(
    "voted_bjp is not binary 0/1."
  )
}

# The harmonized and strict Center definitions must be identical in 2014.
center_2014_mismatch <- respondents |>
  dplyr::filter(
    year == 2014,
    ideology_complete
  ) |>
  dplyr::summarise(
    n_mismatch =
      sum(
        center_harmonized !=
          center_strict,
        na.rm = TRUE
      )
  ) |>
  dplyr::pull(
    n_mismatch
  )

if (
  center_2014_mismatch != 0
) {
  stop(
    "center_harmonized does not equal center_strict in 2014."
  )
}

# ============================================================
# 6. REFERENCE-VALUE HELPERS
# ============================================================

finite_values_respondent <- function(
    x
) {
  x[
    is.finite(x)
  ]
}

respondent_fdi_reference <- function(
    x
) {
  x <- finite_values_respondent(
    x
  )

  positive <- x[
    x > 0
  ]

  if (
    length(
      positive
    ) == 0
  ) {
    return(
      tibble::tibble(
        fdi_low = NA_real_,
        fdi_high = NA_real_,
        delta_fdi = NA_real_,
        fdi_reference_method =
          "No positive FDI exposure"
      )
    )
  }

  high <- stats::median(
    positive,
    na.rm = TRUE
  )

  tibble::tibble(
    fdi_low = 0,
    fdi_high = high,
    delta_fdi = high,
    fdi_reference_method =
      "0 to median positive FDI"
  )
}

respondent_moderator_reference <- function(
    x,
    orientation = 1
) {
  x <- finite_values_respondent(
    x
  )

  if (
    length(
      unique(x)
    ) < 2
  ) {
    return(
      tibble::tibble(
        moderator_low = NA_real_,
        moderator_high = NA_real_,
        delta_moderator = NA_real_,
        moderator_reference_method =
          "Insufficient moderator variation"
      )
    )
  }

  q <- stats::quantile(
    x,
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE,
    names = FALSE
  )

  q25 <- as.numeric(
    q[[1]]
  )
  q75 <- as.numeric(
    q[[2]]
  )

  method <-
    "25th to 75th percentile"

  if (
    !is.finite(q25) ||
    !is.finite(q75) ||
    q25 == q75
  ) {
    positive <- x[
      x > 0
    ]

    if (
      orientation == 1 &&
      any(
        x == 0
      ) &&
      length(
        positive
      ) > 0
    ) {
      q25 <- 0
      q75 <- stats::median(
        positive,
        na.rm = TRUE
      )

      method <-
        "0 to median positive moderator"
    } else {
      q25 <- min(
        x,
        na.rm = TRUE
      )

      q75 <- max(
        x,
        na.rm = TRUE
      )

      method <-
        "Observed minimum to maximum"
    }
  }

  if (
    orientation == 1
  ) {
    low <-
      q25
    high <-
      q75
  } else {
    low <-
      q75
    high <-
      q25

    method <- paste0(
      method,
      "; reversed for Muslim exposure"
    )
  }

  tibble::tibble(
    moderator_low = low,
    moderator_high = high,
    delta_moderator =
      high - low,
    moderator_reference_method =
      method
  )
}

# ============================================================
# 7. DATA/SAMPLE/WEIGHT HELPERS
# ============================================================

get_respondent_moderator_meta <- function(
    domain
) {
  if (
    domain == "muslim"
  ) {
    muslim_meta
  } else {
    migration_meta
  }
}

get_center_variable_for_family <- function(
    family_row
) {
  if (
    family_row$center_definition_mode ==
    "strict"
  ) {
    "center_strict"
  } else {
    "center_harmonized"
  }
}

get_outcome_sample_flag <- function(
    outcome_sample
) {
  if (
    outcome_sample ==
    "all_valid"
  ) {
    "respondent_sample_all_valid"
  } else {
    "respondent_sample_candidate_present"
  }
}

get_primary_weight_variable <- function(
    design_type,
    interaction_order,
    outcome_sample
) {
  if (
    design_type ==
    "baseline_2014"
  ) {
    return(
      "survey_weight_norm_year"
    )
  }

  if (
    outcome_sample ==
    "all_valid"
  ) {
    if (
      interaction_order ==
      "triple"
    ) {
      return(
        "respondent_weight_equal_all_valid_triple"
      )
    }

    return(
      "respondent_weight_equal_all_valid_two_way"
    )
  }

  if (
    interaction_order ==
    "triple"
  ) {
    "respondent_weight_equal_candidate_triple"
  } else {
    "respondent_weight_equal_candidate_two_way"
  }
}

get_fixed_effects_for_spec <- function(
    design_row,
    family_row
) {
  if (
    family_row$fixed_effect_mode ==
    "pooled_additive"
  ) {
    if (
      design_row$design_type !=
      "pooled"
    ) {
      stop(
        "pooled_additive FE family was requested for a non-pooled design."
      )
    }

    return(
      "state_no + year"
    )
  }

  design_row$fixed_effects_primary
}

respondent_base_sample <- function(
    design_row,
    interaction_order,
    outcome_sample =
      "candidate_present",
    center_var =
      "center_harmonized"
) {
  sample_flag <-
    get_outcome_sample_flag(
      outcome_sample
    )

  keep <-
    respondents[[
      sample_flag
    ]]

  if (
    design_row$design_type ==
    "baseline_2014"
  ) {
    keep <-
      keep &
      respondents$year == 2014
  } else {
    keep <-
      keep &
      respondents$year %in%
      c(
        2009,
        2014
      )
  }

  if (
    interaction_order ==
    "triple"
  ) {
    keep <-
      keep &
      respondents$ideology_complete &
      !is.na(
        respondents[[
          center_var
        ]]
      )
  }

  respondents[
    keep,
    ,
    drop = FALSE
  ]
}

weighted_ess_respondent <- function(
    w
) {
  w <- as.numeric(w)
  w <- w[
    is.finite(w) &
      w > 0
  ]

  if (
    length(w) == 0
  ) {
    return(
      NA_real_
    )
  }

  sum(w)^2 /
    sum(w^2)
}

complete_finite_cases_respondent <- function(
    data,
    vars
) {
  vars <- unique(
    vars[
      !is.na(vars) &
        nzchar(vars)
    ]
  )

  missing <- setdiff(
    vars,
    names(data)
  )

  if (
    length(missing) > 0
  ) {
    stop(
      "Requested fit variables are missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  keep <- rep(
    TRUE,
    nrow(data)
  )

  for (
    v in vars
  ) {
    x <- data[[
      v
    ]]

    if (
      is.numeric(x) ||
      is.integer(x)
    ) {
      keep <-
        keep &
        !is.na(x) &
        is.finite(
          as.numeric(x)
        )
    } else {
      keep <-
        keep &
        !is.na(x)
    }
  }

  keep
}

# ============================================================
# 8. GRID CONSTRUCTION
# ============================================================

respondent_controls_crossing <- tidyr::crossing(
  voter_control_sets,
  context_control_sets
)

make_respondent_curve_grid <- function(
    design_row,
    fdi_family,
    interaction_order,
    family_row
) {
  if (
    family_row$analysis_family ==
      "pooled_additive_fe" &&
    design_row$design_type !=
      "pooled"
  ) {
    return(
      tibble::tibble()
    )
  }

  if (
    family_row$analysis_family ==
      "strict_center" &&
    !(
      design_row$design_type ==
        "pooled" &&
      interaction_order ==
        "triple"
    )
  ) {
    return(
      tibble::tibble()
    )
  }

  moderator_rows <-
    get_respondent_moderator_meta(
      design_row$moderator_domain
    ) |>
    dplyr::select(
      -dplyr::any_of(
        "change_var"
      )
    ) |>
    dplyr::rename(
      moderator_var =
        pooled_var
    )

  fdi_rows <- fdi_meta |>
    dplyr::filter(
      .data$fdi_family ==
        .env$fdi_family
    ) |>
    dplyr::select(
      -dplyr::any_of(
        "change_var"
      )
    ) |>
    dplyr::rename(
      exposure_var =
        pooled_var
    )

  if (
    RESPONDENT_RUN_MODE ==
    "pilot"
  ) {
    if (
      fdi_family !=
      RESPONDENT_PILOT_FDI_FAMILY
    ) {
      return(
        tibble::tibble()
      )
    }

    fdi_rows <-
      fdi_rows |>
      dplyr::filter(
        fdi_scope ==
          RESPONDENT_PILOT_FDI_SCOPE,
        fdi_status ==
          RESPONDENT_PILOT_FDI_STATUS,
        fdi_form ==
          RESPONDENT_PILOT_FDI_FORM
      )

    moderator_rows <-
      if (
        design_row$moderator_domain ==
        "muslim"
      ) {
        moderator_rows |>
          dplyr::filter(
            moderator_var %in%
              RESPONDENT_PILOT_MUSLIM_VARS
          )
      } else {
        moderator_rows |>
          dplyr::filter(
            moderator_var %in%
              RESPONDENT_PILOT_MIGRATION_VARS
          )
      }
  }

  if (
    family_row$voter_control_mode ==
    "vary"
  ) {
    voter_rows <-
      voter_control_sets
  } else {
    voter_rows <-
      voter_control_sets |>
      dplyr::filter(
        voter_control_preferred
      )
  }

  if (
    family_row$context_control_mode ==
    "vary"
  ) {
    context_rows <-
      context_control_sets
  } else {
    context_rows <-
      context_control_sets |>
      dplyr::filter(
        context_control_preferred
      )
  }

  center_var <-
    get_center_variable_for_family(
      family_row
    )

  fixed_effects <-
    get_fixed_effects_for_spec(
      design_row,
      family_row
    )

  weight_var <- if (
    family_row$weighting_rule ==
    "unweighted"
  ) {
    NA_character_
  } else {
    get_primary_weight_variable(
      design_type =
        design_row$design_type,
      interaction_order =
        interaction_order,
      outcome_sample =
        family_row$outcome_sample
    )
  }

  grid <- tidyr::crossing(
    fdi_rows,
    moderator_rows,
    voter_rows,
    context_rows
  ) |>
    dplyr::mutate(
      analysis_family =
        family_row$analysis_family,
      analysis_family_label =
        family_row$analysis_family_label,
      estimator =
        family_row$estimator,
      outcome_sample =
        family_row$outcome_sample,
      weighting_rule =
        family_row$weighting_rule,
      design_id =
        design_row$design_id,
      design_type =
        design_row$design_type,
      design_label =
        design_row$design_label,
      design_preferred =
        design_row$design_preferred,
      moderator_domain =
        design_row$moderator_domain,
      interaction_order =
        .env$interaction_order,
      baseline_fdi_var =
        if (
          design_row$design_type ==
          "baseline_2014"
        ) {
          baseline_var
        } else {
          NA_character_
        },
      center_var =
        if (
          .env$interaction_order ==
          "triple"
        ) {
          .env$center_var
        } else {
          NA_character_
        },
      center_definition =
        dplyr::case_when(
          .env$interaction_order !=
            "triple" ~
            NA_character_,
          .env$center_var ==
            "center_strict" ~
            "Strict Center",
          TRUE ~
            "Harmonized Center"
        ),
      fixed_effects =
        fixed_effects,
      cluster_rule =
        "PC + district multiway",
      cluster_formula =
        "~pc_cluster_id + district_harmonization_group_id",
      weight_var =
        weight_var,
      fdi_preferred =
        fdi_family == "mfg" &
        fdi_scope == "local" &
        fdi_status == "all" &
        fdi_form ==
          "log1p_pc100k",
      respondent_model_preferred =
        analysis_family ==
          "primary" &
        design_type ==
          "baseline_2014" &
        dplyr::coalesce(
          fdi_preferred,
          FALSE
        ) &
        dplyr::coalesce(
          moderator_preferred,
          FALSE
        ) &
        voter_control_preferred &
        context_control_preferred,
      spec_key = paste(
        analysis_family,
        design_id,
        fdi_family,
        fdi_scope,
        fdi_status,
        fdi_form,
        moderator_var,
        voter_control_set,
        context_control_set,
        interaction_order,
        center_definition,
        fixed_effects,
        sep = "__"
      )
    )

  # Reference values are anchored to the PRIMARY candidate-present base sample,
  # even for parallel sample/estimator robustness families, so sample/estimator
  # comparisons use identical substantive FDI/moderator movements.
  primary_ref_data <-
    respondent_base_sample(
      design_row = design_row,
      interaction_order =
        interaction_order,
      outcome_sample =
        "candidate_present",
      center_var =
        if (
          interaction_order ==
          "triple"
        ) {
          if (
            family_row$center_definition_mode ==
            "strict"
          ) {
            "center_strict"
          } else {
            "center_harmonized"
          }
        } else {
          "center_harmonized"
        }
    )

  reference_combinations <-
    grid |>
    dplyr::distinct(
      exposure_var,
      moderator_var,
      orientation
    )

  reference_rows <-
    purrr::map_dfr(
      seq_len(
        nrow(
          reference_combinations
        )
      ),
      function(i) {
        fvar <-
          reference_combinations$exposure_var[[i]]

        mvar <-
          reference_combinations$moderator_var[[i]]

        orient <-
          reference_combinations$orientation[[i]]

        fref <-
          respondent_fdi_reference(
            primary_ref_data[[
              fvar
            ]]
          )

        mref <-
          respondent_moderator_reference(
            primary_ref_data[[
              mvar
            ]],
            orientation =
              orient
          )

        dplyr::bind_cols(
          reference_combinations[
            i,
            ,
            drop = FALSE
          ],
          fref,
          mref
        )
      }
    )

  grid |>
    dplyr::left_join(
      reference_rows,
      by = c(
        "exposure_var",
        "moderator_var",
        "orientation"
      )
    ) |>
    dplyr::mutate(
      contrast_multiplier =
        100 *
        delta_fdi *
        delta_moderator,
      reference_valid =
        is.finite(
          delta_fdi
        ) &
        delta_fdi != 0 &
        is.finite(
          delta_moderator
        ) &
        delta_moderator != 0
    )
}

# ============================================================
# 9. FORMULA CONSTRUCTION
# ============================================================

respondent_rhs_terms <- function(
    spec
) {
  interaction_term <-
    if (
      spec$interaction_order ==
      "two_way"
    ) {
      paste0(
        spec$exposure_var,
        " * ",
        spec$moderator_var
      )
    } else {
      paste0(
        spec$exposure_var,
        " * ",
        spec$moderator_var,
        " * ",
        spec$center_var
      )
    }

  always_controls <-
    character(0)

  if (
    spec$design_type ==
    "baseline_2014"
  ) {
    always_controls <-
      c(
        always_controls,
        spec$baseline_fdi_var,
        "bjp_vote_share_2009"
      )
  }

  if (
    spec$design_type ==
    "pooled" &&
    spec$interaction_order ==
    "triple"
  ) {
    always_controls <-
      c(
        always_controls,
        paste0(
          "factor(year):",
          spec$center_var
        )
      )
  }

  rhs <- c(
    interaction_term,
    always_controls,
    spec$voter_control_string,
    spec$context_control_string
  )

  rhs[
    !is.na(rhs) &
      nzchar(rhs)
  ]
}

make_respondent_formulas <- function(
    spec
) {
  rhs <-
    respondent_rhs_terms(
      spec
    )

  rhs_text <-
    paste(
      rhs,
      collapse = " + "
    )

  list(
    rhs_formula =
      stats::as.formula(
        paste0(
          "~ ",
          rhs_text
        )
      ),

    full_formula =
      stats::as.formula(
        paste0(
          "voted_bjp ~ ",
          rhs_text,
          " | ",
          spec$fixed_effects
        )
      )
  )
}

# ============================================================
# 10. FIT-DATA CONSTRUCTION
# ============================================================

prepare_respondent_fit_data <- function(
    spec
) {
  design_row <-
    respondent_design_meta |>
    dplyr::filter(
      design_id ==
        spec$design_id
    )

  if (
    nrow(design_row) != 1
  ) {
    stop(
      "Could not uniquely resolve respondent design: ",
      spec$design_id
    )
  }

  center_var <-
    if (
      spec$interaction_order ==
      "triple"
    ) {
      spec$center_var
    } else {
      "center_harmonized"
    }

  data <-
    respondent_base_sample(
      design_row =
        design_row,
      interaction_order =
        spec$interaction_order,
      outcome_sample =
        spec$outcome_sample,
      center_var =
        center_var
    )

  formulas <-
    make_respondent_formulas(
      spec
    )

  required_vars <- unique(
    c(
      all.vars(
        formulas$full_formula
      ),
      "pc_cluster_id",
      "district_harmonization_group_id",
      spec$weight_var
    )
  )

  required_vars <-
    required_vars[
      !is.na(required_vars) &
        nzchar(required_vars)
    ]

  keep <-
    complete_finite_cases_respondent(
      data,
      required_vars
    )

  data <-
    data[
      keep,
      ,
      drop = FALSE
    ]

  if (
    !is.na(
      spec$weight_var
    )
  ) {
    w <-
      data[[
        spec$weight_var
      ]]

    positive_weight <-
      is.finite(w) &
      w > 0

    data <-
      data[
        positive_weight,
        ,
        drop = FALSE
      ]
  }

  data
}

# ============================================================
# 11. TERM IDENTIFICATION AND LPM CONTRASTS
# ============================================================

interaction_term_name_respondent <- function(
    fit,
    variables
) {
  coef_names <-
    names(
      stats::coef(fit)
    )

  target <- paste(
    sort(variables),
    collapse = ":"
  )

  normalized <- vapply(
    strsplit(
      coef_names,
      ":",
      fixed = TRUE
    ),
    function(parts) {
      parts <- gsub(
        "`",
        "",
        parts,
        fixed = TRUE
      )

      paste(
        sort(parts),
        collapse = ":"
      )
    },
    character(1)
  )

  matches <-
    coef_names[
      normalized ==
        target
    ]

  if (
    length(matches) != 1
  ) {
    return(
      NA_character_
    )
  }

  matches[[1]]
}

respondent_fit_r2 <- function(
    fit
) {
  tryCatch(
    as.numeric(
      fixest::fitstat(
        fit,
        "r2"
      )[[1]]
    ),
    error =
      function(e) {
        NA_real_
      }
  )
}

fit_lpm_respondent_spec <- function(
    spec,
    fit_data
) {
  formulas <-
    make_respondent_formulas(
      spec
    )

  warnings_captured <-
    character(0)

  fit <- withCallingHandlers(
    tryCatch(
      {
        if (
          is.na(
            spec$weight_var
          )
        ) {
          fixest::feols(
            formulas$full_formula,
            data = fit_data,
            vcov =
              stats::as.formula(
                spec$cluster_formula
              ),
            notes = FALSE,
            warn = FALSE
          )
        } else {
          fixest::feols(
            formulas$full_formula,
            data = fit_data,
            weights =
              fit_data[[
                spec$weight_var
              ]],
            vcov =
              stats::as.formula(
                spec$cluster_formula
              ),
            notes = FALSE,
            warn = FALSE
          )
        }
      },
      error =
        function(e) {
          e
        }
    ),
    warning =
      function(w) {
        warnings_captured <<-
          c(
            warnings_captured,
            conditionMessage(w)
          )

        invokeRestart(
          "muffleWarning"
        )
      }
  )

  if (
    inherits(
      fit,
      "error"
    )
  ) {
    return(
      list(
        fit = NULL,
        result =
          tibble::tibble(
            fit_ok = FALSE,
            interaction_term =
              NA_character_,
            interaction_estimate =
              NA_real_,
            interaction_se =
              NA_real_,
            interaction_p =
              NA_real_,
            interaction_conf_low =
              NA_real_,
            interaction_conf_high =
              NA_real_,
            contrast_estimate =
              NA_real_,
            contrast_conf_low =
              NA_real_,
            contrast_conf_high =
              NA_real_,
            ci_excludes_zero =
              FALSE,
            nobs =
              NA_integer_,
            weighted_ess =
              NA_real_,
            n_acs =
              NA_integer_,
            n_pcs =
              NA_integer_,
            n_districts =
              NA_integer_,
            r2 =
              NA_real_,
            formula =
              paste(
                deparse(
                  formulas$full_formula
                ),
                collapse = " "
              ),
            converged =
              NA,
            separation_warning =
              FALSE,
            n_dropped =
              NA_integer_,
            warning =
              paste(
                warnings_captured,
                collapse = " | "
              ),
            error =
              conditionMessage(fit),
            contrast_ci_method =
              "Linear coefficient scaling"
          )
      )
    )
  }

  target_variables <- if (
    spec$interaction_order ==
    "two_way"
  ) {
    c(
      spec$exposure_var,
      spec$moderator_var
    )
  } else {
    c(
      spec$exposure_var,
      spec$moderator_var,
      spec$center_var
    )
  }

  term <-
    interaction_term_name_respondent(
      fit,
      target_variables
    )

  if (
    is.na(term)
  ) {
    return(
      list(
        fit = fit,
        result =
          tibble::tibble(
            fit_ok = FALSE,
            interaction_term =
              NA_character_,
            interaction_estimate =
              NA_real_,
            interaction_se =
              NA_real_,
            interaction_p =
              NA_real_,
            interaction_conf_low =
              NA_real_,
            interaction_conf_high =
              NA_real_,
            contrast_estimate =
              NA_real_,
            contrast_conf_low =
              NA_real_,
            contrast_conf_high =
              NA_real_,
            ci_excludes_zero =
              FALSE,
            nobs =
              stats::nobs(fit),
            weighted_ess =
              NA_real_,
            n_acs =
              dplyr::n_distinct(
                fit_data$ac_uid
              ),
            n_pcs =
              dplyr::n_distinct(
                fit_data$pc_cluster_id
              ),
            n_districts =
              dplyr::n_distinct(
                fit_data$district_harmonization_group_id
              ),
            r2 =
              respondent_fit_r2(
                fit
              ),
            formula =
              paste(
                deparse(
                  formulas$full_formula
                ),
                collapse = " "
              ),
            converged =
              TRUE,
            separation_warning =
              FALSE,
            n_dropped =
              nrow(fit_data) -
              stats::nobs(fit),
            warning =
              paste(
                warnings_captured,
                collapse = " | "
              ),
            error =
              "Target interaction term absent, usually because of collinearity.",
            contrast_ci_method =
              "Linear coefficient scaling"
          )
      )
    )
  }

  estimate <-
    unname(
      stats::coef(fit)[
        term
      ]
    )

  standard_error <-
    unname(
      fixest::se(fit)[
        term
      ]
    )

  p_value <-
    unname(
      fixest::pvalue(fit)[
        term
      ]
    )

  ci <- tryCatch(
    stats::confint(
      fit,
      parm = term,
      level =
        RESPONDENT_CONFIDENCE_LEVEL
    ),
    error =
      function(e) {
        NULL
      }
  )

  if (
    is.null(ci)
  ) {
    z <- stats::qnorm(
      1 -
        (
          1 -
          RESPONDENT_CONFIDENCE_LEVEL
        ) /
        2
    )

    conf_low <-
      estimate -
      z *
      standard_error

    conf_high <-
      estimate +
      z *
      standard_error
  } else {
    conf_low <-
      as.numeric(
        ci[
          1,
          1
        ]
      )

    conf_high <-
      as.numeric(
        ci[
          1,
          2
        ]
      )
  }

  multiplier <-
    spec$contrast_multiplier

  contrast <-
    estimate *
    multiplier

  contrast_low_raw <-
    conf_low *
    multiplier

  contrast_high_raw <-
    conf_high *
    multiplier

  fit_weights <- if (
    is.na(
      spec$weight_var
    )
  ) {
    rep(
      1,
      nrow(fit_data)
    )
  } else {
    fit_data[[
      spec$weight_var
    ]]
  }

  contrast_low <-
    min(
      contrast_low_raw,
      contrast_high_raw
    )

  contrast_high <-
    max(
      contrast_low_raw,
      contrast_high_raw
    )

  list(
    fit = fit,
    result =
      tibble::tibble(
        fit_ok =
          is.finite(contrast),
        interaction_term =
          term,
        interaction_estimate =
          estimate,
        interaction_se =
          standard_error,
        interaction_p =
          p_value,
        interaction_conf_low =
          conf_low,
        interaction_conf_high =
          conf_high,
        contrast_estimate =
          contrast,
        contrast_conf_low =
          contrast_low,
        contrast_conf_high =
          contrast_high,
        ci_excludes_zero =
          is.finite(
            contrast_low
          ) &&
          is.finite(
            contrast_high
          ) &&
          (
            contrast_low > 0 ||
            contrast_high < 0
          ),
        nobs =
          stats::nobs(fit),
        weighted_ess =
          weighted_ess_respondent(
            fit_weights
          ),
        n_acs =
          dplyr::n_distinct(
            fit_data$ac_uid
          ),
        n_pcs =
          dplyr::n_distinct(
            fit_data$pc_cluster_id
          ),
        n_districts =
          dplyr::n_distinct(
            fit_data$district_harmonization_group_id
          ),
        r2 =
          respondent_fit_r2(
            fit
          ),
        formula =
          paste(
            deparse(
              formulas$full_formula
            ),
            collapse = " "
          ),
        converged =
          TRUE,
        separation_warning =
          FALSE,
        n_dropped =
          nrow(fit_data) -
          stats::nobs(fit),
        warning =
          paste(
            unique(
              warnings_captured
            ),
            collapse = " | "
          ),
        error =
          if (
            is.finite(contrast)
          ) {
            NA_character_
          } else {
            "Non-finite linear substantive contrast."
          },
        contrast_ci_method =
          "Linear coefficient scaling"
      )
  )
}

# ============================================================
# 12. LOGIT G-COMPUTATION / DELTA-METHOD CONTRAST
# ============================================================

weighted_col_means <- function(
    matrix,
    weights
) {
  weights <- as.numeric(
    weights
  )

  denom <- sum(
    weights
  )

  if (
    !is.finite(denom) ||
    denom <= 0
  ) {
    return(
      rep(
        NA_real_,
        ncol(matrix)
      )
    )
  }

  as.numeric(
    crossprod(
      weights,
      matrix
    ) /
      denom
  )
}

model_matrix_for_fit <- function(
    rhs_formula,
    data,
    beta_names
) {
  mm <- stats::model.matrix(
    rhs_formula,
    data = data
  )

  missing_beta_columns <-
    setdiff(
      beta_names,
      colnames(mm)
    )

  if (
    length(
      missing_beta_columns
    ) > 0
  ) {
    stop(
      "Could not match logit coefficient columns in scenario model matrix: ",
      paste(
        missing_beta_columns,
        collapse = ", "
      )
    )
  }

  mm[
    ,
    beta_names,
    drop = FALSE
  ]
}

logit_probability_contrast <- function(
    fit,
    spec,
    fit_data
) {
  formulas <-
    make_respondent_formulas(
      spec
    )

  beta <-
    stats::coef(fit)

  beta_names <-
    names(beta)

  if (
    length(beta_names) == 0
  ) {
    stop(
      "Logit model has no slope coefficients."
    )
  }

  if (
    stats::nobs(fit) !=
    nrow(fit_data)
  ) {
    stop(
      "Logit fit dropped observations after pre-filtering; cannot safely align g-computation rows."
    )
  }

  x_observed <-
    model_matrix_for_fit(
      rhs_formula =
        formulas$rhs_formula,
      data =
        fit_data,
      beta_names =
        beta_names
    )

  eta_observed <-
    as.numeric(
      stats::predict(
        fit,
        type = "link"
      )
    )

  if (
    length(
      eta_observed
    ) !=
    nrow(fit_data)
  ) {
    stop(
      "Logit linear predictor is not aligned with fit data."
    )
  }

  slope_observed <-
    as.numeric(
      x_observed %*%
      beta
    )

  fe_offset <-
    eta_observed -
    slope_observed

  scenario_weights <- if (
    is.na(
      spec$weight_var
    )
  ) {
    rep(
      1,
      nrow(fit_data)
    )
  } else {
    fit_data[[
      spec$weight_var
    ]]
  }

  scenarios <- if (
    spec$interaction_order ==
    "two_way"
  ) {
    tibble::tribble(
      ~fdi_value,
      ~moderator_value,
      ~center_value,
      ~contrast_sign,

      spec$fdi_high,
      spec$moderator_high,
      NA_real_,
      1,

      spec$fdi_low,
      spec$moderator_high,
      NA_real_,
      -1,

      spec$fdi_high,
      spec$moderator_low,
      NA_real_,
      -1,

      spec$fdi_low,
      spec$moderator_low,
      NA_real_,
      1
    )
  } else {
    tibble::tribble(
      ~fdi_value,
      ~moderator_value,
      ~center_value,
      ~contrast_sign,

      spec$fdi_high,
      spec$moderator_high,
      1,
      1,

      spec$fdi_low,
      spec$moderator_high,
      1,
      -1,

      spec$fdi_high,
      spec$moderator_low,
      1,
      -1,

      spec$fdi_low,
      spec$moderator_low,
      1,
      1,

      spec$fdi_high,
      spec$moderator_high,
      0,
      -1,

      spec$fdi_low,
      spec$moderator_high,
      0,
      1,

      spec$fdi_high,
      spec$moderator_low,
      0,
      1,

      spec$fdi_low,
      spec$moderator_low,
      0,
      -1
    )
  }

  contrast_probability <-
    0

  contrast_gradient <-
    rep(
      0,
      length(beta)
    )

  for (
    s in seq_len(
      nrow(scenarios)
    )
  ) {
    scenario_data <-
      fit_data

    scenario_data[[
      spec$exposure_var
    ]] <-
      scenarios$fdi_value[[s]]

    scenario_data[[
      spec$moderator_var
    ]] <-
      scenarios$moderator_value[[s]]

    if (
      spec$interaction_order ==
      "triple"
    ) {
      scenario_data[[
        spec$center_var
      ]] <-
        scenarios$center_value[[s]]
    }

    x_s <-
      model_matrix_for_fit(
        rhs_formula =
          formulas$rhs_formula,
        data =
          scenario_data,
        beta_names =
          beta_names
      )

    eta_s <-
      fe_offset +
      as.numeric(
        x_s %*%
        beta
      )

    p_s <-
      stats::plogis(
        eta_s
      )

    avg_p <-
      stats::weighted.mean(
        p_s,
        w =
          scenario_weights
      )

    derivative_weight <-
      scenario_weights *
      p_s *
      (
        1 -
        p_s
      )

    gradient_s <-
      weighted_col_means(
        matrix =
          x_s,
        weights =
          derivative_weight
      ) *
      (
        sum(
          derivative_weight
        ) /
        sum(
          scenario_weights
        )
      )

    # The preceding expression algebraically equals:
    # sum(w * p(1-p) * X) / sum(w).
    # Recalculate directly for clarity and numerical stability.
    gradient_s <-
      as.numeric(
        crossprod(
          scenario_weights *
          p_s *
          (
            1 -
            p_s
          ),
          x_s
        ) /
        sum(
          scenario_weights
        )
      )

    sign_s <-
      scenarios$contrast_sign[[s]]

    contrast_probability <-
      contrast_probability +
      sign_s *
      avg_p

    contrast_gradient <-
      contrast_gradient +
      sign_s *
      gradient_s
  }

  vc <-
    stats::vcov(fit)

  vc <-
    vc[
      beta_names,
      beta_names,
      drop = FALSE
    ]

  variance <-
    as.numeric(
      t(
        contrast_gradient
      ) %*%
      vc %*%
      contrast_gradient
    )

  se_probability <- if (
    is.finite(
      variance
    ) &&
    variance >= 0
  ) {
    sqrt(
      variance
    )
  } else {
    NA_real_
  }

  z <- stats::qnorm(
    1 -
    (
      1 -
      RESPONDENT_CONFIDENCE_LEVEL
    ) /
    2
  )

  estimate_pp <-
    100 *
    contrast_probability

  se_pp <-
    100 *
    se_probability

  tibble::tibble(
    contrast_estimate =
      estimate_pp,
    contrast_conf_low =
      estimate_pp -
      z *
      se_pp,
    contrast_conf_high =
      estimate_pp +
      z *
      se_pp,
    ci_excludes_zero =
      is.finite(
        estimate_pp -
          z *
          se_pp
      ) &&
      is.finite(
        estimate_pp +
          z *
          se_pp
      ) &&
      (
        estimate_pp -
          z *
          se_pp >
          0 ||
        estimate_pp +
          z *
          se_pp <
          0
      ),
    contrast_standard_error =
      se_pp,
    contrast_ci_method =
      "Probability-scale g-computation; cluster-robust delta method over slope coefficients with fixed effects treated as nuisance offsets"
  )
}

fit_logit_respondent_spec <- function(
    spec,
    fit_data
) {
  formulas <-
    make_respondent_formulas(
      spec
    )

  warnings_captured <-
    character(0)

  fit <- withCallingHandlers(
    tryCatch(
      {
        fixest::feglm(
          formulas$full_formula,
          data = fit_data,
          weights =
            fit_data[[
              spec$weight_var
            ]],
          family =
            stats::binomial(
              link = "logit"
            ),
          vcov =
            stats::as.formula(
              spec$cluster_formula
            ),
          notes = FALSE,
          warn = FALSE
        )
      },
      error =
        function(e) {
          e
        }
    ),
    warning =
      function(w) {
        warnings_captured <<-
          c(
            warnings_captured,
            conditionMessage(w)
          )

        invokeRestart(
          "muffleWarning"
        )
      }
  )

  if (
    inherits(
      fit,
      "error"
    )
  ) {
    return(
      list(
        fit = NULL,
        result =
          tibble::tibble(
            fit_ok = FALSE,
            interaction_term =
              NA_character_,
            interaction_estimate =
              NA_real_,
            interaction_se =
              NA_real_,
            interaction_p =
              NA_real_,
            interaction_conf_low =
              NA_real_,
            interaction_conf_high =
              NA_real_,
            contrast_estimate =
              NA_real_,
            contrast_conf_low =
              NA_real_,
            contrast_conf_high =
              NA_real_,
            ci_excludes_zero =
              FALSE,
            nobs =
              NA_integer_,
            weighted_ess =
              NA_real_,
            n_acs =
              NA_integer_,
            n_pcs =
              NA_integer_,
            n_districts =
              NA_integer_,
            r2 =
              NA_real_,
            formula =
              paste(
                deparse(
                  formulas$full_formula
                ),
                collapse = " "
              ),
            converged =
              FALSE,
            separation_warning =
              grepl(
                "separ|perfect|fitted probabilities",
                conditionMessage(fit),
                ignore.case = TRUE
              ),
            n_dropped =
              NA_integer_,
            warning =
              paste(
                unique(
                  warnings_captured
                ),
                collapse = " | "
              ),
            error =
              conditionMessage(fit),
            contrast_ci_method =
              NA_character_
          )
      )
    )
  }

  target_variables <- if (
    spec$interaction_order ==
    "two_way"
  ) {
    c(
      spec$exposure_var,
      spec$moderator_var
    )
  } else {
    c(
      spec$exposure_var,
      spec$moderator_var,
      spec$center_var
    )
  }

  target_term <-
    interaction_term_name_respondent(
      fit,
      target_variables
    )

  raw_estimate <- if (
    is.na(
      target_term
    )
  ) {
    NA_real_
  } else {
    unname(
      stats::coef(fit)[
        target_term
      ]
    )
  }

  raw_se <- if (
    is.na(
      target_term
    )
  ) {
    NA_real_
  } else {
    unname(
      fixest::se(fit)[
        target_term
      ]
    )
  }

  raw_p <- if (
    is.na(
      target_term
    )
  ) {
    NA_real_
  } else {
    unname(
      fixest::pvalue(fit)[
        target_term
      ]
    )
  }

  gcomp <- tryCatch(
    logit_probability_contrast(
      fit = fit,
      spec = spec,
      fit_data =
        fit_data
    ),
    error =
      function(e) {
        e
      }
  )

  separation_text <- paste(
    warnings_captured,
    collapse = " | "
  )

  separation_warning <-
    grepl(
      "separ|perfect|fitted probabilities",
      separation_text,
      ignore.case = TRUE
    )

  if (
    inherits(
      gcomp,
      "error"
    )
  ) {
    gcomp_row <-
      tibble::tibble(
        contrast_estimate =
          NA_real_,
        contrast_conf_low =
          NA_real_,
        contrast_conf_high =
          NA_real_,
        ci_excludes_zero =
          FALSE,
        contrast_standard_error =
          NA_real_,
        contrast_ci_method =
          NA_character_
      )

    gcomp_error <-
      conditionMessage(
        gcomp
      )
  } else {
    gcomp_row <-
      gcomp
    gcomp_error <-
      NA_character_
  }

  fit_weights <-
    fit_data[[
      spec$weight_var
    ]]

  result <-
    tibble::tibble(
      fit_ok =
        !inherits(
          gcomp,
          "error"
        ) &&
        is.finite(
          gcomp_row$contrast_estimate
        ),
      interaction_term =
        target_term,
      interaction_estimate =
        raw_estimate,
      interaction_se =
        raw_se,
      interaction_p =
        raw_p,
      interaction_conf_low =
        NA_real_,
      interaction_conf_high =
        NA_real_,
      nobs =
        stats::nobs(fit),
      weighted_ess =
        weighted_ess_respondent(
          fit_weights
        ),
      n_acs =
        dplyr::n_distinct(
          fit_data$ac_uid
        ),
      n_pcs =
        dplyr::n_distinct(
          fit_data$pc_cluster_id
        ),
      n_districts =
        dplyr::n_distinct(
          fit_data$district_harmonization_group_id
        ),
      r2 =
        respondent_fit_r2(
          fit
        ),
      formula =
        paste(
          deparse(
            formulas$full_formula
          ),
          collapse = " "
        ),
      converged =
        TRUE,
      separation_warning =
        separation_warning,
      n_dropped =
        nrow(fit_data) -
        stats::nobs(fit),
      warning =
        paste(
          unique(
            warnings_captured
          ),
          collapse = " | "
        ),
      error =
        gcomp_error
    ) |>
    dplyr::bind_cols(
      gcomp_row
    )

  list(
    fit = fit,
    result = result
  )
}

# ============================================================
# 13. SINGLE-SPEC FIT WRAPPER
# ============================================================

fit_one_respondent_spec <- function(
    spec
) {
  if (
    !isTRUE(
      spec$reference_valid
    )
  ) {
    return(
      tibble::tibble(
        spec_key =
          spec$spec_key,
        fit_ok =
          FALSE,
        interaction_term =
          NA_character_,
        interaction_estimate =
          NA_real_,
        interaction_se =
          NA_real_,
        interaction_p =
          NA_real_,
        interaction_conf_low =
          NA_real_,
        interaction_conf_high =
          NA_real_,
        contrast_estimate =
          NA_real_,
        contrast_conf_low =
          NA_real_,
        contrast_conf_high =
          NA_real_,
        ci_excludes_zero =
          FALSE,
        nobs =
          NA_integer_,
        weighted_ess =
          NA_real_,
        n_acs =
          NA_integer_,
        n_pcs =
          NA_integer_,
        n_districts =
          NA_integer_,
        r2 =
          NA_real_,
        formula =
          NA_character_,
        converged =
          NA,
        separation_warning =
          FALSE,
        n_dropped =
          NA_integer_,
        warning =
          NA_character_,
        error =
          "Invalid or zero-width substantive contrast reference.",
        contrast_ci_method =
          NA_character_
      )
    )
  }

  fit_data <- tryCatch(
    prepare_respondent_fit_data(
      spec
    ),
    error =
      function(e) {
        e
      }
  )

  if (
    inherits(
      fit_data,
      "error"
    )
  ) {
    return(
      tibble::tibble(
        spec_key =
          spec$spec_key,
        fit_ok =
          FALSE,
        interaction_term =
          NA_character_,
        interaction_estimate =
          NA_real_,
        interaction_se =
          NA_real_,
        interaction_p =
          NA_real_,
        interaction_conf_low =
          NA_real_,
        interaction_conf_high =
          NA_real_,
        contrast_estimate =
          NA_real_,
        contrast_conf_low =
          NA_real_,
        contrast_conf_high =
          NA_real_,
        ci_excludes_zero =
          FALSE,
        nobs =
          NA_integer_,
        weighted_ess =
          NA_real_,
        n_acs =
          NA_integer_,
        n_pcs =
          NA_integer_,
        n_districts =
          NA_integer_,
        r2 =
          NA_real_,
        formula =
          NA_character_,
        converged =
          NA,
        separation_warning =
          FALSE,
        n_dropped =
          NA_integer_,
        warning =
          NA_character_,
        error =
          conditionMessage(
            fit_data
          ),
        contrast_ci_method =
          NA_character_
      )
    )
  }

  if (
    nrow(fit_data) == 0
  ) {
    return(
      tibble::tibble(
        spec_key =
          spec$spec_key,
        fit_ok =
          FALSE,
        interaction_term =
          NA_character_,
        interaction_estimate =
          NA_real_,
        interaction_se =
          NA_real_,
        interaction_p =
          NA_real_,
        interaction_conf_low =
          NA_real_,
        interaction_conf_high =
          NA_real_,
        contrast_estimate =
          NA_real_,
        contrast_conf_low =
          NA_real_,
        contrast_conf_high =
          NA_real_,
        ci_excludes_zero =
          FALSE,
        nobs =
          0L,
        weighted_ess =
          0,
        n_acs =
          0L,
        n_pcs =
          0L,
        n_districts =
          0L,
        r2 =
          NA_real_,
        formula =
          NA_character_,
        converged =
          NA,
        separation_warning =
          FALSE,
        n_dropped =
          NA_integer_,
        warning =
          NA_character_,
        error =
          "No complete observations for specification.",
        contrast_ci_method =
          NA_character_
      )
    )
  }

  fit_result <- if (
    spec$estimator ==
    "logit"
  ) {
    fit_logit_respondent_spec(
      spec = spec,
      fit_data =
        fit_data
    )
  } else {
    fit_lpm_respondent_spec(
      spec = spec,
      fit_data =
        fit_data
    )
  }

  dplyr::bind_cols(
    tibble::tibble(
      spec_key =
        spec$spec_key
    ),
    fit_result$result
  )
}

# ============================================================
# 14. CURVE RUNNER WITH CHECKPOINTING
# ============================================================

respondent_curve_id <- function(
    family_row,
    design_row,
    fdi_family,
    interaction_order
) {
  paste(
    family_row$analysis_family,
    design_row$design_id,
    fdi_family,
    interaction_order,
    RESPONDENT_RUN_MODE,
    RESPONDENT_SCRIPT_REVISION,
    sep = "__"
  )
}

run_respondent_curve <- function(
    family_row,
    design_row,
    fdi_family,
    interaction_order
) {
  grid <-
    make_respondent_curve_grid(
      design_row =
        design_row,
      fdi_family =
        fdi_family,
      interaction_order =
        interaction_order,
      family_row =
        family_row
    )

  if (
    nrow(grid) == 0
  ) {
    return(
      tibble::tibble()
    )
  }

  curve_id <-
    respondent_curve_id(
      family_row =
        family_row,
      design_row =
        design_row,
      fdi_family =
        fdi_family,
      interaction_order =
        interaction_order
    )

  final_path <- file.path(
    respondent_result_dir,
    paste0(
      curve_id,
      ".rds"
    )
  )

  csv_path <- file.path(
    respondent_result_dir,
    paste0(
      curve_id,
      ".csv"
    )
  )

  partial_path <- file.path(
    respondent_result_dir,
    paste0(
      curve_id,
      "__partial.rds"
    )
  )

  if (
    file.exists(
      final_path
    ) &&
    !RESPONDENT_OVERWRITE_EXISTING
  ) {
    message(
      "Loading cached respondent curve: ",
      curve_id
    )

    return(
      readRDS(
        final_path
      )
    )
  }

  existing <-
    tibble::tibble()

  if (
    file.exists(
      partial_path
    ) &&
    !RESPONDENT_OVERWRITE_EXISTING
  ) {
    existing <-
      readRDS(
        partial_path
      )

    message(
      "Resuming ",
      curve_id,
      " from ",
      nrow(existing),
      " completed specifications."
    )
  }

  completed_keys <- if (
    nrow(existing) > 0
  ) {
    existing$spec_key
  } else {
    character(0)
  }

  remaining <- grid |>
    dplyr::filter(
      !spec_key %in%
        completed_keys
    )

  message(
    "Running respondent curve ",
    curve_id,
    ": ",
    nrow(remaining),
    " remaining of ",
    nrow(grid),
    "."
  )

  new_results <-
    vector(
      "list",
      nrow(remaining)
    )

  for (
    i in seq_len(
      nrow(remaining)
    )
  ) {
    spec <-
      remaining[
        i,
        ,
        drop = FALSE
      ]

    result <-
      fit_one_respondent_spec(
        spec
      )

    new_results[[i]] <-
      dplyr::bind_cols(
        spec,
        result |>
          dplyr::select(
            -spec_key
          )
      )

    if (
      i %%
        RESPONDENT_CHECKPOINT_EVERY ==
        0L ||
      i ==
        nrow(remaining)
    ) {
      checkpoint <-
        dplyr::bind_rows(
          existing,
          dplyr::bind_rows(
            new_results[
              seq_len(i)
            ]
          )
        )

      saveRDS(
        checkpoint,
        partial_path
      )

      message(
        "  respondent checkpoint ",
        i,
        "/",
        nrow(remaining),
        " for ",
        curve_id
      )
    }
  }

  final_result <-
    dplyr::bind_rows(
      existing,
      dplyr::bind_rows(
        new_results
      )
    ) |>
    dplyr::arrange(
      spec_key
    )

  saveRDS(
    final_result,
    final_path
  )

  readr::write_csv(
    final_result,
    csv_path
  )

  if (
    file.exists(
      partial_path
    )
  ) {
    unlink(
      partial_path
    )
  }

  final_result
}

# ============================================================
# 15. PLANNED COUNTS AND RUN MANIFEST
# ============================================================

planned_rows <- list()
planned_index <- 0L

for (
  family_name in RUN_ANALYSIS_FAMILIES
) {
  family_row <-
    respondent_analysis_family_meta |>
    dplyr::filter(
      analysis_family ==
        family_name
    )

  for (
    d in seq_len(
      nrow(
        respondent_design_meta
      )
    )
  ) {
    design_row <-
      respondent_design_meta[
        d,
        ,
        drop = FALSE
      ]

    for (
      fdi_family in c(
        "total",
        "mfg",
        "services"
      )
    ) {
      for (
        order in c(
          "two_way",
          "triple"
        )
      ) {
        grid <-
          make_respondent_curve_grid(
            design_row =
              design_row,
            fdi_family =
              fdi_family,
            interaction_order =
              order,
            family_row =
              family_row
          )

        if (
          nrow(grid) > 0
        ) {
          planned_index <-
            planned_index +
            1L

          planned_rows[[
            planned_index
          ]] <-
            tibble::tibble(
              analysis_family =
                family_name,
              design_id =
                design_row$design_id,
              moderator_domain =
                design_row$moderator_domain,
              fdi_family =
                fdi_family,
              interaction_order =
                order,
              planned_models =
                nrow(grid)
            )
        }
      }
    }
  }
}

respondent_planned_counts <-
  dplyr::bind_rows(
    planned_rows
  )

readr::write_csv(
  respondent_planned_counts,
  file.path(
    respondent_manifest_dir,
    paste0(
      "respondent_planned_counts_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

message(
  "Planned respondent models in ",
  RESPONDENT_RUN_MODE,
  " mode for requested families: ",
  format(
    sum(
      respondent_planned_counts$planned_models
    ),
    big.mark = ","
  )
)

# ============================================================
# 16. RUN REQUESTED CURVES
# ============================================================

all_respondent_results <- list()
respondent_result_index <- 0L

for (
  family_name in RUN_ANALYSIS_FAMILIES
) {
  family_row <-
    respondent_analysis_family_meta |>
    dplyr::filter(
      analysis_family ==
        family_name
    )

  for (
    d in seq_len(
      nrow(
        respondent_design_meta
      )
    )
  ) {
    design_row <-
      respondent_design_meta[
        d,
        ,
        drop = FALSE
      ]

    for (
      fdi_family in c(
        "total",
        "mfg",
        "services"
      )
    ) {
      for (
        order in c(
          "two_way",
          "triple"
        )
      ) {
        result <-
          run_respondent_curve(
            family_row =
              family_row,
            design_row =
              design_row,
            fdi_family =
              fdi_family,
            interaction_order =
              order
          )

        if (
          nrow(result) > 0
        ) {
          respondent_result_index <-
            respondent_result_index +
            1L

          all_respondent_results[[
            respondent_result_index
          ]] <- result
        }
      }
    }
  }
}

respondent_results_all <-
  dplyr::bind_rows(
    all_respondent_results
  )

readr::write_csv(
  respondent_results_all,
  file.path(
    respondent_summary_dir,
    paste0(
      "respondent_results_all_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 17. CURVE SUMMARIES
# ============================================================

summarize_respondent_curve <- function(
    data
) {
  valid <- data |>
    dplyr::filter(
      fit_ok,
      is.finite(
        contrast_estimate
      )
    )

  tibble::tibble(
    n_planned =
      nrow(data),
    n_fit =
      nrow(valid),
    n_failed =
      nrow(data) -
      nrow(valid),
    median_contrast =
      if (
        nrow(valid) > 0
      ) {
        stats::median(
          valid$contrast_estimate,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    share_positive =
      if (
        nrow(valid) > 0
      ) {
        mean(
          valid$contrast_estimate >
            0,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    share_ci_positive =
      if (
        nrow(valid) > 0
      ) {
        mean(
          valid$contrast_conf_low >
            0,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    share_ci_negative =
      if (
        nrow(valid) > 0
      ) {
        mean(
          valid$contrast_conf_high <
            0,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    median_n =
      if (
        nrow(valid) > 0
      ) {
        stats::median(
          valid$nobs,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    median_weighted_ess =
      if (
        nrow(valid) > 0
      ) {
        stats::median(
          valid$weighted_ess,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    median_pcs =
      if (
        nrow(valid) > 0
      ) {
        stats::median(
          valid$n_pcs,
          na.rm = TRUE
        )
      } else {
        NA_real_
      }
  )
}

respondent_curve_summaries <-
  respondent_results_all |>
  dplyr::group_by(
    analysis_family,
    design_id,
    design_type,
    design_label,
    moderator_domain,
    fdi_family,
    fdi_family_label,
    interaction_order,
    estimator,
    outcome_sample
  ) |>
  dplyr::group_modify(
    ~summarize_respondent_curve(
      .x
    )
  ) |>
  dplyr::ungroup()

readr::write_csv(
  respondent_curve_summaries,
  file.path(
    respondent_summary_dir,
    paste0(
      "respondent_curve_summaries_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

respondent_failure_summary <-
  respondent_results_all |>
  dplyr::filter(
    !fit_ok
  ) |>
  dplyr::count(
    analysis_family,
    design_id,
    fdi_family,
    interaction_order,
    error,
    sort = TRUE,
    name =
      "n_failures"
  )

readr::write_csv(
  respondent_failure_summary,
  file.path(
    respondent_log_dir,
    paste0(
      "respondent_failure_summary_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 18. PRIMARY SPECIFICATION-CURVE FIGURES
# ============================================================

prepare_respondent_curve_plot_data <- function(
    results
) {
  out <- results |>
    dplyr::filter(
      analysis_family ==
        "primary",
      fit_ok,
      is.finite(
        contrast_estimate
      )
    ) |>
    dplyr::arrange(
      contrast_estimate,
      spec_key
    ) |>
    dplyr::mutate(
      curve_order =
        dplyr::row_number()
    )

  if (
    nrow(out) == 0
  ) {
    return(
      out |>
        dplyr::mutate(
          curve_percentile =
            numeric(0)
        )
    )
  }

  out |>
    dplyr::mutate(
      curve_percentile =
        if (
          dplyr::n() ==
          1L
        ) {
          rep(
            50,
            dplyr::n()
          )
        } else {
          100 *
          (
            curve_order -
            1
          ) /
          (
            dplyr::n() -
            1
          )
        }
    )
}

respondent_curve_y_label <- function(
    interaction_order,
    moderator_domain
) {
  domain_label <- if (
    moderator_domain ==
    "muslim"
  ) {
    "Muslim exposure"
  } else {
    "migration/compositional exposure"
  }

  if (
    interaction_order ==
    "two_way"
  ) {
    paste0(
      "Difference in FDI effect: high vs low ",
      domain_label,
      "\nBJP-voting probability (percentage points)"
    )
  } else {
    paste0(
      "Center amplification of FDI x ",
      domain_label,
      " contrast\nBJP-voting probability (percentage points)"
    )
  }
}

respondent_matrix_rows <- function(
    plot_data
) {
  dplyr::bind_rows(
    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "FDI geography: ",
            fdi_scope_label
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "FDI status: ",
            fdi_status_label
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "FDI form: ",
            fdi_form_label
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "Moderator family: ",
            moderator_family
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "Moderator form: ",
            moderator_form
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "Voter controls: ",
            voter_control_set
          )
      ),

    plot_data |>
      dplyr::transmute(
        curve_percentile,
        matrix_row =
          paste0(
            "Context controls: ",
            context_control_set
          )
      )
  ) |>
    dplyr::mutate(
      matrix_row =
        factor(
          matrix_row,
          levels =
            rev(
              unique(
                matrix_row
              )
            )
        )
    )
}

draw_respondent_curve_pdf <- function(
    results
) {
  plot_data <-
    prepare_respondent_curve_plot_data(
      results
    )

  if (
    nrow(plot_data) == 0
  ) {
    return(
      invisible(NULL)
    )
  }

  median_estimate <-
    stats::median(
      plot_data$contrast_estimate,
      na.rm = TRUE
    )

  p_top <-
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = curve_percentile,
        y = contrast_estimate
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept =
        median_estimate,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin =
          contrast_conf_low,
        ymax =
          contrast_conf_high
      ),
      linewidth = 0.12,
      alpha = 0.15
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape =
          ci_excludes_zero
      ),
      size = 0.65
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        `FALSE` = 1,
        `TRUE` = 16
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        100
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y =
        respondent_curve_y_label(
          dplyr::first(
            plot_data$interaction_order
          ),
          dplyr::first(
            plot_data$moderator_domain
          )
        ),
      subtitle =
        paste0(
          format(
            nrow(plot_data),
            big.mark = ","
          ),
          " fitted specifications; median contrast = ",
          sprintf(
            "%.2f",
            median_estimate
          ),
          " pp; ",
          sprintf(
            "%.0f",
            100 *
            mean(
              plot_data$contrast_estimate >
                0
            )
          ),
          "% positive"
        )
    ) +
    ggplot2::theme_minimal(
      base_size = 10
    ) +
    ggplot2::theme(
      legend.position =
        "none",
      axis.text.x =
        ggplot2::element_blank(),
      axis.ticks.x =
        ggplot2::element_blank()
    )

  matrix_data <-
    respondent_matrix_rows(
      plot_data
    )

  p_bottom <-
    ggplot2::ggplot(
      matrix_data,
      ggplot2::aes(
        x = curve_percentile,
        y = matrix_row
      )
    ) +
    ggplot2::geom_point(
      size = 0.32,
      alpha = 0.7
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        100
      ),
      breaks = c(
        0,
        25,
        50,
        75,
        100
      )
    ) +
    ggplot2::labs(
      x =
        "Specification rank percentile (lowest to highest substantive contrast)",
      y = NULL
    ) +
    ggplot2::theme_minimal(
      base_size = 8
    ) +
    ggplot2::theme(
      panel.grid.minor =
        ggplot2::element_blank(),
      axis.text.y =
        ggplot2::element_text(
          size = 6.5
        )
    )

  title <- paste(
    dplyr::first(
      plot_data$design_label
    ),
    dplyr::first(
      plot_data$fdi_family_label
    ),
    if (
      dplyr::first(
        plot_data$interaction_order
      ) ==
      "two_way"
    ) {
      "Two-way interaction"
    } else {
      "Triple interaction: harmonized Center amplification"
    },
    sep = " | "
  )

  filename <- paste0(
    "respondent_primary__",
    dplyr::first(
      plot_data$design_id
    ),
    "__",
    dplyr::first(
      plot_data$fdi_family
    ),
    "__",
    dplyr::first(
      plot_data$interaction_order
    ),
    "__",
    RESPONDENT_RUN_MODE,
    "__",
    RESPONDENT_SCRIPT_REVISION,
    ".pdf"
  )

  grDevices::pdf(
    file.path(
      respondent_figure_dir,
      filename
    ),
    width = 12,
    height = 8.5
  )

  grid::grid.newpage()

  layout <-
    grid::grid.layout(
      nrow = 3,
      ncol = 1,
      heights =
        grid::unit(
          c(
            0.10,
            0.58,
            0.32
          ),
          "null"
        )
    )

  grid::pushViewport(
    grid::viewport(
      layout = layout
    )
  )

  grid::grid.text(
    title,
    vp =
      grid::viewport(
        layout.pos.row = 1
      ),
    gp =
      grid::gpar(
        fontsize = 13,
        fontface = "bold"
      )
  )

  print(
    p_top,
    vp =
      grid::viewport(
        layout.pos.row = 2
      )
  )

  print(
    p_bottom,
    vp =
      grid::viewport(
        layout.pos.row = 3
      )
  )

  grid::popViewport()

  grDevices::dev.off()

  invisible(
    filename
  )
}

primary_plot_groups <-
  respondent_results_all |>
  dplyr::filter(
    analysis_family ==
      "primary"
  ) |>
  dplyr::group_split(
    design_id,
    fdi_family,
    interaction_order
  )

purrr::walk(
  primary_plot_groups,
  draw_respondent_curve_pdf
)

# ============================================================
# 19. THEORY-PREFERRED MODEL EXTRACTION
# ============================================================

preferred_respondent_rows <-
  respondent_results_all |>
  dplyr::filter(
    analysis_family ==
      "primary",
    respondent_model_preferred,
    moderator_domain ==
      "muslim",
    moderator_var ==
      "muslim_share_2001_dist_proxy"
  ) |>
  dplyr::arrange(
    design_type,
    interaction_order
  )

readr::write_csv(
  preferred_respondent_rows,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_respondent_models_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 20. PREFERRED LPM PREDICTION-BOUND AUDIT
# ============================================================

prediction_bound_rows <- list()
prediction_bound_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES
) {
  for (
    i in seq_len(
      nrow(
        preferred_respondent_rows
      )
    )
  ) {
    spec <-
      preferred_respondent_rows[
        i,
        ,
        drop = FALSE
      ]

    fit_data <-
      prepare_respondent_fit_data(
        spec
      )

    fit_object <-
      fit_lpm_respondent_spec(
        spec,
        fit_data
      )$fit

    if (
      !is.null(
        fit_object
      )
    ) {
      fitted_probability <-
        as.numeric(
          stats::predict(
            fit_object
          )
        )

      prediction_bound_index <-
        prediction_bound_index +
        1L

      prediction_bound_rows[[
        prediction_bound_index
      ]] <-
        tibble::tibble(
          design_id =
            spec$design_id,
          interaction_order =
            spec$interaction_order,
          n_predictions =
            length(
              fitted_probability
            ),
          n_below_zero =
            sum(
              fitted_probability <
                0,
              na.rm = TRUE
            ),
          share_below_zero =
            mean(
              fitted_probability <
                0,
              na.rm = TRUE
            ),
          n_above_one =
            sum(
              fitted_probability >
                1,
              na.rm = TRUE
            ),
          share_above_one =
            mean(
              fitted_probability >
                1,
              na.rm = TRUE
            ),
          min_fitted =
            min(
              fitted_probability,
              na.rm = TRUE
            ),
          max_fitted =
            max(
              fitted_probability,
              na.rm = TRUE
            )
        )
    }
  }
}

preferred_lpm_prediction_bounds <-
  dplyr::bind_rows(
    prediction_bound_rows
  )

readr::write_csv(
  preferred_lpm_prediction_bounds,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_lpm_prediction_bounds_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 21. TARGETED PC-ONLY CLUSTER SENSITIVITY
# ============================================================

cluster_sensitivity_rows <- list()
cluster_sensitivity_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES
) {
  for (
    i in seq_len(
      nrow(
        preferred_respondent_rows
      )
    )
  ) {
    spec <-
      preferred_respondent_rows[
        i,
        ,
        drop = FALSE
      ]

    for (
      cluster_label in c(
        "PC + district multiway",
        "PC only"
      )
    ) {
      spec_cluster <-
        spec

      spec_cluster$cluster_rule <-
        cluster_label

      spec_cluster$cluster_formula <-
        if (
          cluster_label ==
          "PC only"
        ) {
          "~pc_cluster_id"
        } else {
          "~pc_cluster_id + district_harmonization_group_id"
        }

      fit_data <-
        prepare_respondent_fit_data(
          spec_cluster
        )

      result <-
        fit_lpm_respondent_spec(
          spec_cluster,
          fit_data
        )$result

      cluster_sensitivity_index <-
        cluster_sensitivity_index +
        1L

      cluster_sensitivity_rows[[
        cluster_sensitivity_index
      ]] <-
        dplyr::bind_cols(
          tibble::tibble(
            design_id =
              spec$design_id,
            interaction_order =
              spec$interaction_order,
            cluster_rule =
              cluster_label
          ),
          result
        )
    }
  }
}

preferred_cluster_sensitivity <-
  dplyr::bind_rows(
    cluster_sensitivity_rows
  )

readr::write_csv(
  preferred_cluster_sensitivity,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_cluster_sensitivity_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 22. TARGETED HINDU-ONLY MUSLIM SENSITIVITY
# ============================================================

# This is deliberately restricted to the theory-preferred Muslim specification
# rather than multiplied through the full curve.
hindu_only_rows <- list()
hindu_only_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES &&
  "religion_group" %in%
  names(respondents)
) {
  hindu_labels <-
    unique(
      as.character(
        respondents$religion_group
      )
    )

  hindu_label <-
    hindu_labels[
      grepl(
        "hindu",
        hindu_labels,
        ignore.case = TRUE
      )
    ]

  if (
    length(
      hindu_label
    ) >= 1
  ) {
    hindu_label <-
      hindu_label[[1]]

    for (
      i in seq_len(
        nrow(
          preferred_respondent_rows
        )
      )
    ) {
      spec <-
        preferred_respondent_rows[
          i,
          ,
          drop = FALSE
        ]

      # religion_group defines this restricted sample and therefore has only
      # one observed level. Do not also include it as a regression covariate.
      spec_hindu <-
        spec

      spec_hindu$voter_control_string <-
        spec_hindu$voter_control_string |>
        strsplit(
          " \\+ "
        ) |>
        unlist() |>
        trimws() |>
        setdiff(
          "religion_group"
        ) |>
        paste(
          collapse = " + "
        )

      fit_data <-
        prepare_respondent_fit_data(
          spec_hindu
        ) |>
        dplyr::filter(
          as.character(
            religion_group
          ) ==
            hindu_label
        )

      result <-
        fit_lpm_respondent_spec(
          spec_hindu,
          fit_data
        )$result

      hindu_only_index <-
        hindu_only_index +
        1L

      hindu_only_rows[[
        hindu_only_index
      ]] <-
        dplyr::bind_cols(
          tibble::tibble(
            design_id =
              spec$design_id,
            interaction_order =
              spec$interaction_order,
            respondent_subsample =
              paste0(
                "Hindu only: ",
                hindu_label
              )
          ),
          result
        )
    }
  }
}

preferred_hindu_only_sensitivity <-
  dplyr::bind_rows(
    hindu_only_rows
  )

readr::write_csv(
  preferred_hindu_only_sensitivity,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_hindu_only_sensitivity_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 23. TARGETED 80%-ITEM CENTER SENSITIVITY
# ============================================================

# The 80%-item rule is deliberately NOT a full multiverse dimension. It is used
# only for the theory-preferred pooled triple model to check that the result is
# not specific to the axis-aware harmonization rule.
center80_rows <- list()
center80_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES
) {
  center80_source_specs <-
    respondent_results_all |>
    dplyr::filter(
      analysis_family ==
        "primary",
      design_id ==
        "respondent_pooled_muslim",
      interaction_order ==
        "triple",
      fdi_family ==
        "mfg",
      fdi_scope ==
        "local",
      fdi_status ==
        "all",
      fdi_form ==
        "log1p_pc100k",
      moderator_var ==
        "muslim_share_2001_dist_proxy",
      voter_control_set ==
        "V2",
      context_control_set ==
        "C1"
    ) |>
    dplyr::slice(1)

  if (
    RESPONDENT_RUN_MODE ==
      "pilot" &&
    nrow(
      center80_source_specs
    ) != 1
  ) {
    stop(
      "Pilot could not uniquely resolve the preferred pooled Muslim triple ",
      "for the 80%-Center sensitivity."
    )
  }

  for (
    i in seq_len(
      nrow(
        center80_source_specs
      )
    )
  ) {
    spec <-
      center80_source_specs[
        i,
        ,
        drop = FALSE
      ]

    spec$center_var <-
      "center_relaxed_80"

    spec$center_definition <-
      "80%-of-items Center sensitivity"

    # Reconstruct the pooled triple equal-election weight on the same
    # ideology-complete candidate-present base sample; only the moderator
    # definition changes.
    fit_data <-
      prepare_respondent_fit_data(
        spec
      )

    result <-
      fit_lpm_respondent_spec(
        spec,
        fit_data
      )$result

    center80_index <-
      center80_index +
      1L

    center80_rows[[
      center80_index
    ]] <-
      dplyr::bind_cols(
        tibble::tibble(
          design_id =
            spec$design_id,
          interaction_order =
            spec$interaction_order,
          center_definition =
            spec$center_definition
        ),
        result
      )
  }
}

preferred_center80_sensitivity <-
  dplyr::bind_rows(
    center80_rows
  )

readr::write_csv(
  preferred_center80_sensitivity,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_center80_sensitivity_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 24. 2014 FOUR-CATEGORY IDEOLOGY DECOMPOSITION
# ============================================================

# Fit one joint preferred 2014 model with Mixed/Center/Left/Right ideology, then
# calculate the FDI x Muslim-share probability contrast separately at each
# ideology category. This is a mechanism/decomposition analysis, not a curve
# dimension.
four_category_rows <- list()
four_category_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES
) {
  preferred_2014_triple <-
    preferred_respondent_rows |>
    dplyr::filter(
      design_type ==
        "baseline_2014",
      interaction_order ==
        "triple"
    ) |>
    dplyr::slice(1)

  if (
    nrow(
      preferred_2014_triple
    ) == 1
  ) {
    spec <-
      preferred_2014_triple

    # Build the preferred candidate-present, ideology-complete sample and keep
    # all four observed ideology categories.
    fit_data <-
      prepare_respondent_fit_data(
        spec
      ) |>
      dplyr::filter(
        !is.na(
          voter_ideology
        ),
        as.character(
          voter_ideology
        ) %in%
          c(
            "Mixed",
            "Center",
            "Left",
            "Right"
          )
      ) |>
      dplyr::mutate(
        voter_ideology_four =
          factor(
            as.character(
              voter_ideology
            ),
            levels = c(
              "Mixed",
              "Center",
              "Left",
              "Right"
            )
          )
      )

    rhs <- c(
      paste0(
        spec$exposure_var,
        " * ",
        spec$moderator_var,
        " * voter_ideology_four"
      ),
      spec$baseline_fdi_var,
      "bjp_vote_share_2009",
      spec$voter_control_string,
      spec$context_control_string
    )

    rhs <-
      rhs[
        !is.na(rhs) &
          nzchar(rhs)
      ]

    rhs_formula <-
      stats::as.formula(
        paste0(
          "~ ",
          paste(
            rhs,
            collapse = " + "
          )
        )
      )

    full_formula <-
      stats::as.formula(
        paste0(
          "voted_bjp ~ ",
          paste(
            rhs,
            collapse = " + "
          ),
          " | state_no"
        )
      )

    fit_four <- tryCatch(
      fixest::feols(
        full_formula,
        data = fit_data,
        weights =
          fit_data[[
            spec$weight_var
          ]],
        vcov =
          ~pc_cluster_id +
          district_harmonization_group_id,
        notes = FALSE,
        warn = FALSE
      ),
      error =
        function(e) {
          e
        }
    )

    if (
      !inherits(
        fit_four,
        "error"
      )
    ) {
      beta <-
        stats::coef(
          fit_four
        )

      beta_names <-
        names(beta)

      x_obs <-
        model_matrix_for_fit(
          rhs_formula =
            rhs_formula,
          data =
            fit_data,
          beta_names =
            beta_names
        )

      eta_obs <-
        as.numeric(
          stats::predict(
            fit_four
          )
        )

      offset <-
        eta_obs -
        as.numeric(
          x_obs %*%
          beta
        )

      weights_four <-
        fit_data[[
          spec$weight_var
        ]]

      for (
        ideology_level in c(
          "Mixed",
          "Center",
          "Left",
          "Right"
        )
      ) {
        scenario_definitions <-
          tibble::tribble(
            ~fdi_value,
            ~moderator_value,
            ~contrast_sign,

            spec$fdi_high,
            spec$moderator_high,
            1,

            spec$fdi_low,
            spec$moderator_high,
            -1,

            spec$fdi_high,
            spec$moderator_low,
            -1,

            spec$fdi_low,
            spec$moderator_low,
            1
          )

        contrast <-
          0

        gradient <-
          rep(
            0,
            length(beta)
          )

        for (
          s in seq_len(
            nrow(
              scenario_definitions
            )
          )
        ) {
          scenario_data <-
            fit_data

          scenario_data[[
            spec$exposure_var
          ]] <-
            scenario_definitions$fdi_value[[s]]

          scenario_data[[
            spec$moderator_var
          ]] <-
            scenario_definitions$moderator_value[[s]]

          scenario_data$voter_ideology_four <-
            factor(
              ideology_level,
              levels = c(
                "Mixed",
                "Center",
                "Left",
                "Right"
              )
            )

          x_s <-
            model_matrix_for_fit(
              rhs_formula =
                rhs_formula,
              data =
                scenario_data,
              beta_names =
                beta_names
            )

          pred_s <-
            offset +
            as.numeric(
              x_s %*%
              beta
            )

          avg_s <-
            stats::weighted.mean(
              pred_s,
              weights_four
            )

          grad_s <-
            as.numeric(
              crossprod(
                weights_four,
                x_s
              ) /
              sum(
                weights_four
              )
            )

          sign_s <-
            scenario_definitions$contrast_sign[[s]]

          contrast <-
            contrast +
            sign_s *
            avg_s

          gradient <-
            gradient +
            sign_s *
            grad_s
        }

        vc <-
          stats::vcov(
            fit_four
          )

        vc <-
          vc[
            beta_names,
            beta_names,
            drop = FALSE
          ]

        variance <-
          as.numeric(
            t(gradient) %*%
              vc %*%
              gradient
          )

        se <-
          if (
            is.finite(
              variance
            ) &&
            variance >= 0
          ) {
            sqrt(
              variance
            )
          } else {
            NA_real_
          }

        z <-
          stats::qnorm(
            1 -
              (
                1 -
                RESPONDENT_CONFIDENCE_LEVEL
              ) /
              2
          )

        four_category_index <-
          four_category_index +
          1L

        four_category_rows[[
          four_category_index
        ]] <-
          tibble::tibble(
            ideology =
              ideology_level,
            contrast_estimate =
              100 *
              contrast,
            contrast_conf_low =
              100 *
              (
                contrast -
                z *
                se
              ),
            contrast_conf_high =
              100 *
              (
                contrast +
                z *
                se
              ),
            nobs =
              stats::nobs(
                fit_four
              ),
            formula =
              paste(
                deparse(
                  full_formula
                ),
                collapse = " "
              )
          )
      }
    }
  }
}

preferred_2014_four_category_ideology <-
  dplyr::bind_rows(
    four_category_rows
  )

readr::write_csv(
  preferred_2014_four_category_ideology,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_2014_four_category_ideology_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 25. COMMON-SAMPLE CONTROL ROBUSTNESS
# ============================================================

# For the theory-preferred FDI x 2001 Muslim-share model, estimate every V x C
# combination normally and again on the exact V3+C3-complete sample. This
# separates control adjustment from sample-composition changes.
common_sample_rows <- list()
common_sample_index <- 0L

if (
  "primary" %in%
  RUN_ANALYSIS_FAMILIES
) {
  preferred_fdi_row <-
    fdi_meta |>
    dplyr::filter(
      fdi_family ==
        "mfg",
      fdi_scope ==
        "local",
      fdi_status ==
        "all",
      fdi_form ==
        "log1p_pc100k"
    )

  preferred_muslim_row <-
    muslim_meta |>
    dplyr::filter(
      pooled_var ==
        "muslim_share_2001_dist_proxy"
    )

  for (
    d in seq_len(
      nrow(
        respondent_design_meta |>
          dplyr::filter(
            moderator_domain ==
              "muslim"
          )
      )
    )
  ) {
    design_row <-
      respondent_design_meta |>
      dplyr::filter(
        moderator_domain ==
          "muslim"
      ) |>
      dplyr::slice(d)

    for (
      order in c(
        "two_way",
        "triple"
      )
    ) {
      family_row <-
        respondent_analysis_family_meta |>
        dplyr::filter(
          analysis_family ==
            "primary"
        )

      full_grid <-
        make_respondent_curve_grid(
          design_row =
            design_row,
          fdi_family =
            "mfg",
          interaction_order =
            order,
          family_row =
            family_row
        ) |>
        dplyr::filter(
          fdi_scope ==
            "local",
          fdi_status ==
            "all",
          fdi_form ==
            "log1p_pc100k",
          moderator_var ==
            "muslim_share_2001_dist_proxy"
        )

      # Determine the common V3+C3 complete sample from the same design/order.
      v3c3_spec <-
        full_grid |>
        dplyr::filter(
          voter_control_set ==
            "V3",
          context_control_set ==
            "C3"
        ) |>
        dplyr::slice(1)

      if (
        nrow(
          v3c3_spec
        ) == 0
      ) {
        next
      }

      common_data <-
        prepare_respondent_fit_data(
          v3c3_spec
        )

      common_ids <-
        common_data$respondent_uid

      for (
        i in seq_len(
          nrow(
            full_grid
          )
        )
      ) {
        spec <-
          full_grid[
            i,
            ,
            drop = FALSE
          ]

        # Available-case fit.
        available_data <-
          prepare_respondent_fit_data(
            spec
          )

        available_result <-
          fit_lpm_respondent_spec(
            spec,
            available_data
          )$result

        common_sample_index <-
          common_sample_index +
          1L

        common_sample_rows[[
          common_sample_index
        ]] <-
          dplyr::bind_cols(
            tibble::tibble(
              design_id =
                spec$design_id,
              interaction_order =
                spec$interaction_order,
              voter_control_set =
                spec$voter_control_set,
              context_control_set =
                spec$context_control_set,
              sample_rule =
                "Available cases"
            ),
            available_result
          )

        # Same V3+C3-complete respondents, while each formula varies controls.
        common_fit_data <-
          available_data |>
          dplyr::filter(
            respondent_uid %in%
              common_ids
          )

        common_result <-
          fit_lpm_respondent_spec(
            spec,
            common_fit_data
          )$result

        common_sample_index <-
          common_sample_index +
          1L

        common_sample_rows[[
          common_sample_index
        ]] <-
          dplyr::bind_cols(
            tibble::tibble(
              design_id =
                spec$design_id,
              interaction_order =
                spec$interaction_order,
              voter_control_set =
                spec$voter_control_set,
              context_control_set =
                spec$context_control_set,
              sample_rule =
                "Common V3+C3-complete sample"
            ),
            common_result
          )
      }
    }
  }
}

preferred_common_sample_controls <-
  dplyr::bind_rows(
    common_sample_rows
  )

readr::write_csv(
  preferred_common_sample_controls,
  file.path(
    respondent_preferred_dir,
    paste0(
      "preferred_common_sample_controls_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

# ============================================================
# 26. TARGETED ROBUSTNESS HEALTH AUDIT
# ============================================================

targeted_robustness_health <-
  tibble::tibble(
    check = c(
      "Hindu-only preferred models",
      "80%-Center pooled preferred triple",
      "2014 four-category ideology decomposition",
      "Common-sample control fits"
    ),

    expected_rows = c(
      nrow(
        preferred_respondent_rows
      ),
      1L,
      4L,
      128L
    ),

    observed_rows = c(
      nrow(
        preferred_hindu_only_sensitivity
      ),
      nrow(
        preferred_center80_sensitivity
      ),
      nrow(
        preferred_2014_four_category_ideology
      ),
      nrow(
        preferred_common_sample_controls
      )
    ),

    n_failed = c(
      if (
        nrow(
          preferred_hindu_only_sensitivity
        ) > 0
      ) {
        sum(
          !preferred_hindu_only_sensitivity$fit_ok
        )
      } else {
        NA_integer_
      },

      if (
        nrow(
          preferred_center80_sensitivity
        ) > 0
      ) {
        sum(
          !preferred_center80_sensitivity$fit_ok
        )
      } else {
        NA_integer_
      },

      0L,

      if (
        nrow(
          preferred_common_sample_controls
        ) > 0
      ) {
        sum(
          !preferred_common_sample_controls$fit_ok
        )
      } else {
        NA_integer_
      }
    )
  ) |>
  dplyr::mutate(
    health_ok =
      observed_rows ==
        expected_rows &
      dplyr::coalesce(
        n_failed,
        1L
      ) ==
        0L
  )

readr::write_csv(
  targeted_robustness_health,
  file.path(
    respondent_log_dir,
    paste0(
      "targeted_robustness_health_",
      RESPONDENT_RUN_MODE,
      ".csv"
    )
  )
)

if (
  RESPONDENT_RUN_MODE ==
    "pilot" &&
  any(
    !targeted_robustness_health$health_ok
  )
) {
  bad_checks <-
    targeted_robustness_health |>
    dplyr::filter(
      !health_ok
    ) |>
    dplyr::pull(
      check
    )

  stop(
    "Pilot core curves completed, but targeted robustness health checks failed: ",
    paste(
      bad_checks,
      collapse = "; "
    )
  )
}

# ============================================================
# 27. CONSOLE SUMMARY
# ============================================================

message("")
message(
  "Respondent specification analysis complete."
)
message(
  "Revision: ",
  RESPONDENT_SCRIPT_REVISION
)
message(
  "Mode: ",
  RESPONDENT_RUN_MODE
)
message(
  "Families requested: ",
  paste(
    RUN_ANALYSIS_FAMILIES,
    collapse = ", "
  )
)
message(
  "Results: ",
  respondent_result_dir
)
message(
  "Summaries: ",
  respondent_summary_dir
)
message(
  "Figures: ",
  respondent_figure_dir
)
message(
  "Preferred-model outputs: ",
  respondent_preferred_dir
)
message(
  "Failure logs: ",
  respondent_log_dir
)
message("")
message(
  "Primary full-run target, when RESPONDENT_RUN_MODE='full' and family='primary': ",
  "165,888 weighted-LPM specifications."
)
message(
  "The complete frozen parallel universe across all families is 204,768 planned curve/parallel specifications."
)

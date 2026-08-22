# ============================================================
# 19_respondent_multilevel_hierarchy.R
# Pooled 2009/2014 unweighted voter-level LPM hierarchy
# Revision: 2026-08-16-v1.0
# ============================================================
#
# PURPOSE
# -------
# Fit a deliberately simple hierarchy of pooled voter-level linear probability
# models using the existing Switchers respondent analysis data:
#
#   M0. No random structure: ordinary unweighted LPM (lm)
#   M1. Voters nested in Assembly Constituencies:
#         random intercept for AC
#   M2. Voters nested in ACs nested in districts:
#         random intercept for district + random intercept for AC within district
#
# The FIXED part of each hierarchy is held identical within a substantive
# specification so any change in the focal coefficients is attributable to the
# geographic random-intercept structure rather than a changing covariate set.
#
# CURRENT DEFAULT SUBSTANTIVE SPECIFICATIONS
# ------------------------------------------
# Preferred FDI exposure:
#   log1p_fdi_mfg_local_all_pc100k
#
# Demographic moderators:
#   Muslim:    muslim_share_2001_dist_proxy
#   Migration: mig_total_upto_2001_share_ac_pop
#
# Interaction orders:
#   two_way: FDI x demographic context
#   triple:  FDI x demographic context x harmonized ideology bucket
#
# The triple moderator is the harmonized four-category ideology bucket:
#   Center / Left / Right / Mixed
# with Center as the reference category.
#
# 2009 harmonization:
#   pure Left/Center/Right requires 2/2 recognition items in that category and
#   at least 2/3 statism items in that category; otherwise Mixed.
# 2014 harmonization:
#   pure Left/Center/Right requires 2/2 recognition items and 1/1 statism item
#   in that category; otherwise Mixed.
#
# PRIMARY SAMPLE FOR THIS EXPLORATORY MULTILEVEL RUN
# --------------------------------------------------
#   - pooled 2009 + 2014 respondents
#   - valid BJP vote outcome
#   - BJP candidate present
#   - UNWEIGHTED
#   - V2 voter controls: religion + caste + education
#   - C1 contextual controls: AC population + land area + SC/ST shares
#   - state x year fixed effects, matching the existing pooled respondent design
#   - all three hierarchy models use the SAME complete-case sample within each
#     substantive specification
#
# IMPORTANT DISTRICT-MAPPING RULE
# -------------------------------
# If one ac_uid maps to more than one district_harmonization_group_id anywhere
# in the pooled candidate-present respondent data, that AC is excluded from ALL
# three hierarchy models. This is intentionally conservative and implements the
# requested strict AC-within-district nesting rule.
#
# INFERENCE NOTE
# --------------
# These are Gaussian LPMs. lmer does not supply design-based survey inference.
# This first pass is intentionally unweighted. For lmer fits, coefficient CSVs
# report estimate, model-based SE, t statistic, 95% normal-approximation CI, and
# a normal-approximation p value. Treat these as exploratory until a final
# inference strategy is chosen.
# ============================================================

# ============================================================
# 0. PROJECT + PACKAGES
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

required_extra_packages <- c(
  "lme4"
)

missing_extra_packages <- required_extra_packages[
  !vapply(
    required_extra_packages,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
]

if (length(missing_extra_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_extra_packages, collapse = ", "),
    ". Install with install.packages(c(\"",
    paste(missing_extra_packages, collapse = "\", \""),
    "\"))."
  )
}

paths <- build_project_paths(project_root)

SCRIPT_REVISION <- "2026-08-16-v1.0.1"

message(
  "Running respondent multilevel hierarchy revision: ",
  SCRIPT_REVISION
)

# ============================================================
# 1. USER-EDITABLE ANALYSIS SETTINGS
# ============================================================

# Pooled 2009/2014 only.
ANALYSIS_YEARS <- c(2009L, 2014L)

# Preferred manufacturing/local/all/log1p exposure used in the respondent work.
FDI_VAR <- "log1p_fdi_mfg_local_all_pc100k"

# Run both preferred demographic domains by default.
RUN_DOMAINS <- c(
  "muslim",
  "migration"
)

# Run both the two-way and full ideology-bucket triple specifications.
RUN_INTERACTION_ORDERS <- c(
  "two_way",
  "triple"
)

DOMAIN_VARS <- c(
  muslim = "muslim_share_2001_dist_proxy",
  migration = "mig_total_upto_2001_share_ac_pop"
)

DOMAIN_LABELS <- c(
  muslim = "Muslim share 2001",
  migration = "Established migrant share 2001"
)

# Existing preferred control blocks.
V2_VOTER_CONTROLS <- c(
  "religion_group",
  "caste_group",
  "education_harmonized"
)

C1_CONTEXT_CONTROLS <- c(
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share"
)

# Retain the pooled respondent design's state x year fixed effects in every
# hierarchy level. A single factor gives one intercept for every observed
# state-year cell and is the lm/lmer analogue of the existing fixest FE.
USE_STATE_YEAR_FIXED_EFFECTS <- TRUE

# Mixed models are estimated by ML rather than REML so AIC/BIC/logLik are on a
# common basis for comparing M1 and M2. This is an exploratory hierarchy check.
LMER_REML <- FALSE

# Conservative optimizer settings for the larger nested random-effects model.
LMER_CONTROL <- lme4::lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000)
)

# ============================================================
# 2. OUTPUT DIRECTORIES
# ============================================================

output_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "respondent_multilevel_hierarchy",
  SCRIPT_REVISION
)

output_csv <- file.path(output_root, "csv")
output_models <- file.path(output_root, "models")
output_logs <- file.path(output_root, "audit")

purrr::walk(
  c(
    output_root,
    output_csv,
    output_models,
    output_logs
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 3. LOAD EXISTING RESPONDENT DATA + IDEOLOGY ITEM DATA
# ============================================================

respondents <- readRDS(
  file.path(
    paths$final_dir,
    "nes_respondent_analysis.rds"
  )
)

ideology_items_long <- readRDS(
  file.path(
    paths$intermediate_dir,
    "ideology_item_responses_long.rds"
  )
)

message(
  "Loaded respondents: ",
  scales::comma(nrow(respondents))
)

# ============================================================
# 4. RECONSTRUCT SAMPLE FLAGS IF NEEDED
# ============================================================

required_sample_vars <- c(
  "year",
  "voted_bjp",
  "vote_valid",
  "bjp_candidate_present",
  "ac_uid",
  "district_harmonization_group_id",
  "state_no"
)

missing_sample_vars <- setdiff(
  required_sample_vars,
  names(respondents)
)

if (length(missing_sample_vars) > 0) {
  stop(
    "nes_respondent_analysis.rds is missing required variables: ",
    paste(missing_sample_vars, collapse = ", ")
  )
}

if (!"respondent_sample_candidate_present" %in% names(respondents)) {
  respondents <- respondents |>
    dplyr::mutate(
      respondent_sample_candidate_present =
        vote_valid &
        !is.na(voted_bjp) &
        !is.na(bjp_candidate_present) &
        bjp_candidate_present == 1
    )
}

# ============================================================
# 5. BUILD THE HARMONIZED FOUR-CATEGORY IDEOLOGY BUCKET
# ============================================================

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
  "response_bucket" %in% names(ideology_items_long) ~ "response_bucket",
  "item_bucket" %in% names(ideology_items_long) ~ "item_bucket",
  TRUE ~ NA_character_
)

if (is.na(bucket_col)) {
  stop(
    "Could not find response_bucket/item_bucket in ideology_item_responses_long.rds."
  )
}

required_ideology_key_vars <- c(
  "respondent_uid",
  "year",
  "item"
)

missing_ideology_key_vars <- setdiff(
  required_ideology_key_vars,
  names(ideology_items_long)
)

if (length(missing_ideology_key_vars) > 0) {
  stop(
    "ideology_item_responses_long.rds is missing: ",
    paste(missing_ideology_key_vars, collapse = ", ")
  )
}

if (!"respondent_uid" %in% names(respondents)) {
  stop(
    "nes_respondent_analysis.rds is missing respondent_uid, which is required ",
    "to reconstruct the harmonized full ideology bucket."
  )
}

# One row per respondent x item is required for a safe one-to-one wide join.
duplicate_ideology_rows <- ideology_items_long |>
  dplyr::filter(item %in% classification_items) |>
  dplyr::count(
    respondent_uid,
    year,
    item,
    name = "n"
  ) |>
  dplyr::filter(n > 1)

if (nrow(duplicate_ideology_rows) > 0) {
  readr::write_csv(
    duplicate_ideology_rows,
    file.path(
      output_logs,
      "ideology_item_duplicate_rows_ERROR.csv"
    )
  )
  stop(
    "Duplicate respondent x ideology-item rows found. See audit file."
  )
}

ideology_buckets_wide <- ideology_items_long |>
  dplyr::filter(item %in% classification_items) |>
  dplyr::transmute(
    respondent_uid,
    year,
    item,
    item_bucket = as.character(.data[[bucket_col]])
  ) |>
  tidyr::pivot_wider(
    names_from = item,
    values_from = item_bucket,
    names_glue = "ideology_{item}_bucket"
  )

n_before_ideology_join <- nrow(respondents)

respondents <- respondents |>
  dplyr::left_join(
    ideology_buckets_wide,
    by = c(
      "respondent_uid",
      "year"
    ),
    relationship = "one-to-one"
  )

if (nrow(respondents) != n_before_ideology_join) {
  stop(
    "Ideology item join changed respondent row count."
  )
}

# Helper: number of items in a row equal to one ideology category.
count_bucket_matches <- function(...) {
  x <- cbind(...)
  rowSums(x, na.rm = TRUE)
}

respondents <- respondents |>
  dplyr::mutate(
    # Recognition counts by ideological category.
    recognition_left_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4b_bucket == "Left",
        ideology_a4c_bucket == "Left"
      ),
      year == 2014 ~ count_bucket_matches(
        ideology_q10b_bucket == "Left",
        ideology_q10e_bucket == "Left"
      ),
      TRUE ~ NA_real_
    ),
    recognition_center_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4b_bucket == "Center",
        ideology_a4c_bucket == "Center"
      ),
      year == 2014 ~ count_bucket_matches(
        ideology_q10b_bucket == "Center",
        ideology_q10e_bucket == "Center"
      ),
      TRUE ~ NA_real_
    ),
    recognition_right_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4b_bucket == "Right",
        ideology_a4c_bucket == "Right"
      ),
      year == 2014 ~ count_bucket_matches(
        ideology_q10b_bucket == "Right",
        ideology_q10e_bucket == "Right"
      ),
      TRUE ~ NA_real_
    ),

    # Statism counts by ideological category.
    statism_left_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4d_bucket == "Left",
        ideology_a4g_bucket == "Left",
        ideology_q26a_bucket == "Left"
      ),
      year == 2014 ~ as.numeric(
        ideology_q23c_bucket == "Left"
      ),
      TRUE ~ NA_real_
    ),
    statism_center_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4d_bucket == "Center",
        ideology_a4g_bucket == "Center",
        ideology_q26a_bucket == "Center"
      ),
      year == 2014 ~ as.numeric(
        ideology_q23c_bucket == "Center"
      ),
      TRUE ~ NA_real_
    ),
    statism_right_n = dplyr::case_when(
      year == 2009 ~ count_bucket_matches(
        ideology_a4d_bucket == "Right",
        ideology_a4g_bucket == "Right",
        ideology_q26a_bucket == "Right"
      ),
      year == 2014 ~ as.numeric(
        ideology_q23c_bucket == "Right"
      ),
      TRUE ~ NA_real_
    )
  )

# Use the existing ideology_complete flag when available. If an older saved
# respondent file lacks it, reconstruct completeness from the required item
# buckets rather than silently treating missing items as Mixed.
if (!"ideology_complete" %in% names(respondents)) {
  respondents <- respondents |>
    dplyr::mutate(
      ideology_complete = dplyr::case_when(
        year == 2009 ~
          !is.na(ideology_a4b_bucket) &
          !is.na(ideology_a4c_bucket) &
          !is.na(ideology_a4d_bucket) &
          !is.na(ideology_a4g_bucket) &
          !is.na(ideology_q26a_bucket),
        year == 2014 ~
          !is.na(ideology_q10b_bucket) &
          !is.na(ideology_q10e_bucket) &
          !is.na(ideology_q23c_bucket),
        TRUE ~ FALSE
      )
    )
}

respondents <- respondents |>
  dplyr::mutate(
    voter_ideology_harmonized = dplyr::case_when(
      !ideology_complete ~ NA_character_,

      year == 2009 &
        recognition_left_n == 2 &
        statism_left_n >= 2 ~ "Left",

      year == 2009 &
        recognition_center_n == 2 &
        statism_center_n >= 2 ~ "Center",

      year == 2009 &
        recognition_right_n == 2 &
        statism_right_n >= 2 ~ "Right",

      year == 2014 &
        recognition_left_n == 2 &
        statism_left_n == 1 ~ "Left",

      year == 2014 &
        recognition_center_n == 2 &
        statism_center_n == 1 ~ "Center",

      year == 2014 &
        recognition_right_n == 2 &
        statism_right_n == 1 ~ "Right",

      TRUE ~ "Mixed"
    ),
    voter_ideology_harmonized = factor(
      voter_ideology_harmonized,
      # Center is the reference category for the triple interaction.
      levels = c(
        "Center",
        "Left",
        "Right",
        "Mixed"
      )
    )
  )

# Audit strict -> harmonized movement whenever strict voter_ideology exists.
if ("voter_ideology" %in% names(respondents)) {
  ideology_harmonization_audit <- respondents |>
    dplyr::filter(
      year %in% ANALYSIS_YEARS,
      ideology_complete
    ) |>
    dplyr::count(
      year,
      strict_ideology = as.character(voter_ideology),
      harmonized_ideology = as.character(voter_ideology_harmonized),
      name = "n"
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      share_year = n / sum(n)
    ) |>
    dplyr::ungroup()

  readr::write_csv(
    ideology_harmonization_audit,
    file.path(
      output_logs,
      "01_strict_to_harmonized_ideology_transition.csv"
    )
  )
}

# ============================================================
# 6. STRICT AC -> DISTRICT NESTING AUDIT AND EXCLUSION
# ============================================================

# Define nesting using the pooled candidate-present respondent universe BEFORE
# model-specific complete-case restrictions. An AC that maps to multiple
# districts anywhere in this primary universe is excluded everywhere.
ac_district_mapping <- respondents |>
  dplyr::filter(
    year %in% ANALYSIS_YEARS,
    respondent_sample_candidate_present,
    !is.na(ac_uid),
    !is.na(district_harmonization_group_id)
  ) |>
  dplyr::distinct(
    ac_uid,
    district_harmonization_group_id
  )

ac_district_nesting_audit <- ac_district_mapping |>
  dplyr::count(
    ac_uid,
    name = "n_districts"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_districts),
    ac_uid
  )

ambiguous_ac_uids <- ac_district_nesting_audit |>
  dplyr::filter(n_districts > 1) |>
  dplyr::pull(ac_uid)

ambiguous_ac_detail <- ac_district_mapping |>
  dplyr::filter(ac_uid %in% ambiguous_ac_uids) |>
  dplyr::arrange(
    ac_uid,
    district_harmonization_group_id
  )

readr::write_csv(
  ac_district_nesting_audit,
  file.path(
    output_logs,
    "02_ac_to_district_nesting_audit.csv"
  )
)

readr::write_csv(
  ambiguous_ac_detail,
  file.path(
    output_logs,
    "03_ambiguous_ac_to_district_mappings_EXCLUDED.csv"
  )
)

message(
  "AC nesting audit: ",
  scales::comma(nrow(ac_district_nesting_audit)),
  " ACs with observed district mappings; ",
  scales::comma(length(ambiguous_ac_uids)),
  " ACs map to >1 district and will be excluded."
)

# ============================================================
# 7. CHECK REQUIRED MODEL VARIABLES
# ============================================================

required_model_vars <- unique(
  c(
    "voted_bjp",
    "year",
    "state_no",
    "ac_uid",
    "district_harmonization_group_id",
    "respondent_sample_candidate_present",
    FDI_VAR,
    unname(DOMAIN_VARS[RUN_DOMAINS]),
    V2_VOTER_CONTROLS,
    C1_CONTEXT_CONTROLS
  )
)

missing_model_vars <- setdiff(
  required_model_vars,
  names(respondents)
)

if (length(missing_model_vars) > 0) {
  stop(
    "Respondent analysis data are missing model variables: ",
    paste(missing_model_vars, collapse = ", ")
  )
}

# ============================================================
# 8. BUILD COMMON POOLED BASE SAMPLE
# ============================================================

pooled_base <- respondents |>
  dplyr::filter(
    year %in% ANALYSIS_YEARS,
    respondent_sample_candidate_present,
    !is.na(ac_uid),
    !is.na(district_harmonization_group_id),
    !(ac_uid %in% ambiguous_ac_uids)
  ) |>
  dplyr::mutate(
    voted_bjp_lpm = as.numeric(voted_bjp),
    ac_uid_re = factor(ac_uid),
    district_re = factor(district_harmonization_group_id),
    state_year_fe = interaction(
      state_no,
      year,
      drop = TRUE,
      lex.order = TRUE
    )
  )

base_sample_flow <- tibble::tibble(
  stage = c(
    "All respondent rows",
    "2009/2014",
    "Candidate-present valid-voter sample",
    "Nonmissing AC and district IDs",
    "After excluding ACs mapping to >1 district"
  ),
  n_respondents = c(
    nrow(respondents),
    sum(respondents$year %in% ANALYSIS_YEARS, na.rm = TRUE),
    sum(
      respondents$year %in% ANALYSIS_YEARS &
        respondents$respondent_sample_candidate_present,
      na.rm = TRUE
    ),
    sum(
      respondents$year %in% ANALYSIS_YEARS &
        respondents$respondent_sample_candidate_present &
        !is.na(respondents$ac_uid) &
        !is.na(respondents$district_harmonization_group_id),
      na.rm = TRUE
    ),
    nrow(pooled_base)
  )
)

readr::write_csv(
  base_sample_flow,
  file.path(
    output_logs,
    "04_base_sample_flow.csv"
  )
)

# ============================================================
# 9. FORMULA HELPERS
# ============================================================

collapse_terms <- function(x) {
  paste(x, collapse = " + ")
}

make_fixed_formula_string <- function(interaction_order) {
  focal_term <- dplyr::case_when(
    interaction_order == "two_way" ~ "fdi_x * demo_x",
    interaction_order == "triple" ~ "fdi_x * demo_x * ideology_bucket",
    TRUE ~ NA_character_
  )

  if (is.na(focal_term)) {
    stop("Unknown interaction_order: ", interaction_order)
  }

  fixed_terms <- c(
    focal_term,
    V2_VOTER_CONTROLS,
    C1_CONTEXT_CONTROLS
  )

  if (USE_STATE_YEAR_FIXED_EFFECTS) {
    fixed_terms <- c(
      fixed_terms,
      "state_year_fe"
    )
  }

  paste0(
    "voted_bjp_lpm ~ ",
    collapse_terms(fixed_terms)
  )
}

vars_for_spec <- function(domain, interaction_order) {
  out <- c(
    "voted_bjp_lpm",
    "fdi_x",
    "demo_x",
    "ac_uid_re",
    "district_re",
    V2_VOTER_CONTROLS,
    C1_CONTEXT_CONTROLS
  )

  if (USE_STATE_YEAR_FIXED_EFFECTS) {
    out <- c(out, "state_year_fe")
  }

  if (interaction_order == "triple") {
    out <- c(out, "ideology_bucket")
  }

  unique(out)
}

# ============================================================
# 10. MODEL EXTRACTION HELPERS
# ============================================================

extract_lm_coefficients <- function(fit, hierarchy_level) {
  ct <- stats::coef(summary(fit))

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    term = rownames(ct),
    estimate = as.numeric(ct[, "Estimate"]),
    std_error = as.numeric(ct[, "Std. Error"]),
    statistic = as.numeric(ct[, "t value"]),
    p_value = as.numeric(ct[, "Pr(>|t|)"]),
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    inference_note = "lm t-test; 95% CI shown as estimate +/- 1.96 SE"
  )
}

extract_lmer_coefficients <- function(fit, hierarchy_level) {
  ct <- stats::coef(summary(fit))

  estimate <- as.numeric(ct[, "Estimate"])
  std_error <- as.numeric(ct[, "Std. Error"])
  statistic <- as.numeric(ct[, "t value"])

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    term = rownames(ct),
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = 2 * stats::pnorm(
      abs(statistic),
      lower.tail = FALSE
    ),
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    inference_note = "lmer model-based SE; normal-approximation p and 95% CI"
  )
}

get_convergence_message <- function(fit) {
  if (!inherits(fit, "merMod")) {
    return(NA_character_)
  }

  msg <- fit@optinfo$conv$lme4$messages

  if (is.null(msg)) {
    return("")
  }

  paste(msg, collapse = " | ")
}

extract_variance_components <- function(fit, hierarchy_level) {
  if (!inherits(fit, "merMod")) {
    return(
      tibble::tibble(
        hierarchy_level = hierarchy_level,
        grouping_factor = "Residual",
        variance = stats::sigma(fit)^2,
        sd = stats::sigma(fit)
      )
    )
  }

  vc <- as.data.frame(lme4::VarCorr(fit))

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    grouping_factor = as.character(vc$grp),
    variance = as.numeric(vc$vcov),
    sd = as.numeric(vc$sdcor)
  )
}

extract_icc <- function(fit, hierarchy_level) {
  if (!inherits(fit, "merMod")) {
    return(
      tibble::tibble(
        hierarchy_level = hierarchy_level,
        district_variance = 0,
        ac_variance = 0,
        residual_variance = stats::sigma(fit)^2,
        total_variance = stats::sigma(fit)^2,
        icc_same_district_different_ac = 0,
        icc_same_ac = 0
      )
    )
  }

  vc <- as.data.frame(lme4::VarCorr(fit))

  residual_var <- vc |>
    dplyr::filter(grp == "Residual") |>
    dplyr::pull(vcov)

  if (length(residual_var) == 0) {
    residual_var <- stats::sigma(fit)^2
  } else {
    residual_var <- residual_var[[1]]
  }

  random_rows <- vc |>
    dplyr::filter(grp != "Residual")

  # In M1 the only non-residual grouping factor is AC.
  # In M2 nested syntax expands to district_re and district_re:ac_uid_re.
  district_var <- random_rows |>
    dplyr::filter(grp == "district_re") |>
    dplyr::summarise(x = sum(vcov, na.rm = TRUE)) |>
    dplyr::pull(x)

  if (length(district_var) == 0 || !is.finite(district_var)) {
    district_var <- 0
  }

  ac_var <- random_rows |>
    dplyr::filter(grp != "district_re") |>
    dplyr::summarise(x = sum(vcov, na.rm = TRUE)) |>
    dplyr::pull(x)

  if (length(ac_var) == 0 || !is.finite(ac_var)) {
    ac_var <- 0
  }

  total_var <- district_var + ac_var + residual_var

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    district_variance = district_var,
    ac_variance = ac_var,
    residual_variance = residual_var,
    total_variance = total_var,
    # Correlation for two respondents in different ACs but same district.
    icc_same_district_different_ac = ifelse(
      total_var > 0,
      district_var / total_var,
      NA_real_
    ),
    # Correlation for two respondents in the same AC (and therefore district).
    icc_same_ac = ifelse(
      total_var > 0,
      (district_var + ac_var) / total_var,
      NA_real_
    )
  )
}

model_fit_stats <- function(fit, hierarchy_level, data) {
  is_mixed <- inherits(fit, "merMod")

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    model_class = paste(class(fit), collapse = " | "),
    n_respondents = nrow(data),
    n_acs = dplyr::n_distinct(data$ac_uid_re),
    n_districts = dplyr::n_distinct(data$district_re),
    logLik = as.numeric(stats::logLik(fit)),
    AIC = stats::AIC(fit),
    BIC = stats::BIC(fit),
    residual_sd = stats::sigma(fit),
    singular = if (is_mixed) {
      lme4::isSingular(fit, tol = 1e-4)
    } else {
      FALSE
    },
    convergence_message = get_convergence_message(fit)
  )
}

# ============================================================
# 11. FIT ONE SUBSTANTIVE SPECIFICATION THROUGH THE HIERARCHY
# ============================================================

fit_hierarchy <- function(domain, interaction_order) {
  # Freeze scalar function arguments under names that cannot be shadowed by
  # dplyr columns created later in mutate().  In v1.0, `domain = domain`
  # created a new column and the next expression DOMAIN_LABELS[[domain]]
  # then saw the whole column rather than the scalar function argument.
  domain_id <- as.character(domain)[1]
  interaction_order_id <- as.character(interaction_order)[1]

  if (!domain_id %in% names(DOMAIN_VARS)) {
    stop("Unknown domain: ", domain_id)
  }

  if (!interaction_order_id %in% c("two_way", "triple")) {
    stop("Unknown interaction order: ", interaction_order_id)
  }

  demographic_var <- unname(DOMAIN_VARS[[domain_id]])
  domain_label_value <- unname(DOMAIN_LABELS[[domain_id]])

  d <- pooled_base |>
    dplyr::mutate(
      fdi_x = .data[[FDI_VAR]],
      demo_x = .data[[demographic_var]],
      ideology_bucket = voter_ideology_harmonized
    )

  complete_vars <- vars_for_spec(
    domain = domain_id,
    interaction_order = interaction_order_id
  )

  d <- d |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(complete_vars),
        ~ !is.na(.x)
      )
    ) |>
    droplevels()

  if (nrow(d) == 0) {
    stop(
      "No complete-case observations for ",
      domain_id,
      " / ",
      interaction_order_id,
      "."
    )
  }

  # Re-check strict one-district-per-AC nesting in the actual complete-case
  # sample. This should be guaranteed by the global exclusion above.
  sample_nesting_check <- d |>
    dplyr::distinct(
      ac_uid_re,
      district_re
    ) |>
    dplyr::count(
      ac_uid_re,
      name = "n_districts"
    ) |>
    dplyr::filter(n_districts > 1)

  if (nrow(sample_nesting_check) > 0) {
    stop(
      "Complete-case sample still contains ACs mapped to multiple districts."
    )
  }

  fixed_formula_string <- make_fixed_formula_string(
    interaction_order_id
  )

  formula_m0 <- stats::as.formula(
    fixed_formula_string
  )

  formula_m1 <- stats::as.formula(
    paste0(
      fixed_formula_string,
      " + (1 | ac_uid_re)"
    )
  )

  formula_m2 <- stats::as.formula(
    paste0(
      fixed_formula_string,
      " + (1 | district_re/ac_uid_re)"
    )
  )

  message("")
  message("------------------------------------------------------------")
  message(
    "Fitting: ",
    domain_id,
    " / ",
    interaction_order_id
  )
  message("N respondents: ", scales::comma(nrow(d)))
  message("N ACs: ", scales::comma(dplyr::n_distinct(d$ac_uid_re)))
  message("N districts: ", scales::comma(dplyr::n_distinct(d$district_re)))
  message("Fixed formula: ", fixed_formula_string)
  message("------------------------------------------------------------")

  # M0: same fixed part, no random structure.
  m0 <- stats::lm(
    formula = formula_m0,
    data = d
  )

  # M1: random intercept for AC.
  m1 <- lme4::lmer(
    formula = formula_m1,
    data = d,
    REML = LMER_REML,
    control = LMER_CONTROL
  )

  # M2: AC nested in district, after dropping ambiguous AC mappings.
  m2 <- lme4::lmer(
    formula = formula_m2,
    data = d,
    REML = LMER_REML,
    control = LMER_CONTROL
  )

  fits <- list(
    M0_no_random_lm = m0,
    M1_ac_random_intercept = m1,
    M2_district_ac_nested_random_intercepts = m2
  )

  coef_tbl <- dplyr::bind_rows(
    extract_lm_coefficients(
      m0,
      "M0_no_random_lm"
    ),
    extract_lmer_coefficients(
      m1,
      "M1_ac_random_intercept"
    ),
    extract_lmer_coefficients(
      m2,
      "M2_district_ac_nested_random_intercepts"
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      domain_label = domain_label_value,
      interaction_order = interaction_order_id,
      fdi_var = FDI_VAR,
      demographic_var = demographic_var,
      sample = "pooled_2009_2014_candidate_present_unweighted_V2_C1",
      state_year_fixed_effects = USE_STATE_YEAR_FIXED_EFFECTS,
      .before = 1
    )

  fit_tbl <- dplyr::bind_rows(
    model_fit_stats(
      m0,
      "M0_no_random_lm",
      d
    ),
    model_fit_stats(
      m1,
      "M1_ac_random_intercept",
      d
    ),
    model_fit_stats(
      m2,
      "M2_district_ac_nested_random_intercepts",
      d
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      interaction_order = interaction_order_id,
      .before = 1
    )

  variance_tbl <- dplyr::bind_rows(
    extract_variance_components(
      m0,
      "M0_no_random_lm"
    ),
    extract_variance_components(
      m1,
      "M1_ac_random_intercept"
    ),
    extract_variance_components(
      m2,
      "M2_district_ac_nested_random_intercepts"
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      interaction_order = interaction_order_id,
      .before = 1
    )

  icc_tbl <- dplyr::bind_rows(
    extract_icc(
      m0,
      "M0_no_random_lm"
    ),
    extract_icc(
      m1,
      "M1_ac_random_intercept"
    ),
    extract_icc(
      m2,
      "M2_district_ac_nested_random_intercepts"
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      interaction_order = interaction_order_id,
      .before = 1
    )

  sample_tbl <- tibble::tibble(
    domain = domain_id,
    interaction_order = interaction_order_id,
    n_respondents = nrow(d),
    n_acs = dplyr::n_distinct(d$ac_uid_re),
    n_districts = dplyr::n_distinct(d$district_re),
    n_states = dplyr::n_distinct(d$state_no),
    n_state_year_cells = dplyr::n_distinct(d$state_year_fe),
    n_2009 = sum(d$year == 2009),
    n_2014 = sum(d$year == 2014),
    ideology_required = interaction_order == "triple",
    ambiguous_acs_excluded_globally = length(ambiguous_ac_uids)
  )

  # Save the exact analysis rows used in the three-model hierarchy as a compact
  # row-id manifest rather than duplicating the full respondent microdata.
  sample_ids <- d |>
    dplyr::select(
      respondent_uid,
      year,
      ac_uid,
      district_harmonization_group_id
    )

  list(
    fits = fits,
    coefficients = coef_tbl,
    fit_stats = fit_tbl,
    variance_components = variance_tbl,
    icc = icc_tbl,
    sample_summary = sample_tbl,
    sample_ids = sample_ids,
    fixed_formula = fixed_formula_string
  )
}

# ============================================================
# 12. RUN REQUESTED HIERARCHIES
# ============================================================

run_grid <- tidyr::crossing(
  domain = RUN_DOMAINS,
  interaction_order = RUN_INTERACTION_ORDERS
)

all_results <- vector(
  "list",
  nrow(run_grid)
)

for (i in seq_len(nrow(run_grid))) {
  domain_i <- run_grid$domain[[i]]
  order_i <- run_grid$interaction_order[[i]]

  result_i <- fit_hierarchy(
    domain = domain_i,
    interaction_order = order_i
  )

  key <- paste(
    domain_i,
    order_i,
    sep = "__"
  )

  all_results[[i]] <- result_i
  names(all_results)[[i]] <- key

  # Save fitted objects separately for easy inspection in R.
  saveRDS(
    result_i$fits,
    file.path(
      output_models,
      paste0(
        key,
        "__hierarchy_models.rds"
      )
    )
  )

  readr::write_csv(
    result_i$sample_ids,
    file.path(
      output_logs,
      paste0(
        "sample_ids__",
        key,
        ".csv"
      )
    )
  )
}

# ============================================================
# 13. COMBINE + WRITE OUTPUTS
# ============================================================

coefficients_all <- purrr::map_dfr(
  all_results,
  "coefficients"
)

fit_stats_all <- purrr::map_dfr(
  all_results,
  "fit_stats"
)

variance_components_all <- purrr::map_dfr(
  all_results,
  "variance_components"
)

icc_all <- purrr::map_dfr(
  all_results,
  "icc"
)

sample_summary_all <- purrr::map_dfr(
  all_results,
  "sample_summary"
)

# Focal terms only, for rapid hierarchy comparison.
focal_coefficients <- coefficients_all |>
  dplyr::filter(
    stringr::str_detect(
      term,
      "(^fdi_x$)|(^demo_x$)|fdi_x:demo_x|demo_x:fdi_x|ideology_bucket"
    )
  ) |>
  dplyr::arrange(
    domain,
    interaction_order,
    hierarchy_level,
    term
  )

readr::write_csv(
  coefficients_all,
  file.path(
    output_csv,
    "01_all_model_coefficients.csv"
  )
)

readr::write_csv(
  focal_coefficients,
  file.path(
    output_csv,
    "02_focal_hierarchy_coefficients.csv"
  )
)

readr::write_csv(
  fit_stats_all,
  file.path(
    output_csv,
    "03_model_fit_and_convergence.csv"
  )
)

readr::write_csv(
  variance_components_all,
  file.path(
    output_csv,
    "04_random_effect_variance_components.csv"
  )
)

readr::write_csv(
  icc_all,
  file.path(
    output_csv,
    "05_intraclass_correlations.csv"
  )
)

readr::write_csv(
  sample_summary_all,
  file.path(
    output_csv,
    "06_model_sample_summary.csv"
  )
)

# Save one complete bundle for programmatic follow-up.
saveRDS(
  list(
    revision = SCRIPT_REVISION,
    settings = list(
      analysis_years = ANALYSIS_YEARS,
      fdi_var = FDI_VAR,
      domain_vars = DOMAIN_VARS,
      run_domains = RUN_DOMAINS,
      run_interaction_orders = RUN_INTERACTION_ORDERS,
      voter_controls = V2_VOTER_CONTROLS,
      context_controls = C1_CONTEXT_CONTROLS,
      state_year_fixed_effects = USE_STATE_YEAR_FIXED_EFFECTS,
      lmer_reml = LMER_REML
    ),
    ambiguous_ac_uids = ambiguous_ac_uids,
    results = all_results
  ),
  file.path(
    output_root,
    "respondent_multilevel_hierarchy_bundle.rds"
  )
)

# ============================================================
# 14. CONSOLE READOUT
# ============================================================

message("")
message("============================================================")
message("RESPONDENT MULTILEVEL HIERARCHY COMPLETE")
message("============================================================")
message("Output root: ", output_root)
message("")
message("Hierarchy:")
message("  M0 = lm, no random intercept")
message("  M1 = lmer, AC random intercept")
message("  M2 = lmer, AC nested in district random intercepts")
message("")
message("Ambiguous ACs excluded: ", length(ambiguous_ac_uids))
message("")
message("Focal coefficient preview:")
print(
  focal_coefficients |>
    dplyr::select(
      domain,
      interaction_order,
      hierarchy_level,
      term,
      estimate,
      std_error,
      p_value,
      conf_low,
      conf_high
    ),
  n = Inf
)
message("")
message("ICC preview:")
print(icc_all, n = Inf)
message("")
message("Fit/convergence preview:")
print(fit_stats_all, n = Inf)
message("============================================================")

# ============================================================
# 19_respondent_two_level_ac_hierarchy.R
# Pooled 2009/2014 unweighted voter-level LPM / AC hierarchy
# Revision: 2026-08-17-v1.1.2
# ============================================================
#
# HOTFIX v1.1.2
# -------------
# Fixes three malformed list-index expressions accidentally written as
# `x[ [key] ]` across line breaks. They are now valid `x[[key]]` expressions.
# No sample, formula, hierarchy, ruler, estimator, or diagnostic logic changed.
#
# PURPOSE
# -------
# Fit a deliberately simple TWO-LEVEL pooled voter hierarchy:
#
#   M0. No random structure:
#         ordinary unweighted linear probability model (lm)
#
#   M1. Voters nested in Assembly Constituencies:
#         random intercept for AC (lmer)
#
# District is NOT part of this version of the model.
#
# For each demographic domain (Muslim share; established migrant share), fit:
#
#   A. two_way_full
#        FDI x demographic context on the full complete-case two-way sample
#
#   B. two_way_restricted_ideology_complete
#        the IDENTICAL two-way model, but estimated on the exact respondent
#        sample used by the triple model
#
#   C. triple_ideology_complete
#        FDI x demographic context x harmonized ideology bucket
#
# This allows us to separate:
#   (i) changes caused by adding the AC random intercept,
#   (ii) changes caused by restricting to ideology-complete respondents, and
#   (iii) changes caused by adding ideology moderation.
#
# FIXED PART
# ----------
# Preferred FDI:
#   log1p_fdi_mfg_local_all_pc100k
#
# Demographic moderators:
#   Muslim:    muslim_share_2001_dist_proxy
#   Migration: mig_total_upto_2001_share_ac_pop
#
# V2 voter controls:
#   religion_group + caste_group + education_harmonized
#
# C1 contextual controls:
#   proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share
#
# Every model includes state x year fixed effects.
#
# SAMPLE
# ------
#   - pooled 2009 + 2014
#   - valid BJP vote outcome
#   - BJP candidate present
#   - nonmissing AC ID
#   - UNWEIGHTED
#
# The restricted two-way sample is hard-checked to be exactly identical to the
# triple-model sample for the same demographic domain.
#
# SUBSTANTIVE CONTRASTS
# ---------------------
# Raw interaction coefficients are converted to percentage-point contrasts
# using the paper's common informative-unit rulers, calculated from unique
# 2014 AC rows in ac_year.rds:
#
#   FDI:        0 -> median POSITIVE manufacturing FDI exposure
#   Muslim:     0 -> ordinary median Muslim share (positive-median fallback)
#   Migration:  0 -> ordinary median established migrant share
#               (positive-median fallback)
#
# For two-way models, the reported substantive quantity is the difference in
# the FDI effect between the high and low demographic contexts:
#
#   [Y(FDI_high, D_high) - Y(0, D_high)]
# - [Y(FDI_high, 0)      - Y(0, 0)]
#
# in percentage points.
#
# For triple models, the same FDI x demographic contrast is reported separately
# for Center, Left, Right, and Mixed voters. Center is the reference group.
# The output also reports each non-Center group's contrast difference versus
# Center.
#
# DIAGNOSTICS
# -----------
# M1 outputs explicitly report:
#   - lme4::isSingular()
#   - convergence_message
#   - any lmer warning messages captured during fitting
#   - a compact diagnostic_status
#
# A singular fit is recorded, not automatically treated as an error.
#
# INFERENCE NOTE
# --------------
# These are Gaussian LPMs. This first pass is intentionally unweighted.
# For lmer fits, model-based SEs and normal-approximation inference are used.
# The lm baseline uses its conventional residual-df t inference.
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

SCRIPT_REVISION <- "2026-08-17-v1.1.2"

message(
  "Running respondent two-level AC hierarchy revision: ",
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

# Three specifications are fit for each demographic domain:
# 1) full-sample two-way;
# 2) two-way on the exact ideology-complete triple sample;
# 3) ideology triple on that same restricted sample.
RUN_SPECIFICATIONS <- c(
  "two_way_full",
  "two_way_restricted_ideology_complete",
  "triple_ideology_complete"
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
# common ML basis for the AC-random-intercept fits. This is exploratory.
LMER_REML <- FALSE

# Conservative optimizer settings for the AC random-intercept model.
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
  "respondent_two_level_ac_hierarchy",
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
# 6. COMMON INFORMATIVE-UNIT RULERS FOR SUBSTANTIVE CONTRASTS
# ============================================================

ac_year <- readRDS(
  file.path(
    paths$final_dir,
    "ac_year.rds"
  )
)

required_ruler_vars <- c(
  "year",
  "ac_uid",
  FDI_VAR,
  unname(DOMAIN_VARS[RUN_DOMAINS])
)

missing_ruler_vars <- setdiff(
  required_ruler_vars,
  names(ac_year)
)

if (length(missing_ruler_vars) > 0) {
  stop(
    "ac_year.rds is missing variables required for informative-unit rulers: ",
    paste(missing_ruler_vars, collapse = ", ")
  )
}

reference_ac <- ac_year |>
  dplyr::filter(year == 2014) |>
  dplyr::distinct(ac_uid, .keep_all = TRUE)

safe_median_local <- function(x, positive = FALSE) {
  z <- x[is.finite(x)]

  if (positive) {
    z <- z[z > 0]
  }

  if (length(z) == 0) {
    return(NA_real_)
  }

  stats::median(z)
}

choose_zero_to_typical <- function(x, always_positive_median = FALSE) {
  med_all <- safe_median_local(
    x,
    positive = FALSE
  )

  med_pos <- safe_median_local(
    x,
    positive = TRUE
  )

  if (always_positive_median) {
    return(
      list(
        high = med_pos,
        rule = "0 to median positive"
      )
    )
  }

  if (is.finite(med_all) && med_all > 0) {
    return(
      list(
        high = med_all,
        rule = "0 to median"
      )
    )
  }

  if (is.finite(med_pos) && med_pos > 0) {
    return(
      list(
        high = med_pos,
        rule = "0 to median positive (ordinary median = 0)"
      )
    )
  }

  list(
    high = NA_real_,
    rule = "FAILED: no positive finite reference value"
  )
}

fdi_choice <- choose_zero_to_typical(
  reference_ac[[FDI_VAR]],
  always_positive_median = TRUE
)

muslim_choice <- choose_zero_to_typical(
  reference_ac[[DOMAIN_VARS[["muslim"]]]],
  always_positive_median = FALSE
)

migration_choice <- choose_zero_to_typical(
  reference_ac[[DOMAIN_VARS[["migration"]]]],
  always_positive_median = FALSE
)

informative_reference <- tibble::tibble(
  quantity = c(
    "FDI",
    "Muslim share 2001",
    "Established migrant share 2001"
  ),
  low = 0,
  high = c(
    fdi_choice$high,
    muslim_choice$high,
    migration_choice$high
  ),
  rule = c(
    paste0(fdi_choice$rule, " among unique 2014 ACs"),
    paste0(muslim_choice$rule, " among unique 2014 ACs"),
    paste0(migration_choice$rule, " among unique 2014 ACs")
  )
)

if (
  any(
    !is.finite(informative_reference$high) |
      informative_reference$high <= 0
  )
) {
  stop(
    "At least one informative-unit ruler is nonpositive or missing."
  )
}

readr::write_csv(
  informative_reference,
  file.path(
    output_logs,
    "02_informative_unit_reference_values.csv"
  )
)

REF_FDI <- informative_reference$high[
  informative_reference$quantity == "FDI"
]

REF_DEMO <- c(
  muslim = informative_reference$high[
    informative_reference$quantity == "Muslim share 2001"
  ],
  migration = informative_reference$high[
    informative_reference$quantity == "Established migrant share 2001"
  ]
)

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

# ============================================================
# 7. CHECK REQUIRED MODEL VARIABLES
# ============================================================

required_model_vars <- unique(
  c(
    "voted_bjp",
    "year",
    "state_no",
    "ac_uid",
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
    !is.na(ac_uid)
  ) |>
  dplyr::mutate(
    voted_bjp_lpm = as.numeric(voted_bjp),
    ac_uid_re = factor(ac_uid),
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
    "Nonmissing AC ID"
  ),
  n_respondents = c(
    nrow(respondents),
    sum(
      respondents$year %in% ANALYSIS_YEARS,
      na.rm = TRUE
    ),
    sum(
      respondents$year %in% ANALYSIS_YEARS &
        respondents$respondent_sample_candidate_present,
      na.rm = TRUE
    ),
    nrow(pooled_base)
  )
)

readr::write_csv(
  base_sample_flow,
  file.path(
    output_logs,
    "03_base_sample_flow.csv"
  )
)

# ============================================================
# 9. FORMULA + SAMPLE HELPERS
# ============================================================

collapse_terms <- function(x) {
  paste(
    x,
    collapse = " + "
  )
}

make_fixed_formula_string <- function(specification) {
  focal_term <- dplyr::case_when(
    specification %in% c(
      "two_way_full",
      "two_way_restricted_ideology_complete"
    ) ~ "fdi_x * demo_x",
    specification == "triple_ideology_complete" ~
      "fdi_x * demo_x * ideology_bucket",
    TRUE ~ NA_character_
  )

  if (is.na(focal_term)) {
    stop(
      "Unknown specification: ",
      specification
    )
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

complete_vars_for_two_way <- function() {
  out <- c(
    "respondent_uid",
    "voted_bjp_lpm",
    "fdi_x",
    "demo_x",
    "ac_uid_re",
    V2_VOTER_CONTROLS,
    C1_CONTEXT_CONTROLS
  )

  if (USE_STATE_YEAR_FIXED_EFFECTS) {
    out <- c(
      out,
      "state_year_fe"
    )
  }

  unique(out)
}

complete_vars_for_triple <- function() {
  unique(
    c(
      complete_vars_for_two_way(),
      "ideology_bucket"
    )
  )
}

build_domain_samples <- function(domain) {
  domain_id <- as.character(domain)[1]

  if (!domain_id %in% names(DOMAIN_VARS)) {
    stop(
      "Unknown domain: ",
      domain_id
    )
  }

  demographic_var <- unname(
    DOMAIN_VARS[[domain_id]]
  )

  d0 <- pooled_base |>
    dplyr::mutate(
      fdi_x = .data[[FDI_VAR]],
      demo_x = .data[[demographic_var]],
      ideology_bucket = voter_ideology_harmonized
    )

  d_two_full <- d0 |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          complete_vars_for_two_way()
        ),
        ~ !is.na(.x)
      )
    ) |>
    droplevels()

  d_triple <- d0 |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          complete_vars_for_triple()
        ),
        ~ !is.na(.x)
      )
    ) |>
    droplevels()

  # This is intentionally the EXACT triple sample.
  d_two_restricted <- d_triple

  ids_restricted <- sort(
    as.character(
      d_two_restricted$respondent_uid
    )
  )

  ids_triple <- sort(
    as.character(
      d_triple$respondent_uid
    )
  )

  if (!identical(ids_restricted, ids_triple)) {
    stop(
      "Restricted two-way sample is not identical to the triple sample for ",
      domain_id,
      "."
    )
  }

  list(
    two_way_full = d_two_full,
    two_way_restricted_ideology_complete = d_two_restricted,
    triple_ideology_complete = d_triple
  )
}

# ============================================================
# 10. MODEL EXTRACTION + DIAGNOSTIC HELPERS
# ============================================================

extract_lm_coefficients <- function(
    fit,
    hierarchy_level
) {
  ct <- stats::coef(
    summary(fit)
  )

  estimate <- as.numeric(
    ct[, "Estimate"]
  )

  std_error <- as.numeric(
    ct[, "Std. Error"]
  )

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    term = rownames(ct),
    estimate = estimate,
    std_error = std_error,
    statistic = as.numeric(
      ct[, "t value"]
    ),
    p_value = as.numeric(
      ct[, "Pr(>|t|)"]
    ),
    conf_low = estimate +
      stats::qt(
        0.025,
        df = stats::df.residual(fit)
      ) * std_error,
    conf_high = estimate +
      stats::qt(
        0.975,
        df = stats::df.residual(fit)
      ) * std_error,
    inference_note =
      "lm residual-df t inference"
  )
}

extract_lmer_coefficients <- function(
    fit,
    hierarchy_level
) {
  ct <- stats::coef(
    summary(fit)
  )

  estimate <- as.numeric(
    ct[, "Estimate"]
  )

  std_error <- as.numeric(
    ct[, "Std. Error"]
  )

  statistic <- as.numeric(
    ct[, "t value"]
  )

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
    conf_low = estimate -
      1.96 * std_error,
    conf_high = estimate +
      1.96 * std_error,
    inference_note =
      "lmer model-based SE; normal-approximation p and 95% CI"
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

  paste(
    msg,
    collapse = " | "
  )
}

fit_lmer_capture_warnings <- function(
    formula,
    data
) {
  warning_messages <- character()

  fit <- withCallingHandlers(
    lme4::lmer(
      formula = formula,
      data = data,
      REML = LMER_REML,
      control = LMER_CONTROL
    ),
    warning = function(w) {
      warning_messages <<- c(
        warning_messages,
        conditionMessage(w)
      )

      invokeRestart(
        "muffleWarning"
      )
    }
  )

  list(
    fit = fit,
    warning_messages = unique(
      warning_messages
    )
  )
}

model_fit_stats <- function(
    fit,
    hierarchy_level,
    data,
    fit_warning_messages = character()
) {
  is_mixed <- inherits(
    fit,
    "merMod"
  )

  singular_flag <- if (is_mixed) {
    lme4::isSingular(
      fit,
      tol = 1e-4
    )
  } else {
    FALSE
  }

  conv_msg <- get_convergence_message(
    fit
  )

  conv_problem <- is_mixed &&
    !is.na(conv_msg) &&
    nzchar(
      trimws(conv_msg)
    )

  warning_text <- if (
    length(fit_warning_messages) == 0
  ) {
    ""
  } else {
    paste(
      fit_warning_messages,
      collapse = " | "
    )
  }

  diagnostic_status <- dplyr::case_when(
    singular_flag && conv_problem ~
      "singular_and_convergence_message",
    singular_flag ~
      "singular_boundary",
    conv_problem ~
      "convergence_message_present",
    TRUE ~
      "ok"
  )

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    model_class = paste(
      class(fit),
      collapse = " | "
    ),
    n_respondents = nrow(data),
    n_acs = dplyr::n_distinct(
      data$ac_uid_re
    ),
    logLik = as.numeric(
      stats::logLik(fit)
    ),
    AIC = stats::AIC(fit),
    BIC = stats::BIC(fit),
    residual_sd = stats::sigma(fit),
    singular = singular_flag,
    convergence_message = conv_msg,
    convergence_problem = conv_problem,
    fit_warning_messages = warning_text,
    diagnostic_status = diagnostic_status
  )
}

extract_variance_components <- function(
    fit,
    hierarchy_level
) {
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

  vc <- as.data.frame(
    lme4::VarCorr(fit)
  )

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    grouping_factor = as.character(
      vc$grp
    ),
    variance = as.numeric(
      vc$vcov
    ),
    sd = as.numeric(
      vc$sdcor
    )
  )
}

extract_ac_icc <- function(
    fit,
    hierarchy_level
) {
  if (!inherits(fit, "merMod")) {
    residual_var <- stats::sigma(fit)^2

    return(
      tibble::tibble(
        hierarchy_level = hierarchy_level,
        ac_variance = 0,
        residual_variance = residual_var,
        total_variance = residual_var,
        icc_same_ac = 0
      )
    )
  }

  vc <- as.data.frame(
    lme4::VarCorr(fit)
  )

  residual_var <- vc |>
    dplyr::filter(
      grp == "Residual"
    ) |>
    dplyr::pull(vcov)

  if (
    length(residual_var) == 0 ||
      !is.finite(residual_var[[1]])
  ) {
    residual_var <- stats::sigma(fit)^2
  } else {
    residual_var <- residual_var[[1]]
  }

  ac_var <- vc |>
    dplyr::filter(
      grp != "Residual"
    ) |>
    dplyr::summarise(
      x = sum(
        vcov,
        na.rm = TRUE
      )
    ) |>
    dplyr::pull(x)

  if (
    length(ac_var) == 0 ||
      !is.finite(ac_var)
  ) {
    ac_var <- 0
  }

  total_var <- ac_var +
    residual_var

  tibble::tibble(
    hierarchy_level = hierarchy_level,
    ac_variance = ac_var,
    residual_variance = residual_var,
    total_variance = total_var,
    icc_same_ac = ifelse(
      total_var > 0,
      ac_var / total_var,
      NA_real_
    )
  )
}

# ============================================================
# 11. SUBSTANTIVE-CONTRAST HELPERS
# ============================================================

find_term_by_components <- function(
    coefficient_names,
    components
) {
  target <- sort(
    components
  )

  matches <- coefficient_names[
    vapply(
      strsplit(
        coefficient_names,
        ":",
        fixed = TRUE
      ),
      function(x) {
        identical(
          sort(x),
          target
        )
      },
      logical(1)
    )
  ]

  if (length(matches) != 1L) {
    stop(
      "Could not uniquely identify coefficient term with components: ",
      paste(
        components,
        collapse = " : "
      ),
      ". Matching coefficient names: ",
      paste(
        matches,
        collapse = " | "
      )
    )
  }

  matches[[1]]
}

fixed_effect_vector <- function(fit) {
  if (inherits(fit, "merMod")) {
    return(
      lme4::fixef(fit)
    )
  }

  stats::coef(fit)
}

linear_combination_summary <- function(
    fit,
    weights
) {
  beta <- fixed_effect_vector(fit)
  V <- stats::vcov(fit)

  L <- rep(
    0,
    length(beta)
  )

  names(L) <- names(beta)

  unknown_terms <- setdiff(
    names(weights),
    names(beta)
  )

  if (length(unknown_terms) > 0) {
    stop(
      "Linear contrast refers to coefficient(s) not in model: ",
      paste(
        unknown_terms,
        collapse = ", "
      )
    )
  }

  L[
    names(weights)
  ] <- weights

  estimate <- sum(
    L * beta
  )

  variance <- as.numeric(
    t(L) %*%
      V %*%
      L
  )

  variance <- max(
    variance,
    0
  )

  se <- sqrt(
    variance
  )

  statistic <- if (
    is.finite(se) &&
      se > 0
  ) {
    estimate / se
  } else {
    NA_real_
  }

  if (inherits(fit, "lm")) {
    df <- stats::df.residual(
      fit
    )

    critical <- stats::qt(
      0.975,
      df = df
    )

    p_value <- if (
      is.finite(statistic)
    ) {
      2 * stats::pt(
        abs(statistic),
        df = df,
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }

    inference_note <-
      "linear combination; lm residual-df t inference"
  } else {
    critical <- stats::qnorm(
      0.975
    )

    p_value <- if (
      is.finite(statistic)
    ) {
      2 * stats::pnorm(
        abs(statistic),
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }

    inference_note <-
      "linear combination; lmer model-based normal approximation"
  }

  tibble::tibble(
    estimate = estimate,
    std_error = se,
    statistic = statistic,
    p_value = p_value,
    conf_low = estimate -
      critical * se,
    conf_high = estimate +
      critical * se,
    inference_note = inference_note
  )
}

make_substantive_contrasts <- function(
    fit,
    hierarchy_level,
    domain,
    specification
) {
  domain_id <- as.character(
    domain
  )[1]

  specification_id <- as.character(
    specification
  )[1]

  ref_demo <- unname(
    REF_DEMO[[domain_id]]
  )

  scale_to_pp <- REF_FDI *
    ref_demo *
    100

  coef_names <- names(
    fixed_effect_vector(fit)
  )

  base_interaction <- find_term_by_components(
    coef_names,
    c(
      "fdi_x",
      "demo_x"
    )
  )

  base_metadata <- list(
    domain = domain_id,
    domain_label = unname(
      DOMAIN_LABELS[[domain_id]]
    ),
    specification = specification_id,
    hierarchy_level = hierarchy_level,
    fdi_low = 0,
    fdi_high = REF_FDI,
    demographic_low = 0,
    demographic_high = ref_demo,
    coefficient_scale_to_pp = scale_to_pp
  )

  if (
    specification_id %in% c(
      "two_way_full",
      "two_way_restricted_ideology_complete"
    )
  ) {
    raw <- linear_combination_summary(
      fit,
      stats::setNames(
        1,
        base_interaction
      )
    )

    return(
      raw |>
        dplyr::mutate(
          !!!base_metadata,
          contrast_type =
            "FDI_x_demographic_difference_in_differences",
          ideology_group =
            "No ideology moderator",
          reference_ideology =
            NA_character_,
          estimate_pp =
            estimate * scale_to_pp,
          std_error_pp =
            std_error * scale_to_pp,
          conf_low_pp =
            conf_low * scale_to_pp,
          conf_high_pp =
            conf_high * scale_to_pp,
          contrast_definition = paste0(
            "[Y(FDI_high,D_high)-Y(0,D_high)] - ",
            "[Y(FDI_high,0)-Y(0,0)]"
          ),
          .before = 1
        )
    )
  }

  if (
    specification_id !=
      "triple_ideology_complete"
  ) {
    stop(
      "Unknown specification for substantive contrasts: ",
      specification_id
    )
  }

  ideology_levels <- c(
    "Center",
    "Left",
    "Right",
    "Mixed"
  )

  within_rows <- purrr::map_dfr(
    ideology_levels,
    function(group_name) {
      w <- stats::setNames(
        1,
        base_interaction
      )

      if (
        group_name != "Center"
      ) {
        triple_term <-
          find_term_by_components(
            coef_names,
            c(
              "fdi_x",
              "demo_x",
              paste0(
                "ideology_bucket",
                group_name
              )
            )
          )

        w[
          triple_term
        ] <- 1
      }

      raw <- linear_combination_summary(
        fit,
        w
      )

      raw |>
        dplyr::mutate(
          !!!base_metadata,
          contrast_type =
            "FDI_x_demographic_DiD_within_ideology",
          ideology_group =
            group_name,
          reference_ideology =
            "Center",
          estimate_pp =
            estimate * scale_to_pp,
          std_error_pp =
            std_error * scale_to_pp,
          conf_low_pp =
            conf_low * scale_to_pp,
          conf_high_pp =
            conf_high * scale_to_pp,
          contrast_definition = paste0(
            "Within ",
            group_name,
            ": [Y(FDI_high,D_high)-Y(0,D_high)] - ",
            "[Y(FDI_high,0)-Y(0,0)]"
          ),
          .before = 1
        )
    }
  )

  difference_rows <- purrr::map_dfr(
    c(
      "Left",
      "Right",
      "Mixed"
    ),
    function(group_name) {
      triple_term <-
        find_term_by_components(
          coef_names,
          c(
            "fdi_x",
            "demo_x",
            paste0(
              "ideology_bucket",
              group_name
            )
          )
        )

      raw <- linear_combination_summary(
        fit,
        stats::setNames(
          1,
          triple_term
        )
      )

      raw |>
        dplyr::mutate(
          !!!base_metadata,
          contrast_type =
            "ideology_difference_in_FDI_x_demographic_DiD_vs_Center",
          ideology_group =
            group_name,
          reference_ideology =
            "Center",
          estimate_pp =
            estimate * scale_to_pp,
          std_error_pp =
            std_error * scale_to_pp,
          conf_low_pp =
            conf_low * scale_to_pp,
          conf_high_pp =
            conf_high * scale_to_pp,
          contrast_definition = paste0(
            group_name,
            " FDI x demographic contrast minus Center contrast"
          ),
          .before = 1
        )
    }
  )

  dplyr::bind_rows(
    within_rows,
    difference_rows
  )
}

# ============================================================
# 12. FIT ONE SPECIFICATION AT M0 AND M1
# ============================================================

fit_two_level_specification <- function(
    domain,
    specification,
    data
) {
  domain_id <- as.character(
    domain
  )[1]

  specification_id <- as.character(
    specification
  )[1]

  fixed_formula_string <-
    make_fixed_formula_string(
      specification_id
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

  message("")
  message("------------------------------------------------------------")
  message(
    "Fitting: ",
    domain_id,
    " / ",
    specification_id
  )
  message(
    "N respondents: ",
    scales::comma(
      nrow(data)
    )
  )
  message(
    "N ACs: ",
    scales::comma(
      dplyr::n_distinct(
        data$ac_uid_re
      )
    )
  )
  message(
    "Fixed formula: ",
    fixed_formula_string
  )
  message("------------------------------------------------------------")

  m0 <- stats::lm(
    formula = formula_m0,
    data = data
  )

  m1_result <- fit_lmer_capture_warnings(
    formula = formula_m1,
    data = data
  )

  m1 <- m1_result$fit

  if (
    length(
      m1_result$warning_messages
    ) > 0
  ) {
    message(
      "Captured M1 warning(s): ",
      paste(
        m1_result$warning_messages,
        collapse = " | "
      )
    )
  }

  fits <- list(
    M0_no_random_lm = m0,
    M1_ac_random_intercept = m1
  )

  coefficients <- dplyr::bind_rows(
    extract_lm_coefficients(
      m0,
      "M0_no_random_lm"
    ),
    extract_lmer_coefficients(
      m1,
      "M1_ac_random_intercept"
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      domain_label = unname(
        DOMAIN_LABELS[[domain_id]]
      ),
      specification =
        specification_id,
      fdi_var = FDI_VAR,
      demographic_var =
        unname(
          DOMAIN_VARS[[domain_id]]
        ),
      sample = dplyr::case_when(
        specification_id ==
          "two_way_full" ~
          "pooled_2009_2014_candidate_present_unweighted_full_two_way_complete_cases",
        specification_id ==
          "two_way_restricted_ideology_complete" ~
          "pooled_2009_2014_candidate_present_unweighted_exact_triple_sample_two_way",
        specification_id ==
          "triple_ideology_complete" ~
          "pooled_2009_2014_candidate_present_unweighted_ideology_complete_triple_sample",
        TRUE ~
          "unknown"
      ),
      state_year_fixed_effects =
        USE_STATE_YEAR_FIXED_EFFECTS,
      .before = 1
    )

  fit_stats <- dplyr::bind_rows(
    model_fit_stats(
      m0,
      "M0_no_random_lm",
      data,
      character()
    ),
    model_fit_stats(
      m1,
      "M1_ac_random_intercept",
      data,
      m1_result$warning_messages
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      specification =
        specification_id,
      .before = 1
    )

  variance_components <-
    dplyr::bind_rows(
      extract_variance_components(
        m0,
        "M0_no_random_lm"
      ),
      extract_variance_components(
        m1,
        "M1_ac_random_intercept"
      )
    ) |>
    dplyr::mutate(
      domain = domain_id,
      specification =
        specification_id,
      .before = 1
    )

  icc <- dplyr::bind_rows(
    extract_ac_icc(
      m0,
      "M0_no_random_lm"
    ),
    extract_ac_icc(
      m1,
      "M1_ac_random_intercept"
    )
  ) |>
    dplyr::mutate(
      domain = domain_id,
      specification =
        specification_id,
      .before = 1
    )

  substantive_contrasts <-
    dplyr::bind_rows(
      make_substantive_contrasts(
        m0,
        "M0_no_random_lm",
        domain_id,
        specification_id
      ),
      make_substantive_contrasts(
        m1,
        "M1_ac_random_intercept",
        domain_id,
        specification_id
      )
    )

  sample_summary <- tibble::tibble(
    domain = domain_id,
    specification =
      specification_id,
    n_respondents = nrow(data),
    n_acs = dplyr::n_distinct(
      data$ac_uid_re
    ),
    n_states = dplyr::n_distinct(
      data$state_no
    ),
    n_state_year_cells =
      dplyr::n_distinct(
        data$state_year_fe
      ),
    n_2009 = sum(
      data$year == 2009
    ),
    n_2014 = sum(
      data$year == 2014
    ),
    ideology_term_in_formula =
      specification_id ==
      "triple_ideology_complete",
    ideology_complete_sample_restriction =
      specification_id %in% c(
        "two_way_restricted_ideology_complete",
        "triple_ideology_complete"
      ),
    exact_triple_sample =
      specification_id %in% c(
        "two_way_restricted_ideology_complete",
        "triple_ideology_complete"
      )
  )

  sample_ids <- data |>
    dplyr::select(
      respondent_uid,
      year,
      ac_uid
    )

  list(
    fits = fits,
    coefficients = coefficients,
    fit_stats = fit_stats,
    variance_components =
      variance_components,
    icc = icc,
    substantive_contrasts =
      substantive_contrasts,
    sample_summary =
      sample_summary,
    sample_ids = sample_ids,
    fixed_formula =
      fixed_formula_string
  )
}

# ============================================================
# 13. RUN ALL REQUESTED TWO-LEVEL SPECIFICATIONS
# ============================================================

all_results <- list()

for (domain_i in RUN_DOMAINS) {
  domain_samples <-
    build_domain_samples(
      domain_i
    )

  # Hard audit: restricted two-way and triple sample IDs must match exactly.
  restricted_ids <- sort(
    as.character(
      domain_samples[["two_way_restricted_ideology_complete"]]$respondent_uid
    )
  )

  triple_ids <- sort(
    as.character(
      domain_samples[["triple_ideology_complete"]]$respondent_uid
    )
  )

  sample_match <- identical(
    restricted_ids,
    triple_ids
  )

  sample_match_audit <- tibble::tibble(
    domain = domain_i,
    restricted_two_way_n =
      length(
        restricted_ids
      ),
    triple_n =
      length(
        triple_ids
      ),
    exact_id_match =
      sample_match
  )

  readr::write_csv(
    sample_match_audit,
    file.path(
      output_logs,
      paste0(
        "04_restricted_vs_triple_sample_match__",
        domain_i,
        ".csv"
      )
    )
  )

  if (!sample_match) {
    stop(
      "Restricted two-way and triple respondent IDs differ for ",
      domain_i,
      "."
    )
  }

  for (
    specification_i in
      RUN_SPECIFICATIONS
  ) {
    data_i <-
      domain_samples[[specification_i]]

    result_i <-
      fit_two_level_specification(
        domain = domain_i,
        specification =
          specification_i,
        data = data_i
      )

    key <- paste(
      domain_i,
      specification_i,
      sep = "__"
    )

    all_results[[key]] <-
      result_i

    saveRDS(
      result_i$fits,
      file.path(
        output_models,
        paste0(
          key,
          "__two_level_models.rds"
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
}

# ============================================================
# 14. COMBINE + WRITE OUTPUTS
# ============================================================

coefficients_all <- purrr::map_dfr(
  all_results,
  "coefficients"
)

fit_stats_all <- purrr::map_dfr(
  all_results,
  "fit_stats"
)

variance_components_all <-
  purrr::map_dfr(
    all_results,
    "variance_components"
  )

icc_all <- purrr::map_dfr(
  all_results,
  "icc"
)

substantive_contrasts_all <-
  purrr::map_dfr(
    all_results,
    "substantive_contrasts"
  )

sample_summary_all <-
  purrr::map_dfr(
    all_results,
    "sample_summary"
  )

focal_coefficients <-
  coefficients_all |>
  dplyr::filter(
    stringr::str_detect(
      term,
      "(^fdi_x$)|(^demo_x$)|fdi_x:demo_x|demo_x:fdi_x|ideology_bucket"
    )
  ) |>
  dplyr::arrange(
    domain,
    specification,
    hierarchy_level,
    term
  )

mixed_model_diagnostics <-
  fit_stats_all |>
  dplyr::filter(
    hierarchy_level ==
      "M1_ac_random_intercept"
  ) |>
  dplyr::select(
    domain,
    specification,
    hierarchy_level,
    n_respondents,
    n_acs,
    singular,
    convergence_message,
    convergence_problem,
    fit_warning_messages,
    diagnostic_status
  ) |>
  dplyr::arrange(
    domain,
    specification
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
  mixed_model_diagnostics,
  file.path(
    output_csv,
    "06_mixed_model_singularity_and_convergence_audit.csv"
  )
)

readr::write_csv(
  substantive_contrasts_all,
  file.path(
    output_csv,
    "07_substantive_contrasts_percentage_points.csv"
  )
)

readr::write_csv(
  sample_summary_all,
  file.path(
    output_csv,
    "08_model_sample_summary.csv"
  )
)

# Compact sample-comparison audit for the question motivating this revision.
restricted_sample_comparison <-
  sample_summary_all |>
  dplyr::filter(
    specification %in% c(
      "two_way_full",
      "two_way_restricted_ideology_complete",
      "triple_ideology_complete"
    )
  ) |>
  dplyr::select(
    domain,
    specification,
    n_respondents,
    n_acs,
    n_2009,
    n_2014
  ) |>
  dplyr::arrange(
    domain,
    factor(
      specification,
      levels =
        RUN_SPECIFICATIONS
    )
  )

readr::write_csv(
  restricted_sample_comparison,
  file.path(
    output_csv,
    "09_full_vs_restricted_sample_sizes.csv"
  )
)

saveRDS(
  list(
    revision = SCRIPT_REVISION,
    settings = list(
      analysis_years =
        ANALYSIS_YEARS,
      fdi_var =
        FDI_VAR,
      domain_vars =
        DOMAIN_VARS,
      run_domains =
        RUN_DOMAINS,
      run_specifications =
        RUN_SPECIFICATIONS,
      voter_controls =
        V2_VOTER_CONTROLS,
      context_controls =
        C1_CONTEXT_CONTROLS,
      state_year_fixed_effects =
        USE_STATE_YEAR_FIXED_EFFECTS,
      lmer_reml =
        LMER_REML,
      informative_reference =
        informative_reference
    ),
    results = all_results
  ),
  file.path(
    output_root,
    "respondent_two_level_ac_hierarchy_bundle.rds"
  )
)

# ============================================================
# 15. CONSOLE READOUT
# ============================================================

message("")
message("============================================================")
message("RESPONDENT TWO-LEVEL AC HIERARCHY COMPLETE")
message("============================================================")
message(
  "Output root: ",
  output_root
)
message("")
message("Hierarchy:")
message(
  "  M0 = lm, no random intercept"
)
message(
  "  M1 = lmer, AC random intercept"
)
message("")
message(
  "No district random effect or district support audit is used in this version."
)
message("")
message(
  "Restricted-sample check: two-way restricted sample is exactly the triple sample within each domain."
)
message("")
message("Mixed-model singularity / convergence audit:")
print(
  mixed_model_diagnostics,
  n = Inf,
  width = Inf
)

if (
  any(
    mixed_model_diagnostics$singular,
    na.rm = TRUE
  )
) {
  message("")
  message(
    "NOTE: At least one AC-random-intercept fit is singular. ",
    "Inspect 06_mixed_model_singularity_and_convergence_audit.csv and ",
    "04_random_effect_variance_components.csv."
  )
}

if (
  any(
    mixed_model_diagnostics$convergence_problem,
    na.rm = TRUE
  )
) {
  message("")
  message(
    "NOTE: At least one AC-random-intercept fit has a nonempty lme4 ",
    "convergence message. Inspect the diagnostic audit before interpretation."
  )
}

message("")
message("Sample-size comparison:")
print(
  restricted_sample_comparison,
  n = Inf
)

message("")
message(
  "Substantive percentage-point contrasts:"
)
print(
  substantive_contrasts_all |>
    dplyr::select(
      domain,
      specification,
      hierarchy_level,
      contrast_type,
      ideology_group,
      estimate_pp,
      std_error_pp,
      conf_low_pp,
      conf_high_pp,
      p_value
    ),
  n = Inf,
  width = Inf
)

message("")
message("Focal native-unit coefficients:")
print(
  focal_coefficients |>
    dplyr::select(
      domain,
      specification,
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
message("AC ICC preview:")
print(
  icc_all,
  n = Inf
)

message("============================================================")

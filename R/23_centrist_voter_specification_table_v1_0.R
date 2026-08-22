# ============================================================
# 23_centrist_voter_specification_table_v1_0.R
# Revision: 2026-08-20-v1.0
#
# Runs the 10 voter-level specifications developed after the
# multilevel-support diagnostics.
#
# Common preferred adjustment set in every fitted model:
#   state fixed effects
#   C1 AC controls: population, land area, SC share, ST share
#   V2 voter controls: religion, caste, education
#   AC random intercept: (1 | ac_random)
#   normalized 2014 NES survey weights as lmer prior weights
#   ML estimation (REML = FALSE)
#
# NOTATION
#   F1 = log(1 + local manufacturing FDI/100k, 2009-14)
#   F0 = log(1 + local manufacturing FDI/100k, 2004-09)
#   dF = F1 - F0
#   M  = 2001 Muslim share, percentage points
#   C  = Center-voter indicator
#   D1 = any manufacturing FDI in 2009-14
#   D0 = any manufacturing FDI in 2004-09
#
# IMPORTANT
# 1. dF alone imposes beta_F1 = -beta_F0 when rewritten in levels.
#    dF + F0 is simply a reparameterization of F1 + F0.
# 2. M*dF + M*F0 is algebraically equivalent to M*F1 + M*F0.
#    S02 and S08 are both fit and explicitly checked for equivalence.
# 3. Three-way interactions obey strong hierarchy. S03 and S04 include
#    every constituent main effect and two-way interaction automatically.
# 4. Binary FDI is treated as an extensive-margin LEVEL measure, not as
#    a generic "change" dummy.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages()
paths <- build_project_paths(project_root)

if (!requireNamespace("lme4", quietly = TRUE)) {
  stop("Package 'lme4' is required.")
}

REV <- "2026-08-20-v1.0"

OUTCOME <- "voted_bjp"
WEIGHT <- "survey_weight_norm_year"
MUSLIM <- "muslim_share_2001_dist_proxy"

FDI_CURRENT_LOG <- "log1p_fdi_mfg_local_all_pc100k_2014"
FDI_BASELINE_LOG <- "log1p_fdi_mfg_local_all_pc100k_2009"
FDI_CURRENT_COUNT <- "fdi_mfg_local_all_n_2014"
FDI_BASELINE_COUNT <- "fdi_mfg_local_all_n_2009"

C1_RAW <- c(
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share"
)

V2_RAW <- c(
  "religion_group",
  "caste_group",
  "education_harmonized"
)

C1_TERMS <- c(
  "ac_pop_100k",
  "land_area",
  "sc_share_pp",
  "st_share_pp"
)

V2_TERMS <- c(
  "religion_x",
  "caste_x",
  "education_x"
)

COMMON_TERMS <- c(
  "state_fe",
  C1_TERMS,
  V2_TERMS
)

out_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_voter_specification_table",
  REV
)
out_data <- file.path(out_root, "data")
out_models <- file.path(out_root, "models")

purrr::walk(
  c(out_root, out_data, out_models),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Load and attach AC-level variables
# ------------------------------------------------------------

respondents <- readRDS(
  file.path(paths$final_dir, "nes_respondent_analysis.rds")
)

ac_change <- readRDS(
  file.path(paths$final_dir, "ac_change.rds")
)

attach_missing <- function(data, ac, vars) {
  miss <- setdiff(vars, names(data))
  if (!length(miss)) return(data)

  absent <- setdiff(miss, names(ac))
  if (length(absent)) {
    stop(
      "Missing from both respondent data and ac_change: ",
      paste(absent, collapse = ", ")
    )
  }

  payload <- ac |>
    dplyr::select(ac_uid, dplyr::all_of(miss)) |>
    dplyr::distinct(ac_uid, .keep_all = TRUE)

  if (anyDuplicated(payload$ac_uid)) {
    stop("ac_change payload is not unique by ac_uid.")
  }

  n0 <- nrow(data)
  out <- data |>
    dplyr::left_join(
      payload,
      by = "ac_uid",
      relationship = "many-to-one"
    )

  if (nrow(out) != n0) {
    stop("AC join changed respondent row count.")
  }

  out
}

respondents <- attach_missing(
  respondents,
  ac_change,
  c(
    MUSLIM,
    FDI_CURRENT_LOG,
    FDI_BASELINE_LOG,
    FDI_CURRENT_COUNT,
    FDI_BASELINE_COUNT,
    C1_RAW
  )
)

required <- c(
  "year", OUTCOME, "vote_valid", "bjp_candidate_present",
  "ac_uid", "state_no", "ideology_complete", WEIGHT,
  "voter_ideology", MUSLIM,
  FDI_CURRENT_LOG, FDI_BASELINE_LOG,
  FDI_CURRENT_COUNT, FDI_BASELINE_COUNT,
  C1_RAW, V2_RAW
)

missing_required <- setdiff(required, names(respondents))
if (length(missing_required)) {
  stop(
    "Missing required variables: ",
    paste(missing_required, collapse = ", ")
  )
}

if (!"respondent_sample_candidate_present" %in% names(respondents)) {
  respondents <- respondents |>
    dplyr::mutate(
      respondent_sample_candidate_present =
        dplyr::coalesce(vote_valid, FALSE) &
        !is.na(.data[[OUTCOME]]) &
        !is.na(bjp_candidate_present) &
        bjp_candidate_present == 1
    )
}

if (!"center_harmonized" %in% names(respondents)) {
  respondents <- respondents |>
    dplyr::mutate(
      center_harmonized = dplyr::case_when(
        !ideology_complete ~ NA_real_,
        voter_ideology == "Center" ~ 1,
        TRUE ~ 0
      )
    )
}

# ------------------------------------------------------------
# Common 2014 analysis variables
# ------------------------------------------------------------

base_all <- respondents |>
  dplyr::filter(
    year == 2014,
    dplyr::coalesce(respondent_sample_candidate_present, FALSE),
    ideology_complete,
    !is.na(voter_ideology),
    !is.na(center_harmonized),
    !is.na(ac_uid),
    !is.na(state_no),
    !is.na(.data[[OUTCOME]]),
    is.finite(.data[[MUSLIM]]),
    is.finite(.data[[FDI_CURRENT_LOG]]),
    is.finite(.data[[FDI_BASELINE_LOG]]),
    is.finite(.data[[FDI_CURRENT_COUNT]]),
    is.finite(.data[[FDI_BASELINE_COUNT]]),
    is.finite(.data[[WEIGHT]]),
    .data[[WEIGHT]] > 0
  ) |>
  dplyr::mutate(
    y = as.numeric(.data[[OUTCOME]]),

    muslim_pp = 100 * as.numeric(.data[[MUSLIM]]),

    fdi_current = as.numeric(.data[[FDI_CURRENT_LOG]]),
    fdi_baseline = as.numeric(.data[[FDI_BASELINE_LOG]]),
    delta_fdi = fdi_current - fdi_baseline,

    any_fdi_current =
      as.numeric(as.numeric(.data[[FDI_CURRENT_COUNT]]) > 0),
    any_fdi_baseline =
      as.numeric(as.numeric(.data[[FDI_BASELINE_COUNT]]) > 0),

    center_binary = as.numeric(center_harmonized == 1),

    ideology4 = factor(
      as.character(voter_ideology),
      levels = c("Left", "Center", "Right", "Mixed")
    ),

    fdi_transition = factor(
      dplyr::case_when(
        any_fdi_baseline == 0 & any_fdi_current == 0 ~ "Never",
        any_fdi_baseline == 0 & any_fdi_current == 1 ~ "Entry",
        any_fdi_baseline == 1 & any_fdi_current == 0 ~ "Exit",
        any_fdi_baseline == 1 & any_fdi_current == 1 ~ "Persistent",
        TRUE ~ NA_character_
      ),
      levels = c("Never", "Entry", "Exit", "Persistent")
    ),

    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    land_area = as.numeric(con08_land_area),
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),

    state_fe = factor(state_no),
    ac_random = factor(ac_uid),
    religion_x = factor(religion_group),
    caste_x = factor(caste_group),
    education_x = factor(education_harmonized),

    model_weight = as.numeric(.data[[WEIGHT]])
  )

if (!nrow(base_all)) stop("Common 2014 analysis sample is empty.")

if (!all(unique(base_all$y) %in% c(0, 1))) {
  stop("Outcome is not coded 0/1.")
}

sample_all <- base_all
sample_center <- base_all |>
  dplyr::filter(center_binary == 1)
sample_positive_current_fdi <- sample_center |>
  dplyr::filter(any_fdi_current == 1)

analysis_samples <- list(
  all = sample_all,
  center = sample_center,
  positive_current_fdi = sample_positive_current_fdi
)

# ------------------------------------------------------------
# Ten-model specification registry
# ------------------------------------------------------------

spec_registry <- tibble::tribble(
  ~spec_id, ~short_name, ~question, ~role, ~sample_key, ~rhs_focal, ~interpretation,

  "S01", "current_exposure_center",
  "Current globalization exposure",
  "Primary centrist-only", "center",
  "muslim_pp * fdi_current + fdi_baseline",
  "Current FDI x Muslim among Center voters, controlling additively for prior FDI.",

  "S02", "dynamic_saturated_center",
  "Current exposure conditional on prior joint exposure",
  "Primary stronger centrist-only", "center",
  "muslim_pp * fdi_current + muslim_pp * fdi_baseline",
  "Allows current and prior FDI to have separate Muslim-share interactions.",

  "S03", "center_vs_noncenter_pooled",
  "Is the joint FDI-demographic relationship different for centrists?",
  "Primary pooled heterogeneity", "all",
  "fdi_current * muslim_pp * center_binary + fdi_baseline",
  "Uses all voters and formally tests Center vs noncenter heterogeneity with a hierarchical three-way interaction.",

  "S04", "four_ideology_saturated",
  "Relax common slopes across Left, Center, Right, Mixed",
  "Robustness", "all",
  "fdi_current * muslim_pp * ideology4 + fdi_baseline",
  "Fully saturates the current FDI x Muslim relationship by four-category ideology.",

  "S05", "extensive_margin_center",
  "Extensive margin: any versus no current FDI",
  "Important robustness", "center",
  "muslim_pp * any_fdi_current + any_fdi_baseline",
  "Tests any vs no current FDI while controlling additively for prior any-FDI status.",

  "S06", "dynamic_binary_center",
  "Dynamic extensive margin",
  "Important robustness", "center",
  "muslim_pp * any_fdi_current + muslim_pp * any_fdi_baseline",
  "Allows current and prior any-FDI status to have separate Muslim-share interactions.",

  "S07", "change_score_center",
  "Change/acceleration in logged FDI",
  "Robustness", "center",
  "muslim_pp * delta_fdi",
  "Uses net logged-FDI change alone and therefore imposes stronger dynamic restrictions.",

  "S08", "baseline_adjusted_change_center",
  "Baseline-adjusted change parameterization",
  "Equivalence / robustness", "center",
  "muslim_pp * delta_fdi + muslim_pp * fdi_baseline",
  "Algebraically equivalent to S02 when all lower-order and interaction terms are included.",

  "S09", "intensity_among_recipients",
  "FDI intensity among current recipients",
  "Appendix / exploratory", "positive_current_fdi",
  "muslim_pp * fdi_current + fdi_baseline",
  "Restricts to current FDI recipients and asks whether FDI intensity interacts with Muslim share.",

  "S10", "fdi_transition_center",
  "Entry / exit / persistence of FDI exposure",
  "Potential mechanism robustness", "center",
  "muslim_pp * fdi_transition",
  "Compares Never, Entry, Exit, and Persistent exposure histories; Never is the reference."
)

readr::write_csv(
  spec_registry,
  file.path(out_data, "01_specification_registry.csv")
)

# ------------------------------------------------------------
# Fit models
# ------------------------------------------------------------

complete_finite <- function(data, vars) {
  keep <- rep(TRUE, nrow(data))

  for (v in unique(vars)) {
    x <- data[[v]]
    if (is.numeric(x) || is.integer(x)) {
      keep <- keep & !is.na(x) & is.finite(as.numeric(x))
    } else {
      keep <- keep & !is.na(x)
    }
  }

  keep
}

make_formula <- function(rhs_focal) {
  rhs <- paste(c(rhs_focal, COMMON_TERMS), collapse = " + ")
  stats::as.formula(
    paste0("y ~ ", rhs, " + (1 | ac_random)")
  )
}

fit_spec <- function(row) {
  dd <- analysis_samples[[row$sample_key[[1]]]]
  fml <- make_formula(row$rhs_focal[[1]])

  needed <- unique(c(all.vars(fml), "model_weight"))
  dd <- dd[complete_finite(dd, needed), , drop = FALSE]
  dd <- droplevels(dd)

  if (!nrow(dd)) stop("Zero complete cases.")
  if (dplyr::n_distinct(dd$ac_random) < 2) {
    stop("Fewer than two ACs remain.")
  }

  warns <- character()

  fit <- withCallingHandlers(
    lme4::lmer(
      fml,
      data = dd,
      weights = model_weight,
      REML = FALSE,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 300000)
      )
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    fit = fit,
    data = dd,
    formula = paste(deparse(fml), collapse = ""),
    warnings = unique(warns),
    short_name = row$short_name[[1]]
  )
}

attempts <- purrr::map(
  seq_len(nrow(spec_registry)),
  function(i) {
    row <- spec_registry[i, , drop = FALSE]
    tryCatch(
      list(ok = TRUE, result = fit_spec(row), error = ""),
      error = function(e) {
        list(ok = FALSE, result = NULL, error = conditionMessage(e))
      }
    )
  }
)
names(attempts) <- spec_registry$spec_id

fit_status <- tibble::tibble(
  spec_id = names(attempts),
  fit_success = vapply(attempts, function(x) x$ok, logical(1)),
  fit_error = vapply(attempts, function(x) x$error, character(1))
)

fits <- lapply(attempts, function(x) x$result)
fits <- fits[!vapply(fits, is.null, logical(1))]

# ------------------------------------------------------------
# Raw coefficient table
# ------------------------------------------------------------

tidy_fixed <- function(fit) {
  ct <- stats::coef(summary(fit))
  est <- as.numeric(ct[, "Estimate"])
  se <- as.numeric(ct[, "Std. Error"])
  z <- est / se

  tibble::tibble(
    term = rownames(ct),
    estimate = est,
    se = se,
    z_normal_approx = z,
    p_normal_approx =
      2 * stats::pnorm(abs(z), lower.tail = FALSE),
    conf_low_95 = est - stats::qnorm(.975) * se,
    conf_high_95 = est + stats::qnorm(.975) * se
  )
}

coef_out <- purrr::imap_dfr(
  fits,
  function(obj, id) {
    tidy_fixed(obj$fit) |>
      dplyr::mutate(
        spec_id = id,
        short_name = obj$short_name,
        .before = 1
      )
  }
)

readr::write_csv(
  coef_out,
  file.path(out_data, "02_model_coefficients.csv")
)

# ------------------------------------------------------------
# Model fit / random-effect table
# ------------------------------------------------------------

random_stats <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  ac <- vc |>
    dplyr::filter(grp == "ac_random", is.na(var2))

  if (nrow(ac) != 1) {
    return(tibble::tibble(
      ac_random_intercept_sd = NA_real_,
      residual_sd = stats::sigma(fit),
      icc_ac = NA_real_
    ))
  }

  av <- ac$vcov[[1]]
  rv <- stats::sigma(fit)^2

  tibble::tibble(
    ac_random_intercept_sd = sqrt(av),
    residual_sd = sqrt(rv),
    icc_ac = av / (av + rv)
  )
}

conv_message <- function(fit) {
  x <- fit@optinfo$conv$lme4$messages
  if (is.null(x) || !length(x)) "" else paste(unique(x), collapse = " | ")
}

fit_success_out <- purrr::imap_dfr(
  fits,
  function(obj, id) {
    tibble::tibble(
      spec_id = id,
      formula = obj$formula,
      n_respondents = stats::nobs(obj$fit),
      n_acs = dplyr::n_distinct(obj$data$ac_uid),
      n_states = dplyr::n_distinct(obj$data$state_no),
      n_center_voters = sum(obj$data$center_binary == 1),
      n_noncenter_voters = sum(obj$data$center_binary == 0),
      AIC = stats::AIC(obj$fit),
      BIC = stats::BIC(obj$fit),
      logLik = as.numeric(stats::logLik(obj$fit)),
      singular = lme4::isSingular(obj$fit, tol = 1e-4),
      convergence_message = conv_message(obj$fit),
      fit_warning_messages = paste(obj$warnings, collapse = " | ")
    ) |>
      dplyr::bind_cols(random_stats(obj$fit))
  }
)

fit_out <- spec_registry |>
  dplyr::select(spec_id, short_name, question, role) |>
  dplyr::left_join(fit_status, by = "spec_id") |>
  dplyr::left_join(fit_success_out, by = "spec_id")

readr::write_csv(
  fit_out,
  file.path(out_data, "03_model_fit_random_effects.csv")
)

saveRDS(
  lapply(fits, function(x) x$fit),
  file.path(out_models, "voter_specification_models.rds")
)

# ------------------------------------------------------------
# Linear-combination utilities
# ------------------------------------------------------------

components <- function(term) {
  sort(strsplit(term, ":", fixed = TRUE)[[1]])
}

find_term <- function(fit, wanted, allow_none = FALSE) {
  wanted <- sort(wanted)
  nms <- names(lme4::fixef(fit))

  hit <- nms[
    vapply(
      nms,
      function(x) identical(components(x), wanted),
      logical(1)
    )
  ]

  if (length(hit) == 1) return(hit[[1]])
  if (allow_none && length(hit) == 0) return(NA_character_)

  stop(
    "Could not uniquely identify term: ",
    paste(wanted, collapse = " : "),
    ". Matches: ",
    paste(hit, collapse = ", ")
  )
}

lincom <- function(fit, weights, id, estimand, group = NA_character_) {
  b <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  L <- setNames(rep(0, length(b)), names(b))

  if (length(setdiff(names(weights), names(L)))) {
    stop("Contrast references an absent coefficient.")
  }

  L[names(weights)] <- weights
  est <- sum(L * b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  z <- est / se

  tibble::tibble(
    spec_id = id,
    estimand = estimand,
    comparison_group = group,
    estimate = est,
    se = se,
    z_normal_approx = z,
    p_normal_approx = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    conf_low_95 = est - stats::qnorm(.975) * se,
    conf_high_95 = est + stats::qnorm(.975) * se
  )
}

one_term <- function(fit, wanted, id, estimand, group = NA_character_) {
  term <- find_term(fit, wanted)
  lincom(fit, setNames(1, term), id, estimand, group)
}

estimands <- list()
push <- function(x) estimands[[length(estimands) + 1]] <<- x

# S01
if ("S01" %in% names(fits)) {
  push(one_term(
    fits$S01$fit,
    c("muslim_pp", "fdi_current"),
    "S01",
    "Current FDI x Muslim interaction among Center voters"
  ))
}

# S02
if ("S02" %in% names(fits)) {
  push(one_term(
    fits$S02$fit,
    c("muslim_pp", "fdi_current"),
    "S02",
    "Current FDI x Muslim interaction conditional on prior interaction"
  ))
  push(one_term(
    fits$S02$fit,
    c("muslim_pp", "fdi_baseline"),
    "S02",
    "Prior FDI x Muslim interaction"
  ))
}

# S03: noncenter slope, Center slope, formal difference
if ("S03" %in% names(fits)) {
  f <- fits$S03$fit
  fm <- find_term(f, c("fdi_current", "muslim_pp"))
  fmc <- find_term(f, c("fdi_current", "muslim_pp", "center_binary"))

  push(lincom(
    f, setNames(1, fm), "S03",
    "Current FDI x Muslim interaction", "Noncenter"
  ))
  push(lincom(
    f, setNames(c(1, 1), c(fm, fmc)), "S03",
    "Current FDI x Muslim interaction", "Center"
  ))
  push(lincom(
    f, setNames(1, fmc), "S03",
    "Difference in current FDI x Muslim interaction",
    "Center - Noncenter"
  ))
}

# S04: ideology-specific FDI x Muslim slopes
if ("S04" %in% names(fits)) {
  f <- fits$S04$fit
  fm <- find_term(f, c("fdi_current", "muslim_pp"))

  push(lincom(
    f, setNames(1, fm), "S04",
    "Current FDI x Muslim interaction by ideology", "Left"
  ))

  for (g in c("Center", "Right", "Mixed")) {
    triple <- find_term(
      f,
      c("fdi_current", "muslim_pp", paste0("ideology4", g)),
      allow_none = TRUE
    )

    if (!is.na(triple)) {
      push(lincom(
        f, setNames(c(1, 1), c(fm, triple)), "S04",
        "Current FDI x Muslim interaction by ideology", g
      ))
      push(lincom(
        f, setNames(1, triple), "S04",
        "Difference from Left in current FDI x Muslim interaction",
        paste0(g, " - Left")
      ))
    }
  }
}

# S05
if ("S05" %in% names(fits)) {
  push(one_term(
    fits$S05$fit,
    c("muslim_pp", "any_fdi_current"),
    "S05",
    "Any-current-FDI x Muslim interaction among Center voters"
  ))
}

# S06
if ("S06" %in% names(fits)) {
  push(one_term(
    fits$S06$fit,
    c("muslim_pp", "any_fdi_current"),
    "S06",
    "Current any-FDI x Muslim interaction conditional on prior binary interaction"
  ))
  push(one_term(
    fits$S06$fit,
    c("muslim_pp", "any_fdi_baseline"),
    "S06",
    "Prior any-FDI x Muslim interaction"
  ))
}

# S07
if ("S07" %in% names(fits)) {
  push(one_term(
    fits$S07$fit,
    c("muslim_pp", "delta_fdi"),
    "S07",
    "Change in logged FDI x Muslim interaction"
  ))
}

# S08
if ("S08" %in% names(fits)) {
  push(one_term(
    fits$S08$fit,
    c("muslim_pp", "delta_fdi"),
    "S08",
    "Delta-FDI interaction in baseline-adjusted change coordinates"
  ))
  push(one_term(
    fits$S08$fit,
    c("muslim_pp", "fdi_baseline"),
    "S08",
    "Baseline interaction coefficient in change coordinates"
  ))
}

# S09
if ("S09" %in% names(fits)) {
  push(one_term(
    fits$S09$fit,
    c("muslim_pp", "fdi_current"),
    "S09",
    "Current FDI-intensity x Muslim interaction among current recipients"
  ))
}

# S10: transition-specific Muslim slopes and differences from Never
if ("S10" %in% names(fits)) {
  f <- fits$S10$fit
  m <- find_term(f, "muslim_pp")

  push(lincom(
    f, setNames(1, m), "S10",
    "Muslim-share slope by FDI transition history", "Never"
  ))

  for (g in c("Entry", "Exit", "Persistent")) {
    it <- find_term(
      f,
      c("muslim_pp", paste0("fdi_transition", g)),
      allow_none = TRUE
    )

    if (!is.na(it)) {
      push(lincom(
        f, setNames(c(1, 1), c(m, it)), "S10",
        "Muslim-share slope by FDI transition history", g
      ))
      push(lincom(
        f, setNames(1, it), "S10",
        "Difference from Never in Muslim-share slope",
        paste0(g, " - Never")
      ))
    }
  }
}

focal_out <- if (length(estimands)) {
  dplyr::bind_rows(estimands) |>
    dplyr::left_join(
      spec_registry |>
        dplyr::select(spec_id, short_name, question, role),
      by = "spec_id"
    ) |>
    dplyr::relocate(spec_id, short_name, question, role)
} else {
  tibble::tibble()
}

readr::write_csv(
  focal_out,
  file.path(out_data, "04_focal_estimands.csv")
)

# ------------------------------------------------------------
# S02 versus S08 equivalence audit
# ------------------------------------------------------------

equiv <- tibble::tibble()

if ("S02" %in% names(fits) && "S08" %in% names(fits)) {
  f2 <- fits$S02$fit
  f8 <- fits$S08$fit
  b2 <- lme4::fixef(f2)
  b8 <- lme4::fixef(f8)

  s02_f1 <- find_term(f2, "fdi_current")
  s02_f0 <- find_term(f2, "fdi_baseline")
  s02_mf1 <- find_term(f2, c("muslim_pp", "fdi_current"))
  s02_mf0 <- find_term(f2, c("muslim_pp", "fdi_baseline"))

  s08_df <- find_term(f8, "delta_fdi")
  s08_f0 <- find_term(f8, "fdi_baseline")
  s08_mdf <- find_term(f8, c("muslim_pp", "delta_fdi"))
  s08_mf0 <- find_term(f8, c("muslim_pp", "fdi_baseline"))

  tol <- 1e-6

  checks <- c(
    identical_rows =
      as.numeric(identical(rownames(model.frame(f2)), rownames(model.frame(f8)))),

    max_abs_fitted_difference =
      max(abs(stats::fitted(f2) - stats::fitted(f8))),

    absolute_logLik_difference =
      abs(as.numeric(stats::logLik(f2)) - as.numeric(stats::logLik(f8))),

    delta_main_minus_current_main =
      abs(b8[[s08_df]] - b2[[s02_f1]]),

    change_baseline_main_minus_level_sum =
      abs(b8[[s08_f0]] - (b2[[s02_f0]] + b2[[s02_f1]])),

    delta_interaction_minus_current_interaction =
      abs(b8[[s08_mdf]] - b2[[s02_mf1]]),

    change_baseline_interaction_minus_level_sum =
      abs(b8[[s08_mf0]] - (b2[[s02_mf0]] + b2[[s02_mf1]]))
  )

  equiv <- tibble::tibble(
    check = names(checks),
    value = as.numeric(checks),
    tolerance = c(0, rep(tol, length(checks) - 1)),
    pass = c(
      checks[["identical_rows"]] == 1,
      checks[-1] < tol
    )
  )
}

readr::write_csv(
  equiv,
  file.path(out_data, "05_spec2_spec8_equivalence_audit.csv")
)

# ------------------------------------------------------------
# Transition and sample-support audits
# ------------------------------------------------------------

transition_support <- sample_center |>
  dplyr::summarise(
    n_center_respondents = dplyr::n(),
    weighted_center_respondents = sum(model_weight),
    n_acs = dplyr::n_distinct(ac_uid),
    n_states = dplyr::n_distinct(state_no),
    .by = fdi_transition
  ) |>
  dplyr::arrange(fdi_transition)

readr::write_csv(
  transition_support,
  file.path(out_data, "06_fdi_transition_support.csv")
)

sample_support <- purrr::imap_dfr(
  analysis_samples,
  function(dd, key) {
    ac <- dd |>
      dplyr::distinct(
        ac_uid, state_no,
        any_fdi_current, any_fdi_baseline,
        fdi_transition,
        fdi_current, fdi_baseline, delta_fdi,
        muslim_pp
      )

    tibble::tibble(
      sample_key = key,
      n_respondents = nrow(dd),
      n_acs = dplyr::n_distinct(dd$ac_uid),
      n_states = dplyr::n_distinct(dd$state_no),
      n_current_any_fdi_acs = sum(ac$any_fdi_current == 1),
      n_current_no_fdi_acs = sum(ac$any_fdi_current == 0),
      n_baseline_any_fdi_acs = sum(ac$any_fdi_baseline == 1),
      n_baseline_no_fdi_acs = sum(ac$any_fdi_baseline == 0),
      sd_fdi_current = stats::sd(ac$fdi_current),
      sd_fdi_baseline = stats::sd(ac$fdi_baseline),
      sd_delta_fdi = stats::sd(ac$delta_fdi),
      sd_muslim_pp = stats::sd(ac$muslim_pp)
    )
  }
)

readr::write_csv(
  sample_support,
  file.path(out_data, "07_analysis_sample_support.csv")
)

# ------------------------------------------------------------
# Human-readable guide
# ------------------------------------------------------------

guide <- c(
  "# Voter-Level Specification Guide",
  "",
  paste0("Revision: ", REV),
  "",
  "## Common estimator",
  "",
  "Every successful model is a survey-weighted linear mixed probability model estimated by maximum likelihood with state fixed effects, C1 constituency controls, V2 voter controls, and an AC random intercept.",
  "",
  "## Why level and change specifications are related",
  "",
  "Let F1 be logged FDI in 2009-14, F0 logged FDI in 2004-09, and dF = F1 - F0.",
  "",
  "A model with dF alone can be rewritten beta*dF = beta*F1 - beta*F0, so it imposes equal-and-opposite coefficients on the two period levels.",
  "",
  "Once F0 is also included, dF + F0 and F1 + F0 are simply reparameterizations of the same model. The same result holds for M*dF + M*F0 versus M*F1 + M*F0. S02 and S08 therefore provide the same fitted model in different coordinates. The script checks this numerically in 05_spec2_spec8_equivalence_audit.csv.",
  "",
  "## Why the pooled three-way models are hierarchical",
  "",
  "S03 uses fdi_current * muslim_pp * center_binary. In R, * expands to all three main effects, all three two-way interactions, and the three-way interaction. No required lower-order interaction is omitted.",
  "",
  "S04 does the same with the four-category ideology factor.",
  "",
  "## What each specification answers",
  "",
  "S01: M*F1 + F0, Center voters. Current FDI x Muslim relationship controlling additively for prior FDI.",
  "",
  "S02: M*F1 + M*F0, Center voters. Flexible dynamic level model; current and prior FDI each have their own Muslim interaction.",
  "",
  "S03: F1*M*C + F0, all voters. Formal Center-versus-noncenter heterogeneity test. Report the Center slope, noncenter slope, and their difference from 04_focal_estimands.csv.",
  "",
  "S04: F1*M*ideology4 + F0, all voters. Fully relaxes the current FDI x Muslim relationship across Left, Center, Right, Mixed.",
  "",
  "S05: M*D1 + D0, Center voters. Extensive margin: any versus no current FDI, controlling prior any-FDI status.",
  "",
  "S06: M*D1 + M*D0, Center voters. Dynamic binary model; current and prior any-FDI status each have their own Muslim interaction.",
  "",
  "S07: M*dF, Center voters. Restrictive change/acceleration specification.",
  "",
  "S08: M*dF + M*F0, Center voters. Algebraically equivalent to S02, retained as an equivalence demonstration rather than independent evidence.",
  "",
  "S09: M*F1 + F0 among Center voters in D1=1 ACs only. Intensive-margin model among recipients; interpret cautiously if exposed AC support is small.",
  "",
  "S10: M*transition, Center voters. Never/Entry/Exit/Persistent exposure histories. Inspect 06_fdi_transition_support.csv before interpreting.",
  "",
  "## Suggested reporting hierarchy",
  "",
  "Primary centrist-only: S02.",
  "Primary pooled heterogeneity test: S03.",
  "Four-ideology robustness: S04.",
  "Extensive-margin robustness: S05 and S06.",
  "Change robustness: S07.",
  "S08: equivalence demonstration only.",
  "Intensity among recipients: S09 appendix/exploratory.",
  "Transition mechanism: S10 only if cell support is adequate.",
  "",
  "Do not infer heterogeneity because one subgroup coefficient is significant and another is not. Use the pooled difference estimands reported in 04_focal_estimands.csv."
)

writeLines(
  guide,
  file.path(out_root, "MODEL_SPECIFICATION_GUIDE.md")
)

# ------------------------------------------------------------
# Console output
# ------------------------------------------------------------

message("")
message("============================================================")
message("VOTER-LEVEL SPECIFICATION SUITE COMPLETE")
message("============================================================")
message("Output root: ", out_root)

message("")
message("Fit status:")
print(
  spec_registry |>
    dplyr::select(spec_id, short_name, role) |>
    dplyr::left_join(fit_status, by = "spec_id"),
  n = Inf,
  width = Inf
)

message("")
message("Sample support:")
print(sample_support, n = Inf, width = Inf)

message("")
message("Transition support:")
print(transition_support, n = Inf, width = Inf)

message("")
message("Focal estimands:")
print(focal_out, n = Inf, width = Inf)

if (nrow(equiv)) {
  message("")
  message("S02 / S08 equivalence audit:")
  print(equiv, n = Inf, width = Inf)
}

message("")
message("Guide: ", file.path(out_root, "MODEL_SPECIFICATION_GUIDE.md"))
message("============================================================")

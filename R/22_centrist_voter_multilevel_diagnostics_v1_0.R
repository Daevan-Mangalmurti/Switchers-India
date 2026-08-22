# ============================================================
# 22_centrist_voter_multilevel_diagnostics_v1_0.R
# Revision: 2026-08-20-v1.0
#
# Diagnostics for large standard errors in 2014 Center-voter
# multilevel linear probability models.
#
# Compares:
#   A. Full-sample TRUE change in logged manufacturing FDI per 100k
#   B. Positive-FDI-only TRUE change in logged manufacturing FDI per 100k
#   C. Full-sample binary any-vs-no manufacturing FDI, 2009-14
#
# Main diagnostic questions:
#   1. How many independent ACs actually identify the focal terms?
#   2. How many Center respondents are observed within each AC?
#   3. How much Muslim-share / FDI / interaction variation survives state FE?
#   4. Is multicollinearity inflating the interaction SE?
#   5. At which model step do the focal SEs grow?
#   6. Do survey weights materially change precision?
#   7. Are LPM predictions frequently outside [0,1]?
#   8. Is the focal interaction driven by one state?
#
# Every mixed model uses:
#   (1 | ac_uid)
#
# The script writes CSV diagnostics plus publication-quality review figures.
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
RUN_LEAVE_ONE_STATE_OUT <- TRUE

# ------------------------------------------------------------
# 1. Frozen variables
# ------------------------------------------------------------

OUTCOME <- "voted_bjp"
MUSLIM <- "muslim_share_2001_dist_proxy"
WEIGHT <- "survey_weight_norm_year"

FDI_CURRENT_LOG <- "log1p_fdi_mfg_local_all_pc100k_2014"
FDI_BASELINE_LOG <- "log1p_fdi_mfg_local_all_pc100k_2009"
FDI_CURRENT_COUNT <- "fdi_mfg_local_all_n_2014"

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

out_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_centrist_multilevel_diagnostics",
  REV
)

out_data <- file.path(out_root, "data")
out_fig <- file.path(out_root, "figures")
out_models <- file.path(out_root, "models")

purrr::walk(
  c(out_root, out_data, out_fig, out_models),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. Load data and attach AC fields
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
      "Variables missing from both respondent data and ac_change: ",
      paste(absent, collapse = ", ")
    )
  }

  payload <- ac |>
    dplyr::select(ac_uid, dplyr::all_of(miss)) |>
    dplyr::distinct(ac_uid, .keep_all = TRUE)

  if (anyDuplicated(payload$ac_uid)) {
    stop("AC payload is not unique by ac_uid.")
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
    C1_RAW
  )
)

required <- c(
  "year", OUTCOME, "vote_valid", "bjp_candidate_present",
  "ac_uid", "state_no", "ideology_complete", WEIGHT,
  MUSLIM, FDI_CURRENT_LOG, FDI_BASELINE_LOG, FDI_CURRENT_COUNT,
  C1_RAW, V2_RAW
)

missing <- setdiff(required, names(respondents))
if (length(missing)) {
  stop("Missing required variables: ", paste(missing, collapse = ", "))
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
  if (!"voter_ideology" %in% names(respondents)) {
    stop("Need center_harmonized or voter_ideology.")
  }

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
# 3. Construct the common 2014 Center-voter base sample
# ------------------------------------------------------------

base <- respondents |>
  dplyr::filter(
    year == 2014,
    dplyr::coalesce(respondent_sample_candidate_present, FALSE),
    ideology_complete,
    !is.na(center_harmonized),
    center_harmonized == 1,
    !is.na(ac_uid),
    !is.na(state_no),
    is.finite(.data[[MUSLIM]]),
    is.finite(.data[[FDI_CURRENT_LOG]]),
    is.finite(.data[[FDI_BASELINE_LOG]]),
    is.finite(.data[[FDI_CURRENT_COUNT]]),
    is.finite(.data[[WEIGHT]]),
    .data[[WEIGHT]] > 0
  ) |>
  dplyr::mutate(
    y = as.numeric(.data[[OUTCOME]]),

    muslim_pp = 100 * as.numeric(.data[[MUSLIM]]),

    fdi_log_current = as.numeric(.data[[FDI_CURRENT_LOG]]),
    fdi_log_baseline = as.numeric(.data[[FDI_BASELINE_LOG]]),
    delta_log_fdi = fdi_log_current - fdi_log_baseline,

    fdi_current_count = as.numeric(.data[[FDI_CURRENT_COUNT]]),
    any_fdi_0914 = as.numeric(fdi_current_count > 0),

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

if (!nrow(base)) stop("2014 Center-voter base sample is empty.")

# ------------------------------------------------------------
# 4. Define the three diagnostic analyses
# ------------------------------------------------------------

analysis_data <- list(
  full_delta_log = base,
  positive_fdi_delta_log = base |>
    dplyr::filter(any_fdi_0914 == 1),
  full_any_fdi_binary = base
)

analysis_exposure <- c(
  full_delta_log = "delta_log_fdi",
  positive_fdi_delta_log = "delta_log_fdi",
  full_any_fdi_binary = "any_fdi_0914"
)

analysis_label <- c(
  full_delta_log = "Full sample: change in logged FDI",
  positive_fdi_delta_log = "Positive-FDI ACs: change in logged FDI",
  full_any_fdi_binary = "Full sample: any vs no FDI"
)

# ------------------------------------------------------------
# 5. Sample size, AC count, and cluster-size diagnostics
# ------------------------------------------------------------

kish_ess <- function(w) {
  w <- as.numeric(w)
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

cluster_sizes <- purrr::imap_dfr(
  analysis_data,
  function(dd, key) {
    dd |>
      dplyr::summarise(
        n_center_voters = dplyr::n(),
        weighted_center_voters = sum(model_weight),
        .by = c(ac_uid, state_no)
      ) |>
      dplyr::mutate(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  cluster_sizes,
  file.path(out_data, "01_ac_center_voter_cluster_sizes.csv")
)

sample_summary <- purrr::imap_dfr(
  analysis_data,
  function(dd, key) {
    sizes <- dd |>
      dplyr::count(ac_uid, name = "n")

    tibble::tibble(
      analysis = key,
      analysis_label = unname(analysis_label[[key]]),
      n_respondents = nrow(dd),
      n_acs = dplyr::n_distinct(dd$ac_uid),
      n_states = dplyr::n_distinct(dd$state_no),
      respondents_per_ac_mean = mean(sizes$n),
      respondents_per_ac_median = stats::median(sizes$n),
      respondents_per_ac_p25 = as.numeric(stats::quantile(sizes$n, .25)),
      respondents_per_ac_p75 = as.numeric(stats::quantile(sizes$n, .75)),
      respondents_per_ac_max = max(sizes$n),
      kish_weight_ess = kish_ess(dd$model_weight),
      weighted_bjp_vote_rate = stats::weighted.mean(dd$y, dd$model_weight)
    )
  }
)

readr::write_csv(
  sample_summary,
  file.path(out_data, "02_analysis_sample_summary.csv")
)

# ------------------------------------------------------------
# 6. Unique-AC support datasets
# ------------------------------------------------------------

ac_level <- purrr::imap_dfr(
  analysis_data,
  function(dd, key) {
    exposure <- analysis_exposure[[key]]

    dd |>
      dplyr::summarise(
        state_no = dplyr::first(state_no),
        muslim_pp = dplyr::first(muslim_pp),
        exposure = dplyr::first(.data[[exposure]]),
        delta_log_fdi = dplyr::first(delta_log_fdi),
        any_fdi_0914 = dplyr::first(any_fdi_0914),
        fdi_current_count = dplyr::first(fdi_current_count),
        ac_pop_100k = dplyr::first(ac_pop_100k),
        land_area = dplyr::first(land_area),
        sc_share_pp = dplyr::first(sc_share_pp),
        st_share_pp = dplyr::first(st_share_pp),
        n_center_voters = dplyr::n(),
        weighted_center_voters = sum(model_weight),
        .by = ac_uid
      ) |>
      dplyr::mutate(
        focal_interaction = muslim_pp * exposure,
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  ac_level,
  file.path(out_data, "03_unique_ac_support.csv")
)

summarize_support <- function(x) {
  x <- x[is.finite(x)]
  tibble::tibble(
    n = length(x),
    n_unique = dplyr::n_distinct(x),
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    p10 = as.numeric(stats::quantile(x, .10)),
    p25 = as.numeric(stats::quantile(x, .25)),
    median = stats::median(x),
    p75 = as.numeric(stats::quantile(x, .75)),
    p90 = as.numeric(stats::quantile(x, .90)),
    max = max(x)
  )
}

support_summary <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    aa <- ac_level |>
      dplyr::filter(analysis == key)

    dplyr::bind_rows(
      summarize_support(aa$muslim_pp) |>
        dplyr::mutate(variable = "muslim_pp", .before = 1),
      summarize_support(aa$exposure) |>
        dplyr::mutate(variable = analysis_exposure[[key]], .before = 1),
      summarize_support(aa$focal_interaction) |>
        dplyr::mutate(variable = "muslim_x_exposure", .before = 1)
    ) |>
      dplyr::mutate(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  support_summary,
  file.path(out_data, "04_level2_support_summary.csv")
)

# ------------------------------------------------------------
# 7. Binary-treatment balance and state support
# ------------------------------------------------------------

binary_balance_state <- base |>
  dplyr::summarise(
    n_respondents = dplyr::n(),
    n_acs = dplyr::n_distinct(ac_uid),
    n_any_fdi_respondents = sum(any_fdi_0914 == 1),
    n_no_fdi_respondents = sum(any_fdi_0914 == 0),
    n_any_fdi_acs = dplyr::n_distinct(ac_uid[any_fdi_0914 == 1]),
    n_no_fdi_acs = dplyr::n_distinct(ac_uid[any_fdi_0914 == 0]),
    has_both_fdi_categories =
      dplyr::n_distinct(any_fdi_0914) == 2,
    .by = state_no
  ) |>
  dplyr::arrange(state_no)

readr::write_csv(
  binary_balance_state,
  file.path(out_data, "05_binary_fdi_balance_by_state.csv")
)

# ------------------------------------------------------------
# 8. How much variation survives state fixed effects?
# ------------------------------------------------------------

within_state_rows <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    aa <- ac_level |>
      dplyr::filter(analysis == key)

    aa |>
      dplyr::summarise(
        n_acs = dplyr::n(),
        muslim_sd = stats::sd(muslim_pp),
        exposure_sd = stats::sd(exposure),
        interaction_sd = stats::sd(focal_interaction),
        n_unique_exposure = dplyr::n_distinct(exposure),
        has_exposure_variation = dplyr::n_distinct(exposure) > 1,
        .by = state_no
      ) |>
      dplyr::mutate(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  within_state_rows,
  file.path(out_data, "06_within_state_support_by_state.csv")
)

within_state_global <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    aa <- ac_level |>
      dplyr::filter(analysis == key) |>
      dplyr::mutate(
        muslim_within = muslim_pp - mean(muslim_pp),
        exposure_within = exposure - mean(exposure),
        interaction_within =
          focal_interaction - mean(focal_interaction),
        .by = state_no
      )

    tibble::tibble(
      analysis = key,
      analysis_label = unname(analysis_label[[key]]),

      total_sd_muslim = stats::sd(aa$muslim_pp),
      within_state_sd_muslim = stats::sd(aa$muslim_within),

      total_sd_exposure = stats::sd(aa$exposure),
      within_state_sd_exposure = stats::sd(aa$exposure_within),

      total_sd_interaction = stats::sd(aa$focal_interaction),
      within_state_sd_interaction = stats::sd(aa$interaction_within),

      retained_share_muslim =
        within_state_sd_muslim / total_sd_muslim,

      retained_share_exposure =
        within_state_sd_exposure / total_sd_exposure,

      retained_share_interaction =
        within_state_sd_interaction / total_sd_interaction,

      states_with_exposure_variation =
        sum(
          (aa |>
             dplyr::summarise(
               v = dplyr::n_distinct(exposure) > 1,
               .by = state_no
             ))$v
        ),
      n_states = dplyr::n_distinct(aa$state_no)
    )
  }
)

readr::write_csv(
  within_state_global,
  file.path(out_data, "07_variation_retained_after_state_fe.csv")
)

# ------------------------------------------------------------
# 9. AC-level correlations and VIF diagnostics
# ------------------------------------------------------------

corr_vars <- c(
  "muslim_pp",
  "exposure",
  "focal_interaction",
  C1_TERMS
)

correlation_long <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    aa <- ac_level |>
      dplyr::filter(analysis == key) |>
      dplyr::select(dplyr::all_of(corr_vars))

    cc <- stats::cor(
      aa,
      use = "pairwise.complete.obs"
    )

    as.data.frame(as.table(cc)) |>
      tibble::as_tibble() |>
      dplyr::rename(
        variable_1 = Var1,
        variable_2 = Var2,
        correlation = Freq
      ) |>
      dplyr::mutate(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  correlation_long,
  file.path(out_data, "08_ac_level_correlations.csv")
)

manual_vif <- function(data, target_vars, state_var = "state_no") {
  purrr::map_dfr(
    target_vars,
    function(target) {
      others <- setdiff(target_vars, target)

      dd <- data |>
        dplyr::select(
          dplyr::all_of(c(target, others, state_var))
        ) |>
        tidyr::drop_na()

      if (
        nrow(dd) < 10 ||
        dplyr::n_distinct(dd[[target]]) < 2
      ) {
        return(
          tibble::tibble(
            term = target,
            n_acs = nrow(dd),
            r2_from_other_regressors = NA_real_,
            vif = NA_real_
          )
        )
      }

      rhs <- paste(
        c(others, paste0("factor(", state_var, ")")),
        collapse = " + "
      )

      fml <- stats::as.formula(
        paste0(target, " ~ ", rhs)
      )

      fit <- stats::lm(fml, data = dd)
      r2 <- summary(fit)$r.squared

      tibble::tibble(
        term = target,
        n_acs = nrow(dd),
        r2_from_other_regressors = r2,
        vif = if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf
      )
    }
  )
}

vif_out <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    aa <- ac_level |>
      dplyr::filter(analysis == key)

    manual_vif(
      aa,
      target_vars = corr_vars
    ) |>
      dplyr::mutate(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        .before = 1
      )
  }
)

readr::write_csv(
  vif_out,
  file.path(out_data, "09_ac_level_vif_with_state_fe.csv")
)

# ------------------------------------------------------------
# 10. Refit the five-model sequence for precision decomposition
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

fit_lmer <- function(
    data,
    exposure,
    interaction,
    state_fe,
    c1,
    v2,
    use_weights = TRUE
) {
  focal <- if (interaction) {
    paste0("muslim_pp * ", exposure)
  } else {
    paste0("muslim_pp + ", exposure)
  }

  extra <- c(
    if (state_fe) "state_fe",
    if (c1) C1_TERMS,
    if (v2) V2_TERMS
  )

  rhs <- paste(c(focal, extra), collapse = " + ")
  ftxt <- paste0("y ~ ", rhs, " + (1 | ac_random)")
  fml <- stats::as.formula(ftxt)

  vars <- unique(c(
    all.vars(fml),
    if (use_weights) "model_weight"
  ))

  dd <- data[complete_finite(data, vars), , drop = FALSE]
  dd <- droplevels(dd)

  warns <- character()

  fit <- withCallingHandlers(
    if (use_weights) {
      lme4::lmer(
        fml,
        data = dd,
        weights = model_weight,
        REML = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 200000)
        )
      )
    } else {
      lme4::lmer(
        fml,
        data = dd,
        REML = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 200000)
        )
      )
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    fit = fit,
    data = dd,
    formula = ftxt,
    warnings = unique(warns)
  )
}

fit_sequence <- function(data, exposure, use_weights = TRUE) {
  list(
    m1 = fit_lmer(data, exposure, FALSE, FALSE, FALSE, FALSE, use_weights),
    m2 = fit_lmer(data, exposure, TRUE,  FALSE, FALSE, FALSE, use_weights),
    m3 = fit_lmer(data, exposure, TRUE,  TRUE,  FALSE, FALSE, use_weights),
    m4 = fit_lmer(data, exposure, TRUE,  TRUE,  TRUE,  FALSE, use_weights),
    m5 = fit_lmer(data, exposure, TRUE,  TRUE,  TRUE,  TRUE,  use_weights)
  )
}

all_fits <- purrr::imap(
  analysis_data,
  ~ fit_sequence(.x, analysis_exposure[[.y]], use_weights = TRUE)
)

interaction_term <- function(fit, exposure) {
  candidates <- c(
    paste0("muslim_pp:", exposure),
    paste0(exposure, ":muslim_pp")
  )

  hit <- intersect(
    candidates,
    rownames(stats::coef(summary(fit)))
  )

  if (length(hit) != 1) {
    stop("Could not uniquely identify interaction term.")
  }

  hit[[1]]
}

random_stats <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  rr <- vc |>
    dplyr::filter(grp == "ac_random", is.na(var2))

  ac_var <- rr$vcov[[1]]
  resid_var <- stats::sigma(fit)^2

  list(
    ac_sd = sqrt(ac_var),
    resid_sd = sqrt(resid_var),
    icc = ac_var / (ac_var + resid_var)
  )
}

precision_progression <- purrr::imap_dfr(
  all_fits,
  function(fit_list, key) {
    exposure <- analysis_exposure[[key]]

    purrr::imap_dfr(
      fit_list,
      function(obj, model) {
        ct <- stats::coef(summary(obj$fit))
        rs <- random_stats(obj$fit)

        if (model == "m1") {
          term <- NA_character_
          est <- NA_real_
          se <- NA_real_
        } else {
          term <- interaction_term(obj$fit, exposure)
          est <- ct[term, "Estimate"]
          se <- ct[term, "Std. Error"]
        }

        tibble::tibble(
          analysis = key,
          analysis_label = unname(analysis_label[[key]]),
          model = model,
          interaction_term = term,
          interaction_estimate = est,
          interaction_se = se,
          n_respondents = nrow(obj$data),
          n_acs = dplyr::n_distinct(obj$data$ac_uid),
          n_states = dplyr::n_distinct(obj$data$state_no),
          ac_random_intercept_sd = rs$ac_sd,
          residual_sd = rs$resid_sd,
          icc_ac = rs$icc,
          singular = lme4::isSingular(obj$fit, tol = 1e-4),
          warnings = paste(obj$warnings, collapse = " | "),
          formula = obj$formula
        )
      }
    )
  }
) |>
  dplyr::group_by(analysis) |>
  dplyr::mutate(
    se_relative_to_m2 =
      interaction_se /
      interaction_se[model == "m2"][1]
  ) |>
  dplyr::ungroup()

readr::write_csv(
  precision_progression,
  file.path(out_data, "10_interaction_precision_across_models.csv")
)

saveRDS(
  lapply(
    all_fits,
    function(x) lapply(x, `[[`, "fit")
  ),
  file.path(out_models, "diagnostic_weighted_models.rds")
)

# ------------------------------------------------------------
# 11. Survey-weight sensitivity of preferred M5
# ------------------------------------------------------------

weight_sensitivity <- purrr::map_dfr(
  names(analysis_data),
  function(key) {
    dd <- analysis_data[[key]]
    exposure <- analysis_exposure[[key]]

    weighted <- all_fits[[key]]$m5

    unweighted <- fit_lmer(
      dd,
      exposure,
      interaction = TRUE,
      state_fe = TRUE,
      c1 = TRUE,
      v2 = TRUE,
      use_weights = FALSE
    )

    extract <- function(obj, weighting) {
      fit <- obj$fit
      term <- interaction_term(fit, exposure)
      ct <- stats::coef(summary(fit))

      tibble::tibble(
        analysis = key,
        analysis_label = unname(analysis_label[[key]]),
        weighting = weighting,
        interaction_estimate = ct[term, "Estimate"],
        interaction_se = ct[term, "Std. Error"],
        n_respondents = nrow(obj$data),
        n_acs = dplyr::n_distinct(obj$data$ac_uid),
        singular = lme4::isSingular(fit, tol = 1e-4)
      )
    }

    dplyr::bind_rows(
      extract(weighted, "NES survey weighted"),
      extract(unweighted, "Unweighted")
    )
  }
)

readr::write_csv(
  weight_sensitivity,
  file.path(out_data, "11_m5_weight_sensitivity.csv")
)

# ------------------------------------------------------------
# 12. LPM prediction and residual audit
# ------------------------------------------------------------

prediction_audit <- purrr::imap_dfr(
  all_fits,
  function(fit_list, key) {
    obj <- fit_list$m5
    fit <- obj$fit

    pred_conditional <- stats::predict(fit)
    pred_fixed_only <- stats::predict(fit, re.form = NA)
    resid <- obj$data$y - pred_conditional

    tibble::tibble(
      analysis = key,
      analysis_label = unname(analysis_label[[key]]),
      n = length(pred_conditional),

      conditional_pred_min = min(pred_conditional),
      conditional_pred_max = max(pred_conditional),
      conditional_share_below_0 = mean(pred_conditional < 0),
      conditional_share_above_1 = mean(pred_conditional > 1),
      conditional_share_outside_01 =
        mean(pred_conditional < 0 | pred_conditional > 1),

      fixed_pred_min = min(pred_fixed_only),
      fixed_pred_max = max(pred_fixed_only),
      fixed_share_below_0 = mean(pred_fixed_only < 0),
      fixed_share_above_1 = mean(pred_fixed_only > 1),
      fixed_share_outside_01 =
        mean(pred_fixed_only < 0 | pred_fixed_only > 1),

      residual_mean = mean(resid),
      residual_sd = stats::sd(resid),
      rmse = sqrt(mean(resid^2))
    )
  }
)

readr::write_csv(
  prediction_audit,
  file.path(out_data, "12_lpm_prediction_residual_audit.csv")
)

# ------------------------------------------------------------
# 13. Random-effect estimates
# ------------------------------------------------------------

random_effects_out <- purrr::imap_dfr(
  all_fits,
  function(fit_list, key) {
    re <- lme4::ranef(fit_list$m5$fit)$ac_random

    tibble::tibble(
      ac_uid = rownames(re),
      random_intercept = re[[1]],
      analysis = key,
      analysis_label = unname(analysis_label[[key]])
    )
  }
)

readr::write_csv(
  random_effects_out,
  file.path(out_data, "13_m5_ac_random_effects.csv")
)

# ------------------------------------------------------------
# 14. Leave-one-state-out influence diagnostic
# ------------------------------------------------------------

loo_fit_one <- function(data, exposure, omitted_state) {
  dd <- data |>
    dplyr::filter(state_no != omitted_state) |>
    droplevels()

  out <- tryCatch(
    {
      obj <- fit_lmer(
        dd,
        exposure,
        interaction = TRUE,
        state_fe = TRUE,
        c1 = TRUE,
        v2 = TRUE,
        use_weights = TRUE
      )

      term <- interaction_term(obj$fit, exposure)
      ct <- stats::coef(summary(obj$fit))

      tibble::tibble(
        omitted_state = as.character(omitted_state),
        estimate = ct[term, "Estimate"],
        se = ct[term, "Std. Error"],
        n_respondents = nrow(obj$data),
        n_acs = dplyr::n_distinct(obj$data$ac_uid),
        singular = lme4::isSingular(obj$fit, tol = 1e-4),
        warnings = paste(obj$warnings, collapse = " | "),
        error = ""
      )
    },
    error = function(e) {
      tibble::tibble(
        omitted_state = as.character(omitted_state),
        estimate = NA_real_,
        se = NA_real_,
        n_respondents = NA_integer_,
        n_acs = NA_integer_,
        singular = NA,
        warnings = "",
        error = conditionMessage(e)
      )
    }
  )

  out
}

if (RUN_LEAVE_ONE_STATE_OUT) {
  loo_out <- purrr::map_dfr(
    names(analysis_data),
    function(key) {
      dd <- analysis_data[[key]]
      exposure <- analysis_exposure[[key]]
      states <- sort(unique(dd$state_no))

      full_fit <- all_fits[[key]]$m5$fit
      full_term <- interaction_term(full_fit, exposure)
      full_ct <- stats::coef(summary(full_fit))
      full_est <- full_ct[full_term, "Estimate"]

      purrr::map_dfr(
        states,
        ~ loo_fit_one(dd, exposure, .x)
      ) |>
        dplyr::mutate(
          analysis = key,
          analysis_label = unname(analysis_label[[key]]),
          full_sample_estimate = full_est,
          sign_differs_from_full =
            is.finite(estimate) &
            sign(estimate) != sign(full_est),
          .before = 1
        )
    }
  )

  readr::write_csv(
    loo_out,
    file.path(out_data, "14_leave_one_state_out_m5.csv")
  )

  loo_summary <- loo_out |>
    dplyr::summarise(
      full_sample_estimate = dplyr::first(full_sample_estimate),
      n_successful = sum(is.finite(estimate)),
      min_leave_one_out_estimate = min(estimate, na.rm = TRUE),
      max_leave_one_out_estimate = max(estimate, na.rm = TRUE),
      sd_leave_one_out_estimate = stats::sd(estimate, na.rm = TRUE),
      n_sign_flips = sum(sign_differs_from_full, na.rm = TRUE),
      .by = c(analysis, analysis_label)
    )

  readr::write_csv(
    loo_summary,
    file.path(out_data, "15_leave_one_state_out_summary.csv")
  )
}

# ------------------------------------------------------------
# 15. Figures
# ------------------------------------------------------------

save_plot <- function(plot, stem, width = 8, height = 5.5) {
  ggplot2::ggsave(
    file.path(out_fig, paste0(stem, ".png")),
    plot,
    width = width,
    height = height,
    dpi = 400
  )

  ggplot2::ggsave(
    file.path(out_fig, paste0(stem, ".pdf")),
    plot,
    width = width,
    height = height
  )
}

# A. Full-sample AC support scatter.
plot_full_support <- ac_level |>
  dplyr::filter(analysis == "full_delta_log") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = exposure,
      y = muslim_pp,
      size = n_center_voters
    )
  ) +
  ggplot2::geom_point(alpha = .65) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "AC-level support for the continuous interaction",
    subtitle = "2014 Center-voter sample; point size is number of sampled Center voters",
    x = "Change in log(1 + manufacturing FDI projects per 100,000)",
    y = "Muslim population share, 2001 (percentage points)",
    size = "Center voters"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

save_plot(
  plot_full_support,
  "01_full_sample_ac_interaction_support"
)

# B. Positive-FDI-only support.
plot_positive_support <- ac_level |>
  dplyr::filter(analysis == "positive_fdi_delta_log") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = exposure,
      y = muslim_pp,
      size = n_center_voters
    )
  ) +
  ggplot2::geom_point(alpha = .65) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Interaction support after restricting to FDI-receiving ACs",
    subtitle = "Look for loss of AC count and compression of the FDI range",
    x = "Change in log(1 + manufacturing FDI projects per 100,000)",
    y = "Muslim population share, 2001 (percentage points)",
    size = "Center voters"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

save_plot(
  plot_positive_support,
  "02_positive_fdi_ac_interaction_support"
)

# C. Binary FDI support.
plot_binary <- ac_level |>
  dplyr::filter(analysis == "full_any_fdi_binary") |>
  dplyr::mutate(
    fdi_group = factor(
      exposure,
      levels = c(0, 1),
      labels = c("No FDI", "Any FDI")
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = fdi_group,
      y = muslim_pp
    )
  ) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(width = .15, alpha = .35) +
  ggplot2::labs(
    title = "Muslim-share support in the binary FDI specification",
    subtitle = "Each point is an assembly constituency",
    x = NULL,
    y = "Muslim population share, 2001 (percentage points)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

save_plot(
  plot_binary,
  "03_binary_fdi_muslim_support"
)

# D. Respondents per AC.
plot_clusters <- cluster_sizes |>
  ggplot2::ggplot(
    ggplot2::aes(x = n_center_voters)
  ) +
  ggplot2::geom_histogram(binwidth = 1, boundary = 0) +
  ggplot2::facet_wrap(
    ggplot2::vars(analysis_label),
    scales = "free_y"
  ) +
  ggplot2::labs(
    title = "How many Center voters are observed per AC?",
    x = "Center respondents per assembly constituency",
    y = "Number of assembly constituencies"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

save_plot(
  plot_clusters,
  "04_center_voters_per_ac",
  width = 10,
  height = 6
)

# E. SE progression across model sequence.
plot_precision <- precision_progression |>
  dplyr::filter(model != "m1") |>
  dplyr::mutate(
    model = factor(
      model,
      levels = c("m2", "m3", "m4", "m5"),
      labels = c(
        "Interaction",
        "+ State FE",
        "+ FE + C1",
        "+ FE + C1 + V2"
      )
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = model,
      y = interaction_se,
      group = analysis_label
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(
    ggplot2::vars(analysis_label),
    scales = "free_y"
  ) +
  ggplot2::labs(
    title = "Where does the interaction standard error increase?",
    x = NULL,
    y = "Standard error of Muslim share × FDI interaction"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

save_plot(
  plot_precision,
  "05_interaction_se_progression",
  width = 10,
  height = 6
)

# F. VIF.
plot_vif <- vif_out |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(term, vif),
      y = vif
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(
    ggplot2::vars(analysis_label),
    scales = "free_y"
  ) +
  ggplot2::labs(
    title = "AC-level multicollinearity after accounting for state",
    subtitle = "VIF is based on unique ACs; state indicators and other displayed AC regressors predict each term",
    x = NULL,
    y = "Variance inflation factor"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

save_plot(
  plot_vif,
  "06_ac_level_vif",
  width = 10,
  height = 6
)

# G. Within-state exposure variation.
plot_within <- within_state_rows |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(as.factor(state_no), exposure_sd),
      y = exposure_sd
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(
    ggplot2::vars(analysis_label),
    scales = "free"
  ) +
  ggplot2::labs(
    title = "Within-state FDI variation available to state-FE models",
    x = "State",
    y = "Within-state SD of focal FDI exposure"
  ) +
  ggplot2::theme_minimal(base_size = 9) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

save_plot(
  plot_within,
  "07_within_state_fdi_variation",
  width = 11,
  height = 7
)

# H. Weighted vs unweighted preferred-model SE.
plot_weights <- weight_sensitivity |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = weighting,
      y = interaction_se,
      group = analysis_label
    )
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(
    ggplot2::vars(analysis_label),
    scales = "free_y"
  ) +
  ggplot2::labs(
    title = "Does survey weighting materially affect interaction precision?",
    x = NULL,
    y = "M5 interaction standard error"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

save_plot(
  plot_weights,
  "08_weighted_unweighted_se_comparison",
  width = 10,
  height = 6
)

# I. Leave-one-state-out influence.
if (RUN_LEAVE_ONE_STATE_OUT) {
  plot_loo <- loo_out |>
    dplyr::filter(is.finite(estimate)) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = reorder(omitted_state, estimate),
        y = estimate
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = estimate - 1.96 * se,
        ymax = estimate + 1.96 * se
      ),
      width = .15
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = full_sample_estimate),
      linetype = "dashed"
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ggplot2::vars(analysis_label),
      scales = "free"
    ) +
    ggplot2::labs(
      title = "Leave-one-state-out sensitivity of the preferred interaction",
      subtitle = "Dashed line is the full-sample M5 estimate",
      x = "Omitted state",
      y = "Muslim share × FDI interaction estimate"
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )

  save_plot(
    plot_loo,
    "09_leave_one_state_out_interaction",
    width = 11,
    height = 7
  )
}

# ------------------------------------------------------------
# 16. Human-readable diagnostic summary
# ------------------------------------------------------------

summary_lines <- c(
  paste0("Revision: ", REV),
  "",
  "INTERPRETATION GUIDE",
  "",
  "1. 02_analysis_sample_summary.csv",
  "   Focus on N ACs, not only N respondents. Very small median respondents per AC means the voter-level N greatly overstates independent level-2 information.",
  "",
  "2. 04_level2_support_summary.csv and Figures 01-03",
  "   Check range and SD of Muslim share, FDI, and their interaction. Compression after positive-FDI restriction means less information and normally larger SEs.",
  "",
  "3. 07_variation_retained_after_state_fe.csv",
  "   retained_share_exposure and retained_share_interaction show how much variation remains after demeaning by state. Small values explain why state FE enlarge SEs.",
  "",
  "4. 09_ac_level_vif_with_state_fe.csv",
  "   As a rule of thumb, VIF around 1-2 is mild; >5 deserves attention; >10 indicates severe linear redundancy. Do not mechanically delete theoretically required variables solely to lower VIF.",
  "",
  "5. 10_interaction_precision_across_models.csv and Figure 05",
  "   This directly shows whether the SE jumps when adding state FE, C1 controls, V2 controls, or because of sample loss.",
  "",
  "6. 11_m5_weight_sensitivity.csv",
  "   Large weighted/unweighted SE differences indicate unequal survey weights are reducing effective information. The weighted model should remain primary if weights are substantively required.",
  "",
  "7. 12_lpm_prediction_residual_audit.csv",
  "   A substantial share of fitted probabilities outside [0,1] strengthens the case for a multilevel-logit robustness check.",
  "",
  "8. 14_leave_one_state_out_m5.csv / 15_leave_one_state_out_summary.csv",
  "   Sign flips or large movements after omitting a single state indicate limited geographic support and influential states.",
  "",
  "The purpose is not to find a specification with the smallest SE. It is to learn which source of limited information produces the uncertainty."
)

writeLines(
  summary_lines,
  file.path(out_root, "DIAGNOSTIC_INTERPRETATION_GUIDE.txt")
)

message("")
message("============================================================")
message("CENTRIST-VOTER MULTILEVEL DIAGNOSTICS COMPLETE")
message("============================================================")
message("Output root: ", out_root)
message("")
message("Sample summary:")
print(sample_summary, width = Inf)
message("")
message("Variation retained after state FE:")
print(within_state_global, width = Inf)
message("")
message("Interaction precision progression:")
print(
  precision_progression |>
    dplyr::select(
      analysis,
      model,
      interaction_estimate,
      interaction_se,
      se_relative_to_m2,
      n_respondents,
      n_acs,
      icc_ac,
      singular
    ),
  n = Inf,
  width = Inf
)
message("")
message("VIF diagnostics:")
print(vif_out, n = Inf, width = Inf)
message("")
message("Interpretation guide: ",
        file.path(out_root, "DIAGNOSTIC_INTERPRETATION_GUIDE.txt"))
message("============================================================")

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(lme4)
})

# =============================================================================
# R38C4 v1.0
# Center-reference pooled four-ideology voter triple-interaction mixed LPM
#
# Re-estimates the pooled four-ideology model with CENTER as the explicit
# reference category.
#
# Primary cells:
#   1. Total FDI, raw
#   2. Manufacturing FDI, raw
#
# Outcome:
#   Whether a 2014 respondent voted BJP (0/1)
#
# Model:
#   ideology4 * (
#       muslim * fdi_current +
#       muslim * fdi_baseline
#   )
# + common nuisance controls
# + state fixed effects
# + shared AC random intercept
#
# Center-reference interpretation:
#   muslim:fdi_current
#       = Center current FDI x Muslim interaction
#
#   ideology4Left:muslim:fdi_current
#       = Left minus Center difference
#
#   ideology4Right:muslim:fdi_current
#       = Right minus Center difference
#
#   ideology4Mixed:muslim:fdi_current
#       = Mixed minus Center difference
#
# Wald outputs:
#   - all six pairwise current interaction comparisons
#   - Center-vs-Left / Right / Mixed
#   - 3-df omnibus equality test
#   - corresponding baseline tests
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

input_dir <- file.path(
  project_root, "data", "derived", "switchers_rewrite", "final"
)
output_dir <- file.path(
  project_root, "outputs",
  "r38c4_voter_center_reference_four_ideology_wald_v1_0"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

respondent_path <- file.path(input_dir, "nes_respondent_analysis.rds")
change_path <- file.path(input_dir, "ac_change.rds")

required_files <- c(respondent_path, change_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Missing required input(s): ",
    paste(missing_files, collapse = ", ")
  )
}

respondents <- readRDS(respondent_path)
ac_change <- readRDS(change_path)

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      label, " is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
}

individual_controls <- c(
  "religion_group",
  "caste_group",
  "education_harmonized"
)

primary_ac_controls <- c(
  "proxy_ac_pop",
  "sc_pop_share",
  "st_pop_share"
)

respondent_required <- c(
  "respondent_uid", "year", "state_no", "ac_uid",
  "vote_valid", "voted_bjp",
  "bjp_candidate_present", "fdi_spatial_support",
  "ideology_complete", "voter_ideology",
  "muslim_share_2001_dist_proxy",
  individual_controls,
  primary_ac_controls
)

require_columns(
  respondents,
  respondent_required,
  "nes_respondent_analysis"
)

fdi_variables <- c(
  "fdi_total_local_all_pc100k_2009",
  "fdi_total_local_all_pc100k_2014",
  "fdi_mfg_local_all_pc100k_2009",
  "fdi_mfg_local_all_pc100k_2014"
)

require_columns(
  ac_change,
  c("ac_uid", fdi_variables),
  "ac_change"
)

fdi_payload <- ac_change |>
  select(ac_uid, all_of(fdi_variables))

if (anyDuplicated(fdi_payload$ac_uid) > 0L) {
  stop("ac_change FDI payload is not unique by ac_uid.")
}

respondents <- respondents |>
  select(-any_of(fdi_variables)) |>
  left_join(
    fdi_payload,
    by = "ac_uid",
    relationship = "many-to-one"
  )

if (anyDuplicated(respondents$respondent_uid) > 0L) {
  stop("Respondent data are not unique by respondent_uid.")
}

relevel_if_present <- function(x, reference) {
  out <- factor(as.character(x))
  if (reference %in% levels(out)) {
    out <- stats::relevel(out, ref = reference)
  }
  out
}

respondents <- respondents |>
  mutate(
    y = as.numeric(voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),

    total_current =
      as.numeric(fdi_total_local_all_pc100k_2014),
    total_baseline =
      as.numeric(fdi_total_local_all_pc100k_2009),

    mfg_current =
      as.numeric(fdi_mfg_local_all_pc100k_2014),
    mfg_baseline =
      as.numeric(fdi_mfg_local_all_pc100k_2009),

    ac_pop_100k =
      as.numeric(proxy_ac_pop) / 100000,

    sc_share_pp =
      100 * as.numeric(sc_pop_share),

    st_share_pp =
      100 * as.numeric(st_pop_share),

    state_fe =
      factor(state_no),

    ac_random =
      factor(ac_uid),

    religion_x =
      relevel_if_present(religion_group, "1: Hindu"),

    caste_x =
      relevel_if_present(caste_group, "4: Others"),

    education_x =
      relevel_if_present(
        education_harmonized,
        "Secondary"
      ),

    # Explicit CENTER reference.
    ideology4 =
      factor(
        as.character(voter_ideology),
        levels = c(
          "Center",
          "Left",
          "Right",
          "Mixed"
        )
      )
  )

if (
  any(
    !is.na(respondents$y) &
      !respondents$y %in% c(0, 1)
  )
) {
  stop("voted_bjp is not coded 0/1.")
}

analysis_base <- respondents |>
  filter(
    year == 2014,
    vote_valid %in% TRUE,
    !is.na(y),
    ideology_complete %in% TRUE,
    !is.na(ideology4),
    bjp_candidate_present %in% TRUE,
    fdi_spatial_support %in% TRUE,
    is.finite(muslim),
    is.finite(ac_pop_100k),
    is.finite(sc_share_pp),
    is.finite(st_share_pp),
    !is.na(state_fe),
    !is.na(ac_random),
    !is.na(religion_x),
    !is.na(caste_x),
    !is.na(education_x)
  )

cell_registry <- tribble(
  ~cell_id, ~sector, ~current_col, ~baseline_col,
  "total_raw", "Total",
  "total_current", "total_baseline",
  "manufacturing_raw", "Manufacturing",
  "mfg_current", "mfg_baseline"
)

pair_registry <- tribble(
  ~contrast_id, ~ideology_a, ~ideology_b,
  "center_vs_left", "Center", "Left",
  "center_vs_right", "Center", "Right",
  "center_vs_mixed", "Center", "Mixed",
  "left_vs_right", "Left", "Right",
  "left_vs_mixed", "Left", "Mixed",
  "right_vs_mixed", "Right", "Mixed"
)

optimizer_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 300000)
)

collapse_messages <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  paste(unique(as.character(x)), collapse = " | ")
}

complete_model_data <- function(data, variables) {
  keep <- rep(TRUE, nrow(data))

  for (variable in unique(variables)) {
    x <- data[[variable]]

    if (is.numeric(x) || is.integer(x)) {
      keep <- keep &
        !is.na(x) &
        is.finite(as.numeric(x))
    } else {
      keep <- keep &
        !is.na(x)
    }
  }

  data[keep, , drop = FALSE]
}

fit_lmer_captured <- function(formula, data) {
  dd <- complete_model_data(
    data,
    all.vars(formula)
  ) |>
    droplevels()

  warnings <- character()
  messages <- character()

  fit <- withCallingHandlers(
    lmer(
      formula,
      data = dd,
      REML = FALSE,
      control = optimizer_control
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  list(
    fit = fit,
    data = dd,
    warnings = unique(warnings),
    messages = unique(messages)
  )
}

model_formula <- as.formula(
  paste0(
    "y ~ ",
    "ideology4 * (",
    "muslim * fdi_current + ",
    "muslim * fdi_baseline",
    ") + ",
    "ac_pop_100k + ",
    "sc_share_pp + ",
    "st_share_pp + ",
    "religion_x + ",
    "caste_x + ",
    "education_x + ",
    "state_fe + ",
    "(1 | ac_random)"
  )
)

find_exact_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <- coefficient_names[
    vapply(
      strsplit(
        coefficient_names,
        ":",
        fixed = TRUE
      ),
      function(pieces) {
        length(pieces) == length(variables) &&
          setequal(pieces, variables)
      },
      logical(1)
    )
  ]

  if (length(hits) != 1L) {
    stop(
      "Could not uniquely identify interaction among: ",
      paste(variables, collapse = ", "),
      ". Hits: ",
      paste(hits, collapse = ", ")
    )
  }

  hits[[1]]
}

linear_combo_lmer <- function(fit, weights) {
  beta <- fixef(fit)
  V <- as.matrix(vcov(fit))

  unknown <- setdiff(names(weights), names(beta))
  if (length(unknown) > 0L) {
    stop(
      "Unknown coefficient(s): ",
      paste(unknown, collapse = ", ")
    )
  }

  L <- setNames(rep(0, length(beta)), names(beta))
  L[names(weights)] <- as.numeric(weights)

  estimate <- sum(L * beta)
  variance <- as.numeric(t(L) %*% V %*% L)
  variance <- max(variance, 0)
  se <- sqrt(variance)

  z <- if (se == 0) {
    ifelse(estimate == 0, 0, sign(estimate) * Inf)
  } else {
    estimate / se
  }

  tibble(
    estimate = estimate,
    std_error = se,
    conf_low = estimate - 1.96 * se,
    conf_high = estimate + 1.96 * se,
    statistic = z,
    wald_chisq_1df = z^2,
    p_value = pchisq(z^2, df = 1, lower.tail = FALSE)
  )
}

group_interaction_weights <- function(
  fit,
  ideology_group,
  period = c("current", "baseline")
) {
  period <- match.arg(period)

  beta_names <- names(fixef(fit))

  exposure_name <- if (period == "current") {
    "fdi_current"
  } else {
    "fdi_baseline"
  }

  center_term <- find_exact_interaction_term(
    beta_names,
    c("muslim", exposure_name)
  )

  if (ideology_group == "Center") {
    return(
      setNames(1, center_term)
    )
  }

  difference_term <- find_exact_interaction_term(
    beta_names,
    c(
      paste0("ideology4", ideology_group),
      "muslim",
      exposure_name
    )
  )

  setNames(
    c(1, 1),
    c(
      center_term,
      difference_term
    )
  )
}

difference_weights <- function(
  fit,
  ideology_a,
  ideology_b,
  period
) {
  wa <- group_interaction_weights(
    fit,
    ideology_a,
    period
  )

  wb <- group_interaction_weights(
    fit,
    ideology_b,
    period
  )

  terms <- union(
    names(wa),
    names(wb)
  )

  out <- setNames(
    rep(0, length(terms)),
    terms
  )

  out[names(wa)] <-
    out[names(wa)] +
    wa

  out[names(wb)] <-
    out[names(wb)] -
    wb

  out[
    abs(out) >
      .Machine$double.eps
  ]
}

omnibus_center_reference <- function(
  fit,
  period = c("current", "baseline")
) {
  period <- match.arg(period)

  beta <- fixef(fit)
  V <- as.matrix(vcov(fit))

  exposure_name <- if (period == "current") {
    "fdi_current"
  } else {
    "fdi_baseline"
  }

  other_groups <- c(
    "Left",
    "Right",
    "Mixed"
  )

  triple_terms <- vapply(
    other_groups,
    function(g) {
      find_exact_interaction_term(
        names(beta),
        c(
          paste0("ideology4", g),
          "muslim",
          exposure_name
        )
      )
    },
    character(1)
  )

  R <- matrix(
    0,
    nrow = length(triple_terms),
    ncol = length(beta),
    dimnames = list(
      paste0(other_groups, "_minus_Center"),
      names(beta)
    )
  )

  for (i in seq_along(triple_terms)) {
    R[i, triple_terms[[i]]] <- 1
  }

  d <- as.numeric(R %*% beta)
  S <- R %*% V %*% t(R)
  S <- (S + t(S)) / 2

  eig <- eigen(S, symmetric = TRUE)
  values <- eig$values
  max_eig <- max(abs(values))
  tol <- if (max_eig == 0) 0 else max_eig * 1e-10
  keep <- values > tol
  effective_rank <- sum(keep)

  ordinary_stat <- NA_real_
  ordinary_p <- NA_real_

  if (effective_rank == 3L) {
    ordinary_stat <-
      as.numeric(t(d) %*% solve(S, d))
    ordinary_p <-
      pchisq(
        ordinary_stat,
        df = 3,
        lower.tail = FALSE
      )
  }

  rank_stat <- NA_real_
  rank_p <- NA_real_

  if (effective_rank > 0L) {
    Q_keep <- eig$vectors[, keep, drop = FALSE]
    projected <- as.numeric(crossprod(Q_keep, d))
    rank_stat <- sum((projected^2) / values[keep])
    rank_p <- pchisq(
      rank_stat,
      df = effective_rank,
      lower.tail = FALSE
    )
  }

  tibble(
    period = period,
    left_minus_center = d[[1]],
    right_minus_center = d[[2]],
    mixed_minus_center = d[[3]],
    ordinary_full_rank = effective_rank == 3L,
    ordinary_wald_chisq = ordinary_stat,
    ordinary_df = if (effective_rank == 3L) 3L else NA_integer_,
    ordinary_p = ordinary_p,
    effective_rank = effective_rank,
    rank_reduced_wald_chisq_diagnostic = rank_stat,
    rank_reduced_df_diagnostic = effective_rank,
    rank_reduced_p_diagnostic = rank_p,
    covariance_min_eigenvalue = min(values),
    covariance_max_eigenvalue = max(values)
  )
}

tidy_fixed_effects <- function(
  fit,
  cell_id,
  sector
) {
  beta <- fixef(fit)
  se <- sqrt(diag(vcov(fit)))
  z <- beta / se

  tibble(
    cell_id = cell_id,
    sector = sector,
    term = names(beta),
    estimate = unname(beta),
    std_error = unname(se),
    statistic = unname(z),
    p_value =
      pchisq(
        z^2,
        df = 1,
        lower.tail = FALSE
      ),
    conf_low =
      unname(beta - 1.96 * se),
    conf_high =
      unname(beta + 1.96 * se)
  )
}

models <- list()
samples <- list()
all_coefficients <- list()
center_reference_coefficients <- list()
group_interactions <- list()
pairwise_wald <- list()
omnibus_wald <- list()
diagnostics <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  dd <- analysis_base |>
    mutate(
      fdi_current =
        .data[[spec$current_col]],
      fdi_baseline =
        .data[[spec$baseline_col]]
    ) |>
    filter(
      is.finite(fdi_current),
      is.finite(fdi_baseline)
    )

  result <- fit_lmer_captured(
    model_formula,
    dd
  )

  fit <- result$fit
  dd_fit <- result$data

  models[[spec$cell_id]] <- fit
  samples[[spec$cell_id]] <- dd_fit

  all_coefficients[[spec$cell_id]] <-
    tidy_fixed_effects(
      fit,
      spec$cell_id,
      spec$sector
    )

  beta_names <- names(fixef(fit))

  for (period in c("current", "baseline")) {
    exposure_name <- if (period == "current") {
      "fdi_current"
    } else {
      "fdi_baseline"
    }

    center_term <- find_exact_interaction_term(
      beta_names,
      c("muslim", exposure_name)
    )

    center_reference_coefficients[[
      paste(
        spec$cell_id,
        period,
        "center",
        sep = "__"
      )
    ]] <- linear_combo_lmer(
      fit,
      setNames(1, center_term)
    ) |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        period = period,
        coefficient_role =
          "Center FDI x Muslim interaction",
        ideology_reference = "Center",
        ideology_comparison = "Center",
        term = center_term,
        .before = 1
      )

    for (g in c("Left", "Right", "Mixed")) {
      triple_term <- find_exact_interaction_term(
        beta_names,
        c(
          paste0("ideology4", g),
          "muslim",
          exposure_name
        )
      )

      center_reference_coefficients[[
        paste(
          spec$cell_id,
          period,
          tolower(g),
          sep = "__"
        )
      ]] <- linear_combo_lmer(
        fit,
        setNames(1, triple_term)
      ) |>
        mutate(
          cell_id = spec$cell_id,
          sector = spec$sector,
          period = period,
          coefficient_role =
            paste0(
              g,
              " minus Center difference in FDI x Muslim interaction"
            ),
          ideology_reference = "Center",
          ideology_comparison = g,
          term = triple_term,
          .before = 1
        )
    }

    group_interactions[[
      paste(spec$cell_id, period, sep = "__")
    ]] <- map_dfr(
      c(
        "Center",
        "Left",
        "Right",
        "Mixed"
      ),
      function(g) {
        linear_combo_lmer(
          fit,
          group_interaction_weights(
            fit,
            g,
            period
          )
        ) |>
          mutate(
            cell_id = spec$cell_id,
            sector = spec$sector,
            period = period,
            ideology = g,
            .before = 1
          )
      }
    )

    pairwise_wald[[
      paste(spec$cell_id, period, sep = "__")
    ]] <- map_dfr(
      seq_len(nrow(pair_registry)),
      function(j) {
        pair <- pair_registry[j, , drop = FALSE]

        linear_combo_lmer(
          fit,
          difference_weights(
            fit,
            pair$ideology_a,
            pair$ideology_b,
            period
          )
        ) |>
          mutate(
            cell_id = spec$cell_id,
            sector = spec$sector,
            period = period,
            contrast_id = pair$contrast_id,
            ideology_a = pair$ideology_a,
            ideology_b = pair$ideology_b,
            interpretation =
              paste0(
                pair$ideology_a,
                " minus ",
                pair$ideology_b,
                " difference in ",
                period,
                " FDI x Muslim interaction"
              ),
            .before = 1
          )
      }
    )

    omnibus_wald[[
      paste(spec$cell_id, period, sep = "__")
    ]] <- omnibus_center_reference(
      fit,
      period
    ) |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        test =
          paste0(
            "Joint equality of ",
            period,
            " FDI x Muslim interaction across Center/Left/Right/Mixed"
          ),
        .before = 1
      )
  }

  conv_msg <- fit@optinfo$conv$lme4$messages
  optimizer_code <- fit@optinfo$conv$opt

  diagnostics[[spec$cell_id]] <- tibble(
    cell_id = spec$cell_id,
    sector = spec$sector,
    n_voters = nrow(dd_fit),
    n_ac = n_distinct(dd_fit$ac_uid),
    n_states = n_distinct(dd_fit$state_no),
    n_center = sum(dd_fit$ideology4 == "Center"),
    n_left = sum(dd_fit$ideology4 == "Left"),
    n_right = sum(dd_fit$ideology4 == "Right"),
    n_mixed = sum(dd_fit$ideology4 == "Mixed"),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code =
      if (
        is.null(optimizer_code) ||
        length(optimizer_code) == 0L
      ) {
        NA_integer_
      } else {
        as.integer(optimizer_code[[1]])
      },
    convergence_messages =
      collapse_messages(conv_msg),
    fit_warnings =
      collapse_messages(result$warnings),
    logLik =
      as.numeric(logLik(fit))
  )
}

all_coefficients <- bind_rows(all_coefficients)
center_reference_coefficients <-
  bind_rows(center_reference_coefficients)
group_interactions <- bind_rows(group_interactions)
pairwise_wald <- bind_rows(pairwise_wald)
omnibus_wald <- bind_rows(omnibus_wald)
diagnostics <- bind_rows(diagnostics)

model_registry <- cell_registry |>
  mutate(
    outcome = "Whether 2014 respondent voted BJP",
    ideology_reference = "Center",
    ideology_levels = "Center; Left; Right; Mixed",
    focal_formula =
      "ideology4 * (muslim * fdi_current + muslim * fdi_baseline)",
    nuisance_controls =
      "ac_pop_100k + sc_share_pp + st_share_pp + religion_x + caste_x + education_x",
    fixed_effects = "state_fe",
    random_effect = "(1 | ac_random)",
    estimator = "lmer mixed linear probability model; ML"
  )

# Optional equivalence check against the already-completed Left-reference
# R38C v1.1 model. Re-referencing a factor should not change n, logLik,
# fitted values, or substantive linear combinations.
prior_models_path <- file.path(
  project_root,
  "outputs",
  "r38c_voter_four_ideology_heterogeneity_v1_1",
  "14_PRIMARY_all_available_four_group_models.rds"
)

equivalence_check <- tibble()

if (file.exists(prior_models_path)) {
  prior_models <- readRDS(prior_models_path)

  prior_keys <- c(
    total_raw =
      "total_raw__focal_hierarchy_interacted",
    manufacturing_raw =
      "manufacturing_raw__focal_hierarchy_interacted"
  )

  equivalence_rows <- list()

  for (cell_id in names(prior_keys)) {
    prior_key <- prior_keys[[cell_id]]

    if (
      prior_key %in% names(prior_models) &&
      cell_id %in% names(models)
    ) {
      old_fit <- prior_models[[prior_key]]
      new_fit <- models[[cell_id]]

      equivalence_rows[[cell_id]] <- tibble(
        cell_id = cell_id,
        prior_model_key = prior_key,
        old_n = nobs(old_fit),
        new_n = nobs(new_fit),
        n_equal = nobs(old_fit) == nobs(new_fit),
        old_logLik = as.numeric(logLik(old_fit)),
        new_logLik = as.numeric(logLik(new_fit)),
        logLik_abs_difference =
          abs(
            as.numeric(logLik(old_fit)) -
              as.numeric(logLik(new_fit))
          ),
        logLik_equal_within_1e_8 =
          abs(
            as.numeric(logLik(old_fit)) -
              as.numeric(logLik(new_fit))
          ) < 1e-8
      )
    }
  }

  equivalence_check <- bind_rows(equivalence_rows)
}

write_csv(
  model_registry,
  file.path(output_dir, "00_model_registry.csv")
)

write_csv(
  center_reference_coefficients,
  file.path(
    output_dir,
    "01_center_reference_direct_coefficients.csv"
  )
)

write_csv(
  group_interactions,
  file.path(
    output_dir,
    "02_implied_group_fdi_muslim_interactions.csv"
  )
)

write_csv(
  pairwise_wald,
  file.path(
    output_dir,
    "03_pairwise_wald_tests.csv"
  )
)

write_csv(
  omnibus_wald,
  file.path(
    output_dir,
    "04_four_group_omnibus_wald_tests.csv"
  )
)

write_csv(
  all_coefficients,
  file.path(
    output_dir,
    "05_all_fixed_effect_coefficients.csv"
  )
)

write_csv(
  diagnostics,
  file.path(
    output_dir,
    "06_model_diagnostics.csv"
  )
)

write_csv(
  equivalence_check,
  file.path(
    output_dir,
    "07_optional_left_reference_equivalence_check.csv"
  )
)

saveRDS(
  models,
  file.path(
    output_dir,
    "08_center_reference_models.rds"
  )
)

saveRDS(
  samples,
  file.path(
    output_dir,
    "09_center_reference_model_samples.rds"
  )
)

notes <- c(
  "R38C4 — CENTER-REFERENCE POOLED FOUR-IDEOLOGY VOTER MODEL",
  "",
  "Purpose:",
  "Re-estimate the completed pooled voter FDI x Muslim x ideology model with Center as the explicit reference category.",
  "",
  "Primary cells:",
  "Total raw FDI and Manufacturing raw FDI.",
  "",
  "Outcome:",
  "Whether a 2014 respondent voted BJP.",
  "",
  "Center-reference parameterization:",
  "muslim:fdi_current is the Center current FDI x Muslim interaction.",
  "ideology4Left:muslim:fdi_current is Left minus Center.",
  "ideology4Right:muslim:fdi_current is Right minus Center.",
  "ideology4Mixed:muslim:fdi_current is Mixed minus Center.",
  "The same structure is estimated for baseline FDI.",
  "",
  "Model structure:",
  "Only the focal Muslim/FDI hierarchy varies by ideology.",
  "AC controls, individual controls, and state fixed-effect coefficients remain common.",
  "One shared AC random intercept is used.",
  "",
  "Wald tests:",
  "All six pairwise ideology differences are saved for current and baseline FDI x Muslim interactions.",
  "The three Center comparisons are direct coefficient tests under this reference coding.",
  "A 3-df omnibus test asks whether Left-Center, Right-Center, and Mixed-Center are jointly zero.",
  "",
  "Optional equivalence audit:",
  "If the prior R38C v1.1 pooled models exist, n and log-likelihood are compared.",
  "A pure change of factor reference category should leave them unchanged."
)

writeLines(
  notes,
  file.path(
    output_dir,
    "10_notes.txt"
  )
)

cat("\n===== R38C4 MODEL REGISTRY =====\n\n")
print(model_registry, n = Inf, width = Inf)

cat("\n===== CURRENT CENTER-REFERENCE DIRECT COEFFICIENTS =====\n\n")
print(
  center_reference_coefficients |>
    filter(period == "current"),
  n = Inf,
  width = Inf
)

cat("\n===== CURRENT IMPLIED GROUP INTERACTIONS =====\n\n")
print(
  group_interactions |>
    filter(period == "current"),
  n = Inf,
  width = Inf
)

cat("\n===== CURRENT CENTER-FOCUSED WALD TESTS =====\n\n")
print(
  pairwise_wald |>
    filter(
      period == "current",
      contrast_id %in%
        c(
          "center_vs_left",
          "center_vs_right",
          "center_vs_mixed"
        )
    ),
  n = Inf,
  width = Inf
)

cat("\n===== CURRENT FOUR-GROUP OMNIBUS WALD =====\n\n")
print(
  omnibus_wald |>
    filter(period == "current"),
  n = Inf,
  width = Inf
)

cat("\n===== MODEL DIAGNOSTICS =====\n\n")
print(diagnostics, n = Inf, width = Inf)

if (nrow(equivalence_check) > 0L) {
  cat("\n===== OPTIONAL LEFT-REFERENCE EQUIVALENCE CHECK =====\n\n")
  print(equivalence_check, n = Inf, width = Inf)
}

cat(
  "\nOUTPUT_DIR=",
  output_dir,
  "\n",
  sep = ""
)

cat("R38C4_COMPLETE\n")

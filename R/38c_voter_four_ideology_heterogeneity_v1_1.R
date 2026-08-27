suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(lme4)
})

required_packages <- c("dplyr", "purrr", "readr", "tibble", "lme4")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

input_dir <- file.path(
  project_root, "data", "derived", "switchers_rewrite", "final"
)
output_dir <- file.path(
  project_root, "outputs", "r38c_voter_four_ideology_heterogeneity_v1_1"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

respondent_path <- file.path(input_dir, "nes_respondent_analysis.rds")
change_path <- file.path(input_dir, "ac_change.rds")
canonical_focal_path <- file.path(
  project_root, "outputs", "voter_canonical_v1_0",
  "04_focal_interaction_coefficients.csv"
)

required_files <- c(
  respondent_path, change_path, canonical_focal_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required input(s): ", paste(missing_files, collapse = ", "))
}

respondents <- readRDS(respondent_path)
ac_change <- readRDS(change_path)
canonical_focal <- read_csv(
  canonical_focal_path,
  show_col_types = FALSE
)

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
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
  "log1p_fdi_total_local_all_pc100k_2009",
  "log1p_fdi_total_local_all_pc100k_2014",
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

ideology_levels <- c("Left", "Center", "Right", "Mixed")

respondents <- respondents |>
  mutate(
    y = as.numeric(voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),

    total_raw_current =
      as.numeric(fdi_total_local_all_pc100k_2014),
    total_raw_baseline =
      as.numeric(fdi_total_local_all_pc100k_2009),
    total_log_current =
      as.numeric(log1p_fdi_total_local_all_pc100k_2014),
    total_log_baseline =
      as.numeric(log1p_fdi_total_local_all_pc100k_2009),

    mfg_raw_current =
      as.numeric(fdi_mfg_local_all_pc100k_2014),
    mfg_raw_baseline =
      as.numeric(fdi_mfg_local_all_pc100k_2009),
    mfg_log_current = log1p(mfg_raw_current),
    mfg_log_baseline = log1p(mfg_raw_baseline),

    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),

    state_fe = factor(state_no),
    ac_random = factor(ac_uid),

    religion_x = relevel_if_present(religion_group, "1: Hindu"),
    caste_x = relevel_if_present(caste_group, "4: Others"),
    education_x = relevel_if_present(
      education_harmonized,
      "Secondary"
    ),

    ideology4 = factor(
      as.character(voter_ideology),
      levels = ideology_levels
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
  ~cell_id, ~sector, ~functional_form, ~current_col, ~baseline_col, ~raw_current_col,
  "total_raw", "Total", "Raw",
  "total_raw_current", "total_raw_baseline", "total_raw_current",
  "total_log1p", "Total", "log1p",
  "total_log_current", "total_log_baseline", "total_raw_current",
  "manufacturing_raw", "Manufacturing", "Raw",
  "mfg_raw_current", "mfg_raw_baseline", "mfg_raw_current",
  "manufacturing_log1p", "Manufacturing", "log1p",
  "mfg_log_current", "mfg_log_baseline", "mfg_raw_current"
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

variant_registry <- tribble(
  ~model_variant, ~fully_interacted_fixed,
  "focal_hierarchy_interacted", FALSE,
  "fully_interacted_fixed_sensitivity", TRUE
)

optimizer_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 300000)
)

collapse_messages <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  paste(unique(as.character(x)), collapse = " | ")
}

complete_model_data <- function(data, variables) {
  keep <- rep(TRUE, nrow(data))
  for (variable in unique(variables)) {
    x <- data[[variable]]
    if (is.numeric(x) || is.integer(x)) {
      keep <- keep & !is.na(x) & is.finite(as.numeric(x))
    } else {
      keep <- keep & !is.na(x)
    }
  }
  data[keep, , drop = FALSE]
}

fit_lmer_captured <- function(formula, data) {
  dd <- complete_model_data(data, all.vars(formula)) |>
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

find_exact_interaction_term <- function(coefficient_names, variables) {
  hits <- coefficient_names[
    vapply(
      strsplit(coefficient_names, ":", fixed = TRUE),
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
      ". Hits: ", paste(hits, collapse = ", ")
    )
  }
  hits[[1]]
}

linear_combo <- function(fit, weights) {
  beta <- fixef(fit)
  V <- as.matrix(vcov(fit))

  unknown <- setdiff(names(weights), names(beta))
  if (length(unknown) > 0L) {
    stop("Unknown coefficient(s): ", paste(unknown, collapse = ", "))
  }

  L <- setNames(rep(0, length(beta)), names(beta))
  L[names(weights)] <- as.numeric(weights)

  estimate <- sum(L * beta)
  variance <- as.numeric(t(L) %*% V %*% L)
  variance <- max(variance, 0)
  se <- sqrt(variance)
  z <- estimate / se

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

safe_omnibus_from_restrictions <- function(
  fit,
  R,
  labels,
  rcond_threshold = 1e-12,
  eigen_relative_tolerance = 1e-10
) {
  beta <- fixef(fit)
  V <- as.matrix(vcov(fit))

  d <- as.numeric(R %*% beta)
  S <- R %*% V %*% t(R)
  S <- (S + t(S)) / 2

  eig <- eigen(S, symmetric = TRUE)
  values <- eig$values
  q <- nrow(R)
  max_eig <- max(abs(values))
  tol <- if (max_eig == 0) 0 else max_eig * eigen_relative_tolerance
  keep <- values > tol
  effective_rank <- sum(keep)
  rc <- suppressWarnings(tryCatch(rcond(S), error = function(e) 0))

  full_rank_stable <-
    effective_rank == q &&
    is.finite(rc) &&
    rc >= rcond_threshold

  ordinary_stat <- NA_real_
  ordinary_p <- NA_real_

  if (full_rank_stable) {
    ordinary_stat <- as.numeric(t(d) %*% solve(S, d))
    ordinary_p <- pchisq(
      ordinary_stat,
      df = q,
      lower.tail = FALSE
    )
  }

  rank_stat <- NA_real_
  rank_p <- NA_real_
  null_component_norm <- NA_real_

  if (effective_rank > 0L) {
    Q_keep <- eig$vectors[, keep, drop = FALSE]
    projected <- as.numeric(crossprod(Q_keep, d))
    rank_stat <- sum((projected^2) / values[keep])
    rank_p <- pchisq(
      rank_stat,
      df = effective_rank,
      lower.tail = FALSE
    )

    if (effective_rank < q) {
      Q_null <- eig$vectors[, !keep, drop = FALSE]
      null_component_norm <- sqrt(
        sum(as.numeric(crossprod(Q_null, d))^2)
      )
    } else {
      null_component_norm <- 0
    }
  }

  result <- tibble(
    omnibus_full_rank_stable = full_rank_stable,
    ordinary_wald_chisq = ordinary_stat,
    ordinary_df = if (full_rank_stable) q else NA_integer_,
    ordinary_p = ordinary_p,
    effective_rank = effective_rank,
    rank_reduced_wald_chisq_diagnostic = rank_stat,
    rank_reduced_df_diagnostic = effective_rank,
    rank_reduced_p_diagnostic = rank_p,
    null_space_component_norm = null_component_norm,
    restriction_covariance_rcond = rc,
    restriction_covariance_min_eigenvalue = min(values),
    restriction_covariance_max_eigenvalue = max(values),
    restriction_covariance_condition_number =
      ifelse(
        min(abs(values)) > 0,
        max(abs(values)) / min(abs(values)),
        Inf
      ),
    omnibus_status = if (full_rank_stable) {
      "ordinary full-rank Wald estimable"
    } else {
      "ordinary omnibus numerically singular/ill-conditioned; rank-reduced result is diagnostic only"
    }
  )

  for (k in seq_along(labels)) {
    result[[labels[[k]]]] <- d[[k]]
  }

  result
}

native_formula <- as.formula(
  paste0(
    "y ~ ",
    "muslim * fdi_current + ",
    "muslim * fdi_baseline + ",
    "ac_pop_100k + sc_share_pp + st_share_pp + ",
    "religion_x + caste_x + education_x + state_fe + ",
    "(1 | ac_random)"
  )
)

native_models <- list()
native_samples <- list()
native_results <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  for (g in ideology_levels) {
    dd <- analysis_base |>
      filter(as.character(ideology4) == g) |>
      mutate(
        fdi_current = .data[[spec$current_col]],
        fdi_baseline = .data[[spec$baseline_col]],
        fdi_current_raw = .data[[spec$raw_current_col]]
      ) |>
      filter(
        is.finite(fdi_current),
        is.finite(fdi_baseline),
        is.finite(fdi_current_raw)
      )

    result <- fit_lmer_captured(native_formula, dd)
    key <- paste(spec$cell_id, tolower(g), sep = "__")

    native_models[[key]] <- result$fit
    native_samples[[key]] <- result$data

    focal_term <- find_exact_interaction_term(
      names(fixef(result$fit)),
      c("muslim", "fdi_current")
    )
    focal <- linear_combo(
      result$fit,
      setNames(1, focal_term)
    )

    conv_msg <- result$fit@optinfo$conv$lme4$messages
    optimizer_code <- result$fit@optinfo$conv$opt
    unique_ac <- result$data |>
      distinct(ac_uid, .keep_all = TRUE)

    native_results[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      ideology = g,
      n_voters = nrow(result$data),
      n_ac = n_distinct(result$data$ac_uid),
      n_states = n_distinct(result$data$state_no),
      n_positive_current_fdi_ac =
        sum(unique_ac$fdi_current_raw > 0),
      share_zero_current_fdi_ac =
        mean(unique_ac$fdi_current_raw == 0),
      estimate = focal$estimate,
      std_error = focal$std_error,
      conf_low = focal$conf_low,
      conf_high = focal$conf_high,
      p_value = focal$p_value,
      singular = isSingular(result$fit, tol = 1e-4),
      optimizer_code = if (
        is.null(optimizer_code) ||
          length(optimizer_code) == 0L
      ) NA_integer_ else as.integer(optimizer_code[[1]]),
      convergence_messages = collapse_messages(conv_msg),
      fit_warnings = collapse_messages(result$warnings)
    )
  }
}

native_results <- bind_rows(native_results)

canonical_map <- tribble(
  ~cell_id, ~ideology, ~canonical_model_id,
  "total_raw", "Center", "V01",
  "total_raw", "Left", "V08",
  "total_raw", "Right", "V09",
  "total_log1p", "Center", "V03",
  "manufacturing_raw", "Center", "V05"
)

canonical_check <- canonical_map |>
  left_join(
    native_results |>
      select(
        cell_id,
        ideology,
        audit_estimate = estimate,
        audit_se = std_error,
        audit_n = n_voters
      ),
    by = c("cell_id", "ideology"),
    relationship = "one-to-one"
  ) |>
  left_join(
    canonical_focal |>
      select(
        model_id,
        canonical_estimate = estimate,
        canonical_se = std_error
      ),
    by = c("canonical_model_id" = "model_id"),
    relationship = "one-to-one"
  ) |>
  mutate(
    estimate_abs_diff = abs(audit_estimate - canonical_estimate),
    se_abs_diff = abs(audit_se - canonical_se),
    reproduces_canonical =
      estimate_abs_diff < 1e-6 &
      se_abs_diff < 1e-6
  )

if (any(!canonical_check$reproduces_canonical)) {
  print(canonical_check, n = Inf, width = Inf)
  stop("R38C v1.1 failed to reproduce at least one canonical voter result.")
}

build_pair_formula <- function(
  factor_name = "ideology_pair",
  fully_interacted_fixed
) {
  if (!fully_interacted_fixed) {
    as.formula(
      paste0(
        "y ~ ", factor_name, " * (",
        "muslim * fdi_current + ",
        "muslim * fdi_baseline",
        ") + ",
        "ac_pop_100k + sc_share_pp + st_share_pp + ",
        "religion_x + caste_x + education_x + state_fe + ",
        "(1 | ac_random)"
      )
    )
  } else {
    as.formula(
      paste0(
        "y ~ ", factor_name, " * (",
        "muslim * fdi_current + ",
        "muslim * fdi_baseline + ",
        "ac_pop_100k + sc_share_pp + st_share_pp + ",
        "religion_x + caste_x + education_x + state_fe",
        ") + ",
        "(1 | ac_random)"
      )
    )
  }
}

fit_pair <- function(
  data_a,
  data_b,
  ideology_a,
  ideology_b,
  fully_interacted_fixed,
  sample_mode = c("all_available", "common_ac")
) {
  sample_mode <- match.arg(sample_mode)

  ac_a <- unique(as.character(data_a$ac_uid))
  ac_b <- unique(as.character(data_b$ac_uid))
  overlap <- sort(unique(intersect(ac_a, ac_b)))
  union_ids <- sort(unique(union(ac_a, ac_b)))

  if (sample_mode == "common_ac" && length(overlap) < 2L) {
    stop("Fewer than two common ACs for ", ideology_a, " vs ", ideology_b)
  }

  if (sample_mode == "common_ac") {
    data_a <- data_a |>
      filter(as.character(ac_uid) %in% overlap)
    data_b <- data_b |>
      filter(as.character(ac_uid) %in% overlap)
  }

  stacked <- bind_rows(data_a, data_b) |>
    mutate(
      ideology_pair = factor(
        as.character(ideology4),
        levels = c(ideology_a, ideology_b)
      ),
      ac_random = factor(ac_uid)
    ) |>
    droplevels()

  formula <- build_pair_formula(
    "ideology_pair",
    fully_interacted_fixed
  )

  result <- fit_lmer_captured(formula, stacked)

  list(
    fit = result$fit,
    data = result$data,
    warnings = result$warnings,
    messages = result$messages,
    overlap_ids = overlap,
    union_ids = union_ids
  )
}

extract_pair_effects <- function(
  fit,
  ideology_a,
  ideology_b
) {
  beta_names <- names(fixef(fit))

  base_term <- find_exact_interaction_term(
    beta_names,
    c("muslim", "fdi_current")
  )
  diff_term <- find_exact_interaction_term(
    beta_names,
    c(
      paste0("ideology_pair", ideology_b),
      "muslim",
      "fdi_current"
    )
  )

  effect_a <- linear_combo(
    fit,
    setNames(1, base_term)
  )
  effect_b <- linear_combo(
    fit,
    setNames(
      c(1, 1),
      c(base_term, diff_term)
    )
  )
  a_minus_b <- linear_combo(
    fit,
    setNames(-1, diff_term)
  )

  list(
    effect_a = effect_a,
    effect_b = effect_b,
    difference = a_minus_b
  )
}

primary_pairwise <- list()
secondary_pairwise <- list()
primary_pair_models <- list()
secondary_pair_models <- list()
pair_support <- list()
common_membership <- list()
native_vs_joint <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  for (j in seq_len(nrow(pair_registry))) {
    pair <- pair_registry[j, , drop = FALSE]

    key_a <- paste(spec$cell_id, tolower(pair$ideology_a), sep = "__")
    key_b <- paste(spec$cell_id, tolower(pair$ideology_b), sep = "__")

    data_a <- native_samples[[key_a]]
    data_b <- native_samples[[key_b]]

    native_a <- native_results |>
      filter(
        cell_id == spec$cell_id,
        ideology == pair$ideology_a
      )
    native_b <- native_results |>
      filter(
        cell_id == spec$cell_id,
        ideology == pair$ideology_b
      )

    for (v in seq_len(nrow(variant_registry))) {
      variant <- variant_registry[v, , drop = FALSE]

      primary <- fit_pair(
        data_a,
        data_b,
        pair$ideology_a,
        pair$ideology_b,
        variant$fully_interacted_fixed,
        sample_mode = "all_available"
      )
      secondary <- fit_pair(
        data_a,
        data_b,
        pair$ideology_a,
        pair$ideology_b,
        variant$fully_interacted_fixed,
        sample_mode = "common_ac"
      )

      primary_effects <- extract_pair_effects(
        primary$fit,
        pair$ideology_a,
        pair$ideology_b
      )
      secondary_effects <- extract_pair_effects(
        secondary$fit,
        pair$ideology_a,
        pair$ideology_b
      )

      key <- paste(
        spec$cell_id,
        pair$contrast_id,
        variant$model_variant,
        sep = "__"
      )

      primary_pair_models[[key]] <- primary$fit
      secondary_pair_models[[key]] <- secondary$fit

      primary_pairwise[[key]] <- tibble(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        model_variant = variant$model_variant,
        contrast_id = pair$contrast_id,
        ideology_a = pair$ideology_a,
        ideology_b = pair$ideology_b,

        native_n_voters_a = native_a$n_voters,
        native_n_voters_b = native_b$n_voters,
        native_n_ac_a = native_a$n_ac,
        native_n_ac_b = native_b$n_ac,

        n_union_ac = length(primary$union_ids),
        n_overlap_ac = length(primary$overlap_ids),
        n_stacked_voters = nrow(primary$data),

        joint_interaction_a = primary_effects$effect_a$estimate,
        joint_interaction_b = primary_effects$effect_b$estimate,
        difference_a_minus_b =
          primary_effects$difference$estimate,
        difference_se =
          primary_effects$difference$std_error,
        conf_low =
          primary_effects$difference$conf_low,
        conf_high =
          primary_effects$difference$conf_high,
        wald_chisq_1df =
          primary_effects$difference$wald_chisq_1df,
        wald_p =
          primary_effects$difference$p_value,

        singular =
          isSingular(primary$fit, tol = 1e-4),
        convergence_messages =
          collapse_messages(
            primary$fit@optinfo$conv$lme4$messages
          ),
        fit_warnings =
          collapse_messages(primary$warnings)
      )

      secondary_pairwise[[key]] <- tibble(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        model_variant = variant$model_variant,
        contrast_id = pair$contrast_id,
        ideology_a = pair$ideology_a,
        ideology_b = pair$ideology_b,

        n_common_ac = length(secondary$overlap_ids),
        n_voters_a =
          sum(as.character(secondary$data$ideology_pair) == pair$ideology_a),
        n_voters_b =
          sum(as.character(secondary$data$ideology_pair) == pair$ideology_b),

        joint_interaction_a =
          secondary_effects$effect_a$estimate,
        joint_interaction_b =
          secondary_effects$effect_b$estimate,
        difference_a_minus_b =
          secondary_effects$difference$estimate,
        difference_se =
          secondary_effects$difference$std_error,
        conf_low =
          secondary_effects$difference$conf_low,
        conf_high =
          secondary_effects$difference$conf_high,
        wald_chisq_1df =
          secondary_effects$difference$wald_chisq_1df,
        wald_p =
          secondary_effects$difference$p_value,

        singular =
          isSingular(secondary$fit, tol = 1e-4),
        convergence_messages =
          collapse_messages(
            secondary$fit@optinfo$conv$lme4$messages
          ),
        fit_warnings =
          collapse_messages(secondary$warnings)
      )

      native_vs_joint[[key]] <- bind_rows(
        tibble(
          cell_id = spec$cell_id,
          sector = spec$sector,
          functional_form = spec$functional_form,
          model_variant = variant$model_variant,
          contrast_id = pair$contrast_id,
          ideology = pair$ideology_a,
          native_separate_estimate = native_a$estimate,
          all_available_joint_estimate =
            primary_effects$effect_a$estimate
        ),
        tibble(
          cell_id = spec$cell_id,
          sector = spec$sector,
          functional_form = spec$functional_form,
          model_variant = variant$model_variant,
          contrast_id = pair$contrast_id,
          ideology = pair$ideology_b,
          native_separate_estimate = native_b$estimate,
          all_available_joint_estimate =
            primary_effects$effect_b$estimate
        )
      ) |>
        mutate(
          joint_minus_native =
            all_available_joint_estimate -
            native_separate_estimate
        )

      common_membership[[key]] <- tibble(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        model_variant = variant$model_variant,
        contrast_id = pair$contrast_id,
        ac_uid = secondary$overlap_ids
      )
    }

    union_support <- bind_rows(data_a, data_b) |>
      distinct(ac_uid, .keep_all = TRUE)
    overlap_ids <- intersect(
      as.character(data_a$ac_uid),
      as.character(data_b$ac_uid)
    )
    overlap_support <- union_support |>
      filter(as.character(ac_uid) %in% overlap_ids)

    pair_support[[paste(spec$cell_id, pair$contrast_id, sep = "__")]] <-
      tibble(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        contrast_id = pair$contrast_id,
        ideology_a = pair$ideology_a,
        ideology_b = pair$ideology_b,
        n_voters_a = nrow(data_a),
        n_voters_b = nrow(data_b),
        n_ac_a = n_distinct(data_a$ac_uid),
        n_ac_b = n_distinct(data_b$ac_uid),
        n_union_ac = nrow(union_support),
        n_overlap_ac = nrow(overlap_support),
        overlap_share_of_smaller_ac_sample =
          nrow(overlap_support) /
          min(
            n_distinct(data_a$ac_uid),
            n_distinct(data_b$ac_uid)
          ),
        n_positive_fdi_union_ac =
          sum(union_support$fdi_current_raw > 0),
        n_positive_fdi_overlap_ac =
          sum(overlap_support$fdi_current_raw > 0),
        zero_share_union =
          mean(union_support$fdi_current_raw == 0),
        zero_share_overlap =
          mean(overlap_support$fdi_current_raw == 0)
      )
  }
}

primary_pairwise <- bind_rows(primary_pairwise)
secondary_pairwise <- bind_rows(secondary_pairwise)
pair_support <- bind_rows(pair_support)
common_membership <- bind_rows(common_membership)
native_vs_joint <- bind_rows(native_vs_joint)

build_four_group_formula <- function(fully_interacted_fixed) {
  if (!fully_interacted_fixed) {
    as.formula(
      paste0(
        "y ~ ideology4 * (",
        "muslim * fdi_current + ",
        "muslim * fdi_baseline",
        ") + ",
        "ac_pop_100k + sc_share_pp + st_share_pp + ",
        "religion_x + caste_x + education_x + state_fe + ",
        "(1 | ac_random)"
      )
    )
  } else {
    as.formula(
      paste0(
        "y ~ ideology4 * (",
        "muslim * fdi_current + ",
        "muslim * fdi_baseline + ",
        "ac_pop_100k + sc_share_pp + st_share_pp + ",
        "religion_x + caste_x + education_x + state_fe",
        ") + ",
        "(1 | ac_random)"
      )
    )
  }
}

four_group_primary <- list()
four_group_secondary <- list()
four_group_primary_models <- list()
four_group_secondary_models <- list()
four_group_common_membership <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  pooled <- analysis_base |>
    mutate(
      fdi_current = .data[[spec$current_col]],
      fdi_baseline = .data[[spec$baseline_col]],
      fdi_current_raw = .data[[spec$raw_current_col]]
    ) |>
    filter(
      is.finite(fdi_current),
      is.finite(fdi_baseline),
      is.finite(fdi_current_raw)
    ) |>
    droplevels()

  ac_sets <- lapply(
    ideology_levels,
    function(g) {
      unique(
        as.character(
          pooled$ac_uid[
            as.character(pooled$ideology4) == g
          ]
        )
      )
    }
  )
  names(ac_sets) <- ideology_levels
  all_four_common <- sort(unique(Reduce(intersect, ac_sets)))

  for (v in seq_len(nrow(variant_registry))) {
    variant <- variant_registry[v, , drop = FALSE]
    fml <- build_four_group_formula(
      variant$fully_interacted_fixed
    )

    primary <- fit_lmer_captured(fml, pooled)
    common_data <- pooled |>
      filter(as.character(ac_uid) %in% all_four_common)
    secondary <- fit_lmer_captured(fml, common_data)

    make_R <- function(fit) {
      beta_names <- names(fixef(fit))

      center_diff <- find_exact_interaction_term(
        beta_names,
        c("ideology4Center", "muslim", "fdi_current")
      )
      right_diff <- find_exact_interaction_term(
        beta_names,
        c("ideology4Right", "muslim", "fdi_current")
      )
      mixed_diff <- find_exact_interaction_term(
        beta_names,
        c("ideology4Mixed", "muslim", "fdi_current")
      )

      R <- matrix(
        0,
        nrow = 3,
        ncol = length(beta_names),
        dimnames = list(
          c(
            "center_minus_left",
            "right_minus_left",
            "mixed_minus_left"
          ),
          beta_names
        )
      )

      R[1, center_diff] <- 1
      R[2, right_diff] <- 1
      R[3, mixed_diff] <- 1
      R
    }

    key <- paste(
      spec$cell_id,
      variant$model_variant,
      sep = "__"
    )

    four_group_primary_models[[key]] <- primary$fit
    four_group_secondary_models[[key]] <- secondary$fit

    four_group_primary[[key]] <- safe_omnibus_from_restrictions(
      primary$fit,
      make_R(primary$fit),
      c(
        "center_minus_left",
        "right_minus_left",
        "mixed_minus_left"
      )
    ) |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        model_variant = variant$model_variant,
        n_voters = nrow(primary$data),
        n_ac = n_distinct(primary$data$ac_uid),
        singular = isSingular(primary$fit, tol = 1e-4),
        convergence_messages =
          collapse_messages(
            primary$fit@optinfo$conv$lme4$messages
          ),
        .before = 1
      )

    four_group_secondary[[key]] <- safe_omnibus_from_restrictions(
      secondary$fit,
      make_R(secondary$fit),
      c(
        "center_minus_left",
        "right_minus_left",
        "mixed_minus_left"
      )
    ) |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        model_variant = variant$model_variant,
        n_voters = nrow(secondary$data),
        n_common_ac = length(all_four_common),
        singular = isSingular(secondary$fit, tol = 1e-4),
        convergence_messages =
          collapse_messages(
            secondary$fit@optinfo$conv$lme4$messages
          ),
        .before = 1
      )

    four_group_common_membership[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      model_variant = variant$model_variant,
      ac_uid = all_four_common
    )
  }
}

four_group_primary <- bind_rows(four_group_primary)
four_group_secondary <- bind_rows(four_group_secondary)
four_group_common_membership <- bind_rows(four_group_common_membership)

write_csv(
  cell_registry,
  file.path(output_dir, "00_cell_registry.csv")
)
write_csv(
  canonical_check,
  file.path(output_dir, "01_canonical_reproduction_checks.csv")
)
write_csv(
  native_results,
  file.path(output_dir, "02_native_four_ideology_coefficients.csv")
)
write_csv(
  primary_pairwise,
  file.path(output_dir, "03_PRIMARY_all_available_pairwise_wald_tests.csv")
)
write_csv(
  secondary_pairwise,
  file.path(output_dir, "04_SECONDARY_common_ac_pairwise_wald_tests.csv")
)
write_csv(
  pair_support,
  file.path(output_dir, "05_pairwise_sample_support_diagnostics.csv")
)
write_csv(
  native_vs_joint,
  file.path(output_dir, "06_native_vs_all_available_joint_interactions.csv")
)
write_csv(
  four_group_primary,
  file.path(output_dir, "07_PRIMARY_all_available_four_group_omnibus.csv")
)
write_csv(
  four_group_secondary,
  file.path(output_dir, "08_SECONDARY_all_four_common_ac_omnibus.csv")
)
write_csv(
  common_membership,
  file.path(output_dir, "09_common_support_pairwise_ac_membership.csv")
)
write_csv(
  four_group_common_membership,
  file.path(output_dir, "10_all_four_common_ac_membership.csv")
)

saveRDS(
  native_models,
  file.path(output_dir, "11_native_models.rds")
)
saveRDS(
  primary_pair_models,
  file.path(output_dir, "12_PRIMARY_all_available_pairwise_models.rds")
)
saveRDS(
  secondary_pair_models,
  file.path(output_dir, "13_SECONDARY_common_support_pairwise_models.rds")
)
saveRDS(
  four_group_primary_models,
  file.path(output_dir, "14_PRIMARY_all_available_four_group_models.rds")
)
saveRDS(
  four_group_secondary_models,
  file.path(output_dir, "15_SECONDARY_common_four_group_models.rds")
)

notes <- c(
  "R38C v1.1 — VOTER FOUR-IDEOLOGY HETEROGENEITY",
  "",
  "PRIMARY pairwise test:",
  "For each ideology pair, use every eligible voter from both native ideology samples; there is NO common-AC restriction.",
  "The two-group mixed model includes one shared AC random intercept.",
  "",
  "Important distinction from the AC-level union stack:",
  "A joint mixed model estimates shared variance components, so its group-specific fixed-effect coefficients need not reproduce coefficients from separately fitted native mixed models exactly.",
  "The script therefore saves native and all-available joint estimates side by side rather than falsely requiring numerical identity.",
  "",
  "SECONDARY pairwise test:",
  "Restrict each pair to its maximal pair-specific common AC set and re-estimate the same joint model.",
  "",
  "Two fixed-effect variants:",
  "1. focal_hierarchy_interacted: ideology interacts with the Muslim/current-FDI/baseline-FDI hierarchy; nuisance slopes remain common.",
  "2. fully_interacted_fixed_sensitivity: ideology also interacts with individual controls, AC controls, and state fixed effects.",
  "",
  "Four-group omnibus tests are optional diagnostics. Numerically singular omnibus restriction covariance does not abort the pairwise analysis.",
  "Rank-reduced omnibus results, when needed, are explicitly labeled diagnostic only.",
  "",
  "Groups: Left, Center, Right, Mixed.",
  "Primary scales: Total raw and Manufacturing raw. log1p cells are robustness checks."
)
writeLines(notes, file.path(output_dir, "16_notes.txt"))

cat("===== CANONICAL REPRODUCTION CHECKS =====\n\n")
print(canonical_check, n = Inf, width = Inf)

cat("\n===== NATIVE FOUR-IDEOLOGY COEFFICIENTS =====\n\n")
print(native_results, n = Inf, width = Inf)

cat("\n===== PRIMARY CENTER WALD TESTS: ALL AVAILABLE PAIR-SPECIFIC VOTERS =====\n\n")
print(
  primary_pairwise |>
    filter(
      model_variant == "focal_hierarchy_interacted",
      contrast_id %in%
        c("center_vs_left", "center_vs_right", "center_vs_mixed")
    ),
  n = Inf,
  width = Inf
)

cat("\n===== SECONDARY CENTER WALD TESTS: COMMON AC SUPPORT =====\n\n")
print(
  secondary_pairwise |>
    filter(
      model_variant == "focal_hierarchy_interacted",
      contrast_id %in%
        c("center_vs_left", "center_vs_right", "center_vs_mixed")
    ),
  n = Inf,
  width = Inf
)

cat("\n===== NATIVE VS PRIMARY JOINT INTERACTIONS =====\n\n")
print(
  native_vs_joint |>
    filter(
      model_variant == "focal_hierarchy_interacted",
      contrast_id %in%
        c("center_vs_left", "center_vs_right", "center_vs_mixed")
    ),
  n = Inf,
  width = Inf
)

cat("\n===== CENTER PAIR SUPPORT =====\n\n")
print(
  pair_support |>
    filter(
      contrast_id %in%
        c("center_vs_left", "center_vs_right", "center_vs_mixed")
    ),
  n = Inf,
  width = Inf
)

cat("\n===== PRIMARY FOUR-GROUP OMNIBUS =====\n\n")
print(four_group_primary, n = Inf, width = Inf)

cat("\n===== SECONDARY FOUR-GROUP COMMON-AC OMNIBUS =====\n\n")
print(four_group_secondary, n = Inf, width = Inf)

cat("\nOUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38C_V1_1_COMPLETE\n")

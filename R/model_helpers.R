# Small model helpers for interactive exploration and selected exports.

collapse_rhs <- function(terms) {
  terms <- unique(stats::na.omit(terms))
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) "1" else paste(terms, collapse = " + ")
}

interaction_formula <- function(outcome, exposure, moderator, controls = NULL) {
  stats::as.formula(
    paste(outcome, "~", collapse_rhs(c(sprintf("%s * %s", exposure, moderator), controls)))
  )
}

triple_interaction_formula <- function(
  outcome,
  exposure,
  moderator,
  group,
  controls = NULL
) {
  stats::as.formula(
    paste(
      outcome,
      "~",
      collapse_rhs(c(sprintf("%s * %s * %s", exposure, moderator, group), controls))
    )
  )
}

add_fixed_effects <- function(formula, fixed_effects = NULL) {
  if (is.null(fixed_effects) || !nzchar(fixed_effects)) return(formula)
  stats::as.formula(paste(paste(deparse(formula), collapse = ""), "|", fixed_effects))
}

assert_model_variables <- function(data, variables, model_label = "model") {
  missing <- setdiff(stats::na.omit(variables), names(data))
  if (length(missing) > 0) {
    stop(model_label, " is missing variables: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

fit_ac_interaction <- function(
  data,
  outcome,
  exposure,
  moderator,
  controls = NULL,
  fixed_effects = "state_no + year",
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  assert_model_variables(data, c(outcome, exposure, moderator, controls), "AC interaction model")
  formula <- interaction_formula(outcome, exposure, moderator, controls) |>
    add_fixed_effects(fixed_effects)

  fixest::feols(
    formula,
    data = data,
    subset = subset,
    cluster = cluster,
    notes = FALSE
  )
}

fit_ac_candidate_entry <- function(
  data,
  exposure,
  moderator,
  controls = NULL,
  fixed_effects = "state_no + year",
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  assert_model_variables(
    data,
    c("fr_candidate_present", exposure, moderator, controls),
    "Candidate-entry model"
  )
  formula <- interaction_formula(
    "fr_candidate_present",
    exposure,
    moderator,
    controls
  ) |>
    add_fixed_effects(fixed_effects)

  fixest::feglm(
    formula,
    data = data,
    family = binomial(),
    subset = subset,
    cluster = cluster,
    notes = FALSE
  )
}

fit_change_interaction <- function(
  data,
  outcome = "d_fr_party_vote_share_2009_2014_pp",
  exposure_change,
  moderator,
  controls = NULL,
  fixed_effects = "state_no",
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  assert_model_variables(
    data,
    c(outcome, exposure_change, moderator, controls),
    "First-difference model"
  )
  formula <- interaction_formula(outcome, exposure_change, moderator, controls) |>
    add_fixed_effects(fixed_effects)

  fixest::feols(
    formula,
    data = data,
    subset = subset,
    cluster = cluster,
    notes = FALSE
  )
}

fit_voter_interaction <- function(
  data,
  outcome = "voted_fr",
  exposure,
  moderator,
  ideology = "voter_ideology",
  controls = NULL,
  fixed_effects = "state_no + year",
  weights = ~survey_weight_norm_year,
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  assert_model_variables(
    data,
    c(outcome, exposure, moderator, ideology, controls),
    "Voter-level model"
  )
  formula <- triple_interaction_formula(
    outcome,
    exposure,
    moderator,
    ideology,
    controls
  ) |>
    add_fixed_effects(fixed_effects)

  fixest::feglm(
    formula,
    data = data,
    family = binomial(),
    weights = weights,
    subset = subset,
    cluster = cluster,
    notes = FALSE
  )
}

fit_voter_binary_outcome <- function(
  data,
  outcome,
  exposure,
  moderator,
  ideology = "voter_ideology",
  controls = NULL,
  fixed_effects = "state_no + year",
  weights = ~survey_weight_norm_year,
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  fit_voter_interaction(
    data = data,
    outcome = outcome,
    exposure = exposure,
    moderator = moderator,
    ideology = ideology,
    controls = controls,
    fixed_effects = fixed_effects,
    weights = weights,
    cluster = cluster,
    subset = subset
  )
}

fit_ideology_cell_binomial <- function(
  data,
  exposure,
  moderator,
  controls = NULL,
  fixed_effects = "state_no + year",
  cluster = ~pc_cluster_id,
  subset = NULL
) {
  assert_model_variables(
    data,
    c("n_voted_fr", "n_vote_valid", "ideology", exposure, moderator, controls),
    "Ideology-cell model"
  )
  formula <- triple_interaction_formula(
    "cbind(n_voted_fr, n_vote_valid - n_voted_fr)",
    exposure,
    moderator,
    "ideology",
    controls
  ) |>
    add_fixed_effects(fixed_effects)

  fixest::feglm(
    formula,
    data = data,
    family = binomial(),
    subset = subset,
    cluster = cluster,
    notes = FALSE
  )
}

fit_voter_multilevel <- function(
  data,
  outcome = "voted_fr",
  exposure,
  moderator,
  ideology = "voter_ideology",
  controls = NULL,
  random_effects = c("(1 | pc_cluster_id)", "(1 | ac_uid)", "(1 | psu_uid)"),
  subset = NULL
) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Install the lme4 package before running multilevel models.")
  }
  assert_model_variables(
    data,
    c(outcome, exposure, moderator, ideology, controls),
    "Multilevel voter model"
  )

  rhs <- collapse_rhs(c(
    sprintf("%s * %s * %s", exposure, moderator, ideology),
    controls,
    random_effects
  ))
  formula <- stats::as.formula(paste(outcome, "~", rhs))
  model_data <- if (is.null(subset)) data else data[eval(substitute(subset), data, parent.frame()), ]

  lme4::glmer(
    formula,
    data = model_data,
    family = binomial(),
    weights = survey_weight_norm_year,
    control = lme4::glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )
}

model_variable_families <- function(data = NULL) {
  families <- list(
    fdi = c(
      total = "log1p_fdi_total_local_all_pc100k",
      manufacturing = "log1p_fdi_mfg_local_all_pc100k",
      services = "log1p_fdi_services_local_all_pc100k",
      announced_total = "log1p_fdi_total_local_announced_pc100k",
      opened_total = "log1p_fdi_total_local_opened_pc100k",
      announced_mfg = "log1p_fdi_mfg_local_announced_pc100k",
      opened_mfg = "log1p_fdi_mfg_local_opened_pc100k"
    ),
    fdi_change = c(
      total = "d_log1p_fdi_total_local_all_pc100k_2009_2014",
      manufacturing = "d_log1p_fdi_mfg_local_all_pc100k_2009_2014",
      services = "d_log1p_fdi_services_local_all_pc100k_2009_2014",
      announced_mfg = "d_log1p_fdi_mfg_local_announced_pc100k_2009_2014",
      opened_mfg = "d_log1p_fdi_mfg_local_opened_pc100k_2009_2014"
    ),
    migration = c(
      recent_total = "log1p_mig_recent_5yr_total",
      prior_total = "log1p_mig_prior_5yr_total",
      prior_10yr_total = "log1p_mig_prior_5_15yr_total",
      baseline_stock = "log1p_mig_total_upto_2001",
      recent_share = "mig_recent_5yr_share_ac_pop",
      prior_share = "mig_prior_5yr_share_ac_pop",
      male_prior = "log1p_male_mig_prior_5yr_total",
      acceleration_ratio = "mig_accel_recent_vs_prior5_ratio",
      acceleration_pct = "mig_accel_recent_vs_prior5_pct_change",
      acceleration_log1p = "mig_accel_recent_vs_prior5_log1p",
      work_share_2011 = "interstate_work_migrant_share_0_9_2011_dist_proxy"
    ),
    demographics = c(
      migration = "log1p_mig_prior_5_15yr_total",
      muslim_2011 = "muslim_share_2011_dist_proxy",
      muslim_change = "d_muslim_share_2001_2011_pp",
      nonlocal_language_2011 = "nonlocal_language_share_common_2011_dist_proxy",
      nonlocal_language_change = "d_nonlocal_language_share_common_2001_2011_pp"
    )
  )

  if (is.null(data)) return(families)
  purrr::map(families, ~ .x[.x %in% names(data)])
}

run_ac_interaction_grid <- function(
  data,
  outcome,
  exposure_variables,
  moderator_variables,
  controls = NULL,
  fixed_effects = "state_no + year",
  cluster = ~pc_cluster_id
) {
  tidyr::crossing(
    exposure_name = names(exposure_variables),
    moderator_name = names(moderator_variables)
  ) |>
    dplyr::mutate(
      exposure = unname(exposure_variables[exposure_name]),
      moderator = unname(moderator_variables[moderator_name]),
      model_id = paste(exposure_name, moderator_name, sep = "__"),
      model = purrr::map2(
        exposure,
        moderator,
        ~ tryCatch(
          fit_ac_interaction(
            data = data,
            outcome = outcome,
            exposure = .x,
            moderator = .y,
            controls = controls,
            fixed_effects = fixed_effects,
            cluster = cluster
          ),
          error = identity
        )
      ),
      error = purrr::map_chr(model, ~ if (inherits(.x, "error")) conditionMessage(.x) else NA_character_)
    )
}

extract_model_terms <- function(model, model_id = NA_character_) {
  if (inherits(model, "error") || is.null(model)) {
    return(tibble::tibble(model_id = model_id, error = if (inherits(model, "error")) conditionMessage(model) else "model is NULL"))
  }
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::mutate(model_id = model_id, .before = 1)
}

model_sample_flow <- function(
  data,
  model_id,
  outcome,
  exposure,
  moderator,
  controls = NULL,
  other_required = NULL
) {
  assert_model_variables(
    data,
    c(outcome, exposure, moderator, controls, other_required),
    model_id
  )

  keep <- rep(TRUE, nrow(data))
  excluded_at_step <- function(variables) {
    variables <- stats::na.omit(variables)
    if (length(variables) == 0) return(0L)
    missing_now <- !stats::complete.cases(data[, variables, drop = FALSE])
    excluded <- sum(keep & missing_now)
    keep <<- keep & !missing_now
    as.integer(excluded)
  }

  tibble::tibble(
    model_id = model_id,
    starting_rows = nrow(data),
    excluded_missing_outcome = excluded_at_step(outcome),
    excluded_missing_fdi = excluded_at_step(exposure),
    excluded_missing_demographic = excluded_at_step(moderator),
    excluded_missing_controls = excluded_at_step(controls),
    excluded_other = excluded_at_step(other_required),
    estimation_rows = sum(keep)
  )
}

moderator_values <- function(data, variable) {
  x <- as.numeric(data[[variable]])
  finite <- x[is.finite(x)]
  if (length(finite) == 0) return(numeric())

  if (any(finite < 0) && any(finite > 0)) {
    values <- stats::quantile(
      finite,
      probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
      names = FALSE,
      na.rm = TRUE
    )
    names(values) <- c("p10", "p25", "median", "p75", "p90")
  } else {
    positive <- finite[finite > 0]
    values <- c(
      zero = if (any(finite == 0)) 0 else NA_real_,
      median_positive = if (length(positive)) stats::median(positive) else NA_real_,
      p75_positive = if (length(positive)) stats::quantile(positive, 0.75, names = FALSE) else NA_real_,
      p90_positive = if (length(positive)) stats::quantile(positive, 0.90, names = FALSE) else NA_real_
    )
  }

  values <- values[is.finite(values)]
  values[!duplicated(unname(values))]
}

interaction_slopes <- function(
  model,
  exposure,
  moderator,
  moderator_at = NULL,
  by = NULL
) {
  if (is.null(moderator_at)) {
    model_data <- tryCatch(insight::get_data(model), error = function(e) NULL)
    if (is.null(model_data)) stop("Supply moderator_at because the model data could not be recovered.")
    moderator_at <- moderator_values(model_data, moderator)
  }

  newdata <- do.call(
    marginaleffects::datagrid,
    c(
      list(model = model),
      stats::setNames(list(unname(moderator_at)), moderator)
    )
  )

  marginaleffects::slopes(
    model,
    variables = exposure,
    newdata = newdata,
    by = by
  )
}

show_models <- function(models, stars = TRUE) {
  modelsummary::modelsummary(models, output = "viewer", stars = stars)
}

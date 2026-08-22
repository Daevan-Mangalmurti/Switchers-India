# ============================================================
# 03_export_results.R
# Export only specifications that have been selected after exploration.
# Edit selected_specs below before producing paper tables and figures.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages()
source(file.path(project_root, "R", "model_helpers.R"))

paths <- build_project_paths(project_root)
create_output_directories(paths)

ac_year <- readRDS(file.path(paths$final_dir, "ac_year.rds"))
ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))
voters <- readRDS(file.path(paths$final_dir, "nes_respondent_analysis.rds"))
ideology_cells <- readRDS(file.path(paths$final_dir, "ac_year_ideology_summary.rds"))

ac_controls <- c(
  "proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share"
)

voter_controls <- c(
  "education_harmonized", "income_harmonized", "caste_group", "religion_group",
  "proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share"
)

# ============================================================
# EDIT THIS TABLE: one row per specification to preserve/export.
# ============================================================

selected_specs <- tibble::tribble(
  ~model_id, ~model_label, ~model_family, ~dataset, ~outcome, ~exposure, ~moderator, ~controls_name, ~fixed_effects, ~cluster, ~weights, ~sample, ~interpretive_status,
  "ac_mfg_migration", "Manufacturing FDI × migration", "pooled_ac", "ac_year", "fr_party_vote_share", "log1p_fdi_mfg_local_all_pc100k", "log1p_mig_prior_5_15yr_total", "ac", "state_no + year", "pc_cluster_id", NA_character_, "all AC-years", "descriptive",
  "ac_total_migration", "Total FDI × migration", "pooled_ac", "ac_year", "fr_party_vote_share", "log1p_fdi_total_local_all_pc100k", "log1p_mig_prior_5_15yr_total", "ac", "state_no + year", "pc_cluster_id", NA_character_, "all AC-years", "descriptive",
  "fd_mfg_baseline_migration", "Change in manufacturing FDI × baseline migration", "first_difference", "ac_change", "d_fr_party_vote_share_2009_2014_pp", "d_log1p_fdi_mfg_local_all_pc100k_2009_2014", "log1p_mig_total_upto_2001", "change", "state_no", "pc_cluster_id", NA_character_, "ACs observed in both years", "within-unit causal-leaning",
  "voter_mfg_migration", "Voter far-right choice: manufacturing FDI × migration × ideology", "voter_logit", "voters", "voted_fr", "log1p_fdi_mfg_local_all_pc100k", "log1p_mig_prior_5_15yr_total", "voter", "state_no + year", "pc_cluster_id", "survey_weight_norm_year", "valid vote and ideology", "adjusted individual association"
)

control_sets <- list(
  ac = ac_controls,
  change = c(
    "fr_party_vote_share_2009", "proxy_ac_pop", "con08_land_area",
    "sc_pop_share", "st_pop_share"
  ),
  voter = voter_controls
)

data_sets <- list(
  ac_year = ac_year,
  ac_change = ac_change,
  voters = voters |> dplyr::filter(vote_valid, !is.na(voter_ideology)),
  ideology_cells = ideology_cells |> dplyr::filter(n_vote_valid >= 5)
)

fit_specification <- function(spec) {
  data <- data_sets[[spec$dataset]]
  controls <- control_sets[[spec$controls_name]]

  switch(
    spec$model_family,
    pooled_ac = fit_ac_interaction(
      data = data,
      outcome = spec$outcome,
      exposure = spec$exposure,
      moderator = spec$moderator,
      controls = controls,
      fixed_effects = spec$fixed_effects,
      cluster = stats::as.formula(paste0("~", spec$cluster))
    ),
    first_difference = fit_change_interaction(
      data = data,
      outcome = spec$outcome,
      exposure_change = spec$exposure,
      moderator = spec$moderator,
      controls = controls,
      fixed_effects = spec$fixed_effects,
      cluster = stats::as.formula(paste0("~", spec$cluster))
    ),
    voter_logit = fit_voter_interaction(
      data = data,
      outcome = spec$outcome,
      exposure = spec$exposure,
      moderator = spec$moderator,
      ideology = "voter_ideology",
      controls = controls,
      fixed_effects = spec$fixed_effects,
      weights = stats::as.formula(paste0("~", spec$weights)),
      cluster = stats::as.formula(paste0("~", spec$cluster))
    ),
    voter_closeness = fit_voter_binary_outcome(
      data = data,
      outcome = spec$outcome,
      exposure = spec$exposure,
      moderator = spec$moderator,
      ideology = "voter_ideology",
      controls = controls,
      fixed_effects = spec$fixed_effects,
      weights = stats::as.formula(paste0("~", spec$weights)),
      cluster = stats::as.formula(paste0("~", spec$cluster))
    ),
    ideology_cell = fit_ideology_cell_binomial(
      data = data,
      exposure = spec$exposure,
      moderator = spec$moderator,
      controls = controls,
      fixed_effects = spec$fixed_effects,
      cluster = stats::as.formula(paste0("~", spec$cluster))
    ),
    stop("Unknown model family: ", spec$model_family)
  )
}

model_results <- selected_specs |>
  dplyr::mutate(
    fit = purrr::pmap(
      dplyr::pick(dplyr::everything()),
      function(...) {
        spec <- list(...)
        tryCatch(fit_specification(spec), error = identity)
      }
    ),
    model_fit = !purrr::map_lgl(fit, inherits, "error"),
    error = purrr::map_chr(
      fit,
      ~ if (inherits(.x, "error")) conditionMessage(.x) else NA_character_
    )
  )

# ============================================================
# Machine-readable model outputs
# ============================================================

selected_model_manifest <- model_results |>
  dplyr::transmute(
    model_id, model_label, model_family, dataset, outcome,
    sample, formula = paste0(outcome, " ~ ", exposure, " * ", moderator),
    fixed_effects, cluster, weights, fdi_variable = exposure,
    demographic_variable = moderator,
    controls = purrr::map_chr(controls_name, ~ paste(control_sets[[.x]], collapse = " + ")),
    interpretive_status, model_fit, error
  )

selected_model_estimates <- model_results |>
  dplyr::filter(model_fit) |>
  dplyr::transmute(
    model_id,
    estimates = purrr::map2(
      fit,
      model_id,
      ~ broom::tidy(.x, conf.int = TRUE) |>
        dplyr::transmute(
          model_id = .y,
          term,
          estimate,
          std_error = std.error,
          statistic,
          p_value = p.value,
          conf_low = conf.low,
          conf_high = conf.high,
          nobs = stats::nobs(.x)
        )
    )
  ) |>
  tidyr::unnest(estimates)

specification_failures <- model_results |>
  dplyr::filter(!model_fit) |>
  dplyr::transmute(model_id, stage = "model fitting", error, warning = NA_character_)

model_fit_diagnostics <- model_results |>
  dplyr::mutate(
    data = purrr::map(dataset, ~ data_sets[[.x]]),
    nobs = purrr::map_int(fit, ~ if (inherits(.x, "error")) NA_integer_ else as.integer(stats::nobs(.x))),
    n_acs = purrr::map_int(data, ~ dplyr::n_distinct(.x$ac_uid, na.rm = TRUE)),
    n_pcs = purrr::map_int(data, ~ dplyr::n_distinct(.x$pc_cluster_id, na.rm = TRUE)),
    n_district_groups = purrr::map_int(data, ~ dplyr::n_distinct(.x$district_harmonization_group_id, na.rm = TRUE)),
    n_states = purrr::map_int(data, ~ dplyr::n_distinct(.x$state_no, na.rm = TRUE))
  ) |>
  dplyr::transmute(
    model_id, model_fit, nobs, n_acs, n_pcs, n_district_groups, n_states,
    convergence = dplyr::if_else(model_fit, "fit returned", "failed"),
    singular_fit = NA,
    separation = NA,
    warning = error
  )

sample_flow <- selected_specs |>
  dplyr::mutate(
    controls = purrr::map(controls_name, ~ control_sets[[.x]]),
    data = purrr::map(dataset, ~ data_sets[[.x]]),
    flow = purrr::pmap(
      list(data, model_id, outcome, exposure, moderator, controls, model_family, weights),
      function(data, model_id, outcome, exposure, moderator, controls, model_family, weights) {
        other_required <- c(
          if (model_family %in% c("voter_logit", "voter_closeness")) "voter_ideology" else NULL,
          if (!is.na(weights) && nzchar(weights)) weights else NULL
        )
        model_sample_flow(
          data = data,
          model_id = model_id,
          outcome = outcome,
          exposure = exposure,
          moderator = moderator,
          controls = controls,
          other_required = other_required
        )
      }
    )
  ) |>
  dplyr::select(model_id, flow) |>
  tidyr::unnest(flow)

# ============================================================
# Marginal effects for selected interaction models
# ============================================================

prepare_slope_output <- function(effects, spec, model, ideology_variable = NULL) {
  effects <- tibble::as_tibble(as.data.frame(effects))
  if (!spec$moderator %in% names(effects)) effects[[spec$moderator]] <- NA_real_
  if (!"contrast" %in% names(effects)) {
    effects$contrast <- paste0("Marginal effect of ", spec$exposure)
  }
  for (column in c("estimate", "std.error", "conf.low", "conf.high", "p.value")) {
    if (!column %in% names(effects)) effects[[column]] <- NA_real_
  }

  tibble::tibble(
    model_id = spec$model_id,
    focal_variable = spec$exposure,
    moderator_variable = spec$moderator,
    moderator_value = as.numeric(effects[[spec$moderator]]),
    ideology = if (is.null(ideology_variable) || !ideology_variable %in% names(effects)) {
      rep(NA_character_, nrow(effects))
    } else {
      as.character(effects[[ideology_variable]])
    },
    contrast = as.character(effects$contrast),
    estimate = as.numeric(effects$estimate),
    std_error = as.numeric(effects$std.error),
    conf_low = as.numeric(effects$conf.low),
    conf_high = as.numeric(effects$conf.high),
    p_value = as.numeric(effects$p.value),
    nobs = stats::nobs(model)
  )
}

make_marginal_effects <- function(spec, model) {
  if (inherits(model, "error")) return(tibble::tibble())
  data <- data_sets[[spec$dataset]]
  values <- moderator_values(data, spec$moderator)
  if (length(values) == 0) return(tibble::tibble())

  if (spec$model_family %in% c("voter_logit", "voter_closeness")) {
    grid <- do.call(
      marginaleffects::datagrid,
      c(
        list(model = model),
        stats::setNames(list(unname(values)), spec$moderator),
        list(voter_ideology = levels(droplevels(data$voter_ideology)))
      )
    )

    effects <- marginaleffects::slopes(
      model,
      variables = spec$exposure,
      newdata = grid,
      by = c(spec$moderator, "voter_ideology"),
      type = "response"
    )
    prepare_slope_output(effects, spec, model, "voter_ideology")
  } else {
    grid <- do.call(
      marginaleffects::datagrid,
      c(list(model = model), stats::setNames(list(unname(values)), spec$moderator))
    )

    effects <- marginaleffects::slopes(
      model,
      variables = spec$exposure,
      newdata = grid,
      by = spec$moderator
    )
    prepare_slope_output(effects, spec, model)
  }
}

selected_marginal_effects <- purrr::map2_dfr(
  split(selected_specs, seq_len(nrow(selected_specs))),
  model_results$fit,
  ~ tryCatch(make_marginal_effects(.x, .y), error = function(e) {
    tibble::tibble(
      model_id = .x$model_id,
      focal_variable = .x$exposure,
      moderator_variable = .x$moderator,
      moderator_value = NA_real_, ideology = NA_character_, contrast = NA_character_,
      estimate = NA_real_, std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      p_value = NA_real_, nobs = if (inherits(.y, "error")) NA_integer_ else stats::nobs(.y),
      marginal_effect_error = conditionMessage(e)
    )
  })
)

readr::write_csv(selected_model_manifest, file.path(paths$result_dir, "selected_model_manifest.csv"), na = "")
readr::write_csv(selected_model_estimates, file.path(paths$result_dir, "selected_model_estimates.csv"), na = "")
readr::write_csv(selected_marginal_effects, file.path(paths$result_dir, "selected_marginal_effects.csv"), na = "")
readr::write_csv(model_fit_diagnostics, file.path(paths$result_dir, "model_fit_diagnostics.csv"), na = "")
readr::write_csv(specification_failures, file.path(paths$result_dir, "specification_failures.csv"), na = "")
readr::write_csv(sample_flow, file.path(paths$result_dir, "sample_flow.csv"), na = "")

# Preserve fitted model objects for exact reproduction.
saveRDS(model_results, file.path(paths$result_dir, "selected_models.rds"))

# ============================================================
# Selected human-readable tables and figures
# ============================================================

fitted_models <- model_results |>
  dplyr::filter(model_fit) |>
  dplyr::select(model_label, fit) |>
  tibble::deframe()

if (length(fitted_models) > 0) {
  modelsummary::modelsummary(
    fitted_models,
    output = file.path(paths$result_dir, "selected_models.html"),
    stars = TRUE,
    gof_omit = "IC|Log|Adj|Within|RMSE"
  )
}

purrr::pwalk(
  list(
    spec = split(selected_specs, seq_len(nrow(selected_specs))),
    model = model_results$fit
  ),
  function(spec, model) {
    if (inherits(model, "error")) return(invisible(NULL))
    plot <- tryCatch({
      condition <- if (spec$model_family %in% c("voter_logit", "voter_closeness")) {
        c(spec$moderator, "voter_ideology")
      } else {
        spec$moderator
      }
      if (spec$model_family %in% c("voter_logit", "voter_closeness", "ideology_cell")) {
        marginaleffects::plot_slopes(
          model,
          variables = spec$exposure,
          condition = condition,
          type = "response"
        )
      } else {
        marginaleffects::plot_slopes(
          model,
          variables = spec$exposure,
          condition = condition
        )
      }
    }, error = function(e) NULL)
    if (!is.null(plot)) {
      ggplot2::ggsave(
        file.path(paths$result_dir, paste0(spec$model_id, "_marginal_effects.png")),
        plot,
        width = 9,
        height = 6,
        dpi = 300
      )
    }
  }
)

message("Selected results exported to: ", paths$result_dir)

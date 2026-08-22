# ============================================================
# 03_respondent_post_estimation_audit.R
#
# Post-estimation audit for the COMPLETE primary respondent
# specification universe.
#
# IMPORTANT:
#   - This script does NOT rerun the 165,888-model primary multiverse.
#   - It reads only finalized FULL v1.0.3 primary CSVs.
#   - It ignores pilot outputs automatically.
#   - It performs two small preferred-model refits only to construct
#     interpretable conditional-effect plots.
#
# Expected frozen primary universe:
#   24 curves
#   165,888 specifications
#   Revision:
#     2026-08-08-v1.0.3-targeted-robustness-hotfix
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

AUDIT_REVISION <-
  "2026-08-08-v1.0-respondent-post-estimation-audit"

PRIMARY_MODEL_REVISION <-
  "2026-08-08-v1.0.3-targeted-robustness-hotfix"

message(
  "Starting respondent post-estimation audit: ",
  AUDIT_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

respondent_spec_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "respondent_specification_curves"
)

result_dir <- file.path(
  respondent_spec_root,
  "results"
)

audit_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "respondent_post_estimation_audit"
)

audit_summary_dir <- file.path(
  audit_root,
  "summaries"
)

audit_figure_dir <- file.path(
  audit_root,
  "figures"
)

audit_diagnostic_dir <- file.path(
  audit_root,
  "diagnostics"
)

audit_preferred_dir <- file.path(
  audit_root,
  "preferred_models"
)

audit_manifest_dir <- file.path(
  audit_root,
  "manifests"
)

purrr::walk(
  c(
    audit_root,
    audit_summary_dir,
    audit_figure_dir,
    audit_diagnostic_dir,
    audit_preferred_dir,
    audit_manifest_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD ONLY FINALIZED FULL PRIMARY RESULT FILES
# ============================================================

all_result_files <- list.files(
  result_dir,
  full.names = TRUE
)

primary_full_files <- all_result_files[
  grepl(
    "^primary__",
    basename(all_result_files)
  ) &
    grepl(
      "__full__",
      basename(all_result_files),
      fixed = TRUE
    ) &
    grepl(
      PRIMARY_MODEL_REVISION,
      basename(all_result_files),
      fixed = TRUE
    ) &
    grepl(
      "\\.csv$",
      basename(all_result_files)
    )
]

primary_full_files <- sort(
  primary_full_files
)

if (
  length(primary_full_files) != 24
) {
  stop(
    "Expected exactly 24 finalized full primary CSVs, but found ",
    length(primary_full_files),
    ". Do not continue until the result directory is audited."
  )
}

message(
  "Found all 24 finalized full primary curve files."
)

# Keep the columns required for the audit. Reading fewer repeated string
# columns materially reduces RAM use.
audit_columns <- c(
  "fdi_family",
  "fdi_scope",
  "fdi_status",
  "fdi_form",
  "fdi_family_label",
  "fdi_scope_label",
  "fdi_status_label",
  "fdi_form_label",
  "exposure_var",
  "fdi_preferred",
  "moderator_var",
  "moderator_family",
  "moderator_form",
  "orientation",
  "moderator_domain",
  "moderator_preferred",
  "voter_control_set",
  "voter_control_label",
  "voter_control_preferred",
  "context_control_set",
  "context_control_label",
  "context_control_preferred",
  "analysis_family",
  "estimator",
  "outcome_sample",
  "design_id",
  "design_type",
  "design_label",
  "design_preferred",
  "interaction_order",
  "center_var",
  "center_definition",
  "fixed_effects",
  "cluster_rule",
  "respondent_model_preferred",
  "spec_key",
  "fdi_low",
  "fdi_high",
  "delta_fdi",
  "fdi_reference_method",
  "moderator_low",
  "moderator_high",
  "delta_moderator",
  "moderator_reference_method",
  "reference_valid",
  "fit_ok",
  "interaction_estimate",
  "interaction_se",
  "interaction_p",
  "interaction_conf_low",
  "interaction_conf_high",
  "contrast_estimate",
  "contrast_conf_low",
  "contrast_conf_high",
  "ci_excludes_zero",
  "nobs",
  "weighted_ess",
  "n_acs",
  "n_pcs",
  "n_districts",
  "r2",
  "error"
)

read_one_primary_result <- function(
    path
) {
  out <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )

  missing <- setdiff(
    audit_columns,
    names(out)
  )

  if (
    length(missing) > 0
  ) {
    stop(
      "Result file ",
      basename(path),
      " is missing required columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  out |>
    dplyr::select(
      dplyr::all_of(
        audit_columns
      )
    ) |>
    dplyr::mutate(
      source_file =
        basename(path)
    )
}

primary_results <- purrr::map_dfr(
  primary_full_files,
  read_one_primary_result
)

# ============================================================
# 2. HARD COMPLETION / UNIQUENESS AUDIT
# ============================================================

expected_total_models <- 165888L

if (
  nrow(primary_results) !=
  expected_total_models
) {
  stop(
    "Expected ",
    expected_total_models,
    " full primary rows but loaded ",
    nrow(primary_results),
    "."
  )
}

n_unique_specs <- dplyr::n_distinct(
  primary_results$spec_key
)

if (
  n_unique_specs !=
  expected_total_models
) {
  stop(
    "Expected ",
    expected_total_models,
    " unique spec_key values but found ",
    n_unique_specs,
    "."
  )
}

curve_completion <- primary_results |>
  dplyr::count(
    design_id,
    design_type,
    design_label,
    moderator_domain,
    fdi_family,
    fdi_family_label,
    interaction_order,
    name = "observed_models"
  ) |>
  dplyr::mutate(
    expected_models =
      dplyr::if_else(
        moderator_domain ==
          "muslim",
        4608L,
        9216L
      ),
    complete =
      observed_models ==
        expected_models
  ) |>
  dplyr::arrange(
    design_type,
    moderator_domain,
    fdi_family,
    interaction_order
  )

if (
  nrow(curve_completion) != 24 ||
  any(
    !curve_completion$complete
  )
) {
  stop(
    "At least one of the 24 primary curves is incomplete."
  )
}

readr::write_csv(
  curve_completion,
  file.path(
    audit_manifest_dir,
    "01_primary_curve_completion_audit.csv"
  )
)

primary_completion_manifest <- tibble::tibble(
  audit_revision =
    AUDIT_REVISION,
  primary_model_revision =
    PRIMARY_MODEL_REVISION,
  n_curve_files =
    length(
      primary_full_files
    ),
  n_primary_rows =
    nrow(
      primary_results
    ),
  n_unique_spec_keys =
    n_unique_specs,
  n_successful =
    sum(
      primary_results$fit_ok,
      na.rm = TRUE
    ),
  n_failed =
    sum(
      !primary_results$fit_ok,
      na.rm = TRUE
    ),
  share_successful =
    mean(
      primary_results$fit_ok,
      na.rm = TRUE
    )
)

readr::write_csv(
  primary_completion_manifest,
  file.path(
    audit_manifest_dir,
    "00_primary_completion_manifest.csv"
  )
)

message(
  "Completion audit passed: ",
  format(
    nrow(primary_results),
    big.mark = ","
  ),
  " unique primary specifications."
)

# ============================================================
# 3. FAILURE AUDIT
# ============================================================

failure_rows <- primary_results |>
  dplyr::filter(
    !fit_ok
  )

failure_summary <- failure_rows |>
  dplyr::count(
    design_id,
    moderator_domain,
    fdi_family,
    interaction_order,
    fdi_scope,
    fdi_status,
    fdi_form,
    voter_control_set,
    context_control_set,
    error,
    sort = TRUE,
    name =
      "n_failures"
  )

readr::write_csv(
  failure_rows,
  file.path(
    audit_diagnostic_dir,
    "02_all_failed_primary_specifications.csv"
  )
)

readr::write_csv(
  failure_summary,
  file.path(
    audit_diagnostic_dir,
    "03_failure_localization_summary.csv"
  )
)

# ============================================================
# 4. CORE SUMMARY HELPERS
# ============================================================

summarize_effect_distribution <- function(
    data
) {
  valid <- data |>
    dplyr::filter(
      fit_ok,
      is.finite(
        contrast_estimate
      )
    )

  if (
    nrow(valid) == 0
  ) {
    return(
      tibble::tibble(
        n_planned = nrow(data),
        n_fit = 0L,
        share_fit = 0,
        median_contrast_pp = NA_real_,
        mean_contrast_pp = NA_real_,
        q10_contrast_pp = NA_real_,
        q25_contrast_pp = NA_real_,
        q75_contrast_pp = NA_real_,
        q90_contrast_pp = NA_real_,
        share_positive = NA_real_,
        share_negative = NA_real_,
        share_ci_positive = NA_real_,
        share_ci_negative = NA_real_,
        share_ci_excludes_zero = NA_real_,
        median_nobs = NA_real_,
        median_weighted_ess = NA_real_,
        median_n_acs = NA_real_,
        median_n_pcs = NA_real_,
        median_n_districts = NA_real_
      )
    )
  }

  tibble::tibble(
    n_planned =
      nrow(data),

    n_fit =
      nrow(valid),

    share_fit =
      nrow(valid) /
      nrow(data),

    median_contrast_pp =
      stats::median(
        valid$contrast_estimate,
        na.rm = TRUE
      ),

    mean_contrast_pp =
      mean(
        valid$contrast_estimate,
        na.rm = TRUE
      ),

    q10_contrast_pp =
      stats::quantile(
        valid$contrast_estimate,
        0.10,
        na.rm = TRUE,
        names = FALSE
      ),

    q25_contrast_pp =
      stats::quantile(
        valid$contrast_estimate,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),

    q75_contrast_pp =
      stats::quantile(
        valid$contrast_estimate,
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),

    q90_contrast_pp =
      stats::quantile(
        valid$contrast_estimate,
        0.90,
        na.rm = TRUE,
        names = FALSE
      ),

    share_positive =
      mean(
        valid$contrast_estimate >
          0,
        na.rm = TRUE
      ),

    share_negative =
      mean(
        valid$contrast_estimate <
          0,
        na.rm = TRUE
      ),

    share_ci_positive =
      mean(
        valid$contrast_conf_low >
          0,
        na.rm = TRUE
      ),

    share_ci_negative =
      mean(
        valid$contrast_conf_high <
          0,
        na.rm = TRUE
      ),

    share_ci_excludes_zero =
      mean(
        valid$ci_excludes_zero,
        na.rm = TRUE
      ),

    median_nobs =
      stats::median(
        valid$nobs,
        na.rm = TRUE
      ),

    median_weighted_ess =
      stats::median(
        valid$weighted_ess,
        na.rm = TRUE
      ),

    median_n_acs =
      stats::median(
        valid$n_acs,
        na.rm = TRUE
      ),

    median_n_pcs =
      stats::median(
        valid$n_pcs,
        na.rm = TRUE
      ),

    median_n_districts =
      stats::median(
        valid$n_districts,
        na.rm = TRUE
      )
  )
}

grouped_effect_summary <- function(
    data,
    grouping_vars
) {
  data |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          grouping_vars
        )
      )
    ) |>
    dplyr::group_modify(
      ~summarize_effect_distribution(
        .x
      )
    ) |>
    dplyr::ungroup()
}

# ============================================================
# 5. CURVE-LEVEL SUMMARY: THE MAIN TABLE
# ============================================================

curve_summary <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "design_type",
    "design_label",
    "moderator_domain",
    "fdi_family",
    "fdi_family_label",
    "interaction_order"
  )
) |>
  dplyr::arrange(
    design_type,
    moderator_domain,
    fdi_family,
    interaction_order
  )

readr::write_csv(
  curve_summary,
  file.path(
    audit_summary_dir,
    "10_primary_curve_summary.csv"
  )
)

# ============================================================
# 6. DECOMPOSITION SUMMARIES
# ============================================================

summary_by_fdi_scope <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "fdi_scope",
    "fdi_scope_label"
  )
)

summary_by_fdi_status <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "fdi_status",
    "fdi_status_label"
  )
)

summary_by_fdi_form <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "fdi_form",
    "fdi_form_label"
  )
)

summary_by_fdi_definition <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "fdi_scope",
    "fdi_status",
    "fdi_form",
    "fdi_scope_label",
    "fdi_status_label",
    "fdi_form_label"
  )
)

summary_by_moderator_family <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "moderator_family"
  )
)

summary_by_moderator_variable <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "moderator_family",
    "moderator_var",
    "moderator_form"
  )
)

summary_by_voter_controls <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "voter_control_set",
    "voter_control_label"
  )
)

summary_by_context_controls <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "context_control_set",
    "context_control_label"
  )
)

summary_by_vxc_controls <- grouped_effect_summary(
  primary_results,
  c(
    "design_id",
    "moderator_domain",
    "fdi_family",
    "interaction_order",
    "voter_control_set",
    "context_control_set"
  )
)

summary_objects <- list(
  "11_summary_by_fdi_scope.csv" =
    summary_by_fdi_scope,
  "12_summary_by_fdi_status.csv" =
    summary_by_fdi_status,
  "13_summary_by_fdi_form.csv" =
    summary_by_fdi_form,
  "14_summary_by_fdi_definition.csv" =
    summary_by_fdi_definition,
  "15_summary_by_moderator_family.csv" =
    summary_by_moderator_family,
  "16_summary_by_moderator_variable.csv" =
    summary_by_moderator_variable,
  "17_summary_by_voter_controls.csv" =
    summary_by_voter_controls,
  "18_summary_by_context_controls.csv" =
    summary_by_context_controls,
  "19_summary_by_vxc_controls.csv" =
    summary_by_vxc_controls
)

purrr::iwalk(
  summary_objects,
  ~readr::write_csv(
    .x,
    file.path(
      audit_summary_dir,
      .y
    )
  )
)

# ============================================================
# 7. THEORY-PREFERRED ROWS
# ============================================================

preferred_2014_muslim_rows <- primary_results |>
  dplyr::filter(
    design_id ==
      "respondent_2014_muslim",
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
  dplyr::arrange(
    interaction_order
  )

if (
  nrow(
    preferred_2014_muslim_rows
  ) != 2
) {
  stop(
    "Could not uniquely recover the two preferred 2014 Muslim rows."
  )
}

readr::write_csv(
  preferred_2014_muslim_rows,
  file.path(
    audit_preferred_dir,
    "20_preferred_2014_mfg_muslim_models.csv"
  )
)

# Hold moderator + V2+C1 fixed and inspect all 18 manufacturing-FDI definitions.
preferred_moderator_all_mfg_definitions <- primary_results |>
  dplyr::filter(
    design_id ==
      "respondent_2014_muslim",
    fdi_family ==
      "mfg",
    moderator_var ==
      "muslim_share_2001_dist_proxy",
    voter_control_set ==
      "V2",
    context_control_set ==
      "C1"
  ) |>
  dplyr::arrange(
    interaction_order,
    fdi_scope,
    fdi_status,
    fdi_form
  )

if (
  nrow(
    preferred_moderator_all_mfg_definitions
  ) != 36
) {
  stop(
    "Expected 36 rows = 18 manufacturing FDI definitions x 2 interaction orders."
  )
}

readr::write_csv(
  preferred_moderator_all_mfg_definitions,
  file.path(
    audit_preferred_dir,
    "21_preferred_muslim_moderator_all_18_mfg_fdi_definitions.csv"
  )
)

# ============================================================
# 8. 24-PAGE FULL SPECIFICATION-CURVE PDF
# ============================================================

make_curve_page <- function(
    data
) {
  valid <- data |>
    dplyr::filter(
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
      specification_rank =
        dplyr::row_number(),
      rank_percentile =
        100 *
        (
          specification_rank -
            1
        ) /
        pmax(
          dplyr::n() -
            1,
          1
        )
    )

  median_effect <- stats::median(
    valid$contrast_estimate,
    na.rm = TRUE
  )

  ggplot2::ggplot(
    valid,
    ggplot2::aes(
      x = rank_percentile,
      y = contrast_estimate
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.35
    ) +
    ggplot2::geom_hline(
      yintercept =
        median_effect,
      linetype = "dotted",
      linewidth = 0.35
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin =
          contrast_conf_low,
        ymax =
          contrast_conf_high
      ),
      alpha = 0.08,
      linewidth = 0.10
    ) +
    ggplot2::geom_point(
      size = 0.45,
      alpha = 0.55
    ) +
    ggplot2::labs(
      title =
        paste(
          dplyr::first(
            valid$design_label
          ),
          dplyr::first(
            valid$fdi_family_label
          ),
          if (
            dplyr::first(
              valid$interaction_order
            ) ==
              "two_way"
          ) {
            "Two-way FDI x demographic contrast"
          } else {
            "Center amplification of FDI x demographic contrast"
          },
          sep = " | "
        ),

      subtitle =
        paste0(
          "N fitted = ",
          format(
            nrow(valid),
            big.mark = ","
          ),
          "; median = ",
          sprintf(
            "%.2f",
            median_effect
          ),
          " pp; ",
          sprintf(
            "%.1f",
            100 *
              mean(
                valid$contrast_estimate >
                  0
              )
          ),
          "% positive; ",
          sprintf(
            "%.1f",
            100 *
              mean(
                valid$contrast_conf_low >
                  0
              )
          ),
          "% CI entirely > 0"
        ),

      x =
        "Specification-rank percentile",

      y =
        "Substantive contrast in BJP-voting probability (percentage points)"
    ) +
    ggplot2::theme_minimal(
      base_size = 10
    )
}

curve_groups <- primary_results |>
  dplyr::group_split(
    design_id,
    fdi_family,
    interaction_order
  )

curve_pdf <- file.path(
  audit_figure_dir,
  "30_full_primary_specification_curves_24_pages.pdf"
)

grDevices::pdf(
  curve_pdf,
  width = 11,
  height = 7.5,
  onefile = TRUE
)

for (
  curve_data in curve_groups
) {
  print(
    make_curve_page(
      curve_data
    )
  )
}

grDevices::dev.off()

# ============================================================
# 9. CURVE-SUMMARY FOREST PLOT
# ============================================================

curve_summary_plot_data <- curve_summary |>
  dplyr::mutate(
    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI x demographic",
        triple =
          "Center amplification"
      ),

    design_label_short =
      dplyr::case_when(
        design_type ==
          "baseline_2014" ~
          "2014 baseline-adjusted",
        TRUE ~
          "Pooled 2009/2014"
      ),

    curve_label =
      paste(
        fdi_family_label,
        interaction_label,
        sep = " — "
      )
  )

p_curve_summary <- ggplot2::ggplot(
  curve_summary_plot_data,
  ggplot2::aes(
    x = median_contrast_pp,
    y = curve_label
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = q25_contrast_pp,
      xmax = q75_contrast_pp
    ),
    linewidth = 0.8,
    orientation = "y"
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_grid(
    moderator_domain ~
      design_label_short,
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Respondent specification universe: median substantive effects",

    subtitle =
      "Points are specification medians; horizontal lines are the interquartile range across specifications.",

    x =
      "BJP-voting probability contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "31_curve_summary_median_iqr.pdf"
  ),
  p_curve_summary,
  width = 12,
  height = 8
)

# ============================================================
# 10. 2014 MODERATOR-FAMILY DECOMPOSITION
# ============================================================

moderator_family_2014 <- summary_by_moderator_family |>
  dplyr::filter(
    grepl(
      "^respondent_2014_",
      design_id
    )
  ) |>
  dplyr::mutate(
    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI x demographic",
        triple =
          "Center amplification"
      )
  )

readr::write_csv(
  moderator_family_2014,
  file.path(
    audit_summary_dir,
    "22_2014_moderator_family_summary.csv"
  )
)

p_mod_family <- ggplot2::ggplot(
  moderator_family_2014,
  ggplot2::aes(
    x = median_contrast_pp,
    y = stats::reorder(
      moderator_family,
      median_contrast_pp
    )
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin = q25_contrast_pp,
      xmax = q75_contrast_pp
    ),
    orientation = "y",
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    size = 1.5
  ) +
  ggplot2::facet_grid(
    moderator_domain +
      interaction_label ~
      fdi_family_label,
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::labs(
    title =
      "2014 baseline-adjusted results by demographic moderator family",

    subtitle =
      "Point = median specification contrast; line = interquartile range.",

    x =
      "BJP-voting probability contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 8.5
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "32_2014_moderator_family_decomposition.pdf"
  ),
  p_mod_family,
  width = 14,
  height = 11
)

# ============================================================
# 11. FDI-DIMENSION DECOMPOSITION
# ============================================================

fdi_dimension_long <- dplyr::bind_rows(
  summary_by_fdi_scope |>
    dplyr::transmute(
      design_id,
      moderator_domain,
      fdi_family,
      fdi_family_label,
      interaction_order,
      dimension =
        "Geography",
      choice =
        fdi_scope_label,
      dplyr::across(
        n_planned:
          median_n_districts
      )
    ),

  summary_by_fdi_status |>
    dplyr::transmute(
      design_id,
      moderator_domain,
      fdi_family,
      fdi_family_label,
      interaction_order,
      dimension =
        "Status",
      choice =
        fdi_status_label,
      dplyr::across(
        n_planned:
          median_n_districts
      )
    ),

  summary_by_fdi_form |>
    dplyr::transmute(
      design_id,
      moderator_domain,
      fdi_family,
      fdi_family_label,
      interaction_order,
      dimension =
        "Functional form",
      choice =
        fdi_form_label,
      dplyr::across(
        n_planned:
          median_n_districts
      )
    )
)

readr::write_csv(
  fdi_dimension_long,
  file.path(
    audit_summary_dir,
    "23_fdi_dimension_decomposition_long.csv"
  )
)

# Main 2014-only visual, where the temporal design is strongest.
p_fdi_dimensions <- fdi_dimension_long |>
  dplyr::filter(
    grepl(
      "^respondent_2014_",
      design_id
    )
  ) |>
  dplyr::mutate(
    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "Two-way",
        triple =
          "Center amplification"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = choice,
      y = median_contrast_pp
    )
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 1.8
  ) +
  ggplot2::facet_grid(
    moderator_domain +
      interaction_label ~
      fdi_family_label +
      dimension,
    scales = "free_x",
    space = "free_x"
  ) +
  ggplot2::labs(
    title =
      "2014 respondent results by FDI design choice",

    x = NULL,

    y =
      "Median substantive contrast (percentage points)"
  ) +
  ggplot2::theme_minimal(
    base_size = 8
  ) +
  ggplot2::theme(
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "33_2014_fdi_dimension_decomposition.pdf"
  ),
  p_fdi_dimensions,
  width = 18,
  height = 10
)

# ============================================================
# 12. V x C CONTROL-SENSITIVITY HEATMAPS
# ============================================================

control_heatmap_data <- summary_by_vxc_controls |>
  dplyr::filter(
    grepl(
      "^respondent_2014_",
      design_id
    )
  ) |>
  dplyr::mutate(
    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "Two-way",
        triple =
          "Center amplification"
      )
  )

control_heatmap_pdf <- file.path(
  audit_figure_dir,
  "34_2014_control_sensitivity_heatmaps.pdf"
)

grDevices::pdf(
  control_heatmap_pdf,
  width = 8.5,
  height = 6.5,
  onefile = TRUE
)

control_groups <- control_heatmap_data |>
  dplyr::group_split(
    moderator_domain,
    fdi_family,
    interaction_order
  )

for (
  d in control_groups
) {
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = context_control_set,
      y = voter_control_set,
      fill = median_contrast_pp
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(
        label =
          sprintf(
            "%.2f",
            median_contrast_pp
          )
      ),
      size = 4
    ) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0
    ) +
    ggplot2::labs(
      title =
        paste(
          "2014",
          dplyr::first(
            d$moderator_domain
          ),
          dplyr::first(
            d$fdi_family_label
          ),
          dplyr::first(
            d$interaction_label
          ),
          sep = " | "
        ),

      subtitle =
        "Cell value = median substantive contrast across FDI and moderator definitions.",

      x =
        "Context-control block",

      y =
        "Voter-control block",

      fill =
        "Median pp"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    )

  print(p)
}

grDevices::dev.off()

# ============================================================
# 13. PREFERRED MUSLIM MODERATOR ACROSS ALL 18 MFG FDI DEFINITIONS
# ============================================================

preferred_18_plot_data <-
  preferred_moderator_all_mfg_definitions |>
  dplyr::mutate(
    fdi_definition =
      paste(
        fdi_scope_label,
        fdi_status_label,
        fdi_form_label,
        sep = " | "
      ),

    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI x Muslim share",
        triple =
          "Center amplification"
      )
  )

p_preferred_18 <- ggplot2::ggplot(
  preferred_18_plot_data,
  ggplot2::aes(
    x = contrast_estimate,
    y = stats::reorder(
      fdi_definition,
      contrast_estimate
    )
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        contrast_conf_low,
      xmax =
        contrast_conf_high
    ),
    orientation = "y",
    linewidth = 0.45
  ) +
  ggplot2::geom_point(
    size = 1.7
  ) +
  ggplot2::facet_wrap(
    ~interaction_label,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "2014 manufacturing FDI x 2001 Muslim share",

    subtitle =
      "All 18 manufacturing-FDI definitions; V2+C1 held fixed.",

    x =
      "BJP-voting probability contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 8.5
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "35_preferred_muslim_moderator_all_18_mfg_fdi_definitions.pdf"
  ),
  p_preferred_18,
  width = 11,
  height = 9
)

# ============================================================
# 14. SERVICES-FDI EXTREME-RESULT AUDIT FROM SAVED RESULTS
# ============================================================

services_2014_triples <- primary_results |>
  dplyr::filter(
    design_type ==
      "baseline_2014",
    fdi_family ==
      "services",
    interaction_order ==
      "triple",
    fit_ok,
    is.finite(
      contrast_estimate
    )
  ) |>
  dplyr::mutate(
    abs_contrast_pp =
      abs(
        contrast_estimate
      )
  )

services_extreme_specs <- services_2014_triples |>
  dplyr::arrange(
    dplyr::desc(
      abs_contrast_pp
    )
  ) |>
  dplyr::slice_head(
    n = 250
  )

readr::write_csv(
  services_extreme_specs,
  file.path(
    audit_diagnostic_dir,
    "40_services_triple_top_250_absolute_contrasts.csv"
  )
)

services_definition_extremes <- services_2014_triples |>
  dplyr::group_by(
    moderator_domain,
    fdi_scope,
    fdi_status,
    fdi_form,
    fdi_scope_label,
    fdi_status_label,
    fdi_form_label
  ) |>
  dplyr::summarise(
    n =
      dplyr::n(),

    median_contrast_pp =
      stats::median(
        contrast_estimate,
        na.rm = TRUE
      ),

    p90_abs_contrast_pp =
      stats::quantile(
        abs_contrast_pp,
        0.90,
        na.rm = TRUE,
        names = FALSE
      ),

    p95_abs_contrast_pp =
      stats::quantile(
        abs_contrast_pp,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    max_abs_contrast_pp =
      max(
        abs_contrast_pp,
        na.rm = TRUE
      ),

    share_abs_gt_10pp =
      mean(
        abs_contrast_pp >
          10,
        na.rm = TRUE
      ),

    share_positive =
      mean(
        contrast_estimate >
          0,
        na.rm = TRUE
      ),

    share_ci_positive =
      mean(
        contrast_conf_low >
          0,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      max_abs_contrast_pp
    )
  )

readr::write_csv(
  services_definition_extremes,
  file.path(
    audit_diagnostic_dir,
    "41_services_triple_extremeness_by_fdi_definition.csv"
  )
)

# ============================================================
# 15. LOAD RESPONDENT DATA FOR SERVICES SUPPORT + TWO SMALL PREFERRED REFITS
# ============================================================

respondents <- readRDS(
  file.path(
    paths$final_dir,
    "nes_respondent_analysis.rds"
  )
)

ac_change <- readRDS(
  file.path(
    paths$final_dir,
    "ac_change.rds"
  )
)

# In 2014 the harmonized Center definition is exactly identical to strict Center,
# as established in the measurement audit.
respondents <- respondents |>
  dplyr::mutate(
    center_harmonized =
      dplyr::case_when(
        year == 2014 &
          ideology_complete &
          voter_ideology ==
            "Center" ~
          1,

        year == 2014 &
          ideology_complete ~
          0,

        TRUE ~
          NA_real_
      ),

    respondent_sample_candidate_present =
      vote_valid &
      !is.na(
        voted_bjp
      ) &
      !is.na(
        bjp_candidate_present
      ) &
      bjp_candidate_present ==
        1
  )

# Reconstruct the 54 FDI definitions exactly.
fdi_meta <- tidyr::crossing(
  fdi_family =
    c(
      "total",
      "mfg",
      "services"
    ),

  fdi_scope =
    c(
      "own",
      "local"
    ),

  fdi_status =
    c(
      "all",
      "announced",
      "opened"
    ),

  fdi_form =
    c(
      "count",
      "pc100k",
      "log1p_pc100k"
    )
) |>
  dplyr::mutate(
    pooled_var =
      dplyr::case_when(
        fdi_form ==
          "count" ~
          paste0(
            "fdi_",
            fdi_family,
            "_",
            fdi_scope,
            "_",
            fdi_status,
            "_n"
          ),

        fdi_form ==
          "pc100k" ~
          paste0(
            "fdi_",
            fdi_family,
            "_",
            fdi_scope,
            "_",
            fdi_status,
            "_pc100k"
          ),

        TRUE ~
          paste0(
            "log1p_fdi_",
            fdi_family,
            "_",
            fdi_scope,
            "_",
            fdi_status,
            "_pc100k"
          )
      ),

    baseline_var =
      paste0(
        pooled_var,
        "_2009"
      ),

    definition_label =
      paste(
        fdi_scope,
        fdi_status,
        fdi_form,
        sep = " | "
      )
  )

needed_baseline_vars <- unique(
  c(
    "ac_uid",
    "bjp_vote_share_2009",
    fdi_meta$baseline_var
  )
)

missing_in_change <- setdiff(
  needed_baseline_vars,
  names(
    ac_change
  )
)

if (
  length(missing_in_change) > 0
) {
  stop(
    "ac_change is missing required baseline variables: ",
    paste(
      missing_in_change,
      collapse = ", "
    )
  )
}

baseline_payload <- setdiff(
  needed_baseline_vars,
  c(
    "ac_uid",
    names(
      respondents
    )
  )
)

if (
  length(
    baseline_payload
  ) > 0
) {
  baseline_context <- ac_change |>
    dplyr::select(
      ac_uid,
      dplyr::all_of(
        baseline_payload
      )
    ) |>
    dplyr::distinct(
      ac_uid,
      .keep_all = TRUE
    )

  respondents <- respondents |>
    dplyr::left_join(
      baseline_context,
      by = "ac_uid",
      relationship =
        "many-to-one"
    )
}

# ============================================================
# 16. SERVICES-FDI TREATMENT-SUPPORT AUDIT
# ============================================================

core_2014_triple_vars <- c(
  "voted_bjp",
  "survey_weight_norm_year",
  "center_harmonized",
  "religion_group",
  "caste_group",
  "education_harmonized",
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share",
  "state_no",
  "ac_uid",
  "pc_cluster_id",
  "district_harmonization_group_id"
)

missing_core <- setdiff(
  core_2014_triple_vars,
  names(
    respondents
  )
)

if (
  length(missing_core) > 0
) {
  stop(
    "Respondent data are missing required support-audit variables: ",
    paste(
      missing_core,
      collapse = ", "
    )
  )
}

is_complete_finite <- function(
    data,
    vars
) {
  keep <- rep(
    TRUE,
    nrow(data)
  )

  for (
    v in vars
  ) {
    x <- data[[v]]

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

services_meta <- fdi_meta |>
  dplyr::filter(
    fdi_family ==
      "services"
  )

base_2014_triple <- respondents |>
  dplyr::filter(
    year == 2014,
    respondent_sample_candidate_present,
    ideology_complete,
    !is.na(
      center_harmonized
    )
  )

services_support_rows <- purrr::map_dfr(
  seq_len(
    nrow(
      services_meta
    )
  ),
  function(i) {
    meta <- services_meta[
      i,
      ,
      drop = FALSE
    ]

    fvar <-
      meta$pooled_var[[1]]

    bvar <-
      meta$baseline_var[[1]]

    required <- c(
      core_2014_triple_vars,
      fvar,
      bvar
    )

    if (
      !all(
        required %in%
          names(
            base_2014_triple
          )
      )
    ) {
      stop(
        "Missing services support variables for ",
        meta$definition_label[[1]],
        "."
      )
    }

    d <- base_2014_triple[
      is_complete_finite(
        base_2014_triple,
        required
      ),
      ,
      drop = FALSE
    ]

    d <- d |>
      dplyr::filter(
        survey_weight_norm_year >
          0
      )

    positive <- d |>
      dplyr::filter(
        .data[[fvar]] >
          0
      )

    positive_ac <- positive |>
      dplyr::distinct(
        ac_uid,
        pc_cluster_id,
        district_harmonization_group_id,
        exposure =
          .data[[fvar]]
      )

    pc_concentration <- positive |>
      dplyr::count(
        pc_cluster_id,
        name =
          "n_positive_respondents"
      ) |>
      dplyr::arrange(
        dplyr::desc(
          n_positive_respondents
        )
      )

    top1_share <- if (
      nrow(
        pc_concentration
      ) > 0
    ) {
      max(
        pc_concentration$n_positive_respondents
      ) /
        sum(
          pc_concentration$n_positive_respondents
        )
    } else {
      NA_real_
    }

    top5_share <- if (
      nrow(
        pc_concentration
      ) > 0
    ) {
      sum(
        utils::head(
          pc_concentration$n_positive_respondents,
          5
        )
      ) /
        sum(
          pc_concentration$n_positive_respondents
        )
    } else {
      NA_real_
    }

    positive_exposure <-
      positive[[fvar]]

    positive_exposure <-
      positive_exposure[
        is.finite(
          positive_exposure
        ) &
        positive_exposure >
          0
      ]

    median_positive <- if (
      length(
        positive_exposure
      ) > 0
    ) {
      stats::median(
        positive_exposure
      )
    } else {
      NA_real_
    }

    tibble::tibble(
      fdi_scope =
        meta$fdi_scope[[1]],

      fdi_status =
        meta$fdi_status[[1]],

      fdi_form =
        meta$fdi_form[[1]],

      fdi_var =
        fvar,

      baseline_fdi_var =
        bvar,

      definition_label =
        meta$definition_label[[1]],

      n_analysis_respondents =
        nrow(d),

      n_positive_respondents =
        nrow(
          positive
        ),

      share_positive_respondents =
        nrow(
          positive
        ) /
        nrow(d),

      n_positive_centers =
        sum(
          positive$center_harmonized ==
            1,
          na.rm = TRUE
        ),

      n_positive_acs =
        dplyr::n_distinct(
          positive$ac_uid
        ),

      n_positive_pcs =
        dplyr::n_distinct(
          positive$pc_cluster_id
        ),

      n_positive_districts =
        dplyr::n_distinct(
          positive$district_harmonization_group_id
        ),

      median_positive_exposure =
        median_positive,

      p90_positive_exposure =
        if (
          length(
            positive_exposure
          ) > 0
        ) {
          stats::quantile(
            positive_exposure,
            0.90,
            names = FALSE
          )
        } else {
          NA_real_
        },

      p95_positive_exposure =
        if (
          length(
            positive_exposure
          ) > 0
        ) {
          stats::quantile(
            positive_exposure,
            0.95,
            names = FALSE
          )
        } else {
          NA_real_
        },

      max_positive_exposure =
        if (
          length(
            positive_exposure
          ) > 0
        ) {
          max(
            positive_exposure
          )
        } else {
          NA_real_
        },

      max_to_median_positive =
        if (
          is.finite(
            median_positive
          ) &&
          median_positive >
            0
        ) {
          max(
            positive_exposure
          ) /
            median_positive
        } else {
          NA_real_
        },

      share_positive_respondents_largest_pc =
        top1_share,

      share_positive_respondents_top5_pcs =
        top5_share
    )
  }
)

readr::write_csv(
  services_support_rows,
  file.path(
    audit_diagnostic_dir,
    "42_services_fdi_treatment_support_2014_triple_v2c1.csv"
  )
)

p_services_support <- ggplot2::ggplot(
  services_support_rows,
  ggplot2::aes(
    x = stats::reorder(
      definition_label,
      n_positive_pcs
    ),
    y = n_positive_pcs
  )
) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title =
      "2014 services-FDI support in the candidate-present ideology-complete V2+C1 sample",

    subtitle =
      "Number of parliamentary constituencies containing respondents with positive FDI exposure.",

    x =
      "Services-FDI definition",

    y =
      "PCs with positive exposure"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "36_services_fdi_treated_pc_support.pdf"
  ),
  p_services_support,
  width = 10,
  height = 8
)

p_services_leverage <- ggplot2::ggplot(
  services_support_rows,
  ggplot2::aes(
    x = n_positive_pcs,
    y = max_to_median_positive,
    label =
      definition_label
  )
) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::geom_text(
    check_overlap = TRUE,
    size = 2.4,
    hjust = 0
  ) +
  ggplot2::labs(
    title =
      "Services-FDI support versus exposure-tail leverage",

    subtitle =
      "Large max/median ratios combined with few treated PCs flag definitions requiring extra scrutiny.",

    x =
      "PCs with positive services-FDI exposure",

    y =
      "Maximum / median positive exposure"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  )

ggplot2::ggsave(
  file.path(
    audit_figure_dir,
    "37_services_fdi_support_vs_tail_leverage.pdf"
  ),
  p_services_leverage,
  width = 11,
  height = 7
)

# ============================================================
# 17. PREFERRED 2014 MANUFACTURING x MUSLIM CONDITIONAL-EFFECT REFITS
# ============================================================

preferred_fdi <-
  "log1p_fdi_mfg_local_all_pc100k"

preferred_fdi_baseline <-
  paste0(
    preferred_fdi,
    "_2009"
  )

preferred_demographic <-
  "muslim_share_2001_dist_proxy"

preferred_controls <- c(
  "religion_group",
  "caste_group",
  "education_harmonized",
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share"
)

preferred_common_vars <- c(
  "voted_bjp",
  preferred_fdi,
  preferred_fdi_baseline,
  preferred_demographic,
  "bjp_vote_share_2009",
  preferred_controls,
  "state_no",
  "pc_cluster_id",
  "district_harmonization_group_id",
  "survey_weight_norm_year"
)

missing_preferred <- setdiff(
  preferred_common_vars,
  names(
    respondents
  )
)

if (
  length(missing_preferred) > 0
) {
  stop(
    "Missing preferred-model variables: ",
    paste(
      missing_preferred,
      collapse = ", "
    )
  )
}

preferred_base <- respondents |>
  dplyr::filter(
    year == 2014,
    respondent_sample_candidate_present
  )

preferred_two_way_data <- preferred_base[
  is_complete_finite(
    preferred_base,
    preferred_common_vars
  ),
  ,
  drop = FALSE
] |>
  dplyr::filter(
    survey_weight_norm_year >
      0
  )

preferred_triple_data <- preferred_base |>
  dplyr::filter(
    ideology_complete,
    !is.na(
      center_harmonized
    )
  )

preferred_triple_vars <- c(
  preferred_common_vars,
  "center_harmonized"
)

preferred_triple_data <- preferred_triple_data[
  is_complete_finite(
    preferred_triple_data,
    preferred_triple_vars
  ),
  ,
  drop = FALSE
] |>
  dplyr::filter(
    survey_weight_norm_year >
      0
  )

preferred_two_way_formula <- stats::as.formula(
  paste0(
    "voted_bjp ~ ",
    preferred_fdi,
    " * ",
    preferred_demographic,
    " + ",
    preferred_fdi_baseline,
    " + bjp_vote_share_2009 + ",
    paste(
      preferred_controls,
      collapse = " + "
    ),
    " | state_no"
  )
)

preferred_triple_formula <- stats::as.formula(
  paste0(
    "voted_bjp ~ ",
    preferred_fdi,
    " * ",
    preferred_demographic,
    " * center_harmonized + ",
    preferred_fdi_baseline,
    " + bjp_vote_share_2009 + ",
    paste(
      preferred_controls,
      collapse = " + "
    ),
    " | state_no"
  )
)

preferred_two_way_fit <- fixest::feols(
  preferred_two_way_formula,
  data =
    preferred_two_way_data,
  weights =
    ~survey_weight_norm_year,
  vcov =
    ~pc_cluster_id +
    district_harmonization_group_id,
  notes = FALSE,
  warn = FALSE
)

preferred_triple_fit <- fixest::feols(
  preferred_triple_formula,
  data =
    preferred_triple_data,
  weights =
    ~survey_weight_norm_year,
  vcov =
    ~pc_cluster_id +
    district_harmonization_group_id,
  notes = FALSE,
  warn = FALSE
)

normalize_term <- function(
    x
) {
  vapply(
    strsplit(
      x,
      ":",
      fixed = TRUE
    ),
    function(parts) {
      paste(
        sort(
          gsub(
            "`",
            "",
            parts,
            fixed = TRUE
          )
        ),
        collapse = ":"
      )
    },
    character(1)
  )
}

find_interaction_term <- function(
    fit,
    vars
) {
  names_beta <- names(
    stats::coef(
      fit
    )
  )

  target <- paste(
    sort(vars),
    collapse = ":"
  )

  matches <- names_beta[
    normalize_term(
      names_beta
    ) ==
      target
  ]

  if (
    length(matches) != 1
  ) {
    stop(
      "Could not uniquely locate interaction term for: ",
      paste(
        vars,
        collapse = " x "
      )
    )
  }

  matches[[1]]
}

linear_combo <- function(
    fit,
    weights_named
) {
  beta <- stats::coef(
    fit
  )

  vc <- stats::vcov(
    fit
  )

  L <- rep(
    0,
    length(beta)
  )

  names(L) <- names(beta)

  unknown <- setdiff(
    names(
      weights_named
    ),
    names(beta)
  )

  if (
    length(unknown) > 0
  ) {
    stop(
      "Linear-combination term(s) absent from fit: ",
      paste(
        unknown,
        collapse = ", "
      )
    )
  }

  L[
    names(
      weights_named
    )
  ] <- weights_named

  estimate <- sum(
    L *
      beta
  )

  variance <- as.numeric(
    t(L) %*%
      vc %*%
      L
  )

  standard_error <- sqrt(
    pmax(
      variance,
      0
    )
  )

  z <- stats::qnorm(
    0.975
  )

  tibble::tibble(
    estimate =
      estimate,
    se =
      standard_error,
    conf_low =
      estimate -
      z *
      standard_error,
    conf_high =
      estimate +
      z *
      standard_error
  )
}

fdi_term_two <-
  preferred_fdi

fd_term_two <- find_interaction_term(
  preferred_two_way_fit,
  c(
    preferred_fdi,
    preferred_demographic
  )
)

fdi_term_triple <-
  preferred_fdi

fd_term_triple <- find_interaction_term(
  preferred_triple_fit,
  c(
    preferred_fdi,
    preferred_demographic
  )
)

fc_term_triple <- find_interaction_term(
  preferred_triple_fit,
  c(
    preferred_fdi,
    "center_harmonized"
  )
)

fdc_term_triple <- find_interaction_term(
  preferred_triple_fit,
  c(
    preferred_fdi,
    preferred_demographic,
    "center_harmonized"
  )
)

delta_fdi_two <- stats::median(
  preferred_two_way_data[[
    preferred_fdi
  ]][
    preferred_two_way_data[[
      preferred_fdi
    ]] >
      0
  ],
  na.rm = TRUE
)

delta_fdi_triple <- stats::median(
  preferred_triple_data[[
    preferred_fdi
  ]][
    preferred_triple_data[[
      preferred_fdi
    ]] >
      0
  ],
  na.rm = TRUE
)

muslim_grid <- seq(
  stats::quantile(
    preferred_triple_data[[
      preferred_demographic
    ]],
    0.05,
    na.rm = TRUE
  ),
  stats::quantile(
    preferred_triple_data[[
      preferred_demographic
    ]],
    0.95,
    na.rm = TRUE
  ),
  length.out = 101
)

preferred_two_way_conditional <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    out <- linear_combo(
      preferred_two_way_fit,
      stats::setNames(
        c(
          delta_fdi_two,
          delta_fdi_two *
            m
        ),
        c(
          fdi_term_two,
          fd_term_two
        )
      )
    )

    out |>
      dplyr::mutate(
        muslim_share =
          m,
        center_group =
          "All candidate-present voters",
        delta_fdi =
          delta_fdi_two
      )
  }
) |>
  dplyr::mutate(
    dplyr::across(
      c(
        estimate,
        se,
        conf_low,
        conf_high
      ),
      ~100 *
        .x
    )
  )

preferred_triple_conditional <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    other <- linear_combo(
      preferred_triple_fit,
      stats::setNames(
        c(
          delta_fdi_triple,
          delta_fdi_triple *
            m
        ),
        c(
          fdi_term_triple,
          fd_term_triple
        )
      )
    ) |>
      dplyr::mutate(
        center_group =
          "Other ideology-complete"
      )

    center <- linear_combo(
      preferred_triple_fit,
      stats::setNames(
        c(
          delta_fdi_triple,
          delta_fdi_triple *
            m,
          delta_fdi_triple,
          delta_fdi_triple *
            m
        ),
        c(
          fdi_term_triple,
          fd_term_triple,
          fc_term_triple,
          fdc_term_triple
        )
      )
    ) |>
      dplyr::mutate(
        center_group =
          "Harmonized Center"
      )

    dplyr::bind_rows(
      other,
      center
    ) |>
      dplyr::mutate(
        muslim_share =
          m,
        delta_fdi =
          delta_fdi_triple
      )
  }
) |>
  dplyr::mutate(
    dplyr::across(
      c(
        estimate,
        se,
        conf_low,
        conf_high
      ),
      ~100 *
        .x
    )
  )

readr::write_csv(
  preferred_two_way_conditional,
  file.path(
    audit_preferred_dir,
    "50_preferred_two_way_conditional_fdi_effect_by_muslim_share.csv"
  )
)

readr::write_csv(
  preferred_triple_conditional,
  file.path(
    audit_preferred_dir,
    "51_preferred_triple_conditional_fdi_effect_by_muslim_share_and_center.csv"
  )
)

conditional_pdf <- file.path(
  audit_figure_dir,
  "38_preferred_2014_mfg_muslim_conditional_effects.pdf"
)

grDevices::pdf(
  conditional_pdf,
  width = 9,
  height = 6.5,
  onefile = TRUE
)

p_cond_two <- ggplot2::ggplot(
  preferred_two_way_conditional,
  ggplot2::aes(
    x = muslim_share,
    y = estimate
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = conf_low,
      ymax = conf_high
    ),
    alpha = 0.18
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::scale_x_continuous(
    labels =
      scales::percent_format(
        accuracy = 1
      )
  ) +
  ggplot2::labs(
    title =
      "Preferred 2014 model: manufacturing-FDI effect by 2001 Muslim share",

    subtitle =
      paste0(
        "Effect is the predicted change from FDI = 0 to median positive FDI (Δ = ",
        sprintf(
          "%.3f",
          delta_fdi_two
        ),
        ")."
      ),

    x =
      "2001 Muslim population share",

    y =
      "Change in BJP-voting probability (percentage points)"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

print(
  p_cond_two
)

p_cond_triple <- ggplot2::ggplot(
  preferred_triple_conditional,
  ggplot2::aes(
    x = muslim_share,
    y = estimate,
    group =
      center_group,
    linetype =
      center_group
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin =
        conf_low,
      ymax =
        conf_high,
      group =
        center_group
    ),
    alpha = 0.12,
    inherit.aes = TRUE
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::scale_x_continuous(
    labels =
      scales::percent_format(
        accuracy = 1
      )
  ) +
  ggplot2::labs(
    title =
      "Preferred 2014 triple model: manufacturing-FDI effect by Muslim share and Center status",

    subtitle =
      paste0(
        "Effect is the predicted change from FDI = 0 to median positive FDI (Δ = ",
        sprintf(
          "%.3f",
          delta_fdi_triple
        ),
        ")."
      ),

    x =
      "2001 Muslim population share",

    y =
      "Change in BJP-voting probability (percentage points)",

    linetype =
      "Ideology group"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

print(
  p_cond_triple
)

grDevices::dev.off()

# ============================================================
# 18. COMPACT "WHAT TO READ FIRST" SUMMARY
# ============================================================

headline_summary <- curve_summary |>
  dplyr::select(
    design_id,
    moderator_domain,
    fdi_family,
    interaction_order,
    n_planned,
    n_fit,
    median_contrast_pp,
    q25_contrast_pp,
    q75_contrast_pp,
    share_positive,
    share_ci_positive,
    share_ci_negative,
    median_nobs,
    median_n_pcs
  ) |>
  dplyr::arrange(
    design_id,
    moderator_domain,
    interaction_order,
    dplyr::desc(
      median_contrast_pp
    )
  )

readr::write_csv(
  headline_summary,
  file.path(
    audit_summary_dir,
    "00_READ_FIRST_headline_curve_summary.csv"
  )
)

readr::write_lines(
  c(
    paste0(
      "Respondent post-estimation audit revision: ",
      AUDIT_REVISION
    ),
    paste0(
      "Frozen primary model revision: ",
      PRIMARY_MODEL_REVISION
    ),
    "",
    paste0(
      "Primary specifications loaded: ",
      format(
        nrow(primary_results),
        big.mark = ","
      )
    ),
    paste0(
      "Successful fits: ",
      format(
        sum(
          primary_results$fit_ok,
          na.rm = TRUE
        ),
        big.mark = ","
      )
    ),
    paste0(
      "Failed/collinear fits: ",
      format(
        sum(
          !primary_results$fit_ok,
          na.rm = TRUE
        ),
        big.mark = ","
      )
    ),
    "",
    "Read these first:",
    "  summaries/00_READ_FIRST_headline_curve_summary.csv",
    "  figures/31_curve_summary_median_iqr.pdf",
    "  figures/32_2014_moderator_family_decomposition.pdf",
    "  preferred_models/20_preferred_2014_mfg_muslim_models.csv",
    "  preferred_models/21_preferred_muslim_moderator_all_18_mfg_fdi_definitions.csv",
    "  diagnostics/41_services_triple_extremeness_by_fdi_definition.csv",
    "  diagnostics/42_services_fdi_treatment_support_2014_triple_v2c1.csv",
    "  figures/38_preferred_2014_mfg_muslim_conditional_effects.pdf"
  ),
  file.path(
    audit_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Respondent post-estimation audit COMPLETE."
)
message(
  "Output directory: ",
  audit_root
)
message(
  "Primary rows audited: ",
  format(
    nrow(primary_results),
    big.mark = ","
  )
)
message(
  "Successful primary fits: ",
  format(
    sum(
      primary_results$fit_ok,
      na.rm = TRUE
    ),
    big.mark = ","
  )
)
message(
  "Failed/collinear primary fits: ",
  format(
    sum(
      !primary_results$fit_ok,
      na.rm = TRUE
    ),
    big.mark = ","
  )
)
message(
  "Start with: ",
  file.path(
    audit_root,
    "README_FIRST.txt"
  )
)

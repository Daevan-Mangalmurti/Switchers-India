# ============================================================
# 24_plot_s02_s03_marginal_effects_v1_0.R
# Revision: 2026-08-20-v1.0
#
# Marginal-effects plots for:
#   S02 = preferred dynamic centrist-only level model
#   S03 = pooled Center-vs-noncenter heterogeneity model
#
# Plots:
#   1. S02: marginal effect of current logged FDI across Muslim share
#   2. S03: same marginal effect for Center vs non-Center voters
#   3. S03: Center minus non-Center marginal-effect contrast
#
# IMPORTANT:
# Because these are linear mixed probability models with NO random slopes,
# the marginal effect of current FDI is a linear combination of fixed-effect
# coefficients. The AC random intercept shifts predicted levels but does not
# alter the marginal FDI slope.
#
# Uncertainty is calculated from the full fixed-effect variance-covariance
# matrix using the delta method. The curves therefore incorporate covariance
# among the constituent interaction coefficients.
#
# For readability, plots show the effect of a ONE-SD increase in current
# logged FDI, expressed in percentage points of BJP-vote probability.
# The same FDI SD, calculated from unique ACs in the pooled S03 sample,
# is used for all plots so magnitudes are directly comparable.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages()
paths <- build_project_paths(project_root)

REV <- "2026-08-20-v1.0"

model_path <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_voter_specification_table",
  "2026-08-20-v1.0",
  "models",
  "voter_specification_models.rds"
)

if (!file.exists(model_path)) {
  stop("Model RDS not found: ", model_path)
}

mods <- readRDS(model_path)

if (!all(c("S02", "S03") %in% names(mods))) {
  stop(
    "Expected S02 and S03 in model RDS. Found: ",
    paste(names(mods), collapse = ", ")
  )
}

m02 <- mods[["S02"]]
m03 <- mods[["S03"]]

out_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_voter_specification_table",
  "2026-08-20-v1.0"
)

out_fig <- file.path(out_root, "figures", "marginal_effects")
out_data <- file.path(out_root, "data")

dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Helpers to resolve interaction terms independent of term order
# ------------------------------------------------------------

term_components <- function(term) {
  sort(strsplit(term, ":", fixed = TRUE)[[1]])
}

find_term <- function(fit, wanted) {
  wanted <- sort(wanted)
  nms <- names(lme4::fixef(fit))

  hit <- nms[
    vapply(
      nms,
      function(x) identical(term_components(x), wanted),
      logical(1)
    )
  ]

  if (length(hit) != 1) {
    stop(
      "Could not uniquely resolve coefficient containing: ",
      paste(wanted, collapse = " : "),
      ". Matches: ",
      paste(hit, collapse = ", ")
    )
  }

  hit[[1]]
}

lincom_at <- function(fit, weights) {
  b <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))

  L <- setNames(rep(0, length(b)), names(b))

  absent <- setdiff(names(weights), names(L))
  if (length(absent)) {
    stop(
      "Contrast references absent coefficient(s): ",
      paste(absent, collapse = ", ")
    )
  }

  L[names(weights)] <- weights

  estimate <- sum(L * b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))

  tibble::tibble(
    estimate = estimate,
    se = se,
    conf_low = estimate - stats::qnorm(.975) * se,
    conf_high = estimate + stats::qnorm(.975) * se
  )
}

# ------------------------------------------------------------
# 2. Common plotting range and effect scale
# ------------------------------------------------------------

mf03 <- model.frame(m03)

needed_mf03 <- c("muslim_pp", "fdi_current", "ac_random")
missing_mf03 <- setdiff(needed_mf03, names(mf03))

if (length(missing_mf03)) {
  stop(
    "S03 model frame is missing: ",
    paste(missing_mf03, collapse = ", ")
  )
}

ac_support <- mf03 |>
  dplyr::transmute(
    ac_random = as.character(ac_random),
    muslim_pp = as.numeric(muslim_pp),
    fdi_current = as.numeric(fdi_current)
  ) |>
  dplyr::distinct(ac_random, .keep_all = TRUE)

# Avoid emphasizing extrapolation into the extreme tails.
muslim_limits <- stats::quantile(
  ac_support$muslim_pp,
  probs = c(.05, .95),
  na.rm = TRUE,
  names = FALSE
)

muslim_grid <- seq(
  muslim_limits[[1]],
  muslim_limits[[2]],
  length.out = 101
)

# A common one-SD change makes S02 and S03 directly comparable.
FDI_STEP <- stats::sd(
  ac_support$fdi_current,
  na.rm = TRUE
)

if (!is.finite(FDI_STEP) || FDI_STEP <= 0) {
  stop("Could not calculate a positive pooled AC-level SD for fdi_current.")
}

message("Common FDI effect step (1 pooled AC-level SD): ", signif(FDI_STEP, 5))
message(
  "Muslim-share plotting range (5th-95th percentile): ",
  signif(muslim_limits[[1]], 4),
  " to ",
  signif(muslim_limits[[2]], 4),
  " percentage points"
)

# Conversion from probability units to percentage points for a 1-SD FDI move.
SCALE_TO_PP <- 100 * FDI_STEP

# ------------------------------------------------------------
# 3. S02 marginal effect
#
# S02:
#   y = ... + b_F*F1 + b_MF*(M*F1) + ...
#
# Therefore:
#   dY/dF1 | M = b_F + b_MF*M
#
# Baseline FDI and M*baseline FDI are held constant. They matter for
# predicted levels, but not for this derivative with respect to current FDI.
# ------------------------------------------------------------

s02_f <- find_term(m02, "fdi_current")
s02_mf <- find_term(m02, c("muslim_pp", "fdi_current"))

s02_me <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    x <- lincom_at(
      m02,
      setNames(
        c(1, m),
        c(s02_f, s02_mf)
      )
    )

    x |>
      dplyr::mutate(
        muslim_pp = m,
        effect_pp = estimate * SCALE_TO_PP,
        se_pp = se * SCALE_TO_PP,
        conf_low_pp = conf_low * SCALE_TO_PP,
        conf_high_pp = conf_high * SCALE_TO_PP
      )
  }
)

readr::write_csv(
  s02_me,
  file.path(out_data, "08_s02_marginal_effect_current_fdi.csv")
)

p02 <- ggplot2::ggplot(
  s02_me,
  ggplot2::aes(
    x = muslim_pp,
    y = effect_pp
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp
    ),
    alpha = .18
  ) +
  ggplot2::geom_line(
    linewidth = .8
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = .45
  ) +
  ggplot2::labs(
    title = "Marginal effect of current manufacturing FDI among Center voters",
    subtitle = paste0(
      "S02: effect of a 1-SD increase in 2009-14 logged FDI; ",
      "95% model-based confidence interval"
    ),
    x = "Muslim population share, 2001 (percentage points)",
    y = "Change in BJP-vote probability (percentage points)",
    caption = paste0(
      "Model conditions on 2004-09 FDI and its interaction with Muslim share, ",
      "state fixed effects, constituency controls, voter controls, and an AC random intercept."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

# ------------------------------------------------------------
# 4. S03 marginal effects by Center status
#
# S03:
# y = ... +
#     b_F*F +
#     b_MF*(M*F) +
#     b_FC*(F*C) +
#     b_MFC*(M*F*C) + ...
#
# Noncenter:
#   dY/dF = b_F + b_MF*M
#
# Center:
#   dY/dF = b_F + b_MF*M + b_FC + b_MFC*M
#
# Center - Noncenter:
#   difference = b_FC + b_MFC*M
#
# Notice: the three-way coefficient alone is the DIFFERENCE IN THE
# FDI-by-Muslim interaction slope. The group difference in the marginal
# effect of FDI at a particular Muslim share also includes b_FC.
# ------------------------------------------------------------

s03_f <- find_term(m03, "fdi_current")
s03_mf <- find_term(m03, c("fdi_current", "muslim_pp"))
s03_fc <- find_term(m03, c("fdi_current", "center_binary"))
s03_mfc <- find_term(
  m03,
  c("fdi_current", "muslim_pp", "center_binary")
)

s03_noncenter <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    lincom_at(
      m03,
      setNames(
        c(1, m),
        c(s03_f, s03_mf)
      )
    ) |>
      dplyr::mutate(
        muslim_pp = m,
        voter_group = "Non-Center"
      )
  }
)

s03_center <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    lincom_at(
      m03,
      setNames(
        c(1, m, 1, m),
        c(s03_f, s03_mf, s03_fc, s03_mfc)
      )
    ) |>
      dplyr::mutate(
        muslim_pp = m,
        voter_group = "Center"
      )
  }
)

s03_me <- dplyr::bind_rows(
  s03_noncenter,
  s03_center
) |>
  dplyr::mutate(
    voter_group = factor(
      voter_group,
      levels = c("Non-Center", "Center")
    ),
    effect_pp = estimate * SCALE_TO_PP,
    se_pp = se * SCALE_TO_PP,
    conf_low_pp = conf_low * SCALE_TO_PP,
    conf_high_pp = conf_high * SCALE_TO_PP
  )

readr::write_csv(
  s03_me,
  file.path(out_data, "09_s03_marginal_effect_current_fdi_by_center.csv")
)

p03 <- ggplot2::ggplot(
  s03_me,
  ggplot2::aes(
    x = muslim_pp,
    y = effect_pp,
    linetype = voter_group,
    group = voter_group
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp,
      group = voter_group
    ),
    alpha = .10,
    linetype = 0
  ) +
  ggplot2::geom_line(
    linewidth = .85
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = .45
  ) +
  ggplot2::labs(
    title = "Marginal effect of current manufacturing FDI by voter ideology",
    subtitle = paste0(
      "S03: Center versus non-Center; effect of a 1-SD increase in 2009-14 logged FDI"
    ),
    x = "Muslim population share, 2001 (percentage points)",
    y = "Change in BJP-vote probability (percentage points)",
    linetype = NULL,
    caption = paste0(
      "Shaded regions are 95% model-based confidence intervals. ",
      "The pooled model includes the full hierarchical FDI x Muslim share x Center interaction."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.caption = ggplot2::element_text(hjust = 0)
  )

# ------------------------------------------------------------
# 5. S03 Center-minus-noncenter contrast
#
# This is the direct group-difference plot:
#   [dY/dF | Center] - [dY/dF | Noncenter]
# = b_FC + b_MFC*M
# ------------------------------------------------------------

s03_difference <- purrr::map_dfr(
  muslim_grid,
  function(m) {
    lincom_at(
      m03,
      setNames(
        c(1, m),
        c(s03_fc, s03_mfc)
      )
    ) |>
      dplyr::mutate(
        muslim_pp = m,
        difference_pp = estimate * SCALE_TO_PP,
        se_pp = se * SCALE_TO_PP,
        conf_low_pp = conf_low * SCALE_TO_PP,
        conf_high_pp = conf_high * SCALE_TO_PP
      )
  }
)

readr::write_csv(
  s03_difference,
  file.path(out_data, "10_s03_center_minus_noncenter_marginal_effect.csv")
)

p03diff <- ggplot2::ggplot(
  s03_difference,
  ggplot2::aes(
    x = muslim_pp,
    y = difference_pp
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp
    ),
    alpha = .18
  ) +
  ggplot2::geom_line(
    linewidth = .8
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = .45
  ) +
  ggplot2::labs(
    title = "Center minus non-Center difference in the marginal effect of FDI",
    subtitle = "S03: direct heterogeneity contrast with 95% model-based confidence interval",
    x = "Muslim population share, 2001 (percentage points)",
    y = "Difference in FDI marginal effect (percentage points)",
    caption = paste0(
      "Positive values mean a 1-SD increase in current FDI has a more positive ",
      "association with BJP voting among Center than non-Center voters."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

# ------------------------------------------------------------
# 6. Save
# ------------------------------------------------------------

save_plot <- function(plot, stem, width = 8, height = 5.7) {
  ggplot2::ggsave(
    file.path(out_fig, paste0(stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 400
  )

  ggplot2::ggsave(
    file.path(out_fig, paste0(stem, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
}

save_plot(
  p02,
  "01_s02_current_fdi_marginal_effect"
)

save_plot(
  p03,
  "02_s03_current_fdi_marginal_effect_center_vs_noncenter"
)

save_plot(
  p03diff,
  "03_s03_center_minus_noncenter_marginal_effect_difference"
)

# ------------------------------------------------------------
# 7. Reproducibility metadata
# ------------------------------------------------------------

metadata <- tibble::tibble(
  revision = REV,
  model_source = model_path,
  fdi_effect_step_log_units = FDI_STEP,
  fdi_effect_step_definition =
    "1 pooled S03 unique-AC SD of current logged manufacturing FDI per 100k",
  muslim_plot_min_pp = muslim_limits[[1]],
  muslim_plot_max_pp = muslim_limits[[2]],
  muslim_plot_range_definition =
    "5th to 95th percentile of Muslim share across unique ACs in the S03 model sample",
  ci_level = 0.95,
  ci_method =
    "Delta method using fixed-effect variance-covariance matrix; normal approximation"
)

readr::write_csv(
  metadata,
  file.path(out_data, "11_s02_s03_marginal_effect_plot_metadata.csv")
)

message("")
message("============================================================")
message("S02 / S03 MARGINAL-EFFECT PLOTS COMPLETE")
message("============================================================")
message("Common one-SD FDI step: ", signif(FDI_STEP, 5))
message(
  "Muslim range: ",
  signif(muslim_limits[[1]], 4),
  " to ",
  signif(muslim_limits[[2]], 4),
  " percentage points"
)
message("Figures: ", out_fig)
message("Data: ", out_data)
message("============================================================")

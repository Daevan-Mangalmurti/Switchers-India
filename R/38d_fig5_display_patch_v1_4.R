suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(tibble)
})

# =============================================================================
# R38D v1.4
# Figure 5 display-only revision
#
# PURPOSE
#   Add, to all four Figure 5 alternatives:
#     - point-estimate line
#     - 90% CI
#     - 95% CI
#     - zero-effect line
#     - common legend/key
#
# IMPORTANT
#   This script DOES NOT fit or refit any statistical model.
#   It reads only already-completed curve/support display sources.
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

source_r38d_dir <- file.path(
  project_root,
  "outputs",
  "r38d_magnitude_support_power_fig5_audit_v1_3"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "r38d_fig5_display_patch_v1_4"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

curve_path <- file.path(
  project_root,
  "outputs",
  "manufacturing_marginal_effects_v1_0",
  "01_primary_muslim_effect_1pp_grid.csv"
)

support_summary_path <- file.path(
  source_r38d_dir,
  "01_fig5_support_summary.csv"
)

hist_path <- file.path(
  source_r38d_dir,
  "05_histogram_bin_counts.csv"
)

required_files <- c(
  curve_path,
  support_summary_path,
  hist_path
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing required frozen/display source(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

# =============================================================================
# 1. Read the already-completed Figure 5 sources
# =============================================================================

curve <- read_csv(
  curve_path,
  show_col_types = FALSE
) |>
  filter(functional_form == "Raw")

support_lines <- read_csv(
  support_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    zero_label = paste0(
      round(100 * zero_share, 1),
      "% of ACs at zero FDI"
    )
  )

hist_data <- read_csv(
  hist_path,
  show_col_types = FALSE
)

hist_all <- hist_data |>
  filter(!positive_only)

hist_positive <- hist_data |>
  filter(positive_only)

required_curve_cols <- c(
  "level",
  "outcome_label",
  "current_fdi_raw",
  "effect_pp",
  "conf_low_pp",
  "conf_high_pp"
)

missing_curve_cols <- setdiff(
  required_curve_cols,
  names(curve)
)

if (length(missing_curve_cols) > 0L) {
  stop(
    "Required curve column(s) missing: ",
    paste(missing_curve_cols, collapse = ", ")
  )
}

# =============================================================================
# 2. Add the 90% CI without re-estimating any model
#
# Preferred route:
#   Use a stored pointwise SE if the completed R29 curve contains one.
#
# Fallback:
#   If R29 persisted only its symmetric 95% interval, recover the display SE
#   from that frozen interval and use the corresponding normal critical value.
#   The fallback is explicitly recorded below for provenance.
# =============================================================================

se_candidates <- intersect(
  c(
    "std_error_pp",
    "standard_error_pp",
    "se_pp",
    "std.error_pp"
  ),
  names(curve)
)

if (length(se_candidates) > 1L) {
  stop(
    "Multiple candidate SE columns found: ",
    paste(se_candidates, collapse = ", "),
    ". Resolve explicitly before final promotion."
  )
}

if (length(se_candidates) == 1L) {

  se_col <- se_candidates[[1]]

  curve$display_se_pp <- as.numeric(curve[[se_col]])

  ci90_method <- paste0(
    "90% CI calculated from frozen pointwise SE column: ",
    se_col
  )

} else {

  # Confirm that the persisted 95% interval is symmetric around the
  # persisted point estimate before doing any reconstruction.
  midpoint_error <- max(
    abs(
      curve$effect_pp -
        (curve$conf_low_pp + curve$conf_high_pp) / 2
    ),
    na.rm = TRUE
  )

  if (
    !is.finite(midpoint_error) ||
    midpoint_error > 1e-7
  ) {
    stop(
      paste0(
        "The frozen 95% CI is not symmetric around the point estimate. ",
        "Do not reconstruct a 90% interval from it. ",
        "Return to the persisted R29 SE/inference source instead."
      )
    )
  }

  curve <- curve |>
    mutate(
      display_se_pp =
        (conf_high_pp - conf_low_pp) /
        (2 * qnorm(.975))
    )

  ci90_method <- paste0(
    "90% CI reconstructed from the symmetric frozen 95% CI ",
    "using the normal critical-value relationship"
  )
}

if (
  any(!is.finite(curve$display_se_pp)) ||
  any(curve$display_se_pp < 0)
) {
  stop("Invalid display SE encountered.")
}

curve <- curve |>
  mutate(
    conf90_low_pp =
      effect_pp - qnorm(.95) * display_se_pp,
    conf90_high_pp =
      effect_pp + qnorm(.95) * display_se_pp
  )

# 90% CI must nest inside the 95% CI.
nesting_bad <- curve |>
  filter(
    conf90_low_pp < conf_low_pp - 1e-8 |
      conf90_high_pp > conf_high_pp + 1e-8
  )

if (nrow(nesting_bad) > 0L) {
  stop("90% CI failed nesting check against frozen 95% CI.")
}

write_csv(
  curve |>
    select(
      level,
      outcome_label,
      functional_form,
      current_fdi_raw,
      effect_pp,
      conf90_low_pp,
      conf90_high_pp,
      conf_low_pp,
      conf_high_pp,
      display_se_pp
    ),
  file.path(
    output_dir,
    "00_figure5_curve_with_90_95_ci.csv"
  )
)

writeLines(
  c(
    "R38D Figure 5 display-only CI provenance",
    "",
    ci90_method,
    "",
    "No statistical model was fitted or refitted.",
    "The point estimate and 95% CI are inherited unchanged from the completed R29 curve.",
    "The 90% CI is a display-layer addition.",
    "All four alternatives use the same inferential layers."
  ),
  file.path(
    output_dir,
    "00_figure5_ci_provenance.txt"
  )
)

# =============================================================================
# 3. Common Figure 5 visual grammar
# =============================================================================

base_curve_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      size = 8.5
    ),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank()
  )

# These scales deliberately use the same labels for every A/B/C/D version.
effect_key <- list(
  scale_colour_manual(
    values = c(
      "Point estimate" = "black"
    ),
    breaks = "Point estimate",
    name = NULL
  ),
  scale_fill_manual(
    values = c(
      "90% CI" = "grey55",
      "95% CI" = "grey80"
    ),
    breaks = c(
      "90% CI",
      "95% CI"
    ),
    name = NULL
  ),
  scale_linetype_manual(
    values = c(
      "Zero effect" = "dashed"
    ),
    breaks = "Zero effect",
    name = NULL
  ),
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(linewidth = .9)
    ),
    fill = guide_legend(
      order = 2
    ),
    linetype = guide_legend(
      order = 3,
      override.aes = list(
        colour = "grey35",
        linewidth = .5
      )
    )
  )
)

zero_standard <- curve |>
  distinct(outcome_label) |>
  mutate(
    zero_y = 0,
    key = "Zero effect"
  )

# =============================================================================
# 4A. Option A
#     Curve + p90/p95 support markers
# =============================================================================

option_a <- ggplot(
  curve,
  aes(
    x = current_fdi_raw,
    y = effect_pp
  )
) +
  # Zero-effect reference
  geom_hline(
    data = zero_standard,
    aes(
      yintercept = zero_y,
      linetype = key
    ),
    inherit.aes = FALSE,
    colour = "grey35",
    linewidth = .45
  ) +

  # Existing support markers: unchanged
  geom_vline(
    data = support_lines,
    aes(xintercept = overall_p90),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = .4
  ) +
  geom_vline(
    data = support_lines,
    aes(xintercept = overall_p95),
    inherit.aes = FALSE,
    linetype = "dotdash",
    linewidth = .4
  ) +

  # Outer 95% interval
  geom_ribbon(
    aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp,
      fill = "95% CI"
    ),
    alpha = .20,
    linewidth = 0
  ) +

  # Inner 90% interval
  geom_ribbon(
    aes(
      ymin = conf90_low_pp,
      ymax = conf90_high_pp,
      fill = "90% CI"
    ),
    alpha = .38,
    linewidth = 0
  ) +

  # Point estimate
  geom_line(
    aes(colour = "Point estimate"),
    linewidth = .9
  ) +

  facet_wrap(
    vars(outcome_label),
    nrow = 1
  ) +

  labs(
    title =
      "Option A: marginal effects with p90/p95 support markers",
    x =
      "Manufacturing FDI projects per 100,000 residents, 2009-2014",
    y =
      "Effect of +1 pp Muslim population share\non BJP support (percentage points)",
    caption =
      paste0(
        "Dotted and dot-dash vertical lines mark the 90th and 95th ",
        "percentiles of current Manufacturing FDI."
      )
  ) +

  base_curve_theme +
  effect_key

# =============================================================================
# 4B/C. Histogram overlays
# =============================================================================

make_overlay <- function(hist_data_i, positive_only) {

  # Preserve the original R38D y-range logic using the OUTER 95% CI.
  effect_min <- min(
    c(curve$conf_low_pp, 0),
    na.rm = TRUE
  )

  effect_max <- max(
    c(curve$conf_high_pp, 0),
    na.rm = TRUE
  )

  span <- effect_max - effect_min

  effect_min <- effect_min - .06 * span
  effect_max <- effect_max + .06 * span

  count_max <-
    max(hist_data_i$count, na.rm = TRUE) * 1.08

  effect_to_count <- function(x) {
    (x - effect_min) /
      (effect_max - effect_min) *
      count_max
  }

  count_to_effect <- function(y) {
    effect_min +
      y / count_max *
      (effect_max - effect_min)
  }

  curve_overlay <- curve |>
    mutate(
      plot_effect =
        effect_to_count(effect_pp),
      plot_95_low =
        effect_to_count(conf_low_pp),
      plot_95_high =
        effect_to_count(conf_high_pp),
      plot_90_low =
        effect_to_count(conf90_low_pp),
      plot_90_high =
        effect_to_count(conf90_high_pp)
    )

  zero_overlay <- curve |>
    distinct(outcome_label) |>
    mutate(
      zero_y = effect_to_count(0),
      key = "Zero effect"
    )

  hist_label <- if (positive_only) {
    "Unique positive-FDI ACs per bin"
  } else {
    "Unique ACs per FDI bin"
  }

  subtitle <- if (positive_only) {
    "Positive-FDI AC support; zero-FDI share shown in each panel"
  } else {
    "Full AC support, including the point mass at zero"
  }

  ggplot() +

    geom_rect(
      data = hist_data_i,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = count
      ),
      fill = "grey92",
      colour = "grey78",
      alpha = .75,
      linewidth = .25
    ) +

    geom_hline(
      data = zero_overlay,
      aes(
        yintercept = zero_y,
        linetype = key
      ),
      inherit.aes = FALSE,
      colour = "grey35",
      linewidth = .45
    ) +

    # 95% first: wider/lighter
    geom_ribbon(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        ymin = plot_95_low,
        ymax = plot_95_high,
        fill = "95% CI"
      ),
      alpha = .20,
      linewidth = 0
    ) +

    # 90% second: narrower/darker
    geom_ribbon(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        ymin = plot_90_low,
        ymax = plot_90_high,
        fill = "90% CI"
      ),
      alpha = .38,
      linewidth = 0
    ) +

    geom_line(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        y = plot_effect,
        colour = "Point estimate"
      ),
      linewidth = .9
    ) +

    geom_text(
      data = support_lines,
      aes(
        x = Inf,
        y = Inf,
        label = zero_label
      ),
      inherit.aes = FALSE,
      hjust = 1.05,
      vjust = 1.4,
      size = 3
    ) +

    facet_wrap(
      vars(outcome_label),
      nrow = 1
    ) +

    scale_y_continuous(
      name = hist_label,
      limits = c(0, count_max),
      expand = expansion(mult = c(0, 0)),
      sec.axis = sec_axis(
        trans = ~ count_to_effect(.),
        name =
          paste0(
            "Effect of +1 pp Muslim population share\n",
            "on BJP support (percentage points)"
          )
      )
    ) +

    labs(
      title = if (positive_only) {
        "Option C: marginal effects with positive-FDI histogram overlay"
      } else {
        "Option B: marginal effects with full-support histogram overlay"
      },
      subtitle = subtitle,
      x =
        "Manufacturing FDI projects per 100,000 residents, 2009-2014",
      caption =
        paste0(
          "The dual axes are linked only by a linear display transformation; ",
          "the relative vertical height of bars and the marginal-effect curve ",
          "has no substantive meaning."
        )
    ) +

    base_curve_theme +
    effect_key
}

option_b <- make_overlay(
  hist_all,
  FALSE
)

option_c <- make_overlay(
  hist_positive,
  TRUE
)

# =============================================================================
# 4D. Aligned curve + full-support histogram
# =============================================================================

make_stacked_panel <- function(level_value) {

  curve_i <- curve |>
    filter(level == level_value)

  hist_i <- hist_all |>
    filter(level == level_value)

  summary_i <- support_lines |>
    filter(level == level_value)

  zero_i <- tibble(
    zero_y = 0,
    key = "Zero effect"
  )

  top <- ggplot(
    curve_i,
    aes(
      x = current_fdi_raw,
      y = effect_pp
    )
  ) +

    geom_hline(
      data = zero_i,
      aes(
        yintercept = zero_y,
        linetype = key
      ),
      inherit.aes = FALSE,
      colour = "grey35",
      linewidth = .45
    ) +

    geom_ribbon(
      aes(
        ymin = conf_low_pp,
        ymax = conf_high_pp,
        fill = "95% CI"
      ),
      alpha = .20,
      linewidth = 0
    ) +

    geom_ribbon(
      aes(
        ymin = conf90_low_pp,
        ymax = conf90_high_pp,
        fill = "90% CI"
      ),
      alpha = .38,
      linewidth = 0
    ) +

    geom_line(
      aes(colour = "Point estimate"),
      linewidth = .9
    ) +

    labs(
      title = unique(curve_i$outcome_label),
      x = NULL,
      y =
        "Marginal effect\n(percentage points)"
    ) +

    base_curve_theme +
    effect_key +

    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 11.5)
    )

  # Use the already-persisted R38D histogram bins rather than rebuilding
  # support from any model/sample object.
  bottom <- ggplot() +

    geom_rect(
      data = hist_i,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = count
      ),
      fill = "grey85",
      colour = "grey70",
      linewidth = .25
    ) +

    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = summary_i$zero_label,
      hjust = 1.05,
      vjust = 1.3,
      size = 3
    ) +

    labs(
      x =
        "Manufacturing FDI projects per 100,000 residents, 2009-2014",
      y = "Unique ACs"
    ) +

    theme_minimal(base_size = 10) +

    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  top / bottom +
    plot_layout(
      heights = c(3, 1.25)
    )
}

option_d <- (
  make_stacked_panel("AC") |
    make_stacked_panel("Voter")
) +
  plot_layout(
    guides = "collect"
  ) +
  plot_annotation(
    title =
      "Option D: marginal effects with aligned full-support histograms",
    subtitle =
      paste0(
        "Separate y-axes avoid a dual-axis overlay while retaining ",
        "the complete FDI support distribution."
      )
  )

option_d <- option_d &
  theme(
    legend.position = "bottom"
  )

# =============================================================================
# 5. Save all four final display candidates
# =============================================================================

save_pair <- function(
  plot_object,
  stem,
  width = 10.8,
  height = 5.3
) {

  ggsave(
    file.path(
      output_dir,
      paste0(stem, ".png")
    ),
    plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path(
      output_dir,
      paste0(stem, ".pdf")
    ),
    plot_object,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

save_pair(
  option_a,
  "06_fig5_option_A_p90_p95_FINAL"
)

save_pair(
  option_b,
  "07_fig5_option_B_full_hist_overlay_FINAL"
)

save_pair(
  option_c,
  "08_fig5_option_C_positive_hist_overlay_FINAL"
)

save_pair(
  option_d,
  "09_fig5_option_D_aligned_hist_below_FINAL",
  width = 11.2,
  height = 6.5
)

# =============================================================================
# 6. Explicit audit
# =============================================================================

audit <- tibble(
  requirement = c(
    "Point-estimate line",
    "90% CI",
    "95% CI",
    "Zero-effect line",
    "Legend/key",
    "Primary model re-estimated",
    "Model object read"
  ),
  option_A = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  ),
  option_B = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  ),
  option_C = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  ),
  option_D = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  )
)

write_csv(
  audit,
  file.path(
    output_dir,
    "10_fig5_display_requirement_audit.csv"
  )
)

cat("\n===== R38D FIGURE 5 v1.4 DISPLAY PATCH =====\n")
cat("CI90_METHOD=", ci90_method, "\n", sep = "")
cat("POINT_ESTIMATE_LINE=TRUE\n")
cat("CI_90=TRUE\n")
cat("CI_95=TRUE\n")
cat("ZERO_EFFECT_LINE=TRUE\n")
cat("LEGEND_KEY=TRUE\n")
cat("MODEL_REESTIMATION_RUN=FALSE\n")
cat("MODEL_OBJECTS_READ=FALSE\n")
cat("OUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38D_FIG5_DISPLAY_PATCH_V1_4_COMPLETE\n")

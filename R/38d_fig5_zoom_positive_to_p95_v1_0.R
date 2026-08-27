suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(tibble)
})

# =============================================================================
# R38D Figure 5 zoomed display-only patch
#
# PURPOSE
#   Build zoomed versions of Figure 5 options A/B/C/D that display only:
#     FDI > 0 and FDI <= panel-specific 95th percentile
#
# IMPORTANT
#   This is a display-only script.
#   No models are fit or refit.
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

curve_path <- file.path(
  project_root,
  "outputs",
  "r38d_fig5_display_patch_v1_4",
  "00_figure5_curve_with_90_95_ci.csv"
)

support_summary_path <- file.path(
  project_root,
  "outputs",
  "r38d_magnitude_support_power_fig5_audit_v1_3",
  "01_fig5_support_summary.csv"
)

hist_path <- file.path(
  project_root,
  "outputs",
  "r38d_magnitude_support_power_fig5_audit_v1_3",
  "05_histogram_bin_counts.csv"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "r38d_fig5_zoom_positive_to_p95_v1_0"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

required_files <- c(
  curve_path,
  support_summary_path,
  hist_path
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing required display source(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

curve <- read_csv(
  curve_path,
  show_col_types = FALSE
)

support_lines <- read_csv(
  support_summary_path,
  show_col_types = FALSE
) |>
  mutate(
    zero_label = paste0(
      round(100 * zero_share, 1),
      "% of ACs at zero FDI"
    )
  ) |>
  transmute(
    level,
    outcome_label,
    overall_p90,
    overall_p95,
    zero_share,
    zero_label
  )

hist_data <- read_csv(
  hist_path,
  show_col_types = FALSE
)

required_curve_cols <- c(
  "level",
  "outcome_label",
  "current_fdi_raw",
  "effect_pp",
  "conf90_low_pp",
  "conf90_high_pp",
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

zoom_bounds <- support_lines |>
  transmute(
    level,
    x_left = 0,
    x_right = overall_p95
  )

curve_zoom <- curve |>
  left_join(
    zoom_bounds,
    by = "level"
  ) |>
  filter(
    current_fdi_raw > x_left,
    current_fdi_raw <= x_right
  )

hist_zoom <- hist_data |>
  left_join(
    zoom_bounds,
    by = "level"
  ) |>
  filter(
    xmax > x_left,
    xmin < x_right
  ) |>
  mutate(
    xmin_plot = pmax(xmin, x_left),
    xmax_plot = pmin(xmax, x_right)
  ) |>
  filter(
    xmax_plot > xmin_plot
  )

hist_all_zoom <- hist_zoom |>
  filter(!positive_only)

hist_positive_zoom <- hist_zoom |>
  filter(positive_only)

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
      override.aes = list(linewidth = 0.9)
    ),
    fill = guide_legend(order = 2),
    linetype = guide_legend(
      order = 3,
      override.aes = list(
        colour = "grey35",
        linewidth = 0.5
      )
    )
  )
)

zero_standard <- curve_zoom |>
  distinct(
    level,
    outcome_label
  ) |>
  mutate(
    zero_y = 0,
    key = "Zero effect"
  )

# =============================================================================
# Option A: zoomed curve with p90/p95 markers
# =============================================================================

option_a_zoom <- ggplot(
  curve_zoom,
  aes(
    x = current_fdi_raw,
    y = effect_pp
  )
) +
  geom_hline(
    data = zero_standard,
    aes(
      yintercept = zero_y,
      linetype = key
    ),
    inherit.aes = FALSE,
    colour = "grey35",
    linewidth = 0.45
  ) +
  geom_vline(
    data = support_lines,
    aes(xintercept = overall_p90),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  geom_vline(
    data = support_lines,
    aes(xintercept = overall_p95),
    inherit.aes = FALSE,
    linetype = "dotdash",
    linewidth = 0.4
  ) +
  geom_ribbon(
    aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp,
      fill = "95% CI"
    ),
    alpha = 0.20,
    linewidth = 0
  ) +
  geom_ribbon(
    aes(
      ymin = conf90_low_pp,
      ymax = conf90_high_pp,
      fill = "90% CI"
    ),
    alpha = 0.38,
    linewidth = 0
  ) +
  geom_line(
    aes(colour = "Point estimate"),
    linewidth = 0.9
  ) +
  facet_wrap(
    vars(outcome_label),
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    title =
      "Option A (zoomed): marginal effects for positive FDI through the 95th percentile",
    x =
      "Manufacturing FDI projects per 100,000 residents, 2009-2014",
    y =
      "Effect of +1 pp Muslim population share\non BJP support (percentage points)",
    caption =
      paste0(
        "Window shown: FDI > 0 and FDI <= panel-specific 95th percentile. ",
        "Dotted and dot-dash vertical lines mark the 90th and 95th percentiles."
      )
  ) +
  base_curve_theme +
  effect_key

# =============================================================================
# Options B/C: zoomed histogram overlays
# =============================================================================

make_overlay_zoom <- function(hist_data_i, positive_only) {

  effect_min <- min(
    c(curve_zoom$conf_low_pp, 0),
    na.rm = TRUE
  )

  effect_max <- max(
    c(curve_zoom$conf_high_pp, 0),
    na.rm = TRUE
  )

  span <- effect_max - effect_min

  effect_min <- effect_min - 0.06 * span
  effect_max <- effect_max + 0.06 * span

  count_max <- max(hist_data_i$count, na.rm = TRUE) * 1.08

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

  curve_overlay <- curve_zoom |>
    mutate(
      plot_effect  = effect_to_count(effect_pp),
      plot_95_low  = effect_to_count(conf_low_pp),
      plot_95_high = effect_to_count(conf_high_pp),
      plot_90_low  = effect_to_count(conf90_low_pp),
      plot_90_high = effect_to_count(conf90_high_pp)
    )

  zero_overlay <- curve_zoom |>
    distinct(
      level,
      outcome_label
    ) |>
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
    "Zoom window: positive-FDI AC support through the 95th percentile"
  } else {
    "Zoom window: full-support histogram restricted to positive FDI through the 95th percentile"
  }

  ggplot() +
    geom_rect(
      data = hist_data_i,
      aes(
        xmin = xmin_plot,
        xmax = xmax_plot,
        ymin = 0,
        ymax = count
      ),
      fill = "grey92",
      colour = "grey78",
      alpha = 0.75,
      linewidth = 0.25
    ) +
    geom_hline(
      data = zero_overlay,
      aes(
        yintercept = zero_y,
        linetype = key
      ),
      inherit.aes = FALSE,
      colour = "grey35",
      linewidth = 0.45
    ) +
    geom_ribbon(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        ymin = plot_95_low,
        ymax = plot_95_high,
        fill = "95% CI"
      ),
      alpha = 0.20,
      linewidth = 0
    ) +
    geom_ribbon(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        ymin = plot_90_low,
        ymax = plot_90_high,
        fill = "90% CI"
      ),
      alpha = 0.38,
      linewidth = 0
    ) +
    geom_line(
      data = curve_overlay,
      aes(
        x = current_fdi_raw,
        y = plot_effect,
        colour = "Point estimate"
      ),
      linewidth = 0.9
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
      nrow = 1,
      scales = "free_x"
    ) +
    scale_y_continuous(
      name = hist_label,
      limits = c(0, count_max),
      expand = expansion(mult = c(0, 0)),
      sec.axis = sec_axis(
        transform = ~ count_to_effect(.),
        name =
          paste0(
            "Effect of +1 pp Muslim population share\n",
            "on BJP support (percentage points)"
          )
      )
    ) +
    labs(
      title = if (positive_only) {
        "Option C (zoomed): marginal effects with positive-FDI histogram overlay"
      } else {
        "Option B (zoomed): marginal effects with histogram overlay"
      },
      subtitle = subtitle,
      x =
        "Manufacturing FDI projects per 100,000 residents, 2009-2014",
      caption =
        paste0(
          "Window shown: FDI > 0 and FDI <= panel-specific 95th percentile. ",
          "Zero-FDI mass is not drawn because it lies outside the zoom window; ",
          "its share is annotated in each panel."
        )
    ) +
    base_curve_theme +
    effect_key
}

option_b_zoom <- make_overlay_zoom(
  hist_all_zoom,
  FALSE
)

option_c_zoom <- make_overlay_zoom(
  hist_positive_zoom,
  TRUE
)

# =============================================================================
# Option D: aligned curve + histogram, zoomed
# =============================================================================

make_stacked_panel_zoom <- function(level_value) {

  curve_i <- curve_zoom |>
    filter(level == level_value)

  hist_i <- hist_all_zoom |>
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
      linewidth = 0.45
    ) +
    geom_ribbon(
      aes(
        ymin = conf_low_pp,
        ymax = conf_high_pp,
        fill = "95% CI"
      ),
      alpha = 0.20,
      linewidth = 0
    ) +
    geom_ribbon(
      aes(
        ymin = conf90_low_pp,
        ymax = conf90_high_pp,
        fill = "90% CI"
      ),
      alpha = 0.38,
      linewidth = 0
    ) +
    geom_line(
      aes(colour = "Point estimate"),
      linewidth = 0.9
    ) +
    labs(
      title = unique(curve_i$outcome_label),
      x = NULL,
      y = "Marginal effect\n(percentage points)"
    ) +
    base_curve_theme +
    effect_key +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 11.5)
    )

  bottom <- ggplot() +
    geom_rect(
      data = hist_i,
      aes(
        xmin = xmin_plot,
        xmax = xmax_plot,
        ymin = 0,
        ymax = count
      ),
      fill = "grey85",
      colour = "grey70",
      linewidth = 0.25
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

option_d_zoom <- (
  make_stacked_panel_zoom("AC") |
    make_stacked_panel_zoom("Voter")
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title =
      "Option D (zoomed): marginal effects with aligned histograms",
    subtitle =
      paste0(
        "Window shown: FDI > 0 and FDI <= panel-specific 95th percentile. ",
        "Zero-FDI mass is annotated but not drawn."
      )
  )

option_d_zoom <- option_d_zoom &
  theme(
    legend.position = "bottom"
  )

save_pair <- function(
  plot_object,
  stem,
  width = 10.8,
  height = 5.3
) {
  ggsave(
    file.path(output_dir, paste0(stem, ".png")),
    plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path(output_dir, paste0(stem, ".pdf")),
    plot_object,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

save_pair(
  option_a_zoom,
  "06_fig5_option_A_zoom_gt0_to_p95_FINAL"
)

save_pair(
  option_b_zoom,
  "07_fig5_option_B_zoom_gt0_to_p95_FINAL"
)

save_pair(
  option_c_zoom,
  "08_fig5_option_C_zoom_gt0_to_p95_FINAL"
)

save_pair(
  option_d_zoom,
  "09_fig5_option_D_zoom_gt0_to_p95_FINAL",
  width = 11.2,
  height = 6.5
)

audit <- tibble(
  requirement = c(
    "FDI > 0 only",
    "Upper bound = 95th percentile",
    "Point-estimate line",
    "90% CI",
    "95% CI",
    "Zero-effect line",
    "Legend/key",
    "Primary model re-estimated"
  ),
  option_A = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  option_B = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  option_C = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  option_D = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)

write_csv(
  audit,
  file.path(
    output_dir,
    "10_fig5_zoom_requirement_audit.csv"
  )
)

writeLines(
  c(
    "R38D Figure 5 zoomed display-only provenance",
    "",
    "Source curve: outputs/r38d_fig5_display_patch_v1_4/00_figure5_curve_with_90_95_ci.csv",
    "Source support summary: outputs/r38d_magnitude_support_power_fig5_audit_v1_3/01_fig5_support_summary.csv",
    "Source histograms: outputs/r38d_magnitude_support_power_fig5_audit_v1_3/05_histogram_bin_counts.csv",
    "",
    "Display window: FDI > 0 and FDI <= panel-specific 95th percentile.",
    "No statistical model was fitted or refitted."
  ),
  file.path(
    output_dir,
    "00_zoom_provenance.txt"
  )
)

cat("\n===== R38D FIGURE 5 ZOOM PATCH =====\n")
cat("WINDOW=FDI_GT_0_TO_PANEL_SPECIFIC_P95\n")
cat("POINT_ESTIMATE_LINE=TRUE\n")
cat("CI_90=TRUE\n")
cat("CI_95=TRUE\n")
cat("ZERO_EFFECT_LINE=TRUE\n")
cat("LEGEND_KEY=TRUE\n")
cat("MODEL_REESTIMATION_RUN=FALSE\n")
cat("OUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38D_FIG5_ZOOM_PATCH_COMPLETE\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
})

# =============================================================================
# R39 v1.1
# Project-wide R38-style marginal-effects display harmonization
#
# PURPOSE
#   Apply the final R38D inferential visual grammar to the remaining ACTIVE
#   Manufacturing marginal-effects displays without re-estimating any model.
#
# REQUIRED GRAMMAR
#   - point-estimate line
#   - inner 90% CI
#   - outer 95% CI
#   - zero-effect line
#   - explicit legend/key
#   - empirical x-axis support
#
# SUPPORT OPTIONS
#   A = p90 / p95 support markers
#   B = full-support histogram overlay
#   C = positive-support histogram overlay, with zero mass noted
#   D = aligned full-support histogram below the curve
#
# ACTIVE FAMILIES HANDLED HERE
#   1. Appendix Figure A4 candidate:
#        log1p Manufacturing FDI, +1 pp Muslim-share marginal effect.
#
#   2. Appendix Figure A5 candidate:
#        +1 Manufacturing FDI project per 100k across Muslim share.
#
#   3. Internal/review 10-pp Muslim-share marginal-effect display.
#
# Figure 5 raw +1-pp Manufacturing display is ALREADY handled by
# R/38d_fig5_display_patch_v1_4.R and is not duplicated here.
#
# IMPORTANT
#   This is a display-only script. It does not read model objects and does not
#   fit or refit any statistical model.
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

source_dir <- file.path(
  project_root,
  "outputs",
  "manufacturing_marginal_effects_v1_0"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "r39_marginal_effect_display_standard_v1_1"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

primary_1pp_path <- file.path(
  source_dir,
  "01_primary_muslim_effect_1pp_grid.csv"
)

review_10pp_path <- file.path(
  source_dir,
  "02_review_muslim_effect_10pp_grid.csv"
)

reverse_path <- file.path(
  source_dir,
  "03_appendix_reverse_plus1_project_grid.csv"
)

ac_samples_path <- file.path(
  project_root,
  "outputs",
  "ac_canonical_v1_0",
  "model_samples.rds"
)

voter_samples_path <- file.path(
  project_root,
  "outputs",
  "voter_canonical_v1_0",
  "model_samples.rds"
)

required_files <- c(
  primary_1pp_path,
  review_10pp_path,
  reverse_path,
  ac_samples_path,
  voter_samples_path
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing required frozen/display source(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

primary_1pp <- read_csv(
  primary_1pp_path,
  show_col_types = FALSE
)

review_10pp <- read_csv(
  review_10pp_path,
  show_col_types = FALSE
)

reverse_curve <- read_csv(
  reverse_path,
  show_col_types = FALSE
)

ac_samples <- readRDS(ac_samples_path)
voter_samples <- readRDS(voter_samples_path)

if (!"AC01" %in% names(ac_samples)) {
  stop("AC01 sample missing from AC canonical model samples.")
}

if (!"V01" %in% names(voter_samples)) {
  stop("V01 sample missing from voter canonical model samples.")
}

# -----------------------------------------------------------------------------
# 1. Frozen support distributions
# -----------------------------------------------------------------------------

ac_support <- ac_samples[["AC01"]] |>
  distinct(ac_uid, .keep_all = TRUE) |>
  transmute(
    level = "AC",
    outcome_label = "AC-level centrist BJP share",
    ac_uid = as.character(ac_uid),
    current_fdi_raw =
      as.numeric(fdi_mfg_local_all_pc100k_2014),
    muslim_share_percent =
      100 * as.numeric(muslim)
  )

voter_support <- voter_samples[["V01"]] |>
  distinct(ac_uid, .keep_all = TRUE) |>
  transmute(
    level = "Voter",
    outcome_label = "Individual centrist BJP vote",
    ac_uid = as.character(ac_uid),
    current_fdi_raw =
      as.numeric(fdi_mfg_current),
    muslim_share_percent =
      100 * as.numeric(muslim)
  )

support <- bind_rows(
  ac_support,
  voter_support
)

if (
  any(!is.finite(support$current_fdi_raw)) ||
  any(support$current_fdi_raw < 0) ||
  any(!is.finite(support$muslim_share_percent))
) {
  stop("Invalid support variable encountered.")
}

# -----------------------------------------------------------------------------
# 2. Add 90% CI to frozen curves
# -----------------------------------------------------------------------------

add_display_ci <- function(curve, family_id) {

  required <- c(
    "level",
    "outcome_label",
    "functional_form",
    "effect_pp",
    "conf_low_pp",
    "conf_high_pp"
  )

  missing <- setdiff(required, names(curve))

  if (length(missing) > 0L) {
    stop(
      family_id,
      " is missing required curve columns: ",
      paste(missing, collapse = ", ")
    )
  }

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
      family_id,
      ": multiple candidate SE columns found: ",
      paste(se_candidates, collapse = ", ")
    )
  }

  if (length(se_candidates) == 1L) {

    se_col <- se_candidates[[1]]

    curve$display_se_pp <- as.numeric(
      curve[[se_col]]
    )

    method <- paste0(
      "Stored pointwise SE column: ",
      se_col
    )

  } else {

    midpoint_error <- max(
      abs(
        curve$effect_pp -
          (
            curve$conf_low_pp +
            curve$conf_high_pp
          ) / 2
      ),
      na.rm = TRUE
    )

    if (
      !is.finite(midpoint_error) ||
      midpoint_error > 1e-7
    ) {
      stop(
        family_id,
        ": frozen 95% CI is not symmetric around the point estimate; ",
        "do not reconstruct 90% CI."
      )
    }

    curve <- curve |>
      mutate(
        display_se_pp =
          (
            conf_high_pp -
            conf_low_pp
          ) /
          (
            2 *
            qnorm(.975)
          )
      )

    method <-
      "Reconstructed from symmetric frozen 95% CI"
  }

  if (
    any(!is.finite(curve$display_se_pp)) ||
    any(curve$display_se_pp < 0)
  ) {
    stop(
      family_id,
      ": invalid display SE."
    )
  }

  curve <- curve |>
    mutate(
      conf90_low_pp =
        effect_pp -
        qnorm(.95) *
        display_se_pp,

      conf90_high_pp =
        effect_pp +
        qnorm(.95) *
        display_se_pp
    )

  nesting_bad <- curve |>
    filter(
      conf90_low_pp <
        conf_low_pp - 1e-8 |
      conf90_high_pp >
        conf_high_pp + 1e-8
    )

  if (nrow(nesting_bad) > 0L) {
    stop(
      family_id,
      ": 90% CI does not nest inside frozen 95% CI."
    )
  }

  attr(curve, "ci90_method") <- method
  curve
}

primary_1pp_ci <- add_display_ci(
  primary_1pp,
  "primary_1pp"
)

review_10pp_ci <- add_display_ci(
  review_10pp,
  "review_10pp"
)

reverse_ci <- add_display_ci(
  reverse_curve,
  "reverse_plus1"
)

# -----------------------------------------------------------------------------
# 3. Common R38 inferential visual grammar
# -----------------------------------------------------------------------------

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
      override.aes = list(
        linewidth = .9
      )
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

make_support_summary <- function(
  support_data,
  x_var
) {

  support_data |>
    group_by(
      level,
      outcome_label
    ) |>
    summarise(
      n_ac = n(),

      zero_share =
        mean(
          .data[[x_var]] == 0
        ),

      p90 =
        quantile(
          .data[[x_var]],
          .90,
          names = FALSE,
          type = 8
        ),

      p95 =
        quantile(
          .data[[x_var]],
          .95,
          names = FALSE,
          type = 8
        ),

      max_x =
        max(
          .data[[x_var]]
        ),

      .groups = "drop"
    ) |>
    mutate(
      zero_label =
        paste0(
          round(
            100 * zero_share,
            1
          ),
          "% at zero"
        )
    )
}

make_histogram_data <- function(
  support_data,
  x_var,
  positive_only = FALSE,
  bins = 18L
) {

  map_dfr(
    split(
      support_data,
      support_data$outcome_label
    ),
    function(dd) {

      x <- as.numeric(
        dd[[x_var]]
      )

      if (positive_only) {
        x <- x[x > 0]
      }

      if (length(x) == 0L) {
        return(tibble())
      }

      x_min <- if (positive_only) {
        min(x)
      } else {
        min(0, min(x))
      }

      x_max <- max(x)

      if (x_max <= x_min) {
        x_max <- x_min + 1
      }

      breaks <- seq(
        x_min,
        x_max + sqrt(.Machine$double.eps) * max(1, abs(x_max)),
        length.out = bins + 1L
      )

      h <- hist(
        x,
        breaks = breaks,
        plot = FALSE,
        include.lowest = TRUE,
        right = FALSE
      )

      tibble(
        level = dd$level[[1]],
        outcome_label =
          dd$outcome_label[[1]],
        xmin =
          head(h$breaks, -1),
        xmax =
          tail(h$breaks, -1),
        count =
          h$counts,
        positive_only =
          positive_only
      )
    }
  )
}

facet_formula_for <- function(curve) {
  if (
    dplyr::n_distinct(
      curve$functional_form
    ) > 1L
  ) {
    outcome_label ~ functional_form
  } else {
    outcome_label ~ .
  }
}

make_option_a <- function(
  curve,
  support_data,
  x_var,
  x_label,
  y_label,
  title,
  subtitle
) {

  ss <- make_support_summary(
    support_data,
    x_var
  )

  zero_data <- curve |>
    distinct(
      outcome_label,
      functional_form
    ) |>
    mutate(
      zero_y = 0,
      key = "Zero effect"
    )

  ggplot(
    curve,
    aes(
      x = .data[[x_var]],
      y = effect_pp
    )
  ) +

    geom_hline(
      data = zero_data,
      aes(
        yintercept = zero_y,
        linetype = key
      ),
      inherit.aes = FALSE,
      colour = "grey35",
      linewidth = .45
    ) +

    geom_vline(
      data = ss,
      aes(
        xintercept = p90
      ),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = .4
    ) +

    geom_vline(
      data = ss,
      aes(
        xintercept = p95
      ),
      inherit.aes = FALSE,
      linetype = "dotdash",
      linewidth = .4
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
      aes(
        colour = "Point estimate"
      ),
      linewidth = .9
    ) +

    facet_grid(
      facet_formula_for(curve),
      scales = "free_y"
    ) +

    labs(
      title = paste0(
        "Option A: ",
        title
      ),
      subtitle = paste0(
        subtitle,
        "; dotted/dot-dash lines mark p90/p95 support"
      ),
      x = x_label,
      y = y_label
    ) +

    base_curve_theme +
    effect_key
}

make_overlay_panel <- function(
  curve_i,
  hist_i,
  summary_i,
  x_var,
  x_label,
  y_label,
  positive_only
) {

  effect_min <- min(
    c(
      curve_i$conf_low_pp,
      0
    ),
    na.rm = TRUE
  )

  effect_max <- max(
    c(
      curve_i$conf_high_pp,
      0
    ),
    na.rm = TRUE
  )

  span <- effect_max -
    effect_min

  if (
    !is.finite(span) ||
    span <= 0
  ) {
    span <- 1
  }

  effect_min <- effect_min -
    .06 * span

  effect_max <- effect_max +
    .06 * span

  count_max <- max(
    hist_i$count,
    na.rm = TRUE
  ) * 1.08

  if (
    !is.finite(count_max) ||
    count_max <= 0
  ) {
    count_max <- 1
  }

  effect_to_count <- function(x) {
    (
      x -
        effect_min
    ) /
      (
        effect_max -
          effect_min
      ) *
      count_max
  }

  count_to_effect <- function(y) {
    effect_min +
      y /
      count_max *
      (
        effect_max -
          effect_min
      )
  }

  curve_overlay <- curve_i |>
    mutate(
      plot_effect =
        effect_to_count(
          effect_pp
        ),

      plot_95_low =
        effect_to_count(
          conf_low_pp
        ),

      plot_95_high =
        effect_to_count(
          conf_high_pp
        ),

      plot_90_low =
        effect_to_count(
          conf90_low_pp
        ),

      plot_90_high =
        effect_to_count(
          conf90_high_pp
        )
    )

  zero_overlay <- tibble(
    zero_y =
      effect_to_count(0),
    key = "Zero effect"
  )

  panel_title <- paste0(
    unique(curve_i$outcome_label),
    if (
      dplyr::n_distinct(
        curve_i$functional_form
      ) == 1L
    ) {
      paste0(
        " - ",
        unique(
          curve_i$functional_form
        )
      )
    } else {
      ""
    }
  )

  ggplot() +

    geom_rect(
      data = hist_i,
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

    geom_ribbon(
      data = curve_overlay,
      aes(
        x = .data[[x_var]],
        ymin = plot_95_low,
        ymax = plot_95_high,
        fill = "95% CI"
      ),
      alpha = .20,
      linewidth = 0
    ) +

    geom_ribbon(
      data = curve_overlay,
      aes(
        x = .data[[x_var]],
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
        x = .data[[x_var]],
        y = plot_effect,
        colour = "Point estimate"
      ),
      linewidth = .9
    ) +

    annotate(
      "text",
      x = Inf,
      y = Inf,
      label =
        summary_i$zero_label[[1]],
      hjust = 1.05,
      vjust = 1.3,
      size = 3
    ) +

    scale_y_continuous(
      name = if (positive_only) {
        "Positive-support ACs per bin"
      } else {
        "Unique ACs per bin"
      },
      sec.axis = sec_axis(
        transform = ~ count_to_effect(.),
        name = y_label
      )
    ) +

    labs(
      title = panel_title,
      x = x_label
    ) +

    base_curve_theme +
    effect_key
}

make_option_overlay <- function(
  curve,
  support_data,
  x_var,
  x_label,
  y_label,
  title,
  subtitle,
  positive_only
) {

  hist <- make_histogram_data(
    support_data,
    x_var,
    positive_only =
      positive_only
  )

  ss <- make_support_summary(
    support_data,
    x_var
  )

  combos <- curve |>
    distinct(
      level,
      outcome_label,
      functional_form
    )

  plots <- pmap(
    combos,
    function(
      level,
      outcome_label,
      functional_form
    ) {

      curve_i <- curve |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label,
          .data$functional_form ==
            functional_form
        )

      hist_i <- hist |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label
        )

      summary_i <- ss |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label
        )

      make_overlay_panel(
        curve_i,
        hist_i,
        summary_i,
        x_var,
        x_label,
        y_label,
        positive_only
      )
    }
  )

  wrap_plots(
    plots,
    ncol =
      if (
        dplyr::n_distinct(
          curve$functional_form
        ) >
          1L
      ) {
        2
      } else {
        2
      },
    guides = "collect"
  ) +
    plot_annotation(
      title = paste0(
        if (positive_only) {
          "Option C: "
        } else {
          "Option B: "
        },
        title
      ),
      subtitle = subtitle
    ) &
    theme(
      legend.position =
        "bottom"
    )
}

make_stacked_panel <- function(
  curve_i,
  hist_i,
  summary_i,
  x_var,
  x_label,
  y_label
) {

  zero_data <- tibble(
    zero_y = 0,
    key = "Zero effect"
  )

  panel_title <- paste0(
    unique(curve_i$outcome_label),
    " - ",
    unique(curve_i$functional_form)
  )

  top <- ggplot(
    curve_i,
    aes(
      x = .data[[x_var]],
      y = effect_pp
    )
  ) +

    geom_hline(
      data = zero_data,
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
      aes(
        colour = "Point estimate"
      ),
      linewidth = .9
    ) +

    labs(
      title = panel_title,
      x = NULL,
      y = y_label
    ) +

    base_curve_theme +
    effect_key +

    theme(
      axis.text.x =
        element_blank(),
      axis.ticks.x =
        element_blank(),
      plot.title =
        element_text(
          size = 10.5
        )
    )

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
      label =
        summary_i$zero_label[[1]],
      hjust = 1.05,
      vjust = 1.3,
      size = 3
    ) +

    labs(
      x = x_label,
      y = "Unique ACs"
    ) +

    theme_minimal(
      base_size = 10
    ) +

    theme(
      panel.grid.minor =
        element_blank(),
      panel.grid.major.x =
        element_blank()
    )

  top /
    bottom +
    plot_layout(
      heights =
        c(
          3,
          1.25
        )
    )
}

make_option_d <- function(
  curve,
  support_data,
  x_var,
  x_label,
  y_label,
  title,
  subtitle
) {

  hist <- make_histogram_data(
    support_data,
    x_var,
    positive_only = FALSE
  )

  ss <- make_support_summary(
    support_data,
    x_var
  )

  combos <- curve |>
    distinct(
      level,
      outcome_label,
      functional_form
    )

  plots <- pmap(
    combos,
    function(
      level,
      outcome_label,
      functional_form
    ) {

      curve_i <- curve |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label,
          .data$functional_form ==
            functional_form
        )

      hist_i <- hist |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label
        )

      summary_i <- ss |>
        filter(
          .data$level == level,
          .data$outcome_label ==
            outcome_label
        )

      make_stacked_panel(
        curve_i,
        hist_i,
        summary_i,
        x_var,
        x_label,
        y_label
      )
    }
  )

  wrap_plots(
    plots,
    ncol =
      if (
        dplyr::n_distinct(
          curve$functional_form
        ) >
          1L
      ) {
        2
      } else {
        2
      },
    guides = "collect"
  ) +
    plot_annotation(
      title = paste0(
        "Option D: ",
        title
      ),
      subtitle = subtitle
    ) &
    theme(
      legend.position =
        "bottom"
    )
}

save_pair <- function(
  plot_object,
  stem,
  width = 11.2,
  height = 6.6
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

# -----------------------------------------------------------------------------
# 4. Family registry
# -----------------------------------------------------------------------------

families <- list(
  appendix_A4_log1p_1pp = list(
    curve =
      primary_1pp_ci |>
      filter(
        functional_form ==
          "log1p"
      ),

    support =
      support,

    x_var =
      "current_fdi_raw",

    x_label =
      "Manufacturing FDI projects per 100,000 residents, 2009-2014",

    y_label =
      "Change in BJP support (percentage points)\nfor +1 pp Muslim population share",

    title =
      "Logged Manufacturing FDI and the Muslim-share gradient",

    subtitle =
      "R38-style display of the completed log1p Manufacturing marginal-effect curve"
  ),

  appendix_A5_reverse_plus1 = list(
    curve =
      reverse_ci,

    support =
      support,

    x_var =
      "muslim_share_percent",

    x_label =
      "Muslim population share, 2001 (%)",

    y_label =
      "Change in BJP support (percentage points)\nfor +1 Manufacturing FDI project per 100,000",

    title =
      "Marginal effect of Manufacturing FDI across Muslim population share",

    subtitle =
      "R38-style display of the completed +1-project reverse marginal-effect curve"
  ),

  review_10pp = list(
    curve =
      review_10pp_ci,

    support =
      support,

    x_var =
      "current_fdi_raw",

    x_label =
      "Manufacturing FDI projects per 100,000 residents, 2009-2014",

    y_label =
      "Change in BJP support (percentage points)\nfor +10 pp Muslim population share",

    title =
      "Manufacturing FDI and the 10-pp Muslim-share gradient",

    subtitle =
      "Internal/review scale alternative; same completed R29 calculations"
  )
)

# -----------------------------------------------------------------------------
# 5. Generate A/B/C/D for every remaining active marginal-effect family
# -----------------------------------------------------------------------------

family_audits <- list()
support_summaries <- list()
curve_exports <- list()

for (
  family_id in
  names(families)
) {

  cfg <- families[[family_id]]

  curve <- cfg$curve
  support_i <- cfg$support
  x_var <- cfg$x_var

  if (
    !x_var %in%
      names(curve)
  ) {
    stop(
      family_id,
      ": x variable absent from curve: ",
      x_var
    )
  }

  if (
    !x_var %in%
      names(support_i)
  ) {
    stop(
      family_id,
      ": x variable absent from support: ",
      x_var
    )
  }

  option_a <- make_option_a(
    curve,
    support_i,
    x_var,
    cfg$x_label,
    cfg$y_label,
    cfg$title,
    cfg$subtitle
  )

  option_b <- make_option_overlay(
    curve,
    support_i,
    x_var,
    cfg$x_label,
    cfg$y_label,
    cfg$title,
    paste0(
      cfg$subtitle,
      "; full empirical x-axis support"
    ),
    positive_only = FALSE
  )

  option_c <- make_option_overlay(
    curve,
    support_i,
    x_var,
    cfg$x_label,
    cfg$y_label,
    cfg$title,
    paste0(
      cfg$subtitle,
      "; positive empirical x-axis support; zero mass noted"
    ),
    positive_only = TRUE
  )

  option_d <- make_option_d(
    curve,
    support_i,
    x_var,
    cfg$x_label,
    cfg$y_label,
    cfg$title,
    paste0(
      cfg$subtitle,
      "; aligned histogram avoids a dual-axis overlay"
    )
  )

  save_pair(
    option_a,
    paste0(
      family_id,
      "__option_A_p90_p95"
    )
  )

  save_pair(
    option_b,
    paste0(
      family_id,
      "__option_B_full_hist_overlay"
    )
  )

  save_pair(
    option_c,
    paste0(
      family_id,
      "__option_C_positive_hist_overlay"
    )
  )

  save_pair(
    option_d,
    paste0(
      family_id,
      "__option_D_aligned_hist_below"
    ),
    height =
      if (
        dplyr::n_distinct(
          curve$functional_form
        ) >
          1L
      ) {
        9.0
      } else {
        6.8
      }
  )

  curve_exports[[family_id]] <-
    curve |>
    mutate(
      family_id =
        family_id,
      x_support_variable =
        x_var,
      .before = 1
    )

  support_summaries[[family_id]] <-
    make_support_summary(
      support_i,
      x_var
    ) |>
    mutate(
      family_id =
        family_id,
      x_support_variable =
        x_var,
      .before = 1
    )

  family_audits[[family_id]] <-
    tibble(
      family_id =
        family_id,

      requirement =
        c(
          "Point-estimate line",
          "90% CI",
          "95% CI",
          "Zero-effect line",
          "Legend/key",
          "Empirical x-axis support",
          "Primary model re-estimated",
          "Model object read"
        ),

      option_A =
        c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          FALSE
        ),

      option_B =
        c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          FALSE
        ),

      option_C =
        c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          FALSE
        ),

      option_D =
        c(
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          FALSE
        )
    )
}

curve_export <- bind_rows(
  curve_exports
)

support_export <- bind_rows(
  support_summaries
)

audit_export <- bind_rows(
  family_audits
)

write_csv(
  curve_export,
  file.path(
    output_dir,
    "00_all_harmonized_curve_data_90_95_ci.csv"
  )
)

write_csv(
  support_export,
  file.path(
    output_dir,
    "01_all_harmonized_support_summary.csv"
  )
)

write_csv(
  audit_export,
  file.path(
    output_dir,
    "02_display_requirement_audit.csv"
  )
)

manifest <- tribble(
  ~paper_artifact, ~family, ~status, ~retained_display_versions,
  "Figure 5",
  "Raw Manufacturing, +1 pp Muslim share",
  "Already harmonized by R38D v1.4",
  "Retain Option A and Option D as paper-ready alternatives",

  "Appendix Figure A4",
  "log1p Manufacturing, +1 pp Muslim share",
  "R39 harmonized",
  "Retain Option A and Option D; Options B/C are review-only",

  "Appendix Figure A5",
  "+1 Manufacturing project across Muslim share",
  "R39 harmonized",
  "Retain Option A and Option D; Options B/C are review-only",

  "Internal/review",
  "+10 pp Muslim-share effect across Manufacturing FDI",
  "R39 harmonized",
  "Retain A/D for review if useful; not required as a paper artifact"
)

write_csv(
  manifest,
  file.path(
    output_dir,
    "03_publication_marginal_effect_display_manifest.csv"
  )
)

ci_methods <- tibble(
  family = c(
    "primary_1pp",
    "review_10pp",
    "reverse_plus1"
  ),
  ci90_method = c(
    attr(
      primary_1pp_ci,
      "ci90_method"
    ),
    attr(
      review_10pp_ci,
      "ci90_method"
    ),
    attr(
      reverse_ci,
      "ci90_method"
    )
  )
)

write_csv(
  ci_methods,
  file.path(
    output_dir,
    "04_ci90_provenance.csv"
  )
)

notes <- c(
  "R39 v1.1 — MARGINAL-EFFECT DISPLAY HARMONIZATION",
  "",
  "This script does not estimate or re-estimate any model.",
  "It reads the completed R29 curve CSVs and frozen canonical sample support.",
  "",
  "Every generated candidate contains:",
  "point-estimate line; 90% CI; 95% CI; zero-effect line; explicit legend/key; empirical x-axis support.",
  "",
  "For FDI-axis figures, support is Manufacturing FDI projects per 100,000.",
  "For the reverse marginal-effect figure, support is 2001 Muslim population share.",
  "",
  "Figure 5 itself is not regenerated here because R38D v1.4 is already the final R38-style display patch.",
  "",
  "No paper registry is modified by this script.",
  "Publication decision: retain both Option A and Option D for A4 and A5; Options B/C remain review-only.",
  "Figure 5 should likewise retain its R38D Option A and Option D paper-ready alternatives."
)

writeLines(
  notes,
  file.path(
    output_dir,
    "05_notes.txt"
  )
)

cat("\n===== R39 DISPLAY REQUIREMENT AUDIT =====\n\n")
print(
  audit_export,
  n = Inf,
  width = Inf
)

cat("\n===== R39 PUBLICATION DISPLAY MANIFEST =====\n\n")
print(
  manifest,
  n = Inf,
  width = Inf
)

cat("\n===== R39 90% CI PROVENANCE =====\n\n")
print(
  ci_methods,
  n = Inf,
  width = Inf
)

cat("\nOUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R39_MARGINAL_EFFECT_DISPLAY_STANDARD_V1_1_COMPLETE\n")

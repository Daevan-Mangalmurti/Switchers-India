suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(fixest)
  library(lme4)
  library(patchwork)
})

required_packages <- c(
  "dplyr", "purrr", "readr", "tibble",
  "ggplot2", "fixest", "lme4", "patchwork"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before rerunning."
  )
}

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

output_dir <- file.path(
  project_root, "outputs", "r38d_magnitude_support_power_fig5_audit_v1_3"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

curve_path <- file.path(
  project_root, "outputs", "manufacturing_marginal_effects_v1_0",
  "01_primary_muslim_effect_1pp_grid.csv"
)
ac_samples_path <- file.path(
  project_root, "outputs", "ac_canonical_v1_0", "model_samples.rds"
)
voter_samples_path <- file.path(
  project_root, "outputs", "voter_canonical_v1_0", "model_samples.rds"
)
ac_models_path <- file.path(
  project_root, "outputs", "main_regression_table_models_v1_0",
  "13_ac_table_models.rds"
)
voter_models_path <- file.path(
  project_root, "outputs", "main_regression_table_models_v1_0",
  "14_voter_table_models.rds"
)
ac_wald_path <- file.path(
  project_root,
  "outputs",
  "r38b_ac_four_ideology_heterogeneity_v1_1",
  "04_PRIMARY_union_native_pairwise_wald_tests.csv"
)

voter_native_wald_path <- file.path(
  project_root,
  "outputs",
  "r38c3_native_voter_cluster_bootstrap_wald_v1_0",
  "06_PRIMARY_native_model_pairwise_cluster_bootstrap_wald_tests.csv"
)

voter_pooled_wald_path <- file.path(
  project_root,
  "outputs",
  "r38c4_voter_center_reference_four_ideology_wald_v1_0",
  "03_pairwise_wald_tests.csv"
)

voter_support_path <- file.path(
  project_root,
  "outputs",
  "r38c_voter_four_ideology_heterogeneity_v1_1",
  "05_pairwise_sample_support_diagnostics.csv"
)

required_files <- c(
  curve_path, ac_samples_path, voter_samples_path,
  ac_models_path, voter_models_path,
  ac_wald_path,
  voter_native_wald_path,
  voter_pooled_wald_path,
  voter_support_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Missing required input(s): ",
    paste(missing_files, collapse = ", "),
    ". Run R38B and R38C before R38D."
  )
}

curve <- read_csv(curve_path, show_col_types = FALSE) |>
  filter(functional_form == "Raw")

ac_samples <- readRDS(ac_samples_path)
voter_samples <- readRDS(voter_samples_path)
ac_models <- readRDS(ac_models_path)
voter_models <- readRDS(voter_models_path)
ac_wald <- read_csv(
  ac_wald_path,
  show_col_types = FALSE
)

voter_native_wald <- read_csv(
  voter_native_wald_path,
  show_col_types = FALSE
)

voter_pooled_wald <- read_csv(
  voter_pooled_wald_path,
  show_col_types = FALSE
)

voter_wald_support <- read_csv(
  voter_support_path,
  show_col_types = FALSE
)

if (!"AC01" %in% names(ac_samples)) stop("AC01 sample missing.")
if (!"V01" %in% names(voter_samples)) stop("V01 sample missing.")
if (!"manufacturing_raw__C3" %in% names(ac_models)) {
  stop("AC manufacturing_raw__C3 model missing.")
}
if (!"manufacturing_raw__C3" %in% names(voter_models)) {
  stop("Voter manufacturing_raw__C3 model missing.")
}

ac_support <- ac_samples[["AC01"]] |>
  distinct(ac_uid, .keep_all = TRUE) |>
  transmute(
    level = "AC",
    outcome_label = "AC-level centrist BJP share",
    unit_id = as.character(ac_uid),
    ac_uid = as.character(ac_uid),
    current_raw =
      as.numeric(fdi_mfg_local_all_pc100k_2014),
    baseline_raw =
      as.numeric(fdi_mfg_local_all_pc100k_2009),
    muslim = as.numeric(muslim)
  )

voter_rows <- voter_samples[["V01"]] |>
  transmute(
    level = "Voter",
    outcome_label = "Individual centrist BJP vote",
    unit_id = as.character(respondent_uid),
    ac_uid = as.character(ac_uid),
    current_raw = as.numeric(fdi_mfg_current),
    baseline_raw = as.numeric(fdi_mfg_baseline),
    muslim = as.numeric(muslim)
  )

voter_ac_support <- voter_rows |>
  distinct(ac_uid, .keep_all = TRUE)

support <- bind_rows(
  ac_support,
  voter_ac_support
)

if (
  any(!is.finite(support$current_raw)) ||
  any(!is.finite(support$baseline_raw)) ||
  any(support$current_raw < 0) ||
  any(support$baseline_raw < 0)
) {
  stop("Invalid Manufacturing FDI support values.")
}

support_summary <- support |>
  group_by(level, outcome_label) |>
  summarise(
    n_unique_ac = n(),
    n_zero = sum(current_raw == 0),
    zero_share = mean(current_raw == 0),
    n_positive = sum(current_raw > 0),
    overall_p90 = quantile(
      current_raw, .90, names = FALSE, type = 8
    ),
    overall_p95 = quantile(
      current_raw, .95, names = FALSE, type = 8
    ),
    positive_median = median(current_raw[current_raw > 0]),
    positive_p75 = quantile(
      current_raw[current_raw > 0],
      .75, names = FALSE, type = 8
    ),
    positive_p90 = quantile(
      current_raw[current_raw > 0],
      .90, names = FALSE, type = 8
    ),
    positive_p95 = quantile(
      current_raw[current_raw > 0],
      .95, names = FALSE, type = 8
    ),
    max_current = max(current_raw),
    .groups = "drop"
  )

interpolate_curve <- function(dd, x) {
  dd <- dd |>
    arrange(current_fdi_raw) |>
    distinct(current_fdi_raw, .keep_all = TRUE)

  tibble(
    effect_pp = approx(
      dd$current_fdi_raw,
      dd$effect_pp,
      xout = x,
      rule = 2
    )$y,
    conf_low_pp = approx(
      dd$current_fdi_raw,
      dd$conf_low_pp,
      xout = x,
      rule = 2
    )$y,
    conf_high_pp = approx(
      dd$current_fdi_raw,
      dd$conf_high_pp,
      xout = x,
      rule = 2
    )$y
  )
}

find_crossing <- function(dd, yvar) {
  dd <- dd |>
    arrange(current_fdi_raw) |>
    distinct(current_fdi_raw, .keep_all = TRUE)

  x <- dd$current_fdi_raw
  y <- dd[[yvar]]

  exact <- which(is.finite(y) & abs(y) < 1e-12)
  intervals <- which(
    is.finite(y[-length(y)]) &
      is.finite(y[-1]) &
      y[-length(y)] * y[-1] < 0
  )

  roots <- numeric(0)

  if (length(exact) > 0L) {
    roots <- c(roots, x[exact])
  }

  if (length(intervals) > 0L) {
    roots <- c(
      roots,
      map_dbl(
        intervals,
        function(i) {
          x1 <- x[[i]]
          x2 <- x[[i + 1]]
          y1 <- y[[i]]
          y2 <- y[[i + 1]]
          x1 + (0 - y1) * (x2 - x1) / (y2 - y1)
        }
      )
    )
  }

  roots <- sort(unique(roots))
  if (length(roots) == 0L) NA_real_ else roots[[1]]
}

magnitude_rows <- list()

for (level_value in unique(curve$level)) {
  curve_i <- curve |>
    filter(level == level_value)

  support_i <- support |>
    filter(level == level_value)

  summary_i <- support_summary |>
    filter(level == level_value)

  point_cross <- find_crossing(curve_i, "effect_pp")
  lower_cross <- find_crossing(curve_i, "conf_low_pp")

  eval <- tribble(
    ~evaluation_label, ~current_fdi_raw,
    "Zero FDI", 0,
    "Overall p90", summary_i$overall_p90,
    "Overall p95", summary_i$overall_p95,
    "Median among positive-FDI ACs", summary_i$positive_median,
    "Positive-FDI p75", summary_i$positive_p75,
    "Positive-FDI p90", summary_i$positive_p90,
    "Point-estimate zero crossing", point_cross,
    "Lower-95%-CI zero crossing", lower_cross
  ) |>
    filter(is.finite(current_fdi_raw))

  magnitude_rows[[level_value]] <- map_dfr(
    seq_len(nrow(eval)),
    function(i) {
      row <- eval[i, , drop = FALSE]
      m <- interpolate_curve(
        curve_i,
        row$current_fdi_raw
      )

      tibble(
        level = level_value,
        outcome_label = unique(curve_i$outcome_label),
        evaluation_label = row$evaluation_label,
        current_fdi_raw = row$current_fdi_raw,
        effect_1pp_muslim_pp = m$effect_pp,
        conf_low_1pp_pp = m$conf_low_pp,
        conf_high_1pp_pp = m$conf_high_pp,
        effect_10pp_muslim_pp = 10 * m$effect_pp,
        conf_low_10pp_pp = 10 * m$conf_low_pp,
        conf_high_10pp_pp = 10 * m$conf_high_pp,
        n_ac_at_or_above =
          sum(support_i$current_raw >= row$current_fdi_raw),
        share_ac_at_or_above =
          mean(support_i$current_raw >= row$current_fdi_raw),
        n_positive_ac_at_or_above =
          sum(
            support_i$current_raw > 0 &
              support_i$current_raw >= row$current_fdi_raw
          )
      )
    }
  )
}

magnitude_table <- bind_rows(magnitude_rows)

find_term <- function(coefficient_names, variables) {
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
      "Could not uniquely identify term: ",
      paste(variables, collapse = " x "),
      ". Hits: ", paste(hits, collapse = ", ")
    )
  }
  hits[[1]]
}

get_beta <- function(model, level) {
  if (level == "AC") {
    coef(model)
  } else {
    fixef(model)
  }
}

compute_observation_effects <- function(
  model,
  data,
  level,
  current_col,
  baseline_col
) {
  beta <- get_beta(model, level)

  muslim_term <- intersect("muslim", names(beta))
  if (length(muslim_term) != 1L) {
    stop("Could not identify Muslim main effect.")
  }

  current_interaction <- find_term(
    names(beta),
    c("muslim", "x_current")
  )
  baseline_interaction <- find_term(
    names(beta),
    c("muslim", "x_baseline")
  )

  derivative <-
    unname(beta[muslim_term]) +
    unname(beta[current_interaction]) *
      as.numeric(data[[current_col]]) +
    unname(beta[baseline_interaction]) *
      as.numeric(data[[baseline_col]])

  tibble(
    effect_1pp_muslim_pp = derivative,
    effect_10pp_muslim_pp = 10 * derivative
  )
}

ac_model <- ac_models[["manufacturing_raw__C3"]]
voter_model <- voter_models[["manufacturing_raw__C3"]]

ac_effects <- compute_observation_effects(
  ac_model,
  ac_support,
  "AC",
  "current_raw",
  "baseline_raw"
)

voter_effects <- compute_observation_effects(
  voter_model,
  voter_rows,
  "Voter",
  "current_raw",
  "baseline_raw"
)

voter_ac_effects <- compute_observation_effects(
  voter_model,
  voter_ac_support,
  "Voter",
  "current_raw",
  "baseline_raw"
)

ame_table <- bind_rows(
  tibble(
    level = "AC",
    averaging_unit = "Unique ACs",
    n = nrow(ac_effects),
    mean_effect_1pp_muslim_pp =
      mean(ac_effects$effect_1pp_muslim_pp),
    median_effect_1pp_muslim_pp =
      median(ac_effects$effect_1pp_muslim_pp),
    mean_effect_10pp_muslim_pp =
      mean(ac_effects$effect_10pp_muslim_pp),
    median_effect_10pp_muslim_pp =
      median(ac_effects$effect_10pp_muslim_pp)
  ),
  tibble(
    level = "Voter",
    averaging_unit = "Respondents",
    n = nrow(voter_effects),
    mean_effect_1pp_muslim_pp =
      mean(voter_effects$effect_1pp_muslim_pp),
    median_effect_1pp_muslim_pp =
      median(voter_effects$effect_1pp_muslim_pp),
    mean_effect_10pp_muslim_pp =
      mean(voter_effects$effect_10pp_muslim_pp),
    median_effect_10pp_muslim_pp =
      median(voter_effects$effect_10pp_muslim_pp)
  ),
  tibble(
    level = "Voter",
    averaging_unit = "Unique ACs (equal-AC sensitivity)",
    n = nrow(voter_ac_effects),
    mean_effect_1pp_muslim_pp =
      mean(voter_ac_effects$effect_1pp_muslim_pp),
    median_effect_1pp_muslim_pp =
      median(voter_ac_effects$effect_1pp_muslim_pp),
    mean_effect_10pp_muslim_pp =
      mean(voter_ac_effects$effect_10pp_muslim_pp),
    median_effect_10pp_muslim_pp =
      median(voter_ac_effects$effect_10pp_muslim_pp)
  )
)

ac_mde <- ac_wald |>
  filter(
    functional_form == "Raw",
    contrast_id %in%
      c(
        "center_vs_left",
        "center_vs_right",
        "center_vs_mixed"
      )
  ) |>
  mutate(
    analysis_level = "AC",
    model_variant =
      "Primary union-native AC Wald",
    critical_975 =
      qt(
        .975,
        df = F_df2
      ),
    approximate_mde_80pct_power =
      (
        critical_975 +
          qnorm(.80)
      ) *
      std_error,
    observed_abs_difference =
      abs(difference),
    observed_to_mde_ratio =
      observed_abs_difference /
      approximate_mde_80pct_power,
    sample_size_descriptor =
      paste0(
        "ACs A=",
        native_n_ac_a,
        "; B=",
        native_n_ac_b,
        "; union=",
        n_union_ac,
        "; overlap=",
        n_overlap_ac
      )
  ) |>
  transmute(
    analysis_level,
    sector,
    functional_form,
    model_variant,
    contrast_id,
    sample_size_descriptor,
    inference_cluster_count =
      n_pc_clusters,
    observed_difference =
      difference,
    std_error,
    p_value =
      cluster_df_F_p,
    approximate_mde_80pct_power,
    observed_to_mde_ratio
  )

voter_native_mde <- voter_native_wald |>
  filter(
    contrast_id %in%
      c(
        "center_vs_left",
        "center_vs_right",
        "center_vs_mixed"
      )
  ) |>
  left_join(
    voter_wald_support |>
      filter(
        functional_form == "Raw",
        contrast_id %in%
          c(
            "center_vs_left",
            "center_vs_right",
            "center_vs_mixed"
          )
      ) |>
      select(
        cell_id,
        contrast_id,
        n_voters_a,
        n_voters_b,
        n_ac_a,
        n_ac_b,
        n_union_ac,
        n_overlap_ac
      ),
    by = c(
      "cell_id",
      "contrast_id"
    ),
    relationship =
      "many-to-one"
  ) |>
  mutate(
    analysis_level = "Voter",
    functional_form = "Raw",
    wald_family =
      "Native separate ideology-specific mixed LPMs",
    display_model_variant =
      "Native separate mixed-LPM coefficients; AC-cluster bootstrap covariance",
    sample_size_descriptor =
      paste0(
        "voters A=",
        n_voters_a,
        "; B=",
        n_voters_b,
        "; ACs A=",
        n_ac_a,
        "; B=",
        n_ac_b,
        "; union ACs=",
        n_union_ac,
        "; overlap ACs=",
        n_overlap_ac,
        "; bootstrap reps=",
        complete_bootstrap_replicates
      )
  ) |>
  transmute(
    analysis_level,
    wald_family,
    sector,
    functional_form,
    model_variant =
      display_model_variant,
    contrast_id,
    sample_size_descriptor,
    inference_cluster_count =
      NA_integer_,
    observed_difference,
    std_error =
      bootstrap_se_difference,
    p_value =
      wald_p_normal,
    approximate_mde_80pct_power,
    observed_to_mde_ratio
  )

voter_pooled_mde <- voter_pooled_wald |>
  filter(
    period == "current",
    sector %in%
      c(
        "Total",
        "Manufacturing"
      ),
    contrast_id %in%
      c(
        "center_vs_left",
        "center_vs_right",
        "center_vs_mixed"
      )
  ) |>
  left_join(
    voter_wald_support |>
      filter(
        functional_form == "Raw",
        contrast_id %in%
          c(
            "center_vs_left",
            "center_vs_right",
            "center_vs_mixed"
          )
      ) |>
      select(
        cell_id,
        contrast_id,
        n_voters_a,
        n_voters_b,
        n_ac_a,
        n_ac_b,
        n_union_ac,
        n_overlap_ac
      ),
    by = c(
      "cell_id",
      "contrast_id"
    ),
    relationship =
      "many-to-one"
  ) |>
  mutate(
    analysis_level = "Voter",
    functional_form = "Raw",
    wald_family =
      "Pooled four-ideology triple-interaction mixed LPM",
    display_model_variant =
      "Pooled all-available-voter FDI x Muslim x ideology model",
    approximate_mde_80pct_power =
      (
        qnorm(.975) +
          qnorm(.80)
      ) *
      std_error,
    observed_to_mde_ratio =
      abs(estimate) /
      approximate_mde_80pct_power,
    sample_size_descriptor =
      paste0(
        "pair support: voters A=",
        n_voters_a,
        "; B=",
        n_voters_b,
        "; ACs A=",
        n_ac_a,
        "; B=",
        n_ac_b,
        "; union ACs=",
        n_union_ac,
        "; overlap ACs=",
        n_overlap_ac,
        "; pooled four-group model uses all eligible ideology-complete voters"
      )
  ) |>
  transmute(
    analysis_level,
    wald_family,
    sector,
    functional_form,
    model_variant =
      display_model_variant,
    contrast_id,
    sample_size_descriptor,
    inference_cluster_count =
      NA_integer_,
    observed_difference =
      estimate,
    std_error,
    p_value,
    approximate_mde_80pct_power,
    observed_to_mde_ratio
  )

ac_mde <- ac_mde |>
  mutate(
    wald_family =
      "AC ideology-specific subgroup-outcome models",
    .after = analysis_level
  )

mde_table <- bind_rows(
  ac_mde,
  voter_native_mde,
  voter_pooled_mde
)

write_csv(
  support_summary,
  file.path(output_dir, "01_fig5_support_summary.csv")
)
write_csv(
  magnitude_table,
  file.path(output_dir, "02_marginal_effect_magnitude_at_supported_values.csv")
)
write_csv(
  ame_table,
  file.path(output_dir, "03_average_marginal_effects.csv")
)
write_csv(
  mde_table,
  file.path(output_dir, "04_approximate_80pct_power_mde.csv")
)

hist_bins <- 18L

make_hist_data <- function(
  support_data,
  positive_only
) {
  x_max <- max(support_data$current_raw)
  breaks <- seq(
    0,
    x_max + sqrt(.Machine$double.eps) * max(1, x_max),
    length.out = hist_bins + 1L
  )

  support_data |>
    group_split(level, outcome_label) |>
    map_dfr(
      function(dd) {
        x <- dd$current_raw
        if (positive_only) x <- x[x > 0]

        h <- hist(
          x,
          breaks = breaks,
          plot = FALSE,
          include.lowest = TRUE,
          right = FALSE
        )

        tibble(
          level = dd$level[[1]],
          outcome_label = dd$outcome_label[[1]],
          xmin = head(h$breaks, -1),
          xmax = tail(h$breaks, -1),
          xmid = h$mids,
          count = h$counts,
          positive_only = positive_only
        )
      }
    )
}

hist_all <- make_hist_data(support, FALSE)
hist_positive <- make_hist_data(support, TRUE)

write_csv(
  bind_rows(hist_all, hist_positive),
  file.path(output_dir, "05_histogram_bin_counts.csv")
)

support_lines <- support_summary |>
  mutate(
    zero_label = paste0(
      round(100 * zero_share, 1),
      "% of ACs at zero FDI"
    )
  )

base_curve_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8.5)
  )

option_a <- ggplot(
  curve,
  aes(
    x = current_fdi_raw,
    y = effect_pp
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = .4
  ) +
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
  geom_ribbon(
    aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp
    ),
    alpha = .18,
    linewidth = 0
  ) +
  geom_line(linewidth = .85) +
  facet_wrap(vars(outcome_label), nrow = 1) +
  labs(
    title =
      "Option A: marginal effects with p90/p95 support markers",
    x =
      "Manufacturing FDI projects per 100,000 residents, 2009-2014",
    y =
      "Effect of +1 pp Muslim population share\non BJP support (percentage points)",
    caption =
      "Dotted and dot-dash lines mark the 90th and 95th percentiles of current Manufacturing FDI."
  ) +
  base_curve_theme

make_overlay <- function(hist_data, positive_only) {
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

  count_max <- max(hist_data$count, na.rm = TRUE) * 1.08

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
      data = hist_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = count
      ),
      fill = "grey80",
      colour = "grey65",
      alpha = .60,
      linewidth = .25
    ) +
    geom_hline(
      yintercept = effect_to_count(0),
      linetype = "dashed",
      linewidth = .4
    ) +
    geom_ribbon(
      data = curve,
      aes(
        x = current_fdi_raw,
        ymin = effect_to_count(conf_low_pp),
        ymax = effect_to_count(conf_high_pp)
      ),
      alpha = .18,
      linewidth = 0
    ) +
    geom_line(
      data = curve,
      aes(
        x = current_fdi_raw,
        y = effect_to_count(effect_pp)
      ),
      linewidth = .85
    ) +
    geom_text(
      data = support_lines,
      aes(
        x = Inf,
        y = Inf,
        label = zero_label
      ),
      hjust = 1.05,
      vjust = 1.4,
      size = 3
    ) +
    facet_wrap(vars(outcome_label), nrow = 1) +
    scale_y_continuous(
      name = hist_label,
      limits = c(0, count_max),
      expand = expansion(mult = c(0, 0)),
      sec.axis = sec_axis(
        transform = ~ count_to_effect(.),
        name =
          "Effect of +1 pp Muslim population share\non BJP support (percentage points)"
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
        "The dual axes are linked only by a linear display transformation; the relative vertical height of bars and the marginal-effect curve has no substantive meaning."
    ) +
    base_curve_theme
}

option_b <- make_overlay(hist_all, FALSE)
option_c <- make_overlay(hist_positive, TRUE)

make_stacked_panel <- function(level_value) {
  curve_i <- curve |>
    filter(level == level_value)
  support_i <- support |>
    filter(level == level_value)
  summary_i <- support_lines |>
    filter(level == level_value)

  top <- ggplot(
    curve_i,
    aes(
      x = current_fdi_raw,
      y = effect_pp
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = .4
    ) +
    geom_ribbon(
      aes(
        ymin = conf_low_pp,
        ymax = conf_high_pp
      ),
      alpha = .18,
      linewidth = 0
    ) +
    geom_line(linewidth = .85) +
    labs(
      title = unique(curve_i$outcome_label),
      x = NULL,
      y =
        "Marginal effect\n(percentage points)"
    ) +
    base_curve_theme +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 11.5)
    )

  bottom <- ggplot(
    support_i,
    aes(x = current_raw)
  ) +
    geom_histogram(
      bins = hist_bins,
      boundary = 0,
      closed = "left",
      fill = "grey80",
      colour = "grey65",
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
    plot_layout(heights = c(3, 1.25))
}

option_d <- (
  make_stacked_panel("AC") |
    make_stacked_panel("Voter")
) +
  plot_annotation(
    title =
      "Option D: marginal effects with aligned full-support histograms",
    subtitle =
      "Separate y-axes avoid a dual-axis overlay while retaining the complete FDI support distribution."
  )

save_pair <- function(plot_object, stem, width = 10.8, height = 5.3) {
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

save_pair(option_a, "06_fig5_option_A_p90_p95")
save_pair(option_b, "07_fig5_option_B_full_hist_overlay")
save_pair(option_c, "08_fig5_option_C_positive_hist_overlay")
save_pair(
  option_d,
  "09_fig5_option_D_aligned_hist_below",
  width = 11.2,
  height = 6.5
)

notes <- c(
  "R38D MAGNITUDE, SUPPORT, POWER, AND FIGURE-5 AUDIT",
  "",
  "No primary model is re-estimated.",
  "",
  "Magnitude table:",
  "Uses the completed R29 Manufacturing raw marginal-effect curve and evaluates it at prespecified, empirically meaningful FDI values.",
  "Both +1 percentage-point and +10 percentage-point Muslim-share contrasts are reported with AC support counts.",
  "",
  "Average marginal effects:",
  "Calculated directly from the completed Manufacturing model coefficients over the observed current and baseline FDI values.",
  "For the voter model, respondent-average is primary; equal-AC averaging is a sensitivity description.",
  "",
  "MDE:",
  "Approximate 80%-power minimum detectable coefficient differences are descriptive power diagnostics, not formal retrospective power tests.",
  "AC MDE uses the R38B v1.1 primary union-native pairwise Wald tests and the cluster-df t critical value.",
  "Voter power/MDE calculations preserve BOTH voter heterogeneity families side by side:",
  "1. R38C3 AC-cluster-bootstrap covariance for the separately fitted native ideology-specific mixed-LPM coefficients.",
  "2. R38C4 model-based SEs for pairwise contrasts from the pooled all-available-voter FDI x Muslim x ideology mixed LPM, reparameterized with Center as the reference category.",
  "These are not interchangeable because the pooled mixed model shares variance components and nuisance-parameter restrictions that the separate native models do not.",
  "",
  "Figure options:",
  "A = curve with p90/p95 support markers.",
  "B = full unique-AC histogram overlay including zero FDI.",
  "C = positive-FDI-only histogram overlay, with zero-FDI share explicitly annotated.",
  "D = aligned full-support histogram beneath the marginal-effect curve; this avoids dual-axis overlay.",
  "",
  "Histogram counts always use unique ACs because FDI is an AC-level exposure."
)
writeLines(notes, file.path(output_dir, "10_notes.txt"))

cat("===== SUPPORT SUMMARY =====\n\n")
print(support_summary, n = Inf, width = Inf)

cat("\n===== MARGINAL-EFFECT MAGNITUDE =====\n\n")
print(magnitude_table, n = Inf, width = Inf)

cat("\n===== AVERAGE MARGINAL EFFECTS =====\n\n")
print(ame_table, n = Inf, width = Inf)

cat("\n===== APPROXIMATE MDE / POWER DIAGNOSTIC =====\n\n")
print(mde_table, n = Inf, width = Inf)

cat("\nOUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38D_COMPLETE\n")

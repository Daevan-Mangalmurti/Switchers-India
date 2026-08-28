suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(fixest)
  library(lme4)
  library(scales)
})

required_packages <- c(
  "dplyr", "tidyr", "purrr", "readr", "tibble", "ggplot2",
  "patchwork", "fixest", "lme4", "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

input_dir <- file.path(project_root, "data", "derived", "switchers_rewrite", "final")
output_dir <- file.path(project_root, "outputs", "r42b_fdi_ideology_marginal_effects_v1_0")
figure_dir <- file.path(output_dir, "figures")
data_dir <- file.path(output_dir, "figure_data")
review_dir <- file.path(output_dir, "review")
for (d in c(output_dir, figure_dir, data_dir, review_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
# Frozen/upstream model sources
# -----------------------------------------------------------------------------

ac_canonical_path <- file.path(project_root, "outputs", "ac_canonical_v1_0", "models.rds")
voter_canonical_path <- file.path(project_root, "outputs", "voter_canonical_v1_0", "models.rds")
ac_heterogeneity_path <- file.path(
  project_root, "outputs", "r38b_ac_four_ideology_heterogeneity_v1_1", "12_native_models.rds"
)
voter_heterogeneity_path <- file.path(
  project_root, "outputs", "r38c_voter_four_ideology_heterogeneity_v1_1", "11_native_models.rds"
)
services_ac_path <- file.path(
  project_root, "outputs", "r42a_services_ideology_extension_v1_0", "03_services_ac_native_models.rds"
)
services_voter_path <- file.path(
  project_root, "outputs", "r42a_services_ideology_extension_v1_0", "04_services_voter_native_models.rds"
)

ideology_path <- file.path(input_dir, "ac_year_ideology_summary.rds")
respondent_path <- file.path(input_dir, "nes_respondent_analysis.rds")
change_path <- file.path(input_dir, "ac_change.rds")

required_files <- c(
  ac_canonical_path, voter_canonical_path,
  ac_heterogeneity_path, voter_heterogeneity_path,
  services_ac_path, services_voter_path,
  ideology_path, respondent_path, change_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required source(s):\n", paste(missing_files, collapse = "\n"))
}

ac_canonical <- readRDS(ac_canonical_path)
voter_canonical <- readRDS(voter_canonical_path)
ac_heterogeneity <- readRDS(ac_heterogeneity_path)
voter_heterogeneity <- readRDS(voter_heterogeneity_path)
services_ac <- readRDS(services_ac_path)
services_voter <- readRDS(services_voter_path)

ideology <- readRDS(ideology_path)
respondents <- readRDS(respondent_path)
ac_change <- readRDS(change_path)

# -----------------------------------------------------------------------------
# Model registry: nine sector x ideology combinations, two levels each
# -----------------------------------------------------------------------------

model_registry <- tribble(
  ~sector, ~sector_slug, ~ideology, ~ideology_slug, ~ac_source, ~ac_key, ~ac_current_term, ~ac_baseline_term, ~voter_source, ~voter_key, ~voter_current_term, ~voter_baseline_term,

  "Total", "total", "Center", "center",
  "canonical", "AC01", "fdi_current", "fdi_baseline",
  "canonical", "V01", "fdi_total_current", "fdi_total_baseline",

  "Total", "total", "Left", "left",
  "heterogeneity", "total_raw__left", "fdi_current", "fdi_baseline",
  "heterogeneity", "total_raw__left", "fdi_current", "fdi_baseline",

  "Total", "total", "Right", "right",
  "heterogeneity", "total_raw__right", "fdi_current", "fdi_baseline",
  "heterogeneity", "total_raw__right", "fdi_current", "fdi_baseline",

  "Manufacturing", "manufacturing", "Center", "center",
  "canonical", "AC05", "fdi_current", "fdi_baseline",
  "canonical", "V05", "fdi_mfg_current", "fdi_mfg_baseline",

  "Manufacturing", "manufacturing", "Left", "left",
  "heterogeneity", "manufacturing_raw__left", "fdi_current", "fdi_baseline",
  "heterogeneity", "manufacturing_raw__left", "fdi_current", "fdi_baseline",

  "Manufacturing", "manufacturing", "Right", "right",
  "heterogeneity", "manufacturing_raw__right", "fdi_current", "fdi_baseline",
  "heterogeneity", "manufacturing_raw__right", "fdi_current", "fdi_baseline",

  "Services", "services", "Center", "center",
  "canonical", "AC06", "fdi_current", "fdi_baseline",
  "canonical", "V06", "fdi_services_current", "fdi_services_baseline",

  "Services", "services", "Left", "left",
  "services_extension", "services_raw__left", "fdi_current", "fdi_baseline",
  "services_extension", "services_raw__left", "fdi_current", "fdi_baseline",

  "Services", "services", "Right", "right",
  "services_extension", "services_raw__right", "fdi_current", "fdi_baseline",
  "services_extension", "services_raw__right", "fdi_current", "fdi_baseline"
)

write_csv(model_registry, file.path(data_dir, "00_model_registry.csv"))

get_model <- function(level, source, key) {
  bank <- if (level == "AC") {
    switch(
      source,
      canonical = ac_canonical,
      heterogeneity = ac_heterogeneity,
      services_extension = services_ac,
      stop("Unknown AC source: ", source)
    )
  } else {
    switch(
      source,
      canonical = voter_canonical,
      heterogeneity = voter_heterogeneity,
      services_extension = services_voter,
      stop("Unknown voter source: ", source)
    )
  }

  if (!key %in% names(bank)) {
    stop(
      "Missing model key ", key, " in ", level, "/", source,
      ". Available keys: ", paste(names(bank), collapse = ", ")
    )
  }
  bank[[key]]
}

# -----------------------------------------------------------------------------
# Reconstruct exact raw-support samples used by the raw native/canonical models
# -----------------------------------------------------------------------------

primary_ac_controls <- c("proxy_ac_pop", "sc_pop_share", "st_pop_share")
individual_controls <- c("religion_group", "caste_group", "education_harmonized")
ideology_levels <- c("Left", "Center", "Right")

sector_registry <- tribble(
  ~sector, ~sector_slug, ~current_col, ~baseline_col,
  "Total", "total", "fdi_total_local_all_pc100k_2014", "fdi_total_local_all_pc100k_2009",
  "Manufacturing", "manufacturing", "fdi_mfg_local_all_pc100k_2014", "fdi_mfg_local_all_pc100k_2009",
  "Services", "services", "fdi_services_local_all_pc100k_2014", "fdi_services_local_all_pc100k_2009"
)

fdi_cols <- unique(c(sector_registry$current_col, sector_registry$baseline_col))
missing_fdi <- setdiff(c("ac_uid", fdi_cols), names(ac_change))
if (length(missing_fdi) > 0L) {
  stop("ac_change missing required raw FDI columns: ", paste(missing_fdi, collapse = ", "))
}

fdi_payload <- ac_change |>
  select(ac_uid, all_of(fdi_cols))
if (anyDuplicated(fdi_payload$ac_uid) > 0L) stop("FDI payload is not unique by ac_uid.")

relevel_if_present <- function(x, reference) {
  out <- factor(as.character(x))
  if (reference %in% levels(out)) out <- stats::relevel(out, ref = reference)
  out
}

ac_support_base <- ideology |>
  filter(
    year == 2014,
    as.character(ideology) %in% ideology_levels
  ) |>
  select(-any_of(fdi_cols)) |>
  left_join(fdi_payload, by = "ac_uid", relationship = "many-to-one") |>
  mutate(
    ideology_name = as.character(ideology),
    y = as.numeric(weighted_share_voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy)
  )

respondents2 <- respondents |>
  select(-any_of(fdi_cols)) |>
  left_join(fdi_payload, by = "ac_uid", relationship = "many-to-one")

voter_support_base <- respondents2 |>
  mutate(
    y = as.numeric(voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),
    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),
    state_fe = factor(state_no),
    ac_random = factor(ac_uid),
    religion_x = relevel_if_present(religion_group, "1: Hindu"),
    caste_x = relevel_if_present(caste_group, "4: Others"),
    education_x = relevel_if_present(education_harmonized, "Secondary"),
    ideology_name = as.character(voter_ideology)
  ) |>
  filter(
    year == 2014,
    vote_valid %in% TRUE,
    !is.na(y),
    ideology_complete %in% TRUE,
    ideology_name %in% ideology_levels,
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

make_support <- function(level, sector_name, ideology_group) {
  ss <- sector_registry |>
    filter(.data$sector == .env$sector_name)
  if (nrow(ss) != 1L) stop("Sector lookup failed: ", sector_name)

  current_col <- ss$current_col[[1]]
  baseline_col <- ss$baseline_col[[1]]

  if (level == "AC") {
    ac_support_base |>
      filter(.data$ideology_name == .env$ideology_group) |>
      mutate(
        current_raw = as.numeric(.data[[current_col]]),
        baseline_raw = as.numeric(.data[[baseline_col]])
      ) |>
      filter(
        !is.na(y),
        bjp_candidate_present %in% TRUE,
        fdi_spatial_support %in% TRUE,
        is.finite(muslim),
        is.finite(current_raw),
        is.finite(baseline_raw),
        if_all(all_of(primary_ac_controls), ~ !is.na(.x)),
        !is.na(state_no),
        !is.na(pc_cluster_id)
      ) |>
      transmute(
        unit_id = as.character(ac_uid),
        ac_uid = as.character(ac_uid),
        current_raw,
        baseline_raw,
        muslim
      )
  } else {
    voter_support_base |>
      filter(.data$ideology_name == .env$ideology_group) |>
      mutate(
        current_raw = as.numeric(.data[[current_col]]),
        baseline_raw = as.numeric(.data[[baseline_col]])
      ) |>
      filter(
        is.finite(current_raw),
        is.finite(baseline_raw)
      ) |>
      transmute(
        unit_id = as.character(respondent_uid),
        ac_uid = as.character(ac_uid),
        current_raw,
        baseline_raw,
        muslim
      )
  }
}

# -----------------------------------------------------------------------------
# Linear-combination marginal-effect machinery
# -----------------------------------------------------------------------------

get_beta_vcov <- function(fit) {
  if (inherits(fit, "fixest")) {
    beta <- coef(fit)
  } else if (inherits(fit, "merMod")) {
    beta <- lme4::fixef(fit)
  } else {
    stop("Unsupported model class: ", paste(class(fit), collapse = "/"))
  }
  V <- as.matrix(vcov(fit))
  V <- V[names(beta), names(beta), drop = FALSE]
  list(beta = beta, V = V)
}

find_interaction_term <- function(coefficient_names, variables) {
  hits <- coefficient_names[
    vapply(
      strsplit(coefficient_names, ":", fixed = TRUE),
      function(parts) length(parts) == length(variables) && setequal(parts, variables),
      logical(1)
    )
  ]
  if (length(hits) != 1L) {
    stop(
      "Could not uniquely identify interaction: ", paste(variables, collapse = " x "),
      ". Hits: ", paste(hits, collapse = ", ")
    )
  }
  hits[[1]]
}

linear_combo <- function(beta, V, weights) {
  L <- setNames(rep(0, length(beta)), names(beta))
  unknown <- setdiff(names(weights), names(L))
  if (length(unknown) > 0L) {
    stop("Unknown coefficient(s): ", paste(unknown, collapse = ", "))
  }
  L[names(weights)] <- as.numeric(weights)
  estimate <- sum(L * beta)
  variance <- as.numeric(t(L) %*% V %*% L)
  if (variance < -1e-10) stop("Negative linear-combination variance.")
  se <- sqrt(max(variance, 0))
  tibble(estimate = estimate, std_error = se)
}

support_quantile_grid <- function(x, n = 201L) {
  sort(unique(as.numeric(quantile(
    x,
    probs = seq(0, 1, length.out = n),
    na.rm = TRUE,
    names = FALSE,
    type = 8
  ))))
}

make_curve <- function(
  fit, support, level, sector, ideology_name,
  current_term, baseline_term
) {
  if (nrow(support) != nobs(fit)) {
    stop(
      "Support/model N mismatch for ", level, " / ", sector, " / ", ideology_name,
      ": support N=", nrow(support), ", model N=", nobs(fit)
    )
  }

  bundle <- get_beta_vcov(fit)
  beta <- bundle$beta
  V <- bundle$V

  if (!"muslim" %in% names(beta)) stop("Muslim main effect absent.")
  current_int <- find_interaction_term(names(beta), c("muslim", current_term))
  baseline_int <- find_interaction_term(names(beta), c("muslim", baseline_term))

  current_grid <- support_quantile_grid(support$current_raw)
  mean_baseline <- mean(support$baseline_raw)

  map_dfr(current_grid, function(current_value) {
    lc <- linear_combo(
      beta,
      V,
      setNames(
        c(1, current_value, mean_baseline),
        c("muslim", current_int, baseline_int)
      )
    )

    # muslim is a proportion. A +1 percentage-point change is +0.01;
    # converting the resulting outcome-probability change to percentage points
    # multiplies by 100, so the net numerical scale factor is 1.
    lc |>
      transmute(
        level = level,
        sector = sector,
        ideology = ideology_name,
        current_fdi_raw = current_value,
        average_baseline_fdi_raw = mean_baseline,
        effect_pp = estimate,
        std_error_pp = std_error,
        conf90_low_pp = estimate - qnorm(.95) * std_error,
        conf90_high_pp = estimate + qnorm(.95) * std_error,
        conf_low_pp = estimate - qnorm(.975) * std_error,
        conf_high_pp = estimate + qnorm(.975) * std_error
      )
  })
}

# -----------------------------------------------------------------------------
# Build all 18 level-specific curves (9 combinations x AC/Voter)
# -----------------------------------------------------------------------------

curve_list <- list()
support_list <- list()
audit_list <- list()

for (i in seq_len(nrow(model_registry))) {
  spec <- model_registry[i, , drop = FALSE]

  for (level in c("AC", "Voter")) {
    source <- if (level == "AC") spec$ac_source[[1]] else spec$voter_source[[1]]
    key <- if (level == "AC") spec$ac_key[[1]] else spec$voter_key[[1]]
    current_term <- if (level == "AC") spec$ac_current_term[[1]] else spec$voter_current_term[[1]]
    baseline_term <- if (level == "AC") spec$ac_baseline_term[[1]] else spec$voter_baseline_term[[1]]

    fit <- get_model(level, source, key)
    support <- make_support(level, spec$sector[[1]], spec$ideology[[1]])

    curve_key <- paste(spec$sector_slug, spec$ideology_slug, tolower(level), sep = "__")

    curve_list[[curve_key]] <- make_curve(
      fit = fit,
      support = support,
      level = level,
      sector = spec$sector[[1]],
      ideology_name = spec$ideology[[1]],
      current_term = current_term,
      baseline_term = baseline_term
    )

    support_list[[curve_key]] <- support |>
      mutate(
        level = level,
        sector = spec$sector[[1]],
        ideology = spec$ideology[[1]],
        .before = 1
      )

    audit_list[[curve_key]] <- tibble(
      level = level,
      sector = spec$sector[[1]],
      ideology = spec$ideology[[1]],
      source = source,
      model_key = key,
      support_n = nrow(support),
      model_n = nobs(fit),
      n_ac = n_distinct(support$ac_uid),
      support_model_n_match = nrow(support) == nobs(fit)
    )
  }
}

curves <- bind_rows(curve_list)
support_all <- bind_rows(support_list)
model_support_audit <- bind_rows(audit_list)

if (any(!model_support_audit$support_model_n_match)) {
  print(model_support_audit, n = Inf, width = Inf)
  stop("At least one reconstructed support sample does not match model N.")
}

support_summary <- support_all |>
  group_by(level, sector, ideology) |>
  summarise(
    n = n(),
    n_ac = n_distinct(ac_uid),
    zero_share = mean(current_raw == 0),
    min = min(current_raw),
    p50 = median(current_raw),
    p75 = quantile(current_raw, .75, names = FALSE, type = 8),
    p90 = quantile(current_raw, .90, names = FALSE, type = 8),
    p95 = quantile(current_raw, .95, names = FALSE, type = 8),
    p99 = quantile(current_raw, .99, names = FALSE, type = 8),
    max = max(current_raw),
    baseline_mean = mean(baseline_raw),
    .groups = "drop"
  )

curves <- curves |>
  left_join(
    support_summary |>
      select(level, sector, ideology, n, n_ac, zero_share, p90, p95, p99),
    by = c("level", "sector", "ideology"),
    relationship = "many-to-one"
  )

write_csv(curves, file.path(data_dir, "01_all_18_marginal_effect_curves.csv"))
write_csv(support_summary, file.path(data_dir, "02_panel_specific_support_summary.csv"))
write_csv(model_support_audit, file.path(data_dir, "03_model_support_reconstruction_audit.csv"))

# -----------------------------------------------------------------------------
# Lower-95%-CI zero-crossing diagnostics
# -----------------------------------------------------------------------------

crossing_one <- function(curve_i, support_i) {
  curve_i <- curve_i |>
    arrange(current_fdi_raw)

  idx <- which(
    curve_i$conf_low_pp[-nrow(curve_i)] < 0 &
      curve_i$conf_low_pp[-1] >= 0
  )

  if (length(idx) == 0L) {
    status <- case_when(
      all(curve_i$conf_low_pp >= 0) ~ "lower 95% CI nonnegative throughout evaluated support",
      all(curve_i$conf_low_pp < 0) ~ "lower 95% CI remains below zero throughout evaluated support",
      TRUE ~ "no negative-to-positive crossing found"
    )

    return(tibble(
      crossing_status = status,
      n_negative_to_positive_crossings = 0L,
      crossing_fdi_per100k = NA_real_,
      crossing_percentile = NA_real_,
      crossing_at_or_below_p95 = NA,
      bracket_low_fdi = NA_real_,
      bracket_low_ci = NA_real_,
      bracket_high_fdi = NA_real_,
      bracket_high_ci = NA_real_
    ))
  }

  i <- idx[[1]]
  x1 <- curve_i$current_fdi_raw[[i]]
  x2 <- curve_i$current_fdi_raw[[i + 1L]]
  y1 <- curve_i$conf_low_pp[[i]]
  y2 <- curve_i$conf_low_pp[[i + 1L]]
  x0 <- if (abs(y2 - y1) < .Machine$double.eps) {
    x1
  } else {
    x1 + (0 - y1) * (x2 - x1) / (y2 - y1)
  }

  p95_value <- quantile(support_i$current_raw, .95, names = FALSE, type = 8)

  tibble(
    crossing_status = "negative-to-positive lower-95%-CI crossing found",
    n_negative_to_positive_crossings = length(idx),
    crossing_fdi_per100k = x0,
    crossing_percentile = 100 * ecdf(support_i$current_raw)(x0),
    crossing_at_or_below_p95 = x0 <= p95_value,
    bracket_low_fdi = x1,
    bracket_low_ci = y1,
    bracket_high_fdi = x2,
    bracket_high_ci = y2
  )
}

crossing_rows <- list()
for (nm in names(curve_list)) {
  c_i <- curve_list[[nm]]
  s_i <- support_list[[nm]]
  crossing_rows[[nm]] <- crossing_one(c_i, s_i) |>
    mutate(
      level = unique(c_i$level),
      sector = unique(c_i$sector),
      ideology = unique(c_i$ideology),
      .before = 1
    )
}

crossing_diagnostics <- bind_rows(crossing_rows)
write_csv(
  crossing_diagnostics,
  file.path(data_dir, "04_lower95_zero_crossing_diagnostics.csv")
)

# -----------------------------------------------------------------------------
# Plotting
# -----------------------------------------------------------------------------

plot_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10.5),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8.3),
    legend.position = "bottom",
    legend.title = element_blank()
  )

effect_key <- list(
  scale_colour_manual(
    values = c("Point estimate" = "black"),
    breaks = "Point estimate",
    name = NULL
  ),
  scale_fill_manual(
    values = c("90% CI" = "grey55", "95% CI" = "grey80"),
    breaks = c("90% CI", "95% CI"),
    name = NULL
  ),
  scale_linetype_manual(
    values = c("Zero effect" = "dashed"),
    breaks = "Zero effect",
    name = NULL
  ),
  guides(
    colour = guide_legend(order = 1, override.aes = list(linewidth = .9)),
    fill = guide_legend(order = 2),
    linetype = guide_legend(
      order = 3,
      override.aes = list(colour = "grey35", linewidth = .5)
    )
  )
)

ideology_title_word <- c(
  Center = "centrist",
  Left = "left",
  Right = "right"
)

panel_label <- function(level, ideology_name) {
  if (level == "AC") {
    paste0("AC-level ", tolower(ideology_name), " BJP share")
  } else {
    paste0("Individual ", tolower(ideology_name), " BJP vote")
  }
}

make_combo_plot <- function(sector_name, ideology_name) {
  dd <- curves |>
    filter(sector == sector_name, ideology == ideology_name) |>
    filter(current_fdi_raw > 0, current_fdi_raw <= p95) |>
    mutate(
      panel = factor(
        vapply(level, panel_label, character(1), ideology_name = ideology_name),
        levels = c(
          panel_label("AC", ideology_name),
          panel_label("Voter", ideology_name)
        )
      )
    )

  ss <- support_summary |>
    filter(sector == sector_name, ideology == ideology_name) |>
    mutate(
      panel = factor(
        vapply(level, panel_label, character(1), ideology_name = ideology_name),
        levels = c(
          panel_label("AC", ideology_name),
          panel_label("Voter", ideology_name)
        )
      )
    )

  zero_data <- dd |>
    distinct(panel) |>
    mutate(zero_y = 0, key = "Zero effect")

  ggplot(dd, aes(x = current_fdi_raw, y = effect_pp)) +
    geom_hline(
      data = zero_data,
      aes(yintercept = zero_y, linetype = key),
      inherit.aes = FALSE,
      colour = "grey35",
      linewidth = .45
    ) +
    geom_vline(
      data = ss,
      aes(xintercept = p90),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = .4
    ) +
    geom_vline(
      data = ss,
      aes(xintercept = p95),
      inherit.aes = FALSE,
      linetype = "dotdash",
      linewidth = .4
    ) +
    geom_ribbon(
      aes(ymin = conf_low_pp, ymax = conf_high_pp, fill = "95% CI"),
      alpha = .20,
      linewidth = 0
    ) +
    geom_ribbon(
      aes(ymin = conf90_low_pp, ymax = conf90_high_pp, fill = "90% CI"),
      alpha = .38,
      linewidth = 0
    ) +
    geom_line(aes(colour = "Point estimate"), linewidth = .9) +
    facet_wrap(vars(panel), nrow = 1, scales = "free_x") +
    labs(
      title = paste0(
        sector_name, " FDI and the Muslim-share gradient in ",
        ideology_title_word[[ideology_name]], " BJP support"
      ),
      x = paste0(sector_name, " FDI projects per 100,000 residents, 2009-2014"),
      y = "Effect of +1 pp Muslim population share\non BJP support (percentage points)",
      caption = paste0(
        "Window shown: FDI > 0 and FDI <= the panel-specific 95th percentile. ",
        "Dotted and dot-dash vertical lines mark the panel-specific 90th and 95th percentiles. ",
        "The black line is the point estimate; inner and outer shaded bands are 90% and 95% confidence intervals."
      )
    ) +
    plot_theme +
    effect_key
}

save_pair <- function(plot_object, stem, width = 10.8, height = 5.3, dir = figure_dir) {
  ggsave(
    file.path(dir, paste0(stem, ".png")),
    plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  ggsave(
    file.path(dir, paste0(stem, ".pdf")),
    plot_object,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

file_registry <- tribble(
  ~order, ~sector, ~sector_slug, ~ideology, ~ideology_slug,
  1L, "Total", "total", "Center", "center",
  2L, "Total", "total", "Left", "left",
  3L, "Total", "total", "Right", "right",
  4L, "Manufacturing", "manufacturing", "Center", "center",
  5L, "Manufacturing", "manufacturing", "Left", "left",
  6L, "Manufacturing", "manufacturing", "Right", "right",
  7L, "Services", "services", "Center", "center",
  8L, "Services", "services", "Left", "left",
  9L, "Services", "services", "Right", "right"
)

plot_objects <- list()
for (i in seq_len(nrow(file_registry))) {
  rr <- file_registry[i, , drop = FALSE]
  p <- make_combo_plot(rr$sector[[1]], rr$ideology[[1]])
  stem <- sprintf(
    "%02d_%s_%s_zoomed",
    rr$order[[1]], rr$sector_slug[[1]], rr$ideology_slug[[1]]
  )
  save_pair(p, stem)
  plot_objects[[paste(rr$sector_slug, rr$ideology_slug, sep = "__")]] <- p
}

# Two 3x3 internal comparison sheets: one AC-level and one voter-level.
make_review_sheet <- function(level_value) {
  dd <- curves |>
    filter(level == level_value) |>
    filter(current_fdi_raw > 0, current_fdi_raw <= p95) |>
    mutate(
      sector = factor(sector, levels = c("Total", "Manufacturing", "Services")),
      ideology = factor(ideology, levels = c("Center", "Left", "Right")),
      panel = interaction(sector, ideology, sep = " | ", lex.order = TRUE)
    )

  ss <- support_summary |>
    filter(level == level_value) |>
    mutate(
      sector = factor(sector, levels = c("Total", "Manufacturing", "Services")),
      ideology = factor(ideology, levels = c("Center", "Left", "Right")),
      panel = interaction(sector, ideology, sep = " | ", lex.order = TRUE)
    )

  zero_data <- dd |>
    distinct(panel) |>
    mutate(zero_y = 0)

  ggplot(dd, aes(x = current_fdi_raw, y = effect_pp)) +
    geom_hline(
      data = zero_data,
      aes(yintercept = zero_y),
      inherit.aes = FALSE,
      linetype = "dashed",
      colour = "grey35",
      linewidth = .35
    ) +
    geom_vline(
      data = ss,
      aes(xintercept = p90),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = .3
    ) +
    geom_vline(
      data = ss,
      aes(xintercept = p95),
      inherit.aes = FALSE,
      linetype = "dotdash",
      linewidth = .3
    ) +
    geom_ribbon(aes(ymin = conf_low_pp, ymax = conf_high_pp), fill = "grey80", alpha = .25) +
    geom_line(linewidth = .65) +
    facet_wrap(vars(panel), ncol = 3, scales = "free_x") +
    labs(
      title = paste0(level_value, " marginal-effects comparison: 3 FDI sectors x 3 ideologies"),
      x = "Current FDI projects per 100,000 residents, 2009-2014",
      y = "Effect of +1 pp Muslim share\non BJP support (pp)",
      caption = "Internal review sheet. Each panel is restricted to positive FDI through its own 95th percentile."
    ) +
    theme_minimal(base_size = 9.5) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 8.5),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 7.5)
    )
}

review_ac <- make_review_sheet("AC")
review_voter <- make_review_sheet("Voter")
save_pair(review_ac, "10_review_grid_AC_3x3", width = 12.5, height = 9.5, dir = review_dir)
save_pair(review_voter, "11_review_grid_Voter_3x3", width = 12.5, height = 9.5, dir = review_dir)

write_csv(file_registry, file.path(data_dir, "05_figure_file_registry.csv"))

writeLines(
  c(
    "R42B FDI x IDEOLOGY MARGINAL EFFECTS v1.0.2",
    "",
    "Nine substantive combinations: Total/Manufacturing/Services x Center/Left/Right.",
    "Each standalone figure contains two panels: AC-level outcome and voter-level outcome.",
    "Center models use canonical AC01/AC05/AC06 and V01/V05/V06.",
    "Total/Manufacturing Left/Right models use frozen native R38B/R38C heterogeneity models.",
    "Services Left/Right models use the R42A Services extension after canonical Center reproduction checks.",
    "All curves show the effect of a +1 percentage-point Muslim population-share increase on BJP support, in percentage points.",
    "Baseline FDI is held at the mean of the corresponding estimation sample.",
    "Current-FDI grid uses 201 empirical quantiles of the corresponding estimation-sample support.",
    "Zoom window is positive FDI through the panel-specific 95th percentile.",
    "p90 is dotted; p95 is dot-dash.",
    "No artifact registry is modified by this script.",
    "v1.0.2 hotfix: make_support() uses explicit .env pronouns for sector and ideology arguments to avoid dplyr data-mask name collisions."
  ),
  file.path(output_dir, "00_provenance.txt")
)

cat("\n===== R42B NINE-COMBINATION MARGINAL EFFECTS v1.0.2 =====\n")
cat("N_COMBINATIONS=9\n")
cat("N_LEVEL_SPECIFIC_CURVES=18\n")
cat("ZOOM=FDI_GT_0_TO_PANEL_SPECIFIC_P95\n")
cat("CI_90=TRUE\n")
cat("CI_95=TRUE\n")
cat("ZERO_CROSSING_DIAGNOSTICS=TRUE\n")
cat("\n===== MODEL/SUPPORT AUDIT =====\n")
print(model_support_audit, n = Inf, width = Inf)
cat("\n===== LOWER 95% CI ZERO-CROSSING DIAGNOSTICS =====\n")
print(crossing_diagnostics, n = Inf, width = Inf)
cat("OUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R42B_FDI_IDEOLOGY_MARGINAL_EFFECTS_COMPLETE\n")

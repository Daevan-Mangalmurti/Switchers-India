suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(fixest)
})

required_packages <- c("dplyr", "purrr", "readr", "tibble", "fixest")
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
  project_root, "outputs", "r38b_ac_four_ideology_heterogeneity_v1_1"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ideology_path <- file.path(input_dir, "ac_year_ideology_summary.rds")
change_path <- file.path(input_dir, "ac_change.rds")
r27b_path <- file.path(
  project_root, "outputs", "ac_ideology_outcome_heterogeneity_v1_0",
  "02_native_ideology_coefficients.csv"
)
r27c_path <- file.path(
  project_root, "outputs", "ac_ideology_pairwise_wald_refinement_v1_0",
  "01_pairwise_common_sample_wald_tests.csv"
)

required_files <- c(ideology_path, change_path, r27b_path, r27c_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required input(s): ", paste(missing_files, collapse = ", "))
}

ideology <- readRDS(ideology_path)
ac_change <- readRDS(change_path)
r27b_results <- read_csv(r27b_path, show_col_types = FALSE)
r27c_results <- read_csv(r27c_path, show_col_types = FALSE)

primary_controls <- c("proxy_ac_pop", "sc_pop_share", "st_pop_share")
ideology_levels <- c("Left", "Center", "Right", "Mixed")

required_ideology_columns <- c(
  "ac_uid", "year", "ideology", "weighted_share_voted_bjp",
  "state_no", "pc_cluster_id",
  "bjp_candidate_present", "fdi_spatial_support",
  "muslim_share_2001_dist_proxy",
  primary_controls
)

missing_ideology_columns <- setdiff(
  required_ideology_columns,
  names(ideology)
)
if (length(missing_ideology_columns) > 0L) {
  stop(
    "ac_year_ideology_summary is missing: ",
    paste(missing_ideology_columns, collapse = ", ")
  )
}

fdi_variables <- c(
  "fdi_total_local_all_pc100k_2009",
  "fdi_total_local_all_pc100k_2014",
  "log1p_fdi_total_local_all_pc100k_2009",
  "log1p_fdi_total_local_all_pc100k_2014",
  "fdi_mfg_local_all_pc100k_2009",
  "fdi_mfg_local_all_pc100k_2014"
)

missing_fdi_columns <- setdiff(c("ac_uid", fdi_variables), names(ac_change))
if (length(missing_fdi_columns) > 0L) {
  stop("ac_change is missing: ", paste(missing_fdi_columns, collapse = ", "))
}

fdi_source <- ac_change |>
  select(ac_uid, all_of(fdi_variables))

if (anyDuplicated(fdi_source$ac_uid) > 0L) {
  stop("FDI source is not unique by ac_uid.")
}

analysis_base <- ideology |>
  filter(
    year == 2014,
    as.character(ideology) %in% ideology_levels
  ) |>
  select(-any_of(fdi_variables)) |>
  left_join(
    fdi_source,
    by = "ac_uid",
    relationship = "many-to-one"
  ) |>
  mutate(
    ideology_name = as.character(ideology),
    y = as.numeric(weighted_share_voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),

    total_raw_current = as.numeric(fdi_total_local_all_pc100k_2014),
    total_raw_baseline = as.numeric(fdi_total_local_all_pc100k_2009),
    total_log_current = as.numeric(log1p_fdi_total_local_all_pc100k_2014),
    total_log_baseline = as.numeric(log1p_fdi_total_local_all_pc100k_2009),

    mfg_raw_current = as.numeric(fdi_mfg_local_all_pc100k_2014),
    mfg_raw_baseline = as.numeric(fdi_mfg_local_all_pc100k_2009),
    mfg_log_current = log1p(mfg_raw_current),
    mfg_log_baseline = log1p(mfg_raw_baseline)
  )

if (anyDuplicated(analysis_base[c("ac_uid", "ideology_name")]) > 0L) {
  stop("Analysis base is not unique by AC x ideology.")
}

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

make_sample <- function(
  ideology_value,
  current_col,
  baseline_col,
  raw_current_col
) {
  analysis_base |>
    filter(ideology_name == ideology_value) |>
    mutate(
      fdi_current = .data[[current_col]],
      fdi_baseline = .data[[baseline_col]],
      fdi_current_raw = .data[[raw_current_col]]
    ) |>
    filter(
      !is.na(y),
      bjp_candidate_present %in% TRUE,
      fdi_spatial_support %in% TRUE,
      is.finite(muslim),
      is.finite(fdi_current),
      is.finite(fdi_baseline),
      is.finite(fdi_current_raw),
      if_all(all_of(primary_controls), ~ !is.na(.x)),
      !is.na(state_no),
      !is.na(pc_cluster_id)
    )
}

fit_native <- function(data) {
  feols(
    y ~
      muslim * fdi_current +
      muslim * fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share |
      state_no,
    data = data,
    vcov = ~ pc_cluster_id,
    warn = FALSE,
    notes = FALSE
  )
}

extract_current_interaction <- function(fit) {
  terms <- names(coef(fit))
  focal <- intersect(
    c("muslim:fdi_current", "fdi_current:muslim"),
    terms
  )
  if (length(focal) != 1L) {
    stop("Could not uniquely identify current FDI x Muslim interaction.")
  }
  ct <- coeftable(fit)
  ci <- as.data.frame(confint(fit, level = .95))
  tibble(
    term = focal,
    estimate = unname(ct[focal, 1]),
    std_error = unname(ct[focal, 2]),
    conf_low = unname(ci[focal, 1]),
    conf_high = unname(ci[focal, 2]),
    p_value = unname(ct[focal, 4])
  )
}

native_models <- list()
native_samples <- list()
native_results <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  for (g in ideology_levels) {
    dd <- make_sample(
      g,
      spec$current_col,
      spec$baseline_col,
      spec$raw_current_col
    )

    if (nrow(dd) == 0L) {
      stop("No estimable observations for ", g, " / ", spec$cell_id)
    }

    fit <- fit_native(dd)
    focal <- extract_current_interaction(fit)
    key <- paste(spec$cell_id, tolower(g), sep = "__")

    native_models[[key]] <- fit
    native_samples[[key]] <- dd

    native_results[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      ideology = g,
      estimate = focal$estimate,
      std_error = focal$std_error,
      conf_low = focal$conf_low,
      conf_high = focal$conf_high,
      p_value = focal$p_value,
      n_ac = nrow(dd),
      n_states = n_distinct(dd$state_no),
      n_pc_clusters = n_distinct(dd$pc_cluster_id),
      n_positive_current_fdi_ac = sum(dd$fdi_current_raw > 0),
      share_zero_current_fdi = mean(dd$fdi_current_raw == 0)
    )
  }
}

native_results <- bind_rows(native_results)

r27b_check <- native_results |>
  filter(ideology %in% c("Left", "Center", "Right")) |>
  select(
    cell_id,
    ideology,
    audit_estimate = estimate,
    audit_se = std_error
  ) |>
  left_join(
    r27b_results |>
      select(
        cell_id,
        ideology,
        canonical_estimate = estimate,
        canonical_se = std_error
      ),
    by = c("cell_id", "ideology"),
    relationship = "one-to-one"
  ) |>
  mutate(
    estimate_abs_diff = abs(audit_estimate - canonical_estimate),
    se_abs_diff = abs(audit_se - canonical_se),
    reproduces_r27b =
      estimate_abs_diff < 1e-8 &
      se_abs_diff < 1e-8
  )

if (any(!r27b_check$reproduces_r27b)) {
  print(r27b_check, n = Inf, width = Inf)
  stop("R38B v1.1 does not reproduce existing R27b native estimates.")
}

build_stacked <- function(
  samples,
  ideology_values,
  sample_mode = c("union_native", "common_support")
) {
  sample_mode <- match.arg(sample_mode)

  all_id_sets <- lapply(
    samples,
    function(dd) as.character(dd$ac_uid)
  )
  overlap_ids <- sort(unique(Reduce(intersect, all_id_sets)))
  union_ids <- sort(unique(Reduce(union, all_id_sets)))

  if (sample_mode == "common_support" && length(overlap_ids) == 0L) {
    stop("No common ACs for ", paste(ideology_values, collapse = " / "))
  }

  pieces <- vector("list", length(ideology_values))
  names(pieces) <- ideology_values

  for (g in ideology_values) {
    dd <- samples[[g]]

    if (sample_mode == "common_support") {
      dd <- dd |>
        filter(as.character(ac_uid) %in% overlap_ids)

      dd <- dd[
        match(overlap_ids, as.character(dd$ac_uid)),
        ,
        drop = FALSE
      ]

      if (!identical(as.character(dd$ac_uid), overlap_ids)) {
        stop("Common-support ordering failure for ", g)
      }
    }

    pieces[[g]] <- dd |>
      mutate(ideology_stack = g)
  }

  stacked <- bind_rows(pieces)

  if (anyDuplicated(stacked[c("ac_uid", "ideology_stack")]) > 0L) {
    stop("Stacked AC data are not unique by AC x ideology.")
  }

  for (g in ideology_values) {
    slug <- tolower(g)
    I <- as.numeric(stacked$ideology_stack == g)

    stacked[[paste0(slug, "_muslim")]] <-
      I * stacked$muslim
    stacked[[paste0(slug, "_current")]] <-
      I * stacked$fdi_current
    stacked[[paste0(slug, "_baseline")]] <-
      I * stacked$fdi_baseline
    stacked[[paste0(slug, "_current_muslim")]] <-
      I * stacked$fdi_current * stacked$muslim
    stacked[[paste0(slug, "_baseline_muslim")]] <-
      I * stacked$fdi_baseline * stacked$muslim
    stacked[[paste0(slug, "_proxy_ac_pop")]] <-
      I * stacked$proxy_ac_pop
    stacked[[paste0(slug, "_sc_pop_share")]] <-
      I * stacked$sc_pop_share
    stacked[[paste0(slug, "_st_pop_share")]] <-
      I * stacked$st_pop_share
  }

  stacked <- stacked |>
    mutate(
      state_ideology_fe = interaction(
        state_no,
        ideology_stack,
        drop = TRUE,
        lex.order = TRUE
      )
    )

  slope_terms <- unlist(
    lapply(
      tolower(ideology_values),
      function(slug) {
        paste0(
          slug,
          c(
            "_muslim",
            "_current",
            "_baseline",
            "_current_muslim",
            "_baseline_muslim",
            "_proxy_ac_pop",
            "_sc_pop_share",
            "_st_pop_share"
          )
        )
      }
    )
  )

  fml <- as.formula(
    paste0(
      "y ~ 0 + ",
      paste(slope_terms, collapse = " + "),
      " | state_ideology_fe"
    )
  )

  fit <- feols(
    fml,
    data = stacked,
    vcov = ~ pc_cluster_id,
    warn = FALSE,
    notes = FALSE
  )

  list(
    fit = fit,
    data = stacked,
    overlap_ids = overlap_ids,
    union_ids = union_ids
  )
}

pairwise_wald <- function(fit, term_a, term_b, n_clusters) {
  beta <- coef(fit)
  V <- as.matrix(vcov(fit))

  if (!all(c(term_a, term_b) %in% names(beta))) {
    stop("Pairwise focal terms absent.")
  }

  diff <- unname(beta[term_a] - beta[term_b])
  var_diff <-
    V[term_a, term_a] +
    V[term_b, term_b] -
    2 * V[term_a, term_b]

  se <- sqrt(max(var_diff, 0))
  z2 <- (diff / se)^2
  df2 <- max(n_clusters - 1L, 1L)

  tibble(
    difference = diff,
    std_error = se,
    conf_low = diff - 1.96 * se,
    conf_high = diff + 1.96 * se,
    wald_chisq = z2,
    chi_square_p = pchisq(z2, df = 1, lower.tail = FALSE),
    wald_F = z2,
    F_df1 = 1L,
    F_df2 = df2,
    cluster_df_F_p = pf(
      z2,
      df1 = 1,
      df2 = df2,
      lower.tail = FALSE
    )
  )
}

safe_omnibus_wald <- function(
  fit,
  terms,
  n_clusters,
  rcond_threshold = 1e-12,
  eigen_relative_tolerance = 1e-10
) {
  beta <- coef(fit)
  V <- as.matrix(vcov(fit))

  center_term <- terms[["Center"]]
  others <- c("Left", "Right", "Mixed")
  q <- length(others)

  if (!all(unname(terms) %in% names(beta))) {
    stop("Omnibus focal terms absent.")
  }

  R <- matrix(
    0,
    nrow = q,
    ncol = length(beta),
    dimnames = list(
      paste0("CenterMinus", others),
      names(beta)
    )
  )

  for (k in seq_along(others)) {
    R[k, center_term] <- 1
    R[k, terms[[others[[k]]]]] <- -1
  }

  d <- as.numeric(R %*% beta)
  S <- R %*% V %*% t(R)
  S <- (S + t(S)) / 2

  eig <- eigen(S, symmetric = TRUE)
  values <- eig$values
  max_eig <- max(abs(values))
  tol <- if (max_eig == 0) 0 else max_eig * eigen_relative_tolerance
  keep <- values > tol
  effective_rank <- sum(keep)
  rc <- suppressWarnings(tryCatch(rcond(S), error = function(e) 0))
  full_rank_stable <- effective_rank == q && is.finite(rc) && rc >= rcond_threshold

  df2 <- max(n_clusters - 1L, 1L)

  ordinary_stat <- NA_real_
  ordinary_chi_p <- NA_real_
  ordinary_F <- NA_real_
  ordinary_F_p <- NA_real_

  if (full_rank_stable) {
    ordinary_stat <- as.numeric(t(d) %*% solve(S, d))
    ordinary_chi_p <- pchisq(
      ordinary_stat,
      df = q,
      lower.tail = FALSE
    )
    ordinary_F <- ordinary_stat / q
    ordinary_F_p <- pf(
      ordinary_F,
      df1 = q,
      df2 = df2,
      lower.tail = FALSE
    )
  }

  rank_reduced_stat <- NA_real_
  rank_reduced_p <- NA_real_
  null_component_norm <- NA_real_

  if (effective_rank > 0L) {
    Q_keep <- eig$vectors[, keep, drop = FALSE]
    projected <- as.numeric(crossprod(Q_keep, d))
    rank_reduced_stat <- sum(
      (projected^2) / values[keep]
    )
    rank_reduced_p <- pchisq(
      rank_reduced_stat,
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

  tibble(
    center_minus_left = d[[1]],
    center_minus_right = d[[2]],
    center_minus_mixed = d[[3]],

    omnibus_full_rank_stable = full_rank_stable,
    ordinary_wald_chisq = ordinary_stat,
    ordinary_chi_square_df = if (full_rank_stable) q else NA_integer_,
    ordinary_chi_square_p = ordinary_chi_p,
    ordinary_wald_F = ordinary_F,
    ordinary_F_df1 = if (full_rank_stable) q else NA_integer_,
    ordinary_F_df2 = if (full_rank_stable) df2 else NA_integer_,
    ordinary_cluster_df_F_p = ordinary_F_p,

    effective_rank = effective_rank,
    rank_reduced_wald_chisq_diagnostic = rank_reduced_stat,
    rank_reduced_df_diagnostic = effective_rank,
    rank_reduced_chi_square_p_diagnostic = rank_reduced_p,
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
}

primary_union_pairwise <- list()
secondary_common_pairwise <- list()
union_native_reproduction <- list()
pair_support <- list()
union_membership <- list()
common_membership <- list()
primary_union_models <- list()
secondary_common_models <- list()

primary_union_omnibus <- list()
secondary_common_omnibus <- list()
primary_union_omnibus_models <- list()
secondary_common_omnibus_models <- list()

for (i in seq_len(nrow(cell_registry))) {
  spec <- cell_registry[i, , drop = FALSE]

  samples <- list()
  for (g in ideology_levels) {
    key <- paste(spec$cell_id, tolower(g), sep = "__")
    samples[[g]] <- native_samples[[key]]
  }

  for (j in seq_len(nrow(pair_registry))) {
    pair <- pair_registry[j, , drop = FALSE]
    pair_groups <- c(pair$ideology_a, pair$ideology_b)
    pair_samples <- samples[pair_groups]

    union_obj <- build_stacked(
      pair_samples,
      pair_groups,
      sample_mode = "union_native"
    )
    common_obj <- build_stacked(
      pair_samples,
      pair_groups,
      sample_mode = "common_support"
    )

    slug_a <- tolower(pair$ideology_a)
    slug_b <- tolower(pair$ideology_b)
    term_a <- paste0(slug_a, "_current_muslim")
    term_b <- paste0(slug_b, "_current_muslim")

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

    beta_union <- coef(union_obj$fit)

    key <- paste(spec$cell_id, pair$contrast_id, sep = "__")

    union_native_reproduction[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      contrast_id = pair$contrast_id,
      ideology = c(pair$ideology_a, pair$ideology_b),
      native_estimate = c(native_a$estimate, native_b$estimate),
      union_stacked_estimate = c(
        unname(beta_union[term_a]),
        unname(beta_union[term_b])
      )
    ) |>
      mutate(
        absolute_difference =
          abs(native_estimate - union_stacked_estimate),
        reproduces_native =
          absolute_difference < 1e-8
      )

    union_test <- pairwise_wald(
      union_obj$fit,
      term_a,
      term_b,
      n_distinct(union_obj$data$pc_cluster_id)
    )

    common_test <- pairwise_wald(
      common_obj$fit,
      term_a,
      term_b,
      n_distinct(common_obj$data$pc_cluster_id)
    )

    primary_union_pairwise[[key]] <- union_test |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        contrast_id = pair$contrast_id,
        ideology_a = pair$ideology_a,
        ideology_b = pair$ideology_b,

        native_interaction_a = native_a$estimate,
        native_interaction_b = native_b$estimate,
        native_n_ac_a = native_a$n_ac,
        native_n_ac_b = native_b$n_ac,

        n_union_ac = length(union_obj$union_ids),
        n_overlap_ac = length(union_obj$overlap_ids),
        n_stacked_rows = nrow(union_obj$data),
        n_states = n_distinct(union_obj$data$state_no),
        n_pc_clusters =
          n_distinct(union_obj$data$pc_cluster_id),
        .before = 1
      )

    secondary_common_pairwise[[key]] <- common_test |>
      mutate(
        cell_id = spec$cell_id,
        sector = spec$sector,
        functional_form = spec$functional_form,
        contrast_id = pair$contrast_id,
        ideology_a = pair$ideology_a,
        ideology_b = pair$ideology_b,
        n_common_ac = length(common_obj$overlap_ids),
        n_stacked_rows = nrow(common_obj$data),
        n_states = n_distinct(common_obj$data$state_no),
        n_pc_clusters =
          n_distinct(common_obj$data$pc_cluster_id),
        .before = 1
      )

    primary_union_models[[key]] <- union_obj$fit
    secondary_common_models[[key]] <- common_obj$fit

    union_membership[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      contrast_id = pair$contrast_id,
      ac_uid = union_obj$union_ids,
      in_ideology_a =
        union_obj$union_ids %in%
        as.character(pair_samples[[pair$ideology_a]]$ac_uid),
      in_ideology_b =
        union_obj$union_ids %in%
        as.character(pair_samples[[pair$ideology_b]]$ac_uid)
    )

    common_membership[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      contrast_id = pair$contrast_id,
      ac_uid = common_obj$overlap_ids
    )

    union_support <- bind_rows(pair_samples) |>
      distinct(ac_uid, .keep_all = TRUE)
    common_support <- union_support |>
      filter(as.character(ac_uid) %in% common_obj$overlap_ids)

    pair_support[[key]] <- tibble(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      contrast_id = pair$contrast_id,
      ideology_a = pair$ideology_a,
      ideology_b = pair$ideology_b,
      native_n_ac_a = nrow(pair_samples[[pair$ideology_a]]),
      native_n_ac_b = nrow(pair_samples[[pair$ideology_b]]),
      n_union_ac = nrow(union_support),
      n_overlap_ac = nrow(common_support),
      overlap_share_of_smaller_native =
        nrow(common_support) /
        min(
          nrow(pair_samples[[pair$ideology_a]]),
          nrow(pair_samples[[pair$ideology_b]])
        ),
      n_positive_fdi_union_ac =
        sum(union_support$fdi_current_raw > 0),
      n_positive_fdi_overlap_ac =
        sum(common_support$fdi_current_raw > 0),
      zero_share_union =
        mean(union_support$fdi_current_raw == 0),
      zero_share_overlap =
        mean(common_support$fdi_current_raw == 0),
      union_p90 = quantile(
        union_support$fdi_current_raw,
        .90,
        names = FALSE,
        type = 8
      ),
      overlap_p90 = quantile(
        common_support$fdi_current_raw,
        .90,
        names = FALSE,
        type = 8
      )
    )
  }

  union_four <- build_stacked(
    samples,
    ideology_levels,
    sample_mode = "union_native"
  )
  common_four <- build_stacked(
    samples,
    ideology_levels,
    sample_mode = "common_support"
  )

  focal_terms <- c(
    Left = "left_current_muslim",
    Center = "center_current_muslim",
    Right = "right_current_muslim",
    Mixed = "mixed_current_muslim"
  )

  primary_union_omnibus[[spec$cell_id]] <- safe_omnibus_wald(
    union_four$fit,
    focal_terms,
    n_distinct(union_four$data$pc_cluster_id)
  ) |>
    mutate(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      n_union_ac = length(union_four$union_ids),
      n_all_four_overlap_ac = length(union_four$overlap_ids),
      n_stacked_rows = nrow(union_four$data),
      n_states = n_distinct(union_four$data$state_no),
      n_pc_clusters = n_distinct(union_four$data$pc_cluster_id),
      .before = 1
    )

  secondary_common_omnibus[[spec$cell_id]] <- safe_omnibus_wald(
    common_four$fit,
    focal_terms,
    n_distinct(common_four$data$pc_cluster_id)
  ) |>
    mutate(
      cell_id = spec$cell_id,
      sector = spec$sector,
      functional_form = spec$functional_form,
      n_common_ac = length(common_four$overlap_ids),
      n_stacked_rows = nrow(common_four$data),
      n_states = n_distinct(common_four$data$state_no),
      n_pc_clusters = n_distinct(common_four$data$pc_cluster_id),
      .before = 1
    )

  primary_union_omnibus_models[[spec$cell_id]] <- union_four$fit
  secondary_common_omnibus_models[[spec$cell_id]] <- common_four$fit
}

primary_union_pairwise <- bind_rows(primary_union_pairwise)
secondary_common_pairwise <- bind_rows(secondary_common_pairwise)
union_native_reproduction <- bind_rows(union_native_reproduction)
pair_support <- bind_rows(pair_support)
union_membership <- bind_rows(union_membership)
common_membership <- bind_rows(common_membership)
primary_union_omnibus <- bind_rows(primary_union_omnibus)
secondary_common_omnibus <- bind_rows(secondary_common_omnibus)

if (any(!union_native_reproduction$reproduces_native)) {
  print(union_native_reproduction, n = Inf, width = Inf)
  stop("Union-stacked system fails to reproduce native AC focal coefficients.")
}

r27c_check <- secondary_common_pairwise |>
  filter(
    contrast_id %in%
      c("center_vs_left", "center_vs_right", "left_vs_right")
  ) |>
  select(
    cell_id,
    contrast_id,
    audit_difference = difference,
    audit_se = std_error,
    audit_p = cluster_df_F_p,
    audit_n_common_ac = n_common_ac
  ) |>
  left_join(
    r27c_results |>
      select(
        cell_id,
        contrast_id,
        canonical_difference = difference,
        canonical_se = std_error,
        canonical_p = cluster_df_F_p,
        canonical_n_common_ac = n_common_ac
      ),
    by = c("cell_id", "contrast_id"),
    relationship = "one-to-one"
  ) |>
  mutate(
    difference_abs_diff =
      abs(audit_difference - canonical_difference),
    se_abs_diff =
      abs(audit_se - canonical_se),
    p_abs_diff =
      abs(audit_p - canonical_p),
    reproduces_r27c =
      difference_abs_diff < 1e-8 &
      se_abs_diff < 1e-8 &
      p_abs_diff < 1e-8 &
      audit_n_common_ac == canonical_n_common_ac
  )

if (any(!r27c_check$reproduces_r27c)) {
  print(r27c_check, n = Inf, width = Inf)
  stop("Secondary common-support system fails to reproduce existing R27c.")
}

write_csv(
  cell_registry,
  file.path(output_dir, "00_cell_registry.csv")
)
write_csv(
  r27b_check,
  file.path(output_dir, "01_r27b_native_reproduction_checks.csv")
)
write_csv(
  r27c_check,
  file.path(output_dir, "02_r27c_common_support_reproduction_checks.csv")
)
write_csv(
  native_results,
  file.path(output_dir, "03_native_four_ideology_coefficients.csv")
)
write_csv(
  primary_union_pairwise,
  file.path(output_dir, "04_PRIMARY_union_native_pairwise_wald_tests.csv")
)
write_csv(
  secondary_common_pairwise,
  file.path(output_dir, "05_SECONDARY_common_support_pairwise_wald_tests.csv")
)
write_csv(
  primary_union_omnibus,
  file.path(output_dir, "06_PRIMARY_union_four_group_omnibus.csv")
)
write_csv(
  secondary_common_omnibus,
  file.path(output_dir, "07_SECONDARY_common_support_four_group_omnibus.csv")
)
write_csv(
  pair_support,
  file.path(output_dir, "08_pairwise_sample_support_diagnostics.csv")
)
write_csv(
  union_native_reproduction,
  file.path(output_dir, "09_union_stacked_native_coefficient_reproduction.csv")
)
write_csv(
  union_membership,
  file.path(output_dir, "10_union_pairwise_ac_membership.csv")
)
write_csv(
  common_membership,
  file.path(output_dir, "11_common_support_pairwise_ac_membership.csv")
)

saveRDS(
  native_models,
  file.path(output_dir, "12_native_models.rds")
)
saveRDS(
  primary_union_models,
  file.path(output_dir, "13_PRIMARY_union_pairwise_models.rds")
)
saveRDS(
  secondary_common_models,
  file.path(output_dir, "14_SECONDARY_common_support_pairwise_models.rds")
)
saveRDS(
  primary_union_omnibus_models,
  file.path(output_dir, "15_PRIMARY_union_omnibus_models.rds")
)
saveRDS(
  secondary_common_omnibus_models,
  file.path(output_dir, "16_SECONDARY_common_support_omnibus_models.rds")
)

notes <- c(
  "R38B v1.1 — AC FOUR-IDEOLOGY HETEROGENEITY",
  "",
  "PRIMARY pairwise Wald test:",
  "Each ideology retains its full native AC sample. The pair's two native samples are stacked on their union.",
  "Every slope, AC control, and state fixed effect is ideology-specific.",
  "The stacked focal coefficient for each ideology is required to reproduce its separate native coefficient.",
  "PC-clustered covariance from the stacked system supplies Cov(beta_A, beta_B), so the Wald variance is Var(A)+Var(B)-2Cov(A,B).",
  "",
  "SECONDARY pairwise test:",
  "The older R27c maximal pair-specific common-AC design is retained as a common-support robustness check.",
  "",
  "Omnibus tests:",
  "Four-group omnibus tests are optional diagnostics. If the restriction covariance is numerically singular or ill-conditioned, the ordinary omnibus is reported as NA rather than crashing the pipeline.",
  "A rank-reduced generalized-Wald diagnostic is also saved, but it must not be substituted silently for the ordinary 3-df omnibus.",
  "",
  "Groups: Left, Center, Right, Mixed.",
  "Primary scales: Total raw and Manufacturing raw. log1p cells are robustness checks."
)
writeLines(notes, file.path(output_dir, "17_notes.txt"))

cat("===== R27B NATIVE REPRODUCTION =====\n\n")
print(r27b_check, n = Inf, width = Inf)

cat("\n===== R27C COMMON-SUPPORT REPRODUCTION =====\n\n")
print(r27c_check, n = Inf, width = Inf)

cat("\n===== UNION STACK REPRODUCES NATIVE COEFFICIENTS =====\n\n")
print(union_native_reproduction, n = Inf, width = Inf)

cat("\n===== NATIVE FOUR-IDEOLOGY COEFFICIENTS =====\n\n")
print(native_results, n = Inf, width = Inf)

cat("\n===== PRIMARY CENTER WALD TESTS: FULL NATIVE SAMPLES =====\n\n")
print(
  primary_union_pairwise |>
    filter(
      contrast_id %in%
        c("center_vs_left", "center_vs_right", "center_vs_mixed")
    ),
  n = Inf,
  width = Inf
)

cat("\n===== SECONDARY CENTER WALD TESTS: COMMON SUPPORT =====\n\n")
print(
  secondary_common_pairwise |>
    filter(
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

cat("\n===== PRIMARY FOUR-GROUP UNION OMNIBUS =====\n\n")
print(primary_union_omnibus, n = Inf, width = Inf)

cat("\n===== SECONDARY FOUR-GROUP COMMON-SUPPORT OMNIBUS =====\n\n")
print(secondary_common_omnibus, n = Inf, width = Inf)

cat("\nOUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R38B_V1_1_COMPLETE\n")

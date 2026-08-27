suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(lme4)
})

required_packages <- c(
  "dplyr",
  "purrr",
  "readr",
  "tibble",
  "tidyr",
  "lme4"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(missing_packages) >
    0L
) {
  stop(
    "Missing required packages: ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = getwd()
)

setwd(
  project_root
)

input_dir <- file.path(
  project_root,
  "data",
  "derived",
  "switchers_rewrite",
  "final"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "r38c3_native_voter_cluster_bootstrap_wald_v1_0"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

respondent_path <- file.path(
  input_dir,
  "nes_respondent_analysis.rds"
)

change_path <- file.path(
  input_dir,
  "ac_change.rds"
)

native_results_path <- file.path(
  project_root,
  "outputs",
  "r38c_voter_four_ideology_heterogeneity_v1_1",
  "02_native_four_ideology_coefficients.csv"
)

required_files <- c(
  respondent_path,
  change_path,
  native_results_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (
  length(missing_files) >
    0L
) {
  stop(
    "Missing required input(s): ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

respondents <- readRDS(
  respondent_path
)

ac_change <- readRDS(
  change_path
)

native_results <- read_csv(
  native_results_path,
  show_col_types = FALSE
)

require_columns <- function(
  data,
  columns,
  label
) {
  missing <- setdiff(
    columns,
    names(data)
  )

  if (
    length(missing) >
      0L
  ) {
    stop(
      label,
      " is missing required columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
}

individual_controls <- c(
  "religion_group",
  "caste_group",
  "education_harmonized"
)

primary_ac_controls <- c(
  "proxy_ac_pop",
  "sc_pop_share",
  "st_pop_share"
)

respondent_required <- c(
  "respondent_uid",
  "year",
  "state_no",
  "ac_uid",
  "vote_valid",
  "voted_bjp",
  "bjp_candidate_present",
  "fdi_spatial_support",
  "ideology_complete",
  "voter_ideology",
  "muslim_share_2001_dist_proxy",
  individual_controls,
  primary_ac_controls
)

require_columns(
  respondents,
  respondent_required,
  "nes_respondent_analysis"
)

fdi_variables <- c(
  "fdi_total_local_all_pc100k_2009",
  "fdi_total_local_all_pc100k_2014",
  "fdi_mfg_local_all_pc100k_2009",
  "fdi_mfg_local_all_pc100k_2014"
)

require_columns(
  ac_change,
  c(
    "ac_uid",
    fdi_variables
  ),
  "ac_change"
)

fdi_payload <- ac_change |>
  select(
    ac_uid,
    all_of(
      fdi_variables
    )
  )

if (
  anyDuplicated(
    fdi_payload$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change FDI payload is not unique by ac_uid."
  )
}

respondents <- respondents |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_payload,
    by = "ac_uid",
    relationship = "many-to-one"
  )

if (
  anyDuplicated(
    respondents$respondent_uid
  ) >
    0L
) {
  stop(
    "Respondent data are not unique by respondent_uid."
  )
}

relevel_if_present <- function(
  x,
  reference
) {
  out <- factor(
    as.character(x)
  )

  if (
    reference %in%
      levels(out)
  ) {
    out <- stats::relevel(
      out,
      ref = reference
    )
  }

  out
}

ideology_levels <- c(
  "Left",
  "Center",
  "Right",
  "Mixed"
)

respondents <- respondents |>
  mutate(
    y =
      as.numeric(
        voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
      ),

    total_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    total_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      ),

    mfg_current =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    mfg_baseline =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    ac_pop_100k =
      as.numeric(
        proxy_ac_pop
      ) /
      100000,

    sc_share_pp =
      100 *
      as.numeric(
        sc_pop_share
      ),

    st_share_pp =
      100 *
      as.numeric(
        st_pop_share
      ),

    state_fe =
      factor(
        state_no
      ),

    religion_x =
      relevel_if_present(
        religion_group,
        "1: Hindu"
      ),

    caste_x =
      relevel_if_present(
        caste_group,
        "4: Others"
      ),

    education_x =
      relevel_if_present(
        education_harmonized,
        "Secondary"
      ),

    ideology4 =
      factor(
        as.character(
          voter_ideology
        ),
        levels =
          ideology_levels
      )
  )

if (
  any(
    !is.na(
      respondents$y
    ) &
      !respondents$y %in%
        c(
          0,
          1
        )
  )
) {
  stop(
    "voted_bjp is not coded 0/1."
  )
}

analysis_base <- respondents |>
  filter(
    year ==
      2014,

    vote_valid %in%
      TRUE,

    !is.na(y),

    ideology_complete %in%
      TRUE,

    !is.na(
      ideology4
    ),

    bjp_candidate_present %in%
      TRUE,

    fdi_spatial_support %in%
      TRUE,

    is.finite(
      muslim
    ),

    is.finite(
      ac_pop_100k
    ),

    is.finite(
      sc_share_pp
    ),

    is.finite(
      st_share_pp
    ),

    !is.na(
      state_fe
    ),

    !is.na(
      religion_x
    ),

    !is.na(
      caste_x
    ),

    !is.na(
      education_x
    )
  )

cell_registry <- tribble(
  ~cell_id,
  ~sector,
  ~current_col,
  ~baseline_col,

  "total_raw",
  "Total",
  "total_current",
  "total_baseline",

  "manufacturing_raw",
  "Manufacturing",
  "mfg_current",
  "mfg_baseline"
)

pair_registry <- tribble(
  ~contrast_id,
  ~ideology_a,
  ~ideology_b,

  "center_vs_left",
  "Center",
  "Left",

  "center_vs_right",
  "Center",
  "Right",

  "center_vs_mixed",
  "Center",
  "Mixed",

  "left_vs_right",
  "Left",
  "Right",

  "left_vs_mixed",
  "Left",
  "Mixed",

  "right_vs_mixed",
  "Right",
  "Mixed"
)

bootstrap_reps <- as.integer(
  Sys.getenv(
    "R38C3_BOOTSTRAP_REPS",
    unset = "499"
  )
)

if (
  is.na(
    bootstrap_reps
  ) ||
  bootstrap_reps <
    99L
) {
  stop(
    "R38C3_BOOTSTRAP_REPS must be an integer >= 99."
  )
}

bootstrap_seed <- as.integer(
  Sys.getenv(
    "R38C3_BOOTSTRAP_SEED",
    unset = "20260825"
  )
)

if (
  is.na(
    bootstrap_seed
  )
) {
  stop(
    "R38C3_BOOTSTRAP_SEED must be an integer."
  )
}

detected_cores <- parallel::detectCores(
  logical = FALSE
)

if (
  is.na(
    detected_cores
  )
) {
  detected_cores <- 2L
}

default_cores <- max(
  1L,
  min(
    4L,
    detected_cores -
      1L
  )
)

bootstrap_cores <- as.integer(
  Sys.getenv(
    "R38C3_BOOTSTRAP_CORES",
    unset =
      as.character(
        default_cores
      )
  )
)

if (
  is.na(
    bootstrap_cores
  ) ||
  bootstrap_cores <
    1L
) {
  bootstrap_cores <- 1L
}

optimizer_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(
    maxfun = 200000
  )
)

native_formula_boot <- as.formula(
  paste0(
    "y ~ ",
    "muslim * fdi_current + ",
    "muslim * fdi_baseline + ",
    "ac_pop_100k + ",
    "sc_share_pp + ",
    "st_share_pp + ",
    "religion_x + ",
    "caste_x + ",
    "education_x + ",
    "state_fe + ",
    "(1 | ac_random_boot)"
  )
)

find_exact_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <- coefficient_names[
    vapply(
      strsplit(
        coefficient_names,
        ":",
        fixed = TRUE
      ),
      function(pieces) {
        length(pieces) ==
          length(variables) &&
          setequal(
            pieces,
            variables
          )
      },
      logical(1)
    )
  ]

  if (
    length(hits) !=
      1L
  ) {
    stop(
      "Could not uniquely identify interaction among: ",
      paste(
        variables,
        collapse = ", "
      ),
      ". Hits: ",
      paste(
        hits,
        collapse = ", "
      )
    )
  }

  hits[[1]]
}

fit_one_group <- function(
  data,
  ideology_group
) {
  dd <- data |>
    filter(
      as.character(
        ideology4
      ) ==
        ideology_group
    ) |>
    droplevels()

  warnings <- character()
  messages <- character()

  fit <- tryCatch(
    withCallingHandlers(
      lmer(
        native_formula_boot,
        data = dd,
        REML = FALSE,
        control =
          optimizer_control
      ),
      warning = function(w) {
        warnings <<- c(
          warnings,
          conditionMessage(w)
        )
        invokeRestart(
          "muffleWarning"
        )
      },
      message = function(m) {
        messages <<- c(
          messages,
          conditionMessage(m)
        )
        invokeRestart(
          "muffleMessage"
        )
      }
    ),
    error = function(e) {
      e
    }
  )

  if (
    inherits(
      fit,
      "error"
    )
  ) {
    return(
      tibble(
        ideology =
          ideology_group,
        estimate =
          NA_real_,
        singular =
          NA,
        n_voters =
          nrow(dd),
        n_boot_ac =
          n_distinct(
            dd$ac_random_boot
          ),
        converged =
          FALSE,
        error_message =
          conditionMessage(fit),
        warnings =
          if (
            length(warnings) ==
              0L
          ) {
            NA_character_
          } else {
            paste(
              unique(warnings),
              collapse = " | "
            )
          }
      )
    )
  }

  beta_names <- names(
    fixef(fit)
  )

  focal_term <-
    find_exact_interaction_term(
      beta_names,
      c(
        "muslim",
        "fdi_current"
      )
    )

  conv_messages <-
    fit@optinfo$conv$lme4$messages

  tibble(
    ideology =
      ideology_group,
    estimate =
      unname(
        fixef(fit)[
          focal_term
        ]
      ),
    singular =
      isSingular(
        fit,
        tol = 1e-4
      ),
    n_voters =
      nrow(dd),
    n_boot_ac =
      n_distinct(
        dd$ac_random_boot
      ),
    converged =
      is.null(
        conv_messages
      ),
    error_message =
      if (
        is.null(
          conv_messages
        )
      ) {
        NA_character_
      } else {
        paste(
          unique(
            as.character(
              conv_messages
            )
          ),
          collapse = " | "
        )
      },
    warnings =
      if (
        length(warnings) ==
          0L
      ) {
        NA_character_
      } else {
        paste(
          unique(warnings),
          collapse = " | "
        )
      }
  )
}

analysis_cells <- list()

for (
  i in
  seq_len(
    nrow(cell_registry)
  )
) {
  spec <- cell_registry[
    i,
    ,
    drop = FALSE
  ]

  dd <- analysis_base |>
    mutate(
      fdi_current =
        .data[[spec$current_col]],
      fdi_baseline =
        .data[[spec$baseline_col]]
    ) |>
    filter(
      is.finite(
        fdi_current
      ),
      is.finite(
        fdi_baseline
      )
    ) |>
    mutate(
      ac_uid_character =
        as.character(
          ac_uid
        )
    )

  analysis_cells[[spec$cell_id]] <- dd
}

all_union_ac_ids <- sort(
  unique(
    unlist(
      lapply(
        analysis_cells,
        function(dd) {
          as.character(
            dd$ac_uid
          )
        }
      ),
      use.names = FALSE
    )
  )
)

n_union_ac <- length(
  all_union_ac_ids
)

if (
  n_union_ac <
    2L
) {
  stop(
    "Fewer than two ACs in the bootstrap universe."
  )
}

fit_bootstrap_replicate <- function(
  replicate_id
) {
  set.seed(
    bootstrap_seed +
      replicate_id *
      1009L
  )

  draw_ids <- sample(
    all_union_ac_ids,
    size =
      n_union_ac,
    replace = TRUE
  )

  draw_map <- tibble(
    ac_uid_character =
      draw_ids,
    bootstrap_copy =
      seq_along(
        draw_ids
      )
  )

  results <- list()

  for (
    i in
    seq_len(
      nrow(cell_registry)
    )
  ) {
    spec <- cell_registry[
      i,
      ,
      drop = FALSE
    ]

    source_data <-
      analysis_cells[[spec$cell_id]]

    boot_data <- draw_map |>
      inner_join(
        source_data,
        by =
          "ac_uid_character",
        relationship =
          "many-to-many"
      ) |>
      mutate(
        ac_random_boot =
          factor(
            paste0(
              ac_uid_character,
              "__copy_",
              bootstrap_copy
            )
          )
      )

    for (
      g in
      ideology_levels
    ) {
      fit_result <-
        fit_one_group(
          boot_data,
          g
        )

      results[[paste(
        spec$cell_id,
        g,
        sep = "__"
      )]] <- fit_result |>
        mutate(
          replicate =
            replicate_id,
          cell_id =
            spec$cell_id,
          sector =
            spec$sector,
          .before = 1
        )
    }
  }

  bind_rows(
    results
  )
}

set.seed(
  bootstrap_seed
)

replicate_ids <- seq_len(
  bootstrap_reps
)

if (
  .Platform$OS.type ==
    "unix" &&
  bootstrap_cores >
    1L
) {
  bootstrap_list <- parallel::mclapply(
    replicate_ids,
    fit_bootstrap_replicate,
    mc.cores =
      bootstrap_cores,
    mc.preschedule =
      FALSE,
    mc.set.seed =
      TRUE
  )
} else {
  bootstrap_list <- lapply(
    replicate_ids,
    fit_bootstrap_replicate
  )
}

bootstrap_long <- bind_rows(
  bootstrap_list
)

write_csv(
  bootstrap_long,
  file.path(
    output_dir,
    "01_bootstrap_replicate_coefficients_long.csv"
  )
)

bootstrap_status <- bootstrap_long |>
  group_by(
    cell_id,
    sector,
    ideology
  ) |>
  summarise(
    requested_reps =
      bootstrap_reps,
    successful_fits =
      sum(
        is.finite(
          estimate
        )
      ),
    success_rate =
      mean(
        is.finite(
          estimate
        )
      ),
    singular_fits =
      sum(
        singular %in%
          TRUE,
        na.rm = TRUE
      ),
    singular_rate_among_successes =
      ifelse(
        successful_fits >
          0,
        singular_fits /
          successful_fits,
        NA_real_
      ),
    convergence_flag_rate =
      mean(
        converged %in%
          TRUE,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

write_csv(
  bootstrap_status,
  file.path(
    output_dir,
    "02_bootstrap_fit_status_summary.csv"
  )
)

bootstrap_wide <- bootstrap_long |>
  select(
    replicate,
    cell_id,
    sector,
    ideology,
    estimate
  ) |>
  pivot_wider(
    names_from =
      ideology,
    values_from =
      estimate
  )

complete_replicates <- bootstrap_wide |>
  filter(
    if_all(
      all_of(
        ideology_levels
      ),
      is.finite
    )
  )

complete_rep_summary <- complete_replicates |>
  count(
    cell_id,
    sector,
    name =
      "complete_four_group_replicates"
  ) |>
  mutate(
    requested_reps =
      bootstrap_reps,
    complete_rate =
      complete_four_group_replicates /
      requested_reps
  )

write_csv(
  complete_rep_summary,
  file.path(
    output_dir,
    "03_complete_replicate_summary.csv"
  )
)

if (
  any(
    complete_rep_summary$complete_rate <
      .80
  )
) {
  print(
    complete_rep_summary,
    n = Inf,
    width = Inf
  )

  stop(
    "Fewer than 80% of bootstrap replicates contain all four ideology coefficients for at least one cell."
  )
}

native_primary <- native_results |>
  filter(
    functional_form ==
      "Raw",
    cell_id %in%
      cell_registry$cell_id,
    ideology %in%
      ideology_levels
  ) |>
  select(
    cell_id,
    sector,
    ideology,
    native_estimate =
      estimate,
    native_std_error =
      std_error,
    native_p_value =
      p_value,
    native_n_voters =
      n_voters,
    native_n_ac =
      n_ac,
    native_singular =
      singular
  )

bootstrap_covariances <- list()
pairwise_results <- list()
omnibus_results <- list()

for (
  i in
  seq_len(
    nrow(cell_registry)
  )
) {
  spec <- cell_registry[
    i,
    ,
    drop = FALSE
  ]

  boot_cell <- complete_replicates |>
    filter(
      cell_id ==
        spec$cell_id
    )

  B_complete <- nrow(
    boot_cell
  )

  coef_matrix <- as.matrix(
    boot_cell[
      ideology_levels
    ]
  )

  covariance_matrix <- cov(
    coef_matrix
  )

  covariance_long <- as.data.frame(
    as.table(
      covariance_matrix
    )
  ) |>
    as_tibble() |>
    rename(
      ideology_a =
        Var1,
      ideology_b =
        Var2,
      covariance =
        Freq
    ) |>
    mutate(
      cell_id =
        spec$cell_id,
      sector =
        spec$sector,
      complete_replicates =
        B_complete,
      .before = 1
    )

  bootstrap_covariances[[spec$cell_id]] <- covariance_long

  native_cell <- native_primary |>
    filter(
      cell_id ==
        spec$cell_id
    )

  original_beta <- setNames(
    native_cell$native_estimate[
      match(
        ideology_levels,
        native_cell$ideology
      )
    ],
    ideology_levels
  )

  for (
    j in
    seq_len(
      nrow(pair_registry)
    )
  ) {
    pair <- pair_registry[
      j,
      ,
      drop = FALSE
    ]

    a <- pair$ideology_a
    b <- pair$ideology_b

    observed_difference <-
      original_beta[[a]] -
      original_beta[[b]]

    boot_difference <-
      boot_cell[[a]] -
      boot_cell[[b]]

    bootstrap_se <- sd(
      boot_difference
    )

    z <- observed_difference /
      bootstrap_se

    wald_p <- 2 *
      pnorm(
        abs(z),
        lower.tail = FALSE
      )

    percentile_ci <- quantile(
      boot_difference,
      c(
        .025,
        .975
      ),
      na.rm = TRUE,
      names = FALSE,
      type = 8
    )

    basic_ci <- c(
      2 *
        observed_difference -
        percentile_ci[[2]],
      2 *
        observed_difference -
        percentile_ci[[1]]
    )

    centered_difference <-
      boot_difference -
      mean(
        boot_difference
      )

    centered_bootstrap_p <-
      (
        1 +
          sum(
            abs(
              centered_difference
            ) >=
              abs(
                observed_difference
              )
          )
      ) /
      (
        B_complete +
          1
      )

    approximate_mde_80pct <-
      (
        qnorm(.975) +
          qnorm(.80)
      ) *
      bootstrap_se

    pairwise_results[[paste(
      spec$cell_id,
      pair$contrast_id,
      sep = "__"
    )]] <- tibble(
      cell_id =
        spec$cell_id,
      sector =
        spec$sector,
      contrast_id =
        pair$contrast_id,
      ideology_a =
        a,
      ideology_b =
        b,

      native_estimate_a =
        original_beta[[a]],
      native_estimate_b =
        original_beta[[b]],
      observed_difference =
        observed_difference,

      bootstrap_se_difference =
        bootstrap_se,

      wald_z =
        z,

      wald_p_normal =
        wald_p,

      percentile_ci_low =
        percentile_ci[[1]],
      percentile_ci_high =
        percentile_ci[[2]],

      basic_ci_low =
        basic_ci[[1]],
      basic_ci_high =
        basic_ci[[2]],

      centered_bootstrap_p_diagnostic =
        centered_bootstrap_p,

      approximate_mde_80pct_power =
        approximate_mde_80pct,

      observed_to_mde_ratio =
        abs(
          observed_difference
        ) /
        approximate_mde_80pct,

      complete_bootstrap_replicates =
        B_complete
    )
  }

  R <- matrix(
    0,
    nrow = 3,
    ncol = 4,
    dimnames = list(
      c(
        "CenterMinusLeft",
        "RightMinusLeft",
        "MixedMinusLeft"
      ),
      ideology_levels
    )
  )

  R[
    "CenterMinusLeft",
    "Center"
  ] <- 1

  R[
    "CenterMinusLeft",
    "Left"
  ] <- -1

  R[
    "RightMinusLeft",
    "Right"
  ] <- 1

  R[
    "RightMinusLeft",
    "Left"
  ] <- -1

  R[
    "MixedMinusLeft",
    "Mixed"
  ] <- 1

  R[
    "MixedMinusLeft",
    "Left"
  ] <- -1

  d <- as.numeric(
    R %*%
      original_beta
  )

  S <- R %*%
    covariance_matrix %*%
    t(R)

  S <- (
    S +
      t(S)
  ) /
    2

  eig <- eigen(
    S,
    symmetric = TRUE
  )

  eigenvalues <-
    eig$values

  max_eig <- max(
    abs(
      eigenvalues
    )
  )

  tolerance <- if (
    max_eig ==
      0
  ) {
    0
  } else {
    max_eig *
      1e-10
  }

  keep <- eigenvalues >
    tolerance

  effective_rank <- sum(
    keep
  )

  if (
    effective_rank ==
      3L
  ) {
    omnibus_stat <-
      as.numeric(
        t(d) %*%
          solve(
            S,
            d
          )
      )

    omnibus_p <-
      pchisq(
        omnibus_stat,
        df = 3,
        lower.tail = FALSE
      )
  } else {
    omnibus_stat <-
      NA_real_

    omnibus_p <-
      NA_real_
  }

  omnibus_results[[spec$cell_id]] <- tibble(
    cell_id =
      spec$cell_id,
    sector =
      spec$sector,

    center_minus_left =
      d[[1]],
    right_minus_left =
      d[[2]],
    mixed_minus_left =
      d[[3]],

    covariance_effective_rank =
      effective_rank,

    omnibus_full_rank =
      effective_rank ==
        3L,

    omnibus_wald_chisq =
      omnibus_stat,

    omnibus_df =
      if (
        effective_rank ==
          3L
      ) {
        3L
      } else {
        NA_integer_
      },

    omnibus_p =
      omnibus_p,

    min_restriction_eigenvalue =
      min(
        eigenvalues
      ),

    max_restriction_eigenvalue =
      max(
        eigenvalues
      ),

    complete_bootstrap_replicates =
      B_complete
  )
}

bootstrap_covariances <-
  bind_rows(
    bootstrap_covariances
  )

pairwise_results <-
  bind_rows(
    pairwise_results
  )

omnibus_results <-
  bind_rows(
    omnibus_results
  )

write_csv(
  native_primary,
  file.path(
    output_dir,
    "04_native_point_estimates_used_for_bootstrap_wald.csv"
  )
)

write_csv(
  bootstrap_covariances,
  file.path(
    output_dir,
    "05_bootstrap_covariance_matrix_long.csv"
  )
)

write_csv(
  pairwise_results,
  file.path(
    output_dir,
    "06_PRIMARY_native_model_pairwise_cluster_bootstrap_wald_tests.csv"
  )
)

write_csv(
  omnibus_results,
  file.path(
    output_dir,
    "07_PRIMARY_native_model_four_group_bootstrap_omnibus.csv"
  )
)

configuration <- tibble(
  bootstrap_reps =
    bootstrap_reps,
  bootstrap_seed =
    bootstrap_seed,
  bootstrap_cores =
    bootstrap_cores,
  union_ac_bootstrap_universe =
    n_union_ac,
  bootstrap_unit =
    "Assembly constituency",
  bootstrap_type =
    "Nonparametric cluster bootstrap with replacement",
  primary_cells =
    "Total raw; Manufacturing raw"
)

write_csv(
  configuration,
  file.path(
    output_dir,
    "08_bootstrap_configuration.csv"
  )
)

notes <- c(
  "R38C3 — NATIVE VOTER-MODEL AC-CLUSTER BOOTSTRAP WALD TESTS",
  "",
  "Purpose:",
  "Obtain valid covariance estimates for comparisons among the four separately fitted native ideology-specific voter mixed models.",
  "",
  "Why bootstrap:",
  "The native Center, Left, Right, and Mixed models are fitted separately. Their coefficients are statistically dependent because ideology groups can occupy the same assembly constituencies and share the same constituency-level FDI, Muslim-share, and AC covariates.",
  "The ordinary regression tables do not provide the cross-model covariance needed for Var(beta_A - beta_B).",
  "",
  "Bootstrap design:",
  "Assembly constituencies are sampled with replacement from the union AC universe.",
  "All eligible respondents from each sampled AC are carried into the bootstrap replicate.",
  "Repeated draws of the same original AC receive distinct bootstrap-cluster IDs.",
  "Within each replicate, the four separate native mixed-LPM specifications are refit, preserving their separate-model structure.",
  "",
  "Primary inference:",
  "For each pair, bootstrap covariance is used to estimate the standard error of the observed native coefficient difference.",
  "A normal-approximation Wald p-value and percentile/basic bootstrap confidence intervals are reported.",
  "A centered-bootstrap p-value is included as a diagnostic rather than silently substituted for the Wald p-value.",
  "",
  "Power diagnostic:",
  "The approximate 80%-power detectable difference is (1.96 + 0.84) times the bootstrap SE of the coefficient difference.",
  "It describes the scale of difference the current design can detect with about 80% power under a normal approximation; it is not an estimate of the true difference.",
  "",
  "Default computation:",
  "499 bootstrap replicates. For final publication inference, rerun with at least 1999 replicates if computationally feasible:",
  "R38C3_BOOTSTRAP_REPS=1999 bash ...",
  "",
  "Singularity:",
  "Singular native Left fits are expected from prior diagnostics. Singular bootstrap fits are retained when the model returns a finite fixed-effect estimate and are separately audited."
)

writeLines(
  notes,
  file.path(
    output_dir,
    "09_notes.txt"
  )
)

cat(
  "===== R38C3 CONFIGURATION =====\n\n"
)

print(
  configuration,
  n = Inf,
  width = Inf
)

cat(
  "\n===== BOOTSTRAP FIT STATUS =====\n\n"
)

print(
  bootstrap_status,
  n = Inf,
  width = Inf
)

cat(
  "\n===== COMPLETE REPLICATE SUMMARY =====\n\n"
)

print(
  complete_rep_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY NATIVE-MODEL BOOTSTRAP WALD TESTS =====\n\n"
)

print(
  pairwise_results,
  n = Inf,
  width = Inf
)

cat(
  "\n===== NATIVE-MODEL BOOTSTRAP OMNIBUS =====\n\n"
)

print(
  omnibus_results,
  n = Inf,
  width = Inf
)

cat(
  "\nOUTPUT_DIR=",
  output_dir,
  "\n",
  sep = ""
)

cat(
  "R38C3_COMPLETE\n"
)

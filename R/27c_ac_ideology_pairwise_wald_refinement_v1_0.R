suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(fixest)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

input_dir <-
  file.path(
    project_root,
    "data",
    "derived",
    "switchers_rewrite",
    "final"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "ac_ideology_pairwise_wald_refinement_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ideology <-
  readRDS(
    file.path(
      input_dir,
      "ac_year_ideology_summary.rds"
    )
  )

ac_change <-
  readRDS(
    file.path(
      input_dir,
      "ac_change.rds"
    )
  )

r27b_results <-
  read_csv(
    file.path(
      "outputs",
      "ac_ideology_outcome_heterogeneity_v1_0",
      "02_native_ideology_coefficients.csv"
    ),
    show_col_types = FALSE
  )

primary_controls <-
  c(
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share"
  )

fdi_variables <-
  c(
    "fdi_total_local_all_pc100k_2009",
    "fdi_total_local_all_pc100k_2014",
    "log1p_fdi_total_local_all_pc100k_2009",
    "log1p_fdi_total_local_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2009",
    "fdi_mfg_local_all_pc100k_2014"
  )

fdi_source <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      fdi_variables
    )
  )

if (
  anyDuplicated(
    fdi_source$ac_uid
  ) >
    0L
) {
  stop(
    "FDI source is not unique by ac_uid."
  )
}

analysis_base <-
  ideology |>
  filter(
    year ==
      2014,
    as.character(
      ideology
    ) %in%
      c(
        "Left",
        "Center",
        "Right"
      )
  ) |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_source,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    ideology_name =
      as.character(
        ideology
      ),

    y =
      as.numeric(
        weighted_share_voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
      ),

    total_raw_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    total_raw_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      ),

    total_log_current =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2014
      ),

    total_log_baseline =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2009
      ),

    mfg_raw_current =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    mfg_raw_baseline =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    mfg_log_current =
      log1p(
        mfg_raw_current
      ),

    mfg_log_baseline =
      log1p(
        mfg_raw_baseline
      )
  )

if (
  anyDuplicated(
    analysis_base[
      c(
        "ac_uid",
        "ideology_name"
      )
    ]
  ) >
    0L
) {
  stop(
    "Analysis base is not unique by AC x ideology."
  )
}

cell_registry <-
  tribble(
    ~cell_id, ~sector, ~functional_form, ~current_col, ~baseline_col,

    "total_raw",
    "Total",
    "Raw",
    "total_raw_current",
    "total_raw_baseline",

    "total_log1p",
    "Total",
    "log1p",
    "total_log_current",
    "total_log_baseline",

    "manufacturing_raw",
    "Manufacturing",
    "Raw",
    "mfg_raw_current",
    "mfg_raw_baseline",

    "manufacturing_log1p",
    "Manufacturing",
    "log1p",
    "mfg_log_current",
    "mfg_log_baseline"
  )

make_sample <- function(
  data,
  ideology_value,
  current_col,
  baseline_col
) {
  data |>
    filter(
      ideology_name ==
        ideology_value
    ) |>
    mutate(
      fdi_current =
        .data[[
          current_col
        ]],

      fdi_baseline =
        .data[[
          baseline_col
        ]]
    ) |>
    filter(
      !is.na(
        y
      ),
      bjp_candidate_present %in%
        TRUE,
      fdi_spatial_support %in%
        TRUE,
      is.finite(
        muslim
      ),
      is.finite(
        fdi_current
      ),
      is.finite(
        fdi_baseline
      ),
      if_all(
        all_of(
          primary_controls
        ),
        ~ !is.na(.x)
      ),
      !is.na(
        state_no
      ),
      !is.na(
        pc_cluster_id
      )
    )
}

fit_separate <- function(
  data
) {
  feols(
    y ~
      muslim *
        fdi_current +
      muslim *
        fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share |
      state_no,
    data =
      data,
    vcov =
      ~ pc_cluster_id,
    warn =
      FALSE,
    notes =
      FALSE
  )
}

extract_focal <- function(
  fit
) {
  term_names <-
    names(
      coef(
        fit
      )
    )

  candidates <-
    c(
      "muslim:fdi_current",
      "fdi_current:muslim"
    )

  term <-
    intersect(
      candidates,
      term_names
    )

  if (
    length(
      term
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify focal interaction."
    )
  }

  ct <-
    coeftable(
      fit
    )

  tibble(
    estimate =
      unname(
        ct[
          term,
          1
        ]
      ),

    std_error =
      unname(
        ct[
          term,
          2
        ]
      ),

    p_value =
      unname(
        ct[
          term,
          4
        ]
      )
  )
}

fit_stacked <- function(
  samples,
  ideology_values
) {
  common_ids <-
    Reduce(
      intersect,
      lapply(
        samples,
        function(
          dd
        ) {
          as.character(
            dd$ac_uid
          )
        }
      )
    )

  common_ids <-
    sort(
      unique(
        common_ids
      )
    )

  if (
    length(
      common_ids
    ) ==
      0L
  ) {
    stop(
      "No common ACs for stacked comparison."
    )
  }

  pieces <-
    vector(
      "list",
      length(
        ideology_values
      )
    )

  names(
    pieces
  ) <-
    ideology_values

  for (
    ideology_value in
      ideology_values
  ) {
    dd <-
      samples[[
        ideology_value
      ]] |>
      filter(
        as.character(
          ac_uid
        ) %in%
          common_ids
      )

    dd <-
      dd[
        match(
          common_ids,
          as.character(
            dd$ac_uid
          )
        ),
        ,
        drop = FALSE
      ]

    if (
      !identical(
        as.character(
          dd$ac_uid
        ),
        common_ids
      )
    ) {
      stop(
        "Common-sample ordering failure."
      )
    }

    pieces[[
      ideology_value
    ]] <-
      dd |>
      mutate(
        ideology_stack =
          ideology_value
      )
  }

  stacked <-
    bind_rows(
      pieces
    )

  slugs <-
    setNames(
      tolower(
        ideology_values
      ),
      ideology_values
    )

  for (
    ideology_value in
      ideology_values
  ) {
    slug <-
      slugs[[
        ideology_value
      ]]

    indicator <-
      as.numeric(
        stacked$ideology_stack ==
          ideology_value
      )

    stacked[[
      paste0(
        slug,
        "_muslim"
      )
    ]] <-
      indicator *
        stacked$muslim

    stacked[[
      paste0(
        slug,
        "_current"
      )
    ]] <-
      indicator *
        stacked$fdi_current

    stacked[[
      paste0(
        slug,
        "_baseline"
      )
    ]] <-
      indicator *
        stacked$fdi_baseline

    stacked[[
      paste0(
        slug,
        "_current_muslim"
      )
    ]] <-
      indicator *
        stacked$fdi_current *
        stacked$muslim

    stacked[[
      paste0(
        slug,
        "_baseline_muslim"
      )
    ]] <-
      indicator *
        stacked$fdi_baseline *
        stacked$muslim

    stacked[[
      paste0(
        slug,
        "_proxy_ac_pop"
      )
    ]] <-
      indicator *
        stacked$proxy_ac_pop

    stacked[[
      paste0(
        slug,
        "_sc_pop_share"
      )
    ]] <-
      indicator *
        stacked$sc_pop_share

    stacked[[
      paste0(
        slug,
        "_st_pop_share"
      )
    ]] <-
      indicator *
        stacked$st_pop_share
  }

  stacked <-
    stacked |>
    mutate(
      state_ideology_fe =
        interaction(
          state_no,
          ideology_stack,
          drop =
            TRUE,
          lex.order =
            TRUE
        )
    )

  slope_terms <-
    unlist(
      lapply(
        tolower(
          ideology_values
        ),
        function(
          slug
        ) {
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

  fml <-
    as.formula(
      paste0(
        "y ~ 0 + ",
        paste(
          slope_terms,
          collapse =
            " + "
        ),
        " | state_ideology_fe"
      )
    )

  fit <-
    feols(
      fml,
      data =
        stacked,
      vcov =
        ~ pc_cluster_id,
      warn =
        FALSE,
      notes =
        FALSE
    )

  list(
    fit =
      fit,

    data =
      stacked,

    common_ids =
      common_ids
  )
}

single_difference_test <- function(
  fit,
  term_a,
  term_b,
  n_clusters
) {
  beta <-
    coef(
      fit
    )

  V <-
    as.matrix(
      vcov(
        fit
      )
    )

  if (
    !all(
      c(
        term_a,
        term_b
      ) %in%
        names(
          beta
        )
    )
  ) {
    stop(
      "Pairwise focal terms absent."
    )
  }

  estimate <-
    unname(
      beta[
        term_a
      ] -
        beta[
          term_b
        ]
    )

  variance <-
    V[
      term_a,
      term_a
    ] +
    V[
      term_b,
      term_b
    ] -
    2 *
      V[
        term_a,
        term_b
      ]

  se <-
    sqrt(
      max(
        variance,
        0
      )
    )

  z2 <-
    (
      estimate /
        se
    )^2

  df2 <-
    max(
      n_clusters -
        1L,
      1L
    )

  tibble(
    difference =
      estimate,

    std_error =
      se,

    wald_chisq =
      z2,

    chi_square_p =
      pchisq(
        z2,
        df =
          1,
        lower.tail =
          FALSE
      ),

    wald_F =
      z2,

    F_df1 =
      1L,

    F_df2 =
      df2,

    cluster_df_F_p =
      pf(
        z2,
        df1 =
          1,
        df2 =
          df2,
        lower.tail =
          FALSE
      )
  )
}

omnibus_test <- function(
  fit,
  terms,
  n_clusters
) {
  beta <-
    coef(
      fit
    )

  V <-
    as.matrix(
      vcov(
        fit
      )
    )

  center_term <-
    terms[[
      "Center"
    ]]

  left_term <-
    terms[[
      "Left"
    ]]

  right_term <-
    terms[[
      "Right"
    ]]

  if (
    !all(
      c(
        center_term,
        left_term,
        right_term
      ) %in%
        names(
          beta
        )
    )
  ) {
    stop(
      "Omnibus focal terms absent."
    )
  }

  R <-
    matrix(
      0,
      nrow =
        2,
      ncol =
        length(
          beta
        ),
      dimnames =
        list(
          c(
            "CenterMinusLeft",
            "CenterMinusRight"
          ),
          names(
            beta
          )
        )
    )

  R[
    1,
    center_term
  ] <-
    1

  R[
    1,
    left_term
  ] <-
    -1

  R[
    2,
    center_term
  ] <-
    1

  R[
    2,
    right_term
  ] <-
    -1

  d <-
    as.numeric(
      R %*%
        beta
    )

  S <-
    R %*%
      V %*%
      t(
        R
      )

  eigenvalues <-
    eigen(
      S,
      symmetric =
        TRUE,
      only.values =
        TRUE
    )$values

  if (
    min(
      eigenvalues
    ) <=
      0
  ) {
    stop(
      "Omnibus restriction covariance is not positive definite."
    )
  }

  statistic <-
    as.numeric(
      t(
        d
      ) %*%
        solve(
          S,
          d
        )
    )

  q <-
    2L

  df2 <-
    max(
      n_clusters -
        1L,
      1L
    )

  restriction_correlation <-
    S[
      1,
      2
    ] /
    sqrt(
      S[
        1,
        1
      ] *
        S[
          2,
          2
        ]
    )

  tibble(
    center_minus_left =
      d[[1]],

    center_minus_right =
      d[[2]],

    wald_chisq =
      statistic,

    chi_square_df =
      q,

    chi_square_p =
      pchisq(
        statistic,
        df =
          q,
        lower.tail =
          FALSE
      ),

    wald_F =
      statistic /
        q,

    F_df1 =
      q,

    F_df2 =
      df2,

    cluster_df_F_p =
      pf(
        statistic /
          q,
        df1 =
          q,
        df2 =
          df2,
        lower.tail =
          FALSE
      ),

    restriction_covariance_correlation =
      restriction_correlation,

    restriction_covariance_min_eigenvalue =
      min(
        eigenvalues
      ),

    restriction_covariance_max_eigenvalue =
      max(
        eigenvalues
      ),

    restriction_covariance_condition_number =
      max(
        eigenvalues
      ) /
      min(
        eigenvalues
      )
  )
}

pair_registry <-
  tribble(
    ~contrast_id, ~ideology_a, ~ideology_b,

    "center_vs_left",
    "Center",
    "Left",

    "center_vs_right",
    "Center",
    "Right",

    "left_vs_right",
    "Left",
    "Right"
  )

pairwise_results <-
  list()

pairwise_models <-
  list()

pairwise_membership <-
  list()

omnibus_results <-
  list()

omnibus_models <-
  list()

for (
  i in
    seq_len(
      nrow(
        cell_registry
      )
    )
) {
  spec <-
    cell_registry[
      i,
      ,
      drop = FALSE
    ]

  ideology_samples <-
    list()

  for (
    ideology_value in
      c(
        "Left",
        "Center",
        "Right"
      )
  ) {
    ideology_samples[[
      ideology_value
    ]] <-
      make_sample(
        analysis_base,
        ideology_value,
        spec$current_col,
        spec$baseline_col
      )
  }

  for (
    j in
      seq_len(
        nrow(
          pair_registry
        )
      )
  ) {
    pair <-
      pair_registry[
        j,
        ,
        drop = FALSE
      ]

    pair_values <-
      c(
        pair$ideology_a,
        pair$ideology_b
      )

    pair_samples <-
      ideology_samples[
        pair_values
      ]

    stacked_object <-
      fit_stacked(
        pair_samples,
        pair_values
      )

    fit <-
      stacked_object$fit

    slug_a <-
      tolower(
        pair$ideology_a
      )

    slug_b <-
      tolower(
        pair$ideology_b
      )

    term_a <-
      paste0(
        slug_a,
        "_current_muslim"
      )

    term_b <-
      paste0(
        slug_b,
        "_current_muslim"
      )

    n_clusters <-
      n_distinct(
        stacked_object$data$pc_cluster_id
      )

    test <-
      single_difference_test(
        fit,
        term_a,
        term_b,
        n_clusters
      )

    key <-
      paste(
        spec$cell_id,
        pair$contrast_id,
        sep =
          "__"
      )

    pairwise_models[[
      key
    ]] <-
      fit

    pairwise_results[[
      key
    ]] <-
      test |>
      mutate(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        functional_form =
          spec$functional_form,

        contrast_id =
          pair$contrast_id,

        ideology_a =
          pair$ideology_a,

        ideology_b =
          pair$ideology_b,

        n_common_ac =
          length(
            stacked_object$common_ids
          ),

        n_stacked_rows =
          nrow(
            stacked_object$data
          ),

        n_states =
          n_distinct(
            stacked_object$data$state_no
          ),

        n_pc_clusters =
          n_clusters,

        .before =
          1
      )

    pairwise_membership[[
      key
    ]] <-
      tibble(
        cell_id =
          spec$cell_id,

        contrast_id =
          pair$contrast_id,

        ac_uid =
          stacked_object$common_ids
      )
  }

  omnibus_object <-
    fit_stacked(
      ideology_samples,
      c(
        "Left",
        "Center",
        "Right"
      )
    )

  omnibus_fit <-
    omnibus_object$fit

  n_clusters <-
    n_distinct(
      omnibus_object$data$pc_cluster_id
    )

  omnibus_i <-
    omnibus_test(
      omnibus_fit,
      c(
        Left =
          "left_current_muslim",

        Center =
          "center_current_muslim",

        Right =
          "right_current_muslim"
      ),
      n_clusters
    ) |>
    mutate(
      cell_id =
        spec$cell_id,

      sector =
        spec$sector,

      functional_form =
        spec$functional_form,

      n_common_ac =
        length(
          omnibus_object$common_ids
        ),

      n_stacked_rows =
        nrow(
          omnibus_object$data
        ),

      n_states =
        n_distinct(
          omnibus_object$data$state_no
        ),

      n_pc_clusters =
        n_clusters,

      .before =
        1
    )

  omnibus_results[[
    spec$cell_id
  ]] <-
    omnibus_i

  omnibus_models[[
    spec$cell_id
  ]] <-
    omnibus_fit
}

pairwise_results <-
  bind_rows(
    pairwise_results
  )

pairwise_membership <-
  bind_rows(
    pairwise_membership
  )

omnibus_results <-
  bind_rows(
    omnibus_results
  )

previous_pairwise <-
  read_csv(
    file.path(
      "outputs",
      "ac_ideology_outcome_heterogeneity_v1_0",
      "04_ideology_coefficient_wald_tests.csv"
    ),
    show_col_types = FALSE
  ) |>
  filter(
    contrast_id %in%
      c(
        "center_vs_left",
        "center_vs_right",
        "left_vs_right"
      )
  ) |>
  select(
    cell_id,
    contrast_id,
    old_three_way_intersection_n =
      n_common_ac,
    old_three_way_intersection_p =
      p_value
  )

pairwise_comparison <-
  pairwise_results |>
  left_join(
    previous_pairwise,
    by =
      c(
        "cell_id",
        "contrast_id"
      )
  ) |>
  mutate(
    additional_pairwise_ACs =
      n_common_ac -
        old_three_way_intersection_n
  )

write_csv(
  pairwise_results,
  file.path(
    output_dir,
    "01_pairwise_common_sample_wald_tests.csv"
  )
)

write_csv(
  omnibus_results,
  file.path(
    output_dir,
    "02_three_ideology_omnibus_wald_tests.csv"
  )
)

write_csv(
  pairwise_comparison,
  file.path(
    output_dir,
    "03_pairwise_vs_three_way_intersection_comparison.csv"
  )
)

write_csv(
  pairwise_membership,
  file.path(
    output_dir,
    "04_pairwise_common_sample_membership.csv"
  )
)

saveRDS(
  pairwise_models,
  file.path(
    output_dir,
    "05_pairwise_stacked_models.rds"
  )
)

saveRDS(
  omnibus_models,
  file.path(
    output_dir,
    "06_omnibus_stacked_models.rds"
  )
)

notes <-
  c(
    "POST-PRIMARY AC IDEOLOGY PAIRWISE WALD REFINEMENT",
    "",
    "R27b used the three-ideology intersection for every Wald comparison.",
    "This refinement uses the maximal pair-specific common sample for each pairwise comparison.",
    "",
    "Center vs Left uses ACs where Center and Left outcomes are both estimable.",
    "Center vs Right uses ACs where Center and Right outcomes are both estimable.",
    "Left vs Right uses ACs where Left and Right outcomes are both estimable.",
    "The omnibus Center = Left = Right test necessarily retains the three-way intersection.",
    "",
    "All stacked models retain ideology-specific slopes for Muslim share, current FDI, baseline FDI, both FDI x Muslim interactions, and the primary AC controls, plus ideology-specific state fixed effects.",
    "Inference remains clustered by pc_cluster_id.",
    "",
    "Both asymptotic chi-square p-values and an F sensitivity using denominator df = number of PC clusters minus one are reported.",
    "The omnibus output also reports the eigenvalues, condition number, and correlation of the restriction covariance matrix.",
    "",
    "No cell-size cutoff is introduced.",
    "This remains a post-primary heterogeneity diagnostic."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "07_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "08_session_info.txt"
  )
)

cat(
  "\n===== PAIRWISE COMMON-SAMPLE WALD TESTS =====\n"
)

print(
  pairwise_results,
  n = Inf,
  width = Inf
)

cat(
  "\n===== PAIRWISE SAMPLE GAIN OVER THREE-WAY INTERSECTION =====\n"
)

print(
  pairwise_comparison |>
    select(
      sector,
      functional_form,
      contrast_id,
      old_three_way_intersection_n,
      n_common_ac,
      additional_pairwise_ACs,
      old_three_way_intersection_p,
      chi_square_p,
      cluster_df_F_p
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== THREE-IDEOLOGY OMNIBUS + COVARIANCE AUDIT =====\n"
)

print(
  omnibus_results,
  n = Inf,
  width = Inf
)

cat(
  "\nAC_IDEOLOGY_PAIRWISE_WALD_REFINEMENT_COMPLETE\n"
)

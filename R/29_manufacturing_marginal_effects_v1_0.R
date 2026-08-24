suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(fixest)
  library(lme4)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

output_dir <-
  file.path(
    project_root,
    "outputs",
    "manufacturing_marginal_effects_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ac_models_path <-
  file.path(
    "outputs",
    "main_regression_table_models_v1_0",
    "13_ac_table_models.rds"
  )

voter_models_path <-
  file.path(
    "outputs",
    "main_regression_table_models_v1_0",
    "14_voter_table_models.rds"
  )

ac_samples_path <-
  file.path(
    "outputs",
    "ac_canonical_v1_0",
    "model_samples.rds"
  )

voter_samples_path <-
  file.path(
    "outputs",
    "voter_canonical_v1_0",
    "model_samples.rds"
  )

required_files <-
  c(
    ac_models_path,
    voter_models_path,
    ac_samples_path,
    voter_samples_path
  )

missing_files <-
  required_files[
    !file.exists(
      required_files
    )
  ]

if (
  length(
    missing_files
  ) >
    0L
) {
  stop(
    "Missing required inputs: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

ac_models <-
  readRDS(
    ac_models_path
  )

voter_models <-
  readRDS(
    voter_models_path
  )

ac_samples <-
  readRDS(
    ac_samples_path
  )

voter_samples <-
  readRDS(
    voter_samples_path
  )

required_ac_models <-
  c(
    "manufacturing_raw__C3",
    "manufacturing_log1p__C3"
  )

required_voter_models <-
  c(
    "manufacturing_raw__C3",
    "manufacturing_log1p__C3"
  )

if (
  !all(
    required_ac_models %in%
      names(
        ac_models
      )
  )
) {
  stop(
    "Required AC Manufacturing models are missing."
  )
}

if (
  !all(
    required_voter_models %in%
      names(
        voter_models
      )
  )
) {
  stop(
    "Required voter Manufacturing models are missing."
  )
}

if (
  !"AC01" %in%
    names(
      ac_samples
    )
) {
  stop(
    "AC01 sample missing."
  )
}

if (
  !"V01" %in%
    names(
      voter_samples
    )
) {
  stop(
    "V01 sample missing."
  )
}

ac_support <-
  ac_samples[[
    "AC01"
  ]] |>
  transmute(
    unit_id =
      as.character(
        ac_uid
      ),

    ac_uid =
      as.character(
        ac_uid
      ),

    current_raw =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    baseline_raw =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    muslim =
      as.numeric(
        muslim
      )
  )

voter_support <-
  voter_samples[[
    "V01"
  ]] |>
  transmute(
    unit_id =
      as.character(
        respondent_uid
      ),

    ac_uid =
      as.character(
        ac_uid
      ),

    current_raw =
      as.numeric(
        fdi_mfg_current
      ),

    baseline_raw =
      as.numeric(
        fdi_mfg_baseline
      ),

    muslim =
      as.numeric(
        muslim
      )
  )

if (
  nrow(
    ac_support
  ) !=
    224L
) {
  stop(
    "AC marginal-effects sample is not 224."
  )
}

if (
  nrow(
    voter_support
  ) !=
    1763L
) {
  stop(
    "Voter marginal-effects sample is not 1763."
  )
}

for (
  dd in
    list(
      ac_support,
      voter_support
    )
) {
  if (
    any(
      !is.finite(
        dd$current_raw
      )
    ) ||
      any(
        !is.finite(
          dd$baseline_raw
        )
      ) ||
      any(
        !is.finite(
          dd$muslim
        )
      )
  ) {
    stop(
      "Non-finite support variable detected."
    )
  }

  if (
    any(
      dd$current_raw <
        0
    ) ||
      any(
        dd$baseline_raw <
          0
      )
  ) {
    stop(
      "Negative Manufacturing FDI rate detected."
    )
  }
}

model_registry <-
  tribble(
    ~level, ~outcome_label, ~functional_form, ~model_id,

    "AC",
    "AC-level centrist BJP share",
    "Raw",
    "manufacturing_raw__C3",

    "AC",
    "AC-level centrist BJP share",
    "log1p",
    "manufacturing_log1p__C3",

    "Voter",
    "Individual centrist BJP vote",
    "Raw",
    "manufacturing_raw__C3",

    "Voter",
    "Individual centrist BJP vote",
    "log1p",
    "manufacturing_log1p__C3"
  )

get_model <- function(
  level,
  model_id
) {
  if (
    level ==
      "AC"
  ) {
    ac_models[[
      model_id
    ]]
  } else {
    voter_models[[
      model_id
    ]]
  }
}

get_support <- function(
  level
) {
  if (
    level ==
      "AC"
  ) {
    ac_support
  } else {
    voter_support
  }
}

get_beta_vcov <- function(
  fit,
  level
) {
  if (
    level ==
      "AC"
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
  } else {
    beta <-
      fixef(
        fit
      )

    V <-
      as.matrix(
        vcov(
          fit
        )
      )
  }

  V <-
    V[
      names(
        beta
      ),
      names(
        beta
      ),
      drop = FALSE
    ]

  list(
    beta =
      beta,
    V =
      V
  )
}

find_one_term <- function(
  coefficient_names,
  candidates,
  label
) {
  hits <-
    intersect(
      candidates,
      coefficient_names
    )

  if (
    length(
      hits
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify ",
      label,
      ". Matches: ",
      paste(
        hits,
        collapse = ", "
      )
    )
  }

  hits[[1]]
}

get_term_map <- function(
  beta
) {
  coefficient_names <-
    names(
      beta
    )

  list(
    muslim =
      find_one_term(
        coefficient_names,
        "muslim",
        "Muslim-share main effect"
      ),

    current =
      find_one_term(
        coefficient_names,
        "x_current",
        "current-FDI main effect"
      ),

    baseline =
      find_one_term(
        coefficient_names,
        "x_baseline",
        "baseline-FDI main effect"
      ),

    current_muslim =
      find_one_term(
        coefficient_names,
        c(
          "muslim:x_current",
          "x_current:muslim"
        ),
        "current FDI x Muslim interaction"
      ),

    baseline_muslim =
      find_one_term(
        coefficient_names,
        c(
          "muslim:x_baseline",
          "x_baseline:muslim"
        ),
        "baseline FDI x Muslim interaction"
      )
  )
}

linear_combination <- function(
  beta,
  V,
  weights
) {
  L <-
    setNames(
      rep(
        0,
        length(
          beta
        )
      ),
      names(
        beta
      )
    )

  for (
    term in
      names(
        weights
      )
  ) {
    if (
      !term %in%
        names(
          L
        )
    ) {
      stop(
        "Linear-combination term absent: ",
        term
      )
    }

    L[
      term
    ] <-
      weights[[
        term
      ]]
  }

  estimate <-
    sum(
      L *
        beta
    )

  variance <-
    as.numeric(
      t(
        L
      ) %*%
        V %*%
        L
    )

  if (
    variance <
      -1e-10
  ) {
    stop(
      "Negative linear-combination variance."
    )
  }

  variance <-
    max(
      variance,
      0
    )

  se <-
    sqrt(
      variance
    )

  tibble(
    estimate =
      estimate,

    std_error =
      se,

    conf_low =
      estimate -
      1.96 *
        se,

    conf_high =
      estimate +
      1.96 *
        se
  )
}

transform_fdi <- function(
  x,
  functional_form
) {
  if (
    functional_form ==
      "Raw"
  ) {
    x
  } else if (
    functional_form ==
      "log1p"
  ) {
    log1p(
      x
    )
  } else {
    stop(
      "Unknown functional form."
    )
  }
}

support_quantile_grid <- function(
  x,
  n =
    201L
) {
  grid <-
    quantile(
      x,
      probs =
        seq(
          0,
          1,
          length.out =
            n
        ),
      na.rm =
        TRUE,
      names =
        FALSE,
      type =
        8
    )

  sort(
    unique(
      as.numeric(
        grid
      )
    )
  )
}

make_muslim_effect_curve <- function(
  fit,
  support,
  level,
  outcome_label,
  functional_form,
  muslim_change_pp
) {
  bundle <-
    get_beta_vcov(
      fit,
      level
    )

  beta <-
    bundle$beta

  V <-
    bundle$V

  terms <-
    get_term_map(
      beta
    )

  current_grid_raw <-
    support_quantile_grid(
      support$current_raw
    )

  baseline_model <-
    transform_fdi(
      support$baseline_raw,
      functional_form
    )

  mean_baseline_model <-
    mean(
      baseline_model
    )

  map_dfr(
    current_grid_raw,
    function(
      current_raw
    ) {
      current_model <-
        transform_fdi(
          current_raw,
          functional_form
        )

      result <-
        linear_combination(
          beta,
          V,
          setNames(
            c(
              1,
              current_model,
              mean_baseline_model
            ),
            c(
              terms$muslim,
              terms$current_muslim,
              terms$baseline_muslim
            )
          )
        )

      scale_factor <-
        muslim_change_pp

      result |>
        transmute(
          level =
            level,

          outcome_label =
            outcome_label,

          functional_form =
            functional_form,

          current_fdi_raw =
            current_raw,

          current_fdi_model_scale =
            current_model,

          average_baseline_fdi_model_scale =
            mean_baseline_model,

          muslim_change_pp =
            muslim_change_pp,

          effect_outcome_probability =
            estimate *
              muslim_change_pp /
              100,

          effect_pp =
            estimate *
              scale_factor,

          std_error_pp =
            std_error *
              scale_factor,

          conf_low_pp =
            conf_low *
              scale_factor,

          conf_high_pp =
            conf_high *
              scale_factor,

          ci_excludes_zero =
            conf_low_pp >
              0 |
              conf_high_pp <
                0
        )
    }
  )
}

make_reverse_fdi_curve <- function(
  fit,
  support,
  level,
  outcome_label,
  functional_form
) {
  bundle <-
    get_beta_vcov(
      fit,
      level
    )

  beta <-
    bundle$beta

  V <-
    bundle$V

  terms <-
    get_term_map(
      beta
    )

  muslim_grid <-
    support_quantile_grid(
      support$muslim
    )

  if (
    functional_form ==
      "Raw"
  ) {
    average_model_scale_change <-
      1
  } else {
    average_model_scale_change <-
      mean(
        log1p(
          support$current_raw +
            1
        ) -
          log1p(
            support$current_raw
          )
      )
  }

  map_dfr(
    muslim_grid,
    function(
      muslim_value
    ) {
      result <-
        linear_combination(
          beta,
          V,
          setNames(
            c(
              average_model_scale_change,
              average_model_scale_change *
                muslim_value
            ),
            c(
              terms$current,
              terms$current_muslim
            )
          )
        )

      result |>
        transmute(
          level =
            level,

          outcome_label =
            outcome_label,

          functional_form =
            functional_form,

          muslim_share =
            muslim_value,

          muslim_share_percent =
            100 *
              muslim_value,

          raw_fdi_increment_per100k =
            1,

          average_model_scale_fdi_increment =
            average_model_scale_change,

          effect_outcome_probability =
            estimate,

          effect_pp =
            100 *
              estimate,

          std_error_pp =
            100 *
              std_error,

          conf_low_pp =
            100 *
              conf_low,

          conf_high_pp =
            100 *
              conf_high,

          ci_excludes_zero =
            conf_low_pp >
              0 |
              conf_high_pp <
                0
        )
    }
  )
}

primary_1pp <-
  list()

primary_10pp <-
  list()

reverse_plus1_project <-
  list()

for (
  i in
    seq_len(
      nrow(
        model_registry
      )
    )
) {
  spec <-
    model_registry[
      i,
      ,
      drop = FALSE
    ]

  fit <-
    get_model(
      spec$level,
      spec$model_id
    )

  support <-
    get_support(
      spec$level
    )

  primary_1pp[[
    i
  ]] <-
    make_muslim_effect_curve(
      fit =
        fit,
      support =
        support,
      level =
        spec$level,
      outcome_label =
        spec$outcome_label,
      functional_form =
        spec$functional_form,
      muslim_change_pp =
        1
    )

  primary_10pp[[
    i
  ]] <-
    make_muslim_effect_curve(
      fit =
        fit,
      support =
        support,
      level =
        spec$level,
      outcome_label =
        spec$outcome_label,
      functional_form =
        spec$functional_form,
      muslim_change_pp =
        10
    )

  reverse_plus1_project[[
    i
  ]] <-
    make_reverse_fdi_curve(
      fit =
        fit,
      support =
        support,
      level =
        spec$level,
      outcome_label =
        spec$outcome_label,
      functional_form =
        spec$functional_form
    )
}

primary_1pp <-
  bind_rows(
    primary_1pp
  )

primary_10pp <-
  bind_rows(
    primary_10pp
  )

reverse_plus1_project <-
  bind_rows(
    reverse_plus1_project
  )

support_summary <-
  bind_rows(
    ac_support |>
      mutate(
        level =
          "AC"
      ),

    voter_support |>
      mutate(
        level =
          "Voter"
      )
  ) |>
  group_by(
    level
  ) |>
  summarise(
    n_rows =
      n(),

    n_ac =
      n_distinct(
        ac_uid
      ),

    current_zero_share =
      mean(
        current_raw ==
          0
      ),

    current_min =
      min(
        current_raw
      ),

    current_p10 =
      quantile(
        current_raw,
        .10,
        names =
          FALSE
      ),

    current_p25 =
      quantile(
        current_raw,
        .25,
        names =
          FALSE
      ),

    current_median =
      median(
        current_raw
      ),

    current_p75 =
      quantile(
        current_raw,
        .75,
        names =
          FALSE
      ),

    current_p90 =
      quantile(
        current_raw,
        .90,
        names =
          FALSE
      ),

    current_p95 =
      quantile(
        current_raw,
        .95,
        names =
          FALSE
      ),

    current_p99 =
      quantile(
        current_raw,
        .99,
        names =
          FALSE
      ),

    current_max =
      max(
        current_raw
      ),

    baseline_mean =
      mean(
        baseline_raw
      ),

    baseline_log1p_mean =
      mean(
        log1p(
          baseline_raw
        )
      ),

    muslim_min_pp =
      100 *
        min(
          muslim
        ),

    muslim_p25_pp =
      100 *
        quantile(
          muslim,
          .25,
          names =
            FALSE
        ),

    muslim_median_pp =
      100 *
        median(
          muslim
        ),

    muslim_p75_pp =
      100 *
        quantile(
          muslim,
          .75,
          names =
            FALSE
        ),

    muslim_max_pp =
      100 *
        max(
          muslim
        ),

    .groups =
      "drop"
  )

evaluation_probabilities <-
  c(
    .10,
    .25,
    .50,
    .75,
    .90
  )

evaluation_points <-
  map_dfr(
    seq_len(
      nrow(
        model_registry
      )
    ),
    function(
      i
    ) {
      spec <-
        model_registry[
          i,
          ,
          drop = FALSE
        ]

      fit <-
        get_model(
          spec$level,
          spec$model_id
        )

      support <-
        get_support(
          spec$level
        )

      raw_points <-
        quantile(
          support$current_raw,
          probs =
            evaluation_probabilities,
          names =
            FALSE,
          type =
            8
        )

      one_pp_curve <-
        make_muslim_effect_curve(
          fit =
            fit,
          support =
            support,
          level =
            spec$level,
          outcome_label =
            spec$outcome_label,
          functional_form =
            spec$functional_form,
          muslim_change_pp =
            1
        )

      ten_pp_curve <-
        make_muslim_effect_curve(
          fit =
            fit,
          support =
            support,
          level =
            spec$level,
          outcome_label =
            spec$outcome_label,
          functional_form =
            spec$functional_form,
          muslim_change_pp =
            10
        )

      map_dfr(
        seq_along(
          raw_points
        ),
        function(
          j
        ) {
          target <-
            raw_points[[
              j
            ]]

          row_1 <-
            one_pp_curve[
              which.min(
                abs(
                  one_pp_curve$current_fdi_raw -
                    target
                )
              ),
              ,
              drop = FALSE
            ]

          row_10 <-
            ten_pp_curve[
              which.min(
                abs(
                  ten_pp_curve$current_fdi_raw -
                    target
                )
              ),
              ,
              drop = FALSE
            ]

          tibble(
            level =
              spec$level,

            outcome_label =
              spec$outcome_label,

            functional_form =
              spec$functional_form,

            support_quantile =
              evaluation_probabilities[[
                j
              ]],

            current_fdi_raw =
              target,

            effect_1pp =
              row_1$effect_pp,

            ci_low_1pp =
              row_1$conf_low_pp,

            ci_high_1pp =
              row_1$conf_high_pp,

            effect_10pp =
              row_10$effect_pp,

            ci_low_10pp =
              row_10$conf_low_pp,

            ci_high_10pp =
              row_10$conf_high_pp
          )
        }
      )
    }
  )

curve_summary <-
  primary_1pp |>
  group_by(
    level,
    outcome_label,
    functional_form
  ) |>
  summarise(
    min_current_fdi =
      min(
        current_fdi_raw
      ),

    max_current_fdi =
      max(
        current_fdi_raw
      ),

    min_effect_1pp =
      min(
        effect_pp
      ),

    max_effect_1pp =
      max(
        effect_pp
      ),

    any_positive_ci =
      any(
        conf_low_pp >
          0
      ),

    any_negative_ci =
      any(
        conf_high_pp <
          0
      ),

    first_fdi_positive_ci =
      if (
        any(
          conf_low_pp >
            0
        )
      ) {
        min(
          current_fdi_raw[
            conf_low_pp >
              0
          ]
        )
      } else {
        NA_real_
      },

    last_fdi_positive_ci =
      if (
        any(
          conf_low_pp >
            0
        )
      ) {
        max(
          current_fdi_raw[
            conf_low_pp >
              0
          ]
        )
      } else {
        NA_real_
      },

    closest_to_zero_fdi =
      current_fdi_raw[
        which.min(
          abs(
            effect_pp
          )
        )
      ],

    closest_to_zero_effect =
      effect_pp[
        which.min(
          abs(
            effect_pp
          )
        )
      ],

    .groups =
      "drop"
  )

support_rug <-
  bind_rows(
    ac_support |>
      distinct(
        ac_uid,
        current_raw
      ) |>
      transmute(
        level =
          "AC",
        outcome_label =
          "AC-level centrist BJP share",
        current_fdi_raw =
          current_raw
      ),

    voter_support |>
      distinct(
        ac_uid,
        current_raw
      ) |>
      transmute(
        level =
          "Voter",
        outcome_label =
          "Individual centrist BJP vote",
        current_fdi_raw =
          current_raw
      )
  ) |>
  crossing(
    functional_form =
      c(
        "Raw",
        "log1p"
      )
  )

muslim_rug <-
  bind_rows(
    ac_support |>
      distinct(
        ac_uid,
        muslim
      ) |>
      transmute(
        level =
          "AC",
        outcome_label =
          "AC-level centrist BJP share",
        muslim_share_percent =
          100 *
            muslim
      ),

    voter_support |>
      distinct(
        ac_uid,
        muslim
      ) |>
      transmute(
        level =
          "Voter",
        outcome_label =
          "Individual centrist BJP vote",
        muslim_share_percent =
          100 *
            muslim
      )
  ) |>
  crossing(
    functional_form =
      c(
        "Raw",
        "log1p"
      )
  )

plot_primary <- function(
  data,
  muslim_change_pp,
  title_text,
  subtitle_text
) {
  ggplot(
    data,
    aes(
      x =
        current_fdi_raw,
      y =
        effect_pp
    )
  ) +
    geom_hline(
      yintercept =
        0,
      linetype =
        "dashed",
      linewidth =
        0.4
    ) +
    geom_ribbon(
      aes(
        ymin =
          conf_low_pp,
        ymax =
          conf_high_pp
      ),
      alpha =
        0.18,
      linewidth =
        0
    ) +
    geom_line(
      linewidth =
        0.8
    ) +
    geom_rug(
      data =
        support_rug,
      aes(
        x =
          current_fdi_raw
      ),
      inherit.aes =
        FALSE,
      sides =
        "b",
      alpha =
        0.12,
      linewidth =
        0.25
    ) +
    facet_grid(
      rows =
        vars(
          outcome_label
        ),
      cols =
        vars(
          functional_form
        )
    ) +
    labs(
      title =
        title_text,

      subtitle =
        subtitle_text,

      x =
        "Manufacturing FDI projects per 100,000 residents, 2009–2014",

      y =
        paste0(
          "Change in BJP support (percentage points)\nfor +",
          muslim_change_pp,
          " pp Muslim population share"
        ),

      caption =
        paste0(
          "Curves use the fully adjusted canonical Manufacturing models. ",
          "Baseline Manufacturing FDI is averaged over the estimation sample. ",
          "Shaded regions are 95% confidence intervals; rugs show observed constituency-level FDI support. ",
          "The log1p panels are evaluated on the model's log1p exposure scale but displayed against raw projects per 100,000."
        )
    ) +
    theme_minimal(
      base_size =
        11
    ) +
    theme(
      panel.grid.minor =
        element_blank(),

      strip.text =
        element_text(
          face =
            "bold"
        ),

      plot.title =
        element_text(
          face =
            "bold"
        ),

      legend.position =
        "none"
    )
}

plot_reverse <- function(
  data
) {
  ggplot(
    data,
    aes(
      x =
        muslim_share_percent,
      y =
        effect_pp
    )
  ) +
    geom_hline(
      yintercept =
        0,
      linetype =
        "dashed",
      linewidth =
        0.4
    ) +
    geom_ribbon(
      aes(
        ymin =
          conf_low_pp,
        ymax =
          conf_high_pp
      ),
      alpha =
        0.18,
      linewidth =
        0
    ) +
    geom_line(
      linewidth =
        0.8
    ) +
    geom_rug(
      data =
        muslim_rug,
      aes(
        x =
          muslim_share_percent
      ),
      inherit.aes =
        FALSE,
      sides =
        "b",
      alpha =
        0.12,
      linewidth =
        0.25
    ) +
    facet_grid(
      rows =
        vars(
          outcome_label
        ),
      cols =
        vars(
          functional_form
        )
    ) +
    labs(
      title =
        "Alternative marginal effect: Manufacturing FDI across Muslim population share",

      subtitle =
        "Average discrete change associated with one additional Manufacturing FDI project per 100,000 residents",

      x =
        "Muslim population share, 2001 (%)",

      y =
        "Change in BJP support (percentage points)\nfor +1 Manufacturing FDI project per 100,000",

      caption =
        paste0(
          "For raw models, this is the exact one-project marginal effect. ",
          "For log1p models, the figure reports the average discrete change from FDI to FDI + 1 project per 100,000, ",
          "averaged over the observed current-FDI distribution. ",
          "Shaded regions are 95% confidence intervals."
        )
    ) +
    theme_minimal(
      base_size =
        11
    ) +
    theme(
      panel.grid.minor =
        element_blank(),

      strip.text =
        element_text(
          face =
            "bold"
        ),

      plot.title =
        element_text(
          face =
            "bold"
        ),

      legend.position =
        "none"
    )
}

figure_1pp <-
  plot_primary(
    primary_1pp,
    muslim_change_pp =
      1,
    title_text =
      "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Primary candidate: effect of a 1-percentage-point increase in Muslim population share"
  )

figure_10pp <-
  plot_primary(
    primary_10pp,
    muslim_change_pp =
      10,
    title_text =
      "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Review alternative: effect of a 10-percentage-point increase in Muslim population share"
  )

figure_reverse <-
  plot_reverse(
    reverse_plus1_project
  )

write_csv(
  model_registry,
  file.path(
    output_dir,
    "00_model_registry.csv"
  )
)

write_csv(
  primary_1pp,
  file.path(
    output_dir,
    "01_primary_muslim_effect_1pp_grid.csv"
  )
)

write_csv(
  primary_10pp,
  file.path(
    output_dir,
    "02_review_muslim_effect_10pp_grid.csv"
  )
)

write_csv(
  reverse_plus1_project,
  file.path(
    output_dir,
    "03_appendix_reverse_plus1_project_grid.csv"
  )
)

write_csv(
  support_summary,
  file.path(
    output_dir,
    "04_support_summary.csv"
  )
)

write_csv(
  evaluation_points,
  file.path(
    output_dir,
    "05_primary_effects_at_fdi_quantiles.csv"
  )
)

write_csv(
  curve_summary,
  file.path(
    output_dir,
    "06_primary_curve_summary.csv"
  )
)

ggsave(
  filename =
    file.path(
      output_dir,
      "07_main_candidate_manufacturing_marginal_effects_1pp.pdf"
    ),
  plot =
    figure_1pp,
  width =
    8.5,
  height =
    6.5,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "07_main_candidate_manufacturing_marginal_effects_1pp.png"
    ),
  plot =
    figure_1pp,
  width =
    8.5,
  height =
    6.5,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "08_review_manufacturing_marginal_effects_10pp.pdf"
    ),
  plot =
    figure_10pp,
  width =
    8.5,
  height =
    6.5,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "08_review_manufacturing_marginal_effects_10pp.png"
    ),
  plot =
    figure_10pp,
  width =
    8.5,
  height =
    6.5,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "09_appendix_reverse_manufacturing_plus1_project.pdf"
    ),
  plot =
    figure_reverse,
  width =
    8.5,
  height =
    6.5,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "09_appendix_reverse_manufacturing_plus1_project.png"
    ),
  plot =
    figure_reverse,
  width =
    8.5,
  height =
    6.5,
  units =
    "in",
  dpi =
    300
)

notes <-
  c(
    "R29 MANUFACTURING MARGINAL EFFECTS",
    "",
    "PRIMARY THEORETICAL ESTIMAND",
    "Change in BJP support associated with an increase in 2001 Muslim population share, evaluated across current Manufacturing FDI exposure.",
    "",
    "Primary presentation:",
    "1-percentage-point increase in Muslim population share.",
    "",
    "Review presentation:",
    "10-percentage-point increase in Muslim population share.",
    "Because the models are linear in Muslim share, the 10-pp effects and confidence intervals are exactly ten times the 1-pp values.",
    "The shape, zero crossings, and significance regions are therefore identical.",
    "",
    "Outcome scaling:",
    "All plotted effects are expressed in percentage points of BJP support.",
    "AC outcome = survey-weighted BJP share among Center respondents.",
    "Voter outcome = probability of BJP voting among Center respondents under the mixed LPM.",
    "",
    "BASELINE FDI HANDLING",
    "The Muslim-share marginal effect depends on both current and baseline FDI because both interact with Muslim share.",
    "At each current-FDI value, the curve averages the baseline-FDI contribution over the observed estimation sample.",
    "For these linear models, this is algebraically equivalent to evaluating the baseline interaction at mean baseline exposure on the model scale.",
    "AC averages weight each AC equally.",
    "Voter averages weight each respondent equally, matching the unweighted voter-level estimand.",
    "",
    "LOG1P PANELS",
    "The x-axis remains raw Manufacturing projects per 100,000.",
    "The logged models internally evaluate log1p(raw FDI).",
    "",
    "ALTERNATIVE / APPENDIX ESTIMAND",
    "Change in BJP support from one additional Manufacturing FDI project per 100,000 across Muslim population share.",
    "For raw models this is the exact marginal effect.",
    "For log1p models this is the average discrete change from FDI to FDI + 1, averaged across the observed current-FDI distribution.",
    "",
    "All models are the fully adjusted R28 Column-3 Manufacturing models."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "10_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "11_session_info.txt"
  )
)

cat(
  "\n===== MANUFACTURING FDI SUPPORT =====\n"
)

print(
  support_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY 1-PP EFFECTS AT FDI QUANTILES =====\n"
)

print(
  evaluation_points |>
    select(
      level,
      functional_form,
      support_quantile,
      current_fdi_raw,
      effect_1pp,
      ci_low_1pp,
      ci_high_1pp
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== REVIEW 10-PP EFFECTS AT FDI QUANTILES =====\n"
)

print(
  evaluation_points |>
    select(
      level,
      functional_form,
      support_quantile,
      current_fdi_raw,
      effect_10pp,
      ci_low_10pp,
      ci_high_10pp
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY CURVE SUMMARY =====\n"
)

print(
  curve_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nR29_MANUFACTURING_MARGINAL_EFFECTS_COMPLETE\n"
)

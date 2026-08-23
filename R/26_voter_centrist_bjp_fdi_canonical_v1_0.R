suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(lme4)
})

required_packages <-
  c(
    "dplyr",
    "tidyr",
    "purrr",
    "readr",
    "tibble",
    "lme4",
    "reformulas"
  )

missing_packages <-
  required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

if (
  length(
    missing_packages
  ) > 0L
) {
  stop(
    "Missing required packages: ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}

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
    "voter_canonical_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

respondent_path <-
  file.path(
    input_dir,
    "nes_respondent_analysis.rds"
  )

change_path <-
  file.path(
    input_dir,
    "ac_change.rds"
  )

for (
  path in
    c(
      respondent_path,
      change_path
    )
) {
  if (
    !file.exists(
      path
    )
  ) {
    stop(
      "Required input missing: ",
      path
    )
  }
}

respondents <-
  readRDS(
    respondent_path
  )

ac_change <-
  readRDS(
    change_path
  )

require_columns <- function(
  data,
  columns,
  label
) {
  missing <-
    setdiff(
      columns,
      names(
        data
      )
    )

  if (
    length(
      missing
    ) > 0L
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

individual_controls <-
  c(
    "religion_group",
    "caste_group",
    "education_harmonized"
  )

primary_ac_controls <-
  c(
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share"
  )

expanded_ac_controls <-
  c(
    "employment_per_total_population",
    "ed_sec_share"
  )

respondent_required <-
  c(
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
    "survey_weight_norm_year",
    individual_controls,
    primary_ac_controls,
    expanded_ac_controls
  )

require_columns(
  respondents,
  respondent_required,
  "nes_respondent_analysis"
)

fdi_variables <-
  c(
    "fdi_total_local_all_pc100k_2009",
    "fdi_total_local_all_pc100k_2014",
    "log1p_fdi_total_local_all_pc100k_2009",
    "log1p_fdi_total_local_all_pc100k_2014",
    "fdi_total_own_all_pc100k_2009",
    "fdi_total_own_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2009",
    "fdi_mfg_local_all_pc100k_2014",
    "fdi_services_local_all_pc100k_2009",
    "fdi_services_local_all_pc100k_2014",
    "fdi_total_local_early21_pc100k",
    "fdi_total_local_late21_pc100k",
    "d_fdi_total_local_21m_pc100k"
  )

require_columns(
  ac_change,
  c(
    "ac_uid",
    fdi_variables
  ),
  "ac_change"
)

fdi_payload <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      fdi_variables
    )
  )

if (
  anyDuplicated(
    fdi_payload$ac_uid
  ) > 0L
) {
  stop(
    "ac_change FDI payload is not unique by ac_uid."
  )
}

respondents <-
  respondents |>
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
  ) > 0L
) {
  stop(
    "Respondent data are not unique by respondent_uid."
  )
}

relevel_if_present <- function(
  x,
  reference
) {
  out <-
    factor(
      as.character(
        x
      )
    )

  if (
    reference %in%
      levels(
        out
      )
  ) {
    out <-
      stats::relevel(
        out,
        ref = reference
      )
  }

  out
}

respondents <-
  respondents |>
  mutate(
    y =
      as.numeric(
        voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
      ),

    fdi_total_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    fdi_total_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      ),

    fdi_log_current =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2014
      ),

    fdi_log_baseline =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2009
      ),

    fdi_own_current =
      as.numeric(
        fdi_total_own_all_pc100k_2014
      ),

    fdi_own_baseline =
      as.numeric(
        fdi_total_own_all_pc100k_2009
      ),

    fdi_mfg_current =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    fdi_mfg_baseline =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    fdi_services_current =
      as.numeric(
        fdi_services_local_all_pc100k_2014
      ),

    fdi_services_baseline =
      as.numeric(
        fdi_services_local_all_pc100k_2009
      ),

    fdi_early21 =
      as.numeric(
        fdi_total_local_early21_pc100k
      ),

    fdi_late21 =
      as.numeric(
        fdi_total_local_late21_pc100k
      ),

    fdi_change_21m =
      as.numeric(
        d_fdi_total_local_21m_pc100k
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

    employment_share_pp =
      100 *
      as.numeric(
        employment_per_total_population
      ),

    ed_sec_share_pp =
      100 *
      as.numeric(
        ed_sec_share
      ),

    state_fe =
      factor(
        state_no
      ),

    ac_random =
      factor(
        ac_uid
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
          c(
            "Center",
            "Left",
            "Right",
            "Mixed"
          )
      ),

    survey_weight =
      as.numeric(
        survey_weight_norm_year
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

stage_2014 <-
  respondents |>
  filter(
    year == 2014
  )

stage_vote <-
  stage_2014 |>
  filter(
    vote_valid %in%
      TRUE
  )

stage_outcome <-
  stage_vote |>
  filter(
    !is.na(
      y
    )
  )

stage_ideology <-
  stage_outcome |>
  filter(
    ideology_complete %in%
      TRUE,
    !is.na(
      voter_ideology
    )
  )

stage_center <-
  stage_ideology |>
  filter(
    as.character(
      voter_ideology
    ) ==
      "Center"
  )

stage_candidate <-
  stage_center |>
  filter(
    bjp_candidate_present %in%
      TRUE
  )

stage_support <-
  stage_candidate |>
  filter(
    fdi_spatial_support %in%
      TRUE
  )

stage_muslim <-
  stage_support |>
  filter(
    is.finite(
      muslim
    )
  )

stage_fdi <-
  stage_muslim |>
  filter(
    is.finite(
      fdi_total_current
    ),
    is.finite(
      fdi_total_baseline
    )
  )

stage_ac_controls <-
  stage_fdi |>
  filter(
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
      ac_random
    )
  )

stage_individual_controls <-
  stage_ac_controls |>
  filter(
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

primary_funnel <-
  tibble(
    step =
      c(
        "All respondents",
        "2014 respondents",
        "Valid vote",
        "BJP outcome observed",
        "Ideology complete",
        "Center ideology",
        "BJP candidate present",
        "FDI spatial support",
        "2001 Muslim share",
        "Current + baseline total local FDI",
        "Primary AC controls",
        "Individual controls"
      ),
    n =
      c(
        nrow(
          respondents
        ),
        nrow(
          stage_2014
        ),
        nrow(
          stage_vote
        ),
        nrow(
          stage_outcome
        ),
        nrow(
          stage_ideology
        ),
        nrow(
          stage_center
        ),
        nrow(
          stage_candidate
        ),
        nrow(
          stage_support
        ),
        nrow(
          stage_muslim
        ),
        nrow(
          stage_fdi
        ),
        nrow(
          stage_ac_controls
        ),
        nrow(
          stage_individual_controls
        )
      )
  )

if (
  nrow(
    stage_individual_controls
  ) !=
    1763L
) {
  stop(
    "Primary voter pre-model sample changed from expected 1763."
  )
}

if (
  n_distinct(
    stage_individual_controls$ac_uid
  ) !=
    222L
) {
  stop(
    "Primary voter AC count changed from expected 222."
  )
}

if (
  n_distinct(
    stage_individual_controls$state_no
  ) !=
    24L
) {
  stop(
    "Primary voter state count changed from expected 24."
  )
}

base_common <-
  stage_ideology |>
  filter(
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
      ac_random
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

sample_center <-
  base_common |>
  filter(
    ideology4 ==
      "Center"
  )

sample_left <-
  base_common |>
  filter(
    ideology4 ==
      "Left"
  )

sample_right <-
  base_common |>
  filter(
    ideology4 ==
      "Right"
  )

sample_all <-
  base_common |>
  filter(
    !is.na(
      ideology4
    )
  )

sample_pools <-
  list(
    center =
      sample_center,
    left =
      sample_left,
    right =
      sample_right,
    all =
      sample_all
  )

primary_control_terms <-
  c(
    "ac_pop_100k",
    "sc_share_pp",
    "st_share_pp",
    "religion_x",
    "caste_x",
    "education_x",
    "state_fe"
  )

expanded_control_terms <-
  c(
    primary_control_terms,
    "employment_share_pp",
    "ed_sec_share_pp"
  )

spec_registry <-
  tribble(
    ~model_id, ~role, ~sample_key, ~sector, ~geography, ~functional_form, ~treatment, ~rhs_focal, ~expanded, ~weighted, ~focal_var,

    "V01", "Primary", "center", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current + muslim * fdi_total_baseline",
    FALSE, FALSE, "fdi_total_current",

    "V02", "Expanded controls", "center", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current + muslim * fdi_total_baseline",
    TRUE, FALSE, "fdi_total_current",

    "V03", "Functional-form robustness", "center", "Total", "Local", "log1p", "60-month current + baseline",
    "muslim * fdi_log_current + muslim * fdi_log_baseline",
    FALSE, FALSE, "fdi_log_current",

    "V04", "Spatial robustness", "center", "Total", "Own AC", "Raw", "60-month current + baseline",
    "muslim * fdi_own_current + muslim * fdi_own_baseline",
    FALSE, FALSE, "fdi_own_current",

    "V05", "Sector comparison", "center", "Manufacturing", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_mfg_current + muslim * fdi_mfg_baseline",
    FALSE, FALSE, "fdi_mfg_current",

    "V06", "Sector comparison", "center", "Services", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_services_current + muslim * fdi_services_baseline",
    FALSE, FALSE, "fdi_services_current",

    "V07", "Alternative treatment robustness", "center", "Total", "Local", "Raw", "21-month change + early baseline",
    "muslim * fdi_change_21m + muslim * fdi_early21",
    FALSE, FALSE, "fdi_change_21m",

    "V08", "Ideology comparison: Left", "left", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current + muslim * fdi_total_baseline",
    FALSE, FALSE, "fdi_total_current",

    "V09", "Ideology comparison: Right", "right", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current + muslim * fdi_total_baseline",
    FALSE, FALSE, "fdi_total_current",

    "V10", "All-ideology heterogeneity", "all", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current * ideology4 + muslim * fdi_total_baseline * ideology4",
    FALSE, FALSE, "fdi_total_current",

    "V11", "Prior-weight sensitivity (not survey-design estimator)", "center", "Total", "Local", "Raw", "60-month current + baseline",
    "muslim * fdi_total_current + muslim * fdi_total_baseline",
    FALSE, TRUE, "fdi_total_current"
  )

write_csv(
  spec_registry,
  file.path(
    output_dir,
    "00_model_registry.csv"
  )
)

complete_model_data <- function(
  data,
  variables
) {
  keep <-
    rep(
      TRUE,
      nrow(
        data
      )
    )

  for (
    variable in
      unique(
        variables
      )
  ) {
    x <-
      data[[
        variable
      ]]

    if (
      is.numeric(
        x
      ) ||
        is.integer(
          x
        )
    ) {
      keep <-
        keep &
        !is.na(
          x
        ) &
        is.finite(
          as.numeric(
            x
          )
        )
    } else {
      keep <-
        keep &
        !is.na(
          x
        )
    }
  }

  data[
    keep,
    ,
    drop = FALSE
  ]
}

collapse_messages <- function(
  x
) {
  if (
    is.null(
      x
    ) ||
      length(
        x
      ) == 0L
  ) {
    return(
      NA_character_
    )
  }

  paste(
    unique(
      as.character(
        x
      )
    ),
    collapse = " | "
  )
}

fit_formula <- function(
  data,
  formula,
  use_weights = FALSE
) {
  needed <-
    all.vars(
      formula
    )

  if (
    use_weights
  ) {
    needed <-
      c(
        needed,
        "survey_weight"
      )
  }

  dd <-
    complete_model_data(
      data,
      needed
    )

  if (
    use_weights
  ) {
    dd <-
      dd |>
      filter(
        survey_weight >
          0
      )
  }

  dd <-
    droplevels(
      dd
    )

  if (
    nrow(
      dd
    ) == 0L
  ) {
    stop(
      "Model sample is empty."
    )
  }

  if (
    n_distinct(
      dd$ac_uid
    ) <
      2L
  ) {
    stop(
      "Fewer than two ACs remain."
    )
  }

  warnings <-
    character()

  messages <-
    character()

  fit <-
    withCallingHandlers(
      {
        if (
          use_weights
        ) {
          lmer(
            formula,
            data = dd,
            weights =
              survey_weight,
            REML = FALSE,
            control =
              lmerControl(
                optimizer = "bobyqa",
                optCtrl =
                  list(
                    maxfun = 300000
                  )
              )
          )
        } else {
          lmer(
            formula,
            data = dd,
            REML = FALSE,
            control =
              lmerControl(
                optimizer = "bobyqa",
                optCtrl =
                  list(
                    maxfun = 300000
                  )
              )
          )
        }
      },
      warning =
        function(
          w
        ) {
          warnings <<-
            c(
              warnings,
              conditionMessage(
                w
              )
            )

          invokeRestart(
            "muffleWarning"
          )
        },
      message =
        function(
          m
        ) {
          messages <<-
            c(
              messages,
              conditionMessage(
                m
              )
            )

          invokeRestart(
            "muffleMessage"
          )
        }
    )

  list(
    fit =
      fit,
    data =
      dd,
    warnings =
      unique(
        warnings
      ),
    messages =
      unique(
        messages
      )
  )
}

fit_one_spec <- function(
  spec
) {
  sample_key <-
    spec$sample_key[[
      1
    ]]

  dd <-
    sample_pools[[
      sample_key
    ]]

  controls <-
    if (
      isTRUE(
        spec$expanded[[
          1
        ]]
      )
    ) {
      expanded_control_terms
    } else {
      primary_control_terms
    }

  rhs <-
    paste(
      c(
        spec$rhs_focal[[
          1
        ]],
        controls
      ),
      collapse = " + "
    )

  formula <-
    as.formula(
      paste0(
        "y ~ ",
        rhs,
        " + (1 | ac_random)"
      )
    )

  result <-
    fit_formula(
      dd,
      formula,
      use_weights =
        isTRUE(
          spec$weighted[[
            1
          ]]
        )
    )

  result$formula_text <-
    paste(
      deparse(
        formula,
        width.cutoff = 500
      ),
      collapse = ""
    )

  result
}

fit_records <-
  map(
    seq_len(
      nrow(
        spec_registry
      )
    ),
    function(
      i
    ) {
      fit_one_spec(
        spec_registry[
          i,
          ,
          drop = FALSE
        ]
      )
    }
  )

names(
  fit_records
) <-
  spec_registry$model_id

models <-
  map(
    fit_records,
    "fit"
  )

model_samples <-
  map(
    fit_records,
    "data"
  )

if (
  nrow(
    model_samples[[
      "V01"
    ]]
  ) !=
    1763L
) {
  stop(
    "V01 model sample does not contain expected 1763 voters."
  )
}

if (
  n_distinct(
    model_samples[[
      "V01"
    ]]$ac_uid
  ) !=
    222L
) {
  stop(
    "V01 model sample does not contain expected 222 ACs."
  )
}

if (
  n_distinct(
    model_samples[[
      "V01"
    ]]$state_no
  ) !=
    24L
) {
  stop(
    "V01 model sample does not contain expected 24 states."
  )
}

if (
  nrow(
    model_samples[[
      "V02"
    ]]
  ) !=
    1325L
) {
  stop(
    "V02 expanded-control sample does not contain expected 1325 voters."
  )
}

extract_coefficients <- function(
  fit,
  model_id
) {
  ct <-
    coef(
      summary(
        fit
      )
    )

  tibble(
    model_id =
      model_id,
    term =
      rownames(
        ct
      ),
    estimate =
      as.numeric(
        ct[
          ,
          1
        ]
      ),
    std_error =
      as.numeric(
        ct[
          ,
          2
        ]
      ),
    statistic =
      as.numeric(
        ct[
          ,
          3
        ]
      )
  ) |>
  mutate(
    conf_low =
      estimate -
      1.96 *
      std_error,

    conf_high =
      estimate +
      1.96 *
      std_error,

    p_value_normal_approx =
      2 *
      pnorm(
        abs(
          statistic
        ),
        lower.tail = FALSE
      )
  )
}

all_coefficients <-
  imap_dfr(
    models,
    extract_coefficients
  ) |>
  left_join(
    spec_registry |>
      select(
        model_id,
        role,
        sample_key,
        sector,
        geography,
        functional_form,
        treatment,
        weighted
      ),
    by = "model_id"
  )

write_csv(
  all_coefficients,
  file.path(
    output_dir,
    "02_all_model_coefficients.csv"
  )
)

find_exact_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <-
    coefficient_names[
      vapply(
        strsplit(
          coefficient_names,
          ":",
          fixed = TRUE
        ),
        function(
          pieces
        ) {
          length(
            pieces
          ) ==
            length(
              variables
            ) &&
            setequal(
              pieces,
              variables
            )
        },
        logical(
          1
        )
      )
    ]

  if (
    length(
      hits
    ) !=
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

  hits[[
    1
  ]]
}

model_metadata <-
  imap_dfr(
    fit_records,
    function(
      record,
      model_id
    ) {
      spec <-
        spec_registry |>
        filter(
          .data$model_id ==
            .env$model_id
        )

      fit <-
        record$fit

      dd <-
        record$data

      dropped <-
        attr(
          getME(
            fit,
            "X"
          ),
          "col.dropped"
        )

      tibble(
        model_id =
          model_id,

        role =
          spec$role,

        sample_key =
          spec$sample_key,

        sector =
          spec$sector,

        geography =
          spec$geography,

        functional_form =
          spec$functional_form,

        treatment =
          spec$treatment,

        weighted =
          spec$weighted,

        n_voters =
          nrow(
            dd
          ),

        n_ac =
          n_distinct(
            dd$ac_uid
          ),

        n_states =
          n_distinct(
            dd$state_no
          ),

        n_fixed_effect_coefficients =
          length(
            fixef(
              fit
            )
          ),

        n_dropped_fixed_columns =
          if (
            is.null(
              dropped
            )
          ) {
            0L
          } else {
            length(
              dropped
            )
          },

        formula =
          record$formula_text
      )
    }
  )

write_csv(
  model_metadata,
  file.path(
    output_dir,
    "03_model_manifest_and_sample_sizes.csv"
  )
)

focal_coefficients <-
  map_dfr(
    spec_registry$model_id,
    function(
      model_id
    ) {
      spec <-
        spec_registry |>
        filter(
          .data$model_id ==
            .env$model_id
        )

      coefficient_names <-
        names(
          fixef(
            models[[
              model_id
            ]]
          )
        )

      focal_term <-
        find_exact_interaction_term(
          coefficient_names,
          c(
            "muslim",
            spec$focal_var
          )
        )

      all_coefficients |>
        filter(
          .data$model_id ==
            .env$model_id,
          term ==
            focal_term
        ) |>
        mutate(
          focal_interpretation =
            if_else(
              model_id ==
                "V10",
              "Center-reference Muslim x current-FDI interaction in all-ideology model",
              if_else(
                model_id ==
                  "V07",
                "Muslim share x 21-month FDI change",
                "Muslim share x focal current FDI exposure"
              )
            )
        )
    }
  )

write_csv(
  focal_coefficients,
  file.path(
    output_dir,
    "04_focal_interaction_coefficients.csv"
  )
)

random_effect_stats <- function(
  fit,
  model_id
) {
  vc <-
    as.data.frame(
      VarCorr(
        fit
      )
    )

  ac_row <-
    vc |>
    filter(
      grp ==
        "ac_random",
      is.na(
        var2
      )
    )

  if (
    nrow(
      ac_row
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify AC random-intercept variance for ",
      model_id
    )
  }

  ac_variance <-
    ac_row$vcov[[
      1
    ]]

  residual_variance <-
    sigma(
      fit
    )^2

  tibble(
    model_id =
      model_id,

    ac_random_intercept_variance =
      ac_variance,

    ac_random_intercept_sd =
      sqrt(
        ac_variance
      ),

    residual_variance =
      residual_variance,

    residual_sd =
      sqrt(
        residual_variance
      ),

    icc =
      ac_variance /
      (
        ac_variance +
          residual_variance
      )
  )
}

random_effects <-
  imap_dfr(
    models,
    random_effect_stats
  )

write_csv(
  random_effects,
  file.path(
    output_dir,
    "06_random_effect_variance_and_icc.csv"
  )
)

convergence_diagnostics <-
  imap_dfr(
    fit_records,
    function(
      record,
      model_id
    ) {
      fit <-
        record$fit

      gradient <-
        fit@optinfo$derivs$gradient

      lme4_messages <-
        fit@optinfo$conv$lme4$messages

      optimizer_code <-
        fit@optinfo$conv$opt

      dropped <-
        attr(
          getME(
            fit,
            "X"
          ),
          "col.dropped"
        )

      tibble(
        model_id =
          model_id,

        singular =
          isSingular(
            fit,
            tol = 1e-4
          ),

        optimizer_code =
          if (
            is.null(
              optimizer_code
            ) ||
              length(
                optimizer_code
              ) == 0L
          ) {
            NA_integer_
          } else {
            as.integer(
              optimizer_code[[
                1
              ]]
            )
          },

        max_abs_gradient =
          if (
            is.null(
              gradient
            ) ||
              length(
                gradient
              ) == 0L
          ) {
            NA_real_
          } else {
            max(
              abs(
                gradient
              ),
              na.rm = TRUE
            )
          },

        n_dropped_fixed_columns =
          if (
            is.null(
              dropped
            )
          ) {
            0L
          } else {
            length(
              dropped
            )
          },

        fit_warnings =
          collapse_messages(
            record$warnings
          ),

        fit_messages =
          collapse_messages(
            record$messages
          ),

        lme4_convergence_messages =
          collapse_messages(
            lme4_messages
          )
      )
    }
  )

write_csv(
  convergence_diagnostics,
  file.path(
    output_dir,
    "07_convergence_and_singularity_audit.csv"
  )
)

v01_data <-
  model_samples[[
    "V01"
  ]] |>
  mutate(
    fdi_delta_60m =
      fdi_total_current -
      fdi_total_baseline
  )

v01_model <-
  models[[
    "V01"
  ]]

v01_beta <-
  fixef(
    v01_model
  )

v01_vcov <-
  as.matrix(
    vcov(
      v01_model
    )
  )

original_current_term <-
  find_exact_interaction_term(
    names(
      v01_beta
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

original_baseline_term <-
  find_exact_interaction_term(
    names(
      v01_beta
    ),
    c(
      "muslim",
      "fdi_total_baseline"
    )
  )

required_reparam_terms <-
  c(
    "fdi_total_current",
    "fdi_total_baseline",
    original_current_term,
    original_baseline_term
  )

missing_reparam_terms <-
  setdiff(
    required_reparam_terms,
    names(
      v01_beta
    )
  )

if (
  length(
    missing_reparam_terms
  ) > 0L
) {
  stop(
    "Missing V01 terms required for algebraic reparameterization: ",
    paste(
      missing_reparam_terms,
      collapse = ", "
    )
  )
}

linear_combo_row <- function(
  weights,
  parameterization,
  term,
  interpretation
) {
  L <-
    setNames(
      rep(
        0,
        length(
          v01_beta
        )
      ),
      names(
        v01_beta
      )
    )

  unknown <-
    setdiff(
      names(
        weights
      ),
      names(
        L
      )
    )

  if (
    length(
      unknown
    ) > 0L
  ) {
    stop(
      "Unknown coefficient names in linear combination: ",
      paste(
        unknown,
        collapse = ", "
      )
    )
  }

  L[
    names(
      weights
    )
  ] <-
    as.numeric(
      weights
    )

  estimate <-
    sum(
      L *
        v01_beta
    )

  variance <-
    as.numeric(
      t(
        L
      ) %*%
        v01_vcov %*%
        L
    )

  std_error <-
    sqrt(
      pmax(
        variance,
        0
      )
    )

  statistic <-
    if (
      is.finite(
        std_error
      ) &&
        std_error >
          0
    ) {
      estimate /
        std_error
    } else {
      NA_real_
    }

  p_value <-
    if (
      is.finite(
        statistic
      )
    ) {
      2 *
        pnorm(
          abs(
            statistic
          ),
          lower.tail = FALSE
        )
    } else {
      NA_real_
    }

  tibble(
    parameterization =
      parameterization,
    term =
      term,
    interpretation =
      interpretation,
    estimate =
      estimate,
    std_error =
      std_error,
    statistic =
      statistic,
    conf_low =
      estimate -
      1.96 *
      std_error,
    conf_high =
      estimate +
      1.96 *
      std_error,
    p_value_normal_approx =
      p_value
  )
}

original_current_interaction <-
  unname(
    v01_beta[
      original_current_term
    ]
  )

original_baseline_interaction <-
  unname(
    v01_beta[
      original_baseline_term
    ]
  )

reparameterized_delta_interaction <-
  original_current_interaction

reparameterized_baseline_interaction <-
  original_current_interaction +
  original_baseline_interaction

original_current_main <-
  unname(
    v01_beta[
      "fdi_total_current"
    ]
  )

original_baseline_main <-
  unname(
    v01_beta[
      "fdi_total_baseline"
    ]
  )

reparameterized_delta_main <-
  original_current_main

reparameterized_baseline_main <-
  original_current_main +
  original_baseline_main

original_fdi_component <-
  original_current_main *
    v01_data$fdi_total_current +
  original_baseline_main *
    v01_data$fdi_total_baseline +
  original_current_interaction *
    v01_data$muslim *
    v01_data$fdi_total_current +
  original_baseline_interaction *
    v01_data$muslim *
    v01_data$fdi_total_baseline

reparameterized_fdi_component <-
  reparameterized_delta_main *
    v01_data$fdi_delta_60m +
  reparameterized_baseline_main *
    v01_data$fdi_total_baseline +
  reparameterized_delta_interaction *
    v01_data$muslim *
    v01_data$fdi_delta_60m +
  reparameterized_baseline_interaction *
    v01_data$muslim *
    v01_data$fdi_total_baseline

max_algebraic_component_difference <-
  max(
    abs(
      original_fdi_component -
      reparameterized_fdi_component
    )
  )

if (
  !is.finite(
    max_algebraic_component_difference
  ) ||
    max_algebraic_component_difference >
      1e-10
) {
  stop(
    "Exact algebraic V01 reparameterization identity failed."
  )
}

if (
  !isTRUE(
    all.equal(
      original_current_interaction,
      reparameterized_delta_interaction,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "V01 current interaction does not equal change interaction."
  )
}

if (
  !isTRUE(
    all.equal(
      original_current_interaction +
        original_baseline_interaction,
      reparameterized_baseline_interaction,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "V01 baseline interaction transformation failed."
  )
}

unique_ac_v01 <-
  v01_data |>
  distinct(
    ac_uid,
    .keep_all = TRUE
  )

reparameterization_diagnostic <-
  bind_rows(
    linear_combo_row(
      setNames(
        1,
        original_current_term
      ),
      "Original V01",
      original_current_term,
      "Muslim share x current 2009-2014 total local FDI per 100,000"
    ),

    linear_combo_row(
      setNames(
        1,
        original_baseline_term
      ),
      "Original V01",
      original_baseline_term,
      "Muslim share x baseline 2004-2009 total local FDI per 100,000"
    ),

    linear_combo_row(
      setNames(
        1,
        original_current_term
      ),
      "Exact algebraic V01 reparameterization",
      "muslim:fdi_delta_60m",
      "Muslim share x change in 60-month FDI: current minus baseline"
    ),

    linear_combo_row(
      setNames(
        c(
          1,
          1
        ),
        c(
          original_current_term,
          original_baseline_term
        )
      ),
      "Exact algebraic V01 reparameterization",
      "muslim:fdi_total_baseline_conditional_on_delta",
      "Muslim share x baseline FDI conditional on the 60-month FDI change"
    )
  ) |>
  mutate(
    n_voters =
      nrow(
        v01_data
      ),

    n_ac =
      n_distinct(
        v01_data$ac_uid
      ),

    current_baseline_correlation_voter_rows =
      cor(
        v01_data$fdi_total_current,
        v01_data$fdi_total_baseline
      ),

    current_baseline_correlation_unique_ac =
      cor(
        unique_ac_v01$fdi_total_current,
        unique_ac_v01$fdi_total_baseline
      ),

    max_algebraic_component_difference =
      max_algebraic_component_difference,

    original_interaction_sum =
      original_current_interaction +
      original_baseline_interaction,

    reparameterized_baseline_interaction =
      reparameterized_baseline_interaction
  )

write_csv(
  reparameterization_diagnostic,
  file.path(
    output_dir,
    "05_primary_60m_change_reparameterization.csv"
  )
)

primary_regression_sample <-
  v01_data |>
  transmute(
    respondent_uid,
    ac_uid,
    state_no,
    y,
    muslim,
    fdi_total_current,
    fdi_total_baseline,
    fdi_delta_60m,
    ac_pop_100k,
    sc_share_pp,
    st_share_pp,
    religion_group,
    caste_group,
    education_harmonized,
    survey_weight
  )

write_csv(
  primary_regression_sample,
  file.path(
    output_dir,
    "08_primary_regression_sample.csv"
  )
)

ac_support_by_model <-
  imap_dfr(
    model_samples,
    function(
      dd,
      model_id
    ) {
      sizes <-
        dd |>
        count(
          ac_uid,
          state_no,
          name = "n_voters"
        )

      tibble(
        model_id =
          model_id,

        n_voters =
          nrow(
            dd
          ),

        n_ac =
          nrow(
            sizes
          ),

        voters_per_ac_min =
          min(
            sizes$n_voters
          ),

        voters_per_ac_q25 =
          as.numeric(
            quantile(
              sizes$n_voters,
              .25
            )
          ),

        voters_per_ac_median =
          median(
            sizes$n_voters
          ),

        voters_per_ac_q75 =
          as.numeric(
            quantile(
              sizes$n_voters,
              .75
            )
          ),

        voters_per_ac_max =
          max(
            sizes$n_voters
          ),

        ac_with_one_voter =
          sum(
            sizes$n_voters ==
              1L
          ),

        ac_with_two_or_fewer =
          sum(
            sizes$n_voters <=
              2L
          ),

        ac_with_five_or_more =
          sum(
            sizes$n_voters >=
              5L
          )
      )
    }
  )

write_csv(
  ac_support_by_model,
  file.path(
    output_dir,
    "09_ac_support_by_model.csv"
  )
)

v10_coefficients <-
  all_coefficients |>
  filter(
    model_id ==
      "V10",
    grepl(
      "muslim.*fdi_total_current|fdi_total_current.*muslim",
      term
    )
  )

write_csv(
  v10_coefficients,
  file.path(
    output_dir,
    "10_ideology_heterogeneity_terms.csv"
  )
)

candidate_audit_base <-
  stage_center |>
  filter(
    !is.na(
      bjp_candidate_present
    ),
    !is.na(
      ac_uid
    )
  )

candidate_voter_summary <-
  candidate_audit_base |>
  group_by(
    bjp_candidate_present
  ) |>
  summarise(
    unit =
      "voters",

    n =
      n(),

    bjp_share =
      mean(
        y,
        na.rm = TRUE
      ),

    mean_muslim_share =
      mean(
        muslim,
        na.rm = TRUE
      ),

    mean_current_fdi =
      mean(
        fdi_total_current,
        na.rm = TRUE
      ),

    mean_proxy_ac_pop =
      mean(
        proxy_ac_pop,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  )

candidate_ac_summary <-
  candidate_audit_base |>
  distinct(
    ac_uid,
    .keep_all = TRUE
  ) |>
  group_by(
    bjp_candidate_present
  ) |>
  summarise(
    unit =
      "assembly_constituencies",

    n =
      n(),

    bjp_share =
      NA_real_,

    mean_muslim_share =
      mean(
        muslim,
        na.rm = TRUE
      ),

    mean_current_fdi =
      mean(
        fdi_total_current,
        na.rm = TRUE
      ),

    mean_proxy_ac_pop =
      mean(
        proxy_ac_pop,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  )

candidate_audit <-
  bind_rows(
    candidate_voter_summary,
    candidate_ac_summary
  ) |>
  relocate(
    unit,
    bjp_candidate_present
  )

write_csv(
  candidate_audit,
  file.path(
    output_dir,
    "11_bjp_contestation_sample_audit.csv"
  )
)


v02_common_sample <-
  droplevels(
    model_samples[[
      "V02"
    ]]
  )

v02_common_warnings <-
  character()

v01_common_sample_model <-
  withCallingHandlers(
    lmer(
      formula(
        models[[
          "V01"
        ]]
      ),
      data =
        v02_common_sample,
      REML =
        FALSE,
      control =
        lmerControl(
          optimizer =
            "bobyqa",
          optCtrl =
            list(
              maxfun =
                300000
            )
        )
    ),
    warning =
      function(
        w
      ) {
        v02_common_warnings <<-
          c(
            v02_common_warnings,
            conditionMessage(
              w
            )
          )

        invokeRestart(
          "muffleWarning"
        )
      }
  )

extract_model_term <- function(
  fit,
  term,
  label,
  data
) {
  ct <-
    coef(
      summary(
        fit
      )
    )

  estimate <-
    as.numeric(
      ct[
        term,
        1
      ]
    )

  std_error <-
    as.numeric(
      ct[
        term,
        2
      ]
    )

  statistic <-
    estimate /
    std_error

  tibble(
    specification =
      label,

    n_voters =
      nrow(
        data
      ),

    n_ac =
      n_distinct(
        data$ac_uid
      ),

    estimate =
      estimate,

    std_error =
      std_error,

    conf_low =
      estimate -
      1.96 *
      std_error,

    conf_high =
      estimate +
      1.96 *
      std_error,

    p_value_normal_approx =
      2 *
      pnorm(
        abs(
          statistic
        ),
        lower.tail = FALSE
      )
  )
}

v01_primary_term <-
  find_exact_interaction_term(
    names(
      fixef(
        models[[
          "V01"
        ]]
      )
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

v01_common_term <-
  find_exact_interaction_term(
    names(
      fixef(
        v01_common_sample_model
      )
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

v02_term <-
  find_exact_interaction_term(
    names(
      fixef(
        models[[
          "V02"
        ]]
      )
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

expanded_control_decomposition <-
  bind_rows(
    extract_model_term(
      models[[
        "V01"
      ]],
      v01_primary_term,
      "V01 primary controls, full primary sample",
      model_samples[[
        "V01"
      ]]
    ),

    extract_model_term(
      v01_common_sample_model,
      v01_common_term,
      "V01 primary controls, V02 common sample",
      v02_common_sample
    ),

    extract_model_term(
      models[[
        "V02"
      ]],
      v02_term,
      "V02 expanded controls, V02 common sample",
      model_samples[[
        "V02"
      ]]
    )
  ) |>
  mutate(
    common_sample_refit_singular =
      c(
        NA,
        isSingular(
          v01_common_sample_model,
          tol =
            1e-4
        ),
        NA
      ),

    common_sample_refit_warnings =
      c(
        NA_character_,
        collapse_messages(
          v02_common_warnings
        ),
        NA_character_
      )
  )

write_csv(
  expanded_control_decomposition,
  file.path(
    output_dir,
    "12_expanded_control_common_sample_decomposition.csv"
  )
)

v10_model <-
  models[[
    "V10"
  ]]

v10_beta <-
  fixef(
    v10_model
  )

v10_vcov <-
  as.matrix(
    vcov(
      v10_model
    )
  )

v10_center_term <-
  find_exact_interaction_term(
    names(
      v10_beta
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

v10_linear_combo <- function(
  weights
) {
  L <-
    setNames(
      rep(
        0,
        length(
          v10_beta
        )
      ),
      names(
        v10_beta
      )
    )

  L[
    names(
      weights
    )
  ] <-
    as.numeric(
      weights
    )

  estimate <-
    sum(
      L *
        v10_beta
    )

  variance <-
    as.numeric(
      t(
        L
      ) %*%
        v10_vcov %*%
        L
    )

  std_error <-
    sqrt(
      pmax(
        variance,
        0
      )
    )

  statistic <-
    estimate /
    std_error

  tibble(
    estimate =
      estimate,

    std_error =
      std_error,

    conf_low =
      estimate -
      1.96 *
      std_error,

    conf_high =
      estimate +
      1.96 *
      std_error,

    p_value_normal_approx =
      2 *
      pnorm(
        abs(
          statistic
        ),
        lower.tail = FALSE
      )
  )
}

v10_ideology_levels <-
  c(
    "Center",
    "Left",
    "Right",
    "Mixed"
  )

ideology_specific_interactions <-
  purrr::map_dfr(
    v10_ideology_levels,
    function(
      ideology
    ) {
      if (
        ideology ==
          "Center"
      ) {
        slope_weights <-
          setNames(
            1,
            v10_center_term
          )

        difference_from_center <-
          0

        difference_se <-
          NA_real_

        difference_p <-
          NA_real_
      } else {
        triple_term <-
          find_exact_interaction_term(
            names(
              v10_beta
            ),
            c(
              "muslim",
              "fdi_total_current",
              paste0(
                "ideology4",
                ideology
              )
            )
          )

        slope_weights <-
          setNames(
            c(
              1,
              1
            ),
            c(
              v10_center_term,
              triple_term
            )
          )

        difference_row <-
          v10_linear_combo(
            setNames(
              1,
              triple_term
            )
          )

        difference_from_center <-
          difference_row$estimate

        difference_se <-
          difference_row$std_error

        difference_p <-
          difference_row$p_value_normal_approx
      }

      bind_cols(
        tibble(
          ideology =
            ideology
        ),

        v10_linear_combo(
          slope_weights
        ),

        tibble(
          difference_from_center =
            difference_from_center,

          difference_se =
            difference_se,

          difference_p =
            difference_p
        )
      )
    }
  )

write_csv(
  ideology_specific_interactions,
  file.path(
    output_dir,
    "13_ideology_specific_current_fdi_interactions.csv"
  )
)

v08_model <-
  models[[
    "V08"
  ]]

v08_data <-
  droplevels(
    model_samples[[
      "V08"
    ]]
  )

v08_fixed_formula <-
  reformulas::nobars(
    formula(
      v08_model
    )
  )

v08_fixed_model <-
  lm(
    v08_fixed_formula,
    data =
      v08_data
  )

v08_mixed_term <-
  find_exact_interaction_term(
    names(
      fixef(
        v08_model
      )
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

v08_fixed_term <-
  find_exact_interaction_term(
    names(
      coef(
        v08_fixed_model
      )
    ),
    c(
      "muslim",
      "fdi_total_current"
    )
  )

v08_mixed_ct <-
  coef(
    summary(
      v08_model
    )
  )

v08_fixed_ct <-
  coef(
    summary(
      v08_fixed_model
    )
  )

v08_mixed_estimate <-
  as.numeric(
    v08_mixed_ct[
      v08_mixed_term,
      1
    ]
  )

v08_mixed_se <-
  as.numeric(
    v08_mixed_ct[
      v08_mixed_term,
      2
    ]
  )

v08_fixed_estimate <-
  as.numeric(
    v08_fixed_ct[
      v08_fixed_term,
      1
    ]
  )

v08_fixed_se <-
  as.numeric(
    v08_fixed_ct[
      v08_fixed_term,
      2
    ]
  )

v08_variance <-
  as.data.frame(
    VarCorr(
      v08_model
    )
  ) |>
  filter(
    grp ==
      "ac_random",
    is.na(
      var2
    )
  ) |>
  pull(
    vcov
  )

v08_boundary_check <-
  tibble(
    estimator =
      c(
        "V08 mixed LPM",
        "Fixed-effects-only boundary sensitivity"
      ),

    estimate =
      c(
        v08_mixed_estimate,
        v08_fixed_estimate
      ),

    std_error =
      c(
        v08_mixed_se,
        v08_fixed_se
      )
  ) |>
  mutate(
    conf_low =
      estimate -
      1.96 *
      std_error,

    conf_high =
      estimate +
      1.96 *
      std_error,

    p_value_normal_approx =
      2 *
      pnorm(
        abs(
          estimate /
            std_error
        ),
        lower.tail = FALSE
      ),

    v08_random_intercept_variance =
      v08_variance,

    v08_singular =
      isSingular(
        v08_model,
        tol =
          1e-4
      ),

    mixed_minus_fixed_estimate =
      v08_mixed_estimate -
      v08_fixed_estimate
  )

write_csv(
  v08_boundary_check,
  file.path(
    output_dir,
    "14_v08_singularity_boundary_sensitivity.csv"
  )
)

cat(
  "\n===== EXPANDED-CONTROL COMMON-SAMPLE DECOMPOSITION =====\n"
)

print(
  expanded_control_decomposition,
  n = Inf,
  width = Inf
)

cat(
  "\n===== IDEOLOGY-SPECIFIC CURRENT-FDI INTERACTIONS =====\n"
)

print(
  ideology_specific_interactions,
  n = Inf,
  width = Inf
)

cat(
  "\n===== V08 SINGULARITY BOUNDARY SENSITIVITY =====\n"
)

print(
  v08_boundary_check,
  n = Inf,
  width = Inf
)

write_csv(
  primary_funnel,
  file.path(
    output_dir,
    "01_primary_sample_funnel.csv"
  )
)

saveRDS(
  models,
  file.path(
    output_dir,
    "models.rds"
  )
)

saveRDS(
  model_samples,
  file.path(
    output_dir,
    "model_samples.rds"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  con =
    file.path(
      output_dir,
      "session_info.txt"
    )
)

cat(
  "\n===== PRIMARY VOTER SAMPLE FUNNEL =====\n"
)

print(
  primary_funnel,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CANONICAL VOTER MODEL MANIFEST =====\n"
)

print(
  model_metadata |>
    select(
      model_id,
      role,
      sample_key,
      sector,
      geography,
      functional_form,
      treatment,
      weighted,
      n_voters,
      n_ac,
      n_states,
      n_dropped_fixed_columns
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== FOCAL INTERACTION ESTIMATES =====\n"
)

print(
  focal_coefficients |>
    select(
      model_id,
      role,
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value_normal_approx,
      sample_key,
      sector,
      geography,
      functional_form,
      weighted
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY 60-MONTH REPARAMETERIZATION =====\n"
)

print(
  reparameterization_diagnostic,
  n = Inf,
  width = Inf
)

cat(
  "\n===== RANDOM-EFFECT VARIANCE / ICC =====\n"
)

print(
  random_effects,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CONVERGENCE / SINGULARITY AUDIT =====\n"
)

print(
  convergence_diagnostics,
  n = Inf,
  width = Inf
)

cat(
  "\n===== ALL-IDEOLOGY CURRENT-FDI HETEROGENEITY TERMS =====\n"
)

print(
  v10_coefficients |>
    select(
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value_normal_approx
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== BJP CONTESTATION SAMPLE AUDIT =====\n"
)

print(
  candidate_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nOutputs written to: ",
  output_dir,
  "\n",
  sep = ""
)

cat(
  "\nCANONICAL_VOTER_MODELS_COMPLETE\n"
)

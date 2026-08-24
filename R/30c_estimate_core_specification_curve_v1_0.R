suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(fixest)
  library(lme4)
  library(ggplot2)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

final_dir <-
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
    "r30_core_specification_curve_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

registry <-
  read_csv(
    "outputs/r30_specification_registry_freeze_v1_0/01_frozen_core_specification_registry.csv",
    show_col_types = FALSE
  )

if (
  nrow(
    registry
  ) !=
    40L
) {
  stop(
    "Frozen R30 registry must contain exactly 40 rows."
  )
}

ideology <-
  readRDS(
    file.path(
      final_dir,
      "ac_year_ideology_summary.rds"
    )
  )

respondents <-
  readRDS(
    file.path(
      final_dir,
      "nes_respondent_analysis.rds"
    )
  )

ac_change <-
  readRDS(
    file.path(
      final_dir,
      "ac_change.rds"
    )
  )


corrected_employment <-
  readRDS(
    "outputs/r31_corrected_employment_control_v1_0/04_corrected_employment_by_ac.rds"
  ) |>
  select(
    ac_uid,
    employment_intensity_ec13_per_2011_population
  )

if (
  anyDuplicated(
    corrected_employment$ac_uid
  ) >
    0L
) {
  stop(
    "Corrected employment-control artifact is not unique by ac_uid."
  )
}

fdi12 <-
  readRDS(
    "outputs/fdi_12m_temporal_robustness_v1_0/03_fdi_ac_12m.rds"
  )

canonical_ac_models <-
  readRDS(
    "outputs/ac_canonical_v1_0/models.rds"
  )

canonical_ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

canonical_voter_models <-
  readRDS(
    "outputs/voter_canonical_v1_0/models.rds"
  )

canonical_voter_samples <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
  )

sector_ac <-
  read_csv(
    "outputs/sector_form_native_common_v1_0/05_centrist_ac_sector_form_native_common.csv",
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

sector_voter <-
  read_csv(
    "outputs/sector_form_native_common_v1_0/06_centrist_voter_sector_form_native_common.csv",
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

canonical_rows <-
  registry |>
  filter(
    source_artifact ==
      "canonical_ac_change"
  )

post12_rows <-
  registry |>
  filter(
    source_artifact ==
      "post_primary_12m"
  )

canonical_fdi_vars <-
  unique(
    c(
      canonical_rows$current_var,
      canonical_rows$baseline_var
    )
  )

post12_fdi_vars <-
  unique(
    c(
      post12_rows$current_var,
      post12_rows$baseline_var
    )
  )

missing_canonical <-
  setdiff(
    canonical_fdi_vars,
    names(
      ac_change
    )
  )

missing_12m <-
  setdiff(
    post12_fdi_vars,
    names(
      fdi12
    )
  )

if (
  length(
    missing_canonical
  ) >
    0L
) {
  stop(
    "Missing canonical FDI variables: ",
    paste(
      missing_canonical,
      collapse = ", "
    )
  )
}

if (
  length(
    missing_12m
  ) >
    0L
) {
  stop(
    "Missing 12-month FDI variables: ",
    paste(
      missing_12m,
      collapse = ", "
    )
  )
}

canonical_payload <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      canonical_fdi_vars
    )
  )

fdi12_payload <-
  fdi12 |>
  select(
    ac_uid,
    all_of(
      post12_fdi_vars
    )
  )

if (
  anyDuplicated(
    canonical_payload$ac_uid
  ) >
    0L ||
    anyDuplicated(
      fdi12_payload$ac_uid
    ) >
      0L
) {
  stop(
    "FDI payload is not unique by ac_uid."
  )
}

all_fdi_vars <-
  unique(
    c(
      canonical_fdi_vars,
      post12_fdi_vars
    )
  )

ac_base <-
  ideology |>
  filter(
    year ==
      2014,
    as.character(
      ideology
    ) ==
      "Center"
  ) |>
  select(
    -any_of(
      all_fdi_vars
    )
  ) |>
  left_join(
    canonical_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  left_join(
    fdi12_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  left_join(
    corrected_employment,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    y =
      as.numeric(
        weighted_share_voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
      )
  )

if (
  anyDuplicated(
    ac_base$ac_uid
  ) >
    0L
) {
  stop(
    "AC base is not unique by ac_uid."
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
        ref =
          reference
      )
  }

  out
}

voter_base <-
  respondents |>
  select(
    -any_of(
      all_fdi_vars
    )
  ) |>
  left_join(
    canonical_payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  left_join(
    fdi12_payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  left_join(
    corrected_employment,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    y =
      as.numeric(
        voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
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

    employment_intensity_ec13_pp =
      100 *
      as.numeric(
        employment_intensity_ec13_per_2011_population
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
      )
  )

if (
  anyDuplicated(
    voter_base$respondent_uid
  ) >
    0L
) {
  stop(
    "Voter base is not unique by respondent_uid."
  )
}

ac_primary_controls <-
  c(
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share"
  )

ac_expanded_controls <-
  c(
    ac_primary_controls,
    "employment_intensity_ec13_per_2011_population",
    "ed_sec_share"
  )

voter_primary_controls <-
  c(
    "ac_pop_100k",
    "sc_share_pp",
    "st_share_pp",
    "religion_x",
    "caste_x",
    "education_x",
    "state_fe"
  )

voter_expanded_controls <-
  c(
    voter_primary_controls,
    "employment_intensity_ec13_pp",
    "ed_sec_share_pp"
  )

complete_variables <- function(
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
      ) ==
        0L
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
    collapse =
      " | "
  )
}

find_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <-
    coefficient_names[
      vapply(
        strsplit(
          coefficient_names,
          ":",
          fixed =
            TRUE
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
      "Could not uniquely identify interaction: ",
      paste(
        variables,
        collapse =
          " x "
      ),
      ". Hits: ",
      paste(
        hits,
        collapse =
          ", "
      )
    )
  }

  hits[[1]]
}

fit_ac_spec <- function(
  spec
) {
  controls <-
    if (
      spec$control_set ==
        "Expanded"
    ) {
      ac_expanded_controls
    } else {
      ac_primary_controls
    }

  dd <-
    ac_base |>
    mutate(
      x_current =
        .data[[
          spec$current_var
        ]],

      x_baseline =
        .data[[
          spec$baseline_var
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
        x_current
      ),
      is.finite(
        x_baseline
      ),
      !is.na(
        state_no
      ),
      !is.na(
        pc_cluster_id
      )
    )

  dd <-
    complete_variables(
      dd,
      controls
    )

  fml <-
    as.formula(
      paste0(
        "y ~ muslim * x_current + muslim * x_baseline + ",
        paste(
          controls,
          collapse =
            " + "
        ),
        " | state_no"
      )
    )

  fit <-
    feols(
      fml,
      data =
        dd,
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
      dd,
    formula =
      fml
  )
}

fit_voter_spec <- function(
  spec
) {
  controls <-
    if (
      spec$control_set ==
        "Expanded"
    ) {
      voter_expanded_controls
    } else {
      voter_primary_controls
    }

  dd <-
    voter_base |>
    filter(
      year ==
        2014,
      vote_valid %in%
        TRUE,
      !is.na(
        y
      ),
      ideology_complete %in%
        TRUE,
      as.character(
        voter_ideology
      ) ==
        "Center",
      bjp_candidate_present %in%
        TRUE,
      fdi_spatial_support %in%
        TRUE
    ) |>
    mutate(
      x_current =
        .data[[
          spec$current_var
        ]],

      x_baseline =
        .data[[
          spec$baseline_var
        ]]
    ) |>
    filter(
      is.finite(
        muslim
      ),
      is.finite(
        x_current
      ),
      is.finite(
        x_baseline
      ),
      !is.na(
        ac_random
      )
    )

  dd <-
    complete_variables(
      dd,
      controls
    ) |>
    droplevels()

  warnings <-
    character()

  messages <-
    character()

  fml <-
    as.formula(
      paste0(
        "y ~ muslim * x_current + muslim * x_baseline + ",
        paste(
          controls,
          collapse =
            " + "
        ),
        " + (1 | ac_random)"
      )
    )

  fit <-
    withCallingHandlers(
      lmer(
        fml,
        data =
          dd,
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

  optimizer_code <-
    fit@optinfo$conv$opt

  if (
    is.null(
      optimizer_code
    ) ||
      length(
        optimizer_code
      ) ==
        0L
  ) {
    optimizer_code <-
      NA_integer_
  } else {
    optimizer_code <-
      as.integer(
        optimizer_code[[
          1
        ]]
      )
  }

  gradient <-
    fit@optinfo$derivs$gradient

  max_gradient <-
    if (
      is.null(
        gradient
      ) ||
        length(
          gradient
        ) ==
          0L
    ) {
      NA_real_
    } else {
      max(
        abs(
          gradient
        )
      )
    }

  list(
    fit =
      fit,
    data =
      dd,
    formula =
      fml,
    singular =
      isSingular(
        fit,
        tol =
          1e-4
      ),
    optimizer_code =
      optimizer_code,
    max_gradient =
      max_gradient,
    warnings =
      collapse_messages(
        warnings
      ),
    messages =
      collapse_messages(
        messages
      )
  )
}

extract_ac_result <- function(
  record,
  spec
) {
  fit <-
    record$fit

  term <-
    find_interaction_term(
      names(
        coef(
          fit
        )
      ),
      c(
        "muslim",
        "x_current"
      )
    )

  ct <-
    coeftable(
      fit
    )

  estimate <-
    unname(
      ct[
        term,
        1
      ]
    )

  se <-
    unname(
      ct[
        term,
        2
      ]
    )

  p <-
    unname(
      ct[
        term,
        4
      ]
    )

  x_sd <-
    sd(
      record$data$x_current
    )

  tibble(
    spec_id =
      spec$spec_id,
    level =
      "AC",
    sector =
      spec$sector,
    family =
      spec$family,
    geography =
      spec$geography,
    functional_form =
      spec$functional_form,
    control_set =
      spec$control_set,
    role =
      spec$role,
    design_status =
      spec$design_status,
    source_artifact =
      spec$source_artifact,
    current_var =
      spec$current_var,
    baseline_var =
      spec$baseline_var,
    focal_term =
      term,
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
        se,
    p_value =
      p,
    current_fdi_sd =
      x_sd,
    standardized_estimate_pp =
      estimate *
        x_sd,
    standardized_se_pp =
      se *
        x_sd,
    standardized_conf_low_pp =
      (
        estimate -
          1.96 *
            se
      ) *
        x_sd,
    standardized_conf_high_pp =
      (
        estimate +
          1.96 *
            se
      ) *
        x_sd,
    n =
      nrow(
        record$data
      ),
    n_ac =
      n_distinct(
        record$data$ac_uid
      ),
    n_states =
      n_distinct(
        record$data$state_no
      ),
    n_clusters =
      n_distinct(
        record$data$pc_cluster_id
      ),
    singular =
      NA,
    optimizer_code =
      NA_integer_,
    max_abs_gradient =
      NA_real_,
    warnings =
      NA_character_
  )
}

extract_voter_result <- function(
  record,
  spec
) {
  fit <-
    record$fit

  beta <-
    fixef(
      fit
    )

  term <-
    find_interaction_term(
      names(
        beta
      ),
      c(
        "muslim",
        "x_current"
      )
    )

  ct <-
    coef(
      summary(
        fit
      )
    )

  estimate <-
    unname(
      ct[
        term,
        1
      ]
    )

  se <-
    unname(
      ct[
        term,
        2
      ]
    )

  statistic <-
    estimate /
      se

  p <-
    2 *
    pnorm(
      abs(
        statistic
      ),
      lower.tail =
        FALSE
    )

  x_sd <-
    sd(
      record$data$x_current
    )

  tibble(
    spec_id =
      spec$spec_id,
    level =
      "Voter",
    sector =
      spec$sector,
    family =
      spec$family,
    geography =
      spec$geography,
    functional_form =
      spec$functional_form,
    control_set =
      spec$control_set,
    role =
      spec$role,
    design_status =
      spec$design_status,
    source_artifact =
      spec$source_artifact,
    current_var =
      spec$current_var,
    baseline_var =
      spec$baseline_var,
    focal_term =
      term,
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
        se,
    p_value =
      p,
    current_fdi_sd =
      x_sd,
    standardized_estimate_pp =
      estimate *
        x_sd,
    standardized_se_pp =
      se *
        x_sd,
    standardized_conf_low_pp =
      (
        estimate -
          1.96 *
            se
      ) *
        x_sd,
    standardized_conf_high_pp =
      (
        estimate +
          1.96 *
            se
      ) *
        x_sd,
    n =
      nrow(
        record$data
      ),
    n_ac =
      n_distinct(
        record$data$ac_uid
      ),
    n_states =
      n_distinct(
        record$data$state_no
      ),
    n_clusters =
      n_distinct(
        record$data$ac_uid
      ),
    singular =
      record$singular,
    optimizer_code =
      record$optimizer_code,
    max_abs_gradient =
      record$max_gradient,
    warnings =
      record$warnings
  )
}

records <-
  list()

results <-
  list()

for (
  i in
    seq_len(
      nrow(
        registry
      )
    )
) {
  spec <-
    registry[
      i,
      ,
      drop = FALSE
    ]

  if (
    spec$level ==
      "AC"
  ) {
    record <-
      fit_ac_spec(
        spec
      )

    result <-
      extract_ac_result(
        record,
        spec
      )
  } else {
    record <-
      fit_voter_spec(
        spec
      )

    result <-
      extract_voter_result(
        record,
        spec
      )
  }

  records[[
    spec$spec_id
  ]] <-
    record

  results[[
    spec$spec_id
  ]] <-
    result
}

results <-
  bind_rows(
    results
  )

if (
  nrow(
    results
  ) !=
    40L
) {
  stop(
    "R30c did not produce exactly 40 core model results."
  )
}

if (
  any(
    !is.finite(
      results$current_fdi_sd
    ) |
      results$current_fdi_sd <=
        0
  )
) {
  print(
    results |>
      filter(
        !is.finite(
          current_fdi_sd
        ) |
          current_fdi_sd <=
            0
      ),
    n = Inf,
    width = Inf
  )

  stop(
    "At least one specification has zero or invalid FDI variation."
  )
}

ac_models_out <-
  records[
    results$spec_id[
      results$level ==
        "AC"
    ]
  ] |>
  map(
    "fit"
  )

voter_models_out <-
  records[
    results$spec_id[
      results$level ==
        "Voter"
    ]
  ] |>
  map(
    "fit"
  )

ac_samples_out <-
  records[
    results$spec_id[
      results$level ==
        "AC"
    ]
  ] |>
  map(
    "data"
  )

voter_samples_out <-
  records[
    results$spec_id[
      results$level ==
        "Voter"
    ]
  ] |>
  map(
    "data"
  )

canonical_anchor_registry <-
  tribble(
    ~new_spec_id, ~old_model_id, ~old_level, ~old_focal_var,

    "AC__T_LOCAL_RAW__Primary",
    "AC01",
    "AC",
    "fdi_current",

    "AC__T_LOCAL_RAW__Expanded",
    "AC02",
    "AC",
    "fdi_current",

    "AC__T_LOCAL_LOG__Primary",
    "AC03",
    "AC",
    "fdi_current",

    "AC__T_OWN_RAW__Primary",
    "AC04",
    "AC",
    "fdi_current",

    "AC__M_LOCAL_RAW__Primary",
    "AC05",
    "AC",
    "fdi_current",

    "AC__T_21M_RAW__Primary",
    "AC07",
    "AC",
    "fdi_current",

    "AC__M_21M_RAW__Primary",
    "AC08",
    "AC",
    "fdi_current",

    "Voter__T_LOCAL_RAW__Primary",
    "V01",
    "Voter",
    "fdi_total_current",

    "Voter__T_LOCAL_RAW__Expanded",
    "V02",
    "Voter",
    "fdi_total_current",

    "Voter__T_LOCAL_LOG__Primary",
    "V03",
    "Voter",
    "fdi_log_current",

    "Voter__T_OWN_RAW__Primary",
    "V04",
    "Voter",
    "fdi_own_current",

    "Voter__M_LOCAL_RAW__Primary",
    "V05",
    "Voter",
    "fdi_mfg_current",

    "Voter__T_21M_RAW__Primary",
    "V07",
    "Voter",
    "fdi_change_21m"
  )

extract_old_estimate <- function(
  fit,
  variables,
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
  } else {
    beta <-
      fixef(
        fit
      )
  }

  term <-
    find_interaction_term(
      names(
        beta
      ),
      variables
    )

  unname(
    beta[
      term
    ]
  )
}

anchor_checks <-
  map_dfr(
    seq_len(
      nrow(
        canonical_anchor_registry
      )
    ),
    function(
      i
    ) {
      anchor <-
        canonical_anchor_registry[
          i,
          ,
          drop = FALSE
        ]

      new_result <-
        results |>
        filter(
          spec_id ==
            anchor$new_spec_id
        )

      if (
        nrow(
          new_result
        ) !=
          1L
      ) {
        stop(
          "New anchor result missing: ",
          anchor$new_spec_id
        )
      }

      if (
        anchor$old_level ==
          "AC"
      ) {
        old_fit <-
          canonical_ac_models[[
            anchor$old_model_id
          ]]

        old_sample <-
          canonical_ac_samples[[
            anchor$old_model_id
          ]]

        new_sample <-
          ac_samples_out[[
            anchor$new_spec_id
          ]]

        ids_identical <-
          identical(
            sort(
              as.character(
                old_sample$ac_uid
              )
            ),
            sort(
              as.character(
                new_sample$ac_uid
              )
            )
          )
      } else {
        old_fit <-
          canonical_voter_models[[
            anchor$old_model_id
          ]]

        old_sample <-
          canonical_voter_samples[[
            anchor$old_model_id
          ]]

        new_sample <-
          voter_samples_out[[
            anchor$new_spec_id
          ]]

        ids_identical <-
          identical(
            sort(
              as.character(
                old_sample$respondent_uid
              )
            ),
            sort(
              as.character(
                new_sample$respondent_uid
              )
            )
          )
      }

      old_estimate <-
        extract_old_estimate(
          old_fit,
          c(
            "muslim",
            anchor$old_focal_var
          ),
          anchor$old_level
        )

      tibble(
        new_spec_id =
          anchor$new_spec_id,
        old_model_id =
          anchor$old_model_id,
        level =
          anchor$old_level,
        old_estimate =
          old_estimate,
        r30c_estimate =
          new_result$estimate,
        absolute_difference =
          abs(
            old_estimate -
              new_result$estimate
          ),
        exact_sample_ids =
          ids_identical,
        old_n =
          nrow(
            old_sample
          ),
        new_n =
          nrow(
            new_sample
          )
      )
    }
  )

if (
  any(
    !anchor_checks$exact_sample_ids
  )
) {
  print(
    anchor_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one R30c anchor does not reproduce the canonical model sample."
  )
}

if (
  any(
    anchor_checks$level ==
      "AC" &
      anchor_checks$absolute_difference >
        1e-8
  ) ||
    any(
      anchor_checks$level ==
        "Voter" &
      anchor_checks$absolute_difference >
        1e-5
    )
) {
  print(
    anchor_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one R30c canonical anchor coefficient failed reproduction."
  )
}

sector_matrix_checks <-
  bind_rows(
    results |>
      filter(
        spec_id ==
          "AC__M_LOCAL_LOG__Primary"
      ) |>
      transmute(
        level =
          "AC",
        spec_id,
        r30c_estimate =
          estimate
      ) |>
      mutate(
        stored_estimate =
          sector_ac |>
          filter(
            cell_id ==
              "manufacturing_log1p"
          ) |>
          pull(
            estimate
          )
      ),

    results |>
      filter(
        spec_id ==
          "Voter__M_LOCAL_LOG__Primary"
      ) |>
      transmute(
        level =
          "Voter",
        spec_id,
        r30c_estimate =
          estimate
      ) |>
      mutate(
        stored_estimate =
          sector_voter |>
          filter(
            cell_id ==
              "manufacturing_log1p"
          ) |>
          pull(
            estimate
          )
      )
  ) |>
  mutate(
    absolute_difference =
      abs(
        stored_estimate -
          r30c_estimate
      )
  )

if (
  nrow(
    sector_matrix_checks
  ) !=
    2L ||
    any(
      sector_matrix_checks$absolute_difference >
        1e-5
    )
) {
  print(
    sector_matrix_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "Manufacturing log1p anchor failed reproduction."
  )
}

fit_diagnostics <-
  results |>
  select(
    spec_id,
    level,
    sector,
    family,
    control_set,
    n,
    n_ac,
    n_states,
    n_clusters,
    singular,
    optimizer_code,
    max_abs_gradient,
    warnings
  )

sample_counts <-
  results |>
  select(
    spec_id,
    level,
    sector,
    family,
    geography,
    functional_form,
    control_set,
    design_status,
    n,
    n_ac,
    n_states,
    n_clusters,
    current_fdi_sd
  )

plot_labels <-
  tibble(
    family =
      c(
        "60-month current + baseline",
        "60-month current + baseline",
        "60-month current + baseline",
        "21-month change + early baseline",
        "12-month change + early baseline"
      ),

    geography =
      c(
        "Local",
        "Local",
        "Own AC",
        "Local",
        "Local"
      ),

    functional_form =
      c(
        "Raw",
        "log1p",
        "Raw",
        "Raw",
        "Raw"
      ),

    spec_label =
      c(
        "60m local raw",
        "60m local log1p",
        "60m own-AC raw",
        "21m change + baseline",
        "12m change + baseline\n(post-estimation)"
      ),

    order =
      1:5
  )

figure_data <-
  results |>
  left_join(
    plot_labels,
    by =
      c(
        "family",
        "geography",
        "functional_form"
      ),
    relationship =
      "many-to-one"
  )

if (
  any(
    is.na(
      figure_data$spec_label
    )
  )
) {
  print(
    figure_data |>
      filter(
        is.na(
          spec_label
        )
      ),
    n = Inf,
    width = Inf
  )

  stop(
    "At least one R30 result lacks a figure label."
  )
}

figure_data <-
  figure_data |>
  mutate(
    spec_label =
      factor(
        spec_label,
        levels =
          rev(
            plot_labels |>
              arrange(
                order
              ) |>
              pull(
                spec_label
              )
          )
      ),

    control_set =
      factor(
        control_set,
        levels =
          c(
            "Primary",
            "Expanded"
          )
      ),

    sector =
      factor(
        sector,
        levels =
          c(
            "Total",
            "Manufacturing"
          )
      )
  )

make_curve_plot <- function(
  data,
  title_text,
  subtitle_text
) {
  dodge <-
    position_dodge(
      width =
        0.5
    )

  ggplot(
    data,
    aes(
      x =
        spec_label,
      y =
        standardized_estimate_pp,
      shape =
        control_set
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
    geom_errorbar(
      aes(
        ymin =
          standardized_conf_low_pp,
        ymax =
          standardized_conf_high_pp
      ),
      width =
        0,
      position =
        dodge,
      linewidth =
        0.5
    ) +
    geom_point(
      position =
        dodge,
      size =
        2.4
    ) +
    facet_wrap(
      vars(
        sector
      ),
      nrow =
        1,
      scales =
        "free_y"
    ) +
    coord_flip() +
    scale_shape_manual(
      values =
        c(
          Primary =
            16,
          Expanded =
            1
        )
    ) +
    labs(
      title =
        title_text,

      subtitle =
        subtitle_text,

      x =
        NULL,

      y =
        paste0(
          "Change in BJP support (pp) in the +1-pp Muslim-share gradient\n",
          "for a 1-SD increase in the specification's FDI exposure"
        ),

      shape =
        "Controls",

      caption =
        paste0(
          "Points are FDI x 2001 Muslim-share interactions normalized by the within-model SD of the relevant FDI exposure. ",
          "Thus raw, log1p, spatial, 21-month-change, and 12-month-change specifications are displayed on a common substantive scale. ",
          "Intervals are 95% CIs. The 12-month specification is a post-estimation temporal robustness check."
        )
    ) +
    theme_minimal(
      base_size =
        11
    ) +
    theme(
      panel.grid.minor =
        element_blank(),
      plot.title =
        element_text(
          face =
            "bold"
        ),
      strip.text =
        element_text(
          face =
            "bold"
        ),
      legend.position =
        "bottom"
    )
}

ac_figure <-
  make_curve_plot(
    figure_data |>
      filter(
        level ==
          "AC"
      ),
    title_text =
      "Robustness of the FDI x Muslim-share relationship",
    subtitle_text =
      "Main Figure 6: constituency-level centrist BJP support"
  )

voter_figure <-
  make_curve_plot(
    figure_data |>
      filter(
        level ==
          "Voter"
      ),
    title_text =
      "Robustness of the FDI x Muslim-share relationship",
    subtitle_text =
      "Appendix: voter-level centrist BJP support"
  )

write_csv(
  results,
  file.path(
    output_dir,
    "01_core_specification_results.csv"
  )
)

write_csv(
  fit_diagnostics,
  file.path(
    output_dir,
    "02_fit_diagnostics.csv"
  )
)

write_csv(
  anchor_checks,
  file.path(
    output_dir,
    "03_canonical_anchor_reproduction.csv"
  )
)

write_csv(
  sector_matrix_checks,
  file.path(
    output_dir,
    "04_manufacturing_log_anchor_reproduction.csv"
  )
)

write_csv(
  sample_counts,
  file.path(
    output_dir,
    "05_sample_counts_and_exposure_sd.csv"
  )
)

write_csv(
  figure_data |>
    mutate(
      spec_label =
        as.character(
          spec_label
        ),
      control_set =
        as.character(
          control_set
        ),
      sector =
        as.character(
          sector
        )
    ),
  file.path(
    output_dir,
    "06_figure_data.csv"
  )
)

saveRDS(
  ac_models_out,
  file.path(
    output_dir,
    "07_ac_models.rds"
  )
)

saveRDS(
  voter_models_out,
  file.path(
    output_dir,
    "08_voter_models.rds"
  )
)

saveRDS(
  ac_samples_out,
  file.path(
    output_dir,
    "09_ac_model_samples.rds"
  )
)

saveRDS(
  voter_samples_out,
  file.path(
    output_dir,
    "10_voter_model_samples.rds"
  )
)

ggsave(
  filename =
    file.path(
      output_dir,
      "11_main_figure_6_ac_specification_robustness.pdf"
    ),
  plot =
    ac_figure,
  width =
    8.5,
  height =
    5.6,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "11_main_figure_6_ac_specification_robustness.png"
    ),
  plot =
    ac_figure,
  width =
    8.5,
  height =
    5.6,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "12_appendix_voter_specification_robustness.pdf"
    ),
  plot =
    voter_figure,
  width =
    8.5,
  height =
    5.6,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "12_appendix_voter_specification_robustness.png"
    ),
  plot =
    voter_figure,
  width =
    8.5,
  height =
    5.6,
  units =
    "in",
  dpi =
    300
)

notes <-
  c(
    "R30c CORE SPECIFICATION ROBUSTNESS",
    "",
    "The specification universe is read from the frozen R30b registry and is not modified based on results.",
    "",
    "There are 20 AC specifications and 20 voter specifications:",
    "five Total-FDI definitions x Primary/Expanded controls;",
    "five Manufacturing-FDI definitions x Primary/Expanded controls.",
    "",
    "The 12-month models are explicitly post-estimation temporal robustness checks.",
    "",
    "MAIN FIGURE STANDARDIZATION",
    "Raw interaction coefficients cannot be compared directly across raw, log1p, own-AC, and temporal-change FDI scales.",
    "The plotted quantity is beta(FDI x Muslim share) multiplied by the within-model SD of the relevant FDI exposure.",
    "Because Muslim share is stored as a 0-1 proportion and BJP support as a 0-1 outcome, this equals the percentage-point change in BJP support in the +1-percentage-point Muslim-share gradient associated with a one-SD increase in FDI.",
    "",
    "All unstandardized coefficients are retained in 01_core_specification_results.csv.",
    "",
    "AC models retain state fixed effects and PC-clustered standard errors.",
    "Voter models retain state fixed effects and an AC random intercept and are unweighted.",
    "",
    "Main Figure 6 is the AC robustness figure.",
    "The parallel voter figure is appendix-only.",
    "",
    "The R30c anchor checks must reproduce the canonical AC and voter models and the previously audited Manufacturing-log models before the script completes."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "13_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "14_session_info.txt"
  )
)

cat(
  "\n===== CANONICAL ANCHOR REPRODUCTION =====\n"
)

print(
  anchor_checks,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MANUFACTURING LOG ANCHOR REPRODUCTION =====\n"
)

print(
  sector_matrix_checks,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CORE SPECIFICATION RESULTS =====\n"
)

print(
  results |>
    select(
      level,
      sector,
      family,
      geography,
      functional_form,
      control_set,
      design_status,
      estimate,
      std_error,
      p_value,
      current_fdi_sd,
      standardized_estimate_pp,
      standardized_conf_low_pp,
      standardized_conf_high_pp,
      n,
      n_ac
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== VOTER FIT DIAGNOSTICS =====\n"
)

print(
  fit_diagnostics |>
    filter(
      level ==
        "Voter"
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nR30C_CORE_SPECIFICATION_CURVE_COMPLETE\n"
)

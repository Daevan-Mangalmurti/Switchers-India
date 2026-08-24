suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
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
    "r31_demographic_context_robustness_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

registry <-
  read_csv(
    "config/r31_demographic_context_registry_v1_0.csv",
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

migration_context <-
  readRDS(
    "outputs/r31_full_lineage_migration_context_v1_0/06_full_lineage_migration_context_by_ac.rds"
  )

corrected_employment <-
  readRDS(
    "outputs/r31_corrected_employment_control_v1_0/04_corrected_employment_by_ac.rds"
  )

ac_samples_canonical <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

ac_models_canonical <-
  readRDS(
    "outputs/ac_canonical_v1_0/models.rds"
  )

voter_samples_canonical <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
  )

voter_models_canonical <-
  readRDS(
    "outputs/voter_canonical_v1_0/models.rds"
  )

expected_registry_ids <-
  sprintf(
    "D%02d",
    1:8
  )

if (
  !identical(
    registry$moderator_id,
    expected_registry_ids
  )
) {
  print(
    registry,
    n = Inf,
    width = Inf
  )

  stop(
    "R31 moderator registry is not exactly D01-D08 in frozen order."
  )
}

canonical_context_vars <-
  c(
    "muslim_share_2001_dist_proxy",
    "d_muslim_share_2001_2011_pp",
    "target_bengali_bhojpuri_share_2001_dist_proxy",
    "d_target_bengali_bhojpuri_share_2001_2011_pp"
  )

migration_context_vars <-
  c(
    "migrant_share_2001_proxy",
    "d_migrant_share_2001_2011_pp",
    "male_migrant_share_2001_proxy",
    "d_male_migrant_share_2001_2011_pp"
  )

expected_variables <-
  c(
    canonical_context_vars,
    migration_context_vars
  )

if (
  !setequal(
    registry$variable,
    expected_variables
  )
) {
  stop(
    "Frozen R31 registry variables differ from the expected corrected eight-moderator set."
  )
}

missing_canonical <-
  setdiff(
    canonical_context_vars,
    names(
      ac_change
    )
  )

if (
  length(
    missing_canonical
  ) >
    0L
) {
  stop(
    "ac_change missing R31 variables: ",
    paste(
      missing_canonical,
      collapse = ", "
    )
  )
}

missing_migration <-
  setdiff(
    migration_context_vars,
    names(
      migration_context
    )
  )

if (
  length(
    missing_migration
  ) >
    0L
) {
  stop(
    "R31a7 migration artifact missing variables: ",
    paste(
      missing_migration,
      collapse = ", "
    )
  )
}

if (
  anyDuplicated(
    ac_change$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change is not unique by ac_uid."
  )
}

if (
  anyDuplicated(
    migration_context$ac_uid
  ) >
    0L
) {
  stop(
    "R31a7 migration artifact is not unique by ac_uid."
  )
}

if (
  anyDuplicated(
    corrected_employment$ac_uid
  ) >
    0L
) {
  stop(
    "R31a8 corrected employment artifact is not unique by ac_uid."
  )
}

moderator_payload <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      canonical_context_vars
    )
  ) |>
  left_join(
    migration_context |>
      select(
        ac_uid,
        all_of(
          migration_context_vars
        )
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

employment_payload <-
  corrected_employment |>
  select(
    ac_uid,
    employment_intensity_ec13_per_2011_population
  )

ac_base <-
  ac_samples_canonical[[
    "AC01"
  ]] |>
  select(
    -any_of(
      expected_variables
    ),
    -any_of(
      "employment_intensity_ec13_per_2011_population"
    )
  ) |>
  left_join(
    moderator_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  left_join(
    employment_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

voter_base <-
  voter_samples_canonical[[
    "V01"
  ]] |>
  select(
    -any_of(
      expected_variables
    ),
    -any_of(
      c(
        "employment_intensity_ec13_per_2011_population",
        "employment_intensity_ec13_pp"
      )
    )
  ) |>
  left_join(
    moderator_payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  left_join(
    employment_payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    employment_intensity_ec13_pp =
      100 *
      as.numeric(
        employment_intensity_ec13_per_2011_population
      )
  )

ac_required <-
  c(
    "ac_uid",
    "y",
    "fdi_current",
    "fdi_baseline",
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share",
    "ed_sec_share",
    "employment_intensity_ec13_per_2011_population",
    "state_no",
    "pc_cluster_id",
    expected_variables
  )

voter_required <-
  c(
    "respondent_uid",
    "ac_uid",
    "y",
    "fdi_total_current",
    "fdi_total_baseline",
    "ac_pop_100k",
    "sc_share_pp",
    "st_share_pp",
    "religion_x",
    "caste_x",
    "education_x",
    "state_fe",
    "ac_random",
    "ed_sec_share_pp",
    "employment_intensity_ec13_pp",
    expected_variables
  )

missing_ac <-
  setdiff(
    ac_required,
    names(
      ac_base
    )
  )

if (
  length(
    missing_ac
  ) >
    0L
) {
  stop(
    "AC01 base missing variables: ",
    paste(
      missing_ac,
      collapse = ", "
    )
  )
}

missing_voter <-
  setdiff(
    voter_required,
    names(
      voter_base
    )
  )

if (
  length(
    missing_voter
  ) >
    0L
) {
  stop(
    "V01 base missing variables: ",
    paste(
      missing_voter,
      collapse = ", "
    )
  )
}

if (
  nrow(
    ac_base
  ) !=
    224L
) {
  stop(
    "R31 AC base is not the 224-AC canonical AC01 sample."
  )
}

if (
  nrow(
    voter_base
  ) !=
    1763L
) {
  stop(
    "R31 voter base is not the 1763-respondent canonical V01 sample."
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

make_ac_formula <- function(
  controls
) {
  rhs <-
    paste(
      c(
        "moderator * fdi_current",
        "moderator * fdi_baseline",
        controls
      ),
      collapse =
        " + "
    )

  as.formula(
    paste0(
      "y ~ ",
      rhs,
      " | state_no"
    )
  )
}

make_voter_formula <- function(
  controls
) {
  rhs <-
    paste(
      c(
        "moderator * fdi_total_current",
        "moderator * fdi_total_baseline",
        controls,
        "(1 | ac_random)"
      ),
      collapse =
        " + "
    )

  as.formula(
    paste0(
      "y ~ ",
      rhs
    )
  )
}

find_term <- function(
  coefficient_names,
  candidates,
  label
) {
  term <-
    candidates[
      candidates %in%
        coefficient_names
    ]

  if (
    length(
      term
    ) !=
      1L
  ) {
    stop(
      label,
      ": could not uniquely identify focal interaction."
    )
  }

  term[[1]]
}

extract_ac_result <- function(
  model
) {
  b <-
    coef(
      model
    )

  vv <-
    vcov(
      model
    )

  term <-
    find_term(
      names(
        b
      ),
      c(
        "moderator:fdi_current",
        "fdi_current:moderator"
      ),
      "AC"
    )

  se <-
    sqrt(
      vv[
        term,
        term
      ]
    )

  ct <-
    coeftable(
      model
    )

  p_columns <-
    grep(
      "^Pr",
      colnames(
        ct
      ),
      value = TRUE
    )

  p_value <-
    if (
      length(
        p_columns
      ) >
        0L
    ) {
      as.numeric(
        ct[
          term,
          p_columns[[1]]
        ]
      )
    } else {
      2 *
        pnorm(
          abs(
            b[[term]] /
              se
          ),
          lower.tail =
            FALSE
        )
    }

  tibble(
    term =
      term,
    estimate =
      as.numeric(
        b[[term]]
      ),
    std_error =
      as.numeric(
        se
      ),
    p_value =
      p_value
  )
}

extract_voter_result <- function(
  model
) {
  b <-
    fixef(
      model
    )

  vv <-
    as.matrix(
      vcov(
        model
      )
    )

  term <-
    find_term(
      names(
        b
      ),
      c(
        "moderator:fdi_total_current",
        "fdi_total_current:moderator"
      ),
      "Voter"
    )

  se <-
    sqrt(
      vv[
        term,
        term
      ]
    )

  z <-
    b[[term]] /
      se

  tibble(
    term =
      term,
    estimate =
      as.numeric(
        b[[term]]
      ),
    std_error =
      as.numeric(
        se
      ),
    p_value =
      2 *
      pnorm(
        abs(
          z
        ),
        lower.tail =
          FALSE
      )
  )
}

max_gradient <- function(
  model
) {
  gradient <-
    model@optinfo$derivs$gradient

  if (
    is.null(
      gradient
    )
  ) {
    NA_real_
  } else {
    max(
      abs(
        gradient
      )
    )
  }
}

optimizer_code <- function(
  model
) {
  code <-
    model@optinfo$conv$opt

  if (
    is.null(
      code
    )
  ) {
    NA_integer_
  } else {
    as.integer(
      code
    )
  }
}

fit_warning <- function(
  model
) {
  messages <-
    model@optinfo$conv$lme4$messages

  if (
    is.null(
      messages
    )
  ) {
    NA_character_
  } else {
    paste(
      messages,
      collapse =
        " | "
    )
  }
}

ac_models <-
  list()

voter_models <-
  list()

ac_model_samples <-
  list()

voter_model_samples <-
  list()

result_rows <-
  list()

fit_rows <-
  list()

sample_rows <-
  list()

row_index <-
  0L

fit_index <-
  0L

sample_index <-
  0L

for (
  i in
    seq_len(
      nrow(
        registry
      )
    )
) {
  reg <-
    registry[
      i,
      ,
      drop = FALSE
    ]

  moderator_id <-
    reg$moderator_id[[1]]

  moderator_var <-
    reg$variable[[1]]

  for (
    control_set in
      c(
        "Primary",
        "Expanded"
      )
  ) {
    ac_controls <-
      if (
        control_set ==
          "Primary"
      ) {
        ac_primary_controls
      } else {
        ac_expanded_controls
      }

    voter_controls <-
      if (
        control_set ==
          "Primary"
      ) {
        voter_primary_controls
      } else {
        voter_expanded_controls
      }

    ac_sample <-
      ac_base |>
      mutate(
        moderator =
          as.numeric(
            .data[[
              moderator_var
            ]]
          )
      ) |>
      filter(
        is.finite(
          moderator
        ),
        if_all(
          all_of(
            ac_controls
          ),
          ~ !is.na(
            .x
          )
        )
      )

    voter_sample <-
      voter_base |>
      mutate(
        moderator =
          as.numeric(
            .data[[
              moderator_var
            ]]
          )
      ) |>
      filter(
        is.finite(
          moderator
        ),
        if_all(
          all_of(
            voter_controls
          ),
          ~ !is.na(
            .x
          )
        )
      )

    ac_spec_id <-
      paste(
        "AC",
        moderator_id,
        control_set,
        sep =
          "__"
      )

    voter_spec_id <-
      paste(
        "Voter",
        moderator_id,
        control_set,
        sep =
          "__"
      )

    if (
      n_distinct(
        ac_sample$pc_cluster_id
      ) <
        2L
    ) {
      stop(
        ac_spec_id,
        ": fewer than two PC clusters."
      )
    }

    if (
      n_distinct(
        voter_sample$ac_uid
      ) <
        2L
    ) {
      stop(
        voter_spec_id,
        ": fewer than two AC clusters."
      )
    }

    ac_model <-
      feols(
        make_ac_formula(
          ac_controls
        ),
        data =
          ac_sample,
        vcov =
          ~ pc_cluster_id,
        warn =
          TRUE,
        notes =
          TRUE
      )

    voter_model <-
      lmer(
        make_voter_formula(
          voter_controls
        ),
        data =
          voter_sample,
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
      )

    ac_result <-
      extract_ac_result(
        ac_model
      )

    voter_result <-
      extract_voter_result(
        voter_model
      )

    ac_mod_sd <-
      sd(
        ac_sample$moderator
      )

    ac_fdi_sd <-
      sd(
        ac_sample$fdi_current
      )

    voter_mod_sd <-
      sd(
        voter_sample$moderator
      )

    voter_fdi_sd <-
      sd(
        voter_sample$fdi_total_current
      )

    row_index <-
      row_index +
      1L

    result_rows[[
      row_index
    ]] <-
      tibble(
        spec_id =
          ac_spec_id,
        level =
          "AC",
        moderator_id =
          moderator_id,
        moderator_variable =
          moderator_var,
        concept =
          reg$concept[[1]],
        preferred_label =
          reg$preferred_label[[1]],
        temporal_type =
          reg$temporal_type[[1]],
        moderator_unit =
          reg$unit[[1]],
        moderator_role =
          reg$role[[1]],
        control_set =
          control_set,
        exposure =
          "Total local FDI per 100k",
        exposure_family =
          "60-month current + baseline",
        current_period =
          "April 2009-March 2014",
        baseline_period =
          "April 2004-March 2009",
        estimate =
          ac_result$estimate,
        std_error =
          ac_result$std_error,
        p_value =
          ac_result$p_value,
        moderator_sd =
          ac_mod_sd,
        current_fdi_sd =
          ac_fdi_sd,
        standardized_interaction_pp =
          100 *
          ac_result$estimate *
          ac_mod_sd *
          ac_fdi_sd,
        standardized_conf_low_pp =
          100 *
          (
            ac_result$estimate -
              1.96 *
              ac_result$std_error
          ) *
          ac_mod_sd *
          ac_fdi_sd,
        standardized_conf_high_pp =
          100 *
          (
            ac_result$estimate +
              1.96 *
              ac_result$std_error
          ) *
          ac_mod_sd *
          ac_fdi_sd,
        n =
          nrow(
            ac_sample
          ),
        n_ac =
          n_distinct(
            ac_sample$ac_uid
          ),
        n_states =
          n_distinct(
            ac_sample$state_no
          ),
        n_clusters =
          n_distinct(
            ac_sample$pc_cluster_id
          )
      )

    row_index <-
      row_index +
      1L

    result_rows[[
      row_index
    ]] <-
      tibble(
        spec_id =
          voter_spec_id,
        level =
          "Voter",
        moderator_id =
          moderator_id,
        moderator_variable =
          moderator_var,
        concept =
          reg$concept[[1]],
        preferred_label =
          reg$preferred_label[[1]],
        temporal_type =
          reg$temporal_type[[1]],
        moderator_unit =
          reg$unit[[1]],
        moderator_role =
          reg$role[[1]],
        control_set =
          control_set,
        exposure =
          "Total local FDI per 100k",
        exposure_family =
          "60-month current + baseline",
        current_period =
          "April 2009-March 2014",
        baseline_period =
          "April 2004-March 2009",
        estimate =
          voter_result$estimate,
        std_error =
          voter_result$std_error,
        p_value =
          voter_result$p_value,
        moderator_sd =
          voter_mod_sd,
        current_fdi_sd =
          voter_fdi_sd,
        standardized_interaction_pp =
          100 *
          voter_result$estimate *
          voter_mod_sd *
          voter_fdi_sd,
        standardized_conf_low_pp =
          100 *
          (
            voter_result$estimate -
              1.96 *
              voter_result$std_error
          ) *
          voter_mod_sd *
          voter_fdi_sd,
        standardized_conf_high_pp =
          100 *
          (
            voter_result$estimate +
              1.96 *
              voter_result$std_error
          ) *
          voter_mod_sd *
          voter_fdi_sd,
        n =
          nrow(
            voter_sample
          ),
        n_ac =
          n_distinct(
            voter_sample$ac_uid
          ),
        n_states =
          n_distinct(
            voter_sample$state_no
          ),
        n_clusters =
          n_distinct(
            voter_sample$ac_uid
          )
      )

    fit_index <-
      fit_index +
      1L

    fit_rows[[
      fit_index
    ]] <-
      tibble(
        spec_id =
          voter_spec_id,
        moderator_id =
          moderator_id,
        control_set =
          control_set,
        n =
          nrow(
            voter_sample
          ),
        n_ac =
          n_distinct(
            voter_sample$ac_uid
          ),
        n_states =
          n_distinct(
            voter_sample$state_no
          ),
        singular =
          isSingular(
            voter_model,
            tol =
              1e-4
          ),
        optimizer_code =
          optimizer_code(
            voter_model
          ),
        max_abs_gradient =
          max_gradient(
            voter_model
          ),
        warnings =
          fit_warning(
            voter_model
          )
      )

    sample_index <-
      sample_index +
      1L

    sample_rows[[
      sample_index
    ]] <-
      tibble(
        spec_id =
          ac_spec_id,
        level =
          "AC",
        moderator_id =
          moderator_id,
        control_set =
          control_set,
        n =
          nrow(
            ac_sample
          ),
        n_ac =
          n_distinct(
            ac_sample$ac_uid
          ),
        n_states =
          n_distinct(
            ac_sample$state_no
          ),
        n_clusters =
          n_distinct(
            ac_sample$pc_cluster_id
          )
      )

    sample_index <-
      sample_index +
      1L

    sample_rows[[
      sample_index
    ]] <-
      tibble(
        spec_id =
          voter_spec_id,
        level =
          "Voter",
        moderator_id =
          moderator_id,
        control_set =
          control_set,
        n =
          nrow(
            voter_sample
          ),
        n_ac =
          n_distinct(
            voter_sample$ac_uid
          ),
        n_states =
          n_distinct(
            voter_sample$state_no
          ),
        n_clusters =
          n_distinct(
            voter_sample$ac_uid
          )
      )

    ac_models[[
      ac_spec_id
    ]] <-
      ac_model

    voter_models[[
      voter_spec_id
    ]] <-
      voter_model

    ac_model_samples[[
      ac_spec_id
    ]] <-
      ac_sample

    voter_model_samples[[
      voter_spec_id
    ]] <-
      voter_sample
  }
}

results <-
  bind_rows(
    result_rows
  ) |>
  arrange(
    moderator_id,
    level,
    factor(
      control_set,
      levels =
        c(
          "Primary",
          "Expanded"
        )
    )
  )

fit_diagnostics <-
  bind_rows(
    fit_rows
  ) |>
  arrange(
    moderator_id,
    factor(
      control_set,
      levels =
        c(
          "Primary",
          "Expanded"
        )
    )
  )

sample_counts <-
  bind_rows(
    sample_rows
  ) |>
  arrange(
    moderator_id,
    level,
    factor(
      control_set,
      levels =
        c(
          "Primary",
          "Expanded"
        )
    )
  )

if (
  nrow(
    results
  ) !=
    32L
) {
  stop(
    "R31 did not produce exactly 32 model-result rows."
  )
}

if (
  length(
    ac_models
  ) !=
    16L ||
    length(
      voter_models
    ) !=
      16L
) {
  stop(
    "R31 did not produce exactly 16 AC and 16 voter models."
  )
}

availability <-
  bind_rows(
    lapply(
      seq_len(
        nrow(
          registry
        )
      ),
      function(
        i
      ) {
        reg <-
          registry[
            i,
            ,
            drop = FALSE
          ]

        var <-
          reg$variable[[1]]

        tibble(
          moderator_id =
            reg$moderator_id[[1]],
          moderator_variable =
            var,
          preferred_label =
            reg$preferred_label[[1]],
          ac01_n =
            nrow(
              ac_base
            ),
          ac01_complete =
            sum(
              is.finite(
                as.numeric(
                  ac_base[[
                    var
                  ]]
                )
              )
            ),
          v01_n =
            nrow(
              voter_base
            ),
          v01_complete =
            sum(
              is.finite(
                as.numeric(
                  voter_base[[
                    var
                  ]]
                )
              )
            )
        )
      }
    )
  )

canonical_ac01 <-
  ac_models_canonical[[
    "AC01"
  ]]

canonical_ac02 <-
  ac_models_canonical[[
    "AC02"
  ]]

canonical_v01 <-
  voter_models_canonical[[
    "V01"
  ]]

canonical_v02 <-
  voter_models_canonical[[
    "V02"
  ]]

canonical_ac_term <- function(
  model
) {
  find_term(
    names(
      coef(
        model
      )
    ),
    c(
      "muslim:fdi_current",
      "fdi_current:muslim"
    ),
    "Canonical AC"
  )
}

canonical_voter_term <- function(
  model
) {
  find_term(
    names(
      fixef(
        model
      )
    ),
    c(
      "muslim:fdi_total_current",
      "fdi_total_current:muslim"
    ),
    "Canonical voter"
  )
}

canonical_ac_extract <- function(
  model
) {
  term <-
    canonical_ac_term(
      model
    )

  vv <-
    vcov(
      model
    )

  c(
    estimate =
      coef(
        model
      )[[term]],
    se =
      sqrt(
        vv[
          term,
          term
        ]
      )
  )
}

canonical_voter_extract <- function(
  model
) {
  term <-
    canonical_voter_term(
      model
    )

  vv <-
    as.matrix(
      vcov(
        model
      )
    )

  c(
    estimate =
      fixef(
        model
      )[[term]],
    se =
      sqrt(
        vv[
          term,
          term
        ]
      )
  )
}

anchor_specs <-
  tribble(
    ~level,
    ~control_set,
    ~r31_spec_id,
    ~canonical_model_id,

    "AC",
    "Primary",
    "AC__D01__Primary",
    "AC01",

    "AC",
    "Expanded",
    "AC__D01__Expanded",
    "AC02",

    "Voter",
    "Primary",
    "Voter__D01__Primary",
    "V01",

    "Voter",
    "Expanded",
    "Voter__D01__Expanded",
    "V02"
  )

anchor_reproduction <-
  bind_rows(
    lapply(
      seq_len(
        nrow(
          anchor_specs
        )
      ),
      function(
        i
      ) {
        row <-
          anchor_specs[
            i,
            ,
            drop = FALSE
          ]

        level <-
          row$level[[1]]

        control_set <-
          row$control_set[[1]]

        r31_spec_id <-
          row$r31_spec_id[[1]]

        canonical_id <-
          row$canonical_model_id[[1]]

        if (
          level ==
            "AC"
        ) {
          new_model <-
            ac_models[[
              r31_spec_id
            ]]

          new_sample <-
            ac_model_samples[[
              r31_spec_id
            ]]

          old_model <-
            ac_models_canonical[[
              canonical_id
            ]]

          old_sample <-
            ac_samples_canonical[[
              canonical_id
            ]]

          new_result <-
            extract_ac_result(
              new_model
            )

          old_result <-
            canonical_ac_extract(
              old_model
            )

          sample_identical <-
            identical(
              sort(
                as.character(
                  new_sample$ac_uid
                )
              ),
              sort(
                as.character(
                  old_sample$ac_uid
                )
              )
            )
        } else {
          new_model <-
            voter_models[[
              r31_spec_id
            ]]

          new_sample <-
            voter_model_samples[[
              r31_spec_id
            ]]

          old_model <-
            voter_models_canonical[[
              canonical_id
            ]]

          old_sample <-
            voter_samples_canonical[[
              canonical_id
            ]]

          new_result <-
            extract_voter_result(
              new_model
            )

          old_result <-
            canonical_voter_extract(
              old_model
            )

          sample_identical <-
            identical(
              sort(
                as.character(
                  new_sample$respondent_uid
                )
              ),
              sort(
                as.character(
                  old_sample$respondent_uid
                )
              )
            )
        }

        tibble(
          level =
            level,
          control_set =
            control_set,
          r31_spec_id =
            r31_spec_id,
          canonical_model_id =
            canonical_id,
          canonical_estimate =
            as.numeric(
              old_result[[
                "estimate"
              ]]
            ),
          r31_estimate =
            new_result$estimate,
          estimate_absolute_difference =
            abs(
              new_result$estimate -
                as.numeric(
                  old_result[[
                    "estimate"
                  ]]
                )
            ),
          canonical_std_error =
            as.numeric(
              old_result[[
                "se"
              ]]
            ),
          r31_std_error =
            new_result$std_error,
          se_absolute_difference =
            abs(
              new_result$std_error -
                as.numeric(
                  old_result[[
                    "se"
                  ]]
                )
            ),
          old_n =
            nrow(
              old_sample
            ),
          new_n =
            nrow(
              new_sample
            ),
          sample_identical =
            sample_identical
        )
      }
    )
  )

if (
  any(
    anchor_reproduction$estimate_absolute_difference >
      1e-8
  ) ||
    any(
      anchor_reproduction$se_absolute_difference >
        1e-8
    ) ||
    any(
      !anchor_reproduction$sample_identical
    ) ||
    any(
      anchor_reproduction$old_n !=
        anchor_reproduction$new_n
    )
) {
  print(
    anchor_reproduction,
    n = Inf,
    width = Inf
  )

  stop(
    "D01 failed canonical AC01/AC02/V01/V02 anchor reproduction."
  )
}

if (
  any(
    fit_diagnostics$singular
  )
) {
  print(
    fit_diagnostics |>
      filter(
        singular
      ),
    n = Inf,
    width = Inf
  )

  stop(
    "At least one R31 voter model is singular."
  )
}

bad_optimizer <-
  fit_diagnostics |>
  filter(
    !is.na(
      optimizer_code
    ),
    optimizer_code !=
      0L
  )

if (
  nrow(
    bad_optimizer
  ) >
    0L
) {
  print(
    bad_optimizer,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one R31 voter model has nonzero optimizer code."
  )
}

write_csv(
  results,
  file.path(
    output_dir,
    "01_demographic_context_model_results.csv"
  )
)

write_csv(
  sample_counts,
  file.path(
    output_dir,
    "02_sample_counts.csv"
  )
)

write_csv(
  fit_diagnostics,
  file.path(
    output_dir,
    "03_voter_fit_diagnostics.csv"
  )
)

write_csv(
  anchor_reproduction,
  file.path(
    output_dir,
    "04_canonical_anchor_reproduction.csv"
  )
)

write_csv(
  availability,
  file.path(
    output_dir,
    "05_moderator_availability.csv"
  )
)

write_csv(
  results |>
    select(
      spec_id,
      level,
      moderator_id,
      preferred_label,
      control_set,
      standardized_interaction_pp,
      standardized_conf_low_pp,
      standardized_conf_high_pp,
      n,
      n_ac
    ),
  file.path(
    output_dir,
    "06_standardized_figure_data.csv"
  )
)

saveRDS(
  ac_models,
  file.path(
    output_dir,
    "07_ac_models.rds"
  )
)

saveRDS(
  voter_models,
  file.path(
    output_dir,
    "08_voter_models.rds"
  )
)

saveRDS(
  ac_model_samples,
  file.path(
    output_dir,
    "09_ac_model_samples.rds"
  )
)

saveRDS(
  voter_model_samples,
  file.path(
    output_dir,
    "10_voter_model_samples.rds"
  )
)

notes <-
  c(
    "R31b DEMOGRAPHIC-CONTEXT ROBUSTNESS",
    "",
    "This appendix branch varies the demographic-context moderator while holding the FDI treatment definition fixed.",
    "",
    "FDI exposure is raw Total local FDI per 100,000 population.",
    "Current exposure: April 2009 through March 2014.",
    "Baseline exposure: April 2004 through March 2009.",
    "",
    "Eight moderator concepts are frozen in config/r31_demographic_context_registry_v1_0.csv.",
    "",
    "Corrected migration variables come from R31a7.",
    "Corrected expanded-control employment comes from R31a8.",
    "",
    "AC models use 2014 survey-weighted centrist BJP share, state fixed effects, and PC-clustered standard errors.",
    "",
    "Voter models use 2014 centrist BJP vote as an unweighted LPM, state fixed effects, individual controls, AC controls, and an AC random intercept.",
    "",
    "Primary and Expanded control sets are estimated for every moderator.",
    "",
    "Samples are native complete-case subsets of the canonical AC01 and V01 samples for each moderator.",
    "",
    "The standardized interaction is the native current-FDI interaction multiplied by the model-sample SD of the moderator and the model-sample SD of current FDI, then multiplied by 100 to express the interaction component in BJP-support percentage points.",
    "",
    "This standardized quantity is a scale-comparison diagnostic, not a marginal effect.",
    "",
    "D01 must reproduce AC01, corrected AC02, V01, and corrected V02 before the script completes."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "11_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "12_session_info.txt"
  )
)

cat(
  "\n===== CANONICAL D01 ANCHOR REPRODUCTION =====\n"
)

print(
  anchor_reproduction,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MODERATOR AVAILABILITY =====\n"
)

print(
  availability,
  n = Inf,
  width = Inf
)

cat(
  "\n===== R31 MODEL RESULTS =====\n"
)

print(
  results |>
    select(
      level,
      moderator_id,
      preferred_label,
      control_set,
      estimate,
      std_error,
      p_value,
      standardized_interaction_pp,
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
  fit_diagnostics,
  n = Inf,
  width = Inf
)

cat(
  "\nR31B_DEMOGRAPHIC_CONTEXT_ROBUSTNESS_COMPLETE\n"
)

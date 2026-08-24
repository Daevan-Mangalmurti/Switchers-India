suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
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
    "main_regression_table_models_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ac_models_path <-
  file.path(
    "outputs",
    "ac_canonical_v1_0",
    "models.rds"
  )

ac_samples_path <-
  file.path(
    "outputs",
    "ac_canonical_v1_0",
    "model_samples.rds"
  )

voter_models_path <-
  file.path(
    "outputs",
    "voter_canonical_v1_0",
    "models.rds"
  )

voter_samples_path <-
  file.path(
    "outputs",
    "voter_canonical_v1_0",
    "model_samples.rds"
  )

sector_ac_path <-
  file.path(
    "outputs",
    "sector_form_native_common_v1_0",
    "05_centrist_ac_sector_form_native_common.csv"
  )

sector_voter_path <-
  file.path(
    "outputs",
    "sector_form_native_common_v1_0",
    "06_centrist_voter_sector_form_native_common.csv"
  )

required_files <-
  c(
    ac_models_path,
    ac_samples_path,
    voter_models_path,
    voter_samples_path,
    sector_ac_path,
    sector_voter_path
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

canonical_ac_models <-
  readRDS(
    ac_models_path
  )

canonical_ac_samples <-
  readRDS(
    ac_samples_path
  )

canonical_voter_models <-
  readRDS(
    voter_models_path
  )

canonical_voter_samples <-
  readRDS(
    voter_samples_path
  )

sector_ac <-
  read_csv(
    sector_ac_path,
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

sector_voter <-
  read_csv(
    sector_voter_path,
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

if (
  !"AC01" %in%
    names(
      canonical_ac_samples
    )
) {
  stop(
    "AC01 sample missing."
  )
}

if (
  !"V01" %in%
    names(
      canonical_voter_samples
    )
) {
  stop(
    "V01 sample missing."
  )
}

ac_base <-
  canonical_ac_samples[[
    "AC01"
  ]]

voter_base <-
  canonical_voter_samples[[
    "V01"
  ]]

if (
  nrow(
    ac_base
  ) !=
    224L
) {
  stop(
    "AC publication-table sample is not 224."
  )
}

if (
  nrow(
    voter_base
  ) !=
    1763L
) {
  stop(
    "Voter publication-table sample is not 1763."
  )
}

if (
  n_distinct(
    voter_base$ac_uid
  ) !=
    222L
) {
  stop(
    "Voter publication-table sample does not contain 222 ACs."
  )
}

required_ac_columns <-
  c(
    "ac_uid",
    "y",
    "muslim",
    "fdi_current",
    "fdi_baseline",
    "fdi_mfg_local_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2009",
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share",
    "state_no",
    "pc_cluster_id"
  )

missing_ac_columns <-
  setdiff(
    required_ac_columns,
    names(
      ac_base
    )
  )

if (
  length(
    missing_ac_columns
  ) >
    0L
) {
  stop(
    "AC01 sample missing: ",
    paste(
      missing_ac_columns,
      collapse = ", "
    )
  )
}

required_voter_columns <-
  c(
    "respondent_uid",
    "ac_uid",
    "y",
    "muslim",
    "fdi_total_current",
    "fdi_total_baseline",
    "fdi_mfg_current",
    "fdi_mfg_baseline",
    "ac_pop_100k",
    "sc_share_pp",
    "st_share_pp",
    "religion_x",
    "caste_x",
    "education_x",
    "state_fe",
    "ac_random"
  )

missing_voter_columns <-
  setdiff(
    required_voter_columns,
    names(
      voter_base
    )
  )

if (
  length(
    missing_voter_columns
  ) >
    0L
) {
  stop(
    "V01 sample missing: ",
    paste(
      missing_voter_columns,
      collapse = ", "
    )
  )
}

ac_work <-
  ac_base |>
  mutate(
    total_raw_current =
      as.numeric(
        fdi_current
      ),

    total_raw_baseline =
      as.numeric(
        fdi_baseline
      ),

    manufacturing_raw_current =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    manufacturing_raw_baseline =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    total_log_current =
      log1p(
        total_raw_current
      ),

    total_log_baseline =
      log1p(
        total_raw_baseline
      ),

    manufacturing_log_current =
      log1p(
        manufacturing_raw_current
      ),

    manufacturing_log_baseline =
      log1p(
        manufacturing_raw_baseline
      )
  )

voter_work <-
  voter_base |>
  mutate(
    total_raw_current =
      as.numeric(
        fdi_total_current
      ),

    total_raw_baseline =
      as.numeric(
        fdi_total_baseline
      ),

    manufacturing_raw_current =
      as.numeric(
        fdi_mfg_current
      ),

    manufacturing_raw_baseline =
      as.numeric(
        fdi_mfg_baseline
      ),

    total_log_current =
      log1p(
        total_raw_current
      ),

    total_log_baseline =
      log1p(
        total_raw_baseline
      ),

    manufacturing_log_current =
      log1p(
        manufacturing_raw_current
      ),

    manufacturing_log_baseline =
      log1p(
        manufacturing_raw_baseline
      )
  ) |>
  droplevels()

all_ac_exposures <-
  c(
    "total_raw_current",
    "total_raw_baseline",
    "manufacturing_raw_current",
    "manufacturing_raw_baseline",
    "total_log_current",
    "total_log_baseline",
    "manufacturing_log_current",
    "manufacturing_log_baseline"
  )

all_voter_exposures <-
  all_ac_exposures

if (
  any(
    !is.finite(
      as.matrix(
        ac_work[
          all_ac_exposures
        ]
      )
    )
  )
) {
  stop(
    "Non-finite AC exposure in fixed publication sample."
  )
}

if (
  any(
    !is.finite(
      as.matrix(
        voter_work[
          all_voter_exposures
        ]
      )
    )
  )
) {
  stop(
    "Non-finite voter exposure in fixed publication sample."
  )
}

ac_sample_identity <-
  tibble(
    canonical_model =
      c(
        "AC01",
        "AC03",
        "AC05"
      ),

    exact_same_ac_ids =
      c(
        identical(
          sort(
            as.character(
              ac_work$ac_uid
            )
          ),
          sort(
            as.character(
              canonical_ac_samples[[
                "AC01"
              ]]$ac_uid
            )
          )
        ),

        identical(
          sort(
            as.character(
              ac_work$ac_uid
            )
          ),
          sort(
            as.character(
              canonical_ac_samples[[
                "AC03"
              ]]$ac_uid
            )
          )
        ),

        identical(
          sort(
            as.character(
              ac_work$ac_uid
            )
          ),
          sort(
            as.character(
              canonical_ac_samples[[
                "AC05"
              ]]$ac_uid
            )
          )
        )
      )
  )

voter_sample_identity <-
  tibble(
    canonical_model =
      c(
        "V01",
        "V03",
        "V05"
      ),

    exact_same_respondent_ids =
      c(
        identical(
          sort(
            as.character(
              voter_work$respondent_uid
            )
          ),
          sort(
            as.character(
              canonical_voter_samples[[
                "V01"
              ]]$respondent_uid
            )
          )
        ),

        identical(
          sort(
            as.character(
              voter_work$respondent_uid
            )
          ),
          sort(
            as.character(
              canonical_voter_samples[[
                "V03"
              ]]$respondent_uid
            )
          )
        ),

        identical(
          sort(
            as.character(
              voter_work$respondent_uid
            )
          ),
          sort(
            as.character(
              canonical_voter_samples[[
                "V05"
              ]]$respondent_uid
            )
          )
        )
      )
  )

if (
  any(
    !ac_sample_identity$exact_same_ac_ids
  )
) {
  print(
    ac_sample_identity
  )

  stop(
    "Canonical AC samples differ across core table cells."
  )
}

if (
  any(
    !voter_sample_identity$exact_same_respondent_ids
  )
) {
  print(
    voter_sample_identity
  )

  stop(
    "Canonical voter samples differ across core table cells."
  )
}

exposure_registry <-
  tribble(
    ~exposure_id, ~sector, ~functional_form, ~current_col, ~baseline_col, ~main_or_appendix,

    "total_raw",
    "Total FDI",
    "Raw per 100k",
    "total_raw_current",
    "total_raw_baseline",
    "Main",

    "manufacturing_raw",
    "Manufacturing FDI",
    "Raw per 100k",
    "manufacturing_raw_current",
    "manufacturing_raw_baseline",
    "Main",

    "total_log1p",
    "Total FDI",
    "log1p(per 100k)",
    "total_log_current",
    "total_log_baseline",
    "Appendix",

    "manufacturing_log1p",
    "Manufacturing FDI",
    "log1p(per 100k)",
    "manufacturing_log_current",
    "manufacturing_log_baseline",
    "Appendix"
  )

column_registry <-
  tribble(
    ~column_stage, ~column_label, ~interacted, ~controls,

    1L,
    "Additive",
    FALSE,
    FALSE,

    2L,
    "Interaction",
    TRUE,
    FALSE,

    3L,
    "Interaction + controls",
    TRUE,
    TRUE
  )

extract_fixest_term <- function(
  fit,
  term
) {
  ct <-
    coeftable(
      fit
    )

  if (
    !term %in%
      rownames(
        ct
      )
  ) {
    return(
      tibble(
        term =
          term,
        estimate =
          NA_real_,
        std_error =
          NA_real_,
        p_value =
          NA_real_
      )
    )
  }

  tibble(
    term =
      term,

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

extract_lmer_term <- function(
  fit,
  term
) {
  ct <-
    coef(
      summary(
        fit
      )
    )

  if (
    !term %in%
      rownames(
        ct
      )
  ) {
    return(
      tibble(
        term =
          term,
        estimate =
          NA_real_,
        std_error =
          NA_real_,
        p_value =
          NA_real_
      )
    )
  }

  estimate <-
    as.numeric(
      ct[
        term,
        1
      ]
    )

  se <-
    as.numeric(
      ct[
        term,
        2
      ]
    )

  tibble(
    term =
      term,

    estimate =
      estimate,

    std_error =
      se,

    p_value =
      2 *
      pnorm(
        abs(
          estimate /
            se
        ),
        lower.tail =
          FALSE
      )
  )
}

fit_ac_table_model <- function(
  data,
  current_col,
  baseline_col,
  interacted,
  controls
) {
  dd <-
    data |>
    mutate(
      x_current =
        .data[[
          current_col
        ]],

      x_baseline =
        .data[[
          baseline_col
        ]]
    )

  if (
    interacted
  ) {
    focal <-
      "muslim * x_current + muslim * x_baseline"
  } else {
    focal <-
      "muslim + x_current + x_baseline"
  }

  control_terms <-
    if (
      controls
    ) {
      " + proxy_ac_pop + sc_pop_share + st_pop_share"
    } else {
      ""
    }

  formula <-
    as.formula(
      paste0(
        "y ~ ",
        focal,
        control_terms,
        " | state_no"
      )
    )

  fit <-
    feols(
      formula,
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
      formula
  )
}

fit_voter_table_model <- function(
  data,
  current_col,
  baseline_col,
  interacted,
  controls
) {
  dd <-
    data |>
    mutate(
      x_current =
        .data[[
          current_col
        ]],

      x_baseline =
        .data[[
          baseline_col
        ]]
    ) |>
    droplevels()

  if (
    interacted
  ) {
    focal <-
      "muslim * x_current + muslim * x_baseline"
  } else {
    focal <-
      "muslim + x_current + x_baseline"
  }

  control_terms <-
    if (
      controls
    ) {
      paste0(
        " + ac_pop_100k",
        " + sc_share_pp",
        " + st_share_pp",
        " + religion_x",
        " + caste_x",
        " + education_x"
      )
    } else {
      ""
    }

  formula <-
    as.formula(
      paste0(
        "y ~ ",
        focal,
        control_terms,
        " + state_fe",
        " + (1 | ac_random)"
      )
    )

  warnings <-
    character()

  messages <-
    character()

  fit <-
    withCallingHandlers(
      lmer(
        formula,
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
        optimizer_code[[1]]
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
      formula,
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
      if (
        length(
          warnings
        ) ==
          0L
      ) {
        NA_character_
      } else {
        paste(
          unique(
            warnings
          ),
          collapse =
            " | "
        )
      },
    messages =
      if (
        length(
          messages
        ) ==
          0L
      ) {
        NA_character_
      } else {
        paste(
          unique(
            messages
          ),
          collapse =
            " | "
        )
      }
  )
}

ac_models_out <-
  list()

voter_models_out <-
  list()

ac_metadata <-
  list()

voter_metadata <-
  list()

ac_coefficients <-
  list()

voter_coefficients <-
  list()

for (
  i in
    seq_len(
      nrow(
        exposure_registry
      )
    )
) {
  exposure <-
    exposure_registry[
      i,
      ,
      drop = FALSE
    ]

  for (
    j in
      seq_len(
        nrow(
          column_registry
        )
      )
  ) {
    column <-
      column_registry[
        j,
        ,
        drop = FALSE
      ]

    model_id <-
      paste(
        exposure$exposure_id,
        paste0(
          "C",
          column$column_stage
        ),
        sep =
          "__"
      )

    ac_result <-
      fit_ac_table_model(
        ac_work,
        exposure$current_col,
        exposure$baseline_col,
        column$interacted,
        column$controls
      )

    voter_result <-
      fit_voter_table_model(
        voter_work,
        exposure$current_col,
        exposure$baseline_col,
        column$interacted,
        column$controls
      )

    ac_models_out[[
      model_id
    ]] <-
      ac_result$fit

    voter_models_out[[
      model_id
    ]] <-
      voter_result$fit

    ac_metadata[[
      model_id
    ]] <-
      tibble(
        model_id =
          model_id,
        exposure_id =
          exposure$exposure_id,
        sector =
          exposure$sector,
        functional_form =
          exposure$functional_form,
        placement =
          exposure$main_or_appendix,
        column_stage =
          column$column_stage,
        column_label =
          column$column_label,
        interacted =
          column$interacted,
        controls =
          column$controls,
        formula =
          paste(
            deparse(
              ac_result$formula,
              width.cutoff =
                500
            ),
            collapse =
              ""
          ),
        n =
          nobs(
            ac_result$fit
          ),
        n_states =
          n_distinct(
            ac_result$data$state_no
          ),
        n_pc_clusters =
          n_distinct(
            ac_result$data$pc_cluster_id
          )
      )

    voter_metadata[[
      model_id
    ]] <-
      tibble(
        model_id =
          model_id,
        exposure_id =
          exposure$exposure_id,
        sector =
          exposure$sector,
        functional_form =
          exposure$functional_form,
        placement =
          exposure$main_or_appendix,
        column_stage =
          column$column_stage,
        column_label =
          column$column_label,
        interacted =
          column$interacted,
        controls =
          column$controls,
        formula =
          paste(
            deparse(
              voter_result$formula,
              width.cutoff =
                500
            ),
            collapse =
              ""
          ),
        n_voters =
          nobs(
            voter_result$fit
          ),
        n_ac =
          n_distinct(
            voter_result$data$ac_uid
          ),
        n_states =
          n_distinct(
            voter_result$data$state_no
          ),
        singular =
          voter_result$singular,
        optimizer_code =
          voter_result$optimizer_code,
        max_abs_gradient =
          voter_result$max_gradient,
        warnings =
          voter_result$warnings,
        messages =
          voter_result$messages
      )

    ac_terms <-
      c(
        "x_current",
        "muslim",
        "x_current:muslim",
        "muslim:x_current",
        "x_baseline",
        "x_baseline:muslim",
        "muslim:x_baseline",
        "proxy_ac_pop",
        "sc_pop_share",
        "st_pop_share"
      )

    ac_ct <-
      coeftable(
        ac_result$fit
      )

    ac_rows <-
      map_dfr(
        unique(
          ac_terms
        ),
        function(
          term
        ) {
          if (
            term %in%
              rownames(
                ac_ct
              )
          ) {
            extract_fixest_term(
              ac_result$fit,
              term
            )
          } else {
            tibble()
          }
        }
      )

    if (
      nrow(
        ac_rows
      ) >
        0L
    ) {
      ac_coefficients[[
        model_id
      ]] <-
        ac_rows |>
        mutate(
          model_id =
            model_id,
          .before =
            1
        )
    }

    voter_terms <-
      c(
        "x_current",
        "muslim",
        "x_current:muslim",
        "muslim:x_current",
        "x_baseline",
        "x_baseline:muslim",
        "muslim:x_baseline",
        "ac_pop_100k",
        "sc_share_pp",
        "st_share_pp"
      )

    voter_ct <-
      coef(
        summary(
          voter_result$fit
        )
      )

    voter_rows <-
      map_dfr(
        unique(
          voter_terms
        ),
        function(
          term
        ) {
          if (
            term %in%
              rownames(
                voter_ct
              )
          ) {
            extract_lmer_term(
              voter_result$fit,
              term
            )
          } else {
            tibble()
          }
        }
      )

    if (
      nrow(
        voter_rows
      ) >
        0L
    ) {
      voter_coefficients[[
        model_id
      ]] <-
        voter_rows |>
        mutate(
          model_id =
            model_id,
          .before =
            1
        )
    }
  }
}

ac_metadata <-
  bind_rows(
    ac_metadata
  )

voter_metadata <-
  bind_rows(
    voter_metadata
  )

ac_coefficients <-
  bind_rows(
    ac_coefficients
  )

voter_coefficients <-
  bind_rows(
    voter_coefficients
  )

find_interaction_estimate_fixest <- function(
  fit
) {
  terms <-
    names(
      coef(
        fit
      )
    )

  focal <-
    intersect(
      c(
        "muslim:x_current",
        "x_current:muslim"
      ),
      terms
    )

  if (
    length(
      focal
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify AC focal interaction."
    )
  }

  unname(
    coef(
      fit
    )[
      focal
    ]
  )
}

find_interaction_estimate_lmer <- function(
  fit
) {
  beta <-
    fixef(
      fit
    )

  focal <-
    intersect(
      c(
        "muslim:x_current",
        "x_current:muslim"
      ),
      names(
        beta
      )
    )

  if (
    length(
      focal
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify voter focal interaction."
    )
  }

  unname(
    beta[
      focal
    ]
  )
}

reproduction_checks <-
  list()

check_registry <-
  tribble(
    ~level, ~exposure_id, ~new_model_id, ~canonical_source, ~canonical_id,

    "AC",
    "total_raw",
    "total_raw__C3",
    "Canonical model",
    "AC01",

    "AC",
    "total_log1p",
    "total_log1p__C3",
    "Canonical model",
    "AC03",

    "AC",
    "manufacturing_raw",
    "manufacturing_raw__C3",
    "Canonical model",
    "AC05",

    "AC",
    "manufacturing_log1p",
    "manufacturing_log1p__C3",
    "Sector matrix",
    "manufacturing_log1p",

    "Voter",
    "total_raw",
    "total_raw__C3",
    "Canonical model",
    "V01",

    "Voter",
    "total_log1p",
    "total_log1p__C3",
    "Canonical model",
    "V03",

    "Voter",
    "manufacturing_raw",
    "manufacturing_raw__C3",
    "Canonical model",
    "V05",

    "Voter",
    "manufacturing_log1p",
    "manufacturing_log1p__C3",
    "Sector matrix",
    "manufacturing_log1p"
  )

for (
  i in
    seq_len(
      nrow(
        check_registry
      )
    )
) {
  check <-
    check_registry[
      i,
      ,
      drop = FALSE
    ]

  if (
    check$level ==
      "AC"
  ) {
    new_estimate <-
      find_interaction_estimate_fixest(
        ac_models_out[[
          check$new_model_id
        ]]
      )

    if (
      check$canonical_source ==
        "Canonical model"
    ) {
      old_fit <-
        canonical_ac_models[[
          check$canonical_id
        ]]

      old_terms <-
        names(
          coef(
            old_fit
          )
        )

      old_focal <-
        intersect(
          c(
            "muslim:fdi_current",
            "fdi_current:muslim"
          ),
          old_terms
        )

      if (
        length(
          old_focal
        ) !=
          1L
      ) {
        stop(
          "Cannot find canonical AC focal term."
        )
      }

      old_estimate <-
        unname(
          coef(
            old_fit
          )[
            old_focal
          ]
        )
    } else {
      old_row <-
        sector_ac |>
        filter(
          cell_id ==
            check$canonical_id
        )

      if (
        nrow(
          old_row
        ) !=
          1L
      ) {
        stop(
          "Cannot find sector-matrix AC result."
        )
      }

      old_estimate <-
        old_row$estimate
    }
  } else {
    new_estimate <-
      find_interaction_estimate_lmer(
        voter_models_out[[
          check$new_model_id
        ]]
      )

    if (
      check$canonical_source ==
        "Canonical model"
    ) {
      old_fit <-
        canonical_voter_models[[
          check$canonical_id
        ]]

      old_beta <-
        fixef(
          old_fit
        )

      focal_variable <-
        case_when(
          check$canonical_id ==
            "V01" ~
            "fdi_total_current",

          check$canonical_id ==
            "V03" ~
            "fdi_log_current",

          check$canonical_id ==
            "V05" ~
            "fdi_mfg_current",

          TRUE ~
            NA_character_
        )

      old_candidates <-
        c(
          paste0(
            "muslim:",
            focal_variable
          ),
          paste0(
            focal_variable,
            ":muslim"
          )
        )

      old_focal <-
        intersect(
          old_candidates,
          names(
            old_beta
          )
        )

      if (
        length(
          old_focal
        ) !=
          1L
      ) {
        stop(
          "Cannot find canonical voter focal term."
        )
      }

      old_estimate <-
        unname(
          old_beta[
            old_focal
          ]
        )
    } else {
      old_row <-
        sector_voter |>
        filter(
          cell_id ==
            check$canonical_id
        )

      if (
        nrow(
          old_row
        ) !=
          1L
      ) {
        stop(
          "Cannot find sector-matrix voter result."
        )
      }

      old_estimate <-
        old_row$estimate
    }
  }

  reproduction_checks[[
    i
  ]] <-
    tibble(
      level =
        check$level,
      exposure_id =
        check$exposure_id,
      new_model_id =
        check$new_model_id,
      comparison_source =
        check$canonical_source,
      comparison_id =
        check$canonical_id,
      stored_estimate =
        old_estimate,
      r28_estimate =
        new_estimate,
      absolute_difference =
        abs(
          old_estimate -
            new_estimate
        )
    )
}

reproduction_checks <-
  bind_rows(
    reproduction_checks
  )

if (
  any(
    reproduction_checks$level ==
      "AC" &
      reproduction_checks$absolute_difference >
        1e-8
  )
) {
  print(
    reproduction_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "R28 AC controlled-column reproduction failure."
  )
}

if (
  any(
    reproduction_checks$level ==
      "Voter" &
      reproduction_checks$absolute_difference >
        1e-5
  )
) {
  print(
    reproduction_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "R28 voter controlled-column reproduction failure."
  )
}

make_ac_table_summary <- function(
  placement_value
) {
  metadata <-
    ac_metadata |>
    filter(
      placement ==
        placement_value
    )

  focal <-
    ac_coefficients |>
    filter(
      term %in%
        c(
          "muslim:x_current",
          "x_current:muslim"
        )
    ) |>
    select(
      model_id,
      focal_estimate =
        estimate,
      focal_se =
        std_error,
      focal_p =
        p_value
    )

  metadata |>
    left_join(
      focal,
      by =
        "model_id"
    ) |>
    arrange(
      factor(
        sector,
        levels =
          c(
            "Total FDI",
            "Manufacturing FDI"
          )
      ),
      column_stage
    )
}

make_voter_table_summary <- function(
  placement_value
) {
  metadata <-
    voter_metadata |>
    filter(
      placement ==
        placement_value
    )

  focal <-
    voter_coefficients |>
    filter(
      term %in%
        c(
          "muslim:x_current",
          "x_current:muslim"
        )
    ) |>
    select(
      model_id,
      focal_estimate =
        estimate,
      focal_se =
        std_error,
      focal_p_normal_approx =
        p_value
    )

  metadata |>
    left_join(
      focal,
      by =
        "model_id"
    ) |>
    arrange(
      factor(
        sector,
        levels =
          c(
            "Total FDI",
            "Manufacturing FDI"
          )
      ),
      column_stage
    )
}

main_ac_summary <-
  make_ac_table_summary(
    "Main"
  )

main_voter_summary <-
  make_voter_table_summary(
    "Main"
  )

appendix_ac_log_summary <-
  make_ac_table_summary(
    "Appendix"
  )

appendix_voter_log_summary <-
  make_voter_table_summary(
    "Appendix"
  )

write_csv(
  exposure_registry,
  file.path(
    output_dir,
    "00_exposure_registry.csv"
  )
)

write_csv(
  column_registry,
  file.path(
    output_dir,
    "01_column_registry.csv"
  )
)

write_csv(
  ac_sample_identity,
  file.path(
    output_dir,
    "02_ac_sample_identity.csv"
  )
)

write_csv(
  voter_sample_identity,
  file.path(
    output_dir,
    "03_voter_sample_identity.csv"
  )
)

write_csv(
  ac_metadata,
  file.path(
    output_dir,
    "04_ac_model_metadata.csv"
  )
)

write_csv(
  voter_metadata,
  file.path(
    output_dir,
    "05_voter_model_metadata.csv"
  )
)

write_csv(
  ac_coefficients,
  file.path(
    output_dir,
    "06_ac_coefficients.csv"
  )
)

write_csv(
  voter_coefficients,
  file.path(
    output_dir,
    "07_voter_coefficients.csv"
  )
)

write_csv(
  reproduction_checks,
  file.path(
    output_dir,
    "08_controlled_column_reproduction_checks.csv"
  )
)

write_csv(
  main_ac_summary,
  file.path(
    output_dir,
    "09_main_ac_table_summary.csv"
  )
)

write_csv(
  main_voter_summary,
  file.path(
    output_dir,
    "10_main_voter_table_summary.csv"
  )
)

write_csv(
  appendix_ac_log_summary,
  file.path(
    output_dir,
    "11_appendix_ac_log_table_summary.csv"
  )
)

write_csv(
  appendix_voter_log_summary,
  file.path(
    output_dir,
    "12_appendix_voter_log_table_summary.csv"
  )
)

saveRDS(
  ac_models_out,
  file.path(
    output_dir,
    "13_ac_table_models.rds"
  )
)

saveRDS(
  voter_models_out,
  file.path(
    output_dir,
    "14_voter_table_models.rds"
  )
)

notes <-
  c(
    "R28 PUBLICATION-TABLE MODEL FAMILIES",
    "",
    "Every column uses the exact frozen canonical Center sample.",
    "AC models use N = 224 ACs throughout.",
    "Voter models use N = 1763 respondents in 222 ACs throughout.",
    "",
    "For each exposure family:",
    "Column 1: additive current FDI + baseline FDI + Muslim share; no controls; state fixed effects.",
    "Column 2: current FDI x Muslim share + baseline FDI x Muslim share; no controls; state fixed effects.",
    "Column 3: same interaction specification plus the prespecified primary controls.",
    "",
    "Voter models retain the AC random intercept in all columns.",
    "Voter Column 3 adds AC population, SC share, ST share, religion, caste, and education.",
    "",
    "Main-table exposure families:",
    "Total raw FDI per100k",
    "Manufacturing raw FDI per100k",
    "",
    "Parallel appendix families:",
    "Total log1p(FDI per100k)",
    "Manufacturing log1p(FDI per100k)",
    "",
    "Expanded employment and secondary-education AC controls are not included here.",
    "They remain robustness/specification-curve analyses.",
    "",
    "The controlled columns are required to reproduce the canonical/model-matrix estimates."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "15_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "16_session_info.txt"
  )
)

cat(
  "\n===== CONTROLLED-COLUMN REPRODUCTION =====\n"
)

print(
  reproduction_checks,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MAIN AC TABLE MODEL FAMILY =====\n"
)

print(
  main_ac_summary |>
    select(
      sector,
      functional_form,
      column_stage,
      column_label,
      focal_estimate,
      focal_se,
      focal_p,
      n,
      n_states,
      n_pc_clusters
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== MAIN VOTER TABLE MODEL FAMILY =====\n"
)

print(
  main_voter_summary |>
    select(
      sector,
      functional_form,
      column_stage,
      column_label,
      focal_estimate,
      focal_se,
      focal_p_normal_approx,
      n_voters,
      n_ac,
      n_states,
      singular,
      optimizer_code
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== APPENDIX AC LOG TABLE MODEL FAMILY =====\n"
)

print(
  appendix_ac_log_summary |>
    select(
      sector,
      functional_form,
      column_stage,
      column_label,
      focal_estimate,
      focal_se,
      focal_p,
      n,
      n_states,
      n_pc_clusters
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== APPENDIX VOTER LOG TABLE MODEL FAMILY =====\n"
)

print(
  appendix_voter_log_summary |>
    select(
      sector,
      functional_form,
      column_stage,
      column_label,
      focal_estimate,
      focal_se,
      focal_p_normal_approx,
      n_voters,
      n_ac,
      n_states,
      singular,
      optimizer_code
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== VOTER FIT DIAGNOSTICS =====\n"
)

print(
  voter_metadata |>
    select(
      model_id,
      sector,
      functional_form,
      column_stage,
      singular,
      optimizer_code,
      max_abs_gradient,
      warnings,
      messages
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nR28_MAIN_REGRESSION_TABLE_MODELS_COMPLETE\n"
)

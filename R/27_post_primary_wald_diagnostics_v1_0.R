suppressPackageStartupMessages({
  library(dplyr)
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

restricted_model_path <-
  file.path(
    "outputs",
    "sector_form_native_common_v1_0",
    "17_official_vote_triple_models.rds"
  )

restricted_summary_path <-
  file.path(
    "outputs",
    "sector_form_native_common_v1_0",
    "07_official_vote_triple_sector_form_native_common.csv"
  )

saturated_model_path <-
  file.path(
    "outputs",
    "official_vote_temporal_saturation_v1_0",
    "04_saturated_current_baseline_models.rds"
  )

saturated_summary_path <-
  file.path(
    "outputs",
    "official_vote_temporal_saturation_v1_0",
    "01_temporal_saturation_summary.csv"
  )

baseline_wald_path <-
  file.path(
    "outputs",
    "official_vote_temporal_saturation_v1_0",
    "02_joint_baseline_restriction_wald_tests.csv"
  )

required_files <-
  c(
    restricted_model_path,
    restricted_summary_path,
    saturated_model_path,
    saturated_summary_path,
    baseline_wald_path
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
    "Missing required diagnostic inputs: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

output_dir <-
  file.path(
    "outputs",
    "post_primary_wald_diagnostics_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

restricted_models <-
  readRDS(
    restricted_model_path
  )

restricted_summary <-
  read_csv(
    restricted_summary_path,
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

saturated_models <-
  readRDS(
    saturated_model_path
  )

saturated_summary <-
  read_csv(
    saturated_summary_path,
    show_col_types = FALSE
  )

baseline_wald <-
  read_csv(
    baseline_wald_path,
    show_col_types = FALSE
  )

cell_registry <-
  tribble(
    ~cell_id, ~sector, ~functional_form, ~restricted_key,

    "total_raw",
    "Total",
    "Raw",
    "total_raw__native",

    "total_log1p",
    "Total",
    "log1p",
    "total_log1p__native",

    "manufacturing_raw",
    "Manufacturing",
    "Raw",
    "manufacturing_raw__native",

    "manufacturing_log1p",
    "Manufacturing",
    "log1p",
    "manufacturing_log1p__native",

    "services_raw",
    "Services",
    "Raw",
    "services_raw__native",

    "services_log1p",
    "Services",
    "log1p",
    "services_log1p__native"
  )

find_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <-
    coefficient_names[
      vapply(
        coefficient_names,
        function(
          term
        ) {
          pieces <-
            strsplit(
              term,
              ":",
              fixed = TRUE
            )[[1]]

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
        logical(1)
      )
    ]

  if (
    length(
      hits
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely locate interaction: ",
      paste(
        variables,
        collapse = " x "
      ),
      ". Matches: ",
      paste(
        hits,
        collapse = ", "
      )
    )
  }

  hits[[1]]
}

joint_wald <- function(
  fit,
  variable_sets
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

  terms <-
    vapply(
      variable_sets,
      function(
        variables
      ) {
        find_interaction_term(
          names(
            beta
          ),
          variables
        )
      },
      character(1)
    )

  b <-
    beta[
      terms
    ]

  VV <-
    V[
      terms,
      terms,
      drop = FALSE
    ]

  if (
    qr(
      VV
    )$rank !=
      nrow(
        VV
      )
  ) {
    stop(
      "Wald covariance submatrix is singular for terms: ",
      paste(
        terms,
        collapse = ", "
      )
    )
  }

  statistic <-
    as.numeric(
      t(
        b
      ) %*%
        solve(
          VV,
          b
        )
    )

  df <-
    length(
      terms
    )

  tibble(
    terms =
      paste(
        terms,
        collapse = " ; "
      ),

    chisq =
      statistic,

    df =
      df,

    p_value =
      pchisq(
        statistic,
        df =
          df,
        lower.tail =
          FALSE
      )
  )
}

extract_triple <- function(
  fit
) {
  term <-
    find_interaction_term(
      names(
        coef(
          fit
        )
      ),
      c(
        "x_current",
        "muslim",
        "centrist_share_2009"
      )
    )

  ct <-
    coeftable(
      fit
    )

  tibble(
    triple_term =
      term,

    triple_estimate =
      unname(
        ct[
          term,
          1
        ]
      ),

    triple_std_error =
      unname(
        ct[
          term,
          2
        ]
      ),

    triple_p_value =
      unname(
        ct[
          term,
          4
        ]
      )
  )
}

results <-
  vector(
    "list",
    nrow(
      cell_registry
    )
  )

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

  if (
    !spec$restricted_key %in%
      names(
        restricted_models
      )
  ) {
    stop(
      "Missing restricted model: ",
      spec$restricted_key
    )
  }

  if (
    !spec$cell_id %in%
      names(
        saturated_models
      )
  ) {
    stop(
      "Missing saturated model: ",
      spec$cell_id
    )
  }

  restricted_fit <-
    restricted_models[[
      spec$restricted_key
    ]]

  saturated_fit <-
    saturated_models[[
      spec$cell_id
    ]]

  restricted_current_context <-
    joint_wald(
      restricted_fit,
      list(
        c(
          "x_current",
          "centrist_share_2009"
        ),
        c(
          "x_current",
          "muslim",
          "centrist_share_2009"
        )
      )
    )

  saturated_current_context <-
    joint_wald(
      saturated_fit,
      list(
        c(
          "x_current",
          "centrist_share_2009"
        ),
        c(
          "x_current",
          "muslim",
          "centrist_share_2009"
        )
      )
    )

  restricted_triple <-
    extract_triple(
      restricted_fit
    )

  saturated_triple <-
    extract_triple(
      saturated_fit
    )

  stored_restricted <-
    restricted_summary |>
    filter(
      cell_id ==
        spec$cell_id
    )

  if (
    nrow(
      stored_restricted
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely locate stored restricted summary for ",
      spec$cell_id
    )
  }

  stored_saturated <-
    saturated_summary |>
    filter(
      cell_id ==
        spec$cell_id
    )

  if (
    nrow(
      stored_saturated
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely locate stored saturated summary for ",
      spec$cell_id
    )
  }

  if (
    abs(
      restricted_triple$triple_estimate -
        stored_restricted$triple_interaction
    ) >
      1e-10
  ) {
    stop(
      "Restricted triple coefficient does not reproduce stored result for ",
      spec$cell_id
    )
  }

  if (
    abs(
      saturated_triple$triple_estimate -
        stored_saturated$saturated_current_triple
    ) >
      1e-10
  ) {
    stop(
      "Saturated triple coefficient does not reproduce stored result for ",
      spec$cell_id
    )
  }

  baseline_test <-
    baseline_wald |>
    filter(
      cell_id ==
        spec$cell_id
    )

  if (
    nrow(
      baseline_test
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely locate baseline-restriction Wald test for ",
      spec$cell_id
    )
  }

  results[[
    i
  ]] <-
    tibble(
      cell_id =
        spec$cell_id,

      sector =
        spec$sector,

      functional_form =
        spec$functional_form,

      restricted_triple_estimate =
        restricted_triple$triple_estimate,

      restricted_triple_se =
        restricted_triple$triple_std_error,

      restricted_triple_p =
        restricted_triple$triple_p_value,

      restricted_current_context_wald_chisq =
        restricted_current_context$chisq,

      restricted_current_context_wald_df =
        restricted_current_context$df,

      restricted_current_context_wald_p =
        restricted_current_context$p_value,

      saturated_triple_estimate =
        saturated_triple$triple_estimate,

      saturated_triple_se =
        saturated_triple$triple_std_error,

      saturated_triple_p =
        saturated_triple$triple_p_value,

      saturated_current_context_wald_chisq =
        saturated_current_context$chisq,

      saturated_current_context_wald_df =
        saturated_current_context$df,

      saturated_current_context_wald_p =
        saturated_current_context$p_value,

      baseline_restriction_wald_chisq =
        baseline_test$wald_chisq,

      baseline_restriction_wald_df =
        baseline_test$df,

      baseline_restriction_wald_p =
        baseline_test$p_value,

      n =
        nobs(
          restricted_fit
        )
    )
}

results <-
  bind_rows(
    results
  )

if (
  any(
    results$n !=
      518L
  )
) {
  stop(
    "At least one official-vote Wald model is not using the expected 518 AC sample."
  )
}

appendix_wald_table <-
  results |>
  select(
    sector,
    functional_form,

    restricted_current_context_wald_chisq,
    restricted_current_context_wald_df,
    restricted_current_context_wald_p,

    saturated_current_context_wald_chisq,
    saturated_current_context_wald_df,
    saturated_current_context_wald_p,

    baseline_restriction_wald_chisq,
    baseline_restriction_wald_df,
    baseline_restriction_wald_p,

    n
  )

write_csv(
  results,
  file.path(
    output_dir,
    "01_full_wald_diagnostics.csv"
  )
)

write_csv(
  appendix_wald_table,
  file.path(
    output_dir,
    "02_appendix_wald_table.csv"
  )
)

diagnostic_registry <-
  tribble(
    ~diagnostic_id, ~status, ~purpose, ~interpretation,

    "D01_sector_form_matrix",
    "Post-primary diagnostic",
    "Compare Total, Manufacturing, and Services FDI under raw and log1p functional forms",
    "Evaluates sectoral and functional-form sensitivity; does not redefine the frozen primary treatment",

    "D02_native_common_sample",
    "Post-primary diagnostic",
    "Determine whether sector/form comparisons are confounded by changing samples",
    "All six cells were found to use identical samples within each outcome family",

    "D03_total_temporal_parameterization",
    "Post-primary diagnostic",
    "Assess high current-baseline FDI correlation and exact change reparameterization",
    "Separates persistent exposure geography from relative growth/change interpretation",

    "D04_official_vote_temporal_saturation",
    "Post-primary diagnostic",
    "Allow full baseline FDI x Muslim share x centrist-context hierarchy",
    "Tests sensitivity of the parsimonious official-vote triple interaction to temporal saturation",

    "D05_current_context_wald",
    "Post-primary diagnostic",
    "Jointly test current FDI x centrist share and current FDI x Muslim share x centrist share",
    "Substantive test of whether centrist context jointly modifies the current-FDI relationship",

    "D06_baseline_restriction_wald",
    "Post-primary diagnostic",
    "Jointly test omitted baseline FDI x centrist share and baseline FDI x Muslim share x centrist share terms",
    "Specification test of restrictions imposed by the parsimonious official-vote model"
  )

write_csv(
  diagnostic_registry,
  file.path(
    output_dir,
    "03_post_primary_diagnostic_registry.csv"
  )
)

notes <-
  c(
    "POST-PRIMARY WALD DIAGNOSTICS",
    "",
    "These analyses were added after inspection of the canonical primary estimates and temporal-correlation diagnostics.",
    "They must not be described as preregistered or pre-specified primary models.",
    "",
    "Restricted current-context Wald test:",
    "H0: current FDI x centrist share = 0 AND current FDI x Muslim share x centrist share = 0.",
    "This is the substantive two-degree-of-freedom test of whether centrist context jointly modifies the current-FDI relationship in the parsimonious official-vote specification.",
    "",
    "Saturated current-context Wald test:",
    "The same two restrictions, evaluated after allowing baseline FDI to enter the full Muslim-share x centrist-context interaction hierarchy.",
    "",
    "Baseline-restriction Wald test:",
    "H0: baseline FDI x centrist share = 0 AND baseline FDI x Muslim share x centrist share = 0.",
    "This tests the restrictions imposed by the parsimonious temporal specification.",
    "Failure to reject this null is not evidence that the restrictions are literally true.",
    "",
    "All six sector x functional-form official-vote models use N = 518 ACs."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "04_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "05_session_info.txt"
  )
)

cat(
  "\n===== APPENDIX-READY WALD TABLE =====\n"
)

print(
  appendix_wald_table,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FULL WALD DIAGNOSTICS =====\n"
)

print(
  results,
  n = Inf,
  width = Inf
)

cat(
  "\nPOST_PRIMARY_WALD_DIAGNOSTICS_COMPLETE\n"
)

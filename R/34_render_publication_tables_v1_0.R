suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
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
    "outputs",
    "paper_tables_v1_0"
  )

main_dir <-
  file.path(
    output_dir,
    "main"
  )

appendix_dir <-
  file.path(
    output_dir,
    "appendix"
  )

source_dir <-
  file.path(
    output_dir,
    "source"
  )

dir.create(
  main_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  appendix_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  source_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

required_file <- function(
  path
) {
  if (
    !file.exists(
      path
    )
  ) {
    stop(
      "Required table source is missing: ",
      path
    )
  }

  path
}

stars <- function(
  p
) {
  if (
    is.na(
      p
    )
  ) {
    return(
      ""
    )
  }

  if (
    p <
      0.01
  ) {
    return(
      "***"
    )
  }

  if (
    p <
      0.05
  ) {
    return(
      "**"
    )
  }

  if (
    p <
      0.10
  ) {
    return(
      "*"
    )
  }

  ""
}

fmt_num <- function(
  x,
  digits = 3
) {
  out <-
    rep(
      "",
      length(
        x
      )
    )

  keep <-
    !is.na(
      x
    )

  out[
    keep
  ] <-
    formatC(
      x[
        keep
      ],
      format =
        "f",
      digits =
        digits
    )

  out
}

fmt_p <- function(
  p
) {
  if (
    is.na(
      p
    )
  ) {
    return(
      ""
    )
  }

  if (
    p <
      0.001
  ) {
    return(
      "<0.001"
    )
  }

  formatC(
    p,
    format =
      "f",
    digits =
      3
  )
}

fmt_coef <- function(
  estimate,
  std_error,
  p_value
) {
  if (
    is.na(
      estimate
    )
  ) {
    return(
      "\u2014"
    )
  }

  paste0(
    fmt_num(
      estimate
    ),
    stars(
      p_value
    ),
    " (",
    fmt_num(
      std_error
    ),
    ")"
  )
}

fmt_est_se <- function(
  estimate,
  std_error
) {
  paste0(
    fmt_num(
      estimate
    ),
    " (",
    fmt_num(
      std_error
    ),
    ")"
  )
}

fmt_wald <- function(
  statistic,
  df,
  p
) {
  paste0(
    fmt_num(
      statistic,
      2
    ),
    " [df=",
    as.integer(
      df
    ),
    "], p=",
    fmt_p(
      p
    )
  )
}

html_escape <- function(
  x
) {
  x <-
    gsub(
      "&",
      "&amp;",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "<",
      "&lt;",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      ">",
      "&gt;",
      x,
      fixed =
        TRUE
    )

  x
}

latex_escape <- function(
  x
) {
  x <-
    gsub(
      "\u00d7",
      " x ",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "\u2013",
      "-",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "\u2014",
      "--",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "&",
      "\\\\&",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "%",
      "\\\\%",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "_",
      "\\\\_",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "#",
      "\\\\#",
      x,
      fixed =
        TRUE
    )

  x
}

write_simple_html <- function(
  data,
  title,
  note,
  path
) {
  headers <-
    paste0(
      "<th>",
      html_escape(
        names(
          data
        )
      ),
      "</th>",
      collapse =
        ""
    )

  rows <-
    apply(
      data,
      1,
      function(row) {
        paste0(
          "<tr>",
          paste0(
            "<td>",
            html_escape(
              as.character(
                row
              )
            ),
            "</td>",
            collapse =
              ""
          ),
          "</tr>"
        )
      }
    )

  html <-
    c(
      "<!doctype html>",
      "<html><head><meta charset=\"utf-8\">",
      "<style>",
      "body{font-family:Arial,Helvetica,sans-serif;margin:32px;color:#111;}",
      "table{border-collapse:collapse;width:100%;font-size:13px;}",
      "th{border-bottom:2px solid #222;padding:6px;text-align:center;}",
      "td{border-bottom:1px solid #ddd;padding:6px;text-align:right;}",
      "td:first-child{text-align:left;}",
      "h2{margin-bottom:10px;}",
      ".note{font-size:11px;line-height:1.4;margin-top:12px;}",
      "</style></head><body>",
      paste0(
        "<h2>",
        html_escape(
          title
        ),
        "</h2>"
      ),
      "<table>",
      paste0(
        "<thead><tr>",
        headers,
        "</tr></thead>"
      ),
      "<tbody>",
      rows,
      "</tbody></table>",
      paste0(
        "<div class=\"note\"><strong>Notes:</strong> ",
        html_escape(
          note
        ),
        "</div>"
      ),
      "</body></html>"
    )

  writeLines(
    html,
    path
  )
}

write_simple_tex <- function(
  data,
  title,
  note,
  path
) {
  ncols <-
    ncol(
      data
    )

  alignment <-
    paste0(
      "l",
      paste(
        rep(
          "r",
          ncols -
            1L
        ),
        collapse =
          ""
      )
    )

  header <-
    paste(
      latex_escape(
        names(
          data
        )
      ),
      collapse =
        " & "
    )

  rows <-
    apply(
      data,
      1,
      function(row) {
        paste0(
          paste(
            latex_escape(
              as.character(
                row
              )
            ),
            collapse =
              " & "
          ),
          " \\\\"
        )
      }
    )

  tex <-
    c(
      "\\begin{table}[!htbp]",
      "\\centering",
      paste0(
        "\\caption{",
        latex_escape(
          title
        ),
        "}"
      ),
      "\\small",
      paste0(
        "\\begin{tabular}{",
        alignment,
        "}"
      ),
      "\\toprule",
      paste0(
        header,
        " \\\\"
      ),
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.96\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} ",
        latex_escape(
          note
        )
      ),
      "\\end{minipage}",
      "\\end{table}"
    )

  writeLines(
    tex,
    path
  )
}

write_grouped_html <- function(
  data,
  title,
  group_labels,
  column_labels,
  note,
  path
) {
  group_header <-
    paste0(
      "<th rowspan=\"2\">Variable</th>",
      paste0(
        "<th colspan=\"3\">",
        html_escape(
          group_labels
        ),
        "</th>",
        collapse =
          ""
      )
    )

  second_header <-
    paste0(
      "<th>",
      html_escape(
        column_labels
      ),
      "</th>",
      collapse =
        ""
    )

  rows <-
    apply(
      data,
      1,
      function(row) {
        paste0(
          "<tr>",
          paste0(
            "<td>",
            html_escape(
              as.character(
                row
              )
            ),
            "</td>",
            collapse =
              ""
          ),
          "</tr>"
        )
      }
    )

  html <-
    c(
      "<!doctype html>",
      "<html><head><meta charset=\"utf-8\">",
      "<style>",
      "body{font-family:Arial,Helvetica,sans-serif;margin:32px;color:#111;}",
      "table{border-collapse:collapse;width:100%;font-size:13px;}",
      "th{border-bottom:2px solid #222;padding:6px;text-align:center;}",
      "td{border-bottom:1px solid #ddd;padding:6px;text-align:right;}",
      "td:first-child{text-align:left;}",
      ".note{font-size:11px;line-height:1.4;margin-top:12px;}",
      "</style></head><body>",
      paste0(
        "<h2>",
        html_escape(
          title
        ),
        "</h2>"
      ),
      "<table>",
      "<thead>",
      paste0(
        "<tr>",
        group_header,
        "</tr>"
      ),
      paste0(
        "<tr>",
        second_header,
        "</tr>"
      ),
      "</thead>",
      "<tbody>",
      rows,
      "</tbody></table>",
      paste0(
        "<div class=\"note\"><strong>Notes:</strong> ",
        html_escape(
          note
        ),
        "</div>"
      ),
      "</body></html>"
    )

  writeLines(
    html,
    path
  )
}

write_grouped_tex <- function(
  data,
  title,
  group_labels,
  column_labels,
  note,
  path
) {
  rows <-
    apply(
      data,
      1,
      function(row) {
        paste0(
          paste(
            latex_escape(
              as.character(
                row
              )
            ),
            collapse =
              " & "
          ),
          " \\\\"
        )
      }
    )

  tex <-
    c(
      "\\begin{table}[!htbp]",
      "\\centering",
      paste0(
        "\\caption{",
        latex_escape(
          title
        ),
        "}"
      ),
      "\\small",
      "\\begin{tabular}{lrrrrrr}",
      "\\toprule",
      paste0(
        " & \\multicolumn{3}{c}{",
        latex_escape(
          group_labels[[1]]
        ),
        "} & \\multicolumn{3}{c}{",
        latex_escape(
          group_labels[[2]]
        ),
        "} \\\\"
      ),
      "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
      paste0(
        "Variable & ",
        paste(
          latex_escape(
            column_labels
          ),
          collapse =
            " & "
        ),
        " \\\\"
      ),
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabular}",
      "\\begin{minipage}{0.96\\textwidth}",
      "\\footnotesize",
      paste0(
        "\\textit{Notes:} ",
        latex_escape(
          note
        )
      ),
      "\\end{minipage}",
      "\\end{table}"
    )

  writeLines(
    tex,
    path
  )
}

ac_coeff_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/06_ac_coefficients.csv"
  )

voter_coeff_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/07_voter_coefficients.csv"
  )

main_ac_summary_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/09_main_ac_table_summary.csv"
  )

main_voter_summary_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/10_main_voter_table_summary.csv"
  )

log_ac_summary_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/11_appendix_ac_log_table_summary.csv"
  )

log_voter_summary_path <-
  required_file(
    "outputs/main_regression_table_models_v1_0/12_appendix_voter_log_table_summary.csv"
  )

ac_coeff <-
  read_csv(
    ac_coeff_path,
    show_col_types =
      FALSE
  )

voter_coeff <-
  read_csv(
    voter_coeff_path,
    show_col_types =
      FALSE
  )

main_ac_summary <-
  read_csv(
    main_ac_summary_path,
    show_col_types =
      FALSE
  )

main_voter_summary <-
  read_csv(
    main_voter_summary_path,
    show_col_types =
      FALSE
  )

log_ac_summary <-
  read_csv(
    log_ac_summary_path,
    show_col_types =
      FALSE
  )

log_voter_summary <-
  read_csv(
    log_voter_summary_path,
    show_col_types =
      FALSE
  )

term_map <-
  list(
    "Muslim population share, 2001" =
      c(
        "muslim"
      ),

    "Current FDI" =
      c(
        "x_current"
      ),

    "Muslim share x Current FDI" =
      c(
        "muslim:x_current",
        "x_current:muslim"
      ),

    "Baseline FDI" =
      c(
        "x_baseline"
      ),

    "Muslim share x Baseline FDI" =
      c(
        "muslim:x_baseline",
        "x_baseline:muslim"
      )
  )

coef_cell <- function(
  coefficients,
  model_id,
  candidate_terms
) {
  z <-
    coefficients |>
    filter(
      .data$model_id ==
        .env$model_id,
      .data$term %in%
        .env$candidate_terms
    )

  if (
    nrow(
      z
    ) ==
      0L
  ) {
    return(
      "\u2014"
    )
  }

  if (
    nrow(
      z
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely resolve term for model ",
      model_id,
      ": ",
      paste(
        candidate_terms,
        collapse =
          " / "
      )
    )
  }

  fmt_coef(
    z$estimate[[1]],
    z$std_error[[1]],
    z$p_value[[1]]
  )
}

build_regression_display <- function(
  summary,
  coefficients,
  voter = FALSE
) {
  ordered <-
    summary |>
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

  if (
    nrow(
      ordered
    ) !=
      6L
  ) {
    stop(
      "Regression display does not contain six columns."
    )
  }

  out <-
    tibble(
      Variable =
        names(
          term_map
        )
    )

  for (
    j in
      seq_len(
        nrow(
          ordered
        )
      )
  ) {
    model_id <-
      ordered$model_id[[j]]

    out[[
      paste0(
        "C",
        j
      )
    ]] <-
      vapply(
        term_map,
        function(candidate_terms) {
          coef_cell(
            coefficients,
            model_id,
            candidate_terms
          )
        },
        character(1)
      )
  }

  stats <-
    tibble(
      Variable =
        if (
          voter
        ) {
          c(
            "Primary controls",
            "State fixed effects",
            "AC random intercept",
            "Voters",
            "Assembly constituencies",
            "States"
          )
        } else {
          c(
            "Primary controls",
            "State fixed effects",
            "PC-clustered standard errors",
            "Assembly constituencies",
            "States",
            "PC clusters"
          )
        }
    )

  for (
    j in
      seq_len(
        nrow(
          ordered
        )
      )
  ) {
    row <-
      ordered[
        j,
        ,
        drop =
          FALSE
      ]

    if (
      voter
    ) {
      values <-
        c(
          ifelse(
            row$controls[[1]],
            "Yes",
            "No"
          ),
          "Yes",
          "Yes",
          as.character(
            row$n_voters[[1]]
          ),
          as.character(
            row$n_ac[[1]]
          ),
          as.character(
            row$n_states[[1]]
          )
        )
    } else {
      values <-
        c(
          ifelse(
            row$controls[[1]],
            "Yes",
            "No"
          ),
          "Yes",
          "Yes",
          as.character(
            row$n[[1]]
          ),
          as.character(
            row$n_states[[1]]
          ),
          as.character(
            row$n_pc_clusters[[1]]
          )
        )
    }

    stats[[
      paste0(
        "C",
        j
      )
    ]] <-
      values
  }

  bind_rows(
    out,
    stats
  )
}

render_regression_table <- function(
  summary,
  coefficients,
  voter,
  title,
  note,
  stem
) {
  display <-
    build_regression_display(
      summary,
      coefficients,
      voter =
        voter
    )

  column_labels <-
    c(
      "(1) Additive",
      "(2) Interaction",
      "(3) Interaction + controls",
      "(4) Additive",
      "(5) Interaction",
      "(6) Interaction + controls"
    )

  write_csv(
    display,
    paste0(
      stem,
      ".csv"
    )
  )

  write_grouped_html(
    display,
    title =
      title,
    group_labels =
      c(
        "Total FDI",
        "Manufacturing FDI"
      ),
    column_labels =
      column_labels,
    note =
      note,
    path =
      paste0(
        stem,
        ".html"
      )
  )

  write_grouped_tex(
    display,
    title =
      title,
    group_labels =
      c(
        "Total FDI",
        "Manufacturing FDI"
      ),
    column_labels =
      column_labels,
    note =
      note,
    path =
      paste0(
        stem,
        ".tex"
      )
  )

  display
}

main_ac_note <-
  paste0(
    "Outcome is the 2014 survey-weighted share of centrist respondents voting BJP. ",
    "Current FDI covers April 2009-March 2014 and baseline FDI covers April 2004-March 2009. ",
    "FDI is measured as projects per 100,000 population. ",
    "All models include state fixed effects; standard errors are clustered by parliamentary constituency. ",
    "Primary controls are AC population and SC/ST population shares. ",
    "Control coefficients are omitted for compactness. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )

main_voter_note <-
  paste0(
    "Outcome is an indicator for voting BJP among 2014 centrist NES respondents. ",
    "Models are unweighted linear probability models with state fixed effects and an assembly-constituency random intercept. ",
    "Current FDI covers April 2009-March 2014 and baseline FDI covers April 2004-March 2009. ",
    "Primary controls add individual religion, caste, and education plus AC population and SC/ST shares. ",
    "Control coefficients are omitted for compactness. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )

table1 <-
  render_regression_table(
    main_ac_summary,
    ac_coeff,
    voter =
      FALSE,
    title =
      "Assembly-constituency models of centrist BJP support",
    note =
      main_ac_note,
    stem =
      file.path(
        main_dir,
        "Table_1_ac_centrist_bjp_models"
      )
  )

table2 <-
  render_regression_table(
    main_voter_summary,
    voter_coeff,
    voter =
      TRUE,
    title =
      "Voter-level models of centrist BJP support",
    note =
      main_voter_note,
    stem =
      file.path(
        main_dir,
        "Table_2_voter_centrist_bjp_models"
      )
  )

appendix_a1 <-
  render_regression_table(
    log_ac_summary,
    ac_coeff,
    voter =
      FALSE,
    title =
      "Assembly-constituency models using log1p FDI",
    note =
      paste0(
        main_ac_note,
        " Total and Manufacturing FDI are transformed as log1p(projects per 100,000)."
      ),
    stem =
      file.path(
        appendix_dir,
        "Appendix_Table_A1_ac_log1p_models"
      )
  )

appendix_a2 <-
  render_regression_table(
    log_voter_summary,
    voter_coeff,
    voter =
      TRUE,
    title =
      "Voter-level models using log1p FDI",
    note =
      paste0(
        main_voter_note,
        " Total and Manufacturing FDI are transformed as log1p(projects per 100,000)."
      ),
    stem =
      file.path(
        appendix_dir,
        "Appendix_Table_A2_voter_log1p_models"
      )
  )

wald <-
  read_csv(
    required_file(
      "outputs/post_primary_wald_diagnostics_v1_0/02_appendix_wald_table.csv"
    ),
    show_col_types =
      FALSE
  )

a3 <-
  wald |>
  transmute(
    Sector =
      sector,
    Form =
      functional_form,
    `Restricted current-context Wald` =
      mapply(
        fmt_wald,
        restricted_current_context_wald_chisq,
        restricted_current_context_wald_df,
        restricted_current_context_wald_p
      ),
    `Saturated current-context Wald` =
      mapply(
        fmt_wald,
        saturated_current_context_wald_chisq,
        saturated_current_context_wald_df,
        saturated_current_context_wald_p
      ),
    `Baseline-restriction Wald` =
      mapply(
        fmt_wald,
        baseline_restriction_wald_chisq,
        baseline_restriction_wald_df,
        baseline_restriction_wald_p
      ),
    N =
      n
  )

a3_note <-
  paste0(
    "Post-estimation Wald diagnostics motivated by the observed correlation between current and baseline FDI. ",
    "The restricted current-context test evaluates the current contextual terms in the parsimonious model; ",
    "the saturated test evaluates them after separately parameterizing baseline contextual terms. ",
    "These are diagnostics rather than preregistered primary tests."
  )

write_csv(
  a3,
  file.path(
    appendix_dir,
    "Appendix_Table_A3_post_estimation_wald_diagnostics.csv"
  )
)

write_simple_html(
  a3,
  "Post-estimation Wald diagnostics",
  a3_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A3_post_estimation_wald_diagnostics.html"
  )
)

write_simple_tex(
  a3,
  "Post-estimation Wald diagnostics",
  a3_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A3_post_estimation_wald_diagnostics.tex"
  )
)

native_ideology <-
  read_csv(
    required_file(
      "outputs/ac_ideology_outcome_heterogeneity_v1_0/02_native_ideology_coefficients.csv"
    ),
    show_col_types =
      FALSE
  )

a4_long <-
  native_ideology |>
  mutate(
    cell =
      paste(
        sector,
        functional_form
      ),
    result =
      mapply(
        fmt_coef,
        estimate,
        std_error,
        p_value
      ),
    n_display =
      as.character(
        n_ac
      )
  ) |>
  select(
    cell,
    ideology,
    result,
    n_display
  )

a4 <-
  a4_long |>
  pivot_wider(
    names_from =
      ideology,
    values_from =
      c(
        result,
        n_display
      )
  ) |>
  transmute(
    Specification =
      cell,
    Left =
      result_Left,
    `N Left` =
      n_display_Left,
    Center =
      result_Center,
    `N Center` =
      n_display_Center,
    Right =
      result_Right,
    `N Right` =
      n_display_Right
  )

a4_note <-
  paste0(
    "Native-sample assembly-constituency interaction estimates for Left, Center, and Right BJP-share outcomes. ",
    "Samples differ across ideology-specific outcomes because estimability differs: Left n=83, Center n=224, Right n=79 in the canonical cells. ",
    "These native coefficients should not be interpreted as formal cross-ideology differences; pairwise common-sample Wald tests are reported separately. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )

write_csv(
  a4,
  file.path(
    appendix_dir,
    "Appendix_Table_A4_native_ideology_outcome_heterogeneity.csv"
  )
)

write_simple_html(
  a4,
  "Native-sample ideology-outcome heterogeneity",
  a4_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A4_native_ideology_outcome_heterogeneity.html"
  )
)

write_simple_tex(
  a4,
  "Native-sample ideology-outcome heterogeneity",
  a4_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A4_native_ideology_outcome_heterogeneity.tex"
  )
)

pairwise <-
  read_csv(
    required_file(
      "outputs/ac_ideology_pairwise_wald_refinement_v1_0/01_pairwise_common_sample_wald_tests.csv"
    ),
    show_col_types =
      FALSE
  )

a5 <-
  pairwise |>
  transmute(
    Specification =
      paste(
        sector,
        functional_form
      ),
    Contrast =
      paste0(
        ideology_a,
        " - ",
        ideology_b
      ),
    `Common ACs` =
      n_common_ac,
    `Difference (SE)` =
      mapply(
        fmt_est_se,
        difference,
        std_error
      ),
    `Chi-square p` =
      vapply(
        chi_square_p,
        fmt_p,
        character(1)
      ),
    `Cluster-df F p` =
      vapply(
        cluster_df_F_p,
        fmt_p,
        character(1)
      )
  )

a5_note <-
  paste0(
    "Pairwise tests use the maximal common sample available for each ideology pair. ",
    "The cluster-df F p-value is the preferred finite-cluster reference reported alongside the asymptotic chi-square p-value."
  )

write_csv(
  a5,
  file.path(
    appendix_dir,
    "Appendix_Table_A5_pairwise_ideology_wald_tests.csv"
  )
)

write_simple_html(
  a5,
  "Pairwise ideology heterogeneity Wald tests",
  a5_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A5_pairwise_ideology_wald_tests.html"
  )
)

write_simple_tex(
  a5,
  "Pairwise ideology heterogeneity Wald tests",
  a5_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A5_pairwise_ideology_wald_tests.tex"
  )
)

omnibus <-
  read_csv(
    required_file(
      "outputs/ac_ideology_pairwise_wald_refinement_v1_0/02_three_ideology_omnibus_wald_tests.csv"
    ),
    show_col_types =
      FALSE
  )

a6 <-
  omnibus |>
  transmute(
    Specification =
      paste(
        sector,
        functional_form
      ),
    `Common ACs` =
      n_common_ac,
    `Center-Left` =
      fmt_num(
        center_minus_left
      ),
    `Center-Right` =
      fmt_num(
        center_minus_right
      ),
    `Chi-square p` =
      vapply(
        chi_square_p,
        fmt_p,
        character(1)
      ),
    `Cluster-df F p` =
      vapply(
        cluster_df_F_p,
        fmt_p,
        character(1)
      ),
    `Restriction corr.` =
      fmt_num(
        restriction_covariance_correlation
      ),
    `Condition no.` =
      formatC(
        restriction_covariance_condition_number,
        format =
          "f",
        digits =
          1
      )
  )

a6_note <-
  paste0(
    "Three-ideology equality tests use the 22-AC common sample. ",
    "The Manufacturing-raw omnibus result is accompanied by an approximately 0.999 restriction-covariance correlation and a condition number above 2,600, ",
    "so it is treated as fragile and secondary rather than substantive evidence of ideology-specific heterogeneity."
  )

write_csv(
  a6,
  file.path(
    appendix_dir,
    "Appendix_Table_A6_three_ideology_omnibus_wald_tests.csv"
  )
)

write_simple_html(
  a6,
  "Three-ideology omnibus Wald tests",
  a6_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A6_three_ideology_omnibus_wald_tests.html"
  )
)

write_simple_tex(
  a6,
  "Three-ideology omnibus Wald tests",
  a6_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A6_three_ideology_omnibus_wald_tests.tex"
  )
)

r30 <-
  read_csv(
    required_file(
      "outputs/r30_core_specification_curve_v1_0/01_core_specification_results.csv"
    ),
    show_col_types =
      FALSE
  )

a7 <-
  r30 |>
  arrange(
    factor(
      level,
      levels =
        c(
          "AC",
          "Voter"
        )
    ),
    factor(
      sector,
      levels =
        c(
          "Total",
          "Manufacturing"
        )
    ),
    family,
    functional_form,
    factor(
      control_set,
      levels =
        c(
          "Primary",
          "Expanded"
        )
    )
  ) |>
  transmute(
    Level =
      level,
    Sector =
      sector,
    Specification =
      family,
    Geography =
      geography,
    Form =
      functional_form,
    Controls =
      control_set,
    Status =
      design_status,
    `Interaction (SE)` =
      mapply(
        fmt_est_se,
        estimate,
        std_error
      ),
    `p-value` =
      vapply(
        p_value,
        fmt_p,
        character(1)
      ),
    `Standardized effect, pp` =
      fmt_num(
        standardized_estimate_pp
      ),
    N =
      n,
    `N AC` =
      n_ac
  )

a7_note <-
  paste0(
    "Full numerical results underlying the specification-robustness figures. ",
    "The standardized effect rescales the interaction to the change in the one-percentage-point Muslim-share gradient associated with a one-SD change in FDI. ",
    "The 12-month specifications are explicitly post-estimation temporal-window robustness checks."
  )

write_csv(
  a7,
  file.path(
    appendix_dir,
    "Appendix_Table_A7_core_specification_robustness.csv"
  )
)

write_simple_html(
  a7,
  "Core FDI specification robustness",
  a7_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A7_core_specification_robustness.html"
  )
)

write_simple_tex(
  a7,
  "Core FDI specification robustness",
  a7_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A7_core_specification_robustness.tex"
  )
)

services_ac <-
  read_csv(
    required_file(
      "outputs/sector_form_native_common_v1_0/05_centrist_ac_sector_form_native_common.csv"
    ),
    show_col_types =
      FALSE
  ) |>
  filter(
    sector ==
      "Services",
    sample_type ==
      "Native"
  ) |>
  transmute(
    Level =
      "AC",
    Form =
      functional_form,
    estimate,
    std_error,
    p_value,
    n,
    n_ac,
    n_states
  )

services_voter <-
  read_csv(
    required_file(
      "outputs/sector_form_native_common_v1_0/06_centrist_voter_sector_form_native_common.csv"
    ),
    show_col_types =
      FALSE
  ) |>
  filter(
    sector ==
      "Services",
    sample_type ==
      "Native"
  ) |>
  transmute(
    Level =
      "Voter",
    Form =
      functional_form,
    estimate,
    std_error,
    p_value =
      p_value_normal_approx,
    n =
      n_voters,
    n_ac,
    n_states
  )

services_source <-
  bind_rows(
    services_ac,
    services_voter
  )

if (
  nrow(
    services_source
  ) !=
    4L
) {
  stop(
    "Expected exactly four native Services rows."
  )
}

write_csv(
  services_source,
  file.path(
    source_dir,
    "Appendix_Table_A8_services_model_source.csv"
  )
)

a8 <-
  services_source |>
  transmute(
    Level,
    Form,
    `Interaction (SE)` =
      mapply(
        fmt_coef,
        estimate,
        std_error,
        p_value
      ),
    `p-value` =
      vapply(
        p_value,
        fmt_p,
        character(1)
      ),
    N =
      n,
    `N AC` =
      n_ac,
    States =
      n_states
  )

a8_note <-
  paste0(
    "Existing controlled Services-FDI robustness estimates on the canonical native samples. ",
    "No new model is estimated for this table. Raw and log1p Services interactions are reported for the assembly-constituency and voter analyses. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )

write_csv(
  a8,
  file.path(
    appendix_dir,
    "Appendix_Table_A8_services_fdi_models.csv"
  )
)

write_simple_html(
  a8,
  "Services FDI robustness",
  a8_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A8_services_fdi_models.html"
  )
)

write_simple_tex(
  a8,
  "Services FDI robustness",
  a8_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A8_services_fdi_models.tex"
  )
)

official_summary <-
  read_csv(
    required_file(
      "outputs/official_vote_temporal_saturation_v1_0/01_temporal_saturation_summary.csv"
    ),
    show_col_types =
      FALSE
  )

official_wald <-
  read_csv(
    required_file(
      "outputs/official_vote_temporal_saturation_v1_0/02_joint_baseline_restriction_wald_tests.csv"
    ),
    show_col_types =
      FALSE
  ) |>
  select(
    cell_id,
    baseline_restriction_wald_p =
      p_value
  )

official_source <-
  official_summary |>
  left_join(
    official_wald,
    by =
      "cell_id",
    relationship =
      "one-to-one"
  )

if (
  nrow(
    official_source
  ) !=
    6L
) {
  stop(
    "Expected six official-vote temporal-saturation cells."
  )
}

write_csv(
  official_source,
  file.path(
    source_dir,
    "Appendix_Table_A9_official_vote_temporal_source.csv"
  )
)

a9 <-
  official_source |>
  transmute(
    Sector =
      sector,
    Form =
      functional_form,
    `Restricted current triple` =
      mapply(
        fmt_est_se,
        restricted_current_triple,
        restricted_current_se
      ),
    `Restricted p` =
      vapply(
        restricted_current_p,
        fmt_p,
        character(1)
      ),
    `Saturated current triple` =
      mapply(
        fmt_est_se,
        saturated_current_triple,
        saturated_current_se
      ),
    `Saturated p` =
      vapply(
        saturated_current_p,
        fmt_p,
        character(1)
      ),
    `Saturated baseline triple` =
      mapply(
        fmt_est_se,
        saturated_baseline_triple,
        saturated_baseline_se
      ),
    `Baseline triple p` =
      vapply(
        saturated_baseline_p,
        fmt_p,
        character(1)
      ),
    `Baseline restriction p` =
      vapply(
        baseline_restriction_wald_p,
        fmt_p,
        character(1)
      ),
    `Current-baseline corr.` =
      fmt_num(
        current_baseline_correlation
      ),
    N =
      n
  )

a9_note <-
  paste0(
    "Official BJP vote-share contextual triple-interaction robustness. ",
    "The restricted specification includes the current Muslim-share x FDI x 2009 centrist-context interaction with the corresponding hierarchy. ",
    "The saturated specification separately parameterizes current and baseline contextual triple terms. ",
    "High current-baseline exposure correlation produces substantial temporal collinearity; none of the saturated current triple terms is conventionally precise. ",
    "The joint baseline-restriction Wald tests do not reject the restrictions in any cell."
  )

write_csv(
  a9,
  file.path(
    appendix_dir,
    "Appendix_Table_A9_official_vote_temporal_saturation.csv"
  )
)

write_simple_html(
  a9,
  "Official-vote contextual triple interactions",
  a9_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A9_official_vote_temporal_saturation.html"
  )
)

write_simple_tex(
  a9,
  "Official-vote contextual triple interactions",
  a9_note,
  file.path(
    appendix_dir,
    "Appendix_Table_A9_official_vote_temporal_saturation.tex"
  )
)

registry_v1_0 <-
  read_csv(
    required_file(
      "config/paper_artifacts_v1_0.csv"
    ),
    show_col_types =
      FALSE
  )

table_paths <-
  tribble(
    ~paper_id, ~publication_artifact, ~source_artifact,

    "Table 1",
    "outputs/paper_tables_v1_0/main/Table_1_ac_centrist_bjp_models.tex",
    main_ac_summary_path,

    "Table 2",
    "outputs/paper_tables_v1_0/main/Table_2_voter_centrist_bjp_models.tex",
    main_voter_summary_path,

    "Appendix Table A1",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A1_ac_log1p_models.tex",
    log_ac_summary_path,

    "Appendix Table A2",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A2_voter_log1p_models.tex",
    log_voter_summary_path,

    "Appendix Table A3",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A3_post_estimation_wald_diagnostics.tex",
    "outputs/post_primary_wald_diagnostics_v1_0/02_appendix_wald_table.csv",

    "Appendix Table A4",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A4_native_ideology_outcome_heterogeneity.tex",
    "outputs/ac_ideology_outcome_heterogeneity_v1_0/02_native_ideology_coefficients.csv",

    "Appendix Table A5",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A5_pairwise_ideology_wald_tests.tex",
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/01_pairwise_common_sample_wald_tests.csv",

    "Appendix Table A6",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A6_three_ideology_omnibus_wald_tests.tex",
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/02_three_ideology_omnibus_wald_tests.csv",

    "Appendix Table A7",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A7_core_specification_robustness.tex",
    "outputs/r30_core_specification_curve_v1_0/01_core_specification_results.csv",

    "Appendix Table A8",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A8_services_fdi_models.tex",
    "outputs/paper_tables_v1_0/source/Appendix_Table_A8_services_model_source.csv",

    "Appendix Table A9",
    "outputs/paper_tables_v1_0/appendix/Appendix_Table_A9_official_vote_temporal_saturation.tex",
    "outputs/paper_tables_v1_0/source/Appendix_Table_A9_official_vote_temporal_source.csv"
  )

registry_v1_1 <-
  registry_v1_0 |>
  select(
    -any_of(
      "architecture_version"
    ),
    -any_of(
      "frozen_date"
    )
  ) |>
  left_join(
    table_paths |>
      rename(
        new_publication_artifact =
          publication_artifact,
        new_source_artifact =
          source_artifact
      ),
    by =
      "paper_id",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    publication_artifact =
      coalesce(
        new_publication_artifact,
        publication_artifact
      ),

    source_artifact =
      coalesce(
        new_source_artifact,
        source_artifact
      ),

    publication_status =
      if_else(
        paper_id %in%
          table_paths$paper_id,
        "Built",
        publication_status
      ),

    generating_script =
      if_else(
        paper_id %in%
          table_paths$paper_id,
        "R/34_render_publication_tables_v1_0.R",
        generating_script
      ),

    architecture_version =
      "v1.1",

    frozen_date =
      as.character(
        Sys.Date()
      )
  ) |>
  select(
    -new_publication_artifact,
    -new_source_artifact
  )

if (
  anyDuplicated(
    registry_v1_1$paper_id
  ) >
    0L
) {
  stop(
    "Duplicate paper IDs after R34 registry update."
  )
}

built_missing <-
  registry_v1_1 |>
  filter(
    publication_status ==
      "Built",
    (
      is.na(
        publication_artifact
      ) |
        !file.exists(
          publication_artifact
        )
    )
  )

if (
  nrow(
    built_missing
  ) >
    0L
) {
  print(
    built_missing,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one built artifact is missing after R34."
  )
}

write_csv(
  registry_v1_1,
  "config/paper_artifacts_v1_1.csv"
)

upstream_sources <-
  c(
    ac_coeff_path,
    voter_coeff_path,
    main_ac_summary_path,
    main_voter_summary_path,
    log_ac_summary_path,
    log_voter_summary_path,
    "outputs/post_primary_wald_diagnostics_v1_0/02_appendix_wald_table.csv",
    "outputs/ac_ideology_outcome_heterogeneity_v1_0/02_native_ideology_coefficients.csv",
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/01_pairwise_common_sample_wald_tests.csv",
    "outputs/ac_ideology_pairwise_wald_refinement_v1_0/02_three_ideology_omnibus_wald_tests.csv",
    "outputs/r30_core_specification_curve_v1_0/01_core_specification_results.csv",
    "outputs/sector_form_native_common_v1_0/05_centrist_ac_sector_form_native_common.csv",
    "outputs/sector_form_native_common_v1_0/06_centrist_voter_sector_form_native_common.csv",
    "outputs/official_vote_temporal_saturation_v1_0/01_temporal_saturation_summary.csv",
    "outputs/official_vote_temporal_saturation_v1_0/02_joint_baseline_restriction_wald_tests.csv"
  )

provenance <-
  tibble(
    path =
      upstream_sources,
    exists =
      file.exists(
        upstream_sources
      ),
    bytes =
      file.info(
        upstream_sources
      )$size,
    md5 =
      unname(
        tools::md5sum(
          upstream_sources
        )
      )
  )

write_csv(
  provenance,
  file.path(
    output_dir,
    "01_upstream_source_provenance.csv"
  )
)

all_table_files <-
  list.files(
    output_dir,
    pattern =
      "\\.(csv|html|tex)$",
    recursive =
      TRUE,
    full.names =
      TRUE
  )

manifest <-
  tibble(
    path =
      all_table_files,
    bytes =
      file.info(
        all_table_files
      )$size,
    md5 =
      unname(
        tools::md5sum(
          all_table_files
        )
      )
  )

write_csv(
  manifest,
  file.path(
    output_dir,
    "02_generated_table_manifest.csv"
  )
)

status_summary <-
  registry_v1_1 |>
  count(
    placement,
    artifact_type,
    publication_status,
    name =
      "n"
  ) |>
  arrange(
    placement,
    artifact_type,
    publication_status
  )

write_csv(
  status_summary,
  file.path(
    output_dir,
    "03_registry_status_after_r34.csv"
  )
)

notes <-
  c(
    "R34 PUBLICATION TABLE RENDERING",
    "",
    "No model is estimated or altered by this script.",
    "",
    "Tables 1 and 2 are rendered from the frozen R28 coefficient and metadata files.",
    "",
    "Appendix Tables A1 and A2 are the parallel log1p Total and Manufacturing models.",
    "",
    "Appendix Table A3 reports the frozen post-estimation Wald diagnostics.",
    "",
    "Appendix Table A4 uses the native ideology-specific samples: Left n=83, Center n=224, Right n=79 in the canonical cells.",
    "",
    "Appendix Table A5 reports maximal pair-specific common-sample Wald tests.",
    "",
    "Appendix Table A6 reports the three-ideology omnibus tests and covariance diagnostics.",
    "",
    "Appendix Table A7 reports the complete R30 specification-robustness results.",
    "",
    "Appendix Table A8 uses already-estimated native Services FDI results and introduces no new model.",
    "",
    "Appendix Table A9 uses the frozen official-vote restricted and temporally saturated triple-interaction results.",
    "",
    "Registry v1.1 supersedes v1.0 for table rendering status while preserving the R33 v1.0 freeze."
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
  "\n===== TABLE 1 =====\n"
)

print(
  table1,
  n = Inf,
  width = Inf
)

cat(
  "\n===== TABLE 2 =====\n"
)

print(
  table2,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CORRECTED APPENDIX TABLE A4 =====\n"
)

print(
  a4,
  n = Inf,
  width = Inf
)

cat(
  "\n===== SERVICES TABLE A8 =====\n"
)

print(
  a8,
  n = Inf,
  width = Inf
)

cat(
  "\n===== OFFICIAL-VOTE TABLE A9 =====\n"
)

print(
  a9,
  n = Inf,
  width = Inf
)

cat(
  "\n===== REGISTRY STATUS AFTER R34 =====\n"
)

print(
  status_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nR34_PUBLICATION_TABLE_RENDERING_COMPLETE\n"
)

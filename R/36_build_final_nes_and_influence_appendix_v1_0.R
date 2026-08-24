suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
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

output_dir <-
  file.path(
    "outputs",
    "paper_appendix_final_diagnostics_v1_0"
  )

a11_dir <-
  file.path(
    output_dir,
    "A11_NES"
  )

a11_tables_dir <-
  file.path(
    a11_dir,
    "tables"
  )

a11_figures_dir <-
  file.path(
    a11_dir,
    "figures"
  )

a12_dir <-
  file.path(
    output_dir,
    "A12_influence_support"
  )

a12_tables_dir <-
  file.path(
    a12_dir,
    "tables"
  )

a12_figures_dir <-
  file.path(
    a12_dir,
    "figures"
  )

for (
  d in
    c(
      a11_dir,
      a11_tables_dir,
      a11_figures_dir,
      a12_dir,
      a12_tables_dir,
      a12_figures_dir
    )
) {
  dir.create(
    d,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

required_file <- function(
  path
) {
  if (
    !file.exists(
      path
    )
  ) {
    stop(
      "Required source missing: ",
      path
    )
  }

  path
}

html_escape <- function(
  x
) {
  x <-
    as.character(
      x
    )

  x[
    is.na(
      x
    )
  ] <-
    ""

  x <-
    gsub(
      "&",
      "&amp;",
      x,
      fixed = TRUE
    )

  x <-
    gsub(
      "<",
      "&lt;",
      x,
      fixed = TRUE
    )

  x <-
    gsub(
      ">",
      "&gt;",
      x,
      fixed = TRUE
    )

  x
}

df_to_html <- function(
  data,
  max_rows = Inf
) {
  shown <-
    if (
      is.finite(
        max_rows
      )
    ) {
      head(
        data,
        max_rows
      )
    } else {
      data
    }

  header <-
    paste0(
      "<th>",
      html_escape(
        names(
          shown
        )
      ),
      "</th>",
      collapse = ""
    )

  if (
    nrow(
      shown
    ) ==
      0L
  ) {
    rows <-
      paste0(
        "<tr><td colspan=\"",
        ncol(
          shown
        ),
        "\">No rows</td></tr>"
      )
  } else {
    rows <-
      apply(
        shown,
        1,
        function(row) {
          paste0(
            "<tr>",
            paste0(
              "<td>",
              html_escape(
                row
              ),
              "</td>",
              collapse = ""
            ),
            "</tr>"
          )
        }
      )
  }

  paste0(
    "<table><thead><tr>",
    header,
    "</tr></thead><tbody>",
    paste(
      rows,
      collapse = "\n"
    ),
    "</tbody></table>"
  )
}

write_table_bundle <- function(
  source_path,
  output_stem,
  title,
  max_rows_html = Inf
) {
  x <-
    read_csv(
      required_file(
        source_path
      ),
      show_col_types = FALSE
    )

  csv_path <-
    paste0(
      output_stem,
      ".csv"
    )

  html_path <-
    paste0(
      output_stem,
      ".html"
    )

  write_csv(
    x,
    csv_path
  )

  html <-
    c(
      "<!doctype html>",
      "<html><head><meta charset=\"utf-8\">",
      "<style>",
      "body{font-family:Arial,Helvetica,sans-serif;margin:30px;color:#111;}",
      "table{border-collapse:collapse;width:100%;font-size:11px;}",
      "th{border-bottom:2px solid #222;padding:5px;text-align:left;}",
      "td{border-bottom:1px solid #ddd;padding:5px;vertical-align:top;}",
      "h2{margin-bottom:12px;}",
      "</style></head><body>",
      paste0(
        "<h2>",
        html_escape(
          title
        ),
        "</h2>"
      ),
      df_to_html(
        x,
        max_rows =
          max_rows_html
      ),
      "</body></html>"
    )

  writeLines(
    html,
    html_path
  )

  tibble(
    title =
      title,
    source_path =
      source_path,
    csv_path =
      csv_path,
    html_path =
      html_path,
    n_rows =
      nrow(
        x
      ),
    n_columns =
      ncol(
        x
      )
  )
}

a11_table_registry <-
  bind_rows(
    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/01_2009_item_by_item_coding_audit.csv",
      file.path(
        a11_tables_dir,
        "A11a_2009_item_by_item_coding_audit"
      ),
      "NES 2009 ideology item-by-item coding audit"
    ),

    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/04_2009_classification_summary.csv",
      file.path(
        a11_tables_dir,
        "A11b_2009_classification_summary"
      ),
      "NES 2009 ideology classification summary"
    ),

    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/05_2009_strict_to_main_harmonized_transition.csv",
      file.path(
        a11_tables_dir,
        "A11c_2009_classification_transition"
      ),
      "NES 2009 strict-to-harmonized ideology classification transition"
    ),

    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/06_crossyear_bjp_vote_by_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11d_bjp_vote_by_ideology_2009_2014"
      ),
      "BJP vote by ideology in the 2009 and 2014 NES"
    ),

    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/07_mixed_category_footnote_audit.csv",
      file.path(
        a11_tables_dir,
        "A11e_mixed_category_audit"
      ),
      "NES Mixed ideology-category audit"
    ),

    write_table_bundle(
      "outputs/nes_2009_2014_ideology_audit_v1_1/09_existing_vs_fresh_2009_strict_summary.csv",
      file.path(
        a11_tables_dir,
        "A11f_2009_existing_vs_fresh_coding"
      ),
      "NES 2009 existing-versus-fresh coding reproduction"
    ),

    write_table_bundle(
      "outputs/nes_2014_ideology_audit_v1_2/02_bjp_vote_by_ideology_scheme.csv",
      file.path(
        a11_tables_dir,
        "A11g_2014_bjp_vote_by_ideology_scheme"
      ),
      "NES 2014 BJP vote by ideology coding scheme"
    ),

    write_table_bundle(
      "outputs/nes_2014_ideology_audit_v1_2/04_classification_transition_3q_to_2q.csv",
      file.path(
        a11_tables_dir,
        "A11h_2014_classification_transition"
      ),
      "NES 2014 ideology-classification transition audit"
    ),

    write_table_bundle(
      "outputs/nes_2014_ideology_audit_v1_2/05_any_party_and_bjp_closeness_by_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11i_2014_party_closeness_by_ideology"
      ),
      "NES 2014 party closeness by ideology"
    ),

    write_table_bundle(
      "outputs/nes_2014_ideology_audit_v1_2/08_existing_vs_fresh_coding_summary.csv",
      file.path(
        a11_tables_dir,
        "A11j_2014_existing_vs_fresh_coding"
      ),
      "NES 2014 existing-versus-fresh coding reproduction"
    ),

    write_table_bundle(
      "data/derived/switchers_rewrite/diagnostics/ideology_classification_by_year.csv",
      file.path(
        a11_tables_dir,
        "A11k_ideology_classification_by_year"
      ),
      "NES ideology classification by year"
    ),

    write_table_bundle(
      "data/derived/switchers_rewrite/diagnostics/income_distribution_by_year_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11l_income_distribution_by_year_ideology"
      ),
      "NES income distribution by year and ideology"
    ),

    write_table_bundle(
      "data/derived/switchers_rewrite/diagnostics/education_distribution_by_year_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11m_education_distribution_by_year_ideology"
      ),
      "NES education distribution by year and ideology"
    ),

    write_table_bundle(
      "data/derived/switchers_rewrite/diagnostics/fr_vote_by_year_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11n_far_right_vote_by_year_ideology"
      ),
      "NES far-right vote by year and ideology"
    ),

    write_table_bundle(
      "data/derived/switchers_rewrite/diagnostics/fr_closeness_by_year_ideology.csv",
      file.path(
        a11_tables_dir,
        "A11o_far_right_closeness_by_year_ideology"
      ),
      "NES far-right party closeness by year and ideology"
    )
  )

a11_figure_sources <-
  c(
    "outputs/nes_2009_2014_ideology_audit_v1_1/12_bjp_vote_share_by_ideology_2009_2014_audited.png",
    "data/derived/switchers_rewrite/diagnostics/ideology_classification_2009.png",
    "data/derived/switchers_rewrite/diagnostics/ideology_classification_2014.png",
    "data/derived/switchers_rewrite/diagnostics/income_distribution_by_ideology_2009.png",
    "data/derived/switchers_rewrite/diagnostics/income_distribution_by_ideology_2014.png",
    "data/derived/switchers_rewrite/diagnostics/education_distribution_by_ideology_2009.png",
    "data/derived/switchers_rewrite/diagnostics/education_distribution_by_ideology_2014.png",
    "data/derived/switchers_rewrite/diagnostics/fr_vote_by_ideology_2009.png",
    "data/derived/switchers_rewrite/diagnostics/fr_vote_by_ideology_2014.png",
    "data/derived/switchers_rewrite/diagnostics/fr_closeness_by_ideology_2009.png",
    "data/derived/switchers_rewrite/diagnostics/fr_closeness_by_ideology_2014.png"
  )

a11_existing_figures <-
  a11_figure_sources[
    file.exists(
      a11_figure_sources
    )
  ]

a11_copied_figures <-
  character()

for (
  source in
    a11_existing_figures
) {
  destination <-
    file.path(
      a11_figures_dir,
      basename(
        source
      )
    )

  ok <-
    file.copy(
      source,
      destination,
      overwrite = TRUE
    )

  if (
    !ok
  ) {
    stop(
      "Could not copy NES diagnostic figure: ",
      source
    )
  }

  a11_copied_figures <-
    c(
      a11_copied_figures,
      destination
    )
}

write_csv(
  a11_table_registry,
  file.path(
    a11_dir,
    "01_A11_table_registry.csv"
  )
)

a11_figure_registry <-
  tibble(
    source_path =
      a11_existing_figures,
    copied_path =
      a11_copied_figures
  )

write_csv(
  a11_figure_registry,
  file.path(
    a11_dir,
    "02_A11_figure_registry.csv"
  )
)

a11_table_links <-
  paste0(
    "<li><a href=\"tables/",
    basename(
      a11_table_registry$html_path
    ),
    "\">",
    html_escape(
      a11_table_registry$title
    ),
    "</a> (",
    a11_table_registry$n_rows,
    " rows)</li>",
    collapse = "\n"
  )

a11_figure_links <-
  if (
    nrow(
      a11_figure_registry
    ) >
      0L
  ) {
    paste0(
      "<div class=\"figure\"><p>",
      html_escape(
        basename(
          a11_figure_registry$copied_path
        )
      ),
      "</p><img src=\"figures/",
      basename(
        a11_figure_registry$copied_path
      ),
      "\"></div>",
      collapse = "\n"
    )
  } else {
    "<p>No diagnostic PNG files were available.</p>"
  }

a11_index <-
  c(
    "<!doctype html>",
    "<html><head><meta charset=\"utf-8\">",
    "<style>",
    "body{font-family:Arial,Helvetica,sans-serif;margin:35px;max-width:1100px;color:#111;}",
    "h1,h2{margin-top:28px;}",
    "li{margin:7px 0;}",
    ".note{background:#f4f4f4;padding:14px;line-height:1.45;}",
    ".figure{margin:26px 0;padding-top:12px;border-top:1px solid #ddd;}",
    "img{max-width:100%;height:auto;border:1px solid #ddd;}",
    "</style></head><body>",
    "<h1>Appendix A11. NES classification and composition diagnostics</h1>",
    paste0(
      "<div class=\"note\">",
      "This appendix package uses the audited NES 2009/2014 ideology outputs and current canonical NES diagnostic files. ",
      "It documents ideology-item coding, classification transitions, BJP vote choice, party closeness, and income/education composition. ",
      "The audited cross-year BJP figure is the same numerical source used for Main Figure 3. ",
      "Older exploratory respondent specification-curve outputs are deliberately excluded.",
      "</div>"
    ),
    "<h2>Tables</h2>",
    "<ul>",
    a11_table_links,
    "</ul>",
    "<h2>Figures</h2>",
    a11_figure_links,
    "</body></html>"
  )

a11_index_path <-
  file.path(
    a11_dir,
    "Appendix_A11_NES_diagnostics_index.html"
  )

writeLines(
  a11_index,
  a11_index_path
)

leave_ac_path <-
  required_file(
    "outputs/ac_canonical_v1_0/06_primary_leave_one_ac_influence.csv"
  )

leave_state_path <-
  required_file(
    "outputs/ac_canonical_v1_0/07_primary_leave_one_state_influence.csv"
  )

ac_focal_path <-
  required_file(
    "outputs/ac_canonical_v1_0/04_focal_interaction_coefficients.csv"
  )

ac_sample_path <-
  required_file(
    "outputs/ac_canonical_v1_0/08_primary_regression_sample.csv"
  )

ac_change_path <-
  required_file(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

leave_ac <-
  read_csv(
    leave_ac_path,
    show_col_types = FALSE
  )

leave_state <-
  read_csv(
    leave_state_path,
    show_col_types = FALSE
  )

ac_focal <-
  read_csv(
    ac_focal_path,
    show_col_types = FALSE
  )

ac_sample <-
  read_csv(
    ac_sample_path,
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    ac_change_path
  )

if (
  nrow(
    leave_ac
  ) !=
    224L
) {
  stop(
    "Expected 224 leave-one-AC refits."
  )
}

if (
  nrow(
    leave_state
  ) !=
    25L
) {
  stop(
    "Expected 25 leave-one-state refits."
  )
}

full_row <-
  ac_focal |>
  filter(
    model_id ==
      "AC01"
  )

if (
  nrow(
    full_row
  ) !=
    1L
) {
  stop(
    "Could not uniquely identify AC01 focal interaction."
  )
}

full_estimate <-
  full_row$estimate[[1]]

full_se <-
  full_row$std_error[[1]]

influence_summary <-
  tibble(
    diagnostic =
      c(
        "Full AC01 estimate",
        "Full AC01 standard error",
        "Minimum leave-one-AC estimate",
        "Maximum leave-one-AC estimate",
        "Maximum absolute leave-one-AC coefficient shift",
        "Maximum leave-one-AC shift in full-model SE units",
        "Leave-one-AC sign reversals",
        "Failed leave-one-AC refits",
        "Minimum leave-one-state estimate",
        "Maximum leave-one-state estimate",
        "Maximum absolute leave-one-state coefficient shift",
        "Maximum leave-one-state shift in full-model SE units",
        "Leave-one-state sign reversals",
        "Failed leave-one-state refits"
      ),

    value =
      c(
        full_estimate,
        full_se,
        min(
          leave_ac$estimate,
          na.rm = TRUE
        ),
        max(
          leave_ac$estimate,
          na.rm = TRUE
        ),
        max(
          abs(
            leave_ac$delta_from_full
          ),
          na.rm = TRUE
        ),
        max(
          abs(
            leave_ac$shift_in_full_se
          ),
          na.rm = TRUE
        ),
        sum(
          sign(
            leave_ac$estimate
          ) !=
            sign(
              full_estimate
            ),
          na.rm = TRUE
        ),
        sum(
          !is.finite(
            leave_ac$estimate
          )
        ),
        min(
          leave_state$estimate,
          na.rm = TRUE
        ),
        max(
          leave_state$estimate,
          na.rm = TRUE
        ),
        max(
          abs(
            leave_state$delta_from_full
          ),
          na.rm = TRUE
        ),
        max(
          abs(
            leave_state$shift_in_full_se
          ),
          na.rm = TRUE
        ),
        sum(
          sign(
            leave_state$estimate
          ) !=
            sign(
              full_estimate
            ),
          na.rm = TRUE
        ),
        sum(
          !is.finite(
            leave_state$estimate
          )
        )
      )
  )

write_csv(
  influence_summary,
  file.path(
    a12_tables_dir,
    "A12a_influence_summary.csv"
  )
)

write_csv(
  leave_ac,
  file.path(
    a12_tables_dir,
    "A12b_leave_one_ac_full_results.csv"
  )
)

write_csv(
  leave_state,
  file.path(
    a12_tables_dir,
    "A12c_leave_one_state_full_results.csv"
  )
)

top_ac <-
  leave_ac |>
  arrange(
    desc(
      abs(
        delta_from_full
      )
    )
  ) |>
  slice_head(
    n = 15
  )

top_state <-
  leave_state |>
  arrange(
    desc(
      abs(
        delta_from_full
      )
    )
  )

write_csv(
  top_ac,
  file.path(
    a12_tables_dir,
    "A12d_most_influential_ac_refits.csv"
  )
)

write_csv(
  top_state,
  file.path(
    a12_tables_dir,
    "A12e_state_influence_ranked.csv"
  )
)

joined_support <-
  ac_sample |>
  select(
    ac_uid,
    total_current =
      fdi_total_local_all_pc100k_2014,
    total_baseline =
      fdi_total_local_all_pc100k_2009
  ) |>
  left_join(
    ac_change |>
      transmute(
        ac_uid =
          as.character(
            ac_uid
          ),

        manufacturing_current =
          as.numeric(
            fdi_mfg_local_all_pc100k_2014
          ),

        manufacturing_baseline =
          as.numeric(
            fdi_mfg_local_all_pc100k_2009
          ),

        services_current =
          as.numeric(
            fdi_services_local_all_pc100k_2014
          ),

        services_baseline =
          as.numeric(
            fdi_services_local_all_pc100k_2009
          )
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

support_summary_one <- function(
  current,
  baseline,
  sector
) {
  tibble(
    sector =
      sector,

    n =
      sum(
        is.finite(
          current
        ) &
          is.finite(
            baseline
          )
      ),

    current_zero_share =
      mean(
        current ==
          0,
        na.rm = TRUE
      ),

    current_positive_share =
      mean(
        current >
          0,
        na.rm = TRUE
      ),

    current_mean =
      mean(
        current,
        na.rm = TRUE
      ),

    current_sd =
      sd(
        current,
        na.rm = TRUE
      ),

    current_median =
      median(
        current,
        na.rm = TRUE
      ),

    current_p90 =
      quantile(
        current,
        0.90,
        na.rm = TRUE,
        names = FALSE
      ),

    current_p95 =
      quantile(
        current,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    current_max =
      max(
        current,
        na.rm = TRUE
      ),

    baseline_zero_share =
      mean(
        baseline ==
          0,
        na.rm = TRUE
      ),

    baseline_mean =
      mean(
        baseline,
        na.rm = TRUE
      ),

    current_baseline_correlation =
      cor(
        current,
        baseline,
        use =
          "complete.obs"
      )
  )
}

support_summary <-
  bind_rows(
    support_summary_one(
      joined_support$total_current,
      joined_support$total_baseline,
      "Total"
    ),

    support_summary_one(
      joined_support$manufacturing_current,
      joined_support$manufacturing_baseline,
      "Manufacturing"
    ),

    support_summary_one(
      joined_support$services_current,
      joined_support$services_baseline,
      "Services"
    )
  )

write_csv(
  support_summary,
  file.path(
    a12_tables_dir,
    "A12f_AC01_FDI_support_summary.csv"
  )
)

r29b_support_files <-
  c(
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/01_support_summary_positive_and_zero_mass.csv",
    "outputs/manufacturing_marginal_effects_display_refinement_v1_0/04_support_at_key_fdi_values.csv"
  )

r29b_support_files <-
  r29b_support_files[
    file.exists(
      r29b_support_files
    )
  ]

r29b_registry <-
  tibble(
    source_path =
      r29b_support_files,
    copied_path =
      file.path(
        a12_tables_dir,
        basename(
          r29b_support_files
        )
      )
  )

if (
  nrow(
    r29b_registry
  ) >
    0L
) {
  for (
    i in
      seq_len(
        nrow(
          r29b_registry
        )
      )
  ) {
    ok <-
      file.copy(
        r29b_registry$source_path[[i]],
        r29b_registry$copied_path[[i]],
        overwrite = TRUE
      )

    if (
      !ok
    ) {
      stop(
        "Failed to copy R29b support file."
      )
    }
  }
}

voter_support_path <-
  "outputs/voter_canonical_v1_0/09_ac_support_by_model.csv"

if (
  file.exists(
    voter_support_path
  )
) {
  file.copy(
    voter_support_path,
    file.path(
      a12_tables_dir,
      "A12g_voter_AC_support_by_model.csv"
    ),
    overwrite = TRUE
  )
}

paper_theme <-
  theme_minimal(
    base_size = 10.5
  ) +
  theme(
    panel.grid.minor =
      element_blank(),
    plot.title =
      element_text(
        face =
          "bold"
      ),
    plot.caption =
      element_text(
        hjust =
          0,
        size =
          8
      )
  )

leave_ac_plot_data <-
  leave_ac |>
  mutate(
    rank =
      rank(
        delta_from_full,
        ties.method =
          "first"
      )
  )

leave_ac_plot <-
  ggplot(
    leave_ac_plot_data,
    aes(
      x =
        rank,
      y =
        estimate
    )
  ) +
  geom_hline(
    yintercept =
      full_estimate,
    linewidth =
      0.55,
    linetype =
      "dashed"
  ) +
  geom_point(
    size =
      1.5
  ) +
  labs(
    title =
      "Leave-one-constituency influence on the AC01 interaction",
    subtitle =
      "Each point re-estimates AC01 after omitting one assembly constituency",
    x =
      "Refit ordered by estimated interaction",
    y =
      "FDI x Muslim-share interaction estimate",
    caption =
      paste0(
        "Dashed line is the full-sample AC01 estimate. ",
        "All 224 canonical AC01 observations are omitted one at a time; ",
        "the underlying table reports coefficient shifts and shifts in full-model standard-error units."
      )
  ) +
  paper_theme

leave_state_plot <-
  leave_state |>
  arrange(
    estimate
  ) |>
  mutate(
    omitted_state =
      factor(
        as.character(
          omitted_state_no
        ),
        levels =
          as.character(
            omitted_state_no
          )
      )
  ) |>
  ggplot(
    aes(
      x =
        estimate,
      y =
        omitted_state
    )
  ) +
  geom_vline(
    xintercept =
      full_estimate,
    linewidth =
      0.55,
    linetype =
      "dashed"
  ) +
  geom_point(
    size =
      2
  ) +
  labs(
    title =
      "Leave-one-state influence on the AC01 interaction",
    subtitle =
      "Each point re-estimates AC01 after omitting one state",
    x =
      "FDI x Muslim-share interaction estimate",
    y =
      "Omitted state number",
    caption =
      "Dashed line is the full-sample AC01 estimate. The underlying table reports all 25 state-omission refits."
  ) +
  paper_theme

save_plot <- function(
  plot,
  stem,
  width,
  height
) {
  ggsave(
    paste0(
      stem,
      ".png"
    ),
    plot =
      plot,
    width =
      width,
    height =
      height,
    units =
      "in",
    dpi =
      300,
    bg =
      "white"
  )

  ggsave(
    paste0(
      stem,
      ".pdf"
    ),
    plot =
      plot,
    width =
      width,
    height =
      height,
    units =
      "in",
    device =
      grDevices::pdf,
    useDingbats =
      FALSE
  )
}

save_plot(
  leave_ac_plot,
  file.path(
    a12_figures_dir,
    "A12a_leave_one_AC_influence"
  ),
  width =
    7.6,
  height =
    5.4
)

save_plot(
  leave_state_plot,
  file.path(
    a12_figures_dir,
    "A12b_leave_one_state_influence"
  ),
  width =
    7.6,
  height =
    6.2
)

a12_summary_html <-
  df_to_html(
    influence_summary
  )

a12_support_html <-
  df_to_html(
    support_summary
  )

a12_top_ac_html <-
  df_to_html(
    top_ac
  )

a12_state_html <-
  df_to_html(
    top_state
  )

a12_index <-
  c(
    "<!doctype html>",
    "<html><head><meta charset=\"utf-8\">",
    "<style>",
    "body{font-family:Arial,Helvetica,sans-serif;margin:35px;max-width:1150px;color:#111;}",
    "table{border-collapse:collapse;width:100%;font-size:11px;margin:14px 0 28px 0;}",
    "th{border-bottom:2px solid #222;padding:5px;text-align:left;}",
    "td{border-bottom:1px solid #ddd;padding:5px;vertical-align:top;}",
    "img{max-width:100%;height:auto;border:1px solid #ddd;margin:10px 0 25px 0;}",
    ".note{background:#f4f4f4;padding:14px;line-height:1.45;}",
    "</style></head><body>",
    "<h1>Appendix A12. Influence and empirical-support diagnostics</h1>",
    paste0(
      "<div class=\"note\">",
      "Influence diagnostics refer to the canonical AC01 Total-FDI model and use the exact frozen 224-AC sample. ",
      "The support table describes Total, Manufacturing, and Services FDI on that same AC01 sample. ",
      "These diagnostics are appendix-only and do not redefine the estimation sample or introduce trimming.",
      "</div>"
    ),
    "<h2>Influence summary</h2>",
    a12_summary_html,
    "<h2>Leave-one-AC influence</h2>",
    "<img src=\"figures/A12a_leave_one_AC_influence.png\">",
    "<h3>Fifteen largest coefficient shifts</h3>",
    a12_top_ac_html,
    "<h2>Leave-one-state influence</h2>",
    "<img src=\"figures/A12b_leave_one_state_influence.png\">",
    a12_state_html,
    "<h2>FDI support in the canonical AC01 sample</h2>",
    a12_support_html,
    "</body></html>"
  )

a12_index_path <-
  file.path(
    a12_dir,
    "Appendix_A12_influence_and_support_diagnostics.html"
  )

writeLines(
  a12_index,
  a12_index_path
)

a11_sources <-
  unique(
    c(
      a11_table_registry$source_path,
      a11_figure_registry$source_path
    )
  )

a12_sources <-
  unique(
    c(
      leave_ac_path,
      leave_state_path,
      ac_focal_path,
      ac_sample_path,
      ac_change_path,
      r29b_support_files,
      if (
        file.exists(
          voter_support_path
        )
      ) {
        voter_support_path
      } else {
        character()
      }
    )
  )

source_manifest <-
  bind_rows(
    tibble(
      appendix_item =
        "A11",
      path =
        a11_sources
    ),

    tibble(
      appendix_item =
        "A12",
      path =
        a12_sources
    )
  ) |>
  mutate(
    exists =
      file.exists(
        path
      ),

    bytes =
      file.info(
        path
      )$size,

    md5 =
      vapply(
        path,
        function(p) {
          if (
            file.exists(
              p
            ) &&
              !dir.exists(
                p
              )
          ) {
            unname(
              tools::md5sum(
                p
              )
            )
          } else {
            NA_character_
          }
        },
        character(1)
      )
  )

write_csv(
  source_manifest,
  file.path(
    output_dir,
    "01_source_manifest.csv"
  )
)

a11_bundle_manifest <-
  tibble(
    path =
      list.files(
        a11_dir,
        recursive = TRUE,
        full.names = TRUE
      )
  ) |>
  mutate(
    bytes =
      file.info(
        path
      )$size,
    md5 =
      unname(
        tools::md5sum(
          path
        )
      )
  )

a12_bundle_manifest <-
  tibble(
    path =
      list.files(
        a12_dir,
        recursive = TRUE,
        full.names = TRUE
      )
  ) |>
  mutate(
    bytes =
      file.info(
        path
      )$size,
    md5 =
      unname(
        tools::md5sum(
          path
        )
      )
  )

write_csv(
  a11_bundle_manifest,
  file.path(
    a11_dir,
    "03_A11_bundle_manifest.csv"
  )
)

write_csv(
  a12_bundle_manifest,
  file.path(
    a12_dir,
    "01_A12_bundle_manifest.csv"
  )
)

registry <-
  read_csv(
    required_file(
      "config/paper_artifacts_v1_2.csv"
    ),
    show_col_types = FALSE
  ) |>
  select(
    -any_of(
      c(
        "architecture_version",
        "frozen_date"
      )
    )
  )

updates <-
  tribble(
    ~paper_id,
    ~publication_artifact,
    ~source_artifact,

    "Appendix Figure/Table A11",
    "outputs/paper_appendix_final_diagnostics_v1_0/A11_NES/Appendix_A11_NES_diagnostics_index.html",
    "outputs/paper_appendix_final_diagnostics_v1_0/A11_NES/03_A11_bundle_manifest.csv",

    "Appendix Diagnostic A12",
    "outputs/paper_appendix_final_diagnostics_v1_0/A12_influence_support/Appendix_A12_influence_and_support_diagnostics.html",
    "outputs/paper_appendix_final_diagnostics_v1_0/A12_influence_support/01_A12_bundle_manifest.csv"
  )

registry_v1_3 <-
  registry |>
  left_join(
    updates |>
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
          updates$paper_id,
        "Built",
        publication_status
      ),

    generating_script =
      if_else(
        paper_id %in%
          updates$paper_id,
        "R/36_build_final_nes_and_influence_appendix_v1_0.R",
        generating_script
      ),

    architecture_version =
      "v1.3",

    frozen_date =
      as.character(
        Sys.Date()
      )
  ) |>
  select(
    -new_publication_artifact,
    -new_source_artifact
  )

remaining_unbuilt <-
  registry_v1_3 |>
  filter(
    publication_status !=
      "Built"
  )

if (
  nrow(
    remaining_unbuilt
  ) >
    0L
) {
  print(
    remaining_unbuilt,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one registered paper artifact remains unbuilt after R36."
  )
}

missing_built <-
  registry_v1_3 |>
  filter(
    is.na(
      publication_artifact
    ) |
      !file.exists(
        publication_artifact
      )
  )

if (
  nrow(
    missing_built
  ) >
    0L
) {
  print(
    missing_built,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one built publication artifact is missing."
  )
}

write_csv(
  registry_v1_3,
  "config/paper_artifacts_v1_3.csv"
)

status_summary <-
  registry_v1_3 |>
  count(
    placement,
    artifact_type,
    publication_status,
    name =
      "n"
  ) |>
  arrange(
    placement,
    artifact_type
  )

write_csv(
  status_summary,
  file.path(
    output_dir,
    "02_registry_status_after_r36.csv"
  )
)

notes <-
  c(
    "R36 FINAL NES AND INFLUENCE APPENDIX ASSEMBLY",
    "",
    "A11 uses only the audited/current NES descriptive and classification outputs.",
    "Older exploratory respondent specification-curve outputs are not included.",
    "",
    "A12 uses the canonical AC01 leave-one-AC and leave-one-state refits.",
    "FDI support is summarized on the exact canonical AC01 sample.",
    "",
    "No observation is removed or trimmed based on these diagnostics.",
    "No regression is estimated or altered by R36.",
    "",
    "After successful completion, every artifact in paper_artifacts_v1_3.csv is marked Built."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "03_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "04_session_info.txt"
  )
)

cat(
  "\n===== A11 NES TABLE REGISTRY =====\n"
)

print(
  a11_table_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== A11 FIGURE REGISTRY =====\n"
)

print(
  a11_figure_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== A12 INFLUENCE SUMMARY =====\n"
)

print(
  influence_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== A12 FDI SUPPORT SUMMARY =====\n"
)

print(
  support_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== REGISTRY STATUS AFTER R36 =====\n"
)

print(
  status_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nR36_FINAL_NES_AND_INFLUENCE_APPENDIX_COMPLETE\n"
)

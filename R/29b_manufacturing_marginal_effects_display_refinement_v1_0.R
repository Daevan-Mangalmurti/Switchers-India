suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
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

input_dir <-
  file.path(
    project_root,
    "outputs",
    "manufacturing_marginal_effects_v1_0"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "manufacturing_marginal_effects_display_refinement_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

decision_path <-
  file.path(
    project_root,
    "config",
    "paper_display_decisions_v1_0.csv"
  )

one_pp_path <-
  file.path(
    input_dir,
    "01_primary_muslim_effect_1pp_grid.csv"
  )

ten_pp_path <-
  file.path(
    input_dir,
    "02_review_muslim_effect_10pp_grid.csv"
  )

reverse_path <-
  file.path(
    input_dir,
    "03_appendix_reverse_plus1_project_grid.csv"
  )

ac_samples_path <-
  file.path(
    project_root,
    "outputs",
    "ac_canonical_v1_0",
    "model_samples.rds"
  )

voter_samples_path <-
  file.path(
    project_root,
    "outputs",
    "voter_canonical_v1_0",
    "model_samples.rds"
  )

required_files <-
  c(
    decision_path,
    one_pp_path,
    ten_pp_path,
    reverse_path,
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

display_decisions <-
  read_csv(
    decision_path,
    show_col_types = FALSE
  )

one_pp <-
  read_csv(
    one_pp_path,
    show_col_types = FALSE
  )

ten_pp <-
  read_csv(
    ten_pp_path,
    show_col_types = FALSE
  )

reverse_curve <-
  read_csv(
    reverse_path,
    show_col_types = FALSE
  )

ac_samples <-
  readRDS(
    ac_samples_path
  )

voter_samples <-
  readRDS(
    voter_samples_path
  )

if (
  !"AC01" %in%
    names(
      ac_samples
    )
) {
  stop(
    "AC01 sample is missing."
  )
}

if (
  !"V01" %in%
    names(
      voter_samples
    )
) {
  stop(
    "V01 sample is missing."
  )
}

ac_support <-
  ac_samples[[
    "AC01"
  ]] |>
  distinct(
    ac_uid,
    .keep_all = TRUE
  ) |>
  transmute(
    level =
      "AC",

    outcome_label =
      "AC-level centrist BJP share",

    ac_uid =
      as.character(
        ac_uid
      ),

    current_raw =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
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
  distinct(
    ac_uid,
    .keep_all = TRUE
  ) |>
  transmute(
    level =
      "Voter",

    outcome_label =
      "Individual centrist BJP vote",

    ac_uid =
      as.character(
        ac_uid
      ),

    current_raw =
      as.numeric(
        fdi_mfg_current
      ),

    muslim =
      as.numeric(
        muslim
      )
  )

support <-
  bind_rows(
    ac_support,
    voter_support
  )

if (
  any(
    !is.finite(
      support$current_raw
    )
  ) ||
    any(
      support$current_raw <
        0
    )
) {
  stop(
    "Invalid Manufacturing FDI support values."
  )
}

support_summary <-
  support |>
  group_by(
    level,
    outcome_label
  ) |>
  summarise(
    n_ac =
      n(),

    n_zero =
      sum(
        current_raw ==
          0
      ),

    zero_share =
      mean(
        current_raw ==
          0
      ),

    n_positive =
      sum(
        current_raw >
          0
      ),

    positive_share =
      mean(
        current_raw >
          0
      ),

    overall_p90 =
      quantile(
        current_raw,
        .90,
        names = FALSE,
        type = 8
      ),

    overall_p95 =
      quantile(
        current_raw,
        .95,
        names = FALSE,
        type = 8
      ),

    overall_p99 =
      quantile(
        current_raw,
        .99,
        names = FALSE,
        type = 8
      ),

    positive_p25 =
      quantile(
        current_raw[
          current_raw >
            0
        ],
        .25,
        names = FALSE,
        type = 8
      ),

    positive_median =
      median(
        current_raw[
          current_raw >
            0
        ]
      ),

    positive_p75 =
      quantile(
        current_raw[
          current_raw >
            0
        ],
        .75,
        names = FALSE,
        type = 8
      ),

    positive_p90 =
      quantile(
        current_raw[
          current_raw >
            0
        ],
        .90,
        names = FALSE,
        type = 8
      ),

    positive_p95 =
      quantile(
        current_raw[
          current_raw >
            0
        ],
        .95,
        names = FALSE,
        type = 8
      ),

    max_current =
      max(
        current_raw
      ),

    .groups =
      "drop"
  ) |>
  mutate(
    zero_label =
      paste0(
        round(
          100 *
            zero_share,
          1
        ),
        "% of ACs at zero FDI"
      )
  )

add_support_metrics <- function(
  curves,
  support_data
) {
  map_dfr(
    unique(
      curves$level
    ),
    function(
      level_value
    ) {
      dd <-
        curves |>
        filter(
          level ==
            level_value
        )

      x <-
        support_data |>
        filter(
          level ==
            level_value
        ) |>
        pull(
          current_raw
        )

      positive_x <-
        x[
          x >
            0
        ]

      dd |>
        mutate(
          overall_percentile_le =
            vapply(
              current_fdi_raw,
              function(
                value
              ) {
                mean(
                  x <=
                    value
                )
              },
              numeric(1)
            ),

          overall_share_at_or_above =
            vapply(
              current_fdi_raw,
              function(
                value
              ) {
                mean(
                  x >=
                    value
                )
              },
              numeric(1)
            ),

          positive_percentile_le =
            vapply(
              current_fdi_raw,
              function(
                value
              ) {
                if (
                  value <=
                    0 ||
                    length(
                      positive_x
                    ) ==
                      0L
                ) {
                  NA_real_
                } else {
                  mean(
                    positive_x <=
                      value
                  )
                }
              },
              numeric(1)
            ),

          positive_share_at_or_above =
            vapply(
              current_fdi_raw,
              function(
                value
              ) {
                if (
                  value <=
                    0 ||
                    length(
                      positive_x
                    ) ==
                      0L
                ) {
                  NA_real_
                } else {
                  mean(
                    positive_x >=
                      value
                  )
                }
              },
              numeric(1)
            )
        )
    }
  )
}

one_pp_support <-
  add_support_metrics(
    one_pp,
    support
  )

ten_pp_support <-
  add_support_metrics(
    ten_pp,
    support
  )

evaluation_values <-
  bind_rows(
    support |>
      group_by(
        level,
        outcome_label
      ) |>
      summarise(
        evaluation_label =
          "Zero FDI",
        current_fdi_raw =
          0,
        .groups =
          "drop"
      ),

    support |>
      group_by(
        level,
        outcome_label
      ) |>
      summarise(
        evaluation_label =
          "90th percentile overall",
        current_fdi_raw =
          quantile(
            current_raw,
            .90,
            names = FALSE,
            type = 8
          ),
        .groups =
          "drop"
      ),

    support |>
      group_by(
        level,
        outcome_label
      ) |>
      summarise(
        evaluation_label =
          "95th percentile overall",
        current_fdi_raw =
          quantile(
            current_raw,
            .95,
            names = FALSE,
            type = 8
          ),
        .groups =
          "drop"
      ),

    support |>
      filter(
        current_raw >
          0
      ) |>
      group_by(
        level,
        outcome_label
      ) |>
      summarise(
        evaluation_label =
          "Median among positive-FDI ACs",
        current_fdi_raw =
          median(
            current_raw
          ),
        .groups =
          "drop"
      )
  )

support_at_key_values <-
  map_dfr(
    seq_len(
      nrow(
        evaluation_values
      )
    ),
    function(
      i
    ) {
      row <-
        evaluation_values[
          i,
          ,
          drop = FALSE
        ]

      x <-
        support |>
        filter(
          level ==
            row$level
        ) |>
        pull(
          current_raw
        )

      positive_x <-
        x[
          x >
            0
        ]

      value <-
        row$current_fdi_raw

      tibble(
        level =
          row$level,

        outcome_label =
          row$outcome_label,

        evaluation_label =
          row$evaluation_label,

        current_fdi_raw =
          value,

        overall_percentile_le =
          mean(
            x <=
              value
          ),

        overall_share_at_or_above =
          mean(
            x >=
              value
          ),

        positive_percentile_le =
          if (
            value >
              0
          ) {
            mean(
              positive_x <=
                value
            )
          } else {
            NA_real_
          },

        positive_share_at_or_above =
          if (
            value >
              0
          ) {
            mean(
              positive_x >=
                value
            )
          } else {
            NA_real_
          },

        n_ac_at_or_above =
          sum(
            x >=
              value
          )
      )
    }
  )

rug_data <-
  support |>
  select(
    level,
    outcome_label,
    current_fdi_raw =
      current_raw
  )

support_lines <-
  support_summary |>
  select(
    level,
    outcome_label,
    overall_p90,
    overall_p95,
    zero_label
  )

plot_primary_form <- function(
  curve_data,
  functional_form_value,
  muslim_change_pp,
  title_text,
  subtitle_text
) {
  plot_data <-
    curve_data |>
    filter(
      functional_form ==
        functional_form_value
    )

  ggplot(
    plot_data,
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
    geom_vline(
      data =
        support_lines,
      aes(
        xintercept =
          overall_p90
      ),
      inherit.aes =
        FALSE,
      linetype =
        "dotted",
      linewidth =
        0.4
    ) +
    geom_vline(
      data =
        support_lines,
      aes(
        xintercept =
          overall_p95
      ),
      inherit.aes =
        FALSE,
      linetype =
        "dotdash",
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
        rug_data,
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
    geom_text(
      data =
        support_lines,
      aes(
        x =
          Inf,
        y =
          Inf,
        label =
          zero_label
      ),
      inherit.aes =
        FALSE,
      hjust =
        1.05,
      vjust =
        1.5,
      size =
        3.1
    ) +
    facet_wrap(
      vars(
        outcome_label
      ),
      nrow =
        1
    ) +
    labs(
      title =
        title_text,

      subtitle =
        subtitle_text,

      x =
        "Manufacturing FDI projects per 100,000 residents, 2009-2014",

      y =
        paste0(
          "Change in BJP support (percentage points)\nfor +",
          muslim_change_pp,
          " pp Muslim population share"
        ),

      caption =
        paste0(
          "Fully adjusted Manufacturing models. ",
          "Baseline Manufacturing FDI is averaged over the estimation sample. ",
          "Shaded regions are 95% confidence intervals. ",
          "Rugs show observed constituency-level FDI support. ",
          "Dotted and dot-dash vertical lines mark the 90th and 95th percentiles of current Manufacturing FDI, respectively."
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

main_raw_1pp <-
  plot_primary_form(
    one_pp_support,
    functional_form_value =
      "Raw",
    muslim_change_pp =
      1,
    title_text =
      "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Main Figure 5: raw Manufacturing FDI"
  )

appendix_log_1pp <-
  plot_primary_form(
    one_pp_support,
    functional_form_value =
      "log1p",
    muslim_change_pp =
      1,
    title_text =
      "Logged Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Appendix robustness figure: log1p Manufacturing FDI"
  )

review_raw_10pp <-
  plot_primary_form(
    ten_pp_support,
    functional_form_value =
      "Raw",
    muslim_change_pp =
      10,
    title_text =
      "Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Internal review alternative: 10-percentage-point Muslim-share scale"
  )

review_log_10pp <-
  plot_primary_form(
    ten_pp_support,
    functional_form_value =
      "log1p",
    muslim_change_pp =
      10,
    title_text =
      "Logged Manufacturing FDI and the Muslim-share gradient in centrist BJP support",
    subtitle_text =
      "Internal review alternative: log1p model on 10-percentage-point Muslim-share scale"
  )

write_csv(
  display_decisions,
  file.path(
    output_dir,
    "00_display_placement_registry.csv"
  )
)

write_csv(
  support_summary,
  file.path(
    output_dir,
    "01_support_summary_positive_and_zero_mass.csv"
  )
)

write_csv(
  one_pp_support,
  file.path(
    output_dir,
    "02_primary_1pp_curve_with_support_metrics.csv"
  )
)

write_csv(
  ten_pp_support,
  file.path(
    output_dir,
    "03_review_10pp_curve_with_support_metrics.csv"
  )
)

write_csv(
  support_at_key_values,
  file.path(
    output_dir,
    "04_support_at_key_fdi_values.csv"
  )
)

write_csv(
  reverse_curve,
  file.path(
    output_dir,
    "05_appendix_reverse_curve_reference.csv"
  )
)

ggsave(
  filename =
    file.path(
      output_dir,
      "07_main_figure_manufacturing_raw_1pp.pdf"
    ),
  plot =
    main_raw_1pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "07_main_figure_manufacturing_raw_1pp.png"
    ),
  plot =
    main_raw_1pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "08_appendix_figure_manufacturing_log1p_1pp.pdf"
    ),
  plot =
    appendix_log_1pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "08_appendix_figure_manufacturing_log1p_1pp.png"
    ),
  plot =
    appendix_log_1pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "09_review_manufacturing_raw_10pp.pdf"
    ),
  plot =
    review_raw_10pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "09_review_manufacturing_raw_10pp.png"
    ),
  plot =
    review_raw_10pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in",
  dpi =
    300
)

ggsave(
  filename =
    file.path(
      output_dir,
      "10_review_manufacturing_log1p_10pp.pdf"
    ),
  plot =
    review_log_10pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "10_review_manufacturing_log1p_10pp.png"
    ),
  plot =
    review_log_10pp,
  width =
    8.5,
  height =
    4.8,
  units =
    "in",
  dpi =
    300
)

notes <-
  c(
    "R29b MANUFACTURING MARGINAL-EFFECT DISPLAY REFINEMENT",
    "",
    "No model is re-estimated in this script.",
    "All marginal-effect values are read from the completed R29 calculations.",
    "",
    "PUBLICATION PLACEMENT DECISION",
    "Main Figure 5 contains RAW Manufacturing FDI only, with two panels: AC and voter.",
    "The log1p Manufacturing marginal-effect figure is appendix-only.",
    "The 10-percentage-point versions are internal review alternatives and are not default paper figures.",
    "The reverse derivative remains appendix-only.",
    "",
    "The previous combined four-panel raw + log1p main-candidate figure is superseded and should not be used in the manuscript.",
    "",
    "SUPPORT REFINEMENT",
    "The figure annotates the percentage of constituency observations at zero Manufacturing FDI.",
    "Vertical reference lines identify the 90th and 95th percentiles of observed current Manufacturing FDI.",
    "Machine-readable curve outputs report the overall percentile, share at or above each FDI value, and percentile/share among positive-FDI constituencies.",
    "",
    "No observations are dropped or trimmed.",
    "The support calculations are descriptive only and do not alter estimation."
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
  "\n===== DISPLAY PLACEMENT REGISTRY =====\n"
)

print(
  display_decisions,
  n = Inf,
  width = Inf
)

cat(
  "\n===== REFINED SUPPORT SUMMARY =====\n"
)

print(
  support_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== SUPPORT AT KEY FDI VALUES =====\n"
)

print(
  support_at_key_values,
  n = Inf,
  width = Inf
)

cat(
  "\nR29B_MANUFACTURING_DISPLAY_REFINEMENT_COMPLETE\n"
)

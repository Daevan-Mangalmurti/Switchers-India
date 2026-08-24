suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
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
    "r30_core_specification_curve_v1_0"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r30_specification_curve_display_refinement_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

results_path <-
  file.path(
    input_dir,
    "01_core_specification_results.csv"
  )

display_registry_path <-
  file.path(
    project_root,
    "config",
    "paper_display_decisions_v1_0.csv"
  )

if (
  !file.exists(
    results_path
  )
) {
  stop(
    "R30c results are missing."
  )
}

if (
  !file.exists(
    display_registry_path
  )
) {
  stop(
    "Paper display registry is missing."
  )
}

results <-
  read_csv(
    results_path,
    show_col_types = FALSE
  )

display_registry <-
  read_csv(
    display_registry_path,
    show_col_types = FALSE
  )

if (
  nrow(
    results
  ) !=
    40L
) {
  stop(
    "Expected exactly 40 R30c results."
  )
}

normalization_keys <-
  c(
    "level",
    "sector",
    "family",
    "geography",
    "functional_form"
  )

reference_sd <-
  results |>
  filter(
    control_set ==
      "Primary"
  ) |>
  select(
    all_of(
      normalization_keys
    ),
    reference_fdi_sd =
      current_fdi_sd,
    reference_n =
      n,
    reference_n_ac =
      n_ac
  )

if (
  anyDuplicated(
    reference_sd[
      normalization_keys
    ]
  ) >
    0L
) {
  stop(
    "Primary-reference SD table is not unique by FDI definition."
  )
}

figure_data <-
  results |>
  select(
    -starts_with(
      "standardized_"
    )
  ) |>
  left_join(
    reference_sd,
    by =
      normalization_keys,
    relationship =
      "many-to-one"
  ) |>
  mutate(
    reference_standardized_estimate_pp =
      estimate *
        reference_fdi_sd,

    reference_standardized_se_pp =
      std_error *
        reference_fdi_sd,

    reference_standardized_conf_low_pp =
      conf_low *
        reference_fdi_sd,

    reference_standardized_conf_high_pp =
      conf_high *
        reference_fdi_sd
  )

if (
  any(
    !is.finite(
      figure_data$reference_fdi_sd
    )
  ) ||
    any(
      figure_data$reference_fdi_sd <=
        0
    )
) {
  stop(
    "Invalid primary-sample reference SD."
  )
}

primary_check <-
  results |>
  filter(
    control_set ==
      "Primary"
  ) |>
  select(
    spec_id,
    old_standardized_estimate =
      standardized_estimate_pp
  ) |>
  left_join(
    figure_data |>
      filter(
        control_set ==
          "Primary"
      ) |>
      select(
        spec_id,
        new_standardized_estimate =
          reference_standardized_estimate_pp
      ),
    by =
      "spec_id",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    absolute_difference =
      abs(
        old_standardized_estimate -
          new_standardized_estimate
      )
  )

if (
  any(
    primary_check$absolute_difference >
      1e-10
  )
) {
  print(
    primary_check,
    n = Inf,
    width = Inf
  )

  stop(
    "Primary models changed under reference-SD normalization."
  )
}

expanded_normalization_comparison <-
  results |>
  filter(
    control_set ==
      "Expanded"
  ) |>
  select(
    spec_id,
    level,
    sector,
    family,
    geography,
    functional_form,
    expanded_native_sd =
      current_fdi_sd,
    old_within_model_standardized_estimate =
      standardized_estimate_pp
  ) |>
  left_join(
    figure_data |>
      filter(
        control_set ==
          "Expanded"
      ) |>
      select(
        spec_id,
        primary_reference_sd =
          reference_fdi_sd,
        new_reference_standardized_estimate =
          reference_standardized_estimate_pp
      ),
    by =
      "spec_id",
    relationship =
      "one-to-one"
  )

plot_labels <-
  tribble(
    ~family, ~geography, ~functional_form, ~spec_label, ~order,

    "60-month current + baseline",
    "Local",
    "Raw",
    "60m local raw",
    1L,

    "60-month current + baseline",
    "Local",
    "log1p",
    "60m local log1p",
    2L,

    "60-month current + baseline",
    "Own AC",
    "Raw",
    "60m own-AC raw",
    3L,

    "21-month change + early baseline",
    "Local",
    "Raw",
    "21m change + baseline",
    4L,

    "12-month change + early baseline",
    "Local",
    "Raw",
    "12m change + baseline\n(post-estimation)",
    5L
  )

figure_data <-
  figure_data |>
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
  stop(
    "At least one specification lacks a display label."
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

    sector =
      factor(
        sector,
        levels =
          c(
            "Total",
            "Manufacturing"
          )
      ),

    control_display =
      case_when(
        level ==
          "AC" &
          control_set ==
            "Primary" ~
          "Primary controls (N=224)",

        level ==
          "AC" &
          control_set ==
            "Expanded" ~
          "Expanded controls (N=154)",

        level ==
          "Voter" &
          control_set ==
            "Primary" ~
          "Primary controls (N=1,763)",

        level ==
          "Voter" &
          control_set ==
            "Expanded" ~
          "Expanded controls (N=1,325)",

        TRUE ~
          NA_character_
      )
  )

if (
  any(
    is.na(
      figure_data$control_display
    )
  )
) {
  stop(
    "Control-display labeling failed."
  )
}

make_plot <- function(
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
        reference_standardized_estimate_pp,
      shape =
        control_display
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
          reference_standardized_conf_low_pp,
        ymax =
          reference_standardized_conf_high_pp
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
        2.5
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
          "for a 1-SD increase in FDI exposure"
        ),

      shape =
        "Specification",

      caption =
        paste0(
          "Points show the FDI x 2001 Muslim-share interaction normalized using the SD of each FDI definition in its primary-control estimation sample. ",
          "The same reference SD is therefore applied to both the Primary and Expanded estimates for a given FDI definition. ",
          "Intervals are 95% CIs. Expanded-control estimates use the smaller complete-case sample required by employment and secondary-education controls. ",
          "The 12-month specification is a post-estimation temporal-window robustness check."
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

main_ac_figure <-
  make_plot(
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

appendix_voter_figure <-
  make_plot(
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

new_display_rows <-
  tribble(
    ~decision_id, ~placement, ~publication_status, ~artifact_title, ~functional_form, ~muslim_change_pp, ~estimand, ~generating_script, ~output_stub, ~notes,

    "R30-AC-ROBUSTNESS",
    "Main",
    "Main Figure 6",
    "Robustness of the FDI x Muslim-share relationship",
    "Mixed registered FDI scales; primary-reference-SD normalized",
    1,
    "Change in the +1-pp Muslim-share BJP-support gradient associated with a one-SD increase in the registered FDI exposure",
    "R/30d_refine_specification_curve_display_v1_0.R",
    "11_main_figure_6_ac_specification_robustness_final",
    "AC robustness figure; Total and Manufacturing; Primary and Expanded control sets; 12m row explicitly post-estimation",

    "R30-VOTER-ROBUSTNESS",
    "Appendix",
    "Appendix figure",
    "Voter-level robustness of the FDI x Muslim-share relationship",
    "Mixed registered FDI scales; primary-reference-SD normalized",
    1,
    "Change in the +1-pp Muslim-share BJP-support gradient associated with a one-SD increase in the registered FDI exposure",
    "R/30d_refine_specification_curve_display_v1_0.R",
    "12_appendix_voter_specification_robustness_final",
    "Parallel voter robustness figure; appendix only"
  )

display_registry_updated <-
  display_registry |>
  filter(
    !decision_id %in%
      new_display_rows$decision_id
  ) |>
  bind_rows(
    new_display_rows
  )

write_csv(
  display_registry_updated,
  display_registry_path
)

write_csv(
  figure_data |>
    mutate(
      spec_label =
        as.character(
          spec_label
        ),
      sector =
        as.character(
          sector
        )
    ),
  file.path(
    output_dir,
    "01_final_figure_data_reference_sd.csv"
  )
)

write_csv(
  primary_check,
  file.path(
    output_dir,
    "02_primary_normalization_reproduction.csv"
  )
)

write_csv(
  expanded_normalization_comparison,
  file.path(
    output_dir,
    "03_expanded_normalization_comparison.csv"
  )
)

write_csv(
  display_registry_updated,
  file.path(
    output_dir,
    "04_updated_paper_display_registry.csv"
  )
)

ggsave(
  filename =
    file.path(
      output_dir,
      "11_main_figure_6_ac_specification_robustness_final.pdf"
    ),
  plot =
    main_ac_figure,
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
      "11_main_figure_6_ac_specification_robustness_final.png"
    ),
  plot =
    main_ac_figure,
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
      "12_appendix_voter_specification_robustness_final.pdf"
    ),
  plot =
    appendix_voter_figure,
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
      "12_appendix_voter_specification_robustness_final.png"
    ),
  plot =
    appendix_voter_figure,
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
    "R30d SPECIFICATION-CURVE DISPLAY REFINEMENT",
    "",
    "No regression model is re-estimated.",
    "",
    "R30c normalized each coefficient using the FDI SD within that model's own estimation sample.",
    "Because Expanded-control models use smaller complete-case samples, that allowed both the coefficient and the normalizing SD to change.",
    "",
    "R30d instead defines one reference SD per FDI definition using the Primary-control sample.",
    "That same reference SD is used to scale both the Primary and Expanded estimates for that FDI definition.",
    "",
    "Primary estimates therefore reproduce the original R30c standardized values exactly.",
    "",
    "The figure explicitly reports that Expanded-control models use smaller complete-case samples.",
    "",
    "Main Figure 6 is the AC robustness figure.",
    "The parallel voter robustness figure is appendix-only.",
    "",
    "The R30c first-pass plots are retained for provenance but are superseded for publication display by the R30d figures."
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
  "\n===== PRIMARY NORMALIZATION REPRODUCTION =====\n"
)

print(
  primary_check,
  n = Inf,
  width = Inf
)

cat(
  "\n===== EXPANDED NORMALIZATION COMPARISON =====\n"
)

print(
  expanded_normalization_comparison,
  n = Inf,
  width = Inf
)

cat(
  "\n===== UPDATED PAPER DISPLAY REGISTRY =====\n"
)

print(
  display_registry_updated,
  n = Inf,
  width = Inf
)

cat(
  "\nR30D_SPECIFICATION_CURVE_DISPLAY_REFINEMENT_COMPLETE\n"
)

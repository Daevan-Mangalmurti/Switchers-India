suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(stringr)
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
    "r31_demographic_context_robustness_v1_0"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_demographic_context_display_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

results <-
  read_csv(
    file.path(
      input_dir,
      "01_demographic_context_model_results.csv"
    ),
    show_col_types = FALSE
  )

registry <-
  read_csv(
    "config/r31_demographic_context_registry_v1_0.csv",
    show_col_types = FALSE
  )

if (
  nrow(
    results
  ) !=
    32L
) {
  stop(
    "Expected exactly 32 R31 result rows."
  )
}

if (
  !setequal(
    unique(
      results$moderator_id
    ),
    sprintf(
      "D%02d",
      1:8
    )
  )
) {
  stop(
    "R31 moderator IDs are not D01-D08."
  )
}

if (
  !setequal(
    unique(
      results$control_set
    ),
    c(
      "Primary",
      "Expanded"
    )
  )
) {
  stop(
    "R31 control sets are not exactly Primary and Expanded."
  )
}

timing_registry <-
  tribble(
    ~moderator_id, ~timing_class, ~display_label, ~interpretation_status,

    "D01",
    "Baseline/context level",
    "Muslim share, 2001",
    "Predetermined demographic-context anchor",

    "D02",
    "2001-2011 change",
    "Change in Muslim share",
    "Post-baseline contextual-change robustness; overlaps current FDI period",

    "D03",
    "Baseline/context level",
    "Established migrant stock by 2001",
    "Retrospective baseline proxy constructed from 2011 duration-of-residence data",

    "D04",
    "2001-2011 change",
    "Change in migrant-stock share",
    "Post-baseline contextual-change robustness; overlaps current FDI period",

    "D05",
    "Baseline/context level",
    "Established male migrant stock by 2001",
    "Retrospective baseline proxy constructed from 2011 duration-of-residence data",

    "D06",
    "2001-2011 change",
    "Change in male migrant-stock share",
    "Post-baseline contextual-change robustness; overlaps current FDI period",

    "D07",
    "Baseline/context level",
    "Bengali/Bhojpuri share, 2001",
    "Predetermined demographic-context alternative",

    "D08",
    "2001-2011 change",
    "Change in Bengali/Bhojpuri share",
    "Post-baseline contextual-change robustness; overlaps current FDI period"
  )

if (
  !setequal(
    timing_registry$moderator_id,
    registry$moderator_id
  )
) {
  stop(
    "Timing registry and frozen moderator registry disagree."
  )
}

primary_reference <-
  results |>
  filter(
    control_set ==
      "Primary"
  ) |>
  select(
    level,
    moderator_id,
    primary_moderator_sd =
      moderator_sd
  )

if (
  nrow(
    primary_reference
  ) !=
    16L
) {
  stop(
    "Expected 16 level-by-moderator Primary reference rows."
  )
}

canonical_fdi_reference <-
  results |>
  filter(
    moderator_id ==
      "D01",
    control_set ==
      "Primary"
  ) |>
  select(
    level,
    canonical_fdi_sd =
      current_fdi_sd
  )

if (
  nrow(
    canonical_fdi_reference
  ) !=
    2L
) {
  stop(
    "Could not identify the two D01 Primary FDI reference SDs."
  )
}

display_data <-
  results |>
  left_join(
    primary_reference,
    by =
      c(
        "level",
        "moderator_id"
      ),
    relationship =
      "many-to-one"
  ) |>
  left_join(
    canonical_fdi_reference,
    by =
      "level",
    relationship =
      "many-to-one"
  ) |>
  left_join(
    timing_registry,
    by =
      "moderator_id",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    reference_standardized_interaction_pp =
      100 *
      estimate *
      primary_moderator_sd *
      canonical_fdi_sd,

    reference_standardized_conf_low_pp =
      100 *
      (
        estimate -
          1.96 *
          std_error
      ) *
      primary_moderator_sd *
      canonical_fdi_sd,

    reference_standardized_conf_high_pp =
      100 *
      (
        estimate +
          1.96 *
          std_error
      ) *
      primary_moderator_sd *
      canonical_fdi_sd
  )

if (
  any(
    !is.finite(
      display_data$reference_standardized_interaction_pp
    )
  )
) {
  stop(
    "Non-finite reference-standardized R31 estimate."
  )
}

d01_reproduction <-
  display_data |>
  filter(
    moderator_id ==
      "D01",
    control_set ==
      "Primary"
  ) |>
  transmute(
    level,
    old_standardized_interaction_pp =
      standardized_interaction_pp,
    new_reference_standardized_interaction_pp =
      reference_standardized_interaction_pp,
    absolute_difference =
      abs(
        old_standardized_interaction_pp -
          new_reference_standardized_interaction_pp
      )
  )

if (
  any(
    d01_reproduction$absolute_difference >
      1e-10
  )
) {
  print(
    d01_reproduction,
    n = Inf,
    width = Inf
  )

  stop(
    "D01 Primary standardized reference does not reproduce itself."
  )
}

ordering <-
  rev(
    c(
      "Muslim share, 2001",
      "Established migrant stock by 2001",
      "Established male migrant stock by 2001",
      "Bengali/Bhojpuri share, 2001",
      "Change in Muslim share",
      "Change in migrant-stock share",
      "Change in male migrant-stock share",
      "Change in Bengali/Bhojpuri share"
    )
  )

figure_data <-
  display_data |>
  mutate(
    display_label =
      factor(
        display_label,
        levels =
          ordering
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

    level =
      factor(
        level,
        levels =
          c(
            "AC",
            "Voter"
          ),
        labels =
          c(
            "Assembly-constituency models",
            "Voter-level models"
          )
      )
  )

p <-
  ggplot(
    figure_data,
    aes(
      x =
        reference_standardized_interaction_pp,
      y =
        display_label,
      shape =
        control_set
    )
  ) +
  geom_vline(
    xintercept =
      0,
    linewidth =
      0.4,
    linetype =
      "dashed"
  ) +
  geom_errorbar(
    aes(
      xmin =
        reference_standardized_conf_low_pp,
      xmax =
        reference_standardized_conf_high_pp
    ),
    width =
      0,
    position =
      position_dodge(
        width =
          0.55
      )
  ) +
  geom_point(
    size =
      2.4,
    position =
      position_dodge(
        width =
          0.55
      )
  ) +
  facet_wrap(
    vars(
      level
    ),
    ncol =
      1,
    scales =
      "free_x"
  ) +
  labs(
    x =
      "Reference-standardized FDI × demographic-context interaction (BJP-support percentage points)",
    y =
      NULL,
    shape =
      "Controls",
    title =
      "Demographic-context robustness of the FDI interaction",
    subtitle =
      "Total local FDI; current 2009-2014 exposure with 2004-2009 baseline exposure",
    caption =
      paste0(
        "Points show interaction coefficients rescaled using each moderator's Primary-sample SD ",
        "and the canonical D01 Primary-sample SD of current FDI within analysis level. ",
        "Intervals are 95% confidence intervals. Change measures span 2001-2011 and overlap ",
        "part of the current FDI period; they are post-baseline contextual robustness checks, ",
        "not predetermined causal moderators."
      )
  ) +
  theme_minimal(
    base_size =
      11
  ) +
  theme(
    legend.position =
      "bottom",
    panel.grid.minor =
      element_blank(),
    strip.text =
      element_text(
        face =
          "bold"
      )
  )

ggsave(
  filename =
    file.path(
      output_dir,
      "01_appendix_demographic_context_robustness.pdf"
    ),
  plot =
    p,
  width =
    8.2,
  height =
    9.0,
  units =
    "in"
)

ggsave(
  filename =
    file.path(
      output_dir,
      "01_appendix_demographic_context_robustness.png"
    ),
  plot =
    p,
  width =
    8.2,
  height =
    9.0,
  units =
    "in",
  dpi =
    300
)

native_table <-
  display_data |>
  select(
    moderator_id,
    preferred_label,
    timing_class,
    interpretation_status,
    level,
    control_set,
    estimate,
    std_error,
    p_value,
    n,
    n_ac,
    n_states,
    n_clusters
  ) |>
  arrange(
    moderator_id,
    factor(
      level,
      levels =
        c(
          "AC",
          "Voter"
        )
    ),
    factor(
      control_set,
      levels =
        c(
          "Primary",
          "Expanded"
        )
    )
  )

write_csv(
  native_table,
  file.path(
    output_dir,
    "02_appendix_native_estimates.csv"
  )
)

write_csv(
  figure_data |>
    select(
      spec_id,
      level,
      moderator_id,
      display_label,
      timing_class,
      interpretation_status,
      control_set,
      estimate,
      std_error,
      p_value,
      primary_moderator_sd,
      canonical_fdi_sd,
      reference_standardized_interaction_pp,
      reference_standardized_conf_low_pp,
      reference_standardized_conf_high_pp,
      n,
      n_ac
    ),
  file.path(
    output_dir,
    "03_appendix_figure_data.csv"
  )
)

write_csv(
  timing_registry,
  file.path(
    output_dir,
    "04_timing_and_interpretation_registry.csv"
  )
)

write_csv(
  d01_reproduction,
  file.path(
    output_dir,
    "05_d01_standardization_reproduction.csv"
  )
)

notes <-
  c(
    "R31c DEMOGRAPHIC-CONTEXT DISPLAY REFINEMENT",
    "",
    "Placement: appendix only.",
    "",
    "Native coefficients, standard errors, p-values, and sample sizes remain the inferential results.",
    "",
    "The figure uses a reference-standardized interaction solely to compare moderators measured in different units.",
    "",
    "For each moderator, its Primary-sample moderator SD is used for both Primary and Expanded models.",
    "",
    "Within each analysis level, current-FDI scaling is fixed to the D01 Primary-sample FDI SD for every moderator and both control sets.",
    "",
    "This prevents control-set comparisons from being mechanically altered by different Expanded-sample SDs.",
    "",
    "D01, D03, D05, and D07 are baseline/context-level measures.",
    "",
    "D02, D04, D06, and D08 measure 2001-2011 change and overlap part of the 2009-2014 FDI exposure window. They are descriptive/post-baseline contextual-change robustness checks rather than predetermined causal moderators.",
    "",
    "No moderator is promoted or dropped based on statistical significance."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "06_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "07_session_info.txt"
  )
)

cat(
  "\n===== D01 REFERENCE STANDARDIZATION REPRODUCTION =====\n"
)

print(
  d01_reproduction,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FINAL R31 APPENDIX DISPLAY DATA =====\n"
)

print(
  figure_data |>
    select(
      level,
      moderator_id,
      display_label,
      timing_class,
      control_set,
      reference_standardized_interaction_pp,
      reference_standardized_conf_low_pp,
      reference_standardized_conf_high_pp,
      n,
      n_ac
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nR31C_DEMOGRAPHIC_CONTEXT_DISPLAY_COMPLETE\n"
)

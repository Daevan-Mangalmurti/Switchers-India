suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
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

main_wvs_dir <-
  file.path(
    "out",
    "aid_lmic",
    "ideology_lt5_vs_5_6_vs_gt6"
  )

appendix_wvs_dir <-
  file.path(
    "out",
    "aid_lmic",
    "ideology_1_2_vs_5_6_vs_9_10"
  )

nes_dir <-
  file.path(
    "outputs",
    "nes_2009_2014_ideology_audit_v1_1"
  )

output_dir <-
  file.path(
    "outputs",
    "paper_descriptive_figures_v1_0"
  )

main_output_dir <-
  file.path(
    output_dir,
    "main"
  )

appendix_output_dir <-
  file.path(
    output_dir,
    "appendix"
  )

data_output_dir <-
  file.path(
    output_dir,
    "figure_data"
  )

dir.create(
  main_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  appendix_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  data_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

support_measure_note <-
  paste0(
    "Far-right support is measured from respondents' party-choice or ",
    "party-appeal responses: E179_WVS in WVS; E179 in EVS waves 1-4, ",
    "with E181/E181A used as fallbacks where applicable; EVS wave 5 ",
    "uses E181A."
  )

ideology_levels <-
  c(
    "Left",
    "Moderate",
    "Right"
  )

ideology_colors <-
  c(
    "Left" =
      "#0000FF",
    "Moderate" =
      "#FFD700",
    "Right" =
      "#FF0000"
  )

ideology_linetypes <-
  c(
    "Left" =
      "solid",
    "Moderate" =
      "dashed",
    "Right" =
      "solid"
  )

survey_shapes <-
  c(
    "WVS" =
      16,
    "EVS" =
      17
  )

paper_theme <-
  theme_minimal(
    base_size =
      11
  ) +
  theme(
    panel.grid.minor =
      element_blank(),
    legend.position =
      "bottom",
    legend.box =
      "vertical",
    plot.title =
      element_text(
        face =
          "bold",
        size =
          13
      ),
    strip.text =
      element_text(
        face =
          "bold"
      ),
    plot.caption =
      element_text(
        size =
          8.5,
        hjust =
          0
      )
  )

save_plot <- function(
  plot,
  stem,
  width,
  height
) {
  png_path <-
    paste0(
      stem,
      ".png"
    )

  pdf_path <-
    paste0(
      stem,
      ".pdf"
    )

  ggsave(
    filename =
      png_path,
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
    filename =
      pdf_path,
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

  c(
    png_path,
    pdf_path
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
      "Required source file is missing: ",
      path
    )
  }

  path
}

main_fig1_path <-
  required_file(
    file.path(
      main_wvs_dir,
      "csv",
      "far_right_support_by_aid_lmic_and_survey_wave_plot_data.csv"
    )
  )

main_fig2_path <-
  required_file(
    file.path(
      main_wvs_dir,
      "csv",
      "country_fe_lpm_adjusted_probabilities_by_ideology.csv"
    )
  )

main_fig2_contrast_path <-
  required_file(
    file.path(
      main_wvs_dir,
      "csv",
      "country_fe_lpm_ideology_contrasts.csv"
    )
  )

main_fig4_path <-
  required_file(
    file.path(
      main_wvs_dir,
      "csv",
      "country_survey_far_right_share_by_ideology.csv"
    )
  )

appendix_fig1_path <-
  required_file(
    file.path(
      appendix_wvs_dir,
      "csv",
      "far_right_support_by_aid_lmic_and_survey_wave_plot_data.csv"
    )
  )

appendix_fig2_path <-
  required_file(
    file.path(
      appendix_wvs_dir,
      "csv",
      "country_fe_lpm_adjusted_probabilities_by_ideology.csv"
    )
  )

appendix_fig2_contrast_path <-
  required_file(
    file.path(
      appendix_wvs_dir,
      "csv",
      "country_fe_lpm_ideology_contrasts.csv"
    )
  )

appendix_fig4_path <-
  required_file(
    file.path(
      appendix_wvs_dir,
      "csv",
      "country_survey_far_right_share_by_ideology.csv"
    )
  )

nes_fig3_path <-
  required_file(
    file.path(
      nes_dir,
      "11_plot_data_left_center_right.csv"
    )
  )

nes_notes_path <-
  required_file(
    file.path(
      nes_dir,
      "08_figure_notes.txt"
    )
  )

build_wave_figure <- function(
  source_path,
  ideology_definition,
  title
) {
  x <-
    read_csv(
      source_path,
      show_col_types = FALSE
    ) |>
    mutate(
      ideology_category =
        factor(
          as.character(
            ideology_category
          ),
          levels =
            ideology_levels
        ),

      display_group =
        case_when(
          analysis_group ==
            "Advanced Industrialized Democracies" ~
            "Advanced Industrialized Democracies (AIDs)",

          analysis_group ==
            "Low & Middle-Income Countries" ~
            "Low & Middle-Income Countries (LMICs)",

          TRUE ~
            analysis_group
        ),

      display_group =
        factor(
          display_group,
          levels =
            c(
              "Advanced Industrialized Democracies (AIDs)",
              "Low & Middle-Income Countries (LMICs)"
            )
        )
    )

  required <-
    c(
      "analysis_group",
      "party_source",
      "survey_wave",
      "wave_calendar_year",
      "ideology_category",
      "pct_far_right",
      "n_countries"
    )

  missing <-
    setdiff(
      required,
      names(
        x
      )
    )

  if (
    length(
      missing
    ) >
      0L
  ) {
    stop(
      "Wave figure source missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  p <-
    ggplot(
      x,
      aes(
        x =
          wave_calendar_year,
        y =
          pct_far_right,
        color =
          ideology_category,
        linetype =
          ideology_category,
        shape =
          party_source,
        group =
          ideology_category
      )
    ) +
    geom_line(
      linewidth =
        0.95
    ) +
    geom_point(
      size =
        2.8
    ) +
    facet_wrap(
      vars(
        display_group
      ),
      nrow =
        1
    ) +
    scale_color_manual(
      values =
        ideology_colors,
      breaks =
        ideology_levels,
      drop =
        FALSE,
      name =
        "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values =
        ideology_linetypes,
      breaks =
        ideology_levels,
      drop =
        FALSE,
      name =
        "Ideological self-placement"
    ) +
    scale_shape_manual(
      values =
        survey_shapes,
      name =
        "Survey program"
    ) +
    scale_y_continuous(
      breaks =
        seq(
          0,
          40,
          by =
            10
        ),
      labels =
        function(z) {
          paste0(
            z,
            "%"
          )
        }
    ) +
    scale_x_continuous(
      breaks =
        scales::breaks_pretty(
          n =
            7
        )
    ) +
    coord_cartesian(
      ylim =
        c(
          0,
          40
        )
    ) +
    labs(
      title =
        title,
      subtitle =
        ideology_definition,
      x =
        "Calendar year",
      y =
        "Far-right vote share",
      caption =
        paste0(
          "Each point is an equal-country survey-wave mean. ",
          "Survey weights are applied within country-surveys; countries are ",
          "then weighted equally within displayed survey waves. AIDs include ",
          "WVS and EVS; LMICs include WVS only. ",
          support_measure_note
        )
    ) +
    paper_theme

  list(
    data =
      x,
    plot =
      p
  )
}

build_lpm_figure <- function(
  probability_path,
  contrast_path,
  ideology_definition,
  title
) {
  probability <-
    read_csv(
      probability_path,
      show_col_types = FALSE
    ) |>
    mutate(
      ideology_category =
        factor(
          as.character(
            ideology_category
          ),
          levels =
            ideology_levels
        ),

      x_position =
        match(
          as.character(
            ideology_category
          ),
          ideology_levels
        )
    ) |>
    arrange(
      x_position
    )

  contrasts <-
    read_csv(
      contrast_path,
      show_col_types = FALSE
    )

  required_probability <-
    c(
      "ideology_category",
      "estimate_pct",
      "conf_low_pct",
      "conf_high_pct",
      "n_respondents",
      "n_countries"
    )

  missing_probability <-
    setdiff(
      required_probability,
      names(
        probability
      )
    )

  if (
    length(
      missing_probability
    ) >
      0L
  ) {
    stop(
      "LPM prediction source missing: ",
      paste(
        missing_probability,
        collapse = ", "
      )
    )
  }

  required_contrasts <-
    c(
      "comparison",
      "estimate_pp",
      "conf_low_pp",
      "conf_high_pp"
    )

  missing_contrasts <-
    setdiff(
      required_contrasts,
      names(
        contrasts
      )
    )

  if (
    length(
      missing_contrasts
    ) >
      0L
  ) {
    stop(
      "LPM contrast source missing: ",
      paste(
        missing_contrasts,
        collapse = ", "
      )
    )
  }

  mod_left <-
    contrasts |>
    filter(
      comparison ==
        "Moderate - Left"
    )

  right_mod <-
    contrasts |>
    filter(
      comparison ==
        "Right - Moderate"
    )

  if (
    nrow(
      mod_left
    ) !=
      1L ||
    nrow(
      right_mod
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify the two adjacent ideology contrasts."
    )
  }

  y1 <-
    max(
      probability$conf_high_pct,
      na.rm =
        TRUE
    ) +
    2.0

  y2 <-
    y1 +
    4.0

  tick <-
    0.55

  n_respondents <-
    unique(
      probability$n_respondents
    )

  n_countries <-
    unique(
      probability$n_countries
    )

  if (
    length(
      n_respondents
    ) !=
      1L ||
    length(
      n_countries
    ) !=
      1L
  ) {
    stop(
      "LPM prediction metadata are inconsistent."
    )
  }

  p <-
    ggplot(
      probability,
      aes(
        x =
          x_position,
        y =
          estimate_pct,
        color =
          ideology_category
      )
    ) +
    geom_errorbar(
      aes(
        ymin =
          conf_low_pct,
        ymax =
          conf_high_pct
      ),
      width =
        0.08,
      linewidth =
        0.8
    ) +
    geom_point(
      size =
        3.2
    ) +
    annotate(
      "segment",
      x =
        1,
      xend =
        2,
      y =
        y1,
      yend =
        y1,
      linewidth =
        0.55
    ) +
    annotate(
      "segment",
      x =
        1,
      xend =
        1,
      y =
        y1,
      yend =
        y1 -
        tick,
      linewidth =
        0.55
    ) +
    annotate(
      "segment",
      x =
        2,
      xend =
        2,
      y =
        y1,
      yend =
        y1 -
        tick,
      linewidth =
        0.55
    ) +
    annotate(
      "text",
      x =
        1.5,
      y =
        y1 +
        0.55,
      label =
        sprintf(
          "%+.1f pp",
          mod_left$estimate_pp
        ),
      size =
        3.6
    ) +
    annotate(
      "segment",
      x =
        2,
      xend =
        3,
      y =
        y2,
      yend =
        y2,
      linewidth =
        0.55
    ) +
    annotate(
      "segment",
      x =
        2,
      xend =
        2,
      y =
        y2,
      yend =
        y2 -
        tick,
      linewidth =
        0.55
    ) +
    annotate(
      "segment",
      x =
        3,
      xend =
        3,
      y =
        y2,
      yend =
        y2 -
        tick,
      linewidth =
        0.55
    ) +
    annotate(
      "text",
      x =
        2.5,
      y =
        y2 +
        0.55,
      label =
        sprintf(
          "%+.1f pp",
          right_mod$estimate_pp
        ),
      size =
        3.6
    ) +
    scale_x_continuous(
      breaks =
        1:3,
      labels =
        ideology_levels
    ) +
    scale_color_manual(
      values =
        ideology_colors,
      guide =
        "none"
    ) +
    scale_y_continuous(
      labels =
        function(z) {
          paste0(
            z,
            "%"
          )
        }
    ) +
    coord_cartesian(
      ylim =
        c(
          0,
          y2 +
            2
        ),
      clip =
        "off"
    ) +
    labs(
      title =
        title,
      subtitle =
        ideology_definition,
      x =
        NULL,
      y =
        "Adjusted probability of far-right support",
      caption =
        paste0(
          "Survey-weighted respondent-level linear probability model with ",
          "country fixed effects and country-clustered standard errors. ",
          "Points are counterfactual-standardized predicted probabilities; ",
          "error bars are 95% confidence intervals. Brackets show adjacent ",
          "ideology contrasts. N = ",
          format(
            n_respondents,
            big.mark =
              ","
          ),
          " respondents in ",
          n_countries,
          " countries."
        )
    ) +
    paper_theme

  list(
    probability =
      probability,
    contrasts =
      contrasts,
    plot =
      p
  )
}

build_india_figure <- function(
  source_path,
  ideology_definition,
  title
) {
  x <-
    read_csv(
      source_path,
      show_col_types = FALSE
    )

  if (
    "S009_code" %in%
      names(
        x
      )
  ) {
    india <-
      x |>
      filter(
        S009_code ==
          "IN"
      )
  } else if (
    "country_code" %in%
      names(
        x
      )
  ) {
    india <-
      x |>
      filter(
        country_code ==
          "IN"
      )
  } else {
    india <-
      x |>
      filter(
        country_label ==
          "India"
      )
  }

  india <-
    india |>
    mutate(
      ideology_category =
        factor(
          as.character(
            ideology_category
          ),
          levels =
            ideology_levels
        )
    ) |>
    arrange(
      ideology_category,
      year
    )

  if (
    nrow(
      india
    ) ==
      0L
  ) {
    stop(
      "No India rows found for Figure 4."
    )
  }

  if (
    !all(
      india$party_source ==
        "WVS"
    )
  ) {
    stop(
      "Figure 4 contains a non-WVS India observation."
    )
  }

  p <-
    ggplot(
      india,
      aes(
        x =
          year,
        y =
          pct_far_right,
        color =
          ideology_category,
        linetype =
          ideology_category,
        group =
          ideology_category
      )
    ) +
    geom_line(
      linewidth =
        1.0
    ) +
    geom_point(
      size =
        2.8
    ) +
    scale_color_manual(
      values =
        ideology_colors,
      breaks =
        ideology_levels,
      drop =
        FALSE,
      name =
        "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values =
        ideology_linetypes,
      breaks =
        ideology_levels,
      drop =
        FALSE,
      name =
        "Ideological self-placement"
    ) +
    scale_x_continuous(
      breaks =
        sort(
          unique(
            india$year
          )
        )
    ) +
    scale_y_continuous(
      breaks =
        seq(
          0,
          70,
          by =
            10
        ),
      labels =
        function(z) {
          paste0(
            z,
            "%"
          )
        }
    ) +
    coord_cartesian(
      ylim =
        c(
          0,
          70
        )
    ) +
    labs(
      title =
        title,
      subtitle =
        ideology_definition,
      x =
        "WVS survey year",
      y =
        "Far-right vote share",
      caption =
        paste0(
          "Observed World Values Survey estimates for India. ",
          "Survey weights are applied within surveys where available. ",
          support_measure_note
        )
    ) +
    paper_theme

  list(
    data =
      india,
    plot =
      p
  )
}

main_fig1 <-
  build_wave_figure(
    source_path =
      main_fig1_path,
    ideology_definition =
      "Left = 1–4; Moderate = 5–6; Right = 7–10",
    title =
      "Far-right support over time by ideology"
  )

main_fig2 <-
  build_lpm_figure(
    probability_path =
      main_fig2_path,
    contrast_path =
      main_fig2_contrast_path,
    ideology_definition =
      "Left = 1–4; Moderate = 5–6; Right = 7–10",
    title =
      "Adjusted probability of far-right support by ideology"
  )

nes_data <-
  read_csv(
    nes_fig3_path,
    show_col_types = FALSE
  ) |>
  filter(
    ideology %in%
      c(
        "Left",
        "Center",
        "Right"
      ),
    year %in%
      c(
        2009,
        2014
      )
  ) |>
  mutate(
    ideology =
      factor(
        ideology,
        levels =
          c(
            "Left",
            "Center",
            "Right"
          )
      ),

    year =
      factor(
        year,
        levels =
          c(
            2009,
            2014
          )
      )
  )

if (
  nrow(
    nes_data
  ) !=
    6L
) {
  stop(
    "Expected exactly six Figure 3 rows."
  )
}

nes_expected <-
  tibble(
    year =
      factor(
        c(
          2009,
          2009,
          2009,
          2014,
          2014,
          2014
        ),
        levels =
          c(
            2009,
            2014
          )
      ),

    ideology =
      factor(
        c(
          "Left",
          "Center",
          "Right",
          "Left",
          "Center",
          "Right"
        ),
        levels =
          c(
            "Left",
            "Center",
            "Right"
          )
      ),

    expected =
      c(
        13.071868031833597,
        21.48717030949682,
        34.632395956920995,
        23.600840469812322,
        37.210055480478616,
        49.00711683512926
      )
  )

nes_check <-
  nes_data |>
  select(
    year,
    ideology,
    observed =
      bjp_vote_pct_weighted
  ) |>
  left_join(
    nes_expected,
    by =
      c(
        "year",
        "ideology"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    absolute_difference =
      abs(
        observed -
          expected
      )
  )

if (
  any(
    nes_check$absolute_difference >
      1e-10
  )
) {
  print(
    nes_check,
    n = Inf,
    width = Inf
  )

  stop(
    "Figure 3 values do not reproduce the audited frozen values."
  )
}

nes_colors <-
  c(
    "Left" =
      "#0000FF",
    "Center" =
      "#FFD700",
    "Right" =
      "#FF0000"
  )

fig3_plot <-
  ggplot(
    nes_data,
    aes(
      x =
        year,
      y =
        bjp_vote_pct_weighted,
      fill =
        ideology,
      group =
        ideology
    )
  ) +
  geom_col(
    position =
      position_dodge(
        width =
          0.82
      ),
    width =
      0.72
  ) +
  geom_text(
    aes(
      label =
        label
    ),
    position =
      position_dodge(
        width =
          0.82
      ),
    vjust =
      -0.35,
    size =
      3.4
  ) +
  scale_fill_manual(
    values =
      nes_colors,
    breaks =
      c(
        "Left",
        "Center",
        "Right"
      ),
    name =
      "Ideology"
  ) +
  scale_y_continuous(
    breaks =
      seq(
        0,
        60,
        by =
          10
      ),
    labels =
      function(z) {
        paste0(
          z,
          "%"
        )
      },
    expand =
      expansion(
        mult =
          c(
            0,
            0.08
          )
      )
  ) +
  coord_cartesian(
    ylim =
      c(
        0,
        60
      )
  ) +
  labs(
    title =
      "BJP vote share by voter ideology, 2009 and 2014",
    x =
      NULL,
    y =
      "Weighted BJP vote share",
    caption =
      paste0(
        "Bars use survey weights stpop1 in 2009 and stpop in 2014. ",
        "The 2009 harmonized classification requires both recognition items ",
        "and at least two of three statism items to fall in the same ideological ",
        "bucket. The 2014 classification requires Q10b, Q10e, and corrected ",
        "Q23c to agree. Mixed respondents are omitted from the figure."
      )
  ) +
  paper_theme

main_fig4 <-
  build_india_figure(
    source_path =
      main_fig4_path,
    ideology_definition =
      "Left = 1–4; Moderate = 5–6; Right = 7–10",
    title =
      "Far-right support in India by ideology"
  )

appendix_fig1 <-
  build_wave_figure(
    source_path =
      appendix_fig1_path,
    ideology_definition =
      "Alternative definition: Left = 1–2; Moderate = 5–6; Right = 9–10",
    title =
      "Far-right support over time by ideology"
  )

appendix_fig2 <-
  build_lpm_figure(
    probability_path =
      appendix_fig2_path,
    contrast_path =
      appendix_fig2_contrast_path,
    ideology_definition =
      "Alternative definition: Left = 1–2; Moderate = 5–6; Right = 9–10",
    title =
      "Adjusted probability of far-right support by ideology"
  )

appendix_fig4 <-
  build_india_figure(
    source_path =
      appendix_fig4_path,
    ideology_definition =
      "Alternative definition: Left = 1–2; Moderate = 5–6; Right = 9–10",
    title =
      "Far-right support in India by ideology"
  )

main_files <-
  c(
    save_plot(
      main_fig1$plot,
      file.path(
        main_output_dir,
        "Figure_1_aid_lmic_far_right_support_over_time"
      ),
      width =
        10.5,
      height =
        6.4
    ),

    save_plot(
      main_fig2$plot,
      file.path(
        main_output_dir,
        "Figure_2_adjusted_far_right_support_by_ideology"
      ),
      width =
        7.4,
      height =
        6.0
    ),

    save_plot(
      fig3_plot,
      file.path(
        main_output_dir,
        "Figure_3_nes_bjp_vote_share_by_ideology"
      ),
      width =
        7.8,
      height =
        5.7
    ),

    save_plot(
      main_fig4$plot,
      file.path(
        main_output_dir,
        "Figure_4_india_wvs_far_right_support_by_ideology"
      ),
      width =
        8.2,
      height =
        5.8
    )
  )

appendix_files <-
  c(
    save_plot(
      appendix_fig1$plot,
      file.path(
        appendix_output_dir,
        "Appendix_narrow_aid_lmic_far_right_support_over_time"
      ),
      width =
        10.5,
      height =
        6.4
    ),

    save_plot(
      appendix_fig2$plot,
      file.path(
        appendix_output_dir,
        "Appendix_narrow_adjusted_far_right_support_by_ideology"
      ),
      width =
        7.4,
      height =
        6.0
    ),

    save_plot(
      appendix_fig4$plot,
      file.path(
        appendix_output_dir,
        "Appendix_narrow_india_wvs_far_right_support_by_ideology"
      ),
      width =
        8.2,
      height =
        5.8
    )
  )

write_csv(
  main_fig1$data,
  file.path(
    data_output_dir,
    "Figure_1_source_data.csv"
  )
)

write_csv(
  main_fig2$probability,
  file.path(
    data_output_dir,
    "Figure_2_adjusted_probabilities.csv"
  )
)

write_csv(
  main_fig2$contrasts,
  file.path(
    data_output_dir,
    "Figure_2_ideology_contrasts.csv"
  )
)

write_csv(
  nes_data,
  file.path(
    data_output_dir,
    "Figure_3_source_data.csv"
  )
)

write_csv(
  main_fig4$data,
  file.path(
    data_output_dir,
    "Figure_4_source_data.csv"
  )
)

write_csv(
  appendix_fig1$data,
  file.path(
    data_output_dir,
    "Appendix_narrow_wave_source_data.csv"
  )
)

write_csv(
  appendix_fig2$probability,
  file.path(
    data_output_dir,
    "Appendix_narrow_adjusted_probabilities.csv"
  )
)

write_csv(
  appendix_fig2$contrasts,
  file.path(
    data_output_dir,
    "Appendix_narrow_ideology_contrasts.csv"
  )
)

write_csv(
  appendix_fig4$data,
  file.path(
    data_output_dir,
    "Appendix_narrow_india_source_data.csv"
  )
)

figure2_mod_left <-
  main_fig2$contrasts |>
  filter(
    comparison ==
      "Moderate - Left"
  )

figure2_right_mod <-
  main_fig2$contrasts |>
  filter(
    comparison ==
      "Right - Moderate"
  )

nes_notes <-
  paste(
    readLines(
      nes_notes_path,
      warn =
        FALSE
    ),
    collapse =
      " "
  )

captions <-
  tribble(
    ~figure_id,
    ~placement,
    ~title,
    ~caption,
    ~source_script,
    ~source_data,

    "Figure 1",
    "Main",
    "Far-right support over time by ideology",
    paste0(
      "Far-right vote share by ideological self-placement and survey wave. ",
      "The main ideology definition classifies E033 scores 1-4 as Left, 5-6 ",
      "as Moderate, and 7-10 as Right. Each point is an equal-country ",
      "survey-wave mean after applying survey weights within country-surveys. ",
      "AIDs include WVS and EVS observations; LMICs include WVS observations ",
      "only. ",
      support_measure_note
    ),
    "status_threat_puzzle_pipeline_party_rewrite_v6_1.R",
    main_fig1_path,

    "Figure 2",
    "Main",
    "Adjusted probability of far-right support by ideology",
    paste0(
      "Counterfactual-standardized probabilities from a survey-weighted ",
      "respondent-level linear probability model with country fixed effects ",
      "and country-clustered standard errors. The ideology definition is ",
      "Left 1-4, Moderate 5-6, and Right 7-10. Error bars are 95% confidence ",
      "intervals. Moderate support exceeds Left support by ",
      sprintf(
        "%.1f",
        figure2_mod_left$estimate_pp
      ),
      " percentage points (95% CI ",
      sprintf(
        "%.1f",
        figure2_mod_left$conf_low_pp
      ),
      " to ",
      sprintf(
        "%.1f",
        figure2_mod_left$conf_high_pp
      ),
      "); Right support exceeds Moderate support by ",
      sprintf(
        "%.1f",
        figure2_right_mod$estimate_pp
      ),
      " percentage points (95% CI ",
      sprintf(
        "%.1f",
        figure2_right_mod$conf_low_pp
      ),
      " to ",
      sprintf(
        "%.1f",
        figure2_right_mod$conf_high_pp
      ),
      ")."
    ),
    "status_threat_puzzle_pipeline_party_rewrite_v6_1.R",
    main_fig2_path,

    "Figure 3",
    "Main",
    "BJP vote share by voter ideology, 2009 and 2014",
    paste0(
      "Survey-weighted BJP vote shares among respondents classified Left, ",
      "Center, or Right in the National Election Studies. ",
      nes_notes
    ),
    "R/21_nes_2009_audit_and_crossyear_bjp_plot_v1_1.R",
    nes_fig3_path,

    "Figure 4",
    "Main",
    "Far-right support in India by ideology",
    paste0(
      "Observed World Values Survey far-right vote shares in India, using ",
      "the main ideology definition: Left 1-4, Moderate 5-6, and Right 7-10. ",
      "The plotted surveys are 1990, 1995, 2001, 2006, 2012, and 2023. ",
      support_measure_note
    ),
    "status_threat_puzzle_pipeline_party_rewrite_v6_1.R",
    main_fig4_path
  )

write_csv(
  captions,
  file.path(
    output_dir,
    "01_main_figure_captions_and_provenance.csv"
  )
)

git_head <-
  tryCatch(
    system2(
      "git",
      c(
        "rev-parse",
        "HEAD"
      ),
      stdout =
        TRUE,
      stderr =
        FALSE
    ),
    error =
      function(e) {
        NA_character_
      }
  )

source_files <-
  c(
    main_fig1_path,
    main_fig2_path,
    main_fig2_contrast_path,
    main_fig4_path,
    nes_fig3_path,
    "status_threat_puzzle_pipeline_party_rewrite_v6_1.R",
    "R/21_nes_2009_audit_and_crossyear_bjp_plot_v1_1.R"
  )

provenance <-
  tibble(
    path =
      source_files,
    exists =
      file.exists(
        source_files
      ),
    md5 =
      unname(
        tools::md5sum(
          source_files
        )
      ),
    git_head =
      if (
        length(
          git_head
        ) >
          0L
      ) {
        git_head[[1]]
      } else {
        NA_character_
      }
  )

write_csv(
  provenance,
  file.path(
    output_dir,
    "02_source_provenance.csv"
  )
)

all_generated_files <-
  c(
    main_files,
    appendix_files,
    list.files(
      data_output_dir,
      full.names =
        TRUE
    ),
    file.path(
      output_dir,
      "01_main_figure_captions_and_provenance.csv"
    ),
    file.path(
      output_dir,
      "02_source_provenance.csv"
    )
  )

manifest <-
  tibble(
    path =
      all_generated_files,
    exists =
      file.exists(
        all_generated_files
      ),
    bytes =
      file.info(
        all_generated_files
      )$size,
    md5 =
      unname(
        tools::md5sum(
          all_generated_files
        )
      )
  )

if (
  any(
    !manifest$exists
  )
) {
  print(
    manifest,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one publication figure artifact was not generated."
  )
}

write_csv(
  manifest,
  file.path(
    output_dir,
    "03_generated_artifact_manifest.csv"
  )
)

notes <-
  c(
    "R32 MAIN DESCRIPTIVE FIGURE BUILD",
    "",
    "Main WVS/EVS ideology definition:",
    "Left = E033 1-4; Moderate = E033 5-6; Right = E033 7-10.",
    "",
    "Appendix WVS/EVS robustness definition:",
    "Left = E033 1-2; Moderate = E033 5-6; Right = E033 9-10.",
    "",
    "Figure 1 is reconstructed from the pipeline's frozen equal-country survey-wave means.",
    "",
    "Figure 2 is reconstructed from the frozen country-FE LPM standardized predictions and contrast CSV.",
    "",
    "Figure 3 is reconstructed from the audited cross-year NES classification and explicitly reproduces the six frozen weighted vote-share values.",
    "",
    "Figure 4 is reconstructed from the India rows of the WVS/EVS country-survey source CSV and requires every displayed observation to come from WVS.",
    "",
    "PDF files use the base R PDF device rather than Cairo.",
    "",
    "No regression specification or substantive classification rule is altered by this script."
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
  "\n===== FIGURE 2 MAIN CONTRASTS =====\n"
)

print(
  main_fig2$contrasts |>
    filter(
      comparison %in%
        c(
          "Moderate - Left",
          "Right - Moderate"
        )
    ) |>
    select(
      comparison,
      estimate_pp,
      conf_low_pp,
      conf_high_pp,
      p_value
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== FIGURE 3 NUMERICAL REPRODUCTION =====\n"
)

print(
  nes_check,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FIGURE 4 INDIA YEARS =====\n"
)

print(
  main_fig4$data |>
    select(
      party_source,
      survey_wave,
      year,
      ideology_category,
      pct_far_right
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== GENERATED ARTIFACTS =====\n"
)

print(
  manifest,
  n = Inf,
  width = Inf
)

cat(
  "\nR32_MAIN_DESCRIPTIVE_FIGURES_COMPLETE\n"
)

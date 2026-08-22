# ============================================================
# 16_generate_paper_figures.R
# Generates numbered paper figures 01-17 and specification-curve family 33.
# Source 15_prepare_paper_outputs.R first (or run 18_run_paper_output_pipeline.R).
# ============================================================

if (!exists("paper_dirs")) {
  .root_for_source <- if (exists("project_root")) project_root else Sys.getenv("SWITCHERS_ROOT", unset = "/Users/Daevan/Downloads/Switchers-India")
  source(file.path(.root_for_source, "R", "15_prepare_paper_outputs.R"))
}

# ------------------------------------------------------------
# Figure 01: weighted BJP vote share by ideology bucket and year
# ------------------------------------------------------------
fig01_data <- respondents |>
  filter(
    year %in% c(2009, 2014),
    vote_valid,
    !is.na(voted_bjp),
    !is.na(voter_ideology_harmonized),
    !is.na(.data[[RESP_WEIGHT_VAR]])
  ) |>
  group_by(year, ideology = voter_ideology_harmonized) |>
  summarise(
    bjp_vote_share = weighted.mean(voted_bjp, .data[[RESP_WEIGHT_VAR]], na.rm = TRUE),
    n_unweighted = n(),
    .groups = "drop"
  ) |>
  mutate(
    year = factor(year),
    ideology = factor(ideology, levels = ideology_levels)
  )

p01 <- ggplot(fig01_data, aes(x = ideology, y = bjp_vote_share, fill = year)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.70) +
  geom_text(
    aes(label = percent(bjp_vote_share, accuracy = 0.1)),
    position = position_dodge(width = 0.78),
    vjust = -0.25,
    size = 3.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "BJP vote share by ideological bucket, 2009 and 2014",
    subtitle = "Survey-weighted NES respondents with valid reported votes; harmonized ideology classification",
    x = NULL,
    y = "Weighted share voting BJP",
    fill = "Election year"
  ) +
  theme_paper()

save_plot_pair(p01, "01_main_bjp_vote_share_by_ideology_2009_2014", paper_dirs$main_figures, 8.4, 5.3, "1")
write_csv(fig01_data, file.path(paper_dirs$data, "01_bjp_vote_share_by_ideology.csv"))

# ------------------------------------------------------------
# Figures 02-05: top 10 parties within each ideology bucket, separately by year
# ------------------------------------------------------------
if (!"reported_vote_party_label" %in% names(respondents)) {
  warning("reported_vote_party_label absent; Figures 02-05 cannot be generated.")
  for (i in 2:5) register_output(i, "figure", "", "FAILED", "reported_vote_party_label absent")
} else {
  respondent_parties <- respondents |>
    filter(
      year %in% c(2009, 2014),
      vote_valid,
      !is.na(reported_vote_party_label),
      !is.na(voter_ideology_harmonized),
      !is.na(.data[[RESP_WEIGHT_VAR]])
    ) |>
    mutate(
      party = case_when(
        voted_bjp == 1 ~ "BJP",
        voted_congress == 1 ~ "Congress",
        voted_shs == 1 ~ "Shiv Sena",
        voted_mns == 1 ~ "MNS",
        TRUE ~ str_squish(as.character(reported_vote_party_label))
      )
    )

  ideology_to_item <- c(Left = 2, Center = 3, Right = 4, Mixed = 5)
  for (ideo in names(ideology_to_item)) {
    item_no <- ideology_to_item[[ideo]]
    dd <- respondent_parties |>
      filter(as.character(voter_ideology_harmonized) == ideo) |>
      group_by(year, party) |>
      summarise(
        weight = sum(.data[[RESP_WEIGHT_VAR]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(year) |>
      mutate(weighted_share = weight / sum(weight)) |>
      slice_max(weighted_share, n = 10, with_ties = FALSE) |>
      arrange(year, weighted_share) |>
      ungroup() |>
      mutate(
        party_year = factor(
          paste(year, party, sep = "___"),
          levels = paste(year, party, sep = "___")
        )
      )

    pp <- ggplot(dd, aes(x = weighted_share, y = party_year)) +
      geom_col() +
      facet_wrap(~year, scales = "free_y", ncol = 2) +
      scale_y_discrete(labels = function(x) sub("^[0-9]{4}___", "", x)) +
      scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .08))) +
      labs(
        title = paste0("Top 10 parties among ", tolower(ideo), " respondents"),
        subtitle = "Top 10 selected separately within each election year; survey weighted",
        x = "Weighted share of valid votes within ideological bucket",
        y = NULL
      ) +
      theme_paper()

    stem <- sprintf("%02d_appendix_top10_parties_%s_2009_2014", item_no, tolower(ideo))
    save_plot_pair(pp, stem, paper_dirs$appendix_figures, 10, 6.6, as.character(item_no))
    write_csv(dd, file.path(paper_dirs$data, paste0(stem, ".csv")))
  }
}

# ------------------------------------------------------------
# Figures 06-09: weighted income distributions by ideology and year
# Figures 10-13: weighted education distributions by ideology and year
# ------------------------------------------------------------
weighted_categorical_plot <- function(data, ideology, variable, title, ylab = "Weighted share of respondents") {
  dd <- data |>
    filter(
      year %in% c(2009, 2014),
      as.character(voter_ideology_harmonized) == ideology,
      !is.na(.data[[variable]]),
      !is.na(.data[[RESP_WEIGHT_VAR]])
    ) |>
    mutate(category = as.character(.data[[variable]])) |>
    group_by(year, category) |>
    summarise(weight = sum(.data[[RESP_WEIGHT_VAR]], na.rm = TRUE), .groups = "drop") |>
    group_by(year) |>
    mutate(weighted_share = weight / sum(weight)) |>
    ungroup() |>
    mutate(year = factor(year))

  # Preserve factor order if the source variable is ordered/factor.
  source_levels <- if (is.factor(data[[variable]])) levels(data[[variable]]) else unique(dd$category)
  dd$category <- factor(dd$category, levels = source_levels[source_levels %in% dd$category])

  p <- ggplot(dd, aes(x = category, y = weighted_share, fill = year)) +
    geom_col(position = position_dodge(width = .78), width = .7) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .08))) +
    labs(
      title = title,
      subtitle = "Survey-weighted NES respondents; harmonized ideology classification",
      x = NULL,
      y = ylab,
      fill = "Election year"
    ) +
    theme_paper() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  list(data = dd, plot = p)
}

for (k in seq_along(ideology_levels)) {
  ideo <- ideology_levels[[k]]
  item_no <- 5 + k
  if ("income_harmonized" %in% names(respondents)) {
    obj <- weighted_categorical_plot(
      respondents, ideo, "income_harmonized",
      paste0("Income distribution among ", tolower(ideo), " respondents, 2009 and 2014")
    )
    stem <- sprintf("%02d_appendix_income_distribution_%s", item_no, tolower(ideo))
    save_plot_pair(obj$plot, stem, paper_dirs$appendix_figures, 9.5, 5.8, as.character(item_no))
    write_csv(obj$data, file.path(paper_dirs$data, paste0(stem, ".csv")))
  } else {
    register_output(item_no, "figure", "", "FAILED", "income_harmonized absent")
  }
}

for (k in seq_along(ideology_levels)) {
  ideo <- ideology_levels[[k]]
  item_no <- 9 + k
  if ("education_harmonized" %in% names(respondents)) {
    obj <- weighted_categorical_plot(
      respondents, ideo, "education_harmonized",
      paste0("Education distribution among ", tolower(ideo), " respondents, 2009 and 2014")
    )
    stem <- sprintf("%02d_appendix_education_distribution_%s", item_no, tolower(ideo))
    save_plot_pair(obj$plot, stem, paper_dirs$appendix_figures, 10.5, 6.0, as.character(item_no))
    write_csv(obj$data, file.path(paper_dirs$data, paste0(stem, ".csv")))
  } else {
    register_output(item_no, "figure", "", "FAILED", "education_harmonized absent")
  }
}

# ------------------------------------------------------------
# Figures 14-17: original agree/disagree ideology-item responses by bucket/year.
# ------------------------------------------------------------
find_first_column <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)][1]
  if (length(hit) == 0 || is.na(hit)) NA_character_ else hit
}

item_label_col <- find_first_column(
  ideology_items_long,
  c("question_text", "question", "item_label", "variable_label", "label", "question_label")
)
response_label_col <- find_first_column(
  ideology_items_long,
  c("response_label", "response_text", "raw_response_label", "answer_label", "value_label", "response", "raw_response")
)

# Recover item wording from SAV variable labels if not persisted in the long RDS.
item_question_map <- tibble(item = classification_items, question = NA_character_)
if (!is.na(item_label_col)) {
  qmap <- ideology_items_long |>
    filter(item %in% classification_items) |>
    transmute(item, question = as.character(.data[[item_label_col]])) |>
    filter(!is.na(question), nzchar(question)) |>
    distinct(item, .keep_all = TRUE)
  item_question_map <- item_question_map |>
    select(item) |>
    left_join(qmap, by = "item")
}

if (any(is.na(item_question_map$question))) {
  sav09 <- first_existing(c(
    file.path(project_root, "data", "lokniti", "nes_2009.sav"),
    file.path(project_root, "data", "nes_2009.sav")
  ))
  sav14 <- first_existing(c(
    file.path(project_root, "data", "lokniti", "nes_2014.sav"),
    file.path(project_root, "data", "nes_2014.sav")
  ))
  for (sav in c(sav09, sav14)) {
    if (!is.na(sav)) {
      raw <- haven::read_sav(sav, n_max = 1)
      for (itm in intersect(classification_items, names(raw))) {
        lab <- attr(raw[[itm]], "label")
        if (!is.null(lab) && nzchar(lab)) {
          item_question_map$question[item_question_map$item == itm & is.na(item_question_map$question)] <- as.character(lab)
        }
      }
    }
  }
}
item_question_map <- item_question_map |>
  mutate(question = if_else(is.na(question) | !nzchar(question), item, question))
write_csv(item_question_map, file.path(paper_dirs$data, "18_ideology_item_question_map.csv"))

if (is.na(response_label_col)) {
  warning("No raw response-label column found in ideology_item_responses_long.rds; Figures 14-17 marked failed.")
  for (i in 14:17) register_output(i, "figure", "", "FAILED", "No raw agree/disagree response-label column")
} else {
  ideology_join <- respondents |>
    select(respondent_uid, year, voter_ideology_harmonized, all_of(RESP_WEIGHT_VAR)) |>
    filter(!is.na(voter_ideology_harmonized))

  item_plot_data <- ideology_items_long |>
    filter(item %in% classification_items) |>
    mutate(
      response_text = if (inherits(.data[[response_label_col]], "haven_labelled")) {
        as.character(haven::as_factor(.data[[response_label_col]], levels = "labels"))
      } else {
        as.character(.data[[response_label_col]])
      },
      response_binary = case_when(
        str_detect(str_to_lower(response_text), "disagree|do not agree|not agree") ~ "Disagree",
        str_detect(str_to_lower(response_text), "agree") ~ "Agree",
        TRUE ~ NA_character_
      ),
      axis = case_when(
        item %in% c("a4b", "a4c", "q10b", "q10e") ~ "Recognition",
        item %in% c("a4d", "a4g", "q26a", "q23c") ~ "Statism",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(response_binary)) |>
    left_join(ideology_join, by = c("respondent_uid", "year"), relationship = "many-to-one") |>
    left_join(item_question_map, by = "item", relationship = "many-to-one")

  # If the long file has no labels containing agree/disagree, do not silently use bucket labels.
  if (nrow(item_plot_data) == 0) {
    warning("Response labels do not expose Agree/Disagree; Figures 14-17 marked failed rather than inferred.")
    for (i in 14:17) register_output(i, "figure", "", "FAILED", "Could not identify Agree/Disagree from response labels")
  } else {
    ideology_to_item2 <- c(Left = 14, Center = 15, Right = 16, Mixed = 17)
    for (ideo in names(ideology_to_item2)) {
      item_no <- ideology_to_item2[[ideo]]
      dd <- item_plot_data |>
        filter(as.character(voter_ideology_harmonized) == ideo, !is.na(.data[[RESP_WEIGHT_VAR]])) |>
        group_by(year, axis, item, question, response_binary) |>
        summarise(weight = sum(.data[[RESP_WEIGHT_VAR]], na.rm = TRUE), .groups = "drop") |>
        group_by(year, item) |>
        mutate(weighted_share = weight / sum(weight)) |>
        ungroup() |>
        mutate(
          question_short = str_wrap(question, width = 34),
          response_binary = factor(response_binary, levels = c("Agree", "Disagree")),
          year = factor(year)
        )

      pp <- ggplot(dd, aes(x = question_short, y = weighted_share, fill = response_binary)) +
        geom_col(position = position_dodge(width = .75), width = .68) +
        facet_grid(axis ~ year, scales = "free_x", space = "free_x") +
        scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .07))) +
        labs(
          title = paste0("Ideological-item responses among ", tolower(ideo), " respondents"),
          subtitle = "Original agree/disagree responses; survey weighted; panels separate recognition and statism items",
          x = NULL,
          y = "Weighted response share",
          fill = "Response"
        ) +
        theme_paper(10) +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

      stem <- sprintf("%02d_appendix_binary_ideology_items_%s", item_no, tolower(ideo))
      save_plot_pair(pp, stem, paper_dirs$appendix_figures, 13, 7.2, as.character(item_no))
      write_csv(dd, file.path(paper_dirs$data, paste0(stem, ".csv")))
    }
  }
}

# ------------------------------------------------------------
# Figure family 33: specification curves by level x demographic family x FDI family.
# This is appendix material. Existing respondent triple curves use the frozen
# center_harmonized robustness specification, consistent with the user's decision
# to place center_harmonized in the appendix. Main respondent regression tables
# use the full harmonized ideology bucket instead (generated in script 17).
# ------------------------------------------------------------

curve_family_from_var <- function(domain, moderator_var, moderator_family = "") {
  v <- as.character(moderator_var)
  fam <- as.character(moderator_family)
  if (domain == "muslim") {
    if (str_detect(v, "^d_|pct_change|2001_2011") || str_detect(str_to_lower(fam), "change")) return("change_muslim")
    if (str_detect(v, "2001") && !str_detect(v, "2011") && !str_detect(v, "^d_|pct_change")) return("existing_muslim")
    return(NA_character_)
  }
  if (domain == "migration") {
    if (str_detect(v, "^d_|pct_change|accel|2001_2011|2009_2014")) return("change_migrant")
    if (str_detect(v, "total_upto_2001|bengali_bhojpuri.*2001|male_mig_total_upto_2001")) return("existing_migrant")
    return(NA_character_)
  }
  NA_character_
}

read_latest_curve_files <- function(level = c("aggregate", "respondent")) {
  level <- match.arg(level)
  root <- if (level == "aggregate") {
    file.path(paths$derived_dir, "model_exploration", "specification_curves", "results")
  } else {
    file.path(paths$derived_dir, "model_exploration", "respondent_specification_curves", "results")
  }
  if (!dir.exists(root)) return(tibble())
  files <- list.files(root, pattern = "__full__.*\\.csv$", full.names = TRUE)
  if (level == "aggregate") {
    # Prefer the repaired v5.0 universe; if absent, latest full CSVs remain available.
    preferred <- files[str_detect(basename(files), "v5\\.0-control-repair")]
    if (length(preferred)) files <- preferred
  } else {
    preferred <- files[str_detect(basename(files), "^primary__")]
    if (length(preferred)) files <- preferred
  }
  if (!length(files)) return(tibble())
  map_dfr(files, function(f) {
    tryCatch(
      read_csv(f, show_col_types = FALSE, progress = FALSE) |>
        mutate(source_file = basename(f), analysis_level = level),
      error = function(e) tibble()
    )
  })
}

plot_curve_subset <- function(dd, title) {
  if (!nrow(dd)) return(NULL)
  est_col <- if ("contrast_estimate" %in% names(dd)) "contrast_estimate" else "estimate"
  lo_col <- if ("contrast_conf_low" %in% names(dd)) "contrast_conf_low" else if ("conf_low" %in% names(dd)) "conf_low" else NA_character_
  hi_col <- if ("contrast_conf_high" %in% names(dd)) "contrast_conf_high" else if ("conf_high" %in% names(dd)) "conf_high" else NA_character_
  if (is.na(lo_col) || is.na(hi_col)) return(NULL)

  dd <- dd |>
    filter(fit_ok %in% c(TRUE, 1), is.finite(.data[[est_col]])) |>
    arrange(.data[[est_col]]) |>
    mutate(spec_index = row_number())
  if (!nrow(dd)) return(NULL)

  ggplot(dd, aes(x = spec_index, y = .data[[est_col]])) +
    geom_hline(yintercept = 0, linewidth = .35) +
    geom_ribbon(aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]]), alpha = .20) +
    geom_line(linewidth = .45) +
    facet_wrap(~interaction_order, scales = "free_x", ncol = 1) +
    labs(
      title = title,
      subtitle = "Specifications sorted within the displayed family by estimated substantive contrast",
      x = "Specification (sorted)",
      y = "Substantive contrast (percentage points)"
    ) +
    theme_paper(10)
}

agg_curves <- read_latest_curve_files("aggregate")
resp_curves <- read_latest_curve_files("respondent")

if (nrow(agg_curves)) {
  agg_curves <- agg_curves |>
    mutate(
      domain = coalesce(moderator_domain, if_else(str_detect(design_id, "muslim"), "muslim", "migration")),
      curve_family = pmap_chr(list(domain, moderator_var, moderator_family), curve_family_from_var)
    )
}
if (nrow(resp_curves)) {
  resp_curves <- resp_curves |>
    mutate(
      domain = coalesce(moderator_domain, if_else(str_detect(design_id, "muslim"), "muslim", "migration")),
      curve_family = pmap_chr(list(domain, moderator_var, moderator_family), curve_family_from_var)
    )
}

curve_family_labels <- c(
  existing_muslim = "Existing Muslim context (2001 family)",
  change_muslim = "Muslim change family",
  existing_migrant = "Existing migration/compositional context",
  change_migrant = "Migration change family"
)

curve_index <- 0L
for (level in c("aggregate", "respondent")) {
  allc <- if (level == "aggregate") agg_curves else resp_curves
  if (!nrow(allc)) next
  for (fam in names(curve_family_labels)) {
    for (fdi_family in c("mfg", "total", "services")) {
      dd <- allc |>
        filter(curve_family == fam, .data$fdi_family == fdi_family)
      if (!nrow(dd)) next
      curve_index <- curve_index + 1L
      suffix <- letters[(curve_index - 1) %% 26 + 1]
      title <- paste0(
        if_else(level == "aggregate", "Constituency", "Voter"),
        " specification curve: ", curve_family_labels[[fam]],
        " × ", case_when(fdi_family == "mfg" ~ "manufacturing FDI", fdi_family == "total" ~ "total FDI", TRUE ~ "services FDI")
      )
      pp <- plot_curve_subset(dd, title)
      if (is.null(pp)) next
      stem <- paste0("33", suffix, "_appendix_curve_", level, "_", fam, "_", fdi_family)
      save_plot_pair(pp, stem, paper_dirs$appendix_figures, 10.5, 7.0, "33")
      write_csv(dd, file.path(paper_dirs$data, paste0(stem, ".csv")))
    }
  }
}

message("Figure generation complete.")
write_status_ledger()

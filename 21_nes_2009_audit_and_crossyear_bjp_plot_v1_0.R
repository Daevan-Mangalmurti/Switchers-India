# ============================================================
# NES 2009 ideology audit + corrected 2009/2014 BJP plot
# Revision: 2026-08-20-v1.0
#
# MAIN CROSS-YEAR CLASSIFICATION
# ------------------------------
# 2009:
#   Recognition: A4b + A4c
#   Statism:     A4d + A4g + Q26a
#   Pure Left/Center/Right requires:
#     - both 2/2 recognition items in that bucket, AND
#     - at least 2/3 statism items in that same bucket.
#   Other ideology-complete respondents -> Mixed.
#
# 2014:
#   Recognition: Q10b + Q10e
#   Statism:     Q23c
#   Pure Left/Center/Right requires all three items to support
#   the corresponding bucket.
#   Other ideology-complete respondents -> Mixed.
#
# IMPORTANT:
#   Q23c is CORRECTED here:
#   "Government should strongly curb strikes by workers and employees"
#     1 Fully/strongly agree    -> +2 Right
#     2 Somewhat agree         -> +1 Center
#     3 Somewhat disagree      -> -1 Center
#     4 Fully/strongly disagree-> -2 Left
#
# The script reads RAW NES .sav files, not derived ideology or BJP variables,
# for the headline results.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(scales)
})

PROJECT_ROOT <- "."
NES_2009_FILE <- file.path(PROJECT_ROOT, "data", "lokniti", "nes_2009.sav")
NES_2014_FILE <- file.path(PROJECT_ROOT, "data", "lokniti", "nes_2014.sav")

OUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "nes_2009_2014_ideology_audit_v1_0"
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

for (p in c(NES_2009_FILE, NES_2014_FILE)) {
  if (!file.exists(p)) stop("Required file not found: ", p)
}

raw09 <- read_sav(NES_2009_FILE)
raw14 <- read_sav(NES_2014_FILE)

required09 <- c(
  "res", "q1a", "stpop1",
  "a4b", "a4c", "a4d", "a4g", "q26a"
)
required14 <- c(
  "resno", "q1a", "stpop",
  "q10b", "q10e", "q23c"
)

missing09 <- setdiff(required09, names(raw09))
missing14 <- setdiff(required14, names(raw14))

if (length(missing09) > 0) {
  stop("NES 2009 missing required variables: ", paste(missing09, collapse = ", "))
}
if (length(missing14) > 0) {
  stop("NES 2014 missing required variables: ", paste(missing14, collapse = ", "))
}

# ------------------------------------------------------------
# 1. Helpers
# ------------------------------------------------------------

variable_label <- function(x) {
  lab <- attr(x, "label")
  if (is.null(lab) || length(lab) == 0 || is.na(lab)) return("")
  as.character(lab)
}

value_label_table <- function(x) {
  labs <- attr(x, "labels")
  if (is.null(labs) || length(labs) == 0) {
    return(tibble(raw_code = numeric(), response_label = character()))
  }

  tibble(
    raw_code = as.numeric(unname(labs)),
    response_label = names(labs)
  ) |>
    arrange(raw_code)
}

bucket_from_oriented <- function(x) {
  case_when(
    x == -2 ~ "Left",
    x %in% c(-1, 1) ~ "Center",
    x == 2 ~ "Right",
    TRUE ~ NA_character_
  )
}

positive_weight <- function(x) {
  x <- as.numeric(x)
  if_else(is.finite(x) & x > 0, x, NA_real_)
}

weighted_pct <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  100 * weighted.mean(x[ok], w[ok])
}

weighted_share_indicator <- function(indicator, w) {
  weighted_pct(as.numeric(indicator), w)
}

normalize_old_ideology <- function(x) {
  case_when(
    as.character(x) %in% c("L", "Left") ~ "Left",
    as.character(x) %in% c("C", "Center", "Centrist") ~ "Center",
    as.character(x) %in% c("R", "Right") ~ "Right",
    as.character(x) %in% c("M", "Mixed") ~ "Mixed",
    TRUE ~ NA_character_
  )
}

assert_label_contains <- function(data, variable, terms) {
  lab <- tolower(variable_label(data[[variable]]))
  missing_terms <- terms[!vapply(
    terms,
    function(term) grepl(tolower(term), lab, fixed = TRUE),
    logical(1)
  )]

  if (length(missing_terms) > 0) {
    stop(
      "Question-label guard failed for ", variable, ".\n",
      "Observed label: ", variable_label(data[[variable]]), "\n",
      "Missing expected term(s): ", paste(missing_terms, collapse = ", ")
    )
  }
}

# ------------------------------------------------------------
# 2. Guard against auditing the wrong 2009 questions
# ------------------------------------------------------------

assert_label_contains(raw09, "a4b", c("community", "marriage", "property"))
assert_label_contains(raw09, "a4c", c("ban", "religious", "conversions"))
assert_label_contains(raw09, "a4d", c("marriage", "castes", "banned"))
assert_label_contains(raw09, "a4g", c("sons", "daughters", "equal", "property"))
assert_label_contains(raw09, "q26a", c("responsible", "poverty", "government"))

# Also guard the corrected 2014 Q23c target.
assert_label_contains(raw14, "q23c", c("curb", "strikes", "workers"))

# ------------------------------------------------------------
# 3. 2009 item specification and conceptual sign audit
#
# Negative score = Left
# Positive score = Right
# Middle responses (-1/+1) = Center
# ------------------------------------------------------------

item_specs_2009 <- tribble(
  ~item,  ~axis,          ~score_1, ~score_2, ~score_3, ~score_4, ~audit_verdict, ~direction_rationale,
  "a4b",  "Recognition",        -2,       -1,        1,        2, "PASS under existing conceptual scheme", "Agreement with community-specific marriage/property law is coded as accommodation/pluralism (Left); disagreement as uniformist position (Right).",
  "a4c",  "Recognition",         2,        1,       -1,       -2, "PASS", "Agreement with a legal ban on religious conversions is coded Right; disagreement Left.",
  "a4d",  "Statism",             2,        1,       -1,       -2, "PASS", "Agreement with banning inter-caste marriage is coded Right; disagreement Left.",
  "a4g",  "Statism",            -2,       -1,        1,        2, "PASS", "Agreement that sons and daughters should have equal inheritance is coded Left; disagreement Right.",
  "q26a", "Statism",             2,        1,       -1,       -2, "PASS", "Agreement that individuals, rather than government, are responsible for poverty is coded Right; disagreement Left."
)

score_long_2009 <- item_specs_2009 |>
  select(item, axis, audit_verdict, direction_rationale, starts_with("score_")) |>
  pivot_longer(
    starts_with("score_"),
    names_to = "raw_code_name",
    values_to = "oriented_score"
  ) |>
  mutate(
    raw_code = as.numeric(str_remove(raw_code_name, "score_")),
    ideology_bucket = bucket_from_oriented(oriented_score)
  ) |>
  select(
    item, axis, raw_code, oriented_score, ideology_bucket,
    audit_verdict, direction_rationale
  )

item_audit_2009 <- map_dfr(
  item_specs_2009$item,
  function(item) {
    spec <- item_specs_2009 |> filter(.data$item == .env$item)
    labels <- value_label_table(raw09[[item]])

    # Ensure codes observed in the raw data are represented even if an SPSS
    # value label is absent.
    observed_codes <- tibble(
      raw_code = sort(unique(as.numeric(raw09[[item]])))
    ) |>
      filter(!is.na(raw_code))

    full_codes <- full_join(labels, observed_codes, by = "raw_code") |>
      arrange(raw_code) |>
      mutate(
        response_label = coalesce(response_label, paste0("Unlabelled code ", raw_code))
      )

    full_codes |>
      left_join(
        score_long_2009 |> filter(.data$item == .env$item),
        by = "raw_code"
      ) |>
      transmute(
        item = item,
        axis = spec$axis,
        question_wording = variable_label(raw09[[item]]),
        raw_code,
        response_label,
        oriented_score,
        ideology_bucket,
        audit_verdict = spec$audit_verdict,
        direction_rationale = spec$direction_rationale
      )
  }
)

write_csv(
  item_audit_2009,
  file.path(OUT_DIR, "01_2009_item_by_item_coding_audit.csv")
)

# ------------------------------------------------------------
# 4. Fresh 2009 reconstruction
# ------------------------------------------------------------

d09 <- raw09 |>
  mutate(
    year = 2009L,
    respondent_uid = paste0("2009_", row_number()),
    respondent_source_id = as.character(res),

    a4b_raw = as.numeric(a4b),
    a4c_raw = as.numeric(a4c),
    a4d_raw = as.numeric(a4d),
    a4g_raw = as.numeric(a4g),
    q26a_raw = as.numeric(q26a),

    # Existing 2009 orientation, audited against wording above.
    a4b_oriented = case_when(
      a4b_raw == 1 ~ -2,
      a4b_raw == 2 ~ -1,
      a4b_raw == 3 ~  1,
      a4b_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),
    a4c_oriented = case_when(
      a4c_raw == 1 ~  2,
      a4c_raw == 2 ~  1,
      a4c_raw == 3 ~ -1,
      a4c_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),
    a4d_oriented = case_when(
      a4d_raw == 1 ~  2,
      a4d_raw == 2 ~  1,
      a4d_raw == 3 ~ -1,
      a4d_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),
    a4g_oriented = case_when(
      a4g_raw == 1 ~ -2,
      a4g_raw == 2 ~ -1,
      a4g_raw == 3 ~  1,
      a4g_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),
    q26a_oriented = case_when(
      q26a_raw == 1 ~  2,
      q26a_raw == 2 ~  1,
      q26a_raw == 3 ~ -1,
      q26a_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    a4b_bucket = bucket_from_oriented(a4b_oriented),
    a4c_bucket = bucket_from_oriented(a4c_oriented),
    a4d_bucket = bucket_from_oriented(a4d_oriented),
    a4g_bucket = bucket_from_oriented(a4g_oriented),
    q26a_bucket = bucket_from_oriented(q26a_oriented),

    ideology_complete =
      !is.na(a4b_bucket) &
      !is.na(a4c_bucket) &
      !is.na(a4d_bucket) &
      !is.na(a4g_bucket) &
      !is.na(q26a_bucket),

    recognition_left_n =
      as.integer(a4b_bucket == "Left") +
      as.integer(a4c_bucket == "Left"),
    recognition_center_n =
      as.integer(a4b_bucket == "Center") +
      as.integer(a4c_bucket == "Center"),
    recognition_right_n =
      as.integer(a4b_bucket == "Right") +
      as.integer(a4c_bucket == "Right"),

    statism_left_n =
      as.integer(a4d_bucket == "Left") +
      as.integer(a4g_bucket == "Left") +
      as.integer(q26a_bucket == "Left"),
    statism_center_n =
      as.integer(a4d_bucket == "Center") +
      as.integer(a4g_bucket == "Center") +
      as.integer(q26a_bucket == "Center"),
    statism_right_n =
      as.integer(a4d_bucket == "Right") +
      as.integer(a4g_bucket == "Right") +
      as.integer(q26a_bucket == "Right"),

    # Strict all-5-item definition retained as an audit/robustness measure.
    ideology_2009_strict = case_when(
      !ideology_complete ~ NA_character_,
      recognition_left_n == 2 & statism_left_n == 3 ~ "Left",
      recognition_center_n == 2 & statism_center_n == 3 ~ "Center",
      recognition_right_n == 2 & statism_right_n == 3 ~ "Right",
      TRUE ~ "Mixed"
    ),

    # MAIN cross-year 2009 rule used by the paper-output pipeline:
    # 2/2 recognition + at least 2/3 statism in same bucket.
    ideology_main = case_when(
      !ideology_complete ~ NA_character_,
      recognition_left_n == 2 & statism_left_n >= 2 ~ "Left",
      recognition_center_n == 2 & statism_center_n >= 2 ~ "Center",
      recognition_right_n == 2 & statism_right_n >= 2 ~ "Right",
      TRUE ~ "Mixed"
    ),

    vote_code = as.numeric(q1a),
    vote_label = as.character(as_factor(q1a, levels = "labels")),

    # In NES 2009 Q1a:
    # 98 = Don't know
    # 99 = Blank/Refused
    # 96 = Independent and 97 = Other Smaller Parties are substantive votes.
    vote_valid =
      !is.na(vote_code) &
      !vote_code %in% c(98, 99),

    voted_bjp = case_when(
      !vote_valid ~ NA_integer_,
      vote_code == 2 ~ 1L,
      TRUE ~ 0L
    ),

    survey_weight_raw = as.numeric(stpop1),
    survey_weight = positive_weight(stpop1)
  )

# ------------------------------------------------------------
# 5. 2009 response distributions for audit
# ------------------------------------------------------------

responses_2009 <- map_dfr(
  item_specs_2009$item,
  function(item) {
    raw_col <- paste0(item, "_raw")
    oriented_col <- paste0(item, "_oriented")
    bucket_col <- paste0(item, "_bucket")

    d09 |>
      transmute(
        item = .env$item,
        raw_code = .data[[raw_col]],
        oriented_score = .data[[oriented_col]],
        ideology_bucket = .data[[bucket_col]],
        survey_weight
      ) |>
      group_by(item, raw_code, oriented_score, ideology_bucket) |>
      summarise(
        n = n(),
        weighted_n = sum(survey_weight, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        question_wording = variable_label(raw09[[item]])
      )
  }
) |>
  group_by(item) |>
  mutate(
    pct_unweighted = 100 * n / sum(n),
    pct_weighted = 100 * weighted_n / sum(weighted_n)
  ) |>
  ungroup() |>
  left_join(
    item_audit_2009 |>
      select(item, raw_code, response_label) |>
      distinct(),
    by = c("item", "raw_code")
  ) |>
  arrange(item, raw_code)

write_csv(
  responses_2009,
  file.path(OUT_DIR, "02_2009_item_response_distributions.csv")
)

# ------------------------------------------------------------
# 6. 2009 weight audit
# ------------------------------------------------------------

weight09_label <- variable_label(raw09$stpop1)

weight_audit_2009 <- tibble(
  year = 2009L,
  variable = "stpop1",
  variable_label = weight09_label,
  n_respondents = nrow(d09),
  n_valid_positive = sum(!is.na(d09$survey_weight)),
  n_missing_nonfinite_or_nonpositive = sum(is.na(d09$survey_weight)),
  n_unique_valid = n_distinct(d09$survey_weight, na.rm = TRUE),
  minimum = if (any(!is.na(d09$survey_weight))) min(d09$survey_weight, na.rm = TRUE) else NA_real_,
  median = if (any(!is.na(d09$survey_weight))) median(d09$survey_weight, na.rm = TRUE) else NA_real_,
  mean = if (any(!is.na(d09$survey_weight))) mean(d09$survey_weight, na.rm = TRUE) else NA_real_,
  maximum = if (any(!is.na(d09$survey_weight))) max(d09$survey_weight, na.rm = TRUE) else NA_real_
)

write_csv(
  weight_audit_2009,
  file.path(OUT_DIR, "03_2009_stpop1_weight_audit.csv")
)

# ------------------------------------------------------------
# 7. 2009 classification summary and strict-vs-main transition
# ------------------------------------------------------------

classification_summary_2009 <- bind_rows(
  d09 |>
    count(ideology_2009_strict, name = "n") |>
    mutate(definition = "Strict: 2/2 recognition + 3/3 statism"),
  d09 |>
    count(ideology_main, name = "n") |>
    rename(ideology_2009_strict = ideology_main) |>
    mutate(definition = "Main harmonized: 2/2 recognition + >=2/3 statism")
) |>
  rename(ideology = ideology_2009_strict) |>
  mutate(
    ideology = replace_na(ideology, "Missing required item"),
    pct_full_sample = 100 * n / nrow(d09)
  ) |>
  select(definition, ideology, n, pct_full_sample)

write_csv(
  classification_summary_2009,
  file.path(OUT_DIR, "04_2009_classification_summary.csv")
)

transition_2009 <- d09 |>
  transmute(
    strict = replace_na(ideology_2009_strict, "Missing"),
    main_harmonized = replace_na(ideology_main, "Missing"),
    survey_weight
  ) |>
  group_by(strict, main_harmonized) |>
  summarise(
    n = n(),
    weighted_n = sum(survey_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(strict) |>
  mutate(
    pct_of_strict_group_unweighted = 100 * n / sum(n),
    pct_of_strict_group_weighted = 100 * weighted_n / sum(weighted_n)
  ) |>
  ungroup()

write_csv(
  transition_2009,
  file.path(OUT_DIR, "05_2009_strict_to_main_harmonized_transition.csv")
)

# ------------------------------------------------------------
# 8. Fresh corrected 2014 reconstruction
# ------------------------------------------------------------

d14 <- raw14 |>
  mutate(
    year = 2014L,
    respondent_uid = paste0("2014_", row_number()),
    respondent_source_id = as.character(resno),

    q10b_raw = as.numeric(q10b),
    q10e_raw = as.numeric(q10e),
    q23c_raw = as.numeric(q23c),

    q10b_oriented = case_when(
      q10b_raw == 1 ~  2,
      q10b_raw == 2 ~  1,
      q10b_raw == 3 ~ -1,
      q10b_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    q10e_oriented = case_when(
      q10e_raw == 1 ~ -2,
      q10e_raw == 2 ~ -1,
      q10e_raw == 3 ~  1,
      q10e_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),

    # CORRECTED relative to the old project code.
    q23c_oriented = case_when(
      q23c_raw == 1 ~  2,
      q23c_raw == 2 ~  1,
      q23c_raw == 3 ~ -1,
      q23c_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    q10b_bucket = bucket_from_oriented(q10b_oriented),
    q10e_bucket = bucket_from_oriented(q10e_oriented),
    q23c_bucket = bucket_from_oriented(q23c_oriented),

    ideology_complete =
      !is.na(q10b_bucket) &
      !is.na(q10e_bucket) &
      !is.na(q23c_bucket),

    ideology_main = case_when(
      !ideology_complete ~ NA_character_,
      q10b_bucket == "Left" &
        q10e_bucket == "Left" &
        q23c_bucket == "Left" ~ "Left",
      q10b_bucket == "Center" &
        q10e_bucket == "Center" &
        q23c_bucket == "Center" ~ "Center",
      q10b_bucket == "Right" &
        q10e_bucket == "Right" &
        q23c_bucket == "Right" ~ "Right",
      TRUE ~ "Mixed"
    ),

    vote_code = as.numeric(q1a),
    vote_label = as.character(as_factor(q1a, levels = "labels")),

    vote_valid =
      !is.na(vote_code) &
      !vote_code %in% c(96, 98, 99),

    voted_bjp = case_when(
      !vote_valid ~ NA_integer_,
      vote_code == 2 ~ 1L,
      TRUE ~ 0L
    ),

    survey_weight_raw = as.numeric(stpop),
    survey_weight = positive_weight(stpop)
  )

# ------------------------------------------------------------
# 9. Cross-year BJP vote summary
# ------------------------------------------------------------

crossyear <- bind_rows(
  d09 |>
    select(
      year, respondent_uid, ideology_main,
      vote_valid, voted_bjp, survey_weight
    ),
  d14 |>
    select(
      year, respondent_uid, ideology_main,
      vote_valid, voted_bjp, survey_weight
    )
)

crossyear_summary <- crossyear |>
  filter(!is.na(ideology_main)) |>
  group_by(year, ideology = ideology_main) |>
  summarise(
    n_ideology = n(),
    pct_full_sample_unweighted = 100 * n() /
      if_else(first(year) == 2009L, nrow(d09), nrow(d14)),
    pct_full_sample_weighted = weighted_share_indicator(
      rep(TRUE, n()),
      survey_weight
    ),
    n_valid_vote = sum(vote_valid, na.rm = TRUE),
    n_bjp = sum(voted_bjp == 1, na.rm = TRUE),
    bjp_vote_pct_unweighted =
      if_else(
        n_valid_vote > 0,
        100 * mean(voted_bjp, na.rm = TRUE),
        NA_real_
      ),
    bjp_vote_pct_weighted =
      weighted_pct(voted_bjp, survey_weight),
    .groups = "drop"
  ) |>
  arrange(
    year,
    factor(ideology, levels = c("Left", "Center", "Right", "Mixed"))
  )

# Correct the weighted full-sample category shares using full-year denominators.
year_weight_totals <- bind_rows(
  d09 |> summarise(year = 2009L, total_weight = sum(survey_weight, na.rm = TRUE)),
  d14 |> summarise(year = 2014L, total_weight = sum(survey_weight, na.rm = TRUE))
)

category_weight_totals <- crossyear |>
  filter(!is.na(ideology_main)) |>
  group_by(year, ideology = ideology_main) |>
  summarise(category_weight = sum(survey_weight, na.rm = TRUE), .groups = "drop")

crossyear_summary <- crossyear_summary |>
  select(-pct_full_sample_weighted) |>
  left_join(category_weight_totals, by = c("year", "ideology")) |>
  left_join(year_weight_totals, by = "year") |>
  mutate(
    pct_full_sample_weighted = 100 * category_weight / total_weight
  ) |>
  select(-category_weight, -total_weight)

write_csv(
  crossyear_summary,
  file.path(OUT_DIR, "06_crossyear_bjp_vote_by_ideology.csv")
)

# ------------------------------------------------------------
# 10. Mixed-category footnote audit
# ------------------------------------------------------------

mixed_summary <- crossyear_summary |>
  filter(ideology == "Mixed") |>
  transmute(
    year,
    mixed_n = n_ideology,
    mixed_pct_full_sample_unweighted = pct_full_sample_unweighted,
    mixed_pct_full_sample_weighted = pct_full_sample_weighted,
    mixed_n_valid_vote = n_valid_vote,
    mixed_n_bjp = n_bjp,
    mixed_bjp_vote_pct_unweighted = bjp_vote_pct_unweighted,
    mixed_bjp_vote_pct_weighted = bjp_vote_pct_weighted
  )

write_csv(
  mixed_summary,
  file.path(OUT_DIR, "07_mixed_category_footnote_audit.csv")
)

mixed09 <- mixed_summary |> filter(year == 2009)
mixed14 <- mixed_summary |> filter(year == 2014)

if (nrow(mixed09) != 1 || nrow(mixed14) != 1) {
  stop("Mixed-category summary did not produce exactly one row per year.")
}

mixed_note <- paste0(
  "Mixed respondents are omitted from the bars. ",
  "2009: n=", comma(mixed09$mixed_n),
  " (", number(mixed09$mixed_pct_full_sample_unweighted, accuracy = 0.1), "% of the full sample); ",
  number(mixed09$mixed_bjp_vote_pct_weighted, accuracy = 0.1),
  "% of Mixed respondents with valid reported votes voted BJP (weighted). ",
  "2014: n=", comma(mixed14$mixed_n),
  " (", number(mixed14$mixed_pct_full_sample_unweighted, accuracy = 0.1), "% of the full sample); ",
  number(mixed14$mixed_bjp_vote_pct_weighted, accuracy = 0.1),
  "% voted BJP (weighted)."
)

method_note <- paste0(
  "2009 classification: 2/2 recognition items + at least 2/3 statism items in the same bucket. ",
  "2014 classification: Q10b + Q10e + corrected Q23c all support the same bucket. ",
  "Weights: stpop1 (2009) and stpop (2014)."
)

writeLines(
  c(mixed_note, method_note),
  file.path(OUT_DIR, "08_figure_notes.txt")
)

# ------------------------------------------------------------
# 11. Optional stale-code comparison against derived respondent RDS
#
# Compare the EXISTING 2009 voter_ideology field with the freshly rebuilt
# STRICT 2009 rule, because the base nes.R voter_ideology is the strict rule.
# This is separate from the main 2/3-statism harmonized plot rule.
# ------------------------------------------------------------

DERIVED_RDS <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "switchers_rewrite",
  "intermediate",
  "nes_respondent_clean.rds"
)

if (file.exists(DERIVED_RDS)) {
  old <- readRDS(DERIVED_RDS) |>
    filter(year == 2009)

  if (
    all(c("respondent_uid", "voter_ideology", "voted_bjp") %in% names(old))
  ) {
    stale_check <- d09 |>
      select(
        respondent_uid,
        ideology_2009_strict_fresh = ideology_2009_strict,
        voted_bjp_fresh = voted_bjp
      ) |>
      left_join(
        old |>
          transmute(
            respondent_uid,
            ideology_2009_strict_existing =
              normalize_old_ideology(voter_ideology),
            voted_bjp_existing = as.numeric(voted_bjp)
          ),
        by = "respondent_uid"
      )

    stale_summary <- stale_check |>
      summarise(
        n_raw_2009 = n(),
        n_ideology_disagreements = sum(
          !is.na(ideology_2009_strict_fresh) &
            !is.na(ideology_2009_strict_existing) &
            ideology_2009_strict_fresh != ideology_2009_strict_existing
        ),
        n_bjp_vote_disagreements = sum(
          !is.na(voted_bjp_fresh) &
            !is.na(voted_bjp_existing) &
            voted_bjp_fresh != voted_bjp_existing
        )
      )

    write_csv(
      stale_summary,
      file.path(OUT_DIR, "09_existing_vs_fresh_2009_strict_summary.csv")
    )

    write_csv(
      stale_check |>
        filter(
          (
            !is.na(ideology_2009_strict_fresh) &
              !is.na(ideology_2009_strict_existing) &
              ideology_2009_strict_fresh != ideology_2009_strict_existing
          ) |
            (
              !is.na(voted_bjp_fresh) &
                !is.na(voted_bjp_existing) &
                voted_bjp_fresh != voted_bjp_existing
            )
        ),
      file.path(OUT_DIR, "10_existing_vs_fresh_2009_strict_disagreements.csv")
    )
  } else {
    message(
      "Existing respondent RDS found but required comparison fields are absent."
    )
  }
} else {
  message(
    "No existing nes_respondent_clean.rds found; raw 2009 audit still completed."
  )
}

# ------------------------------------------------------------
# 12. Plot data: omit Mixed from bars, retain it in figure note
# ------------------------------------------------------------

plot_data <- crossyear_summary |>
  filter(ideology %in% c("Left", "Center", "Right")) |>
  mutate(
    ideology = factor(ideology, levels = c("Left", "Center", "Right")),
    year = factor(year, levels = c(2009, 2014)),
    label = percent(bjp_vote_pct_weighted / 100, accuracy = 0.1)
  )

write_csv(
  plot_data,
  file.path(OUT_DIR, "11_plot_data_left_center_right.csv")
)

y_max <- max(plot_data$bjp_vote_pct_weighted, na.rm = TRUE)
y_limit <- max(60, ceiling((y_max + 7) / 5) * 5)

p <- ggplot(
  plot_data,
  aes(x = ideology, y = bjp_vote_pct_weighted / 100, fill = year)
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.72
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.78),
    vjust = -0.25,
    size = 4.8
  ) +
  scale_fill_manual(
    values = c("2009" = "#F8766D", "2014" = "#00BFC4"),
    name = "Election year"
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, y_limit / 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "BJP vote share by ideological bucket, 2009 and 2014",
    subtitle = "Survey-weighted NES respondents with valid reported votes; audited harmonized ideology classification",
    x = NULL,
    y = "Weighted share voting BJP",
    caption = paste(mixed_note, method_note, sep = "\n")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 22, face = "plain"),
    plot.subtitle = element_text(size = 16, margin = margin(b = 14)),
    plot.caption = element_text(
      size = 9.5,
      hjust = 0,
      margin = margin(t = 15)
    ),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(
      title.position = "left",
      title.hjust = 0.5
    )
  )

ggsave(
  file.path(OUT_DIR, "12_bjp_vote_share_by_ideology_2009_2014_audited.png"),
  p,
  width = 13.5,
  height = 9.5,
  dpi = 400
)

ggsave(
  file.path(OUT_DIR, "12_bjp_vote_share_by_ideology_2009_2014_audited.pdf"),
  p,
  width = 13.5,
  height = 9.5,
  device = cairo_pdf
)

# ------------------------------------------------------------
# 13. Console readout
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("2009 ITEM CODING AUDIT\n")
cat("============================================================\n")
print(
  item_specs_2009 |>
    select(item, axis, audit_verdict, direction_rationale),
  n = Inf,
  width = Inf
)

cat("\n============================================================\n")
cat("2009 CLASSIFICATION SUMMARY\n")
cat("============================================================\n")
print(classification_summary_2009, n = Inf, width = Inf)

cat("\n============================================================\n")
cat("CROSS-YEAR BJP VOTE SUMMARY\n")
cat("============================================================\n")
print(
  crossyear_summary |>
    select(
      year, ideology, n_ideology, n_valid_vote, n_bjp,
      bjp_vote_pct_unweighted, bjp_vote_pct_weighted
    ),
  n = Inf,
  width = Inf
)

cat("\n============================================================\n")
cat("MIXED CATEGORY NOTE\n")
cat("============================================================\n")
cat(mixed_note, "\n")

cat("\n============================================================\n")
cat("WEIGHT NOTE\n")
cat("2009 stpop1 label: ", weight09_label, "\n", sep = "")
cat("2014 stpop label: ", variable_label(raw14$stpop), "\n", sep = "")

cat("\nAudit and figure complete.\n")
cat("Outputs: ", normalizePath(OUT_DIR, mustWork = FALSE), "\n", sep = "")

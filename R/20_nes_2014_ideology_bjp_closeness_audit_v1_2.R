# NES 2014 ideology / BJP vote / party closeness audit
# Version 1.2
#
# Purpose:
#   1. Reconstruct the 2014 ideology classification from raw NES items.
#   2. Correct Q23c orientation so support for curbing strikes is right-coded.
#   3. Compare three-question (Q10b + Q10e + Q23c) and
#      two-question (Q10e + Q23c) classifications.
#   4. Audit reported BJP voting from raw Q1a.
#   5. Summarize Q11/Q11a party closeness by ideology.
#   6. Report both unweighted and candidate stpop-weighted percentages.
#   7. Optionally compare fresh coding with an existing derived respondent file.
#
# Run from the Switchers-India project root.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

PROJECT_ROOT <- "."
NES_FILE <- file.path(PROJECT_ROOT, "data", "lokniti", "nes_2014.sav")

OUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "nes_2014_ideology_audit_v1_2"
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(NES_FILE)) {
  stop("NES 2014 file not found: ", NES_FILE)
}

raw <- read_sav(NES_FILE)

required <- c(
  "resno", "q1a", "q10b", "q10e", "q11", "q11a", "q23c", "stpop"
)

missing_required <- setdiff(required, names(raw))
if (length(missing_required) > 0) {
  stop(
    "NES 2014 is missing required variables: ",
    paste(missing_required, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

bucket_from_oriented <- function(x) {
  case_when(
    x == -2 ~ "Left",
    x %in% c(-1, 1) ~ "Center",
    x == 2 ~ "Right",
    TRUE ~ NA_character_
  )
}

weighted_pct <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  100 * weighted.mean(x[ok], w[ok])
}

normalize_old_ideology <- function(x) {
  # Used ONLY for the optional comparison with an older derived RDS.
  # It prevents "L" vs "Left" (etc.) from being counted as a coding disagreement.
  case_when(
    as.character(x) %in% c("L", "Left") ~ "Left",
    as.character(x) %in% c("C", "Center", "Centrist") ~ "Center",
    as.character(x) %in% c("R", "Right") ~ "Right",
    as.character(x) %in% c("M", "Mixed") ~ "Mixed",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# Fresh reconstruction from raw NES 2014 variables
# ------------------------------------------------------------

audit <- raw %>%
  mutate(
    respondent_uid = paste0("2014_", row_number()),
    respondent_source_id = as.character(resno),

    q10b_raw = as.numeric(q10b),
    q10e_raw = as.numeric(q10e),
    q23c_raw = as.numeric(q23c),

    # Q10b:
    # "Reservations based on caste and religion divide the people of India?"
    # Agreement is right-coded.
    q10b_oriented = case_when(
      q10b_raw == 1 ~  2,
      q10b_raw == 2 ~  1,
      q10b_raw == 3 ~ -1,
      q10b_raw == 4 ~ -2,
      TRUE ~ NA_real_
    ),

    # Q10e:
    # "The government should make special provision to accommodate minorities?"
    # Agreement is left-coded.
    q10e_oriented = case_when(
      q10e_raw == 1 ~ -2,
      q10e_raw == 2 ~ -1,
      q10e_raw == 3 ~  1,
      q10e_raw == 4 ~  2,
      TRUE ~ NA_real_
    ),

    # Q23c:
    # "Government should strongly curb strikes by workers and employees?"
    #
    # IMPORTANT CORRECTION:
    # Agreement is right-coded; disagreement is left-coded.
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

    # Strict three-question scheme.
    ideology_3q = case_when(
      is.na(q10b_bucket) |
        is.na(q10e_bucket) |
        is.na(q23c_bucket) ~ NA_character_,
      q10b_bucket == q10e_bucket &
        q10e_bucket == q23c_bucket ~ q10e_bucket,
      TRUE ~ "Mixed"
    ),

    # Strict two-question sensitivity scheme.
    ideology_2q = case_when(
      is.na(q10e_bucket) |
        is.na(q23c_bucket) ~ NA_character_,
      q10e_bucket == q23c_bucket ~ q10e_bucket,
      TRUE ~ "Mixed"
    ),

    # Q1a reported vote.
    vote_code = as.numeric(q1a),
    vote_label = as.character(as_factor(q1a, levels = "labels")),

    # 2014 Q1a:
    # 96 = Don't want to reveal
    # 98 = Don't know / Can't say / No response
    # 99 = N.A.
    # These are missing for vote-choice analysis, not non-BJP votes.
    vote_valid =
      !is.na(vote_code) &
      !vote_code %in% c(96, 98, 99),

    voted_bjp_fresh = case_when(
      !vote_valid ~ NA_integer_,
      vote_code == 2 ~ 1L,
      TRUE ~ 0L
    ),

    # Q11/Q11a party closeness.
    # 2014 Q11: 1 = No, 2 = Yes.
    q11_code = as.numeric(q11),
    q11a_code = as.numeric(q11a),
    close_party_label = as.character(as_factor(q11a, levels = "labels")),

    reports_close_any = case_when(
      q11_code == 1 ~ 0L,
      q11_code == 2 ~ 1L,
      TRUE ~ NA_integer_
    ),

    # For party-specific closeness:
    # Q11 = No gives a valid 0.
    # Q11 = Yes + a substantive Q11a response gives 1/0 by party.
    # Q11 = Yes + 96/98/99 or missing Q11a remains missing.
    reports_close_bjp = case_when(
      q11_code == 1 ~ 0L,
      q11_code == 2 & q11a_code == 2 ~ 1L,
      q11_code == 2 &
        !is.na(q11a_code) &
        !q11a_code %in% c(96, 98, 99) ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Q11a = 97 ("Other parties") is retained as a substantive
    # non-BJP closeness response, although it does not identify a named party.
    valid_close_party_response =
      q11_code == 2 &
      !is.na(q11a_code) &
      !q11a_code %in% c(96, 98, 99),

    # Candidate survey weight. We report it as stpop-weighted, not as a
    # fully validated official survey weight without documentation.
    weight_stpop_raw = as.numeric(stpop),
    weight_stpop = if_else(
      is.finite(weight_stpop_raw) & weight_stpop_raw > 0,
      weight_stpop_raw,
      NA_real_
    )
  )

# Mean-one normalization is useful for diagnostics and regression weighting.
# It does NOT change weighted percentages because every weight is multiplied
# by the same constant.
mean_valid_stpop <- mean(audit$weight_stpop, na.rm = TRUE)

audit <- audit %>%
  mutate(
    weight_stpop_norm = if_else(
      !is.na(weight_stpop) &
        is.finite(mean_valid_stpop) &
        mean_valid_stpop > 0,
      weight_stpop / mean_valid_stpop,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 01. Weight audit
# ------------------------------------------------------------

weight_label <- attr(raw$stpop, "label")
if (is.null(weight_label) || length(weight_label) == 0 || is.na(weight_label)) {
  weight_label <- ""
}

weight_audit <- tibble(
  variable = "stpop",
  variable_label = as.character(weight_label),
  n_respondents = nrow(audit),
  n_valid_positive = sum(!is.na(audit$weight_stpop)),
  n_missing_nonfinite_or_nonpositive = sum(is.na(audit$weight_stpop)),
  n_unique_valid = n_distinct(audit$weight_stpop, na.rm = TRUE),
  minimum = if (any(!is.na(audit$weight_stpop))) min(audit$weight_stpop, na.rm = TRUE) else NA_real_,
  median = if (any(!is.na(audit$weight_stpop))) median(audit$weight_stpop, na.rm = TRUE) else NA_real_,
  mean = if (any(!is.na(audit$weight_stpop))) mean(audit$weight_stpop, na.rm = TRUE) else NA_real_,
  maximum = if (any(!is.na(audit$weight_stpop))) max(audit$weight_stpop, na.rm = TRUE) else NA_real_,
  normalized_mean = if (any(!is.na(audit$weight_stpop_norm))) mean(audit$weight_stpop_norm, na.rm = TRUE) else NA_real_
)

write_csv(
  weight_audit,
  file.path(OUT_DIR, "01_stpop_weight_audit.csv")
)

# ------------------------------------------------------------
# 02. BJP vote by ideology under both schemes
# ------------------------------------------------------------

long_ideology <- audit %>%
  select(
    respondent_uid,
    ideology_3q,
    ideology_2q,
    voted_bjp_fresh,
    weight_stpop
  ) %>%
  pivot_longer(
    c(ideology_3q, ideology_2q),
    names_to = "scheme",
    values_to = "ideology"
  ) %>%
  mutate(
    scheme = recode(
      scheme,
      ideology_3q = "Three-question: Q10b + Q10e + Q23c",
      ideology_2q = "Two-question: Q10e + Q23c"
    )
  )

bjp_vote_by_ideology <- long_ideology %>%
  filter(ideology %in% c("Left", "Center", "Right")) %>%
  group_by(scheme, ideology) %>%
  summarise(
    n_ideology = n(),
    n_valid_vote = sum(!is.na(voted_bjp_fresh)),
    n_bjp = sum(voted_bjp_fresh == 1, na.rm = TRUE),
    bjp_vote_pct_unweighted =
      if_else(
        n_valid_vote > 0,
        100 * mean(voted_bjp_fresh, na.rm = TRUE),
        NA_real_
      ),
    bjp_vote_pct_stpop_weighted =
      weighted_pct(voted_bjp_fresh, weight_stpop),
    .groups = "drop"
  ) %>%
  arrange(
    scheme,
    factor(ideology, levels = c("Left", "Center", "Right"))
  )

write_csv(
  bjp_vote_by_ideology,
  file.path(OUT_DIR, "02_bjp_vote_by_ideology_scheme.csv")
)

cat("\n=== BJP vote by ideology ===\n")
print(bjp_vote_by_ideology, n = Inf, width = Inf)

# ------------------------------------------------------------
# 03. Raw-response audit of three-question Left BJP voters
# ------------------------------------------------------------

left_bjp_raw <- audit %>%
  filter(
    ideology_3q == "Left",
    voted_bjp_fresh == 1
  ) %>%
  transmute(
    respondent_uid,
    respondent_source_id,

    q10b_raw,
    q10b_response = as.character(as_factor(q10b, levels = "labels")),
    q10b_oriented,
    q10b_bucket,

    q10e_raw,
    q10e_response = as.character(as_factor(q10e, levels = "labels")),
    q10e_oriented,
    q10e_bucket,

    q23c_raw,
    q23c_response = as.character(as_factor(q23c, levels = "labels")),
    q23c_oriented,
    q23c_bucket,

    ideology_3q,
    ideology_2q,

    q1a_code = vote_code,
    q1a_party = vote_label,

    q11_code,
    q11_response = as.character(as_factor(q11, levels = "labels")),
    q11a_code,
    q11a_party = close_party_label,

    stpop = weight_stpop
  )

write_csv(
  left_bjp_raw,
  file.path(OUT_DIR, "03_three_question_left_bjp_raw_responses.csv")
)

# ------------------------------------------------------------
# 04. Classification transition: 3q -> 2q
# ------------------------------------------------------------

classification_transition <- audit %>%
  mutate(
    ideology_3q_display = replace_na(ideology_3q, "Missing"),
    ideology_2q_display = replace_na(ideology_2q, "Missing")
  ) %>%
  group_by(ideology_3q_display, ideology_2q_display) %>%
  summarise(
    n = n(),
    sum_stpop = sum(weight_stpop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ideology_3q_display) %>%
  mutate(
    n_3q_group = sum(n),
    stpop_3q_group = sum(sum_stpop),
    pct_of_3q_group_unweighted = 100 * n / n_3q_group,
    pct_of_3q_group_stpop_weighted = if_else(
      stpop_3q_group > 0,
      100 * sum_stpop / stpop_3q_group,
      NA_real_
    )
  ) %>%
  ungroup()

write_csv(
  classification_transition,
  file.path(OUT_DIR, "04_classification_transition_3q_to_2q.csv")
)

# ------------------------------------------------------------
# 05. Any-party and BJP closeness by ideology
# ------------------------------------------------------------

closeness_summary <- audit %>%
  select(
    ideology_3q,
    ideology_2q,
    reports_close_any,
    reports_close_bjp,
    weight_stpop
  ) %>%
  pivot_longer(
    c(ideology_3q, ideology_2q),
    names_to = "scheme",
    values_to = "ideology"
  ) %>%
  mutate(
    scheme = recode(
      scheme,
      ideology_3q = "Three-question: Q10b + Q10e + Q23c",
      ideology_2q = "Two-question: Q10e + Q23c"
    )
  ) %>%
  filter(ideology %in% c("Left", "Center", "Right")) %>%
  group_by(scheme, ideology) %>%
  summarise(
    n_ideology = n(),

    n_valid_q11 = sum(!is.na(reports_close_any)),
    n_close_any = sum(reports_close_any == 1, na.rm = TRUE),
    close_any_pct_unweighted =
      if_else(
        n_valid_q11 > 0,
        100 * mean(reports_close_any, na.rm = TRUE),
        NA_real_
      ),
    close_any_pct_stpop_weighted =
      weighted_pct(reports_close_any, weight_stpop),

    n_valid_bjp_closeness = sum(!is.na(reports_close_bjp)),
    n_close_bjp = sum(reports_close_bjp == 1, na.rm = TRUE),
    bjp_close_pct_unweighted =
      if_else(
        n_valid_bjp_closeness > 0,
        100 * mean(reports_close_bjp, na.rm = TRUE),
        NA_real_
      ),
    bjp_close_pct_stpop_weighted =
      weighted_pct(reports_close_bjp, weight_stpop),

    .groups = "drop"
  ) %>%
  arrange(
    scheme,
    factor(ideology, levels = c("Left", "Center", "Right"))
  )

write_csv(
  closeness_summary,
  file.path(OUT_DIR, "05_any_party_and_bjp_closeness_by_ideology.csv")
)

cat("\n=== Party closeness by ideology ===\n")
print(closeness_summary, n = Inf, width = Inf)

# ------------------------------------------------------------
# 06. Complete party-closeness distribution
#
# Two denominators:
#   A. pct_of_ideology_*:
#      share of respondents in the ideology group with valid Q11 who
#      explicitly identify this Q11a category.
#
#   B. pct_among_party_attached_*:
#      among respondents who answer Yes to Q11 and give a substantive
#      Q11a response, share giving this category.
# ------------------------------------------------------------

party_closeness_long <- audit %>%
  select(
    ideology_3q,
    ideology_2q,
    q11_code,
    q11a_code,
    close_party_label,
    reports_close_any,
    valid_close_party_response,
    weight_stpop
  ) %>%
  pivot_longer(
    c(ideology_3q, ideology_2q),
    names_to = "scheme",
    values_to = "ideology"
  ) %>%
  mutate(
    scheme = recode(
      scheme,
      ideology_3q = "Three-question: Q10b + Q10e + Q23c",
      ideology_2q = "Two-question: Q10e + Q23c"
    )
  ) %>%
  filter(ideology %in% c("Left", "Center", "Right"))

party_denominators <- party_closeness_long %>%
  group_by(scheme, ideology) %>%
  summarise(
    n_ideology = n(),

    n_valid_q11 = sum(!is.na(reports_close_any)),
    weight_valid_q11 = sum(
      weight_stpop[!is.na(reports_close_any)],
      na.rm = TRUE
    ),

    n_party_attached_with_substantive_q11a =
      sum(valid_close_party_response),

    weight_party_attached_with_substantive_q11a =
      sum(
        weight_stpop[valid_close_party_response],
        na.rm = TRUE
      ),

    .groups = "drop"
  )

party_closeness <- party_closeness_long %>%
  filter(valid_close_party_response) %>%
  mutate(
    close_party_label = if_else(
      !is.na(close_party_label) & close_party_label != "",
      close_party_label,
      paste0("Code ", q11a_code)
    )
  ) %>%
  group_by(
    scheme,
    ideology,
    q11a_code,
    close_party_label
  ) %>%
  summarise(
    n = n(),
    sum_stpop = sum(weight_stpop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    party_denominators,
    by = c("scheme", "ideology")
  ) %>%
  mutate(
    pct_of_ideology_unweighted =
      if_else(n_valid_q11 > 0, 100 * n / n_valid_q11, NA_real_),

    pct_of_ideology_stpop_weighted =
      if_else(
        weight_valid_q11 > 0,
        100 * sum_stpop / weight_valid_q11,
        NA_real_
      ),

    pct_among_party_attached_unweighted =
      if_else(
        n_party_attached_with_substantive_q11a > 0,
        100 * n / n_party_attached_with_substantive_q11a,
        NA_real_
      ),

    pct_among_party_attached_stpop_weighted =
      if_else(
        weight_party_attached_with_substantive_q11a > 0,
        100 * sum_stpop / weight_party_attached_with_substantive_q11a,
        NA_real_
      )
  ) %>%
  arrange(
    scheme,
    factor(ideology, levels = c("Left", "Center", "Right")),
    desc(pct_among_party_attached_unweighted)
  )

write_csv(
  party_closeness,
  file.path(OUT_DIR, "06_all_parties_closeness_by_ideology.csv")
)

# ------------------------------------------------------------
# 07. Compact comparison of Left BJP vote under the two schemes
# ------------------------------------------------------------

left_scheme_comparison <- bjp_vote_by_ideology %>%
  filter(ideology == "Left") %>%
  select(
    scheme,
    n_ideology,
    n_valid_vote,
    n_bjp,
    bjp_vote_pct_unweighted,
    bjp_vote_pct_stpop_weighted
  )

write_csv(
  left_scheme_comparison,
  file.path(OUT_DIR, "07_left_bjp_vote_three_vs_two_question.csv")
)

cat("\n=== Left BJP vote: three-question vs two-question ===\n")
print(left_scheme_comparison, n = Inf, width = Inf)

# ------------------------------------------------------------
# 08. Optional comparison against existing derived pipeline output
# ------------------------------------------------------------

DERIVED_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "switchers_rewrite",
  "intermediate",
  "nes_respondent_clean.rds"
)

if (file.exists(DERIVED_FILE)) {
  old <- readRDS(DERIVED_FILE) %>%
    filter(year == 2014)

  if (
    all(
      c("respondent_uid", "voter_ideology", "voted_bjp") %in%
        names(old)
    )
  ) {
    stale_check <- audit %>%
      select(
        respondent_uid,
        ideology_3q_fresh_corrected = ideology_3q,
        voted_bjp_fresh
      ) %>%
      left_join(
        old %>%
          transmute(
            respondent_uid,
            ideology_existing =
              normalize_old_ideology(voter_ideology),
            voted_bjp_existing = as.numeric(voted_bjp)
          ),
        by = "respondent_uid"
      )

    stale_summary <- stale_check %>%
      summarise(
        n_raw_2014 = n(),

        n_matched_existing =
          sum(
            !is.na(ideology_existing) |
              !is.na(voted_bjp_existing)
          ),

        n_ideology_disagreements =
          sum(
            !is.na(ideology_3q_fresh_corrected) &
              !is.na(ideology_existing) &
              ideology_3q_fresh_corrected != ideology_existing
          ),

        n_bjp_vote_disagreements =
          sum(
            !is.na(voted_bjp_fresh) &
              !is.na(voted_bjp_existing) &
              voted_bjp_fresh != voted_bjp_existing
          )
      )

    write_csv(
      stale_summary,
      file.path(
        OUT_DIR,
        "08_existing_vs_fresh_coding_summary.csv"
      )
    )

    write_csv(
      stale_check %>%
        filter(
          (
            !is.na(ideology_3q_fresh_corrected) &
              !is.na(ideology_existing) &
              ideology_3q_fresh_corrected != ideology_existing
          ) |
            (
              !is.na(voted_bjp_fresh) &
                !is.na(voted_bjp_existing) &
                voted_bjp_fresh != voted_bjp_existing
            )
        ),
      file.path(
        OUT_DIR,
        "09_existing_vs_fresh_coding_disagreements.csv"
      )
    )

    cat("\n=== Existing pipeline vs corrected fresh reconstruction ===\n")
    print(stale_summary)
  } else {
    message(
      "Existing respondent RDS found, but required comparison fields are absent."
    )
  }
} else {
  message(
    "No existing nes_respondent_clean.rds found; raw-data audit completed normally."
  )
}

cat(
  "\nAudit complete.\nOutputs written to:\n",
  normalizePath(OUT_DIR, mustWork = FALSE),
  "\n"
)

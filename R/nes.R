# NES 2009 and 2014 respondent cleaning, ideology classification, party
# closeness, and descriptive outputs.

nes_state_recode <- function(x) {
  dplyr::recode(
    as.numeric(x),
    `1` = 28, `2` = 12, `3` = 18, `4` = 10, `5` = 30, `6` = 24,
    `7` = 6, `8` = 2, `9` = 1, `10` = 29, `11` = 32, `12` = 23,
    `13` = 27, `14` = 14, `15` = 17, `16` = 15, `17` = 13,
    `18` = 21, `19` = 3, `20` = 8, `21` = 11, `22` = 33,
    `23` = 16, `24` = 9, `25` = 19, `26` = 35, `27` = 4,
    `28` = 26, `29` = 25, `30` = 7, `31` = 31, `32` = 34,
    `33` = 20, `34` = 22, `35` = 5, `36` = 28,
    .default = NA_real_
  )
}

ideology_item_bucket <- function(oriented_score) {
  dplyr::case_when(
    oriented_score == -2 ~ "Left",
    oriented_score %in% c(-1, 1) ~ "Center",
    oriented_score == 2 ~ "Right",
    TRUE ~ NA_character_
  )
}

strict_axis_bucket <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  apply(data, 1, function(values) {
    values <- as.character(values)
    if (any(is.na(values))) return(NA_character_)
    unique_values <- unique(values)
    if (length(unique_values) == 1) unique_values else "Mixed"
  })
}

combine_axis_buckets <- function(recognition, statism) {
  dplyr::case_when(
    is.na(recognition) | is.na(statism) ~ NA_character_,
    recognition == statism & recognition %in% c("Left", "Center", "Right") ~ recognition,
    TRUE ~ "Mixed"
  )
}

classification_status <- function(recognition, statism) {
  dplyr::case_when(
    is.na(recognition) | is.na(statism) ~ "missing_required_item",
    recognition == "Mixed" & statism == "Mixed" ~ "mixed_both_axes",
    recognition == "Mixed" ~ "mixed_within_recognition",
    statism == "Mixed" ~ "mixed_within_statism",
    recognition != statism ~ "axis_disagreement",
    recognition == "Left" ~ "pure_left",
    recognition == "Center" ~ "pure_center",
    recognition == "Right" ~ "pure_right",
    TRUE ~ NA_character_
  )
}

recode_vote_indicator <- function(vote_code, target_codes, invalid_codes) {
  vote_code <- as.numeric(vote_code)
  dplyr::case_when(
    is.na(vote_code) | vote_code %in% invalid_codes ~ NA_real_,
    vote_code %in% target_codes ~ 1,
    TRUE ~ 0
  )
}

recode_party_affinity <- function(
    gate_code,
    party_code,
    yes_code,
    no_code,
    target_codes,
    invalid_party_codes = c(96, 97, 98, 99)
) {
  gate_code <- as.numeric(gate_code)
  party_code <- as.numeric(party_code)
  dplyr::case_when(
    gate_code == no_code ~ 0,
    gate_code == yes_code & party_code %in% target_codes ~ 1,
    gate_code == yes_code & !is.na(party_code) & !party_code %in% invalid_party_codes ~ 0,
    TRUE ~ NA_real_
  )
}

harmonize_education <- function(label) {
  label_norm <- norm_name(label)
  dplyr::case_when(
    is.na(label_norm) | label_norm == "" ~ NA_character_,
    stringr::str_detect(label_norm, "ILLITERATE|NO SCHOOL|NOT LITERATE") ~ "No schooling / illiterate",
    stringr::str_detect(label_norm, "PRIMARY") & !stringr::str_detect(label_norm, "MIDDLE|SECONDARY") ~ "Primary",
    stringr::str_detect(label_norm, "MIDDLE") ~ "Middle",
    stringr::str_detect(label_norm, "MATRIC|SECONDARY|INTERMEDIATE|HIGHER SECONDARY|CLASS 10|CLASS 12") ~ "Secondary",
    stringr::str_detect(label_norm, "GRADUATE|COLLEGE|UNIVERSITY|DIPLOMA|POST GRADUATE|PROFESSIONAL") ~ "College or higher",
    TRUE ~ "Other / unclear"
  )
}

clean_group_label <- function(x) {
  label <- as.character(haven::as_factor(haven::zap_missing(x), levels = "labels"))
  dplyr::na_if(stringr::str_squish(label), "")
}

clean_2009_nes <- function(raw) {
  required <- c(
    "st_id", "pc_id", "ac_id", "ps_id", "res", "q1a", "q11", "q11a",
    "q12", "q12a", "z5", "z7", "z7a", "z8", "z18", "stpop1",
    "a4b", "a4c", "a4d", "a4g", "q26a", "a5", "a6a", "a6b", "q27"
  )
  assert_has_columns(raw, required, "NES 2009")

  raw |>
    dplyr::mutate(
      year = 2009L,
      respondent_uid = paste0("2009_", dplyr::row_number()),
      respondent_source_id = as.character(res),
      state_no = as.integer(nes_state_recode(st_id)),
      pc = as.integer(pc_id),
      ac = as.integer(ac_id),
      polling_station_id = as.character(ps_id),

      a4b_raw = as.numeric(a4b),
      a4c_raw = as.numeric(a4c),
      a4d_raw = as.numeric(a4d),
      a4g_raw = as.numeric(a4g),
      q26a_raw = as.numeric(q26a),
      a5_raw = as.numeric(a5),
      a6a_raw = as.numeric(a6a),
      a6b_raw = as.numeric(a6b),
      q27_raw = as.numeric(q27),

      a4b_oriented = dplyr::case_match(a4b_raw, 4 ~ 2, 3 ~ 1, 2 ~ -1, 1 ~ -2),
      a4c_oriented = dplyr::case_match(a4c_raw, 1 ~ 2, 2 ~ 1, 3 ~ -1, 4 ~ -2),
      a4d_oriented = dplyr::case_match(a4d_raw, 1 ~ 2, 2 ~ 1, 3 ~ -1, 4 ~ -2),
      a4g_oriented = dplyr::case_match(a4g_raw, 4 ~ 2, 3 ~ 1, 2 ~ -1, 1 ~ -2),
      q26a_oriented = dplyr::case_match(q26a_raw, 1 ~ 2, 2 ~ 1, 3 ~ -1, 4 ~ -2),

      a4b_bucket = ideology_item_bucket(a4b_oriented),
      a4c_bucket = ideology_item_bucket(a4c_oriented),
      a4d_bucket = ideology_item_bucket(a4d_oriented),
      a4g_bucket = ideology_item_bucket(a4g_oriented),
      q26a_bucket = ideology_item_bucket(q26a_oriented),

      recognition_n_required = 2L,
      statism_n_required = 3L,
      recognition_n_valid = rowSums(!is.na(dplyr::pick(a4b_bucket, a4c_bucket))),
      statism_n_valid = rowSums(!is.na(dplyr::pick(a4d_bucket, a4g_bucket, q26a_bucket))),
      recognition_axis = rowMeans(dplyr::pick(a4b_oriented, a4c_oriented), na.rm = TRUE),
      statism_axis = rowMeans(dplyr::pick(a4d_oriented, a4g_oriented, q26a_oriented), na.rm = TRUE),
      recognition_axis = dplyr::if_else(recognition_n_valid == recognition_n_required, recognition_axis, NA_real_),
      statism_axis = dplyr::if_else(statism_n_valid == statism_n_required, statism_axis, NA_real_),
      recognition_ideology = strict_axis_bucket(dplyr::pick(a4b_bucket, a4c_bucket)),
      statism_ideology = strict_axis_bucket(dplyr::pick(a4d_bucket, a4g_bucket, q26a_bucket)),
      voter_ideology = combine_axis_buckets(recognition_ideology, statism_ideology),
      ideology_classification_status = classification_status(recognition_ideology, statism_ideology),
      ideology_n_required_items = recognition_n_required + statism_n_required,
      ideology_n_valid_items = recognition_n_valid + statism_n_valid,
      ideology_complete = ideology_n_valid_items == ideology_n_required_items,

      education_code_raw = as.numeric(haven::zap_missing(z5)),
      education_label_raw = clean_group_label(z5),
      education_harmonized = harmonize_education(education_label_raw),
      education_below_college = dplyr::case_when(
        is.na(education_harmonized) ~ NA_real_,
        education_harmonized == "College or higher" ~ 0,
        TRUE ~ 1
      ),
      education_valid = !is.na(education_harmonized),

      household_income_monthly = as.numeric(haven::zap_missing(z18)),
      household_income_monthly = dplyr::if_else(
        is.finite(household_income_monthly) & household_income_monthly >= 0,
        household_income_monthly,
        NA_real_
      ),
      household_income_valid = !is.na(household_income_monthly),

      caste_code_raw = as.numeric(haven::zap_missing(z7a)),
      caste_label_raw = clean_group_label(z7),
      caste_group = clean_group_label(z7a),
      caste_valid = !is.na(caste_group),
      religion_code_raw = as.numeric(haven::zap_missing(z8)),
      religion_label_raw = clean_group_label(z8),
      religion_group = religion_label_raw,
      religion_valid = !is.na(religion_group),

      reported_vote_party_code = as.numeric(q1a),
      reported_vote_party_label = clean_group_label(q1a),
      voted_congress = recode_vote_indicator(q1a, 1, c(98, 99)),
      voted_bjp = recode_vote_indicator(q1a, 2, c(98, 99)),
      voted_shs = recode_vote_indicator(q1a, 46, c(98, 99)),
      voted_mns = recode_vote_indicator(q1a, 91, c(98, 99)),
      voted_fr = recode_vote_indicator(q1a, c(2, 46, 91), c(98, 99)),
      vote_valid = !is.na(voted_fr),

      close_congress = recode_party_affinity(q11, q11a, 1, 2, 1),
      close_bjp = recode_party_affinity(q11, q11a, 1, 2, 2),
      close_shs = recode_party_affinity(q11, q11a, 1, 2, 46),
      close_mns = recode_party_affinity(q11, q11a, 1, 2, 91),
      close_any_fr = recode_party_affinity(q11, q11a, 1, 2, c(2, 46, 91)),
      close_response_valid = !is.na(close_any_fr),
      close_fr_party_n = rowSums(dplyr::pick(close_bjp, close_shs, close_mns), na.rm = TRUE),
      close_fr_party_n = dplyr::if_else(!is.na(close_any_fr), close_fr_party_n, NA_real_),

      never_vote_bjp = recode_party_affinity(q12, q12a, 1, 2, 2),
      never_vote_shs = recode_party_affinity(q12, q12a, 1, 2, 46),
      never_vote_mns = recode_party_affinity(q12, q12a, 1, 2, 91),
      never_vote_any_fr = recode_party_affinity(q12, q12a, 1, 2, c(2, 46, 91)),
      never_vote_response_valid = !is.na(never_vote_any_fr),

      a5_binary = dplyr::case_match(a5_raw, 2 ~ 1, 1 ~ -1),
      a6a_binary = dplyr::case_match(a6a_raw, 2 ~ 1, 1 ~ -1),
      a6b_binary = dplyr::case_match(a6b_raw, 2 ~ 1, 1 ~ -1),
      q27_binary = dplyr::case_match(q27_raw, 2 ~ 1, 1 ~ -1),

      survey_weight_raw = get_survey_weight(raw, "stpop1"),
      survey_weight_validated = "stpop1" %in% names(raw)
    )
}

clean_2014_nes <- function(raw) {
  required <- c(
    "state_id", "pc_id", "ac_id", "ps_id", "resno", "q1a", "q11", "q11a",
    "z3", "z5", "z5a", "z6", "z13", "stpop", "q10b", "q10e", "q23c"
  )
  assert_has_columns(raw, required, "NES 2014")

  raw |>
    dplyr::mutate(
      year = 2014L,
      respondent_uid = paste0("2014_", dplyr::row_number()),
      respondent_source_id = as.character(resno),
      state_no = as.integer(nes_state_recode(state_id)),
      pc = as.integer(pc_id),
      ac = as.integer(ac_id),
      polling_station_id = as.character(ps_id),

      q10b_raw = as.numeric(q10b),
      q10e_raw = as.numeric(q10e),
      q23c_raw = as.numeric(q23c),
      q10b_oriented = dplyr::case_match(q10b_raw, 1 ~ 2, 2 ~ 1, 3 ~ -1, 4 ~ -2),
      q10e_oriented = dplyr::case_match(q10e_raw, 4 ~ 2, 3 ~ 1, 2 ~ -1, 1 ~ -2),
      q23c_oriented = dplyr::case_match(q23c_raw, 4 ~ 2, 3 ~ 1, 2 ~ -1, 1 ~ -2),
      q10b_bucket = ideology_item_bucket(q10b_oriented),
      q10e_bucket = ideology_item_bucket(q10e_oriented),
      q23c_bucket = ideology_item_bucket(q23c_oriented),

      recognition_n_required = 2L,
      statism_n_required = 1L,
      recognition_n_valid = rowSums(!is.na(dplyr::pick(q10b_bucket, q10e_bucket))),
      statism_n_valid = rowSums(!is.na(dplyr::pick(q23c_bucket))),
      recognition_axis = rowMeans(dplyr::pick(q10b_oriented, q10e_oriented), na.rm = TRUE),
      statism_axis = q23c_oriented,
      recognition_axis = dplyr::if_else(recognition_n_valid == recognition_n_required, recognition_axis, NA_real_),
      statism_axis = dplyr::if_else(statism_n_valid == statism_n_required, statism_axis, NA_real_),
      recognition_ideology = strict_axis_bucket(dplyr::pick(q10b_bucket, q10e_bucket)),
      statism_ideology = strict_axis_bucket(dplyr::pick(q23c_bucket)),
      voter_ideology = combine_axis_buckets(recognition_ideology, statism_ideology),
      ideology_classification_status = classification_status(recognition_ideology, statism_ideology),
      ideology_n_required_items = recognition_n_required + statism_n_required,
      ideology_n_valid_items = recognition_n_valid + statism_n_valid,
      ideology_complete = ideology_n_valid_items == ideology_n_required_items,

      education_code_raw = as.numeric(haven::zap_missing(z3)),
      education_label_raw = clean_group_label(z3),
      education_harmonized = harmonize_education(education_label_raw),
      education_below_college = dplyr::case_when(
        is.na(education_harmonized) ~ NA_real_,
        education_harmonized == "College or higher" ~ 0,
        TRUE ~ 1
      ),
      education_valid = !is.na(education_harmonized),

      household_income_monthly = as.numeric(haven::zap_missing(z13)),
      household_income_monthly = dplyr::if_else(
        is.finite(household_income_monthly) & household_income_monthly >= 0,
        household_income_monthly,
        NA_real_
      ),
      household_income_valid = !is.na(household_income_monthly),

      caste_code_raw = as.numeric(haven::zap_missing(z5a)),
      caste_label_raw = clean_group_label(z5),
      caste_group = clean_group_label(z5a),
      caste_valid = !is.na(caste_group),
      religion_code_raw = as.numeric(haven::zap_missing(z6)),
      religion_label_raw = clean_group_label(z6),
      religion_group = religion_label_raw,
      religion_valid = !is.na(religion_group),

      reported_vote_party_code = as.numeric(q1a),
      reported_vote_party_label = clean_group_label(q1a),
      voted_congress = recode_vote_indicator(q1a, 1, c(96, 98, 99)),
      voted_bjp = recode_vote_indicator(q1a, 2, c(96, 98, 99)),
      voted_shs = recode_vote_indicator(q1a, 46, c(96, 98, 99)),
      voted_mns = recode_vote_indicator(q1a, 47, c(96, 98, 99)),
      voted_fr = recode_vote_indicator(q1a, c(2, 46, 47), c(96, 98, 99)),
      vote_valid = !is.na(voted_fr),

      close_congress = recode_party_affinity(q11, q11a, 2, 1, 1),
      close_bjp = recode_party_affinity(q11, q11a, 2, 1, 2),
      close_shs = recode_party_affinity(q11, q11a, 2, 1, 46),
      close_mns = recode_party_affinity(q11, q11a, 2, 1, 47),
      close_any_fr = recode_party_affinity(q11, q11a, 2, 1, c(2, 46, 47)),
      close_response_valid = !is.na(close_any_fr),
      close_fr_party_n = rowSums(dplyr::pick(close_bjp, close_shs, close_mns), na.rm = TRUE),
      close_fr_party_n = dplyr::if_else(!is.na(close_any_fr), close_fr_party_n, NA_real_),

      never_vote_response_valid = FALSE,
      never_vote_bjp = NA_real_,
      never_vote_shs = NA_real_,
      never_vote_mns = NA_real_,
      never_vote_any_fr = NA_real_,

      a5_raw = NA_real_, a5_binary = NA_real_,
      a6a_raw = NA_real_, a6a_binary = NA_real_,
      a6b_raw = NA_real_, a6b_binary = NA_real_,
      q27_raw = NA_real_, q27_binary = NA_real_,

      survey_weight_raw = get_survey_weight(raw, "stpop"),
      survey_weight_validated = "stpop" %in% names(raw)
    )
}

add_year_specific_placeholders <- function(data, year) {
  variables <- c(
    "a4b_raw", "a4b_oriented", "a4b_bucket",
    "a4c_raw", "a4c_oriented", "a4c_bucket",
    "a4d_raw", "a4d_oriented", "a4d_bucket",
    "a4g_raw", "a4g_oriented", "a4g_bucket",
    "q26a_raw", "q26a_oriented", "q26a_bucket",
    "q10b_raw", "q10b_oriented", "q10b_bucket",
    "q10e_raw", "q10e_oriented", "q10e_bucket",
    "q23c_raw", "q23c_oriented", "q23c_bucket"
  )

  missing <- setdiff(variables, names(data))
  for (variable in missing) {
    data[[variable]] <- if (stringr::str_detect(variable, "bucket$")) NA_character_ else NA_real_
  }
  data
}

weighted_binary_summary <- function(data, outcome, weight = "survey_weight_norm_year") {
  x <- data[[outcome]]
  w <- data[[weight]]
  tibble::tibble(
    n_valid = sum(!is.na(x)),
    weighted_n_valid = sum(w[!is.na(x)], na.rm = TRUE),
    n_positive = sum(x == 1, na.rm = TRUE),
    share = safe_mean(x),
    weighted_share = safe_weighted_mean(x, w)
  )
}

build_nes_descriptives <- function(respondents, dirs) {
  ideology_order <- c("Left", "Center", "Right", "Mixed")

  ideology <- respondents |>
    dplyr::mutate(
      voter_ideology = dplyr::coalesce(as.character(voter_ideology), "Missing")
    ) |>
    dplyr::summarise(
      n_respondents = dplyr::n(),
      weighted_n_respondents = sum(survey_weight_norm_year, na.rm = TRUE),
      .by = c(year, voter_ideology)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      share_all_respondents = n_respondents / sum(n_respondents),
      weighted_share_all_respondents = weighted_n_respondents / sum(weighted_n_respondents),
      complete_denominator = sum(n_respondents[voter_ideology != "Missing"]),
      weighted_complete_denominator = sum(weighted_n_respondents[voter_ideology != "Missing"]),
      share_ideology_complete = dplyr::if_else(
        voter_ideology != "Missing",
        n_respondents / complete_denominator,
        NA_real_
      ),
      weighted_share_ideology_complete = dplyr::if_else(
        voter_ideology != "Missing",
        weighted_n_respondents / weighted_complete_denominator,
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  education <- respondents |>
    dplyr::filter(!is.na(voter_ideology), education_valid) |>
    dplyr::mutate(
      education_code = education_code_raw,
      education_label = education_label_raw
    ) |>
    dplyr::summarise(
      n_respondents = dplyr::n(),
      weighted_n_respondents = sum(survey_weight_norm_year, na.rm = TRUE),
      .by = c(
        year, voter_ideology, education_code,
        education_label, education_harmonized
      )
    ) |>
    dplyr::group_by(year, voter_ideology) |>
    dplyr::mutate(
      share_within_ideology = n_respondents / sum(n_respondents),
      weighted_share_within_ideology = weighted_n_respondents / sum(weighted_n_respondents)
    ) |>
    dplyr::group_by(year, education_harmonized) |>
    dplyr::mutate(
      share_ideology_within_education = n_respondents / sum(n_respondents),
      weighted_share_ideology_within_education = weighted_n_respondents / sum(weighted_n_respondents)
    ) |>
    dplyr::ungroup()

  income <- respondents |>
    dplyr::filter(!is.na(voter_ideology), household_income_valid) |>
    dplyr::summarise(
      n_respondents = dplyr::n(),
      weighted_n_respondents = sum(survey_weight_norm_year, na.rm = TRUE),
      mean_household_income = mean(household_income_monthly),
      median_household_income = median(household_income_monthly),
      weighted_mean_household_income = safe_weighted_mean(household_income_monthly, survey_weight_norm_year),
      .by = c(year, voter_ideology, income_harmonized)
    ) |>
    dplyr::group_by(year, voter_ideology) |>
    dplyr::mutate(
      share_within_ideology = n_respondents / sum(n_respondents),
      weighted_share_within_ideology = weighted_n_respondents / sum(weighted_n_respondents)
    ) |>
    dplyr::group_by(year, income_harmonized) |>
    dplyr::mutate(
      share_ideology_within_income = n_respondents / sum(n_respondents),
      weighted_share_ideology_within_income = weighted_n_respondents / sum(weighted_n_respondents)
    ) |>
    dplyr::ungroup()

  binary_long <- respondents |>
    dplyr::select(
      year, respondent_uid, voter_ideology, survey_weight_norm_year,
      a5_raw, a5_binary, a6a_raw, a6a_binary, a6b_raw, a6b_binary, q27_raw, q27_binary
    ) |>
    tidyr::pivot_longer(
      cols = c(a5_raw:q27_binary),
      names_to = c("item", ".value"),
      names_pattern = "^(a5|a6a|a6b|q27)_(raw|binary)$"
    ) |>
    dplyr::mutate(
      item_label = dplyr::recode(
        item,
        a5 = "Special concessions for Muslims",
        a6a = "Reservations: social justice versus merit",
        a6b = "Special schemes versus improving the whole economy",
        q27 = "Limits on land and property ownership"
      ),
      response_label = dplyr::case_when(
        binary == -1 ~ "Negative-coded endpoint",
        binary == 1 ~ "Positive-coded endpoint",
        TRUE ~ NA_character_
      )
    )

  binary_summary <- binary_long |>
    dplyr::filter(!is.na(voter_ideology), !is.na(binary)) |>
    dplyr::mutate(
      binary_response = binary
    ) |>
    dplyr::summarise(
      n_responses = dplyr::n(),
      weighted_n_responses = sum(survey_weight_norm_year, na.rm = TRUE),
      .by = c(
        year, item, item_label, voter_ideology,
        binary_response, response_label
      )
    ) |>
    dplyr::group_by(year, item, voter_ideology) |>
    dplyr::mutate(
      n_valid_item = sum(n_responses),
      share_among_valid = n_responses / n_valid_item,
      weighted_share_among_valid = weighted_n_responses / sum(weighted_n_responses)
    ) |>
    dplyr::ungroup()

  summarize_behavior <- function(group_vars, outcome, valid_stem, positive_stem) {
    respondents |>
      dplyr::filter(!is.na(voter_ideology)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::group_modify(~ {
        weighted_binary_summary(.x, outcome) |>
          dplyr::rename(
            "n_{valid_stem}" := n_valid,
            "weighted_n_{valid_stem}" := weighted_n_valid,
            "n_{positive_stem}" := n_positive,
            "share_{positive_stem}" := share,
            "weighted_share_{positive_stem}" := weighted_share
          )
      }) |>
      dplyr::ungroup()
  }

  vote_ideology <- summarize_behavior(c("year", "voter_ideology"), "voted_fr", "vote_valid", "voted_fr")
  vote_education <- summarize_behavior(c("year", "voter_ideology", "education_harmonized"), "voted_fr", "vote_valid", "voted_fr")
  vote_income <- summarize_behavior(c("year", "voter_ideology", "income_harmonized"), "voted_fr", "vote_valid", "voted_fr")
  close_ideology <- summarize_behavior(c("year", "voter_ideology"), "close_any_fr", "close_valid", "close_any_fr")
  close_education <- summarize_behavior(c("year", "voter_ideology", "education_harmonized"), "close_any_fr", "close_valid", "close_any_fr")
  close_income <- summarize_behavior(c("year", "voter_ideology", "income_harmonized"), "close_any_fr", "close_valid", "close_any_fr")
  never_vote <- summarize_behavior(c("year", "voter_ideology"), "never_vote_any_fr", "never_vote_valid", "never_vote_any_fr")

  outputs <- list(
    ideology_classification_by_year = ideology,
    education_distribution_by_year_ideology = education,
    income_distribution_by_year_ideology = income,
    binary_item_responses_by_year_ideology = binary_summary,
    fr_vote_by_year_ideology = vote_ideology,
    fr_vote_by_year_ideology_education = vote_education,
    fr_vote_by_year_ideology_income = vote_income,
    fr_closeness_by_year_ideology = close_ideology,
    fr_closeness_by_year_ideology_education = close_education,
    fr_closeness_by_year_ideology_income = close_income,
    never_vote_fr_by_year_ideology = never_vote
  )

  purrr::iwalk(outputs, ~ readr::write_csv(.x, file.path(dirs$diagnostic_dir, paste0(.y, ".csv"))))

  # Diagnostic figures intentionally remain simple and Viewer-friendly.
  for (year_value in sort(unique(respondents$year))) {
    ideology_plot <- ideology |>
      dplyr::filter(year == year_value, voter_ideology != "Missing") |>
      dplyr::mutate(voter_ideology = factor(voter_ideology, levels = ideology_order)) |>
      ggplot2::ggplot(ggplot2::aes(voter_ideology, weighted_share_ideology_complete)) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::label_percent()) +
      ggplot2::labs(title = paste("Ideology classification, NES", year_value), x = NULL, y = "Weighted share") +
      ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(dirs$diagnostic_dir, paste0("ideology_classification_", year_value, ".png")), ideology_plot, width = 8, height = 5, dpi = 300)

    education_plot <- education |>
      dplyr::filter(year == year_value) |>
      ggplot2::ggplot(ggplot2::aes(weighted_share_within_ideology, voter_ideology, fill = education_harmonized)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(labels = scales::label_percent()) +
      ggplot2::labs(title = paste("Education by ideology, NES", year_value), x = "Weighted share within ideology", y = NULL, fill = "Education") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(file.path(dirs$diagnostic_dir, paste0("education_distribution_by_ideology_", year_value, ".png")), education_plot, width = 10, height = 6, dpi = 300)

    income_plot <- respondents |>
      dplyr::filter(year == year_value, !is.na(voter_ideology), household_income_valid) |>
      ggplot2::ggplot(ggplot2::aes(voter_ideology, household_income_monthly)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.2) +
      ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), labels = scales::label_number(big.mark = ",")) +
      ggplot2::labs(title = paste("Household income by ideology, NES", year_value), x = NULL, y = "Monthly household income") +
      ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(dirs$diagnostic_dir, paste0("income_distribution_by_ideology_", year_value, ".png")), income_plot, width = 8, height = 6, dpi = 300)

    for (outcome in c("voted_fr", "close_any_fr")) {
      plot_data <- respondents |>
        dplyr::filter(year == year_value, !is.na(voter_ideology), !is.na(.data[[outcome]])) |>
        dplyr::summarise(
          share = safe_weighted_mean(.data[[outcome]], survey_weight_norm_year),
          .by = voter_ideology
        )
      plot_object <- ggplot2::ggplot(plot_data, ggplot2::aes(voter_ideology, share)) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(labels = scales::label_percent()) +
        ggplot2::labs(
          title = paste(if (outcome == "voted_fr") "Far-right vote" else "Far-right party closeness", "by ideology, NES", year_value),
          x = NULL,
          y = "Weighted share"
        ) +
        ggplot2::theme_minimal()
      stem <- if (outcome == "voted_fr") "fr_vote" else "fr_closeness"
      ggplot2::ggsave(file.path(dirs$diagnostic_dir, paste0(stem, "_by_ideology_", year_value, ".png")), plot_object, width = 8, height = 5, dpi = 300)
    }

    behavior_heatmap <- function(data, category, value, title, file_name) {
      plot_data <- data |>
        dplyr::filter(year == year_value, !is.na(.data[[category]]))
      if (nrow(plot_data) == 0) return(invisible(NULL))
      plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[category]], y = voter_ideology, fill = .data[[value]])
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_continuous(labels = scales::label_percent()) +
        ggplot2::labs(title = paste(title, year_value), x = NULL, y = NULL, fill = "Weighted share") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
      ggplot2::ggsave(
        file.path(dirs$diagnostic_dir, paste0(file_name, "_", year_value, ".png")),
        plot,
        width = 10,
        height = 6,
        dpi = 300
      )
    }

    behavior_heatmap(
      vote_education, "education_harmonized", "weighted_share_voted_fr",
      "Far-right vote by ideology and education, NES",
      "fr_vote_by_ideology_education"
    )
    behavior_heatmap(
      vote_income, "income_harmonized", "weighted_share_voted_fr",
      "Far-right vote by ideology and income, NES",
      "fr_vote_by_ideology_income"
    )
    behavior_heatmap(
      close_education, "education_harmonized", "weighted_share_close_any_fr",
      "Far-right party closeness by ideology and education, NES",
      "fr_closeness_by_ideology_education"
    )
    behavior_heatmap(
      close_income, "income_harmonized", "weighted_share_close_any_fr",
      "Far-right party closeness by ideology and income, NES",
      "fr_closeness_by_ideology_income"
    )
  }

  for (item in c("a5", "a6a", "a6b", "q27")) {
    plot_data <- binary_summary |>
      dplyr::filter(item == .env$item)
    if (nrow(plot_data) == 0) next
    plot_object <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(weighted_share_among_valid, voter_ideology, fill = response_label)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(labels = scales::label_percent()) +
      ggplot2::labs(title = first_nonmissing(plot_data$item_label), x = "Weighted share of valid responses", y = NULL, fill = "Response") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(file.path(dirs$diagnostic_dir, paste0("binary_", item, "_by_ideology_2009.png")), plot_object, width = 9, height = 5, dpi = 300)
  }

  c(outputs, list(binary_item_responses_long = binary_long))
}

build_nes <- function(paths, dirs, geography) {
  message("Building respondent-level NES data")
  purrr::walk(c(paths$nes_2009, paths$nes_2014), assert_file_exists)

  raw_2009 <- haven::read_sav(paths$nes_2009)
  raw_2014 <- haven::read_sav(paths$nes_2014)

  respondents <- dplyr::bind_rows(
    clean_2009_nes(raw_2009) |>
      add_year_specific_placeholders(2009) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), plain_col)),
    clean_2014_nes(raw_2014) |>
      add_year_specific_placeholders(2014) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), plain_col))
  ) |>
    dplyr::mutate(
      state_no = as.integer(state_no),
      pc = as.integer(pc),
      ac = as.integer(ac),
      ac_uid = make_ac_uid(state_no, ac),
      ac_year_uid = paste(ac_uid, year, sep = "_"),
      pc_cluster_id = make_pc_uid(state_no, pc),
      psu_uid = paste(year, state_no, pc, ac, polling_station_id, sep = "_"),
      survey_weight_norm_year = normalize_weights_within_year(survey_weight_raw, year),
      voter_ideology = factor(voter_ideology, levels = c("Left", "Center", "Right", "Mixed")),
      recognition_ideology = factor(recognition_ideology, levels = c("Left", "Center", "Right", "Mixed")),
      statism_ideology = factor(statism_ideology, levels = c("Left", "Center", "Right", "Mixed"))
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      income_quantile_year = dplyr::if_else(
        household_income_valid,
        dplyr::ntile(household_income_monthly, 5),
        NA_integer_
      ),
      income_harmonized = dplyr::case_when(
        income_quantile_year == 1 ~ "Q1 lowest",
        income_quantile_year == 2 ~ "Q2",
        income_quantile_year == 3 ~ "Q3",
        income_quantile_year == 4 ~ "Q4",
        income_quantile_year == 5 ~ "Q5 highest",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      geography$ac_reference |>
        dplyr::select(
          ac_uid, district_code_2011, district_name_2011,
          district_harmonization_group_id, district_change_comparable,
          district_join_success
        ),
      by = "ac_uid",
      relationship = "many-to-one"
    )

  assert_unique_rows(respondents, "respondent_uid", "NES respondent data")

  descriptives <- build_nes_descriptives(respondents, dirs)

  ideology_item_specs <- tibble::tribble(
    ~year, ~item, ~axis,
    2009L, "a4b", "Recognition",
    2009L, "a4c", "Recognition",
    2009L, "a4d", "Statism",
    2009L, "a4g", "Statism",
    2009L, "q26a", "Statism",
    2014L, "q10b", "Recognition",
    2014L, "q10e", "Recognition",
    2014L, "q23c", "Statism"
  )

  ideology_item_responses_long <- purrr::map_dfr(
    seq_len(nrow(ideology_item_specs)),
    function(i) {
      spec <- ideology_item_specs[i, ]
      respondents |>
        dplyr::filter(year == spec$year) |>
        dplyr::transmute(
          year, respondent_uid, state_no, pc, ac, ac_uid,
          voter_ideology,
          item = spec$item,
          axis = spec$axis,
          raw_response = .data[[paste0(spec$item, "_raw")]],
          oriented_response = .data[[paste0(spec$item, "_oriented")]],
          response_bucket = .data[[paste0(spec$item, "_bucket")]]
        )
    }
  )

  ac_year <- respondents |>
    dplyr::filter(!is.na(ac_uid), !is.na(year)) |>
    dplyr::summarise(
      nes_pc = first_nonmissing(pc),
      nes_n_distinct_pc = dplyr::n_distinct(pc, na.rm = TRUE),
      nes_n_respondents = dplyr::n(),
      nes_n_ideology_complete = sum(ideology_complete, na.rm = TRUE),
      nes_n_ideology_missing = sum(!ideology_complete, na.rm = TRUE),
      nes_n_left = sum(voter_ideology == "Left", na.rm = TRUE),
      nes_n_center = sum(voter_ideology == "Center", na.rm = TRUE),
      nes_n_right = sum(voter_ideology == "Right", na.rm = TRUE),
      nes_n_mixed = sum(voter_ideology == "Mixed", na.rm = TRUE),
      survey_weight_validated = all(survey_weight_validated),
      nes_weighted_n_respondents = sum(survey_weight_norm_year, na.rm = TRUE),
      nes_weighted_n_ideology_complete = sum(survey_weight_norm_year[ideology_complete], na.rm = TRUE),
      .by = c(state_no, ac, ac_uid, year)
    ) |>
    dplyr::mutate(
      nes_share_left_all_respondents = safe_share(nes_n_left, nes_n_respondents),
      nes_share_center_all_respondents = safe_share(nes_n_center, nes_n_respondents),
      nes_share_right_all_respondents = safe_share(nes_n_right, nes_n_respondents),
      nes_share_mixed_all_respondents = safe_share(nes_n_mixed, nes_n_respondents),
      nes_share_ideology_missing_all_respondents = safe_share(nes_n_ideology_missing, nes_n_respondents),
      nes_share_left_among_ideology_complete = safe_share(nes_n_left, nes_n_ideology_complete),
      nes_share_center_among_ideology_complete = safe_share(nes_n_center, nes_n_ideology_complete),
      nes_share_right_among_ideology_complete = safe_share(nes_n_right, nes_n_ideology_complete),
      nes_share_mixed_among_ideology_complete = safe_share(nes_n_mixed, nes_n_ideology_complete)
    )

  weighted_ac_year <- respondents |>
    dplyr::filter(!is.na(ac_uid), !is.na(year)) |>
    dplyr::summarise(
      nes_weighted_share_left_all_respondents = safe_weighted_mean(
        dplyr::coalesce(voter_ideology == "Left", FALSE), survey_weight_norm_year
      ),
      nes_weighted_share_center_all_respondents = safe_weighted_mean(
        dplyr::coalesce(voter_ideology == "Center", FALSE), survey_weight_norm_year
      ),
      nes_weighted_share_right_all_respondents = safe_weighted_mean(
        dplyr::coalesce(voter_ideology == "Right", FALSE), survey_weight_norm_year
      ),
      nes_weighted_share_mixed_all_respondents = safe_weighted_mean(
        dplyr::coalesce(voter_ideology == "Mixed", FALSE), survey_weight_norm_year
      ),
      nes_weighted_share_left_among_ideology_complete = safe_weighted_mean(
        voter_ideology[ideology_complete] == "Left",
        survey_weight_norm_year[ideology_complete]
      ),
      nes_weighted_share_center_among_ideology_complete = safe_weighted_mean(
        voter_ideology[ideology_complete] == "Center",
        survey_weight_norm_year[ideology_complete]
      ),
      nes_weighted_share_right_among_ideology_complete = safe_weighted_mean(
        voter_ideology[ideology_complete] == "Right",
        survey_weight_norm_year[ideology_complete]
      ),
      nes_weighted_share_mixed_among_ideology_complete = safe_weighted_mean(
        voter_ideology[ideology_complete] == "Mixed",
        survey_weight_norm_year[ideology_complete]
      ),
      .by = c(ac_uid, year)
    )

  ac_year <- ac_year |>
    dplyr::left_join(weighted_ac_year, by = c("ac_uid", "year"), relationship = "one-to-one")

  ideology_cells <- respondents |>
    dplyr::filter(!is.na(ac_uid), !is.na(year), !is.na(voter_ideology)) |>
    dplyr::group_by(state_no, ac, ac_uid, year, ideology = voter_ideology) |>
    dplyr::group_modify(~ {
      data <- .x
      tibble::tibble(
        nes_pc = first_nonmissing(data$pc),
        nes_n_distinct_pc = dplyr::n_distinct(data$pc, na.rm = TRUE),
        n_respondents = nrow(data),
        weighted_n_respondents = sum(data$survey_weight_norm_year, na.rm = TRUE),
        n_vote_valid = sum(data$vote_valid, na.rm = TRUE),
        weighted_n_vote_valid = sum(data$survey_weight_norm_year[data$vote_valid], na.rm = TRUE),
        n_voted_congress = sum(data$voted_congress == 1, na.rm = TRUE),
        n_voted_bjp = sum(data$voted_bjp == 1, na.rm = TRUE),
        n_voted_shs = sum(data$voted_shs == 1, na.rm = TRUE),
        n_voted_mns = sum(data$voted_mns == 1, na.rm = TRUE),
        n_voted_fr = sum(data$voted_fr == 1, na.rm = TRUE),
        share_voted_congress = safe_mean(data$voted_congress),
        share_voted_bjp = safe_mean(data$voted_bjp),
        share_voted_shs = safe_mean(data$voted_shs),
        share_voted_mns = safe_mean(data$voted_mns),
        share_voted_fr = safe_mean(data$voted_fr),
        weighted_share_voted_congress = safe_weighted_mean(data$voted_congress, data$survey_weight_norm_year),
        weighted_share_voted_bjp = safe_weighted_mean(data$voted_bjp, data$survey_weight_norm_year),
        weighted_share_voted_shs = safe_weighted_mean(data$voted_shs, data$survey_weight_norm_year),
        weighted_share_voted_mns = safe_weighted_mean(data$voted_mns, data$survey_weight_norm_year),
        weighted_share_voted_fr = safe_weighted_mean(data$voted_fr, data$survey_weight_norm_year),
        n_close_valid = sum(data$close_response_valid, na.rm = TRUE),
        weighted_n_close_valid = sum(data$survey_weight_norm_year[data$close_response_valid], na.rm = TRUE),
        n_close_congress = sum(data$close_congress == 1, na.rm = TRUE),
        n_close_bjp = sum(data$close_bjp == 1, na.rm = TRUE),
        n_close_shs = sum(data$close_shs == 1, na.rm = TRUE),
        n_close_mns = sum(data$close_mns == 1, na.rm = TRUE),
        n_close_any_fr = sum(data$close_any_fr == 1, na.rm = TRUE),
        share_close_congress = safe_mean(data$close_congress),
        share_close_bjp = safe_mean(data$close_bjp),
        share_close_shs = safe_mean(data$close_shs),
        share_close_mns = safe_mean(data$close_mns),
        share_close_any_fr = safe_mean(data$close_any_fr),
        weighted_share_close_congress = safe_weighted_mean(data$close_congress, data$survey_weight_norm_year),
        weighted_share_close_bjp = safe_weighted_mean(data$close_bjp, data$survey_weight_norm_year),
        weighted_share_close_shs = safe_weighted_mean(data$close_shs, data$survey_weight_norm_year),
        weighted_share_close_mns = safe_weighted_mean(data$close_mns, data$survey_weight_norm_year),
        weighted_share_close_any_fr = safe_weighted_mean(data$close_any_fr, data$survey_weight_norm_year),
        n_never_vote_valid = sum(data$never_vote_response_valid, na.rm = TRUE),
        weighted_n_never_vote_valid = sum(data$survey_weight_norm_year[data$never_vote_response_valid], na.rm = TRUE),
        n_never_vote_bjp = sum(data$never_vote_bjp == 1, na.rm = TRUE),
        n_never_vote_shs = sum(data$never_vote_shs == 1, na.rm = TRUE),
        n_never_vote_mns = sum(data$never_vote_mns == 1, na.rm = TRUE),
        n_never_vote_any_fr = sum(data$never_vote_any_fr == 1, na.rm = TRUE),
        share_never_vote_bjp = safe_mean(data$never_vote_bjp),
        share_never_vote_shs = safe_mean(data$never_vote_shs),
        share_never_vote_mns = safe_mean(data$never_vote_mns),
        share_never_vote_any_fr = safe_mean(data$never_vote_any_fr),
        weighted_share_never_vote_bjp = safe_weighted_mean(data$never_vote_bjp, data$survey_weight_norm_year),
        weighted_share_never_vote_shs = safe_weighted_mean(data$never_vote_shs, data$survey_weight_norm_year),
        weighted_share_never_vote_mns = safe_weighted_mean(data$never_vote_mns, data$survey_weight_norm_year),
        weighted_share_never_vote_any_fr = safe_weighted_mean(data$never_vote_any_fr, data$survey_weight_norm_year),
        n_education_valid = sum(data$education_valid),
        weighted_n_education_valid = sum(data$survey_weight_norm_year[data$education_valid], na.rm = TRUE),
        share_education_below_college = safe_mean(data$education_below_college),
        weighted_share_education_below_college = safe_weighted_mean(data$education_below_college, data$survey_weight_norm_year),
        n_income_valid = sum(data$household_income_valid),
        weighted_n_income_valid = sum(data$survey_weight_norm_year[data$household_income_valid], na.rm = TRUE),
        mean_household_income = safe_mean(data$household_income_monthly),
        median_household_income = safe_median(data$household_income_monthly),
        weighted_mean_household_income = safe_weighted_mean(data$household_income_monthly, data$survey_weight_norm_year),
        cell_n_ge_5 = nrow(data) >= 5,
        cell_n_ge_10 = nrow(data) >= 10
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      ac_year |>
        dplyr::select(
          ac_uid, year,
          n_ac_ideology_complete = nes_n_ideology_complete,
          weighted_n_ac_ideology_complete = nes_weighted_n_ideology_complete
        ),
      by = c("ac_uid", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      share_ac_ideology_complete = safe_share(n_respondents, n_ac_ideology_complete),
      weighted_share_ac_ideology_complete = safe_share(
        weighted_n_respondents,
        weighted_n_ac_ideology_complete
      )
    )

  nes_diagnostics <- dplyr::bind_rows(
    respondents |>
      dplyr::summarise(
        check = "Respondents",
        group = "all",
        n = dplyr::n(), denominator = dplyr::n(), pct = 100,
        minimum = NA_real_, median = NA_real_, maximum = NA_real_,
        passed = TRUE, details = NA_character_,
        .by = year
      ),
    respondents |>
      dplyr::summarise(
        check = "Missing AC match",
        group = "geography",
        n = sum(!district_join_success | is.na(district_join_success)),
        denominator = dplyr::n(), pct = 100 * n / denominator,
        minimum = NA_real_, median = NA_real_, maximum = NA_real_,
        passed = n == 0, details = NA_character_,
        .by = year
      ),
    respondents |>
      dplyr::summarise(
        check = "Ideology complete",
        group = "ideology",
        n = sum(ideology_complete), denominator = dplyr::n(), pct = 100 * n / denominator,
        minimum = NA_real_, median = NA_real_, maximum = NA_real_,
        passed = NA, details = NA_character_,
        .by = year
      ),
    respondents |>
      dplyr::filter(!is.na(ac_uid), !is.na(year)) |>
      dplyr::summarise(
        nes_n_distinct_pc = dplyr::n_distinct(pc, na.rm = TRUE),
        .by = c(year, ac_uid)
      ) |>
      dplyr::summarise(
        check = "Multiple NES PC codes within AC-year",
        group = "geography",
        n = sum(nes_n_distinct_pc > 1),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        minimum = safe_min(nes_n_distinct_pc),
        median = safe_median(nes_n_distinct_pc),
        maximum = safe_max(nes_n_distinct_pc),
        passed = n == 0,
        details = paste(
          "PC-code disagreement is retained as a diagnostic;",
          "AC-year and AC-year-ideology aggregation uses AC as the unit."
        ),
        .by = year
      ),
    respondents |>
      dplyr::summarise(
        check = "Effective weighted sample size",
        group = "weights",
        n = round((sum(survey_weight_norm_year, na.rm = TRUE)^2) / sum(survey_weight_norm_year^2, na.rm = TRUE)),
        denominator = dplyr::n(), pct = 100 * n / denominator,
        minimum = safe_min(survey_weight_norm_year),
        median = safe_median(survey_weight_norm_year),
        maximum = safe_max(survey_weight_norm_year),
        passed = all(survey_weight_validated), details = NA_character_,
        .by = year
      )
  )

  extract_item_dictionary <- function(raw, year, variables, item_type, axis_lookup) {
    purrr::map_dfr(variables, function(variable) {
      labels <- attr(raw[[variable]], "labels")
      question <- variable_label(raw[[variable]], variable)
      axis <- unname(axis_lookup[variable])
      if (is.null(labels) || length(labels) == 0) {
        return(tibble::tibble(
          year = year,
          source_variable = variable,
          item_type = item_type,
          axis = axis,
          question_label = question,
          source_response_code = NA_real_,
          source_response_label = NA_character_,
          oriented_value = NA_real_,
          response_bucket = NA_character_,
          missing_or_dk = NA,
          coding_note = "See respondent cleaning code for orientation"
        ))
      }

      tibble::tibble(
        year = year,
        source_variable = variable,
        item_type = item_type,
        axis = axis,
        question_label = question,
        source_response_code = as.numeric(labels),
        source_response_label = names(labels),
        oriented_value = NA_real_,
        response_bucket = NA_character_,
        missing_or_dk = stringr::str_detect(
          norm_name(names(labels)),
          "DONT KNOW|DO NOT KNOW|NO OPINION|NOT APPLICABLE|REFUSED|MISSING"
        ),
        coding_note = "Final oriented value and bucket are defined in clean_2009_nes/clean_2014_nes"
      )
    })
  }

  axis_2009 <- c(
    a4b = "Recognition", a4c = "Recognition", a4d = "Statism",
    a4g = "Statism", q26a = "Statism",
    a5 = "Recognition", a6a = "Recognition", a6b = "Statism", q27 = "Statism"
  )
  axis_2014 <- c(q10b = "Recognition", q10e = "Recognition", q23c = "Statism")

  item_dictionary <- dplyr::bind_rows(
    extract_item_dictionary(
      raw_2009, 2009L,
      c("a4b", "a4c", "a4d", "a4g", "q26a"),
      "ideology", axis_2009
    ),
    extract_item_dictionary(
      raw_2014, 2014L,
      c("q10b", "q10e", "q23c"),
      "ideology", axis_2014
    ),
    extract_item_dictionary(
      raw_2009, 2009L,
      c("a5", "a6a", "a6b", "q27"),
      "binary diagnostic", axis_2009
    )
  )


  write_rds_csv(respondents, file.path(dirs$intermediate_dir, "nes_respondent_clean"), "respondent_uid")
  write_csv_checked(ac_year, file.path(dirs$intermediate_dir, "nes_ac_year.csv"), c("ac_uid", "year"))
  write_csv_checked(ideology_cells, file.path(dirs$intermediate_dir, "ac_year_ideology_summary.csv"), c("ac_uid", "year", "ideology"))
  write_csv_checked(ideology_item_responses_long, file.path(dirs$intermediate_dir, "ideology_item_responses_long.csv"))
  saveRDS(ideology_item_responses_long, file.path(dirs$intermediate_dir, "ideology_item_responses_long.rds"))
  write_csv_checked(descriptives$binary_item_responses_long, file.path(dirs$intermediate_dir, "binary_item_responses_long.csv"))
  write_csv_checked(item_dictionary, file.path(dirs$final_dir, "nes_item_dictionary.csv"))
  write_csv_checked(nes_diagnostics, file.path(dirs$diagnostic_dir, "nes_diagnostics.csv"))

  list(
    respondents = respondents,
    ac_year = ac_year,
    ideology_cells = ideology_cells,
    ideology_item_responses_long = ideology_item_responses_long,
    descriptives = descriptives,
    diagnostics = nes_diagnostics,
    item_dictionary = item_dictionary
  )
}

# 2009 and 2014 AC-level general-election outcomes.

build_elections <- function(paths, dirs, geography) {
  message("Building election outcomes")
  assert_file_exists(paths$elections, "Lok Dhaba general-election file")

  fr_parties <- c("BJP", "SHS", "MNS")
  raw <- readr::read_csv(paths$elections, show_col_types = FALSE)
  assert_has_columns(
    raw,
    c(
      "State_Name", "Constituency_No", "Year", "PC_Name", "PC_No",
      "Constituency_Name", "Candidate", "Party", "Votes", "Valid_Votes"
    ),
    "Lok Dhaba general-election file"
  )

  candidate_clean <- raw |>
    dplyr::filter(Year %in% c(2009, 2014)) |>
    dplyr::transmute(
      state = stringr::str_replace_all(as.character(State_Name), "_", " "),
      state_no = state_name_to_no(State_Name),
      pc = as.integer(PC_No),
      pc_name = as.character(PC_Name),
      ac = as.integer(Constituency_No),
      ac_name = as.character(Constituency_Name),
      year = as.integer(Year),
      candidate = as.character(Candidate),
      party = stringr::str_to_upper(stringr::str_squish(as.character(Party))),
      votes = as.numeric(Votes),
      valid_votes = as.numeric(Valid_Votes),
      is_bjp_candidate = party == "BJP",
      is_shs_candidate = party == "SHS",
      is_mns_candidate = party == "MNS",
      is_fr_candidate = party %in% fr_parties
    ) |>
    dplyr::filter(!is.na(state_no), !is.na(ac), !is.na(year))

  identifier_conflicts <- candidate_clean |>
    dplyr::summarise(
      n_pc = dplyr::n_distinct(pc, na.rm = TRUE),
      n_valid_votes = dplyr::n_distinct(valid_votes, na.rm = TRUE),
      .by = c(state_no, ac, year)
    ) |>
    dplyr::filter(n_pc > 1 | n_valid_votes > 1)

  if (nrow(identifier_conflicts) > 0) {
    readr::write_csv(
      identifier_conflicts,
      file.path(dirs$diagnostic_dir, "election_identifier_conflicts.csv")
    )
    stop("Conflicting PC or valid-vote identifiers remain within AC-year election records")
  }

  preferred_label <- function(x, party) {
    valid <- x[!is.na(x) & !is.na(party) & party != "NOTA"]
    if (length(valid) > 0) valid[1] else first_nonmissing(x)
  }

  ac_year <- candidate_clean |>
    dplyr::summarise(
      state = first_nonmissing(state),
      pc = first_nonmissing(pc),
      pc_name = preferred_label(pc_name, party),
      ac_name = preferred_label(ac_name, party),
      valid_votes = first_nonmissing(valid_votes),
      bjp_votes = sum(dplyr::if_else(party == "BJP", votes, 0), na.rm = TRUE),
      shs_votes = sum(dplyr::if_else(party == "SHS", votes, 0), na.rm = TRUE),
      mns_votes = sum(dplyr::if_else(party == "MNS", votes, 0), na.rm = TRUE),
      fr_party_votes = sum(dplyr::if_else(is_fr_candidate, votes, 0), na.rm = TRUE),
      bjp_candidate_present = any(is_bjp_candidate, na.rm = TRUE),
      shs_candidate_present = any(is_shs_candidate, na.rm = TRUE),
      mns_candidate_present = any(is_mns_candidate, na.rm = TRUE),
      fr_candidate_present = any(is_fr_candidate, na.rm = TRUE),
      fr_candidate_n = dplyr::n_distinct(candidate[is_fr_candidate & !is.na(candidate)]),
      .by = c(state_no, ac, year)
    ) |>
    dplyr::mutate(
      ac_uid = make_ac_uid(state_no, ac),
      ac_year_uid = paste(ac_uid, year, sep = "_"),
      pc_cluster_id = make_pc_uid(state_no, pc),
      bjp_vote_share = safe_pct(bjp_votes, valid_votes),
      shs_vote_share = safe_pct(shs_votes, valid_votes),
      mns_vote_share = safe_pct(mns_votes, valid_votes),
      fr_party_vote_share = safe_pct(fr_party_votes, valid_votes)
    ) |>
    dplyr::left_join(
      geography$ac_reference |>
        dplyr::select(
          state_no, ac, district_code_2011, district_name_2011,
          district_harmonization_group_id, district_relationship_type,
          district_change_comparable, manual_xwalk, district_join_success
        ),
      by = c("state_no", "ac"),
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(state_no, ac, year)

  assert_unique_rows(ac_year, c("state_no", "ac", "year"), "election AC-year data")

  diagnostics <- dplyr::bind_rows(
    candidate_clean |>
      dplyr::summarise(
        check = "candidate rows",
        n = dplyr::n(),
        denominator = dplyr::n(),
        pct = 100,
        passed = TRUE,
        details = NA_character_,
        .by = year
      ),
    ac_year |>
      dplyr::summarise(
        check = "AC-years with invalid or missing valid-vote denominator",
        n = sum(is.na(valid_votes) | valid_votes <= 0),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        passed = n == 0,
        details = NA_character_,
        .by = year
      ),
    ac_year |>
      dplyr::summarise(
        check = "AC-years where party votes exceed valid votes",
        n = sum(fr_party_votes > valid_votes, na.rm = TRUE),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        passed = n == 0,
        details = NA_character_,
        .by = year
      ),
    ac_year |>
      dplyr::summarise(
        check = "AC-years with no far-right candidate",
        n = sum(!fr_candidate_present),
        denominator = dplyr::n(),
        pct = 100 * n / denominator,
        passed = NA,
        details = "Far-right vote share is zero because far-right votes are zero",
        .by = year
      )
  )

  write_csv_checked(candidate_clean, file.path(dirs$intermediate_dir, "elections_candidate_clean.csv"))
  write_csv_checked(ac_year, file.path(dirs$intermediate_dir, "elections_ac_year.csv"), c("state_no", "ac", "year"))
  write_csv_checked(diagnostics, file.path(dirs$diagnostic_dir, "election_diagnostics.csv"))

  party_crosswalk <- tibble::tribble(
    ~year, ~source_party_name, ~standardized_party, ~far_right_party, ~congress_party,
    2009L, "BJP", "BJP", TRUE, FALSE,
    2009L, "SHS", "SHS", TRUE, FALSE,
    2009L, "MNS", "MNS", TRUE, FALSE,
    2009L, "INC", "INC", FALSE, TRUE,
    2014L, "BJP", "BJP", TRUE, FALSE,
    2014L, "SHS", "SHS", TRUE, FALSE,
    2014L, "MNS", "MNS", TRUE, FALSE,
    2014L, "INC", "INC", FALSE, TRUE
  ) |>
    dplyr::mutate(classification_note = "BJP, SHS, and MNS define the far-right party family")
  write_csv_checked(party_crosswalk, file.path(dirs$final_dir, "party_crosswalk.csv"))

  list(candidate_clean = candidate_clean, ac_year = ac_year, diagnostics = diagnostics)
}

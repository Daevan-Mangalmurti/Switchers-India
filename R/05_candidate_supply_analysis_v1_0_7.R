# ============================================================
# 05_candidate_supply_analysis.R
#
# Candidate availability / party-supply analysis.
#
# Main conceptual decomposition:
#
#   Observed BJP vote = I(BJP is the NDA nominee / BJP is available)
#                       × BJP choice conditional on BJP availability.
#
# Candidate supply is a PC-level party/alliance decision.
# Voter choice remains respondent-level.
#
# This script:
#   1. reconstructs BJP and official pre-poll NDA candidate supply
#      in 2009 and 2014 from the AC-segment election file;
#   2. validates the inferred NDA roster against published roster totals
#      (521 in 2009; 542 in 2014);
#   3. builds BJP/NDA supply transitions;
#   4. estimates descriptive PC-level BJP seat-allocation/supply models;
#   5. reruns the preferred respondent models in PCs where BJP contested
#      BOTH 2009 and 2014;
#   6. performs a model-based supply-vs-choice decomposition of realized
#      BJP support.
#
# IMPORTANT:
#   - This is a mechanism/supply analysis, NOT a causal mediation analysis.
#   - We do NOT IPW away candidate availability.
#   - "NDA candidate present" means an official/designated pre-poll NDA
#     nominee, not merely that some party belonging to the NDA happened
#     to field a candidate in the seat.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(
  file.path(
    project_root,
    "R",
    "helpers.R"
  )
)

load_switchers_packages()

paths <- build_project_paths(
  project_root
)

CANDIDATE_SUPPLY_REVISION <-
  "2026-08-08-v1.0.7-decomposition-name-hotfix"

RESPONDENT_RESULT_REVISION <-
  "2026-08-08-v1.0.3-targeted-robustness-hotfix"

message(
  "Starting candidate-supply analysis: ",
  CANDIDATE_SUPPLY_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

model_exploration_dir <- file.path(
  paths$derived_dir,
  "model_exploration"
)

out_root <- file.path(
  model_exploration_dir,
  "candidate_supply_analysis"
)

out_table_dir <- file.path(
  out_root,
  "tables"
)

out_figure_dir <- file.path(
  out_root,
  "figures"
)

out_model_dir <- file.path(
  out_root,
  "models"
)

out_manifest_dir <- file.path(
  out_root,
  "manifests"
)

purrr::walk(
  c(
    out_root,
    out_table_dir,
    out_figure_dir,
    out_model_dir,
    out_manifest_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

candidate_election_paths <- c(
  file.path(
    project_root,
    "data",
    "election",
    "lok_dhaba_ge.csv"
  ),
  file.path(
    project_root,
    "data",
    "raw",
    "lok_dhaba_ge.csv"
  ),
  file.path(
    project_root,
    "lok_dhaba_ge.csv"
  )
)

candidate_election_path <-
  candidate_election_paths[
    file.exists(
      candidate_election_paths
    )
  ][1]

if (
  length(
    candidate_election_path
  ) == 0 ||
  is.na(
    candidate_election_path
  )
) {
  stop(
    "Could not locate lok_dhaba_ge.csv. Checked: ",
    paste(
      candidate_election_paths,
      collapse = "; "
    )
  )
}

message(
  "Election candidate file: ",
  candidate_election_path
)

# ============================================================
# 1. ONLINE-RESEARCHED PRE-POLL NDA ROSTER MANIFEST
# ============================================================
#
# Sources encoded here:
#
# 2009 candidate roster:
# https://en.wikipedia.org/wiki/List_of_National_Democratic_Alliance_candidates_in_the_2009_Indian_general_election
#
# 2014 candidate roster:
# https://en.wikipedia.org/wiki/List_of_National_Democratic_Alliance_candidates_in_the_2014_Indian_general_election
#
# The raw TCPD party labels are cross-checked below against the published
# total number of designated NDA candidates. The party-state scope is
# important: e.g. a party may contest outside the state in which it was
# actually part of NDA seat sharing.
#
# Three 2014 minor allies (IJK, PNK, KMDK) are reported in the published
# roster as contesting on the BJP symbol; they are therefore already
# represented by raw Party == "BJP" for availability purposes.

alliance_party_scope <- tibble::tribble(
  ~year, ~state_name_raw, ~raw_party, ~official_party_name,

  2009, "Bihar", "JD(U)", "Janata Dal (United)",
  2009, "Jharkhand", "JD(U)", "Janata Dal (United)",
  2009, "Tamil_Nadu", "JD(U)", "Janata Dal (United)",
  2009, "Uttar_Pradesh", "JD(U)", "Janata Dal (United)",
  2009, "Kerala", "JD(U)", "Janata Dal (United)",
  2009, "Maharashtra", "SHS", "Shiv Sena",
  2009, "Tamil_Nadu", "SHS", "Shiv Sena",
  2009, "Punjab", "SAD", "Shiromani Akali Dal",
  2009, "Uttar_Pradesh", "RLD", "Rashtriya Lok Dal",
  2009, "Assam", "AGP", "Asom Gana Parishad",
  2009, "Haryana", "INLD", "Indian National Lok Dal",
  2009, "Nagaland", "NPF", "Naga People's Front",

  2014, "Andhra_Pradesh", "TDP", "Telugu Desam Party",
  2014, "Maharashtra", "SHS", "Shiv Sena",
  2014, "Tamil_Nadu", "DMDK", "Desiya Murpokku Dravida Kazhagam",
  2014, "Punjab", "SAD", "Shiromani Akali Dal",
  2014, "Tamil_Nadu", "PMK", "Pattali Makkal Katchi",
  2014, "Tamil_Nadu", "MDMK", "Marumalarchi Dravida Munnetra Kazhagam",
  2014, "Bihar", "LJP", "Lok Janshakti Party",
  2014, "Bihar", "BLSP", "Rashtriya Lok Samta Party",
  2014, "Uttar_Pradesh", "AD", "Apna Dal",
  2014, "Haryana", "HJCBL", "Haryana Janhit Congress (BL)",
  2014, "Maharashtra", "SWP", "Swabhimani Paksha",
  2014, "Puducherry", "AINRC", "All India N.R. Congress",
  2014, "Maharashtra", "RPI(A)", "Republican Party of India (Athawale)",
  2014, "Maharashtra", "RSPS", "Rashtriya Samaj Paksha",
  2014, "Kerala", "RSPK(B)", "Revolutionary Socialist Party (Bolshevik)",
  2014, "Meghalaya", "NPEP", "National People's Party",
  2014, "Nagaland", "NPF", "Naga People's Front"
)

candidate_exceptions <- tibble::tribble(
  ~year, ~state_name_raw, ~pc_no, ~candidate, ~raw_party, ~official_party_name, ~exception_type,

  2009, "Assam", 4L, "ARUN DAS", "RWS",
  "Rashtrawadi Sena",
  "Seat-specific ally: RWS ran elsewhere outside the official NDA allocation",

  2009, "Maharashtra", 44L, "AJITRAO SHANKARRAO GHORPADE", "IND",
  "BJP-supported Independent",
  "Published NDA-supported independent",

  2009, "Mizoram", 1L, "DR. H. LALLUNGMUANA", "IND",
  "BJP-supported Independent",
  "Published NDA-supported independent",

  2014, "Kerala", 14L, "ADV.NOBLE MATHEW", "IND",
  "Kerala Congress (Nationalist)",
  "Official NDA nominee recorded as IND in TCPD",

  2014, "Mizoram", 1L, "ROBERT ROMAWIA ROYTE", "IND",
  "Mizo National Front",
  "Official NDA nominee recorded as IND in TCPD",

  2014, "Assam", 5L, "URKHAO GWRA BRAHMA", "IND",
  "NDA-supported Independent",
  "Published NDA-supported independent"
)

expected_nominee_counts <- tibble::tribble(
  ~year, ~raw_nominee_party, ~expected_n,

  2009, "BJP", 433L,
  2009, "JD(U)", 32L,
  2009, "SHS", 24L,
  2009, "SAD", 10L,
  2009, "RLD", 7L,
  2009, "AGP", 6L,
  2009, "INLD", 5L,
  2009, "NPF", 1L,
  2009, "RWS", 1L,
  2009, "IND", 2L,

  2014, "BJP", 428L,
  2014, "TDP", 30L,
  2014, "SHS", 20L,
  2014, "DMDK", 14L,
  2014, "SAD", 10L,
  2014, "PMK", 8L,
  2014, "MDMK", 7L,
  2014, "LJP", 7L,
  2014, "BLSP", 3L,
  2014, "AD", 2L,
  2014, "HJCBL", 2L,
  2014, "SWP", 2L,
  2014, "AINRC", 1L,
  2014, "RPI(A)", 1L,
  2014, "RSPS", 1L,
  2014, "RSPK(B)", 1L,
  2014, "NPEP", 1L,
  2014, "NPF", 1L,
  2014, "IND", 3L
)

expected_total_nda <- tibble::tribble(
  ~year, ~expected_total,

  2009, 521L,
  2014, 542L
)

readr::write_csv(
  alliance_party_scope,
  file.path(
    out_manifest_dir,
    "nda_alliance_party_scope_2009_2014.csv"
  )
)

readr::write_csv(
  candidate_exceptions,
  file.path(
    out_manifest_dir,
    "nda_candidate_exceptions_2009_2014.csv"
  )
)

# ============================================================
# 2. RECONSTRUCT PC-YEAR CANDIDATE SUPPLY
# ============================================================

normalize_candidate <- function(
    x
) {
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all(
      "[^A-Z0-9]+",
      " "
    ) |>
    stringr::str_squish()
}

normalize_geography <- function(
    x
) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(
      "[^a-z0-9]+",
      ""
    )
}

election_raw <- readr::read_csv(
  candidate_election_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::filter(
    Year %in%
      c(
        2009,
        2014
      )
  )

required_election_vars <- c(
  "Year",
  "State_Name",
  "PC_No",
  "PC_Name",
  "CandID",
  "Candidate",
  "Party"
)

missing_election_vars <- setdiff(
  required_election_vars,
  names(
    election_raw
  )
)

if (
  length(
    missing_election_vars
  ) > 0
) {
  stop(
    "Election file is missing: ",
    paste(
      missing_election_vars,
      collapse = ", "
    )
  )
}

# The source is AC-segment-wise, so the same parliamentary candidate
# appears repeatedly. Deduplicate by candidate ID within PC.
candidate_pc <- election_raw |>
  dplyr::mutate(
    candidate_length =
      nchar(
        dplyr::coalesce(
          Candidate,
          ""
        )
      )
  ) |>
  dplyr::arrange(
    Year,
    State_Name,
    PC_No,
    CandID,
    Party,
    dplyr::desc(
      candidate_length
    )
  ) |>
  dplyr::distinct(
    Year,
    State_Name,
    PC_No,
    CandID,
    Party,
    .keep_all = TRUE
  ) |>
  dplyr::transmute(
    year =
      as.integer(
        Year
      ),

    state_name_raw =
      State_Name,

    pc_no =
      as.integer(
        PC_No
      ),

    pc_name =
      PC_Name,

    cand_id =
      CandID,

    candidate =
      Candidate,

    candidate_norm =
      normalize_candidate(
        Candidate
      ),

    raw_party =
      Party
  )

candidate_pc <- candidate_pc |>
  dplyr::left_join(
    alliance_party_scope |>
      dplyr::mutate(
        eligible_party_scope =
          TRUE
      ),
    by = c(
      "year",
      "state_name_raw",
      "raw_party"
    )
  ) |>
  dplyr::left_join(
    candidate_exceptions |>
      dplyr::mutate(
        candidate_norm =
          normalize_candidate(
            candidate
          ),
        eligible_exception =
          TRUE
      ) |>
      dplyr::select(
        year,
        state_name_raw,
        pc_no,
        candidate_norm,
        exception_official_party_name =
          official_party_name,
        exception_type,
        eligible_exception
      ),
    by = c(
      "year",
      "state_name_raw",
      "pc_no",
      "candidate_norm"
    )
  ) |>
  dplyr::mutate(
    eligible_party_scope =
      dplyr::coalesce(
        eligible_party_scope,
        FALSE
      ),

    eligible_exception =
      dplyr::coalesce(
        eligible_exception,
        FALSE
      ),

    eligible_nda_non_bjp =
      raw_party !=
        "BJP" &
      (
        eligible_party_scope |
        eligible_exception
      ),

    eligible_official_party_name =
      dplyr::case_when(
        raw_party ==
          "BJP" ~
          "Bharatiya Janata Party",

        eligible_exception ~
          exception_official_party_name,

        eligible_party_scope ~
          official_party_name,

        TRUE ~
          NA_character_
      )
  )

resolve_pc_nominee <- function(
    data
) {
  bjp_rows <- data |>
    dplyr::filter(
      raw_party ==
        "BJP"
    )

  ally_rows <- data |>
    dplyr::filter(
      eligible_nda_non_bjp
    )

  # If BJP is present, it is the designated BJP/NDA supply option unless
  # the roster manifest explicitly says otherwise. The official roster
  # validation below protects against silently misclassifying alliance
  # members that happened to contest outside their allocated seats.
  if (
    nrow(
      bjp_rows
    ) == 1
  ) {
    chosen <-
      bjp_rows[
        1,
        ,
        drop = FALSE
      ]

    return(
      tibble::tibble(
        bjp_candidate_present =
          TRUE,
        nda_candidate_present =
          TRUE,
        nda_nominee_raw_party =
          chosen$raw_party,
        nda_nominee_official_party =
          "Bharatiya Janata Party",
        nda_nominee_candidate =
          chosen$candidate,
        nda_nominee_is_bjp =
          TRUE,
        nda_ally_substitution =
          FALSE,
        nda_resolution_ambiguous =
          FALSE,
        n_eligible_non_bjp_nda_candidates =
          nrow(
            ally_rows
          )
      )
    )
  }

  if (
    nrow(
      bjp_rows
    ) > 1
  ) {
    return(
      tibble::tibble(
        bjp_candidate_present =
          TRUE,
        nda_candidate_present =
          NA,
        nda_nominee_raw_party =
          NA_character_,
        nda_nominee_official_party =
          NA_character_,
        nda_nominee_candidate =
          NA_character_,
        nda_nominee_is_bjp =
          NA,
        nda_ally_substitution =
          NA,
        nda_resolution_ambiguous =
          TRUE,
        n_eligible_non_bjp_nda_candidates =
          nrow(
            ally_rows
          )
      )
    )
  }

  if (
    nrow(
      ally_rows
    ) == 1
  ) {
    chosen <-
      ally_rows[
        1,
        ,
        drop = FALSE
      ]

    return(
      tibble::tibble(
        bjp_candidate_present =
          FALSE,
        nda_candidate_present =
          TRUE,
        nda_nominee_raw_party =
          chosen$raw_party,
        nda_nominee_official_party =
          chosen$eligible_official_party_name,
        nda_nominee_candidate =
          chosen$candidate,
        nda_nominee_is_bjp =
          FALSE,
        nda_ally_substitution =
          TRUE,
        nda_resolution_ambiguous =
          FALSE,
        n_eligible_non_bjp_nda_candidates =
          1L
      )
    )
  }

  if (
    nrow(
      ally_rows
    ) > 1
  ) {
    return(
      tibble::tibble(
        bjp_candidate_present =
          FALSE,
        nda_candidate_present =
          NA,
        nda_nominee_raw_party =
          NA_character_,
        nda_nominee_official_party =
          NA_character_,
        nda_nominee_candidate =
          NA_character_,
        nda_nominee_is_bjp =
          NA,
        nda_ally_substitution =
          NA,
        nda_resolution_ambiguous =
          TRUE,
        n_eligible_non_bjp_nda_candidates =
          nrow(
            ally_rows
          )
      )
    )
  }

  tibble::tibble(
    bjp_candidate_present =
      FALSE,
    nda_candidate_present =
      FALSE,
    nda_nominee_raw_party =
      NA_character_,
    nda_nominee_official_party =
      NA_character_,
    nda_nominee_candidate =
      NA_character_,
    nda_nominee_is_bjp =
      FALSE,
    nda_ally_substitution =
      FALSE,
    nda_resolution_ambiguous =
      FALSE,
    n_eligible_non_bjp_nda_candidates =
      0L
  )
}

pc_year_supply <- candidate_pc |>
  dplyr::group_by(
    year,
    state_name_raw,
    pc_no
  ) |>
  dplyr::group_modify(
    ~resolve_pc_nominee(
      .x
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    candidate_pc |>
      dplyr::group_by(
        year,
        state_name_raw,
        pc_no
      ) |>
      dplyr::summarise(
        pc_name =
          dplyr::first(
            stats::na.omit(
              pc_name
            )
          ),
        .groups = "drop"
      ),
    by = c(
      "year",
      "state_name_raw",
      "pc_no"
    )
  ) |>
  dplyr::mutate(
    state_key =
      normalize_geography(
        state_name_raw
      ),
    pc_name_key =
      normalize_geography(
        pc_name
      )
  )

# ============================================================
# 3. HARD NDA ROSTER VALIDATION
# ============================================================

ambiguous_supply <- pc_year_supply |>
  dplyr::filter(
    nda_resolution_ambiguous
  )

readr::write_csv(
  ambiguous_supply,
  file.path(
    out_manifest_dir,
    "nda_roster_ambiguous_pc_years.csv"
  )
)

if (
  nrow(
    ambiguous_supply
  ) > 0
) {
  stop(
    "NDA nominee reconstruction has ambiguous PC-years. ",
    "Review nda_roster_ambiguous_pc_years.csv before analysis."
  )
}

observed_party_counts <- pc_year_supply |>
  dplyr::filter(
    nda_candidate_present
  ) |>
  dplyr::count(
    year,
    nda_nominee_raw_party,
    name =
      "observed_n"
  ) |>
  dplyr::rename(
    raw_nominee_party =
      nda_nominee_raw_party
  )

party_count_audit <- expected_nominee_counts |>
  dplyr::full_join(
    observed_party_counts,
    by = c(
      "year",
      "raw_nominee_party"
    )
  ) |>
  dplyr::mutate(
    expected_n =
      dplyr::coalesce(
        expected_n,
        0L
      ),
    observed_n =
      dplyr::coalesce(
        observed_n,
        0L
      ),
    exact_match =
      expected_n ==
        observed_n
  ) |>
  dplyr::arrange(
    year,
    dplyr::desc(
      expected_n
    ),
    raw_nominee_party
  )

total_count_audit <- pc_year_supply |>
  dplyr::group_by(
    year
  ) |>
  dplyr::summarise(
    observed_total =
      sum(
        nda_candidate_present
      ),
    n_pcs =
      dplyr::n(),
    n_bjp_candidates =
      sum(
        bjp_candidate_present
      ),
    n_ally_substitutions =
      sum(
        nda_ally_substitution
      ),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    expected_total_nda,
    by = "year"
  ) |>
  dplyr::mutate(
    exact_match =
      observed_total ==
        expected_total
  )

readr::write_csv(
  party_count_audit,
  file.path(
    out_manifest_dir,
    "nda_roster_party_count_audit.csv"
  )
)

readr::write_csv(
  total_count_audit,
  file.path(
    out_manifest_dir,
    "nda_roster_total_count_audit.csv"
  )
)

if (
  any(
    !party_count_audit$exact_match
  ) ||
  any(
    !total_count_audit$exact_match
  )
) {
  stop(
    "NDA roster reconstruction does not exactly reproduce published 2009/2014 roster counts."
  )
}

readr::write_csv(
  pc_year_supply,
  file.path(
    out_table_dir,
    "pc_year_candidate_supply_2009_2014.csv"
  )
)

# ============================================================
# 4. MAP SUPPLY TO THE ANALYSIS GEOGRAPHY
# ============================================================

ac_year <- readRDS(
  file.path(
    paths$final_dir,
    "ac_year.rds"
  )
)

if (
  !"pc" %in%
    names(
      ac_year
    ) ||
  !"state" %in%
    names(
      ac_year
    )
) {
  stop(
    "ac_year must contain state and pc for candidate-supply mapping."
  )
}

ac_year_keys <- ac_year |>
  dplyr::mutate(
    state_key =
      normalize_geography(
        state
      ),

    pc_no_key =
      suppressWarnings(
        as.integer(
          as.character(
            pc
          )
        )
      ),

    pc_name_key =
      normalize_geography(
        pc_name
      )
  )

supply_numeric <- pc_year_supply |>
  dplyr::select(
    year,
    state_key,
    pc_no_key =
      pc_no,
    bjp_candidate_present_derived =
      bjp_candidate_present,
    nda_candidate_present,
    nda_nominee_raw_party,
    nda_nominee_official_party,
    nda_nominee_candidate,
    nda_nominee_is_bjp,
    nda_ally_substitution
  )

supply_name <- pc_year_supply |>
  dplyr::select(
    year,
    state_key,
    pc_name_key,
    bjp_candidate_present_by_name =
      bjp_candidate_present,
    nda_candidate_present_by_name =
      nda_candidate_present,
    nda_nominee_raw_party_by_name =
      nda_nominee_raw_party,
    nda_nominee_official_party_by_name =
      nda_nominee_official_party,
    nda_nominee_candidate_by_name =
      nda_nominee_candidate,
    nda_nominee_is_bjp_by_name =
      nda_nominee_is_bjp,
    nda_ally_substitution_by_name =
      nda_ally_substitution
  ) |>
  dplyr::distinct(
    year,
    state_key,
    pc_name_key,
    .keep_all = TRUE
  )

ac_year_supply <- ac_year_keys |>
  dplyr::left_join(
    supply_numeric,
    by = c(
      "year",
      "state_key",
      "pc_no_key"
    )
  ) |>
  dplyr::left_join(
    supply_name,
    by = c(
      "year",
      "state_key",
      "pc_name_key"
    )
  ) |>
  dplyr::mutate(
    bjp_candidate_present_derived =
      dplyr::coalesce(
        bjp_candidate_present_derived,
        bjp_candidate_present_by_name
      ),

    nda_candidate_present =
      dplyr::coalesce(
        nda_candidate_present,
        nda_candidate_present_by_name
      ),

    nda_nominee_raw_party =
      dplyr::coalesce(
        nda_nominee_raw_party,
        nda_nominee_raw_party_by_name
      ),

    nda_nominee_official_party =
      dplyr::coalesce(
        nda_nominee_official_party,
        nda_nominee_official_party_by_name
      ),

    nda_nominee_candidate =
      dplyr::coalesce(
        nda_nominee_candidate,
        nda_nominee_candidate_by_name
      ),

    nda_nominee_is_bjp =
      dplyr::coalesce(
        nda_nominee_is_bjp,
        nda_nominee_is_bjp_by_name
      ),

    nda_ally_substitution =
      dplyr::coalesce(
        nda_ally_substitution,
        nda_ally_substitution_by_name
      )
  )

mapping_audit <- ac_year_supply |>
  dplyr::filter(
    year %in%
      c(
        2009,
        2014
      )
  ) |>
  dplyr::summarise(
    n_ac_year =
      dplyr::n(),

    n_supply_mapped =
      sum(
        !is.na(
          bjp_candidate_present_derived
        )
      ),

    share_supply_mapped =
      mean(
        !is.na(
          bjp_candidate_present_derived
        )
      ),

    n_bjp_flag_comparable =
      sum(
        !is.na(
          bjp_candidate_present
        ) &
        !is.na(
          bjp_candidate_present_derived
        )
      ),

    n_bjp_flag_mismatch =
      sum(
        !is.na(
          bjp_candidate_present
        ) &
        !is.na(
          bjp_candidate_present_derived
        ) &
        as.logical(
          bjp_candidate_present
        ) !=
          as.logical(
            bjp_candidate_present_derived
          )
      )
  )

readr::write_csv(
  mapping_audit,
  file.path(
    out_manifest_dir,
    "candidate_supply_ac_year_mapping_audit.csv"
  )
)

bjp_mapping_mismatches <- ac_year_supply |>
  dplyr::filter(
    year %in%
      c(
        2009,
        2014
      ),
    !is.na(
      bjp_candidate_present
    ),
    !is.na(
      bjp_candidate_present_derived
    ),
    as.logical(
      bjp_candidate_present
    ) !=
      as.logical(
        bjp_candidate_present_derived
      )
  ) |>
  dplyr::select(
    year,
    state,
    pc,
    pc_name,
    pc_cluster_id,
    ac_uid,
    bjp_candidate_present,
    bjp_candidate_present_derived
  )

readr::write_csv(
  bjp_mapping_mismatches,
  file.path(
    out_manifest_dir,
    "candidate_supply_bjp_mapping_mismatches.csv"
  )
)

if (
  mapping_audit$share_supply_mapped <
    0.99
) {
  stop(
    "Less than 99% of 2009/2014 AC-year rows mapped to reconstructed PC supply."
  )
}

# Candidate availability is fundamentally a PARLIAMENTARY-CONSTITUENCY
# quantity. The raw candidate roster therefore provides the canonical supply
# measure for the new mechanism analysis. The existing AC-year pipeline flag is
# retained only to audit the frozen respondent sample that used it.
#
# The current data reveal a tiny number of AC-year disagreements. Because a BJP
# candidate cannot be simultaneously present and absent across assembly
# segments of the same PC, within-PC variation in the old AC flag is itself
# evidence that the AC-level flag should not be promoted to the PC supply
# definition.
if (
  mapping_audit$n_bjp_flag_mismatch >
    0
) {
  warning(
    "Found ",
    mapping_audit$n_bjp_flag_mismatch,
    " AC-year disagreements between the frozen AC flag and the canonical ",
    "raw PC candidate roster. The raw PC roster will define candidate supply; ",
    "the frozen flag is retained for comparison only. Review ",
    "candidate_supply_bjp_mapping_mismatches.csv."
  )
}

ac_year_supply <- ac_year_supply |>
  dplyr::mutate(
    bjp_candidate_present_existing =
      as.logical(
        bjp_candidate_present
      ),

    bjp_candidate_present_canonical =
      dplyr::coalesce(
        as.logical(
          bjp_candidate_present_derived
        ),
        bjp_candidate_present_existing
      ),

    candidate_mapping_conflict =
      !is.na(
        bjp_candidate_present_existing
      ) &
      !is.na(
        bjp_candidate_present_derived
      ) &
      bjp_candidate_present_existing !=
        as.logical(
          bjp_candidate_present_derived
        )
  )

pc_conflict_audit <- ac_year_supply |>
  dplyr::filter(
    year %in%
      c(
        2009,
        2014
      ),
    !is.na(
      pc_cluster_id
    )
  ) |>
  dplyr::group_by(
    year,
    state_no,
    pc_cluster_id
  ) |>
  dplyr::summarise(
    n_ac_segments =
      dplyr::n(),

    n_mapping_conflicts =
      sum(
        candidate_mapping_conflict,
        na.rm = TRUE
      ),

    frozen_ac_flag_varies_within_pc =
      dplyr::n_distinct(
        bjp_candidate_present_existing[
          !is.na(
            bjp_candidate_present_existing
          )
        ]
      ) >
        1,

    canonical_raw_flag_varies_within_pc =
      dplyr::n_distinct(
        bjp_candidate_present_canonical[
          !is.na(
            bjp_candidate_present_canonical
          )
        ]
      ) >
        1,

    .groups = "drop"
  )

readr::write_csv(
  pc_conflict_audit,
  file.path(
    out_manifest_dir,
    "candidate_supply_pc_mapping_conflict_audit.csv"
  )
)

# It is acceptable (and substantively informative) for the OLD AC-level flag
# to vary within a PC; that is precisely the anomaly this analysis is auditing.
# The canonical raw-PC flag, however, must be constant within an analysis PC.
if (
  any(
    pc_conflict_audit$canonical_raw_flag_varies_within_pc
  )
) {
  stop(
    "The canonical raw-election BJP flag varies within an analysis pc_cluster_id. ",
    "That would indicate a genuine PC geography mapping problem. Review ",
    "candidate_supply_pc_mapping_conflict_audit.csv before continuing."
  )
}

# ============================================================
# 5. PC-YEAR CONTEXT AND SUPPLY TRANSITIONS
# ============================================================

weighted_mean_safe <- function(
    x,
    w
) {
  ok <-
    is.finite(x) &
    is.finite(w) &
    w >
      0

  if (
    !any(ok)
  ) {
    return(
      NA_real_
    )
  }

  sum(
    x[ok] *
      w[ok]
  ) /
    sum(
      w[ok]
    )
}

sum_safe <- function(
    x
) {
  x <- x[
    is.finite(x)
  ]

  if (
    length(x) == 0
  ) {
    return(
      NA_real_
    )
  }

  sum(x)
}

pc_year_context <- ac_year_supply |>
  dplyr::filter(
    year %in%
      c(
        2009,
        2014
      ),
    !is.na(
      pc_cluster_id
    )
  ) |>
  dplyr::group_by(
    year,
    state_no,
    pc_cluster_id
  ) |>
  dplyr::summarise(
    state =
      dplyr::first(
        state
      ),

    pc_name =
      dplyr::first(
        pc_name
      ),

    candidate_mapping_conflict =
      any(
        candidate_mapping_conflict,
        na.rm = TRUE
      ),

    frozen_ac_flag_varies_within_pc =
      dplyr::n_distinct(
        bjp_candidate_present_existing[
          !is.na(
            bjp_candidate_present_existing
          )
        ]
      ) >
        1,

    bjp_candidate_present =
      dplyr::first(
        bjp_candidate_present_canonical[
          !is.na(
            bjp_candidate_present_canonical
          )
        ],
        default = NA
      ),

    nda_candidate_present =
      dplyr::first(
        nda_candidate_present[
          !is.na(
            nda_candidate_present
          )
        ],
        default = NA
      ),

    nda_nominee_is_bjp =
      dplyr::first(
        nda_nominee_is_bjp[
          !is.na(
            nda_nominee_is_bjp
          )
        ],
        default = NA
      ),

    nda_ally_substitution =
      dplyr::first(
        nda_ally_substitution[
          !is.na(
            nda_ally_substitution
          )
        ],
        default = NA
      ),

    pc_population =
      sum_safe(
        proxy_ac_pop
      ),

    pc_land_area =
      sum_safe(
        con08_land_area
      ),

    pc_sc_share =
      weighted_mean_safe(
        sc_pop_share,
        proxy_ac_pop
      ),

    pc_st_share =
      weighted_mean_safe(
        st_pop_share,
        proxy_ac_pop
      ),

    pc_fdi_mfg_local_all_log_pc100k =
      weighted_mean_safe(
        log1p_fdi_mfg_local_all_pc100k,
        proxy_ac_pop
      ),

    pc_muslim_share_2001 =
      weighted_mean_safe(
        muslim_share_2001_dist_proxy,
        proxy_ac_pop
      ),

    pc_migration_stock_share_2001 =
      weighted_mean_safe(
        mig_total_upto_2001_share_ac_pop,
        proxy_ac_pop
      ),

    pc_valid_votes =
      sum_safe(
        valid_votes
      ),

    pc_bjp_votes =
      sum_safe(
        bjp_votes
      ),

    pc_bjp_vote_share =
      dplyr::if_else(
        is.finite(
          pc_valid_votes
        ) &
        pc_valid_votes >
          0,
        100 *
          pc_bjp_votes /
          pc_valid_votes,
        NA_real_
      ),

    n_ac_segments =
      dplyr::n(),

    .groups =
      "drop"
  )

pc_wide <- pc_year_context |>
  tidyr::pivot_wider(
    id_cols = c(
      state_no,
      pc_cluster_id
    ),

    names_from =
      year,

    values_from = c(
      state,
      pc_name,
      bjp_candidate_present,
      nda_candidate_present,
      nda_nominee_is_bjp,
      nda_ally_substitution,
      pc_population,
      pc_land_area,
      pc_sc_share,
      pc_st_share,
      pc_fdi_mfg_local_all_log_pc100k,
      pc_muslim_share_2001,
      pc_migration_stock_share_2001,
      pc_valid_votes,
      pc_bjp_votes,
      pc_bjp_vote_share,
      n_ac_segments
    ),

    names_glue =
      "{.value}_{year}"
  ) |>
  dplyr::mutate(
    bjp_supply_transition =
      dplyr::case_when(
        is.na(
          bjp_candidate_present_2009
        ) |
        is.na(
          bjp_candidate_present_2014
        ) ~
          NA_character_,

        !bjp_candidate_present_2009 &
          !bjp_candidate_present_2014 ~
          "Absent -> absent",

        !bjp_candidate_present_2009 &
          bjp_candidate_present_2014 ~
          "Entry: absent -> present",

        bjp_candidate_present_2009 &
          !bjp_candidate_present_2014 ~
          "Exit: present -> absent",

        TRUE ~
          "Present -> present"
      ),

    nda_supply_transition =
      dplyr::case_when(
        is.na(
          nda_candidate_present_2009
        ) |
        is.na(
          nda_candidate_present_2014
        ) ~
          NA_character_,

        !nda_candidate_present_2009 &
          !nda_candidate_present_2014 ~
          "Absent -> absent",

        !nda_candidate_present_2009 &
          nda_candidate_present_2014 ~
          "Entry: absent -> present",

        nda_candidate_present_2009 &
          !nda_candidate_present_2014 ~
          "Exit: present -> absent",

        TRUE ~
          "Present -> present"
      ),

    always_bjp_contested =
      bjp_candidate_present_2009 &
      bjp_candidate_present_2014,

    always_nda_contested =
      nda_candidate_present_2009 &
      nda_candidate_present_2014
  )

readr::write_csv(
  pc_year_context,
  file.path(
    out_table_dir,
    "pc_year_context_candidate_supply.csv"
  )
)

readr::write_csv(
  pc_wide,
  file.path(
    out_table_dir,
    "pc_supply_transitions_2009_2014.csv"
  )
)

transition_counts <- dplyr::bind_rows(
  pc_wide |>
    dplyr::count(
      bjp_supply_transition,
      name =
        "n_pcs"
    ) |>
    dplyr::transmute(
      supply_definition =
        "BJP candidate",
      transition =
        bjp_supply_transition,
      n_pcs
    ),

  pc_wide |>
    dplyr::count(
      nda_supply_transition,
      name =
        "n_pcs"
    ) |>
    dplyr::transmute(
      supply_definition =
        "Official pre-poll NDA nominee",
      transition =
        nda_supply_transition,
      n_pcs
    )
)

readr::write_csv(
  transition_counts,
  file.path(
    out_table_dir,
    "candidate_supply_transition_counts.csv"
  )
)

# ============================================================
# 6. DESCRIPTIVE PC-LEVEL PARTY-SUPPLY MODELS
# ============================================================
#
# These model BJP's 2014 decision/alliance allocation to field the BJP
# itself. They are descriptive mechanism models, not causal estimates.

normalize_interaction_term <- function(
    x
) {
  vapply(
    strsplit(
      x,
      ":",
      fixed = TRUE
    ),
    function(parts) {
      paste(
        sort(
          gsub(
            "`",
            "",
            parts,
            fixed = TRUE
          )
        ),
        collapse = ":"
      )
    },
    character(1)
  )
}

find_interaction_term <- function(
    fit,
    vars
) {
  beta_names <- names(
    stats::coef(
      fit
    )
  )

  target <- paste(
    sort(vars),
    collapse = ":"
  )

  matches <- beta_names[
    normalize_interaction_term(
      beta_names
    ) ==
      target
  ]

  if (
    length(matches) != 1
  ) {
    return(
      NA_character_
    )
  }

  matches[[1]]
}

supply_model_data <- pc_wide |>
  dplyr::mutate(
    log1p_pc_population =
      log1p(
        pc_population_2014
      )
  )

fit_supply_model <- function(
    moderator_var,
    domain_label,
    vcov_mode =
      "hetero"
) {
  rhs <- paste0(
    "bjp_candidate_present_2014 ~ ",
    "bjp_candidate_present_2009 + ",
    "pc_bjp_vote_share_2009 + ",
    "pc_fdi_mfg_local_all_log_pc100k_2014 * ",
    moderator_var,
    " + ",
    "pc_fdi_mfg_local_all_log_pc100k_2009 + ",
    "log1p_pc_population + ",
    "pc_land_area_2014 + ",
    "pc_sc_share_2014 + ",
    "pc_st_share_2014 | ",
    "state_no"
  )

  formula <- stats::as.formula(
    rhs
  )

  required <- unique(
    c(
      all.vars(
        formula
      ),
      if (
        vcov_mode ==
          "state_cluster"
      ) {
        "state_no"
      } else {
        character(0)
      }
    )
  )

  d <- supply_model_data |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          required
        ),
        ~!is.na(
          .x
        )
      )
    )

  fit <- if (
    vcov_mode ==
      "state_cluster"
  ) {
    fixest::feols(
      formula,
      data = d,
      vcov =
        ~state_no,
      notes = FALSE,
      warn = FALSE
    )
  } else {
    fixest::feols(
      formula,
      data = d,
      vcov =
        "hetero",
      notes = FALSE,
      warn = FALSE
    )
  }

  target_term <- find_interaction_term(
    fit,
    c(
      "pc_fdi_mfg_local_all_log_pc100k_2014",
      moderator_var
    )
  )

  if (
    is.na(
      target_term
    )
  ) {
    stop(
      "Supply-model interaction term was dropped for ",
      domain_label,
      " / ",
      vcov_mode,
      "."
    )
  }

  beta <-
    stats::coef(
      fit
    )[
      target_term
    ]

  se <-
    fixest::se(
      fit
    )[
      target_term
    ]

  tibble::tibble(
    domain =
      domain_label,
    moderator_var =
      moderator_var,
    vcov =
      vcov_mode,
    interaction_term =
      target_term,
    interaction_estimate =
      unname(
        beta
      ),
    interaction_se =
      unname(
        se
      ),
    interaction_p =
      unname(
        fixest::pvalue(
          fit
        )[
          target_term
        ]
      ),
    interaction_conf_low =
      unname(
        beta -
          stats::qnorm(
            0.975
          ) *
          se
      ),
    interaction_conf_high =
      unname(
        beta +
          stats::qnorm(
            0.975
          ) *
          se
      ),
    nobs =
      stats::nobs(
        fit
      ),
    n_states =
      dplyr::n_distinct(
        d$state_no
      ),
    formula =
      paste(
        deparse(
          formula
        ),
        collapse = " "
      )
  )
}

supply_model_results <- dplyr::bind_rows(
  fit_supply_model(
    "pc_muslim_share_2001_2014",
    "Muslim share, 2001",
    "hetero"
  ),
  fit_supply_model(
    "pc_muslim_share_2001_2014",
    "Muslim share, 2001",
    "state_cluster"
  ),
  fit_supply_model(
    "pc_migration_stock_share_2001_2014",
    "Established migration stock",
    "hetero"
  ),
  fit_supply_model(
    "pc_migration_stock_share_2001_2014",
    "Established migration stock",
    "state_cluster"
  )
)

readr::write_csv(
  supply_model_results,
  file.path(
    out_model_dir,
    "pc_bjp_candidate_supply_models.csv"
  )
)

# NDA presence itself is nearly universal in 2014. Record whether there is
# enough variation to estimate a meaningful NDA-presence model rather than
# mechanically fitting one.
nda_variation_audit <- supply_model_data |>
  dplyr::summarise(
    n_pcs =
      sum(
        !is.na(
          nda_candidate_present_2014
        )
      ),
    n_nda_present =
      sum(
        nda_candidate_present_2014,
        na.rm = TRUE
      ),
    n_nda_absent =
      sum(
        !nda_candidate_present_2014,
        na.rm = TRUE
      ),
    meaningful_binary_model =
      n_nda_present >=
        10 &
      n_nda_absent >=
        10
  )

readr::write_csv(
  nda_variation_audit,
  file.path(
    out_model_dir,
    "nda_candidate_presence_2014_variation_audit.csv"
  )
)

# ============================================================
# 7. ALWAYS-BJP-CONTESTED RESPONDENT SENSITIVITY
# ============================================================

respondents <- readRDS(
  file.path(
    paths$final_dir,
    "nes_respondent_analysis.rds"
  )
)

ac_change <- readRDS(
  file.path(
    paths$final_dir,
    "ac_change.rds"
  )
)

bridge_fdi <-
  "log1p_fdi_mfg_local_all_pc100k"

bridge_fdi_baseline <-
  "log1p_fdi_mfg_local_all_pc100k_2009"

baseline_payload_vars <- c(
  "ac_uid",
  bridge_fdi_baseline,
  "bjp_vote_share_2009"
)

missing_baseline <- setdiff(
  baseline_payload_vars,
  names(
    ac_change
  )
)

if (
  length(
    missing_baseline
  ) > 0
) {
  stop(
    "ac_change is missing respondent baseline variables: ",
    paste(
      missing_baseline,
      collapse = ", "
    )
  )
}

payload_to_join <- setdiff(
  baseline_payload_vars,
  names(
    respondents
  )
)

if (
  length(
    payload_to_join
  ) > 0
) {
  respondents <- respondents |>
    dplyr::left_join(
      ac_change |>
        dplyr::select(
          ac_uid,
          dplyr::all_of(
            payload_to_join
          )
        ) |>
        dplyr::distinct(
          ac_uid,
          .keep_all = TRUE
        ),
      by =
        "ac_uid",
      relationship =
        "many-to-one"
    )
}

canonical_pc_supply_for_respondents <- pc_wide |>
  dplyr::transmute(
    pc_cluster_id,
    bjp_candidate_present_canonical_2009 =
      as.logical(
        bjp_candidate_present_2009
      ),
    bjp_candidate_present_canonical_2014 =
      as.logical(
        bjp_candidate_present_2014
      ),
    always_bjp_contested_canonical =
      as.logical(
        always_bjp_contested
      )
  )

respondents <- respondents |>
  dplyr::left_join(
    canonical_pc_supply_for_respondents,
    by = "pc_cluster_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    center_2014 =
      dplyr::case_when(
        year ==
          2014 &
        ideology_complete &
        as.character(
          voter_ideology
        ) ==
          "Center" ~
          1,

        year ==
          2014 &
        ideology_complete ~
          0,

        TRUE ~
          NA_real_
      ),

    # Frozen sample flag used in the original respondent multiverse.
    respondent_sample_candidate_present_frozen =
      vote_valid &
      !is.na(
        voted_bjp
      ) &
      !is.na(
        bjp_candidate_present
      ) &
      bjp_candidate_present ==
        1,

    # Canonical PC-level availability reconstructed from the raw candidate
    # roster. This is the correct supply definition for the new analysis.
    respondent_sample_candidate_present_canonical =
      vote_valid &
      !is.na(
        voted_bjp
      ) &
      !is.na(
        bjp_candidate_present_canonical_2014
      ) &
      bjp_candidate_present_canonical_2014,

    respondent_candidate_sample_changed =
      respondent_sample_candidate_present_frozen !=
        respondent_sample_candidate_present_canonical
  )

respondent_candidate_reconciliation <- respondents |>
  dplyr::filter(
    year == 2014,
    vote_valid,
    !is.na(
      voted_bjp
    )
  ) |>
  dplyr::summarise(
    n_valid_2014 =
      dplyr::n(),
    n_frozen_candidate_present =
      sum(
        respondent_sample_candidate_present_frozen,
        na.rm = TRUE
      ),
    n_canonical_candidate_present =
      sum(
        respondent_sample_candidate_present_canonical,
        na.rm = TRUE
      ),
    n_respondents_whose_candidate_sample_changes =
      sum(
        respondent_candidate_sample_changed,
        na.rm = TRUE
      ),
    n_pcs_affected =
      dplyr::n_distinct(
        pc_cluster_id[
          respondent_candidate_sample_changed
        ]
      )
  )

readr::write_csv(
  respondent_candidate_reconciliation,
  file.path(
    out_manifest_dir,
    "respondent_frozen_vs_canonical_candidate_sample_audit.csv"
  )
)

respondent_result_dir <- file.path(
  model_exploration_dir,
  "respondent_specification_curves",
  "results"
)

primary_result_file <- function(
    domain,
    order
) {
  file.path(
    respondent_result_dir,
    paste0(
      "primary__respondent_2014_",
      domain,
      "__mfg__",
      order,
      "__full__",
      RESPONDENT_RESULT_REVISION,
      ".csv"
    )
  )
}

get_primary_reference <- function(
    domain,
    moderator_var,
    order
) {
  path <- primary_result_file(
    domain,
    order
  )

  d <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::filter(
      fdi_scope ==
        "local",
      fdi_status ==
        "all",
      fdi_form ==
        "log1p_pc100k",
      .data$moderator_var ==
        .env$moderator_var,
      voter_control_set ==
        "V2",
      context_control_set ==
        "C1"
    )

  if (
    nrow(d) != 1
  ) {
    stop(
      "Could not uniquely recover primary reference for ",
      domain,
      " / ",
      order,
      " / moderator=",
      moderator_var,
      ". Found ",
      nrow(d),
      " row(s)."
    )
  }

  d
}

fit_candidate_supply_sample <- function(
    domain,
    moderator_var,
    order,
    sample_mode = c(
      "canonical_2014",
      "always_contested"
    )
) {
  sample_mode <- match.arg(
    sample_mode
  )
  primary_ref <- get_primary_reference(
    domain,
    moderator_var,
    order
  )

  d <- respondents |>
    dplyr::filter(
      year ==
        2014
    )

  if (
    sample_mode ==
      "canonical_2014"
  ) {
    d <- d |>
      dplyr::filter(
        respondent_sample_candidate_present_canonical
      )
  } else {
    d <- d |>
      dplyr::filter(
        respondent_sample_candidate_present_canonical,
        always_bjp_contested_canonical
      )
  }

  if (
    order ==
      "triple"
  ) {
    d <- d |>
      dplyr::filter(
        ideology_complete,
        !is.na(
          center_2014
        )
      )
  }

  interaction_text <- if (
    order ==
      "two_way"
  ) {
    paste0(
      bridge_fdi,
      " * ",
      moderator_var
    )
  } else {
    paste0(
      bridge_fdi,
      " * ",
      moderator_var,
      " * center_2014"
    )
  }

  formula <- stats::as.formula(
    paste0(
      "voted_bjp ~ ",
      interaction_text,
      " + ",
      bridge_fdi_baseline,
      " + bjp_vote_share_2009 + ",
      "religion_group + caste_group + education_harmonized + ",
      "proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share | ",
      "state_no"
    )
  )

  required <- unique(
    c(
      all.vars(
        formula
      ),
      "survey_weight_norm_year",
      "pc_cluster_id",
      "district_harmonization_group_id"
    )
  )

  complete <- rep(
    TRUE,
    nrow(d)
  )

  for (
    v in required
  ) {
    x <- d[[
      v
    ]]

    if (
      is.numeric(x) ||
      is.integer(x)
    ) {
      complete <-
        complete &
        !is.na(x) &
        is.finite(
          as.numeric(x)
        )
    } else {
      complete <-
        complete &
        !is.na(x)
    }
  }

  d <- d[
    complete,
    ,
    drop = FALSE
  ] |>
    dplyr::filter(
      survey_weight_norm_year >
        0
    )

  fit <- fixest::feols(
    formula,
    data = d,
    weights =
      ~survey_weight_norm_year,
    vcov =
      ~pc_cluster_id +
      district_harmonization_group_id,
    notes = FALSE,
    warn = FALSE
  )

  target_vars <- if (
    order ==
      "two_way"
  ) {
    c(
      bridge_fdi,
      moderator_var
    )
  } else {
    c(
      bridge_fdi,
      moderator_var,
      "center_2014"
    )
  }

  term <- find_interaction_term(
    fit,
    target_vars
  )

  if (
    is.na(
      term
    )
  ) {
    stop(
      "Always-contested target interaction was dropped: ",
      domain,
      " / ",
      order,
      "."
    )
  }

  multiplier <-
    100 *
    primary_ref$delta_fdi[[1]] *
    primary_ref$delta_moderator[[1]]

  beta <-
    unname(
      stats::coef(
        fit
      )[
        term
      ]
    )

  se <-
    unname(
      fixest::se(
        fit
      )[
        term
      ]
    )

  contrast <-
    beta *
    multiplier

  contrast_se <-
    abs(
      multiplier
    ) *
    se

  tibble::tibble(
    domain =
      domain,
    moderator_var =
      moderator_var,
    interaction_order =
      order,
    sample_mode =
      sample_mode,
    sample =
      dplyr::case_when(
        sample_mode ==
          "canonical_2014" ~
          "Canonical raw-PC BJP candidate present in 2014",
        TRUE ~
          "Canonical raw-PC BJP candidate present in both 2009 and 2014"
      ),
    interaction_estimate =
      beta,
    interaction_se =
      se,
    contrast_estimate_pp =
      contrast,
    contrast_se_pp =
      contrast_se,
    contrast_conf_low_pp =
      contrast -
      stats::qnorm(
        0.975
      ) *
      contrast_se,
    contrast_conf_high_pp =
      contrast +
      stats::qnorm(
        0.975
      ) *
      contrast_se,
    nobs =
      stats::nobs(
        fit
      ),
    n_pcs =
      dplyr::n_distinct(
        d$pc_cluster_id
      ),
    n_districts =
      dplyr::n_distinct(
        d$district_harmonization_group_id
      ),
    primary_full_sample_contrast_pp =
      primary_ref$contrast_estimate[[1]],
    primary_full_sample_conf_low_pp =
      primary_ref$contrast_conf_low[[1]],
    primary_full_sample_conf_high_pp =
      primary_ref$contrast_conf_high[[1]]
  )
}

candidate_supply_sample_results <- purrr::map_dfr(
  c(
    "canonical_2014",
    "always_contested"
  ),
  function(sample_mode_i) {
    dplyr::bind_rows(
      fit_candidate_supply_sample(
        "muslim",
        "muslim_share_2001_dist_proxy",
        "two_way",
        sample_mode_i
      ),

      fit_candidate_supply_sample(
        "muslim",
        "muslim_share_2001_dist_proxy",
        "triple",
        sample_mode_i
      ),

      fit_candidate_supply_sample(
        "migration",
        "mig_total_upto_2001_share_ac_pop",
        "two_way",
        sample_mode_i
      ),

      fit_candidate_supply_sample(
        "migration",
        "mig_total_upto_2001_share_ac_pop",
        "triple",
        sample_mode_i
      )
    )
  }
)

readr::write_csv(
  candidate_supply_sample_results,
  file.path(
    out_model_dir,
    "preferred_respondent_candidate_supply_sample_sensitivities.csv"
  )
)

always_contested_results <- candidate_supply_sample_results |>
  dplyr::filter(
    sample_mode ==
      "always_contested"
  )

readr::write_csv(
  always_contested_results,
  file.path(
    out_model_dir,
    "preferred_respondent_always_bjp_contested_sensitivity.csv"
  )
)

# ============================================================
# 8. SUPPLY-vs-CONDITIONAL-CHOICE DECOMPOSITION
# ============================================================
#
# For this mechanism decomposition, both margins use the SAME PC-level
# contextual exposure:
#   pc_fdi_mfg_local_all_log_pc100k
# and a PC-level demographic context.
#
# This deliberately differs from the preferred respondent AC-context
# specification. It makes the product
#
#   P(BJP candidate supplied | PC context)
#     x
#   P(voter chooses BJP | BJP supplied, same PC context)
#
# coherent on the same contextual scale.
#
# Point decomposition only; inferential bootstrap can be added if this
# mechanism exercise proves substantively informative.

pc2014_for_respondents <- pc_wide |>
  dplyr::transmute(
    pc_cluster_id,
    state_no_pc =
      state_no,

    bjp_candidate_present_2009_pc =
      as.numeric(
        bjp_candidate_present_2009
      ),

    bjp_candidate_present_2014_pc =
      as.numeric(
        bjp_candidate_present_2014
      ),

    bjp_vote_share_pc_2009 =
      pc_bjp_vote_share_2009,

    pc_fdi_2009 =
      pc_fdi_mfg_local_all_log_pc100k_2009,

    pc_fdi_2014 =
      pc_fdi_mfg_local_all_log_pc100k_2014,

    pc_muslim_2001 =
      pc_muslim_share_2001_2014,

    pc_migration_stock_2001 =
      pc_migration_stock_share_2001_2014,

    pc_population =
      pc_population_2014,

    pc_land_area =
      pc_land_area_2014,

    pc_sc_share =
      pc_sc_share_2014,

    pc_st_share =
      pc_st_share_2014
  ) |>
  dplyr::mutate(
    log1p_pc_population =
      log1p(
        pc_population
      )
  )

choice_context_data <- respondents |>
  dplyr::filter(
    year ==
      2014,
    respondent_sample_candidate_present_canonical
  ) |>
  dplyr::left_join(
    pc2014_for_respondents,
    by =
      "pc_cluster_id",
    relationship =
      "many-to-one"
  )

fit_decomposition_domain <- function(
    domain,
    moderator_pc_var
) {
  supply_formula <- stats::as.formula(
    paste0(
      "bjp_candidate_present_2014_pc ~ ",
      "bjp_candidate_present_2009_pc + ",
      "bjp_vote_share_pc_2009 + ",
      "pc_fdi_2014 * ",
      moderator_pc_var,
      " + pc_fdi_2009 + ",
      "log1p_pc_population + pc_land_area + pc_sc_share + pc_st_share | ",
      "state_no_pc"
    )
  )

  supply_d <- pc2014_for_respondents

  supply_required <- all.vars(
    supply_formula
  )

  missing_supply_required <- setdiff(
    supply_required,
    names(
      supply_d
    )
  )

  if (
    length(
      missing_supply_required
    ) > 0
  ) {
    stop(
      "Supply-decomposition data are missing formula variable(s): ",
      paste(
        missing_supply_required,
        collapse = ", "
      )
    )
  }

  supply_d <- supply_d |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          supply_required
        ),
        ~!is.na(
          .x
        )
      )
    )

  # Use a state-FE LPM for the supply propensity in the descriptive
  # decomposition. A fixed-effect logit would mechanically drop states with no
  # within-state variation in BJP-vs-ally seat allocation, making prediction
  # unavailable for precisely the states where BJP always contests.
  supply_fit <- fixest::feols(
    supply_formula,
    data =
      supply_d,
    vcov =
      "hetero",
    notes = FALSE,
    warn = FALSE
  )

  choice_formula <- stats::as.formula(
    paste0(
      "voted_bjp ~ ",
      "pc_fdi_2014 * ",
      moderator_pc_var,
      " + pc_fdi_2009 + ",
      "bjp_vote_share_pc_2009 + ",
      "religion_group + caste_group + education_harmonized + ",
      "log1p_pc_population + pc_land_area + pc_sc_share + pc_st_share | ",
      "state_no_pc"
    )
  )

  choice_required <- unique(
    c(
      all.vars(
        choice_formula
      ),
      "survey_weight_norm_year"
    )
  )

  choice_d <- choice_context_data

  missing_choice_required <- setdiff(
    choice_required,
    names(
      choice_d
    )
  )

  if (
    length(
      missing_choice_required
    ) > 0
  ) {
    stop(
      "Choice-decomposition data are missing formula/weight variable(s): ",
      paste(
        missing_choice_required,
        collapse = ", "
      )
    )
  }

  choice_keep <- rep(
    TRUE,
    nrow(
      choice_d
    )
  )

  for (
    v in choice_required
  ) {
    x <- choice_d[[
      v
    ]]

    if (
      is.numeric(x) ||
      is.integer(x)
    ) {
      choice_keep <-
        choice_keep &
        !is.na(x) &
        is.finite(
          as.numeric(x)
        )
    } else {
      choice_keep <-
        choice_keep &
        !is.na(x)
    }
  }

  choice_d <- choice_d[
    choice_keep,
    ,
    drop = FALSE
  ] |>
    dplyr::filter(
      survey_weight_norm_year >
        0
    )

  # Match the primary respondent estimator and keep all state strata in the
  # prediction sample. For the decomposition only, predictions are clipped to
  # [0,1] and the clipping rate is reported.
  choice_fit <- fixest::feols(
    choice_formula,
    data =
      choice_d,
    weights =
      ~survey_weight_norm_year,
    vcov =
      ~pc_cluster_id +
      district_harmonization_group_id,
    notes = FALSE,
    warn = FALSE
  )

  # Use one PC-level reference distribution for BOTH margins.
  pc_ref <- supply_d |>
    dplyr::filter(
      is.finite(
        pc_fdi_2014
      ),
      is.finite(
        .data[[
          moderator_pc_var
        ]]
      )
    )

  positive_fdi <- pc_ref$pc_fdi_2014[
    pc_ref$pc_fdi_2014 >
      0
  ]

  f_low <- 0
  f_high <- stats::median(
    positive_fdi,
    na.rm = TRUE
  )

  d_quantiles <- stats::quantile(
    pc_ref[[
      moderator_pc_var
    ]],
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE,
    names = FALSE
  )

  d_low <-
    as.numeric(
      d_quantiles[[1]]
    )

  d_high <-
    as.numeric(
      d_quantiles[[2]]
    )

  # Prediction rows are respondent rows, so supply and choice probabilities
  # can be multiplied at the same voter-PC observation before averaging.
  predict_scenario <- function(
      fdi_value,
      moderator_value
  ) {
    nd <- choice_d |>
      dplyr::mutate(
        pc_fdi_2014 =
          fdi_value
      )

    nd[[
      moderator_pc_var
    ]] <-
      moderator_value

    a_raw <- as.numeric(
      stats::predict(
        supply_fit,
        newdata =
          nd
      )
    )

    c_raw <- as.numeric(
      stats::predict(
        choice_fit,
        newdata =
          nd
      )
    )

    a <- pmin(
      pmax(
        a_raw,
        0
      ),
      1
    )

    c <- pmin(
      pmax(
        c_raw,
        0
      ),
      1
    )

    tibble::tibble(
      a =
        a,
      c =
        c,
      realized =
        a *
        c,
      supply_prediction_clipped =
        a_raw !=
          a,
      choice_prediction_clipped =
        c_raw !=
          c
    )
  }

  fl_dl <- predict_scenario(
    f_low,
    d_low
  )

  fh_dl <- predict_scenario(
    f_high,
    d_low
  )

  fl_dh <- predict_scenario(
    f_low,
    d_high
  )

  fh_dh <- predict_scenario(
    f_high,
    d_high
  )

  w <- choice_d$survey_weight_norm_year

  wmean <- function(
      x
  ) {
    stats::weighted.mean(
      x,
      w =
        w,
      na.rm = TRUE
    )
  }

  decompose_pair <- function(
      low_scenario,
      high_scenario,
      demographic_level
  ) {
    supply_component <-
      0.5 *
      (
        high_scenario$c +
        low_scenario$c
      ) *
      (
        high_scenario$a -
        low_scenario$a
      )

    choice_component <-
      0.5 *
      (
        high_scenario$a +
        low_scenario$a
      ) *
      (
        high_scenario$c -
        low_scenario$c
      )

    total_change <-
      high_scenario$realized -
      low_scenario$realized

    tibble::tibble(
      demographic_level =
        demographic_level,

      supply_component =
        wmean(
          supply_component
        ),

      conditional_choice_component =
        wmean(
          choice_component
        ),

      total_realized_change =
        wmean(
          total_change
        ),

      identity_error =
        total_realized_change -
        supply_component -
        conditional_choice_component
    )
  }

  decomposition_by_d <- dplyr::bind_rows(
    decompose_pair(
      fl_dl,
      fh_dl,
      "low"
    ),

    decompose_pair(
      fl_dh,
      fh_dh,
      "high"
    )
  )

  interaction_decomposition <- tibble::tibble(
    domain =
      domain,

    fdi_low =
      f_low,

    fdi_high =
      f_high,

    demographic_low =
      d_low,

    demographic_high =
      d_high,

    supply_contribution_pp =
      100 *
      (
        decomposition_by_d$supply_component[
          decomposition_by_d$demographic_level ==
            "high"
        ] -
        decomposition_by_d$supply_component[
          decomposition_by_d$demographic_level ==
            "low"
        ]
      ),

    conditional_choice_contribution_pp =
      100 *
      (
        decomposition_by_d$conditional_choice_component[
          decomposition_by_d$demographic_level ==
            "high"
        ] -
        decomposition_by_d$conditional_choice_component[
          decomposition_by_d$demographic_level ==
            "low"
        ]
      ),

    realized_interaction_contrast_pp =
      100 *
      (
        decomposition_by_d$total_realized_change[
          decomposition_by_d$demographic_level ==
            "high"
        ] -
        decomposition_by_d$total_realized_change[
          decomposition_by_d$demographic_level ==
            "low"
        ]
      ),

    decomposition_identity_error_pp =
      realized_interaction_contrast_pp -
      supply_contribution_pp -
      conditional_choice_contribution_pp,

    n_choice_respondents =
      nrow(
        choice_d
      ),

    n_supply_pcs =
      nrow(
        supply_d
      ),

    max_supply_prediction_clipping_share =
      max(
        mean(
          fl_dl$supply_prediction_clipped
        ),
        mean(
          fh_dl$supply_prediction_clipped
        ),
        mean(
          fl_dh$supply_prediction_clipped
        ),
        mean(
          fh_dh$supply_prediction_clipped
        )
      ),

    max_choice_prediction_clipping_share =
      max(
        mean(
          fl_dl$choice_prediction_clipped
        ),
        mean(
          fh_dl$choice_prediction_clipped
        ),
        mean(
          fl_dh$choice_prediction_clipped
        ),
        mean(
          fh_dh$choice_prediction_clipped
        )
      ),

    note =
      "Descriptive model-based decomposition using state-FE LPM propensities clipped to [0,1]. Supply = probability BJP is fielded/allocated the NDA seat; choice = BJP vote conditional on BJP availability. Not a causal mediation estimand."
  )

  list(
    decomposition =
      interaction_decomposition,

    scenario_means =
      tibble::tibble(
        domain =
          domain,
        scenario =
          c(
            "FDI low / demographic low",
            "FDI high / demographic low",
            "FDI low / demographic high",
            "FDI high / demographic high"
          ),

        mean_supply_probability =
          c(
            wmean(
              fl_dl$a
            ),
            wmean(
              fh_dl$a
            ),
            wmean(
              fl_dh$a
            ),
            wmean(
              fh_dh$a
            )
          ),

        mean_conditional_choice_probability =
          c(
            wmean(
              fl_dl$c
            ),
            wmean(
              fh_dl$c
            ),
            wmean(
              fl_dh$c
            ),
            wmean(
              fh_dh$c
            )
          ),

        mean_realized_bjp_probability =
          c(
            wmean(
              fl_dl$realized
            ),
            wmean(
              fh_dl$realized
            ),
            wmean(
              fl_dh$realized
            ),
            wmean(
              fh_dh$realized
            )
          )
      )
  )
}

decomp_muslim <- fit_decomposition_domain(
  "muslim",
  "pc_muslim_2001"
)

decomp_migration <- fit_decomposition_domain(
  "migration",
  "pc_migration_stock_2001"
)

decomposition_results <- dplyr::bind_rows(
  decomp_muslim$decomposition,
  decomp_migration$decomposition
)

decomposition_scenarios <- dplyr::bind_rows(
  decomp_muslim$scenario_means,
  decomp_migration$scenario_means
)

readr::write_csv(
  decomposition_results,
  file.path(
    out_model_dir,
    "bjp_support_supply_vs_choice_decomposition.csv"
  )
)

readr::write_csv(
  decomposition_scenarios,
  file.path(
    out_model_dir,
    "bjp_support_decomposition_scenario_probabilities.csv"
  )
)

# ============================================================
# 9. FIGURES
# ============================================================

p_transitions <- transition_counts |>
  dplyr::filter(
    !is.na(
      transition
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        transition,
      y =
        n_pcs
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(
    ~supply_definition,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "BJP and NDA candidate-supply transitions, 2009→2014",

    x = NULL,

    y =
      "Parliamentary constituencies"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(
    axis.text.x =
      ggplot2::element_text(
        angle = 30,
        hjust = 1
      )
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "01_candidate_supply_transitions.pdf"
  ),
  p_transitions,
  width = 11,
  height = 6.5
)

p_always <- always_contested_results |>
  dplyr::mutate(
    domain_label =
      dplyr::recode(
        domain,
        muslim =
          "Muslim share, 2001",
        migration =
          "Established migration stock"
      ),

    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI × demographic",
        triple =
          "Center amplification"
      )
  ) |>
  tidyr::pivot_longer(
    cols = c(
      contrast_estimate_pp,
      primary_full_sample_contrast_pp
    ),
    names_to =
      "sample_type",
    values_to =
      "estimate_pp"
  ) |>
  dplyr::mutate(
    sample_type =
      dplyr::recode(
        sample_type,
        contrast_estimate_pp =
          "BJP contested in both elections",
        primary_full_sample_contrast_pp =
          "Primary 2014 candidate-present sample"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        estimate_pp,
      y =
        sample_type
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_grid(
    interaction_label ~
      domain_label,
    scales = "free_x"
  ) +
  ggplot2::labs(
    title =
      "Preferred respondent effects in the always-BJP-contested sample",

    x =
      "Substantive contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 9.5
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "02_always_bjp_contested_comparison.pdf"
  ),
  p_always,
  width = 11,
  height = 7
)

p_decomp <- decomposition_results |>
  dplyr::select(
    domain,
    supply_contribution_pp,
    conditional_choice_contribution_pp
  ) |>
  tidyr::pivot_longer(
    cols =
      -domain,
    names_to =
      "component",
    values_to =
      "contribution_pp"
  ) |>
  dplyr::mutate(
    domain =
      dplyr::recode(
        domain,
        muslim =
          "Muslim share, 2001",
        migration =
          "Established migration stock"
      ),

    component =
      dplyr::recode(
        component,
        supply_contribution_pp =
          "BJP candidate-supply margin",
        conditional_choice_contribution_pp =
          "Conditional voter-choice margin"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        component,
      y =
        contribution_pp
    )
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(
    ~domain,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Model-based decomposition of the FDI × demographic realized-BJP contrast",

    subtitle =
      "Point decomposition only; descriptive, not causal mediation.",

    x = NULL,

    y =
      "Contribution to interaction contrast (percentage points)"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(
    axis.text.x =
      ggplot2::element_text(
        angle = 20,
        hjust = 1
      )
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "03_supply_vs_choice_decomposition.pdf"
  ),
  p_decomp,
  width = 10,
  height = 6.5
)

# ============================================================
# 10. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Candidate-supply analysis revision: ",
      CANDIDATE_SUPPLY_REVISION
    ),
    "",
    "DEFINITIONS",
    "-----------",
    "bjp_candidate_present: BJP itself fields a parliamentary candidate.",
    "nda_candidate_present: the official/designated pre-poll NDA nominee is reconstructed as present.",
    "nda_ally_substitution: NDA contests the seat, but the designated nominee is an ally rather than BJP.",
    "",
    "The NDA roster is seat-allocation aware. Merely treating every candidate from an NDA-member party",
    "as an NDA nominee is wrong because alliance parties sometimes also ran outside their allocated seats.",
    "The reconstruction therefore uses state-specific ally scope, BJP priority where BJP is the nominee,",
    "and seat/candidate-specific exceptions for supported independents and raw-label anomalies.",
    "",
    "BJP FLAG RECONCILIATION",
    "-----------------------",
    "Candidate supply is a parliamentary-constituency quantity, so the independently reconstructed",
    "raw candidate roster is canonical for the NEW supply analysis. The old AC-year pipeline flag is",
    "retained to document the frozen respondent sample. A small number of AC-year disagreements and",
    "within-PC variation in the old flag are reported rather than promoted into the PC supply measure.",
    "",
    "HARD VALIDATION",
    "---------------",
    "The script refuses to continue unless the raw election data reproduce exactly:",
    "  2009: 521 official NDA nominees",
    "  2014: 542 official NDA nominees",
    "and the expected raw-party nominee counts.",
    "",
    "KEY ANALYSES",
    "------------",
    "1. PC supply transitions (BJP and NDA) from 2009 to 2014.",
    "2. Descriptive PC-level model of whether BJP itself is fielded/allocated the seat in 2014.",
    "3. Preferred respondent models are first re-estimated using the canonical raw-PC 2014 candidate flag,",
    "   then restricted further to PCs with BJP candidates in BOTH 2009 and 2014.",
    "4. The script reports exactly how many respondents change sample membership relative to the frozen",
    "   AC-level candidate flag used by the original respondent multiverse.",
    "5. Descriptive model-based decomposition:",
    "     realized BJP support = BJP supply probability x BJP choice conditional on supply.",
    "",
    "NDA PRESENCE IN 2014",
    "--------------------",
    "Because the published NDA roster covers 542 of 543 PCs in 2014, NDA presence itself has",
    "almost no cross-sectional variation. The substantively useful supply question is therefore",
    "BJP nomination/seat allocation versus an NDA ally, not simply NDA-versus-no-NDA availability.",
    "",
    "CAUTION",
    "-------",
    "Candidate supply can be a post-treatment mediator if FDI changes party entry/seat allocation.",
    "For that reason this script does not inverse-probability-weight candidate presence away.",
    "The supply/choice decomposition is descriptive and is not labeled a causal mediation analysis.",
    "",
    "READ FIRST",
    "----------",
    "manifests/nda_roster_total_count_audit.csv",
    "tables/candidate_supply_transition_counts.csv",
    "models/pc_bjp_candidate_supply_models.csv",
    "models/preferred_respondent_always_bjp_contested_sensitivity.csv",
    "models/bjp_support_supply_vs_choice_decomposition.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Candidate-supply analysis COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "Read first: ",
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

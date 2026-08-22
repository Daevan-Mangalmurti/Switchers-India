# Switchers India: data cleaning and construction ----
#
# Purpose:
#   Clean each source in one self-contained section, create auditable
#   intermediate files, and construct final AC-year, respondent-level,
#   AC-year ideology-summary, and observed AC-year-ideology data.
#
# Unit of final data:
#   1. ac_year_final: one row per assembly constituency and election year.
#   2. nes_respondent_clean: one row per NES respondent.
#   3. ac_year_ideology_summary: one row per AC-year with wide ideology
#      composition and ideology-specific behavioral summaries.
#   4. ac_year_ideology_long_final: one row per observed AC-year-ideology cell.
#
# This script does not estimate regressions. Model-specific centering, factors,
# first differences, and regression objects belong in the analysis script.


# 00. Load libraries ----

library(tidyverse)
library(sf)
library(readxl)
library(haven)
library(janitor)
library(lubridate)
library(modelsummary)
library(fixest)


# 01. Configuration, helpers, and manual mappings ----

# 01.1 Project paths ----

PROJECT_ROOT <- "."

PATHS <- list(
  pc_elections = file.path(PROJECT_ROOT, "data", "election", "lok_dhaba_ge.csv"),
  ac_map_dir = file.path(PROJECT_ROOT, "data", "election", "2014_shp"),
  ac_name_key = file.path(PROJECT_ROOT, "data", "shrug", "ac08_name_key.csv"),
  ac_district_xwalk = file.path(
    PROJECT_ROOT,
    "data",
    "assembly-constituencies",
    "India_AC.shp"
  ),
  delhi_districts = file.path(PROJECT_ROOT, "data", "delhi_districts.geojson"),
  district_codes = file.path(PROJECT_ROOT, "data", "dist_codes.xlsx"),
  ac_population = file.path(PROJECT_ROOT, "data", "shrug", "con08_pop_area_key.csv"),
  population_change_dir = file.path(PROJECT_ROOT, "data", "pop", "pop_change"),
  economic_census = file.path(PROJECT_ROOT, "data", "shrug", "ec13_pc11dist.csv"),
  sc_population_dir = file.path(PROJECT_ROOT, "data", "pop", "sc_pop"),
  st_population_dir = file.path(PROJECT_ROOT, "data", "pop", "st_pop"),
  migration_dir = file.path(PROJECT_ROOT, "data", "pop", "migration"),
  fdi = file.path(PROJECT_ROOT, "data", "IN_FDI_2004_2014.csv"),
  nes_2009 = file.path(PROJECT_ROOT, "data", "lokniti", "nes_2009.sav"),
  nes_2014 = file.path(PROJECT_ROOT, "data", "lokniti", "nes_2014.sav")
)

DERIVED_DIR <- file.path(PROJECT_ROOT, "data", "derived", "switchers_rewrite")
INTERMEDIATE_DIR <- file.path(DERIVED_DIR, "intermediate")
FINAL_DIR <- file.path(DERIVED_DIR, "final")
DIAGNOSTIC_DIR <- file.path(DERIVED_DIR, "diagnostics")

walk(
  c(DERIVED_DIR, INTERMEDIATE_DIR, FINAL_DIR, DIAGNOSTIC_DIR),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# NES weights remain unset until a weight variable is substantively validated.
NES_WEIGHT_VAR_2009 <- NULL
NES_WEIGHT_VAR_2014 <- NULL

# A direct AC population total is treated as inconsistent with its district
# total when the known AC sum exceeds the district total by more than 5%.
POPULATION_INCONSISTENCY_TOLERANCE <- 1.05

# The ideology cut is applied to both standardized axes.
IDEOLOGY_CUT <- 0.25


# 01.2 General helper functions ----

assert_file_exists <- function(path, label = path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", label, " [", path, "]")
  }
  
  invisible(path)
}

assert_directory_exists <- function(path, label = path) {
  if (!dir.exists(path)) {
    stop("Missing required directory: ", label, " [", path, "]")
  }
  
  invisible(path)
}

assert_has_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  invisible(data)
}

assert_unique_rows <- function(data, keys, label) {
  duplicates <- data %>%
    group_by(across(all_of(keys))) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    print(duplicates, n = Inf)
    stop(label, " is not unique by ", paste(keys, collapse = ", "), ".")
  }
  
  invisible(data)
}

read_many <- function(
    path,
    pattern,
    reader = readr::read_csv,
    ...,
    ignore.case = TRUE
) {
  files <- list.files(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = ignore.case
  )
  
  if (length(files) == 0) {
    stop("No files matched pattern '", pattern, "' in ", path, ".")
  }
  
  map_dfr(files, \(file) reader(file, ...))
}

read_many_sf <- function(path, pattern, ..., ignore.case = TRUE) {
  files <- list.files(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = ignore.case
  )
  
  if (length(files) == 0) {
    stop("No spatial files matched pattern '", pattern, "' in ", path, ".")
  }
  
  files %>%
    map(\(file) read_sf(file, ...)) %>%
    bind_rows()
}

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    na_if("") %>%
    na_if("-") %>%
    parse_number()
}

first_nonmissing <- function(x) {
  valid_index <- which(!is.na(x))
  
  if (length(valid_index) == 0) {
    return(x[NA_integer_][1])
  }
  
  x[valid_index[1]]
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}

safe_mean <- function(x) {
  result <- mean(x, na.rm = TRUE)
  
  if (is.nan(result)) {
    NA_real_
  } else {
    result
  }
}

safe_z <- function(x) {
  x <- as.numeric(x)
  standard_deviation <- sd(x, na.rm = TRUE)
  
  if (is.na(standard_deviation) || standard_deviation == 0) {
    rep(NA_real_, length(x))
  } else {
    (x - mean(x, na.rm = TRUE)) / standard_deviation
  }
}

safe_pct <- function(numerator, denominator) {
  if_else(
    !is.na(denominator) & denominator > 0,
    100 * numerator / denominator,
    NA_real_
  )
}

safe_divide <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  result <- rep(NA_real_, length(numerator))
  
  valid <-
    !is.na(numerator) &
    !is.na(denominator) &
    is.finite(numerator) &
    is.finite(denominator) &
    denominator > 0
  
  result[valid] <- numerator[valid] / denominator[valid]
  result
}

safe_log <- function(x) {
  x <- as.numeric(x)
  result <- rep(NA_real_, length(x))
  
  valid <- !is.na(x) & is.finite(x) & x > 0
  
  result[valid] <- log(x[valid])
  result
}

safe_log_ratio <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  result <- rep(NA_real_, length(numerator))
  
  valid <-
    !is.na(numerator) &
    !is.na(denominator) &
    is.finite(numerator) &
    is.finite(denominator) &
    numerator > 0 &
    denominator > 0
  
  result[valid] <- log(numerator[valid] / denominator[valid])
  result
}

per_100k <- function(x, population) {
  if_else(
    !is.na(population) & population > 0,
    100000 * x / population,
    NA_real_
  )
}

norm_name <- function(x) {
  x %>%
    as.character() %>%
    str_remove_all("\\*") %>%
    str_to_upper() %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

norm_delhi_dist <- function(x) {
  norm_name(x) %>%
    str_remove("\\s+DISTRICT$") %>%
    str_remove("\\s+DELHI$") %>%
    str_remove("^DELHI\\s+")
}

make_code_key <- function(x) {
  if_else(is.na(x), NA_character_, paste0("CODE_", as.integer(x)))
}

district_name_key <- function(x) {
  if_else(is.na(x), NA_character_, paste0("NAME_", norm_name(x)))
}

row_mean_valid <- function(data, min_valid = 1L) {
  data <- as.data.frame(data)
  n_valid <- rowSums(!is.na(data))
  result <- rowMeans(data, na.rm = TRUE)
  result[n_valid < min_valid | is.nan(result)] <- NA_real_
  result
}

variable_label <- function(x, fallback) {
  label <- attr(x, "label")
  
  if (is.null(label) || length(label) == 0 || is.na(label) || label == "") {
    fallback
  } else {
    as.character(label)
  }
}

plain_col <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss"))) {
    as.vector(x)
  } else {
    x
  }
}

get_survey_weight <- function(data, weight_var = NULL) {
  if (is.null(weight_var)) {
    return(rep(1, nrow(data)))
  }
  
  if (!weight_var %in% names(data)) {
    stop("Survey weight variable not found: ", weight_var)
  }
  
  weight <- as.numeric(data[[weight_var]])
  if_else(is.finite(weight) & weight > 0, weight, NA_real_)
}

ideology_group <- function(recognition, statism, cut = IDEOLOGY_CUT) {
  has_both_axes <- !is.na(recognition) & !is.na(statism)
  inside <- function(x) x >= -cut & x <= cut
  above <- function(x) x > cut
  below <- function(x) x < -cut
  
  case_when(
    has_both_axes & inside(recognition) & inside(statism) ~ "C",
    has_both_axes & above(recognition) & above(statism) ~ "R",
    has_both_axes & below(recognition) & below(statism) ~ "L",
    TRUE ~ NA_character_
  )
}


ideology_item_bucket <- function(oriented_score) {
  oriented_score <- as.numeric(oriented_score)
  
  case_when(
    oriented_score == -2 ~ "Left",
    oriented_score %in% c(-1, 1) ~ "Center",
    oriented_score == 2 ~ "Right",
    TRUE ~ NA_character_
  )
}

strict_axis_bucket <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  apply(data, 1, function(row_values) {
    row_values <- as.character(row_values)
    
    if (any(is.na(row_values))) {
      return(NA_character_)
    }
    
    unique_values <- unique(row_values)
    
    if (length(unique_values) == 1) {
      unique_values
    } else {
      "Mixed"
    }
  })
}

combine_axis_buckets <- function(recognition_ideology, statism_ideology) {
  case_when(
    is.na(recognition_ideology) | is.na(statism_ideology) ~ NA_character_,
    recognition_ideology == statism_ideology &
      recognition_ideology %in% c("Left", "Center", "Right") ~
      recognition_ideology,
    TRUE ~ "Mixed"
  )
}

classification_status <- function(recognition_ideology, statism_ideology) {
  case_when(
    is.na(recognition_ideology) | is.na(statism_ideology) ~
      "missing_required_item",
    recognition_ideology == "Mixed" & statism_ideology == "Mixed" ~
      "mixed_both_axes",
    recognition_ideology == "Mixed" ~ "mixed_within_recognition",
    statism_ideology == "Mixed" ~ "mixed_within_statism",
    recognition_ideology != statism_ideology ~ "axis_disagreement",
    recognition_ideology == "Left" ~ "pure_left",
    recognition_ideology == "Center" ~ "pure_center",
    recognition_ideology == "Right" ~ "pure_right",
    TRUE ~ NA_character_
  )
}

safe_median <- function(x) {
  result <- median(x, na.rm = TRUE)
  
  if (is.na(result) || is.nan(result)) {
    NA_real_
  } else {
    result
  }
}

recode_vote_indicator <- function(vote_code, target_codes, invalid_codes) {
  vote_code <- as.numeric(vote_code)
  
  case_when(
    is.na(vote_code) ~ NA_real_,
    vote_code %in% invalid_codes ~ NA_real_,
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
    invalid_party_codes = c(98, 99)
) {
  gate_code <- as.numeric(gate_code)
  party_code <- as.numeric(party_code)
  
  case_when(
    gate_code == no_code ~ 0,
    gate_code == yes_code & party_code %in% target_codes ~ 1,
    gate_code == yes_code &
      !is.na(party_code) &
      !party_code %in% invalid_party_codes ~ 0,
    TRUE ~ NA_real_
  )
}

preferred_election_label <- function(x, party) {
  preferred <- x[
    !is.na(x) &
      !is.na(party) &
      party != "NOTA"
  ]
  
  if (length(preferred) > 0) {
    return(first(preferred))
  }
  
  first_nonmissing(x)
}

allocate_district_counts <- function(
    district_data,
    allocation_weights,
    value_columns,
    output_suffix = "_ac"
) {
  assert_unique_rows(
    district_data,
    c("state_no", "district_code_2011"),
    "District source passed to allocate_district_counts()"
  )
  
  allocation_weights %>%
    select(
      state_no,
      ac,
      district_code_2011,
      proxy_ac_pop,
      ac_alloc_share
    ) %>%
    left_join(
      district_data,
      by = c("state_no", "district_code_2011"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      across(
        all_of(value_columns),
        \(x) if_else(
          !is.na(x) & !is.na(ac_alloc_share),
          x * ac_alloc_share,
          NA_real_
        ),
        .names = paste0("{.col}", output_suffix)
      )
    ) %>%
    select(
      state_no,
      ac,
      proxy_ac_pop,
      all_of(paste0(value_columns, output_suffix))
    )
}

clean_group_file <- function(path, code_col, keep_code, out_col) {
  read_excel(path, col_names = FALSE, col_types = "text") %>%
    select(2:10) %>%
    set_names(c(
      "state_code",
      "district_code",
      "area_name",
      code_col,
      "group_name",
      "area_type",
      "persons",
      "males",
      "females"
    )) %>%
    mutate(across(everything(), \(x) str_squish(as.character(x)))) %>%
    filter(
      area_type == "Total",
      district_code != "00",
      .data[[code_col]] == keep_code
    ) %>%
    transmute(
      state_no = as.integer(state_code),
      district_code_2011 = as.integer(str_extract(area_name, "\\d{3}$")),
      district_name = area_name %>%
        str_remove("^District\\s*-\\s*") %>%
        str_remove("\\s+\\d{3}$") %>%
        str_squish(),
      "{out_col}" := clean_num(persons)
    )
}

clean_pop_change_file <- function(path) {
  read_excel(path, col_names = FALSE, col_types = "text") %>%
    select(1:9) %>%
    set_names(c(
      "state_code",
      "district_code",
      "district_name",
      "census_year",
      "persons",
      "abs_change_from_last_census",
      "pct_change_from_last_census",
      "males",
      "females"
    )) %>%
    mutate(across(everything(), \(x) str_squish(as.character(x)))) %>%
    filter(str_detect(census_year, "^\\d{4}\\*?$")) %>%
    fill(state_code, district_code, district_name, .direction = "down") %>%
    filter(district_code != "000") %>%
    mutate(
      census_year = as.integer(str_remove(census_year, "\\*")),
      across(
        c(
          persons,
          abs_change_from_last_census,
          pct_change_from_last_census,
          males,
          females
        ),
        clean_num
      )
    )
}

migration_source_columns <- c(
  "state_code",
  "district_code",
  "area_name",
  "last_residence",
  "last_residence_type",
  "place_of_enumeration",
  "2011_Total_Persons",3
  "2011_Total_Males",
  "2011_Total_Females",
  "2011_Total_Persons_<1",
  "2011_Total_Males_<1",
  "2011_Total_Females_<1",
  "2011_Total_Persons_1-4",
  "2011_Total_Males_1-4",
  "2011_Total_Females_1-4",
  "2011_Total_Persons_5-9",
  "2011_Total_Males_5-9",
  "2011_Total_Females_5-9",
  "2011_Total_Persons_10-19",
  "2011_Total_Males_10-19",
  "2011_Total_Females_10-19",
  "2011_Total_Persons_>20",
  "2011_Total_Males_>20",
  "2011_Total_Females_>20",
  "2011_Total_Persons_NK",
  "2011_Total_Males_NK",
  "2011_Total_Females_NK"
)

clean_migration_file <- function(path) {
  read_excel(path, col_names = FALSE, col_types = "text") %>%
    select(2:28) %>%
    set_names(migration_source_columns) %>%
    mutate(across(everything(), \(x) str_squish(as.character(x)))) %>%
    filter(
      str_detect(state_code, "^\\d+$"),
      str_detect(district_code, "^\\d{3}$"),
      district_code != "000"
    ) %>%
    mutate(across(starts_with("2011_"), clean_num))
}

nes_state_recode <- function(x) {
  recode(
    as.numeric(x),
    `1` = 28,
    `2` = 12,
    `3` = 18,
    `4` = 10,
    `5` = 30,
    `6` = 24,
    `7` = 6,
    `8` = 2,
    `9` = 1,
    `10` = 29,
    `11` = 32,
    `12` = 23,
    `13` = 27,
    `14` = 14,
    `15` = 17,
    `16` = 15,
    `17` = 13,
    `18` = 21,
    `19` = 3,
    `20` = 8,
    `21` = 11,
    `22` = 33,
    `23` = 16,
    `24` = 9,
    `25` = 19,
    `26` = 35,
    `27` = 4,
    `28` = 26,
    `29` = 25,
    `30` = 7,
    `31` = 31,
    `32` = 34,
    `33` = 20,
    `34` = 22,
    `35` = 5,
    `36` = 28,
    .default = NA_real_
  )
}

build_item_response_long <- function(
    data,
    year_value,
    item_spec,
    weight_var = NULL
) {
  respondent_id <- seq_len(nrow(data))
  survey_weight <- get_survey_weight(data, weight_var)
  survey_weight_validated <- !is.null(weight_var)
  
  map_dfr(seq_len(nrow(item_spec)), \(index) {
    item_name <- item_spec$item[index]
    raw_name <- paste0(item_name, "_raw")
    raw_value <- data[[raw_name]]
    score_value <- as.numeric(data[[item_name]])
    dk_codes <- unlist(item_spec$dk_codes[[index]])
    raw_numeric <- as.numeric(raw_value)
    
    tibble(
      year = year_value,
      respondent_id = respondent_id,
      state_no = as.numeric(data$state_no),
      pc = as.numeric(data$pc),
      ac = as.numeric(data$ac),
      voter_ideology = data$voter_ideology,
      recognition_ideology = data$recognition_ideology,
      statism_ideology = data$statism_ideology,
      item = item_name,
      axis = item_spec$axis[index],
      item_label = variable_label(raw_value, item_name),
      raw_response = raw_numeric,
      original_response_label = as.character(
        haven::as_factor(raw_value, levels = "labels")
      ),
      score = score_value,
      dont_know = raw_numeric %in% dk_codes,
      survey_weight = survey_weight,
      survey_weight_validated = survey_weight_validated
    ) %>%
      mutate(
        response_category = case_when(
          dont_know ~ "Don't know / no opinion",
          score == -2 ~ "Strongly negative-coded",
          score == -1 ~ "Negative-coded",
          score == 1 ~ "Positive-coded",
          score == 2 ~ "Strongly positive-coded",
          !is.na(raw_response) ~ "Other non-substantive",
          TRUE ~ NA_character_
        )
      )
  })
}


# 01.3 State, party, migration, and ideology configuration ----

state_codes <- c(
  "Andaman & Nicobar Islands" = 35,
  "Andhra Pradesh" = 28,
  "Arunachal Pradesh" = 12,
  "Assam" = 18,
  "Bihar" = 10,
  "Chandigarh" = 4,
  "Chhattisgarh" = 22,
  "Dadra & Nagar Haveli" = 26,
  "Daman & Diu" = 25,
  "Delhi" = 7,
  "Goa" = 30,
  "Gujarat" = 24,
  "Haryana" = 6,
  "Himachal Pradesh" = 2,
  "Jammu & Kashmir" = 1,
  "Jharkhand" = 20,
  "Karnataka" = 29,
  "Kerala" = 32,
  "Lakshadweep" = 31,
  "Madhya Pradesh" = 23,
  "Maharashtra" = 27,
  "Manipur" = 14,
  "Meghalaya" = 17,
  "Mizoram" = 15,
  "Nagaland" = 13,
  "Odisha" = 21,
  "Orissa" = 21,
  "Puducherry" = 34,
  "Punjab" = 3,
  "Rajasthan" = 8,
  "Sikkim" = 11,
  "Tamil Nadu" = 33,
  "Telangana" = 28,
  "Tripura" = 16,
  "Uttar Pradesh" = 9,
  "Uttarakhand" = 5,
  "West Bengal" = 19
)

state_name_to_no <- function(x) {
  cleaned_name <- str_replace_all(as.character(x), "_", " ")
  as.numeric(unname(state_codes[cleaned_name]))
}

state_lookup <- enframe(state_codes, name = "state", value = "state_no") %>%
  distinct(state_no, .keep_all = TRUE)

zero_st_states <- c(7, 3, 6, 34) # Delhi, Punjab, Haryana, Puducherry
zero_sc_states <- c(12, 13)      # Arunachal Pradesh, Nagaland

fr_parties <- c("BJP", "SHS", "MNS")

nes_party_key <- tribble(
  ~year, ~party_code, ~party,
  2009L, 1, "INC",
  2009L, 2, "BJP",
  2009L, 46, "SHS",
  2009L, 91, "MNS",
  2014L, 1, "INC",
  2014L, 2, "BJP",
  2014L, 46, "SHS",
  2014L, 47, "MNS"
)

nes_fr_codes <- nes_party_key %>%
  filter(party %in% fr_parties) %>%
  summarise(codes = list(party_code), .by = year)

fdi_service_activities <- c(
  "Business Services",
  "Customer Contact Centre",
  "Research & Development",
  "Sales, Marketing & Support",
  "Shared Services Centre",
  "Technical Support Centre"
)

fdi_period_spec <- tribble(
  ~year, ~period_start, ~period_end,
  2009L, as.Date("2004-04-01"), as.Date("2009-04-01"),
  2014L, as.Date("2009-05-01"), as.Date("2014-05-01")
)

migration_bin_spec <- tribble(
  ~bin, ~source_col, ~first_year, ~last_year, ~bin_years,
  "10_19", "mig_10_19_ac", 1992L, 2001L, 10,
  "5_9", "mig_5_9_ac", 2002L, 2006L, 5,
  "1_4", "mig_1_4_ac", 2007L, 2010L, 4,
  "lt1", "mig_lt1_ac", 2011L, 2011L, 1
)

election_migration_windows <- tribble(
  ~year, ~period, ~window_start, ~window_end,
  2009L, "recent", 2004L, 2008L,
  2009L, "prior", 1999L, 2003L,
  2009L, "prior_prior", 1994L, 1998L,
  2014L, "recent", 2009L, 2013L,
  2014L, "prior", 2004L, 2008L,
  2014L, "prior_prior", 1999L, 2003L
)

# Four-response items used to classify ideology.
ideology_item_spec_2009 <- tribble(
  ~item, ~axis, ~dk_codes,
  "a4b", "Recognition", list(8),
  "a4c", "Recognition", list(8),
  "a4d", "Statism", list(8),
  "a4g", "Statism", list(8),
  "q26a", "Statism", list(8)
)

ideology_item_spec_2014 <- tribble(
  ~item, ~axis, ~dk_codes,
  "q10b", "Recognition", list(8),
  "q10e", "Recognition", list(8),
  "q23c", "Statism", list(8)
)

# Binary items are excluded from classification and used as external diagnostics.
binary_item_spec_2009 <- tribble(
  ~item, ~axis, ~dk_codes,
  "a5", "Recognition", list(8),
  "a6a", "Recognition", list(8),
  "a6b", "Statism", list(8),
  "q27", "Statism", list(c(7, 8))
)

# Combined item lists retained for raw-response diagnostics.
item_spec_2009 <- bind_rows(
  ideology_item_spec_2009,
  binary_item_spec_2009
)

item_spec_2014 <- ideology_item_spec_2014

ideology_reference <- tribble(
  ~ideology, ~ideology_label, ~ideology_slug,
  "Left", "Left", "left",
  "Center", "Center", "center",
  "Right", "Right", "right",
  "Mixed", "Mixed", "mixed"
)


# 01.4 Manual AC-district mappings ----

# Manual Delhi mappings are fallbacks when the spatial overlap assignment is
# unavailable or does not match a Census district name.
delhi_ac_manual_key <- tribble(
  ~state_no, ~ac, ~district_name_manual,
  7, 63, "North East",
  7, 64, "North East",
  7, 67, "North East",
  7, 68, "North East",
  7, 41, "South",
  7, 54, "South",
  7, 61, "East",
  7, 62, "East",
  7, 42, "South",
  7, 50, "South",
  7, 49, "South",
  7, 51, "South",
  7, 52, "South",
  7, 53, "South"
)

manual_ac_district_key <- tribble(
  ~state_no, ~ac, ~district_manual,
  # Madhya Pradesh: Indore urban ACs
  23, 205, "Indore",
  23, 206, "Indore",
  23, 207, "Indore",
  23, 208, "Indore",
  # Gujarat: Ahmedabad urban ACs
  24, 45, "Ahmadabad",
  24, 46, "Ahmadabad",
  24, 47, "Ahmadabad",
  24, 48, "Ahmadabad",
  24, 49, "Ahmadabad",
  24, 50, "Ahmadabad",
  24, 51, "Ahmadabad",
  24, 52, "Ahmadabad",
  24, 53, "Ahmadabad",
  24, 54, "Ahmadabad",
  24, 55, "Ahmadabad",
  24, 56, "Ahmadabad",
  # Gujarat: Surat urban ACs
  24, 161, "Surat",
  24, 162, "Surat",
  24, 163, "Surat",
  24, 164, "Surat",
  24, 165, "Surat",
  24, 166, "Surat",
  24, 167, "Surat",
  # Ambiguous ACs resolved manually
  18, 33, "Bongaigaon",
  18, 42, "Barpeta",
  20, 57, "Saraikela Kharsawan",
  20, 59, "Ranchi",
  20, 70, "Simdega"
) %>%
  mutate(district_join_key_name_manual = district_name_key(district_manual))


# 02. Build geographic reference tables ----

message("02. Building geographic reference tables")

walk(
  c(
    PATHS$ac_name_key,
    PATHS$ac_district_xwalk,
    PATHS$delhi_districts,
    PATHS$district_codes
  ),
  assert_file_exists
)
assert_directory_exists(PATHS$ac_map_dir)

# 02.1 Census district reference ----

district_codes_raw <- read_xlsx(PATHS$district_codes, col_types = "text")

assert_has_columns(
  district_codes_raw,
  c("State Code", "District Code", "Sub District Code", "Town-Village Name"),
  "District code file"
)

district_reference <- district_codes_raw %>%
  mutate(
    state_no = as.integer(`State Code`),
    district_code_2011 = as.integer(`District Code`),
    subdistrict_code = str_pad(`Sub District Code`, width = 5, pad = "0")
  ) %>%
  filter(
    district_code_2011 != 0,
    subdistrict_code == "00000"
  ) %>%
  transmute(
    state_no,
    district_code_2011,
    district_name_2011 = str_squish(`Town-Village Name`),
    district_name_norm = norm_name(`Town-Village Name`),
    district_join_key_name = district_name_key(`Town-Village Name`)
  ) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

assert_unique_rows(
  district_reference,
  c("state_no", "district_code_2011"),
  "District reference"
)

# A normalized district name must identify no more than one district per state.
district_name_key_duplicates <- district_reference %>%
  count(state_no, district_join_key_name, name = "n") %>%
  filter(n > 1)

if (nrow(district_name_key_duplicates) > 0) {
  print(district_name_key_duplicates, n = Inf)
  stop("Normalized district names are not unique within state.")
}

# 02.2 AC maps and name keys ----

ac_map_raw <- read_many_sf(
  PATHS$ac_map_dir,
  pattern = "\\.assembly\\.shp$"
)

assert_has_columns(ac_map_raw, c("state", "pc", "ac"), "2014 AC map")

ac_map_clean <- ac_map_raw %>%
  mutate(
    state_no = state_name_to_no(state),
    pc = as.integer(pc),
    ac = as.integer(ac)
  ) %>%
  filter(!is.na(state_no), !is.na(ac), ac > 0) %>%
  st_make_valid()

assert_unique_rows(
  ac_map_clean %>% st_drop_geometry(),
  c("state_no", "ac"),
  "2014 AC map"
)

ac_name_key_raw <- read_csv(PATHS$ac_name_key, show_col_types = FALSE)

assert_has_columns(
  ac_name_key_raw,
  c(
    "ac08_id",
    "ac08_name",
    "pc01_district_id",
    "pc01_district_name"
  ),
  "AC name key"
)

ac_name_key_clean <- ac_name_key_raw %>%
  transmute(
    state_no = as.integer(str_match(ac08_id, "^\\d{4}-(\\d{2})-\\d{3}$")[, 2]),
    ac = as.integer(str_extract(ac08_id, "\\d{3}$")),
    ac_name_key = ac08_name,
    district_code_key = as.integer(pc01_district_id),
    district_name_key_value = pc01_district_name,
    district_join_key_name_key = district_name_key(pc01_district_name)
  ) %>%
  distinct(state_no, ac, .keep_all = TRUE)

assert_unique_rows(ac_name_key_clean, c("state_no", "ac"), "AC name key")

# 02.3 Delhi district assignment ----

delhi_districts_raw <- read_sf(PATHS$delhi_districts, quiet = TRUE) %>%
  clean_names() %>%
  st_make_valid()

delhi_dist_name_col <- names(delhi_districts_raw) %>%
  keep(\(x) str_detect(x, regex("district|dist|name", ignore_case = TRUE))) %>%
  first()

if (is.na(delhi_dist_name_col)) {
  stop("Could not identify a district-name column in the Delhi district file.")
}

delhi_district_codes <- district_reference %>%
  filter(state_no == 7) %>%
  transmute(
    delhi_district_name_norm = norm_delhi_dist(district_name_2011),
    district_code_2011_fixed = district_code_2011,
    district_name_2011_fixed = district_name_2011
  )

delhi_districts_clean <- delhi_districts_raw %>%
  mutate(
    delhi_district_name_norm = norm_delhi_dist(.data[[delhi_dist_name_col]])
  ) %>%
  left_join(delhi_district_codes, by = "delhi_district_name_norm")

delhi_ac_spatial <- ac_map_clean %>%
  filter(state_no == 7) %>%
  select(state_no, ac, geometry) %>%
  st_transform(6933) %>%
  st_intersection(
    delhi_districts_clean %>%
      select(
        district_code_2011_fixed,
        district_name_2011_fixed,
        geometry
      ) %>%
      st_transform(6933)
  ) %>%
  mutate(overlap_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  slice_max(
    overlap_area,
    by = c(state_no, ac),
    n = 1,
    with_ties = FALSE
  ) %>%
  select(
    state_no,
    ac,
    district_code_2011_fixed,
    district_name_2011_fixed
  )

delhi_ac_manual <- delhi_ac_manual_key %>%
  mutate(
    district_name_norm_manual = norm_delhi_dist(district_name_manual)
  ) %>%
  left_join(
    delhi_district_codes %>%
      rename(
        district_name_norm_manual = delhi_district_name_norm,
        district_code_2011_manual = district_code_2011_fixed,
        district_name_2011_manual = district_name_2011_fixed
      ),
    by = "district_name_norm_manual"
  )

# 02.4 Resolve the national AC-district crosswalk ----

ac_district_xwalk_raw <- read_sf(PATHS$ac_district_xwalk)

assert_has_columns(
  ac_district_xwalk_raw,
  c("ST_CODE", "PC_NO", "AC_NO", "DIST_NAME", "DT_CODE"),
  "AC-district crosswalk"
)

ac_dist_raw <- ac_district_xwalk_raw %>%
  st_drop_geometry() %>%
  transmute(
    state_no = as.integer(ST_CODE),
    xwalk_pc = as.integer(PC_NO),
    ac = as.integer(AC_NO),
    district = as.character(DIST_NAME),
    district_code_xwalk = as.integer(DT_CODE),
    district_join_key_name = district_name_key(DIST_NAME),
    manual_xwalk = FALSE
  ) %>%
  filter(!is.na(ac), ac > 0) %>%
  left_join(
    delhi_ac_spatial,
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    delhi_ac_manual %>%
      select(
        state_no,
        ac,
        district_code_2011_manual,
        district_name_2011_manual
      ),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    manual_ac_district_key %>%
      select(
        state_no,
        ac,
        district_manual,
        district_join_key_name_manual
      ),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    district = case_when(
      !is.na(district_manual) ~ district_manual,
      state_no == 7 & !is.na(district_name_2011_fixed) ~
        district_name_2011_fixed,
      state_no == 7 & !is.na(district_name_2011_manual) ~
        district_name_2011_manual,
      TRUE ~ district
    ),
    district_code_xwalk = case_when(
      state_no == 7 & !is.na(district_code_2011_fixed) ~
        district_code_2011_fixed,
      state_no == 7 & !is.na(district_code_2011_manual) ~
        district_code_2011_manual,
      TRUE ~ district_code_xwalk
    ),
    district_join_key_name = case_when(
      !is.na(district_join_key_name_manual) ~
        district_join_key_name_manual,
      TRUE ~ district_name_key(district)
    ),
    manual_xwalk = manual_xwalk |
      !is.na(district_manual) |
      (
        state_no == 7 &
          is.na(district_name_2011_fixed) &
          !is.na(district_name_2011_manual)
      )
  ) %>%
  select(
    state_no,
    xwalk_pc,
    ac,
    district,
    district_code_xwalk,
    district_join_key_name,
    manual_xwalk
  )

ambiguous_acs <- ac_dist_raw %>%
  summarise(
    n_districts = n_distinct(district_join_key_name),
    .by = c(state_no, ac)
  ) %>%
  filter(n_districts > 1) %>%
  select(state_no, ac)

ac_dist_resolved <- ac_dist_raw %>%
  left_join(
    ambiguous_acs %>% mutate(ambiguous_ac = TRUE),
    by = c("state_no", "ac")
  ) %>%
  left_join(
    ac_name_key_clean %>%
      select(
        state_no,
        ac,
        district_code_key,
        district_name_key_value,
        district_join_key_name_key
      ),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    ambiguous_ac = replace_na(ambiguous_ac, FALSE),
    matches_name_key =
      district_join_key_name == district_join_key_name_key
  ) %>%
  group_by(state_no, ac) %>%
  mutate(
    use_name_key = ambiguous_ac & any(matches_name_key, na.rm = TRUE)
  ) %>%
  filter(!use_name_key | matches_name_key) %>%
  ungroup() %>%
  mutate(
    district = if_else(
      use_name_key,
      district_name_key_value,
      district
    ),
    district_code_xwalk = if_else(
      use_name_key,
      district_code_key,
      district_code_xwalk
    ),
    district_join_key_name = if_else(
      use_name_key,
      district_join_key_name_key,
      district_join_key_name
    )
  ) %>%
  select(
    state_no,
    xwalk_pc,
    ac,
    district,
    district_code_xwalk,
    district_join_key_name,
    manual_xwalk
  ) %>%
  distinct(state_no, ac, district_join_key_name, .keep_all = TRUE)

multi_district_acs <- ac_dist_resolved %>%
  summarise(
    n_districts = n_distinct(district_join_key_name),
    districts = paste(sort(unique(district)), collapse = "; "),
    .by = c(state_no, ac)
  ) %>%
  filter(n_districts > 1)

if (nrow(multi_district_acs) > 0) {
  print(multi_district_acs, n = Inf)
  stop("Some ACs still map to multiple districts.")
}

ac_dist_base <- ac_dist_resolved %>%
  distinct(state_no, ac, .keep_all = TRUE)

manual_xwalk_rows <- manual_ac_district_key %>%
  anti_join(ac_dist_base, by = c("state_no", "ac")) %>%
  transmute(
    state_no,
    xwalk_pc = NA_integer_,
    ac = as.integer(ac),
    district = district_manual,
    district_code_xwalk = NA_integer_,
    district_join_key_name = district_join_key_name_manual,
    manual_xwalk = TRUE
  )

ac_dist_prejoin <- bind_rows(ac_dist_base, manual_xwalk_rows)

assert_unique_rows(
  ac_dist_prejoin,
  c("state_no", "ac"),
  "Resolved AC-district crosswalk"
)

ac_reference <- ac_dist_prejoin %>%
  left_join(
    district_reference,
    by = c("state_no", "district_join_key_name"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ac_map_clean %>%
      st_drop_geometry() %>%
      select(state_no, ac, map_pc = pc),
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  ) %>%
  left_join(state_lookup, by = "state_no", relationship = "many-to-one") %>%
  transmute(
    state_no,
    state,
    ac,
    pc = coalesce(map_pc, xwalk_pc),
    district_code_2011,
    district_name_2011,
    district_join_key_name,
    manual_xwalk,
    district_join_success = !is.na(district_code_2011)
  ) %>%
  distinct(state_no, ac, .keep_all = TRUE)

assert_unique_rows(ac_reference, c("state_no", "ac"), "AC reference")

ac_reference_sf <- ac_map_clean %>%
  select(state_no, ac, geometry) %>%
  left_join(
    ac_reference,
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  )

write_csv(ac_reference, file.path(INTERMEDIATE_DIR, "ac_reference.csv"))
saveRDS(ac_reference_sf, file.path(INTERMEDIATE_DIR, "ac_reference_sf.rds"))


# 03. Clean election and candidate data ----

message("03. Cleaning election and candidate data")

assert_file_exists(PATHS$pc_elections)

pc_elections_raw <- read_csv(PATHS$pc_elections, show_col_types = FALSE)

assert_has_columns(
  pc_elections_raw,
  c(
    "State_Name",
    "Constituency_No",
    "Year",
    "PC_Name",
    "PC_No",
    "Constituency_Name",
    "Party",
    "Votes",
    "Valid_Votes"
  ),
  "Parliamentary election data"
)

elections_candidate_clean <- pc_elections_raw %>%
  filter(Year %in% c(2009, 2014)) %>%
  transmute(
    state = str_replace_all(as.character(State_Name), "_", " "),
    state_no = state_name_to_no(State_Name),
    pc = as.integer(PC_No),
    pc_name = as.character(PC_Name),
    ac = as.integer(Constituency_No),
    ac_name = as.character(Constituency_Name),
    year = as.integer(Year),
    candidate = as.character(Candidate),
    party = str_to_upper(str_squish(as.character(Party))),
    votes = as.numeric(Votes),
    valid_votes = as.numeric(Valid_Votes),
    is_bjp_candidate = party == "BJP",
    is_shs_candidate = party == "SHS",
    is_mns_candidate = party == "MNS",
    is_fr_candidate = party %in% fr_parties
  )

election_identifier_diagnostics <- elections_candidate_clean %>%
  summarise(
    n_pc_numbers = n_distinct(pc, na.rm = TRUE),
    n_valid_vote_totals = n_distinct(valid_votes, na.rm = TRUE),
    n_pc_names = n_distinct(pc_name, na.rm = TRUE),
    n_ac_names = n_distinct(ac_name, na.rm = TRUE),
    .by = c(state_no, ac, year)
  )

true_election_conflicts <- election_identifier_diagnostics %>%
  filter(n_pc_numbers > 1 | n_valid_vote_totals > 1)

if (nrow(true_election_conflicts) > 0) {
  print(true_election_conflicts, n = Inf)
  stop("Some AC-year election observations have conflicting identifiers.")
}

elections_ac_year <- elections_candidate_clean %>%
  summarise(
    state = first_nonmissing(state),
    pc = first_nonmissing(pc),
    pc_name = preferred_election_label(pc_name, party),
    ac_name = preferred_election_label(ac_name, party),
    valid_votes = first_nonmissing(valid_votes),
    bjp_votes = sum(if_else(party == "BJP", votes, 0), na.rm = TRUE),
    fr_party_votes = sum(if_else(is_fr_candidate, votes, 0), na.rm = TRUE),
    bjp_candidate_present = any(is_bjp_candidate, na.rm = TRUE),
    shs_candidate_present = any(is_shs_candidate, na.rm = TRUE),
    mns_candidate_present = any(is_mns_candidate, na.rm = TRUE),
    fr_candidate_present = any(is_fr_candidate, na.rm = TRUE),
    fr_candidate_n = n_distinct(candidate[is_fr_candidate & !is.na(candidate)]),
    .by = c(state_no, ac, year)
  ) %>%
  mutate(
    fr_party_vote_share = if_else(
      !is.na(valid_votes) & valid_votes > 0,
      100 * fr_party_votes / valid_votes,
      NA_real_
    ),
    bjp_vote_share = if_else(
      !is.na(valid_votes) & valid_votes > 0,
      100 * bjp_votes / valid_votes,
      NA_real_
    )
  ) %>%
  select(
    state,
    state_no,
    pc_name,
    pc,
    ac_name,
    ac,
    year,
    valid_votes,
    bjp_votes,
    fr_party_votes,
    bjp_vote_share,
    fr_party_vote_share,
    bjp_candidate_present,
    shs_candidate_present,
    mns_candidate_present,
    fr_candidate_present,
    fr_candidate_n
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  elections_ac_year,
  c("state_no", "ac", "year"),
  "AC-year election data"
)

write_csv(
  elections_candidate_clean,
  file.path(INTERMEDIATE_DIR, "elections_candidate_clean.csv")
)
write_csv(
  elections_ac_year,
  file.path(INTERMEDIATE_DIR, "elections_ac_year.csv")
)


# 04. Build population estimates and district-to-AC allocation weights ----

message("04. Building population and allocation weights")

assert_file_exists(PATHS$ac_population)
assert_directory_exists(PATHS$population_change_dir)

# 04.1 Direct AC population and land area ----

ac_population_raw <- read_csv(PATHS$ac_population, show_col_types = FALSE)

assert_has_columns(
  ac_population_raw,
  c("ac08_id", "con08_pc01_pca_tot_p", "con08_land_area"),
  "AC population file"
)

ac_population_clean <- ac_population_raw %>%
  transmute(
    state_no = as.integer(str_match(ac08_id, "^\\d{4}-(\\d{2})-\\d{3}$")[, 2]),
    ac = as.integer(str_extract(ac08_id, "\\d{3}$")),
    con08_pop = clean_num(con08_pc01_pca_tot_p),
    con08_land_area = clean_num(con08_land_area)
  ) %>%
  distinct(state_no, ac, .keep_all = TRUE)

assert_unique_rows(
  ac_population_clean,
  c("state_no", "ac"),
  "Direct AC population data"
)

# 04.2 District population in 2011 ----

population_change_district <- read_many(
  PATHS$population_change_dir,
  pattern = "\\.xlsx?$",
  reader = clean_pop_change_file
) %>%
  filter(census_year == 2011) %>%
  transmute(
    state_no = as.integer(state_code),
    district_code_2011 = as.integer(district_code),
    district_name_source = district_name,
    district_pop_2011 = persons,
    pct_change_from_last_census
  ) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

assert_unique_rows(
  population_change_district,
  c("state_no", "district_code_2011"),
  "District population data"
)

# 04.3 Construct AC population proxies without election-based weights ----

ac_allocation_weights <- ac_reference_sf %>%
  left_join(
    ac_population_clean,
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    population_change_district,
    by = c("state_no", "district_code_2011"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    geom_land_area = as.numeric(st_area(st_transform(geometry, 6933))) / 1e6,
    con08_land_area = coalesce(con08_land_area, geom_land_area),
    allocation_district_key = coalesce(
      district_join_key_name,
      paste0("UNMATCHED_AC_", state_no, "_", ac)
    )
  ) %>%
  group_by(state_no, allocation_district_key) %>%
  mutate(
    district_pop_2011 = first_nonmissing(district_pop_2011),
    n_ac_in_district = n(),
    direct_population_available = !is.na(con08_pop) & con08_pop > 0,
    n_direct_population = sum(direct_population_available),
    n_missing_direct_population = sum(!direct_population_available),
    direct_population_sum = sum(
      con08_pop[direct_population_available],
      na.rm = TRUE
    ),
    direct_population_inconsistent =
      !is.na(district_pop_2011) &
      direct_population_sum >
      POPULATION_INCONSISTENCY_TOLERANCE * district_pop_2011,
    remaining_district_population = if_else(
      !is.na(district_pop_2011),
      pmax(district_pop_2011 - direct_population_sum, 0),
      NA_real_
    ),
    proxy_ac_pop = case_when(
      direct_population_inconsistent & !is.na(district_pop_2011) ~
        district_pop_2011 / n_ac_in_district,
      direct_population_available ~ con08_pop,
      !is.na(district_pop_2011) & n_direct_population == 0 ~
        district_pop_2011 / n_ac_in_district,
      !is.na(district_pop_2011) &
        n_direct_population > 0 &
        n_missing_direct_population > 0 ~
        remaining_district_population / n_missing_direct_population,
      TRUE ~ NA_real_
    ),
    proxy_ac_pop_source = case_when(
      direct_population_inconsistent & !is.na(district_pop_2011) ~
        "full_district_equal_allocation_inconsistent_direct_population",
      direct_population_available ~ "direct_ac_population",
      !is.na(district_pop_2011) & n_direct_population == 0 ~
        "full_district_equal_allocation_no_direct_population",
      !is.na(district_pop_2011) &
        n_direct_population > 0 &
        n_missing_direct_population > 0 ~
        "remaining_district_population_equal_allocation",
      TRUE ~ "missing"
    ),
    all_proxy_populations_available = all(!is.na(proxy_ac_pop)),
    proxy_population_sum = sum(proxy_ac_pop, na.rm = TRUE),
    ac_alloc_share = case_when(
      all_proxy_populations_available & proxy_population_sum > 0 ~
        proxy_ac_pop / proxy_population_sum,
      TRUE ~ NA_real_
    ),
    allocation_warning = case_when(
      is.na(district_code_2011) ~ "missing_district_crosswalk",
      is.na(district_pop_2011) ~ "missing_district_population",
      direct_population_inconsistent ~
        "known_ac_population_exceeds_district_population",
      is.na(ac_alloc_share) ~ "allocation_share_unavailable",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(
    state_no,
    state,
    pc,
    ac,
    district_code_2011,
    district_name_2011,
    con08_pop,
    district_pop_2011,
    proxy_ac_pop,
    proxy_ac_pop_source,
    con08_land_area,
    ac_alloc_share,
    n_ac_in_district,
    direct_population_sum,
    direct_population_inconsistent,
    allocation_warning
  )

assert_unique_rows(
  ac_allocation_weights,
  c("state_no", "ac"),
  "AC allocation weights"
)

allocation_share_diagnostics <- ac_allocation_weights %>%
  filter(!is.na(district_code_2011)) %>%
  summarise(
    n_ac = n(),
    share_sum = sum(ac_alloc_share, na.rm = TRUE),
    n_missing_share = sum(is.na(ac_alloc_share)),
    .by = c(state_no, district_code_2011)
  ) %>%
  mutate(
    share_sum_valid =
      n_missing_share == 0 & abs(share_sum - 1) < 1e-8
  )

invalid_allocation_shares <- allocation_share_diagnostics %>%
  filter(!share_sum_valid)

if (nrow(invalid_allocation_shares) > 0) {
  warning(
    nrow(invalid_allocation_shares),
    " districts do not have complete allocation shares. See diagnostics."
  )
}

write_csv(
  ac_allocation_weights,
  file.path(INTERMEDIATE_DIR, "ac_allocation_weights.csv")
)
write_csv(
  allocation_share_diagnostics,
  file.path(DIAGNOSTIC_DIR, "allocation_share_diagnostics.csv")
)


# 05. Clean and allocate demographic controls ----

message("05. Cleaning and allocating demographic controls")

assert_file_exists(PATHS$economic_census)
assert_directory_exists(PATHS$sc_population_dir)
assert_directory_exists(PATHS$st_population_dir)

# 05.1 Economic Census employment ----

economic_census_raw <- read_csv(PATHS$economic_census, show_col_types = FALSE)

assert_has_columns(
  economic_census_raw,
  c(
    "pc11_state_id",
    "pc11_district_id",
    "ec13_emp_all",
    "ec13_emp_manuf",
    "ec13_emp_services"
  ),
  "Economic Census data"
)

employment_district_clean <- economic_census_raw %>%
  transmute(
    state_no = as.integer(pc11_state_id),
    district_code_2011 = as.integer(pc11_district_id),
    employment_total_district = clean_num(ec13_emp_all),
    employment_manufacturing_district = clean_num(ec13_emp_manuf),
    employment_services_district = clean_num(ec13_emp_services)
  ) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

employment_ac <- allocate_district_counts(
  district_data = employment_district_clean,
  allocation_weights = ac_allocation_weights,
  value_columns = c(
    "employment_total_district",
    "employment_manufacturing_district",
    "employment_services_district"
  )
) %>%
  rename(
    employment_total_ac = employment_total_district_ac,
    employment_manufacturing_ac = employment_manufacturing_district_ac,
    employment_services_ac = employment_services_district_ac
  )

# 05.2 Scheduled Caste population ----

sc_population_district_clean <- read_many(
  PATHS$sc_population_dir,
  pattern = "-PCA-A10-APPENDIX\\.xlsx$",
  reader = \(file) clean_group_file(
    file,
    code_col = "sc_code",
    keep_code = "000",
    out_col = "sc_population_district"
  )
) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

sc_population_ac <- allocate_district_counts(
  district_data = sc_population_district_clean %>%
    select(state_no, district_code_2011, sc_population_district),
  allocation_weights = ac_allocation_weights,
  value_columns = "sc_population_district"
) %>%
  rename(sc_population_ac = sc_population_district_ac)

# 05.3 Scheduled Tribe population ----

st_population_district_clean <- read_many(
  PATHS$st_population_dir,
  pattern = "-PCA-A11-APPENDIX\\.xlsx$",
  reader = \(file) clean_group_file(
    file,
    code_col = "st_code",
    keep_code = "500",
    out_col = "st_population_district"
  )
) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

st_population_ac <- allocate_district_counts(
  district_data = st_population_district_clean %>%
    select(state_no, district_code_2011, st_population_district),
  allocation_weights = ac_allocation_weights,
  value_columns = "st_population_district"
) %>%
  rename(st_population_ac = st_population_district_ac)

# 05.4 Final AC demographic controls ----

demographics_ac <- ac_allocation_weights %>%
  select(
    state_no,
    ac,
    proxy_ac_pop,
    proxy_ac_pop_source,
    con08_land_area
  ) %>%
  left_join(
    employment_ac %>% select(-proxy_ac_pop),
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    sc_population_ac %>% select(-proxy_ac_pop),
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    st_population_ac %>% select(-proxy_ac_pop),
    by = c("state_no", "ac"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    sc_population_ac = if_else(
      state_no %in% zero_sc_states &
        is.na(sc_population_ac) &
        !is.na(proxy_ac_pop),
      0,
      sc_population_ac
    ),
    st_population_ac = if_else(
      state_no %in% zero_st_states &
        is.na(st_population_ac) &
        !is.na(proxy_ac_pop),
      0,
      st_population_ac
    ),
    emp_rate = if_else(
      !is.na(proxy_ac_pop) & proxy_ac_pop > 0,
      employment_total_ac / proxy_ac_pop,
      NA_real_
    ),
    sc_pop_share = if_else(
      !is.na(proxy_ac_pop) & proxy_ac_pop > 0,
      sc_population_ac / proxy_ac_pop,
      NA_real_
    ),
    st_pop_share = if_else(
      !is.na(proxy_ac_pop) & proxy_ac_pop > 0,
      st_population_ac / proxy_ac_pop,
      NA_real_
    )
  )

assert_unique_rows(demographics_ac, c("state_no", "ac"), "AC demographics")

write_csv(
  employment_district_clean,
  file.path(INTERMEDIATE_DIR, "employment_district_clean.csv")
)
write_csv(
  sc_population_district_clean,
  file.path(INTERMEDIATE_DIR, "sc_population_district_clean.csv")
)
write_csv(
  st_population_district_clean,
  file.path(INTERMEDIATE_DIR, "st_population_district_clean.csv")
)
write_csv(demographics_ac, file.path(INTERMEDIATE_DIR, "demographics_ac.csv"))


# 06. Clean, allocate, and annualize migration ----

message("06. Cleaning, allocating, and annualizing migration")

assert_directory_exists(PATHS$migration_dir)

# 06.1 Clean district duration bins ----

migration_district_clean <- read_many(
  PATHS$migration_dir,
  pattern = "\\.xlsx?$",
  reader = clean_migration_file
) %>%
  filter(
    last_residence == "States in India beyond the state of enumeration",
    last_residence_type == "Total",
    place_of_enumeration == "Total"
  ) %>%
  transmute(
    state_no = as.integer(state_code),
    district_code_2011 = as.integer(district_code),
    district_name_source = as.character(area_name),
    mig_total_district = `2011_Total_Persons`,
    mig_lt1_district = `2011_Total_Persons_<1`,
    mig_1_4_district = `2011_Total_Persons_1-4`,
    mig_5_9_district = `2011_Total_Persons_5-9`,
    mig_10_19_district = `2011_Total_Persons_10-19`,
    mig_gt20_district = `2011_Total_Persons_>20`
  ) %>%
  distinct(state_no, district_code_2011, .keep_all = TRUE)

assert_unique_rows(
  migration_district_clean,
  c("state_no", "district_code_2011"),
  "District migration data"
)

# 06.2 Allocate district migration counts to ACs ----

migration_ac_bins <- allocate_district_counts(
  district_data = migration_district_clean %>%
    select(-district_name_source),
  allocation_weights = ac_allocation_weights,
  value_columns = c(
    "mig_total_district",
    "mig_lt1_district",
    "mig_1_4_district",
    "mig_5_9_district",
    "mig_10_19_district",
    "mig_gt20_district"
  )
) %>%
  rename(
    mig_total_ac = mig_total_district_ac,
    mig_lt1_ac = mig_lt1_district_ac,
    mig_1_4_ac = mig_1_4_district_ac,
    mig_5_9_ac = mig_5_9_district_ac,
    mig_10_19_ac = mig_10_19_district_ac,
    mig_gt20_ac = mig_gt20_district_ac
  )

assert_unique_rows(
  migration_ac_bins,
  c("state_no", "ac"),
  "AC migration bins"
)

# 06.3 Reconstruct annual migration from t = -19 through t = 0 ----

migration_annual_observed <- migration_ac_bins %>%
  select(
    state_no,
    ac,
    proxy_ac_pop,
    all_of(migration_bin_spec$source_col)
  ) %>%
  pivot_longer(
    cols = all_of(migration_bin_spec$source_col),
    names_to = "source_col",
    values_to = "bin_total"
  ) %>%
  left_join(
    migration_bin_spec,
    by = "source_col",
    relationship = "many-to-one"
  ) %>%
  mutate(
    annual_migrants_observed = as.numeric(bin_total) / bin_years,
    migration_year = map2(first_year, last_year, seq)
  ) %>%
  unnest(migration_year) %>%
  transmute(
    state_no,
    ac,
    proxy_ac_pop,
    migration_year = as.integer(migration_year),
    t = migration_year - 2011L,
    migration_bin = bin,
    annual_migrants_observed
  )

# Create a complete AC-year panel through 2014. Values after 2011 are imputed
# using each AC's mean annual migration in 2009, 2010, and 2011 (t = -2:0).
migration_annual_ac <- migration_ac_bins %>%
  distinct(state_no, ac, proxy_ac_pop) %>%
  crossing(migration_year = 1992L:2014L) %>%
  mutate(t = migration_year - 2011L) %>%
  left_join(
    migration_annual_observed %>%
      select(
        state_no,
        ac,
        migration_year,
        migration_bin,
        annual_migrants_observed
      ),
    by = c("state_no", "ac", "migration_year"),
    relationship = "one-to-one"
  ) %>%
  group_by(state_no, ac) %>%
  mutate(
    post_2011_imputation_mean = safe_mean(
      annual_migrants_observed[t %in% -2:0]
    ),
    annual_migrants = case_when(
      migration_year <= 2011L ~ annual_migrants_observed,
      migration_year > 2011L & !is.na(post_2011_imputation_mean) ~
        post_2011_imputation_mean,
      TRUE ~ NA_real_
    ),
    migration_imputed =
      migration_year > 2011L &
      is.na(annual_migrants_observed) &
      !is.na(annual_migrants),
    migration_value_source = case_when(
      !is.na(annual_migrants_observed) ~ "census_duration_bin",
      migration_imputed ~ "imputed_ac_mean_t_minus2_to_0",
      TRUE ~ "missing"
    )
  ) %>%
  ungroup() %>%
  select(
    state_no,
    ac,
    proxy_ac_pop,
    migration_year,
    t,
    migration_bin,
    annual_migrants,
    migration_imputed,
    migration_value_source,
    post_2011_imputation_mean
  )

assert_unique_rows(
  migration_annual_ac,
  c("state_no", "ac", "migration_year"),
  "Annual AC migration data"
)

# 06.4 Construct election-relative migration windows and measures ----

migration_period_totals <- migration_ac_bins %>%
  distinct(state_no, ac, proxy_ac_pop) %>%
  crossing(election_migration_windows) %>%
  mutate(migration_year = map2(window_start, window_end, seq)) %>%
  unnest(migration_year) %>%
  left_join(
    migration_annual_ac %>%
      select(state_no, ac, migration_year, annual_migrants),
    by = c("state_no", "ac", "migration_year"),
    relationship = "many-to-one"
  ) %>%
  summarise(
    migration_total = if_else(
      n() == 5 & all(!is.na(annual_migrants)),
      sum(annual_migrants),
      NA_real_
    ),
    .by = c(state_no, ac, proxy_ac_pop, year, period)
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = migration_total,
    names_glue = "mig_{period}_5yr_total"
  )

migration_ac_year_base <- migration_period_totals %>%
  left_join(
    ac_allocation_weights %>%
      select(state_no, ac, con08_land_area),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    # Election-relative periods:
    #   recent:       y - 5 through y - 1
    #   prior:        y - 10 through y - 6
    #   prior_prior:  y - 15 through y - 11
    mig_prior_5_15yr_total = if_else(
      !is.na(mig_prior_5yr_total) &
        !is.na(mig_prior_prior_5yr_total),
      mig_prior_5yr_total + mig_prior_prior_5yr_total,
      NA_real_
    ),
    mig_baseline_5yr_total = mig_prior_prior_5yr_total,
    
    # Logged count levels.
    log1p_mig_recent_5yr_total = log1p(mig_recent_5yr_total),
    log1p_mig_prior_5yr_total = log1p(mig_prior_5yr_total),
    log1p_mig_prior_5_15yr_total = log1p(mig_prior_5_15yr_total),
    log1p_mig_baseline_5yr_total = log1p(mig_baseline_5yr_total),
    
    # Arrival-cohort counts as shares of estimated AC population.
    mig_recent_5yr_share_ac_pop = safe_divide(
      mig_recent_5yr_total,
      proxy_ac_pop
    ),
    mig_prior_5yr_share_ac_pop = safe_divide(
      mig_prior_5yr_total,
      proxy_ac_pop
    ),
    mig_prior_5_15yr_share_ac_pop = safe_divide(
      mig_prior_5_15yr_total,
      proxy_ac_pop
    ),
    mig_baseline_5yr_share_ac_pop = safe_divide(
      mig_baseline_5yr_total,
      proxy_ac_pop
    ),
    
    mig_recent_5yr_share_ac_pop_pct =
      100 * mig_recent_5yr_share_ac_pop,
    mig_prior_5yr_share_ac_pop_pct =
      100 * mig_prior_5yr_share_ac_pop,
    mig_prior_5_15yr_share_ac_pop_pct =
      100 * mig_prior_5_15yr_share_ac_pop,
    mig_baseline_5yr_share_ac_pop_pct =
      100 * mig_baseline_5yr_share_ac_pop,
    
    log_mig_recent_5yr_share_ac_pop = safe_log(
      mig_recent_5yr_share_ac_pop
    ),
    log_mig_prior_5yr_share_ac_pop = safe_log(
      mig_prior_5yr_share_ac_pop
    ),
    log_mig_prior_5_15yr_share_ac_pop = safe_log(
      mig_prior_5_15yr_share_ac_pop
    ),
    log_mig_baseline_5yr_share_ac_pop = safe_log(
      mig_baseline_5yr_share_ac_pop
    ),
    
    # Contemporary migration acceleration.
    mig_accel_recent_vs_prior5_log = safe_log_ratio(
      mig_recent_5yr_total,
      mig_prior_5yr_total
    ),
    mig_accel_recent_vs_prior5_log1p =
      log1p(mig_recent_5yr_total) -
      log1p(mig_prior_5yr_total),
    
    # Migration acceleration before the current election/FDI period.
    mig_accel_prior5_vs_baseline5_log = safe_log_ratio(
      mig_prior_5yr_total,
      mig_baseline_5yr_total
    ),
    mig_accel_prior5_vs_baseline5_log1p =
      log1p(mig_prior_5yr_total) -
      log1p(mig_baseline_5yr_total),
    
    # Recent annual migration relative to the average annual migration rate
    # during the preceding ten years.
    mig_accel_recent5_vs_prior10_annual_log = safe_log_ratio(
      mig_recent_5yr_total / 5,
      mig_prior_5_15yr_total / 10
    ),
    
    # Migrant density and the population-density control required to
    # distinguish migrant concentration from general settlement density.
    mig_recent_5yr_density_sqkm = safe_divide(
      mig_recent_5yr_total,
      con08_land_area
    ),
    mig_prior_5yr_density_sqkm = safe_divide(
      mig_prior_5yr_total,
      con08_land_area
    ),
    mig_prior_5_15yr_density_sqkm = safe_divide(
      mig_prior_5_15yr_total,
      con08_land_area
    ),
    mig_baseline_5yr_density_sqkm = safe_divide(
      mig_baseline_5yr_total,
      con08_land_area
    ),
    log1p_mig_recent_5yr_density_sqkm =
      log1p(mig_recent_5yr_density_sqkm),
    log1p_mig_prior_5yr_density_sqkm =
      log1p(mig_prior_5yr_density_sqkm),
    log1p_mig_prior_5_15yr_density_sqkm =
      log1p(mig_prior_5_15yr_density_sqkm),
    log1p_mig_baseline_5yr_density_sqkm =
      log1p(mig_baseline_5yr_density_sqkm),
    ac_pop_density_sqkm = safe_divide(
      proxy_ac_pop,
      con08_land_area
    ),
    log_ac_pop_density_sqkm = safe_log(ac_pop_density_sqkm),
    
    # Compatibility aliases retained so existing analysis code still runs.
    mig_change_log_ratio = mig_accel_recent_vs_prior5_log,
    mig_change_log1p_ratio = mig_accel_recent_vs_prior5_log1p
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  migration_ac_year_base,
  c("state_no", "ac", "year"),
  "Base AC-year migration data"
)

# 06.5 Construct migration exposure in touching ACs ----

ac_neighbor_pairs <- tibble(
  local_row = seq_len(nrow(ac_reference_sf)),
  neighbor_rows = st_touches(ac_reference_sf)
) %>%
  unnest_longer(neighbor_rows, values_to = "neighbor_row") %>%
  transmute(
    state_no = ac_reference_sf$state_no[local_row],
    ac = ac_reference_sf$ac[local_row],
    neighbor_state_no = ac_reference_sf$state_no[neighbor_row],
    neighbor_ac = ac_reference_sf$ac[neighbor_row]
  ) %>%
  distinct()

neighbor_migration_ac_year <- ac_neighbor_pairs %>%
  left_join(
    migration_ac_year_base %>%
      transmute(
        neighbor_state_no = state_no,
        neighbor_ac = ac,
        year,
        neighbor_recent_share = mig_recent_5yr_share_ac_pop,
        neighbor_prior_share = mig_prior_5yr_share_ac_pop,
        neighbor_acceleration = mig_accel_recent_vs_prior5_log
      ),
    by = c("neighbor_state_no", "neighbor_ac"),
    relationship = "many-to-many"
  ) %>%
  summarise(
    mig_neighbor_n = n_distinct(
      paste(neighbor_state_no, neighbor_ac, sep = "_")
    ),
    mig_neighbor_recent_5yr_share_ac_pop =
      safe_mean(neighbor_recent_share),
    mig_neighbor_prior_5yr_share_ac_pop =
      safe_mean(neighbor_prior_share),
    mig_neighbor_accel_recent_vs_prior5_log =
      safe_mean(neighbor_acceleration),
    .by = c(state_no, ac, year)
  )

migration_ac_year <- migration_ac_year_base %>%
  left_join(
    neighbor_migration_ac_year,
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    mig_neighbor_n = replace_na(mig_neighbor_n, 0L),
    mig_local_vs_neighbor_recent_share_log = safe_log_ratio(
      mig_recent_5yr_share_ac_pop,
      mig_neighbor_recent_5yr_share_ac_pop
    ),
    mig_local_minus_neighbor_acceleration =
      mig_accel_recent_vs_prior5_log -
      mig_neighbor_accel_recent_vs_prior5_log
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  migration_ac_year,
  c("state_no", "ac", "year"),
  "AC-year migration data"
)

write_csv(
  migration_district_clean,
  file.path(INTERMEDIATE_DIR, "migration_district_clean.csv")
)
write_csv(
  migration_ac_bins,
  file.path(INTERMEDIATE_DIR, "migration_ac_bins.csv")
)
write_csv(
  migration_annual_ac,
  file.path(INTERMEDIATE_DIR, "migration_ac_annual.csv")
)
saveRDS(
  migration_annual_ac,
  file.path(INTERMEDIATE_DIR, "migration_ac_annual.rds")
)
write_csv(
  migration_ac_year,
  file.path(INTERMEDIATE_DIR, "migration_ac_year.csv")
)


# 07. Clean and aggregate FDI project counts ----

message("07. Cleaning and aggregating FDI project counts")

assert_file_exists(PATHS$fdi)

fdi_raw <- read_csv(PATHS$fdi, show_col_types = FALSE) %>%
  clean_names()

assert_has_columns(
  fdi_raw,
  c("project_date", "coordinates", "activity"),
  "FDI project data"
)

# 07.1 Parse project dates, activities, and coordinates ----

fdi_projects_clean <- fdi_raw %>%
  mutate(
    fdi_id = row_number(),
    project_month = as.Date(
      parse_date_time(project_date, orders = c("b Y", "B Y"))
    ),
    activity = str_squish(activity),
    activity_type = case_when(
      activity == "Manufacturing" ~ "manufacturing",
      activity %in% fdi_service_activities ~ "services",
      TRUE ~ "other"
    )
  ) %>%
  extract(
    coordinates,
    into = c("lat", "lon"),
    regex = "^\\s*(-?\\d+(?:\\.\\d+)?)\\s*,\\s*(-?\\d+(?:\\.\\d+)?)\\s*$",
    remove = FALSE,
    convert = TRUE
  ) %>%
  mutate(
    coordinate_valid = !is.na(lat) & !is.na(lon),
    year = case_when(
      !is.na(project_month) &
        project_month >= fdi_period_spec$period_start[1] &
        project_month <= fdi_period_spec$period_end[1] ~ 2009L,
      !is.na(project_month) &
        project_month >= fdi_period_spec$period_start[2] &
        project_month <= fdi_period_spec$period_end[2] ~ 2014L,
      TRUE ~ NA_integer_
    )
  )

# 07.2 Assign projects to containing and adjacent ACs ----

fdi_points <- fdi_projects_clean %>%
  filter(coordinate_valid) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(ac_reference_sf))

ac_spatial_index <- ac_reference_sf %>%
  mutate(ac_uid_spatial = row_number()) %>%
  select(ac_uid_spatial, state_no, ac, geometry)

fdi_local_lookup <- fdi_points %>%
  st_join(
    ac_spatial_index,
    join = st_within,
    left = TRUE
  ) %>%
  st_drop_geometry() %>%
  transmute(
    fdi_id,
    local_ac_uid = ac_uid_spatial,
    local_state_no = state_no,
    local_ac = ac
  )

# A project should have at most one containing AC.
fdi_multiple_local_matches <- fdi_local_lookup %>%
  filter(!is.na(local_ac_uid)) %>%
  count(fdi_id, name = "n") %>%
  filter(n > 1)

if (nrow(fdi_multiple_local_matches) > 0) {
  print(fdi_multiple_local_matches, n = Inf)
  stop("Some FDI projects spatially match multiple containing ACs.")
}

ac_neighbors <- tibble(
  local_ac_uid = ac_spatial_index$ac_uid_spatial,
  neighbor_row = st_touches(ac_spatial_index)
) %>%
  unnest_longer(neighbor_row, values_to = "neighbor_row") %>%
  transmute(
    local_ac_uid,
    exposed_ac_uid = ac_spatial_index$ac_uid_spatial[neighbor_row],
    exposure_type = "adjacent"
  )

ac_self <- tibble(
  local_ac_uid = ac_spatial_index$ac_uid_spatial,
  exposed_ac_uid = ac_spatial_index$ac_uid_spatial,
  exposure_type = "own"
)

ac_exposure_map <- bind_rows(ac_self, ac_neighbors) %>%
  left_join(
    ac_spatial_index %>%
      st_drop_geometry() %>%
      transmute(
        exposed_ac_uid = ac_uid_spatial,
        state_no,
        ac
      ),
    by = "exposed_ac_uid",
    relationship = "many-to-one"
  )

fdi_project_exposure <- fdi_projects_clean %>%
  left_join(
    fdi_local_lookup %>%
      filter(!is.na(local_ac_uid)) %>%
      left_join(
        ac_exposure_map,
        by = "local_ac_uid",
        relationship = "many-to-many"
      ) %>%
      distinct(fdi_id, state_no, ac, exposure_type),
    by = "fdi_id",
    relationship = "one-to-many"
  ) %>%
  mutate(spatial_match_valid = !is.na(state_no) & !is.na(ac))

# 07.3 Count total, manufacturing, and services projects per 100,000 ----

fdi_ac_year_observed <- fdi_project_exposure %>%
  filter(
    spatial_match_valid,
    !is.na(year)
  ) %>%
  distinct(fdi_id, state_no, ac, year, activity_type) %>%
  summarise(
    fdi_total_projects_n = n_distinct(fdi_id),
    fdi_mfg_projects_n = n_distinct(
      fdi_id[activity_type == "manufacturing"]
    ),
    fdi_services_projects_n = n_distinct(
      fdi_id[activity_type == "services"]
    ),
    .by = c(state_no, ac, year)
  )

fdi_ac_year <- elections_ac_year %>%
  select(state_no, ac, year) %>%
  left_join(
    fdi_ac_year_observed,
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    demographics_ac %>%
      select(state_no, ac, proxy_ac_pop),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    across(
      c(
        fdi_total_projects_n,
        fdi_mfg_projects_n,
        fdi_services_projects_n
      ),
      \(x) replace_na(x, 0L)
    ),
    fdi_total_projects_pc100k = per_100k(
      fdi_total_projects_n,
      proxy_ac_pop
    ),
    fdi_mfg_projects_pc100k = per_100k(
      fdi_mfg_projects_n,
      proxy_ac_pop
    ),
    fdi_services_projects_pc100k = per_100k(
      fdi_services_projects_n,
      proxy_ac_pop
    ),
    log1p_fdi_total_projects_pc100k =
      log1p(fdi_total_projects_pc100k),
    log1p_fdi_mfg_projects_pc100k =
      log1p(fdi_mfg_projects_pc100k),
    log1p_fdi_services_projects_pc100k =
      log1p(fdi_services_projects_pc100k)
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  fdi_ac_year,
  c("state_no", "ac", "year"),
  "AC-year FDI data"
)

fdi_project_diagnostics <- fdi_project_exposure %>%
  summarise(
    n_projects = n_distinct(fdi_id),
    n_coordinate_valid = n_distinct(fdi_id[coordinate_valid]),
    n_coordinate_invalid = n_distinct(fdi_id[!coordinate_valid]),
    n_spatially_matched = n_distinct(fdi_id[spatial_match_valid]),
    n_spatially_unmatched = n_distinct(
      fdi_id[coordinate_valid & !spatial_match_valid]
    ),
    n_outside_election_periods = n_distinct(fdi_id[is.na(year)])
  )

write_csv(
  fdi_projects_clean,
  file.path(INTERMEDIATE_DIR, "fdi_projects_clean.csv")
)
write_csv(
  fdi_project_exposure,
  file.path(INTERMEDIATE_DIR, "fdi_project_exposure.csv")
)
write_csv(fdi_ac_year, file.path(INTERMEDIATE_DIR, "fdi_ac_year.csv"))
write_csv(
  fdi_project_diagnostics,
  file.path(DIAGNOSTIC_DIR, "fdi_project_diagnostics.csv")
)


# 08. Harmonize, classify, summarize, and diagnose NES data ----

message("08. Harmonizing, classifying, and diagnosing NES data")

assert_file_exists(PATHS$nes_2009)
assert_file_exists(PATHS$nes_2014)

nes_2009_raw <- read_sav(PATHS$nes_2009)
nes_2014_raw <- read_sav(PATHS$nes_2014)

assert_has_columns(
  nes_2009_raw,
  c(
    "st_id",
    "pc_id",
    "ac_id",
    "q1a",
    "q11",
    "q11a",
    "q12",
    "q12a",
    "z5",
    "z18",
    item_spec_2009$item
  ),
  "NES 2009 data"
)

assert_has_columns(
  nes_2014_raw,
  c(
    "state_id",
    "pc_id",
    "ac_id",
    "q1a",
    "q11",
    "q11a",
    "z3",
    "z13",
    item_spec_2014$item
  ),
  "NES 2014 data"
)

# 08.1 Recode 2009 ideology, socioeconomic variables, and behavior ----

fr_codes_2009 <- nes_fr_codes %>%
  filter(year == 2009L) %>%
  pull(codes) %>%
  first()

nes_2009_clean <- nes_2009_raw %>%
  mutate(
    year = 2009L,
    respondent_uid = paste0("2009_", row_number()),
    state_no = as.integer(nes_state_recode(st_id)),
    pc = as.integer(pc_id),
    ac = as.integer(ac_id),
    across(
      all_of(item_spec_2009$item),
      identity,
      .names = "{.col}_raw"
    ),
    
    # Orient every four-response item so -2 is the left endpoint and +2
    # is the right endpoint. The middle two options are the center bucket.
    q26a = case_match(
      as.numeric(q26a_raw),
      1 ~ 2,
      2 ~ 1,
      3 ~ -1,
      4 ~ -2
    ),
    a4b = case_match(
      as.numeric(a4b_raw),
      4 ~ 2,
      3 ~ 1,
      2 ~ -1,
      1 ~ -2
    ),
    a4c = case_match(
      as.numeric(a4c_raw),
      1 ~ 2,
      2 ~ 1,
      3 ~ -1,
      4 ~ -2
    ),
    a4d = case_match(
      as.numeric(a4d_raw),
      1 ~ 2,
      2 ~ 1,
      3 ~ -1,
      4 ~ -2
    ),
    a4g = case_match(
      as.numeric(a4g_raw),
      4 ~ 2,
      3 ~ 1,
      2 ~ -1,
      1 ~ -2
    ),
    
    # Binary items are diagnostics only.
    a5 = case_match(as.numeric(a5_raw), 2 ~ 1, 1 ~ -1),
    a6a = case_match(as.numeric(a6a_raw), 2 ~ 1, 1 ~ -1),
    a6b = case_match(as.numeric(a6b_raw), 2 ~ 1, 1 ~ -1),
    q27 = case_match(as.numeric(q27_raw), 2 ~ 1, 1 ~ -1),
    
    ideology_a4b_bucket = ideology_item_bucket(a4b),
    ideology_a4c_bucket = ideology_item_bucket(a4c),
    ideology_a4d_bucket = ideology_item_bucket(a4d),
    ideology_a4g_bucket = ideology_item_bucket(a4g),
    ideology_q26a_bucket = ideology_item_bucket(q26a),
    
    recognition_n_required = 2L,
    statism_n_required = 3L,
    recognition_n_valid = rowSums(
      !is.na(pick(ideology_a4b_bucket, ideology_a4c_bucket))
    ),
    statism_n_valid = rowSums(
      !is.na(
        pick(
          ideology_a4d_bucket,
          ideology_a4g_bucket,
          ideology_q26a_bucket
        )
      )
    ),
    recognition_ideology = strict_axis_bucket(
      pick(ideology_a4b_bucket, ideology_a4c_bucket)
    ),
    statism_ideology = strict_axis_bucket(
      pick(
        ideology_a4d_bucket,
        ideology_a4g_bucket,
        ideology_q26a_bucket
      )
    ),
    voter_ideology = combine_axis_buckets(
      recognition_ideology,
      statism_ideology
    ),
    ideology_classification_status = classification_status(
      recognition_ideology,
      statism_ideology
    ),
    ideology_n_required_items =
      recognition_n_required + statism_n_required,
    ideology_n_valid_items = recognition_n_valid + statism_n_valid,
    ideology_complete =
      ideology_n_valid_items == ideology_n_required_items,
    
    education_code = as.numeric(haven::zap_missing(z5)),
    education_label = as.character(
      haven::as_factor(haven::zap_missing(z5), levels = "labels")
    ),
    household_income_monthly = as.numeric(haven::zap_missing(z18)),
    household_income_monthly = if_else(
      is.finite(household_income_monthly) & household_income_monthly >= 0,
      household_income_monthly,
      NA_real_
    ),
    household_income_valid = !is.na(household_income_monthly),
    
    reported_vote_party = as.numeric(q1a),
    voted_congress = recode_vote_indicator(
      q1a,
      target_codes = 1,
      invalid_codes = c(98, 99)
    ),
    voted_bjp = recode_vote_indicator(
      q1a,
      target_codes = 2,
      invalid_codes = c(98, 99)
    ),
    voted_shs = recode_vote_indicator(
      q1a,
      target_codes = 46,
      invalid_codes = c(98, 99)
    ),
    voted_mns = recode_vote_indicator(
      q1a,
      target_codes = 91,
      invalid_codes = c(98, 99)
    ),
    voted_fr = recode_vote_indicator(
      q1a,
      target_codes = fr_codes_2009,
      invalid_codes = c(98, 99)
    ),
    close_congress = recode_party_affinity(
      q11,
      q11a,
      yes_code = 1,
      no_code = 2,
      target_codes = 1
    ),
    close_bjp = recode_party_affinity(
      q11,
      q11a,
      yes_code = 1,
      no_code = 2,
      target_codes = 2
    ),
    close_shs = recode_party_affinity(
      q11,
      q11a,
      yes_code = 1,
      no_code = 2,
      target_codes = 46
    ),
    close_mns = recode_party_affinity(
      q11,
      q11a,
      yes_code = 1,
      no_code = 2,
      target_codes = 91
    ),
    close_fr = recode_party_affinity(
      q11,
      q11a,
      yes_code = 1,
      no_code = 2,
      target_codes = fr_codes_2009
    ),
    not_close_bjp = if_else(!is.na(close_bjp), 1 - close_bjp, NA_real_),
    not_close_fr = if_else(!is.na(close_fr), 1 - close_fr, NA_real_),
    never_vote_bjp = recode_party_affinity(
      q12,
      q12a,
      yes_code = 1,
      no_code = 2,
      target_codes = 2
    ),
    never_vote_shs = recode_party_affinity(
      q12,
      q12a,
      yes_code = 1,
      no_code = 2,
      target_codes = 46
    ),
    never_vote_mns = recode_party_affinity(
      q12,
      q12a,
      yes_code = 1,
      no_code = 2,
      target_codes = 91
    ),
    never_vote_fr = recode_party_affinity(
      q12,
      q12a,
      yes_code = 1,
      no_code = 2,
      target_codes = fr_codes_2009
    ),
    survey_weight = get_survey_weight(nes_2009_raw, NES_WEIGHT_VAR_2009),
    survey_weight_validated = !is.null(NES_WEIGHT_VAR_2009)
  )

# 08.2 Recode 2014 ideology, socioeconomic variables, and behavior ----

fr_codes_2014 <- nes_fr_codes %>%
  filter(year == 2014L) %>%
  pull(codes) %>%
  first()

nes_2014_clean <- nes_2014_raw %>%
  mutate(
    year = 2014L,
    respondent_uid = paste0("2014_", row_number()),
    state_no = as.integer(nes_state_recode(state_id)),
    pc = as.integer(pc_id),
    ac = as.integer(ac_id),
    across(
      all_of(item_spec_2014$item),
      identity,
      .names = "{.col}_raw"
    ),
    q10b = case_match(
      as.numeric(q10b_raw),
      1 ~ 2,
      2 ~ 1,
      3 ~ -1,
      4 ~ -2
    ),
    q10e = case_match(
      as.numeric(q10e_raw),
      4 ~ 2,
      3 ~ 1,
      2 ~ -1,
      1 ~ -2
    ),
    q23c = case_match(
      as.numeric(q23c_raw),
      4 ~ 2,
      3 ~ 1,
      2 ~ -1,
      1 ~ -2
    ),
    
    ideology_q10b_bucket = ideology_item_bucket(q10b),
    ideology_q10e_bucket = ideology_item_bucket(q10e),
    ideology_q23c_bucket = ideology_item_bucket(q23c),
    
    recognition_n_required = 2L,
    statism_n_required = 1L,
    recognition_n_valid = rowSums(
      !is.na(pick(ideology_q10b_bucket, ideology_q10e_bucket))
    ),
    statism_n_valid = rowSums(!is.na(pick(ideology_q23c_bucket))),
    recognition_ideology = strict_axis_bucket(
      pick(ideology_q10b_bucket, ideology_q10e_bucket)
    ),
    statism_ideology = strict_axis_bucket(pick(ideology_q23c_bucket)),
    voter_ideology = combine_axis_buckets(
      recognition_ideology,
      statism_ideology
    ),
    ideology_classification_status = classification_status(
      recognition_ideology,
      statism_ideology
    ),
    ideology_n_required_items =
      recognition_n_required + statism_n_required,
    ideology_n_valid_items = recognition_n_valid + statism_n_valid,
    ideology_complete =
      ideology_n_valid_items == ideology_n_required_items,
    
    education_code = as.numeric(haven::zap_missing(z3)),
    education_label = as.character(
      haven::as_factor(haven::zap_missing(z3), levels = "labels")
    ),
    household_income_monthly = as.numeric(haven::zap_missing(z13)),
    household_income_monthly = if_else(
      is.finite(household_income_monthly) & household_income_monthly >= 0,
      household_income_monthly,
      NA_real_
    ),
    household_income_valid = !is.na(household_income_monthly),
    
    reported_vote_party = as.numeric(q1a),
    voted_congress = recode_vote_indicator(
      q1a,
      target_codes = 1,
      invalid_codes = c(96, 98, 99)
    ),
    voted_bjp = recode_vote_indicator(
      q1a,
      target_codes = 2,
      invalid_codes = c(96, 98, 99)
    ),
    voted_shs = recode_vote_indicator(
      q1a,
      target_codes = 46,
      invalid_codes = c(96, 98, 99)
    ),
    voted_mns = recode_vote_indicator(
      q1a,
      target_codes = 47,
      invalid_codes = c(96, 98, 99)
    ),
    voted_fr = recode_vote_indicator(
      q1a,
      target_codes = fr_codes_2014,
      invalid_codes = c(96, 98, 99)
    ),
    close_congress = recode_party_affinity(
      q11,
      q11a,
      yes_code = 2,
      no_code = 1,
      target_codes = 1
    ),
    close_bjp = recode_party_affinity(
      q11,
      q11a,
      yes_code = 2,
      no_code = 1,
      target_codes = 2
    ),
    close_shs = recode_party_affinity(
      q11,
      q11a,
      yes_code = 2,
      no_code = 1,
      target_codes = 46
    ),
    close_mns = recode_party_affinity(
      q11,
      q11a,
      yes_code = 2,
      no_code = 1,
      target_codes = 47
    ),
    close_fr = recode_party_affinity(
      q11,
      q11a,
      yes_code = 2,
      no_code = 1,
      target_codes = fr_codes_2014
    ),
    not_close_bjp = if_else(!is.na(close_bjp), 1 - close_bjp, NA_real_),
    not_close_fr = if_else(!is.na(close_fr), 1 - close_fr, NA_real_),
    # NES 2014 does not contain the 2009 q12/q12a never-vote sequence.
    never_vote_bjp = NA_real_,
    never_vote_shs = NA_real_,
    never_vote_mns = NA_real_,
    never_vote_fr = NA_real_,
    survey_weight = get_survey_weight(nes_2014_raw, NES_WEIGHT_VAR_2014),
    survey_weight_validated = !is.null(NES_WEIGHT_VAR_2014)
  )

# Add year-specific placeholder columns so bind_rows can select one schema.
nes_2009_clean <- nes_2009_clean %>%
  mutate(
    ideology_q10b_bucket = NA_character_,
    ideology_q10e_bucket = NA_character_,
    ideology_q23c_bucket = NA_character_
  )

nes_2014_clean <- nes_2014_clean %>%
  mutate(
    ideology_a4b_bucket = NA_character_,
    ideology_a4c_bucket = NA_character_,
    ideology_a4d_bucket = NA_character_,
    ideology_a4g_bucket = NA_character_,
    ideology_q26a_bucket = NA_character_
  )

# 08.3 Harmonized respondent-level output ----

nes_respondent_variables <- c(
  "year",
  "respondent_uid",
  "state_no",
  "pc",
  "ac",
  "ideology_a4b_bucket",
  "ideology_a4c_bucket",
  "ideology_a4d_bucket",
  "ideology_a4g_bucket",
  "ideology_q26a_bucket",
  "ideology_q10b_bucket",
  "ideology_q10e_bucket",
  "ideology_q23c_bucket",
  "recognition_ideology",
  "statism_ideology",
  "recognition_n_required",
  "recognition_n_valid",
  "statism_n_required",
  "statism_n_valid",
  "ideology_n_required_items",
  "ideology_n_valid_items",
  "ideology_complete",
  "ideology_classification_status",
  "voter_ideology",
  "education_code",
  "education_label",
  "household_income_monthly",
  "household_income_valid",
  "reported_vote_party",
  "voted_congress",
  "voted_bjp",
  "voted_shs",
  "voted_mns",
  "voted_fr",
  "close_congress",
  "close_bjp",
  "close_shs",
  "close_mns",
  "close_fr",
  "not_close_bjp",
  "not_close_fr",
  "never_vote_bjp",
  "never_vote_shs",
  "never_vote_mns",
  "never_vote_fr",
  "survey_weight",
  "survey_weight_validated"
)

nes_respondent_clean <- bind_rows(
  nes_2009_clean %>%
    mutate(across(everything(), plain_col)) %>%
    select(all_of(nes_respondent_variables)),
  nes_2014_clean %>%
    mutate(across(everything(), plain_col)) %>%
    select(all_of(nes_respondent_variables))
) %>%
  mutate(
    state_no = as.integer(state_no),
    pc = as.integer(pc),
    ac = as.integer(ac),
    voter_ideology = factor(
      voter_ideology,
      levels = c("Left", "Center", "Right", "Mixed")
    ),
    recognition_ideology = factor(
      recognition_ideology,
      levels = c("Left", "Center", "Right", "Mixed")
    ),
    statism_ideology = factor(
      statism_ideology,
      levels = c("Left", "Center", "Right", "Mixed")
    )
  )

assert_unique_rows(
  nes_respondent_clean,
  "respondent_uid",
  "Harmonized NES respondent data"
)

# 08.4 Preserve item-level and binary-diagnostic response files ----

ideology_item_responses_long <- bind_rows(
  build_item_response_long(
    nes_2009_clean,
    2009L,
    item_spec_2009,
    NES_WEIGHT_VAR_2009
  ),
  build_item_response_long(
    nes_2014_clean,
    2014L,
    item_spec_2014,
    NES_WEIGHT_VAR_2014
  )
)

binary_item_responses_long <- map_dfr(
  binary_item_spec_2009$item,
  function(item_name) {
    raw_name <- paste0(item_name, "_raw")
    raw_value <- nes_2009_clean[[raw_name]]
    oriented_value <- as.numeric(nes_2009_clean[[item_name]])
    
    tibble(
      year = 2009L,
      respondent_uid = nes_2009_clean$respondent_uid,
      voter_ideology = as.character(nes_2009_clean$voter_ideology),
      item = item_name,
      item_label = variable_label(raw_value, item_name),
      raw_response = as.numeric(raw_value),
      original_response_label = as.character(
        haven::as_factor(raw_value, levels = "labels")
      ),
      response_bucket = case_when(
        oriented_value == -1 ~ "Left-coded response",
        oriented_value == 1 ~ "Right-coded response",
        TRUE ~ NA_character_
      )
    )
  }
)

# 08.5 Construct observed AC-year-ideology cells ----

nes_ac_year_classification <- nes_respondent_clean %>%
  filter(!is.na(state_no), !is.na(ac), !is.na(year)) %>%
  summarise(
    nes_pc = first_nonmissing(pc),
    nes_n_distinct_pc = n_distinct(pc, na.rm = TRUE),
    nes_n_respondents = n(),
    nes_n_ideology_complete = sum(ideology_complete, na.rm = TRUE),
    nes_n_ideology_missing = sum(!ideology_complete, na.rm = TRUE),
    nes_n_left = sum(voter_ideology == "Left", na.rm = TRUE),
    nes_n_center = sum(voter_ideology == "Center", na.rm = TRUE),
    nes_n_right = sum(voter_ideology == "Right", na.rm = TRUE),
    nes_n_mixed = sum(voter_ideology == "Mixed", na.rm = TRUE),
    survey_weight_validated = any(survey_weight_validated),
    .by = c(state_no, ac, year)
  ) %>%
  mutate(
    nes_pct_left_all_respondents = safe_pct(
      nes_n_left,
      nes_n_respondents
    ),
    nes_pct_center_all_respondents = safe_pct(
      nes_n_center,
      nes_n_respondents
    ),
    nes_pct_right_all_respondents = safe_pct(
      nes_n_right,
      nes_n_respondents
    ),
    nes_pct_mixed_all_respondents = safe_pct(
      nes_n_mixed,
      nes_n_respondents
    ),
    nes_pct_ideology_missing_all_respondents = safe_pct(
      nes_n_ideology_missing,
      nes_n_respondents
    ),
    nes_pct_left_among_ideology_complete = safe_pct(
      nes_n_left,
      nes_n_ideology_complete
    ),
    nes_pct_center_among_ideology_complete = safe_pct(
      nes_n_center,
      nes_n_ideology_complete
    ),
    nes_pct_right_among_ideology_complete = safe_pct(
      nes_n_right,
      nes_n_ideology_complete
    ),
    nes_pct_mixed_among_ideology_complete = safe_pct(
      nes_n_mixed,
      nes_n_ideology_complete
    )
  )

nes_ac_year_ideology_observed <- nes_respondent_clean %>%
  filter(
    !is.na(state_no),
    !is.na(ac),
    !is.na(year),
    !is.na(voter_ideology)
  ) %>%
  mutate(ideology = as.character(voter_ideology)) %>%
  summarise(
    n_respondents = n(),
    n_vote_valid = sum(!is.na(voted_fr)),
    n_voted_bjp = sum(voted_bjp == 1, na.rm = TRUE),
    n_voted_fr = sum(voted_fr == 1, na.rm = TRUE),
    n_close_valid = sum(!is.na(close_fr)),
    n_close_congress = sum(close_congress == 1, na.rm = TRUE),
    n_close_bjp = sum(close_bjp == 1, na.rm = TRUE),
    n_close_fr = sum(close_fr == 1, na.rm = TRUE),
    n_never_vote_valid = sum(!is.na(never_vote_fr)),
    n_never_vote_bjp = sum(never_vote_bjp == 1, na.rm = TRUE),
    n_never_vote_fr = sum(never_vote_fr == 1, na.rm = TRUE),
    n_income_valid = sum(household_income_valid, na.rm = TRUE),
    mean_household_income = if_else(
      n_income_valid > 0,
      mean(household_income_monthly, na.rm = TRUE),
      NA_real_
    ),
    median_household_income = if_else(
      n_income_valid > 0,
      median(household_income_monthly, na.rm = TRUE),
      NA_real_
    ),
    .by = c(state_no, ac, year, ideology)
  ) %>%
  mutate(
    pct_voted_bjp = safe_pct(n_voted_bjp, n_vote_valid),
    pct_voted_fr = safe_pct(n_voted_fr, n_vote_valid),
    pct_close_congress = safe_pct(n_close_congress, n_close_valid),
    pct_close_bjp = safe_pct(n_close_bjp, n_close_valid),
    pct_close_fr = safe_pct(n_close_fr, n_close_valid),
    pct_never_vote_bjp = safe_pct(
      n_never_vote_bjp,
      n_never_vote_valid
    ),
    pct_never_vote_fr = safe_pct(
      n_never_vote_fr,
      n_never_vote_valid
    ),
    cell_n_ge_5 = n_respondents >= 5,
    cell_n_ge_10 = n_respondents >= 10
  ) %>%
  left_join(
    nes_ac_year_classification %>%
      select(state_no, ac, year, nes_n_ideology_complete),
    by = c("state_no", "ac", "year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    pct_ac_ideology_complete = safe_pct(
      n_respondents,
      nes_n_ideology_complete
    )
  ) %>%
  arrange(
    state_no,
    ac,
    year,
    factor(ideology, levels = c("Left", "Center", "Right", "Mixed"))
  )

# 08.6 Construct the wide one-row-per-AC-year ideology summary ----

nes_sampled_ac_years <- nes_ac_year_classification %>%
  select(state_no, ac, year)

nes_ac_year_ideology_complete_grid <- nes_sampled_ac_years %>%
  crossing(ideology_reference) %>%
  left_join(
    nes_ac_year_ideology_observed,
    by = c("state_no", "ac", "year", "ideology"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(
        n_respondents,
        n_vote_valid,
        n_voted_bjp,
        n_voted_fr,
        n_close_valid,
        n_close_congress,
        n_close_bjp,
        n_close_fr,
        n_never_vote_valid,
        n_never_vote_bjp,
        n_never_vote_fr,
        n_income_valid
      ),
      \(x) replace_na(x, 0L)
    ),
    cell_n_ge_5 = replace_na(cell_n_ge_5, FALSE),
    cell_n_ge_10 = replace_na(cell_n_ge_10, FALSE)
  )

nes_ac_year_ideology_wide <- nes_ac_year_ideology_complete_grid %>%
  select(
    state_no,
    ac,
    year,
    ideology_slug,
    n_respondents,
    pct_ac_ideology_complete,
    n_vote_valid,
    pct_voted_bjp,
    pct_voted_fr,
    n_close_valid,
    pct_close_congress,
    pct_close_bjp,
    pct_close_fr,
    n_never_vote_valid,
    pct_never_vote_bjp,
    pct_never_vote_fr,
    n_income_valid,
    mean_household_income,
    median_household_income,
    cell_n_ge_5,
    cell_n_ge_10
  ) %>%
  pivot_wider(
    names_from = ideology_slug,
    values_from = c(
      n_respondents,
      pct_ac_ideology_complete,
      n_vote_valid,
      pct_voted_bjp,
      pct_voted_fr,
      n_close_valid,
      pct_close_congress,
      pct_close_bjp,
      pct_close_fr,
      n_never_vote_valid,
      pct_never_vote_bjp,
      pct_never_vote_fr,
      n_income_valid,
      mean_household_income,
      median_household_income,
      cell_n_ge_5,
      cell_n_ge_10
    ),
    names_glue = "nes_{ideology_slug}_{.value}"
  )

nes_ac_year_ideology_summary <- nes_ac_year_classification %>%
  left_join(
    nes_ac_year_ideology_wide,
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  nes_ac_year_ideology_summary,
  c("state_no", "ac", "year"),
  "Wide NES AC-year ideology summary"
)

assert_unique_rows(
  nes_ac_year_ideology_observed,
  c("state_no", "ac", "year", "ideology"),
  "Observed NES AC-year-ideology cells"
)

# 08.7 Construct diagnostic tables ----

ideology_classification_by_year <- nes_respondent_clean %>%
  mutate(
    ideology_display = if_else(
      is.na(voter_ideology),
      "Missing",
      as.character(voter_ideology)
    ),
    ideology_display = factor(
      ideology_display,
      levels = c("Left", "Center", "Right", "Mixed", "Missing")
    )
  ) %>%
  count(year, ideology_display, name = "n_respondents") %>%
  group_by(year) %>%
  mutate(
    n_year = sum(n_respondents),
    pct_all_respondents = 100 * n_respondents / n_year,
    n_ideology_complete = sum(
      n_respondents[ideology_display != "Missing"]
    ),
    pct_ideology_complete = if_else(
      ideology_display != "Missing" & n_ideology_complete > 0,
      100 * n_respondents / n_ideology_complete,
      NA_real_
    )
  ) %>%
  ungroup()

binary_item_responses_by_year_ideology <- binary_item_responses_long %>%
  filter(!is.na(voter_ideology)) %>%
  group_by(year, item, item_label, voter_ideology) %>%
  mutate(
    n_group_total = n(),
    n_valid = sum(!is.na(response_bucket))
  ) %>%
  ungroup() %>%
  filter(!is.na(response_bucket)) %>%
  count(
    year,
    item,
    item_label,
    voter_ideology,
    response_bucket,
    n_group_total,
    n_valid,
    name = "n_response"
  ) %>%
  mutate(
    pct_among_valid = safe_pct(n_response, n_valid),
    pct_among_group_total = safe_pct(n_response, n_group_total)
  )

education_distribution_by_year_ideology <- nes_respondent_clean %>%
  filter(!is.na(voter_ideology)) %>%
  group_by(year, voter_ideology) %>%
  mutate(n_group_total = n()) %>%
  ungroup() %>%
  filter(!is.na(education_code), !is.na(education_label)) %>%
  count(
    year,
    voter_ideology,
    education_code,
    education_label,
    n_group_total,
    name = "n_education"
  ) %>%
  group_by(year, voter_ideology) %>%
  mutate(
    n_education_valid = sum(n_education),
    pct_within_ideology = 100 * n_education / n_education_valid
  ) %>%
  ungroup()

income_summary_by_year_ideology <- nes_respondent_clean %>%
  filter(!is.na(voter_ideology)) %>%
  summarise(
    n_group = n(),
    n_income_valid = sum(household_income_valid, na.rm = TRUE),
    n_income_zero = sum(household_income_monthly == 0, na.rm = TRUE),
    mean_income = if_else(
      n_income_valid > 0,
      mean(household_income_monthly, na.rm = TRUE),
      NA_real_
    ),
    median_income = if_else(
      n_income_valid > 0,
      median(household_income_monthly, na.rm = TRUE),
      NA_real_
    ),
    income_p10 = if_else(
      n_income_valid > 0,
      as.numeric(quantile(household_income_monthly, 0.10, na.rm = TRUE)),
      NA_real_
    ),
    income_p25 = if_else(
      n_income_valid > 0,
      as.numeric(quantile(household_income_monthly, 0.25, na.rm = TRUE)),
      NA_real_
    ),
    income_p75 = if_else(
      n_income_valid > 0,
      as.numeric(quantile(household_income_monthly, 0.75, na.rm = TRUE)),
      NA_real_
    ),
    income_p90 = if_else(
      n_income_valid > 0,
      as.numeric(quantile(household_income_monthly, 0.90, na.rm = TRUE)),
      NA_real_
    ),
    .by = c(year, voter_ideology)
  )

# 08.8 Export diagnostic figures ----

plot_ideology_levels <- c("Left", "Center", "Right", "Mixed")

walk(sort(unique(nes_respondent_clean$year)), function(year_value) {
  plot_data <- ideology_classification_by_year %>%
    filter(year == year_value)
  
  plot_object <- ggplot(
    plot_data,
    aes(x = pct_all_respondents, y = ideology_display)
  ) +
    geom_col() +
    geom_text(
      aes(label = paste0(round(pct_all_respondents, 1), "% (N=", n_respondents, ")")),
      hjust = -0.05,
      size = 3.5
    ) +
    scale_x_continuous(
      limits = c(0, max(plot_data$pct_all_respondents, na.rm = TRUE) * 1.25),
      labels = scales::label_percent(scale = 1)
    ) +
    labs(
      title = paste("Ideology classification in NES", year_value),
      subtitle = "Strict item agreement within axes and agreement across axes",
      x = "Share of the full survey sample",
      y = NULL
    ) +
    theme_minimal(base_size = 11)
  
  ggsave(
    file.path(
      DIAGNOSTIC_DIR,
      paste0("ideology_classification_", year_value, ".png")
    ),
    plot_object,
    width = 8,
    height = 5,
    dpi = 300
  )
})

walk(binary_item_spec_2009$item, function(item_name) {
  plot_data <- binary_item_responses_by_year_ideology %>%
    filter(item == item_name) %>%
    mutate(
      voter_ideology = factor(voter_ideology, levels = plot_ideology_levels)
    )
  
  item_title <- first_nonmissing(plot_data$item_label)
  
  plot_object <- ggplot(
    plot_data,
    aes(
      x = pct_among_valid,
      y = voter_ideology,
      fill = response_bucket
    )
  ) +
    geom_col() +
    scale_x_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1)
    ) +
    labs(
      title = item_title,
      subtitle = "NES 2009; ideology classified from four-response items only",
      x = "Share of valid substantive responses",
      y = NULL,
      fill = "Response"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave(
    file.path(
      DIAGNOSTIC_DIR,
      paste0("binary_", item_name, "_by_ideology_2009.png")
    ),
    plot_object,
    width = 9,
    height = 5,
    dpi = 300
  )
})

walk(sort(unique(nes_respondent_clean$year)), function(year_value) {
  education_plot_data <- education_distribution_by_year_ideology %>%
    filter(year == year_value) %>%
    mutate(
      voter_ideology = factor(voter_ideology, levels = plot_ideology_levels),
      education_label = fct_reorder(
        education_label,
        education_code,
        .fun = min
      )
    )
  
  education_plot <- ggplot(
    education_plot_data,
    aes(
      x = pct_within_ideology,
      y = voter_ideology,
      fill = education_label
    )
  ) +
    geom_col() +
    scale_x_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1)
    ) +
    labs(
      title = paste("Education distribution by ideology, NES", year_value),
      subtitle = "Original ordered education categories for this survey year",
      x = "Share within ideology group",
      y = NULL,
      fill = "Education"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
  
  ggsave(
    file.path(
      DIAGNOSTIC_DIR,
      paste0("education_distribution_by_ideology_", year_value, ".png")
    ),
    education_plot,
    width = 11,
    height = 6,
    dpi = 300
  )
  
  income_plot_data <- nes_respondent_clean %>%
    filter(
      year == year_value,
      !is.na(voter_ideology),
      household_income_valid
    ) %>%
    mutate(
      voter_ideology = factor(voter_ideology, levels = plot_ideology_levels)
    )
  
  income_plot <- ggplot(
    income_plot_data,
    aes(x = voter_ideology, y = household_income_monthly)
  ) +
    geom_boxplot(outlier.alpha = 0.2) +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(base = 10),
      labels = scales::label_number(big.mark = ",")
    ) +
    labs(
      title = paste("Monthly household income by ideology, NES", year_value),
      subtitle = "Pseudo-log scale retains zero and low reported incomes",
      x = NULL,
      y = "Monthly household income (rupees)"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave(
    file.path(
      DIAGNOSTIC_DIR,
      paste0("income_distribution_by_ideology_", year_value, ".png")
    ),
    income_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
})

# 08.9 Export respondent, summary, and diagnostic data ----

write_csv(
  nes_respondent_clean,
  file.path(INTERMEDIATE_DIR, "nes_respondent_clean.csv")
)
saveRDS(
  nes_respondent_clean,
  file.path(INTERMEDIATE_DIR, "nes_respondent_clean.rds")
)
write_csv(
  ideology_item_responses_long,
  file.path(INTERMEDIATE_DIR, "ideology_item_responses_long.csv")
)
saveRDS(
  ideology_item_responses_long,
  file.path(INTERMEDIATE_DIR, "ideology_item_responses_long.rds")
)
write_csv(
  binary_item_responses_long,
  file.path(INTERMEDIATE_DIR, "binary_item_responses_long.csv")
)
write_csv(
  nes_ac_year_ideology_summary,
  file.path(INTERMEDIATE_DIR, "ac_year_ideology_summary.csv")
)
write_csv(
  nes_ac_year_ideology_observed,
  file.path(INTERMEDIATE_DIR, "ac_year_ideology_long.csv")
)
write_csv(
  ideology_classification_by_year,
  file.path(DIAGNOSTIC_DIR, "ideology_classification_by_year.csv")
)
write_csv(
  binary_item_responses_by_year_ideology,
  file.path(DIAGNOSTIC_DIR, "binary_item_responses_by_year_ideology.csv")
)
write_csv(
  education_distribution_by_year_ideology,
  file.path(DIAGNOSTIC_DIR, "education_distribution_by_year_ideology.csv")
)
write_csv(
  income_summary_by_year_ideology,
  file.path(DIAGNOSTIC_DIR, "income_summary_by_year_ideology.csv")
)


# 09. Construct final AC-year and observed AC-year-ideology datasets ----

message("09. Constructing final joined datasets")

# 09.1 One row per AC-year, including the wide ideology summary ----

ac_year_final <- elections_ac_year %>%
  left_join(
    ac_reference %>%
      select(
        state_no,
        ac,
        district_code_2011,
        district_name_2011,
        manual_xwalk,
        district_join_success
      ),
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    demographics_ac,
    by = c("state_no", "ac"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    migration_ac_year %>%
      select(-any_of(c("proxy_ac_pop", "con08_land_area"))),
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    fdi_ac_year %>% select(-proxy_ac_pop),
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    nes_ac_year_ideology_summary,
    by = c("state_no", "ac", "year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    nes_pc_matches_election_pc = case_when(
      is.na(nes_pc) ~ NA,
      TRUE ~ nes_pc == pc
    ),
    ac_uid = paste(state_no, ac, sep = "_"),
    pc_cluster_id = paste(state_no, pc, sep = "_")
  ) %>%
  select(
    state,
    state_no,
    pc_name,
    pc,
    ac_name,
    ac,
    year,
    ac_uid,
    pc_cluster_id,
    district_code_2011,
    district_name_2011,
    manual_xwalk,
    district_join_success,
    valid_votes,
    bjp_votes,
    fr_party_votes,
    bjp_vote_share,
    fr_party_vote_share,
    bjp_candidate_present,
    shs_candidate_present,
    mns_candidate_present,
    fr_candidate_present,
    fr_candidate_n,
    proxy_ac_pop,
    proxy_ac_pop_source,
    con08_land_area,
    employment_total_ac,
    employment_manufacturing_ac,
    employment_services_ac,
    emp_rate,
    sc_population_ac,
    st_population_ac,
    sc_pop_share,
    st_pop_share,
    matches("^(mig_|log_mig_|log1p_mig_)"),
    ac_pop_density_sqkm,
    log_ac_pop_density_sqkm,
    fdi_total_projects_n,
    fdi_mfg_projects_n,
    fdi_services_projects_n,
    fdi_total_projects_pc100k,
    fdi_mfg_projects_pc100k,
    fdi_services_projects_pc100k,
    log1p_fdi_total_projects_pc100k,
    log1p_fdi_mfg_projects_pc100k,
    log1p_fdi_services_projects_pc100k,
    starts_with("nes_"),
    survey_weight_validated
  ) %>%
  arrange(state_no, ac, year)

assert_unique_rows(
  ac_year_final,
  c("state_no", "ac", "year"),
  "Final AC-year data"
)

# 09.2 Observed AC-year-ideology cells only ----

ac_year_ideology_long_final <- ac_year_final %>%
  select(-starts_with("nes_"), -survey_weight_validated) %>%
  inner_join(
    nes_ac_year_ideology_observed,
    by = c("state_no", "ac", "year"),
    relationship = "one-to-many"
  ) %>%
  arrange(
    state_no,
    ac,
    year,
    factor(ideology, levels = c("Left", "Center", "Right", "Mixed"))
  )

assert_unique_rows(
  ac_year_ideology_long_final,
  c("state_no", "ac", "year", "ideology"),
  "Final observed AC-year-ideology data"
)


# 10. Validate, document, and export ----

message("10. Validating and exporting final data")

# 10.1 Final missingness and sample diagnostics ----

final_missingness <- ac_year_final %>%
  summarise(across(everything(), \(x) sum(is.na(x)))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_rows = nrow(ac_year_final),
    pct_missing = 100 * n_missing / n_rows
  ) %>%
  arrange(desc(pct_missing), variable)

geography_diagnostics <- ac_reference %>%
  summarise(
    n_ac = n(),
    n_missing_district = sum(is.na(district_code_2011)),
    n_manual_xwalk = sum(manual_xwalk, na.rm = TRUE),
    .by = state_no
  ) %>%
  left_join(state_lookup, by = "state_no") %>%
  arrange(desc(n_missing_district), state_no)

population_allocation_diagnostics <- ac_allocation_weights %>%
  count(proxy_ac_pop_source, allocation_warning, name = "n_ac") %>%
  arrange(desc(n_ac))

migration_diagnostics <- migration_annual_ac %>%
  summarise(
    n_ac_years = n(),
    n_missing = sum(is.na(annual_migrants)),
    n_imputed = sum(migration_imputed),
    .by = migration_year
  ) %>%
  arrange(migration_year)

nes_diagnostics <- nes_respondent_clean %>%
  summarise(
    n_respondents = n(),
    n_missing_state = sum(is.na(state_no)),
    n_missing_ac = sum(is.na(ac)),
    n_ideology_complete = sum(ideology_complete, na.rm = TRUE),
    n_left = sum(voter_ideology == "Left", na.rm = TRUE),
    n_center = sum(voter_ideology == "Center", na.rm = TRUE),
    n_right = sum(voter_ideology == "Right", na.rm = TRUE),
    n_mixed = sum(voter_ideology == "Mixed", na.rm = TRUE),
    n_vote_valid = sum(!is.na(voted_fr)),
    n_close_valid = sum(!is.na(close_fr)),
    n_never_vote_valid = sum(!is.na(never_vote_fr)),
    n_education_valid = sum(!is.na(education_code)),
    n_income_valid = sum(household_income_valid, na.rm = TRUE),
    .by = year
  )

# 10.2 Human-readable data dictionary ----

variable_dictionary_manual <- tribble(
  ~variable, ~label, ~source, ~unit, ~level, ~formula_or_definition, ~missing_meaning,
  "voter_ideology", "Strict respondent ideology", "NES/Lokniti", "Left/Center/Right/Mixed", "Respondent", "All required four-response items must fall in one bucket within each axis, and the axes must agree for a pure category", "At least one required ideology item is missing",
  "recognition_ideology", "Strict recognition-axis ideology", "NES/Lokniti", "Left/Center/Right/Mixed", "Respondent", "All required recognition items fall in one bucket; otherwise Mixed", "At least one required recognition item is missing",
  "statism_ideology", "Strict statism-axis ideology", "NES/Lokniti", "Left/Center/Right/Mixed", "Respondent", "All required statism items fall in one bucket; otherwise Mixed", "At least one required statism item is missing",
  "education_code", "Original education response code", "NES/Lokniti", "ordered category code", "Respondent", "Year-specific original survey response", "Education response missing",
  "education_label", "Original education response label", "NES/Lokniti", "ordered category label", "Respondent", "Year-specific original survey label", "Education response missing",
  "household_income_monthly", "Total monthly household income", "NES/Lokniti", "rupees", "Respondent", "Reported income of all household members", "Income response missing or invalid",
  "nes_pct_left_among_ideology_complete", "Left share of complete ideology classifications", "NES/Lokniti", "percent", "AC-year", "Left respondents divided by respondents with all required ideology items", "No complete ideology classifications",
  "nes_pct_center_among_ideology_complete", "Center share of complete ideology classifications", "NES/Lokniti", "percent", "AC-year", "Center respondents divided by respondents with all required ideology items", "No complete ideology classifications",
  "nes_pct_right_among_ideology_complete", "Right share of complete ideology classifications", "NES/Lokniti", "percent", "AC-year", "Right respondents divided by respondents with all required ideology items", "No complete ideology classifications",
  "nes_pct_mixed_among_ideology_complete", "Mixed share of complete ideology classifications", "NES/Lokniti", "percent", "AC-year", "Mixed respondents divided by respondents with all required ideology items", "No complete ideology classifications"
)

data_dictionary <- tibble(variable = names(ac_year_final)) %>%
  left_join(variable_dictionary_manual, by = "variable") %>%
  mutate(
    label = coalesce(
      label,
      variable %>% str_replace_all("_", " ") %>% str_to_sentence()
    ),
    source = coalesce(source, "See construction script"),
    unit = coalesce(unit, "See construction script"),
    level = coalesce(level, "AC-year"),
    formula_or_definition = coalesce(
      formula_or_definition,
      "See named construction step in script"
    ),
    missing_meaning = coalesce(
      missing_meaning,
      "Source value or required denominator unavailable"
    )
  ) %>%
  select(
    variable,
    label,
    source,
    unit,
    level,
    formula_or_definition,
    missing_meaning
  )

# 10.3 Export final data and supporting files ----

write_csv(ac_year_final, file.path(FINAL_DIR, "ac_year.csv"))
saveRDS(ac_year_final, file.path(FINAL_DIR, "ac_year.rds"))

write_csv(
  nes_ac_year_ideology_summary,
  file.path(FINAL_DIR, "ac_year_ideology_summary.csv")
)
saveRDS(
  nes_ac_year_ideology_summary,
  file.path(FINAL_DIR, "ac_year_ideology_summary.rds")
)

write_csv(
  ac_year_ideology_long_final,
  file.path(FINAL_DIR, "ac_year_ideology_long.csv")
)
saveRDS(
  ac_year_ideology_long_final,
  file.path(FINAL_DIR, "ac_year_ideology_long.rds")
)

write_csv(data_dictionary, file.path(FINAL_DIR, "data_dictionary.csv"))
write_csv(final_missingness, file.path(DIAGNOSTIC_DIR, "final_missingness.csv"))
write_csv(
  geography_diagnostics,
  file.path(DIAGNOSTIC_DIR, "geography_diagnostics.csv")
)
write_csv(
  population_allocation_diagnostics,
  file.path(DIAGNOSTIC_DIR, "population_allocation_diagnostics.csv")
)
write_csv(
  migration_diagnostics,
  file.path(DIAGNOSTIC_DIR, "migration_diagnostics.csv")
)
write_csv(nes_diagnostics, file.path(DIAGNOSTIC_DIR, "nes_diagnostics.csv"))

output_manifest <- tribble(
  ~file, ~unit, ~description,
  "intermediate/ac_reference.csv", "AC", "Resolved AC, PC, and district identifiers",
  "intermediate/ac_allocation_weights.csv", "AC", "Population proxies and allocation shares",
  "intermediate/elections_ac_year.csv", "AC-year", "Official election outcomes and candidate-presence indicators",
  "intermediate/demographics_ac.csv", "AC", "Allocated employment, SC, ST, land, and population controls",
  "intermediate/migration_ac_annual.csv", "AC-calendar year", "Annualized migration with post-2011 imputation",
  "intermediate/migration_ac_year.csv", "AC-year", "Election-relative migration levels, shares, acceleration, density, and neighboring exposure",
  "intermediate/fdi_ac_year.csv", "AC-year", "FDI project counts and projects per 100,000",
  "intermediate/nes_respondent_clean.rds", "NES respondent", "Strict ideology classification, behavior, education, and income",
  "intermediate/ac_year_ideology_summary.csv", "AC-year", "Wide ideology composition and ideology-specific behavior and income summaries",
  "intermediate/ac_year_ideology_long.csv", "Observed AC-year-ideology", "Observed ideology cells only",
  "diagnostics/ideology_classification_by_year.csv", "Year-ideology", "Classification counts and shares",
  "diagnostics/binary_item_responses_by_year_ideology.csv", "Year-item-ideology-response", "Binary-item diagnostic responses",
  "diagnostics/education_distribution_by_year_ideology.csv", "Year-ideology-education", "Education distributions",
  "diagnostics/income_summary_by_year_ideology.csv", "Year-ideology", "Income summaries",
  "final/ac_year.csv", "AC-year", "Final AC-year data including wide ideology summaries",
  "final/ac_year_ideology_summary.csv", "AC-year", "Standalone wide ideology summary",
  "final/ac_year_ideology_long.csv", "Observed AC-year-ideology", "Joined observed ideology-cell analysis data",
  "final/data_dictionary.csv", "Variable", "Definitions, sources, units, and missing-value meanings"
)

write_csv(output_manifest, file.path(DERIVED_DIR, "output_manifest.csv"))

message("Data construction complete.")
message("Final AC-year file: ", file.path(FINAL_DIR, "ac_year.csv"))
message(
  "Final AC-year ideology summary: ",
  file.path(FINAL_DIR, "ac_year_ideology_summary.csv")
)
message(
  "Final observed AC-year-ideology file: ",
  file.path(FINAL_DIR, "ac_year_ideology_long.csv")
)

# Regressions ----
#1
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k, data = ac_year_final), stars = TRUE)

#2
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k + proxy_ac_pop, data = ac_year_final), stars = TRUE)

#3
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k + proxy_ac_pop + con08_land_area, data = ac_year_final), stars = TRUE)

#4
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share, data = ac_year_final), stars = TRUE)

#5
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share, data = ac_year_final), stars = TRUE)

#6
modelsummary(lm(bjp_vote_share ~ mig_accel_prior5_vs_baseline5_log1p + log1p_fdi_mfg_projects_pc100k + mig_accel_prior5_vs_baseline5_log1p * log1p_fdi_mfg_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share + mig_recent_5yr_share_ac_pop_pct, data = ac_year_final), stars = TRUE)

#7
modelsummary(lm(fr_party_vote_share ~ mig_local_vs_neighbor_recent_share_log + log1p_fdi_total_projects_pc100k + mig_local_vs_neighbor_recent_share_log * log1p_fdi_total_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share + mig_recent_5yr_share_ac_pop_pct + mig_prior_5_15yr_share_ac_pop_pct + factor(year), data = ac_year_final), stars = TRUE)

#8
modelsummary(feols(fr_party_vote_share ~ mig_local_vs_neighbor_recent_share_log + log1p_fdi_total_projects_pc100k + mig_local_vs_neighbor_recent_share_log * log1p_fdi_total_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share + mig_recent_5yr_share_ac_pop_pct + mig_prior_5_15yr_share_ac_pop_pct + factor(year) | state_no, data = ac_year_final), stars = TRUE)

modelsummary(feols(fr_party_vote_share ~ mig_accel_recent_vs_prior5_log1p + log1p_fdi_total_projects_pc100k + mig_accel_recent_vs_prior5_log1p * log1p_fdi_total_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share + log_mig_prior_5yr_share_ac_pop + log_mig_baseline_5yr_share_ac_pop | state_no, data = ac_year_final), stars = TRUE)


# Control pair 1: log_mig_prior_5yr_share_ac_pop + log_mig_baseline_5yr_share_ac_pop
# Rationale: Starting-level and earlier-baseline population shares
modelsummary(feols(nes_center_pct_voted_fr ~ log1p_mig_prior_5_15yr_total + log1p_fdi_total_projects_pc100k + proxy_ac_pop + con08_land_area + sc_pop_share + st_pop_share + factor(year) | state_no, data = ac_year_final), stars = TRUE)


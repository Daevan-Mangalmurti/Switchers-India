# =============================================================================
# STATUS THREAT: INTEGRATED "PUZZLE" PIPELINE
# =============================================================================
#
# PURPOSE
# -------
# Build one canonical respondent-level IVS dataset and use it to produce:
#
#   1A. Grouped puzzle figure:
#       Far-right vote share over time for Left / Moderate / Right under
#       the current ideology definition and the AID vs LMIC classification.
#       Export both a lines-only version and a faint-points + lines version.
#
#   1B. Global puzzle figure:
#       Same Left / Moderate / Right trends pooling all included countries.
#       Export both lines-only and faint-points + lines versions.
#
#   1B-WAVE. Survey-wave descriptive figures:
#       Global and AID/LMIC wave means positioned on calendar time, with WVS
#       and EVS retained as distinct survey programs.
#
#   1B-BALANCED. Balanced-panel robustness figures:
#       Separate WVS and EVS global trend replications using countries observed
#       with usable data in every post-1990 wave of that survey program.
#
#   1C. Country-specific outputs:
#       Plot observed ideological-group trajectories for every included country,
#       and retain country-specific slope estimates as CSV diagnostics.
#
#   2. Moderate policy-position distributions:
#       For respondents in each ideology group, with Moderates defined as E033 = 5-6,
#       response category on selected policy items. One PDF page per country.
#
#   3. Moderate prevalence table:
#       For every included country, report the mean survey-specific percentage of
#       valid ideology respondents who identify as 5 or 6, plus the range
#       across surveys, number of surveys, and respondent counts.
#
# DESIGN PRINCIPLE
# ----------------
# Raw IVS -> one cleaned respondent-level dataset -> two ideology-specific output trees.
# Definitions and sample restrictions are declared once in the configuration
# section and reused everywhere.
#
# PARTY CODING REVISION v6 (2026-08-12):
# Adds an IVS-harmonized-to-PPC label bridge so EVS5/WVS categories whose
# numeric codes changed during harmonization are not falsely excluded.
#
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(countrycode)
  library(gridExtra)
  library(readxl)
  library(ggforce)
})


# =============================================================================
# 1. PATHS AND ANALYSIS SETTINGS
# =============================================================================

# ---- Input files -------------------------------------------------------------

status_threat_data_dir <-
  Sys.getenv(
    "STATUS_THREAT_DATA_DIR",
    unset = file.path(
      "Status Threat",
      "Status Threat Data"
    )
  )

ivs_path <- file.path(
  status_threat_data_dir,
  "Integrated_values_surveys_1981-2022.dta"
)

# Expected long format:
#   code_type   code
#   wvs_id      ...
#   evs_id      ...
#   party_id    ...
#   party_id_2  ...
fr_codes_path <- file.path(
  status_threat_data_dir,
  "far_right_party_codes.csv"
)

# Reviewed PopuList/WVS/EVS crosswalk created for this project. It remains
# useful as a manually reviewed supplement, especially for WVS7, which is not
# covered by PPC v1.1.
populist_crosswalk_path <- file.path(
  status_threat_data_dir,
  "populist_far_right_wvs_evs_crosswalk.csv"
)

# Full PopuList 4.0 file. The analysis now uses the *year-specific* strict
# far-right interval farright_startnobl:farright_endnobl rather than treating a
# party as far right in every survey year merely because it is ever classified
# as far right.
populist_raw_path <- file.path(
  status_threat_data_dir,
  "The PopuList 4.0.csv"
)

# Political Parties Crosswalk (PPC) v1.1. PPC is used to identify the actual
# party represented by country × survey × wave × year and to distinguish real
# parties from non-party response categories ("other", "none", blank ballot,
# etc.). IMPORTANT: PPC source-file numeric codes are not assumed to equal IVS
# harmonized numeric codes. When the numeric join fails, a conservative label
# bridge is attempted before the observation is treated as PPC-unmatched.
ppc_path <- file.path(
  status_threat_data_dir,
  "PPC_v1_1.xlsx"
)

# Explicit reviewed exceptions for countries outside PopuList or for survey
# combinations that cannot be resolved by PPC/PopuList. Negative overrides are
# intentionally conservative: they prevent known false positives without
# creating new far-right positives.
FR_MANUAL_OVERRIDES <- tibble::tribble(
  ~party_mapping_country_code, ~year, ~party_source, ~resp_party_id, ~far_right_override, ~override_reason,
  "GB", 2009L, "WVS", 826015, FALSE,
  "WVS 2009 code 826015 is the Scottish National Party (SNP), not Reform UK",
  "AL", 2002L, "WVS", 8014, FALSE,
  "WVS 2002 Albania code 8014 is the New Labor/Party of Labour option, not far right"
)

# ---- Country classification --------------------------------------------------

COUNTRY_SCHEME_FILE_STUB <- "aid_lmic"


# ---- Ideology specifications -------------------------------------------------
#
# One execution produces the full downstream analysis under BOTH definitions:
#
#   "narrow"
#       Left     = E033 = 1 or 2
#       Moderate = E033 = 5 or 6
#       Right    = E033 = 9 or 10
#
#   "broad"
#       Left     = E033 = 1-4
#       Moderate = E033 = 5 or 6
#       Right    = E033 = 7-10
#
# Party cleaning/classification is performed only once. The ideology-dependent
# analyses are then run twice and written to separate folders.

IDEOLOGY_SCHEMES <- tibble::tribble(
  ~scheme_id, ~file_stub, ~scheme_label,
  "narrow", "ideology_1_2_vs_5_6_vs_9_10",
  "Left = 1-2; Moderate = 5-6; Right = 9-10",
  "broad", "ideology_lt5_vs_5_6_vs_gt6",
  "Left = 1-4; Moderate = 5-6; Right = 7-10"
)


# ---- Output directories ------------------------------------------------------
#
# Shared party-coding and sample audits are written once under common/.
# Every ideology-dependent analysis is written under its own complete folder.

OUT_ROOT <- file.path("out", COUNTRY_SCHEME_FILE_STUB)

# ---- Clean previous outputs -------------------------------------------------
#
# Start every complete pipeline run from an empty output tree so obsolete
# files from earlier versions cannot be mistaken for current results.

CLEAN_OUTPUT <- TRUE

if (
  CLEAN_OUTPUT &&
  dir.exists(OUT_ROOT)
) {
  message(
    "Removing previous pipeline outputs: ",
    OUT_ROOT
  )
  
  unlink(
    OUT_ROOT,
    recursive = TRUE,
    force = TRUE
  )
}

out_dir_data  <- file.path(OUT_ROOT, "common", "data")
out_dir_csv   <- file.path(OUT_ROOT, "common", "csv")
out_dir_plots <- file.path(OUT_ROOT, "common", "plots")

dir.create(out_dir_data,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_csv,   recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_plots, recursive = TRUE, showWarnings = FALSE)

# ---- Main sample rules -------------------------------------------------------

START_YEAR <- 1990L

# Require a country to have at least this many distinct post-START_YEAR survey
# YEARS with valid E033 data before it enters the common included-country sample.
MIN_COUNTRY_YEARS <- 4L

# Preserve the earlier analysis choice that countries should have at least one
# observed far-right voter among self-identified moderates (E033 = 5-6).
# Set FALSE to retain countries even if moderate far-right support is always 0.
REQUIRE_ANY_MODERATE_FR <- TRUE

# Minimum number of distinct usable country-years required to report a
# country-specific time slope and its uncertainty.
MIN_SLOPE_YEARS <- 4L

# Minimum number of respondents with an observed party choice required for a
# country-year-ideology cell to enter trend and slope calculations.
# The original analysis effectively allowed cells with any observed data.
MIN_FR_CELL_N <- 1L

# Cap point-size scaling in the points + lines puzzle figures so very large
# country-year-ideology cells do not visually dominate the figure.
POINT_SIZE_CAP <- 1500L

# Keep the country-year observations visible but visually subordinate to the
# fitted trend lines in the points + lines versions.
POINT_ALPHA <- 0.15

# Cell-size thresholds to audit. The primary analysis still uses MIN_FR_CELL_N,
# but these values are used to show how many cells are thin and whether trend
# or country-slope estimates materially change when thin cells are excluded.
CELL_N_DIAGNOSTIC_THRESHOLDS <- c(1L, 5L, 10L, 25L, 50L, 100L)
CELL_N_SENSITIVITY_THRESHOLDS <- c(1L, 10L, 25L, 50L)

# ---- Survey weights ---------------------------------------------------------
#
# The IVS/WVS time-series convention uses S017 as the survey weight. The script
# always preserves both weighted and unweighted country-year far-right shares
# when S017 is available. Set this TRUE to make weighted shares the quantity
# used in the trend figures and country-slope calculations.
#
# Important: survey weights correct the composition of respondents WITHIN a
# survey. They do not by themselves determine how countries should be weighted
# against one another in a cross-national trend line. The trend lines below
# continue to give each country-year cell equal weight unless changed explicitly.
USE_SURVEY_WEIGHTS <- TRUE
SURVEY_WEIGHT_VAR <- "S017"

# ---- Country classification --------------------------------------------------
#
# Binary AID/LMIC classification only.
#
#   - the 24 countries in BINARY_LMIC_CODES are LMICs
#   - Russia, Egypt, Belarus, and Hong Kong are dropped
#   - every other usable country is assigned to AID

BINARY_LMIC_CODES <- c(
  "AL",  # Albania
  "AM",  # Armenia
  "AR",  # Argentina
  "BA",  # Bosnia and Herzegovina
  "BG",  # Bulgaria
  "BR",  # Brazil
  "CO",  # Colombia
  "GE",  # Georgia
  "IN",  # India
  "ID",  # Indonesia
  "MA",  # Morocco
  "MD",  # Moldova
  "ME",  # Montenegro
  "MK",  # North Macedonia
  "MX",  # Mexico
  "NG",  # Nigeria
  "PE",  # Peru
  "PH",  # Philippines
  "RO",  # Romania
  "RS",  # Serbia
  "ZA",  # South Africa
  "TR",  # Turkey
  "UA",  # Ukraine
  "ZW"   # Zimbabwe
)

DROP_CODES <- c(
  "RU",  # Russia
  "EG",  # Egypt
  "HK",  # Hong Kong
  "BY"   # Belarus
)

ANALYSIS_GROUP_LEVELS <- c(
  "Advanced Industrialized Democracies",
  "Low & Middle-Income Countries"
)

# ---- Ideology display settings -----------------------------------------------

IDEOLOGY_LEVELS <- c("Left", "Moderate", "Right")

IDEOLOGY_COLORS <- c(
  "Left"     = "#0000FF",
  "Moderate" = "#FFD700",
  "Right"    = "#FF0000"
)

# Redundant non-color encoding: Moderate is dashed on EVERY line graph.
IDEOLOGY_LINETYPES <- c(
  "Left"     = "solid",
  "Moderate" = "dashed",
  "Right"    = "solid"
)

SURVEY_SOURCE_SHAPES <- c(
  "WVS" = 16,
  "EVS" = 17
)

SUPPORT_MEASURE_NOTE <- paste0(
  "Support is measured using respondents' party-choice or party-appeal responses: ",
  "E179_WVS (\"Which party would you vote for if there were a national election tomorrow?\") ",
  "in the World Values Survey; E179 (\"Which political party would you vote for?\") ",
  "in EVS waves 1-4, with E181/E181A ",
  "(\"Which political party appeals to you most?\") used as fallbacks where applicable; ",
  "EVS wave 5 uses E181A."
)

classify_ideology <- function(e033, scheme_id) {
  if (scheme_id == "narrow") {
    return(
      factor(
        dplyr::case_when(
          is.na(e033) ~ NA_character_,
          e033 %in% c(1, 2) ~ "Left",
          e033 %in% c(5, 6) ~ "Moderate",
          e033 %in% c(9, 10) ~ "Right",
          TRUE ~ NA_character_
        ),
        levels = IDEOLOGY_LEVELS
      )
    )
  }
  
  if (scheme_id == "broad") {
    return(
      factor(
        dplyr::case_when(
          is.na(e033) ~ NA_character_,
          e033 < 5 ~ "Left",
          e033 %in% c(5, 6) ~ "Moderate",
          e033 > 6 ~ "Right",
          TRUE ~ NA_character_
        ),
        levels = IDEOLOGY_LEVELS
      )
    )
  }
  
  stop("Unknown ideology scheme: ", scheme_id)
}

ideology_labels_for_scheme <- function(scheme_id) {
  if (scheme_id == "narrow") {
    return(
      c(
        "Left"     = "Left (1-2)",
        "Moderate" = "Moderate (5,6)",
        "Right"    = "Right (9-10)"
      )
    )
  }
  
  if (scheme_id == "broad") {
    return(
      c(
        "Left"     = "Left (1-4)",
        "Moderate" = "Moderate (5,6)",
        "Right"    = "Right (7-10)"
      )
    )
  }
  
  stop("Unknown ideology scheme: ", scheme_id)
}

THEME_PUZZLE <- theme_minimal(
  base_size = 12,
) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      lineheight = 1.1,
      margin = margin(b = 10)
    ),
    
    plot.caption = element_text(
      size = 9,
      lineheight = 1.1,
      hjust = 0,
      margin = margin(t = 10)
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    legend.position = "bottom",
    
    legend.box = "vertical",
    
    legend.text = element_text(
      size = 10
    ),
    
    legend.title = element_text(
      size = 10
    ),
    
    # More whitespace around the entire plot.
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 20
    )
  )

# ---- Policy variables for Output 2 ------------------------------------------
#
# Each row declares:
#   var        = IVS variable name
#   label      = facet title in the PDF
#   min_value  = lowest valid response
#   max_value  = highest valid response
#   reverse    = whether to reverse the scale so all configured items point in
#                the same conceptual direction.
#
# The original composite-index code reversed E036 and E039 using 11 - response.
# This configuration preserves that harmonization logic.
#
# IMPORTANT: Replace the generic labels below with the actual survey-question
# wording or concise substantive labels before publication.

POLICY_SPECS <- tribble(
  ~var,   ~label,                              ~min_value, ~max_value, ~reverse,
  "E035", "E035: Policy position",                      1,         10, FALSE,
  "E036", "E036: Policy position (reversed)",           1,         10, TRUE,
  "E037", "E037: Policy position",                      1,         10, FALSE,
  "E039", "E039: Policy position (reversed)",           1,         10, TRUE
)

LPM_Y_MAX <- 35

# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

make_analysis_group <- function(code) {
  dplyr::case_when(
    is.na(code) ~ NA_character_,
    code %in% DROP_CODES ~ NA_character_,
    code %in% BINARY_LMIC_CODES ~ "Low & Middle-Income Countries",
    TRUE ~ "Advanced Industrialized Democracies"
  )
}

# Return a column if it exists; otherwise return a same-length NA vector.
col_or_na <- function(df, var, mode = c("numeric", "character")) {
  mode <- match.arg(mode)
  
  if (var %in% names(df)) {
    return(df[[var]])
  }
  
  if (mode == "numeric") {
    rep(NA_real_, nrow(df))
  } else {
    rep(NA_character_, nrow(df))
  }
}

# Strip haven value labels and convert to numeric.
to_num <- function(x) {
  suppressWarnings(as.numeric(haven::zap_labels(x)))
}

# IVS uses negative values for non-substantive responses such as:
# don't know, no answer, not asked, missing, etc.
NEGATIVE_MISSING_CODES <- -5:-1

# Clean a bounded numeric survey scale.
clean_scale <- function(x, min_value, max_value) {
  xi <- to_num(x)
  xi[xi %in% NEGATIVE_MISSING_CODES] <- NA_real_
  xi[!(xi >= min_value & xi <= max_value)] <- NA_real_
  xi
}

# Party identifiers are positive numeric codes. Negative and non-positive values
# are treated as missing.
clean_party_id <- function(x) {
  xi <- to_num(x)
  xi[xi %in% NEGATIVE_MISSING_CODES] <- NA_real_
  xi[xi <= 0] <- NA_real_
  xi
}

# IVS S025 often embeds country code + year in one numeric field, e.g. 3482018.
extract_year <- function(x) {
  if (is.numeric(x)) {
    y <- floor(x)
    return(as.integer(y %% 10000))
  }
  
  first_year <- stringr::str_extract(as.character(x), "\\d{4}")
  suppressWarnings(as.integer(first_year))
}

# Clean an explicit survey-wave variable. Unlike the previous version, the
# script does NOT infer WVS versus EVS from which party variable happens to be
# nonmissing. WVS wave comes from S002; EVS wave comes from S002evs.
clean_wave <- function(x, valid_waves) {
  w <- suppressWarnings(as.integer(to_num(x)))
  w[!(w %in% valid_waves)] <- NA_integer_
  w
}

# Convert a labelled variable to readable text without treating its numeric
# value as a party ID. Useful for the study/source audit.
labelled_to_character <- function(x) {
  if (inherits(x, "haven_labelled")) {
    as.character(haven::as_factor(x, levels = "default"))
  } else {
    as.character(x)
  }
}

# Recover the readable value label associated with a party-choice variable.
# This is used for auditing and for a conservative non-party screen in waves
# not covered by PPC (especially WVS7).
party_label_or_na <- function(x) {
  if (inherits(x, "haven_labelled")) {
    out <- as.character(haven::as_factor(x, levels = "default"))
  } else {
    out <- as.character(x)
  }
  valid_id <- clean_party_id(x)
  out[is.na(valid_id)] <- NA_character_
  out
}

# Explicitly identify labels that are response categories rather than parties.
# PPC remains the preferred validator where available; this is a fallback for
# survey-waves not represented in PPC v1.1.
is_nonparty_label <- function(x) {
  z <- stringr::str_to_lower(stringr::str_squish(dplyr::coalesce(x, "")))
  z == "" |
    stringr::str_detect(
      z,
      paste0(
        "(^|: )other($| party$|, please)|",
        "no \\[(no )?other\\] party|no other party|no party|",
        "none of (the )?(above|parties)|none$|",
        "would not vote|will not vote|not vote|wouldn't vote|",
        "no right to vote|not eligible to vote|ineligible to vote|",
        "empty ballot|blank ballot|invalid ballot|spoilt ballot|",
        "no answer|don.t know|not applicable|not asked|missing|",
        "no \\[no other\\] party appeals"
      )
    )
}

# Normalize the country codes that were explicitly handled in the original code.
normalize_s009 <- function(x) {
  y <- as.character(x)
  y[y %in% c("GB-GBN", "UK")] <- "GB"
  y[y == "PO"] <- "PL"
  y
}

# Party mapping uses a slightly broader normalization than the substantive
# country analysis: Great Britain and Northern Ireland can both use the UK
# PopuList/PPC party universe without forcing the analysis records themselves
# to be merged.
normalize_party_mapping_country <- function(x) {
  raw <- as.character(x)
  y <- normalize_s009(raw)
  y[raw %in% c("GB-NIR", "NIR")] <- "GB"
  y
}

normalize_party_name_key <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", " ") %>%
    stringr::str_squish()
}

# Normalize a survey party label for bridging IVS harmonized party categories
# to PPC source-file categories. The IVS 2017/18 EVS harmonization frequently
# preserves the label while changing the numeric code (e.g. Italy FdI 380048
# in IVS versus 38014 in PPC). This key therefore strips country prefixes and
# punctuation but retains substantive words.
normalize_party_bridge_label <- function(x) {
  z <- as.character(x)
  
  # Transliterate accents where possible while preserving a fallback if the
  # platform cannot transliterate a particular string.
  z_ascii <- suppressWarnings(iconv(z, from = "", to = "ASCII//TRANSLIT"))
  z <- dplyr::if_else(!is.na(z_ascii), z_ascii, z)
  
  z %>%
    stringr::str_to_lower() %>%
    stringr::str_squish() %>%
    # Strip prefixes such as "FR:", "GB-GBN:", "CZE:", etc.
    stringr::str_replace("^[a-z]{2,3}(?:-[a-z]{2,3})?\\s*:\\s*", "") %>%
    stringr::str_replace_all("&", " and ") %>%
    stringr::str_replace_all("[^a-z0-9]+", " ") %>%
    # Known spelling variation in the harmonized labels.
    stringr::str_replace_all("\\bspontaneus\\b", "spontaneous") %>%
    stringr::str_replace_all("\\bconseravative\\b", "conservative") %>%
    stringr::str_squish()
}

# Generate country display labels, with manual fallbacks.
make_country_labels <- function(code) {
  label <- suppressWarnings(
    countrycode::countrycode(
      code,
      origin = "iso2c",
      destination = "country.name"
    )
  )
  
  ifelse(
    is.na(label),
    paste0("Country_", code),
    label
  )
}

# Clean survey weights. Positive finite values are retained; everything else
# becomes missing.
clean_weight <- function(x) {
  w <- to_num(x)
  w[!is.finite(w) | w <= 0] <- NA_real_
  w
}

# Safely compute a percentage.
safe_pct <- function(num, denom) {
  ifelse(is.finite(denom) & denom > 0, 100 * num / denom, NA_real_)
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  mean(x)
}

safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  max(x)
}

# Kish effective sample size for weighted cells. This is useful for detecting
# cases where a nominally large cell is effectively much smaller because a few
# respondents carry large weights.
effective_n <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (length(w) == 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

# Fit a simple time slope and return uncertainty. With only two distinct year points the
# slope exists algebraically but its residual standard error is undefined, so
# the confidence interval and p-value are returned as missing.
fit_time_slope <- function(d, outcome = "pct_far_right") {
  d <- d %>%
    dplyr::filter(
      !is.na(year),
      is.finite(.data[[outcome]])
    )
  
  n_years <- dplyr::n_distinct(d$year)
  
  if (n_years < MIN_SLOPE_YEARS) {
    return(
      tibble::tibble(
        slope_pp_per_year = NA_real_,
        slope_se = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        n_years_for_slope = n_years,
        first_year = if (nrow(d)) min(d$year) else NA_integer_,
        last_year = if (nrow(d)) max(d$year) else NA_integer_
      )
    )
  }
  
  fit <- stats::lm(
    stats::reformulate("year", response = outcome),
    data = d
  )
  
  slope <- unname(stats::coef(fit)[["year"]])
  
  if (stats::df.residual(fit) > 0) {
    coefs <- summary(fit)$coefficients
    se <- unname(coefs["year", "Std. Error"])
    p <- unname(coefs["year", "Pr(>|t|)"])
    ci <- stats::confint(fit, "year", level = 0.95)
    lo <- unname(ci[1])
    hi <- unname(ci[2])
  } else {
    se <- p <- lo <- hi <- NA_real_
  }
  
  tibble::tibble(
    slope_pp_per_year = slope,
    slope_se = se,
    conf_low = lo,
    conf_high = hi,
    p_value = p,
    n_years_for_slope = n_years,
    first_year = min(d$year),
    last_year = max(d$year)
  )
}

# Build a multipage PDF table.
write_paginated_table_pdf <- function(
    df,
    file,
    title,
    rows_per_page = 24L,
    width = 12,
    height = 8.5
) {
  grDevices::pdf(file, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  if (nrow(df) == 0) {
    grid::grid.newpage()
    grid::grid.text(
      paste0(title, "\n\nNo rows available under the current filters."),
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
    return(invisible(NULL))
  }
  
  starts <- seq.int(1L, nrow(df), by = rows_per_page)
  
  for (start in starts) {
    end <- min(start + rows_per_page - 1L, nrow(df))
    page_df <- df[start:end, , drop = FALSE]
    
    page_title <- grid::textGrob(
      title,
      x = 0.01,
      hjust = 0,
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    )
    
    table <- gridExtra::tableGrob(
      page_df,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        core = list(fg_params = list(cex = 0.78)),
        colhead = list(fg_params = list(cex = 0.82, fontface = 2))
      )
    )
    
    grid::grid.newpage()
    gridExtra::grid.arrange(
      page_title,
      table,
      ncol = 1,
      heights = c(0.08, 0.92)
    )
  }
  
  invisible(NULL)
}

wrap_plot_text <- function(x, width = 100) {
  stringr::str_wrap(
    x,
    width = width
  )
}

# Create a country-list caption from the data actually entering a plot.
country_list_caption <- function(
    data,
    width = 110,
    prefix = "Countries included"
) {
  
  countries <- data %>%
    dplyr::filter(
      !is.na(country_label)
    ) %>%
    dplyr::distinct(
      country_label
    ) %>%
    dplyr::arrange(
      country_label
    ) %>%
    dplyr::pull(
      country_label
    )
  
  wrap_plot_text(
    paste0(
      prefix,
      " (N = ",
      length(countries),
      "): ",
      paste(
        countries,
        collapse = ", "
      )
    ),
    width = width
  )
}

# Create separate country lists for the AID and LMIC facets.
country_list_by_group_caption <- function(
    data,
    width = 62,
    gap = 6
) {
  
  country_group_df <- data %>%
    dplyr::filter(
      !is.na(country_label),
      !is.na(analysis_group)
    ) %>%
    dplyr::distinct(
      analysis_group,
      country_label
    ) %>%
    dplyr::mutate(
      analysis_group = as.character(analysis_group)
    )
  
  group_short_labels <- c(
    "Advanced Industrialized Democracies" = "AIDs",
    "Low & Middle-Income Countries" = "LMICs"
  )
  
  make_group_lines <- function(g) {
    
    countries <- country_group_df %>%
      dplyr::filter(
        analysis_group == g
      ) %>%
      dplyr::arrange(
        country_label
      ) %>%
      dplyr::pull(
        country_label
      )
    
    if (length(countries) == 0L) {
      return(character(0))
    }
    
    txt <- paste0(
      unname(group_short_labels[g]),
      " (N = ",
      length(countries),
      "): ",
      paste(countries, collapse = ", ")
    )
    
    strsplit(
      wrap_plot_text(
        txt,
        width = width
      ),
      "\n",
      fixed = TRUE
    )[[1]]
  }
  
  aid_lines <- make_group_lines(
    "Advanced Industrialized Democracies"
  )
  
  lmic_lines <- make_group_lines(
    "Low & Middle-Income Countries"
  )
  
  n_lines <- max(
    length(aid_lines),
    length(lmic_lines),
    1L
  )
  
  length(aid_lines) <- n_lines
  length(lmic_lines) <- n_lines
  
  aid_lines[is.na(aid_lines)] <- ""
  lmic_lines[is.na(lmic_lines)] <- ""
  
  left_width <- max(
    nchar(aid_lines),
    1L
  )
  
  paste(
    sprintf(
      paste0(
        "%-",
        left_width,
        "s",
        strrep(" ", gap),
        "%s"
      ),
      aid_lines,
      lmic_lines
    ),
    collapse = "\n"
  )
}

# =============================================================================
# 3. LOAD ONLY THE VARIABLES NEEDED FOR THIS PIPELINE
# =============================================================================

needed_cols <- unique(c(
  # Country, explicit WVS/EVS wave metadata, survey year, study, weight
  "S001", "S002", "S002evs", "S002EVS", "S009", "S025", SURVEY_WEIGHT_VAR,
  
  # Ideological self-placement
  "E033",
  
  # Party identifiers. IMPORTANT:
  #   E179WVS = WVS party choice
  #   E179    = EVS party choice in older EVS waves
  #   E181    = EVS fallback party appeal in some older waves
  #   E181A   = EVS party appeal in EVS5 (2017)
  #   E181C   = party left-right scale and is deliberately NOT used as party ID
  "E179_WVS", "E179WVS", "E179", "E181", "E181A", "E181C",
  
  # Configured policy variables
  POLICY_SPECS$var
))

ivs <- haven::read_dta(
  ivs_path,
  col_select = tidyselect::any_of(needed_cols)
)

# Standardize spellings that vary across IVS releases.
if ("E179WVS" %in% names(ivs) && !("E179_WVS" %in% names(ivs))) {
  ivs <- dplyr::rename(ivs, E179_WVS = E179WVS)
}
if ("S002EVS" %in% names(ivs) && !("S002evs" %in% names(ivs))) {
  ivs <- dplyr::rename(ivs, S002evs = S002EVS)
}

message(
  "Loaded IVS columns: ",
  paste(names(ivs), collapse = ", ")
)


# =============================================================================
# 4. CLEAN PARTY IDENTIFIERS AND BUILD TEMPORAL FAR-RIGHT CROSSWALK
# =============================================================================
#
# CRITICAL DESIGN CHANGE
# ----------------------
# 1. WVS versus EVS is identified from IVS metadata (S002 / S002evs), not from
#    whichever party variable is nonmissing.
# 2. E181C is NEVER used as a party identifier; it is a party left-right scale.
# 3. PPC validates the exact party and distinguishes real parties from positive
#    non-party codes such as "No party appeals to me". The manually reviewed
#    project crosswalk remains the primary party-to-survey mapping.
# 4. In PopuList countries, the legacy crosswalk is NEVER used. A party is far
#    right only when its survey-year falls inside PopuList's strict,
#    non-borderline interval farright_startnobl:farright_endnobl.
# 5. Only countries outside PopuList use the legacy far_right_party_codes.csv,
#    subject to party-validity screening and reviewed manual exclusions.
# =============================================================================

# ---- 4A. Resolve survey source and wave from IVS metadata --------------------

survey_year <- extract_year(col_or_na(ivs, "S025"))
wvs_wave_meta <- clean_wave(col_or_na(ivs, "S002"), 1:7)
evs_wave_meta <- clean_wave(col_or_na(ivs, "S002evs"), 1:5)
study_label <- labelled_to_character(col_or_na(ivs, "S001"))

party_source <- dplyr::case_when(
  !is.na(wvs_wave_meta) & is.na(evs_wave_meta) ~ "WVS",
  is.na(wvs_wave_meta) & !is.na(evs_wave_meta) ~ "EVS",
  
  # Fallback only when the explicit wave metadata are ambiguous/missing and the
  # labelled study variable itself clearly identifies the source.
  stringr::str_detect(stringr::str_to_lower(study_label), "world values|\\bwvs\\b") ~ "WVS",
  stringr::str_detect(stringr::str_to_lower(study_label), "european values|\\bevs\\b") ~ "EVS",
  TRUE ~ NA_character_
)

survey_wave <- dplyr::case_when(
  party_source == "WVS" ~ wvs_wave_meta,
  party_source == "EVS" ~ evs_wave_meta,
  TRUE ~ NA_integer_
)

# ---- 4B. Select the correct source-specific party variable ------------------

# WVS: E179WVS only. E179 is NOT a WVS fallback.
wvs_party_id <- clean_party_id(col_or_na(ivs, "E179_WVS"))
wvs_party_label <- party_label_or_na(col_or_na(ivs, "E179_WVS"))

# EVS older-wave vote choice and fallback appeal variables.
evs_e179_id <- clean_party_id(col_or_na(ivs, "E179"))
evs_e179_label <- party_label_or_na(col_or_na(ivs, "E179"))
evs_e181_id <- clean_party_id(col_or_na(ivs, "E181"))
evs_e181_label <- party_label_or_na(col_or_na(ivs, "E181"))

# EVS5 uses E181A. E181C is deliberately ignored because the IVS dictionary
# defines it as the left-right location of the party, not the party identity.
evs_e181a_id <- clean_party_id(col_or_na(ivs, "E181A"))
evs_e181a_label <- party_label_or_na(col_or_na(ivs, "E181A"))

resp_party_id <- dplyr::case_when(
  party_source == "WVS" ~ wvs_party_id,
  
  party_source == "EVS" & survey_wave %in% 1:4 ~
    dplyr::coalesce(evs_e179_id, evs_e181_id, evs_e181a_id),
  
  party_source == "EVS" & survey_wave == 5 ~ evs_e181a_id,
  
  TRUE ~ NA_real_
)

party_choice_basis <- dplyr::case_when(
  party_source == "WVS" & !is.na(wvs_party_id) ~ "WVS_E179WVS_vote_choice",
  
  party_source == "EVS" & survey_wave %in% 1:4 & !is.na(evs_e179_id) ~
    "EVS_E179_vote_choice",
  
  party_source == "EVS" & survey_wave %in% 1:4 & is.na(evs_e179_id) & !is.na(evs_e181_id) ~
    "EVS_E181_party_appeal_fallback",
  
  party_source == "EVS" & survey_wave %in% 1:4 & is.na(evs_e179_id) & is.na(evs_e181_id) & !is.na(evs_e181a_id) ~
    "EVS_E181A_fallback",
  
  party_source == "EVS" & survey_wave == 5 & !is.na(evs_e181a_id) ~
    "EVS5_E181A_party_appeal",
  
  TRUE ~ NA_character_
)

resp_party_label <- dplyr::case_when(
  party_choice_basis == "WVS_E179WVS_vote_choice" ~ wvs_party_label,
  party_choice_basis == "EVS_E179_vote_choice" ~ evs_e179_label,
  party_choice_basis == "EVS_E181_party_appeal_fallback" ~ evs_e181_label,
  party_choice_basis %in% c("EVS_E181A_fallback", "EVS5_E181A_party_appeal") ~ evs_e181a_label,
  TRUE ~ NA_character_
)

# Stop rather than silently guess if substantive party data exist but survey
# source cannot be resolved.
raw_any_party_id <- dplyr::coalesce(
  wvs_party_id,
  evs_e179_id,
  evs_e181_id,
  evs_e181a_id
)

unresolved_source_n <- sum(
  survey_year >= START_YEAR &
    !is.na(raw_any_party_id) &
    (is.na(party_source) | is.na(survey_wave)),
  na.rm = TRUE
)

if (unresolved_source_n > 0) {
  stop(
    unresolved_source_n,
    " post-START_YEAR respondents have a positive party value but unresolved ",
    "WVS/EVS source or wave. Inspect S001/S002/S002evs before proceeding."
  )
}

# ---- 4C. Legacy classification: retained ONLY outside PopuList --------------

fr_codes <- readr::read_csv(
  fr_codes_path,
  show_col_types = FALSE
) %>%
  dplyr::rename_with(tolower)

required_fr_cols <- c("code_type", "code")
if (!all(required_fr_cols %in% names(fr_codes))) {
  stop(
    "far_right_party_codes.csv must contain columns named: ",
    paste(required_fr_cols, collapse = ", ")
  )
}

fr_codes <- fr_codes %>%
  dplyr::transmute(
    code_type = tolower(as.character(.data[["code_type"]])),
    code = as.integer(readr::parse_number(as.character(.data[["code"]])))
  ) %>%
  dplyr::filter(
    !is.na(code),
    code_type %in% c("wvs_id", "evs_id", "party_id", "party_id_2")
  ) %>%
  dplyr::distinct()

fr_wvs_ids <- fr_codes %>%
  dplyr::filter(code_type %in% c("wvs_id", "party_id", "party_id_2")) %>%
  dplyr::pull(code) %>%
  unique()

fr_evs_ids <- fr_codes %>%
  dplyr::filter(code_type %in% c("evs_id", "party_id", "party_id_2")) %>%
  dplyr::pull(code) %>%
  unique()

legacy_far_right_vote_raw <- dplyr::case_when(
  party_source == "WVS" & !is.na(resp_party_id) & resp_party_id %in% fr_wvs_ids ~ TRUE,
  party_source == "EVS" & !is.na(resp_party_id) & resp_party_id %in% fr_evs_ids ~ TRUE,
  party_source %in% c("WVS", "EVS") & !is.na(resp_party_id) ~ FALSE,
  TRUE ~ NA
)

# ---- 4D. Load PopuList 4.0 and define YEAR-SPECIFIC strict far right --------

if (!file.exists(populist_raw_path)) {
  stop("PopuList 4.0 file not found at: ", populist_raw_path)
}

populist_raw <- readr::read_delim(
  populist_raw_path,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
) %>%
  dplyr::rename_with(~ stringr::str_replace_all(tolower(.x), "[^a-z0-9]+", "_"))

required_populist_raw_cols <- c(
  "country_name", "party_name_english", "party_name_short", "partyfacts_id",
  "farright", "farright_bl", "farright_startnobl", "farright_endnobl"
)
if (!all(required_populist_raw_cols %in% names(populist_raw))) {
  stop(
    "PopuList file is missing required columns: ",
    paste(setdiff(required_populist_raw_cols, names(populist_raw)), collapse = ", ")
  )
}

populist_raw <- populist_raw %>%
  dplyr::mutate(
    party_mapping_country_code = suppressWarnings(
      countrycode::countrycode(country_name, origin = "country.name", destination = "iso2c")
    ),
    party_mapping_country_code = dplyr::case_when(
      country_name == "Czech Republic" ~ "CZ",
      country_name == "United Kingdom" ~ "GB",
      TRUE ~ party_mapping_country_code
    ),
    partyfacts_id = suppressWarnings(as.numeric(partyfacts_id)),
    farright = suppressWarnings(as.integer(farright)),
    farright_startnobl = suppressWarnings(as.integer(farright_startnobl)),
    farright_endnobl = suppressWarnings(as.integer(farright_endnobl)),
    party_name_key = normalize_party_name_key(party_name_english)
  )

# PopuList country coverage is COUNTRY-level. Once a country is covered here,
# the legacy crosswalk is never allowed to classify its parties.
populist_country_coverage <- populist_raw %>%
  dplyr::filter(!is.na(party_mapping_country_code)) %>%
  dplyr::distinct(party_mapping_country_code) %>%
  dplyr::mutate(populist_country_covered = TRUE)

# Keep parties that have a real non-borderline far-right period. farright_bl is
# NOT used as a timeless party filter; the survey year is compared to these
# start/end dates. This correctly admits the Finns Party from 2017 onward and
# delays Slovenia's SDS until 2015.
populist_temporal_parties <- populist_raw %>%
  dplyr::filter(
    farright == 1,
    !is.na(farright_startnobl),
    !is.na(farright_endnobl),
    farright_startnobl < 2100,
    farright_endnobl >= farright_startnobl
  ) %>%
  dplyr::select(
    party_mapping_country_code,
    party_name_english,
    party_name_short,
    party_name_key,
    partyfacts_id,
    farright_startnobl,
    farright_endnobl
  ) %>%
  dplyr::distinct()

# ---- 4E. PPC: exact identity + harmonized-label bridge -----------------------
#
# The PPC source files and the IVS harmonized file do NOT always use the same
# numeric category codes. This is especially systematic in EVS5 (2017/18):
# for example IVS 380048 = Brothers of Italy, whereas PPC v174_cs uses 38014.
# We therefore use two PPC matching routes:
#
#   A. exact numeric match where the coding systems coincide;
#   B. a conservative label bridge within the SAME country × source × wave ×
#      survey year when the numeric match fails.
#
# The label bridge uses, in order: exact normalized label, a small reviewed
# alias table, unique prefix/truncation matching, and unique PartyFacts-name
# containment. It never matches across countries, survey programs, waves, or
# years. All bridge decisions are exported for audit.

if (!file.exists(ppc_path)) {
  stop("Political Parties Crosswalk file not found at: ", ppc_path)
}

ppc_raw <- readxl::read_excel(
  ppc_path,
  sheet = "variables"
) %>%
  dplyr::rename_with(~ stringr::str_replace_all(tolower(.x), "[^a-z0-9]+", "_"))

required_ppc_cols <- c(
  "proj", "wave", "ctry", "yr", "var", "value_code",
  "merged_value_label", "pfid", "merged_type", "pf_english_name"
)
if (!all(required_ppc_cols %in% names(ppc_raw))) {
  stop(
    "PPC file is missing required columns: ",
    paste(setdiff(required_ppc_cols, names(ppc_raw)), collapse = ", ")
  )
}

# Candidate PPC categories corresponding to the party-choice variable used in
# each source/wave. Keep the PPC *source* value code separate from the IVS code.
ppc_source_candidates <- ppc_raw %>%
  dplyr::mutate(
    proj = toupper(as.character(proj)),
    survey_wave = suppressWarnings(as.integer(wave)),
    year = suppressWarnings(as.integer(yr)),
    ppc_var = as.character(var),
    party_mapping_country_code = normalize_party_mapping_country(ctry),
    ppc_source_party_id = suppressWarnings(as.numeric(value_code)),
    ppc_partyfacts_id = suppressWarnings(as.numeric(pfid)),
    ppc_type = tolower(as.character(merged_type)),
    ppc_label = as.character(merged_value_label),
    ppc_party_name = as.character(pf_english_name),
    ppc_label_key = normalize_party_bridge_label(ppc_label),
    ppc_valid_party = ppc_type %in% c("party", "indep")
  ) %>%
  dplyr::filter(
    proj %in% c("WVS", "EVS"),
    !is.na(party_mapping_country_code),
    !is.na(survey_wave),
    !is.na(year),
    !is.na(ppc_source_party_id),
    (
      proj == "WVS" & ppc_var == "E179WVS"
    ) |
      (
        proj == "EVS" & survey_wave %in% 1:4 & ppc_var == "E179"
      ) |
      (
        proj == "EVS" & survey_wave == 5 & tolower(ppc_var) == "v174_cs"
      )
  ) %>%
  dplyr::transmute(
    party_mapping_country_code,
    party_source = proj,
    survey_wave,
    year,
    ppc_source_party_id,
    ppc_label,
    ppc_label_key,
    ppc_partyfacts_id,
    ppc_type,
    ppc_party_name,
    ppc_valid_party
  ) %>%
  dplyr::distinct()

# Exact numeric lookup. These are PPC source-file IDs, so this will work only
# where IVS retained the same category code.
ppc_exact_lookup <- ppc_source_candidates %>%
  dplyr::rename(resp_party_id = ppc_source_party_id) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id
  ) %>%
  dplyr::summarise(
    ppc_exact_label = paste(unique(ppc_label[!is.na(ppc_label)]), collapse = " | "),
    ppc_exact_partyfacts_id = dplyr::first(
      ppc_partyfacts_id[!is.na(ppc_partyfacts_id)],
      default = NA_real_
    ),
    ppc_exact_type = paste(unique(ppc_type[!is.na(ppc_type)]), collapse = " | "),
    ppc_exact_party_name = paste(unique(ppc_party_name[!is.na(ppc_party_name)]), collapse = " | "),
    ppc_exact_valid_party = any(ppc_valid_party),
    .groups = "drop"
  )

ppc_wave_coverage <- ppc_source_candidates %>%
  dplyr::distinct(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year
  ) %>%
  dplyr::mutate(ppc_wave_available = TRUE)

# Build a catalogue of the party categories actually observed in IVS.
observed_party_catalog <- tibble::tibble(
  party_mapping_country_code = normalize_party_mapping_country(
    as.character(col_or_na(ivs, "S009", mode = "character"))
  ),
  party_source = party_source,
  survey_wave = survey_wave,
  year = survey_year,
  resp_party_id = resp_party_id,
  resp_party_label = resp_party_label
) %>%
  dplyr::filter(
    year >= START_YEAR,
    !is.na(party_mapping_country_code),
    !is.na(party_source),
    !is.na(survey_wave),
    !is.na(resp_party_id)
  ) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    ivs_label_key = normalize_party_bridge_label(resp_party_label),
    ivs_label_is_nonparty = is_nonparty_label(resp_party_label)
  )

# Attempt the label bridge for ALL observed categories, not only numerically
# unmatched categories. This is intentional: harmonization can create a
# dangerous *false exact* numeric match where an IVS code happens to equal a
# different PPC source category in the same wave. When a conservative label
# bridge succeeds, its identity takes precedence over the raw numeric join.
observed_party_for_bridge <- observed_party_catalog

# Reviewed aliases for the handful of EVS5 harmonized labels whose wording is
# genuinely different from PPC rather than merely recoded/truncated. Keeping
# them explicit prevents broad fuzzy matching from silently creating errors.
PPC_LABEL_BRIDGE_ALIASES <- tibble::tribble(
  ~party_mapping_country_code, ~party_source, ~survey_wave, ~year, ~ivs_label_raw, ~ppc_source_party_id, ~bridge_alias_reason,
  "AT", "EVS", 5L, 2018L,
  "AT: FPÖ",
  4003,
  "IVS EVS5 harmonizes FPÖ as code 40003 / short label AT: FPÖ; PPC source uses 4003 Freedom Party of Austria",
  "BG", "EVS", 5L, 2017L,
  "BG: Balgarska Socialisticheska Partiya (BSP) - Bulgarian Socialist Party",
  10002,
  "IVS uses Bulgarian+English label; PPC uses English Bulgarian Socialist Party label",
  "BG", "EVS", 5L, 2017L,
  "BG: Dvijenie za Prava i Svobodi (DPS) - Movement for Rights and Freedom",
  10003,
  "IVS uses Bulgarian+English label and singular Freedom; PPC uses English plural Freedoms",
  "BG", "EVS", 5L, 2017L,
  "BG: Democrats for Strong Bulgaria",
  10006,
  "PPC label is Democrats for a Strong Bulgaria",
  "BG", "EVS", 5L, 2017L,
  "BG: Green Party of Bulgaria",
  10013,
  "PPC label is Party The Greens",
  "SE", "EVS", 5L, 2017L,
  "SE: Liberals",
  75205,
  "PPC source label is People's party; PartyFacts identity is the Liberals"
) %>%
  dplyr::mutate(ivs_label_key = normalize_party_bridge_label(ivs_label_raw))

# Exact normalized-label matches.
ppc_bridge_exact_label <- observed_party_for_bridge %>%
  dplyr::filter(!ivs_label_is_nonparty, ivs_label_key != "") %>%
  dplyr::inner_join(
    ppc_source_candidates,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "ivs_label_key" = "ppc_label_key"
    )
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id,
    resp_party_label,
    ivs_label_key
  ) %>%
  dplyr::filter(dplyr::n_distinct(ppc_source_party_id) == 1L) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    bridge_priority = 1L,
    ppc_bridge_method = "exact_normalized_label",
    ppc_bridge_reason = NA_character_
  )

# Explicit reviewed aliases.
ppc_bridge_manual_alias <- observed_party_for_bridge %>%
  dplyr::filter(!ivs_label_is_nonparty) %>%
  dplyr::inner_join(
    PPC_LABEL_BRIDGE_ALIASES,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "ivs_label_key"
    )
  ) %>%
  dplyr::inner_join(
    ppc_source_candidates,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "ppc_source_party_id"
    )
  ) %>%
  dplyr::mutate(
    bridge_priority = 2L,
    ppc_bridge_method = "reviewed_label_alias",
    ppc_bridge_reason = bridge_alias_reason
  )

# Unique truncation/prefix match. This handles labels truncated by the IVS
# harmonization (e.g. Czech KSČM/SPD labels) without general fuzzy matching.
ppc_bridge_prefix <- observed_party_for_bridge %>%
  dplyr::filter(!ivs_label_is_nonparty, nchar(ivs_label_key) >= 12L) %>%
  dplyr::inner_join(
    ppc_source_candidates %>% dplyr::filter(nchar(ppc_label_key) >= 12L),
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year"
    )
  ) %>%
  dplyr::filter(
    startsWith(ivs_label_key, ppc_label_key) |
      startsWith(ppc_label_key, ivs_label_key)
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id,
    resp_party_label,
    ivs_label_key
  ) %>%
  dplyr::filter(dplyr::n_distinct(ppc_source_party_id) == 1L) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    bridge_priority = 3L,
    ppc_bridge_method = "unique_prefix_or_truncation",
    ppc_bridge_reason = NA_character_
  )

# A final conservative route uses PartyFacts English-name aliases only when a
# single PPC category in the same survey cell is compatible. Generic values
# such as "other" and "unknown" are excluded.
ppc_party_name_aliases <- ppc_source_candidates %>%
  dplyr::filter(!is.na(ppc_party_name), ppc_party_name != "") %>%
  dplyr::mutate(ppc_party_name_alias_raw = ppc_party_name) %>%
  tidyr::separate_rows(ppc_party_name_alias_raw, sep = "\\s*/\\s*") %>%
  dplyr::mutate(
    ppc_party_name_alias_key = normalize_party_bridge_label(ppc_party_name_alias_raw),
    ppc_party_name_alias_key = stringr::str_remove(ppc_party_name_alias_key, "^the\\s+")
  ) %>%
  dplyr::filter(
    nchar(ppc_party_name_alias_key) >= 8L,
    !ppc_party_name_alias_key %in% c("unknown", "other")
  )

ppc_bridge_partyfacts_name <- observed_party_for_bridge %>%
  dplyr::filter(!ivs_label_is_nonparty, nchar(ivs_label_key) >= 8L) %>%
  dplyr::inner_join(
    ppc_party_name_aliases,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year"
    )
  ) %>%
  dplyr::filter(
    stringr::str_detect(ivs_label_key, stringr::fixed(ppc_party_name_alias_key)) |
      stringr::str_detect(ppc_party_name_alias_key, stringr::fixed(ivs_label_key))
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id,
    resp_party_label,
    ivs_label_key
  ) %>%
  dplyr::filter(dplyr::n_distinct(ppc_source_party_id) == 1L) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    bridge_priority = 4L,
    ppc_bridge_method = "unique_partyfacts_name_containment",
    ppc_bridge_reason = NA_character_
  )

# Select one bridge per observed IVS category by the conservative priority
# order above. The bridge stores BOTH the IVS ID and PPC source ID so the code
# systems remain explicit rather than being conflated.
ppc_label_bridge <- dplyr::bind_rows(
  ppc_bridge_exact_label,
  ppc_bridge_manual_alias,
  ppc_bridge_prefix,
  ppc_bridge_partyfacts_name
) %>%
  dplyr::arrange(
    bridge_priority,
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id,
    resp_party_label
  ) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id,
    resp_party_label,
    ppc_bridge_source_party_id = ppc_source_party_id,
    ppc_bridge_label = ppc_label,
    ppc_bridge_partyfacts_id = ppc_partyfacts_id,
    ppc_bridge_type = ppc_type,
    ppc_bridge_party_name = ppc_party_name,
    ppc_bridge_valid_party = ppc_valid_party,
    ppc_bridge_method,
    ppc_bridge_reason
  )

# Export the bridge itself and unresolved positive labels. Unmatched rows are
# NOT automatically discarded downstream: clearly non-party labels are
# excluded, while party-looking labels are retained with an explicit audit flag.
ppc_label_bridge_audit <- observed_party_catalog %>%
  dplyr::left_join(
    ppc_exact_lookup %>%
      dplyr::select(
        party_mapping_country_code,
        party_source,
        survey_wave,
        year,
        resp_party_id,
        ppc_exact_label,
        ppc_exact_partyfacts_id,
        ppc_exact_type,
        ppc_exact_valid_party
      ),
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "resp_party_id"
    )
  ) %>%
  dplyr::left_join(
    ppc_label_bridge,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "resp_party_id",
      "resp_party_label"
    )
  ) %>%
  dplyr::mutate(
    ppc_exact_matched = !is.na(ppc_exact_type) | !is.na(ppc_exact_label),
    ppc_bridge_matched = !is.na(ppc_bridge_type) | !is.na(ppc_bridge_label),
    ppc_any_match = ppc_exact_matched | ppc_bridge_matched,
    ppc_numeric_label_conflict =
      ppc_exact_matched &
      ppc_bridge_matched &
      !is.na(ppc_bridge_source_party_id) &
      resp_party_id != ppc_bridge_source_party_id,
    ppc_match_route = dplyr::case_when(
      ppc_bridge_matched & ppc_numeric_label_conflict ~ paste0(ppc_bridge_method, ":overrides_conflicting_numeric_id"),
      ppc_bridge_matched ~ ppc_bridge_method,
      ppc_exact_matched ~ "exact_numeric_id_no_label_bridge",
      ivs_label_is_nonparty ~ "unmatched_but_nonparty_label",
      TRUE ~ "unmatched_party_label_retained"
    )
  )

readr::write_csv(
  ppc_label_bridge_audit,
  file.path(out_dir_csv, "ppc_ivsharmonized_label_bridge_audit.csv")
)

# Numeric-ID collisions exposed by a successful label bridge. These are exactly
# the cases where blindly trusting PPC source IDs would classify the wrong
# party/category despite matching country, source, wave, and year.
ppc_numeric_label_conflict_audit <- ppc_label_bridge_audit %>%
  dplyr::filter(ppc_numeric_label_conflict) %>%
  dplyr::arrange(
    party_mapping_country_code, party_source, survey_wave, year, resp_party_id
  )

readr::write_csv(
  ppc_numeric_label_conflict_audit,
  file.path(out_dir_csv, "ppc_numeric_id_conflicts_resolved_by_label_bridge.csv")
)

ppc_unresolved_label_bridge <- ppc_label_bridge_audit %>%
  dplyr::filter(
    !ppc_any_match,
    !ivs_label_is_nonparty
  ) %>%
  dplyr::arrange(
    party_mapping_country_code,
    party_source,
    survey_wave,
    year,
    resp_party_id
  )

readr::write_csv(
  ppc_unresolved_label_bridge,
  file.path(out_dir_csv, "ppc_unresolved_party_labels_after_bridge.csv")
)

if (nrow(ppc_unresolved_label_bridge) > 0) {
  warning(
    nrow(ppc_unresolved_label_bridge),
    " observed positive party categories remain unmatched to PPC after the ",
    "label bridge. They are retained as valid party choices unless their label ",
    "is explicitly non-party. Review ppc_unresolved_party_labels_after_bridge.csv."
  )
}

# PPC is a validation/identity layer. The manually reviewed PopuList crosswalk
# remains the authoritative far-right mapping for PopuList countries.

# ---- 4F. Reviewed PopuList mapping: PRIMARY authoritative party mapping -----

if (!file.exists(populist_crosswalk_path)) {
  stop("Reviewed PopuList crosswalk not found at: ", populist_crosswalk_path)
}

reviewed_crosswalk <- readr::read_csv(
  populist_crosswalk_path,
  show_col_types = FALSE
) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(
    party_mapping_country_code = suppressWarnings(
      countrycode::countrycode(country, origin = "country.name", destination = "iso2c")
    ),
    party_mapping_country_code = dplyr::case_when(
      country == "Czech Republic" ~ "CZ",
      country == "United Kingdom" ~ "GB",
      TRUE ~ party_mapping_country_code
    ),
    reviewed_partyfacts_id = suppressWarnings(as.numeric(populist_partyfacts_id)),
    reviewed_name_key = normalize_party_name_key(party_name_english)
  )

# Attach the temporal PopuList interval by Party Facts ID first, with a
# country+name fallback for rare rows lacking Party Facts ID.
reviewed_temporal_base <- reviewed_crosswalk %>%
  dplyr::left_join(
    populist_temporal_parties %>%
      dplyr::filter(!is.na(partyfacts_id)) %>%
      dplyr::select(
        party_mapping_country_code,
        partyfacts_id,
        farright_startnobl,
        farright_endnobl,
        temporal_party_name = party_name_english,
        temporal_party_abbrev = party_name_short
      ) %>%
      dplyr::rename(reviewed_partyfacts_id = partyfacts_id),
    by = c("party_mapping_country_code", "reviewed_partyfacts_id")
  )

name_temporal_lookup <- populist_temporal_parties %>%
  dplyr::select(
    party_mapping_country_code,
    reviewed_name_key = party_name_key,
    name_start = farright_startnobl,
    name_end = farright_endnobl,
    name_party = party_name_english,
    name_abbrev = party_name_short
  ) %>%
  dplyr::distinct()

reviewed_temporal_base <- reviewed_temporal_base %>%
  dplyr::left_join(
    name_temporal_lookup,
    by = c("party_mapping_country_code", "reviewed_name_key")
  ) %>%
  dplyr::mutate(
    reviewed_fr_start = dplyr::coalesce(farright_startnobl, name_start),
    reviewed_fr_end = dplyr::coalesce(farright_endnobl, name_end),
    reviewed_temporal_party_name = dplyr::coalesce(temporal_party_name, name_party, party_name_english),
    reviewed_temporal_party_abbrev = dplyr::coalesce(temporal_party_abbrev, name_abbrev, party_abbreviation)
  )


# PartyFacts-ID representation of the SAME manually reviewed crosswalk. This is
# crucial for harmonized IVS categories whose numeric ID differs from the PPC
# source ID: once PPC identifies the PartyFacts identity via the label bridge,
# the party can still be checked against the reviewed PopuList mapping without
# relying on equality of the two numeric code systems.
reviewed_partyfacts_temporal_lookup <- reviewed_temporal_base %>%
  dplyr::filter(
    !is.na(party_mapping_country_code),
    !is.na(reviewed_partyfacts_id),
    !is.na(reviewed_fr_start),
    !is.na(reviewed_fr_end)
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    reviewed_partyfacts_id
  ) %>%
  dplyr::summarise(
    reviewed_pf_party_name = paste(
      unique(reviewed_temporal_party_name[!is.na(reviewed_temporal_party_name)]),
      collapse = " | "
    ),
    reviewed_pf_party_abbreviation = paste(
      unique(reviewed_temporal_party_abbrev[!is.na(reviewed_temporal_party_abbrev)]),
      collapse = " | "
    ),
    reviewed_pf_fr_start = suppressWarnings(min(reviewed_fr_start, na.rm = TRUE)),
    reviewed_pf_fr_end = suppressWarnings(max(reviewed_fr_end, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    reviewed_pf_fr_start = dplyr::if_else(
      is.infinite(reviewed_pf_fr_start), NA_real_, reviewed_pf_fr_start
    ),
    reviewed_pf_fr_end = dplyr::if_else(
      is.infinite(reviewed_pf_fr_end), NA_real_, reviewed_pf_fr_end
    )
  ) %>%
  dplyr::rename(ppc_partyfacts_id = reviewed_partyfacts_id)

reviewed_mapping_specs <- dplyr::bind_rows(
  tibble::tibble(
    party_source = "WVS",
    survey_wave = 3:7,
    id_col = paste0("wvs", 3:7, "_id"),
    label_col = paste0("wvs", 3:7, "_label"),
    status_col = paste0("wvs", 3:7, "_status")
  ),
  tibble::tibble(
    party_source = "EVS",
    survey_wave = 1:5,
    id_col = paste0("evs", 1:5, "_id"),
    label_col = paste0("evs", 1:5, "_label"),
    status_col = paste0("evs", 1:5, "_status")
  )
)

reviewed_fr_lookup <- purrr::map_dfr(
  seq_len(nrow(reviewed_mapping_specs)),
  function(i) {
    spec <- reviewed_mapping_specs[i, ]
    id_col <- spec$id_col[[1]]
    label_col <- spec$label_col[[1]]
    status_col <- spec$status_col[[1]]
    
    if (!id_col %in% names(reviewed_temporal_base)) {
      return(tibble::tibble())
    }
    
    reviewed_temporal_base %>%
      dplyr::transmute(
        party_mapping_country_code,
        party_source = spec$party_source[[1]],
        survey_wave = as.integer(spec$survey_wave[[1]]),
        mapping_id_raw = as.character(.data[[id_col]]),
        reviewed_party_name = reviewed_temporal_party_name,
        reviewed_party_abbreviation = reviewed_temporal_party_abbrev,
        reviewed_fr_start,
        reviewed_fr_end,
        reviewed_survey_label = if (label_col %in% names(reviewed_temporal_base)) {
          as.character(.data[[label_col]])
        } else {
          NA_character_
        },
        reviewed_mapping_status = if (status_col %in% names(reviewed_temporal_base)) {
          as.character(.data[[status_col]])
        } else {
          NA_character_
        }
      ) %>%
      dplyr::filter(
        !is.na(party_mapping_country_code),
        !is.na(mapping_id_raw),
        stringr::str_trim(mapping_id_raw) != "",
        !is.na(reviewed_fr_start),
        !is.na(reviewed_fr_end)
      ) %>%
      tidyr::separate_rows(mapping_id_raw, sep = "\\s*\\|\\s*") %>%
      dplyr::mutate(
        resp_party_id = suppressWarnings(as.numeric(stringr::str_trim(as.character(mapping_id_raw))))
      ) %>%
      dplyr::filter(!is.na(resp_party_id)) %>%
      dplyr::select(-mapping_id_raw)
  }
) %>%
  dplyr::group_by(
    party_mapping_country_code,
    party_source,
    survey_wave,
    resp_party_id
  ) %>%
  dplyr::summarise(
    reviewed_party_name = paste(unique(reviewed_party_name[!is.na(reviewed_party_name)]), collapse = " | "),
    reviewed_party_abbreviation = paste(unique(reviewed_party_abbreviation[!is.na(reviewed_party_abbreviation)]), collapse = " | "),
    reviewed_fr_start = suppressWarnings(min(reviewed_fr_start, na.rm = TRUE)),
    reviewed_fr_end = suppressWarnings(max(reviewed_fr_end, na.rm = TRUE)),
    reviewed_survey_label = paste(unique(reviewed_survey_label[!is.na(reviewed_survey_label)]), collapse = " | "),
    reviewed_mapping_status = paste(unique(reviewed_mapping_status[!is.na(reviewed_mapping_status)]), collapse = " | "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    reviewed_fr_start = dplyr::if_else(is.infinite(reviewed_fr_start), NA_real_, reviewed_fr_start),
    reviewed_fr_end = dplyr::if_else(is.infinite(reviewed_fr_end), NA_real_, reviewed_fr_end)
  )

# ---- 4G. Temporal-borderline supplement --------------------------------------
#
# The original reviewed CSV was built from the party-level rule
# farright == 1 & farright_bl == 0. That omitted parties whose global/current
# flag is borderline even though PopuList records a real non-borderline period.
# PopuList 4.0 contains three such parties with a strict interval:
#   - Finland: Finns Party, strict from 2017
#   - Poland: Law and Justice, strict from 2015
#   - Croatia: Bridge, strict from 2020
#
# Represent this supplement by PartyFacts ID rather than a survey numeric ID so
# it works even when IVS harmonization changed the category code.

temporal_borderline_parties <- populist_raw %>%
  dplyr::filter(
    farright == 1,
    suppressWarnings(as.integer(farright_bl)) == 1,
    !is.na(farright_startnobl),
    !is.na(farright_endnobl),
    farright_startnobl < 2100,
    farright_endnobl >= farright_startnobl,
    !is.na(partyfacts_id)
  ) %>%
  dplyr::transmute(
    party_mapping_country_code,
    ppc_partyfacts_id = partyfacts_id,
    supplemental_party_name = party_name_english,
    supplemental_party_abbreviation = party_name_short,
    supplemental_fr_start = farright_startnobl,
    supplemental_fr_end = farright_endnobl
  ) %>%
  dplyr::distinct()

# Export the mapping layers so they can be inspected independently of the
# respondent-level results.
readr::write_csv(
  reviewed_fr_lookup,
  file.path(out_dir_csv, "reviewed_populist_temporal_lookup.csv")
)
readr::write_csv(
  reviewed_partyfacts_temporal_lookup,
  file.path(out_dir_csv, "reviewed_populist_partyfacts_temporal_lookup.csv")
)
readr::write_csv(
  temporal_borderline_parties,
  file.path(out_dir_csv, "temporal_borderline_populist_supplement.csv")
)


# =============================================================================
# 5. CLEAN IDEOLOGY AND POLICY VARIABLES
# =============================================================================

# Ideology: valid responses only, 1-10.
#
# The cleaned E033 score is created once here. Ideological groups are NOT
# assigned yet; downstream analyses assign them separately under both requested
# ideology definitions.
E033_num <- clean_scale(
  col_or_na(ivs, "E033"),
  min_value = 1,
  max_value = 10
)

ideology_category <- factor(
  rep(NA_character_, length(E033_num)),
  levels = IDEOLOGY_LEVELS
)

# Survey weight. Official WVS time-series examples use S017. We keep it in the
# canonical microdata even when USE_SURVEY_WEIGHTS is FALSE so weighted and
# unweighted outputs can be compared without rebuilding the import.
survey_weight <- clean_weight(
  col_or_na(ivs, SURVEY_WEIGHT_VAR)
)

if (USE_SURVEY_WEIGHTS && all(is.na(survey_weight))) {
  stop(
    "USE_SURVEY_WEIGHTS is TRUE, but no positive values were found in ",
    SURVEY_WEIGHT_VAR,
    ". Check the IVS weight variable before proceeding."
  )
}

# Clean every policy variable according to the central POLICY_SPECS table.
policy_clean <- purrr::map_dfc(
  seq_len(nrow(POLICY_SPECS)),
  function(i) {
    spec <- POLICY_SPECS[i, ]
    
    cleaned <- clean_scale(
      col_or_na(ivs, spec$var),
      min_value = spec$min_value,
      max_value = spec$max_value
    )
    
    tibble::tibble(!!spec$var := cleaned)
  }
)


# =============================================================================
# 6. BUILD THE ONE CANONICAL RESPONDENT-LEVEL DATASET
# =============================================================================

S009_raw <- as.character(col_or_na(ivs, "S009", mode = "character"))

analysis_micro <- tibble::tibble(
  respondent_id = seq_len(nrow(ivs)),
  
  S009_raw = S009_raw,
  S009_code = normalize_s009(S009_raw),
  party_mapping_country_code = normalize_party_mapping_country(S009_raw),
  year = survey_year,
  survey_wave = survey_wave,
  study_label = study_label,
  
  E033_num = E033_num,
  ideology_category = ideology_category,
  is_moderate = !is.na(E033_num) & E033_num %in% c(5, 6),
  
  party_source = party_source,
  party_choice_basis = party_choice_basis,
  resp_party_id = resp_party_id,
  resp_party_label = resp_party_label,
  resp_party_label_key = normalize_party_bridge_label(resp_party_label),
  raw_label_is_nonparty = is_nonparty_label(resp_party_label),
  
  legacy_far_right_vote_raw = legacy_far_right_vote_raw,
  survey_weight = survey_weight
) %>%
  dplyr::mutate(
    country_label = make_country_labels(S009_code),
    analysis_group = factor(
      make_analysis_group(S009_code),
      levels = ANALYSIS_GROUP_LEVELS
    )
  ) %>%
  dplyr::left_join(
    populist_country_coverage,
    by = "party_mapping_country_code"
  ) %>%
  dplyr::left_join(
    ppc_wave_coverage,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year"
    )
  ) %>%
  dplyr::left_join(
    ppc_exact_lookup,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "resp_party_id"
    )
  ) %>%
  dplyr::left_join(
    ppc_label_bridge,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "year",
      "resp_party_id",
      "resp_party_label"
    )
  ) %>%
  dplyr::mutate(
    populist_country_covered = dplyr::coalesce(populist_country_covered, FALSE),
    ppc_wave_available = dplyr::coalesce(ppc_wave_available, FALSE),
    
    ppc_exact_id_matched = !is.na(ppc_exact_type) | !is.na(ppc_exact_label),
    ppc_label_bridge_matched = !is.na(ppc_bridge_type) | !is.na(ppc_bridge_label),
    ppc_id_matched = ppc_exact_id_matched | ppc_label_bridge_matched,
    
    # Harmonized final PPC identity. A successful conservative label bridge takes
    # precedence because IVS harmonization can create misleading numeric matches;
    # the exact numeric PPC match is used when no bridge is available.
    ppc_label = dplyr::coalesce(ppc_bridge_label, ppc_exact_label),
    ppc_partyfacts_id = dplyr::coalesce(
      ppc_bridge_partyfacts_id,
      ppc_exact_partyfacts_id
    ),
    ppc_type = dplyr::coalesce(ppc_bridge_type, ppc_exact_type),
    ppc_party_name = dplyr::coalesce(
      ppc_bridge_party_name,
      ppc_exact_party_name
    ),
    ppc_valid_party = dplyr::case_when(
      ppc_label_bridge_matched ~ dplyr::coalesce(ppc_bridge_valid_party, FALSE),
      ppc_exact_id_matched ~ dplyr::coalesce(ppc_exact_valid_party, FALSE),
      TRUE ~ NA
    ),
    ppc_numeric_label_conflict =
      ppc_exact_id_matched &
      ppc_label_bridge_matched &
      !is.na(ppc_bridge_source_party_id) &
      resp_party_id != ppc_bridge_source_party_id,
    ppc_match_route = dplyr::case_when(
      ppc_label_bridge_matched & ppc_numeric_label_conflict ~ paste0(ppc_bridge_method, ":overrides_conflicting_numeric_id"),
      ppc_label_bridge_matched ~ ppc_bridge_method,
      ppc_exact_id_matched ~ "exact_numeric_id_no_label_bridge",
      ppc_wave_available ~ "PPC_unmatched_after_bridge",
      TRUE ~ "PPC_not_available"
    ),
    
    # PPC is used positively where it can identify a category, but an unmatched
    # positive ID is NOT automatically discarded. That was the source of the
    # 2017/18 false-negative problem. A clearly non-party label is always
    # excluded; otherwise an unmatched positive label remains a valid party
    # choice and is surfaced for manual audit.
    party_choice_valid = dplyr::case_when(
      is.na(resp_party_id) | is.na(party_source) ~ FALSE,
      raw_label_is_nonparty ~ FALSE,
      ppc_id_matched ~ dplyr::coalesce(ppc_valid_party, FALSE),
      TRUE ~ TRUE
    ),
    
    party_validation_source = dplyr::case_when(
      is.na(resp_party_id) | is.na(party_source) ~ "no_party_value",
      raw_label_is_nonparty & ppc_id_matched ~ "nonparty_label_and_PPC_excluded",
      raw_label_is_nonparty ~ "label_nonparty_excluded",
      ppc_label_bridge_matched & party_choice_valid ~ paste0("PPC_bridge_valid_party:", ppc_match_route),
      ppc_label_bridge_matched & !party_choice_valid ~ paste0("PPC_bridge_nonparty_excluded:", ppc_match_route),
      ppc_exact_id_matched & party_choice_valid ~ "PPC_exact_valid_party_no_label_bridge",
      ppc_exact_id_matched & !party_choice_valid ~ "PPC_exact_nonparty_excluded_no_label_bridge",
      ppc_wave_available & party_choice_valid ~ "PPC_unmatched_label_validated_party",
      !ppc_wave_available & party_choice_valid ~ "label_validated_no_PPC",
      TRUE ~ "invalid_or_nonparty"
    )
  ) %>%
  # PartyFacts representation of the manually reviewed PopuList crosswalk.
  # This is what rescues harmonized EVS5 categories such as FdI 380048 -> PPC
  # 38014 -> PartyFacts 2280 without assuming the numeric IDs are equivalent.
  dplyr::left_join(
    reviewed_partyfacts_temporal_lookup,
    by = c(
      "party_mapping_country_code",
      "ppc_partyfacts_id"
    )
  ) %>%
  dplyr::left_join(
    temporal_borderline_parties,
    by = c(
      "party_mapping_country_code",
      "ppc_partyfacts_id"
    )
  ) %>%
  # Retain the manually reviewed survey-ID route as well. It remains important
  # for waves such as WVS7 where PPC may not provide an identity.
  dplyr::left_join(
    reviewed_fr_lookup,
    by = c(
      "party_mapping_country_code",
      "party_source",
      "survey_wave",
      "resp_party_id"
    )
  ) %>%
  dplyr::mutate(
    reviewed_temporal_match =
      !is.na(reviewed_party_name) &
      !is.na(reviewed_fr_start) &
      !is.na(reviewed_fr_end) &
      year >= reviewed_fr_start &
      year <= reviewed_fr_end,
    
    reviewed_partyfacts_temporal_match =
      !is.na(reviewed_pf_party_name) &
      !is.na(reviewed_pf_fr_start) &
      !is.na(reviewed_pf_fr_end) &
      year >= reviewed_pf_fr_start &
      year <= reviewed_pf_fr_end,
    
    supplemental_temporal_match =
      !is.na(supplemental_party_name) &
      !is.na(supplemental_fr_start) &
      !is.na(supplemental_fr_end) &
      year >= supplemental_fr_start &
      year <= supplemental_fr_end,
    
    # PopuList is authoritative at the COUNTRY level. Inside a PopuList country
    # the legacy global-ID list is never consulted. A party can become a strict
    # far-right match via either the reviewed survey-ID crosswalk OR the same
    # reviewed party's PartyFacts identity recovered through PPC/label bridging.
    far_right_vote_pre_override = dplyr::case_when(
      !party_choice_valid ~ NA,
      populist_country_covered & reviewed_temporal_match ~ TRUE,
      populist_country_covered & reviewed_partyfacts_temporal_match ~ TRUE,
      populist_country_covered & supplemental_temporal_match ~ TRUE,
      populist_country_covered ~ FALSE,
      TRUE ~ legacy_far_right_vote_raw
    ),
    
    fr_classification_source_pre_override = dplyr::case_when(
      !party_choice_valid ~ "invalid_or_nonparty_excluded",
      populist_country_covered & reviewed_temporal_match ~
        "PopuList_temporal_reviewed_crosswalk_id",
      populist_country_covered & reviewed_partyfacts_temporal_match & ppc_label_bridge_matched ~
        "PopuList_temporal_reviewed_PartyFacts_via_PPC_bridge",
      populist_country_covered & reviewed_partyfacts_temporal_match ~
        "PopuList_temporal_reviewed_PartyFacts",
      populist_country_covered & supplemental_temporal_match & ppc_label_bridge_matched ~
        "PopuList_temporal_borderline_PartyFacts_via_PPC_bridge",
      populist_country_covered & supplemental_temporal_match ~
        "PopuList_temporal_borderline_PartyFacts",
      populist_country_covered ~ "PopuList_country_not_strict_far_right_this_year",
      TRUE ~ "legacy_outside_PopuList"
    ),
    
    matched_populist_party = dplyr::case_when(
      reviewed_temporal_match ~ reviewed_party_name,
      reviewed_partyfacts_temporal_match ~ reviewed_pf_party_name,
      supplemental_temporal_match ~ supplemental_party_name,
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::left_join(
    FR_MANUAL_OVERRIDES,
    by = c(
      "party_mapping_country_code",
      "year",
      "party_source",
      "resp_party_id"
    )
  ) %>%
  dplyr::mutate(
    # An override can only act on a valid party choice. This prevents an
    # accidental positive override from resurrecting a non-party response.
    far_right_vote = dplyr::case_when(
      !party_choice_valid ~ NA,
      !is.na(far_right_override) ~ far_right_override,
      TRUE ~ far_right_vote_pre_override
    ),
    fr_classification_source = dplyr::case_when(
      !party_choice_valid ~ "invalid_or_nonparty_excluded",
      !is.na(far_right_override) ~ "manual_reviewed_override",
      TRUE ~ fr_classification_source_pre_override
    )
  ) %>%
  dplyr::bind_cols(policy_clean)

# =============================================================================
# REVIEWED PARTY-IDENTITY × TIME OVERRIDES
# =============================================================================
#
# These rules correct substantive classifications in countries outside
# PopuList. They are deliberately based on party identity rather than raw
# survey IDs because numeric party IDs can be reused or change across waves.
#
# Rules:
#
# Serbia
#   - Socialist Party of Serbia:
#       NEVER far right
#   - Serbian Progressive Party:
#       NEVER far right
#
# Ukraine
#   - All-Ukrainian Union "Fatherland":
#       NEVER far right
#   - Radical Party of Oleh Lyashko:
#       NEVER far right
#
# United States
#   - Republican Party:
#       NOT far right through 2011
#       far right only after 2011
#
# Montenegro
#   - Socialist People's Party:
#       NEVER far right
#
# South Africa
#   - Economic Freedom Fighters:
#       NEVER far right
#
# ppc_partyfacts_id is used when available. Party labels provide a fallback
# for waves in which PPC / PartyFacts identity is unavailable, including some
# recent WVS waves.

analysis_micro <- analysis_micro %>%
  dplyr::mutate(
    
    # Construct a normalized party-identity string for reviewed overrides.
    # Prefer the standardized PPC/PartyFacts party name when available;
    # otherwise fall back to the IVS survey label.
    reviewed_party_identity = stringr::str_to_lower(
      stringr::str_squish(
        dplyr::coalesce(
          ppc_party_name,
          resp_party_label,
          ""
        )
      )
    ),
    
    # Identify the particular reviewed parties.
    is_sps_serbia =
      party_mapping_country_code == "RS" &
      (
        dplyr::coalesce(ppc_partyfacts_id == 2178, FALSE) |
          stringr::str_detect(
            reviewed_party_identity,
            "socialist party of serbia"
          )
      ),
    
    is_sns_serbia =
      party_mapping_country_code == "RS" &
      (
        dplyr::coalesce(ppc_partyfacts_id == 3177, FALSE) |
          stringr::str_detect(
            reviewed_party_identity,
            "serbian progressive party"
          )
      ),
    
    is_fatherland_ukraine =
      party_mapping_country_code == "UA" &
      (
        dplyr::coalesce(ppc_partyfacts_id == 3266, FALSE) |
          stringr::str_detect(
            reviewed_party_identity,
            "fatherland"
          )
      ),
    
    is_lyashko_ukraine =
      party_mapping_country_code == "UA" &
      stringr::str_detect(
        reviewed_party_identity,
        "radical party of oleh lyashko"
      ),
    
    is_republican_us =
      party_mapping_country_code == "US" &
      stringr::str_detect(
        reviewed_party_identity,
        "republican party"
      ),
    
    is_snp_montenegro =
      party_mapping_country_code == "ME" &
      stringr::str_detect(
        resp_party_label_key,
        "socialist people s party|socijalisticka narodna partija"
      ),
    
    is_eff_south_africa =
      party_mapping_country_code == "ZA" &
      stringr::str_detect(
        resp_party_label_key,
        "economic freedom fighters"
      ),
    
    is_srs_serbia =
      party_mapping_country_code == "RS" &
      (
        dplyr::coalesce(ppc_partyfacts_id == 2175, FALSE) |
          stringr::str_detect(
            reviewed_party_identity,
            "serbian radical party"
          )
      ),
    
    # -------------------------------------------------------------------------
    # Turkey: reviewed party-identity rule
    #
    # Primary rule for the Turkish observations:
    #   MHP = far right
    #   BBP = far right
    #   AKP = not far right
    #   all other Turkish parties = not far right
    #
    # This replaces the unstable legacy numeric-ID classification for Turkey.
    # -------------------------------------------------------------------------
    
    is_mhp_turkey =
      dplyr::coalesce(
        party_mapping_country_code == "TR",
        FALSE
      ) &
      (
        dplyr::coalesce(
          resp_party_id == 792006,
          FALSE
        ) |
          dplyr::coalesce(
            stringr::str_detect(
              reviewed_party_identity,
              "nationalist (movement|action) party"
            ),
            FALSE
          ) |
          dplyr::coalesce(
            stringr::str_detect(
              resp_party_label_key,
              "(^| )mhp( |$)"
            ),
            FALSE
          )
      ),
    
    is_bbp_turkey =
      dplyr::coalesce(
        party_mapping_country_code == "TR",
        FALSE
      ) &
      (
        dplyr::coalesce(
          resp_party_id == 792013,
          FALSE
        ) |
          dplyr::coalesce(
            stringr::str_detect(
              reviewed_party_identity,
              "great unity party"
            ),
            FALSE
          ) |
          dplyr::coalesce(
            stringr::str_detect(
              resp_party_label_key,
              "(^| )bbp( |$)"
            ),
            FALSE
          )
      ),
    
    is_akp_turkey =
      dplyr::coalesce(
        party_mapping_country_code == "TR",
        FALSE
      ) &
      (
        dplyr::coalesce(
          resp_party_id == 792008,
          FALSE
        ) |
          dplyr::coalesce(
            stringr::str_detect(
              reviewed_party_identity,
              "justice and development party"
            ),
            FALSE
          ) |
          dplyr::coalesce(
            stringr::str_detect(
              resp_party_label_key,
              "(^| )akp( |$)"
            ),
            FALSE
          )
      ),
    
    is_turkey_reviewed =
      dplyr::coalesce(
        party_mapping_country_code == "TR",
        FALSE
      ) &
      dplyr::coalesce(
        party_choice_valid,
        FALSE
      ),
    
    # Record whether this respondent is affected by one of the reviewed rules.
    identity_time_override_applied = dplyr::coalesce(
      party_choice_valid &
        (
          is_sps_serbia |
            is_sns_serbia |
            is_fatherland_ukraine |
            is_lyashko_ukraine |
            is_republican_us |
            is_snp_montenegro |
            is_eff_south_africa |
            is_srs_serbia |
            is_turkey_reviewed
        ),
      FALSE
    ),
    
    # Human-readable audit reason.
    identity_time_override_reason = dplyr::case_when(
      
      party_choice_valid &
        is_sps_serbia ~
        "Reviewed rule: Socialist Party of Serbia is not far right",
      
      party_choice_valid &
        is_sns_serbia ~
        "Reviewed rule: Serbian Progressive Party is not far right",
      
      party_choice_valid &
        is_srs_serbia ~
        "Reviewed rule: Serbian Radical Party is far right",
      
      party_choice_valid &
        is_fatherland_ukraine ~
        "Reviewed rule: Fatherland is not far right",
      
      party_choice_valid &
        is_lyashko_ukraine ~
        "Reviewed rule: Radical Party of Oleh Lyashko is not far right",
      
      party_choice_valid &
        is_republican_us &
        year <= 2011 ~
        "Reviewed rule: US Republican Party not classified far right through 2011",
      
      party_choice_valid &
        is_republican_us &
        year > 2011 ~
        "Reviewed rule: US Republican Party classified far right after 2011",
      
      party_choice_valid &
        is_snp_montenegro ~
        "Reviewed rule: Socialist People's Party of Montenegro is not far right",
      
      party_choice_valid &
        is_eff_south_africa ~
        "Reviewed rule: Economic Freedom Fighters is not far right",
      
      party_choice_valid &
        is_mhp_turkey ~
        "Reviewed Turkey rule: MHP is far right",
      
      party_choice_valid &
        is_bbp_turkey ~
        "Reviewed Turkey rule: BBP is far right",
      
      party_choice_valid &
        is_akp_turkey ~
        "Reviewed Turkey rule: AKP is not far right",
      
      party_choice_valid &
        is_turkey_reviewed ~
        "Reviewed Turkey rule: other Turkish party is not far right",
      
      TRUE ~ NA_character_
    ),
    
    # Override the final classification.
    far_right_vote = dplyr::case_when(
      
      # Never resurrect an invalid/non-party response.
      !party_choice_valid ~ NA,
      
      # -------------------------------------------------------------
      # Turkey: explicit reviewed rule replaces legacy classification
      # -------------------------------------------------------------
      is_mhp_turkey ~ TRUE,
      is_bbp_turkey ~ TRUE,
      
      # AKP and every other valid Turkish party are not coded far right.
      is_turkey_reviewed ~ FALSE,
      
      # Serbia: SPS never far right.
      is_sps_serbia ~ FALSE,
      
      # Serbia: SNS never far right.
      is_sns_serbia ~ FALSE,
      
      # Serbia: Serbian Radical Party is far right throughout.
      is_srs_serbia ~ TRUE,
      
      # Ukraine: neither of these parties is far right.
      is_fatherland_ukraine ~ FALSE,
      is_lyashko_ukraine ~ FALSE,
      
      # United States: Republican Party only after 2011.
      is_republican_us & year <= 2011 ~ FALSE,
      is_republican_us & year > 2011  ~ TRUE,
      
      # Montenegro: Socialist People's Party is never far right.
      is_snp_montenegro ~ FALSE,
      
      # South Africa: Economic Freedom Fighters is never far right.
      is_eff_south_africa ~ FALSE,
      
      # Everything else keeps its existing classification.
      TRUE ~ far_right_vote
    ),
    
    # Make the audit show when the reviewed rule, rather than the underlying
    # PopuList/legacy classifier, produced the final answer.
    fr_classification_source = dplyr::case_when(
      identity_time_override_applied ~
        "manual_identity_time_override",
      TRUE ~ fr_classification_source
    )
  )

# -----------------------------------------------------------------------------
# HARD SAFETY CHECKS FOR THE ERRORS ALREADY IDENTIFIED
# -----------------------------------------------------------------------------

# No PopuList-covered country is ever allowed to leak back to the legacy rule.
stopifnot(
  !any(
    analysis_micro$populist_country_covered &
      analysis_micro$party_choice_valid &
      stringr::str_detect(analysis_micro$fr_classification_source, "^legacy"),
    na.rm = TRUE
  )
)

# Known false positives must remain false/excluded. Use labels wherever
# possible so the check itself is robust to harmonized numeric recoding.
known_false_positive_check <- analysis_micro %>%
  dplyr::filter(
    (party_mapping_country_code == "GB" & year == 2009L & party_source == "WVS" & resp_party_id == 826015) |
      (party_mapping_country_code == "AL" & year == 2002L & party_source == "WVS" & resp_party_id == 8014) |
      (party_mapping_country_code == "BG" & stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
        "bulgarian socialist|balgarska socialisticheska"
      )) |
      (party_mapping_country_code == "EE" & is_nonparty_label(resp_party_label)) |
      (party_mapping_country_code == "CY" & stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
        "democratic rally|\\bdisy\\b"
      )) |
      (party_mapping_country_code == "SI" & year < 2015 & stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
        "slovenian democratic|social democratic party \\(sds\\)"
      )) |
      (party_mapping_country_code == "SI" & stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
        "socialni demokrati|social democrats"
      ))
  )

if (any(known_false_positive_check$far_right_vote %in% TRUE, na.rm = TRUE)) {
  print(
    known_false_positive_check %>%
      dplyr::filter(far_right_vote %in% TRUE) %>%
      dplyr::select(
        country_label, year, survey_wave, party_source,
        resp_party_id, resp_party_label, ppc_label, ppc_match_route,
        far_right_vote, fr_classification_source
      ),
    n = Inf
  )
  stop("Known false-positive party coding reappeared. See rows printed above.")
}

# Known positive parties should be counted ONLY when observed during their
# strict non-borderline PopuList interval.  Do not test the party lineage
# outside that interval: UKIP is strict far right from 2002 onward, AfD from
# 2015 onward, and the Finns Party from 2017 onward.
#
# These checks target the EVS5 harmonized-ID bridge without accidentally
# requiring a party to be far right before PopuList's own start date.
known_positive_check <- analysis_micro %>%
  dplyr::filter(
    party_choice_valid,
    (party_mapping_country_code == "FI" & year >= 2017 & ppc_partyfacts_id == 1689) |
      (party_mapping_country_code == "FR" & ppc_partyfacts_id == 433) |
      (party_mapping_country_code == "IT" & year >= 2018 & ppc_partyfacts_id == 2280) |
      (party_mapping_country_code == "DE" & year >= 2015 & ppc_partyfacts_id == 1976) |
      (party_mapping_country_code == "NL" & ppc_partyfacts_id %in% c(298, 5855)) |
      (party_mapping_country_code == "GB" & year >= 2002 & ppc_partyfacts_id == 601)
  )

if (nrow(known_positive_check) > 0 && any(!(known_positive_check$far_right_vote %in% TRUE), na.rm = TRUE)) {
  print(
    known_positive_check %>%
      dplyr::filter(!(far_right_vote %in% TRUE)) %>%
      dplyr::select(
        country_label, year, survey_wave, party_source,
        resp_party_id, resp_party_label,
        ppc_label, ppc_partyfacts_id, ppc_match_route,
        reviewed_temporal_match,
        reviewed_partyfacts_temporal_match,
        supplemental_temporal_match,
        far_right_vote, fr_classification_source
      ),
    n = Inf
  )
  stop("Known far-right party failed the temporal PopuList mapping. See rows printed above.")
}

# Complementary pre-period check: parties with documented later far-right
# transitions must NOT be back-coded as far right before the strict interval.
# This protects exactly the temporal logic that the previous QA block
# accidentally contradicted.
known_preperiod_check <- analysis_micro %>%
  dplyr::filter(
    party_choice_valid,
    (party_mapping_country_code == "FI" & year < 2017 & ppc_partyfacts_id == 1689) |
      (party_mapping_country_code == "DE" & year < 2015 & ppc_partyfacts_id == 1976) |
      (party_mapping_country_code == "GB" & year < 2002 & ppc_partyfacts_id == 601)
  )

if (nrow(known_preperiod_check) > 0 && any(known_preperiod_check$far_right_vote %in% TRUE, na.rm = TRUE)) {
  print(
    known_preperiod_check %>%
      dplyr::filter(far_right_vote %in% TRUE) %>%
      dplyr::select(
        country_label, year, survey_wave, party_source,
        resp_party_id, resp_party_label,
        ppc_label, ppc_partyfacts_id, ppc_match_route,
        reviewed_temporal_match,
        reviewed_partyfacts_temporal_match,
        supplemental_temporal_match,
        far_right_vote, fr_classification_source
      ),
    n = Inf
  )
  stop("A party was back-coded as far right before its strict PopuList interval.")
}

# The 2017/18 EVS harmonized categories that motivated this rewrite must be
# bridged, not left as PPC-unmatched. If the named categories are present, fail
# loudly if their PPC PartyFacts identity was not recovered.
recent_bridge_check <- analysis_micro %>%
  dplyr::filter(
    party_source == "EVS",
    survey_wave == 5,
    (
      (party_mapping_country_code == "FR" & stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
        "national front"
      )) |
        (party_mapping_country_code == "IT" & stringr::str_detect(
          stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
          "brothers of italy"
        )) |
        (party_mapping_country_code == "DE" & stringr::str_detect(
          stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
          "alternative for germany"
        )) |
        (party_mapping_country_code == "NL" & stringr::str_detect(
          stringr::str_to_lower(dplyr::coalesce(resp_party_label, "")),
          "party for freedom|forum for democracy"
        ))
    )
  )

if (nrow(recent_bridge_check) > 0 && any(is.na(recent_bridge_check$ppc_partyfacts_id))) {
  print(
    recent_bridge_check %>%
      dplyr::filter(is.na(ppc_partyfacts_id)) %>%
      dplyr::select(
        country_label, year, resp_party_id, resp_party_label,
        ppc_match_route, ppc_bridge_method, ppc_bridge_reason
      ),
    n = Inf
  )
  stop("Known EVS5 harmonized party failed the PPC label bridge.")
}

# Finns Party must be classifiable as strict far right from 2017 onward when it
# is observed with a valid PPC PartyFacts match.
finns_audit <- analysis_micro %>%
  dplyr::filter(
    party_mapping_country_code == "FI",
    ppc_partyfacts_id == 1689
  ) %>%
  dplyr::select(
    year, survey_wave, party_source, resp_party_id, resp_party_label,
    ppc_partyfacts_id, ppc_match_route,
    supplemental_temporal_match,
    far_right_vote, fr_classification_source
  ) %>%
  dplyr::distinct()

# Ideology groups are assigned later under both requested definitions.
analysis_micro %>%
  dplyr::count(E033_num) %>%
  dplyr::arrange(E033_num) %>%
  print(n = Inf)

print(
  analysis_micro %>%
    dplyr::filter(!is.na(resp_party_id)) %>%
    dplyr::count(fr_classification_source, sort = TRUE),
  n = Inf
)

if (nrow(finns_audit) > 0) {
  message("Finns Party temporal audit:")
  print(finns_audit, n = Inf)
}

# -----------------------------------------------------------------------------
# HARD CHECKS: REVIEWED LEGACY-PARTY RULES
# -----------------------------------------------------------------------------

# Socialist Party of Serbia must never be far right.
stopifnot(
  !any(
    analysis_micro$is_sps_serbia &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# Serbian Progressive Party must never be far right.
stopifnot(
  !any(
    analysis_micro$is_sns_serbia &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# Fatherland must never be far right.
stopifnot(
  !any(
    analysis_micro$is_fatherland_ukraine &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# Radical Party of Oleh Lyashko must never be far right.
stopifnot(
  !any(
    analysis_micro$is_lyashko_ukraine &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# US Republican Party:
# not far right through 2011; far right after 2011.
stopifnot(
  !any(
    analysis_micro$is_republican_us &
      analysis_micro$year <= 2011 &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

stopifnot(
  !any(
    analysis_micro$is_republican_us &
      analysis_micro$year > 2011 &
      !(analysis_micro$far_right_vote %in% TRUE),
    na.rm = TRUE
  )
)

# Serbian Radical Party must always be far right.
stopifnot(
  !any(
    analysis_micro$is_srs_serbia &
      analysis_micro$party_choice_valid &
      !(analysis_micro$far_right_vote %in% TRUE),
    na.rm = TRUE
  )
)

# Socialist People's Party of Montenegro must never be far right.
stopifnot(
  !any(
    analysis_micro$is_snp_montenegro &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# Economic Freedom Fighters must never be far right.
stopifnot(
  !any(
    analysis_micro$is_eff_south_africa &
      analysis_micro$far_right_vote %in% TRUE,
    na.rm = TRUE
  )
)

# =============================================================================
# 7. DEFINE ONE COMMON SET OF INCLUDED COUNTRIES
# =============================================================================

country_eligibility <- analysis_micro %>%
  dplyr::filter(
    year >= START_YEAR,
    !(S009_code %in% DROP_CODES),
    analysis_group %in% ANALYSIS_GROUP_LEVELS,
    !is.na(country_label)
  ) %>%
  dplyr::group_by(
    S009_code,
    country_label,
    analysis_group
  ) %>%
  dplyr::summarise(
    n_ideology_years = dplyr::n_distinct(
      year[!is.na(E033_num)]
    ),
    
    n_party_years = dplyr::n_distinct(
      year[
        !is.na(E033_num) &
          !is.na(far_right_vote)
      ]
    ),
    
    any_moderate_fr = any(
      is_moderate &
        far_right_vote %in% TRUE,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n_ideology_years >= MIN_COUNTRY_YEARS,
    (!REQUIRE_ANY_MODERATE_FR) | any_moderate_fr
  ) %>%
  dplyr::mutate(
    analysis_group = factor(
      analysis_group,
      levels = ANALYSIS_GROUP_LEVELS
    )
  ) %>%
  dplyr::arrange(
    analysis_group,
    country_label
  )

included_country_codes <- country_eligibility$S009_code

analysis_micro <- analysis_micro %>%
  dplyr::mutate(
    included_country = S009_code %in% included_country_codes
  )

# Re-save with the inclusion flag attached.
saveRDS(
  analysis_micro,
  file.path(out_dir_data, "analysis_micro.rds")
)

readr::write_csv(
  country_eligibility,
  file.path(out_dir_csv, "included_countries.csv")
)

country_group_audit <- country_eligibility %>%
  dplyr::select(
    S009_code,
    country_label,
    analysis_group,
    n_ideology_years,
    n_party_years,
    any_moderate_fr
  ) %>%
  dplyr::arrange(
    analysis_group,
    country_label
  )

readr::write_csv(
  country_group_audit,
  file.path(
    out_dir_csv,
    "included_country_group_mapping.csv"
  )
)

message(
  "Included countries: ",
  length(included_country_codes)
)

unclassified_candidates <- analysis_micro %>%
  dplyr::filter(
    year >= START_YEAR,
    !(S009_code %in% DROP_CODES),
    is.na(analysis_group)
  ) %>%
  dplyr::group_by(
    S009_code,
    country_label
  ) %>%
  dplyr::summarise(
    n_years = dplyr::n_distinct(
      year[!is.na(E033_num)]
    ),
    any_moderate_fr = any(
      is_moderate &
        far_right_vote %in% TRUE,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n_years >= MIN_COUNTRY_YEARS,
    (!REQUIRE_ANY_MODERATE_FR) | any_moderate_fr
  )

if (nrow(unclassified_candidates) > 0) {
  warning(
    "Countries meet the substantive inclusion rules but have no analysis-group classification."
  )
  print(unclassified_candidates, n = Inf)
}



# =============================================================================
# 7A. PARTY CLASSIFICATION AUDIT
# =============================================================================
#
# These outputs are intentionally redundant. The goal is to make every step of
# the party coding inspectable before interpreting any trend figure.

party_classification_audit <- analysis_micro %>%
  dplyr::filter(
    included_country,
    year >= START_YEAR,
    !is.na(resp_party_id)
  ) %>%
  dplyr::group_by(
    S009_code,
    party_mapping_country_code,
    country_label,
    analysis_group,
    year,
    survey_wave,
    party_source,
    party_choice_basis,
    resp_party_id,
    resp_party_label,
    party_choice_valid,
    party_validation_source,
    ppc_wave_available,
    ppc_exact_id_matched,
    ppc_label_bridge_matched,
    ppc_id_matched,
    ppc_numeric_label_conflict,
    ppc_match_route,
    ppc_bridge_method,
    ppc_bridge_reason,
    ppc_bridge_source_party_id,
    ppc_type,
    ppc_label,
    ppc_partyfacts_id,
    ppc_party_name,
    populist_country_covered,
    supplemental_temporal_match,
    supplemental_party_name,
    supplemental_fr_start,
    supplemental_fr_end,
    reviewed_temporal_match,
    reviewed_partyfacts_temporal_match,
    reviewed_party_name,
    reviewed_pf_party_name,
    reviewed_fr_start,
    reviewed_fr_end,
    legacy_far_right_vote_raw,
    far_right_vote_pre_override,
    far_right_override,
    override_reason,
    far_right_vote,
    fr_classification_source,
    matched_populist_party,
    identity_time_override_applied,
    identity_time_override_reason
  ) %>%
  dplyr::summarise(
    n_voters = dplyr::n(),
    weighted_voters = sum(survey_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    country_label,
    year,
    party_source,
    resp_party_id
  )

readr::write_csv(
  party_classification_audit,
  file.path(out_dir_csv, "party_classification_audit_detailed.csv")
)

# Parties actually counted as far right after all temporal rules and overrides.
far_right_party_audit <- party_classification_audit %>%
  dplyr::filter(far_right_vote %in% TRUE) %>%
  dplyr::arrange(country_label, year, party_source, resp_party_id)

readr::write_csv(
  far_right_party_audit,
  file.path(out_dir_csv, "far_right_parties_by_country_year_wave.csv")
)

far_right_party_audit_compact <- far_right_party_audit %>%
  dplyr::group_by(
    S009_code,
    country_label,
    analysis_group,
    year,
    survey_wave,
    party_source
  ) %>%
  dplyr::summarise(
    far_right_parties = paste(
      unique(
        dplyr::coalesce(
          matched_populist_party,
          resp_party_label,
          ppc_label,
          paste0("Party ID ", resp_party_id)
        )
      ),
      collapse = " | "
    ),
    far_right_party_ids = paste(unique(resp_party_id), collapse = " | "),
    classification_sources = paste(unique(fr_classification_source), collapse = " | "),
    n_far_right_voters = sum(n_voters),
    weighted_far_right_voters = sum(weighted_voters),
    .groups = "drop"
  ) %>%
  dplyr::arrange(country_label, year)

readr::write_csv(
  far_right_party_audit_compact,
  file.path(out_dir_csv, "far_right_parties_by_country_year_wave_compact.csv")
)

# All positive survey responses that were excluded because they are not valid
# parties (e.g., "No party appeals to me", Other, blank ballot).
nonparty_exclusion_audit <- party_classification_audit %>%
  dplyr::filter(!party_choice_valid) %>%
  dplyr::arrange(country_label, year, resp_party_id)

readr::write_csv(
  nonparty_exclusion_audit,
  file.path(out_dir_csv, "nonparty_party_responses_excluded.csv")
)

# Positive IDs that remain PPC-unmatched even after the label bridge. Unlike
# v5, party-looking labels are retained rather than automatically excluded.
ppc_unmatched_audit <- party_classification_audit %>%
  dplyr::filter(
    ppc_wave_available,
    !ppc_id_matched
  ) %>%
  dplyr::arrange(country_label, year, party_source, resp_party_id)

readr::write_csv(
  ppc_unmatched_audit,
  file.path(out_dir_csv, "ppc_unmatched_observed_party_ids.csv")
)

ppc_unmatched_valid_party_audit <- ppc_unmatched_audit %>%
  dplyr::filter(party_choice_valid)

readr::write_csv(
  ppc_unmatched_valid_party_audit,
  file.path(out_dir_csv, "ppc_unmatched_but_retained_party_choices.csv")
)

# Compare legacy versus final classification. PopuList-country differences are
# expected and are exactly where the temporal/PPC correction should operate.
classification_changes <- party_classification_audit %>%
  dplyr::filter(
    party_choice_valid,
    !is.na(legacy_far_right_vote_raw),
    !is.na(far_right_vote),
    legacy_far_right_vote_raw != far_right_vote
  )

readr::write_csv(
  classification_changes,
  file.path(out_dir_csv, "far_right_classification_changes_vs_legacy.csv")
)

classification_source_audit <- analysis_micro %>%
  dplyr::filter(
    included_country,
    year >= START_YEAR,
    !is.na(resp_party_id)
  ) %>%
  dplyr::count(
    S009_code,
    country_label,
    year,
    survey_wave,
    party_source,
    populist_country_covered,
    party_validation_source,
    fr_classification_source,
    name = "n_respondents"
  ) %>%
  dplyr::arrange(country_label, year, party_source, fr_classification_source)

readr::write_csv(
  classification_source_audit,
  file.path(out_dir_csv, "far_right_classification_source_audit.csv")
)

# Hard proof that PopuList-covered countries never use the legacy classifier.
populist_legacy_leak_audit <- party_classification_audit %>%
  dplyr::filter(
    populist_country_covered,
    party_choice_valid,
    stringr::str_detect(fr_classification_source, "^legacy")
  )

readr::write_csv(
  populist_legacy_leak_audit,
  file.path(out_dir_csv, "populist_country_legacy_leak_audit.csv")
)

if (nrow(populist_legacy_leak_audit) > 0) {
  stop("Legacy classification leaked into a PopuList-covered country.")
}

# Reused ID / changing-label diagnostic. This remains useful even though PPC and
# temporal PopuList now prevent those reused IDs from determining classification.
party_id_wave_collision_audit <- party_classification_audit %>%
  dplyr::filter(!is.na(resp_party_label)) %>%
  dplyr::distinct(
    party_mapping_country_code,
    country_label,
    party_source,
    resp_party_id,
    year,
    survey_wave,
    resp_party_label,
    ppc_partyfacts_id,
    far_right_vote,
    fr_classification_source
  ) %>%
  dplyr::group_by(
    party_mapping_country_code,
    country_label,
    party_source,
    resp_party_id
  ) %>%
  dplyr::summarise(
    n_distinct_labels = dplyr::n_distinct(resp_party_label),
    n_distinct_partyfacts = dplyr::n_distinct(ppc_partyfacts_id, na.rm = TRUE),
    mappings = paste(
      unique(
        paste0(
          year, " (wave ", survey_wave, "): ", resp_party_label,
          " [PF=", ppc_partyfacts_id,
          "; FR=", far_right_vote,
          "; ", fr_classification_source, "]"
        )
      ),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_distinct_labels > 1 | n_distinct_partyfacts > 1) %>%
  dplyr::arrange(
    dplyr::desc(n_distinct_partyfacts),
    dplyr::desc(n_distinct_labels),
    country_label,
    resp_party_id
  )

readr::write_csv(
  party_id_wave_collision_audit,
  file.path(out_dir_csv, "party_id_wave_collision_audit.csv")
)

# Legacy-only far-right positives are the remaining substantive vulnerability.
# This file is intentionally short enough for manual review and should be
# checked before publication.
legacy_far_right_review <- far_right_party_audit %>%
  dplyr::filter(fr_classification_source == "legacy_outside_PopuList") %>%
  dplyr::arrange(country_label, year, resp_party_id)

readr::write_csv(
  legacy_far_right_review,
  file.path(out_dir_csv, "legacy_outside_populist_far_right_parties_review.csv")
)

message(
  "Party audit written. Final far-right party rows: ",
  nrow(far_right_party_audit),
  "; changed vs legacy: ", nrow(classification_changes),
  "; non-party positive responses excluded: ", nrow(nonparty_exclusion_audit),
  "; PPC-unmatched positive IDs after bridge: ", nrow(ppc_unmatched_audit),
  "; legacy-only far-right rows requiring manual review: ", nrow(legacy_far_right_review),
  "."
)


# =============================================================================

# =============================================================================
# 7B. COMMON BALANCED-PANEL AUDIT FOR WVS AND EVS
# =============================================================================
#
# "Balanced" means a country has usable ideology AND far-right outcome data
# in every native survey-program wave represented at year >= START_YEAR.
# WVS and EVS are balanced separately; their wave numbering is never merged.
#
# This membership is ideology-scheme invariant because it is defined from the
# cleaned 1-10 E033 score and the final far-right outcome before assigning the
# narrow or broad ideology buckets.

balanced_wave_presence <- analysis_micro %>%
  dplyr::filter(
    included_country,
    year >= START_YEAR,
    party_source %in% c("WVS", "EVS"),
    !is.na(survey_wave),
    !is.na(E033_num),
    !is.na(far_right_vote)
  ) %>%
  dplyr::distinct(
    party_source,
    survey_wave,
    S009_code,
    country_label
  )

balanced_wave_universe <- balanced_wave_presence %>%
  dplyr::group_by(
    party_source,
    survey_wave
  ) %>%
  dplyr::summarise(
    n_countries_with_usable_data = dplyr::n_distinct(S009_code),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    party_source,
    survey_wave
  )

balanced_required_wave_counts <- balanced_wave_universe %>%
  dplyr::count(
    party_source,
    name = "n_required_post1990_waves"
  )

balanced_country_coverage <- balanced_wave_presence %>%
  dplyr::group_by(
    party_source,
    S009_code,
    country_label
  ) %>%
  dplyr::summarise(
    n_post1990_waves_present = dplyr::n_distinct(survey_wave),
    waves_present = paste(
      sort(unique(survey_wave)),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    balanced_required_wave_counts,
    by = "party_source"
  ) %>%
  dplyr::mutate(
    balanced_panel_member =
      n_post1990_waves_present == n_required_post1990_waves
  ) %>%
  dplyr::arrange(
    party_source,
    dplyr::desc(balanced_panel_member),
    country_label
  )

readr::write_csv(
  balanced_wave_universe,
  file.path(
    out_dir_csv,
    "balanced_panel_post1990_wave_universe.csv"
  )
)

readr::write_csv(
  balanced_country_coverage,
  file.path(
    out_dir_csv,
    "balanced_panel_country_membership.csv"
  )
)

message(
  "Balanced-panel audit: WVS members = ",
  sum(
    balanced_country_coverage$party_source == "WVS" &
      balanced_country_coverage$balanced_panel_member,
    na.rm = TRUE
  ),
  "; EVS members = ",
  sum(
    balanced_country_coverage$party_source == "EVS" &
      balanced_country_coverage$balanced_panel_member,
    na.rm = TRUE
  ),
  "."
)

# =============================================================================
# 14. OUTPUT 3 DATA: HOW LARGE IS THE MODERATE ELECTORATE?
# =============================================================================

# Calculate moderate prevalence separately within every country-survey.
# Survey weights operate within each survey.

moderate_share_by_survey <- analysis_micro %>%
  dplyr::filter(
    included_country,
    year >= START_YEAR,
    !is.na(E033_num)
  ) %>%
  dplyr::group_by(
    S009_code,
    country_label,
    analysis_group,
    party_source,
    survey_wave,
    year
  ) %>%
  dplyr::summarise(
    # Raw counts retained for transparency.
    n_valid_ideology = dplyr::n(),
    n_moderate = sum(
      E033_num %in% c(5, 6)
    ),
    
    # Weight diagnostics.
    n_weighted_ideology = sum(
      !is.na(survey_weight)
    ),
    
    weight_sum = sum(
      survey_weight,
      na.rm = TRUE
    ),
    
    moderate_weight_sum = sum(
      dplyr::if_else(
        E033_num %in% c(5, 6) &
          !is.na(survey_weight),
        survey_weight,
        0
      ),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_moderate_unweighted =
      safe_pct(
        n_moderate,
        n_valid_ideology
      ),
    
    pct_moderate_weighted =
      safe_pct(
        moderate_weight_sum,
        weight_sum
      ),
    
    # Primary displayed quantity follows the global weighting toggle.
    pct_moderate = if (USE_SURVEY_WEIGHTS) {
      pct_moderate_weighted
    } else {
      pct_moderate_unweighted
    }
  )

# Give every survey wave equal weight in the country-level summary.
# Both weighted and unweighted versions are retained for audit.

moderate_share_country <- moderate_share_by_survey %>%
  dplyr::group_by(
    S009_code,
    country_label,
    analysis_group
  ) %>%
  dplyr::summarise(
    # Primary quantity selected by USE_SURVEY_WEIGHTS.
    mean_moderate_share = safe_mean(
      pct_moderate
    ),
    
    min_moderate_share = safe_min(
      pct_moderate
    ),
    
    max_moderate_share = safe_max(
      pct_moderate
    ),
    
    mean_moderate_share_unweighted = safe_mean(
      pct_moderate_unweighted
    ),
    
    mean_moderate_share_weighted = safe_mean(
      pct_moderate_weighted
    ),
    
    n_surveys = dplyr::n(),
    
    total_valid_ideology_n = sum(
      n_valid_ideology
    ),
    
    total_moderate_n = sum(
      n_moderate
    ),
    
    total_weight_sum = sum(
      weight_sum,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(mean_moderate_share),
    country_label
  )

readr::write_csv(
  moderate_share_by_survey,
  file.path(out_dir_csv, "moderate_share_by_survey.csv")
)

readr::write_csv(
  moderate_share_country,
  file.path(out_dir_csv, "moderate_share_by_country.csv")
)


# =============================================================================
# 15. OUTPUT 3: PDF-FORMATTED COUNTRY LIST
# =============================================================================

moderate_share_table <- moderate_share_country %>%
  dplyr::transmute(
    Country = country_label,
    Group = analysis_group,
    `Mean moderate share (weighted)` =
      sprintf("%.1f%%", mean_moderate_share),
    `Range across surveys` = sprintf(
      "%.1f%%–%.1f%%",
      min_moderate_share,
      max_moderate_share
    ),
    Surveys = n_surveys,
    `Valid ideology N` = scales::comma(total_valid_ideology_n),
    `Moderate N` = scales::comma(total_moderate_n)
  )

write_paginated_table_pdf(
  df = moderate_share_table,
  file = file.path(
    out_dir_plots,
    "moderate_share_by_country.pdf"
  ),
  title = paste0(
    "Survey-weighted self-identified moderates (E033 = 5-6), ",
    START_YEAR,
    " onward"
  ),
  rows_per_page = 22L
)

# =============================================================================
# 8-16. RUN ALL IDEOLOGY-DEPENDENT OUTPUTS UNDER BOTH DEFINITIONS
# =============================================================================

analysis_micro_base <- analysis_micro

for (scheme_i in seq_len(nrow(IDEOLOGY_SCHEMES))) {
  
  IDEOLOGY_SCHEME <- IDEOLOGY_SCHEMES$scheme_id[[scheme_i]]
  IDEOLOGY_SCHEME_FILE_STUB <- IDEOLOGY_SCHEMES$file_stub[[scheme_i]]
  IDEOLOGY_SCHEME_LABEL <- IDEOLOGY_SCHEMES$scheme_label[[scheme_i]]
  IDEOLOGY_LABELS <- ideology_labels_for_scheme(IDEOLOGY_SCHEME)
  
  out_dir_data <- file.path(
    OUT_ROOT,
    IDEOLOGY_SCHEME_FILE_STUB,
    "data"
  )
  out_dir_csv <- file.path(
    OUT_ROOT,
    IDEOLOGY_SCHEME_FILE_STUB,
    "csv"
  )
  out_dir_plots <- file.path(
    OUT_ROOT,
    IDEOLOGY_SCHEME_FILE_STUB,
    "plots"
  )
  
  dir.create(out_dir_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir_plots, recursive = TRUE, showWarnings = FALSE)
  
  message("")
  message("============================================================")
  message("Running ideology scheme: ", IDEOLOGY_SCHEME_LABEL)
  message("Output folder: ", file.path(OUT_ROOT, IDEOLOGY_SCHEME_FILE_STUB))
  message("============================================================")
  
  analysis_micro <- analysis_micro_base %>%
    dplyr::mutate(
      ideology_category = classify_ideology(
        E033_num,
        IDEOLOGY_SCHEME
      )
    )
  
  saveRDS(
    analysis_micro,
    file.path(
      out_dir_data,
      "analysis_micro.rds"
    )
  )
  
  # =============================================================================
  # GLOBAL IDEOLOGICAL COMPOSITION
  # =============================================================================
  #
  # What share of respondents are classified Left / Moderate / Right?
  #
  # Survey weights operate WITHIN each survey.
  #
  # We export:
  #   1. survey-specific ideology shares
  #   2. equal-country global averages
  #   3. simple unweighted pooled respondent shares for reference
  
  
  # ---- A. Ideology shares within each actual survey ---------------------------
  
  ideology_composition_by_survey <- analysis_micro %>%
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(E033_num)
    ) %>%
    dplyr::mutate(
      ideology_category = dplyr::if_else(
        is.na(ideology_category),
        "Other valid E033",
        as.character(ideology_category)
      )
    ) %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      party_source,
      survey_wave,
      year,
      ideology_category
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      weighted_n = sum(survey_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      party_source,
      survey_wave,
      year
    ) %>%
    dplyr::mutate(
      pct_unweighted = safe_pct(n, sum(n)),
      pct_weighted = safe_pct(weighted_n, sum(weighted_n)),
      pct_ideology = if (USE_SURVEY_WEIGHTS) {
        pct_weighted
      } else {
        pct_unweighted
      }
    ) %>%
    dplyr::ungroup()
  
  
  readr::write_csv(
    ideology_composition_by_survey,
    file.path(
      out_dir_csv,
      "ideology_composition_by_survey.csv"
    )
  )
  
  # ---- C. Equal-country global average ----------------------------------------
  #
  # First average surveys equally within each country.
  # Then give every country equal weight globally.
  
  ideology_composition_by_country <-
    ideology_composition_by_survey %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      ideology_category
    ) %>%
    dplyr::summarise(
      mean_share_pct = safe_mean(
        pct_ideology
      ),
      
      n_surveys = dplyr::n(),
      
      .groups = "drop"
    )
  
  
  global_ideology_composition_equal_country <-
    ideology_composition_by_country %>%
    dplyr::group_by(
      ideology_category
    ) %>%
    dplyr::summarise(
      mean_share_pct = safe_mean(
        mean_share_pct
      ),
      
      n_countries = dplyr::n_distinct(
        S009_code
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      aggregation = "Equal country"
    )
  
  
  readr::write_csv(
    ideology_composition_by_country,
    file.path(
      out_dir_csv,
      "ideology_composition_by_country.csv"
    )
  )
  
  readr::write_csv(
    global_ideology_composition_equal_country,
    file.path(
      out_dir_csv,
      "global_ideology_composition_equal_country.csv"
    )
  )
  
  
  # ---- D. Raw pooled respondent composition ----------------------------------
  #
  # Useful as a descriptive audit, but unlike the two estimators above,
  # countries/surveys with more respondents receive more influence.
  
  global_ideology_composition_pooled <-
    analysis_micro %>%
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(E033_num)
    ) %>%
    dplyr::mutate(
      ideology_category = dplyr::if_else(
        is.na(ideology_category),
        "Other valid E033",
        as.character(ideology_category)
      )
    ) %>%
    dplyr::count(
      ideology_category,
      name = "n_respondents"
    ) %>%
    dplyr::mutate(
      pct_respondents =
        100 * n_respondents / sum(n_respondents),
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      aggregation = "Pooled respondents, unweighted"
    )
  
  readr::write_csv(
    global_ideology_composition_pooled,
    file.path(
      out_dir_csv,
      "global_ideology_composition_pooled_respondents.csv"
    )
  )
  
  
  message("")
  message("Global ideological composition — equal-country estimator:")
  print(
    global_ideology_composition_equal_country
  )
  
  ideology_classification_population_audit <- analysis_micro %>%
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(E033_num)
    ) %>%
    dplyr::summarise(
      n_valid_ideology = dplyr::n(),
      
      n_classified_left_mod_right =
        sum(!is.na(ideology_category)),
      
      pct_classified =
        100 *
        n_classified_left_mod_right /
        n_valid_ideology,
      
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL
    )
  
  readr::write_csv(
    ideology_classification_population_audit,
    file.path(
      out_dir_csv,
      "global_ideology_classification_population_audit.csv"
    )
  )
  # =============================================================================
  # 8. OUTPUT 1 DATA: COUNTRY-YEAR FAR-RIGHT SHARE BY IDEOLOGY
  # =============================================================================
  
  # First build every eligible country-year-ideology cell WITHOUT applying the
  # MIN_FR_CELL_N restriction. This lets us audit thin cells before deciding what
  # threshold is substantively appropriate.
  fr_country_survey_all <- analysis_micro %>% 
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(ideology_category)
    ) %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      party_source,
      survey_wave,
      year,
      ideology_category
    ) %>%
    dplyr::summarise(
      # Unweighted denominator = respondents in the ideology group with an
      # observed party choice, NOT all respondents in the ideology group.
      n_fr_obs = sum(!is.na(far_right_vote)),
      n_far_right = sum(far_right_vote %in% TRUE, na.rm = TRUE),
      pct_far_right_unweighted = safe_pct(n_far_right, n_fr_obs),
      
      # Weighted version. Only respondents with BOTH a usable party choice and a
      # valid positive survey weight contribute.
      n_weight_obs = sum(!is.na(far_right_vote) & !is.na(survey_weight)),
      weight_sum = sum(
        dplyr::if_else(
          !is.na(far_right_vote) & !is.na(survey_weight),
          survey_weight,
          0
        ),
        na.rm = TRUE
      ),
      weighted_far_right_sum = sum(
        dplyr::if_else(
          far_right_vote %in% TRUE & !is.na(survey_weight),
          survey_weight,
          0
        ),
        na.rm = TRUE
      ),
      weighted_effective_n = effective_n(
        survey_weight[!is.na(far_right_vote)]
      ),
      pct_far_right_weighted = safe_pct(
        weighted_far_right_sum,
        weight_sum
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ideology_category = factor(
        as.character(ideology_category),
        levels = IDEOLOGY_LEVELS
      ),
      pct_far_right = if (USE_SURVEY_WEIGHTS) {
        pct_far_right_weighted
      } else {
        pct_far_right_unweighted
      }
    )
  
  # Collapse separate EVS/WVS surveys in the same country-year.
  # Each underlying survey receives equal weight in the country-year estimate.
  #
  # Survey weights have already been applied WITHIN each survey above, so they
  # must not be summed across different surveys here.
  
  fr_country_year_all <- fr_country_survey_all %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      year,
      ideology_category
    ) %>%
    dplyr::summarise(
      
      # Diagnostics across underlying surveys.
      n_surveys = dplyr::n(),
      
      n_fr_obs = sum(
        n_fr_obs,
        na.rm = TRUE
      ),
      
      n_far_right = sum(
        n_far_right,
        na.rm = TRUE
      ),
      
      n_weight_obs = sum(
        n_weight_obs,
        na.rm = TRUE
      ),
      
      # For audit only. Do not use these summed weights to combine surveys.
      weight_sum = sum(
        weight_sum,
        na.rm = TRUE
      ),
      
      # Pooled respondent share retained only for audit.
      pct_far_right_unweighted_pooled =
        safe_pct(
          n_far_right,
          n_fr_obs
        ),
      
      # Primary unweighted analogue:
      # calculate each survey separately, then average surveys equally.
      pct_far_right_unweighted = safe_mean(
        pct_far_right_unweighted
      ),
      
      pct_far_right_weighted = safe_mean(
        pct_far_right_weighted
      ),
      
      # ESS is retained as an audit quantity; summing independent survey ESSs is
      # a reasonable descriptive total for the country-year.
      weighted_effective_n = sum(
        weighted_effective_n,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ideology_category = factor(
        as.character(ideology_category),
        levels = IDEOLOGY_LEVELS
      ),
      
      pct_far_right = if (USE_SURVEY_WEIGHTS) {
        pct_far_right_weighted
      } else {
        pct_far_right_unweighted
      }
    )
  
  # ---- Cell-size audit --------------------------------------------------------
  
  # Full cell-level diagnostic file. Extreme 0/100 percentages are not errors,
  # but they deserve scrutiny when they are generated from tiny denominators.
  fr_cell_size_diagnostics <- fr_country_year_all %>%
    dplyr::mutate(
      pct_is_extreme = pct_far_right_unweighted %in% c(0, 100),
      weight_coverage_pct = safe_pct(n_weight_obs, n_fr_obs),
      n_lt_5 = n_fr_obs < 5,
      n_lt_10 = n_fr_obs < 10,
      n_lt_25 = n_fr_obs < 25,
      n_lt_50 = n_fr_obs < 50
    ) %>%
    dplyr::arrange(n_fr_obs, country_label, year, ideology_category)
  
  readr::write_csv(
    fr_cell_size_diagnostics,
    file.path(out_dir_csv, "far_right_cell_size_diagnostics.csv")
  )

  # Summary of how much of the data would be removed at different candidate
  # thresholds, overall and separately by ideology.
  fr_cell_threshold_summary_overall <- purrr::map_dfr(
    CELL_N_DIAGNOSTIC_THRESHOLDS,
    function(threshold) {
      fr_country_year_all %>%
        dplyr::summarise(
          scope = "All ideology groups",
          ideology_category = NA_character_,
          min_n_threshold = threshold,
          n_cells_total = dplyr::n(),
          n_cells_below = sum(n_fr_obs < threshold),
          pct_cells_below = 100 * n_cells_below / n_cells_total,
          n_extreme_cells_below = sum(
            n_fr_obs < threshold &
              pct_far_right_unweighted %in% c(0, 100),
            na.rm = TRUE
          )
        )
    }
  )
  
  fr_cell_threshold_summary_by_ideology <- purrr::map_dfr(
    CELL_N_DIAGNOSTIC_THRESHOLDS,
    function(threshold) {
      fr_country_year_all %>%
        dplyr::group_by(ideology_category) %>%
        dplyr::summarise(
          scope = "By ideology",
          min_n_threshold = threshold,
          n_cells_total = dplyr::n(),
          n_cells_below = sum(n_fr_obs < threshold),
          pct_cells_below = 100 * n_cells_below / n_cells_total,
          n_extreme_cells_below = sum(
            n_fr_obs < threshold &
              pct_far_right_unweighted %in% c(0, 100),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          ideology_category = as.character(ideology_category)
        )
    }
  )
  
  fr_cell_threshold_summary <- dplyr::bind_rows(
    fr_cell_threshold_summary_overall,
    fr_cell_threshold_summary_by_ideology
  ) %>%
    dplyr::select(
      scope, ideology_category, min_n_threshold,
      n_cells_total, n_cells_below, pct_cells_below,
      n_extreme_cells_below
    )
  
  readr::write_csv(
    fr_cell_threshold_summary,
    file.path(out_dir_csv, "far_right_cell_size_threshold_summary.csv")
  )
  
  readr::write_csv(
    fr_country_survey_all,
    file.path(
      out_dir_csv,
      "country_survey_far_right_share_by_ideology.csv"
    )
  )
  
  # ---- Sensitivity of fitted trends to MIN_FR_CELL_N -------------------------
  
  fit_grouped_trend_sensitivity <- function(data, grouping_var, threshold) {
    grouping_sym <- rlang::sym(grouping_var)
    
    data %>%
      dplyr::filter(
        n_fr_obs >= threshold,
        !is.na(pct_far_right),
        !is.na(!!grouping_sym)
      ) %>%
      dplyr::group_by(!!grouping_sym, ideology_category) %>%
      dplyr::group_modify(
        ~ fit_time_slope(.x, outcome = "pct_far_right")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        min_fr_cell_n = threshold,
        grouping = grouping_var
      )
  }
  
  trend_sensitivity_analysis_group <- purrr::map_dfr(
    CELL_N_SENSITIVITY_THRESHOLDS,
    ~ fit_grouped_trend_sensitivity(
      fr_country_year_all,
      "analysis_group",
      .x
    )
  )
  
  readr::write_csv(
    trend_sensitivity_analysis_group,
    file.path(
      out_dir_csv,
      paste0(COUNTRY_SCHEME_FILE_STUB, "_trend_sensitivity_by_min_cell_n.csv")
    )
  )
  
  # Country-specific slope sensitivity. This directly identifies countries whose
  # estimated trajectories are being driven by low-N country-year cells.
  country_slope_sensitivity <- purrr::map_dfr(
    CELL_N_SENSITIVITY_THRESHOLDS,
    function(threshold) {
      fr_country_year_all %>%
        dplyr::filter(
          n_fr_obs >= threshold,
          !is.na(pct_far_right)
        ) %>%
        dplyr::group_by(
          S009_code,
          country_label,
          analysis_group,
          ideology_category
        ) %>%
        dplyr::group_modify(
          ~ fit_time_slope(.x, outcome = "pct_far_right")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          min_fr_cell_n = threshold
        )
    }
  )
  
  readr::write_csv(
    country_slope_sensitivity,
    file.path(
      out_dir_csv,
      "country_slope_sensitivity_by_min_cell_n.csv"
    )
  )
  
  # The actual dataset used by the primary trend and slope outputs.
  fr_country_year <- fr_country_year_all %>%
    dplyr::filter(
      n_fr_obs >= MIN_FR_CELL_N,
      !is.na(pct_far_right)
    ) %>%
    dplyr::mutate(
      point_size = pmin(n_fr_obs, POINT_SIZE_CAP)
    )
  
  readr::write_csv(
    fr_country_year,
    file.path(out_dir_csv, "country_year_far_right_share_by_ideology.csv")
  )
  
  message(
    "FR cell N: minimum = ",
    min(fr_country_year_all$n_fr_obs, na.rm = TRUE),
    "; median = ",
    stats::median(fr_country_year_all$n_fr_obs, na.rm = TRUE),
    "; cells under 10 = ",
    sum(fr_country_year_all$n_fr_obs < 10, na.rm = TRUE),
    "; cells under 25 = ",
    sum(fr_country_year_all$n_fr_obs < 25, na.rm = TRUE),
    ". See far_right_cell_size_diagnostics.csv for details."
  )
  
  
  # =============================================================================
  
  # =============================================================================
  # 9. OUTPUTS 1A-1B: PRIMARY CALENDAR-YEAR PUZZLE FIGURES
  # =============================================================================
  #
  # Lines-only figures:
  #   - equal-weight country-year-cell OLS fits
  #   - displayed y-axis fixed at 0-40%
  #
  # Points + lines figures:
  #   - same OLS fits
  #   - faint country-year observations retained for transparency
  #   - displayed y-axis fixed at 0-50%
  #
  # Moderate is dashed on every line graph so interpretation does not depend on
  # color alone.
  
  share_method_label <- if (USE_SURVEY_WEIGHTS) {
    "survey-weighted country-year shares"
  } else {
    "unweighted country-year shares"
  }
  
  GROUP_PLOT_WIDTH <- 12
  
  # ---- 1A. AID / LMIC: fitted lines only --------------------------------------
  
  primary_group_country_caption <-
    country_list_by_group_caption(
      fr_country_year
    )
  
  p_group_lines <- ggplot(
    fr_country_year,
    aes(
      x = year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category
    )
  ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.2
    ) +
    facet_wrap(
      ~ analysis_group,
      nrow = 1
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Far-right support by ideological self-placement",
      subtitle = wrap_plot_text(
        paste0(
          "Country-year observations since ", START_YEAR,
          "; ", share_method_label,
          "; lines are linear best fits"
        ),
        width = 95
      ),
      caption = paste0(
        wrap_plot_text(SUPPORT_MEASURE_NOTE, width = 110),
        "\n\n",
        primary_group_country_caption
      ),
      x = "Year",
      y = "Far-right vote share"
    ) +
    THEME_PUZZLE + 
    theme(
      strip.text = element_text(
        face = "bold"
      )
    )
  
  p_group_lines
  
  ggsave(
    file.path(
      out_dir_plots,
      "aid_lmic_far_right_support_by_ideology_lines_only.png"
    ),
    p_group_lines,
    width = GROUP_PLOT_WIDTH,
    height = 7,
    dpi = 300
  )
  
  # ---- 1A. AID / LMIC: faint country-year points + fitted lines ---------------
  
  p_group_points <- ggplot(
    fr_country_year,
    aes(
      x = year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category
    )
  ) +
    geom_point(
      aes(size = point_size),
      alpha = POINT_ALPHA
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.2
    ) +
    facet_wrap(
      ~ analysis_group,
      nrow = 1
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_size(
      range = c(1.0, 4.0),
      guide = "none"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Far-right support by ideological self-placement",
      subtitle = wrap_plot_text(
        paste0(
          "Country-year observations since ", START_YEAR,
          "; ", share_method_label,
          "; faint points show country-year cells; lines are linear best fits"
        ),
        width = 95
      ),
      caption = paste0(
        wrap_plot_text(SUPPORT_MEASURE_NOTE, width = 110),
        "\n\n",
        primary_group_country_caption
      ),
      x = "Year",
      y = "Far-right vote share"
    ) +
    THEME_PUZZLE +
    theme(
      strip.text = element_text(
        face = "bold"
      )
    )
  
  p_group_points
  
  ggsave(
    file.path(
      out_dir_plots,
      "aid_lmic_far_right_support_by_ideology_points_and_lines.png"
    ),
    p_group_points,
    width = GROUP_PLOT_WIDTH,
    height = 7,
    dpi = 300
  )
  
  # ---- 1B. Global: fitted lines only ------------------------------------------
  
  primary_global_country_caption <-
    country_list_caption(
      fr_country_year
    )
  
  p_global_lines <- ggplot(
    fr_country_year,
    aes(
      x = year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category
    )
  ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.2
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Global far-right support by ideological self-placement",
      subtitle = wrap_plot_text(
        paste0(
          "Country-year observations since ", START_YEAR,
          "; ", share_method_label,
          "; lines are linear best fits"
        ),
        width = 90
      ),
      caption = paste0(
        wrap_plot_text(SUPPORT_MEASURE_NOTE, width = 110),
        "\n\n",
        primary_global_country_caption
      ),
      x = "Year",
      y = "Far-right vote share"
    ) +
    THEME_PUZZLE
  
  p_global_lines
  
  ggsave(
    file.path(
      out_dir_plots,
      "global_all_countries_far_right_support_lines_only.png"
    ),
    p_global_lines,
    width = 9,
    height = 6.75,
    dpi = 300
  )
  
  # ---- 1B. Global: faint country-year points + fitted lines --------------------
  
  p_global_points <- ggplot(
    fr_country_year,
    aes(
      x = year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category
    )
  ) +
    geom_point(
      aes(size = point_size),
      alpha = POINT_ALPHA
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 1.2
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_size(
      range = c(1.0, 4.0),
      guide = "none"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Global far-right support by ideological self-placement",
      subtitle = paste0(
        START_YEAR, " onward; ", share_method_label,
        "; faint points show country-year cells; lines are linear best fits"
      ),
      caption = paste0(
        wrap_plot_text(SUPPORT_MEASURE_NOTE, width = 110),
        "\n\n",
        primary_global_country_caption
      ),
      x = "Year",
      y = "Far-right vote share"
    ) +
    THEME_PUZZLE
  
  p_global_points
  
  ggsave(
    file.path(
      out_dir_plots,
      "global_all_countries_far_right_support_points_and_lines.png"
    ),
    p_global_points,
    width = 9,
    height = 6.75,
    dpi = 300
  )
  
  # =============================================================================
  # 9A-2. APPENDIX FACETED COUNTRY-SPECIFIC TRAJECTORIES
  # =============================================================================
  #
  # Appendix-friendly versions of the country-specific plots.
  #
  # Separate multipage PDFs are produced for:
  #   1. Advanced Industrialized Democracies
  #   2. Low & Middle-Income Countries
  #
  # Each page contains up to 9 countries in a 3 × 3 grid.
  # All facets use the same 0–100% y-axis.
  # Moderate is dashed, as in the other longitudinal figures.
  
  
  COUNTRY_FACET_NCOL <- 3L
  COUNTRY_FACET_NROW <- 3L
  COUNTRIES_PER_FACET_PAGE <-
    COUNTRY_FACET_NCOL * COUNTRY_FACET_NROW
  
  
  # -----------------------------------------------------------------------------
  # Reusable function
  # -----------------------------------------------------------------------------
  
  write_country_facet_pdf <- function(
    group_name,
    file_stub
  ) {
    
    facet_data <- fr_country_year %>%
      dplyr::filter(
        as.character(analysis_group) == group_name
      ) %>%
      dplyr::mutate(
        # Alphabetical facet ordering.
        country_label = factor(
          country_label,
          levels = sort(
            unique(country_label)
          )
        ),
        
        ideology_category = factor(
          as.character(ideology_category),
          levels = IDEOLOGY_LEVELS
        )
      ) %>%
      dplyr::arrange(
        country_label,
        ideology_category,
        year
      )
    
    if (nrow(facet_data) == 0L) {
      warning(
        "No country trajectory data available for ",
        group_name,
        "."
      )
      return(invisible(NULL))
    }
    
    n_countries <- dplyr::n_distinct(
      facet_data$country_label
    )
    
    n_pages <- ceiling(
      n_countries /
        COUNTRIES_PER_FACET_PAGE
    )
    
    # Shorter display label for the figure title.
    group_display <- dplyr::case_when(
      group_name ==
        "Advanced Industrialized Democracies" ~
        "Advanced Industrialized Democracies (AIDs)",
      
      group_name ==
        "Low & Middle-Income Countries" ~
        "Low & Middle-Income Countries (LMICs)",
      
      TRUE ~ group_name
    )
    
    pdf_path <- file.path(
      out_dir_plots,
      paste0(
        file_stub,
        "_country_specific_far_right_support_facets.pdf"
      )
    )
    
    grDevices::pdf(
      pdf_path,
      width = 12,
      height = 9
    )
    
    for (page_i in seq_len(n_pages)) {
      
      p_country_facets <- ggplot(
        facet_data,
        aes(
          x = year,
          y = pct_far_right,
          color = ideology_category,
          linetype = ideology_category,
          group = ideology_category
        )
      ) +
        
        geom_line(
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        
        geom_point(
          size = 1.8,
          alpha = 0.85,
          na.rm = TRUE
        ) +
        
        ggforce::facet_wrap_paginate(
          ~ country_label,
          ncol = COUNTRY_FACET_NCOL,
          nrow = COUNTRY_FACET_NROW,
          page = page_i,
          scales = "fixed"
        ) +
        
        scale_color_manual(
          values = IDEOLOGY_COLORS,
          labels = IDEOLOGY_LABELS,
          breaks = IDEOLOGY_LEVELS,
          name = "Ideological self-placement"
        ) +
        
        scale_linetype_manual(
          values = IDEOLOGY_LINETYPES,
          labels = IDEOLOGY_LABELS,
          breaks = IDEOLOGY_LEVELS,
          name = "Ideological self-placement"
        ) +
        
        scale_y_continuous(
          breaks = seq(
            0,
            100,
            by = 25
          ),
          labels = function(x) {
            paste0(x, "%")
          }
        ) +
        
        scale_x_continuous(
          breaks = scales::breaks_pretty(
            n = 5
          )
        ) +
        
        coord_cartesian(
          ylim = c(0, 100)
        ) +
        
        labs(
          title = paste0(
            "Far-right support by ideological self-placement: ",
            group_display
          ),
          
          subtitle = wrap_plot_text(
            paste0(
              IDEOLOGY_SCHEME_LABEL,
              "; observed country-year shares since ",
              START_YEAR,
              "; page ",
              page_i,
              " of ",
              n_pages
            ),
            width = 105
          ),
          caption = wrap_plot_text(
            SUPPORT_MEASURE_NOTE,
            width = 110
          ),
          x = "Year",
          y = "Far-right vote share",
          color = "Ideological self-placement",
          linetype = "Ideological self-placement"
        ) +
        
        THEME_PUZZLE +
        
        theme(
          # Country names.
          strip.text = element_text(
            face = "bold",
            size = 10
          ),
          
          # Slightly smaller axis text because each page has 9 panels.
          axis.text = element_text(
            size = 8
          ),
          
          axis.title = element_text(
            size = 10
          ),
          
          # One shared legend underneath the grid.
          legend.position = "bottom",
          
          # Give individual panels a little breathing room.
          panel.spacing = grid::unit(
            0.7,
            "lines"
          )
        )
      
      print(
        p_country_facets
      )
      
    }
    
    grDevices::dev.off()
    
    message(
      "Country-facet appendix PDF written: ",
      pdf_path,
      " (",
      n_countries,
      " countries; ",
      n_pages,
      " pages)"
    )
    
    invisible(
      pdf_path
    )
  }
  
  
  # -----------------------------------------------------------------------------
  # Produce separate AID and LMIC appendix figures
  # -----------------------------------------------------------------------------
  
  aid_country_facet_pdf <- write_country_facet_pdf(
    group_name =
      "Advanced Industrialized Democracies",
    file_stub = "aid"
  )
  
  lmic_country_facet_pdf <- write_country_facet_pdf(
    group_name =
      "Low & Middle-Income Countries",
    file_stub = "lmic"
  )
  
  # =============================================================================
  # 9B. SURVEY-WAVE DESCRIPTIVE TRAJECTORIES ON CALENDAR TIME
  # =============================================================================
  #
  # WVS and EVS retain their own native wave identities. Each source-specific
  # wave is positioned at the median calendar year in which included countries
  # were surveyed in that wave. Survey weights operate within country-surveys;
  # countries are then averaged equally within each source-specific wave.
  #
  # Because these graphs contain observed wave points, their y-axis is 0-40%.
  
  wave_calendar_lookup <- fr_country_survey_all %>%
    dplyr::filter(
      n_fr_obs >= MIN_FR_CELL_N,
      !is.na(pct_far_right),
      party_source %in% c("WVS", "EVS"),
      !is.na(survey_wave),
      !is.na(year)
    ) %>%
    dplyr::distinct(
      S009_code,
      party_source,
      survey_wave,
      year
    ) %>%
    dplyr::group_by(
      party_source,
      survey_wave
    ) %>%
    dplyr::summarise(
      wave_calendar_year = stats::median(
        year,
        na.rm = TRUE
      ),
      first_survey_year = min(year, na.rm = TRUE),
      last_survey_year = max(year, na.rm = TRUE),
      n_countries = dplyr::n_distinct(S009_code),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      wave_label = paste0(
        party_source,
        " Wave ",
        survey_wave
      )
    ) %>%
    dplyr::arrange(
      wave_calendar_year,
      party_source,
      survey_wave
    )
  
  fr_country_wave <- fr_country_survey_all %>%
    dplyr::filter(
      n_fr_obs >= MIN_FR_CELL_N,
      !is.na(pct_far_right),
      party_source %in% c("WVS", "EVS"),
      !is.na(survey_wave)
    ) %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      party_source,
      survey_wave,
      ideology_category
    ) %>%
    dplyr::summarise(
      pct_far_right = safe_mean(pct_far_right),
      n_survey_years = dplyr::n_distinct(year),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      wave_calendar_lookup %>%
        dplyr::select(
          party_source,
          survey_wave,
          wave_calendar_year,
          wave_label
        ),
      by = c(
        "party_source",
        "survey_wave"
      )
    )
  
  fr_group_wave <- fr_country_wave %>%
    dplyr::group_by(
      analysis_group,
      party_source,
      survey_wave,
      wave_calendar_year,
      wave_label,
      ideology_category
    ) %>%
    dplyr::summarise(
      pct_far_right = safe_mean(pct_far_right),
      n_countries = dplyr::n_distinct(S009_code),
      .groups = "drop"
    )
  
  # Figure-specific source restriction:
  #   AIDs  = WVS + EVS
  #   LMICs = WVS only
  #
  # Keep fr_group_wave itself unchanged so the full WVS/EVS information remains
  # available in CSVs and diagnostics.
  
  fr_group_wave_plot <- fr_group_wave %>%
    dplyr::filter(
      analysis_group == "Advanced Industrialized Democracies" |
        (
          analysis_group == "Low & Middle-Income Countries" &
            party_source == "WVS"
        )
    )
  
  readr::write_csv(
    fr_group_wave_plot,
    file.path(
      out_dir_csv,
      "far_right_support_by_aid_lmic_and_survey_wave_plot_data.csv"
    )
  )
  
  wave_country_sample <- fr_country_wave %>%
    dplyr::filter(
      analysis_group == "Advanced Industrialized Democracies" |
        (
          analysis_group == "Low & Middle-Income Countries" &
            party_source == "WVS"
        )
    )
  
  wave_group_country_caption <-
    country_list_by_group_caption(
      wave_country_sample
    )
  
  wave_global_country_caption <-
    country_list_caption(
      wave_country_sample,
      prefix = "Countries contributing to at least one displayed wave"
    )
  
  fr_global_wave <- wave_country_sample %>%
    dplyr::group_by(
      party_source,
      survey_wave,
      wave_calendar_year,
      wave_label,
      ideology_category
    ) %>%
    dplyr::summarise(
      pct_far_right = safe_mean(pct_far_right),
      n_countries = dplyr::n_distinct(S009_code),
      .groups = "drop"
    )
  
  readr::write_csv(
    wave_calendar_lookup,
    file.path(
      out_dir_csv,
      "survey_wave_calendar_time_lookup.csv"
    )
  )
  
  readr::write_csv(
    fr_group_wave,
    file.path(
      out_dir_csv,
      "far_right_support_by_aid_lmic_and_survey_wave.csv"
    )
  )
  
  readr::write_csv(
    fr_global_wave,
    file.path(
      out_dir_csv,
      "far_right_support_global_by_survey_wave.csv"
    )
  )
  
  p_group_wave <- ggplot(
    fr_group_wave_plot,
    aes(
      x = wave_calendar_year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category,
      shape = party_source,
      group = ideology_category
    )
  ) +
    geom_line(
      linewidth = 1.1
    ) +
    geom_point(
      size = 3
    ) +
    facet_wrap(
      ~ analysis_group,
      nrow = 1
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_shape_manual(
      values = SURVEY_SOURCE_SHAPES,
      name = "Survey program"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Far-right support by survey wave and ideology",
      x = "Calendar year",
      y = "Far-right vote share",
      caption = paste0(
        wrap_plot_text(
          paste0(
            "Each point is an equal-country survey-wave mean. ",
            "AIDs include WVS and EVS; LMICs include WVS only. ",
            SUPPORT_MEASURE_NOTE
          ),
          width = 110
        ),
        "\n\n",
        wave_group_country_caption
      )
    ) +
    THEME_PUZZLE +
    theme(
      strip.text = element_text(
        face = "bold"
      )
    )
  
  p_group_wave
  
  ggsave(
    file.path(
      out_dir_plots,
      "aid_lmic_far_right_support_by_survey_wave_calendar_time.png"
    ),
    p_group_wave,
    width = GROUP_PLOT_WIDTH,
    height = 7,
    dpi = 300
  )
  
  p_global_wave <- ggplot(
    fr_global_wave,
    aes(
      x = wave_calendar_year,
      y = pct_far_right,
      color = ideology_category,
      linetype = ideology_category,
      shape = party_source,
      group = ideology_category
    )
  ) +
    geom_line(
      linewidth = 1.1
    ) +
    geom_point(
      size = 3
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_linetype_manual(
      values = IDEOLOGY_LINETYPES,
      labels = IDEOLOGY_LABELS,
      breaks = IDEOLOGY_LEVELS,
      name = "Ideological self-placement"
    ) +
    scale_shape_manual(
      values = SURVEY_SOURCE_SHAPES,
      name = "Survey program"
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)
    ) +
    coord_cartesian(
      ylim = c(0, 40)
    ) +
    labs(
      title = "Global far-right support by survey wave and ideology",
      x = "Calendar year",
      y = "Far-right vote share",
      caption = paste0(
        wrap_plot_text(
          paste0(
            "Each point is an equal-country survey-wave mean. ",
            "AIDs include WVS and EVS; LMICs include WVS only. ",
            SUPPORT_MEASURE_NOTE
          ),
          width = 110
        ),
        "\n\n",
        wave_global_country_caption
      )
    ) +
    THEME_PUZZLE
  
  p_global_wave
  
  ggsave(
    file.path(
      out_dir_plots,
      "global_far_right_support_by_survey_wave_calendar_time.png"
    ),
    p_global_wave,
    width = 9,
    height = 6.75,
    dpi = 300
  )
  
  
  # =============================================================================
  # 9C. BALANCED WVS AND EVS GLOBAL TREND REPLICATIONS
  # =============================================================================
  #
  # Separate WVS and EVS balanced panels include only countries with usable
  # ideology + far-right outcome data in EVERY native wave represented from 1990
  # onward in that survey program. These replicate the primary global fitted-line
  # figure and therefore use a 0-50% display scale and contain no raw points.
  
  build_balanced_source_data <- function(source_name) {
    
    balanced_codes <- balanced_country_coverage %>%
      dplyr::filter(
        party_source == source_name,
        balanced_panel_member
      ) %>%
      dplyr::pull(S009_code) %>%
      unique()
    
    if (length(balanced_codes) == 0L) {
      return(tibble::tibble())
    }
    
    fr_country_survey_all %>%
      dplyr::filter(
        party_source == source_name,
        S009_code %in% balanced_codes,
        n_fr_obs >= MIN_FR_CELL_N,
        !is.na(pct_far_right)
      ) %>%
      dplyr::group_by(
        S009_code,
        country_label,
        year,
        ideology_category
      ) %>%
      dplyr::summarise(
        pct_far_right = safe_mean(pct_far_right),
        n_fr_obs = sum(n_fr_obs, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  save_balanced_source_plot <- function(source_name) {
    
    balanced_data <- build_balanced_source_data(source_name)
    
    readr::write_csv(
      balanced_data,
      file.path(
        out_dir_csv,
        paste0(
          tolower(source_name),
          "_balanced_panel_country_year_far_right_share.csv"
        )
      )
    )
    
    if (nrow(balanced_data) == 0L) {
      warning(
        "No ", source_name,
        " countries satisfy the all-post-1990-waves balanced-panel rule; ",
        "no balanced ", source_name, " figure was produced."
      )
      return(invisible(NULL))
    }
    
    n_balanced_countries <- dplyr::n_distinct(
      balanced_data$S009_code
    )
    
    balanced_country_caption <-
      country_list_caption(
        balanced_data,
        width = 105,
        prefix = "Balanced-panel countries"
      )
    
    p_balanced <- ggplot(
      balanced_data,
      aes(
        x = year,
        y = pct_far_right,
        color = ideology_category,
        linetype = ideology_category
      )
    ) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 1.2
      ) +
      scale_color_manual(
        values = IDEOLOGY_COLORS,
        labels = IDEOLOGY_LABELS,
        breaks = IDEOLOGY_LEVELS,
        name = "Ideological self-placement"
      ) +
      scale_linetype_manual(
        values = IDEOLOGY_LINETYPES,
        labels = IDEOLOGY_LABELS,
        breaks = IDEOLOGY_LEVELS,
        name = "Ideological self-placement"
      ) +
      scale_y_continuous(
        breaks = seq(0, 50, by = 10),
        labels = function(x) paste0(x, "%")
      ) +
      coord_cartesian(
        ylim = c(0, 50)
      ) +
      labs(
        title = paste0(
          source_name,
          " balanced-panel far-right support"
        ),
        subtitle = wrap_plot_text(
          paste0(
            IDEOLOGY_SCHEME_LABEL,
            "; countries observed with usable data in every ",
            source_name,
            " wave represented from ", START_YEAR,
            " onward; N = ", n_balanced_countries,
            " countries"
          ),
          width = 90
        ),
        caption = paste0(
          wrap_plot_text(SUPPORT_MEASURE_NOTE, width = 110),
          "\n\n",
          balanced_country_caption
        ),
        x = "Year",
        y = "Far-right vote share"
      ) +
      THEME_PUZZLE
    
    print(p_balanced)
    
    file_stub <- paste0(
      tolower(source_name),
      "_balanced_panel_global_far_right_support_lines_only"
    )
    
    ggsave(
      file.path(
        out_dir_plots,
        paste0(file_stub, ".png")
      ),
      p_balanced,
      width = 9,
      height = 6.75,
      dpi = 300
    )
    
    invisible(p_balanced)
  }
  
  balanced_wvs_plot <- save_balanced_source_plot("WVS")
  balanced_evs_plot <- save_balanced_source_plot("EVS")
  
  # 10. OUTPUT 1C DATA: FIT COUNTRY-SPECIFIC SLOPES (CSV ONLY)
  # =============================================================================
  
  # Fit:
  #   pct_far_right = intercept + slope * year
  #
  # separately for each country and ideological group, using all available
  # country-year observations since START_YEAR that also meet MIN_FR_CELL_N.
  #
  # The coefficient on year is interpreted as percentage-point change in
  # far-right support per calendar year. We now also retain its standard error,
  # 95% confidence interval, and p-value whenever there are enough residual
  # degrees of freedom. With exactly two wave points, the slope exists but its
  # sampling uncertainty cannot be estimated from this regression.
  
  country_slopes <- fr_country_year %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      ideology_category
    ) %>%
    dplyr::group_modify(
      ~ fit_time_slope(.x, outcome = "pct_far_right")
    ) %>%
    dplyr::ungroup()
  
  # Identify countries where the moderate POINT ESTIMATE is steeper than BOTH
  # left and right slopes. This is descriptive only; it does not mean the slope
  # differences are statistically distinguishable from one another.
  slope_comparison <- country_slopes %>%
    dplyr::select(
      S009_code,
      country_label,
      ideology_category,
      slope_pp_per_year
    ) %>%
    tidyr::pivot_wider(
      names_from = ideology_category,
      values_from = slope_pp_per_year
    ) %>%
    dplyr::mutate(
      moderate_steepest = dplyr::if_else(
        !is.na(Left) &
          !is.na(Moderate) &
          !is.na(Right),
        Moderate > Left & Moderate > Right,
        FALSE
      )
    )
  
  country_slopes <- country_slopes %>%
    dplyr::left_join(
      slope_comparison %>%
        dplyr::select(
          S009_code,
          moderate_steepest,
          moderate_slope = Moderate
        ),
      by = "S009_code"
    )
  
  readr::write_csv(
    country_slopes,
    file.path(out_dir_csv, "country_slopes_far_right_support_by_ideology.csv")
  )
  
  readr::write_csv(
    slope_comparison,
    file.path(out_dir_csv, "country_slope_comparison_wide.csv")
  )
  
  
  # =============================================================================
  # 11A. SIMPLE DESCRIPTIVE IDEOLOGY MODELS
  # =============================================================================
  #
  # PURPOSE
  # -------
  # Quantify the descriptive relationship between ideological self-placement
  # and far-right support in two complementary ways:
  #
  #   A. Respondent-level linear probability model (LPM) with country fixed
  #      effects and country-clustered standard errors.
  #
  #   B. Equal-country differences in means:
  #      calculate ideology-group far-right support separately within each
  #      survey, calculate within-survey differences, average surveys within
  #      country, then average countries equally.
  #
  # Both analyses automatically follow IDEOLOGY_SCHEME:
  #
  #   endpoints:
  #     Left = 1-2; Moderate = 5-6; Right = 9-10
  #
  #   broad:
  #     Left = 1-4; Moderate = 5-6; Right = 7-10
  #
  # The sample also automatically follows the common included-country sample
  # already constructed above.
  # =============================================================================
  
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop(
      "Package 'fixest' is required for Section 11A. ",
      "Install it with install.packages('fixest')."
    )
  }
  
  
  # =============================================================================
  # 11A-1. PREPARE COMMON RESPONDENT-LEVEL SAMPLE
  # =============================================================================
  
  ideology_lpm_df <- analysis_micro %>%
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(S009_code),
      !is.na(ideology_category),
      !is.na(far_right_vote)
    ) %>%
    dplyr::mutate(
      far_right_support = as.integer(far_right_vote),
      
      # Ensure Left is the omitted/reference ideology category.
      ideology_category = factor(
        as.character(ideology_category),
        levels = c("Left", "Moderate", "Right")
      ),
      
      # Explicit factor representation of the country fixed effects.
      # Using explicit country indicators rather than absorbed FEs makes
      # adjusted predicted levels straightforward to calculate below.
      country_fe = factor(S009_code)
    )
  
  # Follow the pipeline-wide survey-weight toggle.
  if (USE_SURVEY_WEIGHTS) {
    
    ideology_lpm_df <- ideology_lpm_df %>%
      dplyr::filter(
        !is.na(survey_weight),
        is.finite(survey_weight),
        survey_weight > 0
      ) %>%
      dplyr::mutate(
        model_weight = survey_weight
      )
    
  } else {
    
    ideology_lpm_df <- ideology_lpm_df %>%
      dplyr::mutate(
        model_weight = 1
      )
  }
  
  lpm_country_caption <-
    country_list_caption(
      ideology_lpm_df,
      width = 100
    )
  
  # Basic audit of the estimation sample.
  ideology_lpm_sample_audit <- ideology_lpm_df %>%
    dplyr::summarise(
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      survey_weighted = USE_SURVEY_WEIGHTS,
      n_respondents = dplyr::n(),
      n_countries = dplyr::n_distinct(S009_code),
      n_left = sum(ideology_category == "Left"),
      n_moderate = sum(ideology_category == "Moderate"),
      n_right = sum(ideology_category == "Right")
    )
  
  readr::write_csv(
    ideology_lpm_sample_audit,
    file.path(
      out_dir_csv,
      "ideology_far_right_descriptive_model_sample_audit.csv"
    )
  )
  
  print(ideology_lpm_sample_audit)
  
  
  # =============================================================================
  # 11A-2. RESPONDENT-LEVEL COUNTRY-FE LINEAR PROBABILITY MODEL
  # =============================================================================
  #
  # Model:
  #
  # FarRightSupport_ic =
  #   beta_0
  #   + beta_1 Moderate_ic
  #   + beta_2 Right_ic
  #   + country fixed effects
  #   + error_ic
  #
  # Left is the reference category.
  #
  # Because the outcome is 0/1, coefficients can be multiplied by 100 and read
  # directly as percentage-point differences in far-right support.
  #
  # Standard errors are clustered by country.
  
  
  m_ideology_lpm <- fixest::feols(
    far_right_support ~ ideology_category + country_fe,
    data = ideology_lpm_df,
    weights = ~ model_weight,
    cluster = ~ S009_code,
    data.save = TRUE
  )
  
  print(summary(m_ideology_lpm))
  
  
  # Save the fitted model for later use.
  saveRDS(
    m_ideology_lpm,
    file.path(
      out_dir_data,
      "country_fe_lpm_far_right_support_by_ideology.rds"
    )
  )
  
  
  # =============================================================================
  # 11A-3. LPM CONTRASTS
  # =============================================================================
  #
  # Report three directly interpretable quantities:
  #
  #   Moderate - Left
  #   Right - Left
  #   Moderate - Right
  #
  # The first two are regression coefficients.
  # The third is a linear combination of those coefficients.
  
  lpm_beta <- stats::coef(m_ideology_lpm)
  lpm_vcov <- stats::vcov(m_ideology_lpm)
  
  moderate_coef_name <- grep(
    "^ideology_categoryModerate$",
    names(lpm_beta),
    value = TRUE
  )
  
  right_coef_name <- grep(
    "^ideology_categoryRight$",
    names(lpm_beta),
    value = TRUE
  )
  
  if (
    length(moderate_coef_name) != 1L ||
    length(right_coef_name) != 1L
  ) {
    stop(
      "Could not uniquely identify the Moderate and Right ideology coefficients ",
      "in the country-FE LPM."
    )
  }
  
  
  # Use country-cluster degrees of freedom for inference.
  n_country_clusters <- dplyr::n_distinct(
    ideology_lpm_df$S009_code
  )
  
  cluster_df <- fixest::degrees_freedom(
    m_ideology_lpm,
    type = "t"
  )
  
  cluster_crit <- stats::qt(
    0.975,
    df = cluster_df
  )
  
  
  make_lpm_contrast <- function(
    comparison,
    moderate_weight = 0,
    right_weight = 0
  ) {
    
    L <- rep(
      0,
      length(lpm_beta)
    )
    
    names(L) <- names(lpm_beta)
    
    L[moderate_coef_name] <- moderate_weight
    L[right_coef_name] <- right_weight
    
    estimate <- sum(
      L * lpm_beta
    )
    
    variance <- as.numeric(
      t(L) %*%
        lpm_vcov %*%
        L
    )
    
    # Protect against tiny negative values due only to floating-point error.
    variance <- max(
      variance,
      0
    )
    
    se <- sqrt(
      variance
    )
    
    t_stat <- if (
      is.finite(se) &&
      se > 0
    ) {
      estimate / se
    } else {
      NA_real_
    }
    
    p_value <- if (
      is.finite(t_stat)
    ) {
      2 * stats::pt(
        abs(t_stat),
        df = cluster_df,
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }
    
    tibble::tibble(
      comparison = comparison,
      estimate = estimate,
      std_error = se,
      conf_low = estimate - cluster_crit * se,
      conf_high = estimate + cluster_crit * se,
      t_statistic = t_stat,
      p_value = p_value,
      
      # Percentage-point versions for substantive interpretation.
      estimate_pp = 100 * estimate,
      std_error_pp = 100 * se,
      conf_low_pp = 100 * (estimate - cluster_crit * se),
      conf_high_pp = 100 * (estimate + cluster_crit * se),
      
      n_respondents = nrow(ideology_lpm_df),
      n_countries = n_country_clusters,
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      survey_weighted = USE_SURVEY_WEIGHTS
    )
  }
  
  
  lpm_ideology_contrasts <- dplyr::bind_rows(
    
    make_lpm_contrast(
      comparison = "Moderate - Left",
      moderate_weight = 1,
      right_weight = 0
    ),
    
    make_lpm_contrast(
      comparison = "Right - Left",
      moderate_weight = 0,
      right_weight = 1
    ),
    
    make_lpm_contrast(
      comparison = "Moderate - Right",
      moderate_weight = 1,
      right_weight = -1
    ),
    
    make_lpm_contrast(
      comparison = "Right - Moderate",
      moderate_weight = -1,
      right_weight = 1
    )
  )
  
  
  readr::write_csv(
    lpm_ideology_contrasts,
    file.path(
      out_dir_csv,
      "country_fe_lpm_ideology_contrasts.csv"
    )
  )
  
  print(
    lpm_ideology_contrasts %>%
      dplyr::select(
        comparison,
        estimate_pp,
        std_error_pp,
        conf_low_pp,
        conf_high_pp,
        p_value
      )
  )
  
  
  # =============================================================================
  # 11A-4. ADJUSTED PREDICTED PROBABILITIES FROM THE COUNTRY-FE LPM
  # =============================================================================
  #
  # Counterfactual-standardization interpretation:
  #
  # Take the same estimation sample and country composition and ask what the
  # model predicts if every observation is assigned, in turn:
  #
  #   Left
  #   Moderate
  #   Right
  #
  # Then average the predictions using the model weights.
  #
  # Because country indicators are explicitly included in m_ideology_lpm,
  # uncertainty in the adjusted predicted levels can be calculated directly
  # from the model covariance matrix.
  
  
  prediction_rhs <- ~ ideology_category + country_fe
  
  
  make_adjusted_prediction <- function(group_name) {
    
    prediction_data <- ideology_lpm_df %>%
      dplyr::mutate(
        ideology_category = factor(
          group_name,
          levels = c(
            "Left",
            "Moderate",
            "Right"
          )
        )
      )
    
    X <- stats::model.matrix(
      prediction_rhs,
      data = prediction_data
    )
    
    # Make absolutely sure the model matrix and coefficient vector use the same
    # columns in the same order.
    missing_model_columns <- setdiff(
      names(lpm_beta),
      colnames(X)
    )
    
    if (length(missing_model_columns) > 0) {
      stop(
        "Prediction model matrix is missing coefficient columns: ",
        paste(
          missing_model_columns,
          collapse = ", "
        )
      )
    }
    
    X <- X[
      ,
      names(lpm_beta),
      drop = FALSE
    ]
    
    w <- prediction_data$model_weight
    
    # Weighted mean of every model-matrix column.
    x_bar <- colSums(
      sweep(
        X,
        1,
        w,
        `*`
      )
    ) / sum(w)
    
    estimate <- as.numeric(
      x_bar %*%
        lpm_beta
    )
    
    variance <- as.numeric(
      t(x_bar) %*%
        lpm_vcov %*%
        x_bar
    )
    
    variance <- max(
      variance,
      0
    )
    
    se <- sqrt(
      variance
    )
    
    tibble::tibble(
      ideology_category = group_name,
      estimate = estimate,
      std_error = se,
      conf_low = estimate - cluster_crit * se,
      conf_high = estimate + cluster_crit * se,
      estimate_pct = 100 * estimate,
      std_error_pct = 100 * se,
      conf_low_pct = 100 * (estimate - cluster_crit * se),
      conf_high_pct = 100 * (estimate + cluster_crit * se)
    )
  }
  
  
  lpm_adjusted_predictions <- purrr::map_dfr(
    c(
      "Left",
      "Moderate",
      "Right"
    ),
    make_adjusted_prediction
  ) %>%
    dplyr::mutate(
      ideology_category = factor(
        ideology_category,
        levels = c(
          "Left",
          "Moderate",
          "Right"
        )
      ),
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      survey_weighted = USE_SURVEY_WEIGHTS,
      n_respondents = nrow(ideology_lpm_df),
      n_countries = n_country_clusters
    )
  
  
  readr::write_csv(
    lpm_adjusted_predictions,
    file.path(
      out_dir_csv,
      "country_fe_lpm_adjusted_probabilities_by_ideology.csv"
    )
  )
  
  print(lpm_adjusted_predictions)
  
  
  # ---- Plot adjusted probabilities with adjacent pp-change annotations ---------
  
  lpm_mod_left_for_plot <- lpm_ideology_contrasts %>%
    dplyr::filter(
      comparison == "Moderate - Left"
    )
  
  lpm_right_mod_for_plot <- lpm_ideology_contrasts %>%
    dplyr::filter(
      comparison == "Right - Moderate"
    )
  
  prediction_top <- max(
    lpm_adjusted_predictions$conf_high_pct,
    na.rm = TRUE
  )
  
  prediction_span <- max(
    5,
    max(lpm_adjusted_predictions$conf_high_pct, na.rm = TRUE) -
      min(lpm_adjusted_predictions$conf_low_pct, na.rm = TRUE)
  )
  
  annotation_gap <- max(
    1.5,
    0.12 * prediction_span
  )
  
  lpm_adjacent_annotations <- tibble::tibble(
    x_start = c(1, 2),
    x_end = c(2, 3),
    y = c(
      prediction_top + annotation_gap,
      prediction_top + 3 * annotation_gap
    ),
    label = c(
      sprintf(
        "%+.1f pp",
        lpm_mod_left_for_plot$estimate_pp
      ),
      sprintf(
        "%+.1f pp",
        lpm_right_mod_for_plot$estimate_pp
      )
    )
  )
  
  p_lpm_adjusted_predictions <- ggplot(
    lpm_adjusted_predictions,
    aes(
      x = ideology_category,
      y = estimate_pct,
      color = ideology_category
    )
  ) +
    geom_errorbar(
      aes(
        ymin = conf_low_pct,
        ymax = conf_high_pct
      ),
      width = 0.12,
      linewidth = 0.7
    ) +
    geom_point(
      size = 3.6
    ) +
    geom_segment(
      data = lpm_adjacent_annotations,
      aes(
        x = x_start,
        xend = x_end,
        y = y,
        yend = y
      ),
      inherit.aes = FALSE,
      linewidth = 0.65
    ) +
    geom_segment(
      data = lpm_adjacent_annotations,
      aes(
        x = x_start,
        xend = x_start,
        y = y,
        yend = y - 0.45 * annotation_gap
      ),
      inherit.aes = FALSE,
      linewidth = 0.65
    ) +
    geom_segment(
      data = lpm_adjacent_annotations,
      aes(
        x = x_end,
        xend = x_end,
        y = y,
        yend = y - 0.45 * annotation_gap
      ),
      inherit.aes = FALSE,
      linewidth = 0.65
    ) +
    geom_text(
      data = lpm_adjacent_annotations,
      aes(
        x = (x_start + x_end) / 2,
        y = y + 0.30 * annotation_gap,
        label = label
      ),
      inherit.aes = FALSE,
      size = 4
    ) +
    scale_color_manual(
      values = IDEOLOGY_COLORS,
      breaks = IDEOLOGY_LEVELS,
      labels = IDEOLOGY_LABELS,
      guide = "none"
    ) +
    scale_x_discrete(
      labels = IDEOLOGY_LABELS
    ) +
    scale_y_continuous(
      breaks = seq(0, LPM_Y_MAX, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = expansion(
        mult = c(0.02, 0.02)
      )
    ) +
    coord_cartesian(
      ylim = c(0, LPM_Y_MAX),
      clip = "off"
    ) +
    labs(
      title = "Far-right support by ideological self-placement",
      subtitle = wrap_plot_text(
        paste0(
          "Adjusted probabilities from individual-level country-FE LPM; ",
          IDEOLOGY_SCHEME_LABEL,
          if (USE_SURVEY_WEIGHTS) {
            "; survey-weighted"
          } else {
            "; unweighted"
          },
          "; 95% CIs; SEs clustered by country"
        ),
        width = 85
      ),
      
      caption = paste0(
        wrap_plot_text(
          paste0(
            "Brackets report the regression-implied percentage-point change ",
            "between adjacent ideological categories."
          ),
          width = 95
        ),
        "\n\n",
        lpm_country_caption
      ),
      
      x = "Ideological self-placement",
      y = "Adjusted probability of far-right support"
    ) +
    THEME_PUZZLE
  
  p_lpm_adjusted_predictions
  
  readr::write_csv(
    lpm_adjacent_annotations,
    file.path(
      out_dir_csv,
      "country_fe_lpm_adjacent_ideology_annotations.csv"
    )
  )
  
  ggsave(
    file.path(
      out_dir_plots,
      "country_fe_lpm_adjusted_probabilities_by_ideology.png"
    ),
    p_lpm_adjusted_predictions,
    width = 9,
    height = 6.75,
    dpi = 300
  )
  
  # =============================================================================
  # 11B. EQUAL-COUNTRY DIFFERENCE IN MEANS
  # =============================================================================
  #
  # Estimand:
  #
  #   1. Calculate ideology-specific far-right support separately within each
  #      actual survey.
  #
  #   2. Calculate the ideology-group differences within that survey.
  #
  #   3. Average those survey-specific differences equally within each country.
  #
  #   4. Average the resulting country-specific differences equally across
  #      countries.
  #
  # Thus, unlike the respondent-level LPM, every country contributes exactly one
  # country-level difference to the final cross-national mean.
  
  
  # ---- 11B-1. Ideology-specific means within each survey ----------------------
  
  survey_ideology_means <- ideology_lpm_df %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group,
      party_source,
      survey_wave,
      year,
      ideology_category
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      
      far_right_share = stats::weighted.mean(
        far_right_support,
        w = model_weight,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = ideology_category,
      values_from = c(
        n,
        far_right_share
      ),
      names_sep = "_"
    )
  
  
  # ---- 11B-2. Within-survey differences ---------------------------------------
  
  survey_ideology_differences <- survey_ideology_means %>%
    dplyr::mutate(
      
      moderate_minus_left_pp =
        100 * (
          far_right_share_Moderate -
            far_right_share_Left
        ),
      
      moderate_minus_right_pp =
        100 * (
          far_right_share_Moderate -
            far_right_share_Right
        ),
      
      right_minus_left_pp =
        100 * (
          far_right_share_Right -
            far_right_share_Left
        )
    )
  
  
  readr::write_csv(
    survey_ideology_differences,
    file.path(
      out_dir_csv,
      "equal_country_ideology_differences_by_survey.csv"
    )
  )
  
  
  # ---- 11B-3. Average surveys equally within each country ----------------------
  
  country_ideology_differences <- survey_ideology_differences %>%
    dplyr::group_by(
      S009_code,
      country_label,
      analysis_group
    ) %>%
    dplyr::summarise(
      
      # Count usable survey-specific contrasts BEFORE replacing them
      # with country-level means.
      n_surveys_moderate_left =
        sum(is.finite(moderate_minus_left_pp)),
      
      n_surveys_moderate_right =
        sum(is.finite(moderate_minus_right_pp)),
      
      n_surveys_right_left =
        sum(is.finite(right_minus_left_pp)),
      
      # Then average survey-specific contrasts equally within country.
      moderate_minus_left_pp =
        safe_mean(moderate_minus_left_pp),
      
      moderate_minus_right_pp =
        safe_mean(moderate_minus_right_pp),
      
      right_minus_left_pp =
        safe_mean(right_minus_left_pp),
      
      .groups = "drop"
    )
  
  
  readr::write_csv(
    country_ideology_differences,
    file.path(
      out_dir_csv,
      "equal_country_ideology_differences_country_level.csv"
    )
  )
  
  equal_country_caption <-
    country_list_caption(
      country_ideology_differences,
      width = 100
    )
  
  # =============================================================================
  # 11B-4. EQUAL-WEIGHT CROSS-COUNTRY AVERAGE + 95% CI
  # =============================================================================
  #
  # Countries are now the units of analysis.
  # The uncertainty interval describes variation across country-level differences.
  
  
  summarise_equal_country_difference <- function(
    x,
    comparison
  ) {
    
    x <- x[
      is.finite(x)
    ]
    
    n <- length(x)
    
    estimate <- if (
      n > 0
    ) {
      mean(x)
    } else {
      NA_real_
    }
    
    se <- if (
      n > 1
    ) {
      stats::sd(x) /
        sqrt(n)
    } else {
      NA_real_
    }
    
    df <- n - 1L
    
    critical_value <- if (
      df > 0
    ) {
      stats::qt(
        0.975,
        df = df
      )
    } else {
      NA_real_
    }
    
    t_stat <- if (
      is.finite(se) &&
      se > 0
    ) {
      estimate / se
    } else {
      NA_real_
    }
    
    p_value <- if (
      is.finite(t_stat) &&
      df > 0
    ) {
      2 * stats::pt(
        abs(t_stat),
        df = df,
        lower.tail = FALSE
      )
    } else {
      NA_real_
    }
    
    tibble::tibble(
      comparison = comparison,
      estimate_pp = estimate,
      std_error_pp = se,
      conf_low_pp = estimate - critical_value * se,
      conf_high_pp = estimate + critical_value * se,
      t_statistic = t_stat,
      p_value = p_value,
      n_countries = n,
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      survey_weighted = USE_SURVEY_WEIGHTS
    )
  }
  
  
  equal_country_summary <- dplyr::bind_rows(
    
    summarise_equal_country_difference(
      country_ideology_differences$moderate_minus_left_pp,
      "Moderate - Left"
    ),
    
    summarise_equal_country_difference(
      country_ideology_differences$moderate_minus_right_pp,
      "Moderate - Right"
    ),
    
    summarise_equal_country_difference(
      country_ideology_differences$right_minus_left_pp,
      "Right - Left"
    )
  )
  
  
  readr::write_csv(
    equal_country_summary,
    file.path(
      out_dir_csv,
      "equal_country_ideology_differences_summary.csv"
    )
  )
  
  
  print(equal_country_summary)
  
  
  # =============================================================================
  # 11B-5. PLOT EQUAL-COUNTRY AVERAGE DIFFERENCES
  # =============================================================================
  #
  # Show two contrasts:
  #
  #   Moderate - Left
  #   Right - Left
  #
  # Diamonds = equal-weight mean across countries.
  # Error bars = 95% confidence intervals.
  #
  # Country-specific points are intentionally omitted.
  
  
  # ---- Prepare summary estimates ----------------------------------------------
  
  equal_country_summary_plot_df <- equal_country_summary %>%
    dplyr::filter(
      comparison %in% c(
        "Moderate - Left",
        "Right - Left"
      )
    ) %>%
    dplyr::mutate(
      comparison = factor(
        comparison,
        levels = c(
          "Moderate - Left",
          "Right - Left"
        )
      )
    )
  
  
  # ---- Plot --------------------------------------------------------------------
  
  p_country_differences_equal_only <- ggplot(
    equal_country_summary_plot_df,
    aes(
      x = comparison,
      y = estimate_pp
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      linewidth = 0.8
    ) +
    geom_errorbar(
      aes(
        ymin = conf_low_pp,
        ymax = conf_high_pp
      ),
      width = 0.08,
      linewidth = 1
    ) +
    geom_point(
      shape = 18,
      size = 5,
      color = "black"
    ) +
    labs(
      title = "Within-country differences in far-right support",
      subtitle = wrap_plot_text(
        paste0(
          "Diamonds = equal-country mean with 95% CI; ",
          IDEOLOGY_SCHEME_LABEL,
          if (USE_SURVEY_WEIGHTS) {
            "; survey-weighted within surveys"
          } else {
            "; unweighted within surveys"
          }
        ),
        width = 85
      ),
      
      caption = paste0(
        wrap_plot_text(
          paste0(
            "Survey-specific differences are averaged equally within country; ",
            "countries are then averaged equally."
          ),
          width = 95
        ),
        "\n\n",
        equal_country_caption
      ),
      x = NULL,
      y = "Difference in far-right support (percentage points)"
    ) +
    THEME_PUZZLE
  
  
  p_country_differences_equal_only
  
  
  # ---- Save --------------------------------------------------------------------
  
  ggsave(
    file.path(
      out_dir_plots,
      "equal_country_ideology_differences_summary_only.png"
    ),
    p_country_differences_equal_only,
    width = 9,
    height = 6.75,
    dpi = 300
  )
  
  # =============================================================================
  # 11C. COMPACT TEXT SUMMARY FOR PAPER / FIGURE CAPTION
  # =============================================================================
  
  lpm_mod_left <- lpm_ideology_contrasts %>%
    dplyr::filter(
      comparison == "Moderate - Left"
    )
  
  lpm_right_mod <- lpm_ideology_contrasts %>%
    dplyr::filter(
      comparison == "Right - Moderate"
    )
  
  equal_mod_left <- equal_country_summary %>%
    dplyr::filter(
      comparison == "Moderate - Left"
    )
  
  equal_mod_right <- equal_country_summary %>%
    dplyr::filter(
      comparison == "Moderate - Right"
    )
  
  
  message("")
  message("------------------------------------------------------------")
  message("IDEOLOGY / FAR-RIGHT DESCRIPTIVE SUMMARY")
  message("------------------------------------------------------------")
  message("Ideology definition: ", IDEOLOGY_SCHEME_LABEL)
  message("")
  message(
    "Country-FE LPM: Moderate - Left = ",
    sprintf(
      "%+.2f pp",
      lpm_mod_left$estimate_pp
    ),
    " (95% CI ",
    sprintf(
      "%.2f",
      lpm_mod_left$conf_low_pp
    ),
    " to ",
    sprintf(
      "%.2f",
      lpm_mod_left$conf_high_pp
    ),
    "; p = ",
    format.pval(
      lpm_mod_left$p_value,
      digits = 3,
      eps = 0.001
    ),
    ")."
  )
  
  message(
    "Country-FE LPM: Right - Moderate = ",
    sprintf(
      "%+.2f pp",
      lpm_right_mod$estimate_pp
    ),
    " (95% CI ",
    sprintf(
      "%.2f",
      lpm_right_mod$conf_low_pp
    ),
    " to ",
    sprintf(
      "%.2f",
      lpm_right_mod$conf_high_pp
    ),
    "; p = ",
    format.pval(
      lpm_right_mod$p_value,
      digits = 3,
      eps = 0.001
    ),
    ")."
  )
  
  message("")
  message(
    "Equal-country mean: Moderate - Left = ",
    sprintf(
      "%+.2f pp",
      equal_mod_left$estimate_pp
    ),
    "."
  )
  
  message(
    "Equal-country mean: Moderate - Right = ",
    sprintf(
      "%+.2f pp",
      equal_mod_right$estimate_pp
    ),
    "."
  )
  
  message("------------------------------------------------------------")
  message("")
  
  # =============================================================================
  # 12. OUTPUT 2 DATA:
  # POLICY DISTRIBUTIONS BY LEFT / MODERATE / RIGHT GROUP
  # =============================================================================
  
  # This uses the ideology_category variable already created earlier in the script.
  # It therefore follows the ideology definitions declared in one place rather
  # than hard-coding separate E033 cutoffs again here.
  
  policy_long_by_ideology <- analysis_micro %>%
    dplyr::filter(
      included_country,
      year >= START_YEAR,
      !is.na(ideology_category)
    ) %>%
    dplyr::select(
      respondent_id,
      S009_code,
      country_label,
      party_source,
      survey_wave,
      year,
      ideology_category,
      survey_weight,
      dplyr::all_of(POLICY_SPECS$var)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(POLICY_SPECS$var),
      names_to = "policy_var",
      values_to = "response_raw"
    ) %>%
    dplyr::left_join(
      POLICY_SPECS,
      by = c("policy_var" = "var")
    ) %>%
    dplyr::mutate(
      ideology_category = factor(
        as.character(ideology_category),
        levels = IDEOLOGY_LEVELS
      ),
      
      # Reverse configured items so all policy variables point in the
      # harmonized direction declared in POLICY_SPECS.
      policy_position = dplyr::if_else(
        reverse,
        min_value + max_value - response_raw,
        response_raw
      )
    ) %>%
    dplyr::filter(
      !is.na(policy_position)
    )
  
  
  # Calculate the response distribution separately within:
  #
  #   country × ideology group × policy item
  #
  # Thus, the bars sum to 100% separately for the Left, Moderate, and Right
  # populations within each policy item.
  
  # First calculate the response distribution separately within each survey wave.
  # Survey weights therefore operate WITHIN surveys rather than allowing larger
  # survey samples to dominate the pooled country distribution.
  
  policy_distribution_by_survey <- policy_long_by_ideology %>%
    dplyr::group_by(
      S009_code,
      country_label,
      party_source,
      survey_wave,
      year,
      ideology_category,
      policy_var,
      label,
      min_value,
      max_value,
      policy_position
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      
      weighted_n = sum(
        survey_weight,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    # Explicitly retain zero-count response categories in each country-survey.
    dplyr::group_by(
      S009_code,
      country_label,
      party_source,
      survey_wave,
      year,
      ideology_category,
      policy_var,
      label,
      min_value,
      max_value
    ) %>%
    dplyr::group_modify(
      ~ tidyr::complete(
        .x,
        policy_position = seq.int(
          from = as.integer(.y$min_value[[1]]),
          to   = as.integer(.y$max_value[[1]])
        ),
        fill = list(
          n = 0L,
          weighted_n = 0
        )
      )
    ) %>%
    dplyr::mutate(
      n_valid_policy = sum(n),
      
      weight_valid_policy = sum(
        weighted_n,
        na.rm = TRUE
      ),
      
      pct_respondents_unweighted = dplyr::if_else(
        n_valid_policy > 0,
        100 * n / n_valid_policy,
        NA_real_
      ),
      
      pct_respondents_weighted = dplyr::if_else(
        weight_valid_policy > 0,
        100 * weighted_n / weight_valid_policy,
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  
  # Then average survey-specific percentages across surveys,
  # giving each survey equal influence in the country-level descriptive distribution.
  
  policy_distribution_by_ideology <- policy_distribution_by_survey %>%
    dplyr::group_by(
      S009_code,
      country_label,
      ideology_category,
      policy_var,
      label,
      min_value,
      max_value,
      policy_position
    ) %>%
    dplyr::summarise(
      n = sum(n),
      weighted_n = sum(weighted_n, na.rm = TRUE),
      
      n_surveys = sum(
        !is.na(pct_respondents_unweighted)
      ),
      
      pct_respondents_unweighted = safe_mean(
        pct_respondents_unweighted
      ),
      
      pct_respondents_weighted = safe_mean(
        pct_respondents_weighted
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_respondents = if (USE_SURVEY_WEIGHTS) {
        pct_respondents_weighted
      } else {
        pct_respondents_unweighted
      }
    ) %>%
    dplyr::arrange(
      ideology_category,
      country_label,
      policy_var,
      policy_position
    )
  
  
  # Export all three ideological groups in one long-form CSV.
  
  readr::write_csv(
    policy_distribution_by_ideology,
    file.path(
      out_dir_csv,
      "policy_position_distributions_by_country_and_ideology.csv"
    )
  )
  
  
  # Optional: also export one CSV per ideological group.
  
  purrr::walk(
    IDEOLOGY_LEVELS,
    function(group_name) {
      
      group_slug <- tolower(group_name)
      
      policy_distribution_by_ideology %>%
        dplyr::filter(
          as.character(ideology_category) == group_name
        ) %>%
        readr::write_csv(
          file.path(
            out_dir_csv,
            paste0(
              group_slug,
              "_policy_position_distributions.csv"
            )
          )
        )
    }
  )
  
  
  # =============================================================================
  # 13. OUTPUT 2:
  # ONE COUNTRY PAGE PER IDEOLOGICAL GROUP
  # =============================================================================
  
  # Text used in country-page titles.
  
  POLICY_GROUP_TITLES <- c(
    "Left"     = "left-identifying respondents",
    "Moderate" = "self-identified moderates",
    "Right"    = "right-identifying respondents"
  )
  
  
  # Reusable PDF-writing function.
  #
  # Calling this function once for each ideology group generates:
  #
  #   left_policy_position_distributions_by_country.pdf
  #   moderate_policy_position_distributions_by_country.pdf
  #   right_policy_position_distributions_by_country.pdf
  
  write_policy_distribution_pdf <- function(group_name) {
    
    if (!group_name %in% IDEOLOGY_LEVELS) {
      stop(
        "Unknown ideology group: ",
        group_name
      )
    }
    
    group_slug <- tolower(group_name)
    group_title <- unname(POLICY_GROUP_TITLES[group_name])
    group_definition <- unname(IDEOLOGY_LABELS[group_name])
    
    pdf_path <- file.path(
      out_dir_plots,
      paste0(
        group_slug,
        "_policy_position_distributions_by_country.pdf"
      )
    )
    
    grDevices::pdf(
      pdf_path,
      width = 11,
      height = 8.5
    )
    
    for (cc in included_country_codes) {
      
      d <- policy_distribution_by_ideology %>%
        dplyr::filter(
          S009_code == cc,
          as.character(ideology_category) == group_name
        )
      
      if (nrow(d) == 0) {
        next
      }
      
      cty_name <- unique(d$country_label)[1]
      
      p <- ggplot(
        d,
        aes(
          x = factor(policy_position),
          y = pct_respondents
        )
      ) +
        geom_col(
          width = 0.8,
          fill = "grey40"
        ) +
        facet_wrap(
          ~ label,
          ncol = 2,
          scales = "free_x"
        ) +
        scale_x_discrete(
          drop = FALSE
        ) +
        scale_y_continuous(
          labels = function(x) paste0(x, "%"),
          expand = expansion(
            mult = c(0, 0.08)
          )
        ) +
        labs(
          title = paste0(
            cty_name,
            ": Policy positions of ",
            group_title
          ),
          subtitle = paste0(
            group_definition,
            "; ",
            START_YEAR,
            " onward; ",
            if (USE_SURVEY_WEIGHTS) {
              "survey-weighted within survey; surveys averaged equally"
            } else {
              "unweighted within survey; surveys averaged equally"
            }
          ),
          x = "Policy position",
          y = paste0(
            "Percentage of ",
            tolower(group_name),
            " respondents"
          ),
          caption = paste0(
            "Configured reversed items are shown in harmonized direction. ",
            "Edit POLICY_SPECS labels before publication."
          )
        ) +
        theme_minimal(
          base_size = 11
        ) +
        theme(
          plot.title = element_text(
            face = "bold"
          ),
          strip.text = element_text(
            face = "bold"
          ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
      
      print(p)
    }
    
    grDevices::dev.off()
    
    invisible(pdf_path)
  }
  
  
  # Generate all three PDFs.
  
  policy_pdf_paths <- purrr::map_chr(
    IDEOLOGY_LEVELS,
    write_policy_distribution_pdf
  )
  
  tibble::tibble(
    ideology_category = IDEOLOGY_LEVELS,
    pdf_file = policy_pdf_paths
  ) %>%
    print(n = Inf)
  
  # =============================================================================
  # 16. FINAL DIAGNOSTICS FOR THIS IDEOLOGY RUN
  # =============================================================================
  
  ideology_check <- analysis_micro %>%
    dplyr::filter(!is.na(E033_num)) %>%
    dplyr::summarise(
      ideology_scheme = IDEOLOGY_SCHEME,
      ideology_definition = IDEOLOGY_SCHEME_LABEL,
      n_valid_e033 = dplyr::n(),
      n_classified = sum(!is.na(ideology_category)),
      pct_classified = 100 * n_classified / n_valid_e033
    )
  
  print(ideology_check)
  
  message("")
  message("Ideology run complete.")
  message("Ideology scheme:       ", IDEOLOGY_SCHEME_LABEL)
  message("Canonical run micro:   ", file.path(out_dir_data, "analysis_micro.rds"))
  message("CSV outputs:           ", normalizePath(out_dir_csv))
  message("Plot/PDF outputs:      ", normalizePath(out_dir_plots))
  
} # end ideology-scheme loop

# Restore the ideology-neutral canonical object in the interactive environment.
analysis_micro <- analysis_micro_base

message("")
message("============================================================")
message("Pipeline complete for BOTH ideology definitions.")
message("Shared party/sample audits: ", file.path(OUT_ROOT, "common"))
message("Narrow outputs: ", file.path(OUT_ROOT, "ideology_1_2_vs_5_6_vs_9_10"))
message("Broad outputs:  ", file.path(OUT_ROOT, "ideology_lt5_vs_5_6_vs_gt6"))
message("============================================================")
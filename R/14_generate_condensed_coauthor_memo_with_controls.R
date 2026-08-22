# ============================================================
# 14_generate_condensed_coauthor_memo_with_controls.R
# Exact five-column publication tables for the coauthor empirical memo.
#
# Generates 12 LaTeX tables:
#   Aggregate AC-segment outcome (8):
#     Muslim 2001: 2-way / 3-way x first-difference / lagged-outcome
#     Established migration: 2-way / 3-way x first-difference / lagged-outcome
#   Respondent weighted LPM (4):
#     Muslim 2001: 2-way / 3-way
#     Established migration: 2-way / 3-way
#
# IMPORTANT DESIGN PRINCIPLES
# ---------------------------
# * No pooled-sample tables are produced.
# * Aggregate dynamic models always retain matched baseline (2009) FDI;
#   lagged-outcome models additionally retain 2009 BJP vote share.
# * Respondent 2014 models always retain matched baseline (2009) FDI and
#   2009 BJP vote share.
# * Model 4 is the preferred inferential specification.
# * Model 5 adds the full C3 block; C3 nests C2.
# * Exact p-values are printed alongside coefficient and SE.
# * Aggregate triple models use the preferred weighted 2009 AC Center share
#   and require >=5 ideology-complete 2009 NES respondents.
# * Respondent triple models use center_harmonized.
# * This script validates Model 4's focal coefficient against the frozen
#   specification-curve result whenever that frozen file is available.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
  library(purrr)
  library(stringr)
  library(tibble)
})

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

helper_path <- file.path(project_root, "R", "helpers.R")
if (!file.exists(helper_path)) {
  stop("Cannot find R/helpers.R under SWITCHERS_ROOT: ", project_root)
}
source(helper_path)
paths <- build_project_paths(project_root)

TABLE_REVISION <- "2026-08-10-v2.0-condensed-memo-all-control-coefficients-stars"
message("Starting coauthor publication tables: ", TABLE_REVISION)

out_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "coauthor_empirical_memo"
)
out_table_dir <- file.path(out_root, "tables")
out_manifest_dir <- file.path(out_root, "manifests")
dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_manifest_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. LOAD FINAL ANALYSIS DATA
# ============================================================

ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))
respondents <- readRDS(file.path(paths$final_dir, "nes_respondent_analysis.rds"))

# Reconstruct the baseline NES reliability denominator if ac_change does not
# carry it. This mirrors the repaired v5.0 / cross-level synthesis logic.
if (!"nes_n_ideology_complete_2009" %in% names(ac_change)) {
  ac_year <- readRDS(file.path(paths$final_dir, "ac_year.rds"))
  center_n_2009 <- ac_year |>
    filter(year == 2009) |>
    transmute(
      ac_uid,
      nes_n_ideology_complete_2009 = nes_n_ideology_complete
    ) |>
    distinct(ac_uid, .keep_all = TRUE)
  ac_change <- ac_change |>
    left_join(center_n_2009, by = "ac_uid", relationship = "one-to-one")
}

# Repaired employment intensity used by v5.0 C2/C3.
if (!"log1p_employment_per_total_population" %in% names(ac_change) &&
    "employment_per_total_population" %in% names(ac_change)) {
  ac_change <- ac_change |>
    mutate(
      log1p_employment_per_total_population =
        log1p(employment_per_total_population)
    )
}
if (!"log1p_employment_per_total_population" %in% names(respondents) &&
    "employment_per_total_population" %in% names(respondents)) {
  respondents <- respondents |>
    mutate(
      log1p_employment_per_total_population =
        log1p(employment_per_total_population)
    )
}

# Join baseline variables to respondent rows if needed, exactly as in the
# respondent post-estimation audit.
respondent_baseline_needed <- c(
  "bjp_vote_share_2009",
  "log1p_fdi_mfg_local_all_pc100k_2009"
)
missing_resp_baseline <- setdiff(respondent_baseline_needed, names(respondents))
if (length(missing_resp_baseline) > 0) {
  baseline_payload <- ac_change |>
    select(ac_uid, all_of(missing_resp_baseline)) |>
    distinct(ac_uid, .keep_all = TRUE)
  respondents <- respondents |>
    left_join(baseline_payload, by = "ac_uid", relationship = "many-to-one")
}

# ------------------------------------------------------------
# 1C. Reconstruct respondent-derived flags if the final RDS does not persist them
# ------------------------------------------------------------
#
# The respondent specification-curve runner constructed these variables in
# memory. The final nes_respondent_analysis.rds used by later audits can omit
# them, so reconstruct them here from their frozen definitions rather than
# requiring them to be physically stored in the RDS.
#
# IMPORTANT:
# * These publication tables use only the 2014 baseline-adjusted respondent
#   design.
# * In 2014, the frozen center_harmonized definition is exactly identical to
#   strict Center among ideology-complete respondents.
# * The candidate-present sample definition is:
#       vote_valid & !is.na(voted_bjp) &
#       !is.na(bjp_candidate_present) & bjp_candidate_present == 1
#

if (!"respondent_sample_candidate_present" %in% names(respondents)) {
  required_for_candidate_flag <- c(
    "vote_valid",
    "voted_bjp",
    "bjp_candidate_present"
  )

  missing_candidate_inputs <- setdiff(
    required_for_candidate_flag,
    names(respondents)
  )

  if (length(missing_candidate_inputs) > 0) {
    stop(
      "Cannot reconstruct respondent_sample_candidate_present because ",
      "nes_respondent_analysis.rds is missing: ",
      paste(missing_candidate_inputs, collapse = ", ")
    )
  }

  respondents <- respondents |>
    dplyr::mutate(
      respondent_sample_candidate_present =
        vote_valid &
        !is.na(voted_bjp) &
        !is.na(bjp_candidate_present) &
        bjp_candidate_present == 1
    )

  message(
    "Reconstructed respondent_sample_candidate_present from the frozen ",
    "vote-valid / BJP-candidate-present definition."
  )
}

if (!"center_harmonized" %in% names(respondents)) {
  required_for_center <- c(
    "year",
    "ideology_complete",
    "voter_ideology"
  )

  missing_center_inputs <- setdiff(
    required_for_center,
    names(respondents)
  )

  if (length(missing_center_inputs) > 0) {
    stop(
      "Cannot reconstruct center_harmonized for the 2014 respondent tables ",
      "because nes_respondent_analysis.rds is missing: ",
      paste(missing_center_inputs, collapse = ", ")
    )
  }

  respondents <- respondents |>
    dplyr::mutate(
      center_harmonized =
        dplyr::case_when(
          year == 2014 &
            ideology_complete &
            as.character(voter_ideology) == "Center" ~
            1,

          year == 2014 &
            ideology_complete ~
            0,

          TRUE ~
            NA_real_
        )
    )

  message(
    "Reconstructed center_harmonized for 2014. This is exactly the frozen ",
    "2014 harmonized/strict Center definition."
  )
}

# Guard the 2014 reconstruction logic if a strict Center variable is already
# persisted in the final RDS.
if ("center_strict" %in% names(respondents)) {
  center_guard <- respondents |>
    dplyr::filter(
      year == 2014,
      ideology_complete
    ) |>
    dplyr::summarise(
      n_mismatch =
        sum(
          center_harmonized != center_strict,
          na.rm = TRUE
        )
    ) |>
    dplyr::pull(n_mismatch)

  if (!is.na(center_guard) && center_guard != 0) {
    stop(
      "2014 center_harmonized reconstruction does not match center_strict. ",
      "Do not proceed with publication tables until the respondent data are audited."
    )
  }
}

# ============================================================
# 2. FROZEN VARIABLE DEFINITIONS
# ============================================================

fdi_agg <- "log1p_fdi_mfg_local_all_pc100k_2014"
fdi_agg_base <- "log1p_fdi_mfg_local_all_pc100k_2009"
fdi_resp <- "log1p_fdi_mfg_local_all_pc100k"
fdi_resp_base <- "log1p_fdi_mfg_local_all_pc100k_2009"

muslim_var <- "muslim_share_2001_dist_proxy"
migration_var <- "mig_total_upto_2001_share_ac_pop"
center_agg <- "nes_weighted_share_center_among_ideology_complete_2009"
center_agg_n <- "nes_n_ideology_complete_2009"
center_resp <- "center_harmonized"

C0 <- c("proxy_ac_pop", "con08_land_area")
C1 <- c(C0, "sc_pop_share", "st_pop_share")
C2 <- c(C1, "log1p_employment_per_total_population", "ed_sec_share")
C3 <- c(C2, "log_secc_cons_pc")
V2 <- c("religion_group", "caste_group", "education_harmonized")

# Model 5 uses C3; C3 already contains all C2 variables.

required_ac <- unique(c(
  "ac_uid", "state_no", "pc_cluster_id",
  "d_bjp_vote_share_2009_2014_pp", "bjp_vote_share_2014",
  "bjp_vote_share_2009", fdi_agg, fdi_agg_base,
  muslim_var, migration_var, center_agg, center_agg_n, C3
))
missing_ac <- setdiff(required_ac, names(ac_change))
if (length(missing_ac) > 0) {
  stop("ac_change.rds is missing required variables: ", paste(missing_ac, collapse = ", "))
}

required_resp <- unique(c(
  "year", "voted_bjp", "respondent_sample_candidate_present",
  "ideology_complete", center_resp, "state_no", "pc_cluster_id",
  "district_harmonization_group_id", "survey_weight_norm_year",
  "bjp_vote_share_2009", fdi_resp, fdi_resp_base,
  muslim_var, migration_var, V2, C3
))
missing_resp <- setdiff(required_resp, names(respondents))
if (length(missing_resp) > 0) {
  stop("nes_respondent_analysis.rds is missing required variables: ", paste(missing_resp, collapse = ", "))
}

# ============================================================
# 3. MODEL DEFINITIONS
# ============================================================

aggregate_models <- tibble::tribble(
  ~model_id, ~model_label, ~context_controls, ~state_fe, ~clustered,
  1L, "(1) Interaction only", list(character(0)), FALSE, FALSE,
  2L, "(2) + C0",              list(C0),           FALSE, FALSE,
  3L, "(3) + C1",              list(C1),           FALSE, FALSE,
  4L, "(4) Preferred",         list(C1),           TRUE,  TRUE,
  5L, "(5) + C3",              list(C3),           TRUE,  TRUE
)

respondent_models <- tibble::tribble(
  ~model_id, ~model_label, ~voter_controls, ~context_controls, ~state_fe, ~clustered,
  1L, "(1) Interaction only", list(character(0)), list(character(0)), FALSE, FALSE,
  2L, "(2) + C0",              list(character(0)), list(C0),           FALSE, FALSE,
  3L, "(3) + V2 + C1",         list(V2),           list(C1),           FALSE, FALSE,
  4L, "(4) Preferred",         list(V2),           list(C1),           TRUE,  TRUE,
  5L, "(5) + C3",              list(V2),           list(C3),           TRUE,  TRUE
)

aggregate_specs <- tidyr::crossing(
  domain = c("muslim", "migration"),
  design = c("first_difference", "lagged_outcome"),
  interaction_order = c("two_way", "triple")
) |>
  mutate(
    moderator_var = if_else(domain == "muslim", muslim_var, migration_var),
    domain_label = if_else(
      domain == "muslim",
      "Muslim population share, 2001",
      "Established migrant stock share (resident by 2001)"
    ),
    outcome = if_else(
      design == "first_difference",
      "d_bjp_vote_share_2009_2014_pp",
      "bjp_vote_share_2014"
    ),
    outcome_label = if_else(
      design == "first_difference",
      "Change in BJP vote share, 2009--2014 (percentage points)",
      "BJP vote share, 2014"
    ),
    design_label = if_else(
      design == "first_difference",
      "First-difference",
      "Lagged-outcome"
    )
  )

respondent_specs <- tidyr::crossing(
  domain = c("muslim", "migration"),
  interaction_order = c("two_way", "triple")
) |>
  mutate(
    moderator_var = if_else(domain == "muslim", muslim_var, migration_var),
    domain_label = if_else(
      domain == "muslim",
      "Muslim population share, 2001",
      "Established migrant stock share (resident by 2001)"
    )
  )

# ============================================================
# 4. HELPERS
# ============================================================

collapse_rhs <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  paste(x, collapse = " + ")
}

make_formula <- function(outcome, interaction_text, always_controls,
                         controls, state_fe) {
  rhs <- collapse_rhs(c(interaction_text, always_controls, controls))
  txt <- paste0(outcome, " ~ ", rhs)
  if (isTRUE(state_fe)) txt <- paste0(txt, " | state_no")
  stats::as.formula(txt)
}

fit_aggregate_model <- function(spec, model_row) {
  d <- ac_change
  if (spec$interaction_order == "triple") {
    d <- d |>
      filter(
        !is.na(.data[[center_agg_n]]),
        .data[[center_agg_n]] >= 5,
        !is.na(.data[[center_agg]])
      )
  }

  interaction_text <- if (spec$interaction_order == "two_way") {
    paste0(fdi_agg, " * ", spec$moderator_var)
  } else {
    paste0(fdi_agg, " * ", spec$moderator_var, " * ", center_agg)
  }

  always_controls <- c(fdi_agg_base)
  if (spec$design == "lagged_outcome") {
    always_controls <- c(always_controls, "bjp_vote_share_2009")
  }

  formula <- make_formula(
    spec$outcome,
    interaction_text,
    always_controls,
    unlist(model_row$context_controls),
    model_row$state_fe
  )

  vc <- if (isTRUE(model_row$clustered)) {
    ~pc_cluster_id
  } else {
    "hetero"
  }

  fixest::feols(
    formula,
    data = d,
    vcov = vc,
    notes = FALSE,
    warn = FALSE
  )
}

fit_respondent_model <- function(spec, model_row) {
  d <- respondents |>
    filter(
      year == 2014,
      respondent_sample_candidate_present,
      !is.na(survey_weight_norm_year),
      survey_weight_norm_year > 0
    )

  if (spec$interaction_order == "triple") {
    d <- d |>
      filter(
        ideology_complete,
        !is.na(.data[[center_resp]])
      )
  }

  interaction_text <- if (spec$interaction_order == "two_way") {
    paste0(fdi_resp, " * ", spec$moderator_var)
  } else {
    paste0(fdi_resp, " * ", spec$moderator_var, " * ", center_resp)
  }

  formula <- make_formula(
    "voted_bjp",
    interaction_text,
    c(fdi_resp_base, "bjp_vote_share_2009"),
    c(unlist(model_row$voter_controls), unlist(model_row$context_controls)),
    model_row$state_fe
  )

  vc <- if (isTRUE(model_row$clustered)) {
    ~pc_cluster_id + district_harmonization_group_id
  } else {
    "hetero"
  }

  fixest::feols(
    formula,
    data = d,
    weights = ~survey_weight_norm_year,
    vcov = vc,
    notes = FALSE,
    warn = FALSE
  )
}

normalize_interaction_term <- function(term) {
  parts <- strsplit(gsub("`", "", term, fixed = TRUE), ":", fixed = TRUE)[[1]]
  paste(sort(parts), collapse = ":")
}

find_interaction_term <- function(fit, vars) {
  cn <- names(coef(fit))
  target <- paste(sort(vars), collapse = ":")
  normalized <- vapply(cn, normalize_interaction_term, character(1))
  hit <- cn[normalized == target]
  if (length(hit) == 1) hit[[1]] else NA_character_
}

pretty_coef_label <- function(term, main_lookup = NULL) {
  if (!is.null(main_lookup) && term %in% names(main_lookup)) {
    return(unname(main_lookup[[term]]))
  }

  exact <- c(
    "proxy_ac_pop" = "AC population (2011 proxy)",
    "con08_land_area" = "AC land area",
    "sc_pop_share" = "Scheduled Caste population share",
    "st_pop_share" = "Scheduled Tribe population share",
    "log1p_employment_per_total_population" = "Log(1 + employment / population)",
    "ed_sec_share" = "Secondary-education share",
    "log_secc_cons_pc" = "Log per-capita consumption"
  )
  if (term %in% names(exact)) return(unname(exact[[term]]))

  # fixest names factor coefficients as variable::level or variablelevel,
  # depending on how the source variable is stored. Preserve every estimable
  # category coefficient rather than merely reporting that V2 was included.
  factor_prefixes <- c(
    "religion_group" = "Religion",
    "caste_group" = "Caste",
    "education_harmonized" = "Education"
  )
  for (v in names(factor_prefixes)) {
    if (startsWith(term, paste0(v, "::"))) {
      lev <- sub(paste0("^", v, "::"), "", term)
      return(paste0(factor_prefixes[[v]], ": ", gsub("&", "\\&", lev, fixed = TRUE)))
    }
    if (startsWith(term, v) && term != v) {
      lev <- sub(paste0("^", v), "", term)
      lev <- sub("^[:=._]+", "", lev)
      return(paste0(factor_prefixes[[v]], ": ", gsub("&", "\\&", lev, fixed = TRUE)))
    }
  }

  # Fall back to a readable raw coefficient name.
  out <- gsub("_", " ", term, fixed = TRUE)
  out <- gsub("::", ": ", out, fixed = TRUE)
  out <- gsub("&", "\\&", out, fixed = TRUE)
  out <- gsub("%", "\\%", out, fixed = TRUE)
  out
}

extract_fit_rows <- function(fit, term_defs, model_id, model_label) {
  ct <- as.data.frame(coeftable(fit))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct)[1:4] <- c("estimate", "se", "t", "p")

  # Build the focal/design-term rows first in the exact order used previously.
  focal <- purrr::map_dfr(seq_len(nrow(term_defs)), function(i) {
    vars <- term_defs$vars[[i]]
    is_interaction <- length(vars) > 1
    term <- if (is_interaction) find_interaction_term(fit, vars) else vars[[1]]
    row <- ct |> dplyr::filter(.data$term == !!term)
    if (nrow(row) != 1) {
      tibble(
        term_key = term_defs$term_key[[i]],
        raw_term = term,
        term_label = term_defs$term_label[[i]],
        term_type = "focal_or_design",
        model_id = model_id,
        model_label = model_label,
        estimate = NA_real_, se = NA_real_, p = NA_real_
      )
    } else {
      tibble(
        term_key = term_defs$term_key[[i]],
        raw_term = term,
        term_label = term_defs$term_label[[i]],
        term_type = "focal_or_design",
        model_id = model_id,
        model_label = model_label,
        estimate = row$estimate[[1]], se = row$se[[1]], p = row$p[[1]]
      )
    }
  })

  used_terms <- focal$raw_term[!is.na(focal$raw_term)]

  # Then append EVERY remaining estimable coefficient except the intercept.
  # This includes all continuous C0--C3 controls and every estimable V2 factor
  # category. State fixed-effect coefficients remain absorbed/suppressed.
  controls <- ct |>
    dplyr::filter(
      term != "(Intercept)",
      !term %in% used_terms
    ) |>
    dplyr::mutate(
      term_key = paste0("control__", term),
      raw_term = term,
      term_label = vapply(term, pretty_coef_label, character(1)),
      term_type = "control",
      model_id = model_id,
      model_label = model_label
    ) |>
    dplyr::select(
      term_key, raw_term, term_label, term_type,
      model_id, model_label, estimate, se, p
    )

  dplyr::bind_rows(focal, controls)
}

term_definitions <- function(moderator_var, domain_label, interaction_order,
                             respondent = FALSE, lagged = FALSE) {
  fdi <- if (respondent) fdi_resp else fdi_agg
  fdi_base <- if (respondent) fdi_resp_base else fdi_agg_base
  center <- if (respondent) center_resp else center_agg
  center_label <- if (respondent) {
    "Center respondent"
  } else {
    "2009 constituency Center share"
  }

  rows <- list(
    tibble(term_key = "fdi", term_label = "Manufacturing FDI, 2009--14", vars = list(c(fdi))),
    tibble(term_key = "demo", term_label = domain_label, vars = list(c(moderator_var)))
  )

  if (interaction_order == "triple") {
    rows <- c(rows, list(
      tibble(term_key = "center", term_label = center_label, vars = list(c(center))),
      tibble(term_key = "fdi_demo", term_label = "FDI $\\times$ demographic context", vars = list(c(fdi, moderator_var))),
      tibble(term_key = "fdi_center", term_label = "FDI $\\times$ Center", vars = list(c(fdi, center))),
      tibble(term_key = "demo_center", term_label = "Demographic context $\\times$ Center", vars = list(c(moderator_var, center))),
      tibble(term_key = "triple", term_label = "FDI $\\times$ demographic context $\\times$ Center", vars = list(c(fdi, moderator_var, center)))
    ))
  } else {
    rows <- c(rows, list(
      tibble(term_key = "fdi_demo", term_label = "FDI $\\times$ demographic context", vars = list(c(fdi, moderator_var)))
    ))
  }

  rows <- c(rows, list(
    tibble(term_key = "fdi_base", term_label = "Manufacturing FDI, 2004--09", vars = list(c(fdi_base)))
  ))

  if (respondent || lagged) {
    rows <- c(rows, list(
      tibble(term_key = "bjp_base", term_label = "BJP vote share, 2009", vars = list(c("bjp_vote_share_2009")))
    ))
  }

  bind_rows(rows)
}

fmt_num <- function(x) {
  ifelse(
    is.na(x),
    "",
    ifelse(abs(x) >= 100, sprintf("%.1f", x),
      ifelse(abs(x) >= 10, sprintf("%.2f", x), sprintf("%.3f", x)))
  )
}
fmt_se <- function(x) {
  ifelse(is.na(x), "", ifelse(abs(x) >= 10, sprintf("%.2f", x), sprintf("%.3f", x)))
}
fmt_p <- function(x) {
  ifelse(
    is.na(x),
    "",
    ifelse(x < 0.0001, "$p<0.0001$", paste0("$p=", sprintf("%.4f", x), "$"))
  )
}
sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}
fmt_cell <- function(est, se, p) {
  if (is.na(est)) return("")
  stars <- sig_stars(p)
  star_tex <- if (nzchar(stars)) paste0("$^{", stars, "}$") else ""
  paste0(
    "\\shortstack{", fmt_num(est), star_tex, "\\\\(", fmt_se(se), ")\\\\{\\scriptsize ", fmt_p(p), "}}"
  )
}

latex_escape_plain <- function(x) {
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

write_publication_table <- function(results, fits, spec, respondent = FALSE) {
  table_id <- if (respondent) {
    paste("respondent", spec$domain, spec$interaction_order, sep = "_")
  } else {
    paste("aggregate", spec$domain, spec$interaction_order, spec$design, sep = "_")
  }

  caption <- if (respondent) {
    paste0(
      "Respondent-level weighted LPM: manufacturing FDI, ",
      spec$domain_label,
      ", ",
      ifelse(spec$interaction_order == "two_way", "two-way interaction", "three-way interaction with Center")
    )
  } else {
    paste0(
      spec$design_label,
      " AC-segment model: manufacturing FDI, ",
      spec$domain_label,
      ", ",
      ifelse(spec$interaction_order == "two_way", "two-way interaction", "three-way interaction with Center")
    )
  }

  term_labels <- results |>
    distinct(term_key, term_label, term_type, raw_term) |>
    mutate(
      focal_order = match(term_key, c(
        "fdi", "demo", "center", "fdi_demo", "fdi_center",
        "demo_center", "triple", "fdi_base", "bjp_base"
      )),
      focal_order = if_else(is.na(focal_order), 999L, focal_order),
      control_order = case_when(
        raw_term == "proxy_ac_pop" ~ 1L,
        raw_term == "con08_land_area" ~ 2L,
        raw_term == "sc_pop_share" ~ 3L,
        raw_term == "st_pop_share" ~ 4L,
        raw_term == "log1p_employment_per_total_population" ~ 5L,
        raw_term == "ed_sec_share" ~ 6L,
        raw_term == "log_secc_cons_pc" ~ 7L,
        startsWith(raw_term, "religion_group") ~ 20L,
        startsWith(raw_term, "caste_group") ~ 30L,
        startsWith(raw_term, "education_harmonized") ~ 40L,
        TRUE ~ 50L
      )
    )

  wide <- results |>
    mutate(cell = pmap_chr(list(estimate, se, p), fmt_cell)) |>
    select(term_key, term_label, model_id, cell) |>
    tidyr::pivot_wider(names_from = model_id, values_from = cell, names_prefix = "m") |>
    left_join(term_labels, by = c("term_key", "term_label")) |>
    arrange(term_type == "control", focal_order, control_order, term_label)

  model_labels <- if (respondent) respondent_models$model_label else aggregate_models$model_label

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{tab:", table_id, "}"),
    "\\setlength{\\tabcolsep}{4.2pt}",
    "\\renewcommand{\\arraystretch}{1.05}",
    "\\begin{threeparttable}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    paste0(" & ", paste(model_labels, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (i in seq_len(nrow(wide))) {
    cells <- vapply(1:5, function(m) {
      col <- paste0("m", m)
      if (col %in% names(wide)) wide[[col]][[i]] else ""
    }, character(1))
    lines <- c(lines, paste0(wide$term_label[[i]], " & ", paste(cells, collapse = " & "), " \\\\"))
  }

  lines <- c(lines, "\\midrule")

  if (respondent) {
    stat_rows <- list(
      c("State fixed effects", "No", "No", "No", "Yes", "Yes"),
      c("Survey weights", "Yes", "Yes", "Yes", "Yes", "Yes"),
      c("Candidate-present sample", "Yes", "Yes", "Yes", "Yes", "Yes"),
      c("Standard errors", "HC1", "HC1", "HC1", "PC $\\times$ district", "PC $\\times$ district")
    )
  } else {
    stat_rows <- list(
      c("State fixed effects", "No", "No", "No", "Yes", "Yes"),
      c("Standard errors", "HC1", "HC1", "HC1", "PC-clustered", "PC-clustered")
    )
    if (spec$interaction_order == "triple") {
      stat_rows <- append(
        stat_rows,
        list(c("2009 NES Center reliability", "$N\\geq5$", "$N\\geq5$", "$N\\geq5$", "$N\\geq5$", "$N\\geq5$")),
        after = 1
      )
    }
  }

  for (r in stat_rows) {
    lines <- c(lines, paste0(r[[1]], " & ", paste(r[2:6], collapse = " & "), " \\\\"))
  }

  ns <- vapply(fits, nobs, numeric(1))
  r2s <- vapply(fits, function(f) {
    out <- tryCatch(fixest::fitstat(f, "r2")[[1]], error = function(e) NA_real_)
    as.numeric(out)
  }, numeric(1))
  lines <- c(
    lines,
    paste0("Observations & ", paste(format(round(ns), big.mark = ",", scientific = FALSE), collapse = " & "), " \\\\"),
    paste0("$R^2$ & ", paste(ifelse(is.na(r2s), "", sprintf("%.3f", r2s)), collapse = " & "), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\begin{tablenotes}[flushleft]",
    "\\scriptsize"
  )

  if (respondent) {
    note <- paste0(
      "\\item Notes: Outcome is an indicator for voting BJP in 2014 among valid voters in constituencies where the BJP fielded a candidate. ",
      "All columns are survey-weighted linear probability models. The design-defining 2009 manufacturing-FDI exposure and 2009 BJP vote share are included in every column and are shown above; they are not counted as C-block controls. ",
      "Model 4 is the preferred specification: V2 voter controls (religion, caste, education), C1 contextual controls, state fixed effects, and multiway standard errors clustered by parliamentary constituency and district harmonization group. ",
      "Model 5 retains Model 4 and adds C3; C3 nests C2. All estimable C0--C3 and V2 control coefficients are reported in the body of the table; absorbed state fixed-effect coefficients and omitted factor reference categories are not. ",
      ifelse(spec$interaction_order == "triple",
        "The Center moderator is center_harmonized; in 2014 this equals the strict Center definition among ideology-complete respondents. ", ""),
      "Cells report coefficient, standard error in parentheses, and exact p-value. Significance: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    )
  } else {
    note <- paste0(
      "\\item Notes: Manufacturing FDI is local exposure (own AC plus touching ACs), all announced/opened projects, log(1 + projects per 100,000 residents). ",
      "Every dynamic specification includes matched 2004--09 manufacturing FDI as a design-defining baseline term; lagged-outcome models additionally include 2009 BJP vote share. These terms are not counted as C-block controls. ",
      "Model 4 is the preferred specification with C1, state fixed effects, and parliamentary-constituency-clustered standard errors. Model 5 retains Model 4 and adds C3; C3 nests C2. All estimable C0--C3 control coefficients are reported in the body of the table; absorbed state fixed-effect coefficients are not. ",
      ifelse(spec$interaction_order == "triple",
        "Triple models use the survey-weighted 2009 constituency Center share among ideology-complete NES respondents and restrict to ACs with at least five such respondents. ", ""),
      "Cells report coefficient, standard error in parentheses, and exact p-value. Significance: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    )
  }

  lines <- c(
    lines,
    note,
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}",
    ""
  )

  out_file <- file.path(out_table_dir, paste0(table_id, ".tex"))
  writeLines(lines, out_file)
  out_file
}

# ============================================================
# 5. FROZEN-MODEL VALIDATION
# ============================================================

aggregate_result_revision <- "2026-08-07-v4.3-preferred-n5"
respondent_result_revision <- "2026-08-08-v1.0.3-targeted-robustness-hotfix"

validate_against_frozen <- function(spec, fit, respondent = FALSE, tol = 1e-6) {
  if (respondent) {
    path <- file.path(
      paths$derived_dir, "model_exploration", "respondent_specification_curves", "results",
      paste0(
        "primary__respondent_2014_", spec$domain,
        "__mfg__", spec$interaction_order,
        "__full__", respondent_result_revision, ".csv"
      )
    )
    if (!file.exists(path)) return(tibble(status = "frozen file not found", difference = NA_real_))
    frozen <- read_csv(path, show_col_types = FALSE, progress = FALSE) |>
      filter(
        fdi_scope == "local", fdi_status == "all", fdi_form == "log1p_pc100k",
        .data$moderator_var == !!spec$moderator_var,
        voter_control_set == "V2", context_control_set == "C1"
      )
    if (nrow(frozen) != 1) stop("Frozen respondent validation row not unique for ", spec$domain, "/", spec$interaction_order)
    vars <- if (spec$interaction_order == "two_way") c(fdi_resp, spec$moderator_var) else c(fdi_resp, spec$moderator_var, center_resp)
  } else {
    path <- file.path(
      paths$derived_dir, "model_exploration", "specification_curves", "results",
      paste0(
        spec$design, "_", spec$domain,
        "__mfg__", spec$interaction_order,
        "__full__", aggregate_result_revision, ".csv"
      )
    )
    if (!file.exists(path)) return(tibble(status = "frozen file not found", difference = NA_real_))
    frozen <- read_csv(path, show_col_types = FALSE, progress = FALSE) |>
      filter(
        fdi_scope == "local", fdi_status == "all", fdi_form == "log1p_pc100k",
        .data$moderator_var == !!spec$moderator_var,
        control_set == "C1"
      )
    if (spec$interaction_order == "triple") {
      frozen <- frozen |>
        filter(
          center_var == center_agg,
          center_min_n == 5
        )
    }
    if (nrow(frozen) != 1) stop("Frozen aggregate validation row not unique for ", spec$domain, "/", spec$design, "/", spec$interaction_order)
    vars <- if (spec$interaction_order == "two_way") c(fdi_agg, spec$moderator_var) else c(fdi_agg, spec$moderator_var, center_agg)
  }

  term <- find_interaction_term(fit, vars)
  est <- unname(coef(fit)[term])
  diff <- est - frozen$interaction_estimate[[1]]
  if (!is.finite(diff) || abs(diff) > tol) {
    stop(
      "Model 4 does not reproduce frozen focal coefficient. Difference=", diff,
      ". Check analysis data/revision before using tables."
    )
  }
  tibble(status = "PASS", difference = diff)
}

# ============================================================
# 6. FIT ALL 60 MODELS + WRITE 12 TABLES
# ============================================================

all_coefficients <- list()
all_diagnostics <- list()
table_files <- character(0)
idx <- 0L

for (s in seq_len(nrow(aggregate_specs))) {
  spec <- aggregate_specs[s, , drop = FALSE]
  fits <- vector("list", 5)
  model_rows <- vector("list", 5)

  for (m in seq_len(nrow(aggregate_models))) {
    mr <- aggregate_models[m, , drop = FALSE]
    fits[[m]] <- fit_aggregate_model(spec, mr)
    tdefs <- term_definitions(
      spec$moderator_var,
      spec$domain_label,
      spec$interaction_order,
      respondent = FALSE,
      lagged = spec$design == "lagged_outcome"
    )
    model_rows[[m]] <- extract_fit_rows(fits[[m]], tdefs, mr$model_id, mr$model_label)
  }

  rows <- bind_rows(model_rows) |>
    mutate(
      evidence_level = "aggregate",
      domain = spec$domain,
      design = spec$design,
      interaction_order = spec$interaction_order
    )

  idx <- idx + 1L
  all_coefficients[[idx]] <- rows

  val <- validate_against_frozen(spec, fits[[4]], respondent = FALSE)
  all_diagnostics[[length(all_diagnostics) + 1L]] <- val |>
    mutate(
      evidence_level = "aggregate",
      domain = spec$domain,
      design = spec$design,
      interaction_order = spec$interaction_order
    )

  table_files <- c(table_files, write_publication_table(rows, fits, spec, respondent = FALSE))
}

for (s in seq_len(nrow(respondent_specs))) {
  spec <- respondent_specs[s, , drop = FALSE]
  fits <- vector("list", 5)
  model_rows <- vector("list", 5)

  for (m in seq_len(nrow(respondent_models))) {
    mr <- respondent_models[m, , drop = FALSE]
    fits[[m]] <- fit_respondent_model(spec, mr)
    tdefs <- term_definitions(
      spec$moderator_var,
      spec$domain_label,
      spec$interaction_order,
      respondent = TRUE,
      lagged = TRUE
    )
    model_rows[[m]] <- extract_fit_rows(fits[[m]], tdefs, mr$model_id, mr$model_label)
  }

  rows <- bind_rows(model_rows) |>
    mutate(
      evidence_level = "respondent",
      domain = spec$domain,
      design = "2014_baseline_adjusted",
      interaction_order = spec$interaction_order
    )

  idx <- idx + 1L
  all_coefficients[[idx]] <- rows

  val <- validate_against_frozen(spec, fits[[4]], respondent = TRUE)
  all_diagnostics[[length(all_diagnostics) + 1L]] <- val |>
    mutate(
      evidence_level = "respondent",
      domain = spec$domain,
      design = "2014_baseline_adjusted",
      interaction_order = spec$interaction_order
    )

  table_files <- c(table_files, write_publication_table(rows, fits, spec, respondent = TRUE))
}

coefficient_output <- bind_rows(all_coefficients)
validation_output <- bind_rows(all_diagnostics)

write_csv(
  coefficient_output,
  file.path(out_root, "all_12_publication_table_coefficients.csv")
)
write_csv(
  validation_output,
  file.path(out_manifest_dir, "model4_frozen_reproduction_audit.csv")
)

manifest <- tibble(
  table_revision = TABLE_REVISION,
  table_file = table_files,
  generated_at = as.character(Sys.time())
)
write_csv(manifest, file.path(out_manifest_dir, "table_manifest.csv"))

# ============================================================
# 7. WRITE A SINGLE-FILE, CONDENSED OVERLEAF MEMO
# ============================================================

# Order tables substantively: Muslim aggregate, migration aggregate, then respondents.
desired_table_basenames <- c(
  "aggregate_muslim_two_way_first_difference.tex",
  "aggregate_muslim_two_way_lagged_outcome.tex",
  "aggregate_muslim_triple_first_difference.tex",
  "aggregate_muslim_triple_lagged_outcome.tex",
  "aggregate_migration_two_way_first_difference.tex",
  "aggregate_migration_two_way_lagged_outcome.tex",
  "aggregate_migration_triple_first_difference.tex",
  "aggregate_migration_triple_lagged_outcome.tex",
  "respondent_muslim_two_way.tex",
  "respondent_muslim_triple.tex",
  "respondent_migration_two_way.tex",
  "respondent_migration_triple.tex"
)
ordered_tables <- file.path(out_table_dir, desired_table_basenames)
if (!all(file.exists(ordered_tables))) stop("Not all 12 table files were generated.")

memo_lines <- c(
"% Single-file Overleaf-ready condensed coauthor memo with all estimated control coefficients.",
"\\documentclass[11pt]{article}",
"\\usepackage[margin=1in]{geometry}",
"\\usepackage{booktabs}",
"\\usepackage{threeparttable}",
"\\usepackage{amsmath}",
"\\usepackage{array}",
"\\usepackage{microtype}",
"\\usepackage{enumitem}",
"\\usepackage{hyperref}",
"\\usepackage{setspace}",
"\\usepackage{graphicx}",
"\\setlength{\\parindent}{0pt}",
"\\setlength{\\parskip}{0.35em}",
"\\hypersetup{colorlinks=true,linkcolor=black,urlcolor=black,citecolor=black}",
"\\begin{document}",
"\\begin{center}",
"{\\Large\\bfseries Memo: Recent Empirical Results and Identification Work}\\\\[0.2em]",
"{\\normalsize August 10, 2026}",
"\\end{center}",
"\\begingroup",
"\\small",
"\\setstretch{0.97}",
"\\section*{Headline empirical takeaways}",
"\\begin{itemize}[leftmargin=1.35em,itemsep=0.12em,topsep=0.05em]",
"\\item \\textbf{Manufacturing FDI $\\times$ pre-existing Muslim context is the clearest cross-level result.} On the common substantive ruler, the high-versus-low Muslim-context amplification is \\textbf{+1.50 pp} [0.36, 2.63] in first differences, \\textbf{+1.87 pp} [0.71, 3.02] in the lagged-outcome model, and \\textbf{+2.84 pp} [0.04, 5.63] at the respondent level. Services FDI and recent demographic-change measures are much weaker.",
"\\item \\textbf{Established migration is mainly an individual-level result.} Aggregate contrasts are +0.41 and +0.42 pp and imprecise; the preferred respondent contrast is \\textbf{+3.94 pp} [2.14, 5.75]. The five-column tables show why: the aggregate interaction weakens once preferred state fixed effects and PC-clustered inference are imposed, while the respondent interaction remains strong.",
"\\item \\textbf{Centrism is more convincing as constituency context than as an individual mechanism.} The aggregate Muslim triple implies about \\textbf{+4.21 pp} [1.08, 7.34] of extra amplification in first differences and \\textbf{+3.77 pp} [0.63, 6.91] in the lagged-outcome design. The respondent Center triple is imprecise (+2.55 pp [--4.72, 9.81]); migration triples are null.",
"\\item \\textbf{Candidate supply is unlikely to explain the voter results.} Restricting to PCs contested by the BJP in both elections leaves the main two-way contrasts broadly intact (migration 5.38 vs. 5.40 pp; Muslim 3.35 vs. 2.75 pp, with lower precision in the restricted Muslim sample).",
"\\end{itemize}",
"\\section*{How to read the percentage-point contrasts}",
"\\begin{itemize}[leftmargin=1.35em,itemsep=0.10em,topsep=0.05em]",
"\\item \\textbf{These are not regression coefficients.} They are post-estimation translations onto a common substantive ruler; each aggregate/respondent regression remains estimated on its own sample and native scale.",
"\\item For Muslim share, FDI moves $0\\rightarrow0.5346$ in $\\log(1+\\text{manufacturing projects per 100,000})$ and 2001 Muslim share moves 3.72\\%$\\rightarrow$15.07\\%. Thus +1.50 pp means that \\emph{this specified FDI increase has a 1.50-point larger estimated BJP effect at high than low Muslim share}; it is not a one-unit regression coefficient. For migration, FDI moves $0\\rightarrow0.5273$ and established migrant share moves 0.773\\%$\\rightarrow$3.827\\%. The migration moderator proxies interstate/international migrants resident by roughly 2001.",
"\\item Triple contrasts are \\textbf{second differences}. Aggregate triples vary the \\emph{constituency} weighted 2009 Center share (0$\\rightarrow$9.07\\% on the common ruler); respondent triples compare a Center to non-Center individual. Their magnitudes therefore should not be mechanically compared.",
"\\end{itemize}",
"\\begin{table}[!htbp]",
"\\centering\\footnotesize",
"\\caption{Preferred cross-level substantive interaction contrasts}",
"\\begin{threeparttable}",
"\\resizebox{\\textwidth}{!}{%",
"\\begin{tabular}{llccc}",
"\\toprule",
"Demographic context & Interaction & First difference & Lagged outcome & Respondent, 2014 \\\\",
"\\midrule",
"Muslim share, 2001 & FDI $\\times$ demographic & 1.50 [0.36, 2.63] & 1.87 [0.71, 3.02] & 2.84 [0.04, 5.63] \\\\",
"Muslim share, 2001 & FDI $\\times$ demographic $\\times$ Center & 4.21 [1.08, 7.34] & 3.77 [0.63, 6.91] & 2.55 [$-4.72$, 9.81] \\\\",
"Established migration & FDI $\\times$ demographic & 0.41 [$-0.39$, 1.21] & 0.42 [$-0.54$, 1.37] & 3.94 [2.14, 5.75] \\\\",
"Established migration & FDI $\\times$ demographic $\\times$ Center & 0.39 [$-0.79$, 1.57] & 0.57 [$-0.84$, 1.97] & $-0.99$ [$-6.12$, 4.14] \\\\",
"\\bottomrule\\end{tabular}%", "}",
"\\begin{tablenotes}[flushleft]\\scriptsize",
"\\item Entries are percentage-point substantive contrasts with 95\\% CIs, not raw coefficients. The appendix reports raw coefficients, SEs, exact p-values, and significance stars.",
"\\end{tablenotes}\\end{threeparttable}\\end{table}",
"\\section*{Why pooled models are not headline results}",
"\\begin{itemize}[leftmargin=1.35em,itemsep=0.10em,topsep=0.05em]",
"\\item Pooled AC models mix persistent between-constituency differences with within-constituency change. First differences directly model the 2009--14 BJP shift; lagged outcomes model 2014 support conditional on 2009 support. The NES is also a repeated cross-section rather than a voter panel. Pooled models were estimated as robustness checks but are omitted from the headline tables because they answer a less preferred temporal estimand, not because of their results.",
"\\end{itemize}",
"\\section*{Pre-outcome IV audit}",
"A credible instrument should: \\textbf{(i)} strongly predict 2009--14 FDI; \\textbf{(ii)} not strongly predict 2004--09 FDI in a placebo; \\textbf{(iii)} remain stable to leave-one-industry/source-out tests; and \\textbf{(iv)} draw on sufficiently diffuse, plausibly external shocks for the exclusion restriction to be credible.",
"\\begin{enumerate}[leftmargin=1.5em,itemsep=0.10em,topsep=0.05em]",
"\\item \\textbf{WIR industry / within-manufacturing Bartik:} best own-AC HQ first stage $F=10.25$, placebo $F=1.11$, but only 3.37 effective industries; leave-one-out $F$ falls to 4.25 without textiles, 5.45 without non-metallic minerals, and 6.72 without food/beverages/tobacco.",
"\\item \\textbf{Matched local Bartik:} strict-neighborhood $F_{post}=3.74$ vs. placebo $F_{pre}=10.30$; HQ $12.66$ vs. $7.79$. Geography matching does not eliminate pre-period predictiveness.",
"\\item \\textbf{Official policy shocks:} zero clean 2009--14 national reforms map both to predetermined EC05 exposure and our manufacturing-FDI treatment; the cleanest reforms are in services.",
"\\item \\textbf{Source-country push:} source country matched to 97.7\\% of projects and shocks are more diffuse (about eight effective countries), but placebo performance is poor: full sample $F_{post}=3.78$ vs. $F_{pre}=9.72$; HQ $7.19$ vs. $9.57$.",
"\\end{enumerate}",
"\\textbf{Bottom line:} the evidence supports a robust multi-level longitudinal association, not a point-identified causal IV claim. We deliberately have not inspected/reported BJP 2SLS coefficients from instruments that fail these pre-outcome gates.",
"\\endgroup",
"\\clearpage",
"\\appendix",
"\\section*{Publication-format regression tables}",
"All estimable C0--C3 and V2 control coefficients are shown below; state fixed effects remain absorbed and omitted factor reference categories are not coefficients. Model 4 is the preferred inferential specification."
)

for (tf in ordered_tables) {
  memo_lines <- c(memo_lines, "", readLines(tf, warn = FALSE))
}
memo_lines <- c(memo_lines, "\\end{document}")

standalone_tex <- file.path(out_root, "Switchers_coauthor_memo_CONDENSED_WITH_CONTROLS_OVERLEAF.tex")
writeLines(memo_lines, standalone_tex)
message("Standalone condensed Overleaf file: ", standalone_tex)

message("")
message("Coauthor publication tables COMPLETE.")
message("Output directory: ", out_root)
message("LaTeX tables: ", out_table_dir)
message("Coefficient CSV: ", file.path(out_root, "all_12_publication_table_coefficients.csv"))
message("Frozen reproduction audit: ", file.path(out_manifest_dir, "model4_frozen_reproduction_audit.csv"))

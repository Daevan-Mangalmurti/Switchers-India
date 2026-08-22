# ============================================================
# 20_centrist_voter_lpm_muslim_fdi_change_v1_0.R
# Revision: 2026-08-20-v1.0
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
load_switchers_packages()
paths <- build_project_paths(project_root)

if (!requireNamespace("lme4", quietly = TRUE)) {
  stop("Package 'lme4' is required. Install it with install.packages('lme4').")
}

REV <- "2026-08-20-v1.0"

# ------------------------------------------------------------
# 1. Frozen choices
# ------------------------------------------------------------

OUTCOME <- "voted_bjp"
MUSLIM <- "muslim_share_2001_dist_proxy"

# IMPORTANT: in the frozen respondent metadata, *_2014 is the
# 2004-09 -> 2009-14 CHANGE variable for the baseline-2014 design.
# This is the raw projects-per-100,000 version requested here.
FDI_CHANGE <- "fdi_mfg_local_all_pc100k_2014"

WEIGHT <- "survey_weight_norm_year"

C1 <- c(
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share"
)

V2 <- c(
  "religion_group",
  "caste_group",
  "education_harmonized"
)

out_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_centrist_muslim_fdi_change",
  REV
)
out_tables <- file.path(out_root, "tables")
out_data <- file.path(out_root, "data")
out_audit <- file.path(out_root, "audit")
out_models <- file.path(out_root, "models")

purrr::walk(
  c(out_root, out_tables, out_data, out_audit, out_models),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. Load data and attach missing AC variables
# ------------------------------------------------------------

respondents <- readRDS(file.path(paths$final_dir, "nes_respondent_analysis.rds"))
ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))

attach_missing <- function(data, ac, vars) {
  miss <- setdiff(vars, names(data))
  if (!length(miss)) return(data)

  absent <- setdiff(miss, names(ac))
  if (length(absent)) {
    stop(
      "Missing from both respondent data and ac_change: ",
      paste(absent, collapse = ", ")
    )
  }

  payload <- ac |>
    dplyr::select(ac_uid, dplyr::all_of(miss)) |>
    dplyr::distinct(ac_uid, .keep_all = TRUE)

  if (anyDuplicated(payload$ac_uid)) stop("ac_change is not unique by ac_uid.")

  n0 <- nrow(data)
  data <- data |>
    dplyr::left_join(payload, by = "ac_uid", relationship = "many-to-one")
  if (nrow(data) != n0) stop("AC join changed respondent row count.")
  data
}

respondents <- attach_missing(
  respondents,
  ac_change,
  c(MUSLIM, FDI_CHANGE, C1)
)

required <- c(
  "year", OUTCOME, "vote_valid", "bjp_candidate_present",
  WEIGHT, "ac_uid", "state_no", "ideology_complete",
  MUSLIM, FDI_CHANGE, C1, V2
)

missing <- setdiff(required, names(respondents))
if (length(missing)) {
  stop("Missing required variables: ", paste(missing, collapse = ", "))
}

# Frozen candidate-present sample, reconstructed only if absent.
if (!"respondent_sample_candidate_present" %in% names(respondents)) {
  respondents <- respondents |>
    dplyr::mutate(
      respondent_sample_candidate_present =
        dplyr::coalesce(vote_valid, FALSE) &
        !is.na(.data[[OUTCOME]]) &
        !is.na(bjp_candidate_present) &
        bjp_candidate_present == 1
    )
}

# In the frozen pipeline, harmonized Center == strict Center in 2014.
# If center_harmonized is absent, use the stored 2014 voter_ideology.
if (!"center_harmonized" %in% names(respondents)) {
  if (!"voter_ideology" %in% names(respondents)) {
    stop("Need center_harmonized or voter_ideology to identify 2014 Center voters.")
  }

  respondents <- respondents |>
    dplyr::mutate(
      center_harmonized = dplyr::case_when(
        !ideology_complete ~ NA_real_,
        voter_ideology == "Center" ~ 1,
        TRUE ~ 0
      )
    )
}

observed_y <- sort(unique(respondents[[OUTCOME]][!is.na(respondents[[OUTCOME]])]))
if (!all(observed_y %in% c(0, 1))) {
  stop(OUTCOME, " is not binary 0/1.")
}

# ------------------------------------------------------------
# 3. 2014 Center-voter sample and transparent scaling
# ------------------------------------------------------------

base <- respondents |>
  dplyr::filter(
    year == 2014,
    dplyr::coalesce(respondent_sample_candidate_present, FALSE),
    ideology_complete,
    !is.na(center_harmonized),
    center_harmonized == 1,
    !is.na(ac_uid),
    !is.na(state_no),
    is.finite(.data[[MUSLIM]]),
    is.finite(.data[[FDI_CHANGE]]),
    is.finite(.data[[WEIGHT]]),
    .data[[WEIGHT]] > 0
  ) |>
  dplyr::mutate(
    y = as.numeric(.data[[OUTCOME]]),

    # Focal terms
    muslim_pp = 100 * as.numeric(.data[[MUSLIM]]),
    fdi_change_pc100k = as.numeric(.data[[FDI_CHANGE]]),

    # C1 presentation units
    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    land_area = as.numeric(con08_land_area),
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),

    # FE / RE / V2
    state_fe = factor(state_no),
    ac_random = factor(ac_uid),
    religion_x = factor(religion_group),
    caste_x = factor(caste_group),
    education_x = factor(education_harmonized),

    model_weight = as.numeric(.data[[WEIGHT]])
  )

if (!nrow(base)) stop("2014 Center-voter sample is empty.")
if (dplyr::n_distinct(base$ac_random) < 2) stop("Fewer than two ACs remain.")

readr::write_csv(
  tibble::tibble(
    quantity = c(
      "Muslim share 2001",
      "FDI change",
      "AC population",
      "SC share",
      "ST share"
    ),
    source_variable = c(
      MUSLIM, FDI_CHANGE, "proxy_ac_pop", "sc_pop_share", "st_pop_share"
    ),
    model_unit = c(
      "percentage points",
      "change in manufacturing projects per 100,000",
      "100,000 persons",
      "percentage points",
      "percentage points"
    )
  ),
  file.path(out_audit, "01_model_scaling.csv")
)

weighted_ess <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

sample_audit <- tibble::tibble(
  sample = "2014 Center voters; valid vote; BJP candidate present",
  n_respondents = nrow(base),
  n_acs = dplyr::n_distinct(base$ac_random),
  n_states = dplyr::n_distinct(base$state_fe),
  weighted_ess = weighted_ess(base$model_weight),
  unweighted_bjp_vote_rate = mean(base$y),
  weighted_bjp_vote_rate = stats::weighted.mean(base$y, base$model_weight)
)

readr::write_csv(
  sample_audit,
  file.path(out_audit, "02_centrist_base_sample_audit.csv")
)

# ------------------------------------------------------------
# 4. Fit five requested AC-random-intercept LPMs
# ------------------------------------------------------------

C1_TERMS <- c("ac_pop_100k", "land_area", "sc_share_pp", "st_share_pp")
V2_TERMS <- c("religion_x", "caste_x", "education_x")

complete_finite <- function(data, vars) {
  keep <- rep(TRUE, nrow(data))
  for (v in unique(vars)) {
    x <- data[[v]]
    if (is.numeric(x) || is.integer(x)) {
      keep <- keep & !is.na(x) & is.finite(as.numeric(x))
    } else {
      keep <- keep & !is.na(x)
    }
  }
  keep
}

fit_one <- function(key, interaction, state_fe, c1, v2) {
  rhs <- if (interaction) {
    "muslim_pp * fdi_change_pc100k"
  } else {
    "muslim_pp + fdi_change_pc100k"
  }

  extra <- c(
    if (state_fe) "state_fe",
    if (c1) C1_TERMS,
    if (v2) V2_TERMS
  )

  fixed_rhs <- paste(c(rhs, extra), collapse = " + ")
  ftxt <- paste0("y ~ ", fixed_rhs, " + (1 | ac_random)")
  fml <- stats::as.formula(ftxt)

  vars <- unique(c(all.vars(fml), "model_weight"))
  dd <- base[complete_finite(base, vars), , drop = FALSE]
  dd <- droplevels(dd)

  if (!nrow(dd)) stop("Zero complete cases for ", key, ".")
  if (dplyr::n_distinct(dd$ac_random) < 2) {
    stop("Fewer than two ACs for ", key, ".")
  }

  warns <- character()
  fit <- withCallingHandlers(
    lme4::lmer(
      fml,
      data = dd,
      weights = model_weight,
      REML = FALSE,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 200000)
      )
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    fit = fit,
    data = dd,
    formula = ftxt,
    warnings = unique(warns),
    state_fe = state_fe,
    c1 = c1,
    v2 = v2
  )
}

fits <- list(
  m1 = fit_one("m1", FALSE, FALSE, FALSE, FALSE),
  m2 = fit_one("m2", TRUE,  FALSE, FALSE, FALSE),
  m3 = fit_one("m3", TRUE,  TRUE,  FALSE, FALSE),
  m4 = fit_one("m4", TRUE,  TRUE,  TRUE,  FALSE),
  m5 = fit_one("m5", TRUE,  TRUE,  TRUE,  TRUE)
)

model_labels <- c(
  m1 = "Additive",
  m2 = "Interaction",
  m3 = "+ State FE",
  m4 = "+ FE + C1",
  m5 = "+ FE + C1 + V2"
)

tidy_fixed <- function(fit) {
  ct <- stats::coef(summary(fit))
  est <- as.numeric(ct[, "Estimate"])
  se <- as.numeric(ct[, "Std. Error"])
  z <- est / se

  tibble::tibble(
    term = rownames(ct),
    estimate = est,
    se = se,
    statistic_normal_approx = z,
    p_normal_approx = 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  )
}

random_stats <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  rr <- vc |>
    dplyr::filter(grp == "ac_random", is.na(var2))

  if (nrow(rr) != 1) stop("Could not identify AC random-intercept variance.")

  ac_var <- rr$vcov[[1]]
  residual_var <- stats::sigma(fit)^2

  tibble::tibble(
    ac_random_intercept_sd = sqrt(ac_var),
    residual_sd = sqrt(residual_var),
    icc_ac = ac_var / (ac_var + residual_var)
  )
}

conv_message <- function(fit) {
  x <- fit@optinfo$conv$lme4$messages
  if (is.null(x) || !length(x)) "" else paste(unique(x), collapse = " | ")
}

coef_out <- purrr::imap_dfr(
  fits,
  ~ tidy_fixed(.x$fit) |>
    dplyr::mutate(
      model = .y,
      model_label = unname(model_labels[[.y]]),
      .before = 1
    )
)

readr::write_csv(
  coef_out,
  file.path(out_data, "01_model_coefficients.csv")
)

model_audit <- purrr::imap_dfr(
  fits,
  function(obj, key) {
    tibble::tibble(
      model = key,
      model_label = unname(model_labels[[key]]),
      formula = obj$formula,
      n_respondents = stats::nobs(obj$fit),
      n_acs = dplyr::n_distinct(obj$data$ac_random),
      n_states = dplyr::n_distinct(obj$data$state_fe),
      state_fixed_effects = obj$state_fe,
      c1_controls = obj$c1,
      v2_voter_controls = obj$v2,
      ac_random_intercept = TRUE,
      singular = lme4::isSingular(obj$fit, tol = 1e-4),
      convergence_message = conv_message(obj$fit),
      fit_warning_messages = paste(obj$warnings, collapse = " | "),
      AIC = stats::AIC(obj$fit),
      BIC = stats::BIC(obj$fit)
    ) |>
      dplyr::bind_cols(random_stats(obj$fit))
  }
)

readr::write_csv(
  model_audit,
  file.path(out_audit, "03_model_fit_random_effect_audit.csv")
)

saveRDS(
  lapply(fits, `[[`, "fit"),
  file.path(out_models, "centrist_voter_multilevel_lpm_models.rds")
)

# ------------------------------------------------------------
# 5. Publication-style LaTeX table
# ------------------------------------------------------------

stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p < .01, "$^{***}$",
      ifelse(p < .05, "$^{**}$", ifelse(p < .10, "$^{*}$", ""))
    )
  )
}

fmt <- function(x) {
  if (is.na(x)) return("--")
  ax <- abs(x)
  if (ax > 0 && ax < 1e-4) return(sprintf("%.2e", x))
  if (ax < .01) return(sprintf("%.5f", x))
  if (ax < 1) return(sprintf("%.3f", x))
  if (ax < 100) return(sprintf("%.2f", x))
  sprintf("%.1f", x)
}

cell <- function(e, s, p) {
  paste0("\\shortstack{", fmt(e), stars(p), "\\\\(", fmt(s), ")}")
}

pretty_term <- function(term) {
  dplyr::case_when(
    term == "(Intercept)" ~ "(Intercept)",
    term == "muslim_pp" ~ "Muslim share, 2001 (percentage points)",
    term == "fdi_change_pc100k" ~
      "$\\Delta$ Manufacturing FDI, 2004--09 to 2009--14 (projects per 100,000)",
    term %in% c(
      "muslim_pp:fdi_change_pc100k",
      "fdi_change_pc100k:muslim_pp"
    ) ~ "Muslim share $\\times$ $\\Delta$ Manufacturing FDI",
    term == "ac_pop_100k" ~ "AC population (100,000s)",
    term == "land_area" ~ "Land area",
    term == "sc_share_pp" ~ "SC share (percentage points)",
    term == "st_share_pp" ~ "ST share (percentage points)",
    TRUE ~ term
  )
}

interaction_terms <- intersect(
  c(
    "muslim_pp:fdi_change_pc100k",
    "fdi_change_pc100k:muslim_pp"
  ),
  unique(coef_out$term)
)

if (length(interaction_terms) != 1) {
  stop("Could not uniquely identify focal interaction term.")
}

interaction_term <- interaction_terms[[1]]

row_terms <- c(
  "(Intercept)",
  "muslim_pp",
  "fdi_change_pc100k",
  interaction_term,
  "ac_pop_100k",
  "land_area",
  "sc_share_pp",
  "st_share_pp"
)

table_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\begin{threeparttable}",
  "\\caption{BJP voting among centrist voters: Muslim share and change in manufacturing FDI, 2014}",
  "\\label{tab:centrist_voter_muslim_fdi_change}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  " & Additive & Interaction & + State FE & + FE + C1 & + FE + C1 + V2 \\\\",
  "\\midrule"
)

for (term_i in row_terms) {
  vals <- character(5)

  for (j in seq_along(fits)) {
    model_i <- names(fits)[[j]]

    one <- coef_out |>
      dplyr::filter(
        .data$model == .env$model_i,
        .data$term == .env$term_i
      )

    if (nrow(one) > 1) {
      stop("Duplicate table coefficient for ", model_i, " / ", term_i)
    }

    vals[[j]] <- if (!nrow(one)) {
      "--"
    } else {
      cell(
        one$estimate[[1]],
        one$se[[1]],
        one$p_normal_approx[[1]]
      )
    }
  }

  table_lines <- c(
    table_lines,
    paste0(
      pretty_term(term_i),
      " & ",
      paste(vals, collapse = " & "),
      " \\\\"
    )
  )
}

audit <- model_audit |>
  dplyr::mutate(model = factor(model, levels = names(fits))) |>
  dplyr::arrange(model)

state_row <- ifelse(audit$state_fixed_effects, "Yes", "No")
voter_row <- ifelse(audit$v2_voter_controls, "Yes", "No")
n_row <- formatC(audit$n_respondents, format = "d", big.mark = ",")
ac_row <- formatC(audit$n_acs, format = "d", big.mark = ",")
sd_row <- vapply(audit$ac_random_intercept_sd, fmt, character(1))
icc_row <- ifelse(is.na(audit$icc_ac), "--", sprintf("%.3f", audit$icc_ac))

table_lines <- c(
  table_lines,
  "\\midrule",
  paste0("State fixed effects & ", paste(state_row, collapse = " & "), " \\\\"),
  paste0("Voter controls & ", paste(voter_row, collapse = " & "), " \\\\"),
  "AC random intercept & Yes & Yes & Yes & Yes & Yes \\\\",
  paste0("Observations & ", paste(n_row, collapse = " & "), " \\\\"),
  paste0("Assembly constituencies & ", paste(ac_row, collapse = " & "), " \\\\"),
  paste0("AC random-intercept SD & ", paste(sd_row, collapse = " & "), " \\\\"),
  paste0("AC ICC & ", paste(icc_row, collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item Outcome is an indicator for voting BJP in 2014. The sample is restricted to ideology-complete Center voters with a valid vote in assembly constituencies where a BJP candidate was present.",
  "\\item Every column is a linear mixed probability model with an assembly-constituency random intercept. Normalized 2014 NES survey weights are supplied to \\texttt{lmer} as prior weights. Models are estimated by maximum likelihood (REML = FALSE).",
  "\\item Muslim share is the 2001 Muslim population share expressed in percentage points. Manufacturing FDI is the change in local manufacturing projects per 100,000 residents from the 2004--09 exposure window to the 2009--14 exposure window.",
  "\\item Columns 3--5 include state fixed effects. Columns 4--5 include C1 constituency controls: AC population, land area, SC share, and ST share. Their coefficients are displayed above.",
  "\\item Column 5 additionally includes V2 voter controls: religion, caste, and education. Individual factor-level coefficients for state and voter controls are not displayed.",
  "\\item Standard errors are model-based mixed-model standard errors. Significance markers use a normal approximation to the fixed-effect statistic: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$. Standard errors are in parentheses.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

table_path <- file.path(
  out_tables,
  "centrist_voter_muslim_fdi_change_multilevel_lpm.tex"
)
writeLines(table_lines, table_path)

# ------------------------------------------------------------
# 6. Fail-fast audit of table-cell mapping
# ------------------------------------------------------------

mapping <- tidyr::crossing(
  model = names(fits),
  table_term = row_terms
) |>
  dplyr::left_join(
    coef_out |>
      dplyr::select(model, term, estimate, se, p_normal_approx),
    by = c("model" = "model", "table_term" = "term")
  ) |>
  dplyr::mutate(
    expected_present = dplyr::case_when(
      table_term == interaction_term & model == "m1" ~ FALSE,
      table_term %in% C1_TERMS & !model %in% c("m4", "m5") ~ FALSE,
      TRUE ~ TRUE
    ),
    actual_present = !is.na(estimate),
    pass = expected_present == actual_present
  )

readr::write_csv(
  mapping,
  file.path(out_audit, "04_table_value_mapping_audit.csv")
)

if (any(!mapping$pass)) {
  bad <- mapping |>
    dplyr::filter(!pass)
  stop(
    "Table mapping audit failed: ",
    paste(paste0(bad$model, "/", bad$table_term), collapse = ", ")
  )
}

writeLines(
  c(
    paste0("Revision: ", REV),
    "2014 Center-voter multilevel LPM.",
    paste0("Outcome: ", OUTCOME),
    paste0("Muslim variable: ", MUSLIM),
    paste0("FDI change variable: ", FDI_CHANGE),
    paste0("Weight: ", WEIGHT),
    "Random effect in every model: (1 | ac_uid).",
    "M1 additive; M2 interaction; M3 + state FE; M4 + C1; M5 + V2.",
    "C1 = AC population + land area + SC share + ST share.",
    "V2 = religion + caste + education.",
    paste0("Main LaTeX table: ", table_path)
  ),
  file.path(out_root, "README.txt")
)

message("")
message("============================================================")
message("CENTRIST-VOTER MULTILEVEL LPM COMPLETE")
message("============================================================")
message("Output root: ", out_root)
message("Main LaTeX table: ", table_path)
message("")
message("Base sample:")
print(sample_audit, width = Inf)
message("")
message("Model audit:")
print(
  audit |>
    dplyr::select(
      model, model_label, n_respondents, n_acs, n_states,
      singular, convergence_message, ac_random_intercept_sd, icc_ac
    ),
  n = Inf,
  width = Inf
)
message("============================================================")

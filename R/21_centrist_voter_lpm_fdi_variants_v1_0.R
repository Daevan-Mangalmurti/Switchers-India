# ============================================================
# 21_centrist_voter_lpm_fdi_variants_v1_0.R
# Revision: 2026-08-20-v1.0
#
# Runs TWO separate 2014 Center-voter multilevel LPM analyses:
#
# A. POSITIVE-FDI-ONLY CONTINUOUS SPECIFICATION
#    Restrict ACs to >0 local manufacturing FDI projects in 2009-14.
#    Focal exposure is the TRUE change in logged FDI per 100,000:
#      log1p(FDI per 100k, 2009-14) - log1p(FDI per 100k, 2004-09)
#
# B. FULL-SAMPLE BINARY SPECIFICATION
#    Keep both zero-FDI and positive-FDI ACs.
#    Focal exposure is:
#      1 = any local manufacturing FDI project in 2009-14
#      0 = no local manufacturing FDI project in 2009-14
#
# Both analyses:
#   - restrict respondents to 2014 ideology-complete Center voters
#   - require valid vote and BJP candidate present
#   - use normalized 2014 NES survey weights as lmer prior weights
#   - include AC random intercept in every model
#   - use same five-column sequence:
#       M1 additive
#       M2 interaction
#       M3 interaction + state FE
#       M4 interaction + state FE + C1 AC controls
#       M5 interaction + state FE + C1 + V2 voter controls
#
# IMPORTANT:
#   The positive-FDI restriction and binary any-FDI specification are
#   separate analyses. Do not restrict to positive FDI and then try to
#   estimate the any-FDI dummy, because it would have no variation.
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
# 1. Frozen variables
# ------------------------------------------------------------

OUTCOME <- "voted_bjp"
MUSLIM <- "muslim_share_2001_dist_proxy"
WEIGHT <- "survey_weight_norm_year"

# Period-specific manufacturing-FDI measures.
FDI_CURRENT_LOG <- "log1p_fdi_mfg_local_all_pc100k_2014"
FDI_BASELINE_LOG <- "log1p_fdi_mfg_local_all_pc100k_2009"
FDI_CURRENT_COUNT <- "fdi_mfg_local_all_n_2014"

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

# Literal binary model requested by the user:
# 0/1 current-period FDI status is the focal exposure.
# Set TRUE for a sensitivity that ALSO adjusts every binary model
# for logged 2004-09 baseline FDI.
BINARY_ADJUST_FOR_BASELINE_FDI <- FALSE

out_root <- file.path(
  paths$derived_dir,
  "paper_outputs",
  "respondent_centrist_fdi_variants",
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
# 2. Load and attach AC-level variables
# ------------------------------------------------------------

respondents <- readRDS(file.path(paths$final_dir, "nes_respondent_analysis.rds"))
ac_change <- readRDS(file.path(paths$final_dir, "ac_change.rds"))

attach_missing <- function(data, ac, vars) {
  miss <- setdiff(vars, names(data))
  if (!length(miss)) return(data)

  absent <- setdiff(miss, names(ac))
  if (length(absent)) {
    stop(
      "Required variables missing from both respondent data and ac_change: ",
      paste(absent, collapse = ", ")
    )
  }

  payload <- ac |>
    dplyr::select(ac_uid, dplyr::all_of(miss)) |>
    dplyr::distinct(ac_uid, .keep_all = TRUE)

  if (anyDuplicated(payload$ac_uid)) {
    stop("AC payload is not unique by ac_uid.")
  }

  n0 <- nrow(data)
  data <- data |>
    dplyr::left_join(payload, by = "ac_uid", relationship = "many-to-one")

  if (nrow(data) != n0) {
    stop("AC-level join changed respondent row count.")
  }

  data
}

respondents <- attach_missing(
  respondents,
  ac_change,
  c(
    MUSLIM,
    FDI_CURRENT_LOG,
    FDI_BASELINE_LOG,
    FDI_CURRENT_COUNT,
    C1
  )
)

required <- c(
  "year", OUTCOME, "vote_valid", "bjp_candidate_present",
  WEIGHT, "ac_uid", "state_no", "ideology_complete",
  MUSLIM, FDI_CURRENT_LOG, FDI_BASELINE_LOG, FDI_CURRENT_COUNT,
  C1, V2
)

missing <- setdiff(required, names(respondents))
if (length(missing)) {
  stop("Missing required variables: ", paste(missing, collapse = ", "))
}

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

if (!"center_harmonized" %in% names(respondents)) {
  if (!"voter_ideology" %in% names(respondents)) {
    stop("Need center_harmonized or voter_ideology to identify Center voters.")
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

y_values <- sort(unique(respondents[[OUTCOME]][!is.na(respondents[[OUTCOME]])]))
if (!all(y_values %in% c(0, 1))) {
  stop(OUTCOME, " is not binary 0/1.")
}

# ------------------------------------------------------------
# 3. Full 2014 Center-voter base sample
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
    is.finite(.data[[FDI_CURRENT_LOG]]),
    is.finite(.data[[FDI_BASELINE_LOG]]),
    is.finite(.data[[FDI_CURRENT_COUNT]]),
    is.finite(.data[[WEIGHT]]),
    .data[[WEIGHT]] > 0
  ) |>
  dplyr::mutate(
    y = as.numeric(.data[[OUTCOME]]),

    # Muslim share in percentage points.
    muslim_pp = 100 * as.numeric(.data[[MUSLIM]]),

    # Period-specific logged local manufacturing FDI per 100k.
    fdi_log_current = as.numeric(.data[[FDI_CURRENT_LOG]]),
    fdi_log_baseline = as.numeric(.data[[FDI_BASELINE_LOG]]),

    # TRUE change in logged FDI per 100k.
    delta_log_fdi_pc100k = fdi_log_current - fdi_log_baseline,

    # 2009-14 treatment-status dummy.
    fdi_current_count = as.numeric(.data[[FDI_CURRENT_COUNT]]),
    any_fdi_0914 = as.numeric(fdi_current_count > 0),

    # C1 controls in readable units.
    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    land_area = as.numeric(con08_land_area),
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),

    # FE, RE, V2.
    state_fe = factor(state_no),
    ac_random = factor(ac_uid),
    religion_x = factor(religion_group),
    caste_x = factor(caste_group),
    education_x = factor(education_harmonized),

    model_weight = as.numeric(.data[[WEIGHT]])
  )

if (!nrow(base)) stop("2014 Center-voter base sample is empty.")
if (dplyr::n_distinct(base$ac_random) < 2) stop("Fewer than two ACs remain.")

# Verify binary exposure is truly 0/1.
if (!all(sort(unique(base$any_fdi_0914)) %in% c(0, 1))) {
  stop("any_fdi_0914 is not binary 0/1.")
}

# AC-level support audit, one row per AC.
ac_support <- base |>
  dplyr::distinct(
    ac_uid,
    fdi_current_count,
    any_fdi_0914,
    fdi_log_current,
    fdi_log_baseline,
    delta_log_fdi_pc100k
  )

support_summary <- ac_support |>
  dplyr::summarise(
    n_acs = dplyr::n(),
    n_no_fdi_0914 = sum(any_fdi_0914 == 0),
    n_any_fdi_0914 = sum(any_fdi_0914 == 1),
    share_any_fdi_0914 = mean(any_fdi_0914 == 1),
    delta_log_fdi_min = min(delta_log_fdi_pc100k),
    delta_log_fdi_median = stats::median(delta_log_fdi_pc100k),
    delta_log_fdi_max = max(delta_log_fdi_pc100k)
  )

readr::write_csv(
  ac_support,
  file.path(out_audit, "01_ac_fdi_support.csv")
)
readr::write_csv(
  support_summary,
  file.path(out_audit, "02_ac_fdi_support_summary.csv")
)

# ------------------------------------------------------------
# 4. Analysis datasets
# ------------------------------------------------------------

# A: exposed ACs only.
positive_base <- base |>
  dplyr::filter(any_fdi_0914 == 1)

if (!nrow(positive_base)) stop("Positive-FDI Center-voter sample is empty.")
if (dplyr::n_distinct(positive_base$ac_random) < 2) {
  stop("Fewer than two positive-FDI ACs remain.")
}

# B: full sample, both zero and positive FDI.
binary_base <- base

if (dplyr::n_distinct(binary_base$any_fdi_0914) != 2) {
  stop(
    "Binary analysis requires both no-FDI and any-FDI ACs, but the current ",
    "sample does not contain both categories."
  )
}

weighted_ess <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

sample_audit <- dplyr::bind_rows(
  tibble::tibble(
    analysis = "A_positive_fdi_continuous",
    n_respondents = nrow(positive_base),
    n_acs = dplyr::n_distinct(positive_base$ac_random),
    n_states = dplyr::n_distinct(positive_base$state_fe),
    weighted_ess = weighted_ess(positive_base$model_weight),
    weighted_bjp_vote_rate = stats::weighted.mean(
      positive_base$y,
      positive_base$model_weight
    )
  ),
  tibble::tibble(
    analysis = "B_full_sample_binary",
    n_respondents = nrow(binary_base),
    n_acs = dplyr::n_distinct(binary_base$ac_random),
    n_states = dplyr::n_distinct(binary_base$state_fe),
    weighted_ess = weighted_ess(binary_base$model_weight),
    weighted_bjp_vote_rate = stats::weighted.mean(
      binary_base$y,
      binary_base$model_weight
    )
  )
)

readr::write_csv(
  sample_audit,
  file.path(out_audit, "03_analysis_sample_audit.csv")
)

# ------------------------------------------------------------
# 5. Mixed-model helpers
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

fit_one <- function(
    data,
    key,
    exposure,
    interaction = TRUE,
    state_fe = FALSE,
    c1 = FALSE,
    v2 = FALSE,
    baseline_fdi_control = FALSE
) {
  focal <- if (interaction) {
    paste0("muslim_pp * ", exposure)
  } else {
    paste0("muslim_pp + ", exposure)
  }

  extra <- c(
    if (baseline_fdi_control) "fdi_log_baseline",
    if (state_fe) "state_fe",
    if (c1) C1_TERMS,
    if (v2) V2_TERMS
  )

  rhs <- paste(c(focal, extra), collapse = " + ")
  ftxt <- paste0("y ~ ", rhs, " + (1 | ac_random)")
  fml <- stats::as.formula(ftxt)

  vars <- unique(c(all.vars(fml), "model_weight"))
  dd <- data[complete_finite(data, vars), , drop = FALSE]
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
    v2 = v2,
    baseline_fdi_control = baseline_fdi_control
  )
}

fit_sequence <- function(data, exposure, binary_baseline_adjustment = FALSE) {
  list(
    m1 = fit_one(
      data, "m1", exposure,
      interaction = FALSE,
      baseline_fdi_control = binary_baseline_adjustment
    ),
    m2 = fit_one(
      data, "m2", exposure,
      interaction = TRUE,
      baseline_fdi_control = binary_baseline_adjustment
    ),
    m3 = fit_one(
      data, "m3", exposure,
      interaction = TRUE,
      state_fe = TRUE,
      baseline_fdi_control = binary_baseline_adjustment
    ),
    m4 = fit_one(
      data, "m4", exposure,
      interaction = TRUE,
      state_fe = TRUE,
      c1 = TRUE,
      baseline_fdi_control = binary_baseline_adjustment
    ),
    m5 = fit_one(
      data, "m5", exposure,
      interaction = TRUE,
      state_fe = TRUE,
      c1 = TRUE,
      v2 = TRUE,
      baseline_fdi_control = binary_baseline_adjustment
    )
  )
}

fits_continuous <- fit_sequence(
  positive_base,
  exposure = "delta_log_fdi_pc100k",
  binary_baseline_adjustment = FALSE
)

fits_binary <- fit_sequence(
  binary_base,
  exposure = "any_fdi_0914",
  binary_baseline_adjustment = BINARY_ADJUST_FOR_BASELINE_FDI
)

model_labels <- c(
  m1 = "Additive",
  m2 = "Interaction",
  m3 = "+ State FE",
  m4 = "+ FE + C1",
  m5 = "+ FE + C1 + V2"
)

# ------------------------------------------------------------
# 6. Extract coefficients and diagnostics
# ------------------------------------------------------------

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

extract_analysis <- function(fits, analysis_label) {
  coefs <- purrr::imap_dfr(
    fits,
    ~ tidy_fixed(.x$fit) |>
      dplyr::mutate(
        analysis = analysis_label,
        model = .y,
        model_label = unname(model_labels[[.y]]),
        .before = 1
      )
  )

  audit <- purrr::imap_dfr(
    fits,
    function(obj, key) {
      tibble::tibble(
        analysis = analysis_label,
        model = key,
        model_label = unname(model_labels[[key]]),
        formula = obj$formula,
        n_respondents = stats::nobs(obj$fit),
        n_acs = dplyr::n_distinct(obj$data$ac_random),
        n_states = dplyr::n_distinct(obj$data$state_fe),
        state_fixed_effects = obj$state_fe,
        c1_controls = obj$c1,
        v2_voter_controls = obj$v2,
        baseline_fdi_control = obj$baseline_fdi_control,
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

  list(coefs = coefs, audit = audit)
}

ext_a <- extract_analysis(
  fits_continuous,
  "A_positive_fdi_continuous_delta_log"
)
ext_b <- extract_analysis(
  fits_binary,
  "B_full_sample_any_fdi_binary"
)

coef_out <- dplyr::bind_rows(ext_a$coefs, ext_b$coefs)
model_audit <- dplyr::bind_rows(ext_a$audit, ext_b$audit)

readr::write_csv(
  coef_out,
  file.path(out_data, "01_all_model_coefficients.csv")
)
readr::write_csv(
  model_audit,
  file.path(out_audit, "04_model_fit_random_effect_audit.csv")
)

saveRDS(
  list(
    positive_fdi_continuous = lapply(fits_continuous, `[[`, "fit"),
    full_sample_binary = lapply(fits_binary, `[[`, "fit")
  ),
  file.path(out_models, "centrist_voter_fdi_variant_models.rds")
)

# ------------------------------------------------------------
# 7. LaTeX table helpers
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

pretty_common <- function(term) {
  dplyr::case_when(
    term == "(Intercept)" ~ "(Intercept)",
    term == "muslim_pp" ~ "Muslim share, 2001 (percentage points)",
    term == "ac_pop_100k" ~ "AC population (100,000s)",
    term == "land_area" ~ "Land area",
    term == "sc_share_pp" ~ "SC share (percentage points)",
    term == "st_share_pp" ~ "ST share (percentage points)",
    term == "fdi_log_baseline" ~
      "Manufacturing FDI, 2004--09 (log(1 + projects per 100,000))",
    TRUE ~ term
  )
}

write_table <- function(
    fits,
    coefs,
    audit,
    exposure,
    exposure_label,
    interaction_label,
    caption,
    label,
    path,
    sample_note,
    exposure_note
) {
  interaction_candidates <- c(
    paste0("muslim_pp:", exposure),
    paste0(exposure, ":muslim_pp")
  )

  interaction_terms <- intersect(interaction_candidates, unique(coefs$term))
  if (length(interaction_terms) != 1) {
    stop("Could not uniquely identify interaction term for ", label, ".")
  }
  int_term <- interaction_terms[[1]]

  row_terms <- c(
    "(Intercept)",
    "muslim_pp",
    exposure,
    int_term,
    if (BINARY_ADJUST_FOR_BASELINE_FDI && exposure == "any_fdi_0914") {
      "fdi_log_baseline"
    },
    C1_TERMS
  )

  pretty <- function(term) {
    if (term == exposure) return(exposure_label)
    if (term == int_term) return(interaction_label)
    pretty_common(term)
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\scriptsize",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
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

      one <- coefs |>
        dplyr::filter(
          .data$model == .env$model_i,
          .data$term == .env$term_i
        )

      if (nrow(one) > 1) {
        stop("Duplicate coefficient for ", model_i, " / ", term_i)
      }

      vals[[j]] <- if (!nrow(one)) {
        "--"
      } else {
        cell(one$estimate[[1]], one$se[[1]], one$p_normal_approx[[1]])
      }
    }

    lines <- c(
      lines,
      paste0(pretty(term_i), " & ", paste(vals, collapse = " & "), " \\\\")
    )
  }

  aa <- audit |>
    dplyr::mutate(model = factor(model, levels = names(fits))) |>
    dplyr::arrange(model)

  state_row <- ifelse(aa$state_fixed_effects, "Yes", "No")
  voter_row <- ifelse(aa$v2_voter_controls, "Yes", "No")
  n_row <- formatC(aa$n_respondents, format = "d", big.mark = ",")
  ac_row <- formatC(aa$n_acs, format = "d", big.mark = ",")
  sd_row <- vapply(aa$ac_random_intercept_sd, fmt, character(1))
  icc_row <- ifelse(is.na(aa$icc_ac), "--", sprintf("%.3f", aa$icc_ac))

  lines <- c(
    lines,
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
    paste0("\\item ", sample_note),
    paste0("\\item ", exposure_note),
    "\\item Every column is a linear mixed probability model with an assembly-constituency random intercept. Normalized 2014 NES survey weights are supplied to \\texttt{lmer} as prior weights. Models are estimated by maximum likelihood (REML = FALSE).",
    "\\item Columns 3--5 include state fixed effects. Columns 4--5 include C1 constituency controls: AC population, land area, SC share, and ST share. Column 5 additionally includes V2 voter controls: religion, caste, and education.",
    "\\item Individual state and voter-control factor coefficients are not displayed. Standard errors are model-based mixed-model standard errors. Significance markers use a normal approximation: $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, path)

  # Machine-readable mapping audit.
  mapping <- tidyr::crossing(
    model = names(fits),
    table_term = row_terms
  ) |>
    dplyr::left_join(
      coefs |>
        dplyr::select(model, term, estimate, se, p_normal_approx),
      by = c("model" = "model", "table_term" = "term")
    ) |>
    dplyr::mutate(
      expected_present = dplyr::case_when(
        table_term == int_term & model == "m1" ~ FALSE,
        table_term %in% C1_TERMS & !model %in% c("m4", "m5") ~ FALSE,
        TRUE ~ TRUE
      ),
      actual_present = !is.na(estimate),
      pass = expected_present == actual_present
    )

  if (any(!mapping$pass)) {
    bad <- mapping |>
      dplyr::filter(!pass)
    stop(
      "Table mapping audit failed for ",
      label,
      ": ",
      paste(paste0(bad$model, "/", bad$table_term), collapse = ", ")
    )
  }

  mapping
}

coef_a <- ext_a$coefs
audit_a <- ext_a$audit
coef_b <- ext_b$coefs
audit_b <- ext_b$audit

table_a <- file.path(
  out_tables,
  "21a_centrist_voters_positive_fdi_delta_log_fdi.tex"
)

map_a <- write_table(
  fits = fits_continuous,
  coefs = coef_a,
  audit = audit_a,
  exposure = "delta_log_fdi_pc100k",
  exposure_label =
    "$\\Delta$ log(1 + Manufacturing FDI per 100,000), 2004--09 to 2009--14",
  interaction_label =
    "Muslim share $\\times$ $\\Delta$ log(1 + Manufacturing FDI per 100,000)",
  caption =
    "BJP voting among centrist voters in constituencies receiving manufacturing FDI, 2014",
  label = "tab:centrist_voter_positive_fdi_delta_log",
  path = table_a,
  sample_note =
    "Outcome is an indicator for voting BJP in 2014. The sample is restricted to ideology-complete Center voters with a valid vote and BJP candidate present, and further restricted to assembly constituencies with at least one local manufacturing FDI project in 2009--14.",
  exposure_note =
    "The focal exposure is the change in logged local manufacturing FDI per 100,000: log(1 + projects per 100,000 in 2009--14) minus log(1 + projects per 100,000 in 2004--09). Muslim share is the 2001 Muslim population share expressed in percentage points."
)

table_b <- file.path(
  out_tables,
  "21b_centrist_voters_any_vs_no_fdi_binary.tex"
)

binary_baseline_note <- if (BINARY_ADJUST_FOR_BASELINE_FDI) {
  " All models additionally adjust for logged 2004--09 manufacturing FDI per 100,000."
} else {
  ""
}

map_b <- write_table(
  fits = fits_binary,
  coefs = coef_b,
  audit = audit_b,
  exposure = "any_fdi_0914",
  exposure_label =
    "Any manufacturing FDI, 2009--14",
  interaction_label =
    "Muslim share $\\times$ Any manufacturing FDI, 2009--14",
  caption =
    "BJP voting among centrist voters: any versus no manufacturing FDI, 2014",
  label = "tab:centrist_voter_any_fdi_binary",
  path = table_b,
  sample_note =
    "Outcome is an indicator for voting BJP in 2014. The sample includes ideology-complete Center voters with a valid vote in assembly constituencies where a BJP candidate was present; both zero-FDI and positive-FDI constituencies are retained.",
  exposure_note =
    paste0(
      "Any manufacturing FDI equals 1 if the assembly constituency received at least one local manufacturing FDI project in 2009--14 and 0 if it received none. Muslim share is the 2001 Muslim population share expressed in percentage points.",
      binary_baseline_note
    )
)

readr::write_csv(
  dplyr::mutate(map_a, analysis = "A_positive_fdi_continuous", .before = 1),
  file.path(out_audit, "05_table_mapping_positive_fdi_continuous.csv")
)

readr::write_csv(
  dplyr::mutate(map_b, analysis = "B_full_sample_binary", .before = 1),
  file.path(out_audit, "06_table_mapping_binary.csv")
)

# ------------------------------------------------------------
# 8. README and console summary
# ------------------------------------------------------------

writeLines(
  c(
    paste0("Revision: ", REV),
    "",
    "Two separate 2014 Center-voter multilevel LPM analyses.",
    "",
    "A. Positive-FDI-only continuous analysis:",
    paste0("  Current-period count variable: ", FDI_CURRENT_COUNT),
    paste0("  Current logged FDI: ", FDI_CURRENT_LOG),
    paste0("  Baseline logged FDI: ", FDI_BASELINE_LOG),
    "  Derived exposure = current logged FDI - baseline logged FDI.",
    "  Sample restriction = current-period manufacturing FDI count > 0.",
    "",
    "B. Full-sample binary analysis:",
    "  any_fdi_0914 = 1 if current-period manufacturing FDI count > 0, else 0.",
    paste0(
      "  Adjust for baseline FDI in every binary model: ",
      BINARY_ADJUST_FOR_BASELINE_FDI
    ),
    "",
    "Every model uses an AC random intercept.",
    "M1 additive; M2 interaction; M3 + state FE; M4 + C1; M5 + V2.",
    "",
    paste0("Table A: ", table_a),
    paste0("Table B: ", table_b)
  ),
  file.path(out_root, "README.txt")
)

message("")
message("============================================================")
message("CENTRIST-VOTER FDI VARIANTS COMPLETE")
message("============================================================")
message("Output root: ", out_root)
message("")
message("FDI support:")
print(support_summary, width = Inf)
message("")
message("Analysis samples:")
print(sample_audit, width = Inf)
message("")
message("Table A: ", table_a)
message("Table B: ", table_b)
message("")
message(
  "Binary baseline-FDI adjustment: ",
  BINARY_ADJUST_FOR_BASELINE_FDI
)
message("============================================================")

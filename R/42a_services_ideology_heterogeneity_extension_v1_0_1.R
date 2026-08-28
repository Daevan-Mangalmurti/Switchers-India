suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(fixest)
  library(lme4)
})

required_packages <- c("dplyr", "readr", "tibble", "fixest", "lme4")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

input_dir <- file.path(project_root, "data", "derived", "switchers_rewrite", "final")
output_dir <- file.path(project_root, "outputs", "r42a_services_ideology_extension_v1_0")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ideology_path <- file.path(input_dir, "ac_year_ideology_summary.rds")
respondent_path <- file.path(input_dir, "nes_respondent_analysis.rds")
change_path <- file.path(input_dir, "ac_change.rds")
ac_models_path <- file.path(project_root, "outputs", "ac_canonical_v1_0", "models.rds")
voter_models_path <- file.path(project_root, "outputs", "voter_canonical_v1_0", "models.rds")

required_files <- c(
  ideology_path, respondent_path, change_path,
  ac_models_path, voter_models_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required input(s):\n", paste(missing_files, collapse = "\n"))
}

ideology <- readRDS(ideology_path)
respondents <- readRDS(respondent_path)
ac_change <- readRDS(change_path)
ac_canonical_models <- readRDS(ac_models_path)
voter_canonical_models <- readRDS(voter_models_path)

if (!"AC06" %in% names(ac_canonical_models)) stop("Canonical AC06 model is missing.")
if (!"V06" %in% names(voter_canonical_models)) stop("Canonical V06 model is missing.")

services_vars <- c(
  "fdi_services_local_all_pc100k_2009",
  "fdi_services_local_all_pc100k_2014"
)

require_columns <- function(data, cols, label) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(label, " is missing required column(s): ", paste(missing, collapse = ", "))
  }
}

primary_ac_controls <- c("proxy_ac_pop", "sc_pop_share", "st_pop_share")
individual_controls <- c("religion_group", "caste_group", "education_harmonized")
ideology_levels <- c("Left", "Center", "Right")

require_columns(
  ideology,
  c(
    "ac_uid", "year", "ideology", "weighted_share_voted_bjp",
    "state_no", "pc_cluster_id", "bjp_candidate_present", "fdi_spatial_support",
    "muslim_share_2001_dist_proxy", primary_ac_controls
  ),
  "ac_year_ideology_summary"
)

require_columns(
  respondents,
  c(
    "respondent_uid", "year", "state_no", "ac_uid", "vote_valid", "voted_bjp",
    "bjp_candidate_present", "fdi_spatial_support", "ideology_complete", "voter_ideology",
    "muslim_share_2001_dist_proxy", individual_controls, primary_ac_controls
  ),
  "nes_respondent_analysis"
)

require_columns(ac_change, c("ac_uid", services_vars), "ac_change")

services_payload <- ac_change |>
  select(ac_uid, all_of(services_vars))
if (anyDuplicated(services_payload$ac_uid) > 0L) {
  stop("Services FDI payload is not unique by ac_uid.")
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

find_interaction_term <- function(coefficient_names, variables) {
  hits <- coefficient_names[
    vapply(
      strsplit(coefficient_names, ":", fixed = TRUE),
      function(parts) length(parts) == length(variables) && setequal(parts, variables),
      logical(1)
    )
  ]
  if (length(hits) != 1L) {
    stop(
      "Could not uniquely identify interaction: ", paste(variables, collapse = " x "),
      ". Hits: ", paste(hits, collapse = ", ")
    )
  }
  hits[[1]]
}

extract_focal <- function(fit, current_name) {
  if (inherits(fit, "fixest")) {
    beta <- coef(fit)
    V <- as.matrix(vcov(fit))
  } else if (inherits(fit, "merMod")) {
    beta <- lme4::fixef(fit)
    V <- as.matrix(vcov(fit))
  } else {
    stop("Unsupported model class: ", paste(class(fit), collapse = "/"))
  }

  term <- find_interaction_term(names(beta), c("muslim", current_name))
  est <- unname(beta[[term]])
  se <- sqrt(unname(V[term, term]))

  tibble(
    term = term,
    estimate = est,
    std_error = se,
    conf_low = est - qnorm(.975) * se,
    conf_high = est + qnorm(.975) * se
  )
}

relevel_if_present <- function(x, reference) {
  out <- factor(as.character(x))
  if (reference %in% levels(out)) out <- stats::relevel(out, ref = reference)
  out
}

collapse_messages <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  paste(unique(as.character(x)), collapse = " | ")
}

# -----------------------------------------------------------------------------
# AC-level Services models: exact R27b/canonical architecture
# -----------------------------------------------------------------------------

ac_base <- ideology |>
  filter(
    year == 2014,
    as.character(ideology) %in% ideology_levels
  ) |>
  select(-any_of(services_vars)) |>
  left_join(services_payload, by = "ac_uid", relationship = "many-to-one") |>
  mutate(
    ideology_name = as.character(ideology),
    y = as.numeric(weighted_share_voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),
    fdi_current = as.numeric(fdi_services_local_all_pc100k_2014),
    fdi_baseline = as.numeric(fdi_services_local_all_pc100k_2009)
  )

if (anyDuplicated(ac_base[c("ac_uid", "ideology_name")]) > 0L) {
  stop("AC Services base is not unique by AC x ideology.")
}

make_ac_sample <- function(g) {
  ac_base |>
    filter(ideology_name == g) |>
    filter(
      !is.na(y),
      bjp_candidate_present %in% TRUE,
      fdi_spatial_support %in% TRUE,
      is.finite(muslim),
      is.finite(fdi_current),
      is.finite(fdi_baseline),
      if_all(all_of(primary_ac_controls), ~ !is.na(.x)),
      !is.na(state_no),
      !is.na(pc_cluster_id)
    )
}

fit_ac_services <- function(dd) {
  feols(
    y ~
      muslim * fdi_current +
      muslim * fdi_baseline +
      proxy_ac_pop + sc_pop_share + st_pop_share |
      state_no,
    data = dd,
    vcov = ~ pc_cluster_id,
    warn = FALSE,
    notes = FALSE
  )
}

ac_models <- list()
ac_samples <- list()
ac_results <- list()

for (g in ideology_levels) {
  dd <- make_ac_sample(g)
  if (nrow(dd) == 0L) stop("No estimable AC observations for Services / ", g)
  fit <- fit_ac_services(dd)
  key <- paste("services_raw", tolower(g), sep = "__")
  focal <- extract_focal(fit, "fdi_current")

  ac_models[[key]] <- fit
  ac_samples[[key]] <- dd
  ac_results[[key]] <- focal |>
    mutate(
      level = "AC",
      sector = "Services",
      ideology = g,
      n = nrow(dd),
      n_ac = n_distinct(dd$ac_uid),
      n_states = n_distinct(dd$state_no),
      n_pc_clusters = n_distinct(dd$pc_cluster_id),
      .before = 1
    )
}

# -----------------------------------------------------------------------------
# Voter-level Services models: exact R38c native/canonical architecture
# -----------------------------------------------------------------------------

respondents2 <- respondents |>
  select(-any_of(services_vars)) |>
  left_join(services_payload, by = "ac_uid", relationship = "many-to-one")

if (anyDuplicated(respondents2$respondent_uid) > 0L) {
  stop("Respondent data are not unique by respondent_uid.")
}

voter_base <- respondents2 |>
  mutate(
    y = as.numeric(voted_bjp),
    muslim = as.numeric(muslim_share_2001_dist_proxy),
    fdi_current = as.numeric(fdi_services_local_all_pc100k_2014),
    fdi_baseline = as.numeric(fdi_services_local_all_pc100k_2009),
    ac_pop_100k = as.numeric(proxy_ac_pop) / 100000,
    sc_share_pp = 100 * as.numeric(sc_pop_share),
    st_share_pp = 100 * as.numeric(st_pop_share),
    state_fe = factor(state_no),
    ac_random = factor(ac_uid),
    religion_x = relevel_if_present(religion_group, "1: Hindu"),
    caste_x = relevel_if_present(caste_group, "4: Others"),
    education_x = relevel_if_present(education_harmonized, "Secondary"),
    ideology_name = as.character(voter_ideology)
  ) |>
  filter(
    year == 2014,
    vote_valid %in% TRUE,
    !is.na(y),
    ideology_complete %in% TRUE,
    ideology_name %in% ideology_levels,
    bjp_candidate_present %in% TRUE,
    fdi_spatial_support %in% TRUE,
    is.finite(muslim),
    is.finite(ac_pop_100k),
    is.finite(sc_share_pp),
    is.finite(st_share_pp),
    !is.na(state_fe),
    !is.na(ac_random),
    !is.na(religion_x),
    !is.na(caste_x),
    !is.na(education_x),
    is.finite(fdi_current),
    is.finite(fdi_baseline)
  )

voter_formula <-
  y ~
    muslim * fdi_current +
    muslim * fdi_baseline +
    ac_pop_100k + sc_share_pp + st_share_pp +
    religion_x + caste_x + education_x + state_fe +
    (1 | ac_random)

optimizer_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 300000)
)

fit_voter_services <- function(dd) {
  warnings <- character()
  messages <- character()

  fit <- withCallingHandlers(
    lmer(
      voter_formula,
      data = droplevels(dd),
      REML = FALSE,
      control = optimizer_control
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  list(fit = fit, warnings = unique(warnings), messages = unique(messages))
}

voter_models <- list()
voter_samples <- list()
voter_results <- list()

for (g in ideology_levels) {
  dd <- voter_base |>
    filter(ideology_name == g) |>
    droplevels()

  if (nrow(dd) == 0L) stop("No estimable voter observations for Services / ", g)

  captured <- fit_voter_services(dd)
  fit <- captured$fit
  key <- paste("services_raw", tolower(g), sep = "__")
  focal <- extract_focal(fit, "fdi_current")

  voter_models[[key]] <- fit
  voter_samples[[key]] <- dd
  voter_results[[key]] <- focal |>
    mutate(
      level = "Voter",
      sector = "Services",
      ideology = g,
      n = nrow(dd),
      n_ac = n_distinct(dd$ac_uid),
      n_states = n_distinct(dd$state_no),
      singular = isSingular(fit, tol = 1e-4),
      convergence_messages = collapse_messages(fit@optinfo$conv$lme4$messages),
      captured_warnings = collapse_messages(captured$warnings),
      .before = 1
    )
}

services_results <- do.call(
  bind_rows,
  c(
    unname(ac_results),
    unname(voter_results)
  )
)

if (nrow(services_results) != 6L) {
  stop(
    "Services results table should contain exactly 6 rows (3 ideologies x 2 levels); found ",
    nrow(services_results),
    "."
  )
}

invalid_result_columns <- names(services_results)[
  vapply(
    services_results,
    function(x) is.list(x) || is.matrix(x) || is.data.frame(x),
    logical(1)
  )
]

if (length(invalid_result_columns) > 0L) {
  stop(
    "Services results table contains non-atomic column(s): ",
    paste(invalid_result_columns, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# Canonical Center reproduction audit against AC06 and V06
# -----------------------------------------------------------------------------

new_ac_center <- extract_focal(ac_models[["services_raw__center"]], "fdi_current")
old_ac_center <- extract_focal(ac_canonical_models[["AC06"]], "fdi_current")

# Canonical voter V06 is written in the original Services variable names.
old_voter_center <- extract_focal(
  voter_canonical_models[["V06"]],
  "fdi_services_current"
)
new_voter_center <- extract_focal(
  voter_models[["services_raw__center"]],
  "fdi_current"
)

reproduction_audit <- bind_rows(
  tibble(
    level = "AC",
    canonical_model = "AC06",
    new_model = "services_raw__center",
    canonical_estimate = old_ac_center$estimate,
    new_estimate = new_ac_center$estimate,
    canonical_se = old_ac_center$std_error,
    new_se = new_ac_center$std_error,
    estimate_abs_diff = abs(canonical_estimate - new_estimate),
    se_abs_diff = abs(canonical_se - new_se),
    tolerance = 1e-8
  ),
  tibble(
    level = "Voter",
    canonical_model = "V06",
    new_model = "services_raw__center",
    canonical_estimate = old_voter_center$estimate,
    new_estimate = new_voter_center$estimate,
    canonical_se = old_voter_center$std_error,
    new_se = new_voter_center$std_error,
    estimate_abs_diff = abs(canonical_estimate - new_estimate),
    se_abs_diff = abs(canonical_se - new_se),
    tolerance = 1e-6
  )
) |>
  mutate(
    reproduces_canonical =
      estimate_abs_diff <= tolerance & se_abs_diff <= tolerance
  )

if (any(!reproduction_audit$reproduces_canonical)) {
  print(reproduction_audit, n = Inf, width = Inf)
  stop("Services Center extension fails canonical AC06/V06 reproduction audit.")
}

write_csv(
  services_results,
  file.path(output_dir, "01_services_native_ideology_coefficients.csv")
)
write_csv(
  reproduction_audit,
  file.path(output_dir, "02_center_canonical_reproduction_audit.csv")
)
saveRDS(ac_models, file.path(output_dir, "03_services_ac_native_models.rds"))
saveRDS(voter_models, file.path(output_dir, "04_services_voter_native_models.rds"))
saveRDS(ac_samples, file.path(output_dir, "05_services_ac_native_samples.rds"))
saveRDS(voter_samples, file.path(output_dir, "06_services_voter_native_samples.rds"))

writeLines(
  c(
    "R42A SERVICES IDEOLOGY EXTENSION v1.0.1",
    "",
    "Purpose: estimate the previously missing raw Services FDI x Muslim-share models for Left and Right outcomes while reproducing the existing Center Services models.",
    "AC architecture: native ideology-specific 2014 weighted BJP-share model, state FE, PC-clustered SE, primary AC controls, current + baseline Services FDI.",
    "Voter architecture: native ideology-specific 2014 LPM mixed model, AC random intercept, state FE, individual controls, primary AC controls, current + baseline Services FDI.",
    "Center AC model is required to reproduce canonical AC06.",
    "Center voter model is required to reproduce canonical V06.",
    "Only Services x Left and Services x Right are new substantive ideology-sector cells; Center is an audit reproduction.",
    "No artifact registry is modified by this script."
  ),
  file.path(output_dir, "00_provenance.txt")
)

cat("\n===== R42A SERVICES IDEOLOGY EXTENSION v1.0.1 =====\n")
print(reproduction_audit, n = Inf, width = Inf)
cat("\n===== SERVICES NATIVE IDEOLOGY COEFFICIENTS =====\n")
print(services_results, n = Inf, width = Inf)
cat("OUTPUT_DIR=", output_dir, "\n", sep = "")
cat("R42A_SERVICES_IDEOLOGY_EXTENSION_COMPLETE\n")

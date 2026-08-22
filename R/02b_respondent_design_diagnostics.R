# ============================================================
# 02b_respondent_design_diagnostics.R
# Respondent-level design audit for BJP-vote regressions.
#
# PURPOSE
# -------
# Before adding respondent-level LPM/logit specification curves to
# 02_explore_models.R, audit:
#   1. binary outcome and BJP-candidate availability
#   2. voter-ideology coverage and BJP voting by ideology
#   3. survey weights and weighted effective sample size
#   4. geographic / sampling-cluster support
#   5. contextual treatment support and within-geography variation
#   6. voter- and contextual-control missingness / joint samples
#   7. selection into BJP-candidate-present constituencies
#   8. PC x district cross-classification for clustering choices
#   9. repeated-cross-section / AC-overlap feasibility
#  10. binary-outcome sparsity relevant to logit/separation
#
# This script fits NO substantive regressions. It writes diagnostic CSVs/PDFs
# only. The resulting files are intended to be reviewed before freezing the
# respondent-level specification universe.
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

voters <- readRDS(
  file.path(
    paths$final_dir,
    "nes_respondent_analysis.rds"
  )
)

RESPONDENT_DIAGNOSTIC_REVISION <-
  "2026-08-07-v1-respondent-design-audit"

message(
  "Loading respondent-design diagnostics revision: ",
  RESPONDENT_DIAGNOSTIC_REVISION
)

# ============================================================
# 0. OUTPUT DIRECTORIES
# ============================================================

respondent_diag_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "respondent_design_diagnostics"
)

respondent_diag_summary_dir <- file.path(
  respondent_diag_root,
  "summaries"
)

respondent_diag_figure_dir <- file.path(
  respondent_diag_root,
  "figures"
)

respondent_diag_manifest_dir <- file.path(
  respondent_diag_root,
  "manifests"
)

purrr::walk(
  c(
    respondent_diag_root,
    respondent_diag_summary_dir,
    respondent_diag_figure_dir,
    respondent_diag_manifest_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. HELPERS AND VARIABLE VALIDATION
# ============================================================

weighted_ess_local <- function(w) {
  w <- as.numeric(w)
  w <- w[
    is.finite(w) &
      w > 0
  ]

  if (length(w) == 0) {
    return(NA_real_)
  }

  sum(w)^2 /
    sum(w^2)
}

weighted_mean_local <- function(
    x,
    w
) {
  x <- as.numeric(x)
  w <- as.numeric(w)

  keep <-
    is.finite(x) &
    is.finite(w) &
    w > 0

  if (!any(keep)) {
    return(NA_real_)
  }

  stats::weighted.mean(
    x[keep],
    w[keep]
  )
}

safe_quantile_local <- function(
    x,
    p
) {
  x <- as.numeric(x)
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0) {
    return(NA_real_)
  }

  as.numeric(
    stats::quantile(
      x,
      probs = p,
      na.rm = TRUE,
      names = FALSE
    )
  )
}

weighted_share_condition <- function(
    condition,
    w
) {
  condition <- as.numeric(condition)
  weighted_mean_local(
    condition,
    w
  )
}

assert_respondent_columns <- function(
    data,
    columns,
    label = "nes_respondent_analysis"
) {
  columns <- unique(
    columns[
      !is.na(columns) &
        nzchar(columns)
    ]
  )

  missing <- setdiff(
    columns,
    names(data)
  )

  if (length(missing) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }

  invisible(data)
}

core_required <- c(
  "year",
  "respondent_uid",
  "vote_valid",
  "voted_bjp",
  "bjp_candidate_present",
  "voter_ideology",
  "survey_weight_norm_year",
  "ac_uid",
  "pc_cluster_id",
  "psu_uid",
  "state_no",
  "district_harmonization_group_id"
)

assert_respondent_columns(
  voters,
  core_required
)

# Analysis-only logged employment-intensity proxy, matching the repaired
# contextual C2/C3 definition in the AC-level specification curves.
if (
  "employment_per_total_population" %in%
    names(voters)
) {
  voters <- voters |>
    dplyr::mutate(
      log1p_employment_per_total_population =
        dplyr::if_else(
          is.finite(
            employment_per_total_population
          ) &
            employment_per_total_population >= 0,
          log1p(
            employment_per_total_population
          ),
          NA_real_
        )
    )
}

# Primary respondent samples under discussion.
voters <- voters |>
  dplyr::mutate(
    respondent_sample_all_valid =
      vote_valid &
      !is.na(voted_bjp),

    respondent_sample_bjp_available =
      respondent_sample_all_valid &
      !is.na(bjp_candidate_present) &
      bjp_candidate_present,

    ideology_lcr =
      as.character(voter_ideology) %in%
      c(
        "Left",
        "Center",
        "Right"
      ),

    respondent_sample_triple_lcr =
      respondent_sample_bjp_available &
      ideology_lcr
  )

# Record availability of every planned diagnostic variable instead of allowing
# optional diagnostics to fail silently.
diagnostic_variable_manifest <- tibble::tibble(
  variable = c(
    core_required,
    "religion_group",
    "caste_group",
    "education_harmonized",
    "income_harmonized",
    "household_income_monthly",
    "polling_station_id",
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "employment_per_total_population",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    "log_secc_cons_pc",
    "log1p_fdi_total_local_all_pc100k",
    "log1p_fdi_mfg_local_all_pc100k",
    "log1p_fdi_services_local_all_pc100k",
    "muslim_share_2001_dist_proxy",
    "muslim_share_2011_dist_proxy",
    "mig_prior_5yr_share_ac_pop",
    "mig_prior_5_15yr_share_ac_pop",
    "mig_total_upto_2001_share_ac_pop",
    "target_bengali_bhojpuri_share_2001_dist_proxy"
  )
) |>
  dplyr::mutate(
    available =
      variable %in%
      names(voters)
  )

readr::write_csv(
  diagnostic_variable_manifest,
  file.path(
    respondent_diag_manifest_dir,
    "respondent_diagnostic_variable_manifest.csv"
  )
)

# ============================================================
# 2. OUTCOME COVERAGE AND BJP-CANDIDATE AVAILABILITY
# ============================================================

respondent_outcome_summary <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    )
  ) |>
  dplyr::summarise(
    n_respondents =
      dplyr::n(),

    n_valid_vote =
      sum(
        respondent_sample_all_valid,
        na.rm = TRUE
      ),

    share_valid_vote =
      mean(
        respondent_sample_all_valid,
        na.rm = TRUE
      ),

    n_bjp_candidate_present =
      sum(
        !is.na(bjp_candidate_present) &
          bjp_candidate_present
      ),

    share_bjp_candidate_present =
      mean(
        bjp_candidate_present,
        na.rm = TRUE
      ),

    n_valid_bjp_available =
      sum(
        respondent_sample_bjp_available,
        na.rm = TRUE
      ),

    share_valid_sample_retained_by_candidate_rule =
      if (
        sum(
          respondent_sample_all_valid,
          na.rm = TRUE
        ) > 0
      ) {
        sum(
          respondent_sample_bjp_available,
          na.rm = TRUE
        ) /
          sum(
            respondent_sample_all_valid,
            na.rm = TRUE
          )
      } else {
        NA_real_
      },

    bjp_vote_share_unweighted_all_valid =
      mean(
        voted_bjp[
          respondent_sample_all_valid
        ],
        na.rm = TRUE
      ),

    bjp_vote_share_weighted_all_valid =
      weighted_mean_local(
        voted_bjp[
          respondent_sample_all_valid
        ],
        survey_weight_norm_year[
          respondent_sample_all_valid
        ]
      ),

    bjp_vote_share_unweighted_bjp_available =
      mean(
        voted_bjp[
          respondent_sample_bjp_available
        ],
        na.rm = TRUE
      ),

    bjp_vote_share_weighted_bjp_available =
      weighted_mean_local(
        voted_bjp[
          respondent_sample_bjp_available
        ],
        survey_weight_norm_year[
          respondent_sample_bjp_available
        ]
      ),

    .by = year
  )

readr::write_csv(
  respondent_outcome_summary,
  file.path(
    respondent_diag_summary_dir,
    "respondent_outcome_and_candidate_summary.csv"
  )
)

# Candidate presence at the AC-year level, so respondent counts do not
# mechanically overweight large NES clusters.
candidate_ac_year <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    ),
    !is.na(ac_uid)
  ) |>
  dplyr::distinct(
    ac_uid,
    year,
    .keep_all = TRUE
  )

candidate_ac_year_summary <- candidate_ac_year |>
  dplyr::summarise(
    n_ac_years =
      dplyr::n(),

    n_candidate_present =
      sum(
        bjp_candidate_present,
        na.rm = TRUE
      ),

    n_candidate_absent =
      sum(
        !bjp_candidate_present,
        na.rm = TRUE
      ),

    share_candidate_present =
      mean(
        bjp_candidate_present,
        na.rm = TRUE
      ),

    .by = year
  )

readr::write_csv(
  candidate_ac_year_summary,
  file.path(
    respondent_diag_summary_dir,
    "bjp_candidate_availability_ac_year.csv"
  )
)

# ============================================================
# 3. IDEOLOGY COVERAGE AND BJP VOTING BY IDEOLOGY
# ============================================================

ideology_levels_audit <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    )
  ) |>
  dplyr::mutate(
    ideology_audit =
      dplyr::if_else(
        is.na(voter_ideology),
        "Missing",
        as.character(
          voter_ideology
        )
      )
  ) |>
  dplyr::summarise(
    n =
      dplyr::n(),

    weighted_n =
      sum(
        survey_weight_norm_year,
        na.rm = TRUE
      ),

    .by = c(
      year,
      ideology_audit
    )
  ) |>
  dplyr::group_by(
    year
  ) |>
  dplyr::mutate(
    share_unweighted =
      n /
      sum(n),

    share_weighted =
      weighted_n /
      sum(weighted_n)
  ) |>
  dplyr::ungroup()

readr::write_csv(
  ideology_levels_audit,
  file.path(
    respondent_diag_summary_dir,
    "respondent_ideology_coverage.csv"
  )
)

ideology_bjp_vote <- voters |>
  dplyr::filter(
    respondent_sample_bjp_available,
    !is.na(voter_ideology),
    year %in% c(
      2009,
      2014
    )
  ) |>
  dplyr::summarise(
    n =
      dplyr::n(),

    bjp_share_unweighted =
      mean(
        voted_bjp,
        na.rm = TRUE
      ),

    bjp_share_weighted =
      weighted_mean_local(
        voted_bjp,
        survey_weight_norm_year
      ),

    weighted_ess =
      weighted_ess_local(
        survey_weight_norm_year
      ),

    .by = c(
      year,
      voter_ideology
    )
  )

readr::write_csv(
  ideology_bjp_vote,
  file.path(
    respondent_diag_summary_dir,
    "bjp_vote_by_year_ideology.csv"
  )
)

ideology_primary_sample_summary <- voters |>
  dplyr::filter(
    respondent_sample_bjp_available
  ) |>
  dplyr::summarise(
    n_bjp_available_valid =
      dplyr::n(),

    n_ideology_complete =
      sum(
        !is.na(voter_ideology)
      ),

    n_lcr =
      sum(
        ideology_lcr
      ),

    n_mixed =
      sum(
        as.character(voter_ideology) ==
          "Mixed",
        na.rm = TRUE
      ),

    share_lcr_among_bjp_available_valid =
      mean(
        ideology_lcr
      ),

    share_mixed_among_ideology_complete =
      if (
        sum(
          !is.na(voter_ideology)
        ) > 0
      ) {
        sum(
          as.character(voter_ideology) ==
            "Mixed",
          na.rm = TRUE
        ) /
          sum(
            !is.na(voter_ideology)
          )
      } else {
        NA_real_
      },

    .by = year
  )

readr::write_csv(
  ideology_primary_sample_summary,
  file.path(
    respondent_diag_summary_dir,
    "ideology_primary_triple_sample_summary.csv"
  )
)

# ============================================================
# 4. SURVEY-WEIGHT DIAGNOSTICS
# ============================================================

survey_weight_summary <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    )
  ) |>
  dplyr::summarise(
    n =
      dplyr::n(),

    n_weight_nonmissing =
      sum(
        !is.na(
          survey_weight_norm_year
        )
      ),

    n_weight_positive =
      sum(
        is.finite(
          survey_weight_norm_year
        ) &
          survey_weight_norm_year > 0
      ),

    min =
      if (
        any(
          is.finite(
            survey_weight_norm_year
          )
        )
      ) {
        min(
          survey_weight_norm_year,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },

    p01 =
      safe_quantile_local(
        survey_weight_norm_year,
        0.01
      ),

    p05 =
      safe_quantile_local(
        survey_weight_norm_year,
        0.05
      ),

    median =
      safe_quantile_local(
        survey_weight_norm_year,
        0.50
      ),

    p95 =
      safe_quantile_local(
        survey_weight_norm_year,
        0.95
      ),

    p99 =
      safe_quantile_local(
        survey_weight_norm_year,
        0.99
      ),

    max =
      if (
        any(
          is.finite(
            survey_weight_norm_year
          )
        )
      ) {
        max(
          survey_weight_norm_year,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },

    mean =
      mean(
        survey_weight_norm_year,
        na.rm = TRUE
      ),

    sd =
      stats::sd(
        survey_weight_norm_year,
        na.rm = TRUE
      ),

    coefficient_of_variation =
      sd /
      mean,

    weighted_ess =
      weighted_ess_local(
        survey_weight_norm_year
      ),

    ess_share_of_raw_n =
      weighted_ess /
      n,

    .by = year
  )

readr::write_csv(
  survey_weight_summary,
  file.path(
    respondent_diag_summary_dir,
    "survey_weight_summary.csv"
  )
)

# ============================================================
# 5. GEOGRAPHIC AND SAMPLING-CLUSTER SUPPORT
# ============================================================

geographic_support <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    )
  ) |>
  dplyr::summarise(
    n_respondents =
      dplyr::n(),

    n_psus =
      dplyr::n_distinct(
        psu_uid,
        na.rm = TRUE
      ),

    n_acs =
      dplyr::n_distinct(
        ac_uid,
        na.rm = TRUE
      ),

    n_pcs =
      dplyr::n_distinct(
        pc_cluster_id,
        na.rm = TRUE
      ),

    n_district_groups =
      dplyr::n_distinct(
        district_harmonization_group_id,
        na.rm = TRUE
      ),

    n_states =
      dplyr::n_distinct(
        state_no,
        na.rm = TRUE
      ),

    .by = year
  )

readr::write_csv(
  geographic_support,
  file.path(
    respondent_diag_summary_dir,
    "geographic_support_by_year.csv"
  )
)

cluster_size_summary_one <- function(
    data,
    cluster_var,
    cluster_label,
    sample_label
) {
  if (
    !cluster_var %in%
    names(data)
  ) {
    return(
      tibble::tibble()
    )
  }

  cluster_sizes <- data |>
    dplyr::filter(
      !is.na(
        .data[[
          cluster_var
        ]]
      )
    ) |>
    dplyr::count(
      year,
      .data[[
        cluster_var
      ]],
      name = "respondents"
    )

  cluster_sizes |>
    dplyr::summarise(
      analytic_sample =
        sample_label,

      cluster_level =
        cluster_label,

      n_clusters =
        dplyr::n(),

      n_singleton_clusters =
        sum(
          respondents == 1
        ),

      share_singleton_clusters =
        mean(
          respondents == 1
        ),

      min_cluster_n =
        min(respondents),

      p10_cluster_n =
        safe_quantile_local(
          respondents,
          0.10
        ),

      median_cluster_n =
        stats::median(
          respondents
        ),

      p90_cluster_n =
        safe_quantile_local(
          respondents,
          0.90
        ),

      max_cluster_n =
        max(respondents),

      .by = year
    )
}

cluster_samples <- list(
  "All NES respondents" =
    voters,

  "Valid voters, BJP candidate present" =
    voters |>
    dplyr::filter(
      respondent_sample_bjp_available
    ),

  "Triple sample: valid, BJP present, Left/Center/Right" =
    voters |>
    dplyr::filter(
      respondent_sample_triple_lcr
    )
)

cluster_levels <- list(
  "PSU / polling station" =
    "psu_uid",

  "Assembly constituency" =
    "ac_uid",

  "Parliamentary constituency" =
    "pc_cluster_id",

  "District harmonization group" =
    "district_harmonization_group_id"
)

cluster_size_summary <- purrr::imap_dfr(
  cluster_samples,
  function(sample_data, sample_label) {
    purrr::imap_dfr(
      cluster_levels,
      function(cluster_var, cluster_label) {
        cluster_size_summary_one(
          sample_data,
          cluster_var,
          cluster_label,
          sample_label
        )
      }
    )
  }
)

readr::write_csv(
  cluster_size_summary,
  file.path(
    respondent_diag_summary_dir,
    "cluster_size_summary.csv"
  )
)

# ============================================================
# 6. PC x DISTRICT CROSS-CLASSIFICATION
# ============================================================

pc_district_unique <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    ),
    !is.na(pc_cluster_id),
    !is.na(
      district_harmonization_group_id
    )
  ) |>
  dplyr::distinct(
    year,
    pc_cluster_id,
    district_harmonization_group_id
  )

pc_crossing <- pc_district_unique |>
  dplyr::summarise(
    n_district_groups =
      dplyr::n_distinct(
        district_harmonization_group_id
      ),
    .by = c(
      year,
      pc_cluster_id
    )
  )

district_crossing <- pc_district_unique |>
  dplyr::summarise(
    n_pcs =
      dplyr::n_distinct(
        pc_cluster_id
      ),
    .by = c(
      year,
      district_harmonization_group_id
    )
  )

pc_district_crossclassification_summary <-
  dplyr::bind_rows(
    pc_crossing |>
      dplyr::summarise(
        perspective =
          "PC -> district groups",

        n_units =
          dplyr::n(),

        n_crossing_multiple =
          sum(
            n_district_groups > 1
          ),

        share_crossing_multiple =
          mean(
            n_district_groups > 1
          ),

        median_partners =
          stats::median(
            n_district_groups
          ),

        max_partners =
          max(
            n_district_groups
          ),

        .by = year
      ),

    district_crossing |>
      dplyr::summarise(
        perspective =
          "District group -> PCs",

        n_units =
          dplyr::n(),

        n_crossing_multiple =
          sum(
            n_pcs > 1
          ),

        share_crossing_multiple =
          mean(
            n_pcs > 1
          ),

        median_partners =
          stats::median(
            n_pcs
          ),

        max_partners =
          max(
            n_pcs
          ),

        .by = year
      )
  )

readr::write_csv(
  pc_district_crossclassification_summary,
  file.path(
    respondent_diag_summary_dir,
    "pc_district_crossclassification_summary.csv"
  )
)

# ============================================================
# 7. REPEATED-CROSS-SECTION / AC OVERLAP FEASIBILITY
# ============================================================

ac_year_presence <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    ),
    !is.na(ac_uid)
  ) |>
  dplyr::summarise(
    respondents =
      dplyr::n(),

    valid_voters =
      sum(
        respondent_sample_all_valid,
        na.rm = TRUE
      ),

    bjp_available_valid =
      sum(
        respondent_sample_bjp_available,
        na.rm = TRUE
      ),

    .by = c(
      ac_uid,
      year
    )
  )

ac_overlap <- ac_year_presence |>
  dplyr::summarise(
    n_years =
      dplyr::n_distinct(
        year
      ),

    has_2009 =
      any(
        year == 2009
      ),

    has_2014 =
      any(
        year == 2014
      ),

    respondents_2009 =
      sum(
        respondents[
          year == 2009
        ],
        na.rm = TRUE
      ),

    respondents_2014 =
      sum(
        respondents[
          year == 2014
        ],
        na.rm = TRUE
      ),

    .by = ac_uid
  )

ac_overlap_summary <- tibble::tibble(
  n_unique_acs =
    nrow(
      ac_overlap
    ),

  n_acs_2009 =
    sum(
      ac_overlap$has_2009
    ),

  n_acs_2014 =
    sum(
      ac_overlap$has_2014
    ),

  n_acs_both_years =
    sum(
      ac_overlap$has_2009 &
        ac_overlap$has_2014
    ),

  share_acs_both_years =
    mean(
      ac_overlap$has_2009 &
        ac_overlap$has_2014
    ),

  respondents_2009_in_both_year_acs =
    sum(
      ac_overlap$respondents_2009[
        ac_overlap$has_2009 &
          ac_overlap$has_2014
      ],
      na.rm = TRUE
    ),

  respondents_2014_in_both_year_acs =
    sum(
      ac_overlap$respondents_2014[
        ac_overlap$has_2009 &
          ac_overlap$has_2014
      ],
      na.rm = TRUE
    )
)

readr::write_csv(
  ac_overlap_summary,
  file.path(
    respondent_diag_summary_dir,
    "ac_repeated_cross_section_overlap.csv"
  )
)

readr::write_csv(
  ac_overlap |>
    dplyr::arrange(
      dplyr::desc(
        has_2009 &
          has_2014
      ),
      ac_uid
    ),
  file.path(
    respondent_diag_summary_dir,
    "ac_repeated_cross_section_detail.csv"
  )
)

# ============================================================
# 8. REPRESENTATIVE CONTEXTUAL-TREATMENT SUPPORT
# ============================================================

diagnostic_treatment_candidates <- c(
  "log1p_fdi_total_local_all_pc100k",
  "log1p_fdi_mfg_local_all_pc100k",
  "log1p_fdi_services_local_all_pc100k",
  "muslim_share_2001_dist_proxy",
  "muslim_share_2011_dist_proxy",
  "mig_prior_5yr_share_ac_pop",
  "mig_prior_5_15yr_share_ac_pop",
  "mig_total_upto_2001_share_ac_pop",
  "target_bengali_bhojpuri_share_2001_dist_proxy"
)

diagnostic_treatment_vars <- intersect(
  diagnostic_treatment_candidates,
  names(voters)
)

# Collapse respondents to one AC-year before measuring contextual variation so
# large NES respondent clusters do not receive extra weight.
context_ac_year <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    ),
    !is.na(ac_uid)
  ) |>
  dplyr::distinct(
    ac_uid,
    year,
    .keep_all = TRUE
  )

summarize_variable_support <- function(
    data,
    variable
) {
  x <- as.numeric(
    data[[
      variable
    ]]
  )

  finite <- x[
    is.finite(x)
  ]

  tibble::tibble(
    variable =
      variable,

    n =
      length(x),

    n_finite =
      length(finite),

    share_finite =
      mean(
        is.finite(x)
      ),

    n_unique =
      dplyr::n_distinct(
        finite
      ),

    min =
      if (
        length(finite) > 0
      ) {
        min(finite)
      } else {
        NA_real_
      },

    p25 =
      safe_quantile_local(
        finite,
        0.25
      ),

    median =
      safe_quantile_local(
        finite,
        0.50
      ),

    p75 =
      safe_quantile_local(
        finite,
        0.75
      ),

    max =
      if (
        length(finite) > 0
      ) {
        max(finite)
      } else {
        NA_real_
      },

    share_zero =
      if (
        length(finite) > 0
      ) {
        mean(
          finite == 0
        )
      } else {
        NA_real_
      }
  )
}

treatment_overall_support <- purrr::map_dfr(
  diagnostic_treatment_vars,
  function(v) {
    dplyr::bind_rows(
      context_ac_year |>
        dplyr::filter(
          year == 2009
        ) |>
        summarize_variable_support(v) |>
        dplyr::mutate(
          year = 2009L
        ),

      context_ac_year |>
        dplyr::filter(
          year == 2014
        ) |>
        summarize_variable_support(v) |>
        dplyr::mutate(
          year = 2014L
        )
    )
  }
) |>
  dplyr::relocate(
    year,
    variable
  )

readr::write_csv(
  treatment_overall_support,
  file.path(
    respondent_diag_summary_dir,
    "contextual_treatment_overall_support.csv"
  )
)

# Within-state-year and within-PC-year variation across distinct AC-years.
within_group_support_one <- function(
    data,
    variable,
    grouping,
    grouping_label
) {
  grouped <- data |>
    dplyr::filter(
      is.finite(
        .data[[
          variable
        ]]
      )
    ) |>
    dplyr::summarise(
      n_acs =
        dplyr::n_distinct(
          ac_uid
        ),

      n_unique =
        dplyr::n_distinct(
          .data[[
            variable
          ]]
        ),

      sd =
        stats::sd(
          as.numeric(
            .data[[
              variable
            ]]
          ),
          na.rm = TRUE
        ),

      range =
        max(
          as.numeric(
            .data[[
              variable
            ]]
          ),
          na.rm = TRUE
        ) -
        min(
          as.numeric(
            .data[[
              variable
            ]]
          ),
          na.rm = TRUE
        ),

      .by = dplyr::all_of(
        grouping
      )
    )

  grouped |>
    dplyr::summarise(
      variable =
        variable,

      grouping =
        grouping_label,

      n_groups =
        dplyr::n(),

      n_groups_with_2plus_acs =
        sum(
          n_acs >= 2
        ),

      share_groups_with_2plus_acs =
        mean(
          n_acs >= 2
        ),

      n_groups_with_variation =
        sum(
          n_unique >= 2 &
            is.finite(sd) &
            sd > 0
        ),

      share_groups_with_variation =
        mean(
          n_unique >= 2 &
            is.finite(sd) &
            sd > 0
        ),

      median_within_sd =
        stats::median(
          sd[
            is.finite(sd)
          ],
          na.rm = TRUE
        ),

      median_within_range =
        stats::median(
          range[
            is.finite(range)
          ],
          na.rm = TRUE
        )
    )
}

treatment_within_group_support <-
  purrr::map_dfr(
    diagnostic_treatment_vars,
    function(v) {
      dplyr::bind_rows(
        within_group_support_one(
          context_ac_year,
          v,
          c(
            "state_no",
            "year"
          ),
          "State-year"
        ),

        within_group_support_one(
          context_ac_year,
          v,
          c(
            "pc_cluster_id",
            "year"
          ),
          "PC-year"
        )
      )
    }
  )

readr::write_csv(
  treatment_within_group_support,
  file.path(
    respondent_diag_summary_dir,
    "contextual_treatment_within_group_support.csv"
  )
)

# Within-AC change between 2009 and 2014 for contextual variables.
within_ac_change_support <- purrr::map_dfr(
  diagnostic_treatment_vars,
  function(v) {
    wide <- context_ac_year |>
      dplyr::select(
        ac_uid,
        year,
        value = dplyr::all_of(v)
      ) |>
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "year_"
      )

    if (
      !all(
        c(
          "year_2009",
          "year_2014"
        ) %in%
        names(wide)
      )
    ) {
      return(
        tibble::tibble()
      )
    }

    wide |>
      dplyr::mutate(
        both_finite =
          is.finite(
            year_2009
          ) &
          is.finite(
            year_2014
          ),

        changed =
          both_finite &
          year_2009 !=
          year_2014,

        absolute_change =
          dplyr::if_else(
            both_finite,
            abs(
              year_2014 -
                year_2009
            ),
            NA_real_
          )
      ) |>
      dplyr::summarise(
        variable =
          v,

        n_acs =
          dplyr::n(),

        n_both_years_finite =
          sum(
            both_finite
          ),

        n_changed =
          sum(
            changed
          ),

        share_changed_among_both =
          if (
            sum(
              both_finite
            ) > 0
          ) {
            sum(
              changed
            ) /
              sum(
                both_finite
              )
          } else {
            NA_real_
          },

        median_absolute_change =
          stats::median(
            absolute_change[
              both_finite
            ],
            na.rm = TRUE
          )
      )
  }
)

readr::write_csv(
  within_ac_change_support,
  file.path(
    respondent_diag_summary_dir,
    "contextual_treatment_within_ac_change_support.csv"
  )
)

# ============================================================
# 9. VOTER-LEVEL AND CONTEXTUAL-CONTROL MISSINGNESS
# ============================================================

voter_control_candidates <- c(
  "religion_group",
  "caste_group",
  "education_harmonized",
  "income_harmonized"
)

voter_control_vars <- intersect(
  voter_control_candidates,
  names(voters)
)

context_control_candidates <- c(
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share",
  "log1p_employment_per_total_population",
  "ed_sec_share",
  "log_secc_cons_pc"
)

context_control_vars <- intersect(
  context_control_candidates,
  names(voters)
)

control_missingness <- purrr::map_dfr(
  c(
    voter_control_vars,
    context_control_vars
  ),
  function(v) {
    voters |>
      dplyr::filter(
        year %in% c(
          2009,
          2014
        )
      ) |>
      dplyr::summarise(
        variable =
          v,

        n =
          dplyr::n(),

        n_nonmissing =
          sum(
            !is.na(
              .data[[
                v
              ]]
            )
          ),

        pct_nonmissing =
          100 *
          mean(
            !is.na(
              .data[[
                v
              ]]
            )
          ),

        n_nonmissing_primary_sample =
          sum(
            respondent_sample_bjp_available &
              !is.na(
                .data[[
                  v
                ]]
              )
          ),

        pct_nonmissing_primary_sample =
          100 *
          mean(
            !is.na(
              .data[[
                v
              ]]
            )[
              respondent_sample_bjp_available
            ]
          ),

        .by = year
      )
  }
)

readr::write_csv(
  control_missingness,
  file.path(
    respondent_diag_summary_dir,
    "respondent_control_missingness.csv"
  )
)

# Coherent voter-control blocks under discussion.
voter_control_blocks <- list(
  V0 = character(0),
  V1 = c(
    "religion_group",
    "caste_group"
  ),
  V2 = c(
    "religion_group",
    "caste_group",
    "education_harmonized"
  ),
  V3 = c(
    "religion_group",
    "caste_group",
    "education_harmonized",
    "income_harmonized"
  )
)

# Contextual control blocks matched to the AC-level hierarchy.
context_control_blocks <- list(
  C0 = c(
    "proxy_ac_pop",
    "con08_land_area"
  ),
  C1 = c(
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share"
  ),
  C2 = c(
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share"
  ),
  C3 = c(
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "log1p_employment_per_total_population",
    "ed_sec_share",
    "log_secc_cons_pc"
  )
)

# Record whether all variables required by each proposed block actually exist.
control_block_availability <- dplyr::bind_rows(
  purrr::imap_dfr(
    voter_control_blocks,
    function(vars, block) {
      tibble::tibble(
        control_domain =
          "Voter",
        block =
          block,
        variables =
          paste(
            vars,
            collapse = " + "
          ),
        all_variables_available =
          all(
            vars %in%
            names(voters)
          ),
        missing_variables =
          paste(
            setdiff(
              vars,
              names(voters)
            ),
            collapse = ", "
          )
      )
    }
  ),

  purrr::imap_dfr(
    context_control_blocks,
    function(vars, block) {
      tibble::tibble(
        control_domain =
          "Context",
        block =
          block,
        variables =
          paste(
            vars,
            collapse = " + "
          ),
        all_variables_available =
          all(
            vars %in%
            names(voters)
          ),
        missing_variables =
          paste(
            setdiff(
              vars,
              names(voters)
            ),
            collapse = ", "
          )
      )
    }
  )
)

readr::write_csv(
  control_block_availability,
  file.path(
    respondent_diag_manifest_dir,
    "respondent_control_block_availability.csv"
  )
)

complete_on_vars <- function(
    data,
    vars
) {
  if (length(vars) == 0) {
    return(
      rep(
        TRUE,
        nrow(data)
      )
    )
  }

  if (
    !all(
      vars %in%
      names(data)
    )
  ) {
    return(
      rep(
        FALSE,
        nrow(data)
      )
    )
  }

  stats::complete.cases(
    data[
      ,
      vars,
      drop = FALSE
    ]
  )
}

joint_control_sample_sizes <- purrr::imap_dfr(
  voter_control_blocks,
  function(voter_vars, voter_block) {
    purrr::imap_dfr(
      context_control_blocks,
      function(context_vars, context_block) {
        voter_complete <-
          complete_on_vars(
            voters,
            voter_vars
          )

        context_complete <-
          complete_on_vars(
            voters,
            context_vars
          )

        voters |>
          dplyr::mutate(
            complete_controls =
              voter_complete &
              context_complete
          ) |>
          dplyr::filter(
            year %in% c(
              2009,
              2014
            )
          ) |>
          dplyr::summarise(
            voter_block =
              voter_block,

            context_block =
              context_block,

            n_all_respondents =
              dplyr::n(),

            n_valid_vote =
              sum(
                respondent_sample_all_valid &
                  complete_controls
              ),

            n_bjp_available_valid =
              sum(
                respondent_sample_bjp_available &
                  complete_controls
              ),

            n_triple_lcr =
              sum(
                respondent_sample_triple_lcr &
                  complete_controls
              ),

            weighted_ess_bjp_available =
              weighted_ess_local(
                survey_weight_norm_year[
                  respondent_sample_bjp_available &
                    complete_controls
                ]
              ),

            weighted_ess_triple_lcr =
              weighted_ess_local(
                survey_weight_norm_year[
                  respondent_sample_triple_lcr &
                    complete_controls
                ]
              ),

            .by = year
          )
      }
    )
  }
)

readr::write_csv(
  joint_control_sample_sizes,
  file.path(
    respondent_diag_summary_dir,
    "joint_voter_context_control_sample_sizes.csv"
  )
)

# ============================================================
# 10. CANDIDATE-AVAILABILITY SELECTION ON OBSERVED CONTEXT
# ============================================================

candidate_balance_vars <- intersect(
  c(
    "log1p_fdi_total_local_all_pc100k",
    "log1p_fdi_mfg_local_all_pc100k",
    "log1p_fdi_services_local_all_pc100k",
    "muslim_share_2001_dist_proxy",
    "muslim_share_2011_dist_proxy",
    "mig_prior_5yr_share_ac_pop",
    "mig_prior_5_15yr_share_ac_pop",
    "mig_total_upto_2001_share_ac_pop",
    "proxy_ac_pop",
    "con08_land_area",
    "sc_pop_share",
    "st_pop_share",
    "ed_sec_share",
    "log_secc_cons_pc"
  ),
  names(candidate_ac_year)
)

candidate_balance_long <- candidate_ac_year |>
  dplyr::filter(
    !is.na(
      bjp_candidate_present
    )
  ) |>
  dplyr::mutate(
    candidate_group =
      dplyr::if_else(
        bjp_candidate_present,
        "BJP candidate present",
        "BJP candidate absent"
      )
  ) |>
  dplyr::select(
    year,
    candidate_group,
    dplyr::all_of(
      candidate_balance_vars
    )
  ) |>
  tidyr::pivot_longer(
    cols =
      dplyr::all_of(
        candidate_balance_vars
      ),
    names_to =
      "variable",
    values_to =
      "value"
  ) |>
  dplyr::summarise(
    n =
      sum(
        is.finite(
          value
        )
      ),

    mean =
      mean(
        value,
        na.rm = TRUE
      ),

    sd =
      stats::sd(
        value,
        na.rm = TRUE
      ),

    .by = c(
      year,
      variable,
      candidate_group
    )
  ) |>
  tidyr::pivot_wider(
    names_from =
      candidate_group,
    values_from =
      c(
        n,
        mean,
        sd
      ),
    names_glue =
      "{.value}_{candidate_group}"
  )

# Safer standardized-mean-difference computation with non-syntactic group names.
candidate_balance_names <- names(
  candidate_balance_long
)

present_mean_col <-
  candidate_balance_names[
    stringr::str_detect(
      candidate_balance_names,
      "^mean_BJP candidate present$"
    )
  ]

absent_mean_col <-
  candidate_balance_names[
    stringr::str_detect(
      candidate_balance_names,
      "^mean_BJP candidate absent$"
    )
  ]

present_sd_col <-
  candidate_balance_names[
    stringr::str_detect(
      candidate_balance_names,
      "^sd_BJP candidate present$"
    )
  ]

absent_sd_col <-
  candidate_balance_names[
    stringr::str_detect(
      candidate_balance_names,
      "^sd_BJP candidate absent$"
    )
  ]

if (
  length(present_mean_col) == 1 &&
  length(absent_mean_col) == 1 &&
  length(present_sd_col) == 1 &&
  length(absent_sd_col) == 1
) {
  candidate_balance_long <-
    candidate_balance_long |>
    dplyr::mutate(
      pooled_sd =
        sqrt(
          (
            .data[[
              present_sd_col
            ]]^2 +
              .data[[
                absent_sd_col
              ]]^2
          ) /
            2
        ),

      standardized_mean_difference =
        dplyr::if_else(
          is.finite(
            pooled_sd
          ) &
            pooled_sd > 0,
          (
            .data[[
              present_mean_col
            ]] -
              .data[[
                absent_mean_col
              ]]
          ) /
            pooled_sd,
          NA_real_
        ),

      abs_standardized_mean_difference =
        abs(
          standardized_mean_difference
        )
    )
}

readr::write_csv(
  candidate_balance_long,
  file.path(
    respondent_diag_summary_dir,
    "bjp_candidate_presence_context_balance.csv"
  )
)

# ============================================================
# 11. BINARY-OUTCOME SPARSITY / LOGIT FEASIBILITY
# ============================================================

primary_logit_sample <- voters |>
  dplyr::filter(
    respondent_sample_bjp_available
  )

binary_cluster_variation_one <- function(
    data,
    grouping,
    label
) {
  cells <- data |>
    dplyr::filter(
      !is.na(voted_bjp)
    ) |>
    dplyr::summarise(
      n =
        dplyr::n(),

      bjp_votes =
        sum(
          voted_bjp == 1
        ),

      non_bjp_votes =
        sum(
          voted_bjp == 0
        ),

      outcome_mean =
        mean(
          voted_bjp
        ),

      .by =
        dplyr::all_of(
          grouping
        )
    ) |>
    dplyr::mutate(
      outcome_pattern =
        dplyr::case_when(
          bjp_votes == 0 ~
            "All 0",
          non_bjp_votes == 0 ~
            "All 1",
          TRUE ~
            "Mixed"
        )
    )

  cells |>
    dplyr::summarise(
      grouping =
        label,

      n_cells =
        dplyr::n(),

      n_all_zero =
        sum(
          outcome_pattern ==
            "All 0"
        ),

      n_all_one =
        sum(
          outcome_pattern ==
            "All 1"
        ),

      n_mixed =
        sum(
          outcome_pattern ==
            "Mixed"
        ),

      share_all_zero =
        mean(
          outcome_pattern ==
            "All 0"
        ),

      share_all_one =
        mean(
          outcome_pattern ==
            "All 1"
        ),

      share_mixed =
        mean(
          outcome_pattern ==
            "Mixed"
        ),

      n_cells_lt5 =
        sum(
          n < 5
        ),

      share_cells_lt5 =
        mean(
          n < 5
        ),

      median_cell_n =
        stats::median(
          n
        ),

      max_cell_n =
        max(
          n
        )
    )
}

binary_cluster_variation_summary <-
  dplyr::bind_rows(
    binary_cluster_variation_one(
      primary_logit_sample,
      c(
        "year",
        "psu_uid"
      ),
      "PSU-year"
    ),

    binary_cluster_variation_one(
      primary_logit_sample,
      c(
        "year",
        "ac_uid"
      ),
      "AC-year"
    ),

    binary_cluster_variation_one(
      primary_logit_sample,
      c(
        "year",
        "pc_cluster_id"
      ),
      "PC-year"
    ),

    binary_cluster_variation_one(
      primary_logit_sample,
      c(
        "year",
        "district_harmonization_group_id"
      ),
      "District-group-year"
    ),

    binary_cluster_variation_one(
      primary_logit_sample |>
        dplyr::filter(
          !is.na(voter_ideology)
        ),
      c(
        "year",
        "voter_ideology"
      ),
      "Year x ideology"
    )
  )

readr::write_csv(
  binary_cluster_variation_summary,
  file.path(
    respondent_diag_summary_dir,
    "binary_outcome_cluster_variation.csv"
  )
)

# Categorical-control levels with no outcome variation can create sparse/separated
# logit cells when entered with rich interactions.
categorical_logit_candidates <- intersect(
  c(
    "voter_ideology",
    "religion_group",
    "caste_group",
    "education_harmonized",
    "income_harmonized"
  ),
  names(primary_logit_sample)
)

categorical_outcome_sparsity <- purrr::map_dfr(
  categorical_logit_candidates,
  function(v) {
    primary_logit_sample |>
      dplyr::filter(
        !is.na(
          .data[[
            v
          ]]
        )
      ) |>
      dplyr::mutate(
        level =
          as.character(
            .data[[
              v
            ]]
          )
      ) |>
      dplyr::summarise(
        n =
          dplyr::n(),

        bjp_votes =
          sum(
            voted_bjp == 1
          ),

        non_bjp_votes =
          sum(
            voted_bjp == 0
          ),

        bjp_share =
          mean(
            voted_bjp
          ),

        .by = c(
          year,
          level
        )
      ) |>
      dplyr::mutate(
        variable =
          v,

        outcome_pattern =
          dplyr::case_when(
            bjp_votes == 0 ~
              "All 0",
            non_bjp_votes == 0 ~
              "All 1",
            TRUE ~
              "Mixed"
          ),

        .before = 1
      )
  }
)

readr::write_csv(
  categorical_outcome_sparsity,
  file.path(
    respondent_diag_summary_dir,
    "categorical_control_outcome_sparsity.csv"
  )
)

categorical_outcome_sparsity_summary <-
  categorical_outcome_sparsity |>
  dplyr::summarise(
    n_levels =
      dplyr::n(),

    n_all_zero =
      sum(
        outcome_pattern ==
          "All 0"
      ),

    n_all_one =
      sum(
        outcome_pattern ==
          "All 1"
      ),

    n_levels_lt10 =
      sum(
        n < 10
      ),

    share_levels_without_outcome_variation =
      mean(
        outcome_pattern !=
          "Mixed"
      ),

    .by = c(
      variable,
      year
    )
  )

readr::write_csv(
  categorical_outcome_sparsity_summary,
  file.path(
    respondent_diag_summary_dir,
    "categorical_control_outcome_sparsity_summary.csv"
  )
)

# ============================================================
# 12. SIMPLE FIGURES FOR RAPID DESIGN REVIEW
# ============================================================

ideology_plot_data <- ideology_levels_audit |>
  dplyr::filter(
    ideology_audit !=
      "Missing"
  ) |>
  tidyr::pivot_longer(
    cols = c(
      share_unweighted,
      share_weighted
    ),
    names_to =
      "weighting",
    values_to =
      "share"
  ) |>
  dplyr::mutate(
    weighting =
      dplyr::recode(
        weighting,
        share_unweighted =
          "Unweighted",
        share_weighted =
          "Survey weighted"
      )
  )

p_ideology <- ggplot2::ggplot(
  ideology_plot_data,
  ggplot2::aes(
    x = ideology_audit,
    y = share
  )
) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(
    year ~ weighting
  ) +
  ggplot2::scale_y_continuous(
    labels =
      scales::label_percent()
  ) +
  ggplot2::labs(
    title =
      "NES ideology composition by election year",
    subtitle =
      "Shares use all respondents as the denominator; the missing-ideology category is omitted from the plot.",
    x = NULL,
    y =
      "Share of respondents"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    respondent_diag_figure_dir,
    "respondent_ideology_composition.pdf"
  ),
  p_ideology,
  width = 10,
  height = 6.5
)

bjp_ideology_plot_data <-
  ideology_bjp_vote |>
  tidyr::pivot_longer(
    cols = c(
      bjp_share_unweighted,
      bjp_share_weighted
    ),
    names_to =
      "weighting",
    values_to =
      "bjp_share"
  ) |>
  dplyr::mutate(
    weighting =
      dplyr::recode(
        weighting,
        bjp_share_unweighted =
          "Unweighted",
        bjp_share_weighted =
          "Survey weighted"
      )
  )

p_bjp_ideology <- ggplot2::ggplot(
  bjp_ideology_plot_data,
  ggplot2::aes(
    x = voter_ideology,
    y = bjp_share,
    group = year
  )
) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      group = year
    )
  ) +
  ggplot2::facet_wrap(
    ~weighting
  ) +
  ggplot2::scale_y_continuous(
    labels =
      scales::label_percent()
  ) +
  ggplot2::labs(
    title =
      "BJP voting among valid voters in BJP-contested constituencies",
    subtitle =
      "This is the candidate-present sample proposed for the primary respondent-level outcome.",
    x = NULL,
    y =
      "Share voting BJP"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    respondent_diag_figure_dir,
    "bjp_vote_by_ideology.pdf"
  ),
  p_bjp_ideology,
  width = 10,
  height = 6
)

p_weights <- voters |>
  dplyr::filter(
    year %in% c(
      2009,
      2014
    ),
    is.finite(
      survey_weight_norm_year
    ),
    survey_weight_norm_year > 0
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        survey_weight_norm_year
    )
  ) +
  ggplot2::geom_histogram(
    bins = 40
  ) +
  ggplot2::facet_wrap(
    ~year,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Normalized NES survey-weight distribution",
    x =
      "Normalized survey weight",
    y =
      "Respondents"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    respondent_diag_figure_dir,
    "survey_weight_distribution.pdf"
  ),
  p_weights,
  width = 9,
  height = 5.5
)

candidate_plot_data <-
  respondent_outcome_summary |>
  dplyr::select(
    year,
    share_bjp_candidate_present,
    share_valid_sample_retained_by_candidate_rule
  ) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to =
      "measure",
    values_to =
      "share"
  ) |>
  dplyr::mutate(
    measure =
      dplyr::recode(
        measure,
        share_bjp_candidate_present =
          "Respondents in BJP-contested ACs",
        share_valid_sample_retained_by_candidate_rule =
          "Valid-voter sample retained"
      )
  )

p_candidate <- ggplot2::ggplot(
  candidate_plot_data,
  ggplot2::aes(
    x = factor(year),
    y = share
  )
) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(
    ~measure
  ) +
  ggplot2::scale_y_continuous(
    labels =
      scales::label_percent(),
    limits = c(
      0,
      1
    )
  ) +
  ggplot2::labs(
    title =
      "Consequences of requiring a BJP candidate to be on the ballot",
    x = NULL,
    y = "Share"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    respondent_diag_figure_dir,
    "bjp_candidate_availability.pdf"
  ),
  p_candidate,
  width = 9,
  height = 5.5
)

# ============================================================
# 13. MASTER REVIEW TABLE
# ============================================================

respondent_design_review_manifest <- tibble::tribble(
  ~diagnostic, ~primary_question, ~output,
  "Outcome/candidate availability",
  "How much of the valid-voter sample remains when BJP availability is required?",
  "respondent_outcome_and_candidate_summary.csv",
  "Ideology coverage",
  "How much Left/Center/Right/Mixed/missing ideology data exist in each year?",
  "respondent_ideology_coverage.csv",
  "BJP by ideology",
  "Does BJP voting vary enough across ideology-year cells for the proposed triple interaction?",
  "bjp_vote_by_year_ideology.csv",
  "Survey weights",
  "How unequal are weights and how much do they reduce effective sample size?",
  "survey_weight_summary.csv",
  "Geographic support",
  "How many PSUs, ACs, PCs, district groups, and states are represented?",
  "geographic_support_by_year.csv",
  "Cluster sizes",
  "Are geographic clustering levels supported by enough clusters and observations?",
  "cluster_size_summary.csv",
  "PC x district crossing",
  "Would PC and district clustering capture distinct cross-classified dependence?",
  "pc_district_crossclassification_summary.csv",
  "Repeated cross-section",
  "Is an AC fixed-effect / within-AC repeated-cross-section design feasible?",
  "ac_repeated_cross_section_overlap.csv",
  "Treatment support",
  "Do representative FDI/demographic exposures vary within state-year and PC-year?",
  "contextual_treatment_within_group_support.csv",
  "Within-AC treatment change",
  "Is there enough 2009-2014 within-AC exposure change for an AC-FE respondent design?",
  "contextual_treatment_within_ac_change_support.csv",
  "Control missingness",
  "How much sample is lost under alternative voter and contextual control blocks?",
  "joint_voter_context_control_sample_sizes.csv",
  "Candidate selection",
  "Are BJP-contested AC-years observably different from non-contested AC-years?",
  "bjp_candidate_presence_context_balance.csv",
  "Logit cluster sparsity",
  "How many geographic cells have all-zero/all-one BJP outcomes?",
  "binary_outcome_cluster_variation.csv",
  "Categorical logit sparsity",
  "Do voter-control or ideology categories have sparse/no-variation outcome cells?",
  "categorical_control_outcome_sparsity_summary.csv"
)

readr::write_csv(
  respondent_design_review_manifest,
  file.path(
    respondent_diag_manifest_dir,
    "respondent_design_review_manifest.csv"
  )
)

# ============================================================
# 14. CONSOLE SUMMARY
# ============================================================

message("")
message(
  "Respondent-design diagnostics complete."
)
message(
  "Revision: ",
  RESPONDENT_DIAGNOSTIC_REVISION
)
message(
  "Summaries: ",
  respondent_diag_summary_dir
)
message(
  "Figures: ",
  respondent_diag_figure_dir
)
message(
  "Manifests: ",
  respondent_diag_manifest_dir
)
message("")
message(
  "Review these files first:"
)
message(
  "  1. respondent_outcome_and_candidate_summary.csv"
)
message(
  "  2. respondent_ideology_coverage.csv"
)
message(
  "  3. survey_weight_summary.csv"
)
message(
  "  4. geographic_support_by_year.csv"
)
message(
  "  5. cluster_size_summary.csv"
)
message(
  "  6. pc_district_crossclassification_summary.csv"
)
message(
  "  7. ac_repeated_cross_section_overlap.csv"
)
message(
  "  8. contextual_treatment_within_group_support.csv"
)
message(
  "  9. contextual_treatment_within_ac_change_support.csv"
)
message(
  " 10. joint_voter_context_control_sample_sizes.csv"
)
message(
  " 11. bjp_candidate_presence_context_balance.csv"
)
message(
  " 12. binary_outcome_cluster_variation.csv"
)
message(
  " 13. categorical_control_outcome_sparsity_summary.csv"
)

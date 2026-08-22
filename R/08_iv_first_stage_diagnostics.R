# ============================================================
# 08_iv_first_stage_diagnostics.R
#
# FIRST-STAGE-ONLY diagnostics for the proposed EC05 x WIR shift-share IV.
#
# IMPORTANT: This script intentionally DOES NOT estimate any BJP/political
# second stage. It evaluates whether the proposed external sector shock
# predicts 2009-2014 manufacturing FDI exposure strongly and broadly enough
# to justify proceeding to IV outcome models.
#
# Current FDI windows in the frozen build:
#   2009 exposure = projects dated 2004-04-01 <= date < 2009-04-01
#   2014 exposure = projects dated 2009-04-01 <= date < 2014-04-01
#
# Primary first-stage endogenous exposure for diagnostics:
#   log(1 + own-AC manufacturing FDI project count in the 2014 window)
#
# Why use count rather than the observational per-100k variable here?
#   The per-100k denominator is based on the project's later population proxy.
#   The causal prototype therefore starts with project counts and controls for
#   pre-treatment local economic size using EC05 employment.
#
# Preferred first-stage controls:
#   - state fixed effects
#   - baseline 2004-2009 own manufacturing FDI count (log1p)
#   - log(1 + EC05 total non-farm employment)
#   - 2005 manufacturing employment share
#   - log(1 + AC land area)
#
# The manufacturing-share control is important because:
#   Z_i = sum_{k in mfg} s_ik g_k
# and sum_k s_ik is the AC's baseline manufacturing share. Including the
# share-sum control isolates differential sector-shock exposure from simply
# being a manufacturing-heavy place.
#
# Two pre-outcome instruments are compared:
#   A. hierarchical mapping (mixed SHRIC groups use parent Manufacturing)
#   B. detailed-only mapping (drops parent-mapped manufacturing SHRICs)
#
# Outputs include:
#   - cluster-robust first stages
#   - partial R2
#   - baseline-period placebo first stages
#   - EC05-quality restrictions
#   - leave-one-WIR-industry-out diagnostics
#   - instrument concentration diagnostics
#
# No outcome coefficient is inspected anywhere in this file.
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

FIRST_STAGE_REVISION <-
  "2026-08-09-v1.0-first-stage-only"

message(
  "Starting IV first-stage diagnostics: ",
  FIRST_STAGE_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

iv_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep"
)

wir_root <- file.path(
  iv_root,
  "wir_shocks"
)

iv_data_path <- file.path(
  wir_root,
  "data",
  "ac_iv_base_with_wir_world_shocks.rds"
)

crosswalk_path <- file.path(
  wir_root,
  "tables",
  "shric_to_wir_industry_crosswalk_FROZEN_PREOUTCOME.csv"
)

shock_path <- file.path(
  wir_root,
  "tables",
  "wir_world_sector_shocks.csv"
)

purrr::walk(
  c(
    iv_data_path,
    crosswalk_path,
    shock_path
  ),
  function(p) {
    if (
      !file.exists(
        p
      )
    ) {
      stop(
        "Missing required IV-prep input: ",
        p,
        ". Run corrected 06 and 07 first."
      )
    }
  }
)

out_root <- file.path(
  iv_root,
  "first_stage_diagnostics"
)

out_table_dir <- file.path(
  out_root,
  "tables"
)

out_figure_dir <- file.path(
  out_root,
  "figures"
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
    out_manifest_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD + HARD GUARDS
# ============================================================

d <- readRDS(
  iv_data_path
)

crosswalk <- readr::read_csv(
  crosswalk_path,
  show_col_types = FALSE,
  progress = FALSE
)

shock_table <- readr::read_csv(
  shock_path,
  show_col_types = FALSE,
  progress = FALSE
)

required <- c(
  "ac_uid",
  "state_no",
  "pc_cluster_id",
  "con08_land_area",
  "ec05_emp_all",
  "ec05_emp_manuf",
  "ec05_quality_high",
  "ec05_fragmentation_low",
  "bartik_wir_world_mfg_logchg",
  "bartik_wir_world_mfg_detailed_only_logchg",
  "fdi_mfg_own_all_n_2009",
  "fdi_mfg_own_all_n_2014"
)

missing_required <- setdiff(
  required,
  names(
    d
  )
)

if (
  length(
    missing_required
  ) > 0
) {
  stop(
    "First-stage data are missing: ",
    paste(
      missing_required,
      collapse = ", "
    )
  )
}

share_vars <- paste0(
  "ec05_share_shric_",
  1:90
)

missing_share_vars <- setdiff(
  share_vars,
  names(
    d
  )
)

if (
  length(
    missing_share_vars
  ) > 0
) {
  stop(
    "First-stage data are missing EC05 SHRIC shares."
  )
}

# Corrected 06 must leave no missing sector shares where EC05 total employment
# is observed.
share_complete <- d |>
  dplyr::filter(
    !is.na(
      ec05_emp_all
    )
  ) |>
  dplyr::summarise(
    n =
      dplyr::n(),

    n_any_missing_share =
      sum(
        !dplyr::if_all(
          dplyr::all_of(
            share_vars
          ),
          ~!is.na(
            .x
          )
        )
      )
  )

if (
  share_complete$n_any_missing_share >
    0
) {
  stop(
    "EC05 rows still have missing SHRIC shares. ",
    "Use corrected 06_iv_design_prep_ec05_v1_0_1.R and ",
    "07_prepare_wir_sector_shocks_v1_0_2.R before first-stage diagnostics."
  )
}

# The first-stage script must not use political outcomes.
forbidden_outcome_patterns <- c(
  "bjp_vote_share",
  "d_bjp_vote_share",
  "fr_party_vote_share",
  "voted_bjp",
  "bjp_win"
)

if (
  any(
    vapply(
      forbidden_outcome_patterns,
      function(x) {
        any(
          stringr::str_detect(
            names(
              d
            ),
            fixed(
              x
            )
          )
        )
      },
      logical(1)
    )
  )
) {
  # Political columns may exist in the input file because it is the eventual
  # analysis base. They are deliberately ignored below. This message records
  # the guard rather than stopping.
  message(
    "Political outcome columns are present in the base file but are not ",
    "referenced by any first-stage formula."
  )
}

# ============================================================
# 2. DERIVE FIRST-STAGE VARIABLES
# ============================================================

d <- d |>
  dplyr::mutate(
    fdi_mfg_own_log_count_2014 =
      log1p(
        fdi_mfg_own_all_n_2014
      ),

    fdi_mfg_own_log_count_2009 =
      log1p(
        fdi_mfg_own_all_n_2009
      ),

    fdi_mfg_own_any_2014 =
      as.integer(
        fdi_mfg_own_all_n_2014 >
          0
      ),

    log1p_ec05_emp_all =
      log1p(
        ec05_emp_all
      ),

    ec05_mfg_share =
      dplyr::if_else(
        is.finite(
          ec05_emp_all
        ) &
        ec05_emp_all >
          0,
        ec05_emp_manuf /
          ec05_emp_all,
        NA_real_
      ),

    log1p_land_area =
      log1p(
        con08_land_area
      ),

    ec05_high_quality =
      dplyr::coalesce(
        ec05_quality_high,
        FALSE
      ) &
      dplyr::coalesce(
        ec05_fragmentation_low,
        FALSE
      )
  )

# ============================================================
# 3. FIRST-STAGE ESTIMATION HELPERS
# ============================================================

first_stage_spec_meta <- tibble::tribble(
  ~spec_id, ~label, ~rhs_controls, ~quality_only,

  "FS0_state_fe",
  "State FE only",
  "",
  FALSE,

  "FS1_baseline_fdi",
  "+ baseline 2004-09 manufacturing FDI",
  "fdi_mfg_own_log_count_2009",
  FALSE,

  "FS2_pre_size",
  "+ baseline FDI + 2005 economic size + land area",
  paste(
    "fdi_mfg_own_log_count_2009",
    "log1p_ec05_emp_all",
    "log1p_land_area",
    sep = " + "
  ),
  FALSE,

  "FS3_preferred",
  "+ baseline FDI + 2005 size + manufacturing share + land area",
  paste(
    "fdi_mfg_own_log_count_2009",
    "log1p_ec05_emp_all",
    "ec05_mfg_share",
    "log1p_land_area",
    sep = " + "
  ),
  FALSE,

  "FS4_preferred_high_quality",
  "Preferred controls; high-quality EC05 only",
  paste(
    "fdi_mfg_own_log_count_2009",
    "log1p_ec05_emp_all",
    "ec05_mfg_share",
    "log1p_land_area",
    sep = " + "
  ),
  TRUE
)

instrument_meta <- tibble::tribble(
  ~instrument_id, ~instrument_var, ~instrument_label,

  "hierarchical",
  "bartik_wir_world_mfg_logchg",
  "WIR hierarchical manufacturing Bartik",

  "detailed_only",
  "bartik_wir_world_mfg_detailed_only_logchg",
  "WIR detailed-only manufacturing Bartik"
)

treatment_meta <- tibble::tribble(
  ~treatment_id, ~treatment_var, ~treatment_label, ~primary_treatment,

  "log_count",
  "fdi_mfg_own_log_count_2014",
  "log(1 + own manufacturing projects), 2009-14",
  TRUE,

  "count",
  "fdi_mfg_own_all_n_2014",
  "Own manufacturing project count, 2009-14",
  FALSE,

  "any",
  "fdi_mfg_own_any_2014",
  "Any own manufacturing project, 2009-14",
  FALSE
)

make_formula <- function(
    lhs,
    instrument,
    controls
) {
  rhs <- instrument

  if (
    nzchar(
      controls
    )
  ) {
    rhs <- paste(
      rhs,
      controls,
      sep = " + "
    )
  }

  stats::as.formula(
    paste0(
      lhs,
      " ~ ",
      rhs,
      " | state_no"
    )
  )
}

partial_r2_for_instrument <- function(
    data,
    lhs,
    instrument,
    controls
) {
  reduced_rhs <- if (
    nzchar(
      controls
    )
  ) {
    controls
  } else {
    "1"
  }

  y_fit <- fixest::feols(
    stats::as.formula(
      paste0(
        lhs,
        " ~ ",
        reduced_rhs,
        " | state_no"
      )
    ),
    data =
      data,
    notes = FALSE,
    warn = FALSE
  )

  z_fit <- fixest::feols(
    stats::as.formula(
      paste0(
        instrument,
        " ~ ",
        reduced_rhs,
        " | state_no"
      )
    ),
    data =
      data,
    notes = FALSE,
    warn = FALSE
  )

  ry <- stats::residuals(
    y_fit
  )

  rz <- stats::residuals(
    z_fit
  )

  if (
    stats::sd(
      ry
    ) ==
      0 ||
    stats::sd(
      rz
    ) ==
      0
  ) {
    return(
      NA_real_
    )
  }

  stats::cor(
    ry,
    rz
  )^2
}

run_first_stage <- function(
    treatment_var,
    treatment_label,
    primary_treatment,
    instrument_var,
    instrument_id,
    instrument_label,
    spec_id,
    spec_label,
    controls,
    quality_only
) {
  vars <- unique(
    c(
      treatment_var,
      instrument_var,
      "state_no",
      "pc_cluster_id",
      all.vars(
        stats::as.formula(
          paste0(
            "~",
            if (
              nzchar(
                controls
              )
            ) {
              controls
            } else {
              "1"
            }
          )
        )
      )
    )
  )

  dat <- d |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    quality_only
  ) {
    dat <- dat |>
      dplyr::filter(
        ec05_high_quality
      )
  }

  f <- make_formula(
    treatment_var,
    instrument_var,
    controls
  )

  fit <- fixest::feols(
    f,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      instrument_var
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument_var
    ]

  t_stat <-
    beta /
    se

  partial_r2 <- partial_r2_for_instrument(
    dat,
    treatment_var,
    instrument_var,
    controls
  )

  tibble::tibble(
    treatment_var =
      treatment_var,
    treatment_label =
      treatment_label,
    primary_treatment =
      primary_treatment,
    instrument_id =
      instrument_id,
    instrument_var =
      instrument_var,
    instrument_label =
      instrument_label,
    spec_id =
      spec_id,
    spec_label =
      spec_label,
    quality_only =
      quality_only,
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    t_stat =
      unname(
        t_stat
      ),
    cluster_f_single_instrument =
      unname(
        t_stat^2
      ),
    partial_r2 =
      partial_r2,
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      ),
    n_states =
      dplyr::n_distinct(
        dat$state_no
      ),
    first_stage_sign =
      dplyr::case_when(
        beta >
          0 ~
          "positive",
        beta <
          0 ~
          "negative",
        TRUE ~
          "zero"
      ),
    f_ge_10 =
      t_stat^2 >=
        10
  )
}

first_stage_results <- purrr::pmap_dfr(
  tidyr::crossing(
    treatment_meta,
    instrument_meta,
    first_stage_spec_meta
  ),
  function(
      treatment_id,
      treatment_var,
      treatment_label,
      primary_treatment,
      instrument_id,
      instrument_var,
      instrument_label,
      spec_id,
      label,
      rhs_controls,
      quality_only
  ) {
    run_first_stage(
      treatment_var =
        treatment_var,
      treatment_label =
        treatment_label,
      primary_treatment =
        primary_treatment,
      instrument_var =
        instrument_var,
      instrument_id =
        instrument_id,
      instrument_label =
        instrument_label,
      spec_id =
        spec_id,
      spec_label =
        label,
      controls =
        rhs_controls,
      quality_only =
        quality_only
    )
  }
)

readr::write_csv(
  first_stage_results,
  file.path(
    out_table_dir,
    "01_first_stage_results.csv"
  )
)

# ============================================================
# 4. BASELINE-PERIOD PLACEBO
# ============================================================
#
# Future 2010-13 world sector growth should not simply be a strong predictor
# of PRE-2009 local manufacturing FDI once predetermined size/composition are
# accounted for. This is a diagnostic, not a formal validity test.

placebo_controls <- paste(
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

run_placebo <- function(
    instrument_var,
    instrument_id,
    instrument_label,
    quality_only
) {
  vars <- c(
    "fdi_mfg_own_log_count_2009",
    instrument_var,
    "log1p_ec05_emp_all",
    "ec05_mfg_share",
    "log1p_land_area",
    "state_no",
    "pc_cluster_id"
  )

  dat <- d |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    quality_only
  ) {
    dat <- dat |>
      dplyr::filter(
        ec05_high_quality
      )
  }

  f <- make_formula(
    "fdi_mfg_own_log_count_2009",
    instrument_var,
    placebo_controls
  )

  fit <- fixest::feols(
    f,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      instrument_var
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument_var
    ]

  tibble::tibble(
    instrument_id,
    instrument_label,
    quality_only,
    placebo_outcome =
      "log(1 + own manufacturing projects), 2004-09",
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    cluster_f_single_instrument =
      unname(
        (
          beta /
            se
        )^2
      ),
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      )
  )
}

placebo_results <- dplyr::bind_rows(
  purrr::pmap_dfr(
    instrument_meta,
    function(
        instrument_id,
        instrument_var,
        instrument_label
    ) {
      run_placebo(
        instrument_var,
        instrument_id,
        instrument_label,
        FALSE
      )
    }
  ),

  purrr::pmap_dfr(
    instrument_meta,
    function(
        instrument_id,
        instrument_var,
        instrument_label
    ) {
      run_placebo(
        instrument_var,
        instrument_id,
        instrument_label,
        TRUE
      )
    }
  )
)

readr::write_csv(
  placebo_results,
  file.path(
    out_table_dir,
    "02_preperiod_fdi_placebo_results.csv"
  )
)

# ============================================================
# 5. WIR-INDUSTRY CONTRIBUTIONS + CONCENTRATION
# ============================================================

primary_shocks <- shock_table |>
  dplyr::filter(
    stringr::str_starts(
      shock_definition,
      "PRIMARY:"
    )
  ) |>
  dplyr::select(
    wir_industry,
    shock_log_change
  )

mfg_crosswalk <- crosswalk |>
  dplyr::filter(
    manufacturing_shric
  ) |>
  dplyr::left_join(
    primary_shocks,
    by =
      "wir_industry",
    relationship =
      "many-to-one"
  )

if (
  any(
    is.na(
      mfg_crosswalk$shock_log_change
    )
  )
) {
  stop(
    "At least one manufacturing SHRIC lacks a primary WIR shock."
  )
}

# Construct contribution columns at the WIR-industry level.
wir_industries <- sort(
  unique(
    mfg_crosswalk$wir_industry
  )
)

contribution_data <- d |>
  dplyr::select(
    ac_uid,
    state_no,
    pc_cluster_id,
    ec05_emp_all,
    ec05_high_quality,
    fdi_mfg_own_log_count_2009,
    fdi_mfg_own_log_count_2014,
    log1p_ec05_emp_all,
    ec05_mfg_share,
    log1p_land_area,
    dplyr::all_of(
      share_vars
    )
  )

for (
  industry_i in wir_industries
) {
  rows_i <- mfg_crosswalk |>
    dplyr::filter(
      wir_industry ==
        industry_i
    )

  shric_i <-
    as.integer(
      rows_i$shric
    )

  shock_i <-
    unique(
      rows_i$shock_log_change
    )

  if (
    length(
      shock_i
    ) != 1
  ) {
    stop(
      "WIR industry does not have a unique primary shock: ",
      industry_i
    )
  }

  share_i <- rowSums(
    contribution_data[
      paste0(
        "ec05_share_shric_",
        shric_i
      )
    ],
    na.rm = FALSE
  )

  safe_name <- paste0(
    "z_component__",
    janitor::make_clean_names(
      industry_i
    )
  )

  contribution_data[[
    safe_name
  ]] <-
    share_i *
    shock_i
}

component_vars <- names(
  contribution_data
)[
  stringr::str_starts(
    names(
      contribution_data
    ),
    "z_component__"
  )
]

# Detailed-only contribution set excludes parent Manufacturing.
detailed_wir_industries <- mfg_crosswalk |>
  dplyr::filter(
    mapping_specificity ==
      "detailed"
  ) |>
  dplyr::pull(
    wir_industry
  ) |>
  unique()

detailed_component_vars <- paste0(
  "z_component__",
  janitor::make_clean_names(
    detailed_wir_industries
  )
)

contribution_data <- contribution_data |>
  dplyr::mutate(
    z_hierarchical_rebuilt =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            component_vars
          )
        )
      ),

    z_detailed_rebuilt =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            detailed_component_vars
          )
        )
      )
  )

rebuild_audit <- contribution_data |>
  dplyr::left_join(
    d |>
      dplyr::select(
        ac_uid,
        bartik_wir_world_mfg_logchg,
        bartik_wir_world_mfg_detailed_only_logchg
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  dplyr::summarise(
    max_abs_hierarchical_difference =
      max(
        abs(
          z_hierarchical_rebuilt -
          bartik_wir_world_mfg_logchg
        ),
        na.rm = TRUE
      ),

    max_abs_detailed_difference =
      max(
        abs(
          z_detailed_rebuilt -
          bartik_wir_world_mfg_detailed_only_logchg
        ),
        na.rm = TRUE
      )
  )

readr::write_csv(
  rebuild_audit,
  file.path(
    out_manifest_dir,
    "01_instrument_rebuild_audit.csv"
  )
)

if (
  rebuild_audit$max_abs_hierarchical_difference >
    1e-10 ||
  rebuild_audit$max_abs_detailed_difference >
    1e-10
) {
  stop(
    "Rebuilt WIR-industry contributions do not reproduce saved Bartik instruments."
  )
}

# Preferred complete sample for concentration diagnostics.
pref_vars <- c(
  "fdi_mfg_own_log_count_2014",
  "fdi_mfg_own_log_count_2009",
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  "state_no",
  "pc_cluster_id",
  "z_detailed_rebuilt"
)

pref <- contribution_data |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(
        pref_vars
      ),
      ~!is.na(
        .x
      )
    )
  )

z_pref <- pref$z_detailed_rebuilt
var_z <- stats::var(
  z_pref
)

concentration_rows <- purrr::map_dfr(
  detailed_wir_industries,
  function(industry_i) {
    component_var <- paste0(
      "z_component__",
      janitor::make_clean_names(
        industry_i
      )
    )

    c_i <- pref[[
      component_var
    ]]

    shric_i <- mfg_crosswalk |>
      dplyr::filter(
        wir_industry ==
          industry_i,
        mapping_specificity ==
          "detailed"
      ) |>
      dplyr::pull(
        shric
      )

    exposure_share_i <- rowSums(
      pref[
        paste0(
          "ec05_share_shric_",
          shric_i
        )
      ]
    )

    shock_i <- mfg_crosswalk |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shock_log_change
      ) |>
      unique()

    tibble::tibble(
      wir_industry =
        industry_i,
      shock_log_change =
        shock_i,
      mean_2005_nonfarm_employment_share =
        mean(
          exposure_share_i
        ),
      sd_component =
        stats::sd(
          c_i
        ),
      covariance_contribution_to_instrument_variance =
        stats::cov(
          c_i,
          z_pref
        ) /
        var_z
    )
  }
) |>
  dplyr::arrange(
    dplyr::desc(
      abs(
        covariance_contribution_to_instrument_variance
      )
    )
  )

exposure_weights <- concentration_rows |>
  dplyr::mutate(
    exposure_weight =
      mean_2005_nonfarm_employment_share /
      sum(
        mean_2005_nonfarm_employment_share
      )
  )

effective_industry_count <-
  1 /
  sum(
    exposure_weights$exposure_weight^2
  )

readr::write_csv(
  exposure_weights,
  file.path(
    out_table_dir,
    "03_wir_industry_instrument_concentration.csv"
  )
)

readr::write_csv(
  tibble::tibble(
    n_detailed_wir_industries =
      nrow(
        exposure_weights
      ),
    effective_industry_count_by_mean_exposure_weights =
      effective_industry_count,
    largest_exposure_weight =
      max(
        exposure_weights$exposure_weight
      ),
    top3_exposure_weight =
      sum(
        sort(
          exposure_weights$exposure_weight,
          decreasing = TRUE
        )[
          seq_len(
            min(
              3,
              nrow(
                exposure_weights
              )
            )
          )
        ]
      )
  ),
  file.path(
    out_manifest_dir,
    "02_instrument_concentration_summary.csv"
  )
)

# ============================================================
# 6. LEAVE-ONE-WIR-INDUSTRY-OUT FIRST STAGE
# ============================================================

preferred_controls <- paste(
  "fdi_mfg_own_log_count_2009",
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

run_loo <- function(
    excluded_industry,
    quality_only
) {
  dat <- contribution_data

  if (
    excluded_industry ==
      "NONE"
  ) {
    dat$z_loo <-
      dat$z_detailed_rebuilt
  } else {
    component_var <- paste0(
      "z_component__",
      janitor::make_clean_names(
        excluded_industry
      )
    )

    dat$z_loo <-
      dat$z_detailed_rebuilt -
      dat[[
        component_var
      ]]
  }

  vars <- c(
    "fdi_mfg_own_log_count_2014",
    "z_loo",
    "fdi_mfg_own_log_count_2009",
    "log1p_ec05_emp_all",
    "ec05_mfg_share",
    "log1p_land_area",
    "state_no",
    "pc_cluster_id"
  )

  dat <- dat |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    quality_only
  ) {
    dat <- dat |>
      dplyr::filter(
        ec05_high_quality
      )
  }

  fit <- fixest::feols(
    fdi_mfg_own_log_count_2014 ~
      z_loo +
      fdi_mfg_own_log_count_2009 +
      log1p_ec05_emp_all +
      ec05_mfg_share +
      log1p_land_area |
      state_no,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      "z_loo"
    ]

  se <-
    fixest::se(
      fit
    )[
      "z_loo"
    ]

  tibble::tibble(
    excluded_wir_industry =
      excluded_industry,
    quality_only =
      quality_only,
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    cluster_f_single_instrument =
      unname(
        (
          beta /
            se
        )^2
      ),
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      )
  )
}

loo_results <- dplyr::bind_rows(
  purrr::map_dfr(
    c(
      "NONE",
      detailed_wir_industries
    ),
    ~run_loo(
      .x,
      FALSE
    )
  ),

  purrr::map_dfr(
    c(
      "NONE",
      detailed_wir_industries
    ),
    ~run_loo(
      .x,
      TRUE
    )
  )
) |>
  dplyr::arrange(
    quality_only,
    cluster_f_single_instrument
  )

readr::write_csv(
  loo_results,
  file.path(
    out_table_dir,
    "04_leave_one_wir_industry_out_first_stage.csv"
  )
)

# ============================================================
# 7. SUMMARY / GO-NO-GO TABLE
# ============================================================

headline <- first_stage_results |>
  dplyr::filter(
    primary_treatment,
    spec_id %in%
      c(
        "FS3_preferred",
        "FS4_preferred_high_quality"
      )
  ) |>
  dplyr::select(
    treatment_label,
    instrument_id,
    instrument_label,
    spec_id,
    spec_label,
    coefficient,
    cluster_se,
    cluster_f_single_instrument,
    partial_r2,
    n_ac,
    n_pc_clusters,
    first_stage_sign
  )

readr::write_csv(
  headline,
  file.path(
    out_table_dir,
    "00_HEADLINE_first_stage.csv"
  )
)

# A descriptive assessment only. The conventional F=10 heuristic is not a
# substitute for weak-IV-robust inference, especially in shift-share designs.
assessment <- headline |>
  dplyr::mutate(
    conventional_strength_flag =
      dplyr::case_when(
        cluster_f_single_instrument >=
          10 ~
          "passes conventional F>=10 heuristic",
        cluster_f_single_instrument >=
          5 ~
          "borderline/weak by conventional heuristic",
        TRUE ~
          "weak by conventional heuristic"
      ),

    sign_note =
      dplyr::if_else(
        coefficient >
          0,
        "positive first stage",
        "negative first stage: requires economic interpretation before IV"
      )
  )

readr::write_csv(
  assessment,
  file.path(
    out_table_dir,
    "05_first_stage_assessment.csv"
  )
)

# ============================================================
# 8. FIGURES
# ============================================================

plot_headline <- headline |>
  dplyr::mutate(
    sample_label =
      dplyr::if_else(
        spec_id ==
          "FS4_preferred_high_quality",
        "High-quality EC05",
        "All complete EC05"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        coefficient,
      y =
        instrument_label
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        coefficient -
        1.96 *
        cluster_se,
      xmax =
        coefficient +
        1.96 *
        cluster_se
    ),
    orientation = "y"
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_wrap(
    ~sample_label
  ) +
  ggplot2::labs(
    title =
      "First stage: EC05 × WIR Bartik predicting 2009–14 manufacturing FDI",

    subtitle =
      "Outcome is log(1 + own-AC manufacturing project count); PC-clustered intervals.",

    x =
      "First-stage coefficient",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "01_headline_first_stage.pdf"
  ),
  plot_headline,
  width = 11,
  height = 6.5
)

plot_loo <- loo_results |>
  dplyr::filter(
    excluded_wir_industry !=
      "NONE",
    !quality_only
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        cluster_f_single_instrument,
      y =
        stats::reorder(
          excluded_wir_industry,
          cluster_f_single_instrument
        )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 10,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title =
      "Leave-one-WIR-industry-out first-stage strength",

    subtitle =
      "Detailed-only instrument; preferred pre-treatment controls.",

    x =
      "PC-clustered single-instrument F statistic",

    y =
      "Excluded WIR industry"
  ) +
  ggplot2::theme_minimal(
    base_size = 9.5
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "02_leave_one_industry_out_first_stage.pdf"
  ),
  plot_loo,
  width = 10,
  height = 7
)

# ============================================================
# 9. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "IV first-stage diagnostics revision: ",
      FIRST_STAGE_REVISION
    ),
    "",
    "THIS SCRIPT DOES NOT USE BJP/POLITICAL OUTCOMES.",
    "",
    "PRIMARY FIRST-STAGE EXPOSURE",
    "----------------------------",
    "log(1 + own-AC manufacturing FDI projects in the 2014 exposure window).",
    "The frozen FDI build defines 2014 as 2009-04-01 <= project date < 2014-04-01.",
    "",
    "PREFERRED CONTROLS",
    "------------------",
    "State FE; baseline 2004-09 manufacturing FDI; log EC05 total employment;",
    "2005 manufacturing employment share; log AC land area.",
    "Later 2011 population/SC/ST controls are intentionally not part of the causal first-stage primary specification.",
    "",
    "INTERPRETATION",
    "--------------",
    "A negative first stage is mechanically usable in IV but changes the economic story:",
    "the global sector shock would predict diversion/relative decline rather than attraction.",
    "Do not proceed to second-stage causal claims merely because an F statistic is large.",
    "",
    "The conventional F>=10 flag is only a screening heuristic.",
    "Shift-share and weak-IV-robust inference require additional diagnostics before second stages.",
    "",
    "READ FIRST",
    "----------",
    "tables/00_HEADLINE_first_stage.csv",
    "tables/01_first_stage_results.csv",
    "tables/02_preperiod_fdi_placebo_results.csv",
    "tables/03_wir_industry_instrument_concentration.csv",
    "tables/04_leave_one_wir_industry_out_first_stage.csv",
    "tables/05_first_stage_assessment.csv",
    "manifests/02_instrument_concentration_summary.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "First-stage-only diagnostics COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No BJP/political second stage was estimated."
)

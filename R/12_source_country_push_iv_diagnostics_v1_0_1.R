# ============================================================
# 12_source_country_push_iv_diagnostics.R
#
# PRE-OUTCOME first-stage diagnostics for a new source-country-push
# shift-share design.
#
# IMPORTANT CONCEPTUAL DISTINCTION
# -------------------------------
# WIR Annex 16 reports greenfield FDI project counts by SOURCE COUNTRY and
# year, but NOT by source-country x industry. Therefore this is NOT a true
# joint source-country-by-industry world shock.
#
# Instead, we construct:
#
#   1. source-country outward-FDI shocks:
#        g_c = log(1 + ROW source-c projects, 2010-13 mean)
#            - log(1 + ROW source-c projects, 2005-08 mean)
#
#      where ROW is approximated by WIR world-source counts minus the
#      India-bound project counts in the uploaded Source_Markets export.
#
#   2. predetermined India source-country dependence within industry:
#        omega_ck = pre-2009 India-bound projects from c in industry k
#                   / all pre-2009 India-bound projects in industry k
#
#   3. an industry-level source-push shock:
#        G_k = sum_c omega_ck * g_c
#
#   4. an AC-level shift-share instrument:
#        Z_i = sum_k q_ik,2005 * G_k
#
#      where q_ik is the AC's 2005 EC05 employment share RE-NORMALIZED
#      within the confidently mapped manufacturing industries covered by
#      the source-push design.
#
# This asks:
#   Did ACs specialized in Indian manufacturing industries historically
#   dependent on source countries that subsequently experienced larger
#   outward greenfield-FDI booms receive more manufacturing FDI in 2009-14?
#
# No BJP or political outcome is estimated anywhere in this script.
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

SOURCE_PUSH_REVISION <-
  "2026-08-09-v1.0.1-source-push-mapping-gate-hotfix"

message(
  "Starting source-country-push IV diagnostics: ",
  SOURCE_PUSH_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

iv_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep"
)

ec05_path <- file.path(
  iv_root,
  "data",
  "ac_iv_base_ec05_shares.rds"
)

shric_wir_path <- file.path(
  iv_root,
  "wir_shocks",
  "tables",
  "shric_to_wir_industry_crosswalk_FROZEN_PREOUTCOME.csv"
)

find_input <- function(
    filename
) {
  candidates <- c(
    file.path(
      project_root,
      "data",
      "iv_source_country",
      filename
    ),
    file.path(
      project_root,
      "data",
      "raw",
      "iv_source_country",
      filename
    ),
    file.path(
      project_root,
      "data",
      filename
    ),
    file.path(
      project_root,
      filename
    )
  )

  hit <- candidates[
    file.exists(
      candidates
    )
  ][1]

  if (
    length(
      hit
    ) == 0 ||
    is.na(
      hit
    )
  ) {
    stop(
      "Could not locate ",
      filename,
      ". Put the supplied source-country IV CSV files under data/iv_source_country/."
    )
  }

  hit
}

industry_shock_path <- find_input(
  "source_country_push_wir_industry_shocks.csv"
)

component_path <- find_input(
  "source_country_push_wir_industry_source_components.csv"
)

sector_crosswalk_path <- find_input(
  "fdi_markets_sector_to_wir_industry_source_push_crosswalk.csv"
)

purrr::walk(
  c(
    ec05_path,
    shric_wir_path,
    industry_shock_path,
    component_path,
    sector_crosswalk_path
  ),
  function(p) {
    if (
      !file.exists(
        p
      )
    ) {
      stop(
        "Missing required input: ",
        p
      )
    }
  }
)

out_root <- file.path(
  iv_root,
  "source_country_push_diagnostics"
)

out_table_dir <- file.path(
  out_root,
  "tables"
)

out_manifest_dir <- file.path(
  out_root,
  "manifests"
)

out_figure_dir <- file.path(
  out_root,
  "figures"
)

out_data_dir <- file.path(
  out_root,
  "data"
)

purrr::walk(
  c(
    out_root,
    out_table_dir,
    out_manifest_dir,
    out_figure_dir,
    out_data_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD + FREEZE PRIMARY SOURCE-PUSH INDUSTRY UNIVERSE
# ============================================================

d <- readRDS(
  ec05_path
)

shric_wir <- readr::read_csv(
  shric_wir_path,
  show_col_types = FALSE,
  progress = FALSE
)

industry_shocks <- readr::read_csv(
  industry_shock_path,
  show_col_types = FALSE,
  progress = FALSE
)

components <- readr::read_csv(
  component_path,
  show_col_types = FALSE,
  progress = FALSE
)

sector_crosswalk <- readr::read_csv(
  sector_crosswalk_path,
  show_col_types = FALSE,
  progress = FALSE
)

# Primary source-push industries must:
#   (a) come from the frozen detailed/high-confidence SHRIC -> WIR mapping,
#   (b) be manufacturing industries,
#   (c) have >=20 India-bound preperiod projects in the source-market data.
#
# The >=20 threshold is fixed on pre-treatment measurement precision and does
# not depend on first-stage or political outcomes.

candidate_industries <- industry_shocks |>
  dplyr::filter(
    pre2009_india_project_count >=
      20,
    source_country_shock_coverage_share >=
      0.99
  ) |>
  dplyr::pull(
    wir_industry
  ) |>
  unique() |>
  sort()

# Only retain source-push industries that ALSO have a detailed, high-confidence
# mapping in the frozen EC05 SHRIC -> WIR crosswalk. This is a pre-outcome
# measurement gate. In particular, the frozen crosswalk does not provide
# detailed/high-confidence SHRIC mappings for some otherwise well-measured
# fDi Markets industries (e.g. Automotive, Machinery and equipment, and
# Rubber and plastics products). Those industries must be dropped rather than
# accidentally represented by an empty SHRIC set or promoted to a broader
# parent-sector mapping after seeing first-stage results.
primary_shric <- shric_wir |>
  dplyr::filter(
    manufacturing_shric,
    mapping_specificity ==
      "detailed",
    mapping_confidence ==
      "high",
    wir_industry %in%
      candidate_industries
  ) |>
  dplyr::select(
    shric,
    shric_desc,
    wir_industry
  )

primary_industries <- sort(
  intersect(
    candidate_industries,
    unique(
      primary_shric$wir_industry
    )
  )
)

industry_mapping_gate <- tibble::tibble(
  wir_industry =
    candidate_industries
) |>
  dplyr::mutate(
    retained_primary =
      wir_industry %in%
        primary_industries,

    mapping_status =
      dplyr::if_else(
        retained_primary,
        "retained: detailed high-confidence EC05 mapping exists",
        "dropped: no detailed high-confidence EC05 mapping"
      )
  ) |>
  dplyr::left_join(
    industry_shocks |>
      dplyr::select(
        wir_industry,
        pre2009_india_project_count,
        source_country_shock_coverage_share
      ),
    by =
      "wir_industry",
    relationship =
      "one-to-one"
  )

readr::write_csv(
  industry_mapping_gate,
  file.path(
    out_manifest_dir,
    "00_source_push_industry_mapping_gate.csv"
  )
)

if (
  length(
    primary_industries
  ) ==
    0 ||
  nrow(
    primary_shric
  ) ==
    0
) {
  stop(
    "No source-push industries survive the detailed/high-confidence ",
    "EC05 mapping gate."
  )
}

message(
  "Candidate source-push industries before EC05 mapping gate: ",
  length(
    candidate_industries
  )
)

message(
  "Primary source-push industries after EC05 mapping gate: ",
  length(
    primary_industries
  ),
  " [",
  paste(
    primary_industries,
    collapse = "; "
  ),
  "]"
)

dropped_mapping_industries <- setdiff(
  candidate_industries,
  primary_industries
)

if (
  length(
    dropped_mapping_industries
  ) >
    0
) {
  message(
    "Dropped for lack of detailed/high-confidence EC05 mapping: ",
    paste(
      dropped_mapping_industries,
      collapse = "; "
    )
  )
}

primary_universe <- primary_shric |>
  dplyr::count(
    wir_industry,
    name =
      "n_shric"
  ) |>
  dplyr::left_join(
    industry_shocks,
    by =
      "wir_industry",
    relationship =
      "one-to-one"
  ) |>
  dplyr::arrange(
    wir_industry
  )

readr::write_csv(
  primary_universe,
  file.path(
    out_manifest_dir,
    "01_primary_source_push_industry_universe.csv"
  )
)

# ============================================================
# 2. BUILD AC-LEVEL WITHIN-COVERED-MANUFACTURING INSTRUMENTS
# ============================================================

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
  ) >
    0
) {
  stop(
    "EC05 base lacks corrected 90-industry SHRIC shares."
  )
}

mfg_shric_all <- shric_wir |>
  dplyr::filter(
    manufacturing_shric
  ) |>
  dplyr::pull(
    shric
  )

covered_shrics <- primary_shric$shric

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

    log1p_ec05_emp_all =
      log1p(
        ec05_emp_all
      ),

    ec05_mfg_share =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            paste0(
              "ec05_share_shric_",
              mfg_shric_all
            )
          )
        ),
        na.rm = FALSE
      ),

    source_push_covered_mfg_share =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            paste0(
              "ec05_share_shric_",
              covered_shrics
            )
          )
        ),
        na.rm = FALSE
      ),

    log1p_land_area =
      log1p(
        con08_land_area
      ),

    ec05_high_quality_combined =
      dplyr::coalesce(
        ec05_quality_high,
        FALSE
      ) &
      dplyr::coalesce(
        ec05_fragmentation_low,
        FALSE
      )
  )

industry_shock_lookup <- industry_shocks |>
  dplyr::filter(
    wir_industry %in%
      primary_industries
  ) |>
  dplyr::select(
    wir_industry,
    shock_row =
      source_push_approx_rest_of_world_log_shock,
    shock_world =
      source_push_world_inclusive_log_shock
  )

d$z_source_push_row <-
  0

d$z_source_push_world <-
  0

for (
  industry_i in primary_industries
) {
  shrics_i <- primary_shric |>
    dplyr::filter(
      wir_industry ==
        industry_i
    ) |>
    dplyr::pull(
      shric
    )

  industry_share_i <- rowSums(
    d[
      paste0(
        "ec05_share_shric_",
        shrics_i
      )
    ],
    na.rm = FALSE
  )

  q_i <- dplyr::if_else(
    d$source_push_covered_mfg_share >
      0,
    industry_share_i /
      d$source_push_covered_mfg_share,
    NA_real_
  )

  g_row <- industry_shock_lookup |>
    dplyr::filter(
      wir_industry ==
        industry_i
    ) |>
    dplyr::pull(
      shock_row
    )

  g_world <- industry_shock_lookup |>
    dplyr::filter(
      wir_industry ==
        industry_i
    ) |>
    dplyr::pull(
      shock_world
    )

  if (
    length(
      g_row
    ) !=
      1 ||
    length(
      g_world
    ) !=
      1
  ) {
    stop(
      "Industry shock is not unique for ",
      industry_i
    )
  }

  d$z_source_push_row <-
    d$z_source_push_row +
    q_i *
    g_row

  d$z_source_push_world <-
    d$z_source_push_world +
    q_i *
    g_world
}

d <- d |>
  dplyr::mutate(
    z_source_push_row =
      dplyr::if_else(
        source_push_covered_mfg_share >
          0,
        z_source_push_row,
        NA_real_
      ),

    z_source_push_world =
      dplyr::if_else(
        source_push_covered_mfg_share >
          0,
        z_source_push_world,
        NA_real_
      )
  )

instrument_audit <- d |>
  dplyr::summarise(
    n_ec05 =
      sum(
        !is.na(
          ec05_emp_all
        )
      ),

    n_positive_covered_manufacturing =
      sum(
        source_push_covered_mfg_share >
          0,
        na.rm = TRUE
      ),

    median_covered_share_of_nonfarm =
      stats::median(
        source_push_covered_mfg_share,
        na.rm = TRUE
      ),

    median_covered_fraction_of_manufacturing =
      stats::median(
        source_push_covered_mfg_share /
          ec05_mfg_share,
        na.rm = TRUE
      ),

    sd_row_instrument =
      stats::sd(
        z_source_push_row,
        na.rm = TRUE
      ),

    sd_world_inclusive_instrument =
      stats::sd(
        z_source_push_world,
        na.rm = TRUE
      )
  )

readr::write_csv(
  instrument_audit,
  file.path(
    out_manifest_dir,
    "02_ac_source_push_instrument_audit.csv"
  )
)

# ============================================================
# 3. FIRST-STAGE HELPERS
# ============================================================

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

  stats::cor(
    stats::residuals(
      y_fit
    ),
    stats::residuals(
      z_fit
    )
  )^2
}

fit_source_push <- function(
    instrument,
    placebo =
      FALSE,
    high_quality =
      FALSE
) {
  lhs <- if (
    placebo
  ) {
    "fdi_mfg_own_log_count_2009"
  } else {
    "fdi_mfg_own_log_count_2014"
  }

  controls <- if (
    placebo
  ) {
    paste(
      "log1p_ec05_emp_all",
      "ec05_mfg_share",
      "log1p_land_area",
      sep = " + "
    )
  } else {
    paste(
      "fdi_mfg_own_log_count_2009",
      "log1p_ec05_emp_all",
      "ec05_mfg_share",
      "log1p_land_area",
      sep = " + "
    )
  }

  formula <- stats::as.formula(
    paste0(
      lhs,
      " ~ ",
      instrument,
      " + ",
      controls,
      " | state_no"
    )
  )

  required <- unique(
    c(
      all.vars(
        formula
      ),
      "pc_cluster_id"
    )
  )

  dat <- d |>
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

  if (
    high_quality
  ) {
    dat <- dat |>
      dplyr::filter(
        ec05_high_quality_combined
      )
  }

  fit <- fixest::feols(
    formula,
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
      instrument
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument
    ]

  tibble::tibble(
    instrument =
      instrument,
    shock_universe =
      dplyr::if_else(
        instrument ==
          "z_source_push_row",
        "approximate rest of world",
        "world inclusive"
      ),
    placebo =
      placebo,
    sample =
      dplyr::if_else(
        high_quality,
        "High-quality EC05",
        "All complete EC05"
      ),
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
    partial_r2 =
      partial_r2_for_instrument(
        dat,
        lhs,
        instrument,
        controls
      ),
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      ),
    sign =
      dplyr::case_when(
        beta >
          0 ~
          "positive",
        beta <
          0 ~
          "negative",
        TRUE ~
          "zero"
      )
  )
}

headline <- dplyr::bind_rows(
  fit_source_push(
    "z_source_push_row",
    FALSE,
    FALSE
  ),
  fit_source_push(
    "z_source_push_row",
    FALSE,
    TRUE
  ),
  fit_source_push(
    "z_source_push_row",
    TRUE,
    FALSE
  ),
  fit_source_push(
    "z_source_push_row",
    TRUE,
    TRUE
  ),
  fit_source_push(
    "z_source_push_world",
    FALSE,
    FALSE
  ),
  fit_source_push(
    "z_source_push_world",
    FALSE,
    TRUE
  ),
  fit_source_push(
    "z_source_push_world",
    TRUE,
    FALSE
  ),
  fit_source_push(
    "z_source_push_world",
    TRUE,
    TRUE
  )
)

readr::write_csv(
  headline,
  file.path(
    out_table_dir,
    "00_HEADLINE_source_country_push_first_stage_and_placebo.csv"
  )
)

# ============================================================
# 4. LEAVE-ONE-INDUSTRY-OUT, PRIMARY ROW SHOCK
# ============================================================

build_industry_loo <- function(
    excluded_industry
) {
  kept <- setdiff(
    primary_industries,
    excluded_industry
  )

  kept_shric <- primary_shric |>
    dplyr::filter(
      wir_industry %in%
        kept
    )

  denom <- rowSums(
    d[
      paste0(
        "ec05_share_shric_",
        kept_shric$shric
      )
    ],
    na.rm = FALSE
  )

  z <- rep(
    0,
    nrow(
      d
    )
  )

  for (
    industry_i in kept
  ) {
    shrics_i <- kept_shric |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shric
      )

    share_i <- rowSums(
      d[
        paste0(
          "ec05_share_shric_",
          shrics_i
        )
      ],
      na.rm = FALSE
    )

    q_i <- dplyr::if_else(
      denom >
        0,
      share_i /
        denom,
      NA_real_
    )

    g_i <- industry_shock_lookup |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shock_row
      )

    z <-
      z +
      q_i *
      g_i
  }

  z[
    denom <=
      0
  ] <- NA_real_

  z
}

fit_custom_z <- function(
    z,
    placebo =
      FALSE,
    high_quality =
      FALSE
) {
  tmp <- d
  tmp$z_custom <-
    z

  lhs <- if (
    placebo
  ) {
    "fdi_mfg_own_log_count_2009"
  } else {
    "fdi_mfg_own_log_count_2014"
  }

  controls <- if (
    placebo
  ) {
    paste(
      "log1p_ec05_emp_all",
      "ec05_mfg_share",
      "log1p_land_area",
      sep = " + "
    )
  } else {
    paste(
      "fdi_mfg_own_log_count_2009",
      "log1p_ec05_emp_all",
      "ec05_mfg_share",
      "log1p_land_area",
      sep = " + "
    )
  }

  formula <- stats::as.formula(
    paste0(
      lhs,
      " ~ z_custom + ",
      controls,
      " | state_no"
    )
  )

  vars <- unique(
    c(
      all.vars(
        formula
      ),
      "pc_cluster_id"
    )
  )

  dat <- tmp |>
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
    high_quality
  ) {
    dat <- dat |>
      dplyr::filter(
        ec05_high_quality_combined
      )
  }

  fit <- fixest::feols(
    formula,
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
      "z_custom"
    ]

  se <-
    fixest::se(
      fit
    )[
      "z_custom"
    ]

  tibble::tibble(
    placebo =
      placebo,
    sample =
      dplyr::if_else(
        high_quality,
        "High-quality EC05",
        "All complete EC05"
      ),
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

industry_loo <- purrr::map_dfr(
  c(
    "NONE",
    primary_industries
  ),
  function(excluded) {
    z <- if (
      excluded ==
        "NONE"
    ) {
      d$z_source_push_row
    } else {
      build_industry_loo(
        excluded
      )
    }

    dplyr::bind_rows(
      fit_custom_z(
        z,
        FALSE,
        FALSE
      ),
      fit_custom_z(
        z,
        FALSE,
        TRUE
      ),
      fit_custom_z(
        z,
        TRUE,
        FALSE
      ),
      fit_custom_z(
        z,
        TRUE,
        TRUE
      )
    ) |>
      dplyr::mutate(
        excluded_wir_industry =
          excluded
      )
  }
)

readr::write_csv(
  industry_loo,
  file.path(
    out_table_dir,
    "01_leave_one_industry_out_source_push.csv"
  )
)

# ============================================================
# 5. LEAVE-ONE-MATERIAL-SOURCE-COUNTRY-OUT
# ============================================================
#
# Remove a source country's preperiod contribution from every industry and
# re-normalize the remaining source-country shares within each industry.
# Only countries with >=20 preperiod projects across the primary industry
# universe are subjected to this diagnostic.

primary_components <- components |>
  dplyr::filter(
    wir_industry %in%
      primary_industries
  )

material_countries <- primary_components |>
  dplyr::group_by(
    source_country
  ) |>
  dplyr::summarise(
    pre2009_projects =
      sum(
        pre2009_projects
      ),
    .groups = "drop"
  ) |>
  dplyr::filter(
    pre2009_projects >=
      20
  ) |>
  dplyr::arrange(
    dplyr::desc(
      pre2009_projects
    )
  )

readr::write_csv(
  material_countries,
  file.path(
    out_manifest_dir,
    "03_material_source_countries_for_loo.csv"
  )
)

build_country_loo_industry_shocks <- function(
    excluded_country
) {
  primary_components |>
    dplyr::filter(
      source_country !=
        excluded_country
    ) |>
    dplyr::group_by(
      wir_industry
    ) |>
    dplyr::mutate(
      renorm_weight =
        pre2009_source_share_within_industry /
        sum(
          pre2009_source_share_within_industry
        )
    ) |>
    dplyr::summarise(
      shock_row =
        sum(
          renorm_weight *
            source_country_approx_rest_of_world_log_shock
        ),
      .groups = "drop"
    )
}

build_z_from_industry_table <- function(
    shock_tbl
) {
  denom <- d$source_push_covered_mfg_share

  z <- rep(
    0,
    nrow(
      d
    )
  )

  for (
    industry_i in primary_industries
  ) {
    shrics_i <- primary_shric |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shric
      )

    share_i <- rowSums(
      d[
        paste0(
          "ec05_share_shric_",
          shrics_i
        )
      ],
      na.rm = FALSE
    )

    q_i <- dplyr::if_else(
      denom >
        0,
      share_i /
        denom,
      NA_real_
    )

    g_i <- shock_tbl |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shock_row
      )

    if (
      length(
        g_i
      ) !=
        1
    ) {
      stop(
        "Leave-country-out industry shock missing/nonunique for ",
        industry_i
      )
    }

    z <-
      z +
      q_i *
      g_i
  }

  z[
    denom <=
      0
  ] <- NA_real_

  z
}

country_loo <- purrr::map_dfr(
  c(
    "NONE",
    material_countries$source_country
  ),
  function(excluded) {
    z <- if (
      excluded ==
        "NONE"
    ) {
      d$z_source_push_row
    } else {
      shock_tbl <- build_country_loo_industry_shocks(
        excluded
      )

      build_z_from_industry_table(
        shock_tbl
      )
    }

    dplyr::bind_rows(
      fit_custom_z(
        z,
        FALSE,
        FALSE
      ),
      fit_custom_z(
        z,
        FALSE,
        TRUE
      ),
      fit_custom_z(
        z,
        TRUE,
        FALSE
      ),
      fit_custom_z(
        z,
        TRUE,
        TRUE
      )
    ) |>
      dplyr::mutate(
        excluded_source_country =
          excluded
      )
  }
)

readr::write_csv(
  country_loo,
  file.path(
    out_table_dir,
    "02_leave_one_source_country_out_source_push.csv"
  )
)

# ============================================================
# 6. SOURCE-COUNTRY CONCENTRATION
# ============================================================

country_weights <- primary_components |>
  dplyr::group_by(
    source_country
  ) |>
  dplyr::summarise(
    pre2009_projects =
      sum(
        pre2009_projects
      ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    project_weight =
      pre2009_projects /
      sum(
        pre2009_projects
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(
      project_weight
    )
  )

effective_source_countries <-
  1 /
  sum(
    country_weights$project_weight^2
  )

readr::write_csv(
  country_weights,
  file.path(
    out_table_dir,
    "03_source_country_concentration.csv"
  )
)

readr::write_csv(
  tibble::tibble(
    n_source_countries =
      nrow(
        country_weights
      ),

    effective_number_source_countries =
      effective_source_countries,

    largest_source_weight =
      max(
        country_weights$project_weight
      ),

    top5_source_weight =
      sum(
        head(
          country_weights$project_weight,
          5
        )
      )
  ),
  file.path(
    out_manifest_dir,
    "04_source_country_concentration_summary.csv"
  )
)

# ============================================================
# 7. SAVE ANALYSIS BASE + FIGURE
# ============================================================

readr::write_rds(
  d,
  file.path(
    out_data_dir,
    "ac_iv_base_with_source_country_push.rds"
  )
)

p <- headline |>
  dplyr::filter(
    shock_universe ==
      "approximate rest of world"
  ) |>
  dplyr::mutate(
    period =
      dplyr::if_else(
        placebo,
        "Pre-period placebo: 2004-09 FDI",
        "Post-period first stage: 2009-14 FDI"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        cluster_f_single_instrument,
      y =
        sample
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 10,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(
    ~period
  ) +
  ggplot2::labs(
    title =
      "Source-country-push shift-share: post first stage vs pre-period placebo",

    subtitle =
      "WIR source-country outward shocks × pre-2009 India source-industry links × 2005 EC05 industry composition.",

    x =
      "PC-clustered single-instrument F statistic",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "01_source_country_push_post_vs_placebo.pdf"
  ),
  p,
  width = 10,
  height = 6.5
)

# ============================================================
# 8. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Source-country-push IV diagnostics revision: ",
      SOURCE_PUSH_REVISION
    ),
    "",
    "NO BJP/POLITICAL OUTCOME IS USED.",
    "",
    "WHAT THIS IS",
    "------------",
    "WIR Annex 16 supplies source-country outward greenfield-project shocks.",
    "Pre-2009 Source_Markets data supply each Indian industry's historical source-country mix.",
    "EC05 supplies each AC's predetermined 2005 manufacturing composition.",
    "",
    "The resulting instrument is a source-country-push x industry-composition Bartik.",
    "",
    "WHAT THIS IS NOT",
    "----------------",
    "It is not a true observed source-country x industry world shock because WIR Annex 16",
    "does not cross-tabulate source country and industry. A true joint shock would require",
    "global fDi Markets project rows (source country x industry x destination x year) or an",
    "equivalent joint source-by-industry table.",
    "",
    "PRIMARY SHOCK",
    "-------------",
    "Approximate rest-of-world source-country shock: WIR source-country project counts minus",
    "India-bound Source_Markets project counts, comparing 2010-13 with 2005-08 and omitting 2009.",
    "",
    "UNIVERSE WARNING",
    "----------------",
    "The uploaded Source_Markets India export and 2026 WIR use different database vintages.",
    "The subtraction is therefore approximate rather than an exact leave-India-out identity.",
    "World-inclusive source shocks are retained as a sensitivity.",
    "",
    "READ FIRST",
    "----------",
    "tables/00_HEADLINE_source_country_push_first_stage_and_placebo.csv",
    "tables/01_leave_one_industry_out_source_push.csv",
    "tables/02_leave_one_source_country_out_source_push.csv",
    "tables/03_source_country_concentration.csv",
    "manifests/00_source_push_industry_mapping_gate.csv",
    "manifests/01_primary_source_push_industry_universe.csv",
    "manifests/04_source_country_concentration_summary.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Source-country-push first-stage diagnostics COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No BJP/political outcome was estimated."
)

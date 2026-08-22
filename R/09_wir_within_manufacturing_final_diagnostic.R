# ============================================================
# 09_wir_within_manufacturing_final_diagnostic.R
#
# FINAL PRE-OUTCOME diagnostic for the WIR shift-share route.
#
# Motivation:
#   The earlier manufacturing Bartik used non-farm employment shares:
#
#       Z_i = sum_{k in MFG} (E_ik / E_i_nonfarm) * g_k
#
#   so total manufacturing intensity is mechanically embedded in Z_i.
#   This script instead asks whether the COMPOSITION of manufacturing,
#   conditional on how manufacturing-intensive an AC already is, predicts
#   subsequent manufacturing FDI:
#
#       q_ik = E_ik / E_i_MFG
#       Z_i^within = sum_{k in MFG} q_ik * g_k
#
# Primary instrument:
#   detailed-only, re-normalized within confidently mapped manufacturing
#   SHRICs. This removes both overall manufacturing intensity and the broad
#   parent-mapped SHRIC categories from the instrument itself.
#
# Secondary:
#   hierarchical within-manufacturing instrument using all manufacturing
#   SHRICs, including conservative parent-Manufacturing mappings.
#
# This script:
#   - evaluates OWN-AC manufacturing FDI;
#   - if the matched-local base from 08b exists, evaluates LOCAL FDI too;
#   - runs post-2009 first stages, pre-2009 placebo first stages,
#     high-quality EC05 restrictions, and leave-one-WIR-industry-out tests;
#   - never estimates a BJP/political outcome.
#
# GO / NO-GO PRINCIPLE:
#   This is the final attempt to diagnose the WIR route. If the within-MFG
#   construction still strongly predicts pre-period FDI, is driven by one
#   or two industries, or is weak/unstable, the WIR IV should be retired
#   rather than further tuned to political outcomes.
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

WITHIN_MFG_REVISION <-
  "2026-08-09-v1.0-final-within-manufacturing-diagnostic"

message(
  "Starting final within-manufacturing WIR diagnostic: ",
  WITHIN_MFG_REVISION
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

own_path <- file.path(
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

local_path <- file.path(
  iv_root,
  "local_first_stage_diagnostics",
  "data",
  "ac_iv_local_first_stage_base.rds"
)

purrr::walk(
  c(
    own_path,
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
        "Missing required input: ",
        p,
        ". Run corrected 06 and 07 first."
      )
    }
  }
)

out_root <- file.path(
  iv_root,
  "wir_within_manufacturing_final_diagnostic"
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
# 1. LOAD SHOCKS + FROZEN CROSSWALK
# ============================================================

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

detailed_crosswalk <- mfg_crosswalk |>
  dplyr::filter(
    mapping_specificity ==
      "detailed"
  )

detailed_wir_industries <- sort(
  unique(
    detailed_crosswalk$wir_industry
  )
)

# ============================================================
# 2. HELPERS
# ============================================================

partial_r2_for_instrument <- function(
    data,
    lhs,
    instrument,
    controls,
    fe_var =
      "state_no"
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
        " | ",
        fe_var
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
        " | ",
        fe_var
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

fit_single_first_stage <- function(
    data,
    geography,
    lhs,
    instrument,
    controls,
    quality_filter,
    sample_label,
    placebo =
      FALSE
) {
  formula <- stats::as.formula(
    paste0(
      lhs,
      " ~ ",
      instrument,
      if (
        nzchar(
          controls
        )
      ) {
        paste0(
          " + ",
          controls
        )
      } else {
        ""
      },
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

  dat <- data |>
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
    !is.null(
      quality_filter
    )
  ) {
    dat <- dat |>
      dplyr::filter(
        !!rlang::parse_expr(
          quality_filter
        )
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

  t_stat <-
    beta /
    se

  tibble::tibble(
    geography =
      geography,
    placebo =
      placebo,
    sample =
      sample_label,
    lhs =
      lhs,
    instrument =
      instrument,
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
    n_states =
      dplyr::n_distinct(
        dat$state_no
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

# ============================================================
# 3. OWN-AC WITHIN-MANUFACTURING INSTRUMENTS
# ============================================================

own <- readRDS(
  own_path
)

own_share_vars <- paste0(
  "ec05_share_shric_",
  1:90
)

missing_own_shares <- setdiff(
  own_share_vars,
  names(
    own
  )
)

if (
  length(
    missing_own_shares
  ) >
    0
) {
  stop(
    "Own base lacks corrected 90-SHRIC EC05 shares."
  )
}

own <- own |>
  dplyr::mutate(
    own_mfg_share_from_catalog =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            paste0(
              "ec05_share_shric_",
              as.integer(
                mfg_crosswalk$shric
              )
            )
          )
        ),
        na.rm = FALSE
      ),

    own_detailed_mfg_share_from_catalog =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            paste0(
              "ec05_share_shric_",
              as.integer(
                detailed_crosswalk$shric
              )
            )
          )
        ),
        na.rm = FALSE
      ),

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

own$wir_within_mfg_hierarchical <-
  0

own$wir_within_mfg_detailed_renorm <-
  0

for (
  i in seq_len(
    nrow(
      mfg_crosswalk
    )
  )
) {
  k <- as.integer(
    mfg_crosswalk$shric[[i]]
  )

  share_var <- paste0(
    "ec05_share_shric_",
    k
  )

  conditional_share <-
    dplyr::if_else(
      own$own_mfg_share_from_catalog >
        0,
      own[[
        share_var
      ]] /
        own$own_mfg_share_from_catalog,
      NA_real_
    )

  own$wir_within_mfg_hierarchical <-
    own$wir_within_mfg_hierarchical +
    conditional_share *
    mfg_crosswalk$shock_log_change[[i]]
}

for (
  i in seq_len(
    nrow(
      detailed_crosswalk
    )
  )
) {
  k <- as.integer(
    detailed_crosswalk$shric[[i]]
  )

  share_var <- paste0(
    "ec05_share_shric_",
    k
  )

  conditional_share <-
    dplyr::if_else(
      own$own_detailed_mfg_share_from_catalog >
        0,
      own[[
        share_var
      ]] /
        own$own_detailed_mfg_share_from_catalog,
      NA_real_
    )

  own$wir_within_mfg_detailed_renorm <-
    own$wir_within_mfg_detailed_renorm +
    conditional_share *
    detailed_crosswalk$shock_log_change[[i]]
}

own <- own |>
  dplyr::mutate(
    wir_within_mfg_hierarchical =
      dplyr::if_else(
        own_mfg_share_from_catalog >
          0,
        wir_within_mfg_hierarchical,
        NA_real_
      ),

    wir_within_mfg_detailed_renorm =
      dplyr::if_else(
        own_detailed_mfg_share_from_catalog >
          0,
        wir_within_mfg_detailed_renorm,
        NA_real_
      )
  )

own_instrument_audit <- own |>
  dplyr::summarise(
    n_ec05 =
      sum(
        !is.na(
          ec05_emp_all
        )
      ),

    n_positive_mfg =
      sum(
        own_mfg_share_from_catalog >
          0,
        na.rm = TRUE
      ),

    n_positive_detailed_mfg =
      sum(
        own_detailed_mfg_share_from_catalog >
          0,
        na.rm = TRUE
      ),

    median_mfg_share =
      stats::median(
        own_mfg_share_from_catalog,
        na.rm = TRUE
      ),

    median_detailed_fraction_of_mfg =
      stats::median(
        own_detailed_mfg_share_from_catalog /
          own_mfg_share_from_catalog,
        na.rm = TRUE
      ),

    sd_within_hierarchical =
      stats::sd(
        wir_within_mfg_hierarchical,
        na.rm = TRUE
      ),

    sd_within_detailed =
      stats::sd(
        wir_within_mfg_detailed_renorm,
        na.rm = TRUE
      )
  )

readr::write_csv(
  own_instrument_audit,
  file.path(
    out_manifest_dir,
    "01_own_within_mfg_instrument_audit.csv"
  )
)

own_post_controls <- paste(
  "fdi_mfg_own_log_count_2009",
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

own_placebo_controls <- paste(
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

own_results <- dplyr::bind_rows(
  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2014",
    "wir_within_mfg_hierarchical",
    own_post_controls,
    NULL,
    "All complete EC05",
    FALSE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2014",
    "wir_within_mfg_detailed_renorm",
    own_post_controls,
    NULL,
    "All complete EC05",
    FALSE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2014",
    "wir_within_mfg_hierarchical",
    own_post_controls,
    "ec05_high_quality_combined",
    "High-quality EC05",
    FALSE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2014",
    "wir_within_mfg_detailed_renorm",
    own_post_controls,
    "ec05_high_quality_combined",
    "High-quality EC05",
    FALSE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2009",
    "wir_within_mfg_hierarchical",
    own_placebo_controls,
    NULL,
    "All complete EC05",
    TRUE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2009",
    "wir_within_mfg_detailed_renorm",
    own_placebo_controls,
    NULL,
    "All complete EC05",
    TRUE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2009",
    "wir_within_mfg_hierarchical",
    own_placebo_controls,
    "ec05_high_quality_combined",
    "High-quality EC05",
    TRUE
  ),

  fit_single_first_stage(
    own,
    "own AC",
    "fdi_mfg_own_log_count_2009",
    "wir_within_mfg_detailed_renorm",
    own_placebo_controls,
    "ec05_high_quality_combined",
    "High-quality EC05",
    TRUE
  )
)

# ============================================================
# 4. OWN DETAILED-ONLY LEAVE-ONE-INDUSTRY-OUT, RE-NORMALIZED
# ============================================================

build_own_detailed_loo <- function(
    excluded_industry
) {
  cw <- detailed_crosswalk |>
    dplyr::filter(
      wir_industry !=
        excluded_industry
    )

  included_shrics <- as.integer(
    cw$shric
  )

  denominator <- rowSums(
    own[
      paste0(
        "ec05_share_shric_",
        included_shrics
      )
    ],
    na.rm = FALSE
  )

  z <- rep(
    0,
    nrow(
      own
    )
  )

  for (
    i in seq_len(
      nrow(
        cw
      )
    )
  ) {
    k <- as.integer(
      cw$shric[[i]]
    )

    q <- dplyr::if_else(
      denominator >
        0,
      own[[
        paste0(
          "ec05_share_shric_",
          k
        )
      ]] /
        denominator,
      NA_real_
    )

    z <-
      z +
      q *
      cw$shock_log_change[[i]]
  }

  z[
    denominator <=
      0
  ] <- NA_real_

  z
}

own_loo <- purrr::map_dfr(
  c(
    "NONE",
    detailed_wir_industries
  ),
  function(excluded) {
    tmp <- own

    if (
      excluded ==
        "NONE"
    ) {
      tmp$z_loo <-
        tmp$wir_within_mfg_detailed_renorm
    } else {
      tmp$z_loo <-
        build_own_detailed_loo(
          excluded
        )
    }

    res <- fit_single_first_stage(
      tmp,
      "own AC",
      "fdi_mfg_own_log_count_2014",
      "z_loo",
      own_post_controls,
      NULL,
      "All complete EC05",
      FALSE
    )

    res |>
      dplyr::mutate(
        excluded_wir_industry =
          excluded
      )
  }
)

readr::write_csv(
  own_loo,
  file.path(
    out_table_dir,
    "03_own_within_mfg_leave_one_industry_out.csv"
  )
)

# ============================================================
# 5. OPTIONAL MATCHED-LOCAL WITHIN-MANUFACTURING DIAGNOSTIC
# ============================================================

local_results <- tibble::tibble()
local_loo <- tibble::tibble()

if (
  file.exists(
    local_path
  )
) {
  message(
    "Matched-local base found; adding local within-manufacturing diagnostic."
  )

  local <- readRDS(
    local_path
  )

  local_share_vars <- paste0(
    "local_ec05_share_shric_",
    1:90
  )

  missing_local_shares <- setdiff(
    local_share_vars,
    names(
      local
    )
  )

  if (
    length(
      missing_local_shares
    ) >
      0
  ) {
    stop(
      "Local base lacks pooled local SHRIC shares from 08b."
    )
  }

  local <- local |>
    dplyr::mutate(
      local_mfg_share_from_catalog =
        rowSums(
          dplyr::across(
            dplyr::all_of(
              paste0(
                "local_ec05_share_shric_",
                as.integer(
                  mfg_crosswalk$shric
                )
              )
            )
          ),
          na.rm = FALSE
        ),

      local_detailed_mfg_share_from_catalog =
        rowSums(
          dplyr::across(
            dplyr::all_of(
              paste0(
                "local_ec05_share_shric_",
                as.integer(
                  detailed_crosswalk$shric
                )
              )
            )
          ),
          na.rm = FALSE
        ),

      fdi_mfg_local_log_count_2014 =
        log1p(
          fdi_mfg_local_all_n_2014
        ),

      fdi_mfg_local_log_count_2009 =
        log1p(
          fdi_mfg_local_all_n_2009
        )
    )

  local$wir_within_mfg_local_hierarchical <-
    0

  local$wir_within_mfg_local_detailed_renorm <-
    0

  for (
    i in seq_len(
      nrow(
        mfg_crosswalk
      )
    )
  ) {
    k <- as.integer(
      mfg_crosswalk$shric[[i]]
    )

    q <- dplyr::if_else(
      local$local_mfg_share_from_catalog >
        0,
      local[[
        paste0(
          "local_ec05_share_shric_",
          k
        )
      ]] /
        local$local_mfg_share_from_catalog,
      NA_real_
    )

    local$wir_within_mfg_local_hierarchical <-
      local$wir_within_mfg_local_hierarchical +
      q *
      mfg_crosswalk$shock_log_change[[i]]
  }

  for (
    i in seq_len(
      nrow(
        detailed_crosswalk
      )
    )
  ) {
    k <- as.integer(
      detailed_crosswalk$shric[[i]]
    )

    q <- dplyr::if_else(
      local$local_detailed_mfg_share_from_catalog >
        0,
      local[[
        paste0(
          "local_ec05_share_shric_",
          k
        )
      ]] /
        local$local_detailed_mfg_share_from_catalog,
      NA_real_
    )

    local$wir_within_mfg_local_detailed_renorm <-
      local$wir_within_mfg_local_detailed_renorm +
      q *
      detailed_crosswalk$shock_log_change[[i]]
  }

  local <- local |>
    dplyr::mutate(
      wir_within_mfg_local_hierarchical =
        dplyr::if_else(
          local_mfg_share_from_catalog >
            0,
          wir_within_mfg_local_hierarchical,
          NA_real_
        ),

      wir_within_mfg_local_detailed_renorm =
        dplyr::if_else(
          local_detailed_mfg_share_from_catalog >
            0,
          wir_within_mfg_local_detailed_renorm,
          NA_real_
        )
    )

  local_post_controls <- paste(
    "fdi_mfg_local_log_count_2009",
    "log1p_local_ec05_emp_all",
    "local_ec05_mfg_share",
    "log1p_local_land_area",
    "n_touching_neighbors",
    sep = " + "
  )

  local_placebo_controls <- paste(
    "log1p_local_ec05_emp_all",
    "local_ec05_mfg_share",
    "log1p_local_land_area",
    "n_touching_neighbors",
    sep = " + "
  )

  local_results <- dplyr::bind_rows(
    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2014",
      "wir_within_mfg_local_hierarchical",
      local_post_controls,
      "local_ec05_complete",
      "Strict complete local EC05",
      FALSE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2014",
      "wir_within_mfg_local_detailed_renorm",
      local_post_controls,
      "local_ec05_complete",
      "Strict complete local EC05",
      FALSE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2014",
      "wir_within_mfg_local_hierarchical",
      local_post_controls,
      "local_all_members_high_quality",
      "All local EC05 members high quality",
      FALSE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2014",
      "wir_within_mfg_local_detailed_renorm",
      local_post_controls,
      "local_all_members_high_quality",
      "All local EC05 members high quality",
      FALSE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2009",
      "wir_within_mfg_local_hierarchical",
      local_placebo_controls,
      "local_ec05_complete",
      "Strict complete local EC05",
      TRUE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2009",
      "wir_within_mfg_local_detailed_renorm",
      local_placebo_controls,
      "local_ec05_complete",
      "Strict complete local EC05",
      TRUE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2009",
      "wir_within_mfg_local_hierarchical",
      local_placebo_controls,
      "local_all_members_high_quality",
      "All local EC05 members high quality",
      TRUE
    ),

    fit_single_first_stage(
      local,
      "local: own + touching ACs",
      "fdi_mfg_local_log_count_2009",
      "wir_within_mfg_local_detailed_renorm",
      local_placebo_controls,
      "local_all_members_high_quality",
      "All local EC05 members high quality",
      TRUE
    )
  )

  build_local_detailed_loo <- function(
      excluded_industry
  ) {
    cw <- detailed_crosswalk |>
      dplyr::filter(
        wir_industry !=
          excluded_industry
      )

    included_shrics <- as.integer(
      cw$shric
    )

    denominator <- rowSums(
      local[
        paste0(
          "local_ec05_share_shric_",
          included_shrics
        )
      ],
      na.rm = FALSE
    )

    z <- rep(
      0,
      nrow(
        local
      )
    )

    for (
      i in seq_len(
        nrow(
          cw
        )
      )
    ) {
      k <- as.integer(
        cw$shric[[i]]
      )

      q <- dplyr::if_else(
        denominator >
          0,
        local[[
          paste0(
            "local_ec05_share_shric_",
            k
          )
        ]] /
          denominator,
        NA_real_
      )

      z <-
        z +
        q *
        cw$shock_log_change[[i]]
    }

    z[
      denominator <=
        0
    ] <- NA_real_

    z
  }

  local_loo <- purrr::map_dfr(
    c(
      "NONE",
      detailed_wir_industries
    ),
    function(excluded) {
      tmp <- local

      if (
        excluded ==
          "NONE"
      ) {
        tmp$z_loo <-
          tmp$wir_within_mfg_local_detailed_renorm
      } else {
        tmp$z_loo <-
          build_local_detailed_loo(
            excluded
          )
      }

      res <- fit_single_first_stage(
        tmp,
        "local: own + touching ACs",
        "fdi_mfg_local_log_count_2014",
        "z_loo",
        local_post_controls,
        "local_ec05_complete",
        "Strict complete local EC05",
        FALSE
      )

      res |>
        dplyr::mutate(
          excluded_wir_industry =
            excluded
        )
    }
  )

  readr::write_csv(
    local_loo,
    file.path(
      out_table_dir,
      "04_local_within_mfg_leave_one_industry_out.csv"
    )
  )

  readr::write_rds(
    local,
    file.path(
      out_data_dir,
      "local_base_with_within_mfg_wir_instruments.rds"
    )
  )
}

# ============================================================
# 6. CONSOLIDATED HEADLINE + GO/NO-GO SCREEN
# ============================================================

all_results <- dplyr::bind_rows(
  own_results,
  local_results
)

readr::write_csv(
  all_results,
  file.path(
    out_table_dir,
    "01_all_within_mfg_first_stage_and_placebo_results.csv"
  )
)

headline <- all_results |>
  dplyr::filter(
    stringr::str_detect(
      instrument,
      "detailed_renorm"
    )
  ) |>
  dplyr::arrange(
    geography,
    placebo,
    sample
  )

readr::write_csv(
  headline,
  file.path(
    out_table_dir,
    "00_HEADLINE_within_mfg_diagnostic.csv"
  )
)

go_no_go <- headline |>
  dplyr::select(
    geography,
    placebo,
    sample,
    coefficient,
    cluster_f_single_instrument,
    partial_r2,
    n_ac,
    n_pc_clusters,
    sign
  ) |>
  tidyr::pivot_wider(
    names_from =
      placebo,
    values_from =
      c(
        coefficient,
        cluster_f_single_instrument,
        partial_r2,
        n_ac,
        n_pc_clusters,
        sign
      ),
    names_prefix =
      "placebo_"
  ) |>
  dplyr::mutate(
    post_f =
      cluster_f_single_instrument_placebo_FALSE,

    preperiod_placebo_f =
      cluster_f_single_instrument_placebo_TRUE,

    post_minus_placebo_f =
      post_f -
      preperiod_placebo_f,

    preliminary_screen =
      dplyr::case_when(
        post_f >=
          10 &
        preperiod_placebo_f <
          5 ~
          "potentially viable on relevance/placebo screen",

        post_f >=
          10 &
        preperiod_placebo_f >=
          5 ~
          "relevant but placebo concern",

        TRUE ~
          "does not clear conventional relevance screen"
      )
  )

readr::write_csv(
  go_no_go,
  file.path(
    out_table_dir,
    "02_GO_NO_GO_within_mfg_screen.csv"
  )
)

# ============================================================
# 7. FIGURE
# ============================================================

p <- headline |>
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
  ggplot2::facet_grid(
    geography ~ period,
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Final WIR diagnostic: within-manufacturing composition instrument",

    subtitle =
      "Detailed-only instrument re-normalized within confidently mapped manufacturing employment.",

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
    "01_within_mfg_post_vs_placebo_f.pdf"
  ),
  p,
  width = 11,
  height = 7
)

# ============================================================
# 8. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Final within-manufacturing WIR diagnostic revision: ",
      WITHIN_MFG_REVISION
    ),
    "",
    "THIS SCRIPT DOES NOT USE BJP/POLITICAL OUTCOMES.",
    "",
    "PRIMARY CONSTRUCTION",
    "--------------------",
    "Detailed-only WIR shocks are weighted by 2005 SHRIC employment shares re-normalized",
    "within confidently mapped manufacturing employment, not total non-farm employment.",
    "",
    "This removes overall manufacturing intensity from the instrument itself.",
    "The preferred first stage still controls for the AC's pre-treatment manufacturing share.",
    "",
    "OWN PRIMARY / LOCAL COMPANION",
    "-----------------------------",
    "Own-AC manufacturing FDI is the clean primary diagnostic.",
    "If the 08b matched-local base exists, the same within-manufacturing construction is",
    "also evaluated for the pooled own+touching-AC local economy.",
    "",
    "FINAL GATE",
    "----------",
    "This is intentionally the final WIR redesign before political outcomes.",
    "A conventionally strong post-period first stage is not sufficient if the same instrument",
    "also strongly predicts pre-2009 FDI or collapses under leave-one-industry-out tests.",
    "",
    "READ FIRST",
    "----------",
    "tables/00_HEADLINE_within_mfg_diagnostic.csv",
    "tables/02_GO_NO_GO_within_mfg_screen.csv",
    "tables/03_own_within_mfg_leave_one_industry_out.csv",
    "tables/04_local_within_mfg_leave_one_industry_out.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Final within-manufacturing WIR diagnostic COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No BJP/political outcome was estimated."
)

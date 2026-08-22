# ============================================================
# 11_iv_route_adjudication.R
#
# FINAL PRE-OUTCOME adjudication of the two causal-IV routes explored:
#   1. EC05 x WIR sector shocks
#   2. Official 2009-2014 FDI-policy reforms
#
# This script does NOT estimate BJP/political outcomes.
#
# It closes two remaining diagnostic gaps:
#   - high-quality-EC05 leave-one-WIR-industry-out tests for the only
#     WIR specification that cleared the conventional post/placebo screen;
#   - a formal policy-feasibility adjudication using the already-frozen
#     official-policy manifest and fDi Markets mapping audit.
#
# It then writes a pre-outcome recommendation on whether either route is
# strong enough to justify political 2SLS.
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

ADJUDICATION_REVISION <-
  "2026-08-09-v1.0-preoutcome-iv-route-adjudication"

message(
  "Starting IV route adjudication: ",
  ADJUDICATION_REVISION
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

wir_final_root <- file.path(
  iv_root,
  "wir_within_manufacturing_final_diagnostic"
)

policy_root <- file.path(
  iv_root,
  "official_policy_audit"
)

base_path <- file.path(
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

wir_headline_path <- file.path(
  wir_final_root,
  "tables",
  "00_HEADLINE_within_mfg_diagnostic.csv"
)

policy_manifest_path <- file.path(
  policy_root,
  "tables",
  "01_official_fdi_policy_reform_manifest_2009_2014.csv"
)

policy_feasibility_path <- file.path(
  policy_root,
  "tables",
  "00_HEADLINE_policy_iv_feasibility.csv"
)

policy_mapping_path <- file.path(
  policy_root,
  "tables",
  "06_policy_to_fdi_markets_sector_mapping.csv"
)

telecom_timing_path <- file.path(
  policy_root,
  "manifests",
  "01_telecom_project_timing_audit.csv"
)

required_paths <- c(
  base_path,
  crosswalk_path,
  shock_path,
  wir_headline_path,
  policy_manifest_path,
  policy_feasibility_path,
  policy_mapping_path,
  telecom_timing_path
)

missing_paths <- required_paths[
  !file.exists(
    required_paths
  )
]

if (
  length(
    missing_paths
  ) > 0
) {
  stop(
    "Missing prerequisite output(s): ",
    paste(
      missing_paths,
      collapse = "; "
    ),
    ". Run 09 and 10 first."
  )
}

out_root <- file.path(
  iv_root,
  "iv_route_adjudication"
)

out_table_dir <- file.path(
  out_root,
  "tables"
)

out_manifest_dir <- file.path(
  out_root,
  "manifests"
)

purrr::walk(
  c(
    out_root,
    out_table_dir,
    out_manifest_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD FROZEN WIR INPUTS
# ============================================================

d <- readRDS(
  base_path
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

wir_headline <- readr::read_csv(
  wir_headline_path,
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

detailed_crosswalk <- crosswalk |>
  dplyr::filter(
    manufacturing_shric,
    mapping_specificity ==
      "detailed"
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
      detailed_crosswalk$shock_log_change
    )
  )
) {
  stop(
    "At least one detailed manufacturing SHRIC lacks a WIR shock."
  )
}

detailed_wir_industries <- sort(
  unique(
    detailed_crosswalk$wir_industry
  )
)

share_var <- function(
    k
) {
  paste0(
    "ec05_share_shric_",
    k
  )
}

detailed_shrics <- as.integer(
  detailed_crosswalk$shric
)

required_base_vars <- c(
  "ac_uid",
  "state_no",
  "pc_cluster_id",
  "fdi_mfg_own_all_n_2009",
  "fdi_mfg_own_all_n_2014",
  "ec05_emp_all",
  "ec05_emp_manuf",
  "con08_land_area",
  "ec05_quality_high",
  "ec05_fragmentation_low",
  share_var(
    detailed_shrics
  )
)

missing_base <- setdiff(
  required_base_vars,
  names(
    d
  )
)

if (
  length(
    missing_base
  ) > 0
) {
  stop(
    "WIR base is missing required variable(s): ",
    paste(
      missing_base,
      collapse = ", "
    )
  )
}

# ============================================================
# 2. REBUILD DETAILED WITHIN-MANUFACTURING INSTRUMENT
# ============================================================

d <- d |>
  dplyr::mutate(
    detailed_mfg_share =
      rowSums(
        dplyr::across(
          dplyr::all_of(
            share_var(
              detailed_shrics
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

build_within_detailed <- function(
    excluded_industry =
      NULL
) {
  cw <- detailed_crosswalk

  if (
    !is.null(
      excluded_industry
    )
  ) {
    cw <- cw |>
      dplyr::filter(
        wir_industry !=
          excluded_industry
      )
  }

  shrics <- as.integer(
    cw$shric
  )

  denominator <- rowSums(
    d[
      share_var(
        shrics
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
      d[[
        share_var(
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

d$z_within_detailed <-
  build_within_detailed()

# ============================================================
# 3. HIGH-QUALITY LEAVE-ONE-INDUSTRY-OUT:
#    POST FIRST STAGE + PRE-PERIOD PLACEBO
# ============================================================

post_controls <- paste(
  "fdi_mfg_own_log_count_2009",
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

placebo_controls <- paste(
  "log1p_ec05_emp_all",
  "ec05_mfg_share",
  "log1p_land_area",
  sep = " + "
)

fit_clustered <- function(
    data,
    lhs,
    instrument,
    controls
) {
  f <- stats::as.formula(
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
        f
      ),
      "pc_cluster_id"
    )
  )

  dat <- data |>
    dplyr::filter(
      ec05_high_quality_combined,
      dplyr::if_all(
        dplyr::all_of(
          required
        ),
        ~!is.na(
          .x
        )
      )
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
      instrument
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument
    ]

  tibble::tibble(
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

hq_loo <- purrr::map_dfr(
  c(
    "NONE",
    detailed_wir_industries
  ),
  function(excluded) {
    tmp <- d

    tmp$z_loo <- if (
      excluded ==
        "NONE"
    ) {
      tmp$z_within_detailed
    } else {
      build_within_detailed(
        excluded
      )
    }

    post <- fit_clustered(
      tmp,
      "fdi_mfg_own_log_count_2014",
      "z_loo",
      post_controls
    ) |>
      dplyr::mutate(
        period =
          "post_2009_2014"
      )

    placebo <- fit_clustered(
      tmp,
      "fdi_mfg_own_log_count_2009",
      "z_loo",
      placebo_controls
    ) |>
      dplyr::mutate(
        period =
          "preperiod_placebo_2004_2009"
      )

    dplyr::bind_rows(
      post,
      placebo
    ) |>
      dplyr::mutate(
        excluded_wir_industry =
          excluded
      )
  }
)

readr::write_csv(
  hq_loo,
  file.path(
    out_table_dir,
    "01_high_quality_within_mfg_leave_one_out_post_and_placebo.csv"
  )
)

# ============================================================
# 4. HIGH-QUALITY INSTRUMENT CONCENTRATION
# ============================================================

hq <- d |>
  dplyr::filter(
    ec05_high_quality_combined,
    detailed_mfg_share >
      0
  )

industry_exposure_weights <- purrr::map_dfr(
  detailed_wir_industries,
  function(industry_i) {
    shrics_i <- detailed_crosswalk |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shric
      ) |>
      as.integer()

    numerator <- rowSums(
      hq[
        share_var(
          shrics_i
        )
      ],
      na.rm = FALSE
    )

    weight_i <-
      numerator /
      hq$detailed_mfg_share

    tibble::tibble(
      wir_industry =
        industry_i,
      mean_within_mfg_weight =
        mean(
          weight_i,
          na.rm = TRUE
        ),
      median_within_mfg_weight =
        stats::median(
          weight_i,
          na.rm = TRUE
        ),
      sd_within_mfg_weight =
        stats::sd(
          weight_i,
          na.rm = TRUE
        )
    )
  }
) |>
  dplyr::mutate(
    normalized_mean_weight =
      mean_within_mfg_weight /
      sum(
        mean_within_mfg_weight
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(
      normalized_mean_weight
    )
  )

effective_industry_count <-
  1 /
  sum(
    industry_exposure_weights$normalized_mean_weight^2
  )

top3_weight <-
  sum(
    head(
      industry_exposure_weights$normalized_mean_weight,
      3
    )
  )

readr::write_csv(
  industry_exposure_weights,
  file.path(
    out_table_dir,
    "02_high_quality_within_mfg_industry_exposure_weights.csv"
  )
)

readr::write_csv(
  tibble::tibble(
    n_detailed_wir_industries =
      nrow(
        industry_exposure_weights
      ),

    effective_industry_count =
      effective_industry_count,

    largest_industry_weight =
      max(
        industry_exposure_weights$normalized_mean_weight
      ),

    top3_industry_weight =
      top3_weight
  ),
  file.path(
    out_manifest_dir,
    "01_high_quality_within_mfg_concentration_summary.csv"
  )
)

# ============================================================
# 5. POLICY ROUTE ADJUDICATION
# ============================================================

policy_manifest <- readr::read_csv(
  policy_manifest_path,
  show_col_types = FALSE,
  progress = FALSE
)

policy_feasibility <- readr::read_csv(
  policy_feasibility_path,
  show_col_types = FALSE,
  progress = FALSE
)

policy_mapping <- readr::read_csv(
  policy_mapping_path,
  show_col_types = FALSE,
  progress = FALSE
)

telecom_timing <- readr::read_csv(
  telecom_timing_path,
  show_col_types = FALSE,
  progress = FALSE
)

n_direct_mfg_policy_reforms <-
  sum(
    policy_manifest$eligible_direct_manufacturing_iv,
    na.rm = TRUE
  )

telecom_mapping <- policy_mapping |>
  dplyr::filter(
    policy_family ==
      "Telecom services"
  )

broadcast_manifest <- policy_manifest |>
  dplyr::filter(
    reform_id ==
      "2012_BROADCASTING"
  )

telecom_manifest <- policy_manifest |>
  dplyr::filter(
    reform_id ==
      "2013_TELECOM"
  )

communications_sector_overlap <-
  nrow(
    broadcast_manifest
  ) ==
    1 &&
  nrow(
    telecom_manifest
  ) ==
    1 &&
  identical(
    as.character(
      broadcast_manifest$clean_fdi_market_sector
    ),
    as.character(
      telecom_manifest$clean_fdi_market_sector
    )
  )

telecom_post_n <- telecom_timing |>
  dplyr::filter(
    relative_to_2013_telecom_reform ==
      "after_2013_08_22_to_2014_03_31"
  ) |>
  dplyr::summarise(
    n =
      sum(
        n_communications_projects
      )
  ) |>
  dplyr::pull(
    n
  )

if (
  length(
    telecom_post_n
  ) ==
    0
) {
  telecom_post_n <-
    NA_real_
}

policy_adjudication <- tibble::tibble(
  diagnostic =
    c(
      "Clean national policy reforms directly instrumenting manufacturing FDI",
      "Cleanest services-policy mapping",
      "Post-telecom-reform Communications projects before 2014 FDI cutoff",
      "Communications label also exposed to earlier broadcasting reform"
    ),

  value =
    c(
      as.character(
        n_direct_mfg_policy_reforms
      ),

      if (
        nrow(
          telecom_mapping
        ) ==
          1
      ) {
        paste0(
          telecom_mapping$policy_family,
          " -> ",
          telecom_mapping$fdi_sector
        )
      } else {
        "not uniquely identified"
      },

      as.character(
        telecom_post_n
      ),

      as.character(
        communications_sector_overlap
      )
    ),

  implication =
    c(
      "No direct policy IV is available for the project's preferred manufacturing-FDI treatment from the frozen 2009-14 reform manifest.",
      "Telecom is a services-sector reform, not a manufacturing-FDI instrument.",
      "The available post-reform project window is sparse, making a useful local first stage unlikely even before political outcomes.",
      "The available fDi sector label cannot cleanly isolate the 2013 telecom reform from the earlier 2012 broadcasting-policy change."
    )
)

readr::write_csv(
  policy_adjudication,
  file.path(
    out_table_dir,
    "03_policy_route_adjudication.csv"
  )
)

# ============================================================
# 6. PRE-OUTCOME ROUTE RECOMMENDATION
# ============================================================

hq_post <- hq_loo |>
  dplyr::filter(
    excluded_wir_industry ==
      "NONE",
    period ==
      "post_2009_2014"
  )

hq_placebo <- hq_loo |>
  dplyr::filter(
    excluded_wir_industry ==
      "NONE",
    period ==
      "preperiod_placebo_2004_2009"
  )

hq_post_loo <- hq_loo |>
  dplyr::filter(
    excluded_wir_industry !=
      "NONE",
    period ==
      "post_2009_2014"
  )

summary_values <- tibble::tibble(
  metric =
    c(
      "WIR own-HQ overall post first-stage F",
      "WIR own-HQ overall preperiod placebo F",
      "WIR own-HQ minimum leave-one-industry-out post F",
      "WIR own-HQ effective industry count",
      "WIR own-HQ top-three mean exposure weight",
      "Policy clean direct manufacturing reforms",
      "Telecom post-reform Communications projects"
    ),

  value =
    c(
      hq_post$cluster_f_single_instrument,
      hq_placebo$cluster_f_single_instrument,
      min(
        hq_post_loo$cluster_f_single_instrument,
        na.rm = TRUE
      ),
      effective_industry_count,
      top3_weight,
      n_direct_mfg_policy_reforms,
      telecom_post_n
    )
)

readr::write_csv(
  summary_values,
  file.path(
    out_table_dir,
    "00_HEADLINE_iv_route_adjudication_metrics.csv"
  )
)

route_recommendation <- tibble::tribble(
  ~route, ~status, ~reason,

  "WIR within-manufacturing, own AC, high-quality EC05",
  "Do not promote to primary causal IV; retain only as exploratory/sensitivity evidence",
  "Overall relevance and placebo behavior improve in the high-quality EC05 subset, but identifying variation remains concentrated in a small number of manufacturing industries and first-stage strength is not stable to leave-one-industry-out removal.",

  "WIR within-manufacturing, local",
  "Reject as primary IV",
  "The strict-complete local sample does not clear the conventional relevance screen, while the high-quality local sample retains a material pre-period placebo concern.",

  "Official policy -> manufacturing FDI",
  "Unavailable with current data and 2009-14 policy variation",
  "The frozen official-policy audit identifies no national reform that maps cleanly both to baseline EC05 manufacturing exposure and to the project's new-greenfield manufacturing FDI treatment.",

  "2013 telecom policy -> services/Communications FDI",
  "Do not use as headline IV",
  "This is a services reform, the post-reform project window contains very few Communications projects before the 2014 cutoff, and the fDi Communications label is also relevant to the earlier broadcasting reform."
)

readr::write_csv(
  route_recommendation,
  file.path(
    out_table_dir,
    "04_FINAL_PREOUTCOME_route_recommendation.csv"
  )
)

# ============================================================
# 7. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "IV route adjudication revision: ",
      ADJUDICATION_REVISION
    ),
    "",
    "NO BJP/POLITICAL OUTCOME IS USED IN THIS SCRIPT.",
    "",
    "PURPOSE",
    "-------",
    "This file formalizes the decision reached before political 2SLS.",
    "It closes the high-quality WIR leave-one-industry-out gap and combines",
    "that evidence with the official-policy feasibility audit.",
    "",
    "WIR RULE",
    "--------",
    "The high-quality own-AC within-manufacturing WIR first stage may be reported",
    "as exploratory evidence if useful, but it should not be promoted to a primary",
    "publication-quality causal IV unless the researcher can defend the concentration",
    "and leave-one-industry-out instability on substantive grounds independent of outcomes.",
    "",
    "POLICY RULE",
    "-----------",
    "Do not force the 2012-13 reform package into a manufacturing-FDI instrument.",
    "The current policy/data crosswalk does not provide a clean national manufacturing",
    "policy shock for the project's new-greenfield manufacturing treatment.",
    "",
    "STOPPING RULE",
    "-------------",
    "Because neither investigated route currently supports a clean primary manufacturing-FDI IV,",
    "do not estimate BJP 2SLS merely to inspect whether the political coefficient is attractive.",
    "A new causal design should require new identifying variation or new data, not additional",
    "transformations of the same WIR/policy inputs.",
    "",
    "READ FIRST",
    "----------",
    "tables/00_HEADLINE_iv_route_adjudication_metrics.csv",
    "tables/01_high_quality_within_mfg_leave_one_out_post_and_placebo.csv",
    "tables/02_high_quality_within_mfg_industry_exposure_weights.csv",
    "tables/03_policy_route_adjudication.csv",
    "tables/04_FINAL_PREOUTCOME_route_recommendation.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "IV route adjudication COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No BJP/political 2SLS was estimated."
)

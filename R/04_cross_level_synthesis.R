# ============================================================
# 04_cross_level_synthesis.R
#
# POST-ESTIMATION-ONLY cross-level synthesis:
# aggregate AC-segment BJP outcomes + individual NES BJP choice.
#
# This script DOES NOT rerun either specification multiverse.
# It reads the saved frozen results and:
#   1. extracts pre-specified bridge specifications;
#   2. puts the linear aggregate/respondent estimates on a common
#      geographic exposure ruler;
#   3. keeps the respondent estimator/sample robustness results on
#      their original frozen substantive contrast;
#   4. creates cross-level forest plots and an evidence matrix.
#
# Bridge design frozen for synthesis
# ----------------------------------
# Economic exposure:
#   manufacturing FDI
#   local = own AC + neighbors
#   all announced/opened
#   log1p projects per 100,000
#
# Demographic contexts:
#   A. Muslim share, 2001
#   B. established migration stock share, up to 2001
#
# Aggregate:
#   first-difference + lagged-outcome
#   C1 controls
#   triple: weighted ideology-complete Center share, N >= 5
#
# Respondent:
#   2014 baseline-adjusted
#   V2 + C1
#   candidate-present primary
#   individual Center for triple
#
# IMPORTANT INTERPRETATION
# ------------------------
# The common geographic reference distribution standardizes the
# FDI and demographic movements only. It does not alter estimation
# samples or refit models.
#
# Triple-interaction magnitudes should NOT be compared mechanically
# across levels: aggregate Center is a continuous constituency share;
# respondent Center is an individual binary indicator.
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

SYNTHESIS_REVISION <-
  "2026-08-08-v1.0-cross-level-bridge"

AGGREGATE_RESULT_REVISION <-
  Sys.getenv(
    "SWITCHERS_AGGREGATE_RESULT_REVISION",
    unset = "2026-08-07-v4.3-preferred-n5"
  )

RESPONDENT_RESULT_REVISION <-
  "2026-08-08-v1.0.3-targeted-robustness-hotfix"

message(
  "Starting cross-level synthesis: ",
  SYNTHESIS_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

model_exploration_dir <- file.path(
  paths$derived_dir,
  "model_exploration"
)

aggregate_result_dir <- file.path(
  model_exploration_dir,
  "specification_curves",
  "results"
)

respondent_result_dir <- file.path(
  model_exploration_dir,
  "respondent_specification_curves",
  "results"
)

out_root <- file.path(
  model_exploration_dir,
  "cross_level_synthesis"
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
# 1. FROZEN BRIDGE DEFINITIONS
# ============================================================

bridge_fdi_family <- "mfg"
bridge_fdi_scope <- "local"
bridge_fdi_status <- "all"
bridge_fdi_form <- "log1p_pc100k"

bridge_fdi_2014 <-
  "log1p_fdi_mfg_local_all_pc100k_2014"

bridge_center_2009 <-
  "nes_weighted_share_center_among_ideology_complete_2009"

bridge_center_n_2009 <-
  "nes_n_ideology_complete_2009"

bridge_domains <- tibble::tribble(
  ~domain, ~moderator_var, ~label,

  "muslim",
  "muslim_share_2001_dist_proxy",
  "Muslim population share, 2001",

  "migration",
  "mig_total_upto_2001_share_ac_pop",
  "Established migrant stock share, up to 2001"
)

# ============================================================
# 2. LOAD COMMON GEOGRAPHIC REFERENCE DATA
# ============================================================

ac_change <- readRDS(
  file.path(
    paths$final_dir,
    "ac_change.rds"
  )
)

required_reference_vars <- c(
  "ac_uid",
  bridge_fdi_2014,
  bridge_domains$moderator_var,
  bridge_center_2009,
  bridge_center_n_2009
)

missing_reference_vars <- setdiff(
  required_reference_vars,
  names(ac_change)
)

if (
  length(
    missing_reference_vars
  ) > 0
) {
  stop(
    "ac_change is missing common-reference variables: ",
    paste(
      missing_reference_vars,
      collapse = ", "
    )
  )
}

# One AC gets one observation in the reference distribution.
# This deliberately avoids weighting reference values by the number
# of NES respondents sampled in an AC.
reference_ac <- ac_change |>
  dplyr::select(
    dplyr::all_of(
      required_reference_vars
    )
  ) |>
  dplyr::distinct(
    ac_uid,
    .keep_all = TRUE
  )

reference_fdi <- function(
    x
) {
  x <- x[
    is.finite(x)
  ]

  positive <- x[
    x > 0
  ]

  if (
    length(
      positive
    ) == 0
  ) {
    stop(
      "No positive FDI values in common geographic reference distribution."
    )
  }

  tibble::tibble(
    fdi_low_common = 0,
    fdi_high_common =
      stats::median(
        positive,
        na.rm = TRUE
      ),
    delta_fdi_common =
      fdi_high_common -
      fdi_low_common
  )
}

reference_q25_q75 <- function(
    x,
    prefix
) {
  x <- x[
    is.finite(x)
  ]

  if (
    length(
      unique(x)
    ) < 2
  ) {
    stop(
      "Insufficient variation in common reference: ",
      prefix
    )
  }

  q <- stats::quantile(
    x,
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE,
    names = FALSE
  )

  tibble::tibble(
    low = as.numeric(
      q[[1]]
    ),
    high = as.numeric(
      q[[2]]
    )
  ) |>
    dplyr::mutate(
      delta =
        high -
        low
    ) |>
    dplyr::rename_with(
      ~paste0(
        .x,
        "_",
        prefix
      )
    )
}

common_reference_rows <- purrr::map_dfr(
  seq_len(
    nrow(
      bridge_domains
    )
  ),
  function(i) {
    domain_i <-
      bridge_domains$domain[[i]]

    moderator_i <-
      bridge_domains$moderator_var[[i]]

    label_i <-
      bridge_domains$label[[i]]

    # Domain-specific geographic universe: one row per AC with both
    # FDI and the demographic context observed.
    d <- reference_ac |>
      dplyr::filter(
        is.finite(
          .data[[
            bridge_fdi_2014
          ]]
        ),
        is.finite(
          .data[[
            moderator_i
          ]]
        )
      )

    fref <- reference_fdi(
      d[[
        bridge_fdi_2014
      ]]
    )

    dref <- reference_q25_q75(
      d[[
        moderator_i
      ]],
      "demographic_common"
    )

    tibble::tibble(
      domain =
        domain_i,
      moderator_var =
        moderator_i,
      moderator_label =
        label_i,
      reference_geography =
        "One row per unique 2008-delimitation AC; unweighted across ACs",
      n_reference_acs =
        nrow(d)
    ) |>
      dplyr::bind_cols(
        fref,
        dref
      )
  }
)

center_reference_data <- reference_ac |>
  dplyr::filter(
    is.finite(
      .data[[
        bridge_center_2009
      ]]
    ),
    is.finite(
      .data[[
        bridge_center_n_2009
      ]]
    ),
    .data[[
      bridge_center_n_2009
    ]] >= 5
  )

center_ref <- reference_q25_q75(
  center_reference_data[[
    bridge_center_2009
  ]],
  "aggregate_center_common"
) |>
  dplyr::mutate(
    n_reference_acs_center =
      nrow(
        center_reference_data
      )
  )

common_reference_rows <- common_reference_rows |>
  dplyr::bind_cols(
    center_ref[
      rep(
        1,
        nrow(
          common_reference_rows
        )
      ),
      ,
      drop = FALSE
    ]
  )

readr::write_csv(
  common_reference_rows,
  file.path(
    out_manifest_dir,
    "common_geographic_reference_values.csv"
  )
)

# ============================================================
# 3. RESULT FILE HELPERS
# ============================================================

read_csv_quiet <- function(
    path
) {
  if (
    !file.exists(path)
  ) {
    stop(
      "Required result file does not exist: ",
      path
    )
  }

  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

aggregate_file <- function(
    design_id,
    domain,
    interaction_order
) {
  file.path(
    aggregate_result_dir,
    paste0(
      design_id,
      "__mfg__",
      interaction_order,
      "__full__",
      AGGREGATE_RESULT_REVISION,
      ".csv"
    )
  )
}

respondent_file <- function(
    family,
    domain,
    interaction_order
) {
  file.path(
    respondent_result_dir,
    paste0(
      family,
      "__respondent_2014_",
      domain,
      "__mfg__",
      interaction_order,
      "__full__",
      RESPONDENT_RESULT_REVISION,
      ".csv"
    )
  )
}

# ============================================================
# 4. EXTRACT AGGREGATE BRIDGE ROWS
# ============================================================

aggregate_designs <- tibble::tribble(
  ~design_id, ~design_short, ~design_order,

  "first_difference_muslim",
  "First difference",
  1L,

  "lagged_outcome_muslim",
  "Lagged outcome",
  2L,

  "first_difference_migration",
  "First difference",
  1L,

  "lagged_outcome_migration",
  "Lagged outcome",
  2L
)

extract_aggregate_bridge <- function(
    design_id,
    domain,
    interaction_order
) {
  d <- read_csv_quiet(
    aggregate_file(
      design_id,
      domain,
      interaction_order
    )
  )

  target_moderator <-
    bridge_domains |>
    dplyr::filter(
      .data$domain ==
        domain
    ) |>
    dplyr::pull(
      moderator_var
    )

  keep <- d |>
    dplyr::filter(
      fdi_family ==
        bridge_fdi_family,
      fdi_scope ==
        bridge_fdi_scope,
      fdi_status ==
        bridge_fdi_status,
      fdi_form ==
        bridge_fdi_form,
      moderator_var ==
        target_moderator,
      control_set ==
        "C1"
    )

  if (
    interaction_order ==
      "triple"
  ) {
    keep <- keep |>
      dplyr::filter(
        center_weighting ==
          "Survey weighted",
        center_denominator ==
          "Ideology-complete respondents",
        center_min_n ==
          5
      )
  }

  if (
    nrow(keep) != 1
  ) {
    stop(
      "Aggregate bridge row not unique: ",
      design_id,
      " / ",
      domain,
      " / ",
      interaction_order,
      ". Found ",
      nrow(keep),
      "."
    )
  }

  keep
}

aggregate_bridge <- purrr::map_dfr(
  seq_len(
    nrow(
      aggregate_designs
    )
  ),
  function(i) {
    design_i <-
      aggregate_designs$design_id[[i]]

    domain_i <- if (
      grepl(
        "muslim$",
        design_i
      )
    ) {
      "muslim"
    } else {
      "migration"
    }

    purrr::map_dfr(
      c(
        "two_way",
        "triple"
      ),
      function(order_i) {
        extract_aggregate_bridge(
          design_i,
          domain_i,
          order_i
        ) |>
          dplyr::mutate(
            evidence_level =
              "Aggregate AC-segment outcome",
            bridge_design =
              aggregate_designs$design_short[[i]],
            design_order =
              aggregate_designs$design_order[[i]]
          )
      }
    )
  }
)

# ============================================================
# 5. EXTRACT RESPONDENT PRIMARY BRIDGE ROWS
# ============================================================

extract_respondent_bridge <- function(
    family,
    domain,
    interaction_order
) {
  d <- read_csv_quiet(
    respondent_file(
      family,
      domain,
      interaction_order
    )
  )

  target_moderator <-
    bridge_domains |>
    dplyr::filter(
      .data$domain ==
        domain
    ) |>
    dplyr::pull(
      moderator_var
    )

  keep <- d |>
    dplyr::filter(
      fdi_family ==
        bridge_fdi_family,
      fdi_scope ==
        bridge_fdi_scope,
      fdi_status ==
        bridge_fdi_status,
      fdi_form ==
        bridge_fdi_form,
      moderator_var ==
        target_moderator,
      voter_control_set ==
        "V2",
      context_control_set ==
        "C1"
    )

  if (
    nrow(keep) != 1
  ) {
    stop(
      "Respondent bridge row not unique: ",
      family,
      " / ",
      domain,
      " / ",
      interaction_order,
      ". Found ",
      nrow(keep),
      "."
    )
  }

  keep
}

respondent_primary_bridge <- purrr::map_dfr(
  c(
    "muslim",
    "migration"
  ),
  function(domain_i) {
    purrr::map_dfr(
      c(
        "two_way",
        "triple"
      ),
      function(order_i) {
        extract_respondent_bridge(
          "primary",
          domain_i,
          order_i
        ) |>
          dplyr::mutate(
            evidence_level =
              "Individual voter choice",
            bridge_design =
              "2014 baseline-adjusted",
            design_order =
              3L
          )
      }
    )
  }
)

# ============================================================
# 6. COMMON-RULER RESCALING FOR LINEAR PRIMARY MODELS
# ============================================================

z_crit <- stats::qnorm(
  0.975
)

aggregate_bridge_common <- aggregate_bridge |>
  dplyr::left_join(
    common_reference_rows |>
      dplyr::select(
        domain,
        dplyr::ends_with(
          "_common"
        )
      ),
    by = c(
      "moderator_domain" =
        "domain"
    )
  ) |>
  dplyr::mutate(
    center_delta_for_bridge =
      dplyr::if_else(
        interaction_order ==
          "triple",
        delta_aggregate_center_common,
        1
      ),

    common_multiplier =
      delta_fdi_common *
      delta_demographic_common *
      center_delta_for_bridge,

    common_contrast_pp =
      interaction_estimate *
      common_multiplier,

    common_contrast_se_pp =
      interaction_se *
      abs(
        common_multiplier
      ),

    common_conf_low_pp =
      common_contrast_pp -
      z_crit *
      common_contrast_se_pp,

    common_conf_high_pp =
      common_contrast_pp +
      z_crit *
      common_contrast_se_pp,

    center_scale =
      dplyr::if_else(
        interaction_order ==
          "triple",
        paste0(
          "Aggregate weighted Center share: q25→q75 (Δ=",
          signif(
            delta_aggregate_center_common,
            3
          ),
          ")"
        ),
        NA_character_
      )
  )

respondent_bridge_common <- respondent_primary_bridge |>
  dplyr::left_join(
    common_reference_rows |>
      dplyr::select(
        domain,
        dplyr::ends_with(
          "_common"
        )
      ),
    by = c(
      "moderator_domain" =
        "domain"
    )
  ) |>
  dplyr::mutate(
    # Individual Center changes 0 -> 1, hence center multiplier = 1.
    center_delta_for_bridge =
      1,

    common_multiplier =
      100 *
      delta_fdi_common *
      delta_demographic_common *
      center_delta_for_bridge,

    common_contrast_pp =
      interaction_estimate *
      common_multiplier,

    common_contrast_se_pp =
      interaction_se *
      abs(
        common_multiplier
      ),

    common_conf_low_pp =
      common_contrast_pp -
      z_crit *
      common_contrast_se_pp,

    common_conf_high_pp =
      common_contrast_pp +
      z_crit *
      common_contrast_se_pp,

    center_scale =
      dplyr::if_else(
        interaction_order ==
          "triple",
        "Individual Center: other ideology-complete → Center (0→1)",
        NA_character_
      )
  )

bridge_columns <- c(
  "evidence_level",
  "bridge_design",
  "design_order",
  "moderator_domain",
  "moderator_var",
  "interaction_order",
  "interaction_estimate",
  "interaction_se",
  "common_contrast_pp",
  "common_contrast_se_pp",
  "common_conf_low_pp",
  "common_conf_high_pp",
  "nobs",
  "center_scale"
)

cross_level_bridge <- dplyr::bind_rows(
  aggregate_bridge_common |>
    dplyr::select(
      dplyr::all_of(
        bridge_columns
      )
    ),

  respondent_bridge_common |>
    dplyr::select(
      dplyr::all_of(
        bridge_columns
      )
    )
) |>
  dplyr::left_join(
    bridge_domains |>
      dplyr::select(
        domain,
        moderator_label =
          label
      ),
    by = c(
      "moderator_domain" =
        "domain"
    )
  ) |>
  dplyr::mutate(
    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI × demographic context",
        triple =
          "Centrist moderation"
      ),

    evidence_category =
      dplyr::case_when(
        common_conf_low_pp >
          0 ~
          "Positive; CI excludes zero",

        common_conf_high_pp <
          0 ~
          "Negative; CI excludes zero",

        common_contrast_pp >
          0 ~
          "Positive; imprecise",

        common_contrast_pp <
          0 ~
          "Negative; imprecise",

        TRUE ~
          "Near zero"
      )
  ) |>
  dplyr::arrange(
    moderator_domain,
    interaction_order,
    design_order
  )

readr::write_csv(
  cross_level_bridge,
  file.path(
    out_table_dir,
    "cross_level_bridge_estimates_common_ruler.csv"
  )
)

# ============================================================
# 7. RESPONDENT BRIDGE ROBUSTNESS ON THE ORIGINAL FROZEN ESTIMAND
# ============================================================

respondent_robustness_families <- c(
  "primary",
  "logit",
  "unweighted",
  "all_valid"
)

respondent_bridge_robustness <- purrr::map_dfr(
  respondent_robustness_families,
  function(family_i) {
    purrr::map_dfr(
      c(
        "muslim",
        "migration"
      ),
      function(domain_i) {
        purrr::map_dfr(
          c(
            "two_way",
            "triple"
          ),
          function(order_i) {
            extract_respondent_bridge(
              family_i,
              domain_i,
              order_i
            ) |>
              dplyr::transmute(
                analysis_family,
                analysis_family_label,
                estimator,
                outcome_sample,
                moderator_domain,
                moderator_var,
                interaction_order,
                contrast_estimate_pp =
                  contrast_estimate,
                conf_low_pp =
                  contrast_conf_low,
                conf_high_pp =
                  contrast_conf_high,
                nobs,
                weighted_ess,
                n_pcs,
                n_districts,
                contrast_ci_method,
                reference_note =
                  "Original frozen respondent substantive contrast; reference values were anchored to the primary candidate-present base sample."
              )
          }
        )
      }
    )
  }
)

readr::write_csv(
  respondent_bridge_robustness,
  file.path(
    out_table_dir,
    "respondent_bridge_robustness_saved_estimands.csv"
  )
)

# ============================================================
# 8. THEORY EVIDENCE MATRIX
# ============================================================

summarize_level_evidence <- function(
    data,
    domain,
    interaction_order,
    level
) {
  d <- data |>
    dplyr::filter(
      moderator_domain ==
        domain,
      .data$interaction_order ==
        interaction_order,
      evidence_level ==
        level
    )

  if (
    nrow(d) == 0
  ) {
    return(
      "No bridge estimate"
    )
  }

  paste(
    paste0(
      d$bridge_design,
      ": ",
      sprintf(
        "%+.2f pp",
        d$common_contrast_pp
      ),
      " [",
      sprintf(
        "%+.2f",
        d$common_conf_low_pp
      ),
      ", ",
      sprintf(
        "%+.2f",
        d$common_conf_high_pp
      ),
      "]"
    ),
    collapse = "; "
  )
}

make_matrix_row <- function(
    domain,
    interaction_order,
    proposition
) {
  agg_text <- summarize_level_evidence(
    cross_level_bridge,
    domain,
    interaction_order,
    "Aggregate AC-segment outcome"
  )

  resp_text <- summarize_level_evidence(
    cross_level_bridge,
    domain,
    interaction_order,
    "Individual voter choice"
  )

  agg_rows <- cross_level_bridge |>
    dplyr::filter(
      moderator_domain ==
        domain,
      .data$interaction_order ==
        interaction_order,
      evidence_level ==
        "Aggregate AC-segment outcome"
    )

  resp_rows <- cross_level_bridge |>
    dplyr::filter(
      moderator_domain ==
        domain,
      .data$interaction_order ==
        interaction_order,
      evidence_level ==
        "Individual voter choice"
    )

  agg_direction <- sign(
    stats::median(
      agg_rows$common_contrast_pp,
      na.rm = TRUE
    )
  )

  resp_direction <- sign(
    stats::median(
      resp_rows$common_contrast_pp,
      na.rm = TRUE
    )
  )

  alignment <- dplyr::case_when(
    agg_direction >
      0 &
      resp_direction >
      0 ~
      "Directional replication: positive at both levels",

    agg_direction <
      0 &
      resp_direction <
      0 ~
      "Directional replication: negative at both levels",

    agg_direction !=
      resp_direction ~
      "No directional replication across levels",

    TRUE ~
      "Mixed/near-zero"
  )

  tibble::tibble(
    proposition =
      proposition,
    aggregate_evidence =
      agg_text,
    respondent_evidence =
      resp_text,
    cross_level_assessment =
      alignment
  )
}

theory_evidence_matrix <- dplyr::bind_rows(
  make_matrix_row(
    "muslim",
    "two_way",
    "Manufacturing FDI × pre-treatment Muslim context"
  ),

  make_matrix_row(
    "muslim",
    "triple",
    "Centrist moderation of manufacturing FDI × Muslim context"
  ),

  make_matrix_row(
    "migration",
    "two_way",
    "Manufacturing FDI × established migration context"
  ),

  make_matrix_row(
    "migration",
    "triple",
    "Centrist moderation of manufacturing FDI × established migration context"
  )
) |>
  dplyr::mutate(
    interpretation_note =
      dplyr::if_else(
        grepl(
          "Centrist moderation",
          proposition
        ),
        "For triples, compare direction/uncertainty rather than magnitudes: aggregate Center is a continuous constituency share, respondent Center is binary individual status.",
        "Two-way effects use the same AC-derived FDI and demographic movements across levels."
      )
  )

readr::write_csv(
  theory_evidence_matrix,
  file.path(
    out_table_dir,
    "theory_evidence_matrix.csv"
  )
)

# ============================================================
# 9. CROSS-LEVEL FIGURES
# ============================================================

plot_two_way <- cross_level_bridge |>
  dplyr::filter(
    interaction_order ==
      "two_way"
  ) |>
  dplyr::mutate(
    model_label =
      paste(
        evidence_level,
        bridge_design,
        sep = " — "
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        common_contrast_pp,
      y =
        stats::reorder(
          model_label,
          design_order
        )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        common_conf_low_pp,
      xmax =
        common_conf_high_pp
    ),
    orientation = "y",
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_wrap(
    ~moderator_label,
    scales = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Cross-level bridge: manufacturing FDI × demographic context",

    subtitle =
      "All estimates use the same AC-derived 0→median-positive FDI movement and q25→q75 demographic movement.",

    x =
      "Substantive contrast in BJP support (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "01_cross_level_two_way_common_ruler.pdf"
  ),
  plot_two_way,
  width = 11,
  height = 7
)

plot_triple <- cross_level_bridge |>
  dplyr::filter(
    interaction_order ==
      "triple"
  ) |>
  dplyr::mutate(
    model_label =
      paste(
        evidence_level,
        bridge_design,
        sep = " — "
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        common_contrast_pp,
      y =
        stats::reorder(
          model_label,
          design_order
        )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        common_conf_low_pp,
      xmax =
        common_conf_high_pp
    ),
    orientation = "y",
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_grid(
    moderator_label ~
      evidence_level,
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::labs(
    title =
      "Cross-level centrist-moderation evidence",

    subtitle =
      "Do not compare magnitudes across columns: the aggregate moderator is q25→q75 Center share; the respondent moderator is individual 0→1 Center status.",

    x =
      "Substantive triple-interaction contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 9.5
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "02_cross_level_centrist_moderation.pdf"
  ),
  plot_triple,
  width = 12,
  height = 8
)

robustness_plot_data <- respondent_bridge_robustness |>
  dplyr::mutate(
    family_label =
      dplyr::recode(
        analysis_family,
        primary =
          "Primary weighted LPM",
        logit =
          "Weighted logit",
        unweighted =
          "Unweighted LPM",
        all_valid =
          "All valid voters"
      ),

    interaction_label =
      dplyr::recode(
        interaction_order,
        two_way =
          "FDI × demographic",
        triple =
          "Center amplification"
      ),

    domain_label =
      dplyr::recode(
        moderator_domain,
        muslim =
          "Muslim share, 2001",
        migration =
          "Established migration stock"
      )
  )

plot_robustness <- ggplot2::ggplot(
  robustness_plot_data,
  ggplot2::aes(
    x =
      contrast_estimate_pp,
    y =
      family_label
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        conf_low_pp,
      xmax =
        conf_high_pp
    ),
    orientation = "y",
    linewidth = 0.55
  ) +
  ggplot2::geom_point(
    size = 1.8
  ) +
  ggplot2::facet_grid(
    interaction_label ~
      domain_label,
    scales = "free_x"
  ) +
  ggplot2::labs(
    title =
      "Respondent bridge robustness",

    subtitle =
      "These use each family's frozen pre-specified respondent contrast; logit estimates are probability-scale g-computation.",

    x =
      "BJP-voting probability contrast (percentage points)",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 9.5
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "03_respondent_bridge_robustness.pdf"
  ),
  plot_robustness,
  width = 11,
  height = 7.5
)

# ============================================================
# 10. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Cross-level synthesis revision: ",
      SYNTHESIS_REVISION
    ),
    paste0(
      "Aggregate result revision read: ",
      AGGREGATE_RESULT_REVISION
    ),
    paste0(
      "Respondent result revision read: ",
      RESPONDENT_RESULT_REVISION
    ),
    "",
    "WHAT 'COMMON GEOGRAPHIC REFERENCE DISTRIBUTION' MEANS",
    "-----------------------------------------------------",
    "The models are NOT re-estimated on a common sample.",
    "Instead, a single geographic ruler is computed from one row per unique AC.",
    "Within each demographic domain, FDI is standardized as 0 -> median positive FDI",
    "and the demographic context as q25 -> q75 across those ACs.",
    "The saved linear interaction coefficients are then rescaled to those same numeric movements.",
    "This prevents the respondent sample (which implicitly overweights ACs with more sampled respondents)",
    "and the aggregate complete-case sample from using different medians/quartiles merely because",
    "their estimation samples differ.",
    "",
    "For aggregate triple interactions, Center uses q25 -> q75 of the weighted 2009",
    "ideology-complete AC Center share among ACs with NES N >= 5.",
    "For respondent triple interactions, Center is a binary 0 -> 1 individual contrast.",
    "Therefore triple magnitudes are intentionally NOT treated as directly comparable across levels.",
    "",
    "BRIDGE SPECIFICATIONS",
    "---------------------",
    "FDI: manufacturing / local own+neighbors / all announced-opened / log1p per 100k.",
    "Demographics: Muslim share 2001; established migration stock share up to 2001.",
    "Aggregate: first-difference and lagged-outcome, C1; triple uses weighted Center N>=5.",
    "Respondent: 2014 baseline-adjusted, V2+C1, candidate-present primary.",
    "",
    "IMPORTANT AGGREGATE NOTE",
    "------------------------",
    "This synthesis intentionally reads the frozen v4.3 aggregate C1 results.",
    "The old C2/C3 failure was caused by the obsolete all-NA employment control and does not",
    "invalidate C1. If a repaired full aggregate universe is later completed, set",
    "SWITCHERS_AGGREGATE_RESULT_REVISION to its revision and rerun after verifying filenames.",
    "",
    "READ FIRST",
    "----------",
    "tables/cross_level_bridge_estimates_common_ruler.csv",
    "tables/theory_evidence_matrix.csv",
    "tables/respondent_bridge_robustness_saved_estimands.csv",
    "figures/01_cross_level_two_way_common_ruler.pdf",
    "figures/02_cross_level_centrist_moderation.pdf",
    "figures/03_respondent_bridge_robustness.pdf"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Cross-level synthesis COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "Read first: ",
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

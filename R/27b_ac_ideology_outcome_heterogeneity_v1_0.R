suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(fixest)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

input_dir <-
  file.path(
    project_root,
    "data",
    "derived",
    "switchers_rewrite",
    "final"
  )

output_dir <-
  file.path(
    project_root,
    "outputs",
    "ac_ideology_outcome_heterogeneity_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ideology_path <-
  file.path(
    input_dir,
    "ac_year_ideology_summary.rds"
  )

change_path <-
  file.path(
    input_dir,
    "ac_change.rds"
  )

canonical_models_path <-
  file.path(
    project_root,
    "outputs",
    "ac_canonical_v1_0",
    "models.rds"
  )

sector_matrix_path <-
  file.path(
    project_root,
    "outputs",
    "sector_form_native_common_v1_0",
    "05_centrist_ac_sector_form_native_common.csv"
  )

required_files <-
  c(
    ideology_path,
    change_path,
    canonical_models_path,
    sector_matrix_path
  )

missing_files <-
  required_files[
    !file.exists(
      required_files
    )
  ]

if (
  length(
    missing_files
  ) >
    0L
) {
  stop(
    "Missing required inputs: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

ideology <-
  readRDS(
    ideology_path
  )

ac_change <-
  readRDS(
    change_path
  )

canonical_models <-
  readRDS(
    canonical_models_path
  )

sector_matrix <-
  read_csv(
    sector_matrix_path,
    show_col_types = FALSE
  ) |>
  filter(
    sample_type ==
      "Native"
  )

primary_controls <-
  c(
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share"
  )

required_ideology_columns <-
  c(
    "ac_uid",
    "year",
    "ideology",
    "weighted_share_voted_bjp",
    "state_no",
    "pc_cluster_id",
    "bjp_candidate_present",
    "fdi_spatial_support",
    "muslim_share_2001_dist_proxy",
    primary_controls
  )

missing_ideology_columns <-
  setdiff(
    required_ideology_columns,
    names(
      ideology
    )
  )

if (
  length(
    missing_ideology_columns
  ) >
    0L
) {
  stop(
    "ac_year_ideology_summary is missing: ",
    paste(
      missing_ideology_columns,
      collapse = ", "
    )
  )
}

fdi_variables <-
  c(
    "fdi_total_local_all_pc100k_2009",
    "fdi_total_local_all_pc100k_2014",
    "log1p_fdi_total_local_all_pc100k_2009",
    "log1p_fdi_total_local_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2009",
    "fdi_mfg_local_all_pc100k_2014"
  )

missing_fdi_columns <-
  setdiff(
    c(
      "ac_uid",
      fdi_variables
    ),
    names(
      ac_change
    )
  )

if (
  length(
    missing_fdi_columns
  ) >
    0L
) {
  stop(
    "ac_change is missing: ",
    paste(
      missing_fdi_columns,
      collapse = ", "
    )
  )
}

fdi_source <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      fdi_variables
    )
  )

if (
  anyDuplicated(
    fdi_source$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change FDI payload is not unique by ac_uid."
  )
}

ideology_levels <-
  c(
    "Left",
    "Center",
    "Right"
  )

analysis_base <-
  ideology |>
  filter(
    year ==
      2014,
    as.character(
      ideology
    ) %in%
      ideology_levels
  ) |>
  mutate(
    ideology_outcome =
      factor(
        as.character(
          ideology
        ),
        levels =
          ideology_levels
      )
  ) |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_source,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    y =
      as.numeric(
        weighted_share_voted_bjp
      ),

    muslim =
      as.numeric(
        muslim_share_2001_dist_proxy
      ),

    total_raw_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    total_raw_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      ),

    total_log_current =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2014
      ),

    total_log_baseline =
      as.numeric(
        log1p_fdi_total_local_all_pc100k_2009
      ),

    mfg_raw_current =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    mfg_raw_baseline =
      as.numeric(
        fdi_mfg_local_all_pc100k_2009
      ),

    mfg_log_current =
      log1p(
        mfg_raw_current
      ),

    mfg_log_baseline =
      log1p(
        mfg_raw_baseline
      )
  )

duplicate_cells <-
  analysis_base |>
  count(
    ac_uid,
    ideology_outcome
  ) |>
  filter(
    n >
      1L
  )

if (
  nrow(
    duplicate_cells
  ) >
    0L
) {
  print(
    duplicate_cells,
    n = Inf
  )

  stop(
    "2014 ideology outcomes are not unique by AC x ideology."
  )
}

raw_fdi_columns <-
  c(
    "total_raw_current",
    "total_raw_baseline",
    "mfg_raw_current",
    "mfg_raw_baseline"
  )

raw_matrix <-
  as.matrix(
    analysis_base[
      raw_fdi_columns
    ]
  )

if (
  any(
    is.finite(
      raw_matrix
    ) &
      raw_matrix <
        0
  )
) {
  stop(
    "Negative FDI rate detected."
  )
}

stored_log_check <-
  analysis_base |>
  filter(
    is.finite(
      total_raw_current
    ),
    is.finite(
      total_raw_baseline
    ),
    is.finite(
      total_log_current
    ),
    is.finite(
      total_log_baseline
    )
  )

if (
  any(
    abs(
      log1p(
        stored_log_check$total_raw_current
      ) -
        stored_log_check$total_log_current
    ) >
      1e-10
  ) ||
    any(
      abs(
        log1p(
          stored_log_check$total_raw_baseline
        ) -
          stored_log_check$total_log_baseline
      ) >
        1e-10
    )
) {
  stop(
    "Stored Total log1p variables do not reproduce log1p(raw per100k)."
  )
}

cell_registry <-
  tribble(
    ~cell_id, ~sector, ~functional_form, ~current_col, ~baseline_col, ~canonical_model,

    "total_raw",
    "Total",
    "Raw",
    "total_raw_current",
    "total_raw_baseline",
    "AC01",

    "total_log1p",
    "Total",
    "log1p",
    "total_log_current",
    "total_log_baseline",
    "AC03",

    "manufacturing_raw",
    "Manufacturing",
    "Raw",
    "mfg_raw_current",
    "mfg_raw_baseline",
    "AC05",

    "manufacturing_log1p",
    "Manufacturing",
    "log1p",
    "mfg_log_current",
    "mfg_log_baseline",
    NA_character_
  )

find_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <-
    coefficient_names[
      vapply(
        coefficient_names,
        function(
          term
        ) {
          pieces <-
            strsplit(
              term,
              ":",
              fixed = TRUE
            )[[1]]

          length(
            pieces
          ) ==
            length(
              variables
            ) &&
            setequal(
              pieces,
              variables
            )
        },
        logical(1)
      )
    ]

  if (
    length(
      hits
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify interaction: ",
      paste(
        variables,
        collapse = " x "
      ),
      ". Matches: ",
      paste(
        hits,
        collapse = ", "
      )
    )
  }

  hits[[1]]
}

extract_fixest_term <- function(
  fit,
  variables
) {
  term <-
    find_interaction_term(
      names(
        coef(
          fit
        )
      ),
      variables
    )

  ct <-
    coeftable(
      fit
    )

  ci <-
    as.data.frame(
      confint(
        fit,
        level = 0.95
      )
    )

  tibble(
    term =
      term,

    estimate =
      unname(
        ct[
          term,
          1
        ]
      ),

    std_error =
      unname(
        ct[
          term,
          2
        ]
      ),

    conf_low =
      unname(
        ci[
          term,
          1
        ]
      ),

    conf_high =
      unname(
        ci[
          term,
          2
        ]
      ),

    p_value =
      unname(
        ct[
          term,
          4
        ]
      )
  )
}

make_native_sample <- function(
  data,
  ideology_name,
  current_col,
  baseline_col
) {
  data |>
    filter(
      as.character(
        ideology_outcome
      ) ==
        ideology_name
    ) |>
    mutate(
      fdi_current =
        .data[[
          current_col
        ]],

      fdi_baseline =
        .data[[
          baseline_col
        ]]
    ) |>
    filter(
      !is.na(
        y
      ),
      bjp_candidate_present %in%
        TRUE,
      fdi_spatial_support %in%
        TRUE,
      is.finite(
        muslim
      ),
      is.finite(
        fdi_current
      ),
      is.finite(
        fdi_baseline
      ),
      if_all(
        all_of(
          primary_controls
        ),
        ~ !is.na(.x)
      ),
      !is.na(
        state_no
      ),
      !is.na(
        pc_cluster_id
      )
    )
}

fit_ac_model <- function(
  data
) {
  feols(
    y ~
      muslim *
        fdi_current +
      muslim *
        fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share |
      state_no,
    data =
      data,
    vcov =
      ~ pc_cluster_id,
    warn =
      FALSE,
    notes =
      FALSE
  )
}

native_models <-
  list()

native_samples <-
  list()

native_results <-
  list()

sample_counts <-
  list()

for (
  i in
    seq_len(
      nrow(
        cell_registry
      )
    )
) {
  spec <-
    cell_registry[
      i,
      ,
      drop = FALSE
    ]

  for (
    ideology_name in
      ideology_levels
  ) {
    sample_i <-
      make_native_sample(
        analysis_base,
        ideology_name,
        spec$current_col,
        spec$baseline_col
      )

    if (
      nrow(
        sample_i
      ) ==
        0L
    ) {
      stop(
        "No estimable observations for ",
        ideology_name,
        " / ",
        spec$cell_id
      )
    }

    model_i <-
      fit_ac_model(
        sample_i
      )

    focal_i <-
      extract_fixest_term(
        model_i,
        c(
          "muslim",
          "fdi_current"
        )
      )

    key <-
      paste(
        spec$cell_id,
        tolower(
          ideology_name
        ),
        sep =
          "__"
      )

    native_models[[
      key
    ]] <-
      model_i

    native_samples[[
      key
    ]] <-
      sample_i

    native_results[[
      key
    ]] <-
      tibble(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        functional_form =
          spec$functional_form,

        ideology =
          ideology_name,

        sample_type =
          "Native",

        estimate =
          focal_i$estimate,

        std_error =
          focal_i$std_error,

        conf_low =
          focal_i$conf_low,

        conf_high =
          focal_i$conf_high,

        p_value =
          focal_i$p_value,

        n_ac =
          nrow(
            sample_i
          ),

        n_states =
          n_distinct(
            sample_i$state_no
          ),

        n_pc_clusters =
          n_distinct(
            sample_i$pc_cluster_id
          )
      )

    sample_counts[[
      key
    ]] <-
      tibble(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        functional_form =
          spec$functional_form,

        ideology =
          ideology_name,

        native_n_ac =
          nrow(
            sample_i
          ),

        native_n_states =
          n_distinct(
            sample_i$state_no
          ),

        native_n_pc_clusters =
          n_distinct(
            sample_i$pc_cluster_id
          )
      )
  }
}

native_results <-
  bind_rows(
    native_results
  )

sample_counts <-
  bind_rows(
    sample_counts
  )

reproduction_checks <-
  list()

for (
  i in
    seq_len(
      nrow(
        cell_registry
      )
    )
) {
  spec <-
    cell_registry[
      i,
      ,
      drop = FALSE
    ]

  center_new <-
    native_results |>
    filter(
      cell_id ==
        spec$cell_id,
      ideology ==
        "Center"
    ) |>
    pull(
      estimate
    )

  if (
    !is.na(
      spec$canonical_model
    )
  ) {
    if (
      !spec$canonical_model %in%
        names(
          canonical_models
        )
    ) {
      stop(
        "Canonical model missing: ",
        spec$canonical_model
      )
    }

    old_term <-
      extract_fixest_term(
        canonical_models[[
          spec$canonical_model
        ]],
        c(
          "muslim",
          "fdi_current"
        )
      )

    old_estimate <-
      old_term$estimate

    source_name <-
      spec$canonical_model
  } else {
    old_row <-
      sector_matrix |>
      filter(
        cell_id ==
          spec$cell_id
      )

    if (
      nrow(
        old_row
      ) !=
        1L
    ) {
      stop(
        "Could not uniquely locate prior Manufacturing-log Center result."
      )
    }

    old_estimate <-
      old_row$estimate

    source_name <-
      "sector_form_native_common_v1_0"
  }

  reproduction_checks[[
    i
  ]] <-
    tibble(
      cell_id =
        spec$cell_id,

      source =
        source_name,

      stored_center_estimate =
        old_estimate,

      r27b_center_estimate =
        center_new,

      absolute_difference =
        abs(
          old_estimate -
            center_new
        )
    )
}

reproduction_checks <-
  bind_rows(
    reproduction_checks
  )

if (
  any(
    reproduction_checks$absolute_difference >
      1e-8
  )
) {
  print(
    reproduction_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one Center native model fails canonical reproduction."
  )
}

common_ids_by_cell <-
  list()

common_samples_long <-
  list()

common_separate_models <-
  list()

common_separate_results <-
  list()

stacked_models <-
  list()

stacked_coefficient_checks <-
  list()

wald_results <-
  list()

contrast_wald <- function(
  fit,
  restriction_matrix,
  restriction_labels
) {
  beta <-
    coef(
      fit
    )

  V <-
    as.matrix(
      vcov(
        fit
      )
    )

  R <-
    restriction_matrix[
      ,
      names(
        beta
      ),
      drop = FALSE
    ]

  rb <-
    as.numeric(
      R %*%
        beta
    )

  RVRT <-
    R %*%
      V %*%
      t(
        R
      )

  if (
    qr(
      RVRT
    )$rank !=
      nrow(
        RVRT
      )
  ) {
    stop(
      "Wald restriction covariance matrix is singular."
    )
  }

  statistic <-
    as.numeric(
      t(
        rb
      ) %*%
        solve(
          RVRT,
          rb
        )
    )

  df <-
    nrow(
      R
    )

  tibble(
    contrast =
      paste(
        restriction_labels,
        collapse =
          " AND "
      ),

    wald_chisq =
      statistic,

    df =
      df,

    p_value =
      pchisq(
        statistic,
        df =
          df,
        lower.tail =
          FALSE
      )
  )
}

make_zero_restriction <- function(
  coefficient_names,
  rows
) {
  R <-
    matrix(
      0,
      nrow =
        length(
          rows
        ),
      ncol =
        length(
          coefficient_names
        ),
      dimnames =
        list(
          NULL,
          coefficient_names
        )
    )

  for (
    i in
      seq_along(
        rows
      )
  ) {
    for (
      term_name in
        names(
          rows[[
            i
          ]]
        )
    ) {
      if (
        !term_name %in%
          coefficient_names
      ) {
        stop(
          "Restriction term absent from stacked model: ",
          term_name
        )
      }

      R[
        i,
        term_name
      ] <-
        rows[[
          i
        ]][[
          term_name
        ]]
    }
  }

  R
}

for (
  i in
    seq_len(
      nrow(
        cell_registry
      )
    )
) {
  spec <-
    cell_registry[
      i,
      ,
      drop = FALSE
    ]

  samples_i <-
    lapply(
      ideology_levels,
      function(
        ideology_name
      ) {
        native_samples[[
          paste(
            spec$cell_id,
            tolower(
              ideology_name
            ),
            sep =
              "__"
          )
        ]]
      }
    )

  names(
    samples_i
  ) <-
    ideology_levels

  common_ids <-
    Reduce(
      intersect,
      lapply(
        samples_i,
        function(
          dd
        ) {
          as.character(
            dd$ac_uid
          )
        }
      )
    )

  common_ids <-
    sort(
      unique(
        common_ids
      )
    )

  if (
    length(
      common_ids
    ) ==
      0L
  ) {
    stop(
      "No common Left/Center/Right ACs for ",
      spec$cell_id
    )
  }

  common_ids_by_cell[[
    spec$cell_id
  ]] <-
    common_ids

  common_parts <-
    list()

  for (
    ideology_name in
      ideology_levels
  ) {
    dd <-
      samples_i[[
        ideology_name
      ]] |>
      filter(
        as.character(
          ac_uid
        ) %in%
          common_ids
      )

    dd <-
      dd[
        match(
          common_ids,
          as.character(
            dd$ac_uid
          )
        ),
        ,
        drop = FALSE
      ]

    if (
      !identical(
        as.character(
          dd$ac_uid
        ),
        common_ids
      )
    ) {
      stop(
        "Common-sample ordering failure for ",
        spec$cell_id,
        " / ",
        ideology_name
      )
    }

    model_common <-
      fit_ac_model(
        dd
      )

    focal_common <-
      extract_fixest_term(
        model_common,
        c(
          "muslim",
          "fdi_current"
        )
      )

    common_key <-
      paste(
        spec$cell_id,
        tolower(
          ideology_name
        ),
        sep =
          "__"
      )

    common_separate_models[[
      common_key
    ]] <-
      model_common

    common_separate_results[[
      common_key
    ]] <-
      tibble(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        functional_form =
          spec$functional_form,

        ideology =
          ideology_name,

        sample_type =
          "Common Left-Center-Right",

        estimate =
          focal_common$estimate,

        std_error =
          focal_common$std_error,

        conf_low =
          focal_common$conf_low,

        conf_high =
          focal_common$conf_high,

        p_value =
          focal_common$p_value,

        n_ac =
          nrow(
            dd
          ),

        n_states =
          n_distinct(
            dd$state_no
          ),

        n_pc_clusters =
          n_distinct(
            dd$pc_cluster_id
          )
      )

    common_parts[[
      ideology_name
    ]] <-
      dd |>
      mutate(
        ideology_outcome =
          factor(
            ideology_name,
            levels =
              ideology_levels
          )
      )
  }

  stacked <-
    bind_rows(
      common_parts
    ) |>
    mutate(
      ideology_slug =
        case_when(
          as.character(
            ideology_outcome
          ) ==
            "Left" ~
            "left",

          as.character(
            ideology_outcome
          ) ==
            "Center" ~
            "center",

          as.character(
            ideology_outcome
          ) ==
            "Right" ~
            "right",

          TRUE ~
            NA_character_
        ),

      state_ideology_fe =
        interaction(
          state_no,
          ideology_outcome,
          drop =
            TRUE,
          lex.order =
            TRUE
        )
    )

  for (
    ideology_slug in
      c(
        "left",
        "center",
        "right"
      )
  ) {
    indicator <-
      as.numeric(
        stacked$ideology_slug ==
          ideology_slug
      )

    stacked[[
      paste0(
        ideology_slug,
        "_muslim"
      )
    ]] <-
      indicator *
        stacked$muslim

    stacked[[
      paste0(
        ideology_slug,
        "_current"
      )
    ]] <-
      indicator *
        stacked$fdi_current

    stacked[[
      paste0(
        ideology_slug,
        "_baseline"
      )
    ]] <-
      indicator *
        stacked$fdi_baseline

    stacked[[
      paste0(
        ideology_slug,
        "_current_muslim"
      )
    ]] <-
      indicator *
        stacked$fdi_current *
        stacked$muslim

    stacked[[
      paste0(
        ideology_slug,
        "_baseline_muslim"
      )
    ]] <-
      indicator *
        stacked$fdi_baseline *
        stacked$muslim

    stacked[[
      paste0(
        ideology_slug,
        "_proxy_ac_pop"
      )
    ]] <-
      indicator *
        stacked$proxy_ac_pop

    stacked[[
      paste0(
        ideology_slug,
        "_sc_pop_share"
      )
    ]] <-
      indicator *
        stacked$sc_pop_share

    stacked[[
      paste0(
        ideology_slug,
        "_st_pop_share"
      )
    ]] <-
      indicator *
        stacked$st_pop_share
  }

  slope_terms <-
    unlist(
      lapply(
        c(
          "left",
          "center",
          "right"
        ),
        function(
          ideology_slug
        ) {
          paste0(
            ideology_slug,
            c(
              "_muslim",
              "_current",
              "_baseline",
              "_current_muslim",
              "_baseline_muslim",
              "_proxy_ac_pop",
              "_sc_pop_share",
              "_st_pop_share"
            )
          )
        }
      )
    )

  stacked_formula <-
    as.formula(
      paste0(
        "y ~ 0 + ",
        paste(
          slope_terms,
          collapse =
            " + "
        ),
        " | state_ideology_fe"
      )
    )

  stacked_fit <-
    feols(
      stacked_formula,
      data =
        stacked,
      vcov =
        ~ pc_cluster_id,
      warn =
        FALSE,
      notes =
        FALSE
    )

  stacked_models[[
    spec$cell_id
  ]] <-
    stacked_fit

  stacked_terms <-
    names(
      coef(
        stacked_fit
      )
    )

  focal_terms <-
    c(
      Left =
        "left_current_muslim",
      Center =
        "center_current_muslim",
      Right =
        "right_current_muslim"
    )

  if (
    !all(
      focal_terms %in%
        stacked_terms
    )
  ) {
    stop(
      "A focal ideology-specific interaction was dropped in stacked model ",
      spec$cell_id
    )
  }

  for (
    ideology_name in
      ideology_levels
  ) {
    separate_estimate <-
      common_separate_results[[
        paste(
          spec$cell_id,
          tolower(
            ideology_name
          ),
          sep =
            "__"
        )
      ]]$estimate

    stacked_estimate <-
      unname(
        coef(
          stacked_fit
        )[
          focal_terms[[
            ideology_name
          ]]
        ]
      )

    stacked_coefficient_checks[[
      paste(
        spec$cell_id,
        ideology_name,
        sep =
          "__"
      )
    ]] <-
      tibble(
        cell_id =
          spec$cell_id,

        ideology =
          ideology_name,

        separate_common_estimate =
          separate_estimate,

        stacked_estimate =
          stacked_estimate,

        absolute_difference =
          abs(
            separate_estimate -
              stacked_estimate
          )
      )
  }

  coef_names <-
    names(
      coef(
        stacked_fit
      )
    )

  center_left_R <-
    make_zero_restriction(
      coef_names,
      list(
        c(
          center_current_muslim =
            1,
          left_current_muslim =
            -1
        )
      )
    )

  center_right_R <-
    make_zero_restriction(
      coef_names,
      list(
        c(
          center_current_muslim =
            1,
          right_current_muslim =
            -1
        )
      )
    )

  left_right_R <-
    make_zero_restriction(
      coef_names,
      list(
        c(
          left_current_muslim =
            1,
          right_current_muslim =
            -1
        )
      )
    )

  omnibus_R <-
    make_zero_restriction(
      coef_names,
      list(
        c(
          center_current_muslim =
            1,
          left_current_muslim =
            -1
        ),
        c(
          center_current_muslim =
            1,
          right_current_muslim =
            -1
        )
      )
    )

  contrast_specs <-
    list(
      center_vs_left =
        list(
          R =
            center_left_R,
          labels =
            "Center = Left"
        ),

      center_vs_right =
        list(
          R =
            center_right_R,
          labels =
            "Center = Right"
        ),

      left_vs_right =
        list(
          R =
            left_right_R,
          labels =
            "Left = Right"
        ),

      omnibus =
        list(
          R =
            omnibus_R,
          labels =
            c(
              "Center = Left",
              "Center = Right"
            )
        )
    )

  for (
    contrast_id in
      names(
        contrast_specs
      )
  ) {
    test_i <-
      contrast_wald(
        stacked_fit,
        contrast_specs[[
          contrast_id
        ]]$R,
        contrast_specs[[
          contrast_id
        ]]$labels
      )

    wald_results[[
      paste(
        spec$cell_id,
        contrast_id,
        sep =
          "__"
      )
    ]] <-
      test_i |>
      mutate(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        functional_form =
          spec$functional_form,

        contrast_id =
          contrast_id,

        n_common_ac =
          length(
            common_ids
          ),

        n_stacked_rows =
          nrow(
            stacked
          ),

        n_states =
          n_distinct(
            stacked$state_no
          ),

        n_pc_clusters =
          n_distinct(
            stacked$pc_cluster_id
          ),

        .before =
          1
      )
  }

  common_samples_long[[
    spec$cell_id
  ]] <-
    stacked |>
    select(
      ac_uid,
      ideology_outcome,
      y,
      state_no,
      pc_cluster_id,
      muslim,
      fdi_current,
      fdi_baseline,
      all_of(
        primary_controls
      )
    )
}

common_separate_results <-
  bind_rows(
    common_separate_results
  )

stacked_coefficient_checks <-
  bind_rows(
    stacked_coefficient_checks
  )

wald_results <-
  bind_rows(
    wald_results
  )

if (
  any(
    stacked_coefficient_checks$absolute_difference >
      1e-8
  )
) {
  print(
    stacked_coefficient_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "Stacked common-sample coefficients fail to reproduce separate common-sample coefficients."
  )
}

common_sample_counts <-
  tibble(
    cell_id =
      names(
        common_ids_by_cell
      ),

    common_n_ac =
      vapply(
        common_ids_by_cell,
        length,
        integer(1)
      )
  ) |>
  left_join(
    cell_registry |>
      select(
        cell_id,
        sector,
        functional_form
      ),
    by =
      "cell_id"
  ) |>
  select(
    cell_id,
    sector,
    functional_form,
    common_n_ac
  )

sample_counts <-
  sample_counts |>
  left_join(
    common_sample_counts,
    by =
      c(
        "cell_id",
        "sector",
        "functional_form"
      )
  )

common_membership <-
  map_dfr(
    names(
      common_ids_by_cell
    ),
    function(
      cell_id
    ) {
      tibble(
        cell_id =
          cell_id,

        ac_uid =
          common_ids_by_cell[[
            cell_id
          ]]
      )
    }
  )

appendix_table <-
  common_separate_results |>
  select(
    cell_id,
    sector,
    functional_form,
    ideology,
    estimate,
    std_error,
    conf_low,
    conf_high,
    p_value,
    n_ac
  ) |>
  pivot_wider(
    names_from =
      ideology,
    values_from =
      c(
        estimate,
        std_error,
        conf_low,
        conf_high,
        p_value,
        n_ac
      ),
    names_glue =
      "{ideology}_{.value}"
  ) |>
  left_join(
    wald_results |>
      select(
        cell_id,
        contrast_id,
        wald_chisq,
        df,
        p_value
      ) |>
      pivot_wider(
        names_from =
          contrast_id,
        values_from =
          c(
            wald_chisq,
            df,
            p_value
          ),
        names_glue =
          "{contrast_id}_{.value}"
      ),
    by =
      "cell_id"
  )

write_csv(
  sample_counts,
  file.path(
    output_dir,
    "01_native_and_common_sample_counts.csv"
  )
)

write_csv(
  native_results,
  file.path(
    output_dir,
    "02_native_ideology_coefficients.csv"
  )
)

write_csv(
  common_separate_results,
  file.path(
    output_dir,
    "03_common_sample_ideology_coefficients.csv"
  )
)

write_csv(
  wald_results,
  file.path(
    output_dir,
    "04_ideology_coefficient_wald_tests.csv"
  )
)

write_csv(
  appendix_table,
  file.path(
    output_dir,
    "05_appendix_ideology_heterogeneity_table.csv"
  )
)

write_csv(
  common_membership,
  file.path(
    output_dir,
    "06_common_sample_membership.csv"
  )
)

write_csv(
  reproduction_checks,
  file.path(
    output_dir,
    "07_center_canonical_reproduction_checks.csv"
  )
)

write_csv(
  stacked_coefficient_checks,
  file.path(
    output_dir,
    "08_stacked_coefficient_reproduction_checks.csv"
  )
)

saveRDS(
  native_models,
  file.path(
    output_dir,
    "09_native_models.rds"
  )
)

saveRDS(
  common_separate_models,
  file.path(
    output_dir,
    "10_common_sample_separate_models.rds"
  )
)

saveRDS(
  stacked_models,
  file.path(
    output_dir,
    "11_stacked_wald_models.rds"
  )
)

saveRDS(
  common_samples_long,
  file.path(
    output_dir,
    "12_common_samples_long.rds"
  )
)

notes <-
  c(
    "POST-PRIMARY AC IDEOLOGY-OUTCOME HETEROGENEITY DIAGNOSTIC",
    "",
    "Purpose:",
    "Replace the 2014 AC-level BJP share among Center respondents with the corresponding Left and Right BJP shares while preserving the canonical FDI, Muslim-share, control, fixed-effect, candidate-availability, and clustering architecture.",
    "",
    "Exposure cells:",
    "Total raw per100k",
    "Total log1p(per100k)",
    "Manufacturing raw per100k",
    "Manufacturing log1p(per100k)",
    "",
    "Native regressions:",
    "Each ideology uses every AC for which its own outcome and required regressors are estimable.",
    "",
    "Wald comparison regressions:",
    "Center, Left, and Right are compared only on the intersection of ACs where all three outcomes are estimable.",
    "The stacked model fully allows ideology-specific slopes for Muslim share, current FDI, baseline FDI, both FDI x Muslim interactions, and all primary controls.",
    "State fixed effects are also ideology-specific through state-by-ideology fixed effects.",
    "PC-clustered inference is retained, with rows from the same AC/PC cluster kept in the same variance cluster.",
    "",
    "Primary coefficient-equality tests:",
    "H0: Center current-FDI x Muslim coefficient = Left coefficient.",
    "H0: Center current-FDI x Muslim coefficient = Right coefficient.",
    "Omnibus H0: Center = Left = Right.",
    "",
    "No ideology-cell minimum-N threshold is imposed.",
    "These are post-primary heterogeneity diagnostics and must not be described as preregistered."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "13_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "14_session_info.txt"
  )
)

cat(
  "\n===== CENTER CANONICAL REPRODUCTION =====\n"
)

print(
  reproduction_checks,
  n = Inf,
  width = Inf
)

cat(
  "\n===== NATIVE AND COMMON SAMPLE COUNTS =====\n"
)

print(
  sample_counts,
  n = Inf,
  width = Inf
)

cat(
  "\n===== NATIVE IDEOLOGY-SPECIFIC COEFFICIENTS =====\n"
)

print(
  native_results |>
    select(
      sector,
      functional_form,
      ideology,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value,
      n_ac,
      n_states,
      n_pc_clusters
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== COMMON-SAMPLE IDEOLOGY-SPECIFIC COEFFICIENTS =====\n"
)

print(
  common_separate_results |>
    select(
      sector,
      functional_form,
      ideology,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value,
      n_ac
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== IDEOLOGY COEFFICIENT WALD TESTS =====\n"
)

print(
  wald_results |>
    select(
      sector,
      functional_form,
      contrast_id,
      wald_chisq,
      df,
      p_value,
      n_common_ac,
      n_states,
      n_pc_clusters
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== STACKED COEFFICIENT REPRODUCTION =====\n"
)

print(
  stacked_coefficient_checks,
  n = Inf,
  width = Inf
)

cat(
  "\nAC_IDEOLOGY_OUTCOME_HETEROGENEITY_COMPLETE\n"
)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(fixest)
})

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "tibble",
  "fixest"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(project_root)

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
    "ac_canonical_v1_0"
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

ac_year_path <-
  file.path(
    input_dir,
    "ac_year.rds"
  )

for (path in c(
  ideology_path,
  change_path,
  ac_year_path
)) {
  if (!file.exists(path)) {
    stop(
      "Required input missing: ",
      path
    )
  }
}

ideology <-
  readRDS(
    ideology_path
  )

ac_change <-
  readRDS(
    change_path
  )

ac_year <-
  readRDS(
    ac_year_path
  )

require_columns <- function(
  data,
  columns,
  label
) {
  missing <-
    setdiff(
      columns,
      names(data)
    )

  if (length(missing) > 0L) {
    stop(
      label,
      " is missing required columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
}

primary_controls <-
  c(
    "proxy_ac_pop",
    "sc_pop_share",
    "st_pop_share"
  )

expanded_controls <-
  c(
    primary_controls,
    "employment_per_total_population",
    "ed_sec_share"
  )

common_required <-
  c(
    "ac_uid",
    "state_no",
    "pc_cluster_id",
    "bjp_candidate_present",
    "fdi_spatial_support",
    "muslim_share_2001_dist_proxy",
    primary_controls
  )

require_columns(
  ideology,
  c(
    common_required,
    "year",
    "ideology",
    "weighted_share_voted_bjp",
    "share_voted_bjp",
    "weighted_share_ac_ideology_complete",
    "n_ac_ideology_complete"
  ),
  "ac_year_ideology_summary"
)

require_columns(
  ac_year,
  c(
    common_required,
    "year",
    "bjp_vote_share"
  ),
  "ac_year"
)

fdi_variables <-
  c(
    "fdi_total_local_all_pc100k_2009",
    "fdi_total_local_all_pc100k_2014",
    "log1p_fdi_total_local_all_pc100k_2009",
    "log1p_fdi_total_local_all_pc100k_2014",
    "fdi_total_own_all_pc100k_2009",
    "fdi_total_own_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2009",
    "fdi_mfg_local_all_pc100k_2014",
    "fdi_services_local_all_pc100k_2009",
    "fdi_services_local_all_pc100k_2014",
    "d_fdi_total_local_21m_pc100k",
    "fdi_total_local_early21_pc100k",
    "d_fdi_mfg_local_21m_pc100k",
    "fdi_mfg_local_early21_pc100k",
    "d_fdi_services_local_21m_pc100k",
    "fdi_services_local_early21_pc100k"
  )

require_columns(
  ac_change,
  c(
    "ac_uid",
    fdi_variables
  ),
  "ac_change"
)

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
  ) > 0L
) {
  stop(
    "ac_change contains duplicated ac_uid values."
  )
}

center14 <-
  ideology |>
  filter(
    year == 2014,
    as.character(
      ideology
    ) == "Center"
  ) |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_source,
    by = "ac_uid",
    relationship = "one-to-one"
  )

center09 <-
  ideology |>
  filter(
    year == 2009,
    as.character(
      ideology
    ) == "Center"
  ) |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_source,
    by = "ac_uid",
    relationship = "one-to-one"
  )

if (
  anyDuplicated(
    center14$ac_uid
  ) > 0L
) {
  stop(
    "2014 Center ideology cells are not unique by AC."
  )
}

if (
  anyDuplicated(
    center09$ac_uid
  ) > 0L
) {
  stop(
    "2009 Center ideology cells are not unique by AC."
  )
}

primary_funnel <-
  local({
    s0 <- center14

    s1 <-
      s0 |>
      filter(
        !is.na(
          weighted_share_voted_bjp
        )
      )

    s2 <-
      s1 |>
      filter(
        bjp_candidate_present %in%
          TRUE
      )

    s3 <-
      s2 |>
      filter(
        fdi_spatial_support %in%
          TRUE
      )

    s4 <-
      s3 |>
      filter(
        !is.na(
          fdi_total_local_all_pc100k_2009
        ),
        !is.na(
          fdi_total_local_all_pc100k_2014
        )
      )

    s5 <-
      s4 |>
      filter(
        !is.na(
          muslim_share_2001_dist_proxy
        )
      )

    s6 <-
      s5 |>
      filter(
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

    s7 <-
      s6 |>
      filter(
        !is.na(
          employment_per_total_population
        ),
        !is.na(
          ed_sec_share
        )
      )

    tibble(
      step =
        c(
          "Center 2014 cells",
          "Valid weighted BJP-share outcome",
          "BJP candidate present",
          "FDI spatial support",
          "Current + baseline FDI",
          "2001 Muslim share",
          "Primary controls + FE/cluster IDs",
          "Expanded controls"
        ),
      n =
        c(
          nrow(s0),
          nrow(s1),
          nrow(s2),
          nrow(s3),
          nrow(s4),
          nrow(s5),
          nrow(s6),
          nrow(s7)
        )
    )
  })

write_csv(
  primary_funnel,
  file.path(
    output_dir,
    "01_primary_sample_funnel.csv"
  )
)

if (
  primary_funnel$n[
    primary_funnel$step ==
      "Primary controls + FE/cluster IDs"
  ] != 224L
) {
  stop(
    "Frozen primary sample is no longer 224 ACs."
  )
}

if (
  primary_funnel$n[
    primary_funnel$step ==
      "Expanded controls"
  ] != 154L
) {
  stop(
    "Frozen expanded-control sample is no longer 154 ACs."
  )
}

specifications <-
  tribble(
    ~model_id, ~role, ~family, ~sector, ~geography, ~functional_form, ~current_var, ~baseline_var, ~control_set,
    "AC01", "Primary", "60-month levels", "Total", "Local", "Raw", "fdi_total_local_all_pc100k_2014", "fdi_total_local_all_pc100k_2009", "Primary",
    "AC02", "Expanded controls", "60-month levels", "Total", "Local", "Raw", "fdi_total_local_all_pc100k_2014", "fdi_total_local_all_pc100k_2009", "Expanded",
    "AC03", "Functional-form robustness", "60-month levels", "Total", "Local", "log1p", "log1p_fdi_total_local_all_pc100k_2014", "log1p_fdi_total_local_all_pc100k_2009", "Primary",
    "AC04", "Spatial robustness", "60-month levels", "Total", "Own AC", "Raw", "fdi_total_own_all_pc100k_2014", "fdi_total_own_all_pc100k_2009", "Primary",
    "AC05", "Sector comparison", "60-month levels", "Manufacturing", "Local", "Raw", "fdi_mfg_local_all_pc100k_2014", "fdi_mfg_local_all_pc100k_2009", "Primary",
    "AC06", "Sector comparison", "60-month levels", "Services", "Local", "Raw", "fdi_services_local_all_pc100k_2014", "fdi_services_local_all_pc100k_2009", "Primary",
    "AC07", "Alternative treatment robustness", "21-month change", "Total", "Local", "Raw", "d_fdi_total_local_21m_pc100k", "fdi_total_local_early21_pc100k", "Primary",
    "AC08", "Alternative treatment robustness", "21-month change", "Manufacturing", "Local", "Raw", "d_fdi_mfg_local_21m_pc100k", "fdi_mfg_local_early21_pc100k", "Primary",
    "AC09", "Alternative treatment robustness", "21-month change", "Services", "Local", "Raw", "d_fdi_services_local_21m_pc100k", "fdi_services_local_early21_pc100k", "Primary"
  )

make_model_sample <- function(
  data,
  current_var,
  baseline_var,
  controls
) {
  data |>
    mutate(
      y =
        weighted_share_voted_bjp,
      muslim =
        muslim_share_2001_dist_proxy,
      fdi_current =
        .data[[current_var]],
      fdi_baseline =
        .data[[baseline_var]]
    ) |>
    filter(
      !is.na(
        y
      ),
      bjp_candidate_present %in%
        TRUE,
      fdi_spatial_support %in%
        TRUE,
      !is.na(
        muslim
      ),
      !is.na(
        fdi_current
      ),
      !is.na(
        fdi_baseline
      ),
      if_all(
        all_of(
          controls
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

make_level_formula <- function(
  controls
) {
  rhs <-
    paste(
      c(
        "muslim * fdi_current",
        "muslim * fdi_baseline",
        controls
      ),
      collapse = " + "
    )

  as.formula(
    paste0(
      "y ~ ",
      rhs,
      " | state_no"
    )
  )
}

models <- list()
model_samples <- list()
model_metadata <- list()

for (i in seq_len(
  nrow(
    specifications
  )
)) {
  spec <-
    specifications[
      i,
      ,
      drop = FALSE
    ]

  controls <-
    if (
      spec$control_set ==
        "Expanded"
    ) {
      expanded_controls
    } else {
      primary_controls
    }

  sample_i <-
    make_model_sample(
      center14,
      spec$current_var,
      spec$baseline_var,
      controls
    )

  if (
    spec$model_id ==
      "AC01" &&
      nrow(
        sample_i
      ) != 224L
  ) {
    stop(
      "AC01 primary model sample is not 224."
    )
  }

  if (
    spec$model_id ==
      "AC02" &&
      nrow(
        sample_i
      ) != 154L
  ) {
    stop(
      "AC02 expanded model sample is not 154."
    )
  }

  fml <-
    make_level_formula(
      controls
    )

  model_i <-
    feols(
      fml,
      data =
        sample_i,
      vcov =
        ~ pc_cluster_id,
      warn = TRUE,
      notes = TRUE
    )

  models[[spec$model_id]] <-
    model_i

  model_samples[[spec$model_id]] <-
    sample_i

  model_metadata[[spec$model_id]] <-
    tibble(
      model_id =
        spec$model_id,
      role =
        spec$role,
      family =
        spec$family,
      sector =
        spec$sector,
      geography =
        spec$geography,
      functional_form =
        spec$functional_form,
      current_var =
        spec$current_var,
      baseline_var =
        spec$baseline_var,
      control_set =
        spec$control_set,
      outcome =
        "weighted_share_voted_bjp",
      moderator =
        "muslim_share_2001_dist_proxy",
      fixed_effect =
        "state_no",
      vcov =
        "PC clustered: pc_cluster_id",
      n =
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
}

sample09 <-
  center09 |>
  mutate(
    y =
      weighted_share_voted_bjp,
    muslim =
      muslim_share_2001_dist_proxy,
    fdi_current =
      fdi_total_local_all_pc100k_2009
  ) |>
  filter(
    !is.na(
      y
    ),
    bjp_candidate_present %in%
      TRUE,
    fdi_spatial_support %in%
      TRUE,
    !is.na(
      muslim
    ),
    !is.na(
      fdi_current
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

formula09 <-
  as.formula(
    paste0(
      "y ~ muslim * fdi_current + ",
      paste(
        primary_controls,
        collapse = " + "
      ),
      " | state_no"
    )
  )

models[["AC10"]] <-
  feols(
    formula09,
    data =
      sample09,
    vcov =
      ~ pc_cluster_id,
    warn = TRUE,
    notes = TRUE
  )

model_samples[["AC10"]] <-
  sample09

model_metadata[["AC10"]] <-
  tibble(
    model_id =
      "AC10",
    role =
      "2009 baseline-period comparison",
    family =
      "2009 comparison",
    sector =
      "Total",
    geography =
      "Local",
    functional_form =
      "Raw",
    current_var =
      "fdi_total_local_all_pc100k_2009",
    baseline_var =
      NA_character_,
    control_set =
      "Primary",
    outcome =
      "weighted_share_voted_bjp",
    moderator =
      "muslim_share_2001_dist_proxy",
    fixed_effect =
      "state_no",
    vcov =
      "PC clustered: pc_cluster_id",
    n =
      nrow(
        sample09
      ),
    n_states =
      n_distinct(
        sample09$state_no
      ),
    n_pc_clusters =
      n_distinct(
        sample09$pc_cluster_id
      )
  )

first_nonmissing <- function(x) {
  y <-
    x[
      !is.na(
        x
      )
    ]

  if (
    length(
      y
    ) == 0L
  ) {
    return(
      NA_real_
    )
  }

  y[[1]]
}

centrist_context09 <-
  ideology |>
  filter(
    year == 2009
  ) |>
  group_by(
    ac_uid
  ) |>
  summarise(
    nes_weighted_share_center_among_ideology_complete_2009 =
      if (
        any(
          !is.na(
            weighted_share_ac_ideology_complete
          )
        )
      ) {
        sum(
          weighted_share_ac_ideology_complete[
            as.character(
              ideology
            ) == "Center"
          ],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    nes_n_ideology_complete_2009 =
      first_nonmissing(
        n_ac_ideology_complete
      ),
    .groups = "drop"
  )

if (
  any(
    centrist_context09$nes_weighted_share_center_among_ideology_complete_2009 <
      -1e-10,
    na.rm = TRUE
  ) ||
    any(
      centrist_context09$nes_weighted_share_center_among_ideology_complete_2009 >
        1 + 1e-10,
      na.rm = TRUE
    )
) {
  stop(
    "Constructed 2009 centrist-context share falls outside [0,1]."
  )
}

ac14 <-
  ac_year |>
  filter(
    year == 2014
  ) |>
  select(
    -any_of(
      fdi_variables
    )
  ) |>
  left_join(
    fdi_source,
    by = "ac_uid",
    relationship = "one-to-one"
  ) |>
  left_join(
    centrist_context09,
    by = "ac_uid",
    relationship = "one-to-one"
  )

if (
  anyDuplicated(
    ac14$ac_uid
  ) > 0L
) {
  stop(
    "2014 AC-year data are not unique by AC."
  )
}

max_official_vote <-
  max(
    ac14$bjp_vote_share,
    na.rm = TRUE
  )

if (
  max_official_vote <=
    1.000001
) {
  official_vote_scale <-
    "proportion"

  ac14 <-
    ac14 |>
    mutate(
      official_y =
        bjp_vote_share
    )
} else if (
  max_official_vote <=
    100.000001
) {
  official_vote_scale <-
    "percent converted to proportion"

  ac14 <-
    ac14 |>
    mutate(
      official_y =
        bjp_vote_share /
        100
    )
} else {
  stop(
    "bjp_vote_share has an unexpected scale."
  )
}

triple_sample <-
  ac14 |>
  mutate(
    muslim =
      muslim_share_2001_dist_proxy,
    centrist_share_2009 =
      nes_weighted_share_center_among_ideology_complete_2009,
    fdi_current =
      fdi_total_local_all_pc100k_2014,
    fdi_baseline =
      fdi_total_local_all_pc100k_2009
  ) |>
  filter(
    !is.na(
      official_y
    ),
    bjp_candidate_present %in%
      TRUE,
    fdi_spatial_support %in%
      TRUE,
    !is.na(
      muslim
    ),
    !is.na(
      centrist_share_2009
    ),
    !is.na(
      fdi_current
    ),
    !is.na(
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

triple_formula <-
  as.formula(
    paste0(
      "official_y ~ ",
      "fdi_current * muslim * centrist_share_2009 + ",
      "muslim * fdi_baseline + ",
      paste(
        primary_controls,
        collapse = " + "
      ),
      " | state_no"
    )
  )

models[["AC11"]] <-
  feols(
    triple_formula,
    data =
      triple_sample,
    vcov =
      ~ pc_cluster_id,
    warn = TRUE,
    notes = TRUE
  )

model_samples[["AC11"]] <-
  triple_sample

model_metadata[["AC11"]] <-
  tibble(
    model_id =
      "AC11",
    role =
      "Contextual triple-interaction robustness",
    family =
      "Official 2014 BJP vote-share triple interaction",
    sector =
      "Total",
    geography =
      "Local",
    functional_form =
      "Raw",
    current_var =
      "fdi_total_local_all_pc100k_2014",
    baseline_var =
      "fdi_total_local_all_pc100k_2009",
    control_set =
      "Primary",
    outcome =
      paste0(
        "bjp_vote_share; ",
        official_vote_scale
      ),
    moderator =
      "Muslim share x 2009 centrist-context share",
    fixed_effect =
      "state_no",
    vcov =
      "PC clustered: pc_cluster_id",
    n =
      nrow(
        triple_sample
      ),
    n_states =
      n_distinct(
        triple_sample$state_no
      ),
    n_pc_clusters =
      n_distinct(
        triple_sample$pc_cluster_id
      )
  )

primary_sample <-
  model_samples[["AC01"]]

models[["AC12"]] <-
  feols(
    make_level_formula(
      primary_controls
    ),
    data =
      primary_sample,
    vcov =
      ~ state_no,
    warn = TRUE,
    notes = TRUE
  )

model_samples[["AC12"]] <-
  primary_sample

model_metadata[["AC12"]] <-
  tibble(
    model_id =
      "AC12",
    role =
      "Inference sensitivity",
    family =
      "60-month levels",
    sector =
      "Total",
    geography =
      "Local",
    functional_form =
      "Raw",
    current_var =
      "fdi_total_local_all_pc100k_2014",
    baseline_var =
      "fdi_total_local_all_pc100k_2009",
    control_set =
      "Primary",
    outcome =
      "weighted_share_voted_bjp",
    moderator =
      "muslim_share_2001_dist_proxy",
    fixed_effect =
      "state_no",
    vcov =
      "State clustered: state_no",
    n =
      nrow(
        primary_sample
      ),
    n_states =
      n_distinct(
        primary_sample$state_no
      ),
    n_pc_clusters =
      n_distinct(
        primary_sample$pc_cluster_id
      )
  )

model_metadata <-
  bind_rows(
    model_metadata
  )

tidy_fixest <- function(
  model,
  model_id
) {
  ct <-
    as.data.frame(
      fixest::coeftable(
        model
      )
    )

  ct$term <-
    rownames(
      ct
    )

  rownames(
    ct
  ) <-
    NULL

  ci <-
    as.data.frame(
      stats::confint(
        model,
        level = 0.95
      )
    )

  ci$term <-
    rownames(
      ci
    )

  rownames(
    ci
  ) <-
    NULL

  tibble(
    model_id =
      model_id,
    term =
      ct$term,
    estimate =
      ct[[1]],
    std_error =
      ct[[2]],
    statistic =
      ct[[3]],
    p_value =
      ct[[4]]
  ) |>
    left_join(
      tibble(
        term =
          ci$term,
        conf_low =
          ci[[1]],
        conf_high =
          ci[[2]]
      ),
      by = "term",
      relationship = "one-to-one"
    )
}

coefficients <-
  imap_dfr(
    models,
    tidy_fixest
  ) |>
  left_join(
    model_metadata |>
      select(
        model_id,
        role,
        sector,
        geography,
        functional_form,
        control_set,
        n,
        vcov
      ),
    by = "model_id",
    relationship = "many-to-one"
  )

write_csv(
  coefficients,
  file.path(
    output_dir,
    "02_all_model_coefficients.csv"
  )
)

write_csv(
  model_metadata,
  file.path(
    output_dir,
    "03_model_manifest_and_sample_sizes.csv"
  )
)

find_interaction_term <- function(
  coefficient_names,
  variables
) {
  candidates <-
    coefficient_names[
      vapply(
        coefficient_names,
        function(term) {
          pieces <-
            strsplit(
              term,
              ":",
              fixed = TRUE
            )[[1]]

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
      candidates
    ) != 1L
  ) {
    stop(
      "Could not uniquely locate interaction among: ",
      paste(
        variables,
        collapse = " x "
      ),
      ". Candidates: ",
      paste(
        candidates,
        collapse = ", "
      )
    )
  }

  candidates[[1]]
}

focal_rows <- list()

for (
  model_id in
    c(
      "AC01",
      "AC02",
      "AC03",
      "AC04",
      "AC05",
      "AC06",
      "AC07",
      "AC08",
      "AC09",
      "AC10",
      "AC12"
    )
) {
  model_i <-
    models[[model_id]]

  term_i <-
    find_interaction_term(
      names(
        coef(
          model_i
        )
      ),
      c(
        "muslim",
        "fdi_current"
      )
    )

  focal_rows[[model_id]] <-
    coefficients |>
    filter(
      model_id ==
        !!model_id,
      term ==
        term_i
    )
}

triple_term <-
  find_interaction_term(
    names(
      coef(
        models[["AC11"]]
      )
    ),
    c(
      "fdi_current",
      "muslim",
      "centrist_share_2009"
    )
  )

focal_rows[["AC11"]] <-
  coefficients |>
  filter(
    model_id ==
      "AC11",
    term ==
      triple_term
  )

focal_coefficients <-
  bind_rows(
    focal_rows
  ) |>
  arrange(
    model_id
  )

write_csv(
  focal_coefficients,
  file.path(
    output_dir,
    "04_focal_interaction_coefficients.csv"
  )
)

primary_model <-
  models[["AC01"]]

primary_sample <-
  model_samples[["AC01"]] |>
  mutate(
    fdi_delta_60m =
      fdi_current -
      fdi_baseline
  )

primary_reparameterized <-
  feols(
    y ~
      muslim * fdi_delta_60m +
      muslim * fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share |
      state_no,
    data =
      primary_sample,
    vcov =
      ~ pc_cluster_id,
    warn =
      FALSE,
    notes =
      FALSE
  )

original_current_term <-
  find_interaction_term(
    names(
      coef(
        primary_model
      )
    ),
    c(
      "muslim",
      "fdi_current"
    )
  )

original_baseline_term <-
  find_interaction_term(
    names(
      coef(
        primary_model
      )
    ),
    c(
      "muslim",
      "fdi_baseline"
    )
  )

reparam_delta_term <-
  find_interaction_term(
    names(
      coef(
        primary_reparameterized
      )
    ),
    c(
      "muslim",
      "fdi_delta_60m"
    )
  )

reparam_baseline_term <-
  find_interaction_term(
    names(
      coef(
        primary_reparameterized
      )
    ),
    c(
      "muslim",
      "fdi_baseline"
    )
  )

extract_term_row <- function(
  model,
  term,
  parameterization,
  interpretation
) {
  ct <-
    coeftable(
      model
    )

  if (
    !term %in%
      rownames(
        ct
      )
  ) {
    stop(
      "Requested term not found: ",
      term
    )
  }

  tibble(
    parameterization =
      parameterization,
    term =
      term,
    interpretation =
      interpretation,
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
        ct[
          term,
          1
        ]
      ) -
      1.96 *
      unname(
        ct[
          term,
          2
        ]
      ),
    conf_high =
      unname(
        ct[
          term,
          1
        ]
      ) +
      1.96 *
      unname(
        ct[
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

max_fitted_difference <-
  max(
    abs(
      fitted(
        primary_model
      ) -
      fitted(
        primary_reparameterized
      )
    )
  )

if (
  !is.finite(
    max_fitted_difference
  ) ||
    max_fitted_difference >
      1e-8
) {
  stop(
    "AC01 exact reparameterization failed fitted-value equivalence check."
  )
}

original_focal_estimate <-
  unname(
    coef(
      primary_model
    )[
      original_current_term
    ]
  )

reparam_delta_estimate <-
  unname(
    coef(
      primary_reparameterized
    )[
      reparam_delta_term
    ]
  )

if (
  !isTRUE(
    all.equal(
      original_focal_estimate,
      reparam_delta_estimate,
      tolerance = 1e-10
    )
  )
) {
  stop(
    "AC01 change interaction is not algebraically identical to the original current-FDI interaction."
  )
}

current_baseline_correlation <-
  cor(
    primary_sample$fdi_current,
    primary_sample$fdi_baseline
  )

interaction_correlation <-
  cor(
    primary_sample$muslim *
      primary_sample$fdi_current,
    primary_sample$muslim *
      primary_sample$fdi_baseline
  )

reparameterization_diagnostic <-
  bind_rows(
    extract_term_row(
      primary_model,
      original_current_term,
      "Original AC01",
      "Muslim share x current 2009-2014 local total FDI per 100,000"
    ),
    extract_term_row(
      primary_model,
      original_baseline_term,
      "Original AC01",
      "Muslim share x baseline 2004-2009 local total FDI per 100,000"
    ),
    extract_term_row(
      primary_reparameterized,
      reparam_delta_term,
      "Exact AC01 reparameterization",
      "Muslim share x change in 60-month local total FDI: current minus baseline"
    ),
    extract_term_row(
      primary_reparameterized,
      reparam_baseline_term,
      "Exact AC01 reparameterization",
      "Muslim share x baseline FDI conditional on the 60-month FDI change"
    )
  ) |>
  mutate(
    n =
      nobs(
        primary_model
      ),
    current_baseline_correlation =
      current_baseline_correlation,
    interaction_correlation =
      interaction_correlation,
    max_fitted_difference =
      max_fitted_difference
  )

primary_focal_term <-
  original_current_term

primary_full_ct <-
  coeftable(
    primary_model
  )

primary_full_beta <-
  unname(
    primary_full_ct[
      primary_focal_term,
      1
    ]
  )

primary_full_se <-
  unname(
    primary_full_ct[
      primary_focal_term,
      2
    ]
  )

fit_primary_refit <- function(
  data
) {
  feols(
    y ~
      muslim * fdi_current +
      muslim * fdi_baseline +
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

extract_focal_refit <- function(
  model
) {
  term <-
    find_interaction_term(
      names(
        coef(
          model
        )
      ),
      c(
        "muslim",
        "fdi_current"
      )
    )

  ct <-
    coeftable(
      model
    )

  tibble(
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
    p_value =
      unname(
        ct[
          term,
          4
        ]
      )
  )
}

leave_one_ac <-
  map_dfr(
    seq_len(
      nrow(
        primary_sample
      )
    ),
    function(
      i
    ) {
      fit_i <-
        tryCatch(
          fit_primary_refit(
            primary_sample[
              -i,
              ,
              drop = FALSE
            ]
          ),
          error =
            function(
              e
            ) {
              NULL
            }
        )

      if (
        is.null(
          fit_i
        )
      ) {
        return(
          tibble(
            omitted_ac_uid =
              primary_sample$ac_uid[
                i
              ],
            omitted_state_no =
              primary_sample$state_no[
                i
              ],
            omitted_pc_cluster_id =
              primary_sample$pc_cluster_id[
                i
              ],
            omitted_outcome =
              primary_sample$y[
                i
              ],
            omitted_muslim_share =
              primary_sample$muslim[
                i
              ],
            omitted_current_fdi =
              primary_sample$fdi_current[
                i
              ],
            omitted_baseline_fdi =
              primary_sample$fdi_baseline[
                i
              ],
            estimate =
              NA_real_,
            std_error =
              NA_real_,
            p_value =
              NA_real_,
            delta_from_full =
              NA_real_,
            shift_in_full_se =
              NA_real_
          )
        )
      }

      est_i <-
        extract_focal_refit(
          fit_i
        )

      tibble(
        omitted_ac_uid =
          primary_sample$ac_uid[
            i
          ],
        omitted_state_no =
          primary_sample$state_no[
            i
          ],
        omitted_pc_cluster_id =
          primary_sample$pc_cluster_id[
            i
          ],
        omitted_outcome =
          primary_sample$y[
            i
          ],
        omitted_muslim_share =
          primary_sample$muslim[
            i
          ],
        omitted_current_fdi =
          primary_sample$fdi_current[
            i
          ],
        omitted_baseline_fdi =
          primary_sample$fdi_baseline[
            i
          ],
        estimate =
          est_i$estimate,
        std_error =
          est_i$std_error,
        p_value =
          est_i$p_value,
        delta_from_full =
          est_i$estimate -
          primary_full_beta,
        shift_in_full_se =
          (
            est_i$estimate -
              primary_full_beta
          ) /
          primary_full_se
      )
    }
  ) |>
  arrange(
    desc(
      abs(
        delta_from_full
      )
    )
  )

leave_one_state <-
  map_dfr(
    sort(
      unique(
        primary_sample$state_no
      )
    ),
    function(
      state_i
    ) {
      sample_i <-
        primary_sample |>
        filter(
          state_no !=
            state_i
        )

      fit_i <-
        tryCatch(
          fit_primary_refit(
            sample_i
          ),
          error =
            function(
              e
            ) {
              NULL
            }
        )

      if (
        is.null(
          fit_i
        )
      ) {
        return(
          tibble(
            omitted_state_no =
              state_i,
            omitted_n =
              sum(
                primary_sample$state_no ==
                  state_i
              ),
            estimate =
              NA_real_,
            std_error =
              NA_real_,
            p_value =
              NA_real_,
            delta_from_full =
              NA_real_,
            shift_in_full_se =
              NA_real_
          )
        )
      }

      est_i <-
        extract_focal_refit(
          fit_i
        )

      tibble(
        omitted_state_no =
          state_i,
        omitted_n =
          sum(
            primary_sample$state_no ==
              state_i
          ),
        estimate =
          est_i$estimate,
        std_error =
          est_i$std_error,
        p_value =
          est_i$p_value,
        delta_from_full =
          est_i$estimate -
          primary_full_beta,
        shift_in_full_se =
          (
            est_i$estimate -
              primary_full_beta
          ) /
          primary_full_se
      )
    }
  ) |>
  arrange(
    desc(
      abs(
        delta_from_full
      )
    )
  )

old_marginal_outputs <-
  file.path(
    output_dir,
    c(
      "05_marginal_effect_grid.csv",
      "06_marginal_effect_selected_quantiles.csv",
      "07_marginal_effect_sample_support.csv"
    )
  )

unlink(
  old_marginal_outputs[
    file.exists(
      old_marginal_outputs
    )
  ]
)

write_csv(
  reparameterization_diagnostic,
  file.path(
    output_dir,
    "05_primary_60m_change_reparameterization.csv"
  )
)

write_csv(
  leave_one_ac,
  file.path(
    output_dir,
    "06_primary_leave_one_ac_influence.csv"
  )
)

write_csv(
  leave_one_state,
  file.path(
    output_dir,
    "07_primary_leave_one_state_influence.csv"
  )
)

cat(
  "\n===== PRIMARY 60-MONTH REPARAMETERIZATION =====\n"
)

print(
  reparameterization_diagnostic,
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY LEAVE-ONE-AC INFLUENCE SUMMARY =====\n"
)

print(
  tibble(
    full_estimate =
      primary_full_beta,
    min_leave_one_ac_estimate =
      min(
        leave_one_ac$estimate,
        na.rm = TRUE
      ),
    max_leave_one_ac_estimate =
      max(
        leave_one_ac$estimate,
        na.rm = TRUE
      ),
    max_absolute_shift =
      max(
        abs(
          leave_one_ac$delta_from_full
        ),
        na.rm = TRUE
      ),
    max_shift_in_full_se =
      max(
        abs(
          leave_one_ac$shift_in_full_se
        ),
        na.rm = TRUE
      ),
    n_sign_reversals =
      sum(
        sign(
          leave_one_ac$estimate
        ) !=
          sign(
            primary_full_beta
          ),
        na.rm = TRUE
      ),
    failed_refits =
      sum(
        is.na(
          leave_one_ac$estimate
        )
      )
  ),
  width = Inf
)

cat(
  "\n===== FIVE MOST INFLUENTIAL ACs =====\n"
)

print(
  leave_one_ac |>
    slice_head(
      n = 5
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY LEAVE-ONE-STATE INFLUENCE =====\n"
)

print(
  leave_one_state,
  n = Inf,
  width = Inf
)

primary_audit_columns <-
  intersect(
    c(
      "ac_uid",
      "state",
      "state_no",
      "pc",
      "ac",
      "pc_cluster_id",
      "weighted_share_voted_bjp",
      "share_voted_bjp",
      "n_respondents",
      "weighted_n_respondents",
      "n_vote_valid",
      "weighted_n_vote_valid",
      "n_voted_bjp",
      "muslim_share_2001_dist_proxy",
      "fdi_total_local_all_pc100k_2009",
      "fdi_total_local_all_pc100k_2014",
      "proxy_ac_pop",
      "sc_pop_share",
      "st_pop_share",
      "employment_per_total_population",
      "ed_sec_share"
    ),
    names(
      primary_sample
    )
  )

write_csv(
  primary_sample |>
    select(
      all_of(
        primary_audit_columns
      )
    ),
  file.path(
    output_dir,
    "08_primary_regression_sample.csv"
  )
)

centrist_context_diagnostic <-
  centrist_context09 |>
  summarise(
    n_ac_estimable =
      sum(
        !is.na(
          nes_weighted_share_center_among_ideology_complete_2009
        )
      ),
    n_zero_center_share =
      sum(
        nes_weighted_share_center_among_ideology_complete_2009 ==
          0,
        na.rm = TRUE
      ),
    n_positive_center_share =
      sum(
        nes_weighted_share_center_among_ideology_complete_2009 >
          0,
        na.rm = TRUE
      ),
    n_min =
      min(
        nes_n_ideology_complete_2009,
        na.rm = TRUE
      ),
    n_q25 =
      as.numeric(
        quantile(
          nes_n_ideology_complete_2009,
          0.25,
          na.rm = TRUE
        )
      ),
    n_median =
      median(
        nes_n_ideology_complete_2009,
        na.rm = TRUE
      ),
    n_q75 =
      as.numeric(
        quantile(
          nes_n_ideology_complete_2009,
          0.75,
          na.rm = TRUE
        )
      ),
    n_max =
      max(
        nes_n_ideology_complete_2009,
        na.rm = TRUE
      )
  )

write_csv(
  centrist_context_diagnostic,
  file.path(
    output_dir,
    "09_contextual_centrist_measure_diagnostic.csv"
  )
)

write_csv(
  centrist_context09,
  file.path(
    output_dir,
    "10_contextual_centrist_measure_by_ac.csv"
  )
)

saveRDS(
  models,
  file.path(
    output_dir,
    "models.rds"
  )
)

saveRDS(
  model_samples,
  file.path(
    output_dir,
    "model_samples.rds"
  )
)

capture.output(
  sessionInfo(),
  file =
    file.path(
      output_dir,
      "session_info.txt"
    )
)

cat(
  "\n===== CANONICAL AC MODEL MANIFEST =====\n"
)

print(
  model_metadata |>
    select(
      model_id,
      role,
      sector,
      geography,
      functional_form,
      control_set,
      n,
      n_states,
      n_pc_clusters,
      vcov
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== FOCAL INTERACTION ESTIMATES =====\n"
)

print(
  focal_coefficients |>
    select(
      model_id,
      role,
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      p_value,
      n,
      vcov
    ),
  n = Inf,
  width = Inf
)

cat(
  "\n===== PRIMARY SAMPLE FUNNEL =====\n"
)

print(
  primary_funnel,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CONTEXTUAL CENTRIST MEASURE =====\n"
)

print(
  centrist_context_diagnostic,
  width = Inf
)

cat(
  "\nOutputs written to: ",
  output_dir,
  "\n",
  sep = ""
)

cat(
  "\nCANONICAL_AC_MODELS_COMPLETE\n"
)

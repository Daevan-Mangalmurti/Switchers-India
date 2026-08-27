suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(fixest)
})

# =============================================================================
# R38A3 v1.1
# Official 2014 BJP vote share x 2014 AC centrist share triple interaction
#
# PRIMARY ESTIMAND
#   DV: official BJP vote share in the 2014 assembly constituency
#
#   Contextual moderator:
#     share of 2014 IDEOLOGY-COMPLETE NES respondents in the AC
#     classified as Center
#
#     center_share_2014_ideology_complete =
#       number of 2014 ideology-complete Center respondents in AC
#       ---------------------------------------------------------
#       number of 2014 ideology-complete respondents in AC
#
#   NO minimum NES cell-size threshold.
#   The only requirement is n_ideology_complete_2014 > 0 so the share is defined.
#
# Conventional hierarchical triple interaction:
#   muslim * fdi_current * center_share_2014_ideology_complete
# + muslim * fdi_baseline * center_share_2014_ideology_complete
#
# Primary controls: AC population, SC share, ST share
# State fixed effects
# PC-clustered covariance
#
# Total and Manufacturing FDI are estimated separately.
# =============================================================================

project_root <- Sys.getenv("SWITCHERS_ROOT", unset = getwd())
setwd(project_root)

input_dir <- file.path(
  project_root, "data", "derived", "switchers_rewrite", "final"
)

output_dir <- file.path(
  project_root, "outputs",
  "r38a3_ac_2014_centrist_share_triple_v1_1"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

respondent_path <- file.path(
  input_dir,
  "nes_respondent_analysis.rds"
)

ac_year_path <- file.path(
  input_dir,
  "ac_year.rds"
)

change_path <- file.path(
  input_dir,
  "ac_change.rds"
)

required_files <- c(
  respondent_path,
  ac_year_path,
  change_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Missing required input(s): ",
    paste(missing_files, collapse = ", ")
  )
}

respondents <- readRDS(respondent_path)
ac_year <- readRDS(ac_year_path)
ac_change <- readRDS(change_path)

require_columns <- function(
  data,
  columns,
  label
) {
  missing <- setdiff(
    columns,
    names(data)
  )

  if (length(missing) > 0L) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
}

primary_controls <- c(
  "proxy_ac_pop",
  "sc_pop_share",
  "st_pop_share"
)

require_columns(
  respondents,
  c(
    "respondent_uid",
    "ac_uid",
    "year",
    "ideology_complete",
    "voter_ideology",
    "survey_weight_norm_year"
  ),
  "nes_respondent_analysis"
)

require_columns(
  ac_year,
  c(
    "ac_uid",
    "year",
    "bjp_vote_share",
    "state_no",
    "pc_cluster_id",
    "bjp_candidate_present",
    "fdi_spatial_support",
    "muslim_share_2001_dist_proxy",
    primary_controls
  ),
  "ac_year"
)

fdi_columns <- c(
  "fdi_total_local_all_pc100k_2009",
  "fdi_total_local_all_pc100k_2014",
  "fdi_mfg_local_all_pc100k_2009",
  "fdi_mfg_local_all_pc100k_2014"
)

require_columns(
  ac_change,
  c(
    "ac_uid",
    fdi_columns
  ),
  "ac_change"
)

# -----------------------------------------------------------------------------
# 1. Construct the requested 2014 Center share
#
# PRIMARY:
#   Unweighted share of IDEOLOGY-COMPLETE 2014 NES respondents in each AC
#   who are classified as Center.
#
#   Denominator:
#     respondents with ideology_complete == TRUE and nonmissing voter_ideology
#
#   Numerator:
#     those same respondents with voter_ideology == "Center"
#
# AUDIT:
#   A survey-weighted version is also constructed on the identical
#   ideology-complete denominator and saved for diagnostic comparison,
#   but it is NOT substituted for the requested primary variable.
#
# There is deliberately NO n >= 5, n >= 10, or other cell-size threshold.
# -----------------------------------------------------------------------------

respondents14_ideology_complete <- respondents |>
  filter(
    year == 2014,
    ideology_complete %in% TRUE,
    !is.na(voter_ideology)
  ) |>
  mutate(
    ideology_chr =
      as.character(voter_ideology),

    is_center =
      as.integer(
        ideology_chr == "Center"
      ),

    weight_2014 =
      as.numeric(
        survey_weight_norm_year
      ),

    valid_weight =
      is.finite(weight_2014) &
      weight_2014 > 0
  )

if (
  anyDuplicated(
    respondents14_ideology_complete$respondent_uid
  ) > 0L
) {
  stop(
    "2014 ideology-complete respondent data are not unique by respondent_uid."
  )
}

center14 <- respondents14_ideology_complete |>
  group_by(ac_uid) |>
  summarise(
    n_ideology_complete_2014 =
      n(),

    n_center_2014 =
      sum(is_center),

    center_share_2014_ideology_complete =
      n_center_2014 /
      n_ideology_complete_2014,

    n_valid_weight_ideology_complete_2014 =
      sum(valid_weight),

    center_share_2014_ideology_complete_weighted =
      if (
        sum(valid_weight) > 0L &&
        sum(weight_2014[valid_weight]) > 0
      ) {
        sum(
          weight_2014[valid_weight] *
            is_center[valid_weight]
        ) /
          sum(
            weight_2014[valid_weight]
          )
      } else {
        NA_real_
      },

    .groups = "drop"
  )

if (
  anyDuplicated(
    center14$ac_uid
  ) > 0L
) {
  stop(
    "2014 Center-share construction is not unique by ac_uid."
  )
}

if (
  any(
    center14$center_share_2014_ideology_complete <
      -1e-10 |
      center14$center_share_2014_ideology_complete >
      1 + 1e-10,
    na.rm = TRUE
  )
) {
  stop(
    "Constructed 2014 ideology-complete Center share falls outside [0,1]."
  )
}

if (
  any(
    center14$n_ideology_complete_2014 <= 0L
  )
) {
  stop(
    "At least one retained Center-share row has a nonpositive ideology-complete denominator."
  )
}

# -----------------------------------------------------------------------------
# 2. Build 2014 AC analysis frame
# -----------------------------------------------------------------------------

fdi_source <- ac_change |>
  select(
    ac_uid,
    all_of(fdi_columns)
  )

if (
  anyDuplicated(
    fdi_source$ac_uid
  ) > 0L
) {
  stop(
    "FDI source is not unique by ac_uid."
  )
}

ac14 <- ac_year |>
  filter(
    year == 2014
  ) |>
  select(
    -any_of(fdi_columns)
  ) |>
  left_join(
    fdi_source,
    by = "ac_uid",
    relationship = "one-to-one"
  ) |>
  left_join(
    center14,
    by = "ac_uid",
    relationship = "one-to-one"
  )

if (
  anyDuplicated(
    ac14$ac_uid
  ) > 0L
) {
  stop(
    "2014 AC analysis data are not unique by ac_uid."
  )
}

max_official_vote <- max(
  ac14$bjp_vote_share,
  na.rm = TRUE
)

if (
  max_official_vote <= 1.000001
) {

  ac14 <- ac14 |>
    mutate(
      official_y =
        as.numeric(
          bjp_vote_share
        )
    )

  official_vote_scale <-
    "proportion"

} else if (
  max_official_vote <= 100.000001
) {

  ac14 <- ac14 |>
    mutate(
      official_y =
        as.numeric(
          bjp_vote_share
        ) / 100
    )

  official_vote_scale <-
    "percent converted to proportion"

} else {

  stop(
    "bjp_vote_share has an unexpected scale."
  )
}

cell_registry <- tribble(
  ~cell_id,
  ~sector,
  ~current_col,
  ~baseline_col,

  "total_raw",
  "Total",
  "fdi_total_local_all_pc100k_2014",
  "fdi_total_local_all_pc100k_2009",

  "manufacturing_raw",
  "Manufacturing",
  "fdi_mfg_local_all_pc100k_2014",
  "fdi_mfg_local_all_pc100k_2009"
)

make_sample <- function(
  current_col,
  baseline_col
) {
  ac14 |>
    mutate(
      muslim =
        as.numeric(
          muslim_share_2001_dist_proxy
        ),

      fdi_current =
        as.numeric(
          .data[[current_col]]
        ),

      fdi_baseline =
        as.numeric(
          .data[[baseline_col]]
        )
    ) |>
    filter(
      !is.na(official_y),

      bjp_candidate_present %in%
        TRUE,

      fdi_spatial_support %in%
        TRUE,

      is.finite(muslim),
      is.finite(fdi_current),
      is.finite(fdi_baseline),

      is.finite(
        center_share_2014_ideology_complete
      ),

      n_ideology_complete_2014 >
        0,

      if_all(
        all_of(primary_controls),
        ~ !is.na(.x)
      ),

      !is.na(state_no),
      !is.na(pc_cluster_id)
    )
}

fit_model <- function(data) {
  feols(
    official_y ~
      muslim *
      fdi_current *
      center_share_2014_ideology_complete +

      muslim *
      fdi_baseline *
      center_share_2014_ideology_complete +

      proxy_ac_pop +
      sc_pop_share +
      st_pop_share |
      state_no,

    data = data,

    vcov =
      ~ pc_cluster_id,

    warn = FALSE,
    notes = FALSE
  )
}

find_exact_interaction_term <- function(
  coefficient_names,
  variables
) {
  hits <- coefficient_names[
    vapply(
      strsplit(
        coefficient_names,
        ":",
        fixed = TRUE
      ),
      function(pieces) {
        length(pieces) ==
          length(variables) &&
          setequal(
            pieces,
            variables
          )
      },
      logical(1)
    )
  ]

  if (
    length(hits) !=
      1L
  ) {
    stop(
      "Could not uniquely identify interaction among: ",
      paste(
        variables,
        collapse = ", "
      ),
      ". Hits: ",
      paste(
        hits,
        collapse = ", "
      )
    )
  }

  hits[[1]]
}

tidy_fixest <- function(
  fit,
  cell_id,
  sector
) {
  ct <- as.data.frame(
    coeftable(fit)
  )

  ci <- as.data.frame(
    confint(
      fit,
      level = .95
    )
  )

  tibble(
    cell_id = cell_id,
    sector = sector,
    term = rownames(ct),
    estimate = ct[[1]],
    std_error = ct[[2]],
    statistic = ct[[3]],
    p_value = ct[[4]],
    conf_low =
      ci[
        rownames(ct),
        1
      ],
    conf_high =
      ci[
        rownames(ct),
        2
      ]
  )
}

wald_linear_fixest <- function(
  fit,
  restrictions,
  labels,
  n_clusters
) {
  beta <- coef(fit)
  V <- as.matrix(
    vcov(fit)
  )

  R <- matrix(
    0,
    nrow =
      length(restrictions),
    ncol =
      length(beta),
    dimnames = list(
      labels,
      names(beta)
    )
  )

  for (
    i in
    seq_along(
      restrictions
    )
  ) {
    w <- restrictions[[i]]

    unknown <- setdiff(
      names(w),
      names(beta)
    )

    if (
      length(unknown) >
        0L
    ) {
      stop(
        "Unknown coefficient(s) in Wald restriction: ",
        paste(
          unknown,
          collapse = ", "
        )
      )
    }

    R[
      i,
      names(w)
    ] <-
      as.numeric(w)
  }

  d <- as.numeric(
    R %*%
      beta
  )

  S <- R %*%
    V %*%
    t(R)

  S <- (
    S +
      t(S)
  ) /
    2

  q <- nrow(R)

  if (
    q ==
      1L
  ) {
    variance <- max(
      as.numeric(
        S[
          1,
          1
        ]
      ),
      0
    )

    se <- sqrt(
      variance
    )

    z <- d[[1]] /
      se

    chisq <- z^2

    df2 <- max(
      n_clusters -
        1L,
      1L
    )

    return(
      tibble(
        restriction =
          labels[[1]],

        estimate =
          d[[1]],

        std_error =
          se,

        wald_chisq =
          chisq,

        chi_square_df =
          1L,

        chi_square_p =
          pchisq(
            chisq,
            1,
            lower.tail = FALSE
          ),

        wald_F =
          chisq,

        F_df1 =
          1L,

        F_df2 =
          df2,

        cluster_df_F_p =
          pf(
            chisq,
            1,
            df2,
            lower.tail = FALSE
          )
      )
    )
  }

  eig <- eigen(
    S,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  if (
    min(eig) <=
      0
  ) {
    stop(
      "Joint Wald restriction covariance is not positive definite."
    )
  }

  chisq <- as.numeric(
    t(d) %*%
      solve(
        S,
        d
      )
  )

  df2 <- max(
    n_clusters -
      1L,
    1L
  )

  tibble(
    restriction =
      paste(
        labels,
        collapse = " AND "
      ),

    estimate =
      NA_real_,

    std_error =
      NA_real_,

    wald_chisq =
      chisq,

    chi_square_df =
      q,

    chi_square_p =
      pchisq(
        chisq,
        q,
        lower.tail = FALSE
      ),

    wald_F =
      chisq /
      q,

    F_df1 =
      q,

    F_df2 =
      df2,

    cluster_df_F_p =
      pf(
        chisq / q,
        q,
        df2,
        lower.tail = FALSE
      )
  )
}

models <- list()
samples <- list()
all_coefficients <- list()
focal_triples <- list()
wald_results <- list()
sample_summary <- list()

for (
  i in
  seq_len(
    nrow(
      cell_registry
    )
  )
) {
  spec <- cell_registry[
    i,
    ,
    drop = FALSE
  ]

  dd <- make_sample(
    spec$current_col,
    spec$baseline_col
  )

  if (
    nrow(dd) ==
      0L
  ) {
    stop(
      "No estimable observations for ",
      spec$cell_id
    )
  }

  fit <- fit_model(dd)

  models[[spec$cell_id]] <-
    fit

  samples[[spec$cell_id]] <-
    dd

  all_coefficients[[spec$cell_id]] <-
    tidy_fixest(
      fit,
      spec$cell_id,
      spec$sector
    )

  beta_names <- names(
    coef(fit)
  )

  current_triple <-
    find_exact_interaction_term(
      beta_names,
      c(
        "muslim",
        "fdi_current",
        "center_share_2014_ideology_complete"
      )
    )

  baseline_triple <-
    find_exact_interaction_term(
      beta_names,
      c(
        "muslim",
        "fdi_baseline",
        "center_share_2014_ideology_complete"
      )
    )

  ct <- coeftable(fit)

  focal_triples[[spec$cell_id]] <-
    bind_rows(
      tibble(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        period =
          "Current 2009-2014",

        term =
          current_triple,

        estimate =
          unname(
            ct[
              current_triple,
              1
            ]
          ),

        std_error =
          unname(
            ct[
              current_triple,
              2
            ]
          ),

        statistic =
          unname(
            ct[
              current_triple,
              3
            ]
          ),

        p_value =
          unname(
            ct[
              current_triple,
              4
            ]
          )
      ),

      tibble(
        cell_id =
          spec$cell_id,

        sector =
          spec$sector,

        period =
          "Baseline 2004-2009",

        term =
          baseline_triple,

        estimate =
          unname(
            ct[
              baseline_triple,
              1
            ]
          ),

        std_error =
          unname(
            ct[
              baseline_triple,
              2
            ]
          ),

        statistic =
          unname(
            ct[
              baseline_triple,
              3
            ]
          ),

        p_value =
          unname(
            ct[
              baseline_triple,
              4
            ]
          )
      )
    )

  n_clusters <-
    n_distinct(
      dd$pc_cluster_id
    )

  wald_results[[spec$cell_id]] <-
    bind_rows(
      wald_linear_fixest(
        fit,

        list(
          setNames(
            1,
            current_triple
          )
        ),

        "Current FDI x Muslim x 2014 ideology-complete Center share = 0",

        n_clusters
      ) |>
        mutate(
          cell_id =
            spec$cell_id,

          sector =
            spec$sector,

          test_family =
            "Current triple coefficient",

          .before = 1
        ),

      wald_linear_fixest(
        fit,

        list(
          setNames(
            1,
            baseline_triple
          )
        ),

        "Baseline FDI x Muslim x 2014 ideology-complete Center share = 0",

        n_clusters
      ) |>
        mutate(
          cell_id =
            spec$cell_id,

          sector =
            spec$sector,

          test_family =
            "Baseline triple coefficient",

          .before = 1
        ),

      wald_linear_fixest(
        fit,

        list(
          setNames(
            1,
            current_triple
          ),
          setNames(
            1,
            baseline_triple
          )
        ),

        c(
          "Current triple = 0",
          "Baseline triple = 0"
        ),

        n_clusters
      ) |>
        mutate(
          cell_id =
            spec$cell_id,

          sector =
            spec$sector,

          test_family =
            "Joint current + baseline triple",

          .before = 1
        )
    )

  sample_summary[[spec$cell_id]] <-
    tibble(
      cell_id =
        spec$cell_id,

      sector =
        spec$sector,

      official_vote_scale =
        official_vote_scale,

      n_ac =
        nrow(dd),

      n_states =
        n_distinct(
          dd$state_no
        ),

      n_pc_clusters =
        n_clusters,

      min_nes_ideology_complete_per_ac =
        min(
          dd$n_ideology_complete_2014
        ),

      median_nes_ideology_complete_per_ac =
        median(
          dd$n_ideology_complete_2014
        ),

      max_nes_ideology_complete_per_ac =
        max(
          dd$n_ideology_complete_2014
        ),

      center_share_min =
        min(
          dd$center_share_2014_ideology_complete
        ),

      center_share_median =
        median(
          dd$center_share_2014_ideology_complete
        ),

      center_share_max =
        max(
          dd$center_share_2014_ideology_complete
        ),

      n_ac_below_5_ideology_complete =
        sum(
          dd$n_ideology_complete_2014 <
            5
        ),

      n_ac_below_10_ideology_complete =
        sum(
          dd$n_ideology_complete_2014 <
            10
        )
    )
}

all_coefficients <-
  bind_rows(
    all_coefficients
  )

focal_triples <-
  bind_rows(
    focal_triples
  )

wald_results <-
  bind_rows(
    wald_results
  )

sample_summary <-
  bind_rows(
    sample_summary
  )

model_registry <- cell_registry |>
  mutate(
    outcome =
      "Official 2014 BJP AC vote share",

    center_share_definition =
      paste0(
        "Unweighted Center respondents / all ideology-complete 2014 NES ",
        "respondents in AC"
      ),

    minimum_nes_cell_size =
      "None; only n_ideology_complete_2014 > 0 required",

    current_interaction =
      paste0(
        "muslim * fdi_current * ",
        "center_share_2014_ideology_complete"
      ),

    baseline_interaction =
      paste0(
        "muslim * fdi_baseline * ",
        "center_share_2014_ideology_complete"
      ),

    controls =
      "proxy_ac_pop + sc_pop_share + st_pop_share",

    fixed_effects =
      "state_no",

    inference =
      "PC-clustered covariance: pc_cluster_id"
  )

write_csv(
  model_registry,
  file.path(
    output_dir,
    "00_model_registry.csv"
  )
)

write_csv(
  center14,
  file.path(
    output_dir,
    "01_2014_center_share_ideology_complete_by_ac.csv"
  )
)

write_csv(
  focal_triples,
  file.path(
    output_dir,
    "02_focal_current_and_baseline_triple_coefficients.csv"
  )
)

write_csv(
  wald_results,
  file.path(
    output_dir,
    "03_wald_tests.csv"
  )
)

write_csv(
  all_coefficients,
  file.path(
    output_dir,
    "04_all_model_coefficients.csv"
  )
)

write_csv(
  sample_summary,
  file.path(
    output_dir,
    "05_sample_summary.csv"
  )
)

saveRDS(
  models,
  file.path(
    output_dir,
    "06_models.rds"
  )
)

saveRDS(
  samples,
  file.path(
    output_dir,
    "07_model_samples.rds"
  )
)

notes <- c(
  "R38A3 v1.1 — OFFICIAL 2014 BJP VOTE SHARE x 2014 AC CENTER SHARE",
  "",
  "This is the corrected continuous-context specification.",
  "",
  "Outcome:",
  "Official 2014 BJP assembly-constituency vote share.",
  "",
  "2014 Center-share moderator:",
  "Primary definition = unweighted number of Center respondents divided by all ideology-complete 2014 NES respondents in the AC.",
  "The denominator is restricted to ideology_complete == TRUE and nonmissing voter_ideology.",
  "A survey-weighted version on the same ideology-complete denominator is saved for audit but is not substituted for the primary variable.",
  "",
  "No minimum NES cell-size requirement is imposed.",
  "The only requirement is n_ideology_complete_2014 > 0 so the contextual share is defined.",
  "",
  "Model:",
  "Conventional hierarchical triple interaction for current FDI and separately for baseline FDI.",
  "All main effects, all constituent two-way interactions, and each three-way interaction are included.",
  "",
  "Sectors:",
  "Total raw FDI and Manufacturing raw FDI are estimated separately.",
  "",
  "Inference:",
  "State fixed effects; parliamentary-constituency-clustered covariance; primary AC controls.",
  "",
  "Wald outputs:",
  "Each current and baseline triple coefficient is tested separately.",
  "A 2-df joint test evaluates current and baseline triple coefficients jointly."
)

writeLines(
  notes,
  file.path(
    output_dir,
    "08_notes.txt"
  )
)

cat(
  "\n===== R38A3 v1.1 MODEL REGISTRY =====\n\n"
)

print(
  model_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FOCAL TRIPLE COEFFICIENTS =====\n\n"
)

print(
  focal_triples,
  n = Inf,
  width = Inf
)

cat(
  "\n===== WALD TESTS =====\n\n"
)

print(
  wald_results,
  n = Inf,
  width = Inf
)

cat(
  "\n===== SAMPLE SUMMARY =====\n\n"
)

print(
  sample_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nOUTPUT_DIR=",
  output_dir,
  "\n",
  sep = ""
)

cat(
  "R38A3_V1_1_COMPLETE\n"
)

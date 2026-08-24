suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

final_dir <-
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
    "r31_demographic_context_audit_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

registry <-
  read_csv(
    "config/r30_demographic_moderator_registry_v1_0.csv",
    show_col_types = FALSE
  )

ideology <-
  readRDS(
    file.path(
      final_dir,
      "ac_year_ideology_summary.rds"
    )
  )

respondents <-
  readRDS(
    file.path(
      final_dir,
      "nes_respondent_analysis.rds"
    )
  )

ac_change <-
  readRDS(
    file.path(
      final_dir,
      "ac_change.rds"
    )
  )

if (
  nrow(
    registry
  ) !=
    8L
) {
  stop(
    "Expected exactly eight frozen demographic-context moderators."
  )
}

moderator_vars <-
  registry$variable

missing_from_ac_change <-
  setdiff(
    moderator_vars,
    names(
      ac_change
    )
  )

if (
  length(
    missing_from_ac_change
  ) >
    0L
) {
  stop(
    "Registered moderators missing from ac_change: ",
    paste(
      missing_from_ac_change,
      collapse = ", "
    )
  )
}

fdi_vars <-
  c(
    "fdi_total_local_all_pc100k_2014",
    "fdi_total_local_all_pc100k_2009"
  )

missing_fdi <-
  setdiff(
    fdi_vars,
    names(
      ac_change
    )
  )

if (
  length(
    missing_fdi
  ) >
    0L
) {
  stop(
    "Required Total-FDI variables missing from ac_change."
  )
}

payload <-
  ac_change |>
  select(
    ac_uid,
    all_of(
      fdi_vars
    ),
    all_of(
      moderator_vars
    )
  )

if (
  anyDuplicated(
    payload$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change payload is not unique by ac_uid."
  )
}

scale_registry <-
  registry |>
  mutate(
    stored_scale =
      if_else(
        grepl(
          "_pp$",
          variable
        ),
        "Percentage points",
        "Proportion"
      ),

    to_percentage_point_scale =
      if_else(
        stored_scale ==
          "Proportion",
        100,
        1
      )
  )

share_vars <-
  scale_registry |>
  filter(
    stored_scale ==
      "Proportion"
  ) |>
  pull(
    variable
  )

share_scale_failures <-
  map_dfr(
    share_vars,
    function(
      variable
    ) {
      x <-
        payload[[
          variable
        ]]

      tibble(
        variable =
          variable,

        n_nonmissing =
          sum(
            !is.na(
              x
            )
          ),

        n_below_zero =
          sum(
            is.finite(
              x
            ) &
              x <
                0,
            na.rm =
              TRUE
          ),

        n_above_one =
          sum(
            is.finite(
              x
            ) &
              x >
                1,
            na.rm =
              TRUE
          )
      )
    }
  )

if (
  any(
    share_scale_failures$n_below_zero >
      0L
  ) ||
    any(
      share_scale_failures$n_above_one >
        0L
    )
) {
  print(
    share_scale_failures,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one registered share variable is not on a 0-1 proportion scale."
  )
}

distribution_audit <-
  map_dfr(
    seq_len(
      nrow(
        scale_registry
      )
    ),
    function(
      i
    ) {
      row <-
        scale_registry[
          i,
          ,
          drop = FALSE
        ]

      x <-
        payload[[
          row$variable
        ]]

      x <-
        as.numeric(
          x
        )

      x_pp <-
        x *
        row$to_percentage_point_scale

      finite <-
        is.finite(
          x_pp
        )

      tibble(
        moderator_id =
          row$moderator_id,

        variable =
          row$variable,

        label =
          row$label,

        stored_scale =
          row$stored_scale,

        multiplier_to_pp =
          row$to_percentage_point_scale,

        n_total =
          length(
            x_pp
          ),

        n_nonmissing =
          sum(
            finite
          ),

        missing_share =
          mean(
            !finite
          ),

        min_pp =
          min(
            x_pp[
              finite
            ]
          ),

        p01_pp =
          quantile(
            x_pp[
              finite
            ],
            .01,
            names = FALSE,
            type = 8
          ),

        p10_pp =
          quantile(
            x_pp[
              finite
            ],
            .10,
            names = FALSE,
            type = 8
          ),

        p25_pp =
          quantile(
            x_pp[
              finite
            ],
            .25,
            names = FALSE,
            type = 8
          ),

        median_pp =
          median(
            x_pp[
              finite
            ]
          ),

        p75_pp =
          quantile(
            x_pp[
              finite
            ],
            .75,
            names = FALSE,
            type = 8
          ),

        p90_pp =
          quantile(
            x_pp[
              finite
            ],
            .90,
            names = FALSE,
            type = 8
          ),

        p99_pp =
          quantile(
            x_pp[
              finite
            ],
            .99,
            names = FALSE,
            type = 8
          ),

        max_pp =
          max(
            x_pp[
              finite
            ]
          ),

        sd_pp =
          sd(
            x_pp[
              finite
            ]
          )
      )
    }
  )

dataset_equivalence <- function(
  other_data,
  other_name
) {
  map_dfr(
    moderator_vars,
    function(
      variable
    ) {
      if (
        !variable %in%
          names(
            other_data
          )
      ) {
        return(
          tibble(
            dataset =
              other_name,
            variable =
              variable,
            present =
              FALSE,
            n_ac_compared =
              0L,
            inconsistent_within_dataset =
              NA,
            max_abs_difference =
              NA_real_
          )
        )
      }

      other_values <-
        other_data |>
        select(
          ac_uid,
          value =
            all_of(
              variable
            )
        ) |>
        distinct()

      inconsistency <-
        other_values |>
        group_by(
          ac_uid
        ) |>
        summarise(
          n_values =
            n_distinct(
              value[
                !is.na(
                  value
                )
              ]
            ),
          .groups =
            "drop"
        ) |>
        summarise(
          any_inconsistent =
            any(
              n_values >
                1L
            )
        ) |>
        pull(
          any_inconsistent
        )

      other_unique <-
        other_values |>
        arrange(
          ac_uid,
          is.na(
            value
          )
        ) |>
        group_by(
          ac_uid
        ) |>
        summarise(
          other_value =
            first(
              value
            ),
          .groups =
            "drop"
        )

      comparison <-
        payload |>
        select(
          ac_uid,
          canonical_value =
            all_of(
              variable
            )
        ) |>
        inner_join(
          other_unique,
          by =
            "ac_uid"
        ) |>
        filter(
          is.finite(
            canonical_value
          ),
          is.finite(
            other_value
          )
        )

      tibble(
        dataset =
          other_name,

        variable =
          variable,

        present =
          TRUE,

        n_ac_compared =
          nrow(
            comparison
          ),

        inconsistent_within_dataset =
          inconsistency,

        max_abs_difference =
          if (
            nrow(
              comparison
            ) ==
              0L
          ) {
            NA_real_
          } else {
            max(
              abs(
                comparison$canonical_value -
                  comparison$other_value
              )
            )
          }
      )
    }
  )
}

equivalence_audit <-
  bind_rows(
    dataset_equivalence(
      ideology,
      "ac_year_ideology_summary"
    ),
    dataset_equivalence(
      respondents,
      "nes_respondent_analysis"
    )
  )

ac_base <-
  ideology |>
  filter(
    year ==
      2014,
    as.character(
      ideology
    ) ==
      "Center"
  ) |>
  select(
    -any_of(
      c(
        fdi_vars,
        moderator_vars
      )
    )
  ) |>
  left_join(
    payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    y =
      as.numeric(
        weighted_share_voted_bjp
      ),

    fdi_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    fdi_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      )
  )

voter_base <-
  respondents |>
  select(
    -any_of(
      c(
        fdi_vars,
        moderator_vars
      )
    )
  ) |>
  left_join(
    payload,
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    y =
      as.numeric(
        voted_bjp
      ),

    fdi_current =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    fdi_baseline =
      as.numeric(
        fdi_total_local_all_pc100k_2009
      )
  )

finite_variable <- function(
  x
) {
  !is.na(
    x
  ) &
    if (
      is.numeric(
        x
      ) ||
        is.integer(
          x
        )
    ) {
      is.finite(
        as.numeric(
          x
        )
      )
    } else {
      TRUE
    }
}

ac_primary_base <-
  ac_base |>
  filter(
    !is.na(
      y
    ),
    bjp_candidate_present %in%
      TRUE,
    fdi_spatial_support %in%
      TRUE,
    is.finite(
      fdi_current
    ),
    is.finite(
      fdi_baseline
    ),
    is.finite(
      proxy_ac_pop
    ),
    is.finite(
      sc_pop_share
    ),
    is.finite(
      st_pop_share
    ),
    !is.na(
      state_no
    ),
    !is.na(
      pc_cluster_id
    )
  )

ac_expanded_base <-
  ac_primary_base |>
  filter(
    is.finite(
      employment_per_total_population
    ),
    is.finite(
      ed_sec_share
    )
  )

voter_primary_base <-
  voter_base |>
  filter(
    year ==
      2014,
    vote_valid %in%
      TRUE,
    !is.na(
      y
    ),
    ideology_complete %in%
      TRUE,
    as.character(
      voter_ideology
    ) ==
      "Center",
    bjp_candidate_present %in%
      TRUE,
    fdi_spatial_support %in%
      TRUE,
    is.finite(
      fdi_current
    ),
    is.finite(
      fdi_baseline
    ),
    is.finite(
      proxy_ac_pop
    ),
    is.finite(
      sc_pop_share
    ),
    is.finite(
      st_pop_share
    ),
    !is.na(
      religion_group
    ),
    !is.na(
      caste_group
    ),
    !is.na(
      education_harmonized
    ),
    !is.na(
      state_no
    ),
    !is.na(
      ac_uid
    )
  )

voter_expanded_base <-
  voter_primary_base |>
  filter(
    is.finite(
      employment_per_total_population
    ),
    is.finite(
      ed_sec_share
    )
  )

make_sample_counts <- function(
  primary_data,
  expanded_data,
  level
) {
  map_dfr(
    seq_len(
      nrow(
        scale_registry
      )
    ),
    function(
      i
    ) {
      row <-
        scale_registry[
          i,
          ,
          drop = FALSE
        ]

      variable <-
        row$variable

      primary <-
        primary_data |>
        filter(
          finite_variable(
            .data[[
              variable
            ]]
          )
        )

      expanded <-
        expanded_data |>
        filter(
          finite_variable(
            .data[[
              variable
            ]]
          )
        )

      tibble(
        level =
          level,

        moderator_id =
          row$moderator_id,

        variable =
          variable,

        label =
          row$label,

        stored_scale =
          row$stored_scale,

        n_primary =
          nrow(
            primary
          ),

        n_ac_primary =
          n_distinct(
            primary$ac_uid
          ),

        n_states_primary =
          n_distinct(
            primary$state_no
          ),

        n_expanded =
          nrow(
            expanded
          ),

        n_ac_expanded =
          n_distinct(
            expanded$ac_uid
          ),

        n_states_expanded =
          n_distinct(
            expanded$state_no
          )
      )
    }
  )
}

sample_counts <-
  bind_rows(
    make_sample_counts(
      ac_primary_base,
      ac_expanded_base,
      "AC"
    ),
    make_sample_counts(
      voter_primary_base,
      voter_expanded_base,
      "Voter"
    )
  )

all_moderator_complete <- function(
  data
) {
  keep <-
    rep(
      TRUE,
      nrow(
        data
      )
    )

  for (
    variable in
      moderator_vars
  ) {
    keep <-
      keep &
      finite_variable(
        data[[
          variable
        ]]
      )
  }

  data[
    keep,
    ,
    drop = FALSE
  ]
}

common_sample_summary <-
  bind_rows(
    tibble(
      level =
        "AC",
      control_set =
        "Primary",
      n =
        nrow(
          all_moderator_complete(
            ac_primary_base
          )
        ),
      n_ac =
        n_distinct(
          all_moderator_complete(
            ac_primary_base
          )$ac_uid
        ),
      n_states =
        n_distinct(
          all_moderator_complete(
            ac_primary_base
          )$state_no
        )
    ),

    tibble(
      level =
        "AC",
      control_set =
        "Expanded",
      n =
        nrow(
          all_moderator_complete(
            ac_expanded_base
          )
        ),
      n_ac =
        n_distinct(
          all_moderator_complete(
            ac_expanded_base
          )$ac_uid
        ),
      n_states =
        n_distinct(
          all_moderator_complete(
            ac_expanded_base
          )$state_no
        )
    ),

    tibble(
      level =
        "Voter",
      control_set =
        "Primary",
      n =
        nrow(
          all_moderator_complete(
            voter_primary_base
          )
        ),
      n_ac =
        n_distinct(
          all_moderator_complete(
            voter_primary_base
          )$ac_uid
        ),
      n_states =
        n_distinct(
          all_moderator_complete(
            voter_primary_base
          )$state_no
        )
    ),

    tibble(
      level =
        "Voter",
      control_set =
        "Expanded",
      n =
        nrow(
          all_moderator_complete(
            voter_expanded_base
          )
        ),
      n_ac =
        n_distinct(
          all_moderator_complete(
            voter_expanded_base
          )$ac_uid
        ),
      n_states =
        n_distinct(
          all_moderator_complete(
            voter_expanded_base
          )$state_no
        )
    )
  )

write_csv(
  scale_registry,
  file.path(
    output_dir,
    "01_moderator_scale_registry.csv"
  )
)

write_csv(
  distribution_audit,
  file.path(
    output_dir,
    "02_moderator_distribution_audit.csv"
  )
)

write_csv(
  share_scale_failures,
  file.path(
    output_dir,
    "03_share_scale_validation.csv"
  )
)

write_csv(
  equivalence_audit,
  file.path(
    output_dir,
    "04_cross_dataset_equivalence_audit.csv"
  )
)

write_csv(
  sample_counts,
  file.path(
    output_dir,
    "05_native_sample_counts.csv"
  )
)

write_csv(
  common_sample_summary,
  file.path(
    output_dir,
    "06_all_moderator_common_sample_counts.csv"
  )
)

notes <-
  c(
    "R31a DEMOGRAPHIC-CONTEXT MODERATOR AUDIT",
    "",
    "No regression model is estimated.",
    "",
    "The eight moderator concepts are read from the frozen R30 moderator registry.",
    "ac_change is used as the common AC-level source for moderator values.",
    "",
    "Variables ending in _pp are treated as already measured in percentage points.",
    "Registered share variables without _pp are audited as 0-1 proportions and converted by multiplying by 100 for a common +1-percentage-point interpretation.",
    "",
    "The script audits distributions, cross-dataset consistency, native eligible sample counts, and the all-eight-moderator common sample.",
    "",
    "The subsequent R31b models should not be run until these scale and sample audits are reviewed."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "07_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "08_session_info.txt"
  )
)

cat(
  "\n===== MODERATOR SCALE REGISTRY =====\n"
)

print(
  scale_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MODERATOR DISTRIBUTION AUDIT =====\n"
)

print(
  distribution_audit,
  n = Inf,
  width = Inf
)

cat(
  "\n===== CROSS-DATASET EQUIVALENCE =====\n"
)

print(
  equivalence_audit,
  n = Inf,
  width = Inf
)

cat(
  "\n===== NATIVE SAMPLE COUNTS =====\n"
)

print(
  sample_counts,
  n = Inf,
  width = Inf
)

cat(
  "\n===== ALL-MODERATOR COMMON SAMPLE =====\n"
)

print(
  common_sample_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A_DEMOGRAPHIC_CONTEXT_AUDIT_COMPLETE\n"
)

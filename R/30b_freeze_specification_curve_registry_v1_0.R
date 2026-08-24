suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
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
    "r30_specification_registry_freeze_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

fdi_registry <-
  read_csv(
    "config/r30_core_fdi_specification_registry_v1_0.csv",
    show_col_types = FALSE
  )

control_registry <-
  read_csv(
    "config/r30_control_set_registry_v1_0.csv",
    show_col_types = FALSE
  )

moderator_registry <-
  read_csv(
    "config/r30_demographic_moderator_registry_v1_0.csv",
    show_col_types = FALSE
  )

excluded_registry <-
  read_csv(
    "config/r30_excluded_dimensions_v1_0.csv",
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    file.path(
      final_dir,
      "ac_change.rds"
    )
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

fdi12 <-
  readRDS(
    "outputs/fdi_12m_temporal_robustness_v1_0/03_fdi_ac_12m.rds"
  )

if (
  nrow(
    fdi_registry
  ) !=
    10L
) {
  stop(
    "Core FDI registry must contain exactly 10 definitions."
  )
}

if (
  sum(
    fdi_registry$sector ==
      "Total"
  ) !=
    5L ||
    sum(
      fdi_registry$sector ==
        "Manufacturing"
    ) !=
      5L
) {
  stop(
    "Registry must contain five Total and five Manufacturing definitions."
  )
}

if (
  nrow(
    moderator_registry
  ) !=
    8L
) {
  stop(
    "Demographic-context registry must contain exactly 8 moderator concepts."
  )
}

canonical_rows <-
  fdi_registry |>
  filter(
    source_artifact ==
      "canonical_ac_change"
  )

post12_rows <-
  fdi_registry |>
  filter(
    source_artifact ==
      "post_primary_12m"
  )

canonical_vars <-
  unique(
    c(
      canonical_rows$current_var,
      canonical_rows$baseline_var
    )
  )

post12_vars <-
  unique(
    c(
      post12_rows$current_var,
      post12_rows$baseline_var
    )
  )

missing_canonical_fdi <-
  setdiff(
    canonical_vars,
    names(
      ac_change
    )
  )

missing_12m_fdi <-
  setdiff(
    post12_vars,
    names(
      fdi12
    )
  )

if (
  length(
    missing_canonical_fdi
  ) >
    0L
) {
  stop(
    "Missing canonical FDI variables: ",
    paste(
      missing_canonical_fdi,
      collapse = ", "
    )
  )
}

if (
  length(
    missing_12m_fdi
  ) >
    0L
) {
  stop(
    "Missing 12-month FDI variables: ",
    paste(
      missing_12m_fdi,
      collapse = ", "
    )
  )
}

if (
  anyDuplicated(
    fdi12$ac_uid
  ) >
    0L
) {
  stop(
    "12-month FDI artifact is not unique by ac_uid."
  )
}

window_definition <-
  read_csv(
    "outputs/fdi_12m_temporal_robustness_v1_0/01_window_definition.csv",
    show_col_types = FALSE
  )

if (
  nrow(
    window_definition
  ) !=
    2L ||
    any(
      window_definition$n_months !=
        12L
    )
) {
  stop(
    "12-month window-definition audit failed."
  )
}

nesting_checks <-
  read_csv(
    "outputs/fdi_12m_temporal_robustness_v1_0/06_60m_nesting_checks.csv",
    show_col_types = FALSE
  )

if (
  any(
    nesting_checks$early12_exceeds_60m_count !=
      0L
  ) ||
    any(
      nesting_checks$late12_exceeds_60m_count !=
        0L
    )
) {
  stop(
    "12-month nesting audit is not clean."
  )
}

core_registry <-
  crossing(
    level =
      c(
        "AC",
        "Voter"
      ),

    fdi_spec_id =
      fdi_registry$fdi_spec_id,

    control_set =
      c(
        "Primary",
        "Expanded"
      )
  ) |>
  left_join(
    fdi_registry,
    by =
      "fdi_spec_id"
  ) |>
  mutate(
    spec_id =
      paste(
        level,
        fdi_spec_id,
        control_set,
        sep =
          "__"
      ),

    outcome =
      if_else(
        level ==
          "AC",
        "2014 survey-weighted BJP share among Center respondents",
        "2014 BJP vote among Center respondents"
      ),

    moderator =
      "muslim_share_2001_dist_proxy",

    fixed_effect =
      "State",

    grouping =
      if_else(
        level ==
          "AC",
        "PC-clustered SE",
        "AC random intercept"
      ),

    figure_placement =
      if_else(
        level ==
          "AC",
        "Main Figure 6",
        "Appendix"
      )
  ) |>
  select(
    spec_id,
    level,
    figure_placement,
    sector,
    family,
    geography,
    functional_form,
    current_var,
    baseline_var,
    source_artifact,
    control_set,
    outcome,
    moderator,
    fixed_effect,
    grouping,
    role,
    design_status
  )

if (
  sum(
    core_registry$level ==
      "AC"
  ) !=
    20L ||
    sum(
      core_registry$level ==
        "Voter"
    ) !=
      20L
) {
  stop(
    "Core registry must contain 20 AC and 20 voter specifications."
  )
}

moderator_presence <-
  tibble(
    variable =
      moderator_registry$variable,

    in_ideology =
      variable %in%
        names(
          ideology
        ),

    in_ac_change =
      variable %in%
        names(
          ac_change
        ),

    in_respondents =
      variable %in%
        names(
          respondents
        )
  )

if (
  any(
    !moderator_presence$in_ideology &
      !moderator_presence$in_ac_change &
      !moderator_presence$in_respondents
  )
) {
  print(
    moderator_presence,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one registered moderator is unavailable."
  )
}

moderator_curve_registry <-
  crossing(
    level =
      c(
        "AC",
        "Voter"
      ),

    moderator_id =
      moderator_registry$moderator_id,

    control_set =
      c(
        "Primary",
        "Expanded"
      )
  ) |>
  left_join(
    moderator_registry,
    by =
      "moderator_id"
  ) |>
  mutate(
    spec_id =
      paste(
        level,
        moderator_id,
        control_set,
        sep =
          "__"
      ),

    fdi_definition =
      "Total local raw, 60-month current + baseline",

    current_var =
      "fdi_total_local_all_pc100k_2014",

    baseline_var =
      "fdi_total_local_all_pc100k_2009",

    figure_placement =
      "Appendix"
  )

if (
  sum(
    moderator_curve_registry$level ==
      "AC"
  ) !=
    16L ||
    sum(
      moderator_curve_registry$level ==
        "Voter"
    ) !=
      16L
) {
  stop(
    "Moderator registry must contain 16 AC and 16 voter specifications."
  )
}

write_csv(
  core_registry,
  file.path(
    output_dir,
    "01_frozen_core_specification_registry.csv"
  )
)

write_csv(
  moderator_curve_registry,
  file.path(
    output_dir,
    "02_frozen_demographic_context_registry.csv"
  )
)

write_csv(
  moderator_presence,
  file.path(
    output_dir,
    "03_moderator_variable_presence.csv"
  )
)

write_csv(
  excluded_registry,
  file.path(
    output_dir,
    "04_excluded_dimensions.csv"
  )
)

write_csv(
  window_definition,
  file.path(
    output_dir,
    "05_12m_window_definition.csv"
  )
)

write_csv(
  nesting_checks,
  file.path(
    output_dir,
    "06_12m_nesting_checks.csv"
  )
)

notes <-
  c(
    "R30 SPECIFICATION-CURVE REGISTRY FREEZE",
    "",
    "CORE FDI FAMILY",
    "Ten FDI definitions x two control sets = 20 specifications per analysis level.",
    "AC family is intended for Main Figure 6.",
    "Parallel voter family is appendix-only.",
    "",
    "Each sector contains:",
    "60-month local raw current + baseline",
    "60-month local log1p current + baseline",
    "60-month own-AC raw current + baseline",
    "21-month local raw change + early baseline",
    "12-month local raw change + early baseline",
    "",
    "The 12-month specification is explicitly post-estimation robustness.",
    "Early12 = April 2008 through March 2009.",
    "Late12 = April 2013 through March 2014.",
    "The fitted 12-month specification uses late-minus-early change plus the early baseline interaction.",
    "A separate late-level plus early-level specification is excluded because it is an exact reparameterization.",
    "",
    "No positive-FDI restriction or new ideology-cell minimum-N threshold is introduced.",
    "",
    "Services FDI, Left/Right outcomes, official-vote triple interactions, and demographic-context alternatives remain separate appendix analyses."
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
  "\n===== FROZEN CORE R30 REGISTRY =====\n"
)

print(
  core_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MODERATOR VARIABLE PRESENCE =====\n"
)

print(
  moderator_presence,
  n = Inf,
  width = Inf
)

cat(
  "\n===== 12-MONTH QA CARRIED INTO FREEZE =====\n"
)

print(
  nesting_checks,
  n = Inf,
  width = Inf
)

cat(
  "\nR30B_SPECIFICATION_REGISTRY_FREEZE_COMPLETE\n"
)

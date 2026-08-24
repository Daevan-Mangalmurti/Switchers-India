suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
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

intermediate_dir <-
  file.path(
    project_root,
    "data",
    "derived",
    "switchers_rewrite",
    "intermediate"
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
    "fdi_12m_temporal_robustness_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

exposure_path <-
  file.path(
    intermediate_dir,
    "fdi_project_exposure.csv"
  )

ac_year_path <-
  file.path(
    final_dir,
    "ac_year.rds"
  )

ac_change_path <-
  file.path(
    final_dir,
    "ac_change.rds"
  )

required_files <-
  c(
    exposure_path,
    ac_year_path,
    ac_change_path
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
    "Missing required input files: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

exposure <-
  read_csv(
    exposure_path,
    show_col_types = FALSE
  ) |>
  mutate(
    project_month =
      as.Date(
        project_month
      )
  )

ac_year <-
  readRDS(
    ac_year_path
  )

ac_change <-
  readRDS(
    ac_change_path
  )

required_exposure_columns <-
  c(
    "fdi_project_uid",
    "exposed_ac_uid",
    "exposure_scope",
    "project_month",
    "standardized_sector",
    "standardized_status",
    "spatial_match_valid"
  )

missing_exposure_columns <-
  setdiff(
    required_exposure_columns,
    names(
      exposure
    )
  )

if (
  length(
    missing_exposure_columns
  ) >
    0L
) {
  stop(
    "fdi_project_exposure.csv is missing: ",
    paste(
      missing_exposure_columns,
      collapse = ", "
    )
  )
}

required_ac_columns <-
  c(
    "ac_uid",
    "state_no",
    "ac",
    "proxy_ac_pop",
    "fdi_spatial_support",
    "fdi_n_touching_neighbors"
  )

missing_ac_columns <-
  setdiff(
    required_ac_columns,
    names(
      ac_year
    )
  )

if (
  length(
    missing_ac_columns
  ) >
    0L
) {
  stop(
    "ac_year is missing: ",
    paste(
      missing_ac_columns,
      collapse = ", "
    )
  )
}

window_registry <-
  tribble(
    ~window, ~period_start, ~period_end,

    "early12",
    as.Date(
      "2008-04-01"
    ),
    as.Date(
      "2009-04-01"
    ),

    "late12",
    as.Date(
      "2013-04-01"
    ),
    as.Date(
      "2014-04-01"
    )
  ) |>
  mutate(
    n_months =
      map2_int(
        period_start,
        period_end,
        ~ length(
          seq.Date(
            .x,
            .y - 1,
            by =
              "month"
          )
        )
      )
  )

if (
  any(
    window_registry$n_months !=
      12L
  )
) {
  stop(
    "Each 12-month temporal robustness window must contain exactly 12 months."
  )
}

ac_universe_check <-
  ac_year |>
  group_by(
    ac_uid
  ) |>
  summarise(
    n_state =
      n_distinct(
        state_no,
        na.rm = TRUE
      ),

    n_ac_number =
      n_distinct(
        ac,
        na.rm = TRUE
      ),

    n_population =
      n_distinct(
        proxy_ac_pop,
        na.rm = TRUE
      ),

    n_spatial_support =
      n_distinct(
        fdi_spatial_support,
        na.rm = TRUE
      ),

    n_neighbor_count =
      n_distinct(
        fdi_n_touching_neighbors,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  )

if (
  any(
    ac_universe_check$n_state >
      1L
  ) ||
    any(
      ac_universe_check$n_ac_number >
        1L
    ) ||
    any(
      ac_universe_check$n_population >
        1L
    ) ||
    any(
      ac_universe_check$n_spatial_support >
        1L
    ) ||
    any(
      ac_universe_check$n_neighbor_count >
        1L
    )
) {
  print(
    ac_universe_check |>
      filter(
        n_state >
          1L |
          n_ac_number >
            1L |
          n_population >
            1L |
          n_spatial_support >
            1L |
          n_neighbor_count >
            1L
      ),
    n = Inf,
    width = Inf
  )

  stop(
    "AC-level population or spatial-support quantities vary across election years."
  )
}

ac_universe <-
  ac_year |>
  arrange(
    ac_uid
  ) |>
  group_by(
    ac_uid
  ) |>
  summarise(
    state_no =
      first(
        state_no[
          !is.na(
            state_no
          )
        ]
      ),

    ac =
      first(
        ac[
          !is.na(
            ac
          )
        ]
      ),

    proxy_ac_pop =
      first(
        proxy_ac_pop[
          !is.na(
            proxy_ac_pop
          )
        ]
      ),

    fdi_spatial_support =
      first(
        fdi_spatial_support[
          !is.na(
            fdi_spatial_support
          )
        ]
      ),

    fdi_n_touching_neighbors =
      first(
        fdi_n_touching_neighbors[
          !is.na(
            fdi_n_touching_neighbors
          )
        ]
      ),

    .groups =
      "drop"
  )

if (
  anyDuplicated(
    ac_universe$ac_uid
  ) >
    0L
) {
  stop(
    "AC universe is not unique by ac_uid."
  )
}

population_denominator_diagnostics <-
  ac_universe |>
  summarise(
    n_ac =
      n(),

    n_spatially_supported =
      sum(
        fdi_spatial_support %in%
          TRUE
      ),

    n_spatially_unsupported =
      sum(
        !(fdi_spatial_support %in%
            TRUE)
      ),

    n_missing_population_all =
      sum(
        is.na(
          proxy_ac_pop
        )
      ),

    n_invalid_population_supported =
      sum(
        fdi_spatial_support %in%
          TRUE &
          (
            is.na(
              proxy_ac_pop
            ) |
              !is.finite(
                proxy_ac_pop
              ) |
              proxy_ac_pop <=
                0
          )
      ),

    n_missing_population_unsupported =
      sum(
        !(fdi_spatial_support %in%
            TRUE) &
          is.na(
            proxy_ac_pop
          )
      )
  )

invalid_supported_population <-
  ac_universe |>
  filter(
    fdi_spatial_support %in%
      TRUE,
    is.na(
      proxy_ac_pop
    ) |
      !is.finite(
        proxy_ac_pop
      ) |
      proxy_ac_pop <=
        0
  )

if (
  nrow(
    invalid_supported_population
  ) >
    0L
) {
  print(
    invalid_supported_population,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one spatially supported AC has an invalid population denominator."
  )
}

exposure_windowed <-
  exposure |>
  filter(
    spatial_match_valid %in%
      TRUE,
    standardized_status %in%
      c(
        "announced",
        "opened"
      ),
    exposure_scope %in%
      c(
        "own",
        "adjacent",
        "local"
      )
  ) |>
  mutate(
    window =
      case_when(
        project_month >=
          as.Date(
            "2008-04-01"
          ) &
          project_month <
            as.Date(
              "2009-04-01"
            ) ~
          "early12",

        project_month >=
          as.Date(
            "2013-04-01"
          ) &
          project_month <
            as.Date(
              "2014-04-01"
            ) ~
          "late12",

        TRUE ~
          NA_character_
      ),

    sector_list =
      map(
        standardized_sector,
        ~ c(
          "total",
          case_when(
            .x ==
              "manufacturing" ~
              "mfg",

            .x ==
              "services" ~
              "services",

            TRUE ~
              NA_character_
          )
        )
      )
  ) |>
  filter(
    !is.na(
      window
    )
  ) |>
  unnest_longer(
    sector_list,
    values_to =
      "sector"
  ) |>
  filter(
    !is.na(
      sector
    )
  ) |>
  distinct(
    fdi_project_uid,
    exposed_ac_uid,
    window,
    sector,
    exposure_scope
  )

window_project_counts <-
  exposure_windowed |>
  summarise(
    n_unique_projects =
      n_distinct(
        fdi_project_uid
      ),
    .by =
      c(
        window,
        sector,
        exposure_scope
      )
  ) |>
  arrange(
    window,
    sector,
    exposure_scope
  )

counts_long <-
  exposure_windowed |>
  summarise(
    projects_n =
      n_distinct(
        fdi_project_uid
      ),
    .by =
      c(
        exposed_ac_uid,
        window,
        sector,
        exposure_scope
      )
  )

full_grid <-
  ac_universe |>
  transmute(
    exposed_ac_uid =
      ac_uid,
    state_no,
    ac,
    proxy_ac_pop,
    fdi_spatial_support,
    fdi_n_touching_neighbors
  ) |>
  crossing(
    window =
      c(
        "early12",
        "late12"
      ),
    sector =
      c(
        "total",
        "mfg",
        "services"
      ),
    exposure_scope =
      c(
        "own",
        "adjacent",
        "local"
      )
  ) |>
  left_join(
    counts_long,
    by =
      c(
        "exposed_ac_uid",
        "window",
        "sector",
        "exposure_scope"
      )
  ) |>
  mutate(
    projects_n =
      if_else(
        fdi_spatial_support,
        replace_na(
          projects_n,
          0L
        ),
        NA_integer_
      ),

    projects_pc100k =
      if_else(
        fdi_spatial_support,
        100000 *
          projects_n /
          proxy_ac_pop,
        NA_real_
      ),

    log1p_projects_pc100k =
      log1p(
        projects_pc100k
      ),

    stem =
      paste(
        "fdi",
        sector,
        exposure_scope,
        window,
        sep =
          "_"
      )
  )

wide <-
  full_grid |>
  select(
    exposed_ac_uid,
    state_no,
    ac,
    proxy_ac_pop,
    fdi_spatial_support,
    fdi_n_touching_neighbors,
    stem,
    projects_n,
    projects_pc100k,
    log1p_projects_pc100k
  ) |>
  pivot_wider(
    names_from =
      stem,
    values_from =
      c(
        projects_n,
        projects_pc100k,
        log1p_projects_pc100k
      ),
    names_glue =
      "{stem}_{.value}"
  ) |>
  rename_with(
    ~ sub(
      "_projects_n$",
      "_n",
      .x
    )
  ) |>
  rename_with(
    ~ sub(
      "_projects_pc100k$",
      "_pc100k",
      .x
    )
  ) |>
  rename_with(
    ~ sub(
      "_log1p_projects_pc100k$",
      "_log1p_pc100k",
      .x
    )
  ) |>
  rename_with(
    ~ sub(
      "^(fdi_.+)_log1p_pc100k$",
      "log1p_\\1_pc100k",
      .x
    )
  ) |>
  rename(
    ac_uid =
      exposed_ac_uid
  )

for (
  window_name in
    c(
      "early12",
      "late12"
    )
) {
  for (
    scope_name in
      c(
        "own",
        "adjacent",
        "local"
      )
  ) {
    total_col <-
      paste0(
        "fdi_total_",
        scope_name,
        "_",
        window_name,
        "_n"
      )

    mfg_col <-
      paste0(
        "fdi_mfg_",
        scope_name,
        "_",
        window_name,
        "_n"
      )

    services_col <-
      paste0(
        "fdi_services_",
        scope_name,
        "_",
        window_name,
        "_n"
      )

    failed <-
      !is.na(
        wide[[
          total_col
        ]]
      ) &
      (
        is.na(
          wide[[
            mfg_col
          ]]
        ) |
          is.na(
            wide[[
              services_col
            ]]
          ) |
          wide[[
            total_col
          ]] !=
            wide[[
              mfg_col
            ]] +
              wide[[
                services_col
              ]]
      )

    if (
      any(
        failed
      )
    ) {
      stop(
        "12-month sector identity failed for ",
        window_name,
        " / ",
        scope_name
      )
    }
  }
}

for (
  window_name in
    c(
      "early12",
      "late12"
    )
) {
  for (
    sector_name in
      c(
        "total",
        "mfg",
        "services"
      )
  ) {
    own_col <-
      paste0(
        "fdi_",
        sector_name,
        "_own_",
        window_name,
        "_n"
      )

    adjacent_col <-
      paste0(
        "fdi_",
        sector_name,
        "_adjacent_",
        window_name,
        "_n"
      )

    local_col <-
      paste0(
        "fdi_",
        sector_name,
        "_local_",
        window_name,
        "_n"
      )

    failed <-
      !is.na(
        wide[[
          local_col
        ]]
      ) &
      (
        is.na(
          wide[[
            own_col
          ]]
        ) |
          is.na(
            wide[[
              adjacent_col
            ]]
          ) |
          wide[[
            local_col
          ]] !=
            wide[[
              own_col
            ]] +
              wide[[
                adjacent_col
              ]]
      )

    if (
      any(
        failed
      )
    ) {
      stop(
        "12-month spatial identity failed for ",
        window_name,
        " / ",
        sector_name
      )
    }
  }
}

for (
  sector_name in
    c(
      "total",
      "mfg",
      "services"
    )
) {
  for (
    scope_name in
      c(
        "own",
        "adjacent",
        "local"
      )
  ) {
    early_n <-
      paste0(
        "fdi_",
        sector_name,
        "_",
        scope_name,
        "_early12_n"
      )

    late_n <-
      paste0(
        "fdi_",
        sector_name,
        "_",
        scope_name,
        "_late12_n"
      )

    early_pc <-
      paste0(
        "fdi_",
        sector_name,
        "_",
        scope_name,
        "_early12_pc100k"
      )

    late_pc <-
      paste0(
        "fdi_",
        sector_name,
        "_",
        scope_name,
        "_late12_pc100k"
      )

    early_log <-
      paste0(
        "log1p_fdi_",
        sector_name,
        "_",
        scope_name,
        "_early12_pc100k"
      )

    late_log <-
      paste0(
        "log1p_fdi_",
        sector_name,
        "_",
        scope_name,
        "_late12_pc100k"
      )

    wide[[
      paste0(
        "d_fdi_",
        sector_name,
        "_",
        scope_name,
        "_12m_n"
      )
    ]] <-
      wide[[
        late_n
      ]] -
      wide[[
        early_n
      ]]

    wide[[
      paste0(
        "d_fdi_",
        sector_name,
        "_",
        scope_name,
        "_12m_pc100k"
      )
    ]] <-
      wide[[
        late_pc
      ]] -
      wide[[
        early_pc
      ]]

    wide[[
      paste0(
        "d_log1p_fdi_",
        sector_name,
        "_",
        scope_name,
        "_12m_pc100k"
      )
    ]] <-
      wide[[
        late_log
      ]] -
      wide[[
        early_log
      ]]
  }
}

wide <-
  wide |>
  arrange(
    state_no,
    ac
  )

if (
  anyDuplicated(
    wide$ac_uid
  ) >
    0L
) {
  stop(
    "12-month AC output is not unique by ac_uid."
  )
}

nesting_checks <-
  list()

for (
  sector_name in
    c(
      "total",
      "mfg",
      "services"
    )
) {
  for (
    scope_name in
      c(
        "own",
        "local"
      )
  ) {
    comparison <-
      wide |>
      select(
        ac_uid,
        early12 =
          all_of(
            paste0(
              "fdi_",
              sector_name,
              "_",
              scope_name,
              "_early12_n"
            )
          ),
        late12 =
          all_of(
            paste0(
              "fdi_",
              sector_name,
              "_",
              scope_name,
              "_late12_n"
            )
          )
      ) |>
      left_join(
        ac_change |>
          select(
            ac_uid,
            period2009 =
              all_of(
                paste0(
                  "fdi_",
                  sector_name,
                  "_",
                  scope_name,
                  "_all_n_2009"
                )
              ),
            period2014 =
              all_of(
                paste0(
                  "fdi_",
                  sector_name,
                  "_",
                  scope_name,
                  "_all_n_2014"
                )
              )
          ),
        by =
          "ac_uid",
        relationship =
          "one-to-one"
      )

    early_fail <-
      comparison |>
      filter(
        !is.na(
          early12
        ),
        !is.na(
          period2009
        ),
        early12 >
          period2009
      )

    late_fail <-
      comparison |>
      filter(
        !is.na(
          late12
        ),
        !is.na(
          period2014
        ),
        late12 >
          period2014
      )

    nesting_checks[[
      paste(
        sector_name,
        scope_name,
        sep =
          "__"
      )
    ]] <-
      tibble(
        sector =
          sector_name,

        scope =
          scope_name,

        early12_exceeds_60m_count =
          nrow(
            early_fail
          ),

        late12_exceeds_60m_count =
          nrow(
            late_fail
          )
      )
  }
}

nesting_checks <-
  bind_rows(
    nesting_checks
  )

if (
  any(
    nesting_checks$early12_exceeds_60m_count >
      0L
  ) ||
    any(
      nesting_checks$late12_exceeds_60m_count >
        0L
    )
) {
  print(
    nesting_checks,
    n = Inf,
    width = Inf
  )

  stop(
    "12-month counts are not nested inside canonical 60-month counts."
  )
}

support_diagnostics <-
  map_dfr(
    c(
      "total",
      "mfg",
      "services"
    ),
    function(
      sector_name
    ) {
      early_var <-
        paste0(
          "fdi_",
          sector_name,
          "_local_early12_pc100k"
        )

      late_var <-
        paste0(
          "fdi_",
          sector_name,
          "_local_late12_pc100k"
        )

      delta_var <-
        paste0(
          "d_fdi_",
          sector_name,
          "_local_12m_pc100k"
        )

      dd <-
        wide |>
        filter(
          fdi_spatial_support %in%
            TRUE,
          !is.na(
            .data[[
              early_var
            ]]
          ),
          !is.na(
            .data[[
              late_var
            ]]
          ),
          !is.na(
            .data[[
              delta_var
            ]]
          )
        )

      tibble(
        sector =
          sector_name,

        n_ac =
          nrow(
            dd
          ),

        n_increase =
          sum(
            dd[[
              delta_var
            ]] >
              0
          ),

        n_zero_change =
          sum(
            dd[[
              delta_var
            ]] ==
              0
          ),

        n_decrease =
          sum(
            dd[[
              delta_var
            ]] <
              0
          ),

        zero_change_share =
          mean(
            dd[[
              delta_var
            ]] ==
              0
          ),

        early_mean =
          mean(
            dd[[
              early_var
            ]]
          ),

        late_mean =
          mean(
            dd[[
              late_var
            ]]
          ),

        delta_mean =
          mean(
            dd[[
              delta_var
            ]]
          ),

        delta_sd =
          sd(
            dd[[
              delta_var
            ]]
          ),

        early_late_correlation =
          cor(
            dd[[
              early_var
            ]],
            dd[[
              late_var
            ]]
          )
      )
    }
  )

source_window_summary <-
  exposure_windowed |>
  distinct(
    fdi_project_uid,
    window,
    sector
  ) |>
  count(
    window,
    sector,
    name =
      "n_unique_projects"
  ) |>
  arrange(
    window,
    sector
  )

write_csv(
  population_denominator_diagnostics,
  file.path(
    output_dir,
    "00_population_denominator_diagnostics.csv"
  )
)

write_csv(
  window_registry,
  file.path(
    output_dir,
    "01_window_definition.csv"
  )
)

write_csv(
  wide,
  file.path(
    output_dir,
    "02_fdi_ac_12m.csv"
  )
)

saveRDS(
  wide,
  file.path(
    output_dir,
    "03_fdi_ac_12m.rds"
  )
)

write_csv(
  source_window_summary,
  file.path(
    output_dir,
    "04_source_project_counts.csv"
  )
)

write_csv(
  support_diagnostics,
  file.path(
    output_dir,
    "05_local_support_diagnostics.csv"
  )
)

write_csv(
  nesting_checks,
  file.path(
    output_dir,
    "06_60m_nesting_checks.csv"
  )
)

write_csv(
  window_project_counts,
  file.path(
    output_dir,
    "07_window_sector_scope_project_counts.csv"
  )
)

notes <-
  c(
    "POST-PRIMARY 12-MONTH FDI TEMPORAL-WINDOW ROBUSTNESS",
    "",
    "This script does not alter the canonical FDI spatial assignment.",
    "It aggregates the canonical fdi_project_exposure.csv intermediate artifact.",
    "",
    "Early window: 2008-04-01 through 2009-03-31.",
    "Late window: 2013-04-01 through 2014-03-31.",
    "Both are exactly 12 months and exclude the election month beginning in April.",
    "",
    "Change is late12 minus early12.",
    "",
    "The output contains Total, Manufacturing, and Services FDI for own, adjacent, and local exposure scopes.",
    "Raw project counts, projects per 100,000, log1p rates, raw changes, and log1p differences are retained.",
    "",
    "For R30, the intended compact robustness specification uses the RAW LOCAL 12-month change plus the corresponding early12 baseline interaction.",
    "",
    "This 12-month window is a post-estimation temporal robustness analysis, not part of the original primary design.",
    "",
    "QA requires:",
    "each window contains exactly 12 months;",
    "Total = Manufacturing + Services;",
    "Local = Own + Adjacent;",
    "12-month counts are nested within the corresponding canonical 60-month election-period counts."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "08_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "09_session_info.txt"
  )
)

cat(
  "\n===== POPULATION DENOMINATOR DIAGNOSTICS =====\n"
)

print(
  population_denominator_diagnostics,
  n = Inf,
  width = Inf
)

cat(
  "\n===== 12-MONTH WINDOW DEFINITIONS =====\n"
)

print(
  window_registry,
  n = Inf,
  width = Inf
)

cat(
  "\n===== SOURCE PROJECT COUNTS =====\n"
)

print(
  source_window_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== LOCAL 12-MONTH SUPPORT DIAGNOSTICS =====\n"
)

print(
  support_diagnostics,
  n = Inf,
  width = Inf
)

cat(
  "\n===== 60-MONTH NESTING CHECKS =====\n"
)

print(
  nesting_checks,
  n = Inf,
  width = Inf
)

cat(
  "\nR30A_FDI_12M_TEMPORAL_ROBUSTNESS_COMPLETE\n"
)

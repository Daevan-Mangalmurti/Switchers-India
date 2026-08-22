# ============================================================
# 07_prepare_wir_sector_shocks.R
#
# PRE-ESTIMATION construction of external sector shocks from
# UNCTAD World Investment Report 2026 Annex Tables 17 and 18.
#
# This script DOES NOT estimate a political first stage or second stage.
#
# It:
#   1. reads WIR Annex 17 (destination counts) and Annex 18
#      (world sector/industry counts), 2003-2025;
#   2. audits overlap between the project's India fDi Markets extract
#      and WIR India totals;
#   3. constructs ex-ante world sector growth shocks;
#   4. freezes a conservative SHRIC -> WIR industry crosswalk;
#   5. if 06_iv_design_prep_ec05.R has already been run, constructs
#      AC-level Bartik/shift-share instruments but DOES NOT use outcomes.
#
# Primary external shock:
#
#   g_k = log(1 + mean(projects_k, 2010:2013))
#         - log(1 + mean(projects_k, 2005:2008))
#
# Primary manufacturing Bartik:
#
#   Z_i^MFG = sum_{k in manufacturing SHRICs}
#                 s_ik,2005 * g_{map(k)}
#
# where s_ik is the 2005 SHRIC employment share of total non-farm
# employment in AC i.
#
# IMPORTANT:
#   WIR Annex 18 provides WORLD sector counts, not an India-by-sector
#   matrix. Therefore the primary instrument is explicitly labelled
#   WORLD-INCLUSIVE, not rest-of-world. Annex 17 is used to quantify
#   India's total share of world projects over the relevant periods.
#
#   If a complete fDi Markets export by destination AND sector becomes
#   available, the preferred upgrade is a rest-of-world shock.
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

if (
  !requireNamespace(
    "readxl",
    quietly = TRUE
  )
) {
  stop(
    "Package 'readxl' is required for the WIR XLSX files. ",
    "Install once with install.packages('readxl')."
  )
}

paths <- build_project_paths(
  project_root
)

WIR_SHOCK_REVISION <-
  "2026-08-09-v1.0.1-fdi-path-hotfix"

message(
  "Starting WIR sector-shock preparation: ",
  WIR_SHOCK_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

candidate_wir_dirs <- c(
  file.path(
    project_root,
    "data",
    "wir"
  ),
  file.path(
    project_root,
    "data",
    "raw",
    "wir"
  ),
  file.path(
    project_root,
    "data",
    "raw"
  ),
  project_root
)

find_one_file <- function(
    filename
) {
  candidates <- file.path(
    candidate_wir_dirs,
    filename
  )

  found <- candidates[
    file.exists(
      candidates
    )
  ]

  if (
    length(found) == 0
  ) {
    stop(
      "Could not find ",
      filename,
      ". Checked: ",
      paste(
        candidates,
        collapse = "; "
      )
    )
  }

  found[[1]]
}

wir17_path <- find_one_file(
  "wir26_tab17.xlsx"
)

wir18_path <- find_one_file(
  "wir26_tab18.xlsx"
)

# The project's build script records this source as
# data/IN_FDI_2004_2014.csv. Check that canonical location first, then
# retain the alternative layouts used in earlier exploratory scripts.
fdi_paths <- c(
  file.path(
    project_root,
    "data",
    "IN_FDI_2004_2014.csv"
  ),
  file.path(
    project_root,
    "data",
    "fdi",
    "IN_FDI_2004_2014.csv"
  ),
  file.path(
    project_root,
    "data",
    "raw",
    "IN_FDI_2004_2014.csv"
  ),
  file.path(
    project_root,
    "IN_FDI_2004_2014.csv"
  )
)

fdi_path <- fdi_paths[
  file.exists(
    fdi_paths
  )
][1]

# Last-resort project-local recursive discovery. This is intentionally
# limited to the project tree and requires an exact filename.
if (
  length(
    fdi_path
  ) == 0 ||
  is.na(
    fdi_path
  )
) {
  recursive_matches <- list.files(
    project_root,
    pattern =
      "^IN_FDI_2004_2014\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (
    length(
      recursive_matches
    ) == 1
  ) {
    fdi_path <-
      recursive_matches[[1]]
  } else if (
    length(
      recursive_matches
    ) > 1
  ) {
    stop(
      "Found multiple copies of IN_FDI_2004_2014.csv: ",
      paste(
        recursive_matches,
        collapse = "; "
      ),
      ". Keep one canonical source or set the script path explicitly."
    )
  }
}

if (
  length(
    fdi_path
  ) == 0 ||
  is.na(
    fdi_path
  )
) {
  stop(
    "Could not locate IN_FDI_2004_2014.csv anywhere under project root: ",
    project_root
  )
}

message(
  "India fDi Markets source: ",
  fdi_path
)

out_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep",
  "wir_shocks"
)

out_table_dir <- file.path(
  out_root,
  "tables"
)

out_manifest_dir <- file.path(
  out_root,
  "manifests"
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
    out_data_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. READ WIR ANNEX TABLES
# ============================================================

wir17 <- readxl::read_excel(
  wir17_path,
  skip = 2
) |>
  dplyr::rename(
    destination =
      1
  )

wir18 <- readxl::read_excel(
  wir18_path,
  skip = 2
) |>
  dplyr::rename(
    wir_industry =
      1
  )

year_cols <- as.character(
  2003:2025
)

missing17 <- setdiff(
  year_cols,
  names(
    wir17
  )
)

missing18 <- setdiff(
  year_cols,
  names(
    wir18
  )
)

if (
  length(
    missing17
  ) > 0 ||
  length(
    missing18
  ) > 0
) {
  stop(
    "WIR tables do not expose all expected 2003-2025 year columns."
  )
}

wir17 <- wir17 |>
  dplyr::filter(
    !is.na(
      destination
    ),
    !stringr::str_starts(
      as.character(
        destination
      ),
      "Source:"
    ),
    !stringr::str_starts(
      as.character(
        destination
      ),
      "Note:"
    )
  )

wir18 <- wir18 |>
  dplyr::filter(
    !is.na(
      wir_industry
    ),
    !stringr::str_starts(
      as.character(
        wir_industry
      ),
      "Source:"
    ),
    !stringr::str_starts(
      as.character(
        wir_industry
      ),
      "Note:"
    )
  )

# ============================================================
# 2. WIR WORLD/INDIA DESTINATION AUDIT
# ============================================================

wir_world_india <- wir17 |>
  dplyr::filter(
    destination %in%
      c(
        "World",
        "India"
      )
  ) |>
  tidyr::pivot_longer(
    cols =
      dplyr::all_of(
        year_cols
      ),
    names_to =
      "year",
    values_to =
      "projects"
  ) |>
  dplyr::mutate(
    year =
      as.integer(
        year
      ),
    projects =
      as.numeric(
        projects
      )
  ) |>
  tidyr::pivot_wider(
    names_from =
      destination,
    values_from =
      projects
  ) |>
  dplyr::mutate(
    india_share_world =
      India /
      World
  ) |>
  dplyr::arrange(
    year
  )

readr::write_csv(
  wir_world_india,
  file.path(
    out_table_dir,
    "wir_world_india_project_counts_2003_2025.csv"
  )
)

wir_period_share_audit <- wir_world_india |>
  dplyr::mutate(
    period =
      dplyr::case_when(
        year %in%
          2005:2008 ~
          "Pre: 2005-2008",

        year %in%
          2010:2013 ~
          "Post: 2010-2013",

        TRUE ~
          NA_character_
      )
  ) |>
  dplyr::filter(
    !is.na(
      period
    )
  ) |>
  dplyr::group_by(
    period
  ) |>
  dplyr::summarise(
    mean_india_projects =
      mean(
        India
      ),

    mean_world_projects =
      mean(
        World
      ),

    india_share_of_world_using_period_means =
      mean_india_projects /
      mean_world_projects,

    mean_annual_india_share_world =
      mean(
        india_share_world
      ),

    .groups = "drop"
  )

readr::write_csv(
  wir_period_share_audit,
  file.path(
    out_manifest_dir,
    "01_wir_india_share_of_world_audit.csv"
  )
)

# ============================================================
# 3. PROJECT fDi MARKETS EXTRACT vs WIR INDIA TOTALS
# ============================================================

fdi_raw <- readr::read_csv(
  fdi_path,
  show_col_types = FALSE,
  progress = FALSE
)

required_fdi <- c(
  "Project date",
  "Sector",
  "Activity",
  "Project type",
  "Project status"
)

missing_fdi <- setdiff(
  required_fdi,
  names(
    fdi_raw
  )
)

if (
  length(
    missing_fdi
  ) > 0
) {
  stop(
    "India fDi Markets extract is missing: ",
    paste(
      missing_fdi,
      collapse = ", "
    )
  )
}

fdi <- fdi_raw |>
  dplyr::mutate(
    project_date =
      as.Date(
        paste0(
          "01 ",
          `Project date`
        ),
        format =
          "%d %b %Y"
      ),

    year =
      as.integer(
        format(
          project_date,
          "%Y"
        )
      )
  )

project_type_audit <- fdi |>
  dplyr::count(
    `Project type`,
    name =
      "n_projects"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      n_projects
    )
  )

readr::write_csv(
  project_type_audit,
  file.path(
    out_manifest_dir,
    "02_india_fdi_project_type_audit.csv"
  )
)

india_extract_annual <- fdi |>
  dplyr::filter(
    year %in%
      2003:2025
  ) |>
  dplyr::count(
    year,
    name =
      "india_extract_projects"
  )

india_wir_overlap <- wir_world_india |>
  dplyr::left_join(
    india_extract_annual,
    by =
      "year"
  ) |>
  dplyr::mutate(
    india_extract_projects =
      dplyr::coalesce(
        india_extract_projects,
        0L
      ),

    extract_share_of_wir_india =
      india_extract_projects /
      India
  )

readr::write_csv(
  india_wir_overlap,
  file.path(
    out_table_dir,
    "india_fdi_extract_vs_wir_india_counts.csv"
  )
)

# ============================================================
# 4. CONSTRUCT WIR WORLD INDUSTRY SHOCKS
# ============================================================

aggregate_rows <- c(
  "Total",
  "Primary",
  "Manufacturing",
  "Services"
)

wir18_long <- wir18 |>
  tidyr::pivot_longer(
    cols =
      dplyr::all_of(
        year_cols
      ),
    names_to =
      "year",
    values_to =
      "projects"
  ) |>
  dplyr::mutate(
    year =
      as.integer(
        year
      ),
    projects =
      as.numeric(
        projects
      ),
    industry_level =
      dplyr::if_else(
        wir_industry %in%
          aggregate_rows,
        "parent/aggregate",
        "detailed"
      )
  )

make_shock <- function(
    data,
    pre_years,
    post_years,
    label
) {
  pre <- data |>
    dplyr::filter(
      year %in%
        pre_years
    ) |>
    dplyr::group_by(
      wir_industry,
      industry_level
    ) |>
    dplyr::summarise(
      pre_mean =
        mean(
          projects,
          na.rm = TRUE
        ),
      .groups = "drop"
    )

  post <- data |>
    dplyr::filter(
      year %in%
        post_years
    ) |>
    dplyr::group_by(
      wir_industry,
      industry_level
    ) |>
    dplyr::summarise(
      post_mean =
        mean(
          projects,
          na.rm = TRUE
        ),
      .groups = "drop"
    )

  pre |>
    dplyr::inner_join(
      post,
      by = c(
        "wir_industry",
        "industry_level"
      )
    ) |>
    dplyr::mutate(
      shock_definition =
        label,

      shock_log_change =
        log1p(
          post_mean
        ) -
        log1p(
          pre_mean
        ),

      shock_proportional_change =
        dplyr::if_else(
          pre_mean >
            0,
          post_mean /
            pre_mean -
            1,
          NA_real_
        ),

      pre_years =
        paste(
          range(
            pre_years
          ),
          collapse = "-"
        ),

      post_years =
        paste(
          range(
            post_years
          ),
          collapse = "-"
        )
    )
}

shock_primary <- make_shock(
  wir18_long,
  2005:2008,
  2010:2013,
  "PRIMARY: log change in mean annual world projects, 2005-08 -> 2010-13"
)

shock_sensitivity_5yr <- make_shock(
  wir18_long,
  2004:2008,
  2010:2013,
  "Sensitivity: log change in mean annual world projects, 2004-08 -> 2010-13"
)

shock_table <- dplyr::bind_rows(
  shock_primary,
  shock_sensitivity_5yr
)

readr::write_csv(
  shock_table,
  file.path(
    out_table_dir,
    "wir_world_sector_shocks.csv"
  )
)

# ============================================================
# 5. FREEZE A CONSERVATIVE SHRIC -> WIR CROSSWALK
# ============================================================
#
# Principle:
#   - Use detailed WIR industries where the SHRIC description is clear.
#   - For genuinely mixed SHRIC supergroups, use the WIR parent-sector
#     shock rather than arbitrarily splitting employment across detailed
#     industries.
#   - Parent-mapped employment is explicitly quantified and can be
#     excluded in a sensitivity instrument.
#
# Particularly important:
#   SHRIC 72 is a harmonization catch-all containing paper, rubber/plastic,
#   fabricated metals, machinery, electrical/electronic equipment,
#   motor vehicles, ships, aircraft, instruments, etc. It is therefore
#   mapped to the parent "Manufacturing" shock, not forced into one
#   detailed WIR industry.

shric_desc_paths <- c(
  file.path(
    paths$derived_dir,
    "model_exploration",
    "iv_design_prep",
    "tables",
    "shric_catalog_90_industries.csv"
  )
)

if (
  !file.exists(
    shric_desc_paths[[1]]
  )
) {
  stop(
    "Run 06_iv_design_prep_ec05.R first. Missing SHRIC catalog: ",
    shric_desc_paths[[1]]
  )
}

shric_catalog <- readr::read_csv(
  shric_desc_paths[[1]],
  show_col_types = FALSE,
  progress = FALSE
)

map_wir_industry <- function(
    shric
) {
  dplyr::case_when(
    shric %in%
      c(
        1L,
        2L
      ) ~
      "Agriculture, forestry and fishing",

    shric %in%
      c(
        3L,
        4L
      ) ~
      "Extractive industries",

    shric %in%
      5:12 ~
      "Food, beverages and tobacco",

    shric %in%
      13:16 ~
      "Textiles, clothing and leather",

    shric ==
      17L ~
      "Wood products",

    shric ==
      18L ~
      "Printing",

    shric %in%
      c(
        19L,
        20L
      ) ~
      "Coke and refined petroleum",

    shric ==
      21L ~
      "Pharmaceuticals",

    shric %in%
      c(
        22L,
        23L
      ) ~
      "Chemicals",

    shric ==
      24L ~
      "Other non-metallic mineral products",

    shric %in%
      25:27 ~
      "Basic metal and metal products",

    # Domestic appliances are not cleanly separable into the WIR
    # electronics vs machinery buckets in the harmonized SHRIC.
    shric ==
      28L ~
      "Manufacturing",

    shric ==
      29L ~
      "Electronics and electrical equipment",

    # SHRIC 30 is non-motor-vehicle transport equipment under NIC04
    # (motorcycles, bicycles, other transport equipment), so use the
    # broad manufacturing parent rather than WIR "Automotive".
    shric ==
      30L ~
      "Manufacturing",

    shric ==
      31L ~
      "Furniture",

    shric ==
      32L ~
      "Other manufacturing",

    shric %in%
      c(
        33L,
        34L
      ) ~
      "Energy and gas supply",

    shric ==
      35L ~
      "Water and waste management services",

    shric %in%
      36:38 ~
      "Construction",

    shric %in%
      39:50 ~
      "Trade",

    shric %in%
      51:52 ~
      "Hospitality",

    shric %in%
      53:59 ~
      "Transportation and storage",

    shric ==
      60L ~
      "Administrative and support services",

    shric %in%
      61:63 ~
      "Transportation and storage",

    shric ==
      64L ~
      "Information and communication",

    shric %in%
      65:68 ~
      "Finance and insurance",

    shric ==
      69L ~
      "Real estate",

    shric %in%
      c(
        70L,
        71L
      ) ~
      "Administrative and support services",

    shric ==
      72L ~
      "Manufacturing",

    shric ==
      73L ~
      "Information and communication",

    shric %in%
      74:78 ~
      "Professional services",

    shric ==
      79L ~
      "Administrative and support services",

    shric ==
      80L ~
      "Education",

    shric ==
      81L ~
      "Health services",

    # Veterinary services do not map cleanly into the WIR detailed
    # categories; retain the parent service shock.
    shric ==
      82L ~
      "Services",

    shric ==
      83L ~
      "Health services",

    shric ==
      84L ~
      "Water and waste management services",

    shric ==
      85L ~
      "Other services",

    shric ==
      86L ~
      "Entertainment",

    # SHRIC 87 combines management/architecture with call centres and
    # cleaning; use parent services.
    shric ==
      87L ~
      "Services",

    shric ==
      88L ~
      "Entertainment",

    # SHRIC 89 combines broadcasting/publishing with gambling/sports.
    shric ==
      89L ~
      "Services",

    shric ==
      90L ~
      "Other services",

    TRUE ~
      NA_character_
  )
}

shric_wir_crosswalk <- shric_catalog |>
  dplyr::mutate(
    shric =
      as.integer(
        shric
      ),

    wir_industry =
      map_wir_industry(
        shric
      ),

    mapping_specificity =
      dplyr::case_when(
        shric %in%
          c(
            28L,
            30L,
            72L,
            82L,
            87L,
            89L
          ) ~
          "parent-sector",

        TRUE ~
          "detailed"
      ),

    mapping_confidence =
      dplyr::case_when(
        shric %in%
          c(
            72L,
            87L,
            89L
          ) ~
          "low specificity, defensible parent mapping",

        shric %in%
          c(
            28L,
            30L,
            82L
          ) ~
          "medium specificity, conservative parent mapping",

        TRUE ~
          "high"
      ),

    manufacturing_shric =
      shric %in%
        c(
          5:32,
          72L
        )
  )

if (
  any(
    is.na(
      shric_wir_crosswalk$wir_industry
    )
  )
) {
  stop(
    "At least one SHRIC has no WIR mapping."
  )
}

if (
  any(
    !shric_wir_crosswalk$wir_industry %in%
      wir18$wir_industry
  )
) {
  bad <- unique(
    shric_wir_crosswalk$wir_industry[
      !shric_wir_crosswalk$wir_industry %in%
        wir18$wir_industry
    ]
  )

  stop(
    "SHRIC crosswalk refers to WIR rows absent from Annex 18: ",
    paste(
      bad,
      collapse = ", "
    )
  )
}

readr::write_csv(
  shric_wir_crosswalk,
  file.path(
    out_table_dir,
    "shric_to_wir_industry_crosswalk_FROZEN_PREOUTCOME.csv"
  )
)

# ============================================================
# 6. CONSTRUCT AC BARTIK INSTRUMENTS FROM 06 OUTPUT
# ============================================================

iv_base_path <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep",
  "data",
  "ac_iv_base_ec05_shares.rds"
)

if (
  !file.exists(
    iv_base_path
  )
) {
  stop(
    "Run 06_iv_design_prep_ec05.R before 07. Missing: ",
    iv_base_path
  )
}

iv_base <- readRDS(
  iv_base_path
)

primary_shocks <- shock_primary |>
  dplyr::select(
    wir_industry,
    shock_log_change
  )

crosswalk_shocks <- shric_wir_crosswalk |>
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
      crosswalk_shocks$shock_log_change
    )
  )
) {
  stop(
    "Some SHRICs failed to obtain the primary WIR shock."
  )
}

# Instrument contributions use employment shares of TOTAL non-farm
# employment, the standard Bartik exposure-share convention.
iv_base$bartik_wir_world_total_logchg <-
  0

iv_base$bartik_wir_world_mfg_logchg <-
  0

iv_base$bartik_wir_world_mfg_detailed_only_logchg <-
  0

iv_base$ec05_parent_mapped_share_all <-
  0

iv_base$ec05_parent_mapped_share_mfg <-
  0

for (
  i in seq_len(
    nrow(
      crosswalk_shocks
    )
  )
) {
  k <-
    crosswalk_shocks$shric[[i]]

  share_var <- paste0(
    "ec05_share_shric_",
    k
  )

  if (
    !share_var %in%
      names(
        iv_base
      )
  ) {
    stop(
      "Missing EC05 share variable in IV base: ",
      share_var
    )
  }

  shock_i <-
    crosswalk_shocks$shock_log_change[[i]]

  share_i <-
    iv_base[[
      share_var
    ]]

  iv_base$bartik_wir_world_total_logchg <-
    iv_base$bartik_wir_world_total_logchg +
    share_i *
    shock_i

  if (
    crosswalk_shocks$manufacturing_shric[[i]]
  ) {
    iv_base$bartik_wir_world_mfg_logchg <-
      iv_base$bartik_wir_world_mfg_logchg +
      share_i *
      shock_i

    if (
      crosswalk_shocks$mapping_specificity[[i]] ==
        "detailed"
    ) {
      iv_base$bartik_wir_world_mfg_detailed_only_logchg <-
        iv_base$bartik_wir_world_mfg_detailed_only_logchg +
        share_i *
        shock_i
    }
  }

  if (
    crosswalk_shocks$mapping_specificity[[i]] ==
      "parent-sector"
  ) {
    iv_base$ec05_parent_mapped_share_all <-
      iv_base$ec05_parent_mapped_share_all +
      share_i

    if (
      crosswalk_shocks$manufacturing_shric[[i]]
    ) {
      iv_base$ec05_parent_mapped_share_mfg <-
        iv_base$ec05_parent_mapped_share_mfg +
        share_i
    }
  }
}

# If EC05 is absent, row arithmetic above leaves NAs if any share is NA.
# Make that status explicit.
iv_base <- iv_base |>
  dplyr::mutate(
    bartik_wir_world_total_logchg =
      dplyr::if_else(
        is.na(
          ec05_emp_all
        ),
        NA_real_,
        bartik_wir_world_total_logchg
      ),

    bartik_wir_world_mfg_logchg =
      dplyr::if_else(
        is.na(
          ec05_emp_all
        ),
        NA_real_,
        bartik_wir_world_mfg_logchg
      ),

    bartik_wir_world_mfg_detailed_only_logchg =
      dplyr::if_else(
        is.na(
          ec05_emp_all
        ),
        NA_real_,
        bartik_wir_world_mfg_detailed_only_logchg
      ),

    wir_shock_source =
      "UNCTAD WIR 2026 Annex 18; world-inclusive fDi Markets project counts",

    wir_shock_primary_pre_period =
      "2005-2008",

    wir_shock_primary_post_period =
      "2010-2013"
  )

instrument_audit <- iv_base |>
  dplyr::summarise(
    n_ac =
      dplyr::n(),

    n_bartik_nonmissing =
      sum(
        !is.na(
          bartik_wir_world_mfg_logchg
        )
      ),

    median_bartik_mfg =
      stats::median(
        bartik_wir_world_mfg_logchg,
        na.rm = TRUE
      ),

    sd_bartik_mfg =
      stats::sd(
        bartik_wir_world_mfg_logchg,
        na.rm = TRUE
      ),

    min_bartik_mfg =
      min(
        bartik_wir_world_mfg_logchg,
        na.rm = TRUE
      ),

    max_bartik_mfg =
      max(
        bartik_wir_world_mfg_logchg,
        na.rm = TRUE
      ),

    median_parent_mapped_mfg_share =
      stats::median(
        ec05_parent_mapped_share_mfg,
        na.rm = TRUE
      ),

    p90_parent_mapped_mfg_share =
      stats::quantile(
        ec05_parent_mapped_share_mfg,
        0.90,
        na.rm = TRUE,
        names = FALSE
      )
  )

readr::write_csv(
  instrument_audit,
  file.path(
    out_manifest_dir,
    "03_bartik_construction_audit.csv"
  )
)

readr::write_rds(
  iv_base,
  file.path(
    out_data_dir,
    "ac_iv_base_with_wir_world_shocks.rds"
  )
)

readr::write_csv(
  iv_base |>
    dplyr::select(
      ac_uid,
      state_no,
      pc_cluster_id,
      ec05_emp_all,
      bartik_wir_world_mfg_logchg,
      bartik_wir_world_mfg_detailed_only_logchg,
      bartik_wir_world_total_logchg,
      ec05_parent_mapped_share_mfg,
      ec05_parent_mapped_share_all,
      dplyr::everything()
    ),
  file.path(
    out_data_dir,
    "ac_iv_base_with_wir_world_shocks.csv.gz"
  )
)

# ============================================================
# 7. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "WIR sector-shock preparation revision: ",
      WIR_SHOCK_REVISION
    ),
    "",
    "PRIMARY SHOCK",
    "-------------",
    "g_k = log(1 + mean annual WORLD greenfield-project count in 2010-2013)",
    "      - log(1 + mean annual WORLD greenfield-project count in 2005-2008).",
    "",
    "WHY WORLD-INCLUSIVE",
    "-------------------",
    "WIR Annex 18 supplies world counts by sector/industry but not India-by-sector counts.",
    "WIR Annex 17 supplies India totals only. Therefore the instrument is deliberately labelled",
    "world-inclusive, not rest-of-world. Do not subtract the project's India extract sector-by-sector:",
    "the project extract contains only Project type == New and covers a smaller project universe",
    "than WIR India totals.",
    "",
    "CROSSWALK PRINCIPLE",
    "-------------------",
    "Clear SHRIC industries use detailed WIR shocks.",
    "Mixed harmonization supergroups use the parent Manufacturing or Services shock.",
    "A detailed-only manufacturing instrument is saved as a pre-specified sensitivity.",
    "",
    "IMPORTANT SHRIC 72 ISSUE",
    "------------------------",
    "SHRIC 72 is a broad manufacturing catch-all and is too heterogeneous for a detailed WIR mapping.",
    "The primary mapping therefore assigns it the overall Manufacturing shock rather than an arbitrary",
    "automotive/electronics/machinery allocation.",
    "",
    "NO OUTCOME MODELS",
    "-----------------",
    "This script constructs the instrument but does not estimate its first-stage relevance or any BJP outcome.",
    "Those diagnostics belong in the next stage and should be evaluated before political second stages.",
    "",
    "READ FIRST",
    "----------",
    "manifests/01_wir_india_share_of_world_audit.csv",
    "manifests/02_india_fdi_project_type_audit.csv",
    "tables/india_fdi_extract_vs_wir_india_counts.csv",
    "tables/wir_world_sector_shocks.csv",
    "tables/shric_to_wir_industry_crosswalk_FROZEN_PREOUTCOME.csv",
    "manifests/03_bartik_construction_audit.csv",
    "data/ac_iv_base_with_wir_world_shocks.rds"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "WIR sector-shock preparation COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No first-stage or BJP outcome model has been estimated."
)

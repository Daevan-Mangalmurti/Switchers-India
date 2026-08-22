# ============================================================
# 10_official_fdi_policy_exposure_audit.R
#
# PRE-OUTCOME audit of India FDI-policy reforms during the 2009-2014
# electoral exposure window.
#
# Purpose:
#   Before constructing a policy IV, establish whether the actual policy
#   changes can be mapped cleanly enough to:
#     (a) 2005 EC05/SHRIC industrial structure, and
#     (b) the fDi Markets treatment universe used in this project.
#
# This script intentionally DOES NOT estimate a political outcome and DOES
# NOT force a single arbitrary "policy score."
#
# It freezes:
#   - an official reform manifest based on DIPP/DPIIT Press Notes and PIB;
#   - conservative SHRIC mappings;
#   - fDi Markets sector-mapping feasibility;
#   - AC-level baseline exposure shares for clean and broad policy groups.
#
# Key methodological rule:
#   Cap changes, route changes, new investor eligibility, and state-option
#   reforms are NOT mechanically treated as equivalent. They remain separate
#   policy dimensions unless a later first-stage design has a defensible
#   reason to combine them.
#
# Official sources used to freeze the manifest:
#   - DIPP Press Note 1 (2012): Single-Brand Retail, 10 Jan 2012.
#   - PIB 20 Sep 2012 notification page for Press Notes 4-8 (2012):
#       Single-Brand Retail, Multi-Brand Retail, Civil Aviation,
#       Broadcasting, Power Exchanges.
#   - DIPP Press Note 6 (2013), issued 22 Aug 2013:
#       Tea, petroleum refining by PSUs, defence, courier, telecom,
#       test marketing, single-brand retail, ARCs, commodity exchanges,
#       CICs, securities-market infrastructure, power exchanges.
#   - DIPP Press Notes 1 and 2 (2014): pharmaceuticals and insurance.
#
# IMPORTANT:
#   The project FDI exposure window ends at 2014-04-01. Policy changes after
#   that date are outside the treatment window and are not used here.
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

POLICY_AUDIT_REVISION <-
  "2026-08-09-v1.0-official-policy-feasibility-audit"

message(
  "Starting official FDI-policy exposure audit: ",
  POLICY_AUDIT_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

iv_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep"
)

ec05_base_path <- file.path(
  iv_root,
  "data",
  "ac_iv_base_ec05_shares.rds"
)

shric_catalog_path <- file.path(
  iv_root,
  "tables",
  "shric_catalog_90_industries.csv"
)

purrr::walk(
  c(
    ec05_base_path,
    shric_catalog_path
  ),
  function(p) {
    if (
      !file.exists(
        p
      )
    ) {
      stop(
        "Missing IV-prep input: ",
        p,
        ". Run corrected 06 first."
      )
    }
  }
)

out_root <- file.path(
  iv_root,
  "official_policy_audit"
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

# Locate the exact fDi Markets file used by the local project.
fdi_candidates <- c(
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

fdi_path <- fdi_candidates[
  file.exists(
    fdi_candidates
  )
][1]

if (
  length(
    fdi_path
  ) ==
    0 ||
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
    ) ==
      1
  ) {
    fdi_path <-
      recursive_matches[[1]]
  } else {
    stop(
      "Could not uniquely locate IN_FDI_2004_2014.csv."
    )
  }
}

message(
  "fDi Markets source used for policy audit: ",
  fdi_path
)

# ============================================================
# 1. FREEZE OFFICIAL POLICY-REFORM MANIFEST
# ============================================================
#
# eligible_clean_policy_exposure:
#   TRUE only where there is a reasonably clean mapping to a SHRIC sector
#   and the reform applies nationally enough to support a first-stage audit.
#
# eligible_direct_manufacturing_iv:
#   TRUE only if the reform cleanly targets manufacturing activity comparable
#   to the project's manufacturing FDI treatment. We intentionally apply a
#   high bar here.

policy_manifest <- tibble::tribble(
  ~reform_id,
  ~effective_date,
  ~official_source,
  ~policy_sector,
  ~change_type,
  ~cap_before,
  ~cap_after,
  ~route_before,
  ~route_after,
  ~state_option,
  ~clean_shric,
  ~shric_mapping_confidence,
  ~clean_fdi_market_sector,
  ~fdi_market_mapping_confidence,
  ~eligible_clean_policy_exposure,
  ~eligible_direct_manufacturing_iv,
  ~reason,

  "2012_SBRT_CAP",
  as.Date("2012-01-10"),
  "DIPP Press Note 1 (2012 Series)",
  "Single-brand product retail trading",
  "cap increase",
  "51%",
  "100%",
  "Government",
  "Government",
  FALSE,
  "47;48;49",
  "medium",
  NA_character_,
  "low",
  FALSE,
  FALSE,
  "Retail employment is broad; policy applies only to single-brand retail and fDi Markets sector labels do not identify single-brand retail cleanly.",

  "2012_SBRT_CONDITIONS",
  as.Date("2012-09-20"),
  "DIPP Press Note 4 (2012 Series)",
  "Single-brand product retail trading",
  "conditions/eligibility clarification",
  "100%",
  "100%",
  "Government",
  "Government",
  FALSE,
  "47;48;49",
  "medium",
  NA_character_,
  "low",
  FALSE,
  FALSE,
  "No clean cap/route shock and single-brand status is not observed in the project FDI extract.",

  "2012_MBRT_OPEN",
  as.Date("2012-09-20"),
  "DIPP Press Note 5 (2012 Series)",
  "Multi-brand retail trading",
  "sector opened",
  "prohibited",
  "51%",
  "prohibited",
  "Government",
  TRUE,
  "47;48;49",
  "medium",
  NA_character_,
  "low",
  FALSE,
  FALSE,
  "Potentially strong reform but implementation is state/UT opt-in and state adoption is politically endogenous; not suitable as a simple national shock.",

  "2012_CIVIL_AVIATION",
  as.Date("2012-09-20"),
  "DIPP Press Note 6 (2012 Series)",
  "Scheduled and non-scheduled air transport",
  "foreign-airline eligibility",
  "foreign airlines not permitted",
  "49%",
  "not permitted for foreign airlines",
  "Government",
  FALSE,
  "58",
  "high",
  "Transportation & Warehousing",
  "low",
  TRUE,
  FALSE,
  "SHRIC air transport is clean, but the fDi Markets sector label is much broader than air transport.",

  "2012_BROADCASTING",
  as.Date("2012-09-20"),
  "DIPP Press Note 7 (2012 Series)",
  "Broadcasting carriage services",
  "cap/route review",
  NA_character_,
  NA_character_,
  NA_character_,
  NA_character_,
  FALSE,
  "89",
  "low",
  "Communications",
  "low",
  FALSE,
  FALSE,
  "SHRIC 89 mixes broadcasting with publishing, gambling and sports; fDi Communications is broader.",

  "2012_POWER_EXCHANGES",
  as.Date("2012-09-20"),
  "DIPP Press Note 8 (2012 Series)",
  "Power exchanges",
  "new foreign-investment dispensation",
  "no specific dispensation",
  "49% foreign investment (26% FDI + 23% FII)",
  "no specific dispensation",
  "Government for FDI",
  FALSE,
  NA_character_,
  "none",
  NA_character_,
  "none",
  FALSE,
  FALSE,
  "Power exchanges are market infrastructure, not electricity generation; mapping to SHRIC 33 would be substantively wrong.",

  "2013_TEA",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Tea sector including tea plantations",
  "condition removal",
  "100%",
  "100%",
  "Government",
  "Government",
  FALSE,
  NA_character_,
  "none",
  "Food and Beverages",
  "low",
  FALSE,
  FALSE,
  "Reform removes compulsory divestment but EC05 non-farm SHRIC structure does not cleanly identify tea plantations.",

  "2013_PETROLEUM_PSU",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Petroleum refining by PSUs without disinvestment/dilution",
  "route liberalization",
  "49%",
  "49%",
  "Government",
  "Automatic",
  FALSE,
  "20",
  "high for industry; low for PSU restriction",
  "Coal, oil & gas",
  "low",
  FALSE,
  FALSE,
  "The reform applies only to PSU refining under specific ownership conditions; EC05/fDi data do not isolate that policy-eligible subset.",

  "2013_DEFENCE",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Defence industry",
  "case-by-case above-cap eligibility",
  "26%",
  "26% generally; above 26% possible for state-of-art technology",
  "Government",
  "Government/CCS",
  FALSE,
  "30;72",
  "low",
  "Space & defence",
  "high",
  FALSE,
  FALSE,
  "fDi sector is clean but EC05 SHRIC 30/72 are much broader than defence production; no clean local employment exposure measure.",

  "2013_COURIER",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Courier services",
  "route liberalization",
  "100%",
  "100%",
  "Government",
  "Automatic",
  FALSE,
  "63",
  "high",
  "Transportation & Warehousing",
  "low",
  TRUE,
  FALSE,
  "Clean EC05 courier exposure, but the project FDI sector label is broader than courier.",

  "2013_TELECOM",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Telecom services",
  "cap increase",
  "74%",
  "100%",
  "Automatic to 49%; Government 49-74%",
  "Automatic to 49%; Government above 49%",
  FALSE,
  "64",
  "high",
  "Communications",
  "high",
  TRUE,
  FALSE,
  "This is the cleanest policy-to-EC05-to-fDi Markets mapping, but it is a services reform and occurs only about seven months before the project's FDI window closes.",

  "2013_TEST_MARKETING",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Test marketing linked to manufacturing",
  "provision deleted",
  "100%",
  NA_character_,
  "Government",
  NA_character_,
  FALSE,
  NA_character_,
  "none",
  NA_character_,
  "none",
  FALSE,
  FALSE,
  "Deleted provision is not a clean industry-specific liberalization shock.",

  "2013_SBRT_ROUTE",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Single-brand product retail trading",
  "route liberalization",
  "100%",
  "100%",
  "Government",
  "Automatic to 49%; Government above 49%",
  FALSE,
  "47;48;49",
  "medium",
  NA_character_,
  "low",
  FALSE,
  FALSE,
  "Broad retail employment does not identify single-brand retailers; no clean fDi Markets single-brand label.",

  "2013_ARC",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Asset reconstruction companies",
  "cap + route liberalization",
  "74%",
  "100%",
  "Government",
  "Automatic to 49%; Government above 49%",
  FALSE,
  "67",
  "low",
  "Financial services",
  "low",
  FALSE,
  FALSE,
  "SHRIC/fDi financial-services categories are much broader than ARCs.",

  "2013_COMMODITY_EXCHANGES",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Commodity exchanges",
  "route liberalization",
  "49%",
  "49%",
  "Government",
  "Automatic",
  FALSE,
  "67",
  "low",
  "Financial services",
  "low",
  FALSE,
  FALSE,
  "Broad financial categories do not identify commodity exchanges.",

  "2013_CIC",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Credit information companies",
  "cap + route liberalization",
  "49%",
  "74%",
  "Government",
  "Automatic",
  FALSE,
  "67",
  "low",
  "Financial services",
  "low",
  FALSE,
  FALSE,
  "Broad financial categories do not identify credit-information companies.",

  "2013_SECURITIES_INFRA",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Stock exchanges, depositories and clearing corporations",
  "route liberalization",
  "49%",
  "49%",
  "Government",
  "Automatic",
  FALSE,
  "67",
  "low",
  "Financial services",
  "low",
  FALSE,
  FALSE,
  "Broad financial categories do not identify securities-market infrastructure.",

  "2013_POWER_EXCHANGE_ROUTE",
  as.Date("2013-08-22"),
  "DIPP Press Note 6 (2013 Series)",
  "Power exchanges",
  "route liberalization",
  "49%",
  "49%",
  "Government",
  "Automatic",
  FALSE,
  NA_character_,
  "none",
  NA_character_,
  "none",
  FALSE,
  FALSE,
  "No defensible mapping to electricity-generation employment.",

  "2014_PHARMA_REVIEW",
  as.Date("2014-01-08"),
  "DIPP Press Note 1 (2014 Series)",
  "Pharmaceuticals",
  "brownfield condition review",
  "Greenfield 100%; Brownfield 100%",
  "No cap change",
  "Greenfield Automatic; Brownfield Government",
  "No route change",
  FALSE,
  "21",
  "high",
  "Pharmaceuticals",
  "high",
  FALSE,
  FALSE,
  "No cap or entry-route liberalization; policy mainly adds a non-compete condition for brownfield cases.",

  "2014_INSURANCE_SCOPE",
  as.Date("2014-02-04"),
  "DIPP Press Note 2 (2014 Series)",
  "Insurance and specified intermediaries",
  "scope clarification",
  "26%",
  "26%",
  "Automatic",
  "Automatic",
  FALSE,
  "66;68",
  "medium",
  "Financial services",
  "low",
  FALSE,
  FALSE,
  "No cap/route increase and less than two months remain before the project's 2014 FDI exposure cutoff."
)

readr::write_csv(
  policy_manifest,
  file.path(
    out_table_dir,
    "01_official_fdi_policy_reform_manifest_2009_2014.csv"
  )
)

# ============================================================
# 2. CONSERVATIVE SHRIC POLICY CROSSWALK
# ============================================================

shric_catalog <- readr::read_csv(
  shric_catalog_path,
  show_col_types = FALSE,
  progress = FALSE
)

policy_shric_map <- tibble::tribble(
  ~shric, ~policy_family, ~mapping_confidence, ~primary_clean_policy_exposure, ~direct_manufacturing_policy, ~note,

  47L, "Retail trading", "medium", FALSE, FALSE, "Broad retail; cannot distinguish single-brand or multi-brand status.",
  48L, "Retail trading", "medium", FALSE, FALSE, "Broad retail; cannot distinguish single-brand or multi-brand status.",
  49L, "Retail trading", "medium", FALSE, FALSE, "Broad retail; cannot distinguish single-brand or multi-brand status.",

  58L, "Civil aviation", "high", TRUE, FALSE, "Direct match to air transport.",
  63L, "Courier services", "high", TRUE, FALSE, "Direct match to courier activities.",
  64L, "Telecom services", "high", TRUE, FALSE, "Direct match to telecoms.",

  20L, "Petroleum refining by PSU", "conditional", FALSE, FALSE, "Industry match is good but PSU/ownership restriction is not observable.",
  30L, "Defence", "low", FALSE, FALSE, "Transport equipment is much broader than defence.",
  72L, "Defence", "low", FALSE, FALSE, "Broad equipment/manufacturing catch-all is much broader than defence.",

  66L, "Insurance", "medium", FALSE, FALSE, "Insurance is identifiable but 2014 reform did not increase cap/route.",
  68L, "Insurance intermediaries", "medium", FALSE, FALSE, "Auxiliary insurance is identifiable but 2014 reform did not increase cap/route.",

  67L, "Financial-market infrastructure", "low", FALSE, FALSE, "Financial services is much broader than ARC/CIC/exchange infrastructure.",
  89L, "Broadcasting", "low", FALSE, FALSE, "SHRIC combines broadcasting with publishing, gambling and sports."
) |>
  dplyr::left_join(
    shric_catalog,
    by =
      "shric",
    relationship =
      "many-to-one"
  )

readr::write_csv(
  policy_shric_map,
  file.path(
    out_table_dir,
    "02_shric_to_policy_reform_crosswalk.csv"
  )
)

# ============================================================
# 3. AC-LEVEL BASELINE POLICY EXPOSURE AUDIT
# ============================================================

ac <- readRDS(
  ec05_base_path
)

share_var <- function(
    k
) {
  paste0(
    "ec05_share_shric_",
    k
  )
}

required_shares <- share_var(
  unique(
    policy_shric_map$shric
  )
)

missing_shares <- setdiff(
  required_shares,
  names(
    ac
  )
)

if (
  length(
    missing_shares
  ) >
    0
) {
  stop(
    "EC05 base is missing policy-mapped SHRIC shares: ",
    paste(
      missing_shares,
      collapse = ", "
    )
  )
}

ac_policy <- ac |>
  dplyr::mutate(
    policy_exposure_air_transport_2005 =
      .data[[
        share_var(
          58
        )
      ]],

    policy_exposure_courier_2005 =
      .data[[
        share_var(
          63
        )
      ]],

    policy_exposure_telecom_2005 =
      .data[[
        share_var(
          64
        )
      ]],

    # Descriptive only. This is NOT a quantitative policy IV score:
    # it merely reports total baseline employment in the three cleanly
    # mapped national reform sectors.
    policy_exposure_clean_three_sector_share_2005 =
      policy_exposure_air_transport_2005 +
      policy_exposure_courier_2005 +
      policy_exposure_telecom_2005,

    policy_exposure_retail_broad_2005 =
      .data[[
        share_var(
          47
        )
      ]] +
      .data[[
        share_var(
          48
        )
      ]] +
      .data[[
        share_var(
          49
        )
      ]],

    policy_exposure_finance_broad_2005 =
      .data[[
        share_var(
          65
        )
      ]] +
      .data[[
        share_var(
          66
        )
      ]] +
      .data[[
        share_var(
          67
        )
      ]] +
      .data[[
        share_var(
          68
        )
      ]],

    policy_exposure_petroleum_refining_2005 =
      .data[[
        share_var(
          20
        )
      ]],

    policy_exposure_transport_equipment_2005 =
      .data[[
        share_var(
          30
        )
      ]],

    policy_exposure_broad_equipment_shric72_2005 =
      .data[[
        share_var(
          72
        )
      ]],

    policy_exposure_broadcast_mixed_2005 =
      .data[[
        share_var(
          89
        )
      ]]
  )

exposure_vars <- c(
  "policy_exposure_air_transport_2005",
  "policy_exposure_courier_2005",
  "policy_exposure_telecom_2005",
  "policy_exposure_clean_three_sector_share_2005",
  "policy_exposure_retail_broad_2005",
  "policy_exposure_finance_broad_2005",
  "policy_exposure_petroleum_refining_2005",
  "policy_exposure_transport_equipment_2005",
  "policy_exposure_broad_equipment_shric72_2005",
  "policy_exposure_broadcast_mixed_2005"
)

exposure_summary <- purrr::map_dfr(
  exposure_vars,
  function(v) {
    x <- ac_policy[[
      v
    ]]

    x <- x[
      !is.na(
        ac_policy$ec05_emp_all
      )
    ]

    tibble::tibble(
      exposure_variable =
        v,
      n =
        sum(
          !is.na(
            x
          )
        ),
      mean =
        mean(
          x,
          na.rm = TRUE
        ),
      sd =
        stats::sd(
          x,
          na.rm = TRUE
        ),
      median =
        stats::median(
          x,
          na.rm = TRUE
        ),
      p90 =
        stats::quantile(
          x,
          0.90,
          na.rm = TRUE,
          names = FALSE
        ),
      share_nonzero =
        mean(
          x >
            0,
          na.rm = TRUE
        )
    )
  }
)

readr::write_csv(
  exposure_summary,
  file.path(
    out_table_dir,
    "03_ec05_policy_exposure_summary.csv"
  )
)

readr::write_rds(
  ac_policy,
  file.path(
    out_data_dir,
    "ac_policy_exposure_audit_base.rds"
  )
)

readr::write_csv(
  ac_policy |>
    dplyr::select(
      ac_uid,
      state_no,
      pc_cluster_id,
      dplyr::all_of(
        exposure_vars
      )
    ),
  file.path(
    out_data_dir,
    "ac_policy_exposure_audit_base.csv.gz"
  )
)

# ============================================================
# 4. fDi MARKETS TREATMENT-MAPPING FEASIBILITY
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
  ) >
    0
) {
  stop(
    "fDi Markets file is missing: ",
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

    policy_period =
      dplyr::case_when(
        project_date <
          as.Date(
            "2009-04-01"
          ) ~
          "baseline_2004_04_to_2009_03",

        project_date <
          as.Date(
            "2012-01-10"
          ) ~
          "post2009_before_2012_SBRT",

        project_date <
          as.Date(
            "2012-09-20"
          ) ~
          "2012_SBRT_cap_to_Sep_reforms",

        project_date <
          as.Date(
            "2013-08-22"
          ) ~
          "Sep2012_to_Aug2013",

        project_date <
          as.Date(
            "2014-04-01"
          ) ~
          "Aug2013_to_FDI_cutoff",

        TRUE ~
          "outside_project_2014_window"
      )
  )

fdi_sector_period <- fdi |>
  dplyr::filter(
    !is.na(
      project_date
    )
  ) |>
  dplyr::count(
    policy_period,
    Sector,
    name =
      "n_projects"
  ) |>
  dplyr::arrange(
    policy_period,
    dplyr::desc(
      n_projects
    )
  )

readr::write_csv(
  fdi_sector_period,
  file.path(
    out_table_dir,
    "04_fdi_markets_sector_counts_by_policy_period.csv"
  )
)

fdi_activity_period <- fdi |>
  dplyr::filter(
    !is.na(
      project_date
    )
  ) |>
  dplyr::count(
    policy_period,
    Activity,
    name =
      "n_projects"
  ) |>
  dplyr::arrange(
    policy_period,
    dplyr::desc(
      n_projects
    )
  )

readr::write_csv(
  fdi_activity_period,
  file.path(
    out_table_dir,
    "05_fdi_markets_activity_counts_by_policy_period.csv"
  )
)

# Candidate fDi-sector mappings. These are deliberately conservative.
fdi_policy_candidate_mapping <- tibble::tribble(
  ~policy_family, ~fdi_sector, ~mapping_confidence, ~usable_as_clean_policy_treatment, ~reason,

  "Telecom services",
  "Communications",
  "high",
  TRUE,
  "Best direct match between policy sector and fDi Markets sector label.",

  "Civil aviation",
  "Transportation & Warehousing",
  "low",
  FALSE,
  "fDi sector mixes aviation, warehousing and other transportation/logistics.",

  "Courier services",
  "Transportation & Warehousing",
  "low",
  FALSE,
  "fDi sector is much broader than courier services.",

  "Defence",
  "Space & defence",
  "high",
  FALSE,
  "fDi label is clean, but EC05 local exposure cannot isolate defence from much broader transport/equipment SHRICs.",

  "Petroleum refining by PSU",
  "Coal, oil & gas",
  "low",
  FALSE,
  "fDi label includes extraction and private projects and does not identify PSU refining eligibility.",

  "Broadcasting",
  "Communications",
  "low",
  FALSE,
  "Communications is much broader than broadcasting.",

  "Financial-market infrastructure",
  "Financial services",
  "low",
  FALSE,
  "Financial services is much broader than exchanges/ARCs/CICs.",

  "Retail trading",
  NA_character_,
  "none",
  FALSE,
  "fDi Markets industry sector does not identify single-brand/multi-brand retail status."
)

readr::write_csv(
  fdi_policy_candidate_mapping,
  file.path(
    out_table_dir,
    "06_policy_to_fdi_markets_sector_mapping.csv"
  )
)

# Count the cleanest project-side category around its actual reform date.
telecom_timing_audit <- fdi |>
  dplyr::filter(
    Sector ==
      "Communications",
    project_date >=
      as.Date(
        "2009-04-01"
      ),
    project_date <
      as.Date(
        "2014-04-01"
      )
  ) |>
  dplyr::mutate(
    relative_to_2013_telecom_reform =
      dplyr::if_else(
        project_date <
          as.Date(
            "2013-08-22"
          ),
        "before_2013_08_22",
        "after_2013_08_22_to_2014_03_31"
      )
  ) |>
  dplyr::count(
    relative_to_2013_telecom_reform,
    name =
      "n_communications_projects"
  )

readr::write_csv(
  telecom_timing_audit,
  file.path(
    out_manifest_dir,
    "01_telecom_project_timing_audit.csv"
  )
)

# ============================================================
# 5. FEASIBILITY GATES
# ============================================================

n_clean_policy_rows <- policy_manifest |>
  dplyr::filter(
    eligible_clean_policy_exposure
  ) |>
  nrow()

n_clean_mfg_policy_rows <- policy_manifest |>
  dplyr::filter(
    eligible_direct_manufacturing_iv
  ) |>
  nrow()

n_clean_project_mappings <- fdi_policy_candidate_mapping |>
  dplyr::filter(
    usable_as_clean_policy_treatment
  ) |>
  nrow()

clean_policy_sectors <- policy_manifest |>
  dplyr::filter(
    eligible_clean_policy_exposure
  ) |>
  dplyr::pull(
    policy_sector
  )

feasibility <- tibble::tibble(
  criterion =
    c(
      "National reforms with clean EC05/SHRIC exposure mapping",
      "National reforms cleanly targeting the project's manufacturing FDI treatment",
      "Policy sectors with clean fDi Markets project-sector mapping",
      "Clean national policy exposure sectors"
    ),

  value =
    c(
      as.character(
        n_clean_policy_rows
      ),
      as.character(
        n_clean_mfg_policy_rows
      ),
      as.character(
        n_clean_project_mappings
      ),
      paste(
        clean_policy_sectors,
        collapse = " | "
      )
    ),

  interpretation =
    c(
      "These are candidate exposure dimensions for a policy first-stage audit; they should remain separate rather than be combined with arbitrary weights.",
      "A value of zero means the official 2012-13 reform package does not furnish a clean direct instrument for manufacturing FDI using the current EC05/fDi classifications.",
      "A low count means even a services-policy IV may require narrower project classifications than the current fDi Markets export provides.",
      "Clean EC05 mappings are air transport, courier, and telecom; all are service-sector reforms."
    )
)

readr::write_csv(
  feasibility,
  file.path(
    out_table_dir,
    "00_HEADLINE_policy_iv_feasibility.csv"
  )
)

# ============================================================
# 6. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Official FDI-policy audit revision: ",
      POLICY_AUDIT_REVISION
    ),
    "",
    "THIS SCRIPT DOES NOT USE BJP/POLITICAL OUTCOMES.",
    "",
    "CORE FINDING BUILT INTO THE AUDIT LOGIC",
    "---------------------------------------",
    "Do not assume the 2012-13 liberalizations form a clean manufacturing-FDI instrument.",
    "The reforms are concentrated in retail and services, and many have sector definitions that",
    "cannot be mapped cleanly to either 2005 SHRIC employment or the fDi Markets project-sector labels.",
    "",
    "HIGH-CONFIDENCE EC05 POLICY EXPOSURES",
    "-------------------------------------",
    "SHRIC 58: air transport",
    "SHRIC 63: courier services",
    "SHRIC 64: telecoms",
    "",
    "These should remain separate policy exposures. A later first-stage script may test them jointly",
    "for service/total FDI, but should not invent an arbitrary common policy weight.",
    "",
    "MANUFACTURING WARNING",
    "---------------------",
    "The 2013 petroleum change is PSU-specific, defence cannot be isolated cleanly in EC05 SHRICs,",
    "and retail reforms are not manufacturing-FDI reforms. Therefore the current official reform set",
    "does not automatically provide a defensible instrument for the project's preferred manufacturing FDI treatment.",
    "",
    "STATE-OPTION WARNING",
    "--------------------",
    "The 2012 multi-brand retail reform is deliberately excluded from the clean national exposure set",
    "because implementation was left to states/UTs. State adoption is a potentially endogenous political choice",
    "and would require a separate design rather than being folded into a national policy Bartik.",
    "",
    "READ FIRST",
    "----------",
    "tables/00_HEADLINE_policy_iv_feasibility.csv",
    "tables/01_official_fdi_policy_reform_manifest_2009_2014.csv",
    "tables/02_shric_to_policy_reform_crosswalk.csv",
    "tables/03_ec05_policy_exposure_summary.csv",
    "tables/04_fdi_markets_sector_counts_by_policy_period.csv",
    "tables/06_policy_to_fdi_markets_sector_mapping.csv",
    "manifests/01_telecom_project_timing_audit.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Official FDI-policy exposure audit COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No policy first stage or political outcome was estimated."
)

# ============================================================
# 06_iv_design_prep_ec05.R
#
# PRE-ESTIMATION preparation for a shift-share IV design.
#
# This script does NOT estimate the political IV.
# It:
#   1. reads SHRUG EC05 AC08 employment-by-SHRIC data;
#   2. validates the exact ac08_id -> project ac_uid crosswalk;
#   3. constructs 2005 local employment shares s_ik;
#   4. audits SHRUG geographic reconstruction quality and analysis coverage;
#   5. joins the shares to the existing AC change-design data;
#   6. writes a first-stage-ready base file awaiting external sector shocks g_k.
#
# The eventual Bartik/shift-share is:
#
#     Z_i = sum_k s_ik,2005 * g_k
#
# where g_k MUST come from an explicitly defended external sectoral shock
# source (e.g. global-minus-India greenfield FDI growth by industry).
#
# No political outcome is regressed on an instrument in this script.
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

IV_PREP_REVISION <-
  "2026-08-08-v1.0-ec05-share-prep"

message(
  "Starting EC05 shift-share preparation: ",
  IV_PREP_REVISION
)

# ============================================================
# 0. PATHS + ZIP DISCOVERY
# ============================================================

candidate_data_dirs <- c(
  file.path(
    project_root,
    "data",
    "shrug"
  ),
  file.path(
    project_root,
    "data",
    "raw",
    "shrug"
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
    candidate_data_dirs,
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

ec05_zip <- find_one_file(
  "shrug-ec05-dta.zip"
)

shric_desc_zip <- find_one_file(
  "shrug-shric-desc-dta.zip"
)

shric_nic04_zip <- find_one_file(
  "shrug-shric-nic04-dta.zip"
)

# NIC87 and NIC08 are not required to construct the 2005 shares, but retain
# them as optional harmonization resources for later external-shock mapping.
optional_zip <- function(
    filename
) {
  candidates <- file.path(
    candidate_data_dirs,
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
    return(
      NA_character_
    )
  }

  found[[1]]
}

shric_nic87_zip <- optional_zip(
  "shrug-shric-nic87-dta.zip"
)

shric_nic08_zip <- optional_zip(
  "shrug-shric-nic08-3d-dta.zip"
)

out_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep"
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
# 1. READ .DTA MEMBERS DIRECTLY FROM ZIPS VIA TEMP FILES
# ============================================================

read_dta_from_zip <- function(
    zip_path,
    member
) {
  listing <- utils::unzip(
    zip_path,
    list = TRUE
  )

  if (
    !member %in%
      listing$Name
  ) {
    stop(
      "Member ",
      member,
      " not found in ",
      zip_path,
      ". Available members: ",
      paste(
        listing$Name,
        collapse = ", "
      )
    )
  }

  temp_dir <- tempfile(
    pattern =
      "switchers_zip_"
  )

  dir.create(
    temp_dir,
    recursive = TRUE
  )

  on.exit(
    unlink(
      temp_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  utils::unzip(
    zip_path,
    files =
      member,
    exdir =
      temp_dir
  )

  haven::read_dta(
    file.path(
      temp_dir,
      member
    )
  )
}

ec05 <- read_dta_from_zip(
  ec05_zip,
  "ec05_con08.dta"
)

shric_desc <- read_dta_from_zip(
  shric_desc_zip,
  "shric_descriptions.dta"
)

shric_nic04 <- read_dta_from_zip(
  shric_nic04_zip,
  "shric_NIC04_key.dta"
)

if (
  !is.na(
    shric_nic87_zip
  )
) {
  shric_nic87 <- read_dta_from_zip(
    shric_nic87_zip,
    "shric_NIC87_key.dta"
  )
} else {
  shric_nic87 <- NULL
}

if (
  !is.na(
    shric_nic08_zip
  )
) {
  shric_nic08 <- read_dta_from_zip(
    shric_nic08_zip,
    "shric_NIC08_3d_key.dta"
  )
} else {
  shric_nic08 <- NULL
}

# ============================================================
# 2. HARD STRUCTURE CHECKS
# ============================================================

sector_emp_vars <- paste0(
  "ec05_emp_shric_",
  1:90
)

required_ec05_vars <- c(
  "ac08_id",
  "ec05_emp_all",
  "ec05_emp_manuf",
  "ec05_emp_services",
  sector_emp_vars,
  "_mean_p_miss",
  "_core_p_miss",
  "_target_weight_share",
  "_target_group_max_weight_share",
  "_frag_pop_share"
)

missing_ec05 <- setdiff(
  required_ec05_vars,
  names(
    ec05
  )
)

if (
  length(
    missing_ec05
  ) > 0
) {
  stop(
    "EC05 AC08 file is missing required variables: ",
    paste(
      missing_ec05,
      collapse = ", "
    )
  )
}

if (
  nrow(
    shric_desc
  ) != 90 ||
  !all(
    sort(
      unique(
        as.integer(
          shric_desc$shric
        )
      )
    ) ==
      1:90
  )
) {
  stop(
    "Expected exactly 90 SHRIC descriptions numbered 1:90."
  )
}

if (
  anyDuplicated(
    ec05$ac08_id
  ) >
    0
) {
  stop(
    "ec05_con08.dta is not unique on ac08_id."
  )
}

# ============================================================
# 3. EXACT AC08 -> PROJECT AC_UID CROSSWALK
# ============================================================

ec05 <- ec05 |>
  dplyr::mutate(
    ac08_id =
      as.character(
        ac08_id
      ),

    ac_uid =
      ac08_id |>
      stringr::str_remove(
        "^2008-"
      ) |>
      stringr::str_replace_all(
        "-",
        "_"
      )
  )

ac08_key_paths <- c(
  file.path(
    project_root,
    "data",
    "shrug",
    "ac08_name_key.csv"
  ),
  file.path(
    project_root,
    "data",
    "crosswalks",
    "ac08_name_key.csv"
  ),
  file.path(
    project_root,
    "ac08_name_key.csv"
  )
)

ac08_key_path <- ac08_key_paths[
  file.exists(
    ac08_key_paths
  )
][1]

if (
  length(
    ac08_key_path
  ) == 0 ||
  is.na(
    ac08_key_path
  )
) {
  stop(
    "Could not locate ac08_name_key.csv."
  )
}

ac08_key <- readr::read_csv(
  ac08_key_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::mutate(
    ac08_id =
      as.character(
        ac08_id
      ),

    ac_uid_expected =
      ac08_id |>
      stringr::str_remove(
        "^2008-"
      ) |>
      stringr::str_replace_all(
        "-",
        "_"
      )
  )

crosswalk_audit <- tibble::tibble(
  n_ec05_rows =
    nrow(
      ec05
    ),

  n_unique_ec05_ac08 =
    dplyr::n_distinct(
      ec05$ac08_id
    ),

  n_ac08_key_rows =
    nrow(
      ac08_key
    ),

  n_ec05_ids_found_in_key =
    sum(
      ec05$ac08_id %in%
        ac08_key$ac08_id
    ),

  share_ec05_ids_found_in_key =
    mean(
      ec05$ac08_id %in%
        ac08_key$ac08_id
    ),

  n_key_acs_with_ec05 =
    sum(
      ac08_key$ac08_id %in%
        ec05$ac08_id
    ),

  share_key_acs_with_ec05 =
    mean(
      ac08_key$ac08_id %in%
        ec05$ac08_id
    )
)

readr::write_csv(
  crosswalk_audit,
  file.path(
    out_manifest_dir,
    "01_ec05_ac08_crosswalk_audit.csv"
  )
)

if (
  crosswalk_audit$share_ec05_ids_found_in_key !=
    1
) {
  stop(
    "Not every EC05 ac08_id is present in ac08_name_key.csv."
  )
}

ec05 <- ec05 |>
  dplyr::left_join(
    ac08_key |>
      dplyr::select(
        ac08_id,
        ac08_name,
        pc01_state_id,
        pc01_state_name,
        pc01_district_id,
        pc01_district_name,
        ac_uid_expected
      ),
    by =
      "ac08_id",
    relationship =
      "many-to-one"
  )

if (
  any(
    ec05$ac_uid !=
      ec05$ac_uid_expected
  )
) {
  stop(
    "Derived ac_uid is inconsistent with ac08_name_key."
  )
}

# ============================================================
# 4. BUILD 2005 EMPLOYMENT SHARES
# ============================================================

for (
  k in 1:90
) {
  emp_var <- paste0(
    "ec05_emp_shric_",
    k
  )

  share_var <- paste0(
    "ec05_share_shric_",
    k
  )

  ec05[[
    share_var
  ]] <-
    dplyr::if_else(
      is.finite(
        ec05$ec05_emp_all
      ) &
      ec05$ec05_emp_all >
        0,
      as.numeric(
        ec05[[
          emp_var
        ]]
      ) /
        as.numeric(
          ec05$ec05_emp_all
        ),
      NA_real_
    )
}

share_vars <- paste0(
  "ec05_share_shric_",
  1:90
)

share_matrix <- as.matrix(
  dplyr::select(
    ec05,
    dplyr::all_of(
      share_vars
    )
  )
)

ec05$ec05_shric_share_sum <-
  rowSums(
    share_matrix,
    na.rm = TRUE
  )

ec05$ec05_shric_hhi <-
  rowSums(
    share_matrix^2,
    na.rm = TRUE
  )

ec05$ec05_max_shric_share <-
  apply(
    share_matrix,
    1,
    max,
    na.rm = TRUE
  )

ec05 <- ec05 |>
  dplyr::mutate(
    # Quality flags are descriptive/sensitivity flags, not automatic
    # exclusions from the primary IV.
    ec05_quality_high =
      `_core_p_miss` <=
        0.10 &
      `_target_weight_share` >=
        0.90,

    ec05_fragmentation_low =
      `_frag_pop_share` <=
        0.10
  )

share_sum_audit <- ec05 |>
  dplyr::summarise(
    n =
      dplyr::n(),

    min_share_sum =
      min(
        ec05_shric_share_sum,
        na.rm = TRUE
      ),

    median_share_sum =
      stats::median(
        ec05_shric_share_sum,
        na.rm = TRUE
      ),

    max_share_sum =
      max(
        ec05_shric_share_sum,
        na.rm = TRUE
      ),

    share_within_1e6_of_one =
      mean(
        abs(
          ec05_shric_share_sum -
          1
        ) <=
          1e-6
      ),

    share_within_0_001_of_one =
      mean(
        abs(
          ec05_shric_share_sum -
          1
        ) <=
          0.001
      )
  )

readr::write_csv(
  share_sum_audit,
  file.path(
    out_manifest_dir,
    "02_ec05_shric_share_sum_audit.csv"
  )
)

# ============================================================
# 5. QUALITY + STATE COVERAGE AUDITS
# ============================================================

ec05_quality_summary <- ec05 |>
  dplyr::summarise(
    n_ac =
      dplyr::n(),

    median_core_p_miss =
      stats::median(
        `_core_p_miss`,
        na.rm = TRUE
      ),

    p90_core_p_miss =
      stats::quantile(
        `_core_p_miss`,
        0.90,
        na.rm = TRUE,
        names = FALSE
      ),

    median_target_weight_share =
      stats::median(
        `_target_weight_share`,
        na.rm = TRUE
      ),

    share_target_weight_ge_0_90 =
      mean(
        `_target_weight_share` >=
          0.90,
        na.rm = TRUE
      ),

    median_fragmented_pop_share =
      stats::median(
        `_frag_pop_share`,
        na.rm = TRUE
      ),

    share_fragmented_pop_le_0_10 =
      mean(
        `_frag_pop_share` <=
          0.10,
        na.rm = TRUE
      ),

    share_quality_high =
      mean(
        ec05_quality_high,
        na.rm = TRUE
      ),

    share_fragmentation_low =
      mean(
        ec05_fragmentation_low,
        na.rm = TRUE
      ),

    median_shric_hhi =
      stats::median(
        ec05_shric_hhi,
        na.rm = TRUE
      ),

    median_max_shric_share =
      stats::median(
        ec05_max_shric_share,
        na.rm = TRUE
      )
  )

readr::write_csv(
  ec05_quality_summary,
  file.path(
    out_manifest_dir,
    "03_ec05_quality_summary.csv"
  )
)

state_key_coverage <- ac08_key |>
  dplyr::mutate(
    state_no =
      suppressWarnings(
        as.integer(
          stringr::str_match(
            ac08_id,
            "^2008-([0-9]+)-"
          )[
            ,
            2
          ]
        )
      ),

    ec05_present =
      ac08_id %in%
        ec05$ac08_id
  ) |>
  dplyr::group_by(
    state_no,
    pc01_state_name
  ) |>
  dplyr::summarise(
    n_ac_key =
      dplyr::n(),

    n_ec05 =
      sum(
        ec05_present
      ),

    ec05_coverage_share =
      mean(
        ec05_present
      ),

    .groups =
      "drop"
  ) |>
  dplyr::arrange(
    state_no
  )

readr::write_csv(
  state_key_coverage,
  file.path(
    out_manifest_dir,
    "04_ec05_state_coverage.csv"
  )
)

# ============================================================
# 6. SHRIC CATALOG + LONG EMPLOYMENT SHARE TABLE
# ============================================================

shric_catalog <- shric_desc |>
  dplyr::transmute(
    shric =
      as.integer(
        shric
      ),

    shric_desc =
      as.character(
        shric_desc
      )
  ) |>
  dplyr::arrange(
    shric
  )

readr::write_csv(
  shric_catalog,
  file.path(
    out_table_dir,
    "shric_catalog_90_industries.csv"
  )
)

readr::write_csv(
  shric_nic04,
  file.path(
    out_table_dir,
    "shric_nic04_concordance.csv"
  )
)

if (
  !is.null(
    shric_nic87
  )
) {
  readr::write_csv(
    shric_nic87,
    file.path(
      out_table_dir,
      "shric_nic87_concordance.csv"
    )
  )
}

if (
  !is.null(
    shric_nic08
  )
) {
  readr::write_csv(
    shric_nic08,
    file.path(
      out_table_dir,
      "shric_nic08_3d_concordance.csv"
    )
  )
}

ec05_long <- ec05 |>
  dplyr::select(
    ac08_id,
    ac_uid,
    ac08_name,
    pc01_state_id,
    pc01_state_name,
    ec05_emp_all,
    dplyr::all_of(
      sector_emp_vars
    ),
    dplyr::all_of(
      share_vars
    ),
    `_mean_p_miss`,
    `_core_p_miss`,
    `_target_weight_share`,
    `_target_group_max_weight_share`,
    `_frag_pop_share`,
    ec05_quality_high,
    ec05_fragmentation_low
  ) |>
  tidyr::pivot_longer(
    cols =
      dplyr::all_of(
        share_vars
      ),
    names_to =
      "share_variable",
    values_to =
      "employment_share_2005"
  ) |>
  dplyr::mutate(
    shric =
      as.integer(
        stringr::str_remove(
          share_variable,
          "^ec05_share_shric_"
        )
      )
  ) |>
  dplyr::left_join(
    shric_catalog,
    by =
      "shric",
    relationship =
      "many-to-one"
  ) |>
  dplyr::select(
    ac08_id,
    ac_uid,
    ac08_name,
    pc01_state_id,
    pc01_state_name,
    shric,
    shric_desc,
    employment_share_2005,
    ec05_emp_all,
    `_mean_p_miss`,
    `_core_p_miss`,
    `_target_weight_share`,
    `_target_group_max_weight_share`,
    `_frag_pop_share`,
    ec05_quality_high,
    ec05_fragmentation_low
  )

readr::write_csv(
  ec05_long,
  file.path(
    out_data_dir,
    "ec05_ac08_shric_employment_shares_long.csv.gz"
  )
)

# ============================================================
# 7. JOIN TO THE EXISTING AC CHANGE-DESIGN DATA
# ============================================================

ac_change <- readRDS(
  file.path(
    paths$final_dir,
    "ac_change.rds"
  )
)

ec05_payload <- ec05 |>
  dplyr::select(
    ac_uid,
    ac08_id,
    ac08_name,
    ec05_emp_all,
    ec05_emp_manuf,
    ec05_emp_services,
    dplyr::all_of(
      share_vars
    ),
    `_mean_p_miss`,
    `_core_p_miss`,
    `_target_weight_share`,
    `_target_group_max_weight_share`,
    `_frag_pop_share`,
    ec05_shric_share_sum,
    ec05_shric_hhi,
    ec05_max_shric_share,
    ec05_quality_high,
    ec05_fragmentation_low
  )

ac_iv_base <- ac_change |>
  dplyr::left_join(
    ec05_payload,
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

analysis_required <- c(
  "d_bjp_vote_share_2009_2014_pp",
  "bjp_vote_share_2014",
  "bjp_vote_share_2009",
  "state_no",
  "pc_cluster_id",
  "fdi_mfg_own_all_n_2014",
  "fdi_mfg_own_all_n_2009",
  "muslim_share_2001_dist_proxy",
  "mig_total_upto_2001_share_ac_pop",
  "proxy_ac_pop",
  "con08_land_area",
  "sc_pop_share",
  "st_pop_share"
)

missing_analysis_vars <- setdiff(
  analysis_required,
  names(
    ac_iv_base
  )
)

if (
  length(
    missing_analysis_vars
  ) > 0
) {
  stop(
    "AC change data are missing IV-prep variables: ",
    paste(
      missing_analysis_vars,
      collapse = ", "
    )
  )
}

analysis_coverage <- ac_iv_base |>
  dplyr::mutate(
    ec05_available =
      !is.na(
        ec05_emp_all
      ),

    preferred_outcome_context_complete =
      dplyr::if_all(
        dplyr::all_of(
          analysis_required
        ),
        ~!is.na(
          .x
        )
      ),

    preferred_complete_with_ec05 =
      preferred_outcome_context_complete &
      ec05_available,

    preferred_complete_with_high_quality_ec05 =
      preferred_complete_with_ec05 &
      ec05_quality_high &
      ec05_fragmentation_low
  ) |>
  dplyr::summarise(
    n_ac_change =
      dplyr::n(),

    n_ec05_available =
      sum(
        ec05_available
      ),

    share_ec05_available =
      mean(
        ec05_available
      ),

    n_preferred_context_complete =
      sum(
        preferred_outcome_context_complete
      ),

    n_preferred_complete_with_ec05 =
      sum(
        preferred_complete_with_ec05
      ),

    share_preferred_context_retained_by_ec05 =
      sum(
        preferred_complete_with_ec05
      ) /
        sum(
          preferred_outcome_context_complete
        ),

    n_preferred_complete_high_quality_ec05 =
      sum(
        preferred_complete_with_high_quality_ec05
      ),

    share_ec05_sample_high_quality =
      sum(
        preferred_complete_with_high_quality_ec05
      ) /
        sum(
          preferred_complete_with_ec05
        )
  )

readr::write_csv(
  analysis_coverage,
  file.path(
    out_manifest_dir,
    "05_iv_analysis_sample_coverage.csv"
  )
)

readr::write_rds(
  ac_iv_base,
  file.path(
    out_data_dir,
    "ac_iv_base_ec05_shares.rds"
  )
)

readr::write_csv(
  ac_iv_base |>
    dplyr::select(
      ac_uid,
      state_no,
      pc_cluster_id,
      d_bjp_vote_share_2009_2014_pp,
      bjp_vote_share_2009,
      bjp_vote_share_2014,
      fdi_mfg_own_all_n_2009,
      fdi_mfg_own_all_n_2014,
      muslim_share_2001_dist_proxy,
      mig_total_upto_2001_share_ac_pop,
      proxy_ac_pop,
      con08_land_area,
      sc_pop_share,
      st_pop_share,
      ac08_id,
      ec05_emp_all,
      dplyr::all_of(
        share_vars
      ),
      `_core_p_miss`,
      `_target_weight_share`,
      `_frag_pop_share`,
      ec05_quality_high,
      ec05_fragmentation_low
    ),
  file.path(
    out_data_dir,
    "ac_iv_base_ec05_shares.csv.gz"
  )
)

# ============================================================
# 8. SHOCK-CROSSWALK TEMPLATE
# ============================================================

# Do not guess the UNCTAD/WIR industry crosswalk before the actual Annex 18
# industry labels are imported. This template is intentionally blank.
shock_crosswalk_template <- shric_catalog |>
  dplyr::mutate(
    external_shock_source =
      "UNCTAD WIR Annex 18 candidate",
    external_industry_label =
      NA_character_,
    mapping_weight =
      NA_real_,
    mapping_status =
      "TO REVIEW AFTER WIR ANNEX 18 IMPORT",
    mapping_note =
      NA_character_
  )

readr::write_csv(
  shock_crosswalk_template,
  file.path(
    out_table_dir,
    "shric_to_external_fdi_industry_crosswalk_TEMPLATE.csv"
  )
)

# ============================================================
# 9. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "IV design-prep revision: ",
      IV_PREP_REVISION
    ),
    "",
    "WHAT THIS FILE ESTABLISHES",
    "--------------------------",
    "EC05 provides predetermined 2005 non-farm employment by 90 harmonized SHRIC industries at AC08 geography.",
    "ac08_id converts deterministically to project ac_uid by removing '2008-' and replacing '-' with '_'.",
    "The script hard-checks every EC05 ac08_id against the project's ac08_name_key.csv.",
    "",
    "WHAT THIS FILE DOES NOT DO",
    "--------------------------",
    "It does not construct an instrument until an external sector-shock file and a reviewed SHRIC-to-shock-industry crosswalk exist.",
    "It does not estimate a first stage or second stage.",
    "",
    "PROPOSED INSTRUMENT",
    "-------------------",
    "Z_i = sum_k [2005 employment share in SHRIC k at AC i] * [external FDI shock for industry k].",
    "",
    "PREFERRED SHOCK PRINCIPLE",
    "-------------------------",
    "Use counts rather than project value as the lead shock because the project's treatment is a project count and many fDi Markets capital values are estimated.",
    "Prefer a global-minus-India industry shock if WIR/fDi Markets classification and India counts can be reconciled.",
    "If only annual WIR data are available, use a temporally clean 2010-2013 or other pre-election annual window rather than incorporating post-election 2014 activity.",
    "",
    "QUALITY",
    "-------",
    "SHRUG itself recommends robustness to Economic Census outliers and geographic reconstruction quality.",
    "The quality flags written here are sensitivity flags; they are not automatic primary exclusions.",
    "",
    "READ FIRST",
    "----------",
    "manifests/01_ec05_ac08_crosswalk_audit.csv",
    "manifests/03_ec05_quality_summary.csv",
    "manifests/04_ec05_state_coverage.csv",
    "manifests/05_iv_analysis_sample_coverage.csv",
    "tables/shric_catalog_90_industries.csv",
    "data/ac_iv_base_ec05_shares.rds",
    "tables/shric_to_external_fdi_industry_crosswalk_TEMPLATE.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "EC05 shift-share preparation COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No IV has been estimated yet."
)

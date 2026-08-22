# ============================================================
# 08b_iv_local_first_stage_diagnostics.R
#
# MATCHED LOCAL first-stage diagnostics for the proposed EC05 x WIR
# shift-share IV.
#
# This is a companion to 08_iv_first_stage_diagnostics.R.
# It DOES NOT estimate any BJP/political second stage.
#
# Treatment geography:
#   local FDI = own AC + all touching ACs
#
# Instrument geography:
#   pool 2005 EC05 employment over the SAME own+touching-AC
#   neighborhood, calculate local pre-treatment industry shares, then
#
#       Z_i^local = sum_k s_ik,2005^local * g_k^WIR
#
# This is preferable to instrumenting local FDI with the focal AC's own
# Bartik because the treatment and instrument then refer to the same local
# economic neighborhood.
#
# Strict primary local sample:
#   every AC in the focal AC's own+touching neighborhood must have EC05
#   employment data. Thus the Bartik covers the full geography used by the
#   local FDI treatment.
#
# High-quality sensitivity:
#   every EC05 member in that neighborhood must satisfy the existing
#   EC05 high-quality + low-fragmentation flags.
#
# Preferred local first-stage controls:
#   state FE
#   baseline local manufacturing FDI (2004-04-01 to 2009-04-01)
#   log(1 + pooled 2005 local non-farm employment)
#   pooled 2005 local manufacturing employment share
#   log(1 + pooled neighborhood land area)
#   number of touching AC neighbors
#
# Outputs:
#   - hierarchical and detailed-only matched-local first stages
#   - local pre-period placebo
#   - leave-one-WIR-industry-out local first stages
#   - local instrument concentration
#   - own-vs-local headline comparison, if 08 outputs are present
#
# No political outcome is referenced anywhere in this script.
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

LOCAL_FIRST_STAGE_REVISION <-
  "2026-08-09-v1.0-matched-local-first-stage"

message(
  "Starting matched-local IV first-stage diagnostics: ",
  LOCAL_FIRST_STAGE_REVISION
)

# ============================================================
# 0. PATHS
# ============================================================

iv_root <- file.path(
  paths$derived_dir,
  "model_exploration",
  "iv_design_prep"
)

wir_root <- file.path(
  iv_root,
  "wir_shocks"
)

iv_data_path <- file.path(
  wir_root,
  "data",
  "ac_iv_base_with_wir_world_shocks.rds"
)

crosswalk_path <- file.path(
  wir_root,
  "tables",
  "shric_to_wir_industry_crosswalk_FROZEN_PREOUTCOME.csv"
)

shock_path <- file.path(
  wir_root,
  "tables",
  "wir_world_sector_shocks.csv"
)

purrr::walk(
  c(
    iv_data_path,
    crosswalk_path,
    shock_path
  ),
  function(p) {
    if (
      !file.exists(
        p
      )
    ) {
      stop(
        "Missing required input: ",
        p,
        ". Run corrected 06 and 07 first."
      )
    }
  }
)

out_root <- file.path(
  iv_root,
  "local_first_stage_diagnostics"
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

out_data_dir <- file.path(
  out_root,
  "data"
)

purrr::walk(
  c(
    out_root,
    out_table_dir,
    out_figure_dir,
    out_manifest_dir,
    out_data_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD IV INPUTS
# ============================================================

d <- readRDS(
  iv_data_path
)

crosswalk <- readr::read_csv(
  crosswalk_path,
  show_col_types = FALSE,
  progress = FALSE
)

shock_table <- readr::read_csv(
  shock_path,
  show_col_types = FALSE,
  progress = FALSE
)

required <- c(
  "ac_uid",
  "state_no",
  "pc_cluster_id",
  "con08_land_area",
  "ec05_emp_all",
  "ec05_emp_manuf",
  "ec05_quality_high",
  "ec05_fragmentation_low",
  "fdi_mfg_own_all_n_2009",
  "fdi_mfg_own_all_n_2014",
  "fdi_mfg_adjacent_all_n_2009",
  "fdi_mfg_adjacent_all_n_2014",
  "fdi_mfg_local_all_n_2009",
  "fdi_mfg_local_all_n_2014"
)

missing_required <- setdiff(
  required,
  names(
    d
  )
)

if (
  length(
    missing_required
  ) > 0
) {
  stop(
    "Local first-stage base is missing: ",
    paste(
      missing_required,
      collapse = ", "
    )
  )
}

share_vars <- paste0(
  "ec05_share_shric_",
  1:90
)

missing_share_vars <- setdiff(
  share_vars,
  names(
    d
  )
)

if (
  length(
    missing_share_vars
  ) > 0
) {
  stop(
    "Missing EC05 SHRIC shares. Run corrected 06 first."
  )
}

share_completeness <- d |>
  dplyr::filter(
    !is.na(
      ec05_emp_all
    )
  ) |>
  dplyr::summarise(
    n_ec05 =
      dplyr::n(),

    n_with_any_missing_share =
      sum(
        !dplyr::if_all(
          dplyr::all_of(
            share_vars
          ),
          ~!is.na(
            .x
          )
        )
      )
  )

if (
  share_completeness$n_with_any_missing_share >
    0
) {
  stop(
    "EC05-observed rows still contain missing SHRIC shares. ",
    "Use 06_iv_design_prep_ec05_v1_0_1.R and ",
    "07_prepare_wir_sector_shocks_v1_0_2.R."
  )
}

# Confirm the project's local count is literally own + adjacent.
local_identity_audit <- d |>
  dplyr::summarise(
    n_2009_comparable =
      sum(
        !is.na(
          fdi_mfg_local_all_n_2009
        ) &
        !is.na(
          fdi_mfg_own_all_n_2009
        ) &
        !is.na(
          fdi_mfg_adjacent_all_n_2009
        )
      ),

    n_2009_mismatch =
      sum(
        !is.na(
          fdi_mfg_local_all_n_2009
        ) &
        !is.na(
          fdi_mfg_own_all_n_2009
        ) &
        !is.na(
          fdi_mfg_adjacent_all_n_2009
        ) &
        abs(
          fdi_mfg_local_all_n_2009 -
          fdi_mfg_own_all_n_2009 -
          fdi_mfg_adjacent_all_n_2009
        ) >
          1e-10
      ),

    n_2014_comparable =
      sum(
        !is.na(
          fdi_mfg_local_all_n_2014
        ) &
        !is.na(
          fdi_mfg_own_all_n_2014
        ) &
        !is.na(
          fdi_mfg_adjacent_all_n_2014
        )
      ),

    n_2014_mismatch =
      sum(
        !is.na(
          fdi_mfg_local_all_n_2014
        ) &
        !is.na(
          fdi_mfg_own_all_n_2014
        ) &
        !is.na(
          fdi_mfg_adjacent_all_n_2014
        ) &
        abs(
          fdi_mfg_local_all_n_2014 -
          fdi_mfg_own_all_n_2014 -
          fdi_mfg_adjacent_all_n_2014
        ) >
          1e-10
      )
  )

readr::write_csv(
  local_identity_audit,
  file.path(
    out_manifest_dir,
    "00_local_fdi_identity_audit.csv"
  )
)

if (
  local_identity_audit$n_2009_mismatch >
    0 ||
  local_identity_audit$n_2014_mismatch >
    0
) {
  stop(
    "Existing local manufacturing FDI does not exactly equal own + adjacent. ",
    "Do not build a matched local Bartik until the treatment definition is reconciled."
  )
}

# ============================================================
# 2. RECOVER THE PROJECT'S AC GEOMETRY
# ============================================================
#
# Preferred route: source the existing geography module and inspect the object
# returned by build_geography(). This keeps the IV neighborhood definition tied
# to the same geography source used by the project's FDI construction.
#
# Fallback route: directly read an India_AC shapefile under the project root.
#
# The script resolves AC IDs conservatively and stops if it cannot map geometry
# to the project's ac_uid with high confidence.

collect_sf_objects <- function(
    x,
    prefix =
      "geography"
) {
  out <- list()

  if (
    inherits(
      x,
      "sf"
    )
  ) {
    out[[
      prefix
    ]] <- x

    return(
      out
    )
  }

  if (
    is.list(
      x
    )
  ) {
    nm <- names(
      x
    )

    if (
      is.null(
        nm
      )
    ) {
      nm <- as.character(
        seq_along(
          x
        )
      )
    }

    for (
      i in seq_along(
        x
      )
    ) {
      child <- collect_sf_objects(
        x[[i]],
        paste0(
          prefix,
          "$",
          nm[[i]]
        )
      )

      out <- c(
        out,
        child
      )
    }
  }

  out
}

normalize_uid_candidate <- function(
    x
) {
  x <- as.character(
    x
  )

  out <- rep(
    NA_character_,
    length(
      x
    )
  )

  # Direct project style: 07_021
  direct <- stringr::str_match(
    x,
    "^\\s*([0-9]{1,2})[_-]([0-9]{1,3})\\s*$"
  )

  ok_direct <- !is.na(
    direct[
      ,
      1
    ]
  )

  out[
    ok_direct
  ] <- sprintf(
    "%02d_%03d",
    as.integer(
      direct[
        ok_direct,
        2
      ]
    ),
    as.integer(
      direct[
        ok_direct,
        3
      ]
    )
  )

  # SHRUG style: 2008-07-021
  shrug <- stringr::str_match(
    x,
    "^\\s*2008-([0-9]{1,2})-([0-9]{1,3})\\s*$"
  )

  ok_shrug <- !is.na(
    shrug[
      ,
      1
    ]
  )

  out[
    ok_shrug
  ] <- sprintf(
    "%02d_%03d",
    as.integer(
      shrug[
        ok_shrug,
        2
      ]
    ),
    as.integer(
      shrug[
        ok_shrug,
        3
      ]
    )
  )

  out
}

resolve_sf_ac_uid <- function(
    sf_obj,
    analysis_uids
) {
  nongeom <- sf::st_drop_geometry(
    sf_obj
  )

  # 1. Exact ac_uid.
  if (
    "ac_uid" %in%
      names(
        nongeom
      )
  ) {
    candidate <- normalize_uid_candidate(
      nongeom$ac_uid
    )

    overlap <- mean(
      candidate %in%
        analysis_uids,
      na.rm = TRUE
    )

    if (
      is.finite(
        overlap
      ) &&
      overlap >=
        0.90
    ) {
      return(
        list(
          uid =
            candidate,
          method =
            "existing ac_uid field",
          overlap =
            overlap
        )
      )
    }
  }

  # 2. Exact ac08_id.
  if (
    "ac08_id" %in%
      names(
        nongeom
      )
  ) {
    candidate <- normalize_uid_candidate(
      nongeom$ac08_id
    )

    overlap <- mean(
      candidate %in%
        analysis_uids,
      na.rm = TRUE
    )

    if (
      is.finite(
        overlap
      ) &&
      overlap >=
        0.90
    ) {
      return(
        list(
          uid =
            candidate,
          method =
            "existing ac08_id field",
          overlap =
            overlap
        )
      )
    }
  }

  # 3. Search any single field containing project/SHRUG-style IDs.
  best_single <- list(
    overlap =
      -Inf
  )

  for (
    v in names(
      nongeom
    )
  ) {
    candidate <- normalize_uid_candidate(
      nongeom[[
        v
      ]]
    )

    valid <- !is.na(
      candidate
    )

    if (
      sum(
        valid
      ) <
        100
    ) {
      next
    }

    overlap <- mean(
      candidate[
        valid
      ] %in%
        analysis_uids
    )

    if (
      is.finite(
        overlap
      ) &&
      overlap >
        best_single$overlap
    ) {
      best_single <- list(
        uid =
          candidate,
        method =
          paste0(
            "single field: ",
            v
          ),
        overlap =
          overlap
      )
    }
  }

  if (
    is.finite(
      best_single$overlap
    ) &&
    best_single$overlap >=
      0.90
  ) {
    return(
      best_single
    )
  }

  # 4. Search plausible state-code x AC-number pairs.
  state_candidates <- names(
    nongeom
  )[
    stringr::str_detect(
      stringr::str_to_lower(
        names(
          nongeom
        )
      ),
      "^(state_no|statecode|state_code|st_code|stcode|st_no|state)$"
    )
  ]

  ac_candidates <- names(
    nongeom
  )[
    stringr::str_detect(
      stringr::str_to_lower(
        names(
          nongeom
        )
      ),
      "^(ac|ac_no|ac_num|ac_number|ac_code|accode|assembly_no|assembly_number)$"
    )
  ]

  best_pair <- list(
    overlap =
      -Inf
  )

  for (
    sv in state_candidates
  ) {
    for (
      av in ac_candidates
    ) {
      state_num <- suppressWarnings(
        as.integer(
          as.character(
            nongeom[[
              sv
            ]]
          )
        )
      )

      ac_num <- suppressWarnings(
        as.integer(
          as.character(
            nongeom[[
              av
            ]]
          )
        )
      )

      candidate <- dplyr::if_else(
        !is.na(
          state_num
        ) &
        !is.na(
          ac_num
        ),
        sprintf(
          "%02d_%03d",
          state_num,
          ac_num
        ),
        NA_character_
      )

      valid <- !is.na(
        candidate
      )

      if (
        sum(
          valid
        ) <
          100
      ) {
        next
      }

      overlap <- mean(
        candidate[
          valid
        ] %in%
          analysis_uids
      )

      if (
        is.finite(
          overlap
        ) &&
        overlap >
          best_pair$overlap
      ) {
        best_pair <- list(
          uid =
            candidate,
          method =
            paste0(
              "field pair: ",
              sv,
              " + ",
              av
            ),
          overlap =
            overlap
        )
      }
    }
  }

  if (
    is.finite(
      best_pair$overlap
    ) &&
    best_pair$overlap >=
      0.90
  ) {
    return(
      best_pair
    )
  }

  NULL
}

geometry_candidates <- list()

geography_module <- file.path(
  paths$r_dir,
  "geography.R"
)

if (
  file.exists(
    geography_module
  )
) {
  source(
    geography_module
  )

  if (
    exists(
      "build_geography",
      mode =
        "function"
    )
  ) {
    message(
      "Recovering AC geometry through the project's build_geography()..."
    )

    geography_obj <- build_geography(
      paths,
      paths
    )

    geometry_candidates <- collect_sf_objects(
      geography_obj
    )
  }
}

# Fallback direct shapefile discovery.
shape_candidates <- unique(
  c(
    file.path(
      project_root,
      "data",
      "India_AC.shp"
    ),
    file.path(
      project_root,
      "data",
      "geography",
      "India_AC.shp"
    ),
    file.path(
      project_root,
      "data",
      "raw",
      "India_AC.shp"
    ),
    file.path(
      project_root,
      "India_AC.shp"
    ),
    list.files(
      project_root,
      pattern =
        "^India_AC\\.shp$",
      recursive = TRUE,
      full.names = TRUE
    )
  )
)

shape_candidates <- shape_candidates[
  file.exists(
    shape_candidates
  )
]

for (
  shp in shape_candidates
) {
  sf_try <- tryCatch(
    suppressWarnings(
      sf::st_read(
        shp,
        quiet = TRUE
      )
    ),
    error =
      function(e) {
        NULL
      }
  )

  if (
    !is.null(
      sf_try
    )
  ) {
    geometry_candidates[[
      paste0(
        "direct:",
        shp
      )
    ]] <- sf_try
  }
}

if (
  length(
    geometry_candidates
  ) == 0
) {
  stop(
    "Could not recover any sf AC geometry from build_geography() or India_AC.shp."
  )
}

resolved_geometry <- NULL
resolved_meta <- NULL

for (
  nm in names(
    geometry_candidates
  )
) {
  candidate_sf <- geometry_candidates[[
    nm
  ]]

  resolution <- resolve_sf_ac_uid(
    candidate_sf,
    unique(
      d$ac_uid
    )
  )

  if (
    is.null(
      resolution
    )
  ) {
    next
  }

  n_match <- sum(
    resolution$uid %in%
      d$ac_uid,
    na.rm = TRUE
  )

  if (
    is.null(
      resolved_meta
    ) ||
    n_match >
      resolved_meta$n_match
  ) {
    resolved_geometry <- candidate_sf |>
      dplyr::mutate(
        ac_uid_resolved =
          resolution$uid
      )

    resolved_meta <- list(
      object_name =
        nm,
      method =
        resolution$method,
      overlap =
        resolution$overlap,
      n_match =
        n_match,
      n_rows =
        nrow(
          candidate_sf
        )
    )
  }
}

if (
  is.null(
    resolved_geometry
  )
) {
  candidate_columns <- paste(
    purrr::map_chr(
      names(
        geometry_candidates
      ),
      function(nm) {
        paste0(
          nm,
          " => ",
          paste(
            names(
              sf::st_drop_geometry(
                geometry_candidates[[
                  nm
                ]]
              )
            ),
            collapse = ", "
          )
        )
      }
    ),
    collapse = "\n"
  )

  stop(
    "Found AC geometry but could not resolve it to project ac_uid with >=90% ",
    "confidence. Candidate object columns were:\n",
    candidate_columns
  )
}

geometry_resolution_audit <- tibble::tibble(
  geometry_object =
    resolved_meta$object_name,
  id_resolution_method =
    resolved_meta$method,
  id_overlap_share =
    resolved_meta$overlap,
  n_geometry_rows =
    resolved_meta$n_rows,
  n_rows_matching_analysis_ac_uid =
    resolved_meta$n_match
)

readr::write_csv(
  geometry_resolution_audit,
  file.path(
    out_manifest_dir,
    "01_geometry_resolution_audit.csv"
  )
)

message(
  "Using geometry object: ",
  resolved_meta$object_name,
  " [",
  resolved_meta$method,
  "]"
)

# Dissolve duplicate pieces to one geometry per analysis AC.
ac_sf <- resolved_geometry |>
  dplyr::filter(
    !is.na(
      ac_uid_resolved
    ),
    ac_uid_resolved %in%
      d$ac_uid
  ) |>
  sf::st_make_valid() |>
  dplyr::group_by(
    ac_uid_resolved
  ) |>
  dplyr::summarise(
    .groups = "drop"
  ) |>
  dplyr::rename(
    ac_uid =
      ac_uid_resolved
  )

if (
  anyDuplicated(
    ac_sf$ac_uid
  ) >
    0
) {
  stop(
    "AC geometry is not unique after dissolve."
  )
}

# ============================================================
# 3. BUILD OWN + TOUCHING-AC NEIGHBORHOODS
# ============================================================

touches <- sf::st_touches(
  ac_sf,
  sparse = TRUE
)

neighbor_edges <- purrr::map_dfr(
  seq_along(
    touches
  ),
  function(i) {
    members <- unique(
      c(
        i,
        touches[[i]]
      )
    )

    tibble::tibble(
      focal_ac_uid =
        ac_sf$ac_uid[[i]],
      member_ac_uid =
        ac_sf$ac_uid[
          members
        ]
    )
  }
)

neighbor_degree <- neighbor_edges |>
  dplyr::count(
    focal_ac_uid,
    name =
      "n_local_members"
  ) |>
  dplyr::mutate(
    n_touching_neighbors =
      n_local_members -
      1L
  )

readr::write_csv(
  neighbor_degree,
  file.path(
    out_manifest_dir,
    "02_local_neighbor_degree.csv"
  )
)

neighbor_audit <- neighbor_degree |>
  dplyr::summarise(
    n_focal_acs =
      dplyr::n(),
    min_neighbors =
      min(
        n_touching_neighbors
      ),
    median_neighbors =
      stats::median(
        n_touching_neighbors
      ),
    mean_neighbors =
      mean(
        n_touching_neighbors
      ),
    p90_neighbors =
      stats::quantile(
        n_touching_neighbors,
        0.90,
        names = FALSE
      ),
    max_neighbors =
      max(
        n_touching_neighbors
      ),
    n_zero_neighbor_acs =
      sum(
        n_touching_neighbors ==
          0
      )
  )

readr::write_csv(
  neighbor_audit,
  file.path(
    out_manifest_dir,
    "03_local_neighbor_summary.csv"
  )
)

# ============================================================
# 4. POOL 2005 EC05 EMPLOYMENT OVER THE MATCHED LOCAL GEOGRAPHY
# ============================================================

d_local_source <- d |>
  dplyr::mutate(
    ec05_high_quality_member =
      dplyr::coalesce(
        ec05_quality_high,
        FALSE
      ) &
      dplyr::coalesce(
        ec05_fragmentation_low,
        FALSE
      )
  )

# Reconstruct sector employment from share x total employment.
for (
  k in 1:90
) {
  share_var <- paste0(
    "ec05_share_shric_",
    k
  )

  emp_var <- paste0(
    "ec05_emp_rebuilt_shric_",
    k
  )

  d_local_source[[
    emp_var
  ]] <-
    d_local_source[[
      share_var
    ]] *
    d_local_source$ec05_emp_all
}

rebuilt_emp_vars <- paste0(
  "ec05_emp_rebuilt_shric_",
  1:90
)

member_payload <- d_local_source |>
  dplyr::select(
    member_ac_uid =
      ac_uid,
    ec05_emp_all,
    ec05_emp_manuf,
    con08_land_area,
    ec05_high_quality_member,
    dplyr::all_of(
      rebuilt_emp_vars
    )
  )

local_pool <- neighbor_edges |>
  dplyr::left_join(
    member_payload,
    by =
      "member_ac_uid",
    relationship =
      "many-to-one"
  ) |>
  dplyr::group_by(
    focal_ac_uid
  ) |>
  dplyr::summarise(
    n_local_members_geometry =
      dplyr::n(),

    n_local_members_with_ec05 =
      sum(
        !is.na(
          ec05_emp_all
        )
      ),

    local_ec05_complete =
      all(
        !is.na(
          ec05_emp_all
        )
      ),

    local_all_members_high_quality =
      all(
        !is.na(
          ec05_emp_all
        ) &
        ec05_high_quality_member
      ),

    local_ec05_coverage_share =
      mean(
        !is.na(
          ec05_emp_all
        )
      ),

    local_ec05_emp_all =
      dplyr::if_else(
        local_ec05_complete,
        sum(
          ec05_emp_all
        ),
        NA_real_
      ),

    local_ec05_emp_manuf =
      dplyr::if_else(
        local_ec05_complete,
        sum(
          ec05_emp_manuf
        ),
        NA_real_
      ),

    local_land_area =
      dplyr::if_else(
        all(
          !is.na(
            con08_land_area
          )
        ),
        sum(
          con08_land_area
        ),
        NA_real_
      ),

    dplyr::across(
      dplyr::all_of(
        rebuilt_emp_vars
      ),
      ~dplyr::if_else(
        local_ec05_complete,
        sum(
          .x
        ),
        NA_real_
      ),
      .names =
        "local_{.col}"
    ),

    .groups = "drop"
  ) |>
  dplyr::left_join(
    neighbor_degree,
    by =
      "focal_ac_uid",
    relationship =
      "one-to-one"
  ) |>
  dplyr::rename(
    ac_uid =
      focal_ac_uid
  ) |>
  dplyr::mutate(
    local_ec05_mfg_share =
      dplyr::if_else(
        is.finite(
          local_ec05_emp_all
        ) &
        local_ec05_emp_all >
          0,
        local_ec05_emp_manuf /
          local_ec05_emp_all,
        NA_real_
      ),

    log1p_local_ec05_emp_all =
      log1p(
        local_ec05_emp_all
      ),

    log1p_local_land_area =
      log1p(
        local_land_area
      )
  )

# Construct pooled local industry shares.
for (
  k in 1:90
) {
  local_emp_var <- paste0(
    "local_ec05_emp_rebuilt_shric_",
    k
  )

  local_share_var <- paste0(
    "local_ec05_share_shric_",
    k
  )

  local_pool[[
    local_share_var
  ]] <-
    dplyr::if_else(
      is.finite(
        local_pool$local_ec05_emp_all
      ) &
      local_pool$local_ec05_emp_all >
        0,
      local_pool[[
        local_emp_var
      ]] /
        local_pool$local_ec05_emp_all,
      NA_real_
    )
}

local_share_vars <- paste0(
  "local_ec05_share_shric_",
  1:90
)

local_pool$local_shric_share_sum <-
  rowSums(
    local_pool[
      local_share_vars
    ],
    na.rm = FALSE
  )

local_pool_audit <- local_pool |>
  dplyr::summarise(
    n_geometry_acs =
      dplyr::n(),

    n_strict_local_ec05_complete =
      sum(
        local_ec05_complete
      ),

    share_strict_local_ec05_complete =
      mean(
        local_ec05_complete
      ),

    n_all_members_high_quality =
      sum(
        local_all_members_high_quality
      ),

    share_all_members_high_quality =
      mean(
        local_all_members_high_quality
      ),

    min_local_ec05_coverage_share =
      min(
        local_ec05_coverage_share
      ),

    median_local_ec05_coverage_share =
      stats::median(
        local_ec05_coverage_share
      ),

    mean_local_ec05_coverage_share =
      mean(
        local_ec05_coverage_share
      ),

    max_abs_share_sum_error_complete =
      max(
        abs(
          local_shric_share_sum[
            local_ec05_complete
          ] -
          1
        ),
        na.rm = TRUE
      )
  )

readr::write_csv(
  local_pool_audit,
  file.path(
    out_manifest_dir,
    "04_local_ec05_pooling_audit.csv"
  )
)

# ============================================================
# 5. BUILD MATCHED LOCAL WIR BARTIKS
# ============================================================

primary_shocks <- shock_table |>
  dplyr::filter(
    stringr::str_starts(
      shock_definition,
      "PRIMARY:"
    )
  ) |>
  dplyr::select(
    wir_industry,
    shock_log_change
  )

mfg_crosswalk <- crosswalk |>
  dplyr::filter(
    manufacturing_shric
  ) |>
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
      mfg_crosswalk$shock_log_change
    )
  )
) {
  stop(
    "At least one manufacturing SHRIC lacks a primary WIR shock."
  )
}

local_pool$bartik_wir_world_mfg_local_pooled_logchg <-
  0

local_pool$bartik_wir_world_mfg_local_pooled_detailed_only_logchg <-
  0

for (
  i in seq_len(
    nrow(
      mfg_crosswalk
    )
  )
) {
  k <-
    as.integer(
      mfg_crosswalk$shric[[i]]
    )

  share_var <- paste0(
    "local_ec05_share_shric_",
    k
  )

  shock_i <-
    mfg_crosswalk$shock_log_change[[i]]

  local_pool$bartik_wir_world_mfg_local_pooled_logchg <-
    local_pool$bartik_wir_world_mfg_local_pooled_logchg +
    local_pool[[
      share_var
    ]] *
    shock_i

  if (
    mfg_crosswalk$mapping_specificity[[i]] ==
      "detailed"
  ) {
    local_pool$bartik_wir_world_mfg_local_pooled_detailed_only_logchg <-
      local_pool$bartik_wir_world_mfg_local_pooled_detailed_only_logchg +
      local_pool[[
        share_var
      ]] *
      shock_i
  }
}

# Make missingness explicit outside the strict neighborhood-complete sample.
local_pool <- local_pool |>
  dplyr::mutate(
    bartik_wir_world_mfg_local_pooled_logchg =
      dplyr::if_else(
        local_ec05_complete,
        bartik_wir_world_mfg_local_pooled_logchg,
        NA_real_
      ),

    bartik_wir_world_mfg_local_pooled_detailed_only_logchg =
      dplyr::if_else(
        local_ec05_complete,
        bartik_wir_world_mfg_local_pooled_detailed_only_logchg,
        NA_real_
      )
  )

local_bartik_audit <- local_pool |>
  dplyr::summarise(
    n_local_bartik_nonmissing =
      sum(
        !is.na(
          bartik_wir_world_mfg_local_pooled_logchg
        )
      ),

    median_hierarchical_local_bartik =
      stats::median(
        bartik_wir_world_mfg_local_pooled_logchg,
        na.rm = TRUE
      ),

    sd_hierarchical_local_bartik =
      stats::sd(
        bartik_wir_world_mfg_local_pooled_logchg,
        na.rm = TRUE
      ),

    median_detailed_local_bartik =
      stats::median(
        bartik_wir_world_mfg_local_pooled_detailed_only_logchg,
        na.rm = TRUE
      ),

    sd_detailed_local_bartik =
      stats::sd(
        bartik_wir_world_mfg_local_pooled_detailed_only_logchg,
        na.rm = TRUE
      )
  )

readr::write_csv(
  local_bartik_audit,
  file.path(
    out_manifest_dir,
    "05_local_bartik_construction_audit.csv"
  )
)

# ============================================================
# 6. JOIN MATCHED LOCAL BARTIK TO EXISTING LOCAL FDI TREATMENTS
# ============================================================

local_data <- d |>
  dplyr::select(
    ac_uid,
    state_no,
    pc_cluster_id,
    fdi_mfg_local_all_n_2009,
    fdi_mfg_local_all_n_2014
  ) |>
  dplyr::left_join(
    local_pool |>
      dplyr::select(
        ac_uid,
        n_touching_neighbors,
        local_ec05_complete,
        local_all_members_high_quality,
        local_ec05_coverage_share,
        local_ec05_emp_all,
        local_ec05_emp_manuf,
        local_ec05_mfg_share,
        local_land_area,
        log1p_local_ec05_emp_all,
        log1p_local_land_area,
        bartik_wir_world_mfg_local_pooled_logchg,
        bartik_wir_world_mfg_local_pooled_detailed_only_logchg,
        dplyr::all_of(
          local_share_vars
        )
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  dplyr::mutate(
    fdi_mfg_local_log_count_2014 =
      log1p(
        fdi_mfg_local_all_n_2014
      ),

    fdi_mfg_local_log_count_2009 =
      log1p(
        fdi_mfg_local_all_n_2009
      ),

    fdi_mfg_local_any_2014 =
      as.integer(
        fdi_mfg_local_all_n_2014 >
          0
      )
  )

readr::write_rds(
  local_data,
  file.path(
    out_data_dir,
    "ac_iv_local_first_stage_base.rds"
  )
)

readr::write_csv(
  local_data |>
    dplyr::select(
      -dplyr::all_of(
        local_share_vars
      )
    ),
  file.path(
    out_data_dir,
    "ac_iv_local_first_stage_base.csv.gz"
  )
)

# ============================================================
# 7. FIRST-STAGE HELPERS
# ============================================================

partial_r2_for_instrument <- function(
    data,
    lhs,
    instrument,
    controls
) {
  reduced_rhs <- if (
    nzchar(
      controls
    )
  ) {
    controls
  } else {
    "1"
  }

  y_fit <- fixest::feols(
    stats::as.formula(
      paste0(
        lhs,
        " ~ ",
        reduced_rhs,
        " | state_no"
      )
    ),
    data =
      data,
    notes = FALSE,
    warn = FALSE
  )

  z_fit <- fixest::feols(
    stats::as.formula(
      paste0(
        instrument,
        " ~ ",
        reduced_rhs,
        " | state_no"
      )
    ),
    data =
      data,
    notes = FALSE,
    warn = FALSE
  )

  ry <- stats::residuals(
    y_fit
  )

  rz <- stats::residuals(
    z_fit
  )

  if (
    stats::sd(
      ry
    ) ==
      0 ||
    stats::sd(
      rz
    ) ==
      0
  ) {
    return(
      NA_real_
    )
  }

  stats::cor(
    ry,
    rz
  )^2
}

local_spec_meta <- tibble::tribble(
  ~spec_id, ~spec_label, ~controls, ~high_quality,

  "LFS0_state_fe",
  "State FE only",
  "",
  FALSE,

  "LFS1_baseline_fdi",
  "+ baseline local manufacturing FDI",
  "fdi_mfg_local_log_count_2009",
  FALSE,

  "LFS2_pre_size",
  "+ baseline FDI + pooled 2005 local size + land area + neighbor degree",
  paste(
    "fdi_mfg_local_log_count_2009",
    "log1p_local_ec05_emp_all",
    "log1p_local_land_area",
    "n_touching_neighbors",
    sep = " + "
  ),
  FALSE,

  "LFS3_preferred",
  "+ baseline FDI + pooled 2005 size + manufacturing share + land area + neighbor degree",
  paste(
    "fdi_mfg_local_log_count_2009",
    "log1p_local_ec05_emp_all",
    "local_ec05_mfg_share",
    "log1p_local_land_area",
    "n_touching_neighbors",
    sep = " + "
  ),
  FALSE,

  "LFS4_preferred_high_quality",
  "Preferred controls; every local EC05 member high quality",
  paste(
    "fdi_mfg_local_log_count_2009",
    "log1p_local_ec05_emp_all",
    "local_ec05_mfg_share",
    "log1p_local_land_area",
    "n_touching_neighbors",
    sep = " + "
  ),
  TRUE
)

local_instrument_meta <- tibble::tribble(
  ~instrument_id, ~instrument_var, ~instrument_label,

  "hierarchical_local_pooled",
  "bartik_wir_world_mfg_local_pooled_logchg",
  "Matched-local pooled WIR manufacturing Bartik",

  "detailed_only_local_pooled",
  "bartik_wir_world_mfg_local_pooled_detailed_only_logchg",
  "Matched-local pooled WIR detailed-only manufacturing Bartik"
)

local_treatment_meta <- tibble::tribble(
  ~treatment_id, ~treatment_var, ~treatment_label, ~primary_treatment,

  "log_count_local",
  "fdi_mfg_local_log_count_2014",
  "log(1 + local manufacturing projects), 2009-14",
  TRUE,

  "count_local",
  "fdi_mfg_local_all_n_2014",
  "Local manufacturing project count, 2009-14",
  FALSE,

  "any_local",
  "fdi_mfg_local_any_2014",
  "Any local manufacturing project, 2009-14",
  FALSE
)

run_local_first_stage <- function(
    treatment_var,
    treatment_label,
    primary_treatment,
    instrument_id,
    instrument_var,
    instrument_label,
    spec_id,
    spec_label,
    controls,
    high_quality
) {
  formula <- stats::as.formula(
    paste0(
      treatment_var,
      " ~ ",
      instrument_var,
      if (
        nzchar(
          controls
        )
      ) {
        paste0(
          " + ",
          controls
        )
      } else {
        ""
      },
      " | state_no"
    )
  )

  vars <- unique(
    c(
      all.vars(
        formula
      ),
      "pc_cluster_id",
      "local_ec05_complete",
      if (
        high_quality
      ) {
        "local_all_members_high_quality"
      } else {
        character(0)
      }
    )
  )

  dat <- local_data |>
    dplyr::filter(
      local_ec05_complete,
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    high_quality
  ) {
    dat <- dat |>
      dplyr::filter(
        local_all_members_high_quality
      )
  }

  fit <- fixest::feols(
    formula,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      instrument_var
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument_var
    ]

  t_stat <-
    beta /
    se

  partial_r2 <- partial_r2_for_instrument(
    dat,
    treatment_var,
    instrument_var,
    controls
  )

  tibble::tibble(
    geography =
      "local: own + touching ACs",
    treatment_var =
      treatment_var,
    treatment_label =
      treatment_label,
    primary_treatment =
      primary_treatment,
    instrument_id =
      instrument_id,
    instrument_var =
      instrument_var,
    instrument_label =
      instrument_label,
    spec_id =
      spec_id,
    spec_label =
      spec_label,
    high_quality =
      high_quality,
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    t_stat =
      unname(
        t_stat
      ),
    cluster_f_single_instrument =
      unname(
        t_stat^2
      ),
    partial_r2 =
      partial_r2,
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      ),
    n_states =
      dplyr::n_distinct(
        dat$state_no
      ),
    first_stage_sign =
      dplyr::case_when(
        beta >
          0 ~
          "positive",
        beta <
          0 ~
          "negative",
        TRUE ~
          "zero"
      )
  )
}

local_first_stage_results <- purrr::pmap_dfr(
  tidyr::crossing(
    local_treatment_meta,
    local_instrument_meta,
    local_spec_meta
  ),
  function(
      treatment_id,
      treatment_var,
      treatment_label,
      primary_treatment,
      instrument_id,
      instrument_var,
      instrument_label,
      spec_id,
      spec_label,
      controls,
      high_quality
  ) {
    run_local_first_stage(
      treatment_var =
        treatment_var,
      treatment_label =
        treatment_label,
      primary_treatment =
        primary_treatment,
      instrument_id =
        instrument_id,
      instrument_var =
        instrument_var,
      instrument_label =
        instrument_label,
      spec_id =
        spec_id,
      spec_label =
        spec_label,
      controls =
        controls,
      high_quality =
        high_quality
    )
  }
)

readr::write_csv(
  local_first_stage_results,
  file.path(
    out_table_dir,
    "01_local_first_stage_results.csv"
  )
)

local_headline <- local_first_stage_results |>
  dplyr::filter(
    primary_treatment,
    spec_id %in%
      c(
        "LFS3_preferred",
        "LFS4_preferred_high_quality"
      )
  )

readr::write_csv(
  local_headline,
  file.path(
    out_table_dir,
    "00_HEADLINE_local_first_stage.csv"
  )
)

# ============================================================
# 8. LOCAL PRE-PERIOD PLACEBO
# ============================================================

placebo_controls <- paste(
  "log1p_local_ec05_emp_all",
  "local_ec05_mfg_share",
  "log1p_local_land_area",
  "n_touching_neighbors",
  sep = " + "
)

run_local_placebo <- function(
    instrument_id,
    instrument_var,
    instrument_label,
    high_quality
) {
  formula <- stats::as.formula(
    paste0(
      "fdi_mfg_local_log_count_2009 ~ ",
      instrument_var,
      " + ",
      placebo_controls,
      " | state_no"
    )
  )

  vars <- unique(
    c(
      all.vars(
        formula
      ),
      "pc_cluster_id",
      "local_ec05_complete",
      if (
        high_quality
      ) {
        "local_all_members_high_quality"
      } else {
        character(0)
      }
    )
  )

  dat <- local_data |>
    dplyr::filter(
      local_ec05_complete,
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    high_quality
  ) {
    dat <- dat |>
      dplyr::filter(
        local_all_members_high_quality
      )
  }

  fit <- fixest::feols(
    formula,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      instrument_var
    ]

  se <-
    fixest::se(
      fit
    )[
      instrument_var
    ]

  tibble::tibble(
    geography =
      "local: own + touching ACs",
    instrument_id,
    instrument_label,
    high_quality,
    placebo_outcome =
      "log(1 + local manufacturing projects), 2004-09",
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    cluster_f_single_instrument =
      unname(
        (
          beta /
            se
        )^2
      ),
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      )
  )
}

local_placebo_results <- dplyr::bind_rows(
  purrr::pmap_dfr(
    local_instrument_meta,
    function(
        instrument_id,
        instrument_var,
        instrument_label
    ) {
      run_local_placebo(
        instrument_id,
        instrument_var,
        instrument_label,
        FALSE
      )
    }
  ),

  purrr::pmap_dfr(
    local_instrument_meta,
    function(
        instrument_id,
        instrument_var,
        instrument_label
    ) {
      run_local_placebo(
        instrument_id,
        instrument_var,
        instrument_label,
        TRUE
      )
    }
  )
)

readr::write_csv(
  local_placebo_results,
  file.path(
    out_table_dir,
    "02_local_preperiod_fdi_placebo_results.csv"
  )
)

# ============================================================
# 9. LOCAL DETAILED-INDUSTRY CONTRIBUTIONS + CONCENTRATION
# ============================================================

detailed_crosswalk <- mfg_crosswalk |>
  dplyr::filter(
    mapping_specificity ==
      "detailed"
  )

detailed_wir_industries <- sort(
  unique(
    detailed_crosswalk$wir_industry
  )
)

local_contrib <- local_data |>
  dplyr::select(
    ac_uid,
    state_no,
    pc_cluster_id,
    local_ec05_complete,
    local_all_members_high_quality,
    fdi_mfg_local_log_count_2009,
    fdi_mfg_local_log_count_2014,
    log1p_local_ec05_emp_all,
    local_ec05_mfg_share,
    log1p_local_land_area,
    n_touching_neighbors,
    dplyr::all_of(
      local_share_vars
    )
  )

for (
  industry_i in detailed_wir_industries
) {
  rows_i <- detailed_crosswalk |>
    dplyr::filter(
      wir_industry ==
        industry_i
    )

  shric_i <- as.integer(
    rows_i$shric
  )

  shock_i <- unique(
    rows_i$shock_log_change
  )

  if (
    length(
      shock_i
    ) != 1
  ) {
    stop(
      "WIR industry does not have a unique shock: ",
      industry_i
    )
  }

  exposure_i <- rowSums(
    local_contrib[
      paste0(
        "local_ec05_share_shric_",
        shric_i
      )
    ],
    na.rm = FALSE
  )

  component_name <- paste0(
    "z_component__",
    janitor::make_clean_names(
      industry_i
    )
  )

  local_contrib[[
    component_name
  ]] <-
    exposure_i *
    shock_i
}

local_component_vars <- names(
  local_contrib
)[
  stringr::str_starts(
    names(
      local_contrib
    ),
    "z_component__"
  )
]

local_contrib$z_detailed_local_rebuilt <-
  rowSums(
    local_contrib[
      local_component_vars
    ],
    na.rm = FALSE
  )

rebuild_check <- local_contrib |>
  dplyr::left_join(
    local_data |>
      dplyr::select(
        ac_uid,
        bartik_wir_world_mfg_local_pooled_detailed_only_logchg
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  ) |>
  dplyr::filter(
    local_ec05_complete
  ) |>
  dplyr::summarise(
    max_abs_difference =
      max(
        abs(
          z_detailed_local_rebuilt -
          bartik_wir_world_mfg_local_pooled_detailed_only_logchg
        ),
        na.rm = TRUE
      )
  )

readr::write_csv(
  rebuild_check,
  file.path(
    out_manifest_dir,
    "06_local_instrument_rebuild_audit.csv"
  )
)

if (
  rebuild_check$max_abs_difference >
    1e-10
) {
  stop(
    "Local detailed instrument reconstruction does not reproduce saved Bartik."
  )
}

pref <- local_contrib |>
  dplyr::filter(
    local_ec05_complete,
    !is.na(
      fdi_mfg_local_log_count_2014
    ),
    !is.na(
      fdi_mfg_local_log_count_2009
    ),
    !is.na(
      log1p_local_ec05_emp_all
    ),
    !is.na(
      local_ec05_mfg_share
    ),
    !is.na(
      log1p_local_land_area
    ),
    !is.na(
      n_touching_neighbors
    ),
    !is.na(
      state_no
    ),
    !is.na(
      pc_cluster_id
    )
  )

z_pref <- pref$z_detailed_local_rebuilt
var_z <- stats::var(
  z_pref
)

local_concentration <- purrr::map_dfr(
  detailed_wir_industries,
  function(industry_i) {
    component_var <- paste0(
      "z_component__",
      janitor::make_clean_names(
        industry_i
      )
    )

    shric_i <- detailed_crosswalk |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shric
      ) |>
      as.integer()

    exposure_i <- rowSums(
      pref[
        paste0(
          "local_ec05_share_shric_",
          shric_i
        )
      ]
    )

    shock_i <- detailed_crosswalk |>
      dplyr::filter(
        wir_industry ==
          industry_i
      ) |>
      dplyr::pull(
        shock_log_change
      ) |>
      unique()

    tibble::tibble(
      wir_industry =
        industry_i,
      shock_log_change =
        shock_i,
      mean_local_2005_nonfarm_employment_share =
        mean(
          exposure_i
        ),
      sd_component =
        stats::sd(
          pref[[
            component_var
          ]]
        ),
      covariance_contribution_to_instrument_variance =
        stats::cov(
          pref[[
            component_var
          ]],
          z_pref
        ) /
        var_z
    )
  }
) |>
  dplyr::mutate(
    exposure_weight =
      mean_local_2005_nonfarm_employment_share /
      sum(
        mean_local_2005_nonfarm_employment_share
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(
      abs(
        covariance_contribution_to_instrument_variance
      )
    )
  )

readr::write_csv(
  local_concentration,
  file.path(
    out_table_dir,
    "03_local_wir_industry_instrument_concentration.csv"
  )
)

local_effective_industry_count <-
  1 /
  sum(
    local_concentration$exposure_weight^2
  )

readr::write_csv(
  tibble::tibble(
    n_detailed_wir_industries =
      nrow(
        local_concentration
      ),
    effective_industry_count_by_mean_exposure_weights =
      local_effective_industry_count,
    largest_exposure_weight =
      max(
        local_concentration$exposure_weight
      ),
    top3_exposure_weight =
      sum(
        sort(
          local_concentration$exposure_weight,
          decreasing = TRUE
        )[
          seq_len(
            min(
              3,
              nrow(
                local_concentration
              )
            )
          )
        ]
      )
  ),
  file.path(
    out_manifest_dir,
    "07_local_instrument_concentration_summary.csv"
  )
)

# ============================================================
# 10. LOCAL LEAVE-ONE-WIR-INDUSTRY-OUT FIRST STAGE
# ============================================================

run_local_loo <- function(
    excluded_industry,
    high_quality
) {
  dat <- local_contrib

  if (
    excluded_industry ==
      "NONE"
  ) {
    dat$z_loo <-
      dat$z_detailed_local_rebuilt
  } else {
    component_var <- paste0(
      "z_component__",
      janitor::make_clean_names(
        excluded_industry
      )
    )

    dat$z_loo <-
      dat$z_detailed_local_rebuilt -
      dat[[
        component_var
      ]]
  }

  vars <- c(
    "fdi_mfg_local_log_count_2014",
    "z_loo",
    "fdi_mfg_local_log_count_2009",
    "log1p_local_ec05_emp_all",
    "local_ec05_mfg_share",
    "log1p_local_land_area",
    "n_touching_neighbors",
    "state_no",
    "pc_cluster_id"
  )

  dat <- dat |>
    dplyr::filter(
      local_ec05_complete,
      dplyr::if_all(
        dplyr::all_of(
          vars
        ),
        ~!is.na(
          .x
        )
      )
    )

  if (
    high_quality
  ) {
    dat <- dat |>
      dplyr::filter(
        local_all_members_high_quality
      )
  }

  fit <- fixest::feols(
    fdi_mfg_local_log_count_2014 ~
      z_loo +
      fdi_mfg_local_log_count_2009 +
      log1p_local_ec05_emp_all +
      local_ec05_mfg_share +
      log1p_local_land_area +
      n_touching_neighbors |
      state_no,
    data =
      dat,
    vcov =
      ~pc_cluster_id,
    notes = FALSE,
    warn = FALSE
  )

  beta <-
    stats::coef(
      fit
    )[
      "z_loo"
    ]

  se <-
    fixest::se(
      fit
    )[
      "z_loo"
    ]

  tibble::tibble(
    geography =
      "local: own + touching ACs",
    excluded_wir_industry =
      excluded_industry,
    high_quality =
      high_quality,
    coefficient =
      unname(
        beta
      ),
    cluster_se =
      unname(
        se
      ),
    cluster_f_single_instrument =
      unname(
        (
          beta /
            se
        )^2
      ),
    n_ac =
      stats::nobs(
        fit
      ),
    n_pc_clusters =
      dplyr::n_distinct(
        dat$pc_cluster_id
      )
  )
}

local_loo_results <- dplyr::bind_rows(
  purrr::map_dfr(
    c(
      "NONE",
      detailed_wir_industries
    ),
    ~run_local_loo(
      .x,
      FALSE
    )
  ),

  purrr::map_dfr(
    c(
      "NONE",
      detailed_wir_industries
    ),
    ~run_local_loo(
      .x,
      TRUE
    )
  )
) |>
  dplyr::arrange(
    high_quality,
    cluster_f_single_instrument
  )

readr::write_csv(
  local_loo_results,
  file.path(
    out_table_dir,
    "04_leave_one_wir_industry_out_local_first_stage.csv"
  )
)

# ============================================================
# 11. OWN-vs-LOCAL HEADLINE COMPARISON
# ============================================================

own_headline_path <- file.path(
  iv_root,
  "first_stage_diagnostics",
  "tables",
  "00_HEADLINE_first_stage.csv"
)

if (
  file.exists(
    own_headline_path
  )
) {
  own_headline <- readr::read_csv(
    own_headline_path,
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::mutate(
      geography =
        "own AC"
    ) |>
    dplyr::transmute(
      geography,
      treatment_label,
      instrument_label,
      spec_id,
      coefficient,
      cluster_se,
      cluster_f_single_instrument,
      partial_r2,
      n_ac,
      n_pc_clusters,
      first_stage_sign
    )

  local_compare <- local_headline |>
    dplyr::transmute(
      geography,
      treatment_label,
      instrument_label,
      spec_id,
      coefficient,
      cluster_se,
      cluster_f_single_instrument,
      partial_r2,
      n_ac,
      n_pc_clusters,
      first_stage_sign
    )

  own_vs_local <- dplyr::bind_rows(
    own_headline,
    local_compare
  )

  readr::write_csv(
    own_vs_local,
    file.path(
      out_table_dir,
      "05_own_vs_local_headline_comparison.csv"
    )
  )
}

# ============================================================
# 12. FIGURES
# ============================================================

p_headline <- local_headline |>
  dplyr::mutate(
    sample_label =
      dplyr::if_else(
        spec_id ==
          "LFS4_preferred_high_quality",
        "All neighborhood EC05 members high quality",
        "Strict complete local EC05 neighborhood"
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        coefficient,
      y =
        instrument_label
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(
      xmin =
        coefficient -
        1.96 *
        cluster_se,
      xmax =
        coefficient +
        1.96 *
        cluster_se
    ),
    orientation = "y"
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::facet_wrap(
    ~sample_label
  ) +
  ggplot2::labs(
    title =
      "Matched-local first stage: EC05 × WIR Bartik predicting local manufacturing FDI",

    subtitle =
      "Treatment and instrument both use own + touching ACs; PC-clustered intervals.",

    x =
      "First-stage coefficient",

    y = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "01_headline_local_first_stage.pdf"
  ),
  p_headline,
  width = 11,
  height = 6.5
)

p_loo <- local_loo_results |>
  dplyr::filter(
    excluded_wir_industry !=
      "NONE",
    !high_quality
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        cluster_f_single_instrument,
      y =
        stats::reorder(
          excluded_wir_industry,
          cluster_f_single_instrument
        )
    )
  ) +
  ggplot2::geom_vline(
    xintercept = 10,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title =
      "Matched-local leave-one-WIR-industry-out first-stage strength",

    subtitle =
      "Detailed-only pooled local instrument; preferred predetermined controls.",

    x =
      "PC-clustered single-instrument F statistic",

    y =
      "Excluded WIR industry"
  ) +
  ggplot2::theme_minimal(
    base_size = 9.5
  )

ggplot2::ggsave(
  file.path(
    out_figure_dir,
    "02_leave_one_industry_out_local_first_stage.pdf"
  ),
  p_loo,
  width = 10,
  height = 7
)

# ============================================================
# 13. README
# ============================================================

readr::write_lines(
  c(
    paste0(
      "Matched-local first-stage revision: ",
      LOCAL_FIRST_STAGE_REVISION
    ),
    "",
    "THIS SCRIPT DOES NOT USE BJP/POLITICAL OUTCOMES.",
    "",
    "MATCHED LOCAL GEOGRAPHY",
    "-----------------------",
    "Treatment: manufacturing FDI in the focal AC plus all touching ACs.",
    "Instrument: 2005 EC05 employment is pooled over exactly the same own+touching neighborhood,",
    "then converted to local industry shares and interacted with the frozen WIR sector shocks.",
    "",
    "STRICT PRIMARY SAMPLE",
    "---------------------",
    "Every member of the focal own+touching neighborhood must have EC05 data.",
    "This prevents the local instrument from representing only part of the geography represented",
    "by the local FDI treatment.",
    "",
    "PREFERRED CONTROLS",
    "------------------",
    "State FE; baseline 2004-09 local manufacturing FDI; pooled 2005 local non-farm employment;",
    "pooled 2005 local manufacturing share; pooled neighborhood land area; number of touching neighbors.",
    "",
    "WHY INCLUDE NEIGHBOR DEGREE",
    "---------------------------",
    "Local FDI is a project count over a larger opportunity set when an AC has more neighbors.",
    "The pooled Bartik is share-normalized, so the preferred first stage conditions on neighborhood degree",
    "as well as pooled economic size and land area.",
    "",
    "HIGH-QUALITY SENSITIVITY",
    "------------------------",
    "The strict high-quality sensitivity requires every EC05 member of the local neighborhood to meet",
    "the existing EC05 quality and low-fragmentation rules.",
    "",
    "READ FIRST",
    "----------",
    "manifests/01_geometry_resolution_audit.csv",
    "manifests/04_local_ec05_pooling_audit.csv",
    "tables/00_HEADLINE_local_first_stage.csv",
    "tables/02_local_preperiod_fdi_placebo_results.csv",
    "tables/03_local_wir_industry_instrument_concentration.csv",
    "tables/04_leave_one_wir_industry_out_local_first_stage.csv",
    "tables/05_own_vs_local_headline_comparison.csv"
  ),
  file.path(
    out_root,
    "README_FIRST.txt"
  )
)

message("")
message(
  "Matched-local first-stage diagnostics COMPLETE."
)
message(
  "Output directory: ",
  out_root
)
message(
  "No BJP/political second stage was estimated."
)

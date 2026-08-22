# Shared helpers for the Switchers India project.

load_switchers_packages <- function(include_models = TRUE) {
  packages <- c(
    "tidyverse", "sf", "readxl", "haven", "janitor", "lubridate",
    "digest", "scales"
  )
  if (include_models) {
    packages <- c(packages, "fixest", "marginaleffects", "modelsummary", "broom", "insight")
  }

  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Install required packages before running the project: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

build_project_paths <- function(project_root) {
  data_dir <- file.path(project_root, "data")
  derived_dir <- file.path(data_dir, "derived", "switchers_rewrite")

  list(
    project_root = project_root,
    data_dir = data_dir,
    config_dir = file.path(project_root, "config"),
    r_dir = file.path(project_root, "R"),

    ac_map_dir = file.path(data_dir, "election", "2014_shp"),
    ac_district_xwalk = file.path(data_dir, "assembly-constituencies", "India_AC.shp"),
    ac_name_key = file.path(data_dir, "shrug", "ac08_name_key.csv"),
    delhi_districts = file.path(data_dir, "delhi_districts.geojson"),
    district_codes = file.path(data_dir, "dist_codes.xlsx"),

    elections = file.path(data_dir, "election", "lok_dhaba_ge.csv"),
    fdi = file.path(data_dir, "IN_FDI_2004_2014.csv"),
    nes_2009 = file.path(data_dir, "lokniti", "nes_2009.sav"),
    nes_2014 = file.path(data_dir, "lokniti", "nes_2014.sav"),

    ac_population = file.path(data_dir, "shrug", "con08_pop_area_key.csv"),
    ac_population_2011 = file.path(data_dir, "shrug", "pc11_pca_clean_con08.dta"),

    secc_rural_ac08 = file.path(
      data_dir, "shrug", "secc_rural_con08.dta"
    ),
    secc_urban_ac08 = file.path(
      data_dir, "shrug", "secc_urban_con08.dta"
    ),
    secc_cons_rural_ac08 = file.path(
      data_dir, "shrug", "secc_cons_rural_con08.dta"
    ),
    secc_cons_urban_ac08 = file.path(
      data_dir, "shrug", "secc_cons_urban_con08.dta"
    ),
    population_change_dir = file.path(data_dir, "pop", "pop_change"),
    economic_census = file.path(data_dir, "shrug", "ec13_pc11dist.csv"),
    sc_population_dir = file.path(data_dir, "pop", "sc_pop"),
    st_population_dir = file.path(data_dir, "pop", "st_pop"),
    age_2011_dir = file.path(data_dir, "pop", "age_2011"),

    migration_dir = file.path(data_dir, "pop", "migration"),
    migration_employment_2011_dir = file.path(data_dir, "pop", "migration_employment_2011"),

    religion_2001_dir = file.path(data_dir, "pop", "religion_2001"),
    religion_2011_dir = file.path(data_dir, "pop", "religion_2011"),
    language_2001_dir = file.path(data_dir, "pop", "language_2001"),
    language_2011_dir = file.path(data_dir, "pop", "language_2011"),
    sc_education_2001_dir = file.path(data_dir, "pop", "sc_education_2001"),
    sc_education_2011_dir = file.path(data_dir, "pop", "sc_education_2011"),
    st_education_2001_dir = file.path(data_dir, "pop", "st_education_2001"),
    st_education_2011_dir = file.path(data_dir, "pop", "st_education_2011"),

    district_lineage_config = file.path(
      project_root, "config", "district_harmonization_2001_2011.csv"
    ),
    local_language_config = file.path(
      project_root, "config", "local_language_crosswalk_final.csv"
    ),
    fdi_sector_config = file.path(project_root, "config", "fdi_sector_taxonomy.csv"),
    fdi_status_config = file.path(project_root, "config", "fdi_status_taxonomy.csv"),

    derived_dir = derived_dir,
    intermediate_dir = file.path(derived_dir, "intermediate"),
    final_dir = file.path(derived_dir, "final"),
    diagnostic_dir = file.path(derived_dir, "diagnostics"),
    result_dir = file.path(derived_dir, "results")
  )
}

create_output_directories <- function(paths) {
  purrr::walk(
    c(
      paths$derived_dir,
      paths$intermediate_dir,
      paths$final_dir,
      paths$diagnostic_dir,
      paths$result_dir
    ),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

assert_file_exists <- function(path, label = path, optional = FALSE) {
  exists <- file.exists(path)
  if (!exists && !optional) stop("Missing required file: ", label, " [", path, "]")
  exists
}

assert_directory_exists <- function(path, label = path, optional = FALSE) {
  exists <- dir.exists(path)
  if (!exists && !optional) stop("Missing required directory: ", label, " [", path, "]")
  exists
}

assert_has_columns <- function(data, columns, label = deparse(substitute(data))) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(data)
}

assert_unique_rows <- function(data, keys, label = deparse(substitute(data))) {
  duplicates <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    print(duplicates, n = Inf)
    stop(label, " is not unique by ", paste(keys, collapse = ", "), ".")
  }
  invisible(data)
}

first_nonmissing <- function(x) {
  index <- which(!is.na(x))
  if (length(index) == 0) return(x[NA_integer_][1])
  x[index[1]]
}

sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  value <- mean(x, na.rm = TRUE)
  if (is.nan(value)) NA_real_ else value
}

safe_median <- function(x) {
  value <- median(x, na.rm = TRUE)
  if (is.nan(value)) NA_real_ else value
}

safe_sd <- function(x) {
  x <- as.numeric(x)
  if (sum(is.finite(x)) < 2) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

safe_min <- function(x) {
  x <- as.numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- as.numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_weighted_mean <- function(x, w) {
  valid <- is.finite(x) & is.finite(w) & w > 0
  if (!any(valid)) return(NA_real_)
  weighted.mean(x[valid], w[valid])
}

safe_weighted_sum <- function(x, w) {
  valid <- is.finite(x) & is.finite(w) & w > 0
  if (!any(valid)) return(NA_real_)
  sum(x[valid] * w[valid])
}

clean_num <- function(x) {
  value <- stringr::str_squish(as.character(x))
  value[value %in% c(
    "", "-", "--", "---", "----", "-----", ".", "..", "...",
    "....", ".....", "NA", "N.A.", "NIL"
  )] <- NA_character_

  suppressWarnings(
    readr::parse_number(
      value,
      na = c("", "NA", "N.A.", "NIL")
    )
  )
}

safe_divide <- function(numerator, denominator) {
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  result <- rep(NA_real_, max(length(numerator), length(denominator)))
  numerator <- rep_len(numerator, length(result))
  denominator <- rep_len(denominator, length(result))
  valid <- is.finite(numerator) & is.finite(denominator) & denominator > 0
  result[valid] <- numerator[valid] / denominator[valid]
  result
}

safe_share <- safe_divide
safe_pct <- function(numerator, denominator) 100 * safe_divide(numerator, denominator)
safe_pp_change <- function(new, old) 100 * (as.numeric(new) - as.numeric(old))
safe_ratio <- safe_divide
safe_pct_change <- function(new, old) 100 * (safe_divide(new, old) - 1)

safe_log <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x) & x > 0, log(x), NA_real_)
}

safe_log_ratio <- function(numerator, denominator) {
  ratio <- safe_divide(numerator, denominator)
  ifelse(is.finite(ratio) & ratio > 0, log(ratio), NA_real_)
}

per_100k <- function(x, population) 100000 * safe_divide(x, population)

norm_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_remove_all("\\*") |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("&", "AND") |>
    stringr::str_replace_all("[^A-Z0-9]+", " ") |>
    stringr::str_squish()
}

district_name_key <- function(x) ifelse(is.na(x), NA_character_, paste0("NAME_", norm_name(x)))
make_ac_uid <- function(state_no, ac) sprintf("%02d_%03d", as.integer(state_no), as.integer(ac))
make_pc_uid <- function(state_no, pc) sprintf("%02d_%03d", as.integer(state_no), as.integer(pc))

state_codes <- c(
  "ANDAMAN AND NICOBAR ISLANDS" = 35L,
  "ANDHRA PRADESH" = 28L,
  "TELANGANA" = 28L,
  "ARUNACHAL PRADESH" = 12L,
  "ASSAM" = 18L,
  "BIHAR" = 10L,
  "CHANDIGARH" = 4L,
  "CHHATTISGARH" = 22L,
  "DADRA AND NAGAR HAVELI" = 26L,
  "DAMAN AND DIU" = 25L,
  "DELHI" = 7L,
  "NCT DELHI" = 7L,
  "GOA" = 30L,
  "GUJARAT" = 24L,
  "HARYANA" = 6L,
  "HIMACHAL PRADESH" = 2L,
  "JAMMU AND KASHMIR" = 1L,
  "JHARKHAND" = 20L,
  "KARNATAKA" = 29L,
  "KERALA" = 32L,
  "LAKSHADWEEP" = 31L,
  "MADHYA PRADESH" = 23L,
  "MAHARASHTRA" = 27L,
  "MANIPUR" = 14L,
  "MEGHALAYA" = 17L,
  "MIZORAM" = 15L,
  "NAGALAND" = 13L,
  "ODISHA" = 21L,
  "ORISSA" = 21L,
  "PUDUCHERRY" = 34L,
  "PONDICHERRY" = 34L,
  "PUNJAB" = 3L,
  "RAJASTHAN" = 8L,
  "SIKKIM" = 11L,
  "TAMIL NADU" = 33L,
  "TRIPURA" = 16L,
  "UTTAR PRADESH" = 9L,
  "UTTARAKHAND" = 5L,
  "UTTARANCHAL" = 5L,
  "WEST BENGAL" = 19L
)

state_name_to_no <- function(x) {
  cleaned <- norm_name(stringr::str_replace_all(as.character(x), "_", " "))
  as.integer(unname(state_codes[cleaned]))
}

state_lookup <- tibble::enframe(state_codes, name = "state", value = "state_no") |>
  dplyr::mutate(
    state = stringr::str_to_title(state),
    state = dplyr::recode(
      state,
      "Andaman And Nicobar Islands" = "Andaman & Nicobar Islands",
      "Dadra And Nagar Haveli" = "Dadra & Nagar Haveli",
      "Daman And Diu" = "Daman & Diu",
      "Jammu And Kashmir" = "Jammu & Kashmir",
      "Nct Delhi" = "Delhi",
      "Orissa" = "Odisha",
      "Pondicherry" = "Puducherry",
      "Telangana" = "Andhra Pradesh",
      "Uttaranchal" = "Uttarakhand"
    )
  ) |>
  dplyr::distinct(state_no, .keep_all = TRUE)

canonical_source_basename <- function(path) {
  basename(path) |>
    stringr::str_replace(" - 20\\d{2}-\\d{2}-\\d{2}T.*(?=\\.[Xx][Ll][Ss][Xx]?$)", "") |>
    stringr::str_replace(" \\([0-9]+\\)(?=\\.[Xx][Ll][Ss][Xx]?$)", "") |>
    stringr::str_to_upper()
}

read_many_excel <- function(path, pattern, reader, ...) {
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  files <- files[!stringr::str_detect(basename(files), "^~\\$|^\\._|__MACOSX")]
  if (length(files) == 0) stop("No source files matched ", pattern, " in ", path)

  # Ignore accidental browser-download duplicates such as "(1)" and timestamped copies.
  canonical <- canonical_source_basename(files)
  order_index <- order(nchar(basename(files)), basename(files))
  files <- files[order_index]
  canonical <- canonical[order_index]
  files <- files[!duplicated(canonical)]

  purrr::map_dfr(files, reader, ...)
}

read_excel_raw <- function(path, sheet = 1) {
  suppressMessages(
    readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text"
    )
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ stringr::str_squish(as.character(.x))
      )
    )
}

plain_col <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss"))) as.vector(x) else x
}

variable_label <- function(x, fallback) {
  label <- attr(x, "label")
  if (is.null(label) || length(label) == 0 || is.na(label) || label == "") fallback else as.character(label)
}

get_survey_weight <- function(data, weight_var = NULL) {
  if (is.null(weight_var) || !weight_var %in% names(data)) return(rep(1, nrow(data)))
  weight <- as.numeric(data[[weight_var]])
  ifelse(is.finite(weight) & weight > 0, weight, NA_real_)
}

normalize_weights_within_year <- function(weight, year) {
  tibble::tibble(
    weight = as.numeric(weight),
    year = as.integer(year)
  ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      valid_weight = is.finite(weight) & weight > 0,
      mean_valid_weight = if (any(valid_weight)) {
        mean(weight[valid_weight], na.rm = TRUE)
      } else {
        NA_real_
      },
      normalized = dplyr::if_else(
        valid_weight & is.finite(mean_valid_weight) & mean_valid_weight > 0,
        weight / mean_valid_weight,
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(normalized)
}

# Union-find for district lineage groups without an extra graph package.
connected_component_ids <- function(from, to, prefix = "DLG") {
  nodes <- unique(c(as.character(from), as.character(to)))
  parent <- stats::setNames(nodes, nodes)

  find_root <- function(x) {
    while (!identical(parent[[x]], x)) {
      parent[[x]] <<- parent[[parent[[x]]]]
      x <- parent[[x]]
    }
    x
  }

  union_nodes <- function(a, b) {
    ra <- find_root(a)
    rb <- find_root(b)
    if (!identical(ra, rb)) parent[[rb]] <<- ra
  }

  purrr::walk2(as.character(from), as.character(to), union_nodes)
  roots <- vapply(nodes, find_root, character(1))
  root_ids <- sprintf("%s_%03d", prefix, match(roots, unique(roots)))
  stats::setNames(root_ids, nodes)
}

write_csv_checked <- function(data, path, keys = NULL) {
  if (!is.null(keys)) assert_unique_rows(data, keys, basename(path))
  readr::write_csv(data, path, na = "")
  invisible(path)
}

write_rds_csv <- function(data, base_path, keys = NULL) {
  if (!is.null(keys)) assert_unique_rows(data, keys, basename(base_path))
  saveRDS(data, paste0(base_path, ".rds"))
  readr::write_csv(data, paste0(base_path, ".csv"), na = "")
  invisible(data)
}

summarize_missingness <- function(data, file_name, year_var = NULL) {
  long <- data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), list(
      n_total = ~ length(.x),
      n_missing = ~ sum(is.na(.x)),
      n_zero = ~ if (is.numeric(.x)) sum(.x == 0, na.rm = TRUE) else NA_integer_,
      minimum = ~ if (is.numeric(.x) && any(!is.na(.x))) min(.x, na.rm = TRUE) else NA_real_,
      median = ~ if (is.numeric(.x) && any(!is.na(.x))) stats::median(.x, na.rm = TRUE) else NA_real_,
      maximum = ~ if (is.numeric(.x) && any(!is.na(.x))) max(.x, na.rm = TRUE) else NA_real_
    ))) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", ".value"),
      names_pattern = "^(.*)_(n_total|n_missing|n_zero|minimum|median|maximum)$"
    ) |>
    dplyr::mutate(
      file = file_name,
      pct_missing = 100 * n_missing / n_total,
      pct_zero = 100 * n_zero / n_total
    )

  if (is.null(year_var) || !year_var %in% names(data)) return(long)

  data |>
    dplyr::group_by(year = .data[[year_var]]) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), list(
      n_total = ~ length(.x),
      n_missing = ~ sum(is.na(.x)),
      n_zero = ~ if (is.numeric(.x)) sum(.x == 0, na.rm = TRUE) else NA_integer_,
      minimum = ~ if (is.numeric(.x) && any(!is.na(.x))) min(.x, na.rm = TRUE) else NA_real_,
      median = ~ if (is.numeric(.x) && any(!is.na(.x))) stats::median(.x, na.rm = TRUE) else NA_real_,
      maximum = ~ if (is.numeric(.x) && any(!is.na(.x))) max(.x, na.rm = TRUE) else NA_real_
    )), .groups = "drop") |>
    tidyr::pivot_longer(
      -year,
      names_to = c("variable", ".value"),
      names_pattern = "^(.*)_(n_total|n_missing|n_zero|minimum|median|maximum)$"
    ) |>
    dplyr::mutate(
      file = file_name,
      pct_missing = 100 * n_missing / n_total,
      pct_zero = 100 * n_zero / n_total
    )
}

make_output_manifest <- function(files, project_root) {
  tibble::tibble(file = files[file.exists(files)]) |>
    dplyr::mutate(
      relative_path = vapply(file, function(path) {
        root_norm <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
        path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
        prefix <- paste0(root_norm, "/")
        if (startsWith(path_norm, prefix)) substring(path_norm, nchar(prefix) + 1L) else basename(path_norm)
      }, character(1)),
      file_size_bytes = file.info(file)$size,
      file_hash = purrr::map_chr(file, digest::digest, file = TRUE, algo = "sha256"),
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
}

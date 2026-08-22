# ============================================================
# 23_paper_distribution_histograms_v1_0.R
#
# Descriptive constituency distributions for the paper:
#   Figure A: 2001 Muslim population share
#   Figure B: 2009-election-to-2014-election FDI projects per 100,000
#             (total, manufacturing, services)
#   Figure C: change in FDI between an early window
#             (Apr 2004-Dec 2005) and a late window
#             (Aug 2012-Mar 2014), by total/manufacturing/services.
#
# FDI definitions match the project pipeline:
#   - local exposure = own AC + touching ACs
#   - eligible statuses = announced + opened
#   - manufacturing/services use standardized_sector from the
#     project's FDI sector taxonomy.
#
# For Figure C, the primary plotted change is ANNUALIZED because the
# early and late windows contain 21 and 20 months, respectively.
# Literal raw-window differences are also saved in the plot-data CSV.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 0. PROJECT + SETTINGS
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

source(file.path(project_root, "R", "helpers.R"))
paths <- build_project_paths(project_root)

FDI_SCOPE <- "local"
FDI_ALLOWED_STATUSES <- c("announced", "opened")

# Existing election-window convention used in the project.
WINDOW_0914_START <- as.Date("2009-04-01")
WINDOW_0914_END   <- as.Date("2014-04-01")

# Requested early/late comparison windows.
# Left-closed/right-open means these are Apr 2004-Dec 2005 inclusive
# and Aug 2012-Mar 2014 inclusive.
EARLY_START <- as.Date("2004-04-01")
EARLY_END   <- as.Date("2006-01-01")
LATE_START  <- as.Date("2012-08-01")
LATE_END    <- as.Date("2014-04-01")

months_between <- function(start_date, end_date) {
  12L * (lubridate::year(end_date) - lubridate::year(start_date)) +
    (lubridate::month(end_date) - lubridate::month(start_date))
}

EARLY_MONTHS <- months_between(EARLY_START, EARLY_END)
LATE_MONTHS  <- months_between(LATE_START, LATE_END)

stopifnot(EARLY_MONTHS == 21L, LATE_MONTHS == 20L)

out_root <- file.path(
  paths$derived_dir,
  "paper_figures",
  "descriptive_histograms_v1_0"
)
out_data_dir <- file.path(out_root, "data")
out_figure_dir <- file.path(out_root, "figures")
dir.create(out_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figure_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. LOAD AC FRAME + FDI EXPOSURE
# ============================================================

ac_year_path <- file.path(paths$final_dir, "ac_year.rds")
fdi_exposure_path <- file.path(paths$intermediate_dir, "fdi_project_exposure.csv")

if (!file.exists(ac_year_path)) stop("Missing: ", ac_year_path)
if (!file.exists(fdi_exposure_path)) stop("Missing: ", fdi_exposure_path)

ac_year <- readRDS(ac_year_path)
fdi_exposure <- readr::read_csv(
  fdi_exposure_path,
  show_col_types = FALSE,
  progress = FALSE
)

required_ac <- c(
  "ac_uid", "state_no", "ac", "year",
  "proxy_ac_pop", "muslim_share_2001_dist_proxy"
)
missing_ac <- setdiff(required_ac, names(ac_year))
if (length(missing_ac) > 0L) {
  stop("ac_year.rds missing: ", paste(missing_ac, collapse = ", "))
}

required_fdi <- c(
  "fdi_project_uid", "exposed_ac_uid", "exposure_scope",
  "project_month", "standardized_sector", "standardized_status"
)
missing_fdi <- setdiff(required_fdi, names(fdi_exposure))
if (length(missing_fdi) > 0L) {
  stop("fdi_project_exposure.csv missing: ", paste(missing_fdi, collapse = ", "))
}

# Use one row per constituency from the 2009 AC frame.
# The Muslim-share proxy and population denominator are fixed AC attributes.
ac_frame <- ac_year |>
  dplyr::filter(year == 2009L) |>
  dplyr::distinct(ac_uid, .keep_all = TRUE) |>
  dplyr::select(
    ac_uid, state_no, ac,
    proxy_ac_pop,
    muslim_share_2001_dist_proxy
  )

fdi_base <- fdi_exposure |>
  dplyr::mutate(
    project_month = as.Date(project_month),
    exposed_ac_uid = as.character(exposed_ac_uid),
    exposure_scope = as.character(exposure_scope),
    standardized_sector = as.character(standardized_sector),
    standardized_status = as.character(standardized_status)
  ) |>
  dplyr::filter(
    exposure_scope == FDI_SCOPE,
    standardized_status %in% FDI_ALLOWED_STATUSES,
    !is.na(project_month),
    !is.na(exposed_ac_uid)
  ) |>
  dplyr::distinct(
    fdi_project_uid,
    exposed_ac_uid,
    exposure_scope,
    project_month,
    standardized_sector,
    standardized_status
  )

per_100k <- function(n, pop) {
  ifelse(
    is.finite(pop) & pop > 0,
    100000 * n / pop,
    NA_real_
  )
}

count_window <- function(start_date, end_date, suffix) {
  observed <- fdi_base |>
    dplyr::filter(
      project_month >= start_date,
      project_month < end_date
    ) |>
    dplyr::group_by(exposed_ac_uid) |>
    dplyr::summarise(
      total_n = dplyr::n_distinct(fdi_project_uid),
      mfg_n = dplyr::n_distinct(
        fdi_project_uid[standardized_sector == "manufacturing"]
      ),
      services_n = dplyr::n_distinct(
        fdi_project_uid[standardized_sector == "services"]
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(ac_uid = exposed_ac_uid)

  out <- ac_frame |>
    dplyr::select(ac_uid, proxy_ac_pop) |>
    dplyr::left_join(observed, by = "ac_uid", relationship = "one-to-one") |>
    dplyr::mutate(
      total_n = tidyr::replace_na(total_n, 0L),
      mfg_n = tidyr::replace_na(mfg_n, 0L),
      services_n = tidyr::replace_na(services_n, 0L),
      total_pc100k = per_100k(total_n, proxy_ac_pop),
      mfg_pc100k = per_100k(mfg_n, proxy_ac_pop),
      services_pc100k = per_100k(services_n, proxy_ac_pop)
    ) |>
    dplyr::select(-proxy_ac_pop)

  names(out)[names(out) != "ac_uid"] <- paste0(
    names(out)[names(out) != "ac_uid"],
    "_",
    suffix
  )

  out
}

fdi_0914 <- count_window(WINDOW_0914_START, WINDOW_0914_END, "0914")
fdi_early <- count_window(EARLY_START, EARLY_END, "early")
fdi_late <- count_window(LATE_START, LATE_END, "late")

# ============================================================
# 2. BUILD PLOT DATA
# ============================================================

plot_data <- ac_frame |>
  dplyr::left_join(fdi_0914, by = "ac_uid", relationship = "one-to-one") |>
  dplyr::left_join(fdi_early, by = "ac_uid", relationship = "one-to-one") |>
  dplyr::left_join(fdi_late, by = "ac_uid", relationship = "one-to-one") |>
  dplyr::mutate(
    muslim_share_2001_pct = 100 * muslim_share_2001_dist_proxy,

    # Literal window-total changes in projects per 100,000.
    change_total_pc100k_raw = total_pc100k_late - total_pc100k_early,
    change_mfg_pc100k_raw = mfg_pc100k_late - mfg_pc100k_early,
    change_services_pc100k_raw = services_pc100k_late - services_pc100k_early,

    # Annualized window rates, preferred for comparing 21-month and 20-month windows.
    total_pc100k_early_annualized = total_pc100k_early * 12 / EARLY_MONTHS,
    mfg_pc100k_early_annualized = mfg_pc100k_early * 12 / EARLY_MONTHS,
    services_pc100k_early_annualized = services_pc100k_early * 12 / EARLY_MONTHS,

    total_pc100k_late_annualized = total_pc100k_late * 12 / LATE_MONTHS,
    mfg_pc100k_late_annualized = mfg_pc100k_late * 12 / LATE_MONTHS,
    services_pc100k_late_annualized = services_pc100k_late * 12 / LATE_MONTHS,

    change_total_pc100k_annualized =
      total_pc100k_late_annualized - total_pc100k_early_annualized,
    change_mfg_pc100k_annualized =
      mfg_pc100k_late_annualized - mfg_pc100k_early_annualized,
    change_services_pc100k_annualized =
      services_pc100k_late_annualized - services_pc100k_early_annualized
  )

readr::write_csv(
  plot_data,
  file.path(out_data_dir, "constituency_distribution_plot_data.csv")
)

# ============================================================
# 3. FIGURE A: 2001 MUSLIM POPULATION SHARE
# ============================================================

p_muslim <- ggplot(
  plot_data |>
    dplyr::filter(!is.na(muslim_share_2001_pct)),
  aes(x = muslim_share_2001_pct)
) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    closed = "left"
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Muslim population share, 2001",
    y = "Number of constituencies",
    title = "Distribution of Muslim Population Share Across Constituencies"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "plain")
  )

print(p_muslim)

ggsave(
  file.path(out_figure_dir, "01_muslim_share_2001_histogram.png"),
  p_muslim,
  width = 7.2,
  height = 4.8,
  dpi = 400
)
ggsave(
  file.path(out_figure_dir, "01_muslim_share_2001_histogram.pdf"),
  p_muslim,
  width = 7.2,
  height = 4.8
)

# ============================================================
# 4. FIGURE B: 2009-2014 FDI LEVELS PER 100,000
# ============================================================

fdi_level_long <- plot_data |>
  dplyr::select(
    ac_uid,
    total_pc100k_0914,
    mfg_pc100k_0914,
    services_pc100k_0914
  ) |>
  tidyr::pivot_longer(
    cols = -ac_uid,
    names_to = "measure",
    values_to = "fdi_pc100k"
  ) |>
  dplyr::mutate(
    measure = dplyr::recode(
      measure,
      total_pc100k_0914 = "Total FDI",
      mfg_pc100k_0914 = "Manufacturing FDI",
      services_pc100k_0914 = "Services FDI"
    ),
    measure = factor(
      measure,
      levels = c("Total FDI", "Manufacturing FDI", "Services FDI")
    )
  )

p_fdi_levels <- ggplot(
  fdi_level_long |>
    dplyr::filter(!is.na(fdi_pc100k)),
  aes(x = fdi_pc100k)
) +
  geom_histogram(bins = 40, boundary = 0, closed = "left") +
  facet_wrap(~measure, nrow = 1, scales = "free_x") +
  labs(
    x = "FDI projects per 100,000 residents",
    y = "Number of constituencies",
    title = "Distribution of Local FDI Exposure Between the 2009 and 2014 Elections",
    subtitle = "Own constituency plus touching constituencies; announced and opened projects"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p_fdi_levels)

ggsave(
  file.path(out_figure_dir, "02_fdi_2009_2014_histograms.png"),
  p_fdi_levels,
  width = 11.5,
  height = 4.4,
  dpi = 400
)
ggsave(
  file.path(out_figure_dir, "02_fdi_2009_2014_histograms.pdf"),
  p_fdi_levels,
  width = 11.5,
  height = 4.4
)

# ============================================================
# 5. FIGURE C: EARLY-TO-LATE CHANGE IN FDI PER 100,000
# ============================================================
# Primary: change in ANNUALIZED project rates, because the requested
# early and late windows contain 21 and 20 months, respectively.

fdi_change_long <- plot_data |>
  dplyr::select(
    ac_uid,
    change_total_pc100k_annualized,
    change_mfg_pc100k_annualized,
    change_services_pc100k_annualized
  ) |>
  tidyr::pivot_longer(
    cols = -ac_uid,
    names_to = "measure",
    values_to = "change_pc100k_annualized"
  ) |>
  dplyr::mutate(
    measure = dplyr::recode(
      measure,
      change_total_pc100k_annualized = "Total FDI",
      change_mfg_pc100k_annualized = "Manufacturing FDI",
      change_services_pc100k_annualized = "Services FDI"
    ),
    measure = factor(
      measure,
      levels = c("Total FDI", "Manufacturing FDI", "Services FDI")
    )
  )

p_fdi_change <- ggplot(
  fdi_change_long |>
    dplyr::filter(!is.na(change_pc100k_annualized)),
  aes(x = change_pc100k_annualized)
) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~measure, nrow = 1, scales = "free_x") +
  labs(
    x = "Change in annualized FDI projects per 100,000 residents",
    y = "Number of constituencies",
    title = "Change in Local FDI Exposure Across Constituencies",
    subtitle = paste0(
      "Late window: Aug 2012-Mar 2014 minus early window: Apr 2004-Dec 2005; ",
      "own constituency plus touching constituencies"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p_fdi_change)

ggsave(
  file.path(out_figure_dir, "03_fdi_change_early_vs_late_histograms.png"),
  p_fdi_change,
  width = 11.5,
  height = 4.4,
  dpi = 400
)
ggsave(
  file.path(out_figure_dir, "03_fdi_change_early_vs_late_histograms.pdf"),
  p_fdi_change,
  width = 11.5,
  height = 4.4
)

# Also save the long-form plotting data used in Figures B and C.
readr::write_csv(
  fdi_level_long,
  file.path(out_data_dir, "fdi_2009_2014_histogram_data_long.csv")
)
readr::write_csv(
  fdi_change_long,
  file.path(out_data_dir, "fdi_change_histogram_data_long.csv")
)

# ============================================================
# 6. AUDIT SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("PAPER DISTRIBUTION HISTOGRAMS COMPLETE\n")
cat("============================================================\n")
cat("Constituencies in AC frame: ", nrow(plot_data), "\n", sep = "")
cat("Early FDI window months: ", EARLY_MONTHS, "\n", sep = "")
cat("Late FDI window months: ", LATE_MONTHS, "\n", sep = "")
cat("Figures: ", out_figure_dir, "\n", sep = "")
cat("Plot data: ", out_data_dir, "\n", sep = "")

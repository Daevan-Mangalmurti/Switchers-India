# ============================================================
# 23_paper_distribution_histograms_v1_0.R
#
# Descriptive constituency distributions for the paper:
#   Figure A: 2001 Muslim population share
#   Figure B: Apr 2009-Mar 2014 FDI projects per 100,000
#             (total, manufacturing, services)
#   Figure C: change in FDI between an early window
#             (Apr 2004-Dec 2005) and a late window
#             (Jul 2012-Mar 2014), by total/manufacturing/services.
#
# FDI measures are consumed directly from the canonical final data
# built by R/fdi.R:
#   - local exposure = own AC + touching ACs
#   - eligible statuses = announced + opened
#   - manufacturing/services use the central FDI sector taxonomy
#   - 60-month and 21-month windows are defined only in R/fdi.R
#
# Figure C plots the canonical late-minus-early difference between
# two equal 21-month windows. No annualization is applied.
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

out_root <- file.path(
  paths$derived_dir,
  "paper_figures",
  "descriptive_histograms_v1_0"
)

out_data_dir <- file.path(
  out_root,
  "data"
)

out_figure_dir <- file.path(
  out_root,
  "figures"
)

dir.create(
  out_data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  out_figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. LOAD CANONICAL FINAL DATA
# ============================================================

ac_year_path <- file.path(
  paths$final_dir,
  "ac_year.rds"
)

if (!file.exists(ac_year_path)) {
  stop(
    "Missing canonical final data: ",
    ac_year_path
  )
}

ac_year <- readRDS(
  ac_year_path
)

required_ac <- c(
  "ac_uid",
  "state_no",
  "ac",
  "year",
  "proxy_ac_pop",
  "muslim_share_2001_dist_proxy",
  "fdi_spatial_support",
  "fdi_total_local_all_n",
  "fdi_mfg_local_all_n",
  "fdi_services_local_all_n",
  "fdi_total_local_all_pc100k",
  "fdi_mfg_local_all_pc100k",
  "fdi_services_local_all_pc100k",
  "fdi_total_local_early21_n",
  "fdi_mfg_local_early21_n",
  "fdi_services_local_early21_n",
  "fdi_total_local_late21_n",
  "fdi_mfg_local_late21_n",
  "fdi_services_local_late21_n",
  "fdi_total_local_early21_pc100k",
  "fdi_mfg_local_early21_pc100k",
  "fdi_services_local_early21_pc100k",
  "fdi_total_local_late21_pc100k",
  "fdi_mfg_local_late21_pc100k",
  "fdi_services_local_late21_pc100k",
  "d_fdi_total_local_21m_pc100k",
  "d_fdi_mfg_local_21m_pc100k",
  "d_fdi_services_local_21m_pc100k"
)

missing_ac <- setdiff(
  required_ac,
  names(ac_year)
)

if (length(missing_ac) > 0L) {
  stop(
    "ac_year.rds missing canonical histogram variables: ",
    paste(
      missing_ac,
      collapse = ", "
    )
  )
}

# ============================================================
# 2. BUILD PLOT DATA FROM CANONICAL VARIABLES
# ============================================================

plot_data <- ac_year |>
  dplyr::filter(
    year == 2014L
  ) |>
  dplyr::distinct(
    ac_uid,
    .keep_all = TRUE
  ) |>
  dplyr::transmute(
    ac_uid,
    state_no,
    ac,
    proxy_ac_pop,
    fdi_spatial_support,
    muslim_share_2001_dist_proxy,
    muslim_share_2001_pct =
      100 *
      muslim_share_2001_dist_proxy,

    total_n_0914 =
      fdi_total_local_all_n,
    mfg_n_0914 =
      fdi_mfg_local_all_n,
    services_n_0914 =
      fdi_services_local_all_n,

    total_pc100k_0914 =
      fdi_total_local_all_pc100k,
    mfg_pc100k_0914 =
      fdi_mfg_local_all_pc100k,
    services_pc100k_0914 =
      fdi_services_local_all_pc100k,

    total_n_early =
      fdi_total_local_early21_n,
    mfg_n_early =
      fdi_mfg_local_early21_n,
    services_n_early =
      fdi_services_local_early21_n,

    total_pc100k_early =
      fdi_total_local_early21_pc100k,
    mfg_pc100k_early =
      fdi_mfg_local_early21_pc100k,
    services_pc100k_early =
      fdi_services_local_early21_pc100k,

    total_n_late =
      fdi_total_local_late21_n,
    mfg_n_late =
      fdi_mfg_local_late21_n,
    services_n_late =
      fdi_services_local_late21_n,

    total_pc100k_late =
      fdi_total_local_late21_pc100k,
    mfg_pc100k_late =
      fdi_mfg_local_late21_pc100k,
    services_pc100k_late =
      fdi_services_local_late21_pc100k,

    change_total_pc100k =
      d_fdi_total_local_21m_pc100k,
    change_mfg_pc100k =
      d_fdi_mfg_local_21m_pc100k,
    change_services_pc100k =
      d_fdi_services_local_21m_pc100k
  ) |>
  dplyr::arrange(
    state_no,
    ac
  )

assert_unique_rows(
  plot_data,
  "ac_uid",
  "paper distribution histogram data"
)

readr::write_csv(
  plot_data,
  file.path(
    out_data_dir,
    "constituency_distribution_plot_data.csv"
  )
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
    title = "Distribution of Local FDI Exposure, April 2009-March 2014",
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
# Primary: literal late-window minus early-window project counts per 100,000.
# Both windows contain exactly 21 months.

fdi_change_long <- plot_data |>
  dplyr::select(
    ac_uid,
    change_total_pc100k,
    change_mfg_pc100k,
    change_services_pc100k
  ) |>
  tidyr::pivot_longer(
    cols = -ac_uid,
    names_to = "measure",
    values_to = "change_pc100k"
  ) |>
  dplyr::mutate(
    measure = dplyr::recode(
      measure,
      change_total_pc100k = "Total FDI",
      change_mfg_pc100k = "Manufacturing FDI",
      change_services_pc100k = "Services FDI"
    ),
    measure = factor(
      measure,
      levels = c("Total FDI", "Manufacturing FDI", "Services FDI")
    )
  )

p_fdi_change <- ggplot(
  fdi_change_long |>
    dplyr::filter(!is.na(change_pc100k)),
  aes(x = change_pc100k)
) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~measure, nrow = 1, scales = "free_x") +
  labs(
    x = "Change in FDI projects per 100,000 residents",
    y = "Number of constituencies",
    title = "Change in Local FDI Exposure Across Constituencies",
    subtitle = paste0(
      "Late window: Jul 2012-Mar 2014 minus early window: Apr 2004-Dec 2005; ",
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
cat("Constituency frame: 2014 election ACs\n")
cat("Constituencies in AC frame: ", nrow(plot_data), "\n", sep = "")
cat(
  "FDI-spatially supported constituencies: ",
  sum(plot_data$fdi_spatial_support, na.rm = TRUE),
  "\n",
  sep = ""
)
cat(
  "FDI-spatially unsupported constituencies: ",
  sum(!plot_data$fdi_spatial_support, na.rm = TRUE),
  "\n",
  sep = ""
)
cat("Early FDI window: Apr 2004-Dec 2005, 21 months\n")
cat("Late FDI window: Jul 2012-Mar 2014, 21 months\n")
cat("FDI source: canonical variables from final/ac_year.rds\n")
cat("Figures: ", out_figure_dir, "\n", sep = "")
cat("Plot data: ", out_data_dir, "\n", sep = "")

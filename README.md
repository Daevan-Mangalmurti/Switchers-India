# Switchers India revised codebase

This directory is designed to be copied into:

`/Users/Daevan/Downloads/Switchers-India`

It does not move or rename the existing `data/` directory.

## Files

- `01_build_data.R`: rebuilds geography, elections, FDI, migration, Census context, NES respondent data, final datasets, dictionaries, and diagnostics.
- `02_explore_models.R`: interactive RStudio script. Edit a few selected variable names and inspect models in the Console, Viewer, and Plots pane. It writes no files.
- `03_export_results.R`: runs only specifications listed in `selected_specs` and exports selected tables, estimates, marginal effects, diagnostics, and figures.
- `R/`: source-specific cleaning and model helper modules.
- `config/`: reviewed district, language, FDI-sector, and FDI-status crosswalks.

## Run order

From the project root:

```r
source("01_build_data.R")
source("02_explore_models.R")
# Edit selected_specs in 03_export_results.R only after exploration.
source("03_export_results.R")
```

The default project root is `/Users/Daevan/Downloads/Switchers-India`. To use another location:

```r
Sys.setenv(SWITCHERS_ROOT = "/another/path/Switchers-India")
```

## Final datasets

The build writes to `data/derived/switchers_rewrite/final/`:

- `ac_year.csv` and `.rds`
- `ac_change.csv` and `.rds`
- `nes_respondent_analysis.csv` and `.rds`
- `ac_year_ideology_summary.csv` and `.rds`
- `data_dictionary.csv`
- geographic and coding crosswalks

## Important construction decisions

- Far-right parties: BJP, SHS, and MNS.
- No 2004–2009 vote-share change is constructed because constituency boundaries are not comparable.
- Local FDI exposure is own AC plus touching ACs.
- Migration uses interstate plus international migrants.
- Missing 2012–2013 migration is filled with each AC's mean reconstructed annual migration for 2009–2011; the imputation appears only in diagnostics.
- Both election-year rows retain the actual 2011 district Muslim, language, and SC/ST education levels. The 2001 baselines and 2001–2011 changes use harmonized district-lineage groups and are separate variables.
- Local-language measures use only languages validly mapped in both 2001 and 2011.
- The respondent analysis file remains one row per voter and includes far-right vote choice, ideology, party closeness, and contextual variables.

## SC/ST education source limitation

The supplied 2011 C-08SC/ST files permit the approved age-20-plus and age-25-plus measures. The supplied 2001 C-08 appendix files do not contain age-specific rows. The code therefore does not fabricate 2001 age-specific measures or changes. It retains 2001 age-7-plus robustness measures and writes an explicit diagnostic. A valid age-specific 2001 source is required before the preferred 2001–2011 SC/ST education changes can be populated.

## Optional working-age employment denominator

`employment_per_total_population` is always constructed. `employment_per_population_15plus` and `employment_per_population_15_64` are constructed only when valid 2011 C-13 files are placed in `data/pop/age_2011/`. Otherwise those variables remain missing and the diagnostic explains why.

## Required R packages

The scripts use R's native pipe and recent dplyr join safeguards. Use R 4.1 or newer and dplyr 1.1 or newer.

```r
install.packages(c(
  "tidyverse", "sf", "readxl", "haven", "janitor", "lubridate",
  "digest", "scales", "fixest", "marginaleffects", "modelsummary",
  "broom", "insight"
))

# Optional, only for the multilevel model in 02_explore_models.R:
install.packages("lme4")
```

## First run

Copy this directory's scripts, `R/`, and `config/` into the project root, leaving the existing `data/` tree in place. Then run `01_build_data.R` from RStudio. Review the files in `data/derived/switchers_rewrite/diagnostics/` before estimating models.

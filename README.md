# Switchers India

Reproducible empirical codebase for the India portion of the Switchers project, centered on whether globalization exposure is associated with BJP support among ideologically centrist voters and whether that relationship varies with local Muslim population share.

The canonical local project root is:

```text
/Users/Daevan/Downloads/Switchers-India
```

Set a different root with `SWITCHERS_ROOT` if needed.

## Current empirical architecture

The active paper analysis distinguishes four questions that should not be conflated.

1. **Center-specific assembly-constituency models.** The primary AC outcome is BJP support among centrist NES respondents. Current and baseline FDI are retained together, Muslim population share is the demographic moderator, and state fixed effects plus the registered constituency controls are used in the primary specifications.

2. **Center-specific voter models.** The primary voter outcome is whether a 2014 centrist respondent voted BJP. The registered mixed linear-probability models include state fixed effects, an AC random intercept, individual controls, constituency controls, and current plus baseline FDI.

3. **Ideology heterogeneity.** R38 asks the harder question of whether the FDI x Muslim relationship differs across Center, Left, Right, and Mixed respondents. The AC, native-voter, pooled-voter, and aggregate-contextual exercises have different estimands and inference procedures and are therefore reported separately.

4. **Contextual Center prevalence.** R38A3 is not a categorical ideology comparison. It asks whether the official 2014 BJP-vote relationship with FDI x Muslim share varies with the continuous share of 2014 ideology-complete NES respondents in an AC who are Center.

The primary empirical claim is the Center-specific relationship itself. The R38 heterogeneity audit does not support a blanket claim that the Manufacturing-FDI relationship is uniquely centrist.

## Data build

Run:

```r
source("01_build_data.R")
```

The main rewritten datasets are written under:

```text
data/derived/switchers_rewrite/final/
```

Key outputs include:

- `ac_year.csv` / `.rds`
- `ac_change.csv` / `.rds`
- `nes_respondent_analysis.csv` / `.rds`
- `ac_year_ideology_summary.csv` / `.rds`
- `data_dictionary.csv`

Important construction decisions include:

- local FDI exposure is own AC plus touching ACs;
- Total FDI is partitioned into Manufacturing and Services using `config/fdi_sector_taxonomy.csv`;
- baseline and current FDI are retained separately in the main models;
- constituency Muslim population share is based on the harmonized 2001 measure used in the registered empirical specifications;
- 2004-to-2009 vote-share changes are not constructed across noncomparable constituency boundaries.

## Primary Center models

The canonical Center-specific analyses are:

- `R/25_ac_centrist_bjp_fdi_canonical_v1_0.R`
- `R/26_voter_centrist_bjp_fdi_canonical_v1_0.R`

These freeze the main AC and voter samples and model families.

## Post-primary inference and robustness

Current post-primary scripts include:

- `R/27_post_primary_wald_diagnostics_v1_0.R`
- `R/27b_ac_ideology_outcome_heterogeneity_v1_0.R`
- `R/27c_ac_ideology_pairwise_wald_refinement_v1_0.R`
- `R/28_main_regression_table_models_v1_0.R`
- `R/29_manufacturing_marginal_effects_v1_0.R`
- `R/30a_construct_fdi_12m_temporal_robustness_v1_0.R`
- `R/30b_freeze_specification_curve_registry_v1_0.R`
- `R/30c_estimate_core_specification_curve_v1_0.R`
- `R/30d_refine_specification_curve_display_v1_0.R`
- `R/31a*` measurement and lineage audits
- `R/31b_estimate_demographic_context_robustness_v1_0.R`
- `R/31c_refine_demographic_context_display_v1_0.R`

`R/29b_manufacturing_marginal_effects_display_refinement_v1_0.R` is retained for the historical R33-R36 publication chain and Appendix A12 support inputs. It is no longer the authoritative Figure 5/A4 display generator.

## Descriptive and pre-R38 publication assembly

The publication build that produced the historical v1.3 artifact registry is:

- `R/32_build_main_descriptive_figures_v1_0.R`
- `R/33_freeze_paper_artifact_registry_v1_0.R`
- `R/34_render_publication_tables_v1_0.R`
- `R/35_build_appendix_distributions_and_taxonomy_v1_0.R`
- `R/36_build_final_nes_and_influence_appendix_v1_0.R`

`config/paper_artifacts_v1_3.csv` is retained unchanged as the pre-R38 publication freeze.

## R38 ideology-heterogeneity architecture

### R38A3: aggregate contextual moderation

`R/38a3_ac_2014_centrist_share_triple_v1_1.R`

- DV: official 2014 BJP AC vote share.
- Third moderator: continuous share of 2014 ideology-complete NES respondents in the AC who are Center.
- Tests current and baseline FDI x Muslim x Center-share triples.
- This is a contextual model, not a categorical Center-versus-Left/Right/Mixed comparison.

### R38B: ideology-specific AC outcomes

`R/38b_ac_four_ideology_heterogeneity_v1_1.R`

Uses ideology-specific BJP-share outcomes and union-native Wald comparisons of the FDI x Muslim coefficients.

### R38C3: native voter comparisons

`R/38c3_native_voter_cluster_bootstrap_wald_v1_0.R`

Fits ideology-specific voter mixed LPMs separately and estimates cross-model covariance by resampling whole ACs.

### R38C4: pooled four-ideology voter model

`R/38c4_voter_center_reference_four_ideology_wald_v1_0.R`

Uses one pooled Center-reference mixed LPM. The base current FDI x Muslim coefficient is the Center interaction. The ideology triple coefficients are differences from Center. Pairwise and omnibus tests are linear combinations of the same fitted model.

R38C4 supersedes R38C2 for the active pooled-Wald presentation. R38D's pooled precision diagnostics now read R38C4 directly.

### R38D: magnitude, support, and Figure 5

- `R/38d_magnitude_support_power_fig5_audit_v1_3.R`
- `R/38d_fig5_display_patch_v1_4.R`
- `R/38d_fig5_zoom_positive_to_p95_v1_0.R`

Figure 5 display requirements are:

- point-estimate line;
- 90% confidence interval;
- 95% confidence interval;
- zero-effect line;
- explicit legend/key;
- empirical exposure support;
- no model re-estimation for display changes.

Both **Option A** and **Option D** are retained as paper-ready alternatives. Option A is the compact/default registered rendering. Option D places an aligned support histogram below the marginal-effect curve.

### R38E: final inference architecture

`R/38e_final_wald_architecture_summary_v1_1.R`

This is a reporting synthesis only and does not estimate models.

## R39 marginal-effect display standard

`R/39_marginal_effect_display_standard_v1_1.R`

R39 applies the R38D visual grammar to the remaining active Manufacturing marginal-effect displays without re-estimating models.

It covers:

- Appendix Figure A4: log1p Manufacturing FDI, +1-pp Muslim-share effect;
- Appendix Figure A5: +1 Manufacturing project per 100,000 across Muslim population share;
- the 10-pp scale as an internal/review display.

For A4 and A5, both **Option A** and **Option D** are retained. Options B/C remain review-only.

## Post-R38 publication registry

`R/40_post_r38_publication_registry_v1_0.R`

R40 preserves `config/paper_artifacts_v1_3.csv` unchanged and creates:

- `config/paper_artifacts_v1_4.csv`: current authoritative publication registry;
- `config/paper_artifact_alternates_v1_0.csv`: retained Option-D alternatives;
- `config/paper_display_decisions_v1_1.csv`: current display decisions.

The compact Option-A rendering is the default `publication_artifact`; Option D is explicitly registered as an alternate rather than silently discarded.

## Final certification

The current post-R38 certification script is:

```text
R/41_final_publication_assembly_and_certification_post_r38_v1_0.R
```

It is derived from the prior R37 final certification and updated to:

- certify `paper_artifacts_v1_4.csv`;
- verify and copy retained alternate artifacts;
- parse the active R38/R39/R40 scripts;
- carry the current display-decision and alternate registries into the frozen final bundle.

The final bundles are intentionally separate:

- `outputs/paper_outputs_final_v1_0/`: historical R37 bundle using `paper_artifacts_v1_3.csv`;
- `outputs/paper_outputs_final_post_r38_v1_0/`: current R41 bundle using `paper_artifacts_v1_4.csv`.

## Interpretation of the R38 heterogeneity results

The heterogeneity exercises are not collapsed into one headline test.

- The corrected R38A3 contextual triple does not show conventional evidence that the FDI x Muslim relationship varies with AC Center prevalence.
- AC ideology-specific outcomes yield differences for some Center-versus-Mixed comparisons.
- Native voter comparisons are particularly imprecise for Left and Right because those groups have limited respondent and positive-FDI support.
- The pooled R38C4 model improves precision and finds evidence of Total-FDI heterogeneity for Left versus Center, but it does not establish a significant Center-specific Manufacturing difference.

Nonsignificant heterogeneity tests should therefore be described as imprecise evidence, not proof that ideology groups respond identically.

## FDI sector classification

The authoritative industry mapping is:

```text
config/fdi_sector_taxonomy.csv
```

It defines which registered FDI activities are classified as Manufacturing versus Services and supplies the appendix taxonomy.

## Repository hygiene

Superseded untracked R38 drafts were archived outside the repository before removal, with SHA-256 hashes, under:

```text
~/Downloads/Switchers-India_precleanup_untracked_2026-08-25/
```

Historical tracked files remain recoverable through Git history. Legacy root-level Rmd/R scripts with no current dependencies are removed from the active tree rather than left beside the numbered pipeline.

Generated `outputs/` and `logs/` are runtime products and should not be staged as source changes.

## R packages

Core packages include:

```r
install.packages(c(
  "tidyverse",
  "sf",
  "readxl",
  "haven",
  "janitor",
  "lubridate",
  "digest",
  "scales",
  "fixest",
  "marginaleffects",
  "modelsummary",
  "broom",
  "insight",
  "lme4",
  "patchwork"
))
```

Use R 4.1 or newer and a recent dplyr version with join relationship checks.

# Switchers-India empirical model registry

## Purpose

This document records the intended empirical architecture for the paper.

It is the reference point for code consolidation. A script should remain in the
active pipeline only if it constructs data, estimates a listed model, produces a
listed robustness check, or generates a required descriptive/paper artifact.

Historical exploratory analyses remain recoverable from Git history and the
`pre-cleanup-2026-08-22` tag.

## Interpretation

The current research design does not provide a validated source of exogenous FDI
variation. Regression estimates are therefore interpreted as conditional
associations rather than causal effects of FDI.

The attempted IV strategies and pre-outcome stopping rules are documented in:

`docs/iv_attempts_and_identification_notes.md`

## 1. Global descriptive analysis

### Purpose

Establish the broader descriptive puzzle that centrist voters increasingly
support far-right parties.

### Canonical source

Current WVS/EVS party-rewrite pipeline, version 6.1.

### Status

Retain and integrate into the main project structure.

## 2. India descriptive analysis

### Core descriptive figure

Survey-weighted BJP vote share by ideological group in the 2009 and 2014
National Election Studies.

Ideological groups:

- Left
- Center
- Right
- Mixed

### Additional descriptive outputs

Retain or regenerate as appropriate:

- top parties by ideological group and year
- income distributions by ideological group
- education distributions by ideological group
- original ideology-item response diagnostics
- ideology-classification audits
- Muslim population-share distribution
- FDI exposure distributions

Survey weights are appropriate for NES descriptive quantities.

## 3. Primary demographic context

### Primary moderator

2001 Muslim population share.

This is the main demographic-context variable in the paper.

### Demographic robustness families

Appendix/specification-curve robustness may include:

- change in Muslim population share
- 2001 migrant population share
- change in migrant population share
- 2001 male migrant population share
- change in male migrant population share
- Bengali/Bhojpuri language-population share
- change in Bengali/Bhojpuri language-population share

Migration is not a co-equal headline moderator.

## 3A. 2011 AC population, SC, and ST construction

### Analysis geography

Population, Scheduled Caste population, and Scheduled Tribe population are
defined on post-2008-delimitation assembly-constituency boundaries.

### Preferred direct source

The canonical direct source is SHRUG's 2011 Population Census Abstract
aggregated to AC08 geography:

`data/shrug/pc11_pca_clean_con08.dta`

The exact input is checksum-pinned in:

`config/population_2011_source_manifest.csv`

Use the following direct values whenever observed:

- `pc11_pca_tot_p`: total 2011 Census population
- `pc11_pca_p_sc`: 2011 Scheduled Caste population
- `pc11_pca_p_st`: 2011 Scheduled Tribe population

Direct AC08 values are retained rather than replaced merely to force
reconciliation with district totals.

### Population imputation

When direct 2011 AC population is unavailable, imputation uses 2011 information
only.

Imputation arithmetic is performed over the union of the project's full
post-2008 AC reference and spatial AC reference. Polygon availability must not
determine which AC populations are subtracted from a district total. Spatial
support is handled separately when constructing spatial FDI exposure.

Within the mapped 2011 district:

1. If no AC has a direct PCA11 population, divide the 2011 district population
   equally across the mapped ACs.
2. If some ACs have direct PCA11 population and the remaining district
   population is positive, divide the residual equally across ACs lacking a
   direct value.
3. If the district residual is nonpositive, use the 2011 district mean as an
   explicit fallback rather than constructing zero or negative AC population.
4. If no usable 2011 district population is available, use the official 2011
   state mean population: the state's 2011 Census population divided by the
   number of post-2008 ACs in the project population-reference universe.

State residual population is not used as the final fallback. Geographic and
district-crosswalk mismatches can accumulate in a state residual and make the
remaining AC absorb reconciliation error. The state mean is therefore the
pre-specified final 2011-only fallback.

Every final value retains a source flag.

### SC and ST construction

Use direct PCA11 AC SC and ST counts whenever observed.

For a missing direct SC or ST count, use the corresponding existing 2011 Census
district SC/ST total.

When the residual after subtracting observed direct AC counts is valid, allocate
that residual across missing ACs in proportion to their final 2011 AC
population.

When the district residual is invalid, apply the district's 2011 SC or ST share
to the final 2011 AC population.

If no usable district SC or ST source is available, apply the official 2011
state SC or ST share to the final 2011 AC population. State residual SC/ST
counts are not used as the final fallback, for the same geographic-reconciliation
reason as total population.

Structural-zero states remain coded as zero where appropriate.

SC and ST shares are calculated only after final counts have been constructed:

`sc_pop_share = sc_population_ac / proxy_ac_pop`

`st_pop_share = st_population_ac / proxy_ac_pop`

### Prohibited population input

`con08_pc01_pca_tot_p` is not part of the canonical population, SC, or ST
construction.

`data/shrug/con08_pop_area_key.csv` remains available for constituency land area
and diagnostics.

## 4. FDI treatment definition

### Primary sector measure

Total FDI.

### Primary geography

Local FDI:

focal assembly constituency + all touching assembly constituencies.

### Primary scaling

Raw FDI projects per 100,000 people.

Zero is retained as a substantively meaningful exposure value only when the
assembly constituency has usable post-2008 spatial support.

`fdi_spatial_support == TRUE` means that the AC has a usable polygon and its
own, touching-neighbor, and local FDI exposures can be measured. For such ACs,
the absence of matched projects is a genuine zero.

If an election AC lacks usable polygon support, FDI project counts, per-100,000
measures, log-transformed measures, and any-FDI indicators are coded missing
rather than zero.

A spatially supported polygon with zero touching neighbors remains supported.
Its adjacent exposure is zero and its local exposure equals its own-AC exposure.

### Baseline/current exposure windows

Use two nonoverlapping 60-month calendar windows aligned to the 2009 and 2014
election cycles.

Baseline exposure:

- April 2004 through March 2009
- code boundary: `project_month >= 2004-04-01`
- code boundary: `project_month < 2009-04-01`

Current exposure:

- April 2009 through March 2014
- code boundary: `project_month >= 2009-04-01`
- code boundary: `project_month < 2014-04-01`

Each interval contains exactly 60 months. April 2009 belongs to the current
window and April 2014 is excluded.

Because the FDI source identifies project month rather than an exact within-month
date, these should be described as calendar windows aligned to the election
cycles rather than as exact post-election intervals.

These boundaries must remain centralized in the FDI construction code rather
than being redefined separately by model scripts.

### Sector robustness

- manufacturing FDI
- services FDI

### Spatial robustness

- own-AC FDI

Neighbor-only exposure is not part of the planned robustness set.

### Functional-form robustness

`log1p` FDI exposure.

This is a robustness transformation, not the primary functional form.

## 5. Alternative 21-month FDI-change treatment

### Early period

April 2004 through December 2005:

- `project_month >= 2004-04-01`
- `project_month < 2006-01-01`

### Late period

July 2012 through March 2014:

- `project_month >= 2012-07-01`
- `project_month < 2014-04-01`

Both windows contain exactly 21 months. Do not annualize the two windows merely
for comparability, because their durations are identical.

### Treatment

Late-period FDI minus early-period FDI, using the same population denominator.

Construct separately for:

- total FDI
- manufacturing FDI
- services FDI

Primary geography remains local; own-AC versions may be retained as robustness.

### Model parameterization

When change in FDI is used, baseline early-period FDI must also enter the model.

For example:

2014 outcome ~ Muslim share x change in FDI
             + Muslim share x early-period FDI
             + controls
             + state fixed effects

Do not estimate a change-in-FDI-only specification as the main version.

If a logarithmic robustness version is used, define change as:

`log1p(late FDI) - log1p(early FDI)`

not:

`log1p(late FDI - early FDI)`.

This treatment-change design is distinct from a first-difference political
outcome model.

## 6. Primary constituency-level model

### Sample

2014 assembly constituencies in which BJP fielded a candidate, with the variables
required by the final specification.

Require:

`bjp_candidate_present == TRUE`

This makes the estimand conditional on BJP electoral availability. Constituencies
in which BJP did not field a candidate are not treated as zero-BJP-support
constituencies, because BJP non-contestation may reflect alliance seat-sharing
rather than voter rejection of an available BJP candidate.

### Outcome

`centrist_bjp_share_weighted_2014`

This is the survey-weighted mean of `voted_bjp` among 2014 NES respondents who:

- are classified as Center
- are assigned to the assembly constituency
- have a valid reported vote

The constituency enters the canonical regression only when
`bjp_candidate_present == TRUE`.

The canonical AC analysis dataset should also retain supporting audit quantities,
including the number of centrist respondents, the number of centrist valid
voters, the weighted centrist valid-voter total, and the corresponding
unweighted BJP share.

### Primary exposure

Raw total local FDI per 100,000 during 2009-2014.

### Baseline exposure

Raw total local FDI per 100,000 during 2004-2009.

### Primary interaction

2001 Muslim population share x current FDI.

### Required baseline interaction

2001 Muslim population share x baseline FDI.

### Conceptual specification

Y_i,2014 =
  beta1 current_FDI_i
  + beta2 Muslim_i
  + beta3 current_FDI_i x Muslim_i
  + beta4 baseline_FDI_i
  + beta5 baseline_FDI_i x Muslim_i
  + controls_i
  + state fixed effects
  + error_i

### Fixed effects

State fixed effects.

Assembly-constituency fixed effects are not possible in the one-observation-
per-AC 2014 cross-sectional model because they would absorb the AC-level
regressors.

### Controls

The canonical control set should be intentionally small and pre-specified.

Current retained contextual controls include:

- 2011 constituency population using the frozen direct-plus-2011-only-imputation rule
- land area
- 2011 SC population share using the same source hierarchy
- 2011 ST population share using the same source hierarchy
- employment intensity
- secondary-education share (`ed_sec_share`)

SECC consumption, household, and tax variables are not part of the intended
primary control set.

### Inference

Primary standard errors are clustered by parliamentary constituency using
`pc_cluster_id`.

This clustering level reflects the shared Lok Sabha contest faced by assembly
constituencies nested within the same parliamentary constituency.

Use the same PC-clustered covariance matrix for coefficient tables,
confidence intervals, and marginal-effect calculations.

State-clustered standard errors should be retained as an inference sensitivity
analysis. A spatial covariance sensitivity may be added if feasible, but is not
part of the primary specification.

## 7. Constituency-level alternative treatment model

Use the same 2014 outcome, demographic context, controls, and state fixed
effects as the primary constituency model.

Replace the election-window exposure parameterization with:

- 21-month change in FDI
- early-period 21-month FDI baseline
- Muslim share x change in FDI
- Muslim share x early-period FDI

This is a treatment-definition robustness check.

It is not a first-difference outcome model.

## 8. Constituency-level contextual triple-interaction robustness

### Outcome

2014 official BJP constituency vote share, restricted to constituencies in which
BJP fielded a candidate:

`bjp_candidate_present == TRUE`

### Purpose

Test whether the Muslim-context x FDI relationship is stronger in
constituencies with a larger pre-existing centrist electorate.

### Interaction

FDI x 2001 Muslim share x constituency centrist share.

### Centrist-context measure

Use the pre-treatment/baseline 2009 survey-weighted share of Center respondents
among ideology-complete NES respondents:

`nes_weighted_share_center_among_ideology_complete_2009`

Survey weights are appropriate here because this variable estimates
constituency-level ideological composition. This does not imply survey weighting
of the primary individual-level multilevel model.

Use all constituencies for which this measure is estimable. Do not impose an
arbitrary minimum respondent-count threshold in the canonical specification.

Retain `nes_n_ideology_complete_2009` as a measurement-support diagnostic and
report the distribution of the number of ideology-complete 2009 NES respondents
underlying the constituency-level centrist-share estimates. Sample-size
restrictions may be examined as appendix sensitivity analyses if useful.

### Status

Robustness analysis, not the primary constituency outcome.

## 9. Primary voter-level model

### Sample

2014 NES respondents classified as centrists who:

- have a valid reported vote
- are assigned to an assembly constituency
- live in a constituency in which BJP fielded a candidate

Require:

`bjp_candidate_present == TRUE`

The primary voter-level estimand is therefore conditional on BJP electoral
availability. Constituencies in which BJP did not field a candidate are excluded
rather than treating the inability to cast a BJP vote as voter rejection of BJP.

### Outcome

`voted_bjp`, coded 0/1.

### Model family

Two-level linear probability model:

- voters nested within assembly constituencies
- random intercept for assembly constituency
- state fixed effects

### Primary exposure

Raw total local FDI per 100,000.

### Primary demographic context

2001 Muslim population share.

### Interaction

FDI x Muslim population share.

### Weighting

Primary multilevel LPM is unweighted.

NES survey weights remain appropriate for descriptive survey quantities but are
not treated as design weights through `lme4::lmer(weights = ...)`.

A weighted model may be considered only as a clearly labeled sensitivity
analysis.

### Controls

Individual-level controls plus the canonical constituency-level controls.

Final individual-control definitions must be frozen in the canonical voter
model script.

### Inference terminology

An AC random intercept is not the same thing as clustered standard errors.

Do not describe the primary voter model as having clustered SEs unless a
separate robust clustered covariance estimator is actually used.

## 10. Voter-level ideological robustness

Estimate corresponding BJP-vote models for:

- Left respondents only
- Right respondents only

These are comparison/robustness models, not additional primary outcomes.

Also retain an all-voter model with ideology interacting with FDI and Muslim
context as a robustness specification.

## 11. Sector, geography, and functional-form robustness

For the primary AC and voter analyses, relevant robustness checks may vary:

### Sector

- total FDI: primary
- manufacturing FDI: robustness/comparison
- services FDI: robustness/comparison

### Geography

- local own+touching: primary
- own AC only: robustness

### Functional form

- raw projects per 100,000: primary
- `log1p` projects per 100,000: robustness

A predefined influence sensitivity such as trimming extreme exposure values may
be considered, but should be specified before inspecting whether it improves
the focal result.

## 12. Marginal-effects presentation

### Primary interpretation

Treat Muslim population share as the focal contextual quantity and FDI as the
moderator.

Plot:

- x-axis: FDI exposure
- y-axis: estimated change in BJP support, in percentage points, associated
  with a 1-percentage-point increase in Muslim population share
- 95% confidence interval

### Support display

Place a compact histogram of the actual FDI distribution from the corresponding
regression sample beneath the marginal-effect plot using the same x-axis.

### Required saved data

For every reported marginal-effect figure save:

- FDI grid
- estimate
- standard error
- confidence interval
- sample-support information

Also save a small table of selected FDI quantiles and corresponding effects.

### Preferred estimand

Where feasible, use an average counterfactual marginal effect at each FDI value,
retaining each observation's actual baseline covariates, rather than fixing all
controls at arbitrary medians.

## 13. Specification curves

Specification curves are robustness summaries rather than the central empirical
design.

The final repository should retain only:

- the small number of specification-curve figures actually reported
- the underlying result files needed to reproduce them
- a compact reproducible runner
- manifests defining included specifications

Large pilot generations and superseded versions should not remain in the active
pipeline.

Primary-versus-robustness choices in the curves must match this registry.

## 14. Descriptive FDI histograms

Generate constituency distributions for:

### Main election-window exposure

2009-2014:

- total local FDI per 100,000
- manufacturing local FDI per 100,000
- services local FDI per 100,000

### 21-month change

Late July 2012-March 2014 minus early April 2004-December 2005:

- total local FDI per 100,000
- manufacturing local FDI per 100,000
- services local FDI per 100,000

Because both intervals contain 21 months, annualization is not required merely
to make their durations comparable.

## 15. Muslim-share descriptive histogram

Generate the distribution of assembly constituencies by 2001 Muslim population
share.

Also save a CSV summary containing at least:

- N
- mean
- median
- selected percentiles
- histogram bin counts
- histogram bin shares

## 16. FDI sector taxonomy

`config/fdi_sector_taxonomy.csv` is to become the sole authoritative editable
classification of fDi Markets activities into the project's manufacturing and
services families.

The intended chain is:

paper classification
-> `config/fdi_sector_taxonomy.csv`
-> `R/fdi.R`
-> derived FDI variables
-> descriptive/table labels

Derived copies of the taxonomy may be written for auditing, but should not be
edited independently.

The final FDI build must validate:

- every included project activity maps to the taxonomy
- total FDI equals manufacturing plus services within the included universe
- local exposure equals own plus adjacent exposure
- the taxonomy/config version or hash is recorded in build provenance

## 17. Analyses not in the active paper design

The following are not part of the current paper analysis:

- candidate-supply/BJP-contestation as a separate outcome or explanatory branch
- pooled AC models as a primary design
- first-difference political-outcome model
- neighbor-only FDI exposure
- IV/2SLS estimates

IV attempts are documented separately for scientific provenance.

BJP candidate presence remains an analytic-sample restriction for BJP-specific
outcomes. The final outputs should include a descriptive sample audit showing
the number and share of constituencies and respondents retained by this
restriction and basic included-versus-excluded constituency characteristics.

## 18. Remaining decisions before final model freeze

The following items must be resolved before the canonical model scripts are
declared final:

1. Final individual-level voter control set.
2. Final paper/appendix placement of the retained specification-curve families.

These unresolved items should be resolved explicitly rather than inherited from
legacy scripts.

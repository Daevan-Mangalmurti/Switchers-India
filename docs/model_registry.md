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

## 4. FDI treatment definition

### Primary sector measure

Total FDI.

### Primary geography

Local FDI:

focal assembly constituency + all touching assembly constituencies.

### Primary scaling

Raw FDI projects per 100,000 people.

Zero is retained as a substantively meaningful exposure value.

### Baseline/current exposure windows

Primary lagged-exposure framework:

- baseline exposure: 2004 election to 2009 election
- current exposure: 2009 election to 2014 election

Exact project-date boundaries must remain centralized in the FDI construction
code rather than redefined separately by model scripts.

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

April 2004 through December 2005.

### Late period

July 2012 through March 2014.

Both windows contain 21 months.

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

2014 assembly constituencies with the variables required by the final
specification.

### Outcome

2014 BJP support among centrist NES respondents aggregated to the constituency
level.

The exact persisted variable name and weighting definition must be documented
when the final model script is frozen.

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

- constituency population proxy
- land area
- SC population share
- ST population share
- employment intensity
- secondary-education share (`ed_sec_share`)

SECC consumption, household, and tax variables are not part of the intended
primary control set.

### Inference

UNRESOLVED BEFORE FINAL FREEZE:

- exact primary AC-level clustering unit

The final choice must be stated once in the canonical model script and used
consistently in paper tables and marginal-effect calculations.

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

2014 official BJP constituency vote share.

### Purpose

Test whether the Muslim-context x FDI relationship is stronger in
constituencies with a larger pre-existing centrist electorate.

### Interaction

FDI x 2001 Muslim share x constituency centrist share.

### Centrist-context timing

Use the pre-treatment/baseline centrist measure from 2009.

### Reliability

A minimum baseline NES respondent count is required for constituency-level
centrist-share measures.

The exact centrist-share denominator/variable must be selected and frozen before
the final script is designated canonical.

### Status

Robustness analysis, not the primary constituency outcome.

## 9. Primary voter-level model

### Sample

2014 NES respondents classified as centrists.

Do not condition the primary sample on BJP candidate presence.

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

- candidate-supply/BJP-contestation branch
- pooled AC models as a primary design
- first-difference political-outcome model
- neighbor-only FDI exposure
- IV/2SLS estimates

IV attempts are documented separately for scientific provenance.

## 18. Remaining decisions before final model freeze

The following items must be resolved before the canonical model scripts are
declared final:

1. Exact persisted variable name and construction for the primary 2014
   constituency centrist-BJP outcome.
2. Exact primary AC-level clustering unit.
3. Exact 2009 constituency centrist-share variable/denominator for the
   contextual triple-interaction robustness.
4. Final individual-level voter control set.
5. Exact date-boundary implementation for the election-window FDI variables,
   verified against the paper wording.
6. Final paper/appendix placement of the retained specification-curve families.

These unresolved items should be resolved explicitly rather than inherited from
legacy scripts.

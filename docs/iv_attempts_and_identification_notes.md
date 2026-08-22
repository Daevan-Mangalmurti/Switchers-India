# IV attempts and identification notes

## Status

No instrumental-variable specification is currently used for the political
results in this project.

All IV routes described below were screened using first-stage, placebo,
concentration, mapping, and leave-one-shock diagnostics before inspecting
any BJP/political 2SLS coefficient.

The current empirical claims should therefore be interpreted as conditional
associations rather than point-identified causal effects of FDI.

## Pre-outcome credibility criteria

A candidate instrument was required to satisfy four conditions before any
political second stage would be considered:

1. Meaningful first-stage relevance for post-2009 FDI.
2. Weak predictiveness of pre-2009 FDI in the corresponding placebo test.
3. Stability to leave-one-industry or leave-one-source-country diagnostics.
4. Sufficiently diffuse, plausibly external variation with a defensible
   exclusion restriction.

The conventional first-stage F statistic of approximately 10 was treated as
a screening heuristic rather than evidence of instrument validity by itself.

## Route 1: EC05 x global-industry WIR shift-share

### Construction

Predetermined 2005 Economic Census employment composition was measured at the
assembly-constituency level using SHRUG SHRIC industries.

The initial manufacturing Bartik took the form

    Z_i = sum_k s_ik,2005 * g_k

where s_ik,2005 is baseline industrial employment exposure and g_k is subsequent
world greenfield-FDI growth in industry k.

Because the initial construction mechanically incorporated an AC's overall
manufacturing intensity, the final diagnostic re-normalized employment shares
within manufacturing. This asks whether baseline manufacturing composition,
conditional on overall manufacturing intensity, predicts subsequent
manufacturing FDI.

### Best diagnostic result

The strongest defensible own-AC specification was the within-manufacturing,
high-quality-EC05 version:

- post-period first-stage F = 10.25
- pre-period placebo F = 1.11
- effective industry count = approximately 3.37
- top three industries account for approximately 86.7% of mean exposure weight

Leave-one-industry-out post-period F statistics fell to approximately:

- 4.25 excluding textiles/clothing/leather
- 5.45 excluding non-metallic minerals
- 6.72 excluding food/beverages/tobacco

### Why it was not promoted to a causal IV

The apparently acceptable headline first stage is concentrated in a small
number of industries and is not stable to important leave-one-industry-out
tests.

The exclusion restriction is also difficult to defend because global shocks
to textiles, food processing, non-metallic minerals, and related industries
could affect Indian employment, output, trade, prices, or politics directly,
rather than solely through local greenfield FDI.

The WIR industry shocks used here are world-inclusive rather than true
rest-of-world shocks because the available WIR data do not provide the
destination-by-sector structure needed to subtract India sector by sector.

Status: exploratory only; not a publication-quality primary IV.

## Route 2: geographically matched local WIR Bartik

### Construction

Because the paper's canonical observational exposure is local FDI, defined as
FDI in the focal AC plus touching ACs, a geographically matched instrument was
constructed.

2005 EC05 employment was pooled over the same own-plus-touching-AC neighborhood
before calculating industry shares and interacting them with WIR shocks.

### Diagnostics

The local specification does not pass the pre-outcome screen.

The strict-neighborhood version has a weak post-period first stage while
strongly predicting pre-period FDI.

The high-quality-neighborhood version improves the post-period first stage but
continues to show substantial pre-period predictiveness.

Representative adjudication results were:

- strict: post F approximately 3.7, placebo F approximately 10
- high-quality: post F approximately 12.7, placebo F approximately 7.8

The exact values vary slightly across intermediate versus final
within-manufacturing implementations, but the substantive conclusion does not.

### Why it was rejected

Matching the instrument geography to the treatment geography does not eliminate
the central problem: the proposed instrument predicts persistent historical FDI
location patterns.

Status: rejected as a primary IV.

## Route 3: official Indian FDI-policy reforms

### Construction

Sector-specific FDI reforms during 2009-2014 were audited using official
DIPP/DPIIT Press Notes and PIB material.

Reforms were cross-walked separately to:

1. predetermined 2005 EC05/SHRIC industrial exposure; and
2. the project's fDi Markets project-sector classification.

Cap changes, route changes, state-option reforms, and eligibility changes were
not collapsed into an arbitrary common policy index.

### Diagnostic conclusion

No national reform during the relevant period maps cleanly both to predetermined
manufacturing exposure and to the project's greenfield manufacturing-FDI
treatment.

The cleanest EC05 policy exposures are:

- scheduled/non-scheduled air transport
- courier services
- telecom services

These are services rather than manufacturing exposures.

Telecom is the cleanest policy-to-EC05-to-fDi Markets mapping, but only six
Communications projects occur after the August 2013 telecom reform and before
the April 2014 FDI cutoff. The Communications category also overlaps with an
earlier broadcasting reform.

Multi-brand retail is unsuitable as a simple national shock because
implementation was left to states/UTs, making adoption itself a potentially
endogenous political choice.

Status: no usable headline manufacturing-policy IV with current data.

## Route 4: source-country-push shift-share

### Construction

The source-country design combined:

1. subsequent outward greenfield-FDI shocks by source country;
2. pre-2009 source-country dependence of Indian industries; and
3. predetermined 2005 AC manufacturing composition.

Approximate rest-of-world source shocks were constructed by subtracting
India-bound projects from world source-country project counts.

This is not a true source-country-by-industry global shock because the WIR data
do not provide that joint structure.

### Diagnostics

Source country could be matched for approximately 97.7% of geocoded Indian FDI
projects, and the source-country shocks were more diffuse than the industry-only
Bartik.

However, the placebo test failed:

- full sample: post F = 3.78; placebo F = 9.72
- high-quality sample: post F = 7.19; placebo F = 9.57

World-inclusive variants performed similarly or worse.

Leave-one-source-country tests did not reveal a single country whose exclusion
rescued the design.

### Why it was rejected

The instrument predicts historical FDI geography at least as strongly as new
post-2009 FDI. This is more consistent with persistent investor-location
networks or industrial selection than with a clean new external capital-supply
shock.

Status: rejected.

## What could revive an IV strategy

The existing failed routes should not be tuned further using political outcomes.

A future IV design would require genuinely new identifying variation or new
data. Promising possibilities include:

1. A true rest-of-world industry shock constructed from destination-by-industry
   greenfield-FDI data, allowing India to be removed sector by sector.
2. A true source-country-by-industry external shock rather than the current
   source-country shock projected through historical Indian industry dependence.
3. A policy reform with a clean, predetermined local exposure measure and a
   treatment category that can be isolated in the project-level FDI data.
4. Another quasi-experimental source of FDI location variation whose exclusion
   restriction can be defended independently of the observed BJP results.

Any revived design should be subjected to the same pre-outcome gates before
political second stages are estimated.

## Archived reproducibility

The complete executable IV scripts and generated diagnostics are preserved in:

- Git tag: `pre-cleanup-2026-08-22`
- verified full-project snapshot:
  `Switchers-India_PRE_CLEANUP_2026-08-22.tar.gz`

The active paper pipeline does not require the archived IV scripts.

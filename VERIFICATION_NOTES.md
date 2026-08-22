# Verification notes

## Checks completed before delivery

- All ten R scripts passed a static delimiter, quotation, and escape-sequence scan.
- Cross-file source order and function references were reviewed.
- The codebase contains no `fr_vote_share_candidate_present` variable and no 2004–2009 vote-share change.
- The FDI constructor explicitly crosses 3 sectors, 3 scopes, 3 statuses, and 3 scales, producing the approved 81-variable family.
- The common-language configuration has 293 state-language assignments, of which 283 have valid 2001 and 2011 mappings. No included state-year source code is duplicated.
- The district configuration has 647 links covering 592 named 2001 districts and 640 named 2011 districts. Twelve many-to-many links remain marked unresolved, so their change variables remain missing rather than being fabricated.
- Source layouts for the uploaded Lok Dhaba, FDI, Census, and NES inputs were checked while drafting the readers.

## Execution limitation

This environment does not contain an R runtime, so the scripts could not be executed end to end here. The first local run is designed to stop on missing columns, duplicate keys, invalid joins, or inconsistent source totals and to write source-specific diagnostics.

## Deliberate missing values

- Preferred 2001 SC/ST age-20-plus and age-25-plus education measures remain missing because the supplied 2001 C-08 appendices contain age-7-plus totals rather than the required age-specific rows.
- Working-age employment-intensity measures remain missing unless valid 2011 C-13 files are added to `data/pop/age_2011/`.
- Census change variables remain missing for district-lineage groups marked unresolved.

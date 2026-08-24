suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(fixest)
  library(lme4)
})

project_root <-
  Sys.getenv(
    "SWITCHERS_ROOT",
    unset = getwd()
  )

setwd(
  project_root
)

output_dir <-
  file.path(
    project_root,
    "outputs",
    "r31_corrected_employment_control_v1_0"
  )

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

economic_census <-
  read_csv(
    "data/shrug/ec13_pc11dist.csv",
    show_col_types = FALSE
  )

allocation <-
  read_csv(
    "data/derived/switchers_rewrite/intermediate/ac_allocation_weights.csv",
    show_col_types = FALSE
  )

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

ac_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

ac_models <-
  readRDS(
    "outputs/ac_canonical_v1_0/models.rds"
  )

voter_samples <-
  readRDS(
    "outputs/voter_canonical_v1_0/model_samples.rds"
  )

voter_models <-
  readRDS(
    "outputs/voter_canonical_v1_0/models.rds"
  )

required_ec <-
  c(
    "pc11_state_id",
    "pc11_district_id",
    "ec13_emp_all"
  )

missing_ec <-
  setdiff(
    required_ec,
    names(
      economic_census
    )
  )

if (
  length(
    missing_ec
  ) >
    0L
) {
  stop(
    "Economic Census data missing: ",
    paste(
      missing_ec,
      collapse = ", "
    )
  )
}

required_allocation <-
  c(
    "ac_uid",
    "state_no",
    "district_code_2011",
    "district_pop_2011"
  )

missing_allocation <-
  setdiff(
    required_allocation,
    names(
      allocation
    )
  )

if (
  length(
    missing_allocation
  ) >
    0L
) {
  stop(
    "Allocation data missing: ",
    paste(
      missing_allocation,
      collapse = ", "
    )
  )
}

if (
  !"AC02" %in%
    names(
      ac_samples
    ) ||
    !"AC02" %in%
      names(
        ac_models
      )
) {
  stop(
    "Canonical AC02 objects are unavailable."
  )
}

if (
  !"V02" %in%
    names(
      voter_samples
    ) ||
    !"V02" %in%
      names(
        voter_models
      )
) {
  stop(
    "Canonical V02 objects are unavailable."
  )
}

district_population <-
  allocation |>
  filter(
    !is.na(
      district_code_2011
    )
  ) |>
  summarise(
    n_population_values =
      n_distinct(
        district_pop_2011[
          is.finite(
            district_pop_2011
          )
        ]
      ),

    district_pop_2011 =
      if (
        any(
          is.finite(
            district_pop_2011
          )
        )
      ) {
        first(
          district_pop_2011[
            is.finite(
              district_pop_2011
            )
          ]
        )
      } else {
        NA_real_
      },

    .by =
      c(
        state_no,
        district_code_2011
      )
  )

population_conflicts <-
  district_population |>
  filter(
    n_population_values >
      1L
  )

if (
  nrow(
    population_conflicts
  ) >
    0L
) {
  print(
    population_conflicts,
    n = Inf,
    width = Inf
  )

  stop(
    "2011 district population is inconsistent within district."
  )
}

employment_district <-
  economic_census |>
  transmute(
    state_no =
      as.integer(
        pc11_state_id
      ),

    district_code_2011 =
      as.integer(
        pc11_district_id
      ),

    employment_total_ec13 =
      suppressWarnings(
        as.numeric(
          ec13_emp_all
        )
      )
  )

employment_duplicates <-
  employment_district |>
  count(
    state_no,
    district_code_2011,
    name =
      "n"
  ) |>
  filter(
    n >
      1L
  )

if (
  nrow(
    employment_duplicates
  ) >
    0L
) {
  print(
    employment_duplicates,
    n = Inf,
    width = Inf
  )

  stop(
    "Economic Census data are not unique by 2011 district."
  )
}

employment_district <-
  employment_district |>
  left_join(
    district_population |>
      select(
        state_no,
        district_code_2011,
        district_pop_2011
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "one-to-one"
  ) |>
  mutate(
    employment_intensity_ec13_per_2011_population =
      if_else(
        is.finite(
          employment_total_ec13
        ) &
          is.finite(
            district_pop_2011
          ) &
          district_pop_2011 >
            0,
        employment_total_ec13 /
          district_pop_2011,
        NA_real_
      )
  )

ac_map <-
  allocation |>
  select(
    ac_uid,
    state_no,
    district_code_2011
  ) |>
  distinct()

if (
  anyDuplicated(
    ac_map$ac_uid
  ) >
    0L
) {
  stop(
    "AC-to-district map is not unique by ac_uid."
  )
}

employment_ac <-
  ac_map |>
  left_join(
    employment_district |>
      select(
        state_no,
        district_code_2011,
        employment_total_ec13,
        district_pop_2011,
        employment_intensity_ec13_per_2011_population
      ),
    by =
      c(
        "state_no",
        "district_code_2011"
      ),
    relationship =
      "many-to-one"
  ) |>
  left_join(
    ac_change |>
      select(
        ac_uid,
        old_employment_per_total_population =
          employment_per_total_population
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

if (
  anyDuplicated(
    employment_ac$ac_uid
  ) >
    0L
) {
  stop(
    "Corrected employment artifact is not unique by ac_uid."
  )
}

ac02 <-
  ac_samples[[
    "AC02"
  ]] |>
  left_join(
    employment_ac |>
      select(
        ac_uid,
        employment_intensity_ec13_per_2011_population
      ),
    by =
      "ac_uid",
    relationship =
      "one-to-one"
  )

v02 <-
  voter_samples[[
    "V02"
  ]] |>
  left_join(
    employment_ac |>
      select(
        ac_uid,
        employment_intensity_ec13_per_2011_population
      ),
    by =
      "ac_uid",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    employment_intensity_ec13_pp =
      100 *
      employment_intensity_ec13_per_2011_population
  )

sample_audit <-
  bind_rows(
    tibble(
      sample =
        "AC02",

      n =
        nrow(
          ac02
        ),

      n_ac =
        n_distinct(
          ac02$ac_uid
        ),

      n_corrected_employment_complete =
        sum(
          is.finite(
            ac02$employment_intensity_ec13_per_2011_population
          )
        ),

      n_corrected_employment_missing =
        sum(
          !is.finite(
            ac02$employment_intensity_ec13_per_2011_population
          )
        )
    ),

    tibble(
      sample =
        "V02 respondents",

      n =
        nrow(
          v02
        ),

      n_ac =
        n_distinct(
          v02$ac_uid
        ),

      n_corrected_employment_complete =
        sum(
          is.finite(
            v02$employment_intensity_ec13_per_2011_population
          )
        ),

      n_corrected_employment_missing =
        sum(
          !is.finite(
            v02$employment_intensity_ec13_per_2011_population
          )
        )
    )
  )

if (
  any(
    sample_audit$n_corrected_employment_missing >
      0L
  )
) {
  print(
    sample_audit,
    n = Inf,
    width = Inf
  )

  stop(
    "Corrected employment is missing within an existing expanded-control sample."
  )
}

ac_old_refit <-
  feols(
    y ~
      muslim * fdi_current +
      muslim * fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share +
      employment_per_total_population +
      ed_sec_share |
      state_no,
    data =
      ac02,
    vcov =
      ~ pc_cluster_id,
    warn =
      TRUE,
    notes =
      TRUE
  )

ac_corrected <-
  feols(
    y ~
      muslim * fdi_current +
      muslim * fdi_baseline +
      proxy_ac_pop +
      sc_pop_share +
      st_pop_share +
      employment_intensity_ec13_per_2011_population +
      ed_sec_share |
      state_no,
    data =
      ac02,
    vcov =
      ~ pc_cluster_id,
    warn =
      TRUE,
    notes =
      TRUE
  )

voter_formula_old <-
  y ~
    muslim * fdi_total_current +
    muslim * fdi_total_baseline +
    ac_pop_100k +
    sc_share_pp +
    st_share_pp +
    religion_x +
    caste_x +
    education_x +
    state_fe +
    employment_share_pp +
    ed_sec_share_pp +
    (1 | ac_random)

voter_formula_corrected <-
  y ~
    muslim * fdi_total_current +
    muslim * fdi_total_baseline +
    ac_pop_100k +
    sc_share_pp +
    st_share_pp +
    religion_x +
    caste_x +
    education_x +
    state_fe +
    employment_intensity_ec13_pp +
    ed_sec_share_pp +
    (1 | ac_random)

voter_old_refit <-
  lmer(
    voter_formula_old,
    data =
      v02,
    REML =
      FALSE,
    control =
      lmerControl(
        optimizer =
          "bobyqa",
        optCtrl =
          list(
            maxfun =
              300000
          )
      )
  )

voter_corrected <-
  lmer(
    voter_formula_corrected,
    data =
      v02,
    REML =
      FALSE,
    control =
      lmerControl(
        optimizer =
          "bobyqa",
        optCtrl =
          list(
            maxfun =
              300000
          )
      )
  )

compare_fixest_coefficients <- function(
  saved,
  refit
) {
  saved_coef <-
    coef(
      saved
    )

  refit_coef <-
    coef(
      refit
    )

  common <-
    intersect(
      names(
        saved_coef
      ),
      names(
        refit_coef
      )
    )

  tibble(
    n_saved =
      length(
        saved_coef
      ),

    n_refit =
      length(
        refit_coef
      ),

    n_common =
      length(
        common
      ),

    max_absolute_difference =
      max(
        abs(
          saved_coef[
            common
          ] -
            refit_coef[
              common
            ]
        )
      )
  )
}

compare_lmer_coefficients <- function(
  saved,
  refit
) {
  saved_coef <-
    fixef(
      saved
    )

  refit_coef <-
    fixef(
      refit
    )

  common <-
    intersect(
      names(
        saved_coef
      ),
      names(
        refit_coef
      )
    )

  tibble(
    n_saved =
      length(
        saved_coef
      ),

    n_refit =
      length(
        refit_coef
      ),

    n_common =
      length(
        common
      ),

    max_absolute_difference =
      max(
        abs(
          saved_coef[
            common
          ] -
            refit_coef[
              common
            ]
        )
      )
  )
}

ac_reproduction <-
  compare_fixest_coefficients(
    ac_models[[
      "AC02"
    ]],
    ac_old_refit
  ) |>
  mutate(
    model =
      "AC02"
  )

voter_reproduction <-
  compare_lmer_coefficients(
    voter_models[[
      "V02"
    ]],
    voter_old_refit
  ) |>
  mutate(
    model =
      "V02"
  )

reproduction_audit <-
  bind_rows(
    ac_reproduction,
    voter_reproduction
  ) |>
  select(
    model,
    everything()
  )

if (
  ac_reproduction$n_saved !=
    ac_reproduction$n_refit ||
    ac_reproduction$n_saved !=
      ac_reproduction$n_common ||
    ac_reproduction$max_absolute_difference >
      1e-10
) {
  print(
    ac_reproduction,
    width = Inf
  )

  stop(
    "Standalone AC02 formula does not reproduce the saved canonical AC02 model."
  )
}

if (
  voter_reproduction$n_saved !=
    voter_reproduction$n_refit ||
    voter_reproduction$n_saved !=
      voter_reproduction$n_common ||
    voter_reproduction$max_absolute_difference >
      1e-8
) {
  print(
    voter_reproduction,
    width = Inf
  )

  stop(
    "Standalone V02 formula does not reproduce the saved canonical V02 model."
  )
}

extract_fixest_term <- function(
  model,
  candidates
) {
  b <-
    coef(
      model
    )

  term <-
    candidates[
      candidates %in%
        names(
          b
        )
    ]

  if (
    length(
      term
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify fixest focal term."
    )
  }

  term <-
    term[[1]]

  vv <-
    vcov(
      model
    )

  tibble(
    term =
      term,

    estimate =
      unname(
        b[[
          term
        ]]
      ),

    std_error =
      sqrt(
        vv[
          term,
          term
        ]
      )
  )
}

extract_lmer_term <- function(
  model,
  candidates
) {
  b <-
    fixef(
      model
    )

  term <-
    candidates[
      candidates %in%
        names(
          b
        )
    ]

  if (
    length(
      term
    ) !=
      1L
  ) {
    stop(
      "Could not uniquely identify lmer focal term."
    )
  }

  term <-
    term[[1]]

  vv <-
    as.matrix(
      vcov(
        model
      )
    )

  tibble(
    term =
      term,

    estimate =
      unname(
        b[[
          term
        ]]
      ),

    std_error =
      sqrt(
        vv[
          term,
          term
        ]
      )
  )
}

ac_old_focal <-
  extract_fixest_term(
    ac_models[[
      "AC02"
    ]],
    c(
      "muslim:fdi_current",
      "fdi_current:muslim"
    )
  )

ac_new_focal <-
  extract_fixest_term(
    ac_corrected,
    c(
      "muslim:fdi_current",
      "fdi_current:muslim"
    )
  )

voter_old_focal <-
  extract_lmer_term(
    voter_models[[
      "V02"
    ]],
    c(
      "muslim:fdi_total_current",
      "fdi_total_current:muslim"
    )
  )

voter_new_focal <-
  extract_lmer_term(
    voter_corrected,
    c(
      "muslim:fdi_total_current",
      "fdi_total_current:muslim"
    )
  )

focal_impact <-
  bind_rows(
    tibble(
      model =
        "AC02",

      old_estimate =
        ac_old_focal$estimate,

      old_std_error =
        ac_old_focal$std_error,

      corrected_estimate =
        ac_new_focal$estimate,

      corrected_std_error =
        ac_new_focal$std_error
    ),

    tibble(
      model =
        "V02",

      old_estimate =
        voter_old_focal$estimate,

      old_std_error =
        voter_old_focal$std_error,

      corrected_estimate =
        voter_new_focal$estimate,

      corrected_std_error =
        voter_new_focal$std_error
    )
  ) |>
  mutate(
    estimate_difference =
      corrected_estimate -
      old_estimate,

    absolute_estimate_difference =
      abs(
        estimate_difference
      )
  )

exact_sample_employment_comparison <-
  bind_rows(
    ac02 |>
      select(
        ac_uid,
        old =
          employment_per_total_population,
        corrected =
          employment_intensity_ec13_per_2011_population
      ) |>
      distinct() |>
      summarise(
        sample =
          "AC02",

        n_ac =
          n(),

        correlation =
          cor(
            old,
            corrected
          ),

        median_absolute_difference =
          median(
            abs(
              old -
                corrected
            )
          ),

        max_absolute_difference =
          max(
            abs(
              old -
                corrected
            )
          )
      ),

    v02 |>
      select(
        ac_uid,
        old =
          employment_share_pp,
        corrected =
          employment_intensity_ec13_pp
      ) |>
      distinct() |>
      summarise(
        sample =
          "V02 ACs",

        n_ac =
          n(),

        correlation =
          cor(
            old,
            corrected
          ),

        median_absolute_difference =
          median(
            abs(
              old -
                corrected
            )
          ),

        max_absolute_difference =
          max(
            abs(
              old -
                corrected
            )
          )
      )
  )

saveRDS(
  list(
    AC02_corrected =
      ac_corrected,

    V02_corrected =
      voter_corrected
  ),
  file.path(
    output_dir,
    "01_corrected_expanded_control_models.rds"
  )
)

write_csv(
  employment_district,
  file.path(
    output_dir,
    "02_corrected_employment_by_district.csv"
  )
)

write_csv(
  employment_ac,
  file.path(
    output_dir,
    "03_corrected_employment_by_ac.csv"
  )
)

saveRDS(
  employment_ac,
  file.path(
    output_dir,
    "04_corrected_employment_by_ac.rds"
  )
)

write_csv(
  sample_audit,
  file.path(
    output_dir,
    "05_exact_sample_availability.csv"
  )
)

write_csv(
  reproduction_audit,
  file.path(
    output_dir,
    "06_old_model_reproduction_audit.csv"
  )
)

write_csv(
  exact_sample_employment_comparison,
  file.path(
    output_dir,
    "07_old_vs_corrected_employment_exact_samples.csv"
  )
)

write_csv(
  focal_impact,
  file.path(
    output_dir,
    "08_focal_interaction_impact.csv"
  )
)

definitions <-
  tribble(
    ~variable,
    ~definition,
    ~preferred_label,
    ~status,

    "employment_intensity_ec13_per_2011_population",
    "Total employment reported by the 2013 Economic Census in the 2011 Census district divided by the corresponding 2011 district population",
    "Economic Census employment intensity",
    "Preferred expanded-control measure",

    "employment_per_total_population",
    "Previously constructed district employment count allocated to AC and divided by AC proxy population",
    "Legacy employment measure",
    "Retired from expanded-control models"
  )

write_csv(
  definitions,
  file.path(
    output_dir,
    "09_variable_definitions.csv"
  )
)

notes <-
  c(
    "R31a8 CORRECTED EMPLOYMENT CONTROL",
    "",
    "No canonical final analysis dataset or R25/R26 source file is overwritten.",
    "",
    "The preferred employment control uses district-level 2013 Economic Census employment divided by the corresponding 2011 Census district population.",
    "",
    "This avoids allocating a district numerator to ACs and then dividing it by an incompatible AC-level population denominator.",
    "",
    "AC02 and V02 are first reproduced with the legacy employment measure on their exact saved samples.",
    "",
    "Corrected models then replace only the employment control while holding observations and all other model terms fixed.",
    "",
    "This correction applies only to Expanded-control robustness models. Primary-control specifications are unaffected."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "10_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "11_session_info.txt"
  )
)

cat(
  "\n===== EXACT SAMPLE AVAILABILITY =====\n"
)

print(
  sample_audit,
  n = Inf,
  width = Inf
)

cat(
  "\n===== OLD MODEL REPRODUCTION =====\n"
)

print(
  reproduction_audit,
  n = Inf,
  width = Inf
)

cat(
  "\n===== OLD VS CORRECTED EMPLOYMENT =====\n"
)

print(
  exact_sample_employment_comparison,
  n = Inf,
  width = Inf
)

cat(
  "\n===== FOCAL INTERACTION IMPACT =====\n"
)

print(
  focal_impact,
  n = Inf,
  width = Inf
)

cat(
  "\nR31A8_CORRECTED_EMPLOYMENT_CONTROL_COMPLETE\n"
)

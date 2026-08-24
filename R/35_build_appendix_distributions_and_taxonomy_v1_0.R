suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
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
    "outputs",
    "paper_appendix_descriptives_v1_0"
  )

figure_dir <-
  file.path(
    output_dir,
    "figures"
  )

table_dir <-
  file.path(
    output_dir,
    "tables"
  )

data_dir <-
  file.path(
    output_dir,
    "figure_data"
  )

review_dir <-
  file.path(
    output_dir,
    "review_ac01"
  )

for (
  d in
    c(
      figure_dir,
      table_dir,
      data_dir,
      review_dir
    )
) {
  dir.create(
    d,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

ac_change <-
  readRDS(
    "data/derived/switchers_rewrite/final/ac_change.rds"
  )

canonical_samples <-
  readRDS(
    "outputs/ac_canonical_v1_0/model_samples.rds"
  )

taxonomy <-
  read_csv(
    "config/fdi_sector_taxonomy.csv",
    show_col_types = FALSE
  )

if (
  !"AC01" %in%
    names(
      canonical_samples
    )
) {
  stop(
    "Canonical AC01 sample not found."
  )
}

if (
  anyDuplicated(
    ac_change$ac_uid
  ) >
    0L
) {
  stop(
    "ac_change is not unique by ac_uid."
  )
}

ac01_ids <-
  unique(
    as.character(
      canonical_samples[[
        "AC01"
      ]]$ac_uid
    )
  )

required_variables <-
  c(
    "ac_uid",
    "muslim_share_2001_dist_proxy",
    "fdi_total_local_all_pc100k_2014",
    "fdi_mfg_local_all_pc100k_2014",
    "fdi_services_local_all_pc100k_2014",
    "d_fdi_total_local_21m_pc100k",
    "d_fdi_mfg_local_21m_pc100k",
    "d_fdi_services_local_21m_pc100k"
  )

missing_variables <-
  setdiff(
    required_variables,
    names(
      ac_change
    )
  )

if (
  length(
    missing_variables
  ) >
    0L
) {
  stop(
    "ac_change missing required distribution variables: ",
    paste(
      missing_variables,
      collapse = ", "
    )
  )
}

distribution_base <-
  ac_change |>
  transmute(
    ac_uid =
      as.character(
        ac_uid
      ),

    in_ac01 =
      ac_uid %in%
        ac01_ids,

    muslim_share_2001 =
      as.numeric(
        muslim_share_2001_dist_proxy
      ),

    current_total =
      as.numeric(
        fdi_total_local_all_pc100k_2014
      ),

    current_manufacturing =
      as.numeric(
        fdi_mfg_local_all_pc100k_2014
      ),

    current_services =
      as.numeric(
        fdi_services_local_all_pc100k_2014
      ),

    change_total =
      as.numeric(
        d_fdi_total_local_21m_pc100k
      ),

    change_manufacturing =
      as.numeric(
        d_fdi_mfg_local_21m_pc100k
      ),

    change_services =
      as.numeric(
        d_fdi_services_local_21m_pc100k
      )
  )

fdi_variables <-
  c(
    "current_total",
    "current_manufacturing",
    "current_services",
    "change_total",
    "change_manufacturing",
    "change_services"
  )

fdi_complete <-
  distribution_base |>
  filter(
    if_all(
      all_of(
        fdi_variables
      ),
      is.finite
    )
  )

if (
  nrow(
    fdi_complete
  ) ==
    0L
) {
  stop(
    "No constituency has complete FDI distribution measures."
  )
}

if (
  any(
    fdi_complete$current_total <
      0
  ) ||
    any(
      fdi_complete$current_manufacturing <
        0
    ) ||
    any(
      fdi_complete$current_services <
        0
    )
) {
  stop(
    "Current-period FDI exposure contains negative values."
  )
}

identity_audit <-
  tibble(
    check =
      c(
        "current total = manufacturing + services",
        "change total = manufacturing + services"
      ),

    max_absolute_difference =
      c(
        max(
          abs(
            fdi_complete$current_total -
              fdi_complete$current_manufacturing -
              fdi_complete$current_services
          ),
          na.rm = TRUE
        ),

        max(
          abs(
            fdi_complete$change_total -
              fdi_complete$change_manufacturing -
              fdi_complete$change_services
          ),
          na.rm = TRUE
        )
      )
  )

if (
  any(
    identity_audit$max_absolute_difference >
      1e-8
  )
) {
  print(
    identity_audit,
    width = Inf
  )

  stop(
    "FDI sector identity failed."
  )
}

panel_registry <-
  tribble(
    ~variable, ~period, ~sector, ~panel_label,

    "current_total",
    "Current exposure",
    "Total",
    "Total FDI, 2009-2014",

    "current_manufacturing",
    "Current exposure",
    "Manufacturing",
    "Manufacturing FDI, 2009-2014",

    "current_services",
    "Current exposure",
    "Services",
    "Services FDI, 2009-2014",

    "change_total",
    "Temporal change",
    "Total",
    "Change in Total FDI, early-to-late 21-month windows",

    "change_manufacturing",
    "Temporal change",
    "Manufacturing",
    "Change in Manufacturing FDI, early-to-late 21-month windows",

    "change_services",
    "Temporal change",
    "Services",
    "Change in Services FDI, early-to-late 21-month windows"
  )

fdi_long <-
  fdi_complete |>
  pivot_longer(
    cols =
      all_of(
        fdi_variables
      ),
    names_to =
      "variable",
    values_to =
      "value"
  ) |>
  left_join(
    panel_registry,
    by =
      "variable",
    relationship =
      "many-to-one"
  ) |>
  mutate(
    panel_label =
      factor(
        panel_label,
        levels =
          panel_registry$panel_label
      )
  )

if (
  any(
    is.na(
      fdi_long$panel_label
    )
  )
) {
  stop(
    "At least one FDI variable did not map to a panel."
  )
}

summary_one <- function(
  x
) {
  tibble(
    n =
      sum(
        is.finite(
          x
        )
      ),

    mean =
      mean(
        x,
        na.rm = TRUE
      ),

    sd =
      sd(
        x,
        na.rm = TRUE
      ),

    min =
      min(
        x,
        na.rm = TRUE
      ),

    p25 =
      quantile(
        x,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),

    median =
      median(
        x,
        na.rm = TRUE
      ),

    p75 =
      quantile(
        x,
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),

    p90 =
      quantile(
        x,
        0.90,
        na.rm = TRUE,
        names = FALSE
      ),

    p95 =
      quantile(
        x,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    max =
      max(
        x,
        na.rm = TRUE
      ),

    share_zero =
      mean(
        x ==
          0,
        na.rm = TRUE
      ),

    share_positive =
      mean(
        x >
          0,
        na.rm = TRUE
      ),

    share_negative =
      mean(
        x <
          0,
        na.rm = TRUE
      )
  )
}

fdi_summary_full <-
  bind_rows(
    lapply(
      seq_len(
        nrow(
          panel_registry
        )
      ),
      function(i) {
        row <-
          panel_registry[
            i,
            ,
            drop = FALSE
          ]

        bind_cols(
          row,
          summary_one(
            fdi_complete[[
              row$variable[[1]]
            ]]
          )
        )
      }
    )
  ) |>
  mutate(
    universe =
      "Full harmonized constituency exposure universe"
  )

fdi_summary_ac01 <-
  bind_rows(
    lapply(
      seq_len(
        nrow(
          panel_registry
        )
      ),
      function(i) {
        row <-
          panel_registry[
            i,
            ,
            drop = FALSE
          ]

        bind_cols(
          row,
          summary_one(
            fdi_complete |>
              filter(
                in_ac01
              ) |>
              pull(
                all_of(
                  row$variable[[1]]
                )
              )
          )
        )
      }
    )
  ) |>
  mutate(
    universe =
      "Canonical AC01 estimation sample"
  )

fdi_summary <-
  bind_rows(
    fdi_summary_full,
    fdi_summary_ac01
  )

muslim_full <-
  distribution_base |>
  filter(
    is.finite(
      muslim_share_2001
    )
  )

if (
  any(
    muslim_full$muslim_share_2001 <
      0 |
      muslim_full$muslim_share_2001 >
        1
  )
) {
  stop(
    "Muslim population share is not bounded between zero and one."
  )
}

muslim_summary <-
  bind_rows(
    summary_one(
      muslim_full$muslim_share_2001
    ) |>
      mutate(
        universe =
          "Full harmonized constituency universe"
      ),

    summary_one(
      muslim_full |>
        filter(
          in_ac01
        ) |>
        pull(
          muslim_share_2001
        )
    ) |>
      mutate(
        universe =
          "Canonical AC01 estimation sample"
      )
  )

paper_theme <-
  theme_minimal(
    base_size = 10.5
  ) +
  theme(
    panel.grid.minor =
      element_blank(),
    strip.text =
      element_text(
        face =
          "bold",
        size =
          9.5
      ),
    plot.title =
      element_text(
        face =
          "bold"
      ),
    plot.caption =
      element_text(
        hjust =
          0,
        size =
          8
      )
  )

fdi_plot <-
  ggplot(
    fdi_long,
    aes(
      x =
        value
    )
  ) +
  geom_histogram(
    bins =
      35,
    boundary =
      0
  ) +
  facet_wrap(
    vars(
      panel_label
    ),
    ncol =
      3,
    scales =
      "free_x"
  ) +
  labs(
    title =
      "Distribution of constituency FDI exposure",
    subtitle =
      paste0(
        "Current exposure and early-to-late temporal change; ",
        "own plus neighboring constituencies"
      ),
    x =
      "FDI projects per 100,000 population",
    y =
      "Number of assembly constituencies",
    caption =
      paste0(
        "Current exposure covers April 2009-March 2014. ",
        "The temporal-change measures equal the late July 2012-March 2014 ",
        "21-month window minus the early April 2004-December 2005 21-month window. ",
        "Histograms use constituencies with complete Total, Manufacturing, and Services ",
        "FDI exposure measures in the harmonized national constituency universe."
      )
  ) +
  paper_theme

muslim_plot <-
  ggplot(
    muslim_full,
    aes(
      x =
        100 *
        muslim_share_2001
    )
  ) +
  geom_histogram(
    bins =
      35,
    boundary =
      0
  ) +
  labs(
    title =
      "Distribution of Muslim population share, 2001",
    x =
      "Muslim population share (%)",
    y =
      "Number of assembly constituencies",
    caption =
      paste0(
        "The histogram uses all harmonized assembly constituencies with a finite ",
        "2001 Muslim population-share measure. The measure is constructed from ",
        "2001 Census Muslim population divided by 2001 Census total population ",
        "within the harmonized district lineage and mapped to constituencies."
      )
  ) +
  paper_theme

ac01_fdi_plot <-
  fdi_long |>
  filter(
    in_ac01
  ) |>
  ggplot(
    aes(
      x =
        value
    )
  ) +
  geom_histogram(
    bins =
      25,
    boundary =
      0
  ) +
  facet_wrap(
    vars(
      panel_label
    ),
    ncol =
      3,
    scales =
      "free_x"
  ) +
  labs(
    title =
      "FDI exposure distributions in the canonical AC01 sample",
    x =
      "FDI projects per 100,000 population",
    y =
      "Number of assembly constituencies",
    caption =
      "Internal review version restricted to the canonical 224-AC primary estimation sample."
  ) +
  paper_theme

ac01_muslim_plot <-
  muslim_full |>
  filter(
    in_ac01
  ) |>
  ggplot(
    aes(
      x =
        100 *
        muslim_share_2001
    )
  ) +
  geom_histogram(
    bins =
      25,
    boundary =
      0
  ) +
  labs(
    title =
      "Muslim population share in the canonical AC01 sample",
    x =
      "Muslim population share (%)",
    y =
      "Number of assembly constituencies",
    caption =
      "Internal review version restricted to the canonical 224-AC primary estimation sample."
  ) +
  paper_theme

save_pdf_png <- function(
  plot,
  stem,
  width,
  height
) {
  ggsave(
    paste0(
      stem,
      ".png"
    ),
    plot =
      plot,
    width =
      width,
    height =
      height,
    units =
      "in",
    dpi =
      300,
    bg =
      "white"
  )

  ggsave(
    paste0(
      stem,
      ".pdf"
    ),
    plot =
      plot,
    width =
      width,
    height =
      height,
    units =
      "in",
    device =
      grDevices::pdf,
    useDingbats =
      FALSE
  )
}

save_pdf_png(
  fdi_plot,
  file.path(
    figure_dir,
    "Appendix_Figure_A8_fdi_exposure_distributions"
  ),
  width =
    11,
  height =
    7.2
)

save_pdf_png(
  muslim_plot,
  file.path(
    figure_dir,
    "Appendix_Figure_A9_muslim_share_2001_distribution"
  ),
  width =
    7.4,
  height =
    5.4
)

save_pdf_png(
  ac01_fdi_plot,
  file.path(
    review_dir,
    "review_ac01_fdi_exposure_distributions"
  ),
  width =
    11,
  height =
    7.2
)

save_pdf_png(
  ac01_muslim_plot,
  file.path(
    review_dir,
    "review_ac01_muslim_share_2001_distribution"
  ),
  width =
    7.4,
  height =
    5.4
)

write_csv(
  fdi_long,
  file.path(
    data_dir,
    "Appendix_Figure_A8_source_data.csv"
  )
)

write_csv(
  muslim_full,
  file.path(
    data_dir,
    "Appendix_Figure_A9_source_data.csv"
  )
)

write_csv(
  fdi_summary,
  file.path(
    data_dir,
    "01_fdi_distribution_summary.csv"
  )
)

write_csv(
  muslim_summary,
  file.path(
    data_dir,
    "02_muslim_share_distribution_summary.csv"
  )
)

write_csv(
  identity_audit,
  file.path(
    data_dir,
    "03_fdi_sector_identity_audit.csv"
  )
)

write_csv(
  taxonomy,
  file.path(
    table_dir,
    "Appendix_Table_A10_fdi_sector_taxonomy.csv"
  )
)

html_escape <- function(x) {
  x <-
    gsub(
      "&",
      "&amp;",
      as.character(
        x
      ),
      fixed =
        TRUE
    )

  x <-
    gsub(
      "<",
      "&lt;",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      ">",
      "&gt;",
      x,
      fixed =
        TRUE
    )

  x
}

taxonomy_header <-
  paste0(
    "<th>",
    html_escape(
      names(
        taxonomy
      )
    ),
    "</th>",
    collapse =
      ""
  )

taxonomy_rows <-
  apply(
    taxonomy,
    1,
    function(row) {
      paste0(
        "<tr>",
        paste0(
          "<td>",
          html_escape(
            row
          ),
          "</td>",
          collapse =
            ""
        ),
        "</tr>"
      )
    }
  )

taxonomy_html <-
  c(
    "<!doctype html>",
    "<html><head><meta charset=\"utf-8\">",
    "<style>",
    "body{font-family:Arial,Helvetica,sans-serif;margin:32px;color:#111;}",
    "table{border-collapse:collapse;width:100%;font-size:12px;}",
    "th{border-bottom:2px solid #222;padding:6px;text-align:left;}",
    "td{border-bottom:1px solid #ddd;padding:6px;text-align:left;}",
    ".note{font-size:11px;line-height:1.4;margin-top:12px;}",
    "</style></head><body>",
    "<h2>FDI sector classification taxonomy</h2>",
    "<table>",
    paste0(
      "<thead><tr>",
      taxonomy_header,
      "</tr></thead>"
    ),
    "<tbody>",
    taxonomy_rows,
    "</tbody></table>",
    paste0(
      "<div class=\"note\"><strong>Notes:</strong> ",
      "This is the authoritative project taxonomy used to classify FDI projects. ",
      "Manufacturing comprises Manufacturing, Extraction, Electricity, and Recycling; ",
      "Services comprises the remaining registered non-manufacturing sectors. ",
      "Total FDI is the union of Manufacturing and Services.",
      "</div>"
    ),
    "</body></html>"
  )

writeLines(
  taxonomy_html,
  file.path(
    table_dir,
    "Appendix_Table_A10_fdi_sector_taxonomy.html"
  )
)

latex_escape <- function(x) {
  x <-
    gsub(
      "&",
      "\\\\&",
      as.character(
        x
      ),
      fixed =
        TRUE
    )

  x <-
    gsub(
      "%",
      "\\\\%",
      x,
      fixed =
        TRUE
    )

  x <-
    gsub(
      "_",
      "\\\\_",
      x,
      fixed =
        TRUE
    )

  x
}

taxonomy_tex_rows <-
  apply(
    taxonomy,
    1,
    function(row) {
      paste0(
        paste(
          latex_escape(
            row
          ),
          collapse =
            " & "
        ),
        " \\\\"
      )
    }
  )

taxonomy_alignment <-
  paste(
    rep(
      "l",
      ncol(
        taxonomy
      )
    ),
    collapse =
      ""
  )

taxonomy_tex <-
  c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{FDI sector classification taxonomy}",
    "\\scriptsize",
    paste0(
      "\\begin{tabular}{",
      taxonomy_alignment,
      "}"
    ),
    "\\toprule",
    paste0(
      paste(
        latex_escape(
          names(
            taxonomy
          )
        ),
        collapse =
          " & "
      ),
      " \\\\"
    ),
    "\\midrule",
    taxonomy_tex_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.96\\textwidth}",
    "\\footnotesize",
    paste0(
      "\\textit{Notes:} This is the authoritative project taxonomy used to classify FDI projects. ",
      "Manufacturing comprises Manufacturing, Extraction, Electricity, and Recycling; ",
      "Services comprises the remaining registered non-manufacturing sectors. ",
      "Total FDI is the union of Manufacturing and Services."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )

writeLines(
  taxonomy_tex,
  file.path(
    table_dir,
    "Appendix_Table_A10_fdi_sector_taxonomy.tex"
  )
)

registry <-
  read_csv(
    "config/paper_artifacts_v1_1.csv",
    show_col_types =
      FALSE
  ) |>
  select(
    -any_of(
      c(
        "architecture_version",
        "frozen_date"
      )
    )
  )

updates <-
  tribble(
    ~paper_id, ~publication_artifact, ~source_artifact,

    "Appendix Figure A8",
    "outputs/paper_appendix_descriptives_v1_0/figures/Appendix_Figure_A8_fdi_exposure_distributions.pdf",
    "outputs/paper_appendix_descriptives_v1_0/figure_data/Appendix_Figure_A8_source_data.csv",

    "Appendix Figure A9",
    "outputs/paper_appendix_descriptives_v1_0/figures/Appendix_Figure_A9_muslim_share_2001_distribution.pdf",
    "outputs/paper_appendix_descriptives_v1_0/figure_data/Appendix_Figure_A9_source_data.csv",

    "Appendix Table A10",
    "outputs/paper_appendix_descriptives_v1_0/tables/Appendix_Table_A10_fdi_sector_taxonomy.tex",
    "config/fdi_sector_taxonomy.csv"
  )

registry_v1_2 <-
  registry |>
  left_join(
    updates |>
      rename(
        new_publication_artifact =
          publication_artifact,
        new_source_artifact =
          source_artifact
      ),
    by =
      "paper_id",
    relationship =
      "one-to-one"
  ) |>
  mutate(
    publication_artifact =
      coalesce(
        new_publication_artifact,
        publication_artifact
      ),

    source_artifact =
      coalesce(
        new_source_artifact,
        source_artifact
      ),

    publication_status =
      if_else(
        paper_id %in%
          updates$paper_id,
        "Built",
        publication_status
      ),

    generating_script =
      if_else(
        paper_id %in%
          updates$paper_id,
        "R/35_build_appendix_distributions_and_taxonomy_v1_0.R",
        generating_script
      ),

    architecture_version =
      "v1.2",

    frozen_date =
      as.character(
        Sys.Date()
      )
  ) |>
  select(
    -new_publication_artifact,
    -new_source_artifact
  )

missing_built <-
  registry_v1_2 |>
  filter(
    publication_status ==
      "Built",
    (
      is.na(
        publication_artifact
      ) |
        !file.exists(
          publication_artifact
        )
    )
  )

if (
  nrow(
    missing_built
  ) >
    0L
) {
  print(
    missing_built,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one Built artifact is missing after R35."
  )
}

write_csv(
  registry_v1_2,
  "config/paper_artifacts_v1_2.csv"
)

status_summary <-
  registry_v1_2 |>
  count(
    placement,
    artifact_type,
    publication_status,
    name =
      "n"
  ) |>
  arrange(
    placement,
    artifact_type,
    publication_status
  )

write_csv(
  status_summary,
  file.path(
    output_dir,
    "01_registry_status_after_r35.csv"
  )
)

manifest_files <-
  list.files(
    output_dir,
    recursive =
      TRUE,
    full.names =
      TRUE
  )

manifest <-
  tibble(
    path =
      manifest_files,
    bytes =
      file.info(
        manifest_files
      )$size,
    md5 =
      unname(
        tools::md5sum(
          manifest_files
        )
      )
  )

write_csv(
  manifest,
  file.path(
    output_dir,
    "02_generated_artifact_manifest.csv"
  )
)

notes <-
  c(
    "R35 APPENDIX DISTRIBUTIONS AND FDI TAXONOMY",
    "",
    "Appendix Figure A8 contains six constituency-level FDI histograms.",
    "",
    "The first row reports raw Total, Manufacturing, and Services local FDI per 100,000 population from April 2009 through March 2014.",
    "",
    "The second row reports the corresponding late-minus-early 21-month change measures.",
    "",
    "Early 21-month window: April 2004 through December 2005.",
    "Late 21-month window: July 2012 through March 2014.",
    "",
    "Appendix Figure A9 reports the constituency distribution of Muslim population share in 2001.",
    "",
    "Publication distribution figures use the full harmonized constituency universe with finite measures.",
    "",
    "AC01-restricted versions are saved only for internal review and are not registered as paper figures.",
    "",
    "Appendix Table A10 reproduces the authoritative config/fdi_sector_taxonomy.csv taxonomy.",
    "",
    "No statistical model is estimated or altered by this script."
  )

writeLines(
  notes,
  file.path(
    output_dir,
    "03_readme.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "04_session_info.txt"
  )
)

cat(
  "\n===== FDI SECTOR IDENTITY AUDIT =====\n"
)

print(
  identity_audit,
  width = Inf
)

cat(
  "\n===== FDI DISTRIBUTION SUMMARY =====\n"
)

print(
  fdi_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== MUSLIM-SHARE DISTRIBUTION SUMMARY =====\n"
)

print(
  muslim_summary,
  n = Inf,
  width = Inf
)

cat(
  "\n===== TAXONOMY =====\n"
)

print(
  taxonomy,
  n = Inf,
  width = Inf
)

cat(
  "\n===== REGISTRY STATUS AFTER R35 =====\n"
)

print(
  status_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nR35_APPENDIX_DISTRIBUTIONS_AND_TAXONOMY_COMPLETE\n"
)

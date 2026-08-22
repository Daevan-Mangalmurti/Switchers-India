# ============================================================
# 17_generate_paper_tables.R
# Hotfix revision: 2026-08-13-v1.0.5
# Generates numbered LaTeX paper tables 18-35.
# Regression tables 24-31 are written twice:
#   a = informative units, b = native units.
# ============================================================

if (!exists("paper_dirs")) {
  .root_for_source <- if (exists("project_root")) project_root else Sys.getenv("SWITCHERS_ROOT", unset = "/Users/Daevan/Downloads/Switchers-India")
  source(file.path(.root_for_source, "R", "15_prepare_paper_outputs.R"))
}

# ------------------------------------------------------------
# LaTeX table helpers
# ------------------------------------------------------------
latex_align_for_df <- function(df) paste0("l", paste(rep("r", max(0, ncol(df)-1)), collapse = ""))

write_df_tex <- function(df, path, caption, label, notes = NULL, longtable = FALSE, escape = TRUE, font_size = "scriptsize") {
  dd <- df
  if (escape) {
    dd[] <- lapply(dd, function(x) if (is.character(x) || is.factor(x)) latex_escape(as.character(x)) else x)
    names(dd) <- latex_escape(names(dd))
  }
  cols <- names(dd)
  align <- paste0("l", paste(rep("r", max(0, ncol(dd)-1)), collapse = ""))
  env <- if (longtable) "longtable" else "tabular"
  lines <- c()
  if (!longtable) {
    lines <- c(lines,
      "\\begin{table}[!htbp]",
      "\\centering",
      paste0("\\", font_size),
      "\\begin{threeparttable}",
      paste0("\\caption{", caption, "}"),
      paste0("\\label{", label, "}"),
      paste0("\\begin{tabular}{", align, "}"))
  } else {
    lines <- c(lines,
      paste0("\\begin{", env, "}{", align, "}"),
      paste0("\\caption{", caption, "}\\label{", label, "}\\\\"))
  }
  lines <- c(lines, "\\toprule", paste(cols, collapse = " & "), "\\\\", "\\midrule")
  for (i in seq_len(nrow(dd))) {
    vals <- vapply(dd[i, , drop = FALSE], function(x) {
      if (is.numeric(x)) {
        ifelse(is.na(x), "--", formatC(x, digits = 3, format = "fg", flag = "#"))
      } else {
        ifelse(is.na(x), "--", as.character(x))
      }
    }, character(1))
    lines <- c(lines, paste(vals, collapse = " & "), "\\\\")
  }
  lines <- c(lines, "\\bottomrule")
  if (!longtable) {
    lines <- c(lines, "\\end{tabular}")
    if (!is.null(notes) && length(notes)) {
      lines <- c(lines, "\\begin{tablenotes}[flushleft]", "\\footnotesize")
      for (n in notes) lines <- c(lines, paste0("\\item ", n))
      lines <- c(lines, "\\end{tablenotes}")
    }
    lines <- c(lines, "\\end{threeparttable}", "\\end{table}")
  } else {
    lines <- c(lines, paste0("\\end{", env, "}"))
    if (!is.null(notes) && length(notes)) {
      lines <- c(lines, "", "\\begin{minipage}{0.97\\linewidth}\\footnotesize", paste(notes, collapse = " "), "\\end{minipage}")
    }
  }
  writeLines(lines, path)
  path
}

star_for_p <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < .01, "$^{***}$",
      ifelse(p < .05, "$^{**}$",
        ifelse(p < .10, "$^{*}$", ""))))
}

format_reg_number <- function(x) {
  if (is.na(x)) return("--")
  ax <- abs(x)
  if (ax > 0 && ax < 1e-4) return(sprintf("%.2e", x))
  if (ax < .01) return(sprintf("%.5f", x))
  if (ax < 1) return(sprintf("%.3f", x))
  if (ax < 100) return(sprintf("%.2f", x))
  sprintf("%.1f", x)
}

format_p <- function(p) {
  if (is.na(p)) return("--")
  if (p < .0001) return("p<0.0001")
  sprintf("p=%.4f", p)
}

# Robust coefficient-table extractor.
#
# On some installed fixest versions, calling fixest::coeftable() explicitly
# from a sourced script can fall through to coeftable.default() even when the
# fitted object is a valid "fixest" model.  The feols object itself stores the
# coefficient table produced with the requested VCOV, so use that first.
# Fall back to summary(fit)$coeftable and only then to the public extractor.
extract_fixest_coeftable <- function(fit) {
  if (is.null(fit)) return(NULL)

  ct <- NULL

  if (inherits(fit, "fixest") && !is.null(fit$coeftable)) {
    ct <- fit$coeftable
  }

  if (is.null(ct) || !is.matrix(ct) || ncol(ct) < 4) {
    sm <- tryCatch(
      summary(fit),
      error = function(e) NULL
    )
    if (is.list(sm) && !is.null(sm$coeftable)) {
      ct <- sm$coeftable
    }
  }

  if (is.null(ct) || !is.matrix(ct) || ncol(ct) < 4) {
    ct <- tryCatch(
      fixest::coeftable(fit),
      error = function(e) NULL
    )
  }

  if (is.null(ct) || !is.matrix(ct) || ncol(ct) < 4) {
    return(NULL)
  }

  ct
}

fit_tidy <- function(fit) {
  if (is.null(fit)) return(tibble())

  ct <- extract_fixest_coeftable(fit)

  if (is.null(ct) || !nrow(ct)) {
    return(tibble())
  }

  tibble(
    term = rownames(ct),
    estimate = as.numeric(ct[, 1]),
    se = as.numeric(ct[, 2]),
    statistic = as.numeric(ct[, 3]),
    p = as.numeric(ct[, 4])
  )
}

safe_nobs <- function(fit) {
  if (is.null(fit)) return(NA_integer_)
  tryCatch(as.integer(stats::nobs(fit)), error = function(e) NA_integer_)
}

safe_r2 <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  tryCatch(
    as.numeric(fixest::r2(fit, "r2")),
    error = function(e) NA_real_
  )
}

audit_fit_object <- function(fit, table_item, units, panel, model) {
  ct <- extract_fixest_coeftable(fit)
  tibble(
    table_item = table_item,
    units = units,
    panel = panel,
    model = model,
    object_class = paste(class(fit), collapse = "|"),
    nobs = safe_nobs(fit),
    n_coefficients = if (is.null(ct)) NA_integer_ else nrow(ct),
    has_fdi_term = if (is.null(ct)) FALSE else any(grepl("fdi_x", rownames(ct), fixed = TRUE)),
    collinearity_detected = if (inherits(fit, "fixest") && !is.null(fit$multicol)) isTRUE(fit$multicol) else NA,
    collinear_terms = if (inherits(fit, "fixest") && length(fit$collin.var)) paste(fit$collin.var, collapse = " | ") else ""
  )
}

pretty_term <- function(term, demo_label = "Demographic context", triple_type = c("none", "ac_center", "ideology"), units = "native") {
  triple_type <- match.arg(triple_type)
  out <- term
  replacements <- c(
    "fdi_x" = "Manufacturing FDI",
    "demo_x" = demo_label,
    "center_x" = "Weighted AC Center share (2009)",
    "baseline_fdi_x" = "Baseline manufacturing FDI (2009)",
    "baseline_bjp_x" = "Baseline BJP vote share (2009)",
    "ac_pop_x" = ifelse(units == "informative", "AC population (100,000s)", "AC population"),
    "land_area_x" = "Land area",
    "sc_share_x" = ifelse(units == "informative", "SC share (percentage points)", "SC share"),
    "st_share_x" = ifelse(units == "informative", "ST share (percentage points)", "ST share"),
    "employment_x" = "Log(1 + employment / population)",
    "education_x" = ifelse(units == "informative", "Secondary education share (percentage points)", "Secondary education share")
  )
  for (nm in names(replacements)) out <- str_replace_all(out, fixed(nm), replacements[[nm]])
  out <- str_replace_all(out, ":", " $\\times$ ")
  out <- str_replace_all(out, "ideology_bucketLeft", "Ideology: Left (vs. Center)")
  out <- str_replace_all(out, "ideology_bucketRight", "Ideology: Right (vs. Center)")
  out <- str_replace_all(out, "ideology_bucketMixed", "Ideology: Mixed (vs. Center)")
  out <- str_replace_all(out, "religion_group", "Religion: ")
  out <- str_replace_all(out, "caste_group", "Caste: ")
  out <- str_replace_all(out, "education_harmonized", "Education: ")
  out
}

rank_term <- function(term) {
  case_when(
    str_detect(term, "fdi_x.*demo_x.*center_x|fdi_x.*demo_x.*ideology_bucket|demo_x.*fdi_x.*ideology_bucket") ~ 1,
    str_detect(term, "fdi_x.*demo_x|demo_x.*fdi_x") ~ 2,
    str_detect(term, "fdi_x.*center_x|center_x.*fdi_x") ~ 3,
    str_detect(term, "demo_x.*center_x|center_x.*demo_x") ~ 4,
    term == "fdi_x" ~ 5,
    term == "demo_x" ~ 6,
    term == "center_x" ~ 7,
    str_detect(term, "^ideology_bucket") ~ 8,
    str_detect(term, "baseline_fdi_x") ~ 9,
    str_detect(term, "baseline_bjp_x") ~ 10,
    str_detect(term, "ac_pop_x|land_area_x|sc_share_x|st_share_x|employment_x|education_x") ~ 20,
    str_detect(term, "religion_group|caste_group|education_harmonized") ~ 30,
    TRUE ~ 40
  )
}

make_reg_panel <- function(fits, model_labels, demo_label, triple_type, units, extra_rows = NULL) {
  tids <- imap_dfr(fits, function(fit, nm) fit_tidy(fit) |> mutate(model = nm))
  terms <- tids |>
    distinct(term) |>
    mutate(rank = rank_term(term), label = map_chr(term, ~pretty_term(.x, demo_label, triple_type, units))) |>
    arrange(rank, term)

  out <- tibble(term = terms$term, label = terms$label)
  for (nm in names(fits)) {
    one <- tids |> filter(model == nm) |> select(term, estimate, se, p)
    out <- out |> left_join(one, by = "term")
    names(out)[(ncol(out)-2):ncol(out)] <- paste0(c("estimate_", "se_", "p_"), nm)
  }
  if (!is.null(extra_rows) && nrow(extra_rows)) {
    out <- bind_rows(extra_rows, out)
  }
  out
}

reg_cell <- function(est, se, p) {
  if (is.na(est)) return("--")
  paste0(
    "\\shortstack{", format_reg_number(est), star_for_p(p),
    "\\\\(", format_reg_number(se), ")",
    "\\\\{\\scriptsize ", format_p(p), "}}"
  )
}

write_two_panel_regression_tex <- function(path, caption, label, fits_a, fits_b,
                                           model_labels_a, model_labels_b, demo_label, triple_type,
                                           units, notes, extra_a = NULL, extra_b = NULL) {
  pa <- make_reg_panel(fits_a, model_labels_a, demo_label, triple_type, units, extra_a)
  pb <- make_reg_panel(fits_b, model_labels_b, demo_label, triple_type, units, extra_b)

  render_panel <- function(tab, fits, labels, panel_title) {
    lines <- c(
      paste0("\\multicolumn{5}{l}{\\textit{", panel_title, "}} \\\\"),
      "\\addlinespace",
      paste0(" & ", paste(paste0("(", seq_along(labels), ")"), collapse = " & "), " \\\\"),
      paste0(" & ", paste(latex_escape(labels), collapse = " & "), " \\\\"),
      "\\midrule"
    )
    for (i in seq_len(nrow(tab))) {
      if (is.na(tab$term[i]) || tab$term[i] == "") next
      cells <- c()
      for (nm in names(fits)) {
        e <- tab[[paste0("estimate_", nm)]][i]
        s <- tab[[paste0("se_", nm)]][i]
        p <- tab[[paste0("p_", nm)]][i]
        cells <- c(cells, reg_cell(e, s, p))
      }
      lines <- c(lines, paste0(latex_escape(tab$label[i]), " & ", paste(cells, collapse = " & "), " \\\\"))
    }
    lines <- c(lines, "\\midrule")
    stat_rows <- list(
      c("Observations", map_chr(fits, ~ifelse(is.na(safe_nobs(.x)), "--", formatC(safe_nobs(.x), format = "d")))),
      c("$R^2$", map_chr(fits, ~ifelse(is.na(safe_r2(.x)), "--", sprintf("%.3f", safe_r2(.x)))))
    )
    for (sr in stat_rows) lines <- c(lines, paste0(sr[1], " & ", paste(sr[-1], collapse = " & "), " \\\\"))
    lines
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\scriptsize",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    render_panel(pa, fits_a, model_labels_a, "Panel A: AC panel progression"),
    "\\addlinespace[0.8em]",
    render_panel(pb, fits_b, model_labels_b, "Panel B: 2014 baseline-adjusted / lagged-outcome specification"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize"
  )
  for (n in notes) lines <- c(lines, paste0("\\item ", n))
  lines <- c(lines, "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  writeLines(lines, path)
  path
}

# ------------------------------------------------------------
# Table 18: ideology questions and scoring scale
# ------------------------------------------------------------
find_first_column <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)][1]
  if (length(hit) == 0 || is.na(hit)) NA_character_ else hit
}
item_label_col <- find_first_column(ideology_items_long, c("question_text", "question", "item_label", "variable_label", "label", "question_label"))
response_label_col <- find_first_column(ideology_items_long, c("response_label", "response_text", "raw_response_label", "answer_label", "value_label", "response", "raw_response"))

qmap_file <- file.path(paper_dirs$data, "18_ideology_item_question_map.csv")
if (file.exists(qmap_file)) {
  qmap <- read_csv(qmap_file, show_col_types = FALSE)
} else {
  qmap <- tibble(item = classification_items, question = classification_items)
}

item_year_axis <- tibble(
  item = classification_items,
  year = c(2009,2009,2009,2009,2009,2014,2014,2014),
  axis = c("Recognition","Recognition","Statism","Statism","Statism","Recognition","Recognition","Statism")
)

if (!is.na(response_label_col)) {
  scale_map <- ideology_items_long |>
    filter(item %in% classification_items) |>
    mutate(
      response_label = if (inherits(.data[[response_label_col]], "haven_labelled")) {
        as.character(haven::as_factor(.data[[response_label_col]], levels = "labels"))
      } else as.character(.data[[response_label_col]]),
      bucket = as.character(.data[[bucket_col]])
    ) |>
    filter(!is.na(response_label), !is.na(bucket)) |>
    distinct(item, response_label, bucket) |>
    arrange(item, response_label) |>
    group_by(item) |>
    summarise(
      scoring_scale = paste0(latex_escape(response_label), " $\\rightarrow$ ", latex_escape(bucket), collapse = "; "),
      .groups = "drop"
    )
} else {
  scale_map <- ideology_items_long |>
    filter(item %in% classification_items) |>
    transmute(item, bucket = as.character(.data[[bucket_col]])) |>
    distinct() |>
    group_by(item) |>
    summarise(scoring_scale = paste(sort(unique(bucket)), collapse = "/"), .groups = "drop")
}

t18 <- item_year_axis |>
  left_join(qmap, by = "item") |>
  left_join(scale_map, by = "item") |>
  transmute(
    Year = year,
    Axis = axis,
    Item = item,
    `Question wording` = latex_escape(question),
    `Response scoring` = scoring_scale
  )

p18 <- file.path(paper_dirs$main_tables, "18_main_nes_ideology_questions_scoring.tex")
write_df_tex(
  t18, p18,
  "NES questions used to classify respondents into ideological buckets",
  "tab:ideology_questions",
  notes = c(
    "Main classification uses the harmonized four-bucket rule. In 2009, Left, Center, and Right require both recognition items to fall in the corresponding bucket and at least two of three statism items to do so; in 2014, the rule requires both recognition items and the single available statism item. Ideology-complete respondents who do not satisfy a pure bucket are classified Mixed.",
    "The 2009 relaxation is used because 2009 contains three statism items whereas 2014 contains only one; applying the strict all-items rule only in 2009 mechanically makes pure ideological categories harder to enter. The original strict four-bucket classification is retained as an appendix robustness definition."
  ),
  longtable = TRUE,
  escape = FALSE
)
register_output("18", "table_tex", p18)

# ------------------------------------------------------------
# Table 19: model universe
# ------------------------------------------------------------
main_two <- "Main: manufacturing FDI $\\times$ 2001 Muslim share"
app_two <- "Appendix: total/services FDI $\\times$ 2001 Muslim share; manufacturing/total/services FDI $\\times$ established migration/compositional measures"
main_three_ac <- "Main: manufacturing FDI $\\times$ 2001 Muslim share $\\times$ weighted 2009 AC Center share"
app_three_ac <- "Appendix: total/services FDI $\\times$ 2001 Muslim share $\\times$ Center share; manufacturing/total/services FDI $\\times$ migration/compositional measures $\\times$ Center share"
main_three_v <- "Main: manufacturing FDI $\\times$ 2001 Muslim share $\\times$ harmonized ideology bucket"
app_three_v <- "Appendix: total/services and migration/compositional triples; Center-vs.-others (center\\_harmonized) robustness"

t19 <- tribble(
  ~`Level of analysis`, ~`Model`, ~`Two-way interaction options`, ~`Three-way interaction options`,
  "Constituency", "Pooled AC-year OLS (appendix robustness)", paste(main_two, app_two, sep="; "), paste(main_three_ac, app_three_ac, sep="; "),
  "Constituency", "AC fixed-effects OLS (main within-AC model)", paste(main_two, app_two, sep="; "), paste(main_three_ac, app_three_ac, sep="; "),
  "Constituency", "First-difference OLS (appendix robustness)", paste(main_two, app_two, sep="; "), paste(main_three_ac, app_three_ac, sep="; "),
  "Constituency", "Lagged-outcome OLS (main complementary model)", paste(main_two, app_two, sep="; "), paste(main_three_ac, app_three_ac, sep="; "),
  "Voter", "Pooled repeated-cross-section LPM", paste(main_two, app_two, sep="; "), paste(main_three_v, app_three_v, sep="; "),
  "Voter", "2014 baseline-adjusted LPM", paste(main_two, app_two, sep="; "), paste(main_three_v, app_three_v, sep="; "),
  "Voter", "Pooled repeated-cross-section logit (appendix)", paste(main_two, app_two, sep="; "), paste(main_three_v, app_three_v, sep="; "),
  "Voter", "2014 baseline-adjusted logit (appendix)", paste(main_two, app_two, sep="; "), paste(main_three_v, app_three_v, sep="; ")
)
p19 <- file.path(paper_dirs$main_tables, "19_main_model_universe.tex")
write_df_tex(t19, p19, "Model universe", "tab:model_universe", notes = c(
  "The AC fixed-effects model identifies associations from within-AC changes between 2009 and 2014. With two elections it is closely related to a first-difference estimator; the first-difference family is therefore treated as a robustness formulation rather than independent identification.",
  "NES data are repeated cross-sections rather than a voter panel. 'Baseline-adjusted' voter models use 2014 vote choice with baseline AC political and FDI covariates."
), longtable = TRUE, escape = FALSE)
register_output("19", "table_tex", p19)

# ------------------------------------------------------------
# Tables 20-21: descriptive diagnostics, year-specific main + pooled appendix
# ------------------------------------------------------------
summary_unweighted <- function(x) {
  x <- x[is.finite(x)]
  tibble(
    N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Q1 = quantile(x,.25), Median = median(x), Q3 = quantile(x,.75), Max = max(x)
  )
}

weighted_quantile <- function(x, w, probs = c(.25,.5,.75)) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  x <- x[ok]; w <- w[ok]
  if (!length(x) || sum(w) <= 0) return(rep(NA_real_, length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}
weighted_stats <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  x <- x[ok]; w <- w[ok]
  if (!length(x) || sum(w)<=0) return(tibble(N=0,Mean=NA,SD=NA,Min=NA,Q1=NA,Median=NA,Q3=NA,Max=NA))
  mu <- weighted.mean(x,w)
  qs <- weighted_quantile(x,w)
  tibble(N=length(x),Mean=mu,SD=sqrt(weighted.mean((x-mu)^2,w)),Min=min(x),Q1=qs[1],Median=qs[2],Q3=qs[3],Max=max(x))
}

ac_diag_vars <- c(
  "log1p_fdi_mfg_local_all_pc100k",
  "log1p_fdi_total_local_all_pc100k",
  "log1p_fdi_services_local_all_pc100k",
  preferred_muslim,
  "proxy_ac_pop", "con08_land_area", "sc_pop_share", "st_pop_share",
  "log1p_employment_per_total_population", "ed_sec_share",
  "bjp_vote_share"
)
ac_diag_vars <- ac_diag_vars[ac_diag_vars %in% names(ac_year)]
ac_diag_labels <- c(
  log1p_fdi_mfg_local_all_pc100k = "Manufacturing FDI: log(1 + local projects per 100k)",
  log1p_fdi_total_local_all_pc100k = "Total FDI: log(1 + local projects per 100k)",
  log1p_fdi_services_local_all_pc100k = "Services FDI: log(1 + local projects per 100k)",
  muslim_share_2001_dist_proxy = "Muslim share, 2001",
  proxy_ac_pop = "AC population proxy",
  con08_land_area = "Land area",
  sc_pop_share = "SC population share",
  st_pop_share = "ST population share",
  log1p_employment_per_total_population = "Log(1 + employment / population)",
  ed_sec_share = "Secondary-education share",
  bjp_vote_share = "BJP vote share"
)

make_ac_diag <- function(data, by_year = TRUE) {
  if (by_year) {
    map_dfr(ac_diag_vars, function(v) {
      map_dfr(c(2009,2014), function(y) {
        s <- summary_unweighted(data[[v]][data$year==y])
        bind_cols(tibble(Variable=unname(ac_diag_labels[v]), Year=y), s)
      })
    })
  } else {
    map_dfr(ac_diag_vars, function(v) bind_cols(tibble(Variable=unname(ac_diag_labels[v])), summary_unweighted(data[[v]])))
  }
}

t20a <- make_ac_diag(ac_year, TRUE)
p20a <- file.path(paper_dirs$main_tables, "20a_main_ac_diagnostics_year_specific.tex")
write_df_tex(t20a, p20a, "Constituency-level descriptive statistics by election year", "tab:ac_diagnostics_year", notes = "Statistics are computed over AC-year observations with nonmissing values for each variable.")
register_output("20", "table_tex", p20a)

t20b <- make_ac_diag(ac_year, FALSE)
p20b <- file.path(paper_dirs$appendix_tables, "20b_appendix_ac_diagnostics_pooled.tex")
write_df_tex(t20b, p20b, "Pooled constituency-level descriptive statistics", "tab:ac_diagnostics_pooled")
register_output("20", "table_tex", p20b)

# Voter diagnostics: ideology indicators, AC ideology counts, voter controls, outcome.
make_voter_diag <- function(data, year_filter = NULL) {
  dd <- data
  if (!is.null(year_filter)) dd <- dd |> filter(year %in% year_filter)
  dd <- dd |> filter(!is.na(.data[[RESP_WEIGHT_VAR]]))
  rows <- list(); idx <- 0L
  for (ideo in ideology_levels) {
    idx <- idx+1L
    x <- as.numeric(as.character(dd$voter_ideology_harmonized)==ideo)
    s <- weighted_stats(x, dd[[RESP_WEIGHT_VAR]])
    rows[[idx]] <- bind_cols(tibble(Variable=paste0("Respondent ideology: ",ideo)), s)
  }
  for (v in intersect(c("voted_bjp"), names(dd))) {
    idx <- idx+1L
    rows[[idx]] <- bind_cols(tibble(Variable="Voted BJP"), weighted_stats(as.numeric(dd[[v]]), dd[[RESP_WEIGHT_VAR]]))
  }
  # V2 categorical controls as category indicators.
  for (v in intersect(c("religion_group","caste_group","education_harmonized"), names(dd))) {
    vals <- sort(unique(as.character(dd[[v]][!is.na(dd[[v]])])))
    for (lev in vals) {
      idx <- idx+1L
      x <- as.numeric(as.character(dd[[v]])==lev)
      rows[[idx]] <- bind_cols(tibble(Variable=paste0(v, ": ", lev)), weighted_stats(x, dd[[RESP_WEIGHT_VAR]]))
    }
  }
  bind_rows(rows)
}

make_ac_ideology_counts <- function(data, year_filter = NULL) {
  dd <- data |> filter(!is.na(voter_ideology_harmonized))
  if (!is.null(year_filter)) dd <- dd |> filter(year %in% year_filter)
  cc <- dd |> count(year, ac_uid, ideology=as.character(voter_ideology_harmonized), name="n") |>
    complete(year, ac_uid, ideology=ideology_levels, fill=list(n=0))
  map_dfr(ideology_levels, function(ideo) {
    x <- cc$n[cc$ideology==ideo]
    bind_cols(tibble(Variable=paste0("Number of ",ideo," respondents in AC")), summary_unweighted(x))
  })
}

make_voter_diag_year <- function(y) {
  bind_rows(
    make_voter_diag(respondents, y) |> mutate(Year=y, .before=1),
    make_ac_ideology_counts(respondents, y) |> mutate(Year=y, .before=1)
  )
}
t21a <- bind_rows(make_voter_diag_year(2009), make_voter_diag_year(2014))
p21a <- file.path(paper_dirs$main_tables, "21a_main_voter_diagnostics_year_specific.tex")
write_df_tex(t21a, p21a, "Voter-level descriptive statistics by election year", "tab:voter_diagnostics_year", notes = c(
  "Respondent-level means and quantiles use NES survey weights. Ideology and categorical V2 controls are represented as 0/1 category indicators, so their weighted means are weighted category shares.",
  "Counts of respondents in each ideology bucket are AC-level unweighted survey-sample counts and are summarized across ACs."
), longtable = TRUE)
register_output("21", "table_tex", p21a)

t21b <- bind_rows(make_voter_diag(respondents, NULL), make_ac_ideology_counts(respondents, NULL))
p21b <- file.path(paper_dirs$appendix_tables, "21b_appendix_voter_diagnostics_pooled.tex")
write_df_tex(t21b, p21b, "Pooled voter-level descriptive statistics", "tab:voter_diagnostics_pooled", longtable = TRUE)
register_output("21", "table_tex", p21b)

# ------------------------------------------------------------
# Tables 22-23: variable dictionaries and role matrices
# ------------------------------------------------------------
dict_path <- first_existing(c(
  file.path(paths$final_dir, "data_dictionary.csv"),
  file.path(paths$derived_dir, "data_dictionary.csv"),
  file.path(project_root, "derived", "data_dictionary.csv")
))
if (!is.na(dict_path)) {
  dictionary <- read_csv(dict_path, show_col_types = FALSE)
} else {
  dictionary <- tibble(variable = unique(c(names(ac_year), names(respondents))), label = variable)
}

aggregate_vars <- unique(c(
  "bjp_vote_share", "d_bjp_vote_share_2009_2014_pp", "bjp_vote_share_2014", "bjp_vote_share_2009",
  if (!is.null(fdi_meta)) c(fdi_meta$pooled_var, fdi_meta$change_var, fdi_meta$baseline_var) else character(),
  if (!is.null(muslim_meta)) c(muslim_meta$pooled_var, muslim_meta$change_var) else character(),
  if (!is.null(migration_meta)) c(migration_meta$pooled_var, migration_meta$change_var) else character(),
  "center_share_2009", C1_VARS, C2_EXTRA_CANDIDATES, "state_no", "ac_uid", "pc_cluster_id", "year"
))
respondent_vars <- unique(c(
  "voted_bjp", "voter_ideology_harmonized", "center_harmonized", RESP_WEIGHT_VAR, "respondent_sample_candidate_present",
  "religion_group", "caste_group", "education_harmonized", "income_harmonized",
  aggregate_vars, "district_harmonization_group_id"
))

make_dictionary <- function(vars) {
  if (!"variable" %in% names(dictionary)) return(tibble(Variable=vars))
  dictionary |>
    filter(variable %in% vars) |>
    select(any_of(c("variable","label","unit","source_file","source_table","source_geography","time_reference","formula_or_definition","main_role","notes"))) |>
    distinct(variable, .keep_all = TRUE) |>
    arrange(variable)
}

t22a <- make_dictionary(aggregate_vars)
p22a <- file.path(paper_dirs$appendix_tables, "22a_appendix_constituency_variable_dictionary.tex")
write_df_tex(t22a, p22a, "Constituency-level variable dictionary", "tab:ac_variable_dictionary", longtable = TRUE)
register_output("22", "table_tex", p22a)

t22b <- tribble(
  ~`Dependent Variable`, ~FDI, ~Demographics, ~`Center / ideology moderator`, ~Controls, ~`FE / clustering`,
  "BJP vote share; 2009-14 change; 2014 BJP vote share", "Manufacturing, total, services; own/local; count/per-capita/log forms; announced/opened/all", "2001 Muslim family; Muslim change family; established migration/language family; migration change family", "Survey-weighted 2009 AC Center share among ideology-complete respondents (N>=5)", "C1: population, area, SC/ST; C2: C1 + employment intensity + secondary education", "AC FE + year FE for within-AC models; state FE for lagged outcome; PC-clustered SEs"
)
p22b <- file.path(paper_dirs$appendix_tables, "22b_appendix_constituency_variables_by_role.tex")
write_df_tex(t22b, p22b, "Constituency variables by analytical role", "tab:ac_variables_role")
register_output("22", "table_tex", p22b)

t23a <- make_dictionary(respondent_vars)
p23a <- file.path(paper_dirs$appendix_tables, "23a_appendix_voter_variable_dictionary.tex")
write_df_tex(t23a, p23a, "Voter-level variable dictionary", "tab:voter_variable_dictionary", longtable = TRUE)
register_output("23", "table_tex", p23a)

t23b <- tribble(
  ~`Dependent Variable`, ~FDI, ~Demographics, ~`Ideology moderator`, ~`Voter controls`, ~`Context / inference`,
  "BJP vote indicator", "AC manufacturing, total, services FDI exposure; preferred local all-project log-per-capita measure", "2001 Muslim family; Muslim change; established migration/language; migration change", "Main: harmonized Left/Center/Right/Mixed bucket; Appendix: center_harmonized Center-vs.-others robustness", "V2: religion + caste + harmonized education", "C1/C2 AC controls; survey weights; BJP-candidate-present primary sample; state or state-year FE; PC + district multiway clusters"
)
p23b <- file.path(paper_dirs$appendix_tables, "23b_appendix_voter_variables_by_role.tex")
write_df_tex(t23b, p23b, "Voter-level variables by analytical role", "tab:voter_variables_role")
register_output("23", "table_tex", p23b)

# ------------------------------------------------------------
# Regression data preparation and fitting: Tables 24-31
# ------------------------------------------------------------
C1_NATIVE <- c("ac_pop_x","land_area_x","sc_share_x","st_share_x")
C2_NATIVE <- c(C1_NATIVE,"employment_x","education_x")
V2_TERMS <- c("religion_group","caste_group","education_harmonized")

# Frozen design-to-FDI-variable mapping used by Tables 24-31.
fdi_variable_mapping_audit <- tibble::tribble(
  ~design, ~analysis_level, ~current_fdi_variable, ~interpretation,
  "panel", "AC", preferred_fdi_pooled, "row-specific 2009/2014 current five-year FDI window",
  "lagged", "AC", preferred_fdi_2014, "2014 five-year FDI exposure",
  "respondent_pooled", "voter", preferred_fdi_pooled, "respondent row's current-year AC FDI exposure",
  "respondent_2014", "voter", preferred_fdi_pooled, "2014 respondent row's current AC FDI exposure"
)

write_csv(
  fdi_variable_mapping_audit,
  file.path(paper_dirs$audit, "05b_regression_design_fdi_variable_mapping.csv")
)

add_common_model_vars <- function(data, domain, units = c("native","informative"), design = c("panel","lagged","respondent_pooled","respondent_2014")) {
  units <- match.arg(units); design <- match.arg(design)
  demo_var <- if (domain == "muslim") preferred_muslim else preferred_migration
  demo_ref <- if (domain == "muslim") REF_MUSLIM else REF_MIGRATION
  # AC and respondent analysis files store the current-period FDI exposure
  # differently:
  #   * ac_change.rds stores the 2014 exposure with an explicit _2014 suffix;
  #   * nes_respondent_analysis.rds stores each respondent row's current-year
  #     exposure in the unsuffixed variable log1p_fdi_mfg_local_all_pc100k.
  #
  # Therefore the 2014 baseline-adjusted respondent design MUST use the same
  # unsuffixed current-period FDI variable as the pooled respondent design.
  # Only the AC lagged-outcome design uses preferred_fdi_2014.
  fdi_var <- dplyr::case_when(
    design == "lagged" ~ preferred_fdi_2014,
    design %in% c("panel", "respondent_pooled", "respondent_2014") ~ preferred_fdi_pooled,
    TRUE ~ NA_character_
  )

  if (is.na(fdi_var) || !nzchar(fdi_var)) {
    stop("No FDI variable mapping defined for design: ", design)
  }

  out <- data
  if (!fdi_var %in% names(out)) stop("Missing FDI variable for ", design, ": ", fdi_var)
  if (!demo_var %in% names(out)) stop("Missing demographic variable: ", demo_var)

  y_vec <- switch(
    design,
    panel = as.numeric(out$bjp_vote_share),
    lagged = as.numeric(out$bjp_vote_share_2014),
    respondent_pooled = as.numeric(out$voted_bjp) * ifelse(units == "informative", 100, 1),
    respondent_2014 = as.numeric(out$voted_bjp) * ifelse(units == "informative", 100, 1)
  )

  center_vec <- if ("center_share_2009" %in% names(out)) out$center_share_2009 else rep(NA_real_, nrow(out))
  baseline_fdi_vec <- if (preferred_fdi_2009 %in% names(out)) out[[preferred_fdi_2009]] else rep(NA_real_, nrow(out))
  baseline_bjp_vec <- if ("bjp_vote_share_2009" %in% names(out)) out$bjp_vote_share_2009 else rep(NA_real_, nrow(out))
  employment_vec <- if ("log1p_employment_per_total_population" %in% names(out)) {
    out$log1p_employment_per_total_population
  } else if ("employment_per_total_population" %in% names(out)) {
    log1p(out$employment_per_total_population)
  } else {
    rep(NA_real_, nrow(out))
  }
  ideology_vec <- if ("voter_ideology_harmonized_center_ref" %in% names(out)) {
    out$voter_ideology_harmonized_center_ref
  } else {
    factor(rep(NA_character_, nrow(out)), levels = c("Center","Left","Right","Mixed"))
  }

  out$y_x <- y_vec
  out$fdi_x <- out[[fdi_var]] / ifelse(units == "informative", REF_FDI, 1)
  out$demo_x <- out[[demo_var]] / ifelse(units == "informative", demo_ref, 1)
  out$center_x <- center_vec / ifelse(units == "informative", REF_CENTER, 1)
  out$baseline_fdi_x <- baseline_fdi_vec / ifelse(units == "informative", REF_FDI, 1)
  out$baseline_bjp_x <- baseline_bjp_vec
  out$ac_pop_x <- out$proxy_ac_pop / ifelse(units == "informative", 100000, 1)
  out$land_area_x <- out$con08_land_area
  out$sc_share_x <- out$sc_pop_share * ifelse(units == "informative", 100, 1)
  out$st_share_x <- out$st_pop_share * ifelse(units == "informative", 100, 1)
  out$employment_x <- employment_vec
  out$education_x <- out$ed_sec_share * ifelse(units == "informative", 100, 1)
  out$ideology_bucket <- ideology_vec
  out
}

rhs_interaction <- function(order = c("two_way","triple"), triple_type = c("ac_center","ideology")) {
  order <- match.arg(order); triple_type <- match.arg(triple_type)
  if (order == "two_way") return("fdi_x * demo_x")
  if (triple_type == "ac_center") return("fdi_x * demo_x * center_x")
  "fdi_x * demo_x * ideology_bucket"
}

fit_feols <- function(data, rhs, controls = character(), baseline = character(), fe = NULL, weights = NULL, vcov = NULL) {
  terms <- c(rhs, baseline, controls)
  terms <- terms[nzchar(terms)]
  ftxt <- paste0("y_x ~ ", paste(terms, collapse = " + "))
  if (!is.null(fe) && nzchar(fe)) ftxt <- paste0(ftxt, " | ", fe)
  fml <- as.formula(ftxt)
  fixest::feols(
    fml,
    data = data,
    weights = weights,
    vcov = vcov,
    notes = FALSE,
    warn = FALSE
  )
}

build_ac_fits <- function(domain, order, units) {
  panel <- add_common_model_vars(ac_year, domain, units, "panel") |>
    filter(year %in% c(2009,2014))
  if (order == "triple") panel <- panel |> filter(center_n_2009 >= 5, !is.na(center_x))
  rhs <- rhs_interaction(order, "ac_center")
  fits_a <- list(
    m1 = fit_feols(panel, rhs, vcov = ~pc_cluster_id),
    m2 = fit_feols(panel, rhs, controls = C1_NATIVE, vcov = ~pc_cluster_id),
    m3 = fit_feols(panel, rhs, fe = "ac_uid + year", vcov = ~pc_cluster_id),
    m4 = fit_feols(panel, rhs, controls = C2_NATIVE, fe = "ac_uid + year", vcov = ~pc_cluster_id)
  )

  lag <- add_common_model_vars(ac_change, domain, units, "lagged")
  if (order == "triple") lag <- lag |> filter(center_n_2009 >= 5, !is.na(center_x))
  baseline <- c("baseline_fdi_x","baseline_bjp_x")
  fits_b <- list(
    m1 = fit_feols(lag, rhs, baseline = baseline, vcov = ~pc_cluster_id),
    m2 = fit_feols(lag, rhs, controls = C1_NATIVE, baseline = baseline, vcov = ~pc_cluster_id),
    m3 = fit_feols(lag, rhs, controls = C1_NATIVE, baseline = baseline, fe = "state_no", vcov = ~pc_cluster_id),
    m4 = fit_feols(lag, rhs, controls = C2_NATIVE, baseline = baseline, fe = "state_no", vcov = ~pc_cluster_id)
  )
  list(a=fits_a,b=fits_b)
}

build_resp_fits <- function(domain, order, units) {
  pool <- add_common_model_vars(respondents, domain, units, "respondent_pooled") |>
    filter(year %in% c(2009,2014), respondent_sample_candidate_present, !is.na(.data[[RESP_WEIGHT_VAR]]))
  if (order == "triple") pool <- pool |> filter(!is.na(ideology_bucket))
  rhs <- rhs_interaction(order, "ideology")
  v2c1 <- c(V2_TERMS, C1_NATIVE)
  v2c2 <- c(V2_TERMS, C2_NATIVE)
  fits_a <- list(
    m1 = fit_feols(pool, rhs, weights = pool[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m2 = fit_feols(pool, rhs, controls = v2c1, weights = pool[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m3 = fit_feols(pool, rhs, controls = v2c1, fe = "state_no^year", weights = pool[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m4 = fit_feols(pool, rhs, controls = v2c2, fe = "state_no^year", weights = pool[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id)
  )

  r14 <- add_common_model_vars(respondents, domain, units, "respondent_2014") |>
    filter(year == 2014, respondent_sample_candidate_present, !is.na(.data[[RESP_WEIGHT_VAR]]))
  if (order == "triple") r14 <- r14 |> filter(!is.na(ideology_bucket))
  baseline <- c("baseline_fdi_x","baseline_bjp_x")
  fits_b <- list(
    m1 = fit_feols(r14, rhs, baseline = baseline, weights = r14[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m2 = fit_feols(r14, rhs, controls = v2c1, baseline = baseline, weights = r14[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m3 = fit_feols(r14, rhs, controls = v2c1, baseline = baseline, fe = "state_no", weights = r14[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id),
    m4 = fit_feols(r14, rhs, controls = v2c2, baseline = baseline, fe = "state_no", weights = r14[[RESP_WEIGHT_VAR]], vcov = ~pc_cluster_id + district_harmonization_group_id)
  )
  list(a=fits_a,b=fits_b)
}

ac_panel_labels <- c("Pooled OLS, no controls", "Pooled OLS + C1", "AC FE + year FE", "AC FE + year FE + C2")
lag_labels <- c("No controls", "+ C1", "+ C1 + state FE", "+ C2 + state FE")
resp_pool_labels <- c("No controls", "+ V2 + C1", "+ V2 + C1 + state-year FE", "+ V2 + C2 + state-year FE")
resp14_labels <- c("Baseline terms only", "+ V2 + C1", "+ V2 + C1 + state FE", "+ V2 + C2 + state FE")

regression_model_object_audit <- list()

reg_specs <- tribble(
  ~item, ~placement, ~level, ~domain, ~order, ~title,
  24, "main", "ac", "muslim", "two_way", "Manufacturing FDI $\\times$ 2001 Muslim share: constituency models",
  25, "main", "ac", "muslim", "triple", "Manufacturing FDI $\\times$ 2001 Muslim share $\\times$ centrist context: constituency models",
  26, "appendix", "ac", "migration", "two_way", "Manufacturing FDI $\\times$ established migrant share: constituency models",
  27, "appendix", "ac", "migration", "triple", "Manufacturing FDI $\\times$ established migrant share $\\times$ centrist context: constituency models",
  28, "main", "voter", "muslim", "two_way", "Manufacturing FDI $\\times$ 2001 Muslim share: voter-level LPMs",
  29, "main", "voter", "muslim", "triple", "Manufacturing FDI $\\times$ 2001 Muslim share $\\times$ ideology bucket: voter-level LPMs",
  30, "appendix", "voter", "migration", "two_way", "Manufacturing FDI $\\times$ established migrant share: voter-level LPMs",
  31, "appendix", "voter", "migration", "triple", "Manufacturing FDI $\\times$ established migrant share $\\times$ ideology bucket: voter-level LPMs"
)

for (i in seq_len(nrow(reg_specs))) {
  sp <- reg_specs[i,]
  demo_label <- if (sp$domain == "muslim") "Muslim share (2001)" else "Established migrant share (2001)"
  triple_type <- if (sp$level == "ac") "ac_center" else "ideology"
  out_dir <- if (sp$placement == "main") paper_dirs$main_tables else paper_dirs$appendix_tables
  for (units in c("informative","native")) {
    fs <- if (sp$level == "ac") build_ac_fits(sp$domain, sp$order, units) else build_resp_fits(sp$domain, sp$order, units)

    audit_rows_this <- bind_rows(
      imap_dfr(fs$a, ~audit_fit_object(.x, sp$item, units, "A", .y)),
      imap_dfr(fs$b, ~audit_fit_object(.x, sp$item, units, "B", .y))
    )
    regression_model_object_audit[[length(regression_model_object_audit) + 1]] <- audit_rows_this

    # Every requested regression model must retain at least one FDI-related
    # estimable coefficient.  If not, fail explicitly rather than silently
    # writing an empty publication table.
    bad_fit <- audit_rows_this |>
      filter(is.na(n_coefficients) | n_coefficients == 0 | !has_fdi_term)

    if (nrow(bad_fit) > 0) {
      audit_so_far <- bind_rows(regression_model_object_audit)
      write_csv(
        audit_so_far,
        file.path(paper_dirs$audit, "06_regression_model_object_audit.csv")
      )
      stop(
        "At least one requested regression model has no extractable FDI coefficient. ",
        "See audit/06_regression_model_object_audit.csv. Problem models: ",
        paste(
          paste0(
            "Table ", bad_fit$table_item,
            " ", bad_fit$units,
            " panel ", bad_fit$panel,
            " ", bad_fit$model
          ),
          collapse = "; "
        )
      )
    }
    suffix <- if (units == "informative") "a" else "b"
    stem <- paste0(sp$item, suffix, "_", sp$placement, "_", sp$level, "_", sp$domain, "_", sp$order, "_", units)
    path <- file.path(out_dir, paste0(stem, ".tex"))
    unit_note <- if (units == "informative") {
      paste0(
        "Informative-unit version: manufacturing FDI is scaled so one unit is 0 to the common median-positive 2014 AC exposure (", sprintf("%.4f", REF_FDI), "); ",
        if (sp$domain == "muslim") paste0("Muslim share is scaled 0 to its common median (", sprintf("%.4f", REF_MUSLIM), ")") else paste0("established migrant share is scaled 0 to its common median (", sprintf("%.4f", REF_MIGRATION), ")"),
        if (sp$order == "triple" && sp$level == "ac") paste0("; AC Center share is scaled 0 to the audited N>=5 reference value (", sprintf("%.4f", REF_CENTER), "), using the ordinary median when positive and the median positive when the ordinary median is zero") else "",
        ". Aggregate outcomes are percentage points/percent as recorded; voter outcomes are multiplied by 100, so LPM coefficients are percentage points. Proportion controls are expressed in percentage points and population in 100,000s."
      )
    } else {
      "Native-unit version: coefficients use the variables exactly as stored in the analysis data. FDI is log(1 + local manufacturing projects per 100,000); demographic shares and Center share are proportions from 0 to 1; voter LPM outcome is 0/1."
    }
    design_note <- if (sp$level == "ac") {
      paste0(
        "Panel A culminates in an AC fixed-effects + year fixed-effects estimator. Time-invariant C1/C2 controls are absorbed by AC FE; Model 4 can differ from Model 3 only for controls that genuinely vary within AC. The audit file 05_ac_fe_control_variation_audit.csv reports this directly. Panel B models 2014 BJP vote share conditional on 2009 BJP vote share and baseline FDI. PC-clustered standard errors are used throughout."
      )
    } else {
      "All respondent models are survey-weighted LPMs on the BJP-candidate-present sample, with standard errors multiway-clustered by parliamentary constituency and district harmonization group. Main triple models use the full harmonized four-category ideology bucket with Center as the native-model reference; center_harmonized is retained for appendix robustness/specification-curve analyses."
    }
    star_note <- "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. Exact p-values and standard errors are reported in each cell."
    write_two_panel_regression_tex(
      path, paste0(sp$title, " (", ifelse(units=="informative","informative units","native units"), ")"),
      paste0("tab:", sp$item, suffix), fs$a, fs$b,
      if (sp$level == "ac") ac_panel_labels else resp_pool_labels,
      if (sp$level == "ac") lag_labels else resp14_labels,
      demo_label, triple_type, units,
      notes = c(unit_note, design_note, star_note)
    )
    register_output(as.character(sp$item), "table_tex", path)
  }
}

# Persist a compact audit of every model object used in Tables 24-31.
if (length(regression_model_object_audit)) {
  write_csv(
    bind_rows(regression_model_object_audit),
    file.path(paper_dirs$audit, "06_regression_model_object_audit.csv")
  )
}

# ------------------------------------------------------------
# Tables 32 and 34: specification-curve readouts
# ------------------------------------------------------------
if (!exists("read_latest_curve_files")) {
  read_latest_curve_files <- function(level = c("aggregate", "respondent")) {
    level <- match.arg(level)
    root <- if (level == "aggregate") file.path(paths$derived_dir, "model_exploration", "specification_curves", "results") else file.path(paths$derived_dir, "model_exploration", "respondent_specification_curves", "results")
    if (!dir.exists(root)) return(tibble())
    files <- list.files(root, pattern = "__full__.*\\.csv$", full.names = TRUE)
    if (level == "aggregate") {
      pref <- files[str_detect(basename(files), "v5\\.0-control-repair")]
      if (length(pref)) files <- pref
    } else {
      pref <- files[str_detect(basename(files), "^primary__")]
      if (length(pref)) files <- pref
    }
    map_dfr(files, ~tryCatch(read_csv(.x, show_col_types=FALSE, progress=FALSE) |> mutate(source_file=basename(.x),analysis_level=level), error=function(e)tibble()))
  }
}
if (!exists("curve_family_from_var")) {
  curve_family_from_var <- function(domain, moderator_var, moderator_family = "") {
    v <- as.character(moderator_var); fam <- as.character(moderator_family)
    if (domain == "muslim") {
      if (str_detect(v, "^d_|pct_change|2001_2011") || str_detect(str_to_lower(fam), "change")) return("change_muslim")
      if (str_detect(v, "2001") && !str_detect(v, "2011") && !str_detect(v, "^d_|pct_change")) return("existing_muslim")
    }
    if (domain == "migration") {
      if (str_detect(v, "^d_|pct_change|accel|2001_2011|2009_2014")) return("change_migrant")
      if (str_detect(v, "total_upto_2001|bengali_bhojpuri.*2001|male_mig_total_upto_2001")) return("existing_migrant")
    }
    NA_character_
  }
}

agg_curve_all <- if (exists("agg_curves")) agg_curves else read_latest_curve_files("aggregate")
resp_curve_all <- if (exists("resp_curves")) resp_curves else read_latest_curve_files("respondent")
for (nm in c("agg_curve_all","resp_curve_all")) {
  obj <- get(nm)
  if (nrow(obj) && !"curve_family" %in% names(obj)) {
    obj <- obj |> mutate(
      domain = coalesce(moderator_domain, if_else(str_detect(design_id,"muslim"),"muslim","migration")),
      curve_family = pmap_chr(list(domain, moderator_var, moderator_family), curve_family_from_var)
    )
    assign(nm,obj)
  }
}

# Harmonize CI/significance fields across the aggregate and respondent
# specification-curve runners.  The aggregate runner persists ci_positive and
# ci_negative directly; some respondent curve files instead persist only
# ci_excludes_zero plus the contrast estimate (and, in newer files,
# contrast_conf_low / contrast_conf_high).
normalize_curve_ci_fields <- function(dd) {
  if (!nrow(dd)) return(dd)

  # First derive ci_excludes_zero if an older file omitted it but retained
  # confidence limits.
  if (!"ci_excludes_zero" %in% names(dd)) {
    if (all(c("contrast_conf_low", "contrast_conf_high") %in% names(dd))) {
      dd <- dd |>
        mutate(
          ci_excludes_zero =
            (contrast_conf_low > 0) |
            (contrast_conf_high < 0)
        )
    } else {
      dd$ci_excludes_zero <- NA
    }
  }

  # Direction-specific significance can be derived exactly from confidence
  # limits when available.  Otherwise, an estimate whose CI excludes zero has
  # the same sign as the entire CI, so combine ci_excludes_zero with the sign
  # of the contrast estimate.
  if (!"ci_positive" %in% names(dd)) {
    if ("contrast_conf_low" %in% names(dd)) {
      dd <- dd |>
        mutate(
          ci_positive = contrast_conf_low > 0
        )
    } else {
      dd <- dd |>
        mutate(
          ci_positive =
            (ci_excludes_zero %in% c(TRUE, 1)) &
            is.finite(contrast_estimate) &
            contrast_estimate > 0
        )
    }
  }

  if (!"ci_negative" %in% names(dd)) {
    if ("contrast_conf_high" %in% names(dd)) {
      dd <- dd |>
        mutate(
          ci_negative = contrast_conf_high < 0
        )
    } else {
      dd <- dd |>
        mutate(
          ci_negative =
            (ci_excludes_zero %in% c(TRUE, 1)) &
            is.finite(contrast_estimate) &
            contrast_estimate < 0
        )
    }
  }

  dd
}

# Persist the input-schema distinction that motivated the harmonization.
curve_schema_audit <- bind_rows(
  tibble(
    analysis_level = "aggregate",
    n_rows = nrow(agg_curve_all),
    has_ci_excludes_zero = "ci_excludes_zero" %in% names(agg_curve_all),
    has_ci_positive = "ci_positive" %in% names(agg_curve_all),
    has_ci_negative = "ci_negative" %in% names(agg_curve_all),
    has_contrast_conf_low = "contrast_conf_low" %in% names(agg_curve_all),
    has_contrast_conf_high = "contrast_conf_high" %in% names(agg_curve_all)
  ),
  tibble(
    analysis_level = "respondent",
    n_rows = nrow(resp_curve_all),
    has_ci_excludes_zero = "ci_excludes_zero" %in% names(resp_curve_all),
    has_ci_positive = "ci_positive" %in% names(resp_curve_all),
    has_ci_negative = "ci_negative" %in% names(resp_curve_all),
    has_contrast_conf_low = "contrast_conf_low" %in% names(resp_curve_all),
    has_contrast_conf_high = "contrast_conf_high" %in% names(resp_curve_all)
  )
)

write_csv(
  curve_schema_audit,
  file.path(
    paper_dirs$audit,
    "07_specification_curve_schema_audit.csv"
  )
)

curve_summary <- function(dd) {
  if (!nrow(dd)) return(tibble())

  dd <- normalize_curve_ci_fields(dd)

  required_curve_cols <- c(
    "fit_ok",
    "contrast_estimate",
    "ci_excludes_zero",
    "ci_positive",
    "ci_negative",
    "nobs",
    "analysis_level",
    "design_id",
    "fdi_family",
    "interaction_order"
  )

  missing_curve_cols <- setdiff(
    required_curve_cols,
    names(dd)
  )

  if (length(missing_curve_cols) > 0) {
    stop(
      "Specification-curve file is missing required fields after schema harmonization: ",
      paste(missing_curve_cols, collapse = ", "),
      ". See audit/07_specification_curve_schema_audit.csv."
    )
  }

  dd |>
    filter(
      fit_ok %in% c(TRUE, 1),
      is.finite(contrast_estimate)
    ) |>
    group_by(
      analysis_level,
      design_id,
      fdi_family,
      interaction_order
    ) |>
    summarise(
      `Specifications` = n(),
      `Median contrast` = median(contrast_estimate, na.rm = TRUE),
      `Q1` = quantile(contrast_estimate, .25, na.rm = TRUE),
      `Q3` = quantile(contrast_estimate, .75, na.rm = TRUE),
      `Share positive` = mean(contrast_estimate > 0, na.rm = TRUE),
      `Share CI excludes 0` = mean(ci_excludes_zero %in% c(TRUE, 1), na.rm = TRUE),
      `Share significant positive` = mean(ci_positive %in% c(TRUE, 1), na.rm = TRUE),
      `Share significant negative` = mean(ci_negative %in% c(TRUE, 1), na.rm = TRUE),
      `Min N` = min(nobs, na.rm = TRUE),
      `Median N` = median(nobs, na.rm = TRUE),
      `Max N` = max(nobs, na.rm = TRUE),
      .groups = "drop"
    )
}

t32 <- bind_rows(
  agg_curve_all |> filter(moderator_var == preferred_muslim) |> mutate(analysis_level="Constituency"),
  resp_curve_all |> filter(moderator_var == preferred_muslim) |> mutate(analysis_level="Voter")
) |> curve_summary()
p32 <- file.path(paper_dirs$main_tables, "32_main_specification_curve_readout_muslim2001_three_fdi.tex")
write_df_tex(t32, p32, "Specification-curve readout: 2001 Muslim share across manufacturing, total, and services FDI", "tab:spec_readout_muslim", notes="Contrasts are the substantive quantities stored by the frozen curve runners. Respondent appendix triple curves use the frozen center_harmonized definition; the main respondent triple regression tables use the full harmonized ideology bucket.", longtable=TRUE)
register_output("32","table_tex",p32)

curve_family_labels <- c(existing_muslim="Existing Muslim context (2001 family)",change_muslim="Muslim change family",existing_migrant="Existing migration/compositional context",change_migrant="Migration change family")
idx34 <- 0L
for (level in c("aggregate","respondent")) {
  dd0 <- if (level=="aggregate") agg_curve_all else resp_curve_all
  if (!nrow(dd0)) next
  for (fam in names(curve_family_labels)) {
    dd <- dd0 |> filter(curve_family==fam)
    if (!nrow(dd)) next
    idx34 <- idx34 + 1L
    suffix <- letters[idx34]
    sm <- curve_summary(dd)
    path <- file.path(paper_dirs$appendix_tables, paste0("34",suffix,"_appendix_specification_curve_readout_",level,"_",fam,".tex"))
    write_df_tex(sm,path,paste0("Specification-curve readout: ",curve_family_labels[[fam]]," (",level,")"),paste0("tab:spec_readout_",level,"_",fam),longtable=TRUE)
    register_output("34","table_tex",path)
  }
}

# ------------------------------------------------------------
# Table 35: shift-share / IV route audit
# ------------------------------------------------------------
t35 <- tribble(
  ~Strategy, ~Construction, ~`Post first stage`, ~`Pre-period placebo`, ~`Concentration / robustness`, ~`Why it is not a clean primary instrument`, ~Adjudication,
  "WIR industry Bartik: own AC, within manufacturing", "2005 EC05 within-manufacturing industry shares $\\times$ subsequent global industry greenfield-FDI shocks", "$F=10.25$ (high-quality EC05)", "$F=1.11$", "Effective industries $\\approx 3.37$; leave-one-out $F=4.25$ without textiles, $5.45$ without non-metallic minerals, $6.72$ without food/beverages/tobacco", "Nominal relevance and placebo are encouraging, but identifying variation is concentrated in a few sectors whose global shocks can affect Indian local economies through trade, prices, production, or employment independently of local FDI.", "Exploratory / sensitivity only",
  "Matched-local WIR Bartik", "Own + touching-AC FDI treatment matched to own + touching-AC EC05 industrial exposure", "Strict: $F=3.74$; high-quality: $F=12.66$", "Strict: $F=10.30$; high-quality: $F=7.79$", "First stage weak in strict geography; placebo remains substantial in high-quality sample", "The purported post-2009 instrument predicts pre-period FDI geography strongly; matching treatment and exposure geography does not solve persistence.", "Reject",
  "Official FDI-policy exposure", "Predetermined sector exposure interacted with 2012--14 national FDI liberalizations", "No clean manufacturing first stage available", "N/A", "Zero reforms cleanly map both to predetermined EC05 manufacturing exposure and the preferred greenfield-manufacturing FDI treatment; telecom/service window is sparse", "Available reforms mainly affect services or do not cleanly identify the preferred manufacturing treatment.", "Unavailable for headline manufacturing IV",
  "Source-country push $\\times$ industry composition", "Pre-2009 source-country dependence of Indian industries $\\times$ subsequent rest-of-world source-country outward-FDI shocks $\\times$ 2005 AC industry composition", "Full: $F=3.78$; high-quality: $F=7.19$", "Full: $F=9.72$; high-quality: $F=9.57$", "About eight effective source countries; no single country rescues the design", "The instrument predicts historical FDI geography at least as strongly as subsequent FDI, indicating persistent investment networks rather than a clean new capital-supply shock.", "Reject"
)
p35 <- file.path(paper_dirs$appendix_tables, "35_appendix_shift_share_iv_strategy_audit.tex")
write_df_tex(t35,p35,"Shift-share and policy identification strategies evaluated before political second stages","tab:iv_audit",notes=c(
  "All diagnostics were evaluated before inspecting BJP 2SLS coefficients. The screening criteria were: (i) relevant post-period first stage; (ii) weak pre-period placebo; (iii) leave-one-shock robustness; and (iv) sufficiently diffuse external variation with a plausible exclusion restriction.",
  "The conventional $F\\approx10$ benchmark is treated as a screening heuristic rather than a sufficient validity test."
),longtable=TRUE,escape=FALSE)
register_output("35","table_tex",p35)

message("Table generation complete.")
write_status_ledger()

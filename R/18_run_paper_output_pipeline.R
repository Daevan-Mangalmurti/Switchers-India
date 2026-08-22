# ============================================================
# 18_run_paper_output_pipeline.R
# Master runner for all numbered Switchers paper outputs.
# ============================================================

project_root <- Sys.getenv(
  "SWITCHERS_ROOT",
  unset = "/Users/Daevan/Downloads/Switchers-India"
)

scripts <- c(
  "15_prepare_paper_outputs.R",
  "16_generate_paper_figures.R",
  "17_generate_paper_tables.R"
)

for (script in scripts) {
  p <- file.path(project_root, "R", script)
  if (!file.exists(p)) stop("Missing paper-output pipeline script: ", p)
  message("\n============================================================")
  message("Running ", script)
  message("============================================================")
  source(p, chdir = FALSE)
}

write_status_ledger()
message("\nPaper output pipeline COMPLETE")
message("Output root: ", paper_output_root)
message("Status ledger: ", file.path(paper_dirs$audit, "00_paper_output_status.csv"))

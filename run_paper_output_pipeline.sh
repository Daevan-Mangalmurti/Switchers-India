#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="${SWITCHERS_ROOT:-/Users/Daevan/Downloads/Switchers-India}"
cd "$PROJECT_ROOT"
mkdir -p logs

Rscript --vanilla R/18_run_paper_output_pipeline.R \
  2>&1 | tee logs/paper_output_pipeline_2026-08-12.log

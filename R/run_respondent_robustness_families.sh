#!/usr/bin/env bash
set -euo pipefail

ROOT="${SWITCHERS_ROOT:-/Users/Daevan/Downloads/Switchers-India}"
RUNNER="R/02c_respondent_models_v1_0_3_robustness_runner.R"

cd "$ROOT"
mkdir -p logs

if [[ ! -f "$RUNNER" ]]; then
  echo "ERROR: Missing $ROOT/$RUNNER" >&2
  exit 1
fi

run_shard () {
  local family="$1"
  local design="$2"
  local log_file="logs/respondent_robustness__${family}__${design}.log"

  echo ""
  echo "============================================================"
  echo "Starting robustness shard"
  echo "  family: $family"
  echo "  design: $design"
  echo "  log:    $log_file"
  echo "============================================================"

  env \
    SWITCHERS_ROOT="$ROOT" \
    SWITCHERS_RESPONDENT_SPEC_MODE="full" \
    SWITCHERS_RESPONDENT_FAMILIES="$family" \
    SWITCHERS_RESPONDENT_DESIGN_FILTER="$design" \
    Rscript --vanilla "$RUNNER" \
    2>&1 | tee "$log_file"

  echo "Completed: $family / $design"
}

POOLED_MUSLIM="respondent_pooled_muslim"
POOLED_MIGRATION="respondent_pooled_migration"
Y2014_MUSLIM="respondent_2014_muslim"
Y2014_MIGRATION="respondent_2014_migration"

# 10,368 models each. Four independent fresh-R shards per family.
for family in logit unweighted all_valid; do
  run_shard "$family" "$POOLED_MUSLIM"
  run_shard "$family" "$POOLED_MIGRATION"
  run_shard "$family" "$Y2014_MUSLIM"
  run_shard "$family" "$Y2014_MIGRATION"
done

# 5,184 total. This sensitivity is defined only for pooled designs.
run_shard "pooled_additive_fe" "$POOLED_MUSLIM"
run_shard "pooled_additive_fe" "$POOLED_MIGRATION"

# 2,592 total. This sensitivity is defined only for pooled triple models.
run_shard "strict_center" "$POOLED_MUSLIM"
run_shard "strict_center" "$POOLED_MIGRATION"

echo ""
echo "============================================================"
echo "ALL PRE-SPECIFIED RESPONDENT ROBUSTNESS SHARDS COMPLETE"
echo "============================================================"
echo "Expected robustness universe: 38,880 planned specifications."

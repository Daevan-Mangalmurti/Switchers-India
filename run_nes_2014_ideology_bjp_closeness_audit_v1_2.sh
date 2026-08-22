#!/bin/bash
set -euo pipefail

PROJECT_ROOT="${PROJECT_ROOT:-/Users/Daevan/Downloads/Switchers-India}"
R_NAME="20_nes_2014_ideology_bjp_closeness_audit_v1_2.R"
RUNNER_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_R_DIR="$PROJECT_ROOT/R"
PROJECT_R_SCRIPT="$PROJECT_R_DIR/$R_NAME"
DOWNLOAD_R_SCRIPT="$RUNNER_DIR/$R_NAME"

echo "Project root: $PROJECT_ROOT"

if [[ ! -d "$PROJECT_ROOT" ]]; then
  echo "ERROR: project root does not exist: $PROJECT_ROOT" >&2
  exit 1
fi

if [[ ! -f "$PROJECT_ROOT/data/lokniti/nes_2014.sav" ]]; then
  echo "ERROR: missing NES 2014 file:" >&2
  echo "  $PROJECT_ROOT/data/lokniti/nes_2014.sav" >&2
  exit 1
fi

mkdir -p "$PROJECT_R_DIR" "$PROJECT_ROOT/logs"

if [[ -f "$DOWNLOAD_R_SCRIPT" ]]; then
  cp "$DOWNLOAD_R_SCRIPT" "$PROJECT_R_SCRIPT"
  echo "Installed audit script at: $PROJECT_R_SCRIPT"
elif [[ ! -f "$PROJECT_R_SCRIPT" ]]; then
  echo "ERROR: could not find $R_NAME beside this runner or in $PROJECT_R_DIR" >&2
  exit 1
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is not on PATH." >&2
  exit 1
fi

STAMP="$(date '+%Y%m%d_%H%M%S')"
LOG="$PROJECT_ROOT/logs/nes_2014_ideology_bjp_closeness_audit_v1_2_${STAMP}.log"

cd "$PROJECT_ROOT"

echo "Running corrected NES 2014 ideology audit v1.2..."
Rscript --vanilla "$PROJECT_R_SCRIPT" 2>&1 | tee "$LOG"

echo
echo "PASS"
echo "Log: $LOG"
echo "Outputs: $PROJECT_ROOT/outputs/nes_2014_ideology_audit_v1_2"

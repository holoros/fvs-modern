#!/bin/bash
# =============================================================================
# FVS Calibration: Complete Launch Script for OSC Cardinal
#
# Run this script after SSH'ing into Cardinal. It will:
#   1. Check FIA data and project files are in place
#   2. Run setup (R packages + CmdStan) if needed
#   3. Submit the SLURM array job (25 variants)
#   4. Print monitoring commands
#
# Usage:
#   ssh crsfaaron@cardinal.osc.edu
#   cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}
#   bash calibration/osc/launch_cardinal.sh
#
# Or to skip setup (if already done):
#   bash calibration/osc/launch_cardinal.sh --skip-setup
# =============================================================================

set -e

SKIP_SETUP=false
for arg in "$@"; do
  case $arg in
    --skip-setup) SKIP_SETUP=true ;;
  esac
done

# Source configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config_osc.sh"

echo "==========================================="
echo "FVS Calibration: Cardinal Launch"
echo "==========================================="
echo "User:      $(whoami)"
echo "Account:   ${OSC_ACCOUNT}"
echo "Project:   ${PROJECT_ROOT}"
echo "FIA data:  ${FIA_DATA_DIR}"
echo "Date:      $(date)"
echo ""

# ============================================================================
# Pre flight checks
# ============================================================================

echo "[PREFLIGHT] Checking files..."

# Check project exists
if [ ! -f "${PROJECT_ROOT}/calibration/R/01_fetch_fia_data.R" ]; then
    echo "ERROR: Project files not found at ${PROJECT_ROOT}"
    echo "  Upload with: rsync -avz --exclude='.git' fvs-modern/ crsfaaron@cardinal.osc.edu:${PROJECT_ROOT}/"
    exit 1
fi
echo "  Project files: OK"

# Check config exists
if [ ! -f "${PROJECT_ROOT}/config/ne.json" ]; then
    echo "ERROR: Config JSONs not found at ${PROJECT_ROOT}/config/"
    exit 1
fi
echo "  Config JSONs: OK ($(ls ${PROJECT_ROOT}/config/*.json | wc -l) variants)"

# Check FIA data
if [ ! -d "${FIA_DATA_DIR}" ]; then
    echo "ERROR: FIA data directory not found: ${FIA_DATA_DIR}"
    echo "  Create it and upload FIA CSV files organized by state subdirectory."
    exit 1
fi

# Count FIA state directories (look for subdirectories with CSV files)
N_STATES=$(find "${FIA_DATA_DIR}" -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l)
N_FILES=$(find "${FIA_DATA_DIR}" -name "*.csv" -o -name "*.CSV" 2>/dev/null | head -100 | wc -l)

if [ "$N_STATES" -gt 0 ]; then
    echo "  FIA data: OK (${N_STATES} state directories)"
elif [ "$N_FILES" -gt 0 ]; then
    echo "  FIA data: OK (flat structure, ${N_FILES}+ CSV files)"
else
    echo "WARNING: FIA directory exists but no state subdirectories or CSV files found."
    echo "  Expected: ${FIA_DATA_DIR}/ME/, ${FIA_DATA_DIR}/NH/, etc."
    echo "  Or:       ${FIA_DATA_DIR}/ME_TREE.csv, ${FIA_DATA_DIR}/ME_PLOT.csv, etc."
    echo ""
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
fi

# Check account access
echo ""
echo "[PREFLIGHT] Checking SLURM allocation..."
if sacctmgr show assoc user=$(whoami) format=Account%20 2>/dev/null | grep -q "${OSC_ACCOUNT}"; then
    echo "  Account ${OSC_ACCOUNT}: OK"
else
    echo "WARNING: Could not verify account ${OSC_ACCOUNT}."
    echo "  If submission fails, check: sacctmgr show assoc user=$(whoami)"
fi

# Check modules
echo ""
echo "[PREFLIGHT] Checking modules..."
module purge 2>/dev/null
for MOD in "${GCC_MODULE}" "${R_MODULE}" "${GDAL_MODULE}" "${PROJ_MODULE}" "${GEOS_MODULE}"; do
    if module load "$MOD" 2>/dev/null; then
        echo "  ${MOD}: OK"
    else
        echo "  ${MOD}: FAILED"
        echo "ERROR: Module ${MOD} not available on $(hostname)."
        echo "  Run 'module spider ${MOD%%/*}' to find available versions."
        exit 1
    fi
done

echo ""
echo "All preflight checks passed."
echo ""

# ============================================================================
# Setup (install R packages + CmdStan)
# ============================================================================

if [ "$SKIP_SETUP" = true ]; then
    echo "[SETUP] Skipped (--skip-setup flag)"
else
    echo "[SETUP] Checking if R packages need installation..."

    SETUP_NEEDED=$(Rscript -e '
      pkgs <- c("tidyverse", "brms", "cmdstanr", "rFIA", "quantreg",
                "posterior", "bayesplot", "jsonlite", "logger")
      missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
      cat(length(missing))
    ' 2>/dev/null || echo "99")

    if [ "$SETUP_NEEDED" = "0" ]; then
        echo "  All R packages present. Checking CmdStan..."

        CMDSTAN_OK=$(Rscript -e '
          ok <- tryCatch({ cmdstanr::cmdstan_path(); cat("1") },
                         error = function(e) cat("0"))
        ' 2>/dev/null || echo "0")

        if [ "$CMDSTAN_OK" = "1" ]; then
            echo "  CmdStan: OK"
            echo "  Setup not needed. Skipping."
        else
            echo "  CmdStan not found. Running setup..."
            bash "${SCRIPT_DIR}/setup_osc.sh"
        fi
    else
        echo "  ${SETUP_NEEDED} packages missing. Running full setup..."
        bash "${SCRIPT_DIR}/setup_osc.sh"
    fi
fi

echo ""

# ============================================================================
# Create output directories
# ============================================================================

echo "[DIRS] Creating output directories..."
mkdir -p "${PROJECT_ROOT}/calibration/logs"
mkdir -p "${PROJECT_ROOT}/calibration/data/processed"
mkdir -p "${PROJECT_ROOT}/config/calibrated"

VARIANTS=(acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws)
for v in "${VARIANTS[@]}"; do
    mkdir -p "${PROJECT_ROOT}/calibration/output/variants/${v}"
    mkdir -p "${PROJECT_ROOT}/calibration/data/processed/${v}"
done
echo "  Created directories for ${#VARIANTS[@]} variants."

# ============================================================================
# Submit
# ============================================================================

echo ""
echo "==========================================="
echo "Submitting calibration pipeline"
echo "==========================================="
echo "  25 variants as SLURM array (0 to 24)"
echo "  ${CORES_PER_VARIANT} cores, ${MEM_PER_VARIANT} memory, ${TIME_PER_VARIANT} walltime each"
echo ""

bash "${SCRIPT_DIR}/submit_cardinal.sh"

echo ""
echo "==========================================="
echo "Monitoring Commands"
echo "==========================================="
echo ""
echo "  Check job status:"
echo "    squeue -u $(whoami)"
echo ""
echo "  Watch progress (updates every 30s):"
echo "    watch -n 30 'squeue -u $(whoami)'"
echo ""
echo "  View live log for a variant (e.g., NE = array index 14):"
echo "    tail -f ${PROJECT_ROOT}/calibration/logs/variant_ne.log"
echo ""
echo "  Check which variants have completed:"
echo "    bash ${PROJECT_ROOT}/calibration/osc/check_status.sh"
echo ""
echo "  Cancel all jobs:"
echo "    scancel -u $(whoami) --name=fvs-cal"
echo ""
echo "  Cancel one variant (e.g., array task 14):"
echo "    scancel <JOBID>_14"
echo ""

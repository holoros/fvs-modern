#!/bin/bash
# =============================================================================
# Deploy PERSEUS Harvest Scripts to Cardinal and Submit Jobs
# =============================================================================
#
# Run this script FROM YOUR LOCAL MACHINE (laptop/desktop) to:
#   1. Build ACD executable on Cardinal (if needed)
#   2. Copy harvest projection scripts to Cardinal
#   3. Submit the SLURM jobs
#
# Prerequisites:
#   - SSH access to Cardinal (key-based auth recommended)
#   - fvs-modern repo already on Cardinal at ~/fvs-modern
#
# Usage:
#   bash deploy_perseus_harvest.sh
#   bash deploy_perseus_harvest.sh --build-only    # just build ACD exe
#   bash deploy_perseus_harvest.sh --submit-only   # just copy + submit
#
# Author: A. Weiskittel
# Date: 2026-04-18
# =============================================================================

set -euo pipefail

# Override via env if your OSC user or project differs:
#   OSC_USER=myname bash deploy_perseus_harvest.sh
OSC_USER="${OSC_USER:-crsfaaron}"
CARDINAL="${OSC_USER}@cardinal.osc.edu"
REMOTE_DIR="${REMOTE_FVS_DIR:-~/fvs-modern}"
LOCAL_DIR="$(cd "$(dirname "$0")/../.." && pwd)"

# Parse args
BUILD=true
SUBMIT=true
for arg in "$@"; do
    case $arg in
        --build-only)  SUBMIT=false ;;
        --submit-only) BUILD=false ;;
    esac
done

echo "================================================================"
echo "PERSEUS Harvest Deployment to Cardinal"
echo "================================================================"
echo "Local:  ${LOCAL_DIR}"
echo "Remote: ${CARDINAL}:${REMOTE_DIR}"
echo "Build ACD exe: ${BUILD}"
echo "Submit jobs:   ${SUBMIT}"
echo "================================================================"
echo ""

# --- Step 1: Copy scripts to Cardinal --------------------------------------
echo "=== Copying scripts to Cardinal ==="

scp "${LOCAL_DIR}/calibration/python/perseus_harvest_projection.py" \
    "${CARDINAL}:${REMOTE_DIR}/calibration/python/"
echo "  Copied: perseus_harvest_projection.py"

scp "${LOCAL_DIR}/calibration/slurm/submit_perseus_harvest.sh" \
    "${CARDINAL}:${REMOTE_DIR}/calibration/slurm/"
echo "  Copied: submit_perseus_harvest.sh"

scp "${LOCAL_DIR}/deployment/scripts/build_fvs_executables.sh" \
    "${CARDINAL}:${REMOTE_DIR}/deployment/scripts/"
echo "  Copied: build_fvs_executables.sh"

echo ""

# --- Step 2: Build ACD executable (if needed) -------------------------------
if [ "${BUILD}" = true ]; then
    echo "=== Building FVS executables on Cardinal ==="
    echo "  (This may take 5-10 minutes per variant)"

    ssh "${CARDINAL}" bash -l << 'REMOTE_BUILD'
set -euo pipefail
cd ~/fvs-modern

# Check what executables exist
echo "Current lib/ contents:"
ls -la lib/FVS{ne,acd}* 2>/dev/null || echo "  (none found)"
echo ""

# Load compiler
module purge
module load gnu/13.1.0 2>/dev/null || module load gcc/13.1.0 2>/dev/null || module load gcc 2>/dev/null || true

# Build executables for ne and acd
echo "Building FVS executables..."
chmod +x deployment/scripts/build_fvs_executables.sh
bash deployment/scripts/build_fvs_executables.sh . ./lib ne acd

echo ""
echo "Verifying executables:"
for var in ne acd; do
    exe="./lib/FVS${var}"
    if [ -f "${exe}" ] && [ -x "${exe}" ]; then
        echo "  FVS${var}: OK ($(ls -lh ${exe} | awk '{print $5}'))"
    else
        echo "  FVS${var}: MISSING or not executable"
    fi
done
REMOTE_BUILD

    echo ""
fi

# --- Step 3: Submit SLURM jobs ----------------------------------------------
if [ "${SUBMIT}" = true ]; then
    echo "=== Submitting SLURM jobs on Cardinal ==="

    ssh "${CARDINAL}" bash -l << 'REMOTE_SUBMIT'
set -euo pipefail
cd ~/fvs-modern

# Create log directory
mkdir -p calibration/logs/perseus_harvest

# Submit
chmod +x calibration/slurm/submit_perseus_harvest.sh
bash calibration/slurm/submit_perseus_harvest.sh

echo ""
echo "Check status with:"
echo "  squeue -u \$USER"
REMOTE_SUBMIT

    echo ""
fi

echo "================================================================"
echo "Deployment complete."
echo ""
echo "Monitor progress on Cardinal:"
echo "  ssh ${CARDINAL}"
echo "  squeue -u \$USER --name='per_harv,per_harv_agg'"
echo ""
echo "Check batch completion:"
echo "  for scen in harvest_noclimate harvest_rcp45 harvest_rcp85 noharvest_noclimate noharvest_rcp45 noharvest_rcp85; do"
echo "    done=\$(ls ~/fvs-modern/calibration/output/perseus/\${scen}/perseus_\${scen}_batch*.csv 2>/dev/null | wc -l)"
echo "    echo \"\${scen}: \${done}/36 batches\""
echo "  done"
echo "================================================================"

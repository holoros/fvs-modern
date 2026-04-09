#!/bin/bash
#SBATCH --job-name=fvs_sdi_fix2
#SBATCH --account=PUOM0008
#SBATCH --time=00:30:00
#SBATCH --mem=16G
#SBATCH --cpus-per-task=1
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/sdi_fix2_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/sdi_fix2_%j.err

# ============================================================================
# SDIMAX post-hoc fixes:
#   1. Revert EC and EM to FVS defaults (negative correlations)
#   2. Raise per-species min stands threshold from 30 to 100
# Quick job, no MCMC needed
# ============================================================================

echo "=== SDIMAX Post-Hoc Fixes ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

module purge
module load gcc/12.3.0
module load R/4.4.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

Rscript "${SCRIPTS_DIR}/09b_fix_sdimax.R" --all
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"

# Show results
if [ -f "${FVS_PROJECT_ROOT}/calibration/output/sdimax_fixes_log.csv" ]; then
    echo "=== Fix Log ==="
    cat "${FVS_PROJECT_ROOT}/calibration/output/sdimax_fixes_log.csv"
fi

echo ""
echo "=== SDIMAX fixes complete ==="
echo "End: $(date)"

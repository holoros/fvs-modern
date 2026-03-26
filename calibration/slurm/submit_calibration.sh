#!/bin/bash
#
# FVS Bayesian Calibration Pipeline: Master SLURM Submission Script
# Submits an array job with one job per FVS variant
# Each job runs the full calibration pipeline for that variant
#
# Usage: sbatch calibration/slurm/submit_calibration.sh
#

# ============================================================================
# SLURM Directives
# ============================================================================

#SBATCH --job-name=fvs-calibration
#SBATCH --array=1-25                    # One job per variant
#SBATCH --ntasks=1                      # Single task per job
#SBATCH --cpus-per-task=4               # 4 cores for parallel Stan chains
#SBATCH --mem=16GB                      # 16GB RAM
#SBATCH --time=24:00:00                 # 24 hour wall time
#SBATCH --account=default                # Update with your allocation
#SBATCH --output=calibration/logs/array_%a.log
#SBATCH --error=calibration/logs/array_%a.err

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ROOT="/home/aweiskittel/Documents/Claude/fvs-modern"
CALIBRATION_DIR="${PROJECT_ROOT}/calibration"

# Array of all FVS variants (must match number of --array tasks)
VARIANTS=(
    "acd"   # 1
    "ak"    # 2
    "bc"    # 3
    "bm"    # 4
    "ca"    # 5
    "ci"    # 6
    "cr"    # 7
    "cs"    # 8
    "ec"    # 9
    "em"    # 10
    "ie"    # 11
    "kt"    # 12
    "ls"    # 13
    "nc"    # 14
    "ne"    # 15
    "oc"    # 16
    "on"    # 17
    "op"    # 18
    "pn"    # 19
    "sn"    # 20
    "so"    # 21
    "tt"    # 22
    "ut"    # 23
    "wc"    # 24
    "ws"    # 25
)

# Get variant for this array job (1-indexed to 0-indexed)
VARIANT_IDX=$((SLURM_ARRAY_TASK_ID - 1))
VARIANT="${VARIANTS[$VARIANT_IDX]}"

# ============================================================================
# Print Job Information
# ============================================================================

echo "=========================================="
echo "FVS Bayesian Calibration Pipeline"
echo "=========================================="
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task: $SLURM_ARRAY_TASK_ID / $SLURM_ARRAY_TASK_MAX"
echo "Variant: $VARIANT"
echo "Hostname: $(hostname)"
echo "Available cores: $(nproc)"
echo "=========================================="

# ============================================================================
# Module Loading (OSC Environment)
# ============================================================================

# Load R module on OSC
module load R/4.3.0

# Verify R installation
echo "R version:"
R --version | head -n 1

echo "Working directory: $(pwd)"

# ============================================================================
# Run Calibration Pipeline for This Variant
# ============================================================================

# Call the per-variant script
cd "$PROJECT_ROOT" || exit 1

bash "$CALIBRATION_DIR/slurm/run_variant.sh" "$VARIANT"

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "Variant $VARIANT completed successfully"
else
    echo "Variant $VARIANT failed with exit code $EXIT_CODE"
fi

exit $EXIT_CODE

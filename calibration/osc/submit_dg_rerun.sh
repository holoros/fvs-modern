#!/bin/bash
#SBATCH --job-name=fvs_dg_v2
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-6
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_v2_%a_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_v2_%a_%j.err

# ============================================================================
# DG Resubmission: ADVI-only for 7 variants that failed/timed out on HMC
# ============================================================================
# Key changes from submit_dg_24h.sh:
#   - Uses 02b_fit_dg_advi_only.R (skips HMC, fullrank ADVI, 15K obs)
#   - Uses wykoff_dg_v2.stan (sigma lower bounds prevent zero-scale errors)
#   - 12h wall time (plenty for MAP + ADVI at 15K obs)
#   - 64G memory (halved from 128G since data is smaller)
#   - 4 CPUs (ADVI is single-threaded; no parallel chains)
# ============================================================================

VARIANTS=(bc bm ne oc op pn so)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG ADVI-only calibration for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"
export FVS_MAX_OBS="15000"

# Load modules
module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

echo "R_LIBS_USER: $R_LIBS_USER"

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"
OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}"

# Skip if DG summary already exists (in case variant completed between submission)
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    echo "DG already complete for $VARIANT, skipping fit"
    # Still re-export JSON
    Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"
    exit 0
fi

# Check data exists
DATA_DIR="${FVS_PROJECT_ROOT}/calibration/data/processed/${VARIANT}"
if [ ! -f "${DATA_DIR}/diameter_growth.csv" ]; then
    echo "No DG data for $VARIANT, fetching first..."
    Rscript "${SCRIPTS_DIR}/01_fetch_fia_data.R" --variant "$VARIANT"
fi

# Fit diameter growth (MAP -> ADVI only, no HMC)
echo "Fitting DG model (ADVI-only v2)..."
Rscript "${SCRIPTS_DIR}/02b_fit_dg_advi_only.R" --variant "$VARIANT"
DG_EXIT=$?

if [ $DG_EXIT -ne 0 ]; then
    echo "DG fitting FAILED for $VARIANT (exit code $DG_EXIT)"
    exit $DG_EXIT
fi

# Re-run JSON export with DG posteriors included
echo "Updating JSON export..."
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo "=== DG complete for variant: $VARIANT ==="
echo "End: $(date)"
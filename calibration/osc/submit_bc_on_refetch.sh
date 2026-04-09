#!/bin/bash
#SBATCH --job-name=fvs_refetch
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-1
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/refetch_%a_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/refetch_%a_%j.err

# ============================================================================
# Re-fetch FIA data and rerun full calibration for BC and ON
# ============================================================================
# BC: now uses WA, OR, ID, MT (Pacific Northwest proxy)
# ON: now uses MN, WI, MI (Great Lakes proxy)
# Previously both used ME (placeholder), which was incorrect
# ============================================================================

VARIANTS=(bc on)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== Full recalibration for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"
export FVS_MAX_OBS="15000"

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
DATA_DIR="${FVS_PROJECT_ROOT}/calibration/data/processed/${VARIANT}"

# ---- Step 0: Remove old (incorrect) data and outputs ----
echo "Removing old placeholder data for $VARIANT..."
rm -f "${DATA_DIR}/diameter_growth.csv"
rm -f "${OUTPUT_DIR}/diameter_growth_summary.csv"
rm -f "${OUTPUT_DIR}/diameter_growth_map.csv"
rm -f "${OUTPUT_DIR}/diameter_growth_posterior.csv"
rm -f "${OUTPUT_DIR}/diameter_growth_samples.rds"
rm -f "${OUTPUT_DIR}/diameter_growth_method.txt"
rm -f "${OUTPUT_DIR}/standardization_params.rds"

# ---- Step 1: Re-fetch FIA data with corrected state mapping ----
echo "Fetching FIA data with corrected state mapping..."
Rscript "${SCRIPTS_DIR}/01_fetch_fia_data.R" --variant "$VARIANT"
FETCH_EXIT=$?

if [ $FETCH_EXIT -ne 0 ]; then
    echo "Data fetch FAILED for $VARIANT (exit code $FETCH_EXIT)"
    exit $FETCH_EXIT
fi

# Verify new data exists and is different
NEW_COUNT=$(wc -l < "${DATA_DIR}/diameter_growth.csv")
echo "New data: ${NEW_COUNT} rows"

# ---- Step 2: Fit DG model (ADVI-only for speed) ----
echo "Fitting diameter growth model..."
Rscript "${SCRIPTS_DIR}/02b_fit_dg_advi_only.R" --variant "$VARIANT"
DG_EXIT=$?

if [ $DG_EXIT -ne 0 ]; then
    echo "DG fitting FAILED for $VARIANT (exit code $DG_EXIT)"
    exit $DG_EXIT
fi

# ---- Step 3: Export JSON configs ----
echo "Updating JSON export..."
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo "=== Full recalibration complete for variant: $VARIANT ==="
echo "End: $(date)"

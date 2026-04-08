#!/bin/bash
#SBATCH --job-name=fvs_dg
#SBATCH --account=PUOM0008
#SBATCH --time=24:00:00
#SBATCH --mem=128G
#SBATCH --cpus-per-task=8
#SBATCH --array=0-17
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_%a_%j.err

# Remaining 17 variants that need DG calibration (TT already complete)
VARIANTS=(ak bc bm ci cr ec em ie ne oc op pn sn so tt ut wc ws)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG calibration for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"
export FVS_MAX_OBS="30000"

# Load modules (matching config_osc.sh)
module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

# Set R library path dynamically (same approach as submit_cardinal.sh)
export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

echo "R_LIBS_USER: $R_LIBS_USER"

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"
OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}"

# Skip if DG summary already exists (TT is complete)
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

# Fit diameter growth (MAP -> HMC -> ADVI fallback)
echo "Fitting DG model..."
Rscript "${SCRIPTS_DIR}/02_fit_diameter_growth.R" --variant "$VARIANT"
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

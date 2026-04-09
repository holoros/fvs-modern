#!/bin/bash
#SBATCH --job-name=fvs_mort_v2
#SBATCH --account=PUOM0008
#SBATCH --time=08:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-8
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/mort_v2_%a_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/mort_v2_%a_%j.err

# ============================================================================
# Mortality v2 refit for 9 variants with AUC < 0.70
# Adds: random slopes (DBH+BAL by species), DBH:BAL interaction, ELEV, SLOPE
# ============================================================================

VARIANTS=(ca ie nc oc op sn so tt ws)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== Mortality v2 for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

Rscript "${SCRIPTS_DIR}/04b_fit_mortality_v2.R" --variant "$VARIANT"
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"

# Show AUC result
if [ -f "${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}/mortality_v2_auc.csv" ]; then
    echo "=== AUC Result ==="
    cat "${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}/mortality_v2_auc.csv"
fi

echo ""
echo "=== Mortality v2 complete for: $VARIANT ==="
echo "End: $(date)"

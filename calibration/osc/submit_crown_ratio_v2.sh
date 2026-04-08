#!/bin/bash
#SBATCH --job-name=fvs_cr_v2
#SBATCH --account=PUOM0008
#SBATCH --time=06:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-24
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/cr_v2_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/cr_v2_%a_%j.err

# ============================================================================
# Crown Ratio v2: beta regression on CR level for ALL 25 variants
# Predicts CR_t2 instead of CR_change; uses Beta family
# ============================================================================

VARIANTS=(acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== Crown Ratio v2 for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

Rscript "${SCRIPTS_DIR}/05b_fit_crown_ratio_v2.R" --variant "$VARIANT"
EXIT_CODE=$?
echo "Exit code: $EXIT_CODE"

# Show evaluation result
if [ -f "${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}/crown_ratio_v2_eval.csv" ]; then
    echo "=== CR v2 Evaluation ==="
    cat "${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}/crown_ratio_v2_eval.csv"
fi

echo ""
echo "=== Crown Ratio v2 complete for: $VARIANT ==="
echo "End: $(date)"

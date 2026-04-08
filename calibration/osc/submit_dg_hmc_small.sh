#!/bin/bash
#SBATCH --job-name=fvs_dg_hmc
#SBATCH --account=PUOM0008
#SBATCH --time=08:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-13
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_hmc_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_hmc_%a_%j.err

# ============================================================================
# DG refit for 14 variants where ADVI diverged (sigma >> 1)
# Uses 02c_fit_dg_hmc_small.R: HMC on 5K obs + constrained ADVI fallback
# ============================================================================

VARIANTS=(ak bm cr ec em ie nc ne op sn so ut wc ws)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG HMC-small refit for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"
export FVS_MAX_OBS="5000"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

echo "R_LIBS_USER: $R_LIBS_USER"
echo "FVS_MAX_OBS: $FVS_MAX_OBS"

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"
OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}"

# Back up existing DG output before overwriting
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    cp "${OUTPUT_DIR}/diameter_growth_summary.csv" "${OUTPUT_DIR}/diameter_growth_summary.csv.advi_bak"
    echo "Backed up existing DG summary"
fi

# Run the HMC-small DG fit
Rscript "${SCRIPTS_DIR}/02c_fit_dg_hmc_small.R" --variant "$VARIANT"
DG_EXIT=$?
echo "DG exit code: $DG_EXIT"

# Check result
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    SIGMA=$(tail -1 "${OUTPUT_DIR}/diameter_growth_summary.csv" | grep -oP 'sigma[^,]*,[^,]*,\K[^,]+' 2>/dev/null || echo "unknown")
    echo "DG complete. Sigma: $SIGMA"
else
    echo "WARNING: DG summary not produced"
fi

# Re-export posteriors to JSON
echo "=== Exporting posteriors to JSON ==="
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo ""
echo "=== DG HMC-small refit complete for variant: $VARIANT ==="
echo "End: $(date)"

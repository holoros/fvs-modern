#!/bin/bash
#SBATCH --job-name=fvs_dg_mbias
#SBATCH --account=PUOM0008
#SBATCH --time=10:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-6
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_mbias_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_mbias_%a_%j.err

# ============================================================================
# DG refit for MARGINAL variants with notable prediction bias (10K obs)
# AK:  R2=0.403, bias=-0.53
# CR:  R2=0.414, bias=+0.23
# EC:  R2=0.548, bias=+0.30
# EM:  R2=0.320, bias=-0.39
# IE:  R2=0.433, bias=+0.50
# UT:  R2=0.397, bias=+0.25
# WC:  R2=0.533, bias=+0.40
# Increasing from 5K to 10K obs to improve representativeness
# ============================================================================

VARIANTS=(ak cr ec em ie ut wc)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG refit (marginal bias, 10K obs) for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"
export FVS_MAX_OBS="10000"

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

# Back up existing DG output (from 5K run)
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    cp "${OUTPUT_DIR}/diameter_growth_summary.csv" "${OUTPUT_DIR}/diameter_growth_summary.csv.5k_bak"
    echo "Backed up existing DG summary (5K run)"
fi
if [ -f "${OUTPUT_DIR}/diameter_growth_samples.rds" ]; then
    cp "${OUTPUT_DIR}/diameter_growth_samples.rds" "${OUTPUT_DIR}/diameter_growth_samples.rds.5k_bak"
fi

# Run HMC-small DG fit with 10K observations
Rscript "${SCRIPTS_DIR}/02c_fit_dg_hmc_small.R" --variant "$VARIANT"
DG_EXIT=$?
echo "DG exit code: $DG_EXIT"

# Check result
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    echo "=== DG Summary ==="
    head -5 "${OUTPUT_DIR}/diameter_growth_summary.csv"
    echo "..."
    grep "^sigma" "${OUTPUT_DIR}/diameter_growth_summary.csv" || true
fi

# Re-export posteriors to JSON
echo "=== Exporting posteriors to JSON ==="
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo ""
echo "=== DG refit (marginal bias, 10K) complete for variant: $VARIANT ==="
echo "End: $(date)"

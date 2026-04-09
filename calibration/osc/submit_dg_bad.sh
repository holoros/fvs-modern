#!/bin/bash
#SBATCH --job-name=fvs_dg_bad
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-2
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_bad_%a_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_bad_%a_%j.err

# ============================================================================
# DG refit for BAD variants (R2 < 0.30) with 15K observations
# BM: R2=0.204, bias=+0.72
# OP: R2=0.137, bias=-0.80
# WS: R2=0.236, bias=+0.76
# Previous 5K subsample was insufficient for these variants
# ============================================================================

VARIANTS=(bm op ws)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG refit (bad, 15K obs) for variant: $VARIANT ==="
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

# Run HMC-small DG fit with 15K observations
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
echo "=== DG refit (bad, 15K) complete for variant: $VARIANT ==="
echo "End: $(date)"

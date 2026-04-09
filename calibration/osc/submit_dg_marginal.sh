#!/bin/bash
#SBATCH --job-name=fvs_dg_marg
#SBATCH --account=PUOM0008
#SBATCH --time=08:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --array=0
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_marginal_%a_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/dg_marginal_%a_%j.err

# ============================================================================
# DG refit for marginal variants (ADVI converged but predictions biased)
# CA: sigma=0.82, R2=0.56, equiv=FALSE, bias=+0.39
# Add more variants here after reviewing HMC batch results
# ============================================================================

VARIANTS=(ca)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG HMC refit (marginal) for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"
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

# Back up existing DG output
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    cp "${OUTPUT_DIR}/diameter_growth_summary.csv" "${OUTPUT_DIR}/diameter_growth_summary.csv.advi_bak"
    echo "Backed up existing DG summary"
fi

# Run HMC-small DG fit
Rscript "${SCRIPTS_DIR}/02c_fit_dg_hmc_small.R" --variant "$VARIANT"
DG_EXIT=$?
echo "DG exit code: $DG_EXIT"

# Re-export posteriors
echo "=== Exporting posteriors to JSON ==="
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo ""
echo "=== DG marginal refit complete for variant: $VARIANT ==="
echo "End: $(date)"

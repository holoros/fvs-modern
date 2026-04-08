#!/bin/bash
#SBATCH --job-name=fvs_dg_miss
#SBATCH --account=PUOM0008
#SBATCH --time=08:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-4
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_miss_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/dg_miss_%a_%j.err

# ============================================================================
# DG fit for 5 variants missing diameter_growth_samples.rds
# These were fit with 02b (ADVI-only) which did not save the samples RDS
# needed by the comparison script (11_full_comparison.R)
# Rerun with 02c to produce proper samples files and enable DG evaluation
# ============================================================================

VARIANTS=(bc ci oc on pn)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== DG fit (missing samples) for variant: $VARIANT ==="
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

# Back up existing DG output
if [ -f "${OUTPUT_DIR}/diameter_growth_summary.csv" ]; then
    cp "${OUTPUT_DIR}/diameter_growth_summary.csv" "${OUTPUT_DIR}/diameter_growth_summary.csv.02b_bak"
    echo "Backed up existing DG summary (02b run)"
fi

# Run 02c to produce proper samples RDS + summary
Rscript "${SCRIPTS_DIR}/02c_fit_dg_hmc_small.R" --variant "$VARIANT"
DG_EXIT=$?
echo "DG exit code: $DG_EXIT"

# Verify samples.rds was created
if [ -f "${OUTPUT_DIR}/diameter_growth_samples.rds" ]; then
    echo "SUCCESS: diameter_growth_samples.rds created"
    ls -la "${OUTPUT_DIR}/diameter_growth_samples.rds"
else
    echo "WARNING: diameter_growth_samples.rds NOT created"
fi

# Check summary
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
echo "=== DG fit (missing samples) complete for variant: $VARIANT ==="
echo "End: $(date)"

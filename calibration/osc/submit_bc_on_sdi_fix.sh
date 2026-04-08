#!/bin/bash
#SBATCH --job-name=fvs_sdi_fix
#SBATCH --account=PUOM0008
#SBATCH --time=02:00:00
#SBATCH --mem=128G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-1
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/sdi_fix_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/sdi_fix_%a_%j.err

# ============================================================================
# Rerun stand density (SDIMAX) for BC and ON with corrected proxy states
# BC: WA, OR, ID, MT  (was ME)
# ON: MN, WI, MI      (was ME)
# The 08_fetch_stand_data.R state mapping has been updated.
# ============================================================================

VARIANTS=(bc on)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== Stand density refit for variant: $VARIANT ==="
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
OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/variants/${VARIANT}"

# Back up old (Maine-based) stand density outputs
echo "Backing up old stand density outputs..."
for f in stand_density_overall.csv stand_density_samples.rds species_sdimax_calibrated.csv species_qr_sdimax.csv self_thinning_slopes.csv sdmd_calibrated.pdf self_thinning_boundary.pdf species_selfthinning_slopes.pdf; do
    if [ -f "${OUTPUT_DIR}/$f" ]; then
        cp "${OUTPUT_DIR}/$f" "${OUTPUT_DIR}/${f}.me_bak"
    fi
done

# Re-fetch stand data with corrected state mapping
echo "=== Fetching stand data ==="
Rscript "${SCRIPTS_DIR}/08_fetch_stand_data.R" --variant "$VARIANT"
FETCH_EXIT=$?
echo "Fetch exit code: $FETCH_EXIT"

# Refit stand density model
echo "=== Fitting stand density model ==="
Rscript "${SCRIPTS_DIR}/09_fit_stand_density.R" --variant "$VARIANT"
FIT_EXIT=$?
echo "Fit exit code: $FIT_EXIT"

# Re-export posteriors
echo "=== Exporting posteriors to JSON ==="
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo ""
echo "=== Stand density refit complete for variant: $VARIANT ==="
echo "End: $(date)"

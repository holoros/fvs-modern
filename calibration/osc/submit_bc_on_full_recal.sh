#!/bin/bash
#SBATCH --job-name=fvs_full_recal
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --array=0-1
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/full_recal_%a_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/full_recal_%a_%j.err

# ============================================================================
# Full recalibration of ALL components for BC and ON
# ============================================================================
# Previous run used Maine FIA data (placeholder). Now using:
#   BC: WA, OR, ID, MT (Pacific Northwest proxy)
#   ON: MN, WI, MI (Great Lakes proxy)
# Data was already re-fetched; this reruns all model components.
# ============================================================================

VARIANTS=(bc on)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

if [ -z "$VARIANT" ]; then
    echo "Invalid array index: $SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "=== Full component recalibration for variant: $VARIANT ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"
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

# ---- Step 0: Clear old (Maine-based) component outputs ----
echo "Clearing old component outputs..."
rm -f "${OUTPUT_DIR}/height_diameter_summary.csv"
rm -f "${OUTPUT_DIR}/height_diameter_posterior.csv"
rm -f "${OUTPUT_DIR}/height_diameter_samples.rds"
rm -f "${OUTPUT_DIR}/mortality_summary.csv"
rm -f "${OUTPUT_DIR}/mortality_posterior.csv"
rm -f "${OUTPUT_DIR}/mortality_samples.rds"
rm -f "${OUTPUT_DIR}/crown_ratio_summary.csv"
rm -f "${OUTPUT_DIR}/crown_ratio_posterior.csv"
rm -f "${OUTPUT_DIR}/crown_ratio_samples.rds"
rm -f "${OUTPUT_DIR}/stand_density_summary.csv"
rm -f "${OUTPUT_DIR}/height_increment_summary.csv"
rm -f "${OUTPUT_DIR}/height_increment_posterior.csv"

# Keep DG (already recalibrated with correct data)
echo "Keeping existing DG calibration (already uses correct data)"

# ---- Step 1: Height-Diameter (Chapman-Richards) ----
echo "=== Fitting Height-Diameter model ==="
Rscript "${SCRIPTS_DIR}/03_fit_height_diameter.R" --variant "$VARIANT"
HD_EXIT=$?
echo "H-D exit code: $HD_EXIT"

# ---- Step 2: Mortality ----
echo "=== Fitting Mortality model ==="
Rscript "${SCRIPTS_DIR}/04_fit_mortality.R" --variant "$VARIANT"
MORT_EXIT=$?
echo "Mortality exit code: $MORT_EXIT"

# ---- Step 3: Crown Ratio ----
echo "=== Fitting Crown Ratio model ==="
Rscript "${SCRIPTS_DIR}/05_fit_crown_ratio.R" --variant "$VARIANT"
CR_EXIT=$?
echo "Crown ratio exit code: $CR_EXIT"

# ---- Step 4: Stand Density (SDIMAX) ----
echo "=== Fitting Stand Density model ==="
Rscript "${SCRIPTS_DIR}/08_fetch_stand_data.R" --variant "$VARIANT" 2>/dev/null
Rscript "${SCRIPTS_DIR}/09_fit_stand_density.R" --variant "$VARIANT"
SDI_EXIT=$?
echo "Stand density exit code: $SDI_EXIT"

# ---- Step 5: Height Increment (if script exists and data available) ----
echo "=== Fitting Height Increment model ==="
if [ -f "${SCRIPTS_DIR}/03b_fit_height_increment.R" ]; then
    Rscript "${SCRIPTS_DIR}/03b_fit_height_increment.R" --variant "$VARIANT" 2>/dev/null
    HI_EXIT=$?
    echo "Height increment exit code: $HI_EXIT"
else
    echo "Height increment script not found, skipping"
fi

# ---- Step 6: Export all posteriors to JSON ----
echo "=== Exporting posteriors to JSON ==="
Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" --variant "$VARIANT"

echo ""
echo "=== Component status summary ==="
for comp in height_diameter mortality crown_ratio stand_density diameter_growth height_increment; do
    if [ -f "${OUTPUT_DIR}/${comp}_summary.csv" ]; then
        echo "  DONE: $comp"
    else
        echo "  MISSING: $comp"
    fi
done

echo ""
echo "=== Full recalibration complete for variant: $VARIANT ==="
echo "End: $(date)"

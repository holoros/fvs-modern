#!/bin/bash
#SBATCH --job-name=perseus_100yr
#SBATCH --account=puom0008
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --array=1-37
#SBATCH --output=calibration/logs/perseus_%A_%a.out
#SBATCH --error=calibration/logs/perseus_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu

# ==============================================================================
# PERSEUS 100-Year FVS Projection: SLURM Array Job
#
# Splits 3,632 Maine plots across 37 array tasks (100 plots each, last batch
# gets the remainder). Runs FVS-ACD and FVS-NE, default and calibrated, for
# each plot over 100 years with 10-year cycles.
#
# Submit from fvs-modern project root:
#   sbatch calibration/slurm/submit_perseus.sh
# ==============================================================================

echo "========================================"
echo "PERSEUS 100-Year FVS Projection"
echo "Job ID:     ${SLURM_JOB_ID}"
echo "Array Task: ${SLURM_ARRAY_TASK_ID}"
echo "Node:       $(hostname)"
echo "Start:      $(date)"
echo "========================================"

# Environment
export FVS_PROJECT_ROOT="${HOME}/fvs-modern"
export FVS_FIA_DATA_DIR="${HOME}/fia_data"
export FVS_LIB_DIR="${FVS_PROJECT_ROOT}/lib"
export NSBE_ROOT="${FVS_PROJECT_ROOT}/data/NSBE"
export FVS_CONFIG_DIR="${FVS_PROJECT_ROOT}/config"
export PERSEUS_CSV="${FVS_PROJECT_ROOT}/calibration/data/perseus_plots.csv"
export PERSEUS_OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/perseus"

# Add project to PYTHONPATH for config imports
export PYTHONPATH="${FVS_PROJECT_ROOT}:${FVS_PROJECT_ROOT}/deployment/fvs2py:${PYTHONPATH:-}"

# Load modules
module load python/3.12
module load gcc/12.3.0  # For FVS shared library compatibility

# Ensure FVS shared libraries exist
for variant in ne acd; do
    if [ ! -f "${FVS_LIB_DIR}/FVS${variant}.so" ]; then
        echo "WARNING: FVS-${variant} library not found at ${FVS_LIB_DIR}"
        echo "Building FVS-${variant} shared library..."
        mkdir -p "${FVS_LIB_DIR}"
        FC=gfortran CC=gcc CXX=g++ \
        bash "${FVS_PROJECT_ROOT}/deployment/scripts/build_fvs_libraries.sh" \
            "${FVS_PROJECT_ROOT}/src-converted" \
            "${FVS_LIB_DIR}" \
            "${variant}"
    fi
done

# Create output and log directories
mkdir -p "${PERSEUS_OUTPUT_DIR}"
mkdir -p "${FVS_PROJECT_ROOT}/calibration/logs"

# Run the projection for this batch
BATCH_SIZE=100

python3 "${FVS_PROJECT_ROOT}/calibration/python/perseus_100yr_projection.py" \
    --batch-id "${SLURM_ARRAY_TASK_ID}" \
    --batch-size "${BATCH_SIZE}" \
    --perseus-csv "${PERSEUS_CSV}" \
    --variants acd ne \
    --configs default calibrated \
    --output-dir "${PERSEUS_OUTPUT_DIR}"

EXIT_CODE=$?

echo "========================================"
echo "Finish:     $(date)"
echo "Exit code:  ${EXIT_CODE}"
echo "========================================"

exit ${EXIT_CODE}

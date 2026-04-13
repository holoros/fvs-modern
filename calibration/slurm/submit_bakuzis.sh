#!/bin/bash
#SBATCH --job-name=bakuzis_100yr
#SBATCH --account=puom0008
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --output=calibration/logs/bakuzis_%j.out
#SBATCH --error=calibration/logs/bakuzis_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu

# ==============================================================================
# Bakuzis Matrix 100-Year FVS Comparison: Default vs Calibrated
#
# 36 scenarios (4 species groups x 3 site classes x 3 density classes)
# Run for both NE and ACD variants, 20 cycles x 5 years = 100 years
#
# Submit from fvs-modern project root:
#   sbatch calibration/slurm/submit_bakuzis.sh
# ==============================================================================

echo "========================================"
echo "Bakuzis Matrix 100-Year Comparison"
echo "Job ID:  ${SLURM_JOB_ID}"
echo "Node:    $(hostname)"
echo "Start:   $(date)"
echo "========================================"

export FVS_PROJECT_ROOT="${HOME}/fvs-modern"
export FVS_LIB_DIR="${FVS_PROJECT_ROOT}/lib"
export FVS_CONFIG_DIR="${FVS_PROJECT_ROOT}/config"
export BAKUZIS_OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output/bakuzis"
export PYTHONPATH="${FVS_PROJECT_ROOT}:${FVS_PROJECT_ROOT}/deployment/fvs2py:${PYTHONPATH:-}"

module load python/3.12
module load gcc/12.3.0

# Ensure FVS shared libraries exist
if [ ! -f "${FVS_LIB_DIR}/FVSne.so" ]; then
    echo "Building FVS-NE shared library..."
    mkdir -p "${FVS_LIB_DIR}"
    bash "${FVS_PROJECT_ROOT}/deployment/scripts/build_fvs_libraries.sh" \
        "${FVS_PROJECT_ROOT}/src-converted" \
        "${FVS_LIB_DIR}" \
        ne
fi

mkdir -p "${BAKUZIS_OUTPUT_DIR}"

python3 "${FVS_PROJECT_ROOT}/calibration/python/bakuzis_100yr_comparison.py" \
    --variant ne \
    --seed 42 \
    --output-dir "${BAKUZIS_OUTPUT_DIR}"

echo "========================================"
echo "Finish:  $(date)"
echo "Exit:    $?"
echo "========================================"

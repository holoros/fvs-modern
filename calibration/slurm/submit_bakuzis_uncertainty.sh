#!/bin/bash
#SBATCH --job-name=bakuzis_unc
#SBATCH --account=puom0008
#SBATCH --time=08:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G
#SBATCH --array=0-5
#SBATCH --output=calibration/logs/bakuzis_unc_%A_%a.out
#SBATCH --error=calibration/logs/bakuzis_unc_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu

# ==============================================================================
# Bakuzis Matrix with Parametric Uncertainty: Default vs Calibrated
#
# 36 scenarios x (1 default + 1 calibrated MAP + N posterior draws) per
# variant. With N=50 posterior draws the per scenario cost is 52 FVS
# runs, so 36 scenarios x 52 runs x 2 variants = 3744 FVS invocations.
#
# The SLURM array spreads the 36 scenarios across 6 tasks of 6
# scenarios each (batch_size=6). Each task handles both NE and ACD.
#
# Submit from fvs-modern project root:
#   sbatch calibration/slurm/submit_bakuzis_uncertainty.sh
#
# To change N draws, edit N_DRAWS below or override via sbatch --export:
#   sbatch --export=N_DRAWS=100 calibration/slurm/submit_bakuzis_uncertainty.sh
# ==============================================================================

echo "========================================"
echo "Bakuzis Matrix Uncertainty Comparison"
echo "Job ID:         ${SLURM_JOB_ID}"
echo "Array task:     ${SLURM_ARRAY_TASK_ID}"
echo "Node:           $(hostname)"
echo "Start:          $(date)"
echo "========================================"

# Account code and project root are set via environment. Adjust the
# --account SBATCH directive above if your OSC project differs.
export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-${HOME}/fvs-modern}"
export FVS_LIB_DIR="${FVS_LIB_DIR:-${FVS_PROJECT_ROOT}/lib}"
export FVS_CONFIG_DIR="${FVS_CONFIG_DIR:-${FVS_PROJECT_ROOT}/config}"
export BAKUZIS_OUTPUT_DIR="${BAKUZIS_OUTPUT_DIR:-${FVS_PROJECT_ROOT}/calibration/output/bakuzis}"
export PYTHONPATH="${FVS_PROJECT_ROOT}:${FVS_PROJECT_ROOT}/deployment/fvs2py:${PYTHONPATH:-}"

# Configurable via sbatch --export
N_DRAWS="${N_DRAWS:-50}"
BATCH_SIZE="${BATCH_SIZE:-6}"
SEED="${SEED:-42}"

module load python/3.12
module load gcc/12.3.0

# Sanity check: shared libraries present
missing=0
for v in ne acd; do
    if [ ! -f "${FVS_LIB_DIR}/FVS${v}.so" ]; then
        echo "ERROR: Missing ${FVS_LIB_DIR}/FVS${v}.so"
        missing=1
    fi
done
if [ "${missing}" -eq 1 ]; then
    echo "Build the libraries first:"
    echo "  bash deployment/scripts/build_fvs_libraries.sh src-converted ${FVS_LIB_DIR}"
    exit 1
fi

mkdir -p "${BAKUZIS_OUTPUT_DIR}"

cd "${FVS_PROJECT_ROOT}"

# Run NE and ACD for this batch
for variant in ne acd; do
    echo ""
    echo ">>> Variant: ${variant} (batch ${SLURM_ARRAY_TASK_ID}) <<<"
    python3 calibration/python/bakuzis_uncertainty_comparison.py \
        --variant "${variant}" \
        --n-draws "${N_DRAWS}" \
        --seed "${SEED}" \
        --batch-id "${SLURM_ARRAY_TASK_ID:-0}" \
        --batch-size "${BATCH_SIZE}" \
        --output-dir "${BAKUZIS_OUTPUT_DIR}"
done

echo ""
echo "========================================"
echo "End: $(date)"
echo "========================================"

# After all array tasks finish, combine and aggregate with:
#   python3 calibration/python/bakuzis_uncertainty_aggregate.py \
#       --input-dir "${BAKUZIS_OUTPUT_DIR}"
#   Rscript calibration/R/17_bakuzis_uncertainty_figures.R

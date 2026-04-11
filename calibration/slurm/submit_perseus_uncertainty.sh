#!/bin/bash
# =============================================================================
# SLURM Submission: PERSEUS 100yr Projections with Uncertainty (1999-2004)
# =============================================================================
#
# Runs FVS-ACD and FVS-NE projections with Bayesian uncertainty propagation
# for Maine FIA plots with initial measurements in 1999-2004.
#
# Each batch processes 100 plots with:
#   - Default and calibrated point estimates (4 runs per plot)
#   - 50 posterior draws per variant (100 additional runs per plot)
#
# Total: ~104 FVS runs per plot x 3586 plots = ~373K runs
#
# Usage:
#   bash submit_perseus_uncertainty.sh
#
# Author: A. Weiskittel
# Date: 2026-04-11
# =============================================================================

set -euo pipefail

# --- Configuration -----------------------------------------------------------
N_DRAWS=50
SEED=42
START_YEAR=1999
END_YEAR=2004
BATCH_SIZE=100
N_PLOTS=3586
N_BATCHES=$(( (N_PLOTS + BATCH_SIZE - 1) / BATCH_SIZE ))

PROJECT_DIR="/users/PUOM0008/crsfaaron/fvs-modern"
OUTPUT_DIR="${PROJECT_DIR}/calibration/output/perseus/uncertainty_1999_2004"
LOG_DIR="${PROJECT_DIR}/calibration/logs/perseus_uncertainty"

PERSEUS_CSV="${PROJECT_DIR}/calibration/data/perseus_plots.csv"
EXPNS_CSV="${PROJECT_DIR}/calibration/output/perseus/untreated_expns.csv"

# --- Create directories ------------------------------------------------------
mkdir -p "${OUTPUT_DIR}" "${LOG_DIR}"

echo "PERSEUS Uncertainty Projection Pipeline"
echo "======================================="
echo "Year range:   ${START_YEAR}-${END_YEAR}"
echo "N draws:      ${N_DRAWS}"
echo "Batch size:   ${BATCH_SIZE}"
echo "N batches:    ${N_BATCHES}"
echo "Output:       ${OUTPUT_DIR}"
echo ""

# --- Submit array job --------------------------------------------------------
JOB_ID=$(sbatch --parsable <<SBATCH
#!/bin/bash
#SBATCH --job-name=perseus_unc
#SBATCH --account=PUOM0008
#SBATCH --partition=serial
#SBATCH --array=1-${N_BATCHES}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=06:00:00
#SBATCH --output=${LOG_DIR}/batch_%a.out
#SBATCH --error=${LOG_DIR}/batch_%a.err

# Load environment
module load python/3.11 gnu/13.2
source "\${PROJECT_DIR}/calibration/osc/fvs_env/bin/activate"

export FVS_PROJECT_ROOT="${PROJECT_DIR}"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/fia_data"
export FVS_LIB_DIR="${PROJECT_DIR}/lib"
export NSBE_ROOT="${PROJECT_DIR}/data/NSBE"
export FVS_CONFIG_DIR="${PROJECT_DIR}/config"
export PERSEUS_OUTPUT_DIR="${OUTPUT_DIR}"
export PERSEUS_CSV="${PERSEUS_CSV}"

echo "Batch \${SLURM_ARRAY_TASK_ID} starting at \$(date)"

python "${PROJECT_DIR}/calibration/python/perseus_uncertainty_projection.py" \\
    --batch-id \${SLURM_ARRAY_TASK_ID} \\
    --batch-size ${BATCH_SIZE} \\
    --n-draws ${N_DRAWS} \\
    --seed ${SEED} \\
    --start-year ${START_YEAR} \\
    --end-year ${END_YEAR} \\
    --expansion-csv "${EXPNS_CSV}" \\
    --output-dir "${OUTPUT_DIR}" \\
    --perseus-csv "${PERSEUS_CSV}"

echo "Batch \${SLURM_ARRAY_TASK_ID} finished at \$(date)"
SBATCH
)

echo "Submitted array job: ${JOB_ID}"
echo "Monitor with: squeue -j ${JOB_ID}"

# --- Submit aggregation job (runs after all batches complete) ----------------
AGG_ID=$(sbatch --parsable --dependency=afterok:${JOB_ID} <<SBATCH
#!/bin/bash
#SBATCH --job-name=perseus_unc_agg
#SBATCH --account=PUOM0008
#SBATCH --partition=serial
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=${LOG_DIR}/aggregate.out
#SBATCH --error=${LOG_DIR}/aggregate.err

module load python/3.11 gnu/13.2
source "\${PROJECT_DIR}/calibration/osc/fvs_env/bin/activate"

export FVS_PROJECT_ROOT="${PROJECT_DIR}"

echo "Aggregation starting at \$(date)"

python "${PROJECT_DIR}/calibration/python/perseus_uncertainty_aggregate.py" \\
    --output-dir "${OUTPUT_DIR}" \\
    --expansion-csv "${EXPNS_CSV}" \\
    --perseus-csv "${PERSEUS_CSV}"

echo "Aggregation finished at \$(date)"
SBATCH
)

echo "Submitted aggregation job: ${AGG_ID} (depends on ${JOB_ID})"
echo ""
echo "Pipeline submitted. Expected runtime: 4-6 hours."

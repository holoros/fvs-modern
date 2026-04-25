#!/bin/bash
# =============================================================================
# SLURM Submission: PERSEUS Climate Scenario Projections
# =============================================================================
#
# Runs FVS-ACD and FVS-NE projections with climate-modified site index
# and Bayesian uncertainty propagation for Maine FIA plots (1999-2004).
#
# Climate scenarios:
#   - RCP 4.5 (HadGEM2-ES deltas, ~SSP245): moderate warming
#   - RCP 8.5 (HadGEM2-ES deltas, ~SSP585): high warming
#
# SI modification: Per-plot time-weighted average SI ratio from the
# ClimateNA + ranger RF delta method projection.
#
# Each batch processes 100 plots with:
#   - Calibrated point estimate with climate SI (2 runs per plot, ACD+NE)
#   - 50 posterior draws per variant with climate SI (100 runs per plot)
#
# Total: ~102 FVS runs per plot x 3586 plots x 2 RCPs = ~732K runs
#
# Usage:
#   bash submit_perseus_climate.sh           # submit both RCPs
#   bash submit_perseus_climate.sh --rcp 4.5 # submit RCP 4.5 only
#   bash submit_perseus_climate.sh --rcp 8.5 # submit RCP 8.5 only
#
# Author: A. Weiskittel
# Date: 2026-04-17
# =============================================================================

set -euo pipefail

# --- Parse arguments ---------------------------------------------------------
RCP_FILTER=""
for arg in "$@"; do
  case $arg in
    --rcp) shift; RCP_FILTER="$1"; shift ;;
    --rcp=*) RCP_FILTER="${arg#*=}" ;;
  esac
done

# --- HPC Configuration ------------------------------------------------------
# All of these accept env var overrides. Defaults assume a workstation layout
# rooted at $HOME. Set the OSC specific paths via sbatch --export or before
# running: FVS_PROJECT_ROOT, SLURM_ACCOUNT, FVS_FIA_DATA_DIR, SI_RATIO_CSV.
FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-${HOME}/fvs-modern}"
SLURM_ACCOUNT="${SLURM_ACCOUNT:-puom0008}"
FVS_FIA_DATA_DIR="${FVS_FIA_DATA_DIR:-${HOME}/fia_data}"
SI_RATIO_CSV="${SI_RATIO_CSV:-${HOME}/SiteIndex/future_SI/perseus_si_ratios.csv}"

# --- Projection Configuration ------------------------------------------------
N_DRAWS=50
SEED=42
START_YEAR=1999
END_YEAR=2004
BATCH_SIZE=100
N_PLOTS=3586
N_BATCHES=$(( (N_PLOTS + BATCH_SIZE - 1) / BATCH_SIZE ))

PROJECT_DIR="${FVS_PROJECT_ROOT}"
BASE_OUTPUT_DIR="${PROJECT_DIR}/calibration/output/perseus"
BASE_LOG_DIR="${PROJECT_DIR}/calibration/logs/perseus_climate"

PERSEUS_CSV="${PROJECT_DIR}/calibration/data/perseus_plots.csv"
# Expansion CSV is optional; aggregation falls back to approximate expansion
EXPNS_CSV="${PROJECT_DIR}/calibration/output/perseus/untreated_expns.csv"
if [ ! -f "${EXPNS_CSV}" ]; then
    echo "NOTE: Expansion CSV not found. Will use approximate expansion factors."
    EXPNS_CSV=""
fi

# --- Verify prerequisites ----------------------------------------------------
echo "PERSEUS Climate Scenario Projection Pipeline"
echo "============================================="
echo ""

if [ ! -f "${SI_RATIO_CSV}" ]; then
    echo "ERROR: SI ratio file not found: ${SI_RATIO_CSV}"
    echo "  Run project_future_si_delta.R first to generate per-plot SI ratios."
    exit 1
fi

if [ ! -f "${PERSEUS_CSV}" ]; then
    echo "ERROR: PERSEUS plots CSV not found: ${PERSEUS_CSV}"
    exit 1
fi

if [ ! -f "${PROJECT_DIR}/calibration/python/perseus_climate_projection.py" ]; then
    echo "ERROR: Climate projection script not found."
    echo "  Expected: ${PROJECT_DIR}/calibration/python/perseus_climate_projection.py"
    exit 1
fi

echo "SI ratio file: ${SI_RATIO_CSV}"
echo "PERSEUS CSV:   ${PERSEUS_CSV}"
echo "Year range:    ${START_YEAR}-${END_YEAR}"
echo "N draws:       ${N_DRAWS}"
echo "Batch size:    ${BATCH_SIZE}"
echo "N batches:     ${N_BATCHES}"
echo ""

# --- Determine which RCPs to run ---------------------------------------------
if [ -n "${RCP_FILTER}" ]; then
    RCPS=("${RCP_FILTER}")
else
    RCPS=("4.5" "8.5")
fi

# Variant selection: NE has a working executable, ACD only has .so (fvs2py)
# ACD through fvs2py + run_with_timeout(fork) hangs on Cardinal
# Run NE only for now; add ACD once executable is built
VARIANTS="ne"

echo "Climate scenarios: ${RCPS[*]}"
echo ""

# --- Submit jobs for each RCP ------------------------------------------------
ALL_JOB_IDS=()
ALL_AGG_IDS=()

for RCP in "${RCPS[@]}"; do
    RCP_TAG="rcp${RCP//.}"  # "rcp45" or "rcp85"

    OUTPUT_DIR="${BASE_OUTPUT_DIR}/climate_${RCP_TAG}"
    LOG_DIR="${BASE_LOG_DIR}/${RCP_TAG}"

    mkdir -p "${OUTPUT_DIR}" "${LOG_DIR}"

    echo "=== Submitting RCP ${RCP} ==="
    echo "  Output: ${OUTPUT_DIR}"
    echo "  Logs:   ${LOG_DIR}"

    # --- Array job: process batches ---
    JOB_ID=$(sbatch --parsable <<SBATCH
#!/bin/bash
#SBATCH --job-name=per_${RCP_TAG}
#SBATCH --account=${SLURM_ACCOUNT}
#SBATCH --partition=cpu
#SBATCH --array=1-${N_BATCHES}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=06:00:00
#SBATCH --output=${LOG_DIR}/batch_%a.out
#SBATCH --error=${LOG_DIR}/batch_%a.err

# Load environment
module purge
module load python/3.12

export FVS_PROJECT_ROOT="${PROJECT_DIR}"
export FVS_FIA_DATA_DIR="${FVS_FIA_DATA_DIR}"
export FVS_LIB_DIR="${PROJECT_DIR}/lib"
export NSBE_ROOT="${PROJECT_DIR}/data/NSBE"
export FVS_CONFIG_DIR="${PROJECT_DIR}/config"
export PERSEUS_OUTPUT_DIR="${BASE_OUTPUT_DIR}"
export PERSEUS_CSV="${PERSEUS_CSV}"
export SI_RATIO_CSV="${SI_RATIO_CSV}"

echo "=== PERSEUS Climate RCP ${RCP} Batch \${SLURM_ARRAY_TASK_ID} ==="
echo "Start: \$(date)"
echo "Node:  \$(hostname)"
echo ""

EXPNS_ARG=""
if [ -f "${EXPNS_CSV}" ]; then
    EXPNS_ARG="--expansion-csv ${EXPNS_CSV}"
fi

python3 "${PROJECT_DIR}/calibration/python/perseus_climate_projection.py" \
    --rcp ${RCP} \
    --batch-id \${SLURM_ARRAY_TASK_ID} \
    --batch-size ${BATCH_SIZE} \
    --n-draws ${N_DRAWS} \
    --seed ${SEED} \
    --start-year ${START_YEAR} \
    --end-year ${END_YEAR} \
    --variants ${VARIANTS} \
    \${EXPNS_ARG} \
    --output-dir "${BASE_OUTPUT_DIR}" \
    --perseus-csv "${PERSEUS_CSV}" \
    --si-ratio-csv "${SI_RATIO_CSV}"

echo ""
echo "Batch \${SLURM_ARRAY_TASK_ID} finished at \$(date)"
SBATCH
)

    echo "  Submitted array job: ${JOB_ID} (${N_BATCHES} batches)"
    ALL_JOB_IDS+=("${JOB_ID}")

    # --- Aggregation job (runs after all batches complete) ---
    AGG_ID=$(sbatch --parsable --dependency=afterok:${JOB_ID} <<SBATCH
#!/bin/bash
#SBATCH --job-name=per_${RCP_TAG}_agg
#SBATCH --account=${SLURM_ACCOUNT}
#SBATCH --partition=cpu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=${LOG_DIR}/aggregate.out
#SBATCH --error=${LOG_DIR}/aggregate.err

module purge
module load python/3.12

export FVS_PROJECT_ROOT="${PROJECT_DIR}"

echo "=== PERSEUS Climate RCP ${RCP} Aggregation ==="
echo "Start: \$(date)"

# Concatenate batch results
cd "${OUTPUT_DIR}"

echo "Concatenating point estimate batches..."
head -1 \$(ls perseus_climate_point_${RCP_TAG}_batch*.csv 2>/dev/null | head -1) > perseus_climate_point_${RCP_TAG}_all.csv
for f in perseus_climate_point_${RCP_TAG}_batch*.csv; do
    tail -n +2 "\$f" >> perseus_climate_point_${RCP_TAG}_all.csv
done
echo "  Point estimates: \$(wc -l < perseus_climate_point_${RCP_TAG}_all.csv) records"

echo "Concatenating draw batches..."
head -1 \$(ls perseus_climate_draws_${RCP_TAG}_batch*.csv 2>/dev/null | head -1) > perseus_climate_draws_${RCP_TAG}_all.csv
for f in perseus_climate_draws_${RCP_TAG}_batch*.csv; do
    tail -n +2 "\$f" >> perseus_climate_draws_${RCP_TAG}_all.csv
done
echo "  Draw results: \$(wc -l < perseus_climate_draws_${RCP_TAG}_all.csv) records"

# Run aggregation using Python
python3 -c "
import sys, os, json
import pandas as pd
import numpy as np

sys.path.insert(0, '${PROJECT_DIR}')
sys.path.insert(0, '${PROJECT_DIR}/deployment/fvs2py')
from calibration.python.perseus_uncertainty_projection import aggregate_with_uncertainty

rcp_tag = '${RCP_TAG}'
output_dir = '${OUTPUT_DIR}'
expns_csv = '${EXPNS_CSV}'

point_df = pd.read_csv(os.path.join(output_dir, f'perseus_climate_point_{rcp_tag}_all.csv'))
draws_df = pd.read_csv(os.path.join(output_dir, f'perseus_climate_draws_{rcp_tag}_all.csv'))

expns_df = None
if os.path.exists(expns_csv):
    expns_df = pd.read_csv(expns_csv)

n_plots = point_df['PLOT'].nunique()
print(f'Aggregating {n_plots} plots, {len(point_df)} point records, {len(draws_df)} draw records')

point_mmt, uncertainty_mmt = aggregate_with_uncertainty(
    point_df, draws_df,
    expns_df=expns_df,
    n_plots_total=n_plots,
)

point_mmt.to_csv(os.path.join(output_dir, f'perseus_climate_mmt_point_{rcp_tag}.csv'), index=False)
uncertainty_mmt.to_csv(os.path.join(output_dir, f'perseus_climate_mmt_bands_{rcp_tag}.csv'), index=False)

print()
print('State-Level MMT Summary:')
print(point_mmt.to_string(float_format='{:.2f}'.format))
print()
if not uncertainty_mmt.empty:
    print('Uncertainty Bands:')
    print(uncertainty_mmt.to_string(float_format='{:.2f}'.format))
"

echo ""
echo "Aggregation finished at \$(date)"
SBATCH
)

    echo "  Submitted aggregation job: ${AGG_ID} (depends on ${JOB_ID})"
    ALL_AGG_IDS+=("${AGG_ID}")
    echo ""

done

# --- Summary -----------------------------------------------------------------
echo "============================================="
echo "All jobs submitted."
echo ""
echo "Array jobs:       ${ALL_JOB_IDS[*]}"
echo "Aggregation jobs: ${ALL_AGG_IDS[*]}"
echo ""
echo "Monitor:"
echo "  squeue -u $(whoami) --name='per_rcp45,per_rcp85,per_rcp45_agg,per_rcp85_agg'"
echo ""
echo "Check batch progress:"
echo "  for rcp in rcp45 rcp85; do"
echo "    done=\$(ls ${BASE_OUTPUT_DIR}/climate_\${rcp}/perseus_climate_point_\${rcp}_batch*.csv 2>/dev/null | wc -l)"
echo "    echo \"\${rcp}: \${done}/${N_BATCHES} batches complete\""
echo "  done"
echo ""
echo "Estimated runtime: 4-6 hours per RCP scenario."

#!/bin/bash
# =============================================================================
# SLURM Submission: PERSEUS Harvest + Climate Factorial Projections
# =============================================================================
#
# Runs the full PERSEUS Round 2 factorial design:
#   2 (Harvest: Yes/No) x 3 (Climate: None/RCP4.5/RCP8.5) = 6 scenarios
#
# Harvest design:
#   - 2% of FIA plots harvested annually (10% per 5-yr cycle)
#   - 50% BA removal from below (ThinBBA)
#   - Targets: high merch volume, private ownership, non-reserve
#   - Re-entry after 50 years
#
# Each batch processes 100 plots across ALL 6 scenarios.
# The harvest schedule is deterministic (computed from the full plot set),
# so each batch independently produces consistent results.
#
# Usage:
#   bash submit_perseus_harvest.sh                    # all 6 scenarios
#   bash submit_perseus_harvest.sh --harvest-only     # harvest runs only
#   bash submit_perseus_harvest.sh --noharvest-only   # baseline (no harvest)
#
# Author: A. Weiskittel
# Date: 2026-04-18
# =============================================================================

set -euo pipefail

# --- Parse arguments ---------------------------------------------------------
MODE="all"  # all, harvest, noharvest
for arg in "$@"; do
  case $arg in
    --harvest-only) MODE="harvest" ;;
    --noharvest-only) MODE="noharvest" ;;
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
SEED=42
START_YEAR=1999
END_YEAR=2004
BATCH_SIZE=100
N_PLOTS=3586
N_BATCHES=$(( (N_PLOTS + BATCH_SIZE - 1) / BATCH_SIZE ))

# Both NE and ACD variants (default + calibrated configs each)
VARIANTS="ne acd"

PROJECT_DIR="${FVS_PROJECT_ROOT}"
BASE_OUTPUT_DIR="${PROJECT_DIR}/calibration/output/perseus"
BASE_LOG_DIR="${PROJECT_DIR}/calibration/logs/perseus_harvest"

PERSEUS_CSV="${PROJECT_DIR}/calibration/data/perseus_plots.csv"
EXPNS_CSV="${BASE_OUTPUT_DIR}/untreated_expns.csv"
if [ ! -f "${EXPNS_CSV}" ]; then
    echo "NOTE: Expansion CSV not found. Will use approximate expansion factors."
    EXPNS_CSV=""
fi

# --- Determine harvest flags --------------------------------------------------
HARVEST_FLAG=""
case $MODE in
    harvest)   HARVEST_FLAG="--harvest" ;;
    noharvest) HARVEST_FLAG="--no-harvest" ;;
    all)       HARVEST_FLAG="" ;;  # default runs both
esac

# --- Verify prerequisites ----------------------------------------------------
echo "PERSEUS Harvest + Climate Factorial Projection Pipeline"
echo "======================================================="
echo ""

if [ ! -f "${SI_RATIO_CSV}" ]; then
    echo "ERROR: SI ratio file not found: ${SI_RATIO_CSV}"
    exit 1
fi

if [ ! -f "${PERSEUS_CSV}" ]; then
    echo "ERROR: PERSEUS plots CSV not found: ${PERSEUS_CSV}"
    exit 1
fi

SCRIPT="${PROJECT_DIR}/calibration/python/perseus_harvest_projection.py"
if [ ! -f "${SCRIPT}" ]; then
    echo "ERROR: Harvest projection script not found: ${SCRIPT}"
    exit 1
fi

# --- Verify FVS executables exist -------------------------------------------
MISSING_EXE=0
for var in ${VARIANTS}; do
    EXE="${PROJECT_DIR}/lib/FVS${var}"
    SO="${PROJECT_DIR}/lib/FVS${var}.so"
    if [ ! -f "${EXE}" ] && [ ! -f "${SO}" ]; then
        echo "WARNING: No executable or library found for ${var}"
        echo "  Expected: ${EXE} or ${SO}"
        MISSING_EXE=1
    elif [ ! -f "${EXE}" ]; then
        echo "NOTE: No executable for ${var} (will use .so via fvs2py)"
        echo "  For better reliability, build executable first:"
        echo "  bash ${PROJECT_DIR}/deployment/scripts/build_fvs_executables.sh ${PROJECT_DIR} ${PROJECT_DIR}/lib ${var}"
    else
        echo "OK: FVS${var} executable found"
    fi
done

if [ ${MISSING_EXE} -eq 1 ]; then
    echo ""
    echo "To build missing executables:"
    echo "  bash ${PROJECT_DIR}/deployment/scripts/build_fvs_executables.sh ${PROJECT_DIR} ${PROJECT_DIR}/lib ${VARIANTS}"
    echo ""
fi

echo ""
echo "Mode:          ${MODE}"
echo "Variants:      ${VARIANTS}"
echo "Configs:       default + calibrated"
echo "Year range:    ${START_YEAR}-${END_YEAR}"
echo "Batch size:    ${BATCH_SIZE}"
echo "N batches:     ${N_BATCHES}"
echo "Harvest flag:  ${HARVEST_FLAG:-'(both harvest + noharvest)'}"
echo "Runs per plot: 2 variants x 2 configs x 6 scenarios = 24"
echo ""

LOG_DIR="${BASE_LOG_DIR}"
mkdir -p "${LOG_DIR}"

# --- Submit array job --------------------------------------------------------
# Each batch runs all 6 scenarios (or subset based on mode).
# The script handles the factorial internally, so one SLURM array covers
# all combinations. Memory bumped to 24G because we load COND data too.

JOB_ID=$(sbatch --parsable <<SBATCH
#!/bin/bash
#SBATCH --job-name=per_harv
#SBATCH --account=${SLURM_ACCOUNT}
#SBATCH --partition=cpu
#SBATCH --array=1-${N_BATCHES}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=24G
#SBATCH --time=24:00:00
#SBATCH --output=${LOG_DIR}/batch_%a.out
#SBATCH --error=${LOG_DIR}/batch_%a.err

module purge
module load python/3.12

export FVS_PROJECT_ROOT="${PROJECT_DIR}"
export FVS_FIA_DATA_DIR="${FVS_FIA_DATA_DIR}"
export FVS_LIB_DIR="${PROJECT_DIR}/lib"
export LD_LIBRARY_PATH="${PROJECT_DIR}/lib:\${LD_LIBRARY_PATH:-}"
export NSBE_ROOT="${PROJECT_DIR}/data/NSBE"
export FVS_CONFIG_DIR="${PROJECT_DIR}/config"
export PERSEUS_OUTPUT_DIR="${BASE_OUTPUT_DIR}"
export PERSEUS_CSV="${PERSEUS_CSV}"
export SI_RATIO_CSV="${SI_RATIO_CSV}"

echo "=== PERSEUS Harvest Factorial Batch \${SLURM_ARRAY_TASK_ID} ==="
echo "Start: \$(date)"
echo "Node:  \$(hostname)"
echo "Mode:  ${MODE}"
echo ""

EXPNS_ARG=""
if [ -f "${EXPNS_CSV}" ]; then
    EXPNS_ARG="--expansion-csv ${EXPNS_CSV}"
fi

python3 "${SCRIPT}" \
    --rcp all \
    --batch-id \${SLURM_ARRAY_TASK_ID} \
    --batch-size ${BATCH_SIZE} \
    --seed ${SEED} \
    --start-year ${START_YEAR} \
    --end-year ${END_YEAR} \
    --variants ${VARIANTS} \
    ${HARVEST_FLAG} \
    \${EXPNS_ARG} \
    --output-dir "${BASE_OUTPUT_DIR}" \
    --perseus-csv "${PERSEUS_CSV}" \
    --si-ratio-csv "${SI_RATIO_CSV}"

echo ""
echo "Batch \${SLURM_ARRAY_TASK_ID} finished at \$(date)"
SBATCH
)

echo "Submitted array job: ${JOB_ID} (${N_BATCHES} batches)"

# --- Aggregation job (runs after all batches complete) -----------------------
AGG_ID=$(sbatch --parsable --dependency=afterok:${JOB_ID} <<SBATCH
#!/bin/bash
#SBATCH --job-name=per_harv_agg
#SBATCH --account=${SLURM_ACCOUNT}
#SBATCH --partition=cpu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=${LOG_DIR}/aggregate.out
#SBATCH --error=${LOG_DIR}/aggregate.err

module purge
module load python/3.12

export FVS_PROJECT_ROOT="${PROJECT_DIR}"

echo "=== PERSEUS Harvest Aggregation ==="
echo "Start: \$(date)"

# Scenario subdirectories to aggregate
SCENARIOS=()
case "${MODE}" in
    harvest)
        SCENARIOS=(harvest_noclimate harvest_rcp45 harvest_rcp85)
        ;;
    noharvest)
        SCENARIOS=(noharvest_noclimate noharvest_rcp45 noharvest_rcp85)
        ;;
    all)
        SCENARIOS=(harvest_noclimate harvest_rcp45 harvest_rcp85 noharvest_noclimate noharvest_rcp45 noharvest_rcp85)
        ;;
esac

for SCEN in "\${SCENARIOS[@]}"; do
    SCEN_DIR="${BASE_OUTPUT_DIR}/\${SCEN}"

    if [ ! -d "\${SCEN_DIR}" ]; then
        echo "  Skipping \${SCEN} (directory not found)"
        continue
    fi

    echo ""
    echo "--- Aggregating \${SCEN} ---"

    # Count batch files
    N_FILES=\$(ls \${SCEN_DIR}/perseus_\${SCEN}_batch*.csv 2>/dev/null | wc -l)
    if [ "\${N_FILES}" -eq 0 ]; then
        echo "  No batch files found for \${SCEN}"
        continue
    fi
    echo "  Found \${N_FILES} batch files"

    # Concatenate
    COMBINED="\${SCEN_DIR}/perseus_\${SCEN}_all.csv"
    head -1 \$(ls \${SCEN_DIR}/perseus_\${SCEN}_batch*.csv | head -1) > "\${COMBINED}"
    for f in \${SCEN_DIR}/perseus_\${SCEN}_batch*.csv; do
        tail -n +2 "\$f" >> "\${COMBINED}"
    done
    echo "  Combined: \$(wc -l < "\${COMBINED}") records -> \${COMBINED}"
done

# Run Python aggregation across all scenarios
python3 -c "
import sys, os, json
import pandas as pd
import numpy as np

sys.path.insert(0, '${PROJECT_DIR}')
sys.path.insert(0, '${PROJECT_DIR}/calibration/python')
sys.path.insert(0, '${PROJECT_DIR}/deployment/fvs2py')
from perseus_uncertainty_projection import aggregate_with_uncertainty

output_dir = '${BASE_OUTPUT_DIR}'
scenarios = '${MODE}'

if scenarios == 'all':
    scen_list = ['harvest_noclimate', 'harvest_rcp45', 'harvest_rcp85',
                 'noharvest_noclimate', 'noharvest_rcp45', 'noharvest_rcp85']
elif scenarios == 'harvest':
    scen_list = ['harvest_noclimate', 'harvest_rcp45', 'harvest_rcp85']
else:
    scen_list = ['noharvest_noclimate', 'noharvest_rcp45', 'noharvest_rcp85']

all_mmt = []

for scen in scen_list:
    combined_path = os.path.join(output_dir, scen, f'perseus_{scen}_all.csv')
    if not os.path.exists(combined_path):
        print(f'  {scen}: combined CSV not found, skipping')
        continue

    df = pd.read_csv(combined_path)
    n_plots = df['PLOT'].nunique()
    print(f'  {scen}: {n_plots} plots, {len(df)} records')

    point_mmt, _ = aggregate_with_uncertainty(
        df, pd.DataFrame(), n_plots_total=n_plots
    )

    if not point_mmt.empty:
        point_mmt['SCENARIO'] = scen
        all_mmt.append(point_mmt)

        mmt_path = os.path.join(output_dir, scen, f'perseus_{scen}_mmt.csv')
        point_mmt.to_csv(mmt_path, index=False)
        print(f'  {scen} MMT saved: {mmt_path}')

if all_mmt:
    combined_mmt = pd.concat(all_mmt, ignore_index=True)
    combined_path = os.path.join(output_dir, 'perseus_harvest_factorial_mmt.csv')
    combined_mmt.to_csv(combined_path, index=False)
    print(f'\nCombined factorial MMT: {combined_path}')
    print(f'{len(combined_mmt)} records across {combined_mmt[\"SCENARIO\"].nunique()} scenarios')
"

echo ""
echo "Aggregation finished at \$(date)"
SBATCH
)

echo "Submitted aggregation job: ${AGG_ID} (depends on ${JOB_ID})"
echo ""

# --- Summary -----------------------------------------------------------------
echo "======================================================="
echo "All jobs submitted."
echo ""
echo "Array job:       ${JOB_ID} (${N_BATCHES} batches)"
echo "Aggregation job: ${AGG_ID}"
echo ""
echo "Monitor:"
echo "  squeue -u $(whoami) --name='per_harv,per_harv_agg'"
echo ""
echo "Check batch progress:"
echo "  for scen in harvest_noclimate harvest_rcp45 harvest_rcp85 noharvest_noclimate noharvest_rcp45 noharvest_rcp85; do"
echo "    done=\$(ls ${BASE_OUTPUT_DIR}/\${scen}/perseus_\${scen}_batch*.csv 2>/dev/null | wc -l)"
echo "    echo \"\${scen}: \${done}/${N_BATCHES} batches complete\""
echo "  done"
echo ""
echo "Estimated runtime: 8-12 hours (6 scenarios x 100 plots/batch)."
echo "Each batch runs all 6 scenarios sequentially per plot."

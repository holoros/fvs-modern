#!/bin/bash
# =============================================================================
# Monitor PERSEUS Uncertainty Jobs on Cardinal
# =============================================================================
#
# Usage:
#   bash monitor_perseus_uncertainty.sh           # one-time check
#   bash monitor_perseus_uncertainty.sh --watch   # refresh every 60s
#   bash monitor_perseus_uncertainty.sh --summary # just show completion stats
#
# Author: A. Weiskittel
# Date: 2026-04-11
# =============================================================================

set -euo pipefail

# --- HPC Configuration (override with environment variables) ---
FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/users/PUOM0008/crsfaaron/fvs-modern}"

PROJECT_DIR="${FVS_PROJECT_ROOT}"
OUTPUT_DIR="${PROJECT_DIR}/calibration/output/perseus/uncertainty_1999_2004"
LOG_DIR="${PROJECT_DIR}/calibration/logs/perseus_uncertainty"

WATCH=false
SUMMARY_ONLY=false

for arg in "$@"; do
    case $arg in
        --watch) WATCH=true ;;
        --summary) SUMMARY_ONLY=true ;;
    esac
done

show_status() {
    echo "============================================================"
    echo "PERSEUS Uncertainty Projection Monitor"
    echo "$(date '+%Y-%m-%d %H:%M:%S')"
    echo "============================================================"

    # Check for running/pending jobs
    JOBS=$(squeue -u "$USER" -n perseus_unc,perseus_unc_agg -h 2>/dev/null || true)
    if [ -n "$JOBS" ]; then
        echo ""
        echo "Active SLURM Jobs:"
        squeue -u "$USER" -n perseus_unc,perseus_unc_agg -o "%.10i %.12j %.8T %.10M %.6D %R" 2>/dev/null || true
        echo ""

        # Count by state
        RUNNING=$(echo "$JOBS" | grep -c "RUNNING" || true)
        PENDING=$(echo "$JOBS" | grep -c "PENDING" || true)
        echo "  Running: ${RUNNING}  Pending: ${PENDING}"
    else
        echo ""
        echo "No active SLURM jobs found."
    fi

    # Check output files
    echo ""
    echo "Output Files:"
    N_POINT=$(ls "${OUTPUT_DIR}"/perseus_uncertainty_point_batch*.csv 2>/dev/null | wc -l)
    N_DRAWS=$(ls "${OUTPUT_DIR}"/perseus_uncertainty_draws_batch*.csv 2>/dev/null | wc -l)
    N_META=$(ls "${OUTPUT_DIR}"/perseus_uncertainty_meta_batch*.json 2>/dev/null | wc -l)
    echo "  Point estimate batches: ${N_POINT}"
    echo "  Draw result batches:    ${N_DRAWS}"
    echo "  Metadata files:         ${N_META}"

    # Check for combined/aggregated output
    if [ -f "${OUTPUT_DIR}/mmt_point_estimates.csv" ]; then
        echo "  Aggregated point MMT:   YES"
    fi
    if [ -f "${OUTPUT_DIR}/mmt_uncertainty_bands.csv" ]; then
        echo "  Uncertainty bands:      YES"
    fi
    if [ -f "${OUTPUT_DIR}/projection_summary.json" ]; then
        echo "  Summary JSON:           YES"
    fi

    # Check log files for errors
    echo ""
    echo "Log Summary:"
    N_LOGS=$(ls "${LOG_DIR}"/batch_*.out 2>/dev/null | wc -l)
    N_ERRS=0
    if [ "$N_LOGS" -gt 0 ]; then
        N_ERRS=$(grep -l "ERROR\|FAILED\|Traceback" "${LOG_DIR}"/batch_*.err 2>/dev/null | wc -l || true)
        N_COMPLETE=$(grep -l "finished at" "${LOG_DIR}"/batch_*.out 2>/dev/null | wc -l || true)
        echo "  Log files:    ${N_LOGS}"
        echo "  Completed:    ${N_COMPLETE}"
        echo "  With errors:  ${N_ERRS}"

        if [ "$N_ERRS" -gt 0 ] && [ "$SUMMARY_ONLY" = false ]; then
            echo ""
            echo "  Error files:"
            grep -l "ERROR\|FAILED\|Traceback" "${LOG_DIR}"/batch_*.err 2>/dev/null | while read f; do
                echo "    $(basename $f): $(tail -1 "$f")"
            done
        fi
    else
        echo "  No log files yet (jobs may not have started)"
    fi

    # Quick timing estimate
    if [ "$N_COMPLETE" -gt 0 ] 2>/dev/null; then
        TOTAL_BATCHES=36
        PCT=$(echo "scale=1; ${N_COMPLETE} * 100 / ${TOTAL_BATCHES}" | bc 2>/dev/null || echo "?")
        echo ""
        echo "Progress: ${N_COMPLETE}/${TOTAL_BATCHES} batches (${PCT}%)"
    fi

    echo ""
    echo "============================================================"
}

if [ "$WATCH" = true ]; then
    while true; do
        clear
        show_status
        echo "Refreshing in 60s... (Ctrl+C to stop)"
        sleep 60
    done
else
    show_status
fi

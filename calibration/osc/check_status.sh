#!/bin/bash
# =============================================================================
# FVS Calibration: Check Pipeline Status Across All Variants
#
# Usage: bash calibration/osc/check_status.sh
# =============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config_osc.sh"

VARIANTS=(acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws)

echo "==========================================="
echo "FVS Calibration Pipeline Status"
echo "$(date)"
echo "==========================================="
echo ""

COMPLETE=0
RUNNING=0
FAILED=0
PENDING=0

printf "%-6s %-12s %-8s %-8s %-8s %-8s %-8s %-10s %s\n" \
    "VAR" "STATUS" "01_FIA" "02_DG" "03_HD" "04_MORT" "05_CR" "06_JSON" "DRAWS"
printf "%s\n" "$(printf '%.0s-' {1..85})"

for v in "${VARIANTS[@]}"; do
    OUT="${PROJECT_ROOT}/calibration/output/variants/${v}"
    LOG="${PROJECT_ROOT}/calibration/logs/variant_${v}.log"
    CAL="${PROJECT_ROOT}/config/calibrated/${v}.json"
    DRAWS="${PROJECT_ROOT}/config/calibrated/${v}_draws.json"

    # Check each step
    s01=" "; s02=" "; s03=" "; s04=" "; s05=" "; s06=" "; s_draws=" "

    [ -f "${PROJECT_ROOT}/calibration/data/processed/${v}/diameter_growth.csv" ] && s01="OK"
    [ -f "${OUT}/diameter_growth_posterior.csv" ] && s02="OK"
    [ -f "${OUT}/height_diameter_summary.csv" ] && s03="OK"
    [ -f "${OUT}/mortality_summary.csv" ] && s04="OK"
    [ -f "${OUT}/crown_ratio_summary.csv" ] && s05="OK"
    [ -f "${CAL}" ] && s06="OK"
    [ -f "${DRAWS}" ] && s_draws="OK"

    # Determine overall status
    if [ -f "${CAL}" ]; then
        STATUS="COMPLETE"
        COMPLETE=$((COMPLETE + 1))
    elif [ -f "${LOG}" ] && grep -q "FATAL" "${LOG}" 2>/dev/null; then
        STATUS="FAILED"
        FAILED=$((FAILED + 1))
    elif [ -f "${LOG}" ] && [ -f "${PROJECT_ROOT}/calibration/data/processed/${v}/diameter_growth.csv" ]; then
        STATUS="RUNNING"
        RUNNING=$((RUNNING + 1))
    elif [ -f "${LOG}" ]; then
        STATUS="STARTED"
        RUNNING=$((RUNNING + 1))
    else
        STATUS="PENDING"
        PENDING=$((PENDING + 1))
    fi

    printf "%-6s %-12s %-8s %-8s %-8s %-8s %-8s %-10s %s\n" \
        "${v}" "${STATUS}" "${s01}" "${s02}" "${s03}" "${s04}" "${s05}" "${s06}" "${s_draws}"
done

echo ""
echo "Summary: ${COMPLETE} complete, ${RUNNING} running, ${PENDING} pending, ${FAILED} failed"
echo ""

# Show SLURM queue if available
if command -v squeue &>/dev/null; then
    echo "Active SLURM jobs:"
    squeue -u $(whoami) --name=fvs-cal -o "%.8i %.4t %.10M %.6D %R" 2>/dev/null || echo "  (none)"
fi

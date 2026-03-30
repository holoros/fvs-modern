#!/bin/bash
# =============================================================================
# FVS Phase 1: Check Component Model Status Across All Variants
#
# Scans the output directory for each variant and reports which component
# models have completed successfully. Useful for monitoring batch jobs.
#
# Usage: bash calibration/osc/check_phase1_status.sh
# =============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config_osc.sh"

OUTPUT_BASE="${PROJECT_ROOT}/calibration/output/variants"

VARIANTS=(
    acd ak bc bm ca ci cr cs ec em
    ie kt ls nc ne oc on op pn sn
    so tt ut wc ws
)

# Component files to check
COMPONENTS=(
    "diameter_growth_posterior.csv:DG"
    "height_diameter_posterior.csv:HD"
    "height_increment_posterior.csv:HI"
    "mortality_posterior.csv:MO"
    "crown_ratio_posterior.csv:CR"
    "stand_density_posterior.csv:SD"
)

# Header
printf "%-6s" "VAR"
for comp in "${COMPONENTS[@]}"; do
    label="${comp#*:}"
    printf "  %-4s" "$label"
done
printf "  %-6s  %s\n" "JSON" "STATUS"
printf "%-6s" "----"
for comp in "${COMPONENTS[@]}"; do
    printf "  %-4s" "----"
done
printf "  %-6s  %s\n" "------" "------"

# Check each variant
TOTAL_COMPLETE=0
TOTAL_VARIANTS=0

for variant in "${VARIANTS[@]}"; do
    TOTAL_VARIANTS=$((TOTAL_VARIANTS + 1))
    VDIR="${OUTPUT_BASE}/${variant}"
    printf "%-6s" "$variant"

    N_DONE=0
    N_POSSIBLE=6

    for comp in "${COMPONENTS[@]}"; do
        file="${comp%%:*}"
        label="${comp#*:}"

        if [ -f "${VDIR}/${file}" ]; then
            printf "  %-4s" "OK"
            N_DONE=$((N_DONE + 1))
        else
            printf "  %-4s" "--"
        fi
    done

    # Check calibrated JSON
    if [ -f "${PROJECT_ROOT}/config/calibrated/${variant}.json" ]; then
        printf "  %-6s" "OK"
    else
        printf "  %-6s" "--"
    fi

    # Status summary
    if [ $N_DONE -eq $N_POSSIBLE ]; then
        printf "  ALL DONE\n"
        TOTAL_COMPLETE=$((TOTAL_COMPLETE + 1))
    elif [ $N_DONE -gt 0 ]; then
        printf "  %d/%d\n" "$N_DONE" "$N_POSSIBLE"
    else
        printf "  NOT STARTED\n"
    fi
done

echo ""
echo "Summary: ${TOTAL_COMPLETE}/${TOTAL_VARIANTS} variants fully complete"
echo ""

# Check for running SLURM jobs
if command -v squeue &> /dev/null; then
    echo "Active SLURM jobs:"
    squeue -u "$USER" --format="%.10i %.9P %.12j %.8T %.10M %.6D %R" 2>/dev/null || true
fi

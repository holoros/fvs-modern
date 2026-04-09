#!/bin/bash
#SBATCH --job-name=fvs_final_v2
#SBATCH --account=PUOM0008
#SBATCH --time=01:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_v2_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_v2_%j.err
#SBATCH --dependency=afterany:7762882_15

# ============================================================================
# Final comparison after all v2 refits complete
# Depends on OC CR v2 (last running job)
# Runs 11_full_comparison.R then generates master summary
# ============================================================================

echo "=== Final Comparison v2 ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

echo "=== Step 1: Full Comparison ==="
Rscript "${SCRIPTS_DIR}/11_full_comparison.R" --all
COMP_EXIT=$?
echo "Comparison exit code: $COMP_EXIT"

echo "=== Step 2: DG Summary ==="
echo ""
echo "=== Final DG sigma values ==="
echo "variant,method,sigma_median"
for v in acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws; do
    sig=$(grep '^sigma,' "${FVS_PROJECT_ROOT}/calibration/output/variants/${v}/diameter_growth_summary.csv" 2>/dev/null | head -1 | awk -F, '{print $3}')
    echo "${v},${sig}"
done

echo ""
echo "=== Step 3: CR v2 Summary ==="
echo "variant,r2_v1,r2_v2"
for v in acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws; do
    r2_v2=$(tail -1 "${FVS_PROJECT_ROOT}/calibration/output/variants/${v}/crown_ratio_v2_eval.csv" 2>/dev/null | awk -F, '{print $9}')
    echo "${v},${r2_v2}"
done

echo ""
echo "=== Step 4: Mortality v2 Summary ==="
echo "variant,auc_v2"
for v in ca ie nc oc op sn so tt ws; do
    auc=$(tail -1 "${FVS_PROJECT_ROOT}/calibration/output/variants/${v}/mortality_v2_auc.csv" 2>/dev/null | awk -F, '{print $9}')
    echo "${v},${auc}"
done

echo ""
echo "=== Final comparison v2 complete ==="
echo "End: $(date)"

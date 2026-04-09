#!/bin/bash
#SBATCH --job-name=fvs_final
#SBATCH --account=PUOM0008
#SBATCH --time=04:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=14
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_compare_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_compare_%j.err

# ============================================================================
# Final comprehensive comparison and assessment after all refits
# Runs: 07_compare_to_baseline.R + 10_assess_calibration.R
# ============================================================================

echo "=== Final Comparison and Assessment ==="
echo "Start: $(date)"
echo "Node: $(hostname)"

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"
export FVS_MAX_OBS="30000"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

# Step 1: Run comparison for ALL 25 variants
echo "=== Step 1: Baseline Comparison ==="
Rscript "${SCRIPTS_DIR}/11_full_comparison.R" --all
COMP_EXIT=$?
echo "Comparison exit code: $COMP_EXIT"

# Step 2: Run comprehensive assessment
echo "=== Step 2: Comprehensive Assessment ==="
Rscript "${SCRIPTS_DIR}/12_comprehensive_assessment.R" --all
ASSESS_EXIT=$?
echo "Assessment exit code: $ASSESS_EXIT"

# Step 3: Generate updated DG summary across all variants
echo "=== Step 3: DG Summary ==="
OUTPUT_DIR="${FVS_PROJECT_ROOT}/calibration/output"
echo "variant,method,n_params,sigma_median" > "${OUTPUT_DIR}/dg_all_variants_summary_final.csv"
for v in acd ak bc bm ca ci cr cs ec em ie kt ls nc ne oc on op pn sn so tt ut wc ws; do
    vdir="${OUTPUT_DIR}/variants/$v"
    if [ -f "$vdir/diameter_growth_summary.csv" ]; then
        method=$(cat "$vdir/diameter_growth_method.txt" 2>/dev/null || echo "unknown")
        n_params=$(wc -l < "$vdir/diameter_growth_summary.csv")
        sigma=$(grep "^sigma," "$vdir/diameter_growth_summary.csv" | cut -d, -f2)
        echo "$v,$method,$n_params,$sigma"
    else
        echo "$v,missing,0,NA"
    fi
done >> "${OUTPUT_DIR}/dg_all_variants_summary_final.csv"

echo ""
echo "=== Final DG sigma values ==="
cat "${OUTPUT_DIR}/dg_all_variants_summary_final.csv"

echo ""
echo "=== Final comparison and assessment complete ==="
echo "End: $(date)"

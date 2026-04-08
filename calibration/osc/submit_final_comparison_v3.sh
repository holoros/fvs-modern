#!/bin/bash
#SBATCH --job-name=fvs_final_v3
#SBATCH --account=PUOM0008
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/final_v3_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/final_v3_%j.err

# Final comparison v3: CR/UT restored to 5K, comprehensive assessment with BAL_s fix

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export FVS_FIA_DATA_DIR="/users/PUOM0008/crsfaaron/FIA"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

cd $FVS_PROJECT_ROOT
SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

echo "============================================"
echo "Step 1: Full comparison (all 25 variants)"
echo "============================================"
echo "Start: $(date)"
Rscript "${SCRIPTS_DIR}/11_full_comparison.R" --all
echo "Finish: $(date)"

echo "============================================"
echo "Step 2: Comprehensive assessment (fixed BAL_s)"
echo "============================================"
echo "Start: $(date)"
Rscript "${SCRIPTS_DIR}/12_comprehensive_assessment.R" --all
echo "Finish: $(date)"

echo "============================================"
echo "ALL DONE"
echo "============================================"

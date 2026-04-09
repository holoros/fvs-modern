#!/bin/bash
#SBATCH --job-name=fvs_final_v5
#SBATCH --account=PUOM0008
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_v5_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/final_v5_%j.err

# Final v5: Mortality v2 AUC from draws_df + assessment with SI column fix

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_FIA_DATA_DIR="/path/to/user/path"

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
echo "Step 1: Mortality v2 AUC (draws_df approach)"
echo "============================================"
echo "Start: $(date)"
Rscript "${SCRIPTS_DIR}/04c_compute_mortality_v2_auc.R" --all
echo "Finish: $(date)"

echo "============================================"
echo "Step 2: Comprehensive assessment (SI fix)"
echo "============================================"
echo "Start: $(date)"
Rscript "${SCRIPTS_DIR}/12_comprehensive_assessment.R" --all
echo "Finish: $(date)"

echo "============================================"
echo "ALL DONE"
echo "============================================"

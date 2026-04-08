#!/bin/bash
#SBATCH --job-name=fvs_final_fixes
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/slurm/final_fixes_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/slurm/final_fixes_%j.err

# Final fixes batch: CR/UT DG revert, mortality v2 AUC, and assessment rerun

module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export FVS_PROJECT_ROOT=/users/PUOM0008/crsfaaron/fvs-modern
export CMDSTAN=/users/PUOM0008/crsfaaron/.cmdstan/cmdstan-2.34.1

cd $FVS_PROJECT_ROOT

echo "============================================"
echo "Step 1: Revert CR and UT DG to 5K obs"
echo "============================================"

export FVS_MAX_OBS=5000

echo "--- CR at 5K ---"
echo "Start: $(date)"
Rscript calibration/R/02c_fit_dg_hmc_small.R --variant cr
echo "Finish: $(date)"

echo "--- UT at 5K ---"
echo "Start: $(date)"
Rscript calibration/R/02c_fit_dg_hmc_small.R --variant ut
echo "Finish: $(date)"

echo "============================================"
echo "Step 2: Compute mortality v2 AUC on full data"
echo "============================================"
echo "Start: $(date)"
Rscript calibration/R/04c_compute_mortality_v2_auc.R --all
echo "Finish: $(date)"

echo "============================================"
echo "Step 3: Rerun full comparison with fixed results"
echo "============================================"
echo "Start: $(date)"
Rscript calibration/R/11_full_comparison.R --all
echo "Finish: $(date)"

echo "============================================"
echo "Step 4: Comprehensive assessment (Part A + B)"
echo "============================================"
echo "Start: $(date)"
Rscript calibration/R/12_comprehensive_assessment.R --all
echo "Finish: $(date)"

echo "============================================"
echo "ALL FIXES COMPLETE"
echo "============================================"

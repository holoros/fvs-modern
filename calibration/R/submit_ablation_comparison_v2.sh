#!/bin/bash
#SBATCH --job-name=fvs_ablation_cmp_v2
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=00:15:00
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/ablation_comparison_v2_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/ablation_comparison_v2_%j.err

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration

Rscript R/21_ablation_comparison.R 2>&1 | tee logs/ablation_comparison_v2_console.log

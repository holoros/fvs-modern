#!/bin/bash
#SBATCH --job-name=fvs_ablation_cmp
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=00:15:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_comparison_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_comparison_%j.err
#SBATCH --dependency=afterok:8392189

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

Rscript R/21_ablation_comparison.R 2>&1 | tee logs/ablation_comparison_console.log

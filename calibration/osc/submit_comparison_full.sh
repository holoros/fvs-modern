#!/bin/bash
#SBATCH --job-name=fvs_compare
#SBATCH --account=PUOM0008
#SBATCH --time=02:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=8
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/comparison_full_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/comparison_full_%j.err

echo "=== Full comparison run ==="
echo "Start: $(date)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
module load R/4.3.0

Rscript ${FVS_PROJECT_ROOT}/calibration/R/11_full_comparison.R --all

echo "End: $(date)"

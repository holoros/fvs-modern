#!/bin/bash
#SBATCH --job-name=fvs_compare
#SBATCH --account=PUOM0008
#SBATCH --time=02:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=8
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/comparison_full_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/comparison_full_%j.err

echo "=== Full comparison across all 25 variants ==="
echo "Start: $(date)"

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

Rscript "${SCRIPTS_DIR}/11_full_comparison.R" --all

echo "=== Comparison complete ==="
echo "End: $(date)"

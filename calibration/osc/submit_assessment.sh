#!/bin/bash
#SBATCH --job-name=fvs_assess
#SBATCH --account=PUOM0008
#SBATCH --time=03:00:00
#SBATCH --mem=96G
#SBATCH --cpus-per-task=8
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/assessment_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/assessment_%j.err

echo "=== Comprehensive Model Assessment ==="
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

SCRIPTS_DIR="${FVS_PROJECT_ROOT}/calibration/R"

Rscript "${SCRIPTS_DIR}/12_comprehensive_assessment.R" --all

echo "=== Assessment complete ==="
echo "End: $(date)"

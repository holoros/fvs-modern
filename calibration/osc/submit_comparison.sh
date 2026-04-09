#!/bin/bash
#SBATCH --job-name=fvs-compare
#SBATCH --account=PUOM0008
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=2:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/comparison_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/comparison_%j.err

echo "==========================================="
echo "FVS Comprehensive Comparison"
echo "Node: $(hostname), Cores: ${SLURM_CPUS_PER_TASK}, Mem: 64G"
echo "Start: $(date)"
echo "==========================================="

module purge
module load gcc/12.3.0
module load R/4.4.0
module load gdal/3.7.3
module load proj/9.2.1
module load geos/3.12.0

export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}

Rscript calibration/R/10_comprehensive_comparison.R --all 2>&1

echo "==========================================="
echo "End: $(date)"
echo "==========================================="

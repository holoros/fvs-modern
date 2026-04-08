#!/bin/bash
#SBATCH --job-name=fvs_assess_v6
#SBATCH --account=PUOM0008
#SBATCH --time=01:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/assess_v6_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/assess_v6_%j.err

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0
export R_LIBS_USER="$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
cd $FVS_PROJECT_ROOT
echo "Start: $(date)"
Rscript calibration/R/12_comprehensive_assessment.R --all
echo "Finish: $(date)"

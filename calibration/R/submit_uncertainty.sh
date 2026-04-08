#!/bin/bash
#SBATCH --job-name=fvs_ci
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/uncertainty_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/uncertainty_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration

Rscript R/21_uncertainty_propagation.R 2>&1 | tee logs/uncertainty_console.log

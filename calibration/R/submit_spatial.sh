#!/bin/bash
#SBATCH --job-name=fvs_spatial
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/spatial_analysis_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/spatial_analysis_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration

Rscript R/20_spatial_performance_analysis.R 2>&1 | tee logs/spatial_analysis_console.log

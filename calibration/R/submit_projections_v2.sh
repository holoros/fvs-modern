#!/bin/bash
#SBATCH --job-name=fvs_proj_v2
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/projections_v2_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/projections_v2_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration

Rscript R/17_stand_projection_engine.R 2>&1 | tee logs/projections_v2_console.log

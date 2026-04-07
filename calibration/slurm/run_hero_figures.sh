#!/bin/bash
#SBATCH --job-name=hero_figs
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --time=01:00:00
#SBATCH --mem=32G
#SBATCH --account=PUOM0008
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/hero_figures_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/hero_figures_%j.err

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
export R_LIBS_USER="/users/PUOM0008/crsfaaron/R/x86_64-pc-linux-gnu-library/4.4:/users/PUOM0008/crsfaaron/R/libs"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration
Rscript R/22_hero_figures.R

#!/bin/bash
#SBATCH --job-name=hero_figs
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --time=01:00:00
#SBATCH --mem=32G
#SBATCH --account=PUOM0008
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/hero_figures_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/hero_figures_%j.err

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export R_LIBS_USER="/path/to/user/path"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration
Rscript R/22_hero_figures.R

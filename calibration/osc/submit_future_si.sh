#!/bin/bash
# =============================================================================
# SLURM submission: Future Site Index projection via ClimateNA rasters
# =============================================================================
#
# Adjust the account line to your OSC project code before first run:
#   sbatch --export=SITEINDEX_DIR=$HOME/SiteIndex calibration/osc/submit_future_si.sh
# =============================================================================
#SBATCH --job-name=future_si
#SBATCH --account=puom0008
#SBATCH --time=04:00:00
#SBATCH --nodes=1 --ntasks-per-node=1 --cpus-per-task=20 --mem=120G
#SBATCH --output=logs/future_si_%j.out
#SBATCH --error=logs/future_si_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu

mkdir -p logs

module load gcc/12.3.0 R/4.4.0 proj/9.2.1 gdal/3.7.3 geos/3.12.0
export R_LIBS="${HOME}/R/cardinal_libs/4.4.0:${HOME}/R/cardinal_libs:${HOME}/R/x86_64-pc-linux-gnu-library/4.4"

# Env driven paths; all have workstation friendly defaults via HOME
export SITEINDEX_DIR="${SITEINDEX_DIR:-${HOME}/SiteIndex}"
export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-${HOME}/fvs-modern}"
export SI_OUTPUT_DIR="${SI_OUTPUT_DIR:-${SITEINDEX_DIR}/future_SI}"

cd "${SITEINDEX_DIR}"
Rscript "${FVS_PROJECT_ROOT}/calibration/python/project_future_si.R"

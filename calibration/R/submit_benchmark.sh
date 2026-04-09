#!/bin/bash
#SBATCH --job-name=fvs_fia_bench
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=06:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/fia_benchmark_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/fia_benchmark_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FIA_DATA_DIR="/path/to/user/path"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/fia_benchmark_console.log

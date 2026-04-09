#!/bin/bash
#SBATCH --job-name=fvs_htfix
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=06:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/benchmark_htfix_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/benchmark_htfix_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

# Swap in the fixed engine (HT ratio approach for top height)
cp R/19_fia_benchmark_engine.R R/19_fia_benchmark_engine_pre_htfix.R
cp R/19_fia_benchmark_engine_v2.R R/19_fia_benchmark_engine.R

echo "Running benchmark with HT top height fix (ratio approach)"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/benchmark_htfix_console.log

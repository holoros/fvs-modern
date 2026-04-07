#!/bin/bash
#SBATCH --job-name=fvs_benchmark
#SBATCH --account=PUOM0008
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=benchmark_%j.out
#SBATCH --error=benchmark_%j.err

echo "=========================================="
echo "FVS FIA Benchmark Engine v2"
echo "Job ID: $SLURM_JOB_ID"
echo "Node: $SLURM_NODELIST"
echo "Start: $(date)"
echo "=========================================="

# Load modules (gcc/12.3.0 required before R/4.4.0)
module load gcc/12.3.0
module load R/4.4.0

# R library paths (includes brms, data.table, ggplot2, cmdstanr)
export R_LIBS_USER="/users/PUOM0008/crsfaaron/R/cardinal/4.4.0:/users/PUOM0008/crsfaaron/R/x86_64-pc-linux-gnu-library/4.4:/users/PUOM0008/crsfaaron/R/libs"

# Project root
export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd "$FVS_PROJECT_ROOT"

echo "R version:"
Rscript --version 2>&1
echo "R_LIBS_USER: $R_LIBS_USER"
echo ""

# Run the benchmark engine
Rscript calibration/R/19_fia_benchmark_engine.R 2>&1

echo ""
echo "=========================================="
echo "Benchmark complete: $(date)"
echo "=========================================="

#!/bin/bash
#SBATCH --job-name=raster_extract
#SBATCH --account=PUOM0008
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=01:00:00
#SBATCH --output=extract_%j.out

echo "=========================================="
echo "Raster Extraction Job"
echo "Job ID: $SLURM_JOB_ID"
echo "Node: $(hostname)"
echo "Start: $(date)"
echo "=========================================="

cd /users/PUOM0008/crsfaaron/fvs-modern

python3 -u calibration/R/extract_raster_values.py

echo ""
echo "=========================================="
echo "Extraction complete: $(date)"
echo "=========================================="

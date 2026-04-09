#!/bin/bash
#SBATCH --job-name=fvs_ablation
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

echo "=========================================="
echo "FVS ABLATION STUDY"
echo "=========================================="
echo ""

# Config 1: Full model (ClimateSI + SDIMAX + Ingrowth)
echo "CONFIG 1: FULL (ClimateSI=ON, SDIMAX=ON, Ingrowth=ON)"
export FVS_CLIMATE_SI=TRUE
export FVS_SDIMAX=TRUE
export FVS_INGROWTH=TRUE
export FVS_CI_BRACKETS=FALSE
export FVS_BOOTSTRAP=FALSE
export FVS_OUTPUT_TAG="ablation_full"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_full.log
echo ""

# Config 2: No ClimateSI (SICOND fallback)
echo "CONFIG 2: NO_CSI (ClimateSI=OFF, SDIMAX=ON, Ingrowth=ON)"
export FVS_CLIMATE_SI=FALSE
export FVS_SDIMAX=TRUE
export FVS_INGROWTH=TRUE
export FVS_OUTPUT_TAG="ablation_no_csi"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_no_csi.log
echo ""

# Config 3: No SDIMAX (no RDI modifier)
echo "CONFIG 3: NO_SDI (ClimateSI=ON, SDIMAX=OFF, Ingrowth=ON)"
export FVS_CLIMATE_SI=TRUE
export FVS_SDIMAX=FALSE
export FVS_INGROWTH=TRUE
export FVS_OUTPUT_TAG="ablation_no_sdi"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_no_sdi.log
echo ""

# Config 4: No Ingrowth
echo "CONFIG 4: NO_IG (ClimateSI=ON, SDIMAX=ON, Ingrowth=OFF)"
export FVS_CLIMATE_SI=TRUE
export FVS_SDIMAX=TRUE
export FVS_INGROWTH=FALSE
export FVS_OUTPUT_TAG="ablation_no_ig"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_no_ig.log
echo ""

# Config 5: Baseline (no ClimateSI, no SDIMAX, no Ingrowth)
echo "CONFIG 5: BASELINE (ClimateSI=OFF, SDIMAX=OFF, Ingrowth=OFF)"
export FVS_CLIMATE_SI=FALSE
export FVS_SDIMAX=FALSE
export FVS_INGROWTH=FALSE
export FVS_OUTPUT_TAG="ablation_baseline"
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_baseline.log
echo ""

echo "=========================================="
echo "ABLATION STUDY COMPLETE"
echo "=========================================="

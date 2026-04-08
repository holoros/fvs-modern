#!/bin/bash
#SBATCH --job-name=fvs_ablation_v2
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=14
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/ablation_v2_%j.out
#SBATCH --error=/users/PUOM0008/crsfaaron/fvs-modern/calibration/logs/ablation_v2_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"

cd /users/PUOM0008/crsfaaron/fvs-modern/calibration

# This is the CLEAN RERUN with the v2 engine (calibrated HTG + H-D imputation)
# Ensure the v2 engine is in place
cp R/19_fia_benchmark_engine_v2.R R/19_fia_benchmark_engine.R

echo "=========================================="
echo "ABLATION STUDY v2: Clean rerun with HTG model"
echo "Engine: 19_fia_benchmark_engine_v2.R (HTG + H-D imputation + H-D ratio fallback)"
echo "$(date)"
echo "=========================================="

# Config 1: Full model (ClimateSI + SDIMAX + Ingrowth + CI brackets + Bootstrap)
echo ""
echo ">>> Config 1: FULL MODEL <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=true
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=true
export FVS_BOOTSTRAP=true
export FVS_OUTPUT_TAG=ablation_full
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v2_full_console.log

# Config 2: No ClimateSI
echo ""
echo ">>> Config 2: NO CLIMATE SI <<<"
echo "$(date)"
export FVS_CLIMATE_SI=false
export FVS_SDIMAX=true
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_OUTPUT_TAG=ablation_no_csi
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v2_no_csi_console.log

# Config 3: No SDIMAX
echo ""
echo ">>> Config 3: NO SDIMAX <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=false
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_OUTPUT_TAG=ablation_no_sdi
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v2_no_sdi_console.log

# Config 4: No Ingrowth
echo ""
echo ">>> Config 4: NO INGROWTH <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=true
export FVS_INGROWTH=false
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_OUTPUT_TAG=ablation_no_ig
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v2_no_ig_console.log

# Config 5: Baseline (Bayes only: no ClimateSI, no SDIMAX, no Ingrowth)
echo ""
echo ">>> Config 5: BASELINE (Bayes only) <<<"
echo "$(date)"
export FVS_CLIMATE_SI=false
export FVS_SDIMAX=false
export FVS_INGROWTH=false
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_OUTPUT_TAG=ablation_baseline
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v2_baseline_console.log

echo ""
echo "=========================================="
echo "ABLATION STUDY v2 COMPLETE"
echo "$(date)"
echo "=========================================="

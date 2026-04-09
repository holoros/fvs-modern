#!/bin/bash
#SBATCH --job-name=fvs_ablation_v3
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=14
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_v3_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/ablation_v3_%j.err

module purge
module load gcc/12.3.0 R/4.4.0 gdal/3.7.3 proj/9.2.1 geos/3.12.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

echo "=========================================="
echo "ABLATION STUDY v3: DG bias fix + softened SDIMAX"
echo ""
echo "Changes from v2:"
echo "  1. DG back-transform: median (no Baskerville correction)"
echo "  2. SDIMAX modifier: asymmetric, onset at RDI=0.70, slope=0.5"
echo "  3. ClimateSI: multi-path lookup for raster CSV"
echo "  4. Variant DG multiplier: optional empirical bias correction"
echo "$(date)"
echo "=========================================="

# -------------------------------------------------------------------
# PRE-FLIGHT: Check if raster lookup exists
# -------------------------------------------------------------------
RASTER_CSV="${FVS_PROJECT_ROOT}/calibration/data/plot_raster_lookup.csv"
if [ -f "$RASTER_CSV" ]; then
  echo "Raster lookup found: $(wc -l < "$RASTER_CSV") rows"
else
  echo "WARNING: Raster lookup not found at $RASTER_CSV"
  echo "ClimateSI and SDIMAX raster toggles will have no effect."
  echo "Run: python3 calibration/R/extract_raster_values.py"
fi
echo ""

# -------------------------------------------------------------------
# Config 1: Full Model (all refinements on, median DG, soft SDIMAX)
# -------------------------------------------------------------------
echo ">>> Config 1: FULL MODEL (median DG, soft SDIMAX) <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=true
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=true
export FVS_BOOTSTRAP=true
export FVS_DG_BACKTRANSFORM=median
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_full
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_full_console.log

# -------------------------------------------------------------------
# Config 2: No ClimateSI
# -------------------------------------------------------------------
echo ""
echo ">>> Config 2: NO CLIMATE SI <<<"
echo "$(date)"
export FVS_CLIMATE_SI=false
export FVS_SDIMAX=true
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_DG_BACKTRANSFORM=median
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_no_csi
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_no_csi_console.log

# -------------------------------------------------------------------
# Config 3: No SDIMAX
# -------------------------------------------------------------------
echo ""
echo ">>> Config 3: NO SDIMAX <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=false
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_DG_BACKTRANSFORM=median
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_no_sdi
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_no_sdi_console.log

# -------------------------------------------------------------------
# Config 4: No Ingrowth
# -------------------------------------------------------------------
echo ""
echo ">>> Config 4: NO INGROWTH <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=true
export FVS_INGROWTH=false
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_DG_BACKTRANSFORM=median
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_no_ig
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_no_ig_console.log

# -------------------------------------------------------------------
# Config 5: Baseline (Bayes only, no refinements)
# -------------------------------------------------------------------
echo ""
echo ">>> Config 5: BASELINE (Bayes only, median DG) <<<"
echo "$(date)"
export FVS_CLIMATE_SI=false
export FVS_SDIMAX=false
export FVS_INGROWTH=false
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_DG_BACKTRANSFORM=median
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_baseline
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_baseline_console.log

# -------------------------------------------------------------------
# Config 6: Baskerville mean DG (for comparison with v2 approach)
# -------------------------------------------------------------------
echo ""
echo ">>> Config 6: BASKERVILLE MEAN DG (v2 equivalent) <<<"
echo "$(date)"
export FVS_CLIMATE_SI=true
export FVS_SDIMAX=true
export FVS_INGROWTH=true
export FVS_CI_BRACKETS=false
export FVS_BOOTSTRAP=false
export FVS_DG_BACKTRANSFORM=mean
export FVS_DG_VARIANT_MULT=false
export FVS_OUTPUT_TAG=ablation_baskerville
Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_baskerville_console.log

# -------------------------------------------------------------------
# Config 7: Full + Variant DG multiplier (empirical bias correction)
# SKIP on first pass: multipliers were calibrated against Baskerville
# predictions. After configs 1-5 finish, recompute multipliers from
# the new BA %Bias and update DG_VARIANT_MULT in 19_fia_benchmark_engine.R,
# then resubmit this config only.
# -------------------------------------------------------------------
# echo ""
# echo ">>> Config 7: FULL + VARIANT DG MULTIPLIER <<<"
# echo "$(date)"
# export FVS_CLIMATE_SI=true
# export FVS_SDIMAX=true
# export FVS_INGROWTH=true
# export FVS_CI_BRACKETS=false
# export FVS_BOOTSTRAP=false
# export FVS_DG_BACKTRANSFORM=median
# export FVS_DG_VARIANT_MULT=true
# export FVS_OUTPUT_TAG=ablation_dgmult
# Rscript R/19_fia_benchmark_engine.R 2>&1 | tee logs/ablation_v3_dgmult_console.log

echo ""
echo "=========================================="
echo "ABLATION STUDY v3 COMPLETE"
echo "$(date)"
echo "=========================================="
echo ""
echo "Next: run comparison table"
echo "  Rscript R/21_ablation_comparison.R"

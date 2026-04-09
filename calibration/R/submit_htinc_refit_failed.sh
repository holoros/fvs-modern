#!/bin/bash
#SBATCH --job-name=fvs_htinc_fix
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=04:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/htinc_refit_fix_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/htinc_refit_fix_%j.err

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_MAX_OBS=30000

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

# Refit ONLY the variants that failed due to uncaught MAP error (CI, IE)
VARIANTS="ci ie"

for VARIANT in $VARIANTS; do
  echo "=========================================="
  echo "Refitting height increment for variant: $VARIANT"
  echo "$(date)"
  echo "=========================================="
  Rscript R/03b_fit_height_increment.R --variant $VARIANT 2>&1
  echo ""
done

echo "Height increment refit (failed variants) complete: $(date)"

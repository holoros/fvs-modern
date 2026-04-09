#!/bin/bash
#SBATCH --job-name=fvs_htinc_refit
#SBATCH --account=PUOM0008
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=24:00:00
#SBATCH --output=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/htinc_refit_%j.out
#SBATCH --error=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/logs/htinc_refit_%j.err

module purge
module load gcc/12.3.0 R/4.4.0

export FVS_PROJECT_ROOT="${FVS_PROJECT_ROOT:-/path/to/fvs-modern}"
export FVS_MAX_OBS=30000

cd ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration

# Refit height increment model for ALL 25 FVS variants
# Variants with HG config priors get informative priors; others use weakly informative
VARIANTS="ak bm ca ci cr cs ec em ie ls nc ne pn sn so tt ut wc ws acd bc kt oc on op"

for VARIANT in $VARIANTS; do
  echo "=========================================="
  echo "Fitting height increment for variant: $VARIANT"
  echo "$(date)"
  echo "=========================================="
  Rscript R/03b_fit_height_increment.R --variant $VARIANT 2>&1
  echo ""
done

echo "All height increment refits complete: $(date)"

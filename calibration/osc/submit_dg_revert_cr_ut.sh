#!/bin/bash
#SBATCH --job-name=fvs_dg_revert
#SBATCH --account=PUOM0008
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --array=0-1
#SBATCH --output=fvs-modern/calibration/slurm/dg_revert_%A_%a.out
#SBATCH --error=fvs-modern/calibration/slurm/dg_revert_%A_%a.err

# Revert CR and UT to 5K obs (10K refits degraded R-squared)
# CR: 0.414 -> 0.124, UT: 0.397 -> 0.091 at 10K
# Restoring with original 5K subsample

module load R/4.3.0
module load gnu/12.3.0

VARIANTS=(cr ut)
VARIANT=${VARIANTS[$SLURM_ARRAY_TASK_ID]}

export FVS_PROJECT_ROOT=${FVS_PROJECT_ROOT:-/path/to/fvs-modern}
export FVS_MAX_OBS=5000
export CMDSTAN=/path/to/user/path

cd $FVS_PROJECT_ROOT

echo "=== Reverting DG for variant: $VARIANT at 5K obs ==="
echo "Start: $(date)"

Rscript calibration/R/02c_fit_dg_hmc_small.R --variant $VARIANT

echo "Finish: $(date)"
echo "=== Done ==="

#!/bin/bash
# =============================================================================
# Evaluation suite runner for FVS-CONUS
# =============================================================================
# Submits one SLURM job per evaluation script. The local-only scripts (10, 40)
# can be run directly without SLURM; the cluster-only scripts (20, 30, 50)
# need real memory.
#
# Usage:
#   bash calibration/slurm/eval_runner.sh local       # run 10, 40 here on login node
#   bash calibration/slurm/eval_runner.sh cluster     # sbatch 20, 30, 50 to compute
#   bash calibration/slurm/eval_runner.sh all
# =============================================================================

set -uo pipefail

PROJ_ROOT="/users/PUOM0008/crsfaaron/fvs-modern"
cd "$PROJ_ROOT"

run_local_scripts() {
  module load gcc/12.3.0 R/4.4.0
  echo ">> Running 10_convergence_dashboard.R (local)"
  Rscript --vanilla calibration/R/eval/10_convergence_dashboard.R

  echo ">> Running 40_trait_coefficients.R (local)"
  Rscript --vanilla calibration/R/eval/40_trait_coefficients.R
}

submit_cluster_residuals() {
  for spec in "hg b2" "hg b1" "dg_kue b2" "dg_kue b1" \
              "htdbh b2" "cr b2" "hcb b2" "mort b2"; do
    set -- $spec
    MODEL=$1; VARIANT=$2
    mkdir -p logs
    cat > /tmp/eval_resid_${MODEL}_${VARIANT}.sh << EOF
#!/bin/bash
#SBATCH --job-name=eval_resid_${MODEL}_${VARIANT}
#SBATCH --time=02:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --account=PUOM0008
#SBATCH --output=${PROJ_ROOT}/logs/eval_resid_${MODEL}_${VARIANT}_%j.out
#SBATCH --error=${PROJ_ROOT}/logs/eval_resid_${MODEL}_${VARIANT}_%j.err

module load gcc/12.3.0 R/4.4.0
cd ${PROJ_ROOT}
Rscript --vanilla calibration/R/eval/20_residual_diagnostics.R \\
  --model=${MODEL} --variant=${VARIANT} --subsample=50000
EOF
    echo ">> sbatch eval_resid_${MODEL}_${VARIANT}"
    sbatch /tmp/eval_resid_${MODEL}_${VARIANT}.sh
  done
}

submit_cluster_loo() {
  for RESPONSE in hg dg_kue; do
    cat > /tmp/eval_loo_${RESPONSE}.sh << EOF
#!/bin/bash
#SBATCH --job-name=eval_loo_${RESPONSE}
#SBATCH --time=04:00:00
#SBATCH --mem=128G
#SBATCH --cpus-per-task=8
#SBATCH --account=PUOM0008
#SBATCH --output=${PROJ_ROOT}/logs/eval_loo_${RESPONSE}_%j.out
#SBATCH --error=${PROJ_ROOT}/logs/eval_loo_${RESPONSE}_%j.err

module load gcc/12.3.0 R/4.4.0
cd ${PROJ_ROOT}
Rscript --vanilla calibration/R/eval/30_loo_comparison.R --response=${RESPONSE}
EOF
    echo ">> sbatch eval_loo_${RESPONSE}"
    sbatch /tmp/eval_loo_${RESPONSE}.sh
  done
}

submit_cluster_ppc() {
  for spec in "hg b1" "hg b2" "dg_kue b1" "dg_kue b2"; do
    set -- $spec
    MODEL=$1; VARIANT=$2
    cat > /tmp/eval_ppc_${MODEL}_${VARIANT}.sh << EOF
#!/bin/bash
#SBATCH --job-name=eval_ppc_${MODEL}_${VARIANT}
#SBATCH --time=01:00:00
#SBATCH --mem=48G
#SBATCH --cpus-per-task=4
#SBATCH --account=PUOM0008
#SBATCH --output=${PROJ_ROOT}/logs/eval_ppc_${MODEL}_${VARIANT}_%j.out
#SBATCH --error=${PROJ_ROOT}/logs/eval_ppc_${MODEL}_${VARIANT}_%j.err

module load gcc/12.3.0 R/4.4.0
cd ${PROJ_ROOT}
Rscript --vanilla calibration/R/eval/50_posterior_predictive.R \\
  --model=${MODEL} --variant=${VARIANT}
EOF
    echo ">> sbatch eval_ppc_${MODEL}_${VARIANT}"
    sbatch /tmp/eval_ppc_${MODEL}_${VARIANT}.sh
  done
}

case "${1:-all}" in
  local)
    run_local_scripts
    ;;
  cluster)
    submit_cluster_residuals
    submit_cluster_loo
    submit_cluster_ppc
    ;;
  all)
    run_local_scripts
    submit_cluster_residuals
    submit_cluster_loo
    submit_cluster_ppc
    ;;
  *)
    echo "Usage: $0 {local|cluster|all}"
    exit 1
    ;;
esac

echo ""
echo "Once all jobs finish, results are in:"
echo "  ${PROJ_ROOT}/calibration/output/evaluation/"

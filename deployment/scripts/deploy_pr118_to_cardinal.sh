#!/usr/bin/env bash
# =========================================================================
# deploy_pr118_to_cardinal.sh
# Pushes the PR #118-synced src-converted/ tree to Cardinal, rebuilds
# .so libraries there, and optionally resubmits uncertainty jobs.
#
# Run from your local fvs-modern checkout AFTER the sync has been
# committed and tested locally.
#
# Usage:
#   ./deploy_pr118_to_cardinal.sh [--rebuild] [--smoke-test]
# =========================================================================

set -euo pipefail

CARDINAL_USER="crsfaaron"
CARDINAL_HOST="cardinal.osc.edu"
CARDINAL_ROOT="/users/PUOM0008/${CARDINAL_USER}/fvs-modern"
LOCAL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

DO_REBUILD=false
DO_SMOKE=false
for arg in "$@"; do
    case "$arg" in
        --rebuild)    DO_REBUILD=true ;;
        --smoke-test) DO_SMOKE=true ;;
    esac
done

echo "=== Deploying PR #118 sync to Cardinal ==="
echo "  Local:  $LOCAL_ROOT"
echo "  Remote: ${CARDINAL_USER}@${CARDINAL_HOST}:${CARDINAL_ROOT}"
echo ""

# Rsync the Fortran tree and build scripts. Exclude lib/ (built on Cardinal).
rsync -avz --delete \
    --exclude='lib/' \
    --exclude='__pycache__/' \
    --exclude='*.pyc' \
    --exclude='.git/' \
    "${LOCAL_ROOT}/src-converted/" \
    "${CARDINAL_USER}@${CARDINAL_HOST}:${CARDINAL_ROOT}/src-converted/"

rsync -avz \
    "${LOCAL_ROOT}/deployment/scripts/build_fvs_libraries.sh" \
    "${LOCAL_ROOT}/deployment/scripts/run_regression_tests.sh" \
    "${CARDINAL_USER}@${CARDINAL_HOST}:${CARDINAL_ROOT}/deployment/scripts/"

if $DO_REBUILD; then
    echo ""
    echo ">>> Triggering remote rebuild..."
    ssh "${CARDINAL_USER}@${CARDINAL_HOST}" "
        cd ${CARDINAL_ROOT} &&
        module load gcc/11 2>/dev/null || true &&
        mkdir -p lib &&
        bash deployment/scripts/build_fvs_libraries.sh src-converted lib ne ie ls ak bm ca ci cr cs ec em kt nc oc op pn sn so tt ut wc ws 2>&1 | tail -40
    "
fi

if $DO_SMOKE; then
    echo ""
    echo ">>> Running Python ctypes smoke test on Cardinal..."
    ssh "${CARDINAL_USER}@${CARDINAL_HOST}" "
        cd ${CARDINAL_ROOT} &&
        python3 -c \"
import ctypes, pathlib
for so in sorted(pathlib.Path('lib').glob('FVS*.so')):
    try:
        ctypes.CDLL(str(so.resolve()), mode=1)
        print(f'  OK  {so.name}')
    except Exception as e:
        print(f'  FAIL {so.name}: {str(e)[:70]}')
\"
    "
fi

echo ""
echo "=== Done ==="
echo ""
echo "Next steps (if not already triggered):"
echo "  1. ssh ${CARDINAL_HOST}"
echo "  2. cd ${CARDINAL_ROOT}"
echo "  3. bash deployment/scripts/build_fvs_libraries.sh src-converted lib"
echo "  4. sbatch calibration/osc/submit_perseus_uncertainty.sh"

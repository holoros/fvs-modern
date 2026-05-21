#!/bin/bash
# link_variant_samples.sh
# Worktree convenience: make calibration/output/variants/<v>/ resolve the
# heavy _samples.rds files that live in the canonical fvs-conus workspace.
#
# Background: git worktrees of fvs-modern only carry the small .csv summaries
# under calibration/output/variants/. The Bayesian posterior _samples.rds
# files (60+ MB each) live in /users/PUOM0008/crsfaaron/fvs-conus/output/variants/.
# Without them, calibration/R/19_fia_benchmark_engine.R loads params$dg = NULL
# for every variant and the calibrated A/B produces all-NA predictions.
#
# This was the root cause of the rounds 10-15 calibration debugging detour.
# Run this once after creating a fresh worktree, before any A/B chain.
#
# Usage: bash calibration/slurm/link_variant_samples.sh [CONUS_DIR]

set -uo pipefail
CONUS=${1:-/users/PUOM0008/crsfaaron/fvs-conus/output/variants}
PROJ=${FVS_PROJECT_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}
DEST=$PROJ/calibration/output/variants

if [ ! -d "$CONUS" ]; then
  echo "ERROR: CONUS variants dir not found: $CONUS" >&2
  exit 1
fi

echo "Linking _samples.rds from $CONUS into $DEST"
linked=0
for vdir in "$CONUS"/*/; do
  v=$(basename "$vdir")
  mkdir -p "$DEST/$v"
  for rds in "$vdir"*_samples.rds "$vdir"standardization_params.rds; do
    [ -f "$rds" ] || continue
    fn=$(basename "$rds")
    target="$DEST/$v/$fn"
    # Only link if the worktree does not already have a real file
    if [ ! -e "$target" ] || [ -L "$target" ]; then
      ln -sf "$rds" "$target"
      linked=$((linked+1))
    fi
  done
done
echo "Linked/refreshed $linked sample files across $(ls -d "$CONUS"/*/ | wc -l) variants"

# ACD special case: it has its own crown_ratio/mortality/species CSVs from the
# HMC refit, but no DG/HD samples.rds of its own. Borrow NE's so the calibrated
# path has a complete param set (the ACD-uses-NE fallback handles the rest).
if [ -d "$DEST/acd" ] && [ ! -e "$DEST/acd/diameter_growth_samples.rds" ]; then
  ln -sf "$CONUS/ne/diameter_growth_samples.rds" "$DEST/acd/diameter_growth_samples.rds"
  ln -sf "$CONUS/ne/height_diameter_samples.rds" "$DEST/acd/height_diameter_samples.rds"
  echo "ACD: linked NE diameter_growth + height_diameter samples (NE fallback)"
fi
echo "Done."

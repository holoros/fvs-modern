#!/bin/bash
# Bakuzis FIA post-array wrapper
#
# Runs the aggregator on the latest output, then prints summary
# statistics (and tar's the three summary CSVs for fast scp pull).
#
# Usage on Cardinal:
#   cd ~/fvs-modern && bash calibration/python/run_bakuzis_post_array.sh
#
# Author: A. Weiskittel
# Date: 2026-04-25
set -euo pipefail

cd "$(dirname "$0")/../.."   # repo root
module load python/3.12 2>/dev/null || true

OUTDIR="calibration/output/bakuzis"
TARNAME="bakuzis_fia_summaries.tar.gz"

echo "=== Aggregating Bakuzis ensemble ==="
python3 calibration/python/bakuzis_uncertainty_aggregate.py \
    --input-dir "$OUTDIR" \
    --output-dir "$OUTDIR"

echo ""
echo "=== Summary CSV sizes ==="
ls -la "$OUTDIR"/bakuzis_uncertainty_summary_long.csv \
       "$OUTDIR"/bakuzis_benchmark_wide.csv \
       "$OUTDIR"/bakuzis_laws_compliance.csv

echo ""
echo "=== Bakuzis law compliance (FIA-mode) ==="
python3 -c "
import pandas as pd
df = pd.read_csv('$OUTDIR/bakuzis_laws_compliance.csv')
print(df.to_string(index=False, float_format='{:.3f}'.format))
"

echo ""
echo "=== Tarball for scp ==="
tar -czf "$OUTDIR/$TARNAME" \
    -C "$OUTDIR" \
    bakuzis_uncertainty_summary_long.csv \
    bakuzis_benchmark_wide.csv \
    bakuzis_laws_compliance.csv
ls -la "$OUTDIR/$TARNAME"
echo ""
echo "Pull via:"
echo "  scp crsfaaron@cardinal.osc.edu:fvs-modern/$OUTDIR/$TARNAME /tmp/"

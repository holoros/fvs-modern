#!/usr/bin/env bash
# diagnose_acd_cardinal.sh
#
# Diagnostic helper for the ACD variant build failure on cardinal.osc.edu.
# Captures everything we need to decide whether the Cardinal source tree,
# the CMake include path, or the compiler (ifort vs gfortran) is at fault.
#
# Run this script from the fvs-modern repo root on cardinal. It writes a
# timestamped report under /tmp that can be scp'd back for review.
#
# Usage:
#   bash deployment/scripts/diagnose_acd_cardinal.sh [workspace-ref-root]
#
# workspace-ref-root (optional) is a path to a known-good copy of src-converted
# (e.g. a rsync'd mirror of the laptop tree). If provided, diffs are produced.

set -u
set -o pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
REF_ROOT="${1:-}"
STAMP="$(date +%Y%m%d_%H%M%S)"
OUT="/tmp/acd_cardinal_diag_${STAMP}"
mkdir -p "$OUT"

echo "ACD Cardinal diagnostic report" | tee "$OUT/summary.txt"
echo "Generated: $(date -Iseconds)" | tee -a "$OUT/summary.txt"
echo "Repo root: $REPO_ROOT" | tee -a "$OUT/summary.txt"
echo "Reference root: ${REF_ROOT:-<none provided>}" | tee -a "$OUT/summary.txt"
echo "Host: $(hostname)" | tee -a "$OUT/summary.txt"
echo "Commit: $(git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null || echo n/a)" | tee -a "$OUT/summary.txt"
echo "" | tee -a "$OUT/summary.txt"

# 1. Compiler inventory
{
  echo "=== Compiler inventory ==="
  for cc in gfortran ifort ifx gcc; do
    if command -v "$cc" >/dev/null 2>&1; then
      echo "-- $cc --"
      "$cc" --version 2>&1 | head -3
    else
      echo "-- $cc : not found --"
    fi
  done
  echo ""
} | tee -a "$OUT/summary.txt"

# 2. Loaded modules (OSC Lmod)
{
  echo "=== Loaded modules ==="
  if command -v module >/dev/null 2>&1; then
    module list 2>&1
  else
    echo "module command not available"
  fi
  echo ""
} | tee -a "$OUT/summary.txt"

# 3. MAXSP audit in DATA statements
ACD="$REPO_ROOT/src-converted/acd"
{
  echo "=== MAXSP audit (src-converted/acd) ==="
  for f in blkdat.f90 crown.f90; do
    if [[ -f "$ACD/$f" ]]; then
      lines=$(grep -c '^' "$ACD/$f")
      echo "$f : exists, $lines lines, sha256=$(sha256sum "$ACD/$f" | awk '{print $1}')"
    else
      echo "$f : MISSING"
    fi
  done
  echo ""
  echo "PRGPRM.f90 candidates on include path:"
  find "$REPO_ROOT/src-converted" -name 'PRGPRM.f90' -printf '%p  sha256='  -exec sha256sum {} \; 2>/dev/null \
    | awk '{print $1, $3}'
  echo ""
} | tee -a "$OUT/summary.txt"

# 4. Count DATA values per MAXSP-dimensioned array (sanity: must equal 108 for ACD)
#    This walks each file line-by-line; crude but portable.
count_data_values() {
  local file="$1"
  python3 - "$file" <<'PY'
import re, sys, pathlib
path = pathlib.Path(sys.argv[1])
if not path.exists():
    print(f"{path.name}: missing")
    sys.exit(0)
text = path.read_text()
# Stitch Fortran continuation lines: join lines ending with & or starting with &
stitched = re.sub(r'&\s*\n\s*&?', '', text)
# Find DATA statements: DATA name /v1, v2, ... /
for m in re.finditer(r'(?im)^\s*data\s+([A-Z0-9_]+)\s*/(.*?)/', stitched):
    name, payload = m.group(1), m.group(2)
    # Split on commas, counting "k*value" forms as k items
    count = 0
    for tok in payload.split(','):
        tok = tok.strip()
        if not tok:
            continue
        if '*' in tok:
            try:
                k = int(tok.split('*', 1)[0].strip())
                count += k
            except ValueError:
                count += 1
        else:
            count += 1
    print(f"{path.name} :: DATA {name} -> {count} values")
PY
}

{
  echo "=== DATA value counts (expect 108 for MAXSP arrays in ACD) ==="
  count_data_values "$ACD/blkdat.f90"
  count_data_values "$ACD/crown.f90"
  echo ""
} | tee -a "$OUT/summary.txt"

# 5. Diff against reference tree if provided
if [[ -n "$REF_ROOT" && -d "$REF_ROOT/acd" ]]; then
  {
    echo "=== Diff against reference tree ($REF_ROOT/acd) ==="
    diff -rq "$REF_ROOT/acd" "$ACD" 2>&1 | tee "$OUT/acd_tree_diff.txt"
    for f in blkdat.f90 crown.f90; do
      if [[ -f "$REF_ROOT/acd/$f" && -f "$ACD/$f" ]]; then
        echo "--- unified diff: $f ---"
        diff -u "$REF_ROOT/acd/$f" "$ACD/$f" > "$OUT/${f%.f90}_diff.patch" || true
        wc -l < "$OUT/${f%.f90}_diff.patch" | awk -v f="$f" '{print f" diff lines: "$1}'
      fi
    done
    echo ""
  } | tee -a "$OUT/summary.txt"
fi

# 6. Standalone compile attempts (mirror CMake flags where possible)
compile_test() {
  local compiler="$1"
  local flags="$2"
  local file="$3"
  local label="$4"
  local log="$OUT/compile_${label}.log"
  echo "--- $compiler $flags -c $file ---" | tee -a "$OUT/summary.txt"
  if command -v "$compiler" >/dev/null 2>&1; then
    ( cd "$ACD" && "$compiler" $flags -c "$file" -I"$REPO_ROOT/src-converted/base/common" -I"$ACD/common" ) \
      >"$log" 2>&1
    rc=$?
    echo "exit=$rc  log=$log  (tail below)" | tee -a "$OUT/summary.txt"
    tail -15 "$log" | sed 's/^/    /' | tee -a "$OUT/summary.txt"
  else
    echo "compiler $compiler not available" | tee -a "$OUT/summary.txt"
  fi
  echo "" | tee -a "$OUT/summary.txt"
}

{
  echo "=== Standalone compile smoke tests ==="
} | tee -a "$OUT/summary.txt"

compile_test gfortran "-ffree-form -std=f2008 -fimplicit-none -Wall" "blkdat.f90" "gfortran_blkdat"
compile_test gfortran "-ffree-form -std=f2008 -fimplicit-none -Wall" "crown.f90"  "gfortran_crown"
compile_test ifort    "-free -warn all -O0"                          "blkdat.f90" "ifort_blkdat"
compile_test ifort    "-free -warn all -O0"                          "crown.f90"  "ifort_crown"
compile_test ifx      "-free -warn all -O0"                          "blkdat.f90" "ifx_blkdat"
compile_test ifx      "-free -warn all -O0"                          "crown.f90"  "ifx_crown"

# 7. CMake cache inspection if a build dir exists
{
  echo "=== CMake build artifacts ==="
  for bd in "$REPO_ROOT/build" "$REPO_ROOT/build-cardinal" "$REPO_ROOT/cmake-build"; do
    if [[ -d "$bd" ]]; then
      echo "-- $bd --"
      [[ -f "$bd/CMakeCache.txt" ]] && grep -E 'CMAKE_Fortran_COMPILER|CMAKE_BUILD_TYPE|Fortran_FLAGS' "$bd/CMakeCache.txt" | head -20
      [[ -f "$bd/CMakeFiles/CMakeError.log" ]] && tail -40 "$bd/CMakeFiles/CMakeError.log" > "$OUT/cmake_error_tail.log"
    fi
  done
  echo ""
} | tee -a "$OUT/summary.txt"

echo ""
echo "Diagnostic complete. Bundle:"
echo "  tar czf ${OUT}.tar.gz -C /tmp $(basename "$OUT")"
tar czf "${OUT}.tar.gz" -C /tmp "$(basename "$OUT")"
echo "  scp cardinal.osc.edu:${OUT}.tar.gz ."

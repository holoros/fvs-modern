#!/usr/bin/env bash
# ==========================================================================
# sync_upstream.sh
# Pulls upstream USFS changes, applies fvs-modern patches, converts new
# Fortran files to free form, and optionally rebuilds all variants.
#
# Usage:
#   ./sync_upstream.sh [OPTIONS]
#
# Options:
#   --pull          Download latest USFS source from GitHub
#   --patch         Apply fvs-modern patches (FORMAT fixes, etc.)
#   --convert       Convert new/changed .f files to .f90 free form
#   --rebuild       Rebuild all variant .so libraries
#   --test          Run regression tests after rebuild
#   --all           Equivalent to --pull --patch --convert --rebuild --test
#   --dry-run       Show what would be done without doing it
#   --fvs-src DIR   Path to FVS source tree (default: auto-detect)
#   --modern-dir DIR Path to fvs-modern directory
#
# The typical update workflow:
#   1. ./check_upstream.sh           (detect what changed)
#   2. ./sync_upstream.sh --all      (pull, patch, convert, rebuild, test)
#   3. Review test results
#   4. Update sync_state.json with new baseline
# ==========================================================================

set -euo pipefail

# Defaults
DO_PULL=false
DO_PATCH=false
DO_CONVERT=false
DO_REBUILD=false
DO_TEST=false
DRY_RUN=false
FVS_SRC=""
MODERN_DIR=""
SYNC_FILE="${HOME}/.fvs-modern/upstream_sync.json"

FVS_REPO="https://github.com/USDAForestService/ForestVegetationSimulator.git"
IFACE_REPO="https://github.com/USDAForestService/ForestVegetationSimulator-Interface.git"

# All 22 US variants
VARIANTS="ak bm ca ci cr cs ec em ie kt ls nc ne oc op pn sn so tt ut wc ws"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --pull)      DO_PULL=true; shift ;;
        --patch)     DO_PATCH=true; shift ;;
        --convert)   DO_CONVERT=true; shift ;;
        --rebuild)   DO_REBUILD=true; shift ;;
        --test)      DO_TEST=true; shift ;;
        --all)       DO_PULL=true; DO_PATCH=true; DO_CONVERT=true; DO_REBUILD=true; DO_TEST=true; shift ;;
        --dry-run)   DRY_RUN=true; shift ;;
        --fvs-src)   FVS_SRC="$2"; shift 2 ;;
        --modern-dir) MODERN_DIR="$2"; shift 2 ;;
        --help)
            head -25 "$0" | grep "^#" | sed 's/^# *//'
            exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Auto detect FVS source
if [ -z "$FVS_SRC" ]; then
    for candidate in \
        "$HOME/ForestVegetationSimulator-main" \
        "$HOME/ForestVegetationSimulator" \
        "$HOME/fvs-source" \
        "/opt/fvs/source"; do
        if [ -d "$candidate/vbase" ]; then
            FVS_SRC="$candidate"
            break
        fi
    done
fi

if [ -z "$FVS_SRC" ]; then
    echo "ERROR: Cannot find FVS source directory. Use --fvs-src PATH"
    exit 1
fi

echo "=== FVS Upstream Sync ==="
echo "  FVS source:  $FVS_SRC"
echo "  Modern dir:  ${MODERN_DIR:-(not set)}"
echo "  Operations:  pull=$DO_PULL patch=$DO_PATCH convert=$DO_CONVERT rebuild=$DO_REBUILD test=$DO_TEST"
echo ""

# =========================================================================
# STEP 1: Pull upstream
# =========================================================================
if $DO_PULL; then
    echo ">>> Step 1: Pulling upstream source..."

    BACKUP_DIR="${FVS_SRC}_backup_$(date +%Y%m%d)"

    if $DRY_RUN; then
        echo "  [DRY RUN] Would backup $FVS_SRC to $BACKUP_DIR"
        echo "  [DRY RUN] Would download latest from $FVS_REPO"
    else
        # Create backup of current source
        if [ ! -d "$BACKUP_DIR" ]; then
            echo "  Creating backup: $BACKUP_DIR"
            cp -a "$FVS_SRC" "$BACKUP_DIR"
        fi

        # If it's already a git repo, just pull
        if [ -d "$FVS_SRC/.git" ]; then
            echo "  Pulling latest from origin..."
            cd "$FVS_SRC" && git pull origin main
        else
            # Download as zip and overlay
            echo "  Downloading latest source archive..."
            TMPDIR=$(mktemp -d)
            curl -sL "https://github.com/USDAForestService/ForestVegetationSimulator/archive/refs/heads/main.zip" \
                -o "$TMPDIR/fvs-latest.zip"
            cd "$TMPDIR" && unzip -q fvs-latest.zip

            # Identify changed files
            echo "  Comparing with current source..."
            CHANGED=0
            NEW=0
            for f in $(find ForestVegetationSimulator-main -name "*.f" -o -name "*.F" -o -name "*.f90" -o -name "*.inc" | sort); do
                relpath="${f#ForestVegetationSimulator-main/}"
                target="$FVS_SRC/$relpath"
                if [ -f "$target" ]; then
                    if ! diff -q "$f" "$target" > /dev/null 2>&1; then
                        echo "    [CHANGED] $relpath"
                        cp "$f" "$target"
                        CHANGED=$((CHANGED + 1))
                    fi
                else
                    echo "    [NEW]     $relpath"
                    mkdir -p "$(dirname "$target")"
                    cp "$f" "$target"
                    NEW=$((NEW + 1))
                fi
            done

            echo "  Updated: $CHANGED changed, $NEW new files"
            rm -rf "$TMPDIR"
        fi
    fi
    echo ""
fi

# =========================================================================
# STEP 2: Apply fvs-modern patches
# =========================================================================
if $DO_PATCH; then
    echo ">>> Step 2: Applying fvs-modern patches..."
    # Note: FORMAT-descriptor comma fixes (A8' -> A8,' etc.) were folded
    # into upstream via PR #118 (SDI reporting update, 2026-03-06, commit
    # 66a5e14e). This step is now a no-op, retained as a hook for future
    # local patches that cannot be upstreamed.
    echo "  (no local patches currently required)"
    echo ""
fi

# =========================================================================
# STEP 3: Convert changed files to free form (.f90)
# =========================================================================
if $DO_CONVERT; then
    echo ">>> Step 3: Converting source to free form..."

    if [ -z "$MODERN_DIR" ]; then
        echo "  SKIPPED: --modern-dir not set. Provide path to fvs-modern/src-converted/"
    elif $DRY_RUN; then
        echo "  [DRY RUN] Would convert changed .f files to .f90 in $MODERN_DIR"
    else
        # Find all .f source files and convert any that are newer than their .f90 counterpart
        CONVERTED=0
        for srcf in $(find "$FVS_SRC" -name "*.f" -not -path "*/bin/*" | sort); do
            relpath="${srcf#$FVS_SRC/}"
            f90path="$MODERN_DIR/${relpath%.f}.f90"

            # Convert if .f90 doesn't exist or is older than .f
            if [ ! -f "$f90path" ] || [ "$srcf" -nt "$f90path" ]; then
                mkdir -p "$(dirname "$f90path")"
                # Simple fixed to free form conversion
                python3 -c "
import sys
with open('$srcf', 'r', errors='replace') as f:
    lines = f.readlines()
out = []
for line in lines:
    stripped = line.rstrip()
    if len(stripped) == 0:
        out.append('')
        continue
    # Comment lines
    if stripped[0] in 'cC*!':
        out.append('!' + stripped[1:])
        continue
    # Continuation lines (col 6 non-blank, non-zero)
    if len(stripped) > 5 and stripped[5] not in ' 0' and stripped[0] == ' ':
        out.append('& ' + stripped[6:].lstrip())
        continue
    # Regular lines: strip column 1-6 label area
    label = stripped[:5].strip() if len(stripped) > 5 else ''
    code = stripped[6:] if len(stripped) > 6 else stripped
    if label:
        out.append(label + ' ' + code)
    else:
        out.append(code)
with open('$f90path', 'w') as f:
    f.write('\n'.join(out) + '\n')
" 2>/dev/null && CONVERTED=$((CONVERTED + 1))
            fi
        done
        echo "  Converted $CONVERTED files"
    fi
    echo ""
fi

# =========================================================================
# STEP 4: Rebuild all variants
# =========================================================================
if $DO_REBUILD; then
    echo ">>> Step 4: Rebuilding variant shared libraries..."

    if $DRY_RUN; then
        echo "  [DRY RUN] Would rebuild $VARIANTS"
    else
        cd "$FVS_SRC"
        BUILT=0
        FAILED=0
        for v in $VARIANTS; do
            printf "  Building FVS%-4s ... " "$v"
            if OSTYPE=linux-gnu make "FVS${v}.so" > /dev/null 2>&1; then
                echo "OK"
                BUILT=$((BUILT + 1))
            else
                echo "FAIL"
                FAILED=$((FAILED + 1))
            fi
        done
        echo ""
        echo "  Built: $BUILT  Failed: $FAILED"
    fi
    echo ""
fi

# =========================================================================
# STEP 5: Run regression tests
# =========================================================================
if $DO_TEST; then
    echo ">>> Step 5: Running regression tests..."

    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    TEST_SCRIPT="$SCRIPT_DIR/run_regression_tests.sh"

    if [ ! -f "$TEST_SCRIPT" ]; then
        echo "  WARNING: Test script not found at $TEST_SCRIPT"
    elif $DRY_RUN; then
        echo "  [DRY RUN] Would run: bash $TEST_SCRIPT $FVS_SRC/bin $FVS_SRC/tests"
    else
        bash "$TEST_SCRIPT" "$FVS_SRC/bin" "$FVS_SRC/tests"
    fi
    echo ""
fi

# =========================================================================
# Update sync state
# =========================================================================
if ! $DRY_RUN; then
    python3 << PYEOF
import json
from datetime import datetime
try:
    with open('$SYNC_FILE', 'r') as f:
        data = json.load(f)
except:
    data = {}
data['last_sync'] = datetime.now().strftime('%Y-%m-%d %H:%M')
data['operations'] = 'pull=$DO_PULL patch=$DO_PATCH convert=$DO_CONVERT rebuild=$DO_REBUILD test=$DO_TEST'
with open('$SYNC_FILE', 'w') as f:
    json.dump(data, f, indent=2)
PYEOF
fi

echo "=== Sync complete ==="

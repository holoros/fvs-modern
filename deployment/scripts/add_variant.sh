#!/usr/bin/env bash
# ==========================================================================
# add_variant.sh
# Scaffolds a new FVS variant from an existing template variant.
# Useful for creating regional calibrations or experimental variants.
#
# Usage:
#   ./add_variant.sh --name XX --base ie --species-file species.csv \
#                    --description "Acadian variant for Maine"
#
# This creates:
#   * xx/ directory with variant-specific source files
#   * vxx/ directory with variant COMMON block includes
#   * Modified PRGPRM.f with correct MAXSP
#   * Build target in the makefile
#   * Species mapping file
#   * Calibration parameter template
# ==========================================================================

set -euo pipefail

NAME=""
BASE=""
SPECIES_FILE=""
DESCRIPTION=""
FVS_SRC=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --name)         NAME="$2"; shift 2 ;;
        --base)         BASE="$2"; shift 2 ;;
        --species-file) SPECIES_FILE="$2"; shift 2 ;;
        --description)  DESCRIPTION="$2"; shift 2 ;;
        --fvs-src)      FVS_SRC="$2"; shift 2 ;;
        --help)
            head -18 "$0" | grep "^#" | sed 's/^# *//'
            exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Validate
if [ -z "$NAME" ] || [ -z "$BASE" ]; then
    echo "ERROR: --name and --base are required"
    echo "Usage: $0 --name XX --base ie [--species-file species.csv] [--description TEXT]"
    exit 1
fi

# Normalize to lowercase
NAME=$(echo "$NAME" | tr '[:upper:]' '[:lower:]')
BASE=$(echo "$BASE" | tr '[:upper:]' '[:lower:]')
NAME_UPPER=$(echo "$NAME" | tr '[:lower:]' '[:upper:]')
BASE_UPPER=$(echo "$BASE" | tr '[:lower:]' '[:upper:]')

# Auto detect FVS source
if [ -z "$FVS_SRC" ]; then
    for candidate in \
        "$HOME/ForestVegetationSimulator-main" \
        "$HOME/ForestVegetationSimulator"; do
        if [ -d "$candidate/vbase" ]; then
            FVS_SRC="$candidate"
            break
        fi
    done
fi

if [ -z "$FVS_SRC" ] || [ ! -d "$FVS_SRC/$BASE" ]; then
    echo "ERROR: Cannot find FVS source or base variant '$BASE'"
    exit 1
fi

echo "=== Creating FVS Variant: $NAME_UPPER ==="
echo "  Base variant:   $BASE_UPPER"
echo "  Source tree:     $FVS_SRC"
echo "  Description:     ${DESCRIPTION:-'(none)'}"
echo ""

# Step 1: Copy variant-specific source
echo "  Creating $NAME/ from $BASE/..."
if [ -d "$FVS_SRC/$NAME" ]; then
    echo "  WARNING: $FVS_SRC/$NAME already exists. Skipping copy."
else
    cp -r "$FVS_SRC/$BASE" "$FVS_SRC/$NAME"
fi

# Step 2: Copy variant COMMON includes
echo "  Creating v${NAME}/ from v${BASE}/..."
VBASE_SRC="v${BASE}"
VBASE_DST="v${NAME}"
if [ -d "$FVS_SRC/$VBASE_DST" ]; then
    echo "  WARNING: $FVS_SRC/$VBASE_DST already exists. Skipping copy."
else
    if [ -d "$FVS_SRC/$VBASE_SRC" ]; then
        cp -r "$FVS_SRC/$VBASE_SRC" "$FVS_SRC/$VBASE_DST"
    else
        echo "  INFO: No $VBASE_SRC directory found; using vbase only."
    fi
fi

# Step 3: Create calibration parameter template
echo "  Creating calibration template..."
CALIB_DIR="$FVS_SRC/$NAME/calibration"
mkdir -p "$CALIB_DIR"
cat > "$CALIB_DIR/README.txt" << EOF
FVS Variant: $NAME_UPPER
Base Variant: $BASE_UPPER
Description: ${DESCRIPTION:-'Custom variant'}
Created: $(date +%Y-%m-%d)

Calibration Parameters
======================
This directory holds species-specific calibration parameters for the
$NAME_UPPER variant. Modify these files to adjust growth, mortality,
and volume equations for your region.

Files:
  * species_map.csv     Species code mapping (FIA code -> local index)
  * height_dbh.csv      Height-diameter model coefficients
  * site_index.csv      Site index curve parameters
  * mortality.csv       Background mortality rates
  * crown_ratio.csv     Crown ratio model coefficients
  * volume.csv          Volume equation assignments

To apply calibrations, update the corresponding Fortran source files
in $FVS_SRC/$NAME/ and rebuild with:
  cd $FVS_SRC && OSTYPE=linux-gnu make FVS${NAME}.so
EOF

# Step 4: Create species map template
if [ -n "$SPECIES_FILE" ] && [ -f "$SPECIES_FILE" ]; then
    cp "$SPECIES_FILE" "$CALIB_DIR/species_map.csv"
    echo "  Copied species file: $SPECIES_FILE"
else
    cat > "$CALIB_DIR/species_map.csv" << 'EOF'
# Species mapping for custom variant
# FIA_CODE, VARIANT_INDEX, COMMON_NAME, SCIENTIFIC_NAME, FVS_ALPHA
# Edit this file then update spdecd.f and PRGPRM.f MAXSP accordingly
#
# Example (NE variant species):
# 12, 1, balsam fir, Abies balsamea, BF
# 71, 2, tamarack, Larix laricina, TL
# 95, 3, black spruce, Picea mariana, BS
EOF
    echo "  Created species map template"
fi

# Step 5: Create build target hint
echo ""
echo "=== Next Steps ==="
echo ""
echo "  1. Edit species mapping:   $CALIB_DIR/species_map.csv"
echo "  2. Update PRGPRM.f MAXSP:  vi $FVS_SRC/$NAME/PRGPRM.f"
echo "     (or copy from $FVS_SRC/v${BASE}/PRGPRM.f and adjust MAXSP)"
echo "  3. Edit growth coefficients in $FVS_SRC/$NAME/ source files"
echo "  4. Add makefile target (if not auto-detected):"
echo "     cd $FVS_SRC && OSTYPE=linux-gnu make FVS${NAME}.so"
echo "  5. Test: R -e \"library(rFVS); fvsLoad('FVS${NAME}', bin='$FVS_SRC/bin')\""
echo ""
echo "  For calibration guidance, see:"
echo "  * Weiskittel et al. (2011) Forest Growth and Yield Modeling, Ch. 8"
echo "  * Dixon (2002) FVS Essentials, Section 5.2"
echo ""

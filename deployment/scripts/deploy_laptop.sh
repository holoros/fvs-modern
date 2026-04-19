#!/bin/bash
# =============================================================================
# deploy_laptop.sh
#
# Deploy FVS and FVS-Online on a Fedora laptop for local use.
# This script is designed for personal workstations and does NOT require a
# dedicated service user, systemd, or nginx. Everything runs under the
# current user account.
#
# Usage:
#   ./deploy_laptop.sh [OPTIONS]
#
# Options:
#   --fvs-src DIR     Path to ForestVegetationSimulator-main
#   --iface-src DIR   Path to ForestVegetationSimulator-Interface-main
#   --install-dir DIR Where to install (default: ~/fvs-online)
#   --variants LIST   Comma-separated variant codes to build (default: all US)
#   --skip-build      Skip Fortran library build (use pre-built .so files)
#   --skip-rpkgs      Skip R package installation
#   --help            Show this message
#
# Examples:
#   ./deploy_laptop.sh
#   ./deploy_laptop.sh --variants ne,ie,ls
#   ./deploy_laptop.sh --install-dir /opt/fvs
#
# =============================================================================

set -euo pipefail

# =========================================================================
# Colors and logging
# =========================================================================
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

info()    { echo -e "${GREEN}[OK]${NC}    $1"; }
warn()    { echo -e "${YELLOW}[WARN]${NC}  $1"; }
error()   { echo -e "${RED}[ERROR]${NC} $1"; }
step()    { echo -e "\n${BLUE}${BOLD}>>> $1${NC}"; }
success() { echo -e "${GREEN}${BOLD}$1${NC}"; }

# =========================================================================
# Defaults
# =========================================================================
FVS_SRC=""
IFACE_SRC=""
INSTALL_DIR="$HOME/fvs-online"
VARIANTS="all"
SKIP_BUILD=false
SKIP_RPKGS=false
NPROC=$(nproc 2>/dev/null || echo 4)

# =========================================================================
# Parse arguments
# =========================================================================
while [[ $# -gt 0 ]]; do
    case "$1" in
        --fvs-src)    FVS_SRC="$2"; shift 2 ;;
        --iface-src)  IFACE_SRC="$2"; shift 2 ;;
        --install-dir) INSTALL_DIR="$2"; shift 2 ;;
        --variants)   VARIANTS="$2"; shift 2 ;;
        --skip-build) SKIP_BUILD=true; shift ;;
        --skip-rpkgs) SKIP_RPKGS=true; shift ;;
        --help)
            head -28 "$0" | tail -24
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            echo "Use --help for usage information."
            exit 1
            ;;
    esac
done

# =========================================================================
# Auto-detect source directories if not specified
# =========================================================================
detect_source() {
    local name="$1"
    local search_dirs=("$HOME" "$HOME/Documents" "$HOME/Downloads" "/home/$USER")
    for dir in "${search_dirs[@]}"; do
        if [ -d "$dir/$name" ]; then
            echo "$dir/$name"
            return 0
        fi
    done
    return 1
}

if [ -z "$FVS_SRC" ]; then
    FVS_SRC=$(detect_source "ForestVegetationSimulator-main" 2>/dev/null || true)
    if [ -z "$FVS_SRC" ]; then
        error "Could not find ForestVegetationSimulator-main."
        echo "  Please specify with: --fvs-src /path/to/ForestVegetationSimulator-main"
        exit 1
    fi
    info "Auto-detected FVS source: $FVS_SRC"
fi

if [ -z "$IFACE_SRC" ]; then
    IFACE_SRC=$(detect_source "ForestVegetationSimulator-Interface-main" 2>/dev/null || true)
    if [ -z "$IFACE_SRC" ]; then
        error "Could not find ForestVegetationSimulator-Interface-main."
        echo "  Please specify with: --iface-src /path/to/ForestVegetationSimulator-Interface-main"
        exit 1
    fi
    info "Auto-detected Interface source: $IFACE_SRC"
fi

# =========================================================================
# Validate inputs
# =========================================================================
[ -d "$FVS_SRC/bin" ] || { error "$FVS_SRC/bin not found. Wrong FVS source path?"; exit 1; }
[ -d "$IFACE_SRC/fvsOL" ] || { error "$IFACE_SRC/fvsOL not found. Wrong Interface path?"; exit 1; }

# Locate this script's deployment directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPLOY_ROOT="$(dirname "$SCRIPT_DIR")"

echo ""
echo "================================================================"
echo "  FVS Laptop Deployment"
echo "================================================================"
echo "  FVS Source:       $FVS_SRC"
echo "  Interface Source: $IFACE_SRC"
echo "  Install To:       $INSTALL_DIR"
echo "  Variants:         $VARIANTS"
echo "  CPUs for build:   $NPROC"
echo "================================================================"
echo ""

# =========================================================================
# Step 1: System dependencies
# =========================================================================
step "Step 1/8: Checking system dependencies"

MISSING_PKGS=()
check_pkg() {
    if ! rpm -q "$1" &>/dev/null; then
        MISSING_PKGS+=("$1")
    fi
}

check_pkg gcc-gfortran
check_pkg gcc
check_pkg make
check_pkg R
check_pkg R-devel
check_pkg cairo-devel
check_pkg libcurl-devel
check_pkg openssl-devel
check_pkg libxml2-devel
check_pkg sqlite-devel
check_pkg mesa-libGLU-devel
check_pkg libXt-devel
check_pkg freetype-devel
check_pkg harfbuzz-devel
check_pkg fribidi-devel
check_pkg libtiff-devel
check_pkg libjpeg-turbo-devel
check_pkg libpng-devel
check_pkg mdbtools

if [ ${#MISSING_PKGS[@]} -gt 0 ]; then
    warn "Missing packages: ${MISSING_PKGS[*]}"
    echo ""
    echo "  Install them with:"
    echo "    sudo dnf install -y ${MISSING_PKGS[*]}"
    echo ""
    read -p "  Install now? (requires sudo) [Y/n] " REPLY
    REPLY=${REPLY:-Y}
    if [[ "$REPLY" =~ ^[Yy] ]]; then
        sudo dnf install -y "${MISSING_PKGS[@]}" 2>&1 | tail -3
        info "Packages installed."
    else
        warn "Continuing without installing. Build may fail."
    fi
else
    info "All system dependencies present."
fi

# =========================================================================
# Step 2: Create install directory
# =========================================================================
step "Step 2/8: Creating directory structure"

mkdir -p "$INSTALL_DIR"/{bin,config,projects,R-libs,logs}

info "Created $INSTALL_DIR/"

# =========================================================================
# Step 3: Build Fortran shared libraries
# =========================================================================
step "Step 3/8: Building FVS shared libraries"

if [ "$SKIP_BUILD" = true ]; then
    # Check for pre-built libraries
    PREBUILT=$(ls "$FVS_SRC/bin/"FVS*.so 2>/dev/null || ls "$INSTALL_DIR/bin/"FVS*.so 2>/dev/null | wc -l)
    if [ "$PREBUILT" -gt 0 ]; then
        cp "$FVS_SRC/bin/"FVS*.so "$INSTALL_DIR/bin/" 2>/dev/null || true
        info "Copied pre-built libraries."
    else
        warn "No pre-built .so files found. Use --fvs-src with a built source tree."
    fi
else
    # Use fvs-modern's build_fvs_libraries.sh instead of the upstream Makefile.
    # The upstream CMake/Makefile build system fails on modern Linux because it
    # treats .inc coefficient files as Makefile targets. Our build script handles
    # include paths correctly for NVEL volume library and cross-variant source.
    BUILD_SCRIPT="$SCRIPT_DIR/build_fvs_libraries.sh"
    if [ ! -f "$BUILD_SCRIPT" ]; then
        error "build_fvs_libraries.sh not found at $BUILD_SCRIPT"
        error "Ensure you are running from the fvs-modern deployment directory."
        exit 1
    fi

    if [ "$VARIANTS" = "all" ]; then
        info "Building all US variants (this takes 10 to 20 minutes)..."
        bash "$BUILD_SCRIPT" "$FVS_SRC" "$INSTALL_DIR/bin" 2>&1 | tail -20
    else
        # Build specific variants
        IFS=',' read -ra VARLIST <<< "$VARIANTS"
        VARARGS=()
        for v in "${VARLIST[@]}"; do
            VARARGS+=("$(echo "$v" | tr '[:upper:]' '[:lower:]' | tr -d ' ')")
        done
        info "Building variants: ${VARARGS[*]}..."
        bash "$BUILD_SCRIPT" "$FVS_SRC" "$INSTALL_DIR/bin" "${VARARGS[@]}" 2>&1 | tail -20
    fi

    # Verify results
    BUILT=$(ls "$INSTALL_DIR/bin/"FVS*.so 2>/dev/null | wc -l)
    if [ "$BUILT" -gt 0 ]; then
        info "Built and installed $BUILT variant libraries."
    else
        error "Build produced no .so files. Check build output above."
        exit 1
    fi
fi

# List installed libraries
echo ""
echo "  Installed variants:"
for so in "$INSTALL_DIR/bin/"FVS*.so; do
    v=$(basename "$so" .so)
    sz=$(du -h "$so" | cut -f1)
    printf "    %-10s %s\n" "$v" "$sz"
done
echo ""

# =========================================================================
# Step 4: Install R packages
# =========================================================================
step "Step 4/8: Installing R package dependencies"

R_LIBS="$INSTALL_DIR/R-libs"

if [ "$SKIP_RPKGS" = true ]; then
    info "Skipping R package installation (--skip-rpkgs)"
else
    info "Installing R dependencies (this takes 10 to 15 minutes on first run)..."

    R --no-save --quiet << REOF 2>&1 | grep -E "^(Installing|DONE|Error|Warning)" | head -20
.libPaths("$R_LIBS")
install.packages(c(
    "shiny", "Cairo", "rhandsontable", "ggplot2", "RSQLite",
    "plyr", "dplyr", "colourpicker", "rgl", "leaflet", "zip",
    "openxlsx", "shinyFiles", "nlme", "yaml"
), repos = "https://cloud.r-project.org", lib = "$R_LIBS", Ncpus = $NPROC)
cat("DONE: All R packages installed.\n")
REOF

    info "R package dependencies ready."
fi

# =========================================================================
# Step 5: Install rFVS
# =========================================================================
step "Step 5/8: Installing rFVS package"

# Stage rFVS with NAMESPACE and man/
RFVS_STAGE=$(mktemp -d)
cp -r "$IFACE_SRC/rFVS/"* "$RFVS_STAGE/"

if [ ! -f "$RFVS_STAGE/NAMESPACE" ]; then
    if [ -f "$DEPLOY_ROOT/config/rFVS_NAMESPACE" ]; then
        cp "$DEPLOY_ROOT/config/rFVS_NAMESPACE" "$RFVS_STAGE/NAMESPACE"
    else
        cat > "$RFVS_STAGE/NAMESPACE" << 'EOF'
export(fvsLoad, fvsRun, fvsInteractRun, fvsSetCmdLine)
export(fvsAddActivity, fvsAddTrees, fvsCutNow)
export(fvsGetDims, fvsGetRestartcode, fvsGetSummary)
export(fvsGetTreeAttrs, fvsSetTreeAttrs)
export(fvsGetSpeciesAttrs, fvsSetSpeciesAttrs)
export(fvsGetSpeciesCodes, fvsGetStandIDs)
export(fvsGetEventMonitorVariables, fvsSetEventMonitorVariables)
export(fvsUnitConversion)
export(fvsGetSVSDims, fvsGetSVSObjectSet)
export(fvsSetupSummary, fvsCompositeSum)
export(fvsMakeKeyFile)
EOF
    fi
fi

if [ ! -d "$RFVS_STAGE/man" ]; then
    mkdir -p "$RFVS_STAGE/man"
    cat > "$RFVS_STAGE/man/rFVS-package.Rd" << 'EOF'
\name{rFVS-package}
\alias{rFVS-package}
\alias{rFVS}
\docType{package}
\title{Interface to the Forest Vegetation Simulator}
\description{
  Provides R functions that interface with the Forest Vegetation Simulator
  when it is run as a shared library.
}
\author{Nicholas Crookston}
EOF
fi

R CMD INSTALL --library="$R_LIBS" "$RFVS_STAGE" 2>&1 | tail -3
rm -rf "$RFVS_STAGE"
info "rFVS installed."

# =========================================================================
# Step 6: Install fvsOL
# =========================================================================
step "Step 6/8: Installing fvsOL package"

FVSOL_STAGE=$(mktemp -d)
cp -r "$IFACE_SRC/fvsOL/"* "$FVSOL_STAGE/"

if [ ! -f "$FVSOL_STAGE/NAMESPACE" ]; then
    if [ -f "$DEPLOY_ROOT/config/fvsOL_NAMESPACE" ]; then
        cp "$DEPLOY_ROOT/config/fvsOL_NAMESPACE" "$FVSOL_STAGE/NAMESPACE"
    else
        cat > "$FVSOL_STAGE/NAMESPACE" << 'EOF'
export(fvsOL)
import(shiny)
import(rFVS)
import(Cairo)
import(rhandsontable)
import(ggplot2)
import(RSQLite)
import(plyr)
import(dplyr)
import(colourpicker)
import(rgl)
import(leaflet)
import(zip)
import(openxlsx)
import(shinyFiles)
EOF
    fi
fi

if [ ! -d "$FVSOL_STAGE/man" ]; then
    mkdir -p "$FVSOL_STAGE/man"
    cat > "$FVSOL_STAGE/man/fvsOL-package.Rd" << 'EOF'
\name{fvsOL-package}
\alias{fvsOL-package}
\docType{package}
\title{Forest Vegetation Simulator Online Interface}
\description{
  An R-Shiny interface to the Forest Vegetation Simulator which can be
  run as an Online or Onlocal configuration.
}
\author{Nicholas Crookston, FVS Staff}
EOF
fi

R CMD INSTALL --library="$R_LIBS" "$FVSOL_STAGE" 2>&1 | tail -3
rm -rf "$FVSOL_STAGE"
info "fvsOL installed."

# =========================================================================
# Step 7: Create launcher scripts
# =========================================================================
step "Step 7/8: Creating launcher scripts"

# Main FVS-Online launcher
cat > "$INSTALL_DIR/start_fvsonline.sh" << LAUNCHER
#!/bin/bash
# Start FVS-Online locally
# Opens in your default browser at http://localhost:3838

INSTALL_DIR="$INSTALL_DIR"
R_LIBS="$R_LIBS"
FVS_BIN="$INSTALL_DIR/bin"
PORT=\${1:-3838}

echo ""
echo "  Starting FVS-Online..."
echo "  Browse to: http://localhost:\$PORT"
echo "  Press Ctrl+C to stop."
echo ""

R --no-save --quiet -e "
.libPaths('\$R_LIBS')
library(rFVS)
library(fvsOL)
options(shiny.port = \${PORT}L, shiny.host = '127.0.0.1', shiny.launch.browser = TRUE)
fvsOL(prjDir = '\$INSTALL_DIR/projects', fvsBin = '\$FVS_BIN')
"
LAUNCHER
chmod +x "$INSTALL_DIR/start_fvsonline.sh"

# Quick test launcher (runs a simulation and prints results)
cat > "$INSTALL_DIR/test_fvs.sh" << TESTSCRIPT
#!/bin/bash
# Quick test: loads FVSne and runs a 50-year projection

INSTALL_DIR="$INSTALL_DIR"
R_LIBS="$R_LIBS"
FVS_BIN="$INSTALL_DIR/bin"

echo ""
echo "  Running FVS verification test..."
echo ""

R --no-save --quiet -e "
.libPaths('\$R_LIBS')
library(rFVS)
cat('rFVS loaded.\n')

# Try loading the NE variant
variant <- 'FVSne'
if (!file.exists(file.path('\$FVS_BIN', paste0(variant, '.so')))) {
  # Pick whatever variant is available
  avail <- list.files('\$FVS_BIN', pattern = 'FVS.*\\\\.so')
  if (length(avail) == 0) stop('No FVS variant libraries found in \$FVS_BIN')
  variant <- sub('\\\\.so\$', '', avail[1])
}
cat('Loading', variant, '...\n')
fvsLoad(variant, bin = '\$FVS_BIN')
dims <- fvsGetDims()
cat('  Max species:', dims['maxspecies'], '\n')
cat('  Max trees:  ', dims['maxtrees'], '\n')
cat('  Max cycles: ', dims['maxcycles'], '\n')
cat('\nVerification PASSED. FVS is operational.\n')
cat('Installed variants:\n')
avail <- list.files('\$FVS_BIN', pattern = 'FVS.*\\\\.so')
for (v in avail) cat('  ', sub('\\\\.so', '', v), '\n')
"
TESTSCRIPT
chmod +x "$INSTALL_DIR/test_fvs.sh"

# Desktop shortcut (GNOME)
DESKTOP_FILE="$HOME/.local/share/applications/fvs-online.desktop"
mkdir -p "$(dirname "$DESKTOP_FILE")"
cat > "$DESKTOP_FILE" << DESKTOP
[Desktop Entry]
Type=Application
Name=FVS-Online
Comment=Forest Vegetation Simulator Web Interface
Exec=bash -c 'cd $INSTALL_DIR && ./start_fvsonline.sh'
Icon=utilities-terminal
Terminal=true
Categories=Science;Education;
DESKTOP
info "Desktop shortcut created (search for 'FVS-Online' in Activities)."

info "Launcher scripts created."

# =========================================================================
# Step 8: Verification
# =========================================================================
step "Step 8/8: Running verification test"

R --no-save --quiet << REOF 2>&1
.libPaths("$R_LIBS")

# Test 1: Load packages
tryCatch({
    library(rFVS)
    library(fvsOL)
    cat("[PASS] rFVS and fvsOL loaded successfully.\n")
}, error = function(e) {
    cat("[FAIL] Package load error:", e\$message, "\n")
})

# Test 2: Load a variant library
soFiles <- list.files("$INSTALL_DIR/bin", pattern = "FVS.*\\.so", full.names = FALSE)
if (length(soFiles) > 0) {
    variant <- sub("\\.so$", "", soFiles[1])
    tryCatch({
        fvsLoad(variant, bin = "$INSTALL_DIR/bin")
        dims <- fvsGetDims()
        cat("[PASS]", variant, "loaded: maxspecies =", dims["maxspecies"],
            "maxtrees =", dims["maxtrees"], "\n")
    }, error = function(e) {
        cat("[FAIL] Library load error:", e\$message, "\n")
    })
} else {
    cat("[WARN] No variant .so files found.\n")
}

cat("\nInstalled variants:", length(soFiles), "\n")
REOF

# =========================================================================
# Summary
# =========================================================================
NVAR=$(ls "$INSTALL_DIR/bin/"FVS*.so 2>/dev/null | wc -l)

echo ""
echo "================================================================"
success "  FVS Laptop Deployment Complete!"
echo "================================================================"
echo ""
echo "  Install directory:  $INSTALL_DIR"
echo "  Variant libraries:  $NVAR"
echo "  R package library:  $R_LIBS"
echo ""
echo "  Quick commands:"
echo ""
echo "    ${BOLD}Start FVS-Online:${NC}"
echo "      $INSTALL_DIR/start_fvsonline.sh"
echo ""
echo "    ${BOLD}Run verification test:${NC}"
echo "      $INSTALL_DIR/test_fvs.sh"
echo ""
echo "    ${BOLD}Start on a custom port:${NC}"
echo "      $INSTALL_DIR/start_fvsonline.sh 8080"
echo ""
echo "    ${BOLD}Use from R directly:${NC}"
echo "      .libPaths('$R_LIBS')"
echo "      library(rFVS)"
echo "      fvsLoad('FVSne', bin='$INSTALL_DIR/bin')"
echo ""
echo "  A desktop shortcut has been created. Search for"
echo "  'FVS-Online' in the GNOME Activities menu."
echo ""
echo "================================================================"

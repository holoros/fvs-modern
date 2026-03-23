#!/bin/bash
# =============================================================================
# setup_fedora.sh
#
# Automated setup script for FVS-Online on Fedora Linux.
# Installs system packages, R dependencies, and configures the deployment.
#
# Usage:
#   sudo ./setup_fedora.sh [FVS_SOURCE_DIR] [INTERFACE_DIR]
#
# Example:
#   sudo ./setup_fedora.sh /home/user/ForestVegetationSimulator-main \
#                           /home/user/ForestVegetationSimulator-Interface-main
#
# Run as root or with sudo. The script will:
#   1. Install system packages (gfortran, R, mdbtools, etc.)
#   2. Create /srv/fvs directory structure
#   3. Create fvs service user
#   4. Build FVS Fortran shared libraries
#   5. Install R package dependencies
#   6. Install rFVS and fvsOL packages
#   7. Copy configuration and patches
#   8. Optionally configure nginx and systemd
# =============================================================================

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info()  { echo -e "${GREEN}[INFO]${NC} $1"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Parse arguments
FVS_SOURCE="${1:-}"
INTERFACE_SOURCE="${2:-}"
FVS_ROOT="/srv/fvs"
R_LIBS="$FVS_ROOT/R-libs"

if [ -z "$FVS_SOURCE" ] || [ -z "$INTERFACE_SOURCE" ]; then
    echo "Usage: sudo $0 FVS_SOURCE_DIR INTERFACE_DIR"
    echo ""
    echo "  FVS_SOURCE_DIR   Path to ForestVegetationSimulator-main"
    echo "  INTERFACE_DIR    Path to ForestVegetationSimulator-Interface-main"
    exit 1
fi

if [ ! -d "$FVS_SOURCE/bin" ]; then
    error "$FVS_SOURCE does not look like a FVS source directory (no bin/)"
    exit 1
fi

if [ ! -d "$INTERFACE_SOURCE/fvsOL" ]; then
    error "$INTERFACE_SOURCE does not look like an Interface directory (no fvsOL/)"
    exit 1
fi

# Detect script location for finding fvs-modern deployment files
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEPLOY_ROOT="$(dirname "$SCRIPT_DIR")"

echo "================================================================"
echo "  FVS-Online Fedora Setup"
echo "================================================================"
echo "  FVS Source:     $FVS_SOURCE"
echo "  Interface:      $INTERFACE_SOURCE"
echo "  Install Root:   $FVS_ROOT"
echo "  Deploy Files:   $DEPLOY_ROOT"
echo "================================================================"
echo ""

# =========================================================================
# Step 1: System packages
# =========================================================================
info "Step 1: Installing system packages..."

dnf install -y \
    gcc-gfortran gcc gcc-c++ make cmake \
    R R-devel \
    cairo-devel libcurl-devel openssl-devel libxml2-devel sqlite-devel \
    mesa-libGLU-devel libXt-devel libX11-devel \
    freetype-devel harfbuzz-devel fribidi-devel \
    libtiff-devel libjpeg-turbo-devel libpng-devel \
    mdbtools sqlite \
    2>&1 | tail -3

info "System packages installed."

# =========================================================================
# Step 2: Directory structure
# =========================================================================
info "Step 2: Creating directory structure..."

mkdir -p "$FVS_ROOT"/{bin,config,projects,interface,patches,R-libs,www,logs}

info "Directories created at $FVS_ROOT"

# =========================================================================
# Step 3: Build Fortran shared libraries
# =========================================================================
info "Step 3: Building FVS Fortran shared libraries..."
info "  This may take 10 to 20 minutes."

cd "$FVS_SOURCE/bin"

# Attempt the standard makefile build
if [ -f makefile ]; then
    make -j$(nproc) all CC=gcc FC=gfortran 2>&1 | tail -5

    # Copy built libraries
    BUILT=$(ls "$FVS_SOURCE"/FVS*.so 2>/dev/null | wc -l)
    if [ "$BUILT" -gt 0 ]; then
        cp "$FVS_SOURCE"/FVS*.so "$FVS_ROOT/bin/"
        info "Built and installed $BUILT shared libraries."
    else
        warn "Makefile build produced no .so files. Check $FVS_SOURCE for errors."
        warn "You can build manually later using: cd $FVS_SOURCE/bin && make all"
    fi
else
    warn "No makefile found. You will need to build FVS libraries manually."
fi

ls -lh "$FVS_ROOT/bin/"FVS*.so 2>/dev/null || warn "No .so files in $FVS_ROOT/bin/"

# =========================================================================
# Step 4: Copy interface source
# =========================================================================
info "Step 4: Copying FVS Interface source..."

cp -r "$INTERFACE_SOURCE"/* "$FVS_ROOT/interface/"
info "Interface source copied."

# =========================================================================
# Step 5: Generate NAMESPACE files if missing
# =========================================================================
info "Step 5: Checking package NAMESPACE files..."

if [ ! -f "$FVS_ROOT/interface/rFVS/NAMESPACE" ]; then
    if [ -f "$DEPLOY_ROOT/config/rFVS_NAMESPACE" ]; then
        cp "$DEPLOY_ROOT/config/rFVS_NAMESPACE" "$FVS_ROOT/interface/rFVS/NAMESPACE"
    else
        cat > "$FVS_ROOT/interface/rFVS/NAMESPACE" << 'EOF'
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
    info "Generated rFVS NAMESPACE."
fi

if [ ! -f "$FVS_ROOT/interface/fvsOL/NAMESPACE" ]; then
    cat > "$FVS_ROOT/interface/fvsOL/NAMESPACE" << 'EOF'
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
    info "Generated fvsOL NAMESPACE."
fi

# =========================================================================
# Step 6: Install R packages
# =========================================================================
info "Step 6: Installing R package dependencies..."
info "  This may take 15 to 30 minutes on first run."

R --no-save --quiet << REOF
.libPaths("$R_LIBS")
install.packages(c(
    "shiny", "Cairo", "rhandsontable", "ggplot2", "RSQLite",
    "plyr", "dplyr", "colourpicker", "rgl", "leaflet", "zip",
    "openxlsx", "shinyFiles", "nlme", "yaml"
), repos = "https://cloud.r-project.org", lib = "$R_LIBS", Ncpus = $(nproc))
cat("R packages installed.\n")
REOF

# =========================================================================
# Step 7: Install rFVS and fvsOL
# =========================================================================
info "Step 7: Installing rFVS and fvsOL packages..."

R CMD INSTALL --library="$R_LIBS" "$FVS_ROOT/interface/rFVS" 2>&1 | tail -3
R CMD INSTALL --library="$R_LIBS" "$FVS_ROOT/interface/fvsOL" 2>&1 | tail -3

info "R packages installed."

# =========================================================================
# Step 8: Copy configuration and patches
# =========================================================================
info "Step 8: Configuring deployment..."

if [ -f "$DEPLOY_ROOT/config/fvsol_config.yml" ]; then
    cp "$DEPLOY_ROOT/config/fvsol_config.yml" "$FVS_ROOT/config/"
    cp "$DEPLOY_ROOT/config/fvsol_config.R" "$FVS_ROOT/config/"
fi

if [ -d "$DEPLOY_ROOT/patches" ]; then
    cp "$DEPLOY_ROOT/patches/"*.R "$FVS_ROOT/patches/" 2>/dev/null || true
fi

# Update config with actual paths
if [ -f "$FVS_ROOT/config/fvsol_config.yml" ]; then
    sed -i "s|work_dir:.*|work_dir: \"$FVS_ROOT/projects\"|" "$FVS_ROOT/config/fvsol_config.yml"
    sed -i "s|fvs_bin:.*|fvs_bin: \"$FVS_ROOT/bin\"|" "$FVS_ROOT/config/fvsol_config.yml"
fi

# Copy launcher scripts
if [ -d "$DEPLOY_ROOT/scripts" ]; then
    cp "$DEPLOY_ROOT/scripts/launch_fvsonline.R" "$FVS_ROOT/" 2>/dev/null || true
    cp "$DEPLOY_ROOT/scripts/launch_prjbldr.R" "$FVS_ROOT/" 2>/dev/null || true
fi

info "Configuration complete."

# =========================================================================
# Step 9: Set permissions
# =========================================================================
info "Step 9: Setting permissions..."

# Create fvs user if running as system service
if ! id -u fvs &>/dev/null; then
    useradd -r -s /sbin/nologin -d "$FVS_ROOT" fvs 2>/dev/null || true
fi

chown -R fvs:fvs "$FVS_ROOT" 2>/dev/null || chown -R "$SUDO_USER:$SUDO_USER" "$FVS_ROOT"
mkdir -p /var/log/fvs
chown fvs:fvs /var/log/fvs 2>/dev/null || true

info "Permissions set."

# =========================================================================
# Done
# =========================================================================
echo ""
echo "================================================================"
echo "  FVS-Online Setup Complete"
echo "================================================================"
echo ""
echo "  Shared libraries: $(ls "$FVS_ROOT/bin/"FVS*.so 2>/dev/null | wc -l) variants"
echo "  R library:        $R_LIBS"
echo "  Config:           $FVS_ROOT/config/fvsol_config.yml"
echo ""
echo "  To launch FVS-Online locally:"
echo ""
echo "    export R_LIBS_USER=$R_LIBS"
echo "    export FVSOL_CONFIG_FILE=$FVS_ROOT/config/fvsol_config.yml"
echo "    cd $FVS_ROOT/projects"
echo "    Rscript -e '.libPaths(\"$R_LIBS\"); library(rFVS); library(fvsOL); options(shiny.port=3838L, shiny.host=\"127.0.0.1\"); fvsOL(fvsBin=\"$FVS_ROOT/bin\")'"
echo ""
echo "  Then open: http://localhost:3838"
echo ""
echo "  For persistent service, install the systemd units:"
echo "    sudo cp $DEPLOY_ROOT/services/*.service /etc/systemd/system/"
echo "    sudo systemctl daemon-reload"
echo "    sudo systemctl start fvsonline"
echo "================================================================"

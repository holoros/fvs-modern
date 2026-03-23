#!/usr/bin/env bash
# ==========================================================================
# setup_wsl.sh
# Installs and builds FVS + FVS-Online inside Windows Subsystem for Linux 2
#
# Prerequisites:
#   * Windows 10 (build 19041+) or Windows 11
#   * WSL2 with Ubuntu installed:
#       wsl --install -d Ubuntu
#
# Run this script INSIDE the WSL2 Ubuntu terminal:
#   bash setup_wsl.sh [OPTIONS]
#
# Options:
#   --fvs-src DIR       Path to FVS Fortran source (auto-detects)
#   --iface-src DIR     Path to FVS Interface source (auto-detects)
#   --install-dir DIR   Where to install (default: ~/fvs-online)
#   --variants LIST     Space-separated variant codes (default: all 22 US)
#   --skip-deps         Skip apt package installation
#   --skip-build        Skip Fortran compilation
#   --skip-rpkgs        Skip R package installation
# ==========================================================================

set -euo pipefail

# Verify we're running in WSL
if ! grep -qi microsoft /proc/version 2>/dev/null; then
    echo "WARNING: This does not appear to be a WSL environment."
    echo "This script is designed for Windows Subsystem for Linux 2."
    echo "On native Linux, use deploy_laptop.sh or the Docker setup instead."
    read -p "Continue anyway? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then exit 1; fi
fi

# Defaults
FVS_SRC=""
IFACE_SRC=""
INSTALL_DIR="$HOME/fvs-online"
VARIANTS="ak bm ca ci cr cs ec em ie kt ls nc ne oc op pn sn so tt ut wc ws"
SKIP_DEPS=false
SKIP_BUILD=false
SKIP_RPKGS=false

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log()  { echo -e "${GREEN}[OK]${NC}   $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
err()  { echo -e "${RED}[ERR]${NC}  $1"; }

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --fvs-src)     FVS_SRC="$2"; shift 2 ;;
        --iface-src)   IFACE_SRC="$2"; shift 2 ;;
        --install-dir) INSTALL_DIR="$2"; shift 2 ;;
        --variants)    VARIANTS="$2"; shift 2 ;;
        --skip-deps)   SKIP_DEPS=true; shift ;;
        --skip-build)  SKIP_BUILD=true; shift ;;
        --skip-rpkgs)  SKIP_RPKGS=true; shift ;;
        --help)
            head -18 "$0" | grep "^#" | sed 's/^# *//'
            exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

echo "=========================================="
echo "  FVS-Online WSL2 Setup"
echo "=========================================="
echo ""

# Detect Windows home for auto-detecting downloads
WIN_HOME=""
if [ -d "/mnt/c/Users" ]; then
    WIN_USER=$(cmd.exe /c "echo %USERNAME%" 2>/dev/null | tr -d '\r' || whoami)
    WIN_HOME="/mnt/c/Users/$WIN_USER"
fi

# =========================================================================
# Step 1: System dependencies
# =========================================================================
if ! $SKIP_DEPS; then
    echo ">>> Step 1: Installing system dependencies..."

    sudo apt-get update -qq
    sudo apt-get install -y -qq \
        gfortran gcc g++ make \
        libsqlite3-dev sqlite3 \
        r-base r-base-dev \
        libcurl4-openssl-dev libssl-dev libxml2-dev \
        libcairo2-dev libxt-dev libx11-dev \
        libglu1-mesa-dev libfreetype6-dev \
        libpng-dev libtiff-dev libjpeg-dev \
        curl unzip git 2>&1 | tail -3

    log "System packages installed"
    log "gfortran $(gfortran --version | head -1)"
    log "R $(R --version | head -1 | cut -d' ' -f1-3)"
fi

# =========================================================================
# Step 2: Auto-detect source directories
# =========================================================================
echo ""
echo ">>> Step 2: Locating source directories..."

# Search both WSL home and Windows Downloads/Documents
SEARCH_PATHS=(
    "$HOME/ForestVegetationSimulator-main"
    "$HOME/ForestVegetationSimulator"
)
if [ -n "$WIN_HOME" ]; then
    SEARCH_PATHS+=(
        "$WIN_HOME/Downloads/ForestVegetationSimulator-main"
        "$WIN_HOME/Documents/ForestVegetationSimulator-main"
        "$WIN_HOME/Desktop/ForestVegetationSimulator-main"
    )
fi

if [ -z "$FVS_SRC" ]; then
    for candidate in "${SEARCH_PATHS[@]}"; do
        if [ -d "$candidate/vbase" ]; then
            FVS_SRC="$candidate"
            break
        fi
    done
fi

if [ -z "$IFACE_SRC" ]; then
    for candidate in "${SEARCH_PATHS[@]/%/}"; do
        iface="${candidate/Simulator-main/Simulator-Interface-main}"
        iface="${iface/Simulator/Simulator-Interface}"
        if [ -d "$iface/rFVS" ]; then
            IFACE_SRC="$iface"
            break
        fi
    done
fi

# If source is on Windows filesystem, copy to WSL for build performance
if [ -n "$FVS_SRC" ] && [[ "$FVS_SRC" == /mnt/* ]]; then
    warn "Source is on Windows filesystem (slow for builds)"
    echo "  Copying to WSL filesystem for better performance..."
    WSL_SRC="$HOME/ForestVegetationSimulator-main"
    if [ ! -d "$WSL_SRC" ]; then
        cp -r "$FVS_SRC" "$WSL_SRC"
    fi
    FVS_SRC="$WSL_SRC"
    log "Copied to $FVS_SRC"
fi

if [ -n "$IFACE_SRC" ] && [[ "$IFACE_SRC" == /mnt/* ]]; then
    WSL_IFACE="$HOME/ForestVegetationSimulator-Interface-main"
    if [ ! -d "$WSL_IFACE" ]; then
        cp -r "$IFACE_SRC" "$WSL_IFACE"
    fi
    IFACE_SRC="$WSL_IFACE"
fi

if [ -z "$FVS_SRC" ]; then
    err "Cannot find FVS source directory."
    echo ""
    echo "  Download from GitHub and unzip, then either:"
    echo "    a) Place in your Windows Downloads folder, or"
    echo "    b) Run: $0 --fvs-src /path/to/ForestVegetationSimulator-main"
    echo ""
    echo "  Download: https://github.com/USDAForestService/ForestVegetationSimulator/archive/refs/heads/main.zip"
    exit 1
fi
log "FVS source: $FVS_SRC"
[ -n "$IFACE_SRC" ] && log "Interface:  $IFACE_SRC" || warn "Interface not found; skipping rFVS/fvsOL"

# =========================================================================
# Step 3: Apply patches
# =========================================================================
echo ""
echo ">>> Step 3: Applying FORMAT patches..."

PATCHED=0
for f in $(find "$FVS_SRC" -name "*.f" -o -name "*.F" 2>/dev/null); do
    if grep -q "A8'" "$f" 2>/dev/null; then
        sed -i "s/A8'/A8,'/g" "$f"
        PATCHED=$((PATCHED + 1))
    fi
    if grep -q "A10'" "$f" 2>/dev/null; then
        sed -i "s/A10'/A10,'/g" "$f"
        PATCHED=$((PATCHED + 1))
    fi
done
log "FORMAT fixes applied ($PATCHED files)"

# =========================================================================
# Step 4: Build variants
# =========================================================================
if ! $SKIP_BUILD; then
    echo ""
    echo ">>> Step 4: Building variant libraries..."

    cd "$FVS_SRC"
    BUILT=0
    FAILED=0
    for v in $VARIANTS; do
        printf "  Building FVS%-4s ... " "$v"
        if OSTYPE=linux-gnu make "FVS${v}.so" > /dev/null 2>&1; then
            echo -e "${GREEN}OK${NC}"
            BUILT=$((BUILT + 1))
        else
            echo -e "${RED}FAIL${NC}"
            FAILED=$((FAILED + 1))
        fi
    done
    echo ""
    log "Built $BUILT variants ($FAILED failed)"
fi

# =========================================================================
# Step 5: R packages
# =========================================================================
if ! $SKIP_RPKGS; then
    echo ""
    echo ">>> Step 5: Installing R packages..."

    R --no-save --quiet -e "
        pkgs <- c('shiny','Cairo','rhandsontable','ggplot2','RSQLite','plyr',
                   'colourpicker','rgl','leaflet','zip','openxlsx','shinyFiles','nlme')
        new_pkgs <- pkgs[!pkgs %in% installed.packages()[,'Package']]
        if (length(new_pkgs) > 0) {
            install.packages(new_pkgs, repos='https://cloud.r-project.org', Ncpus=4)
        }
        cat('All R packages installed.\n')
    " 2>&1 | tail -3

    if [ -n "$IFACE_SRC" ]; then
        SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        CONFIG_DIR="$SCRIPT_DIR/../config"

        # rFVS
        [ -f "$CONFIG_DIR/rFVS_NAMESPACE" ] && cp "$CONFIG_DIR/rFVS_NAMESPACE" "$IFACE_SRC/rFVS/NAMESPACE"
        mkdir -p "$IFACE_SRC/rFVS/man"
        printf '\\name{rFVS-package}\n\\alias{rFVS}\n\\docType{package}\n\\title{rFVS}\n\\description{R interface to FVS.}\n\\author{Nicholas Crookston}\n' \
            > "$IFACE_SRC/rFVS/man/rFVS-package.Rd"
        R CMD INSTALL "$IFACE_SRC/rFVS" 2>&1 | tail -2

        # fvsOL
        [ -f "$CONFIG_DIR/fvsOL_NAMESPACE" ] && cp "$CONFIG_DIR/fvsOL_NAMESPACE" "$IFACE_SRC/fvsOL/NAMESPACE"
        [ -f "$CONFIG_DIR/fvsOL_DESCRIPTION" ] && cp "$CONFIG_DIR/fvsOL_DESCRIPTION" "$IFACE_SRC/fvsOL/DESCRIPTION"
        mkdir -p "$IFACE_SRC/fvsOL/man"
        printf '\\name{fvsOL-package}\n\\alias{fvsOL}\n\\docType{package}\n\\title{fvsOL}\n\\description{FVS-Online Shiny interface.}\n\\author{FVS Staff}\n' \
            > "$IFACE_SRC/fvsOL/man/fvsOL-package.Rd"
        R CMD INSTALL "$IFACE_SRC/fvsOL" 2>&1 | tail -2
    fi
fi

# =========================================================================
# Step 6: Create installation
# =========================================================================
echo ""
echo ">>> Step 6: Creating installation..."

mkdir -p "$INSTALL_DIR/bin" "$INSTALL_DIR/projects"

if [ -d "$FVS_SRC/bin" ]; then
    cp "$FVS_SRC"/bin/FVS*.so "$INSTALL_DIR/bin/" 2>/dev/null || true
fi

cat > "$INSTALL_DIR/start_fvsonline.sh" << LAUNCHER
#!/usr/bin/env bash
echo "Starting FVS-Online..."
echo "Open http://127.0.0.1:3838 in your Windows browser"
R --no-save -e "
    library(fvsOL)
    fvsOL(prjDir='$INSTALL_DIR/projects',
          fvsBin='$INSTALL_DIR/bin')
"
LAUNCHER
chmod +x "$INSTALL_DIR/start_fvsonline.sh"

# Create a Windows shortcut helper
if [ -n "$WIN_HOME" ]; then
    cat > "$WIN_HOME/Desktop/FVS-Online.bat" << 'WINBAT'
@echo off
echo Starting FVS-Online in WSL...
wsl.exe bash -c "~/fvs-online/start_fvsonline.sh" &
timeout /t 5 /nobreak > nul
start http://127.0.0.1:3838
WINBAT
    log "Created desktop shortcut: FVS-Online.bat"
fi

log "Installation complete"

echo ""
echo "=========================================="
echo "  Setup Complete!"
echo "=========================================="
echo ""
echo "  To start FVS-Online from WSL:"
echo "    $INSTALL_DIR/start_fvsonline.sh"
echo ""
echo "  Or double-click FVS-Online.bat on your Windows desktop."
echo ""
echo "  Then open http://127.0.0.1:3838 in your browser."
echo "  (WSL2 shares the network with Windows, so localhost works.)"
echo ""

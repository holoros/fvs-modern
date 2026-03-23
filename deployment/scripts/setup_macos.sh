#!/usr/bin/env bash
# ==========================================================================
# setup_macos.sh
# Installs and builds FVS + FVS-Online on macOS (Apple Silicon and Intel)
#
# Prerequisites: Xcode Command Line Tools (xcode-select --install)
# This script installs Homebrew (if needed), gfortran, R, and all
# dependencies, then builds all 22 US variant shared libraries.
#
# Usage:
#   bash setup_macos.sh [OPTIONS]
#
# Options:
#   --fvs-src DIR       Path to FVS Fortran source (auto-detects)
#   --iface-src DIR     Path to FVS Interface source (auto-detects)
#   --install-dir DIR   Where to install (default: ~/fvs-online)
#   --variants LIST     Space-separated variant codes (default: all 22 US)
#   --skip-brew         Skip Homebrew package installation
#   --skip-build        Skip Fortran compilation
#   --skip-rpkgs        Skip R package installation
# ==========================================================================

set -euo pipefail

# Defaults
FVS_SRC=""
IFACE_SRC=""
INSTALL_DIR="$HOME/fvs-online"
VARIANTS="ak bm ca ci cr cs ec em ie kt ls nc ne oc op pn sn so tt ut wc ws"
SKIP_BREW=false
SKIP_BUILD=false
SKIP_RPKGS=false

# Colors
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
        --skip-brew)   SKIP_BREW=true; shift ;;
        --skip-build)  SKIP_BUILD=true; shift ;;
        --skip-rpkgs)  SKIP_RPKGS=true; shift ;;
        --help)
            head -16 "$0" | grep "^#" | sed 's/^# *//'
            exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

echo "=========================================="
echo "  FVS-Online macOS Setup"
echo "=========================================="
echo ""

# Detect architecture
ARCH=$(uname -m)
if [ "$ARCH" = "arm64" ]; then
    echo "  Architecture: Apple Silicon (arm64)"
    BREW_PREFIX="/opt/homebrew"
else
    echo "  Architecture: Intel (x86_64)"
    BREW_PREFIX="/usr/local"
fi
echo "  Install dir:  $INSTALL_DIR"
echo ""

# =========================================================================
# Step 1: Homebrew and system dependencies
# =========================================================================
if ! $SKIP_BREW; then
    echo ">>> Step 1: System dependencies..."

    # Install Homebrew if needed
    if ! command -v brew &>/dev/null; then
        echo "  Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        eval "$($BREW_PREFIX/bin/brew shellenv)"
    fi
    log "Homebrew $(brew --version | head -1)"

    # Install build tools
    brew install gcc sqlite r 2>/dev/null || brew upgrade gcc sqlite r 2>/dev/null || true

    # Verify gfortran
    GFORTRAN=$(ls $BREW_PREFIX/bin/gfortran-* 2>/dev/null | sort -V | tail -1)
    if [ -z "$GFORTRAN" ]; then
        err "gfortran not found after installing gcc"
        exit 1
    fi

    # Create symlink if needed
    if ! command -v gfortran &>/dev/null; then
        ln -sf "$GFORTRAN" "$BREW_PREFIX/bin/gfortran"
    fi
    log "gfortran $(gfortran --version | head -1)"
    log "R $(R --version | head -1)"
fi

# =========================================================================
# Step 2: Auto-detect source directories
# =========================================================================
echo ""
echo ">>> Step 2: Locating source directories..."

if [ -z "$FVS_SRC" ]; then
    for candidate in \
        "$HOME/ForestVegetationSimulator-main" \
        "$HOME/ForestVegetationSimulator" \
        "$HOME/Downloads/ForestVegetationSimulator-main" \
        "$HOME/Documents/ForestVegetationSimulator-main"; do
        if [ -d "$candidate/vbase" ]; then
            FVS_SRC="$candidate"
            break
        fi
    done
fi

if [ -z "$IFACE_SRC" ]; then
    for candidate in \
        "$HOME/ForestVegetationSimulator-Interface-main" \
        "$HOME/ForestVegetationSimulator-Interface" \
        "$HOME/Downloads/ForestVegetationSimulator-Interface-main" \
        "$HOME/Documents/ForestVegetationSimulator-Interface-main"; do
        if [ -d "$candidate/rFVS" ]; then
            IFACE_SRC="$candidate"
            break
        fi
    done
fi

if [ -z "$FVS_SRC" ]; then
    err "Cannot find FVS source directory."
    echo "  Download from: https://github.com/USDAForestService/ForestVegetationSimulator"
    echo "  Then run: $0 --fvs-src /path/to/ForestVegetationSimulator-main"
    exit 1
fi
log "FVS source: $FVS_SRC"

if [ -z "$IFACE_SRC" ]; then
    warn "Cannot find Interface source. rFVS/fvsOL will not be installed."
    echo "  Download from: https://github.com/USDAForestService/ForestVegetationSimulator-Interface"
else
    log "Interface:  $IFACE_SRC"
fi

# =========================================================================
# Step 3: Apply FORMAT patches
# =========================================================================
echo ""
echo ">>> Step 3: Applying patches..."

PATCHED=0
for f in $(find "$FVS_SRC" -name "*.f" -o -name "*.F" 2>/dev/null); do
    if grep -q "A8'" "$f" 2>/dev/null; then
        sed -i '' "s/A8'/A8,'/g" "$f"
        PATCHED=$((PATCHED + 1))
    fi
    if grep -q "A10'" "$f" 2>/dev/null; then
        sed -i '' "s/A10'/A10,'/g" "$f"
        PATCHED=$((PATCHED + 1))
    fi
done
log "FORMAT fixes applied ($PATCHED files patched)"

# =========================================================================
# Step 4: Build variant shared libraries
# =========================================================================
if ! $SKIP_BUILD; then
    echo ""
    echo ">>> Step 4: Building variant libraries..."

    cd "$FVS_SRC"
    mkdir -p bin

    BUILT=0
    FAILED=0
    for v in $VARIANTS; do
        printf "  Building FVS%-4s ... " "$v"
        # macOS needs OSTYPE and -dynamiclib instead of -shared
        if OSTYPE=darwin make "FVS${v}.so" > /dev/null 2>&1; then
            echo -e "${GREEN}OK${NC}"
            BUILT=$((BUILT + 1))
        else
            # Try with explicit linux-gnu OSTYPE (some makefiles check this)
            if OSTYPE=linux-gnu make "FVS${v}.so" > /dev/null 2>&1; then
                echo -e "${GREEN}OK${NC}"
                BUILT=$((BUILT + 1))
            else
                echo -e "${RED}FAIL${NC}"
                FAILED=$((FAILED + 1))
            fi
        fi
    done
    echo ""
    log "Built $BUILT variants ($FAILED failed)"
fi

# =========================================================================
# Step 5: Install R packages
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

    # Install rFVS
    if [ -n "$IFACE_SRC" ]; then
        SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        CONFIG_DIR="$SCRIPT_DIR/../config"

        if [ -f "$CONFIG_DIR/rFVS_NAMESPACE" ]; then
            cp "$CONFIG_DIR/rFVS_NAMESPACE" "$IFACE_SRC/rFVS/NAMESPACE"
        fi
        mkdir -p "$IFACE_SRC/rFVS/man"
        printf '\\name{rFVS-package}\n\\alias{rFVS}\n\\docType{package}\n\\title{rFVS}\n\\description{R interface to FVS.}\n\\author{Nicholas Crookston}\n' \
            > "$IFACE_SRC/rFVS/man/rFVS-package.Rd"
        R CMD INSTALL "$IFACE_SRC/rFVS" 2>&1 | tail -2

        # Install fvsOL
        if [ -f "$CONFIG_DIR/fvsOL_NAMESPACE" ]; then
            cp "$CONFIG_DIR/fvsOL_NAMESPACE" "$IFACE_SRC/fvsOL/NAMESPACE"
        fi
        if [ -f "$CONFIG_DIR/fvsOL_DESCRIPTION" ]; then
            cp "$CONFIG_DIR/fvsOL_DESCRIPTION" "$IFACE_SRC/fvsOL/DESCRIPTION"
        fi
        mkdir -p "$IFACE_SRC/fvsOL/man"
        printf '\\name{fvsOL-package}\n\\alias{fvsOL}\n\\docType{package}\n\\title{fvsOL}\n\\description{FVS-Online Shiny interface.}\n\\author{FVS Staff}\n' \
            > "$IFACE_SRC/fvsOL/man/fvsOL-package.Rd"
        R CMD INSTALL "$IFACE_SRC/fvsOL" 2>&1 | tail -2
    fi
fi

# =========================================================================
# Step 6: Create install directory and launcher
# =========================================================================
echo ""
echo ">>> Step 6: Creating installation..."

mkdir -p "$INSTALL_DIR/bin" "$INSTALL_DIR/projects"

# Copy libraries
if [ -d "$FVS_SRC/bin" ]; then
    cp "$FVS_SRC"/bin/FVS*.so "$INSTALL_DIR/bin/" 2>/dev/null || true
    cp "$FVS_SRC"/bin/FVS*.dylib "$INSTALL_DIR/bin/" 2>/dev/null || true
fi

# Create launcher script
cat > "$INSTALL_DIR/start_fvsonline.sh" << LAUNCHER
#!/usr/bin/env bash
cd "$INSTALL_DIR"
R --no-save -e "
    library(fvsOL)
    fvsOL(prjDir='$INSTALL_DIR/projects',
          fvsBin='$INSTALL_DIR/bin')
"
LAUNCHER
chmod +x "$INSTALL_DIR/start_fvsonline.sh"

# Create macOS app shortcut (Automator alternative)
APP_DIR="$HOME/Applications/FVS-Online.app/Contents/MacOS"
mkdir -p "$APP_DIR"
cat > "$APP_DIR/FVS-Online" << 'APPSCRIPT'
#!/usr/bin/env bash
open "http://127.0.0.1:3838" &
APPSCRIPT
chmod +x "$APP_DIR/FVS-Online"
cat > "$HOME/Applications/FVS-Online.app/Contents/Info.plist" << 'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>FVS-Online</string>
    <key>CFBundleExecutable</key>
    <string>FVS-Online</string>
    <key>CFBundleIdentifier</key>
    <string>edu.maine.fvs-online</string>
</dict>
</plist>
PLIST

log "Installation complete"

echo ""
echo "=========================================="
echo "  Setup Complete!"
echo "=========================================="
echo ""
echo "  To start FVS-Online:"
echo "    $INSTALL_DIR/start_fvsonline.sh"
echo ""
echo "  Then open http://127.0.0.1:3838 in your browser."
echo ""
echo "  Libraries: $INSTALL_DIR/bin/"
echo "  Projects:  $INSTALL_DIR/projects/"
echo ""

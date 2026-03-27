#!/bin/bash
# =============================================================================
# FVS Bayesian Calibration: OSC Cardinal Cluster Setup
#
# Run this ONCE after cloning the repo on OSC to install all dependencies.
# Estimated time: 15-20 minutes (mostly CmdStan compilation)
#
# Usage:
#   ssh username@cardinal.osc.edu
#   cd /fs/ess/scratch/$USER   # or your project directory
#   git clone https://github.com/holoros/fvs-modern.git
#   bash fvs-modern/calibration/osc/setup_osc.sh
# =============================================================================

set -e

echo "==========================================="
echo "FVS Calibration: OSC Cardinal Setup"
echo "==========================================="
echo "Start time: $(date)"
echo ""

# ============================================================================
# Step 1: Load Required Modules
# ============================================================================

echo "[1/5] Loading modules..."

# Cardinal module names (adjust if needed)
module purge
module load R/4.4.0       # or latest available: module spider R
module load gnu/13.2.0    # GNU compiler for CmdStan
module load cmake         # needed for CmdStan

# Verify R is available
Rscript --version
echo "R loaded successfully"

# ============================================================================
# Step 2: Set Up R Library Path
# ============================================================================

echo "[2/5] Setting up R user library..."

# Create user R library if it doesn't exist
R_LIB_USER="${HOME}/R/libs/$(R --version | head -1 | grep -oP '\d+\.\d+')"
mkdir -p "$R_LIB_USER"

# Add to .Renviron so R always finds it
if ! grep -q "R_LIBS_USER" ~/.Renviron 2>/dev/null; then
    echo "R_LIBS_USER=${R_LIB_USER}" >> ~/.Renviron
    echo "Created ~/.Renviron with R_LIBS_USER=${R_LIB_USER}"
fi

export R_LIBS_USER="$R_LIB_USER"

# ============================================================================
# Step 3: Install R Packages
# ============================================================================

echo "[3/5] Installing R packages (this takes ~10 minutes)..."

Rscript -e '
# Set library path
lib_path <- Sys.getenv("R_LIBS_USER")
.libPaths(c(lib_path, .libPaths()))

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Core packages
pkgs <- c(
  "tidyverse", "data.table", "jsonlite",
  "brms", "cmdstanr", "posterior", "bayesplot", "tidybayes",
  "rFIA", "quantreg",
  "logger", "glue", "progress",
  "parallel", "corrplot"
)

# Install missing packages
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, lib = lib_path)
  } else {
    cat("Already installed:", pkg, "\n")
  }
}

# Install cmdstanr from r-universe if not on CRAN
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr",
                   repos = c("https://stan-dev.r-universe.dev",
                             "https://cloud.r-project.org"),
                   lib = lib_path)
}

cat("\nAll R packages installed.\n")
'

# ============================================================================
# Step 4: Install CmdStan
# ============================================================================

echo "[4/5] Installing CmdStan (this takes ~5 minutes)..."

Rscript -e '
library(cmdstanr)

# Check if already installed
if (is.null(tryCatch(cmdstan_path(), error = function(e) NULL))) {
  cat("Installing CmdStan...\n")
  install_cmdstan(cores = 4, quiet = FALSE)
  cat("CmdStan installed at:", cmdstan_path(), "\n")
} else {
  cat("CmdStan already installed at:", cmdstan_path(), "\n")
}

# Verify
cat("CmdStan version:", cmdstan_version(), "\n")
'

# ============================================================================
# Step 5: Verify Installation
# ============================================================================

echo "[5/5] Verifying installation..."

Rscript -e '
cat("=== Package Verification ===\n")
pkgs <- c("tidyverse", "brms", "cmdstanr", "rFIA", "quantreg",
          "posterior", "bayesplot", "jsonlite", "logger")

all_ok <- TRUE
for (pkg in pkgs) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  status <- ifelse(ok, "OK", "MISSING")
  cat(sprintf("  %-15s %s\n", pkg, status))
  if (!ok) all_ok <- FALSE
}

# Test CmdStan compilation
cat("\n=== CmdStan Test ===\n")
tryCatch({
  mod <- cmdstanr::cmdstan_model(
    write_stan_file("data { int N; } parameters { real mu; } model { mu ~ normal(0, 1); }"),
    quiet = TRUE
  )
  cat("  CmdStan compilation: OK\n")
}, error = function(e) {
  cat("  CmdStan compilation: FAILED -", e$message, "\n")
  all_ok <<- FALSE
})

if (all_ok) {
  cat("\n*** All checks passed. Ready to run calibration. ***\n")
} else {
  cat("\n*** Some checks failed. Review output above. ***\n")
}
'

echo ""
echo "==========================================="
echo "Setup complete: $(date)"
echo "==========================================="
echo ""
echo "Next steps:"
echo "  1. Transfer ENTIRE_FIA.zip to OSC and unzip"
echo "  2. Edit calibration/osc/config_osc.sh with your paths"
echo "  3. Run: sbatch calibration/osc/submit_cardinal.sh"
echo ""

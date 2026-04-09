#!/bin/bash
#
# FVS Bayesian Calibration: Per-Variant Pipeline Script
# Runs the complete calibration pipeline for a single FVS variant
# Called by submit_calibration.sh for each variant
#
# Pipeline steps:
#   01  Fetch FIA data
#   02  Fit diameter growth (Wykoff)
#   03  Fit height-diameter (Chapman-Richards)
#   03b Fit height increment (variants with explicit HG params only)
#   04  Fit mortality (logistic)
#   05  Fit crown ratio change
#   06  Convert all posteriors to calibrated JSON config
#   07  Generate comprehensive diagnostics
#
# Usage: bash run_variant.sh <variant_code>
# Example: bash run_variant.sh ca
#

set -e  # Exit on error

if [ -z "$1" ]; then
    echo "Usage: $0 <variant_code>"
    echo "Example: $0 ca"
    exit 1
fi

VARIANT="$1"

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ROOT="${HOME}/path/to/fvs-modern"
CALIBRATION_DIR="${PROJECT_ROOT}/calibration"
SCRIPTS_DIR="${CALIBRATION_DIR}/R"
LOG_DIR="${CALIBRATION_DIR}/logs"
OUTPUT_DIR="${CALIBRATION_DIR}/output/variants/${VARIANT}"
DATA_DIR="${CALIBRATION_DIR}/data/processed"

# Create necessary directories
mkdir -p "$LOG_DIR"
mkdir -p "$OUTPUT_DIR"
mkdir -p "${DATA_DIR}/${VARIANT}"

# Log file for this variant
VARIANT_LOG="${LOG_DIR}/variant_${VARIANT}.log"

# ============================================================================
# Logging Function
# ============================================================================

log_step() {
    local step=$1
    local msg=$2
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$step] $msg" | tee -a "$VARIANT_LOG"
}

# Initialize log
echo "========================================" > "$VARIANT_LOG"
echo "FVS Calibration Pipeline for Variant: $VARIANT" >> "$VARIANT_LOG"
echo "Start time: $(date)" >> "$VARIANT_LOG"
echo "========================================" >> "$VARIANT_LOG"

log_step "INIT" "Starting calibration pipeline for variant: $VARIANT"
log_step "INFO" "Project root: $PROJECT_ROOT"
log_step "INFO" "Output directory: $OUTPUT_DIR"

# ============================================================================
# Check Prerequisites
# ============================================================================

log_step "CHECK" "Verifying prerequisites..."

# Check if variant config exists
if [ ! -f "${PROJECT_ROOT}/config/${VARIANT}.json" ]; then
    log_step "ERROR" "Config file not found for variant $VARIANT"
    exit 1
fi

# Check if required R scripts exist
for script in 01_fetch_fia_data 02_fit_diameter_growth 03_fit_height_diameter \
              03b_fit_height_increment 04_fit_mortality 05_fit_crown_ratio \
              08_fetch_stand_data 09_fit_stand_density \
              06_posterior_to_json 07_diagnostics; do
    if [ ! -f "${SCRIPTS_DIR}/${script}.R" ]; then
        log_step "ERROR" "Script not found: ${script}.R"
        exit 1
    fi
done

log_step "CHECK" "All prerequisites verified"

# ============================================================================
# Step 01: Fetch FIA Data (Run Once Per Session)
# ============================================================================

log_step "STEP01" "Fetching and preparing FIA data..."

if [ ! -d "${DATA_DIR}/${VARIANT}" ] || [ -z "$(ls -A ${DATA_DIR}/${VARIANT} 2>/dev/null)" ]; then
    log_step "STEP01" "Running FIA data fetch (first time for variant)..."

    if Rscript "${SCRIPTS_DIR}/01_fetch_fia_data.R" \
        --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
        log_step "STEP01" "FIA data fetch completed successfully"
    else
        log_step "ERROR" "FIA data fetch failed"
        exit 1
    fi
else
    log_step "STEP01" "FIA data already exists for variant $VARIANT"
fi

# Check that data was created
if [ ! -f "${DATA_DIR}/${VARIANT}/diameter_growth.csv" ]; then
    log_step "ERROR" "FIA data file not found after fetch"
    exit 1
fi

# ============================================================================
# Step 02: Fit Diameter Growth Model (REQUIRED)
# ============================================================================

log_step "STEP02" "Fitting Bayesian diameter growth model..."

if Rscript "${SCRIPTS_DIR}/02_fit_diameter_growth.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP02" "Diameter growth model completed successfully"
else
    log_step "ERROR" "Diameter growth model failed"
    exit 1
fi

# Verify output
if [ ! -f "${OUTPUT_DIR}/diameter_growth_posterior.csv" ]; then
    log_step "ERROR" "Diameter growth posterior output not found"
    exit 1
fi

# ============================================================================
# Step 03: Fit Height-Diameter Model
# ============================================================================

log_step "STEP03" "Fitting height-diameter model..."

if Rscript "${SCRIPTS_DIR}/03_fit_height_diameter.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP03" "Height-diameter model completed successfully"
else
    log_step "WARN" "Height-diameter model failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 03b: Fit Height Increment Model (conditional)
# Only runs for variants with explicit HG parameters (IE, CI, KT, BC, WS, EM)
# For other variants, height growth is derived from the H-D model + DG
# The script itself detects whether HG params exist and exits gracefully if not
# ============================================================================

log_step "STEP03B" "Fitting height increment model (if variant has HG params)..."

if Rscript "${SCRIPTS_DIR}/03b_fit_height_increment.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP03B" "Height increment model completed successfully"
else
    log_step "WARN" "Height increment model skipped or failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 04: Fit Mortality Model
# ============================================================================

log_step "STEP04" "Fitting mortality model..."

if Rscript "${SCRIPTS_DIR}/04_fit_mortality.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP04" "Mortality model completed successfully"
else
    log_step "WARN" "Mortality model failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 05: Fit Crown Ratio Model
# ============================================================================

log_step "STEP05" "Fitting crown ratio model..."

if Rscript "${SCRIPTS_DIR}/05_fit_crown_ratio.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP05" "Crown ratio model completed successfully"
else
    log_step "WARN" "Crown ratio model failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 08: Fetch Stand-Level Data
# ============================================================================

log_step "STEP08" "Extracting stand-level density data from FIA..."

if Rscript "${SCRIPTS_DIR}/08_fetch_stand_data.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP08" "Stand-level data extraction completed successfully"
else
    log_step "WARN" "Stand-level data extraction failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 09: Fit Stand Density Parameters (SDIMAX, BAMAX, self-thinning)
# ============================================================================

log_step "STEP09" "Calibrating stand-level density parameters..."

if Rscript "${SCRIPTS_DIR}/09_fit_stand_density.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP09" "Stand density calibration completed successfully"
else
    log_step "WARN" "Stand density calibration failed (non-fatal; continuing)"
fi

# ============================================================================
# Step 06: Convert All Posteriors to JSON (REQUIRED)
# ============================================================================

log_step "STEP06" "Converting posterior samples to JSON format..."

if Rscript "${SCRIPTS_DIR}/06_posterior_to_json.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP06" "Posterior to JSON conversion completed successfully"
else
    log_step "ERROR" "Posterior to JSON conversion failed"
    exit 1
fi

# Verify calibrated config was created
if [ ! -f "${PROJECT_ROOT}/config/calibrated/${VARIANT}.json" ]; then
    log_step "ERROR" "Calibrated config file not created"
    exit 1
fi

# ============================================================================
# Step 07: Generate Diagnostics
# ============================================================================

log_step "STEP07" "Generating comprehensive diagnostics..."

if Rscript "${SCRIPTS_DIR}/07_diagnostics.R" \
    --variant "$VARIANT" >> "$VARIANT_LOG" 2>&1; then
    log_step "STEP07" "Diagnostics generated successfully"
else
    log_step "WARN" "Diagnostics generation failed (non-fatal)"
fi

# ============================================================================
# Cleanup and Summary
# ============================================================================

log_step "CLEANUP" "Performing cleanup..."

# Optional: compress output files to save space
# tar -czf "${OUTPUT_DIR}/samples.tar.gz" "${OUTPUT_DIR}"/*_samples.rds

log_step "SUCCESS" "All pipeline steps completed for variant $VARIANT"

# ============================================================================
# Print Summary
# ============================================================================

cat >> "$VARIANT_LOG" << EOF

========================================
Calibration Pipeline Summary
========================================
Variant: $VARIANT
Project Root: $PROJECT_ROOT
Output Directory: $OUTPUT_DIR
Log File: $VARIANT_LOG
Completion Time: $(date)
========================================

Output Files Generated:
EOF

if [ -d "$OUTPUT_DIR" ]; then
    ls -lh "$OUTPUT_DIR" | tail -n +2 >> "$VARIANT_LOG"
fi

if [ -f "${PROJECT_ROOT}/config/calibrated/${VARIANT}.json" ]; then
    echo "Calibrated config: ${PROJECT_ROOT}/config/calibrated/${VARIANT}.json" >> "$VARIANT_LOG"
fi

log_step "INFO" "Pipeline complete. See $VARIANT_LOG for full details."

exit 0

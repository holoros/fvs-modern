#!/bin/bash
# =============================================================================
# FVS Bayesian Calibration: SLURM Submission Wrapper for OSC Cardinal
#
# Generates and submits a SLURM batch script with the correct resource
# requests from config_osc.sh. SLURM does not expand shell variables in
# #SBATCH directives, so this wrapper writes a temporary batch file with
# hardcoded values, then submits it.
#
# Usage:
#   1. Edit config_osc.sh with your paths and account
#   2. bash calibration/osc/submit_cardinal.sh
#
# Monitor: squeue -u $USER
# Cancel:  scancel <job_id>
# =============================================================================

set -e

# Source configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config_osc.sh"

# Create the actual SLURM batch script with hardcoded values
BATCH_SCRIPT="${PROJECT_ROOT}/calibration/osc/_batch_generated.sh"

cat > "$BATCH_SCRIPT" << SLURM_EOF
#!/bin/bash
#SBATCH --job-name=fvs-cal
#SBATCH --account=${OSC_ACCOUNT}
#SBATCH --array=0-24
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=${CORES_PER_VARIANT}
#SBATCH --mem=${MEM_PER_VARIANT}
#SBATCH --time=${TIME_PER_VARIANT}
#SBATCH --output=${PROJECT_ROOT}/calibration/logs/slurm_%A_%a.out
#SBATCH --error=${PROJECT_ROOT}/calibration/logs/slurm_%A_%a.err

# ============================================================================
# Variant Array Mapping
# ============================================================================

VARIANTS=(
  acd ak bc bm ca ci cr cs ec em
  ie kt ls nc ne oc on op pn sn
  so tt ut wc ws
)

VARIANT="\${VARIANTS[\$SLURM_ARRAY_TASK_ID]}"

if [ -z "\$VARIANT" ]; then
    echo "ERROR: Invalid array task ID \$SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "==========================================="
echo "FVS Calibration: Variant \${VARIANT}"
echo "Array Task: \${SLURM_ARRAY_TASK_ID} of \${#VARIANTS[@]}"
echo "Node: \$(hostname)"
echo "Start: \$(date)"
echo "==========================================="

# ============================================================================
# Load Modules
# ============================================================================

module purge
module load ${GCC_MODULE}
module load ${R_MODULE}
module load ${GDAL_MODULE}
module load ${PROJ_MODULE}
module load ${GEOS_MODULE}

# ============================================================================
# Set Environment
# ============================================================================

# OSC uses per cluster R library paths: ~/R/<cluster>/<R_version>
# Detect dynamically from R itself to handle any cluster/version combo
export R_LIBS_USER="\$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=\${SLURM_CPUS_PER_TASK}
export CMDSTAN_NUM_THREADS=\${SLURM_CPUS_PER_TASK}

# Tell the R scripts where the FIA data lives
export FVS_FIA_DATA_DIR="${FIA_DATA_DIR}"
export FVS_PROJECT_ROOT="${PROJECT_ROOT}"

# ============================================================================
# Create Directories
# ============================================================================

mkdir -p "${PROJECT_ROOT}/calibration/logs"
mkdir -p "${PROJECT_ROOT}/calibration/output/variants/\${VARIANT}"
mkdir -p "${PROJECT_ROOT}/calibration/data/processed/\${VARIANT}"

# ============================================================================
# Run Pipeline
# ============================================================================

SCRIPTS_DIR="${PROJECT_ROOT}/calibration/R"
LOG="${PROJECT_ROOT}/calibration/logs/variant_\${VARIANT}.log"

echo "Starting pipeline for variant \${VARIANT}..." | tee "\$LOG"
echo "FIA data: ${FIA_DATA_DIR}" | tee -a "\$LOG"
echo "Cores: \${SLURM_CPUS_PER_TASK}" | tee -a "\$LOG"

# Step 01: Fetch and prepare FIA data from local files
echo "[\$(date)] STEP 01: Preparing FIA data..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/01_fetch_fia_data.R" \\
    --variant "\$VARIANT" --fia-dir "${FIA_DATA_DIR}" >> "\$LOG" 2>&1
echo "[\$(date)] STEP 01: Done" | tee -a "\$LOG"

# Step 02: Diameter growth (REQUIRED)
echo "[\$(date)] STEP 02: Diameter growth model..." | tee -a "\$LOG"
if ! Rscript "\${SCRIPTS_DIR}/02_fit_diameter_growth.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1; then
    echo "FATAL: Diameter growth failed for \${VARIANT}" | tee -a "\$LOG"
    exit 1
fi
echo "[\$(date)] STEP 02: Done" | tee -a "\$LOG"

# Step 03: Height diameter
echo "[\$(date)] STEP 03: Height diameter model..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/03_fit_height_diameter.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 03: Done" | tee -a "\$LOG"

# Step 03b: Height increment (auto skips if variant lacks HG params)
echo "[\$(date)] STEP 03b: Height increment model..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/03b_fit_height_increment.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 03b: Done" | tee -a "\$LOG"

# Step 04: Mortality
echo "[\$(date)] STEP 04: Mortality model..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/04_fit_mortality.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 04: Done" | tee -a "\$LOG"

# Step 05: Crown ratio
echo "[\$(date)] STEP 05: Crown ratio model..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/05_fit_crown_ratio.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 05: Done" | tee -a "\$LOG"

# Step 08: Stand level data extraction
echo "[\$(date)] STEP 08: Stand level data..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/08_fetch_stand_data.R" \\
    --variant "\$VARIANT" --fia-dir "${FIA_DATA_DIR}" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 08: Done" | tee -a "\$LOG"

# Step 09: SDIMAX / BAMAX / self thinning calibration
echo "[\$(date)] STEP 09: Stand density calibration..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/09_fit_stand_density.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 09: Done" | tee -a "\$LOG"

# Step 06: Convert all posteriors to calibrated JSON (REQUIRED)
echo "[\$(date)] STEP 06: Posterior to JSON..." | tee -a "\$LOG"
if ! Rscript "\${SCRIPTS_DIR}/06_posterior_to_json.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1; then
    echo "FATAL: Posterior to JSON failed for \${VARIANT}" | tee -a "\$LOG"
    exit 1
fi
echo "[\$(date)] STEP 06: Done" | tee -a "\$LOG"

# Step 07: Diagnostics
echo "[\$(date)] STEP 07: Diagnostics..." | tee -a "\$LOG"
Rscript "\${SCRIPTS_DIR}/07_diagnostics.R" \\
    --variant "\$VARIANT" >> "\$LOG" 2>&1 || true
echo "[\$(date)] STEP 07: Done" | tee -a "\$LOG"

# ============================================================================
# Summary
# ============================================================================

echo "" | tee -a "\$LOG"
echo "==========================================" | tee -a "\$LOG"
echo "Pipeline complete for variant: \${VARIANT}" | tee -a "\$LOG"
echo "End time: \$(date)" | tee -a "\$LOG"

echo "" | tee -a "\$LOG"
echo "Outputs:" | tee -a "\$LOG"
ls -lh "${PROJECT_ROOT}/calibration/output/variants/\${VARIANT}/" 2>/dev/null | tee -a "\$LOG"

if [ -f "${PROJECT_ROOT}/config/calibrated/\${VARIANT}.json" ]; then
    echo "Calibrated config: config/calibrated/\${VARIANT}.json" | tee -a "\$LOG"
else
    echo "WARNING: No calibrated config produced" | tee -a "\$LOG"
fi
SLURM_EOF

chmod +x "$BATCH_SCRIPT"

# Create log directory
mkdir -p "${PROJECT_ROOT}/calibration/logs"

echo "==========================================="
echo "FVS Calibration: Submitting to Cardinal"
echo "==========================================="
echo "Account:   ${OSC_ACCOUNT}"
echo "Variants:  25 (array 0-24)"
echo "Cores:     ${CORES_PER_VARIANT} per variant"
echo "Memory:    ${MEM_PER_VARIANT} per variant"
echo "Walltime:  ${TIME_PER_VARIANT} per variant"
echo "FIA data:  ${FIA_DATA_DIR}"
echo "Project:   ${PROJECT_ROOT}"
echo ""

# Submit
sbatch "$BATCH_SCRIPT"

echo ""
echo "Monitor with: squeue -u \$USER"
echo "Logs at:      ${PROJECT_ROOT}/calibration/logs/"

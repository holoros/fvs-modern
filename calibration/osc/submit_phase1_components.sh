#!/bin/bash
# =============================================================================
# FVS Bayesian Calibration: Phase 1 Component Models
#
# Submits SLURM array job to fit all component models EXCEPT diameter growth,
# which was already completed in the prior batch. This covers:
#
#   03  Height diameter (Chapman Richards, brms)
#   03b Height increment (CmdStanR, variants with HG params only)
#   04  Mortality (logistic, brms)
#   05  Crown ratio change (linear, brms)
#   08  Stand level data extraction (rFIA)
#   09  SDIMAX / BAMAX / self thinning (quantreg + brms)
#   06  Posterior to JSON (combines all components)
#
# Prerequisites:
#   - FIA data already exists in calibration/data/processed/<variant>/
#   - Diameter growth posteriors exist in calibration/output/variants/<variant>/
#   - R packages installed on Cardinal (brms, cmdstanr, rFIA, quantreg, etc.)
#
# Usage:
#   bash calibration/osc/submit_phase1_components.sh
#   bash calibration/osc/submit_phase1_components.sh --variants "ca ne cs"
#   bash calibration/osc/submit_phase1_components.sh --skip-stand-density
#
# Monitor: squeue -u $USER
# Cancel:  scancel <job_id>
# =============================================================================

set -e

# Source configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config_osc.sh"

# =============================================================================
# Parse Arguments
# =============================================================================

CUSTOM_VARIANTS=""
SKIP_STAND_DENSITY=0

while [[ $# -gt 0 ]]; do
    case $1 in
        --variants)
            CUSTOM_VARIANTS="$2"
            shift 2
            ;;
        --skip-stand-density)
            SKIP_STAND_DENSITY=1
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--variants 'ca ne cs'] [--skip-stand-density]"
            exit 1
            ;;
    esac
done

# =============================================================================
# Build Variant List
# =============================================================================

if [ -n "$CUSTOM_VARIANTS" ]; then
    # User specified variants
    read -ra VARIANTS <<< "$CUSTOM_VARIANTS"
else
    # All 25 FVS variants
    VARIANTS=(
        acd ak bc bm ca ci cr cs ec em
        ie kt ls nc ne oc on op pn sn
        so tt ut wc ws
    )
fi

N_VARIANTS=${#VARIANTS[@]}
ARRAY_MAX=$((N_VARIANTS - 1))

echo "==========================================="
echo "FVS Phase 1: Component Model Calibration"
echo "==========================================="
echo "Variants:  ${N_VARIANTS} (${VARIANTS[*]})"
echo "Account:   ${OSC_ACCOUNT}"
echo "Cores:     ${CORES_PER_VARIANT} per variant"
echo "Memory:    ${MEM_PER_VARIANT} per variant"
echo "Walltime:  ${TIME_PER_VARIANT} per variant"
echo "Skip SDI:  ${SKIP_STAND_DENSITY}"
echo "FIA data:  ${FIA_DATA_DIR}"
echo "Project:   ${PROJECT_ROOT}"
echo ""

# =============================================================================
# Generate SLURM Batch Script
# =============================================================================

BATCH_SCRIPT="${PROJECT_ROOT}/calibration/osc/_phase1_generated.sh"

# Build the variant array string for the batch script
VARIANT_STR=""
for v in "${VARIANTS[@]}"; do
    VARIANT_STR="${VARIANT_STR} ${v}"
done

cat > "$BATCH_SCRIPT" << SLURM_EOF
#!/bin/bash
#SBATCH --job-name=fvs-p1
#SBATCH --account=${OSC_ACCOUNT}
#SBATCH --array=0-${ARRAY_MAX}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=${CORES_PER_VARIANT}
#SBATCH --mem=${MEM_PER_VARIANT}
#SBATCH --time=${TIME_PER_VARIANT}
#SBATCH --output=${PROJECT_ROOT}/calibration/logs/phase1_%A_%a.out
#SBATCH --error=${PROJECT_ROOT}/calibration/logs/phase1_%A_%a.err

# ============================================================================
# Variant Mapping
# ============================================================================

VARIANTS=(${VARIANT_STR})

VARIANT="\${VARIANTS[\$SLURM_ARRAY_TASK_ID]}"

if [ -z "\$VARIANT" ]; then
    echo "ERROR: Invalid array task ID \$SLURM_ARRAY_TASK_ID"
    exit 1
fi

echo "==========================================="
echo "FVS Phase 1: \${VARIANT}"
echo "Array Task: \${SLURM_ARRAY_TASK_ID} of \${#VARIANTS[@]}"
echo "Node: \$(hostname)"
echo "Cores: \${SLURM_CPUS_PER_TASK}"
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
# Environment
# ============================================================================

export R_LIBS_USER="\$(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
export OMP_NUM_THREADS=\${SLURM_CPUS_PER_TASK}
export CMDSTAN_NUM_THREADS=\${SLURM_CPUS_PER_TASK}
export FVS_FIA_DATA_DIR="${FIA_DATA_DIR}"
export FVS_PROJECT_ROOT="${PROJECT_ROOT}"

# Max observations for subsampling (controls runtime)
export FVS_MAX_OBS=30000

# ============================================================================
# Directories
# ============================================================================

SCRIPTS_DIR="${PROJECT_ROOT}/calibration/R"
OUTPUT_DIR="${PROJECT_ROOT}/calibration/output/variants/\${VARIANT}"
DATA_DIR="${PROJECT_ROOT}/calibration/data/processed/\${VARIANT}"
LOG="${PROJECT_ROOT}/calibration/logs/phase1_\${VARIANT}.log"

mkdir -p "\${OUTPUT_DIR}"
mkdir -p "${PROJECT_ROOT}/calibration/logs"

# ============================================================================
# Preflight Checks
# ============================================================================

echo "[\$(date)] Preflight checks for \${VARIANT}..." | tee "\$LOG"

# Verify FIA data exists (produced by step 01 in prior run)
if [ ! -f "\${DATA_DIR}/diameter_growth.csv" ]; then
    echo "FATAL: No diameter_growth.csv for \${VARIANT}. Run 01_fetch_fia_data.R first." | tee -a "\$LOG"
    exit 1
fi

# Verify diameter growth posteriors exist (produced by step 02 in prior run)
if [ ! -f "\${OUTPUT_DIR}/diameter_growth_map.csv" ] && \
   [ ! -f "\${OUTPUT_DIR}/diameter_growth_posterior.csv" ]; then
    echo "WARNING: No diameter growth output for \${VARIANT}. Step 06 may be incomplete." | tee -a "\$LOG"
fi

# Count available data files
echo "Available data files:" | tee -a "\$LOG"
ls -1 "\${DATA_DIR}/"*.csv 2>/dev/null | while read f; do
    echo "  \$(basename \$f): \$(wc -l < \$f) lines" | tee -a "\$LOG"
done

# ============================================================================
# Step 03: Height Diameter Model (Chapman Richards via brms)
# ============================================================================

echo "" | tee -a "\$LOG"
echo "[\$(date)] STEP 03: Height diameter model..." | tee -a "\$LOG"

if [ -f "\${DATA_DIR}/height_diameter.csv" ]; then
    if Rscript "\${SCRIPTS_DIR}/03_fit_height_diameter.R" \
        --variant "\$VARIANT" >> "\$LOG" 2>&1; then
        echo "[\$(date)] STEP 03: SUCCESS" | tee -a "\$LOG"
    else
        echo "[\$(date)] STEP 03: FAILED (non fatal, continuing)" | tee -a "\$LOG"
    fi
else
    echo "[\$(date)] STEP 03: SKIPPED (no height_diameter.csv)" | tee -a "\$LOG"
fi

# ============================================================================
# Step 03b: Height Increment Model (CmdStanR)
# Automatically skips if variant lacks HG parameters in config
# ============================================================================

echo "" | tee -a "\$LOG"
echo "[\$(date)] STEP 03b: Height increment model..." | tee -a "\$LOG"

if [ -f "\${DATA_DIR}/height_growth.csv" ]; then
    if Rscript "\${SCRIPTS_DIR}/03b_fit_height_increment.R" \
        --variant "\$VARIANT" >> "\$LOG" 2>&1; then
        echo "[\$(date)] STEP 03b: SUCCESS" | tee -a "\$LOG"
    else
        echo "[\$(date)] STEP 03b: SKIPPED or FAILED (non fatal)" | tee -a "\$LOG"
    fi
else
    echo "[\$(date)] STEP 03b: SKIPPED (no height_growth.csv)" | tee -a "\$LOG"
fi

# ============================================================================
# Step 04: Mortality Model (Logistic via brms)
# ============================================================================

echo "" | tee -a "\$LOG"
echo "[\$(date)] STEP 04: Mortality model..." | tee -a "\$LOG"

if [ -f "\${DATA_DIR}/mortality.csv" ]; then
    if Rscript "\${SCRIPTS_DIR}/04_fit_mortality.R" \
        --variant "\$VARIANT" >> "\$LOG" 2>&1; then
        echo "[\$(date)] STEP 04: SUCCESS" | tee -a "\$LOG"
    else
        echo "[\$(date)] STEP 04: FAILED (non fatal, continuing)" | tee -a "\$LOG"
    fi
else
    echo "[\$(date)] STEP 04: SKIPPED (no mortality.csv)" | tee -a "\$LOG"
fi

# ============================================================================
# Step 05: Crown Ratio Change Model (Linear via brms)
# ============================================================================

echo "" | tee -a "\$LOG"
echo "[\$(date)] STEP 05: Crown ratio model..." | tee -a "\$LOG"

if [ -f "\${DATA_DIR}/crown_ratio_change.csv" ]; then
    if Rscript "\${SCRIPTS_DIR}/05_fit_crown_ratio.R" \
        --variant "\$VARIANT" >> "\$LOG" 2>&1; then
        echo "[\$(date)] STEP 05: SUCCESS" | tee -a "\$LOG"
    else
        echo "[\$(date)] STEP 05: FAILED (non fatal, continuing)" | tee -a "\$LOG"
    fi
else
    echo "[\$(date)] STEP 05: SKIPPED (no crown_ratio_change.csv)" | tee -a "\$LOG"
fi

# ============================================================================
# Step 08 + 09: Stand Density (conditional)
# ============================================================================

SKIP_SDI=${SKIP_STAND_DENSITY}

if [ "\$SKIP_SDI" -eq 0 ]; then
    echo "" | tee -a "\$LOG"
    echo "[\$(date)] STEP 08: Stand level data extraction..." | tee -a "\$LOG"

    if Rscript "\${SCRIPTS_DIR}/08_fetch_stand_data.R" \
        --variant "\$VARIANT" --fia-dir "${FIA_DATA_DIR}" >> "\$LOG" 2>&1; then
        echo "[\$(date)] STEP 08: SUCCESS" | tee -a "\$LOG"

        echo "[\$(date)] STEP 09: Stand density calibration..." | tee -a "\$LOG"
        if Rscript "\${SCRIPTS_DIR}/09_fit_stand_density.R" \
            --variant "\$VARIANT" >> "\$LOG" 2>&1; then
            echo "[\$(date)] STEP 09: SUCCESS" | tee -a "\$LOG"
        else
            echo "[\$(date)] STEP 09: FAILED (non fatal)" | tee -a "\$LOG"
        fi
    else
        echo "[\$(date)] STEP 08: FAILED (non fatal, skipping step 09)" | tee -a "\$LOG"
    fi
else
    echo "" | tee -a "\$LOG"
    echo "[\$(date)] STEPS 08+09: SKIPPED (--skip-stand-density)" | tee -a "\$LOG"
fi

# ============================================================================
# Step 06: Posterior to JSON (combines ALL components)
# ============================================================================

echo "" | tee -a "\$LOG"
echo "[\$(date)] STEP 06: Posterior to JSON..." | tee -a "\$LOG"

if Rscript "\${SCRIPTS_DIR}/06_posterior_to_json.R" \
    --variant "\$VARIANT" >> "\$LOG" 2>&1; then
    echo "[\$(date)] STEP 06: SUCCESS" | tee -a "\$LOG"
else
    echo "[\$(date)] STEP 06: FAILED" | tee -a "\$LOG"
    echo "WARNING: Calibrated JSON not produced for \${VARIANT}" | tee -a "\$LOG"
fi

# ============================================================================
# Summary
# ============================================================================

echo "" | tee -a "\$LOG"
echo "==========================================" | tee -a "\$LOG"
echo "Phase 1 complete for variant: \${VARIANT}" | tee -a "\$LOG"
echo "End time: \$(date)" | tee -a "\$LOG"
echo "==========================================" | tee -a "\$LOG"

echo "" | tee -a "\$LOG"
echo "Output files:" | tee -a "\$LOG"
ls -lh "\${OUTPUT_DIR}/" 2>/dev/null | tee -a "\$LOG"

# Count successful components
N_DONE=0
for f in height_diameter_posterior.csv height_increment_posterior.csv \
         mortality_posterior.csv crown_ratio_posterior.csv \
         stand_density_posterior.csv; do
    if [ -f "\${OUTPUT_DIR}/\${f}" ]; then
        N_DONE=\$((N_DONE + 1))
    fi
done

echo "" | tee -a "\$LOG"
echo "Components completed: \${N_DONE} of 5 possible" | tee -a "\$LOG"

if [ -f "${PROJECT_ROOT}/config/calibrated/\${VARIANT}.json" ]; then
    echo "Calibrated config: YES" | tee -a "\$LOG"
else
    echo "Calibrated config: NO" | tee -a "\$LOG"
fi
SLURM_EOF

chmod +x "$BATCH_SCRIPT"

# Create log directory
mkdir -p "${PROJECT_ROOT}/calibration/logs"

echo ""
echo "Generated batch script: $BATCH_SCRIPT"
echo ""

# Submit
sbatch "$BATCH_SCRIPT"

echo ""
echo "Monitor with: squeue -u \$USER"
echo "Logs at:      ${PROJECT_ROOT}/calibration/logs/phase1_*.log"
echo "Cancel with:  scancel <job_id>"

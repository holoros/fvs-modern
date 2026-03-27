#!/bin/bash
# =============================================================================
# FVS Calibration: OSC Configuration
#
# Edit these paths ONCE before running the pipeline.
# This file is sourced by all SLURM scripts.
# =============================================================================

# --- Your OSC allocation (run `id -gn` or check https://my.osc.edu) ---
export OSC_ACCOUNT="PAS_CHANGEME"        # <-- CHANGE THIS to your project code

# --- Where you cloned fvs-modern ---
export PROJECT_ROOT="/fs/ess/scratch/${USER}/fvs-modern"

# --- Where you unzipped ENTIRE_FIA.zip ---
# After: cd /fs/ess/scratch/$USER && unzip ENTIRE_FIA.zip
# The directory should contain state subdirectories (AL/, AK/, AZ/, ... WY/)
# each with CSV files (TREE.csv, PLOT.csv, COND.csv, etc.)
export FIA_DATA_DIR="/fs/ess/scratch/${USER}/ENTIRE_FIA"

# --- Cluster selection ---
export OSC_CLUSTER="cardinal"             # cardinal, pitzer, or owens
export OSC_PARTITION="serial"             # serial for single-node jobs

# --- Resource requests per variant ---
export CORES_PER_VARIANT=8                # Stan uses 4 chains, brms uses all cores
export MEM_PER_VARIANT="32G"              # 32 GB should cover all variants
export TIME_PER_VARIANT="36:00:00"        # 36 hours max (large variants like NE)

# --- R configuration ---
export R_MODULE="R/4.4.0"                 # module load name (check: module spider R)
export GNU_MODULE="gnu/13.2.0"            # compiler for CmdStan

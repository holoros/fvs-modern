#!/bin/bash
# =============================================================================
# FVS Calibration: OSC Configuration
#
# Edit these paths ONCE before running the pipeline.
# This file is sourced by all SLURM scripts.
# =============================================================================

# --- Your HPC allocation (run `sacctmgr show assoc user=$USER`) ---
# Override with FVS_HPC_ACCOUNT environment variable
export OSC_ACCOUNT="${FVS_HPC_ACCOUNT:-PUOM0008}"

# --- Where you cloned fvs-modern ---
# Override with FVS_PROJECT_ROOT or FVS_HPC_REMOTE_ROOT environment variable
export PROJECT_ROOT="${FVS_HPC_REMOTE_ROOT:-${FVS_PROJECT_ROOT:-/users/${OSC_ACCOUNT}/${USER}/fvs-modern}}"

# --- Where the FIA data lives (state subdirectory CSVs) ---
# Override with FVS_FIA_DATA_DIR environment variable
export FIA_DATA_DIR="${FVS_FIA_DATA_DIR:-/users/${OSC_ACCOUNT}/${USER}/FIA}"

# --- Cluster selection ---
export OSC_CLUSTER="cardinal"             # cardinal, pitzer, or owens
export OSC_PARTITION="serial"             # serial for single node jobs

# --- Resource requests per variant ---
export CORES_PER_VARIANT=8                # Stan uses 4 chains, brms uses all cores
export MEM_PER_VARIANT="32G"              # 32 GB should cover all variants
export TIME_PER_VARIANT="36:00:00"        # 36 hours max (large variants like NE)

# --- Module configuration ---
# These are loaded by setup_osc.sh and submit_cardinal.sh
# R spatial packages (sf, terra) need gdal/proj/geos
export R_MODULE="R/4.4.0"
export GCC_MODULE="gcc/12.3.0"
export GDAL_MODULE="gdal/3.7.3"
export PROJ_MODULE="proj/9.2.1"
export GEOS_MODULE="geos/3.12.0"

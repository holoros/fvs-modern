# =============================================================================
# Title: FVS Bayesian Calibration Pipeline: Full Reproducibility Script
# Author: A. Weiskittel
# Date: 2026-03-30
# Description:
#   Comprehensive script to reproduce the Bayesian calibration of all 25 FVS
#   geographic variants using FIA remeasurement data. This script orchestrates
#   FIA data acquisition, Bayesian model fitting (brms/CmdStanR), posterior
#   extraction, and calibrated JSON config generation.
#
#   Pipeline steps per variant:
#     01  Fetch FIA data and build remeasurement pairs
#     02  Fit diameter growth (Wykoff log-linear, species random intercepts)
#     03  Fit height-diameter (Chapman-Richards nonlinear, species random a, b)
#     03b Fit height increment (only for variants with explicit HG params)
#     04  Fit mortality (logistic, species random intercepts)
#     05  Fit crown ratio change (linear, species random intercepts)
#     08  Fetch and compute stand-level density metrics from FIA
#     09  Fit SDIMAX (quantile regression + Bayesian hierarchical)
#     06  Convert all posteriors to calibrated JSON config
#     10  Comprehensive comparison of default vs calibrated parameters
#
#   All models use multi-strategy inference:
#     Stage 1: MAP optimization (LBFGS) for initial values
#     Stage 2: HMC sampling (4 chains, 1000-1500 iter) with MAP-based init
#     Stage 3: Variational inference (ADVI meanfield) as fallback
#
# Dependencies:
#   R >= 4.2.0
#   CmdStan >= 2.33 (installed via cmdstanr::install_cmdstan())
#   FIA ENTIRE flat files: ENTIRE_TREE.csv, ENTIRE_PLOT.csv, ENTIRE_COND.csv
#     (download from https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html)
#   FVS variant config JSONs: config/{variant}.json (25 files)
#
# HPC Environment:
#   Original fitting performed on Ohio Supercomputer Center (OSC) Cardinal
#   cluster using SLURM array jobs. Account: PUOM0008. Each variant was
#   allocated 128 GB RAM and 28 cores for 4-6 hours wall time.
#
# To reproduce on an HPC with SLURM:
#   sbatch calibration/slurm/submit_calibration.sh
#
# To reproduce locally (one variant at a time):
#   Rscript 00_reproduce_calibration.R --variant ne --fia-dir /path/to/FIA
# =============================================================================

# --- Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(brms)
library(cmdstanr)
library(jsonlite)
library(posterior)
library(bayesplot)
library(tidybayes)
library(quantreg)
library(rFIA)
library(logger)
library(parallel)

# --- Configuration -----------------------------------------------------------

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
variant_arg <- NULL
fia_dir_arg <- NULL
run_all <- FALSE

for (i in seq_along(args)) {
  if (args[i] == "--variant" && i < length(args)) variant_arg <- tolower(args[i + 1])
  if (args[i] == "--fia-dir" && i < length(args)) fia_dir_arg <- args[i + 1]
  if (args[i] == "--all") run_all <- TRUE
}

# All 25 FVS geographic variants
ALL_VARIANTS <- c("acd", "ak", "bc", "bm", "ca", "ci", "cr", "cs",
                   "ec", "em", "ie", "kt", "ls", "nc", "ne", "oc",
                   "on", "op", "pn", "sn", "so", "tt", "ut", "wc", "ws")

variants_to_run <- if (run_all) ALL_VARIANTS else if (!is.null(variant_arg)) variant_arg else "ne"

# Project root (adjust to your local setup)
PROJECT_ROOT <- Sys.getenv("FVS_PROJECT_ROOT", getwd())
CALIBRATION_DIR <- file.path(PROJECT_ROOT, "calibration")
CONFIG_DIR <- file.path(PROJECT_ROOT, "config")
FIA_DIR <- if (!is.null(fia_dir_arg)) fia_dir_arg else Sys.getenv("FVS_FIA_DATA_DIR", "")

# Global settings
MAX_OBS <- 30000     # Subsample size per model per variant
SEED <- 42           # Reproducibility seed
N_CHAINS <- 4
N_CORES <- min(4, parallel::detectCores())

options(mc.cores = N_CORES, brms.backend = "cmdstanr")

cat("=============================================================\n")
cat("FVS Bayesian Calibration Pipeline\n")
cat("=============================================================\n")
cat("Project root:", PROJECT_ROOT, "\n")
cat("Variants:", paste(variants_to_run, collapse = ", "), "\n")
cat("FIA data:", FIA_DIR, "\n")
cat("Max obs per model:", MAX_OBS, "\n")
cat("Cores:", N_CORES, "\n")
cat("=============================================================\n\n")

# --- Pipeline ----------------------------------------------------------------

# Each step is implemented in its own R script (see calibration/R/).
# This master script sources them in sequence for each variant.
# For HPC use, the SLURM array job calls run_variant.sh instead.

for (variant in variants_to_run) {
  cat("\n###########################################################\n")
  cat("## Processing variant:", toupper(variant), "\n")
  cat("###########################################################\n\n")

  Sys.setenv(FVS_PROJECT_ROOT = PROJECT_ROOT)
  if (nchar(FIA_DIR) > 0) Sys.setenv(FVS_FIA_DATA_DIR = FIA_DIR)
  Sys.setenv(FVS_MAX_OBS = as.character(MAX_OBS))

  scripts_dir <- file.path(CALIBRATION_DIR, "R")

  # Step 01: Fetch FIA data
  cat(">> Step 01: Fetching FIA data...\n")
  tryCatch(
    source_with_args(file.path(scripts_dir, "01_fetch_fia_data.R"),
                     c("--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 02: Diameter growth
  cat(">> Step 02: Fitting diameter growth model...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "02_fit_diameter_growth.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 03: Height-diameter
  cat(">> Step 03: Fitting height-diameter model...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "03_fit_height_diameter.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 03b: Height increment (auto-skips if no HG params)
  cat(">> Step 03b: Fitting height increment model...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "03b_fit_height_increment.R"),
                          "--variant", variant)),
    error = function(e) cat("  SKIPPED or FAILED:", conditionMessage(e), "\n")
  )

  # Step 04: Mortality
  cat(">> Step 04: Fitting mortality model...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "04_fit_mortality.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 05: Crown ratio change
  cat(">> Step 05: Fitting crown ratio change model...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "05_fit_crown_ratio.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 08: Fetch stand-level data
  cat(">> Step 08: Extracting stand-level density data...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "08_fetch_stand_data.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 09: Fit SDIMAX
  cat(">> Step 09: Calibrating stand density parameters...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "09_fit_stand_density.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  # Step 06: Convert posteriors to JSON
  cat(">> Step 06: Exporting calibrated JSON config...\n")
  tryCatch(
    system2("Rscript", c(file.path(scripts_dir, "06_posterior_to_json.R"),
                          "--variant", variant)),
    error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
  )

  cat(">> Variant", toupper(variant), "complete.\n")
}

# Step 10: Cross-variant comparison (runs after all variants)
cat("\n###########################################################\n")
cat("## Step 10: Cross-Variant Comprehensive Comparison\n")
cat("###########################################################\n\n")
tryCatch(
  system2("Rscript", c(file.path(scripts_dir, "10_comprehensive_comparison.R"),
                        "--all")),
  error = function(e) cat("  FAILED:", conditionMessage(e), "\n")
)

cat("\n=============================================================\n")
cat("Pipeline complete.\n")
cat("Calibrated configs: config/calibrated/{variant}.json\n")
cat("Comparison outputs: calibration/output/comparisons/\n")
cat("=============================================================\n")

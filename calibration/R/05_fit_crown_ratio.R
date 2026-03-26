#!/usr/bin/env Rscript
#
# FVS Bayesian Calibration: Fit Crown Ratio Model
# Model change in crown ratio between measurements
# Uses BCR1-BCR4 coefficients from FVS config as informative priors
#
# Usage: Rscript calibration/R/05_fit_crown_ratio.R --variant ca
#

library(tidyverse)
library(data.table)
library(brms)
library(cmdstanr)
library(jsonlite)
library(posterior)
library(bayesplot)
library(tidybayes)
library(logger)

# ============================================================================
# Parse Command Line Arguments
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
variant <- "ca"

if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] == "--variant" & i < length(args)) {
      variant <- args[i + 1]
    }
  }
}

# ============================================================================
# Configuration
# ============================================================================

project_root <- "/home/aweiskittel/Documents/Claude/fvs-modern"
calibration_dir <- file.path(project_root, "calibration")
processed_data_dir <- file.path(calibration_dir, "data", "processed")
output_dir <- file.path(calibration_dir, "output", "variants", variant)

# Create output directory
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Set up logging
log_file <- file.path(calibration_dir, "logs", paste0("05_fit_crown_ratio_", variant, ".log"))
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting crown ratio model fitting for variant {variant}")

# ============================================================================
# Load Data
# ============================================================================

logger::log_info("Loading FIA data for variant {variant}...")

data_file <- file.path(processed_data_dir, variant, "diameter_growth.csv")

if (!file.exists(data_file)) {
  logger::log_error("Data file not found: {data_file}")
  stop("Data file not found: ", data_file)
}

cr_data <- read_csv(data_file, show_col_types = FALSE) %>%
  as_tibble() %>%
  # Calculate change in crown ratio
  mutate(
    delta_CR = CR - lead(CR),
    CR_change = delta_CR / years_interval,  # Annual change
    SPCD = as.factor(SPCD),
    PLT_CN = as.factor(PLT_CN),
    # Standardize predictors
    DBH_std = scale(DIA_t1)[, 1],
    BA_std = scale(BA)[, 1],
    BAL_std = scale(BAL_sqft)[, 1],
    CR_std = scale(CR)[, 1],
    SI_std = scale(log(pmax(SI, 1)))[, 1]
  ) %>%
  # Use only trees where CR changes are observed
  filter(!is.na(CR_change), !is.infinite(CR_change), !is.nan(CR_change))

logger::log_info("Loaded {nrow(cr_data)} tree observations with CR changes")

# ============================================================================
# Load FVS Config for Priors
# ============================================================================

logger::log_info("Loading FVS parameters for priors...")

config_file <- file.path(project_root, "config", paste0(variant, ".json"))
config <- fromJSON(config_file)

# Extract crown ratio coefficients from config if available
fvs_other <- config$categories$other

# Crown ratio parameter names in FVS: BCR1, BCR2, BCR3, BCR4, etc.
cr_param_names <- grep("^BC", names(fvs_other), value = TRUE)

# Use average values as prior
if (length(cr_param_names) > 0) {
  bcr_vals <- unlist(fvs_other[cr_param_names])
  bcr_mean <- mean(bcr_vals, na.rm = TRUE)
  bcr_sd <- sd(bcr_vals, na.rm = TRUE)
} else {
  bcr_mean <- 0
  bcr_sd <- 1
}

logger::log_info("Crown ratio prior means: mean={bcr_mean}, sd={bcr_sd}")

# ============================================================================
# Fit Crown Ratio Model Using brms
# ============================================================================

logger::log_info("Fitting crown ratio model with brms...")

# Bayesian model for crown ratio change
# Change in CR likely depends on: diameter, competition, site quality, current CR

fit_cr <- brm(
  CR_change ~ DBH_std + I(DBH_std^2) + BA_std + BAL_std + CR_std + SI_std +
    (1 | SPCD) +      # Species varying intercept
    (1 | PLT_CN),     # Plot random effect
  data = cr_data,
  family = gaussian(),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = parallel::detectCores(),
  backend = "cmdstanr",
  refresh = 0,
  verbose = FALSE,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

logger::log_info("Crown ratio model fitting complete")

# ============================================================================
# Check Convergence
# ============================================================================

logger::log_info("Checking convergence diagnostics...")

summ_cr <- posterior_summary(fit_cr)

if (any(summ_cr[, "Rhat"] > 1.01, na.rm = TRUE)) {
  logger::log_warn("Some parameters have Rhat > 1.01")
}

# ============================================================================
# Extract and Save Results
# ============================================================================

logger::log_info("Extracting posterior samples...")

# Get posterior draws
draws_cr <- as_draws_df(fit_cr)

# Save draws
draws_file <- file.path(output_dir, "crown_ratio_samples.rds")
saveRDS(draws_cr, draws_file)
logger::log_info("Saved posterior samples to {draws_file}")

# Save summary
summary_df <- as_tibble(summ_cr, rownames = "variable")
summary_file <- file.path(output_dir, "crown_ratio_summary.csv")
write_csv(summary_df, summary_file)
logger::log_info("Saved summary to {summary_file}")

# ============================================================================
# Posterior Predictive Checks
# ============================================================================

logger::log_info("Generating diagnostic plots...")

# PPC plot
pdf(file.path(output_dir, "crown_ratio_ppc.pdf"), width = 10, height = 6)
print(pp_check(fit_cr, ndraws = 100))
dev.off()

# Conditional effects
pdf(file.path(output_dir, "crown_ratio_effects.pdf"), width = 12, height = 8)
print(plot(conditional_effects(fit_cr)))
dev.off()

logger::log_info("Saved diagnostic plots")

# ============================================================================
# Summary Report
# ============================================================================

cat("\n")
cat("========================================\n")
cat("Crown Ratio Model Fitting Complete\n")
cat("========================================\n")
cat("Variant:", variant, "\n")
cat("Observations:", nrow(cr_data), "\n")
cat("Mean CR change (annual):", round(mean(cr_data$CR_change, na.rm = TRUE), 4), "\n")
cat("SD CR change:", round(sd(cr_data$CR_change, na.rm = TRUE), 4), "\n")
cat("\nModel:\n")
print(fit_cr)
cat("\nOutput saved to:", output_dir, "\n\n")

logger::log_info("Crown ratio model fitting complete")

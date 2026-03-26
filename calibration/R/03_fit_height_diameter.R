#!/usr/bin/env Rscript
#
# FVS Bayesian Calibration: Fit Height-Diameter Model
# Chapman-Richards equation: H = 4.5 + b0 * (1 - exp(-b1 * DBH))^b2
# Species-level fitting with site index interaction
#
# Usage: Rscript calibration/R/03_fit_height_diameter.R --variant ca
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
log_file <- file.path(calibration_dir, "logs", paste0("03_fit_htdbh_", variant, ".log"))
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting height-diameter model fitting for variant {variant}")

# ============================================================================
# Load Data
# ============================================================================

logger::log_info("Loading FIA data for variant {variant}...")

data_file <- file.path(processed_data_dir, variant, "diameter_growth.csv")

if (!file.exists(data_file)) {
  logger::log_error("Data file not found: {data_file}")
  stop("Data file not found: ", data_file)
}

dg_data <- read_csv(data_file, show_col_types = FALSE) %>%
  as_tibble() %>%
  # Select unique tree measurements (take the first measurement if multiple)
  group_by(tree_id) %>%
  slice(1) %>%
  ungroup() %>%
  # Remove missing heights
  filter(!is.na(HT_t1), HT_t1 > 4.5, !is.na(DIA_t1), DIA_t1 > 0) %>%
  mutate(
    SPCD = as.factor(SPCD),
    # Scale predictors for better model fitting
    DBH_scaled = DIA_t1 / 20,  # Center around typical diameter
    SI_scaled = pmax(SI, 1) / 50  # Center around typical SI
  )

logger::log_info("Loaded {nrow(dg_data)} tree-level observations")

# ============================================================================
# Load FVS Config for Priors
# ============================================================================

logger::log_info("Loading FVS parameters for priors...")

config_file <- file.path(project_root, "config", paste0(variant, ".json"))
config <- fromJSON(config_file)

# Chapman Richards parameters: H = b0 + b1 * (1 - exp(-b2 * DBH))^b3
# In FVS these may be parameterized differently
# We'll use weakly informative priors based on typical values

# ============================================================================
# Fit Height-Diameter Model Using brms
# ============================================================================

logger::log_info("Fitting height-diameter model with brms...")

# Chapman-Richards equation using brms nonlinear model
# H = 4.5 + a * (1 - exp(-b * DBH))^c

# Specify priors
priors_htdbh <- c(
  # Intercept at DBH=0 should be ~4.5 ft (breast height)
  prior(normal(25, 10), class = "b", coef = "a"),      # Asymptotic height
  prior(normal(0.05, 0.02), class = "b", coef = "b"),  # Rate parameter
  prior(normal(1, 0.5), class = "b", coef = "c"),      # Shape parameter
  prior(exponential(1), class = "sigma")               # Error scale
)

# Fit nonlinear model with species-level effects
fit_htdbh <- brm(
  # Nonlinear Chapman-Richards model
  bf(
    HT_t1 ~ 4.5 + a * (1 - exp(-b * DBH_scaled))^c,
    a ~ 1 + (1 | SPCD),      # Species varying intercept
    b ~ 1 + (1 | SPCD),      # Species varying rate
    c ~ 1,                   # Shared shape parameter
    nl = TRUE
  ),
  data = dg_data,
  family = gaussian(),
  prior = priors_htdbh,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = parallel::detectCores(),
  backend = "cmdstanr",
  refresh = 0,
  verbose = FALSE,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

logger::log_info("Height-diameter model fitting complete")

# ============================================================================
# Check Convergence
# ============================================================================

logger::log_info("Checking convergence diagnostics...")

# Summary
summ_htdbh <- posterior_summary(fit_htdbh)

# Check for convergence issues
if (any(summ_htdbh[, "Rhat"] > 1.01, na.rm = TRUE)) {
  logger::log_warn("Some parameters have Rhat > 1.01")
}

# ============================================================================
# Extract and Save Results
# ============================================================================

logger::log_info("Extracting posterior samples...")

# Get posterior draws
draws_htdbh <- as_draws_df(fit_htdbh)

# Save draws
draws_file <- file.path(output_dir, "height_diameter_samples.rds")
saveRDS(draws_htdbh, draws_file)
logger::log_info("Saved posterior samples to {draws_file}")

# Save summary
summary_df <- as_tibble(summ_htdbh, rownames = "variable")
summary_file <- file.path(output_dir, "height_diameter_summary.csv")
write_csv(summary_df, summary_file)
logger::log_info("Saved summary to {summary_file}")

# ============================================================================
# Posterior Predictive Checks
# ============================================================================

logger::log_info("Generating diagnostic plots...")

# PPC plot
pdf(file.path(output_dir, "height_diameter_ppc.pdf"), width = 10, height = 6)
print(pp_check(fit_htdbh, ndraws = 100))
dev.off()

# Conditional effects plots
pdf(file.path(output_dir, "height_diameter_effects.pdf"), width = 12, height = 8)
plot(conditional_effects(fit_htdbh, effects = "DBH_scaled:SPCD"))
dev.off()

logger::log_info("Saved diagnostic plots")

# ============================================================================
# Summary Report
# ============================================================================

cat("\n")
cat("========================================\n")
cat("Height-Diameter Model Fitting Complete\n")
cat("========================================\n")
cat("Variant:", variant, "\n")
cat("Species:", n_distinct(dg_data$SPCD), "\n")
cat("Observations:", nrow(dg_data), "\n")
cat("\nModel:\n")
print(fit_htdbh)
cat("\nOutput saved to:", output_dir, "\n\n")

logger::log_info("Height-diameter model fitting complete")

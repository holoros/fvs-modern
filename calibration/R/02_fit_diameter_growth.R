#!/usr/bin/env Rscript
#
# FVS Bayesian Calibration: Fit Wykoff Diameter Growth Model
# Bayesian hierarchical regression using CmdStanR and brms
#
# Usage: Rscript calibration/R/02_fit_diameter_growth.R --variant ca --species 122
# If no args provided, runs a quick test fit for variant ca
#

library(tidyverse)
library(data.table)
library(cmdstanr)
library(jsonlite)
library(posterior)
library(bayesplot)
library(tidybayes)
library(parallel)
library(progress)
library(logger)

# ============================================================================
# Parse Command Line Arguments
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)

variant <- "ca"  # Default
species_codes <- NULL

if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] == "--variant" & i < length(args)) {
      variant <- args[i + 1]
    }
    if (args[i] == "--species" & i < length(args)) {
      species_codes <- as.integer(strsplit(args[i + 1], ",")[[1]])
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
stan_dir <- file.path(calibration_dir, "stan")

# Create output directories
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Set up logging
log_file <- file.path(calibration_dir, "logs", paste0("02_fit_dg_", variant, ".log"))
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting diameter growth model fitting for variant {variant}")

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
  as_tibble()

logger::log_info("Loaded {nrow(dg_data)} observations")

# If species codes not provided, fit all species in the variant
if (is.null(species_codes)) {
  species_codes <- unique(dg_data$SPCD)
  logger::log_info("No species specified; will fit {length(species_codes)} species")
}

# Filter to requested species
dg_data <- dg_data %>%
  filter(SPCD %in% species_codes)

logger::log_info("After species filter: {nrow(dg_data)} observations")

# ============================================================================
# Load FVS Config for Priors
# ============================================================================

logger::log_info("Loading FVS parameters for priors...")

config_file <- file.path(project_root, "config", paste0(variant, ".json"))
config <- fromJSON(config_file)

# Extract growth parameters from config
# The config has WEIBB1 (intercept adjustment) and DG* parameters
fvs_params <- config$categories$other

# Map parameter names to our model
param_map <- list(
  b1_ref = "DGLD",           # ln(DBH) coefficient
  b2_ref = "DGDS",           # DBH^2 coefficient
  b3_ref = "DGSITE",         # ln(SI) coefficient
  b4_ref = "DGSLOP",         # SLOPE coefficient
  b5_ref = "DGSLSQ",         # SLOPE^2 coefficient
  b6_ref = "DGSASP",         # SLOPE*sin(ASP) coefficient
  b7_ref = "DGCASP",         # SLOPE*cos(ASP) coefficient
  b8_ref = "DGEL",           # ELEV coefficient
  b9_ref = "DGELSQ",         # ELEV^2 coefficient
  b10_ref = "DGCR",          # CR coefficient
  b11_ref = "DGCRSQ",        # CR^2 coefficient
  b12_ref = "DGBAL",         # BAL coefficient
  b13_ref = "DGPCCF"         # BA/CCF coefficient
)

# Extract reference values (use first species as reference)
prior_means <- sapply(param_map, function(param_name) {
  vals <- fvs_params[[param_name]]
  if (is.null(vals) || length(vals) == 0) {
    return(0)
  }
  # Use mean of all species' values
  mean(unlist(vals), na.rm = TRUE)
})

logger::log_info("Extracted prior means from FVS config")

# ============================================================================
# Prepare Data for Stan Model
# ============================================================================

logger::log_info("Preparing data for Stan model...")

# Create numeric indices for grouping variables
dg_model <- dg_data %>%
  # Create factor levels for species
  mutate(species_idx = as.numeric(factor(SPCD))) %>%
  # Create plot ID (unique combination of PLT_CN and variant)
  group_by(PLT_CN) %>%
  mutate(
    plot_idx = cur_group_id()
  ) %>%
  ungroup() %>%
  # Create location ID (based on lat/lon rounding to ~10 km grid)
  mutate(
    lat_grid = round(LAT * 10) / 10,
    lon_grid = round(LON * 10) / 10,
    location_key = paste0(lat_grid, "_", lon_grid),
    location_idx = as.numeric(factor(location_key))
  )

# Extract Stan data list
N <- nrow(dg_model)
N_species <- n_distinct(dg_model$species_idx)
N_plot <- n_distinct(dg_model$plot_idx)
N_location <- n_distinct(dg_model$location_idx)

logger::log_info("N = {N}, N_species = {N_species}, N_plot = {N_plot}, N_location = {N_location}")

# Create prior specifications for each species
# Use average FVS values, but allow species-level variation
prior_b0_species <- rep(prior_means["b1_ref"] * 0.5, N_species)  # Weakly informative
prior_b1_species <- rep(prior_means["b1_ref"], N_species)
prior_b2_species <- rep(prior_means["b2_ref"], N_species)
prior_b3_species <- rep(prior_means["b3_ref"], N_species)

stan_data <- list(
  N = N,
  N_species = N_species,
  N_plot = N_plot,
  N_location = N_location,

  # Response
  ln_DDS = dg_model$ln_DDS,

  # Predictors
  ln_DBH = dg_model$ln_DBH_std,  # Standardized
  DBH_sq = dg_model$DBH_sq_t1,
  ln_SI = dg_model$ln_SI,
  SLOPE = dg_model$SLOPE_std,
  SLOPE_sq = dg_model$SLOPE_sq,
  SLOPE_SASP = dg_model$SLOPE_SASP,
  SLOPE_CASP = dg_model$SLOPE_CASP,
  ELEV = dg_model$ELEV_std,
  ELEV_sq = dg_model$ELEV_sq,
  CR = dg_model$CR,
  CR_sq = dg_model$CR_sq,
  BAL = dg_model$BAL_sqft,
  BA = dg_model$BA,

  # Group indices
  species_id = dg_model$species_idx,
  plot_id = dg_model$plot_idx,
  location_id = dg_model$location_idx,

  # Priors
  prior_b0 = prior_b0_species,
  prior_b1 = prior_b1_species,
  prior_b2 = prior_b2_species,
  prior_b3 = prior_b3_species,
  prior_b_mu = 0.0,
  prior_b_sigma = 1.0
)

# ============================================================================
# Compile Stan Model
# ============================================================================

logger::log_info("Compiling Stan model...")

stan_file <- file.path(stan_dir, "wykoff_dg.stan")

tryCatch({
  mod <- cmdstanr::cmdstan_model(
    stan_file,
    dir = output_dir  # Use variant dir for compiled model
  )
  logger::log_info("Stan model compiled successfully")
}, error = function(e) {
  logger::log_error("Failed to compile Stan model: {e$message}")
  stop(e)
})

# ============================================================================
# Fit Model
# ============================================================================

logger::log_info("Fitting Bayesian model (this may take several minutes)...")

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,  # Increase for better HMC behavior
  max_treedepth = 15,
  refresh = 500,  # Print progress every 500 iterations
  show_messages = FALSE
)

logger::log_info("Model fitting complete")

# ============================================================================
# Check Convergence Diagnostics
# ============================================================================

logger::log_info("Computing convergence diagnostics...")

diagnostics <- fit$diagnostic_summary()
logger::log_info("Diagnostic summary: {nrow(diagnostics)} issues detected")

# Get summary statistics
summ <- fit$summary()

# Check Rhat and ESS
rhat_issues <- summ %>%
  filter(rhat > 1.01 & !is.na(rhat))

if (nrow(rhat_issues) > 0) {
  logger::log_warn("Found {nrow(rhat_issues)} parameters with Rhat > 1.01")
  logger::log_info("Parameters with poor convergence:")
  logger::log_info("{paste(rhat_issues$variable, collapse = ', ')}")
}

# ============================================================================
# Extract and Save Posterior Samples
# ============================================================================

logger::log_info("Extracting posterior samples...")

# Get draws in tibble format
draws_df <- fit$draws(format = "df")

# Save raw samples
draws_file <- file.path(output_dir, "diameter_growth_samples.rds")
saveRDS(draws_df, draws_file)
logger::log_info("Saved posterior samples to {draws_file}")

# Save summary
summary_file <- file.path(output_dir, "diameter_growth_summary.csv")
write_csv(summ, summary_file)
logger::log_info("Saved summary statistics to {summary_file}")

# ============================================================================
# Extract Posterior Medians and Credible Intervals
# ============================================================================

logger::log_info("Computing posterior summaries...")

# Posterior medians and 95% CIs
posterior_summary <- summ %>%
  select(variable, median, q5, q95, rhat, ess_bulk) %>%
  rename(
    p50 = median,
    p05 = q5,
    p95 = q95
  ) %>%
  mutate(
    ci_width = p95 - p05,
    converged = rhat <= 1.01 & !is.na(rhat)
  )

# Save posterior summary
posterior_file <- file.path(output_dir, "diameter_growth_posterior.csv")
write_csv(posterior_summary, posterior_file)
logger::log_info("Saved posterior summaries to {posterior_file}")

# ============================================================================
# Generate Diagnostic Plots
# ============================================================================

logger::log_info("Generating diagnostic plots...")

# Traceplot for key parameters
key_params <- c("b0", "b1", "b2", "b3", "b4", "b10", "b12", "b13", "sigma")
key_draws <- draws_df %>%
  select(contains(key_params), .chain, .iteration)

pdf(file.path(output_dir, "diameter_growth_traceplots.pdf"), width = 12, height = 8)
print(bayesplot::mcmc_trace(
  as_draws_array(key_draws),
  regex_pars = paste0("(", paste(key_params, collapse = "|"), ")"),
  facet_args = list(nrow = 3)
))
dev.off()
logger::log_info("Saved traceplots to PDF")

# Posterior predictive check
y_rep <- fit$draws("ln_DDS_rep", format = "matrix")
ppc_file <- file.path(output_dir, "diameter_growth_ppc.pdf")
pdf(ppc_file, width = 10, height = 6)
print(bayesplot::ppc_dens_overlay(
  y = stan_data$ln_DDS,
  yrep = y_rep[1:100, ]  # Plot first 100 draws for clarity
))
dev.off()
logger::log_info("Saved posterior predictive check to {ppc_file}")

# ============================================================================
# Summary Report
# ============================================================================

cat("\n")
cat("=========================================\n")
cat("Diameter Growth Model Fitting Complete\n")
cat("=========================================\n")
cat("Variant:", variant, "\n")
cat("Species fitted:", length(species_codes), "\n")
cat("Total observations:", N, "\n")
cat("Number of plots:", N_plot, "\n")
cat("Number of locations:", N_location, "\n")
cat("\nModel summary:\n")
print(summary(fit))
cat("\nOutput saved to:", output_dir, "\n\n")

logger::log_info("Diameter growth model fitting complete for variant {variant}")

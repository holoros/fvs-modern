#!/usr/bin/env Rscript
# =============================================================================
# FVS Bayesian Calibration: Fit Height Increment Model
# For variants with explicit HG parameters (IE, CI, KT, BC, WS, EM)
#
# Variants WITHOUT HG parameters derive height growth from the H-D
# relationship and diameter growth prediction, so script 03
# (height-diameter) effectively calibrates their height growth.
#
# Model: ln(HTG) = species intercept + species*ln(DBH) + ln(HT) +
#         CR + SI + BAL + BA + SLOPE + cos(ASP) + species_group*ln(HT)
#         + plot random effect
#
# Usage: Rscript calibration/R/03b_fit_height_increment.R --variant ie
# =============================================================================

library(tidyverse)
library(data.table)
library(cmdstanr)
library(jsonlite)
library(posterior)
library(bayesplot)
library(tidybayes)
library(parallel)
library(logger)

# =============================================================================
# Parse Command Line Arguments
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
variant <- "ie"  # Default to IE which has HG params

if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] == "--variant" & i < length(args)) {
      variant <- args[i + 1]
    }
  }
}

# =============================================================================
# Configuration
# =============================================================================

project_root <- "/home/aweiskittel/Documents/Claude/fvs-modern"
calibration_dir <- file.path(project_root, "calibration")
processed_data_dir <- file.path(calibration_dir, "data", "processed")
output_dir <- file.path(calibration_dir, "output", "variants", variant)
stan_dir <- file.path(calibration_dir, "stan")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Logging
log_file <- file.path(calibration_dir, "logs",
                       paste0("03b_fit_htinc_", variant, ".log"))
dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting height increment model for variant {variant}")

# =============================================================================
# Check if Variant Has Explicit HG Parameters
# =============================================================================

config_file <- file.path(project_root, "config", paste0(variant, ".json"))
config <- fromJSON(config_file)

# Look for HG parameter keys in the config
hg_param_names <- grep("^HG", names(config$categories$other), value = TRUE)

if (length(hg_param_names) == 0) {
  logger::log_info(
    "Variant {variant} has no explicit HG parameters. ",
    "Height growth is derived from the H-D model (script 03). Skipping."
  )
  cat("Variant", variant, "does not have explicit height growth parameters.\n")
  cat("Height increment calibration handled by the H-D model (script 03).\n")
  quit(save = "no", status = 0)
}

logger::log_info("Found {length(hg_param_names)} HG parameter arrays: {paste(hg_param_names, collapse = ', ')}")

# =============================================================================
# Load Data
# =============================================================================

logger::log_info("Loading FIA data for variant {variant}...")

data_file <- file.path(processed_data_dir, variant, "diameter_growth.csv")

if (!file.exists(data_file)) {
  logger::log_error("Data file not found: {data_file}")
  stop("Data file not found: ", data_file)
}

raw_data <- read_csv(data_file, show_col_types = FALSE) %>%
  as_tibble()

# Compute height increment from remeasurement pairs
# Need both HT_t1 and HT_t2 to calculate HTG
hg_data <- raw_data %>%
  filter(
    !is.na(HT_t1), !is.na(HT_t2),
    HT_t1 > 4.5, HT_t2 > 4.5,        # Above breast height
    !is.na(DIA_t1), DIA_t1 > 0,
    !is.na(years_interval), years_interval > 0
  ) %>%
  mutate(
    # Annual height increment
    HTG_annual = (HT_t2 - HT_t1) / years_interval,
    # Keep only positive growth (exclude measurement errors and dying trees)
    # Small negative values can occur from measurement error
    HTG_annual = pmax(HTG_annual, 0.01)
  ) %>%
  filter(
    HTG_annual > 0,
    HTG_annual < 10  # Sanity cap: no tree grows >10 ft/yr (or m/yr)
  ) %>%
  mutate(
    ln_HTG = log(HTG_annual),
    ln_DBH = log(DIA_t1),
    ln_HT = log(HT_t1),
    CR_prop = CR / 100,             # Convert to proportion if in percent
    SI_scaled = SI / 100,           # Scale for numerical stability
    BAL_scaled = BAL_sqft / 100,
    BA_scaled = BA / 100,
    SLOPE_prop = SLOPE_std,         # Already standardized
    CASP = cos(ASPECT * pi / 180),  # Cosine of aspect in radians
    SPCD = as.integer(SPCD)
  )

logger::log_info("Prepared {nrow(hg_data)} height increment observations")
logger::log_info("Mean annual height increment: {round(mean(hg_data$HTG_annual), 2)}")
logger::log_info("Height increment range: {round(min(hg_data$HTG_annual), 2)} to {round(max(hg_data$HTG_annual), 2)}")

# =============================================================================
# Create Species Group Mapping
# =============================================================================

# FVS uses species groups for the height growth curve shape parameters
# The number of groups matches the HGHC array length
n_spgrp <- length(config$categories$other$HGHC)

# Map species to species groups
# FVS typically uses ISPSPE or similar mapping; we approximate with ordinal groups
species_list <- sort(unique(hg_data$SPCD))
N_species <- length(species_list)

# Create species group assignment (cycle through groups if more species than groups)
species_to_group <- tibble(
  SPCD = species_list,
  species_idx = seq_along(species_list),
  spgrp_idx = ((seq_along(species_list) - 1) %% n_spgrp) + 1
)

hg_data <- hg_data %>%
  left_join(species_to_group, by = "SPCD") %>%
  mutate(
    plot_idx = as.integer(factor(PLT_CN))
  )

logger::log_info("Species: {N_species}, Species groups: {n_spgrp}, Plots: {n_distinct(hg_data$plot_idx)}")

# =============================================================================
# Extract Priors from FVS Config
# =============================================================================

logger::log_info("Extracting priors from FVS config...")

fvs_other <- config$categories$other

# HGLD: ln(DBH) coefficient by species (N_species length in FVS)
prior_hgld <- if ("HGLD" %in% names(fvs_other)) {
  vals <- unlist(fvs_other$HGLD)
  # Pad or truncate to match our species count
  rep_len(vals, N_species)
} else {
  rep(0, N_species)
}

# HGHC: height growth curve shape by species group
prior_hghc <- if ("HGHC" %in% names(fvs_other)) {
  unlist(fvs_other$HGHC)
} else {
  rep(1, n_spgrp)
}

# HGLDD: ln(DBH) coefficient by species group
prior_hgldd <- if ("HGLDD" %in% names(fvs_other)) {
  unlist(fvs_other$HGLDD)
} else {
  rep(0.5, n_spgrp)
}

# HGH2: HT^2 coefficient by species group
prior_hgh2 <- if ("HGH2" %in% names(fvs_other)) {
  unlist(fvs_other$HGH2)
} else {
  rep(0, n_spgrp)
}

logger::log_info("Priors extracted: HGLD mean={round(mean(prior_hgld), 4)}, HGHC mean={round(mean(prior_hghc), 4)}")

# =============================================================================
# Prepare Stan Data
# =============================================================================

logger::log_info("Preparing data for Stan model...")

N <- nrow(hg_data)
N_plot <- n_distinct(hg_data$plot_idx)

stan_data <- list(
  N = N,
  N_species = N_species,
  N_spgrp = n_spgrp,
  N_plot = N_plot,

  # Response
  ln_HTG = hg_data$ln_HTG,

  # Predictors
  ln_DBH = hg_data$ln_DBH,
  ln_HT = hg_data$ln_HT,
  CR = hg_data$CR_prop,
  SI = hg_data$SI_scaled,
  BAL = hg_data$BAL_scaled,
  BA = hg_data$BA_scaled,
  SLOPE = hg_data$SLOPE_prop,
  CASP = hg_data$CASP,

  # Group indices
  species_id = hg_data$species_idx,
  spgrp_id = hg_data$spgrp_idx,
  plot_id = hg_data$plot_idx,

  # Priors from FVS config
  prior_hgld = prior_hgld,
  prior_hghc = prior_hghc,
  prior_hgldd = prior_hgldd,
  prior_hgh2 = prior_hgh2
)

logger::log_info("Stan data: N={N}, N_species={N_species}, N_spgrp={n_spgrp}, N_plot={N_plot}")

# =============================================================================
# Compile Stan Model
# =============================================================================

logger::log_info("Compiling Stan model...")

stan_file <- file.path(stan_dir, "height_increment.stan")

tryCatch({
  mod <- cmdstanr::cmdstan_model(
    stan_file,
    dir = output_dir
  )
  logger::log_info("Stan model compiled successfully")
}, error = function(e) {
  logger::log_error("Failed to compile Stan model: {e$message}")
  stop(e)
})

# =============================================================================
# Fit Model
# =============================================================================

logger::log_info("Fitting Bayesian height increment model (this may take several minutes)...")

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = min(4, parallel::detectCores()),
  iter_warmup = 1000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 15,
  refresh = 500,
  show_messages = FALSE
)

logger::log_info("Model fitting complete")

# =============================================================================
# Check Convergence
# =============================================================================

logger::log_info("Computing convergence diagnostics...")

diagnostics <- fit$diagnostic_summary()
summ <- fit$summary()

rhat_issues <- summ %>%
  filter(rhat > 1.01 & !is.na(rhat))

if (nrow(rhat_issues) > 0) {
  logger::log_warn("Found {nrow(rhat_issues)} parameters with Rhat > 1.01")
  logger::log_info("Parameters: {paste(rhat_issues$variable, collapse = ', ')}")
} else {
  logger::log_info("All parameters converged (Rhat <= 1.01)")
}

# =============================================================================
# Extract and Save Posterior Samples
# =============================================================================

logger::log_info("Extracting posterior samples...")

draws_df <- fit$draws(format = "df")

# Save raw samples
draws_file <- file.path(output_dir, "height_increment_samples.rds")
saveRDS(draws_df, draws_file)
logger::log_info("Saved posterior samples to {draws_file}")

# Save summary
summary_file <- file.path(output_dir, "height_increment_summary.csv")
write_csv(summ, summary_file)
logger::log_info("Saved summary to {summary_file}")

# Posterior summaries with credible intervals
posterior_summary <- summ %>%
  select(variable, median, q5, q95, rhat, ess_bulk) %>%
  rename(p50 = median, p05 = q5, p95 = q95) %>%
  mutate(
    ci_width = p95 - p05,
    converged = rhat <= 1.01 & !is.na(rhat)
  )

posterior_file <- file.path(output_dir, "height_increment_posterior.csv")
write_csv(posterior_summary, posterior_file)
logger::log_info("Saved posterior summaries to {posterior_file}")

# =============================================================================
# Diagnostic Plots
# =============================================================================

logger::log_info("Generating diagnostic plots...")

# Traceplots
key_params <- c("b0[1]", "b1[1]", "b2", "b3", "b4", "b5", "sigma",
                 "gamma_shape[1]")
pdf(file.path(output_dir, "height_increment_traceplots.pdf"), width = 12, height = 8)
tryCatch({
  print(bayesplot::mcmc_trace(
    fit$draws(),
    pars = intersect(key_params, summ$variable),
    facet_args = list(nrow = 3)
  ))
}, error = function(e) {
  logger::log_warn("Traceplot generation failed: {e$message}")
})
dev.off()

# Posterior predictive check
tryCatch({
  y_rep <- fit$draws("ln_HTG_rep", format = "matrix")
  ppc_file <- file.path(output_dir, "height_increment_ppc.pdf")
  pdf(ppc_file, width = 10, height = 6)
  print(bayesplot::ppc_dens_overlay(
    y = stan_data$ln_HTG,
    yrep = y_rep[1:100, ]
  ))
  dev.off()
  logger::log_info("Saved posterior predictive check")
}, error = function(e) {
  logger::log_warn("PPC plot failed: {e$message}")
})

# =============================================================================
# Summary Report
# =============================================================================

cat("\n")
cat("==========================================\n")
cat("Height Increment Model Fitting Complete\n")
cat("==========================================\n")
cat("Variant:", variant, "\n")
cat("Species fitted:", N_species, "\n")
cat("Species groups:", n_spgrp, "\n")
cat("Total observations:", N, "\n")
cat("Number of plots:", N_plot, "\n")
cat("Mean annual height increment:", round(mean(hg_data$HTG_annual), 2), "\n")
cat("\nHG parameters in config:", paste(hg_param_names, collapse = ", "), "\n")
cat("\nOutput saved to:", output_dir, "\n\n")

logger::log_info("Height increment model fitting complete for variant {variant}")

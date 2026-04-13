##=============================================================================
## 31_fit_dg_organon.R
## ORGANON-form Diameter Growth Model for CONUS-wide FVS
##
## Fits the Hann et al. (2002, 2006) SWO diameter growth equation with
## species x ecodivision Bayesian hierarchy via CmdStan.
## K1 and K2 are estimated parameters in Stan, while K4 remains fixed.
##
## Annualization follows Cao (2000) and Weiskittel et al. (2007):
##   For each tree-period, iterate annual predictions forward through the
##   measurement interval, linearly interpolating time-varying covariates.
##
## Two fitting approaches are provided:
##   1. Direct PAI method (Stan): annual DG * years ~ observed periodic DG
##      (fast, approximate; used for initial parameter exploration)
##   2. Full annualized method (R + Stan): Cao/Weiskittel iterative loop
##      embedded in a custom likelihood via cmdstanr optimize/sample
##      (slower, exact; used for final published estimates)
##
## Usage:
##   Rscript 31_fit_dg_organon.R --climate_si
##   Rscript 31_fit_dg_organon.R --bgi
##
## Author: Aaron Weiskittel, Greg Johnson
## Date: 2026-04-11
##=============================================================================

library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)

## Source project utilities
if (file.exists("calibration/R/00_setup.R")) source("calibration/R/00_setup.R")

##-----------------------------------------------------------------------------
## 1. Configuration
##-----------------------------------------------------------------------------

## ORGANON shape constants
## K1 and K2 are now estimated in Stan (initialized from prior, not fixed)
## K4 remains fixed based on ORGANON literature (Hann et al. 2006)
K4 <- 2.7     # additive constant in BAL/ln(DBH + K4); fixed across variants

## Minimum observations per species for species-level estimation
MIN_OBS_SPECIES <- 5000

## Stan model paths
STAN_DIRECT <- "calibration/stan/organon_dg_conus.stan"

## Output directory
OUT_DIR <- "calibration/output/conus"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

##-----------------------------------------------------------------------------
## 2. Data Preparation
##-----------------------------------------------------------------------------

prepare_dg_data <- function(dat, site_var = "climate_si") {
  ## dat: data frame with FIA remeasurement pairs, expected columns:
  ##   SPCD, ecodiv_code, DBH1, DBH2, HT1, CR1, CR2, BAL1, BAL2,
  ##   BA1, BA2, climate_si, bgi, clim_pca1, clim_pca2, YEARS,
  ##   SLOPE, ASPECT, ELEV, EMT

  message("Preparing diameter growth data...")
  message("  Site productivity variable: ", site_var)

  ## Convert FIA imperial to metric
  dat <- dat %>%
    mutate(
      DBH1_cm     = DBH1 * 2.54,           # inches to cm
      DBH2_cm     = DBH2 * 2.54,
      HT1_m       = HT1 * 0.3048,          # feet to meters
      BA1_metric  = BA1 * 0.2296,          # ft2/ac to m2/ha
      BAL1_metric = BAL1 * 0.2296,
      QMD1_metric = QMD1 * 2.54             # quadratic mean diameter
    )

  ## Compute derived variables
  dat <- dat %>%
    mutate(
      dg_obs      = (DBH2_cm - DBH1_cm),           # periodic DG in cm
      ln_cr_adj   = log((CR1 + 0.2) / 1.2),       # ORGANON CR transform
      ln_site_prod = log(pmax(.data[[site_var]], 1.0)), # site productivity
      bal_comp    = BAL1_metric / log(DBH1_cm + K4),    # BAL component (metric)
      sqrt_ba     = sqrt(BA1_metric),                   # sqrt(BA in m2/ha)
      ## Relative density and BAL interactions
      rd          = BA1_metric / sqrt(QMD1_metric),    # Curtis relative density
      ln_bal      = log(BAL1_metric + 5.0),             # log-BAL for interactions
      ## Alternative BAL formulations for reference:
      ## bal_log = log(BAL1_metric + 5.0)  # ORGANON PM_SWO form
      ## bal_ratio = BAL1_metric / BA1_metric
      ln_bal_5    = log(BAL1_metric + 5.0)               # log-BAL for components
    )

  ## Filter: positive growth, reasonable values
  dat <- dat %>%
    filter(
      dg_obs > -1.27,                       # Allow small negative measurement error (cm)
      dg_obs < 2.54 * 5.0 * YEARS,          # Max ~5 cm/year
      DBH1_cm >= 2.54,                      # Min 1 inch in cm
      CR1 > 0 & CR1 <= 1.0,
      .data[[site_var]] > 0,
      BA1_metric > 0,
      QMD1_metric > 0,
      YEARS >= 1 & YEARS <= 20
    )

  ## Species filtering: minimum observation threshold
  sp_counts <- dat %>% count(SPCD) %>% filter(n >= MIN_OBS_SPECIES)
  dat <- dat %>% filter(SPCD %in% sp_counts$SPCD)

  ## Create integer indices for species and ecodivision
  sp_levels  <- sort(unique(dat$SPCD))
  eco_levels <- sort(unique(dat$ecodiv_code))

  dat <- dat %>%
    mutate(
      species_idx = match(SPCD, sp_levels),
      ecodiv_idx  = match(ecodiv_code, eco_levels)
    )

  message("  Trees: ", nrow(dat))
  message("  Species: ", length(sp_levels))
  message("  Ecodivisions: ", length(eco_levels))
  message("  Mean measurement interval: ", round(mean(dat$YEARS), 1), " years")

  list(
    data    = dat,
    species = sp_levels,
    ecodiv  = eco_levels
  )
}

##-----------------------------------------------------------------------------
## 3. Stan Data Assembly (Direct PAI Method)
##-----------------------------------------------------------------------------

make_stan_data_direct <- function(prep) {
  dat <- prep$data

  list(
    N          = nrow(dat),
    N_species  = length(prep$species),
    N_ecodiv   = length(prep$ecodiv),
    dg_obs     = dat$dg_obs,                        # periodic DG in cm
    dbh        = dat$DBH1_cm,                       # raw DBH in cm (K1 estimated in Stan)
    ln_cr_adj  = dat$ln_cr_adj,
    ln_site_prod = dat$ln_site_prod,               # log site productivity
    bal_comp   = dat$bal_comp,
    sqrt_ba    = dat$sqrt_ba,
    rd         = dat$rd,                           # relative density
    ln_bal     = dat$ln_bal,                       # log-BAL for interactions
    clim1      = if ("clim_pca1" %in% names(dat)) dat$clim_pca1 else rep(0, nrow(dat)),
    clim2      = if ("clim_pca2" %in% names(dat)) dat$clim_pca2 else rep(0, nrow(dat)),
    years      = dat$YEARS,
    species_id = dat$species_idx,
    ecodiv_id  = dat$ecodiv_idx
  )
}

##-----------------------------------------------------------------------------
## 4. Cao/Weiskittel Annualized Growth Function (R Implementation)
##-----------------------------------------------------------------------------

#' Predict periodic diameter growth using iterative annualization
#'
#' This implements the gr.hat() function from Weiskittel et al. (2007),
#' adapted for the ORGANON SWO model form with the Bayesian parameter vector.
#' K1 and K2 are now estimated parameters extracted from Stan posteriors.
#'
#' @param params Named vector of model parameters (b0_total, b1:b10, K1, K2)
#' @param dat Data frame with tree-period observations (metric units)
#' @return Vector of predicted periodic diameter growth in cm

predict_dg_annualized <- function(params, dat) {

  b0_total <- params$b0_total   # Vector of length N (species+ecodiv intercept per obs)
  b1 <- params$b1
  b2 <- params$b2
  b3 <- params$b3
  b4 <- params$b4
  b5 <- params$b5
  b6 <- params$b6
  b7 <- params$b7
  b8 <- params$b8
  b9 <- params$b9                # relative density coefficient
  b10 <- params$b10              # relative density by log-BAL interaction
  K1 <- params$K1                # estimated ln(DBH + K1) constant
  K2 <- params$K2                # estimated power on DBH

  N <- nrow(dat)
  max_years <- max(dat$YEARS)

  ## Initialize current state (all in metric: cm, m2/ha)
  d_curr   <- dat$DBH1_cm
  bal_curr <- dat$BAL1_metric
  cr_curr  <- dat$CR1
  ba_curr  <- dat$BA1_metric
  qmd_curr <- dat$QMD1_metric    # needed for RD computation

  ## Linear interpolation rates for time-varying covariates
  bal_rate <- (dat$BAL2_metric - dat$BAL1_metric) / dat$YEARS
  cr_rate  <- (dat$CR2 - dat$CR1) / dat$YEARS
  ba_rate  <- (dat$BA2_metric - dat$BA1_metric) / dat$YEARS

  ## Climate variables (static over measurement period)
  clim1 <- if ("clim_pca1" %in% names(dat)) dat$clim_pca1 else rep(0, N)
  clim2 <- if ("clim_pca2" %in% names(dat)) dat$clim_pca2 else rep(0, N)

  ## Iterate annual growth predictions
  for (t in seq_len(max_years)) {

    ## Compute relative density from current BA and QMD
    rd_curr <- ba_curr / sqrt(qmd_curr)
    ln_bal_curr <- log(bal_curr + 5.0)

    ## Compute annual DG for all trees
    ln_dg <- b0_total +
      b1 * log(d_curr + K1) +
      b2 * d_curr^K2 +
      b3 * log((cr_curr + 0.2) / 1.2) +
      b4 * dat$ln_site_prod +
      b5 * bal_curr / log(d_curr + K4) +
      b6 * sqrt(ba_curr) +
      b7 * clim1 +
      b8 * clim2 +
      b9 * rd_curr +
      b10 * rd_curr * ln_bal_curr

    dg_annual <- exp(ln_dg)

    ## Low-CR adjustment (ORGANON)
    cradj <- ifelse(cr_curr <= 0.17, 1.0 - exp(-(25.0 * cr_curr)^2), 1.0)
    dg_annual <- dg_annual * cradj

    ## Only accumulate if within the tree's measurement period
    d_curr <- d_curr + ifelse(t <= dat$YEARS, dg_annual, 0.0)

    ## Update time-varying covariates
    bal_curr <- bal_curr + bal_rate
    cr_curr  <- cr_curr + cr_rate
    ba_curr  <- ba_curr + ba_rate
  }

  ## Predicted periodic growth in cm
  dg_pred <- d_curr - dat$DBH1_cm
  return(dg_pred)
}

##-----------------------------------------------------------------------------
## 5. Model Fitting (Direct Method via CmdStan)
##-----------------------------------------------------------------------------

fit_dg_direct <- function(prep, stan_file = STAN_DIRECT,
                          chains = 4, iter_warmup = 1000,
                          iter_sampling = 1000, seed = 42) {

  message("Compiling Stan model: ", stan_file)
  mod <- cmdstan_model(stan_file)

  stan_data <- make_stan_data_direct(prep)

  message("Running HMC sampling...")
  message("  Chains: ", chains)
  message("  Warmup: ", iter_warmup)
  message("  Sampling: ", iter_sampling)

  fit <- mod$sample(
    data            = stan_data,
    chains          = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    seed            = seed,
    max_treedepth   = 12,
    adapt_delta     = 0.9,
    refresh         = 100
  )

  return(fit)
}

##-----------------------------------------------------------------------------
## 6. Convergence Diagnostics
##-----------------------------------------------------------------------------

check_convergence <- function(fit, params = c("mu_b0", "b1", "b2", "b3",
                                               "b4", "b5", "b6", "b7", "b8",
                                               "b9", "b10", "K1", "K2",
                                               "sigma_sp", "sigma_eco", "sigma")) {

  message("Checking convergence...")

  ## Summary statistics
  summ <- fit$summary(variables = params)
  print(summ)

  ## Rhat check
  rhat_bad <- summ %>% filter(rhat > 1.05)
  if (nrow(rhat_bad) > 0) {
    warning("Parameters with Rhat > 1.05:")
    print(rhat_bad)
  } else {
    message("  All Rhat < 1.05: PASS")
  }

  ## ESS check
  ess_bad <- summ %>% filter(ess_bulk < 400)
  if (nrow(ess_bad) > 0) {
    warning("Parameters with ESS_bulk < 400:")
    print(ess_bad)
  } else {
    message("  All ESS_bulk > 400: PASS")
  }

  ## Divergence check
  diag <- fit$diagnostic_summary()
  n_div <- sum(diag$num_divergent)
  message("  Divergent transitions: ", n_div)
  if (n_div > 0) warning("Divergent transitions detected! Consider increasing adapt_delta.")

  return(summ)
}

##-----------------------------------------------------------------------------
## 7. Posterior Export
##-----------------------------------------------------------------------------

export_posteriors <- function(fit, prep, out_dir = OUT_DIR) {

  ## Extract posterior draws for fixed effects
  draws <- fit$draws(variables = c("mu_b0", "b1", "b2", "b3", "b4",
                                    "b5", "b6", "b7", "b8", "b9", "b10",
                                    "K1", "K2",
                                    "sigma_sp", "sigma_eco", "sigma"),
                     format = "draws_df")
  write_csv(draws, file.path(out_dir, "dg_organon_fixed_draws.csv"))

  ## Extract species intercepts (posterior medians)
  sp_vars <- paste0("b0_sp[", seq_along(prep$species), "]")
  sp_draws <- fit$draws(variables = "b0_sp", format = "draws_matrix")
  sp_medians <- apply(sp_draws, 2, median)

  sp_summary <- tibble(
    SPCD     = prep$species,
    b0_sp    = sp_medians[seq_along(prep$species)]
  )
  write_csv(sp_summary, file.path(out_dir, "dg_organon_species_intercepts.csv"))

  ## Extract ecodivision intercepts
  eco_draws <- fit$draws(variables = "b0_eco", format = "draws_matrix")
  eco_medians <- apply(eco_draws, 2, median)

  eco_summary <- tibble(
    ecodiv   = prep$ecodiv,
    b0_eco   = eco_medians[seq_along(prep$ecodiv)]
  )
  write_csv(eco_summary, file.path(out_dir, "dg_organon_ecodiv_intercepts.csv"))

  message("Posteriors exported to: ", out_dir)
  list(fixed = draws, species = sp_summary, ecodiv = eco_summary)
}

##-----------------------------------------------------------------------------
## 8. Annualized Validation
##-----------------------------------------------------------------------------

#' After fitting via the direct method, validate predictions using the
#' full Cao/Weiskittel annualization against observed periodic growth.

validate_annualized <- function(fit, prep) {

  message("Running annualized validation...")

  ## Extract posterior medians for all parameters
  summ <- fit$summary()
  get_median <- function(var) summ %>% filter(variable == var) %>% pull(median)

  ## Build per-observation intercepts (species + ecodiv)
  sp_intercepts  <- fit$draws(variables = "b0_sp", format = "draws_matrix") %>%
    apply(2, median)
  eco_intercepts <- fit$draws(variables = "b0_eco", format = "draws_matrix") %>%
    apply(2, median)

  dat <- prep$data
  b0_total <- get_median("mu_b0") +
    sp_intercepts[dat$species_idx] +
    eco_intercepts[dat$ecodiv_idx]

  params <- list(
    b0_total = b0_total,
    b1 = get_median("b1"),
    b2 = get_median("b2"),
    b3 = get_median("b3"),
    b4 = get_median("b4"),
    b5 = get_median("b5"),
    b6 = get_median("b6"),
    b7 = get_median("b7"),
    b8 = get_median("b8"),
    b9 = get_median("b9"),
    b10 = get_median("b10"),
    K1 = get_median("K1"),
    K2 = get_median("K2")
  )

  ## Predict using annualized method
  dg_pred <- predict_dg_annualized(params, dat)

  ## Compute validation statistics
  resid <- dat$dg_obs - dg_pred
  stats <- tibble(
    n         = length(resid),
    bias      = mean(resid),
    bias_pct  = 100 * mean(resid) / mean(dat$dg_obs),
    rmse      = sqrt(mean(resid^2)),
    rmse_pct  = 100 * sqrt(mean(resid^2)) / mean(dat$dg_obs),
    pseudo_r2 = 1 - sum(resid^2) / sum((dat$dg_obs - mean(dat$dg_obs))^2),
    mae       = mean(abs(resid))
  )

  message("Annualized validation results:")
  print(stats)

  list(stats = stats, predicted = dg_pred, observed = dat$dg_obs)
}

##-----------------------------------------------------------------------------
## 9. Main Execution
##-----------------------------------------------------------------------------

## This section runs when the script is called directly.
## For interactive use, source the script and call functions individually.

if (sys.nframe() == 0) {

  ## Parse command-line arguments
  args <- commandArgs(trailingOnly = TRUE)
  site_var <- if ("--bgi" %in% args) "bgi" else "climate_si"
  method   <- if ("--annualized" %in% args) "annualized" else "direct"

  message("=== ORGANON DG CONUS-wide Model Fitting ===")
  message("Site variable: ", site_var)
  message("Method: ", method)

  ## Load data (assumes 30_build_conus_dataset.R has been run)
  data_file <- "calibration/data/conus_remeasurement_pairs.rds"
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file,
         "\nRun 30_build_conus_dataset.R first.")
  }
  raw_data <- readRDS(data_file)

  ## Prepare data
  prep <- prepare_dg_data(raw_data, site_var = site_var)

  ## Fit model
  if (method == "direct") {
    fit <- fit_dg_direct(prep)
    summ <- check_convergence(fit)
    posts <- export_posteriors(fit, prep)

    ## Run annualized validation on direct fit
    val <- validate_annualized(fit, prep)

  } else {
    message("Full annualized fitting not yet implemented in Stan.")
    message("Use direct method, then validate with annualization.")
  }

  message("=== Done ===")
}

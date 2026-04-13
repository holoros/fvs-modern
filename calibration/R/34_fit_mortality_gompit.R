##=============================================================================
## 34_fit_mortality_gompit.R
## Gompit (Complementary Log Log) Mortality Model for CONUS wide FVS
##
## Based on ORGANON PM_SWO (Hann and Hanus, 2001) with exposure offset
## for variable length FIA remeasurement intervals. Extended with
## species x ecodivision Bayesian hierarchy and climate covariates.
##
## The gompit link with exposure ensures annualization is handled
## directly in the likelihood (no iterative loop needed):
##   P(survive T years) = exp(-exp(eta) * T)
##
## This follows the approach validated in the Mortality Modeling Example
## (Gompit.e.m1), which demonstrated cloglog with exposure offset
## outperforming logit alternatives.
##
## Units: DBH in cm, BA in m2/ha, BAL in m2/ha, site productivity in m
##
## Usage:
##   Rscript 34_fit_mortality_gompit.R
##
## Author: Aaron Weiskittel, Greg Johnson
## Date: 2026 04 11
##=============================================================================

library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(pROC)

## Source project utilities
if (file.exists("calibration/R/00_setup.R")) source("calibration/R/00_setup.R")

##-----------------------------------------------------------------------------
## 1. Configuration
##-----------------------------------------------------------------------------

MIN_OBS_SPECIES <- 5000
STAN_MODEL <- "calibration/stan/gompit_mortality_conus.stan"
OUT_DIR <- "calibration/output/conus"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

##-----------------------------------------------------------------------------
## 2. Data Preparation
##-----------------------------------------------------------------------------

prepare_mortality_data <- function(dat, site_var = "climate_si") {

  message("Preparing mortality data...")
  message("  Site productivity variable: ", site_var)

  ## Compute derived variables in metric units
  ## DBH: convert inches to cm (1 inch = 2.54 cm)
  ## BA, BAL: convert ft2/acre to m2/ha (1 ft2/acre = 0.2296 m2/ha)
  ## Site productivity: use ClimateSI or BGI in meters directly
  dat <- dat %>%
    mutate(
      alive        = as.integer(STATUS2 == 1),        # 1 = survived, 0 = died
      DBH1_cm      = DBH1 * 2.54,
      BA1_metric   = BA1 * 0.2296,
      BAL1_metric  = BAL1 * 0.2296,
      QMD1_metric  = (BA1_metric / 0.7854 / (BA1_metric / DBH1_cm / DBH1_cm))^0.5,
      dbh_sq       = DBH1_cm^2,
      site_val     = .data[[site_var]],              # Direct use without offset
      rd           = BA1_metric / QMD1_metric,       # Curtis relative density
      sqrt_ba_rd   = sqrt(BA1_metric * rd),          # sqrt(BA*RD) interaction
      ln_bal       = log(BAL1_metric + 5.0),         # ORGANON PM_SWO transform
      log_years    = log(YEARS)
    )

  ## Filter: valid observations
  dat <- dat %>%
    filter(
      DBH1_cm >= 2.5,                               # Minimum DBH in cm
      CR1 > 0 & CR1 <= 1.0,
      site_val > 0,
      YEARS >= 1 & YEARS <= 20,
      rd > 0,
      is.finite(sqrt_ba_rd)
    )

  ## Species filtering
  sp_counts <- dat %>% count(SPCD) %>% filter(n >= MIN_OBS_SPECIES)
  dat <- dat %>% filter(SPCD %in% sp_counts$SPCD)

  ## Create integer indices
  sp_levels  <- sort(unique(dat$SPCD))
  eco_levels <- sort(unique(dat$ecodiv_code))

  dat <- dat %>%
    mutate(
      species_idx = match(SPCD, sp_levels),
      ecodiv_idx  = match(ecodiv_code, eco_levels)
    )

  ## Mortality summary
  mort_rate <- 1 - mean(dat$alive)
  message("  Trees: ", nrow(dat))
  message("  Species: ", length(sp_levels))
  message("  Ecodivisions: ", length(eco_levels))
  message("  Overall mortality rate: ", round(100 * mort_rate, 2), "%")
  message("  Mean measurement interval: ", round(mean(dat$YEARS), 1), " years")
  message("  Mean RD: ", round(mean(dat$rd, na.rm = TRUE), 3))

  list(
    data    = dat,
    species = sp_levels,
    ecodiv  = eco_levels
  )
}

##-----------------------------------------------------------------------------
## 3. Stan Data Assembly
##-----------------------------------------------------------------------------

make_stan_data_mort <- function(prep) {
  dat <- prep$data

  list(
    N          = nrow(dat),
    N_species  = length(prep$species),
    N_ecodiv   = length(prep$ecodiv),
    alive      = dat$alive,
    dbh        = dat$DBH1_cm,                       # Metric DBH in cm
    dbh_sq     = dat$dbh_sq,
    cr         = dat$CR1,
    site       = dat$site_val,                      # ClimateSI or BGI in m
    ln_bal     = dat$ln_bal,                        # log(BAL + 5.0) in m2/ha
    rd         = dat$rd,
    sqrt_ba_rd = dat$sqrt_ba_rd,
    clim1      = if ("clim_pca1" %in% names(dat)) dat$clim_pca1 else rep(0, nrow(dat)),
    log_years  = dat$log_years,
    species_id = dat$species_idx,
    ecodiv_id  = dat$ecodiv_idx
  )
}

##-----------------------------------------------------------------------------
## 4. Model Fitting
##-----------------------------------------------------------------------------

fit_mortality <- function(prep, stan_file = STAN_MODEL,
                          chains = 4, iter_warmup = 1000,
                          iter_sampling = 1000, seed = 42) {

  message("Compiling Stan model: ", stan_file)
  mod <- cmdstan_model(stan_file)

  stan_data <- make_stan_data_mort(prep)

  message("Running HMC sampling...")
  fit <- mod$sample(
    data            = stan_data,
    chains          = chains,
    parallel_chains = min(chains, parallel::detectCores()),
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    seed            = seed,
    max_treedepth   = 12,
    adapt_delta     = 0.95,                         # Higher for binary data
    refresh         = 100
  )

  return(fit)
}

##-----------------------------------------------------------------------------
## 5. Convergence Diagnostics
##-----------------------------------------------------------------------------

check_convergence <- function(fit) {

  params <- c("mu_b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7",
              "sigma_sp", "sigma_eco")

  message("Checking convergence...")
  summ <- fit$summary(variables = params)
  print(summ)

  rhat_bad <- summ %>% filter(rhat > 1.05)
  if (nrow(rhat_bad) > 0) {
    warning("Parameters with Rhat > 1.05:")
    print(rhat_bad)
  } else {
    message("  All Rhat < 1.05: PASS")
  }

  ess_bad <- summ %>% filter(ess_bulk < 400)
  if (nrow(ess_bad) > 0) {
    warning("Parameters with ESS_bulk < 400:")
    print(ess_bad)
  } else {
    message("  All ESS_bulk > 400: PASS")
  }

  diag <- fit$diagnostic_summary()
  n_div <- sum(diag$num_divergent)
  message("  Divergent transitions: ", n_div)

  return(summ)
}

##-----------------------------------------------------------------------------
## 6. Model Evaluation
##-----------------------------------------------------------------------------

evaluate_mortality <- function(fit, prep) {

  message("Evaluating mortality model...")
  dat <- prep$data

  ## Extract posterior medians
  summ <- fit$summary()
  get_med <- function(var) summ %>% filter(variable == var) %>% pull(median)

  sp_int  <- fit$draws("b0_sp", format = "draws_matrix") %>% apply(2, median)
  eco_int <- fit$draws("b0_eco", format = "draws_matrix") %>% apply(2, median)

  ## Compute linear predictor (eta) for each observation using metric units
  eta <- get_med("mu_b0") +
    sp_int[dat$species_idx] +
    eco_int[dat$ecodiv_idx] +
    get_med("b1") * dat$DBH1_cm +
    get_med("b2") * dat$dbh_sq +
    get_med("b3") * dat$CR1 +
    get_med("b4") * dat$site_val +
    get_med("b5") * dat$ln_bal +
    get_med("b6") * (if ("clim_pca1" %in% names(dat)) dat$clim_pca1 else 0) +
    get_med("b7") * dat$sqrt_ba_rd

  ## Predicted survival probability over measurement period
  ## P(survive T) = exp( exp(eta) * T)
  p_surv <- exp(-exp(eta) * dat$YEARS)
  p_mort <- 1 - p_surv

  ## AUC
  roc_obj <- roc(dat$alive, p_surv, direction = "<")
  auc_val <- auc(roc_obj)
  message("  AUC: ", round(auc_val, 3))

  ## Observed vs predicted mortality rate by species
  by_species <- dat %>%
    mutate(p_mort_pred = p_mort) %>%
    group_by(SPCD) %>%
    summarise(
      n            = n(),
      obs_mort     = 1 - mean(alive),
      pred_mort    = mean(p_mort_pred),
      .groups      = "drop"
    ) %>%
    mutate(
      ratio = pred_mort / pmax(obs_mort, 0.001)
    )

  message("  Species mortality rate comparison:")
  print(by_species %>% arrange(desc(n)) %>% head(20))

  ## U shaped mortality check: predict mortality for a range of DBH values
  ## holding other covariates at their median
  message("  Checking U shaped mortality curve...")
  dbh_seq <- seq(2.5, 100, by = 1.25)
  eta_u <- get_med("mu_b0") +
    get_med("b1") * dbh_seq +
    get_med("b2") * dbh_seq^2 +
    get_med("b3") * median(dat$CR1) +
    get_med("b4") * median(dat$site_val) +
    get_med("b5") * median(dat$ln_bal) +
    get_med("b7") * median(dat$sqrt_ba_rd)
  p_mort_u <- 1 - exp(-exp(eta_u))

  ## Check for U shape: minimum should be at intermediate DBH
  min_idx <- which.min(p_mort_u)
  if (min_idx > 1 && min_idx < length(dbh_seq)) {
    message("  U shape detected: minimum mortality at DBH = ",
            round(dbh_seq[min_idx], 1), " cm")
  } else {
    message("  WARNING: No U shape detected. Mortality is monotonic.")
  }

  list(
    auc        = auc_val,
    roc        = roc_obj,
    by_species = by_species,
    p_surv     = p_surv,
    p_mort     = p_mort
  )
}

##-----------------------------------------------------------------------------
## 7. Posterior Export
##-----------------------------------------------------------------------------

export_posteriors <- function(fit, prep, out_dir = OUT_DIR) {

  ## Fixed effects
  draws <- fit$draws(
    variables = c("mu_b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7",
                  "sigma_sp", "sigma_eco"),
    format = "draws_df"
  )
  write_csv(draws, file.path(out_dir, "mortality_gompit_fixed_draws.csv"))

  ## Species intercepts
  sp_draws <- fit$draws("b0_sp", format = "draws_matrix")
  sp_medians <- apply(sp_draws, 2, median)
  sp_summary <- tibble(
    SPCD  = prep$species,
    b0_sp = sp_medians[seq_along(prep$species)]
  )
  write_csv(sp_summary, file.path(out_dir, "mortality_gompit_species_intercepts.csv"))

  ## Ecodivision intercepts
  eco_draws <- fit$draws("b0_eco", format = "draws_matrix")
  eco_medians <- apply(eco_draws, 2, median)
  eco_summary <- tibble(
    ecodiv = prep$ecodiv,
    b0_eco = eco_medians[seq_along(prep$ecodiv)]
  )
  write_csv(eco_summary, file.path(out_dir, "mortality_gompit_ecodiv_intercepts.csv"))

  message("Posteriors exported to: ", out_dir)
  list(fixed = draws, species = sp_summary, ecodiv = eco_summary)
}

##-----------------------------------------------------------------------------
## 8. Main Execution
##-----------------------------------------------------------------------------

if (sys.nframe() == 0) {

  args <- commandArgs(trailingOnly = TRUE)
  site_var <- if ("--climate_si" %in% args) "climate_si" else "climate_si"

  message("=== Gompit Mortality CONUS wide Model Fitting ===")
  message("Site variable: ", site_var)

  ## Load data
  data_file <- "calibration/data/conus_remeasurement_pairs.rds"
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file,
         "\nRun 30_build_conus_dataset.R first.")
  }
  raw_data <- readRDS(data_file)

  ## Prepare, fit, evaluate, export
  prep <- prepare_mortality_data(raw_data, site_var = site_var)
  fit  <- fit_mortality(prep)
  summ <- check_convergence(fit)
  eval <- evaluate_mortality(fit, prep)
  posts <- export_posteriors(fit, prep)

  message("=== Done ===")
}

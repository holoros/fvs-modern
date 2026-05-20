#!/usr/bin/env Rscript
# =============================================================================
# eval_80_holdout_species_fit.R
#
# Held-out species predictive evaluation: refit a CONUS Phase 4 model
# excluding a curated 20% of species from the training set, so the
# held-out species can be predicted from trait coefficients alone.
#
# This is the FIT half of the held-out species evaluation pipeline.
# The PREDICT half lives in eval_80b_holdout_species_predict.R.
#
# Usage:
#   Rscript eval_80_holdout_species_fit.R --component dg --fold 1
#   Rscript eval_80_holdout_species_fit.R --component hg --fold 3 \
#       --scheme sparsity --n-folds 5
#
# Default holdout scheme is sparsity-based: hold out the 20% of species
# with fewest training observations. This is the case where trait-driven
# extrapolation matters most. Alternative schemes:
#   --scheme random      : random 20% per fold
#   --scheme geographic  : hold out by ecoregion membership
#
# Output (per fold per component):
#   calibration/output/conus/holdout/{component}_fold{N}_fit.rds
#   calibration/output/conus/holdout/{component}_fold{N}_meta.rds
#     (includes the held-out species SPCD vector for the predict step)
#
# Cardinal cost estimate per fit: ~3h wall on a single node, ~8 GB peak.
# 5 folds x 6 components = 30 fits total. Run as a SLURM array.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(jsonlite)
  library(logger)
})

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

project_root    <- Sys.getenv("FVS_PROJECT_ROOT", normalizePath(".."))
calibration_dir <- file.path(project_root, "calibration")
output_dir      <- file.path(calibration_dir, "output", "conus", "holdout")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Component -> (Stan model path, training data path) mapping. These
# should match the production Phase 4 pipeline conventions.
COMPONENT_SPEC <- list(
  dg = list(
    stan_file  = file.path(calibration_dir, "stan", "dg_kuehne_2022_lognormal.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_dg_matched_pairs.rds"),
    spcd_col   = "SPCD"
  ),
  hg = list(
    stan_file  = file.path(calibration_dir, "stan", "hg_organon_fixedK.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_hg_matched_pairs.rds"),
    spcd_col   = "SPCD"
  ),
  ht_dbh = list(
    stan_file  = file.path(calibration_dir, "stan", "htdbh_wykoff_lognormal.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_htdbh_matched_pairs.rds"),
    spcd_col   = "SPCD"
  ),
  hcb = list(
    stan_file  = file.path(calibration_dir, "stan", "hcb_organon.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_hcb_matched_pairs.rds"),
    spcd_col   = "SPCD"
  ),
  mortality = list(
    stan_file  = file.path(calibration_dir, "stan", "mort_logit_simple.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_mortality_matched_pairs.rds"),
    spcd_col   = "SPCD"
  ),
  crown_recession = list(
    stan_file  = file.path(calibration_dir, "stan", "cr_recession.stan"),
    data_file  = file.path(calibration_dir, "data", "conus_cr_matched_pairs.rds"),
    spcd_col   = "SPCD"
  )
)

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

parse_args <- function(args) {
  out <- list(
    component = "dg",
    fold      = 1,
    n_folds   = 5,
    scheme    = "sparsity",
    iter_warmup   = 1000,
    iter_sampling = 1000,
    chains        = 4,
    parallel_chains = 4,
    seed = 42L
  )
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--component" && i < length(args)) {
      out$component <- args[i + 1]; i <- i + 2
    } else if (args[i] == "--fold" && i < length(args)) {
      out$fold <- as.integer(args[i + 1]); i <- i + 2
    } else if (args[i] == "--n-folds" && i < length(args)) {
      out$n_folds <- as.integer(args[i + 1]); i <- i + 2
    } else if (args[i] == "--scheme" && i < length(args)) {
      out$scheme <- args[i + 1]; i <- i + 2
    } else if (args[i] == "--iter-warmup" && i < length(args)) {
      out$iter_warmup <- as.integer(args[i + 1]); i <- i + 2
    } else if (args[i] == "--iter-sampling" && i < length(args)) {
      out$iter_sampling <- as.integer(args[i + 1]); i <- i + 2
    } else if (args[i] == "--seed" && i < length(args)) {
      out$seed <- as.integer(args[i + 1]); i <- i + 2
    } else {
      i <- i + 1
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Holdout schemes
#
# Each scheme returns a vector of SPCDs to exclude from this fold's
# training data. The same fold index across components should produce
# the same holdout set for a given scheme so cross-component comparison
# is meaningful.
# ---------------------------------------------------------------------------

holdout_spcds <- function(scheme, fold, n_folds, all_spcds, spcd_counts) {
  set.seed(42L * fold)  # deterministic per fold

  if (scheme == "random") {
    n_holdout <- ceiling(length(all_spcds) / n_folds)
    sample(all_spcds, n_holdout)
  } else if (scheme == "sparsity") {
    ## Sort species by count, hold out the (fold)-th quintile of
    ## sparsest species. Fold 1 = sparsest 20%, fold 5 = densest 20%.
    sorted <- spcd_counts %>% arrange(n) %>% pull(SPCD)
    n_holdout <- ceiling(length(sorted) / n_folds)
    start_idx <- (fold - 1L) * n_holdout + 1L
    end_idx   <- min(fold * n_holdout, length(sorted))
    sorted[start_idx:end_idx]
  } else if (scheme == "geographic") {
    ## Placeholder: requires species-to-ecoregion crosswalk.
    ## When implemented, hold out species whose plurality range is in
    ## ecoregion fold(N). For now, fall back to random with a warning.
    log_warn("Geographic scheme not yet implemented; falling back to random")
    n_holdout <- ceiling(length(all_spcds) / n_folds)
    sample(all_spcds, n_holdout)
  } else {
    stop("Unknown scheme: ", scheme)
  }
}

# ---------------------------------------------------------------------------
# Main fit
# ---------------------------------------------------------------------------

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  spec <- COMPONENT_SPEC[[args$component]]
  if (is.null(spec)) stop("Unknown component: ", args$component)

  log_info("=== {args$component} fold {args$fold} ({args$scheme}) ===")
  log_info("Stan file: {spec$stan_file}")
  log_info("Data file: {spec$data_file}")

  ## Load training data
  dat <- readRDS(spec$data_file)
  log_info("Loaded {nrow(dat)} observations across {length(unique(dat[[spec$spcd_col]]))} species")

  ## Compute species counts for the sparsity scheme
  spcd_counts <- dat %>%
    count(.data[[spec$spcd_col]], name = "n") %>%
    rename(SPCD = !!spec$spcd_col)
  all_spcds <- spcd_counts$SPCD

  ## Choose holdout SPCDs
  holdout <- holdout_spcds(args$scheme, args$fold, args$n_folds,
                           all_spcds, spcd_counts)
  log_info("Holding out {length(holdout)} species; training on remaining {length(all_spcds) - length(holdout)}")

  ## Filter training data
  train_dat <- dat %>% filter(!(.data[[spec$spcd_col]] %in% holdout))
  log_info("Training set: {nrow(train_dat)} observations after holdout")

  ## Prepare Stan data list. This call delegates to the same prep
  ## helper used by the production scripts (32_fit_hg_organon.R and
  ## siblings); replace with the canonical prep function from your
  ## fitting pipeline once it is in place.
  stan_data <- prepare_stan_data(train_dat, args$component)

  ## Fit
  log_info("Compiling Stan model ...")
  mod <- cmdstan_model(spec$stan_file)

  log_info("Sampling ...")
  fit <- mod$sample(
    data            = stan_data,
    seed            = args$seed,
    chains          = args$chains,
    parallel_chains = args$parallel_chains,
    iter_warmup     = args$iter_warmup,
    iter_sampling   = args$iter_sampling,
    refresh         = 100
  )

  ## Save fit + meta (the predict step needs the holdout SPCD vector
  ## and the training-set SPCD->index map to apply trait coefficients
  ## to held-out species).
  fit_path <- file.path(output_dir,
                        sprintf("%s_fold%d_fit.rds", args$component, args$fold))
  meta_path <- file.path(output_dir,
                         sprintf("%s_fold%d_meta.rds", args$component, args$fold))

  saveRDS(fit, fit_path)
  saveRDS(list(
    component    = args$component,
    fold         = args$fold,
    n_folds      = args$n_folds,
    scheme       = args$scheme,
    holdout_spcds = holdout,
    train_spcds   = setdiff(all_spcds, holdout),
    n_train      = nrow(train_dat),
    n_train_spcds = length(unique(train_dat[[spec$spcd_col]])),
    stan_file    = spec$stan_file,
    data_file    = spec$data_file,
    fit_summary  = fit$summary()
  ), meta_path)

  log_info("Wrote {fit_path} and {meta_path}")
  log_info("=== Done {args$component} fold {args$fold} ===")
}

# ---------------------------------------------------------------------------
# Stan data prep
# This is a thin wrapper; the actual prep logic lives in the
# component-specific fitting scripts (31_fit_dg_organon.R,
# 32_fit_hg_organon.R, etc.). Reuse the prep helper from those scripts
# rather than reimplementing here.
# ---------------------------------------------------------------------------

prepare_stan_data <- function(dat, component) {
  ## TODO: source the production prep function for this component and
  ## delegate. Skeleton fallback below covers the common case where
  ## the Stan model takes a flat list with sp_idx, ecodiv_idx, and the
  ## tree-level covariates.
  ##
  ## Example (uncomment and adapt when wiring up):
  ## source(file.path(calibration_dir, "R", sprintf("prep_%s.R", component)))
  ## prepare_data_for_stan(dat)

  stop("prepare_stan_data() not yet wired to the production prep helper. ",
       "Source the component-specific prep function from the production ",
       "fitting scripts (31_fit_*.R / 32_fit_*.R / ...) before running ",
       "eval_80.")
}

if (!interactive()) main()

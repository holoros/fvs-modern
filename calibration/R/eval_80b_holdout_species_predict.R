#!/usr/bin/env Rscript
# =============================================================================
# eval_80b_holdout_species_predict.R
#
# Predict held-out species' annual increment from the trait coefficients
# of an eval_80 fit. The held-out species were excluded from training,
# so the species random intercept is unavailable. The trait linear
# predictor stands in as the species-level effect.
#
# This is the PREDICT half of the held-out species evaluation pipeline.
# Run after eval_80_holdout_species_fit.R for the corresponding fold.
#
# Usage:
#   Rscript eval_80b_holdout_species_predict.R --component dg --fold 1
#   Rscript eval_80b_holdout_species_predict.R --component hg --fold all
#
# Output (per fold per component):
#   calibration/output/conus/holdout/{component}_fold{N}_predictions.csv
#     columns: SPCD, n_obs, observed_mean, predicted_mean, residual,
#              abs_error, pct_error, in_training (FALSE for held-out)
#
# Aggregating across folds produces Figure 2: predictive R^2 and RMSE
# on held-out species per component, compared to in-training species
# baseline.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(posterior)
  library(logger)
})

# ---------------------------------------------------------------------------
# Paths and config
# ---------------------------------------------------------------------------

project_root    <- Sys.getenv("FVS_PROJECT_ROOT", normalizePath(".."))
calibration_dir <- file.path(project_root, "calibration")
holdout_dir     <- file.path(calibration_dir, "output", "conus", "holdout")

COMPONENT_SPEC <- list(
  dg = list(
    data_file  = file.path(calibration_dir, "data", "conus_dg_matched_pairs.rds"),
    response   = "dbh_increment_annual",
    link       = "log",          # log-linear in DG-Kuehne
    spcd_col   = "SPCD"
  ),
  hg = list(
    data_file  = file.path(calibration_dir, "data", "conus_hg_matched_pairs.rds"),
    response   = "ht_increment_annual",
    link       = "identity",
    spcd_col   = "SPCD"
  ),
  ht_dbh = list(
    data_file  = file.path(calibration_dir, "data", "conus_htdbh_matched_pairs.rds"),
    response   = "ht_obs",
    link       = "log",
    spcd_col   = "SPCD"
  ),
  hcb = list(
    data_file  = file.path(calibration_dir, "data", "conus_hcb_matched_pairs.rds"),
    response   = "hcb_obs",
    link       = "identity",
    spcd_col   = "SPCD"
  ),
  mortality = list(
    data_file  = file.path(calibration_dir, "data", "conus_mortality_matched_pairs.rds"),
    response   = "mortality_event",
    link       = "logit",
    spcd_col   = "SPCD"
  ),
  crown_recession = list(
    data_file  = file.path(calibration_dir, "data", "conus_cr_matched_pairs.rds"),
    response   = "cr_change",
    link       = "identity",
    spcd_col   = "SPCD"
  )
)

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

parse_args <- function(args) {
  out <- list(component = "dg", fold = "1")
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--component" && i < length(args)) {
      out$component <- args[i + 1]; i <- i + 2
    } else if (args[i] == "--fold" && i < length(args)) {
      out$fold <- args[i + 1]; i <- i + 2
    } else {
      i <- i + 1
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Predict for one fold
# ---------------------------------------------------------------------------

predict_one_fold <- function(component, fold, spec) {
  meta_path <- file.path(holdout_dir,
                         sprintf("%s_fold%d_meta.rds", component, fold))
  fit_path  <- file.path(holdout_dir,
                         sprintf("%s_fold%d_fit.rds", component, fold))

  if (!file.exists(meta_path) || !file.exists(fit_path)) {
    log_warn("Missing fold {fold} artifacts for {component}; skipping")
    return(NULL)
  }

  meta <- readRDS(meta_path)
  fit  <- readRDS(fit_path)

  ## Held-out species' observations from the original dataset
  dat <- readRDS(spec$data_file)
  holdout_obs <- dat %>% filter(.data[[spec$spcd_col]] %in% meta$holdout_spcds)
  log_info("Fold {fold}: {nrow(holdout_obs)} holdout observations across {length(meta$holdout_spcds)} species")

  ## Posterior means of fixed effects + trait coefficients
  draws_df <- as_draws_df(fit)
  fixed_means <- summarise_draws(draws_df, mean)
  ## TODO: wire in the component-specific linear-predictor function.
  ## The structure is: eta = b0 + W[sp,] %*% gamma + tree_cov_effects.
  ## For held-out species, W[sp,] is known (trait values) but the
  ## species random intercept z_sp[sp] does NOT exist. The trait
  ## linear predictor is the species-level effect.
  ##
  ## Skeleton: caller fills in the actual eta formula for the
  ## component. Below is the DG-Kuehne shape as an example.

  predictions <- predict_eta_for_holdout(
    holdout_obs, meta, fixed_means, component, spec
  )

  ## Compare predicted vs observed
  per_species <- predictions %>%
    group_by(.data[[spec$spcd_col]]) %>%
    summarise(
      n_obs           = n(),
      observed_mean   = mean(.data[[spec$response]], na.rm = TRUE),
      predicted_mean  = mean(predicted, na.rm = TRUE),
      residual        = observed_mean - predicted_mean,
      abs_error       = abs(residual),
      pct_error       = 100 * abs(residual) / pmax(abs(observed_mean), 1e-6),
      .groups = "drop"
    ) %>%
    rename(SPCD = !!spec$spcd_col) %>%
    mutate(
      component = component,
      fold      = fold,
      in_training = FALSE
    )

  per_species
}

# ---------------------------------------------------------------------------
# eta calculation (component-specific skeleton)
# ---------------------------------------------------------------------------

predict_eta_for_holdout <- function(holdout_obs, meta, fixed_means,
                                    component, spec) {
  ## TODO: implement per component. The structure is:
  ##   1. Look up trait values W[sp,] for each held-out species.
  ##      Source: calibration/data/conus_species_traits.csv (8-trait
  ##      table keyed by SPCD).
  ##   2. Compute the trait linear predictor:
  ##      trait_eta = W[sp,] %*% gamma
  ##      where gamma is the trait coefficient vector posterior mean
  ##      from fixed_means.
  ##   3. Compute the tree-level covariate effects:
  ##      tree_eta = b1*ln_DBH + b2*ln_CR_adj + b3*ln_BAL_sw + ...
  ##   4. Combine with intercept and apply inverse link:
  ##      predicted = inv_link(b0 + trait_eta + tree_eta)
  ##
  ## The species random intercept z_sp[sp] is intentionally OMITTED
  ## from the held-out prediction because the held-out species was
  ## never seen in training; the trait linear predictor is the
  ## species-level effect.

  stop("predict_eta_for_holdout() not yet implemented for component '",
       component, "'. Wire in the component-specific eta formula from ",
       "the corresponding production fitting script (31_fit_*.R / ...).")
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  spec <- COMPONENT_SPEC[[args$component]]
  if (is.null(spec)) stop("Unknown component: ", args$component)

  ## Determine folds to process
  if (args$fold == "all") {
    folds <- 1:5  # default n_folds
  } else {
    folds <- as.integer(args$fold)
  }

  log_info("Predicting held-out species for {args$component}, folds: {paste(folds, collapse=',')}")

  all_predictions <- map_dfr(folds, function(f) {
    predict_one_fold(args$component, f, spec)
  })

  if (nrow(all_predictions) == 0) {
    log_error("No predictions produced; check that fit/meta files exist")
    return(invisible(FALSE))
  }

  out_path <- file.path(holdout_dir,
                        sprintf("%s_holdout_predictions.csv", args$component))
  write_csv(all_predictions, out_path)

  ## Aggregate summary
  overall <- all_predictions %>%
    summarise(
      n_species     = n(),
      n_obs         = sum(n_obs),
      rmse          = sqrt(mean(residual^2)),
      mean_abs_err  = mean(abs_error),
      median_pct_err = median(pct_error),
      p90_pct_err   = quantile(pct_error, 0.90)
    )
  log_info("Overall held-out performance for {args$component}:")
  log_info("  N species: {overall$n_species}, N obs: {overall$n_obs}")
  log_info("  RMSE: {round(overall$rmse, 4)}")
  log_info("  Median % error: {round(overall$median_pct_err, 1)}")
  log_info("  P90 % error: {round(overall$p90_pct_err, 1)}")

  log_info("Wrote {out_path}")
}

if (!interactive()) main()

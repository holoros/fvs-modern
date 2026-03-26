#!/usr/bin/env Rscript
# =============================================================================
# FVS Bayesian Calibration: Convert All Posteriors to JSON Config
#
# Reads posterior samples from ALL fitted component models and writes
# calibrated parameters to JSON matching the original FVS config schema.
#
# Component models integrated:
#   1. Diameter growth (Wykoff) - DG parameters
#   2. Height-diameter (Chapman-Richards) - H-D parameters
#   3. Height increment (where applicable) - HG parameters
#   4. Mortality (logistic) - mortality parameters
#   5. Crown ratio change - BCR parameters
#
# Usage: Rscript calibration/R/06_posterior_to_json.R --variant ca
# =============================================================================

library(tidyverse)
library(jsonlite)
library(data.table)
library(logger)

# =============================================================================
# Parse Command Line Arguments
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
variant <- "ca"

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
output_dir <- file.path(calibration_dir, "output", "variants", variant)
config_dir <- file.path(project_root, "config")
calibrated_dir <- file.path(config_dir, "calibrated")

dir.create(calibrated_dir, showWarnings = FALSE, recursive = TRUE)

# Logging
log_file <- file.path(calibration_dir, "logs",
                       paste0("06_posterior_to_json_", variant, ".log"))
dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Converting posteriors to JSON for variant {variant}")

# =============================================================================
# Load Original Config as Template
# =============================================================================

logger::log_info("Loading original FVS config...")

original_config_file <- file.path(config_dir, paste0(variant, ".json"))

if (!file.exists(original_config_file)) {
  logger::log_error("Original config not found: {original_config_file}")
  stop("Original config not found: ", original_config_file)
}

original_config <- fromJSON(original_config_file)
n_species <- original_config$maxsp

logger::log_info("Loaded config: {original_config$variant_name}, {n_species} species")

# Track which components were updated
components_updated <- character(0)
components_skipped <- character(0)

# =============================================================================
# Helper Functions
# =============================================================================

# Extract posterior median from a summary dataframe
extract_param <- function(posterior_df, param_name) {
  if (is.null(posterior_df)) return(NA_real_)

  # Try exact match
  match_row <- posterior_df %>% filter(variable == param_name)
  if (nrow(match_row) > 0) {
    # Use p50 if available (CmdStanR format), otherwise Estimate (brms format)
    if ("p50" %in% names(match_row)) return(match_row$p50[1])
    if ("Estimate" %in% names(match_row)) return(match_row$Estimate[1])
    if ("median" %in% names(match_row)) return(match_row$median[1])
  }
  return(NA_real_)
}

# Extract species specific posterior medians as a vector
extract_species_params <- function(posterior_df, base_name, n_sp) {
  if (is.null(posterior_df)) return(rep(NA_real_, n_sp))

  values <- rep(NA_real_, n_sp)
  for (idx in seq_len(n_sp)) {
    # Try indexed form: "b1[3]"
    param_key <- paste0(base_name, "[", idx, "]")
    val <- extract_param(posterior_df, param_key)

    if (is.na(val)) {
      # Try brms naming: "b_<name>_SPCD<code>"
      param_key2 <- paste0("b_", base_name)
      val <- extract_param(posterior_df, param_key2)
    }

    if (!is.na(val)) {
      values[idx] <- val
    }
  }

  # If no species specific values found, try global parameter
  if (all(is.na(values))) {
    global_val <- extract_param(posterior_df, base_name)
    if (!is.na(global_val)) {
      values <- rep(global_val, n_sp)
    }
  }

  return(values)
}

# Safely update a config parameter, preserving original where posterior is NA
safe_update <- function(config, category, param_name, new_values) {
  if (param_name %in% names(config$categories[[category]])) {
    original <- unlist(config$categories[[category]][[param_name]])
    # Replace only where new values are available
    updated <- ifelse(is.na(new_values), original, new_values)
    config$categories[[category]][[param_name]] <- as.list(updated)
    n_updated <- sum(!is.na(new_values))
    logger::log_info("  {param_name}: {n_updated}/{length(new_values)} values updated")
    return(list(config = config, n_updated = n_updated))
  } else {
    logger::log_info("  {param_name}: not found in config category '{category}'")
    return(list(config = config, n_updated = 0))
  }
}

# =============================================================================
# 1. Diameter Growth Parameters
# =============================================================================

logger::log_info("=== Processing diameter growth posteriors ===")

dg_posterior_file <- file.path(output_dir, "diameter_growth_posterior.csv")

if (file.exists(dg_posterior_file)) {
  dg_posterior <- read_csv(dg_posterior_file, show_col_types = FALSE) %>% as_tibble()
  logger::log_info("Loaded {nrow(dg_posterior)} diameter growth parameters")

  # Map Stan parameter names to FVS config parameter names
  dg_param_mapping <- list(
    DGLD   = "b1",    # ln(DBH) coefficient
    DGDS   = "b2",    # DBH^2 coefficient
    DGSITE = "b3",    # ln(SI) coefficient
    DGSLOP = "b4",    # SLOPE coefficient
    DGSLSQ = "b5",    # SLOPE^2 coefficient
    DGSASP = "b6",    # SLOPE*sin(ASP) coefficient
    DGCASP = "b7",    # SLOPE*cos(ASP) coefficient
    DGEL   = "b8",    # ELEV coefficient
    DGELSQ = "b9",    # ELEV^2 coefficient
    DGCR   = "b10",   # CR coefficient
    DGCRSQ = "b11",   # CR^2 coefficient
    DGBAL  = "b12",   # BAL coefficient
    DGPCCF = "b13"    # BA/CCF coefficient
  )

  total_dg_updated <- 0

  for (fvs_name in names(dg_param_mapping)) {
    stan_name <- dg_param_mapping[[fvs_name]]

    # Species specific parameters (b0, b1, b2, b3) vs global (b4-b13)
    if (stan_name %in% c("b1", "b2", "b3")) {
      new_vals <- extract_species_params(dg_posterior, stan_name, n_species)
    } else {
      # Global parameter: replicate across species
      global_val <- extract_param(dg_posterior, stan_name)
      new_vals <- rep(global_val, n_species)
    }

    result <- safe_update(original_config, "other", fvs_name, new_vals)
    original_config <- result$config
    total_dg_updated <- total_dg_updated + result$n_updated
  }

  # Also update intercept if available
  b0_vals <- extract_species_params(dg_posterior, "b0", n_species)
  if (any(!is.na(b0_vals))) {
    result <- safe_update(original_config, "growth", "WEIBB1", b0_vals)
    original_config <- result$config
  }

  logger::log_info("Diameter growth: {total_dg_updated} parameter values updated")
  components_updated <- c(components_updated, "diameter_growth")

} else {
  logger::log_warn("Diameter growth posterior not found: {dg_posterior_file}")
  components_skipped <- c(components_skipped, "diameter_growth")
}

# =============================================================================
# 2. Height-Diameter Parameters
# =============================================================================

logger::log_info("=== Processing height-diameter posteriors ===")

htdbh_summary_file <- file.path(output_dir, "height_diameter_summary.csv")

if (file.exists(htdbh_summary_file)) {
  htdbh_posterior <- read_csv(htdbh_summary_file, show_col_types = FALSE) %>% as_tibble()
  logger::log_info("Loaded {nrow(htdbh_posterior)} height-diameter parameters")

  # Chapman-Richards parameters: H = 4.5 + a * (1 - exp(-b * DBH))^c
  # brms stores population level effects with b_ prefix

  a_vals <- extract_species_params(htdbh_posterior, "b_a_Intercept", n_species)
  b_vals <- extract_species_params(htdbh_posterior, "b_b_Intercept", n_species)
  c_val <- extract_param(htdbh_posterior, "b_c_Intercept")

  # Store in height_diameter category
  if (!"height_diameter" %in% names(original_config$categories)) {
    original_config$categories$height_diameter <- list()
  }

  if (any(!is.na(a_vals))) {
    original_config$categories$height_diameter$HD_A <- as.list(a_vals)
    logger::log_info("  HD_A (asymptote): {sum(!is.na(a_vals))} species updated")
  }
  if (any(!is.na(b_vals))) {
    original_config$categories$height_diameter$HD_B <- as.list(b_vals)
    logger::log_info("  HD_B (rate): {sum(!is.na(b_vals))} species updated")
  }
  if (!is.na(c_val)) {
    original_config$categories$height_diameter$HD_C <- c_val
    logger::log_info("  HD_C (shape): updated to {round(c_val, 4)}")
  }

  components_updated <- c(components_updated, "height_diameter")

} else {
  logger::log_warn("Height-diameter posterior not found")
  components_skipped <- c(components_skipped, "height_diameter")
}

# =============================================================================
# 3. Height Increment Parameters (variants with explicit HG params only)
# =============================================================================

logger::log_info("=== Processing height increment posteriors ===")

htinc_posterior_file <- file.path(output_dir, "height_increment_posterior.csv")

if (file.exists(htinc_posterior_file)) {
  htinc_posterior <- read_csv(htinc_posterior_file, show_col_types = FALSE) %>% as_tibble()
  logger::log_info("Loaded {nrow(htinc_posterior)} height increment parameters")

  # Map Stan parameters to FVS HG config parameters
  hg_param_mapping <- list(
    HGLD = "b1",       # ln(DBH) coefficient (species specific)
    HGSC = "b0"        # Species specific intercept (site/competition)
  )

  for (fvs_name in names(hg_param_mapping)) {
    stan_name <- hg_param_mapping[[fvs_name]]

    if (fvs_name %in% names(original_config$categories$other)) {
      fvs_len <- length(unlist(original_config$categories$other[[fvs_name]]))
      new_vals <- extract_species_params(htinc_posterior, stan_name, fvs_len)
      result <- safe_update(original_config, "other", fvs_name, new_vals)
      original_config <- result$config
    }
  }

  # Update species group parameters (HGHC, HGLDD, HGH2)
  hg_grp_mapping <- list(
    HGHC  = "gamma_shape",
    HGLDD = "b1",
    HGH2  = "b2"
  )

  for (fvs_name in names(hg_grp_mapping)) {
    stan_name <- hg_grp_mapping[[fvs_name]]

    if (fvs_name %in% names(original_config$categories$other)) {
      fvs_len <- length(unlist(original_config$categories$other[[fvs_name]]))
      new_vals <- extract_species_params(htinc_posterior, stan_name, fvs_len)
      result <- safe_update(original_config, "other", fvs_name, new_vals)
      original_config <- result$config
    }
  }

  # Update shared fixed effects (HGLH)
  if ("HGLH" %in% names(original_config$categories$other)) {
    val <- extract_param(htinc_posterior, "b2")
    if (!is.na(val)) {
      fvs_len <- length(unlist(original_config$categories$other$HGLH))
      original_config$categories$other$HGLH <- as.list(rep(val, fvs_len))
      logger::log_info("  HGLH: updated to {round(val, 6)}")
    }
  }

  components_updated <- c(components_updated, "height_increment")

} else {
  logger::log_info("Height increment posterior not found (expected for most variants)")
  components_skipped <- c(components_skipped, "height_increment")
}

# =============================================================================
# 4. Mortality Parameters
# =============================================================================

logger::log_info("=== Processing mortality posteriors ===")

mort_summary_file <- file.path(output_dir, "mortality_summary.csv")

if (file.exists(mort_summary_file)) {
  mort_posterior <- read_csv(mort_summary_file, show_col_types = FALSE) %>% as_tibble()
  logger::log_info("Loaded {nrow(mort_posterior)} mortality parameters")

  # Store mortality calibration parameters
  if (!"mortality" %in% names(original_config$categories)) {
    original_config$categories$mortality <- list()
  }

  # Extract population level effects from brms output
  mort_params <- list(
    MORT_INT  = "b_Intercept",
    MORT_DBH  = "b_DBH_std",
    MORT_DBH2 = "b_IDBH_stdE2",
    MORT_BAL  = "b_BAL_std",
    MORT_CR   = "b_CR_std",
    MORT_SI   = "b_SI_std",
    MORT_BA   = "b_BA_std"
  )

  for (fvs_name in names(mort_params)) {
    brms_name <- mort_params[[fvs_name]]
    val <- extract_param(mort_posterior, brms_name)
    if (!is.na(val)) {
      original_config$categories$mortality[[fvs_name]] <- val
      logger::log_info("  {fvs_name}: {round(val, 6)}")
    }
  }

  # Extract species random effects
  sp_effects <- mort_posterior %>%
    filter(grepl("r_SPCD", variable)) %>%
    mutate(
      sp_idx = as.integer(gsub(".*\\[(.+),.*", "\\1", variable))
    )

  if (nrow(sp_effects) > 0) {
    est_col <- if ("Estimate" %in% names(sp_effects)) "Estimate" else
               if ("p50" %in% names(sp_effects)) "p50" else
               if ("median" %in% names(sp_effects)) "median" else NULL

    if (!is.null(est_col)) {
      sp_mort_adj <- sp_effects %>%
        select(sp_idx, value = !!sym(est_col)) %>%
        arrange(sp_idx) %>%
        pull(value)

      original_config$categories$mortality$SPECIES_ADJUSTMENT <- as.list(sp_mort_adj)
      logger::log_info("  Species adjustments: {length(sp_mort_adj)} values")
    }
  }

  components_updated <- c(components_updated, "mortality")

} else {
  logger::log_warn("Mortality posterior not found")
  components_skipped <- c(components_skipped, "mortality")
}

# =============================================================================
# 5. Crown Ratio Change Parameters
# =============================================================================

logger::log_info("=== Processing crown ratio posteriors ===")

cr_summary_file <- file.path(output_dir, "crown_ratio_summary.csv")

if (file.exists(cr_summary_file)) {
  cr_posterior <- read_csv(cr_summary_file, show_col_types = FALSE) %>% as_tibble()
  logger::log_info("Loaded {nrow(cr_posterior)} crown ratio parameters")

  # Map brms fixed effects to FVS BCR parameters
  cr_fixed_mapping <- list(
    b_Intercept = "BCR_INT",
    b_DBH_std   = "BCR1",
    b_BA_std    = "BCR2",
    b_BAL_std   = "BCR_BAL",
    b_CR_std    = "BCR3",
    b_SI_std    = "BCR4"
  )

  for (brms_name in names(cr_fixed_mapping)) {
    fvs_name <- cr_fixed_mapping[[brms_name]]
    val <- extract_param(cr_posterior, brms_name)

    if (!is.na(val)) {
      # BCR1-BCR4 are species indexed arrays in the crown category
      if (fvs_name %in% names(original_config$categories$crown)) {
        fvs_len <- length(unlist(original_config$categories$crown[[fvs_name]]))
        original_vals <- unlist(original_config$categories$crown[[fvs_name]])
        # Additive adjustment to original values
        original_config$categories$crown[[fvs_name]] <- as.list(
          original_vals + val
        )
        logger::log_info("  {fvs_name}: adjusted by {round(val, 6)} ({fvs_len} species)")
      } else {
        # Store as new parameter in crown_change category
        if (!"crown_change" %in% names(original_config$categories)) {
          original_config$categories$crown_change <- list()
        }
        original_config$categories$crown_change[[fvs_name]] <- val
        logger::log_info("  {fvs_name} (new): {round(val, 6)}")
      }
    }
  }

  # Extract species random effects for crown ratio
  sp_cr_effects <- cr_posterior %>%
    filter(grepl("r_SPCD", variable)) %>%
    mutate(
      sp_idx = as.integer(gsub(".*\\[(.+),.*", "\\1", variable))
    )

  if (nrow(sp_cr_effects) > 0) {
    est_col <- if ("Estimate" %in% names(sp_cr_effects)) "Estimate" else
               if ("p50" %in% names(sp_cr_effects)) "p50" else
               if ("median" %in% names(sp_cr_effects)) "median" else NULL

    if (!is.null(est_col)) {
      sp_cr_adj <- sp_cr_effects %>%
        select(sp_idx, value = !!sym(est_col)) %>%
        arrange(sp_idx) %>%
        pull(value)

      if (!"crown_change" %in% names(original_config$categories)) {
        original_config$categories$crown_change <- list()
      }
      original_config$categories$crown_change$SPECIES_ADJUSTMENT <- as.list(sp_cr_adj)
      logger::log_info("  CR species adjustments: {length(sp_cr_adj)} values")
    }
  }

  components_updated <- c(components_updated, "crown_ratio")

} else {
  logger::log_warn("Crown ratio posterior not found")
  components_skipped <- c(components_skipped, "crown_ratio")
}

# =============================================================================
# Add Calibration Metadata
# =============================================================================

logger::log_info("Adding calibration metadata...")

original_config$calibration <- list(
  date = as.character(Sys.Date()),
  method = "Bayesian MCMC (CmdStanR + brms)",
  fia_data_source = "rFIA package",
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  prior_type = "Informative (from original FVS parameters)",
  components_updated = as.list(components_updated),
  components_skipped = as.list(components_skipped),
  notes = paste0(
    "Calibration based on FIA remeasurement pairs. ",
    length(components_updated), " of 5 component models integrated."
  )
)

# =============================================================================
# Save Calibrated Config
# =============================================================================

logger::log_info("Saving calibrated config...")

output_file <- file.path(calibrated_dir, paste0(variant, ".json"))
json_output <- toJSON(original_config, pretty = TRUE, auto_unbox = TRUE)
write(json_output, file = output_file)

logger::log_info("Saved calibrated config to {output_file}")

# =============================================================================
# Summary Report
# =============================================================================

cat("\n")
cat("==========================================\n")
cat("Posterior to JSON Conversion Complete\n")
cat("==========================================\n")
cat("Variant:", variant, "\n")
cat("Original config:", original_config_file, "\n")
cat("Calibrated config:", output_file, "\n")
cat("Species:", n_species, "\n")
cat("Calibration date:", as.character(Sys.Date()), "\n")
cat("\nComponents updated:\n")
for (comp in components_updated) {
  cat("  +", comp, "\n")
}
if (length(components_skipped) > 0) {
  cat("\nComponents skipped (posterior not available):\n")
  for (comp in components_skipped) {
    cat("  -", comp, "\n")
  }
}
cat("\n")

logger::log_info("Posterior to JSON conversion complete: {length(components_updated)} components integrated")

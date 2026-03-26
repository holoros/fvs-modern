#!/usr/bin/env Rscript
#
# FVS Bayesian Calibration: FIA Data Fetch and Preparation
# Downloads FIA data and prepares remeasurement pairs for calibration
#
# Usage: Rscript calibration/R/01_fetch_fia_data.R
#

library(rFIA)
library(tidyverse)
library(data.table)
library(parallel)
library(progress)
library(logger)

# ============================================================================
# Configuration
# ============================================================================

project_root <- "/home/aweiskittel/Documents/Claude/fvs-modern"
calibration_dir <- file.path(project_root, "calibration")
fia_data_dir <- file.path(calibration_dir, "data", "raw_fia")
processed_data_dir <- file.path(calibration_dir, "data", "processed")

# Set up logging
log_file <- file.path(calibration_dir, "logs", "01_fetch_fia_data.log")
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting FIA data fetch and preparation")

# ============================================================================
# FVS Variant to Region Mapping
# This maps FVS variant codes to FIA regions (states)
# ============================================================================

variant_states <- list(
  "acd" = c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"),
  "ak"  = c("AK"),
  "bc"  = c("BC"),
  "bm"  = c("AZ", "NM"),
  "ca"  = c("CA"),
  "ci"  = c("UT", "WY"),
  "cr"  = c("CO"),
  "cs"  = c("CO"),
  "ec"  = c("CT", "MA", "NH", "RI", "VT"),
  "em"  = c("IL", "IN", "MI", "MN", "MO", "OH", "WI"),
  "ie"  = c("IA", "NE"),
  "kt"  = c("KS"),
  "ls"  = c("ME"),
  "nc"  = c("NC"),
  "ne"  = c("NE"),
  "oc"  = c("OR", "WA"),
  "on"  = c("ON"),
  "op"  = c("OR"),
  "pn"  = c("OR", "WA"),
  "sn"  = c("NV", "CA"),
  "so"  = c("AZ", "NM"),
  "tt"  = c("TX"),
  "ut"  = c("UT"),
  "wc"  = c("WA"),
  "ws"  = c("WA")
)

# Get all unique states needed
all_states <- unique(unlist(variant_states))

logger::log_info("Fetching FIA data for {length(all_states)} states")

# ============================================================================
# Fetch FIA Data
# ============================================================================

# Download FIA data for all states
# rFIA will cache data locally in tempdir() by default
fia_inventory <- list()

pb <- progress_bar$new(
  format = "Fetching FIA data [:bar] :current/:total (:percent)",
  total = length(all_states),
  clear = FALSE
)

for (state in all_states) {
  pb$tick()
  tryCatch({
    fia_inventory[[state]] <- rFIA::readFIA(
      nCores = 1,
      states = state
    )
  }, error = function(e) {
    logger::log_warn("Error fetching data for {state}: {e$message}")
  })
}

logger::log_info("Successfully fetched FIA data for {length(fia_inventory)} states")

# ============================================================================
# Extract Tree-Level Remeasurement Pairs
# ============================================================================

logger::log_info("Extracting remeasurement pairs...")

# Combine all FIA data
fia_all <- do.call(rbind, fia_inventory)

# Extract tree data with remeasurement information
tree_data <- fia_all$tree %>%
  as_tibble() %>%
  mutate(
    # Ensure unique identification
    tree_id = paste(PLT_CN, SUBP, TREE,
                    sep = "_")
  ) %>%
  select(
    PLT_CN, SUBP, TREE, STATUSCD, DIA, HT, SPCD,
    tree_id, INVYR
  ) %>%
  arrange(tree_id, INVYR)

# Get plot location and environmental data
plot_data <- fia_all$plot %>%
  as_tibble() %>%
  select(
    PLT_CN, LAT, LON, ELEV, SLOPE, ASPECT,
    PLOT_STATUS_CD, MEASMON, MEASYEAR
  )

# Link tree data with plot data
tree_plot <- tree_data %>%
  left_join(plot_data, by = "PLT_CN")

# Identify remeasurement pairs
# A remeasurement pair: same tree measured twice with alive status both times
remeasurement_pairs <- tree_plot %>%
  group_by(tree_id) %>%
  filter(n() >= 2) %>%  # At least 2 measurements
  arrange(INVYR) %>%
  # Create pairs: measurement i and i+1
  mutate(
    next_meas = lead(INVYR),
    DIA_t1 = DIA,
    DIA_t2 = lead(DIA),
    HT_t1 = HT,
    HT_t2 = lead(HT),
    SPCD_obs = SPCD,
    years_interval = next_meas - INVYR,
    status_t1 = STATUSCD,
    status_t2 = lead(STATUSCD)
  ) %>%
  filter(
    !is.na(DIA_t2),
    !is.na(years_interval),
    status_t1 == 1,  # Alive at measurement 1
    status_t2 == 1,  # Alive at measurement 2
    !is.na(DIA_t1),
    !is.na(DIA_t2),
    DIA_t1 > 0,
    DIA_t2 > 0,
    years_interval > 0,
    years_interval <= 15  # Reasonable measurement interval
  ) %>%
  select(
    PLT_CN, tree_id, SPCD = SPCD_obs, INVYR, next_meas,
    DIA_t1, DIA_t2, HT_t1, HT_t2, years_interval,
    LAT, LON, ELEV, SLOPE, ASPECT,
    MEASMON, MEASYEAR
  )

logger::log_info("Found {nrow(remeasurement_pairs)} remeasurement pairs")

# ============================================================================
# Compute Bark Ratio (Inside-Bark Diameter)
# Use standard FVS bark ratio models
# ============================================================================

# Load bark ratio parameters from config
config_file <- file.path(project_root, "config", "ca.json")  # Use CA as reference
config <- jsonlite::fromJSON(config_file)
bkrat_params <- config$categories$bark_ratio$BKRAT

# Function to compute inside-bark diameter from outside-bark
# Using FVS standard: DIB = DOB * BKRAT (simplified)
# For full accuracy, would use: DIB = B0 + B1 * DOB + B2 * DOB^2
# Here we use the provided BKRAT as average bark ratio per species
compute_inside_bark <- function(dob, spcd, bkrat_vector) {
  # Map species code to index (1-based)
  # Assuming species codes are in order in the config
  # Use average bark ratio if species not found
  avg_bkrat <- mean(bkrat_vector, na.rm = TRUE)
  dib <- dob * avg_bkrat
  return(dib)
}

# For now, use average bark ratio across all species
# (More sophisticated models would use species specific ratios)
avg_bark_ratio <- mean(bkrat_params, na.rm = TRUE)

remeasurement_pairs <- remeasurement_pairs %>%
  mutate(
    # Convert outside-bark to inside-bark diameter
    DIB_t1 = DIA_t1 * avg_bark_ratio,
    DIB_t2 = DIA_t2 * avg_bark_ratio,
    # Compute diameter increment on squared scale (FVS Wykoff model uses DDS)
    DDS = DIB_t2^2 - DIB_t1^2,
    ln_DDS = log(DDS + 0.001)  # Add small constant for numerical stability
  )

# ============================================================================
# Compute Derived Variables
# ============================================================================

logger::log_info("Computing derived variables...")

# Need to compute:
# - Stand-level metrics (BA, CCF, BAL, etc.)
# - Site index (from height-diameter relationship)
# - Crown ratio
# - Slope/aspect transformations

# For each plot, compute BA (basal area in sq ft per acre) and other metrics
stand_metrics <- tree_plot %>%
  filter(STATUSCD == 1) %>%  # Only living trees
  group_by(PLT_CN, INVYR) %>%
  summarise(
    # Basal area (convert from sq cm to sq ft per acre)
    BA_sqftpa = sum((DIA^2 * 0.005454) / 0.01, na.rm = TRUE),
    # Count of trees >= 1 inch DBH per acre
    TPA_1 = n() / 0.01,
    # Dominant height (mean of tallest 100 trees per acre)
    HDTOP = mean(HT[DIA >= quantile(DIA, 0.85, na.rm = TRUE)],
                 na.rm = TRUE),
    .groups = "drop"
  )

# Compute site index using height-age-diameter relationship
# Simplified version: SI based on height of largest trees
site_index_data <- stand_metrics %>%
  mutate(
    # Temporary site index estimate
    SI_tmp = HDTOP * 50  # Placeholder; refined in height diameter fit
  )

# Add stand metrics to remeasurement pairs
remeasurement_pairs <- remeasurement_pairs %>%
  left_join(
    stand_metrics %>%
      select(PLT_CN, INVYR, BA_sqftpa, TPA_1, HDTOP),
    by = c("PLT_CN", "INVYR")
  ) %>%
  left_join(
    site_index_data %>%
      select(PLT_CN, INVYR, SI_tmp),
    by = c("PLT_CN", "INVYR")
  ) %>%
  rename(SI = SI_tmp, BA = BA_sqftpa)

# ============================================================================
# Compute Basal Area Larger (BAL) and Crown Ratio
# ============================================================================

# BAL = basal area of trees larger than subject tree
# Need to recompute from raw tree data
bal_data <- tree_plot %>%
  filter(STATUSCD == 1, !is.na(DIA)) %>%
  group_by(PLT_CN, INVYR) %>%
  arrange(DIA) %>%
  mutate(
    # Convert to inside bark for fair comparison
    DIB_plot = DIA * avg_bark_ratio,
    # Basal area in sq cm
    BA_tree = pi * (DIB_plot / 2)^2,
    # BAL = sum of BA for trees larger than this tree
    BAL_sqft = cumsum(BA_tree * 0.000323) - (BA_tree * 0.000323)
  ) %>%
  select(PLT_CN, INVYR, TREE, SUBP, BAL_sqft) %>%
  ungroup()

# Add BAL to main dataset
remeasurement_pairs <- remeasurement_pairs %>%
  left_join(bal_data, by = c("PLT_CN", "INVYR", "TREE", "SUBP"))

# Crown ratio: HT / (max HT in plot)
crown_data <- tree_plot %>%
  filter(STATUSCD == 1) %>%
  group_by(PLT_CN, INVYR) %>%
  mutate(
    max_ht = max(HT, na.rm = TRUE),
    CR = HT / max_ht
  ) %>%
  select(PLT_CN, INVYR, TREE, SUBP, CR) %>%
  ungroup()

remeasurement_pairs <- remeasurement_pairs %>%
  left_join(crown_data, by = c("PLT_CN", "INVYR", "TREE", "SUBP"))

# ============================================================================
# Compute Slope and Aspect Transformations
# ============================================================================

remeasurement_pairs <- remeasurement_pairs %>%
  mutate(
    # Convert degrees to radians
    ASPECT_rad = ASPECT * pi / 180,
    # Compound transformation for slope and aspect
    SLOPE_SASP = SLOPE * sin(ASPECT_rad),
    SLOPE_CASP = SLOPE * cos(ASPECT_rad),
    # Elevation in hundreds of feet
    ELEV_100ft = ELEV / 100,
    # Convert elevation to kilometers
    ELEV_km = ELEV / 3280.84
  )

# ============================================================================
# Additional Model Variables
# ============================================================================

remeasurement_pairs <- remeasurement_pairs %>%
  mutate(
    # Logarithmic transformations (for Wykoff model)
    ln_DBH_t1 = log(DIA_t1),
    DBH_sq_t1 = DIA_t1^2,
    ln_SI = log(pmax(SI, 1)),  # Avoid log(0)
    SLOPE_sq = SLOPE^2,
    ELEV_sq = ELEV_km^2,
    CR_sq = CR^2,
    # Annual increment (normalize to years)
    DDS_annual = DDS / years_interval,
    ln_DDS_annual = log(DDS_annual + 0.001),
    # Standardized variables (for priors)
    ln_DBH_std = scale(ln_DBH_t1)[, 1],
    SLOPE_std = scale(SLOPE)[, 1],
    ELEV_std = scale(ELEV_km)[, 1],
    CR_std = scale(CR)[, 1]
  ) %>%
  # Remove any rows with missing critical values
  filter(
    !is.na(DDS),
    !is.na(DIA_t1),
    !is.na(SI),
    !is.na(BAL_sqft),
    !is.na(BA),
    !is.na(CR),
    !is.na(SLOPE),
    !is.na(ELEV),
    DDS > 0
  )

logger::log_info("After QA/QC: {nrow(remeasurement_pairs)} observations")

# ============================================================================
# Assign FVS Variants Based on Plot Location
# ============================================================================

# Load variant geographic boundaries (simplified: use state mapping)
# In production, would use spatial intersection with variant shapefiles
remeasurement_pairs <- remeasurement_pairs %>%
  mutate(
    # Get state from coordinates (simplified; production code would use spatial join)
    # For now, assign variant based on region heuristic
    variant = case_when(
      LAT > 42 & LON < -70 ~ "ec",  # Northeast
      LAT > 42 & LON >= -70 & LON < -80 ~ "ls",  # Maine
      LAT < 40 & LAT > 35 & LON > -85 ~ "kt",  # Central plains
      LAT < 35 ~ "acd",  # South
      LAT >= 40 & LON < -100 ~ "op",  # Pacific NW
      TRUE ~ "em"  # Midwest default
    )
  )

# ============================================================================
# Partition by Variant and Save
# ============================================================================

logger::log_info("Saving data by variant...")

# Create output directory structure
for (var in unique(remeasurement_pairs$variant)) {
  var_dir <- file.path(processed_data_dir, var)
  dir.create(var_dir, showWarnings = FALSE, recursive = TRUE)
}

# Save by variant
for (var in unique(remeasurement_pairs$variant)) {
  var_data <- remeasurement_pairs %>%
    filter(variant == var) %>%
    select(
      # Identifiers
      PLT_CN, tree_id, SPCD, variant,
      # Measurements
      DIA_t1, DIA_t2, DIB_t1, DIB_t2, DDS, ln_DDS,
      HT_t1, HT_t2, years_interval,
      # Covariates
      ln_DBH_t1, DBH_sq_t1, ln_SI, SLOPE, SLOPE_sq,
      SLOPE_SASP, SLOPE_CASP, ELEV_km, ELEV_sq,
      CR, CR_sq, BAL_sqft, BA,
      # Location
      LAT, LON, ELEV,
      # Standardized versions
      ln_DBH_std, SLOPE_std, ELEV_std, CR_std
    )

  # Save as CSV
  output_file <- file.path(processed_data_dir, var, "diameter_growth.csv")
  write_csv(var_data, output_file)

  logger::log_info("Saved {nrow(var_data)} observations for variant {var}")
}

# ============================================================================
# Summary Statistics
# ============================================================================

cat("\n")
cat("========================================\n")
cat("FIA Data Fetch and Preparation Complete\n")
cat("========================================\n")
cat("Total remeasurement pairs:", nrow(remeasurement_pairs), "\n")
cat("Variants represented:", n_distinct(remeasurement_pairs$variant), "\n")
cat("Species represented:", n_distinct(remeasurement_pairs$SPCD), "\n")
cat("Date range:", min(remeasurement_pairs$INVYR), "-",
    max(remeasurement_pairs$INVYR), "\n")
cat("Measurement intervals (years):\n")
print(summary(remeasurement_pairs$years_interval))
cat("\nData saved to:", processed_data_dir, "\n\n")

logger::log_info("FIA data fetch and preparation complete")
logger::log_info("Total observations: {nrow(remeasurement_pairs)}")

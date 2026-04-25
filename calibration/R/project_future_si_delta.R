# =============================================================================
# Title: Project Future Site Index via Delta Method
# Author: A. Weiskittel
# Date: 2026-04-17
# Description: Extracts current ClimateNA bioclim at FIA plot locations from
#              existing rasters, applies HadGEM2-ES RCP 4.5/8.5 deltas for
#              2030s, 2060s, and 2090s, and predicts future SI using the
#              fitted ranger model. Produces per-plot SI change ratios for
#              PERSEUS FVS integration.
#
# Delta method:
#   future_value = current_value + delta (additive for temperature vars)
#   future_value = current_value * (1 + delta) (multiplicative for precip)
#
# HadGEM2-ES deltas for northeastern US (Maine) from CMIP5 archive:
#   Approximate 30-year normal changes relative to 1991-2020 baseline.
#
# Usage:
#   module load gcc/12.3.0 R/4.4.0 proj/9.2.1 gdal/3.7.3 geos/3.12.0
#   Rscript project_future_si_delta.R
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(ranger)
  library(sf)
  library(data.table)
})

# --- Configuration -----------------------------------------------------------
# All paths come from environment variables with sensible defaults so the
# script is portable across HPC sites. Override any of these before running:
#   SITEINDEX_DIR: base directory containing rasters and the ranger model
#   FIA_DATA_DIR:  FIA plot database root
#   SI_OUTPUT_DIR: where projected SI rasters are written
BASE_DIR     <- Sys.getenv("SITEINDEX_DIR",
                            file.path(Sys.getenv("HOME"), "SiteIndex"))
RASTER_DIR   <- file.path(BASE_DIR, "rasters/ClimateNA/Normal_1991_2020_bioclim")
MODEL_FILE   <- file.path(BASE_DIR, "final_RF_spatial_model.rds")
ECO_SHP      <- file.path(BASE_DIR, "NA_Eco_L3_WGS84.shp")
CURRENT_SI   <- file.path(BASE_DIR, "ClimateNA_ECO_SI_m .tif")
FIA_DATA_DIR <- Sys.getenv("FIA_DATA_DIR",
                            file.path(Sys.getenv("HOME"), "fia_data"))
OUTPUT_DIR   <- Sys.getenv("SI_OUTPUT_DIR", file.path(BASE_DIR, "future_SI"))
N_CORES      <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "8"))

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- HadGEM2-ES deltas for Maine (CMIP5 archive, NE US region) --------
# Format: list of (period, rcp) -> named list of variable deltas
# Temperature variables: additive (C)
# Precipitation variables: multiplicative fraction
# DD variables: estimated from temperature scaling
#
# Sources: MACA downscaled CMIP5, NCA4 regional projections
# HadGEM2-ES is among the warmer CMIP5 models for NE US
DELTAS <- list(
  "rcp45_2030s" = list(
    MAT = 1.2, MWMT = 1.4, MCMT = 1.5, EMT = 1.8, EXT = 1.2,
    MAP_frac = 0.03, MSP_frac = 0.02, PPT_sm_frac = 0.01,
    PPT_sp_frac = 0.04, PPT_at_frac = 0.03, PPT_wt_frac = 0.04,
    DD5_frac = 0.08, DD_0_frac = -0.12, DD1040_frac = 0.05,
    DD_18_frac = -0.05, DD18_frac = 0.10,
    bFFP = -3, eFFP = 3, FFP = 6, NFFD = 8,
    Tave_sm = 1.4, Tave_sp = 1.2, Tave_at = 1.1, Tave_wt = 1.5,
    AHM_frac = 0.02, CMD_frac = 0.05, CMI_frac = -0.02,
    Eref_frac = 0.04, PAS_frac = -0.10, RH_frac = -0.01,
    SHM_frac = 0.03, TD = 0.0
  ),
  "rcp45_2060s" = list(
    MAT = 2.0, MWMT = 2.3, MCMT = 2.4, EMT = 3.0, EXT = 2.0,
    MAP_frac = 0.05, MSP_frac = 0.03, PPT_sm_frac = 0.01,
    PPT_sp_frac = 0.06, PPT_at_frac = 0.05, PPT_wt_frac = 0.07,
    DD5_frac = 0.15, DD_0_frac = -0.22, DD1040_frac = 0.10,
    DD_18_frac = -0.10, DD18_frac = 0.18,
    bFFP = -5, eFFP = 5, FFP = 10, NFFD = 14,
    Tave_sm = 2.3, Tave_sp = 2.0, Tave_at = 1.8, Tave_wt = 2.4,
    AHM_frac = 0.04, CMD_frac = 0.10, CMI_frac = -0.04,
    Eref_frac = 0.08, PAS_frac = -0.20, RH_frac = -0.02,
    SHM_frac = 0.06, TD = -0.1
  ),
  "rcp45_2090s" = list(
    MAT = 2.5, MWMT = 2.8, MCMT = 3.0, EMT = 3.8, EXT = 2.5,
    MAP_frac = 0.07, MSP_frac = 0.04, PPT_sm_frac = 0.02,
    PPT_sp_frac = 0.08, PPT_at_frac = 0.07, PPT_wt_frac = 0.09,
    DD5_frac = 0.20, DD_0_frac = -0.30, DD1040_frac = 0.13,
    DD_18_frac = -0.14, DD18_frac = 0.25,
    bFFP = -7, eFFP = 7, FFP = 14, NFFD = 18,
    Tave_sm = 2.8, Tave_sp = 2.5, Tave_at = 2.2, Tave_wt = 3.0,
    AHM_frac = 0.05, CMD_frac = 0.14, CMI_frac = -0.05,
    Eref_frac = 0.10, PAS_frac = -0.28, RH_frac = -0.03,
    SHM_frac = 0.08, TD = -0.2
  ),
  "rcp85_2030s" = list(
    MAT = 1.5, MWMT = 1.7, MCMT = 1.8, EMT = 2.2, EXT = 1.5,
    MAP_frac = 0.03, MSP_frac = 0.02, PPT_sm_frac = 0.01,
    PPT_sp_frac = 0.04, PPT_at_frac = 0.03, PPT_wt_frac = 0.05,
    DD5_frac = 0.10, DD_0_frac = -0.15, DD1040_frac = 0.07,
    DD_18_frac = -0.07, DD18_frac = 0.12,
    bFFP = -4, eFFP = 4, FFP = 8, NFFD = 10,
    Tave_sm = 1.7, Tave_sp = 1.5, Tave_at = 1.3, Tave_wt = 1.8,
    AHM_frac = 0.03, CMD_frac = 0.07, CMI_frac = -0.03,
    Eref_frac = 0.05, PAS_frac = -0.12, RH_frac = -0.01,
    SHM_frac = 0.04, TD = 0.0
  ),
  "rcp85_2060s" = list(
    MAT = 3.2, MWMT = 3.6, MCMT = 3.8, EMT = 4.5, EXT = 3.2,
    MAP_frac = 0.06, MSP_frac = 0.04, PPT_sm_frac = 0.00,
    PPT_sp_frac = 0.07, PPT_at_frac = 0.06, PPT_wt_frac = 0.09,
    DD5_frac = 0.25, DD_0_frac = -0.35, DD1040_frac = 0.15,
    DD_18_frac = -0.18, DD18_frac = 0.35,
    bFFP = -9, eFFP = 9, FFP = 18, NFFD = 25,
    Tave_sm = 3.6, Tave_sp = 3.2, Tave_at = 2.8, Tave_wt = 3.8,
    AHM_frac = 0.08, CMD_frac = 0.18, CMI_frac = -0.08,
    Eref_frac = 0.14, PAS_frac = -0.35, RH_frac = -0.04,
    SHM_frac = 0.12, TD = -0.2
  ),
  "rcp85_2090s" = list(
    MAT = 4.8, MWMT = 5.2, MCMT = 5.5, EMT = 6.5, EXT = 4.8,
    MAP_frac = 0.09, MSP_frac = 0.05, PPT_sm_frac = -0.02,
    PPT_sp_frac = 0.10, PPT_at_frac = 0.08, PPT_wt_frac = 0.12,
    DD5_frac = 0.40, DD_0_frac = -0.50, DD1040_frac = 0.20,
    DD_18_frac = -0.28, DD18_frac = 0.55,
    bFFP = -14, eFFP = 14, FFP = 28, NFFD = 38,
    Tave_sm = 5.2, Tave_sp = 4.8, Tave_at = 4.2, Tave_wt = 5.5,
    AHM_frac = 0.12, CMD_frac = 0.30, CMI_frac = -0.12,
    Eref_frac = 0.22, PAS_frac = -0.50, RH_frac = -0.06,
    SHM_frac = 0.18, TD = -0.5
  )
)

# --- Load model and check required variables ---------------------------------
cat("Loading ranger model...\n")
rf_model <- readRDS(MODEL_FILE)
model_vars <- rf_model$forest$independent.variable.names
cat(sprintf("  Model requires %d variables\n", length(model_vars)))

# --- Get FIA Maine plot locations --------------------------------------------
get_maine_plots <- function() {
  plot_file <- file.path(FIA_DATA_DIR, "ME_PLOT.csv")
  if (!file.exists(plot_file)) {
    # Try alternate locations
    for (alt in c("PLOT.csv", "me_plot.csv")) {
      alt_path <- file.path(FIA_DATA_DIR, alt)
      if (file.exists(alt_path)) { plot_file <- alt_path; break }
    }
  }

  if (!file.exists(plot_file)) {
    stop("Cannot find Maine FIA PLOT table at ", plot_file)
  }

  cat(sprintf("  Reading FIA plots from %s\n", plot_file))
  plots <- fread(plot_file)

  # Keep unique plot locations with valid coordinates
  plots <- plots[!is.na(LAT) & !is.na(LON) & STATECD == 23,
                 .(STATECD, COUNTYCD, PLOT, LAT, LON, ELEV, INVYR)]
  # Most recent inventory per plot
  plots <- plots[, .SD[which.max(INVYR)], by = .(STATECD, COUNTYCD, PLOT)]
  plots[, plot_id := paste(STATECD, COUNTYCD, PLOT, sep = "_")]

  cat(sprintf("  %d unique Maine plot locations\n", nrow(plots)))
  return(plots)
}

# --- Extract current bioclim at plot locations --------------------------------
extract_current_climate <- function(plots) {
  cat("Extracting current climate from Normal_1991_2020 rasters...\n")

  raster_files <- list.files(RASTER_DIR, pattern = "\\.tif$",
                             full.names = TRUE, ignore.case = TRUE)
  climate_stack <- rast(raster_files)

  # Clean names
  raw_names <- gsub("\\.(tif|asc)$", "", basename(raster_files))
  clean_names <- gsub("^Normal_1991_2020_", "", raw_names)
  names(climate_stack) <- clean_names

  cat(sprintf("  Loaded %d climate layers\n", nlyr(climate_stack)))

  # Extract at plot locations
  pts <- vect(plots, geom = c("LON", "LAT"), crs = "EPSG:4326")
  extracted <- terra::extract(climate_stack, pts)

  # Merge back
  for (var in clean_names) {
    plots[[var]] <- extracted[[var]]
  }

  # Also extract current SI
  if (file.exists(CURRENT_SI)) {
    si_rast <- rast(CURRENT_SI)
    si_vals <- terra::extract(si_rast, pts)
    plots$SI_current <- si_vals[, 2]
    cat(sprintf("  Current SI: mean=%.1f m, median=%.1f m\n",
                mean(plots$SI_current, na.rm = TRUE),
                median(plots$SI_current, na.rm = TRUE)))
  }

  return(plots)
}

# --- Add ecoregion codes -----------------------------------------------------
add_ecoregions <- function(plots) {
  if (!file.exists(ECO_SHP)) {
    cat("  Ecoregion shapefile not found. Trying rasterized approach...\n")
    return(plots)
  }

  cat("  Assigning ecoregion codes...\n")
  sf_use_s2(FALSE)  # Disable spherical geometry to handle bad geometries
  eco <- st_read(ECO_SHP, quiet = TRUE)
  eco <- st_make_valid(eco)
  plots_sf <- st_as_sf(as.data.frame(plots),
                        coords = c("LON", "LAT"), crs = 4326)
  eco_proj <- st_transform(eco, 4326)
  joined <- st_join(plots_sf, eco_proj[, c("NA_L3CODE", "NA_L2CODE", "NA_L1CODE")])

  plots$NA_L3CODE <- as.factor(joined$NA_L3CODE)
  plots$NA_L2CODE <- as.factor(joined$NA_L2CODE)
  plots$NA_L1CODE <- as.factor(joined$NA_L1CODE)

  return(plots)
}

# --- Apply deltas to create future climate -----------------------------------
apply_deltas <- function(plots, delta_list) {
  future <- copy(plots)

  # Additive temperature variables
  temp_vars <- c("MAT", "MWMT", "MCMT", "EMT", "EXT",
                 "Tave_sm", "Tave_sp", "Tave_at", "Tave_wt", "TD")
  for (var in temp_vars) {
    if (var %in% names(future) && var %in% names(delta_list)) {
      future[[var]] <- future[[var]] + delta_list[[var]]
    }
  }

  # Additive phenology variables (days)
  pheno_vars <- c("bFFP", "eFFP", "FFP", "NFFD")
  for (var in pheno_vars) {
    if (var %in% names(future) && var %in% names(delta_list)) {
      future[[var]] <- future[[var]] + delta_list[[var]]
    }
  }

  # Multiplicative precipitation / DD variables
  frac_vars <- grep("_frac$", names(delta_list), value = TRUE)
  for (fv in frac_vars) {
    base_var <- gsub("_frac$", "", fv)
    if (base_var %in% names(future)) {
      future[[base_var]] <- future[[base_var]] * (1 + delta_list[[fv]])
    }
  }

  return(future)
}

# --- Compute derived variables -----------------------------------------------
add_derived <- function(df) {
  # Renamed aliases (ClimateNA -> model names)
  df$sday <- df$bFFP
  df$fday <- df$eFFP
  df$ffp  <- df$FFP
  df$mmin <- df$EMT
  df$mmax <- df$EXT
  df$d100 <- df$DD1040
  df$map  <- df$MAP
  df$mat  <- df$MAT
  df$mtcm <- df$MCMT
  df$gsp  <- df$MSP
  df$mtwm <- df$MWMT
  df$dd0  <- df$DD_0
  df$dd5  <- df$DD5

  # Derived metrics
  df$tdiff  <- df$MWMT - df$MCMT
  df$pratio <- df$MSP / df$MAP
  df$adi    <- sqrt(pmax(df$DD5, 0)) / pmax(df$MAP, 1)
  df$sdi    <- sqrt(pmax(df$DD5, 0)) / pmax(df$MSP, 1)

  # Interactions
  df$adimindd0 <- df$adi * df$DD_0
  df$sdimindd0 <- df$sdi * df$DD_0
  df$dd5mtcm   <- df$DD5 * df$MCMT
  df$gspdd5    <- df$MSP * df$DD5
  df$mapdd5    <- df$MAP * df$DD5
  df$dd5dd0    <- df$DD5 * df$DD_0
  df$mmindd0   <- df$EMT * df$DD_0
  df$mapdd0    <- df$MAP * df$DD_0

  # Growing season DD5
  if ("GSP" %in% names(df)) {
    df$gsdd5 <- df$GSP * df$DD5
  } else {
    df$gsdd5 <- df$MSP * df$DD5
  }

  # Additional interactions the model expects
  df$gspmtcm <- df$MSP * df$MCMT
  df$gsptd   <- df$MSP * (df$MWMT - df$MCMT)
  df$mapmtcm <- df$MAP * df$MCMT
  df$maptd   <- df$MAP * (df$MWMT - df$MCMT)
  df$mtcmgsp <- df$MCMT * df$MSP
  df$mtcmmap <- df$MCMT * df$MAP
  df$prdd5   <- df$pratio * df$DD5
  df$prmtcm  <- df$pratio * df$MCMT
  df$tdgsp   <- (df$MWMT - df$MCMT) * df$MSP
  df$tdmap   <- (df$MWMT - df$MCMT) * df$MAP

  # Spatial
  df$LAT_LON_P <- df$LAT + df$LON
  df$LAT_LON_M <- df$LAT - df$LON

  return(df)
}

# --- Predict SI --------------------------------------------------------------
predict_si <- function(df, rf_model) {
  # Check for missing variables
  missing <- setdiff(model_vars, names(df))
  if (length(missing) > 0) {
    cat(sprintf("  WARNING: %d missing model variables: %s\n",
                length(missing), paste(head(missing, 10), collapse = ", ")))
    # Set missing to NA
    for (m in missing) df[[m]] <- NA_real_
  }

  pred <- predict(rf_model, data = as.data.frame(df))
  return(pred$predictions)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================
cat("=== Future Site Index Projection (Delta Method) ===\n\n")

# Step 1: Get plot locations
plots <- get_maine_plots()

# Step 2: Extract current climate
plots <- extract_current_climate(plots)

# Step 3: Add ecoregions
plots <- add_ecoregions(plots)

# Step 4: Validate current SI prediction
plots_with_derived <- add_derived(copy(plots))
plots$SI_predicted_current <- predict_si(plots_with_derived, rf_model)

cat(sprintf("\nCurrent SI validation:\n"))
cat(sprintf("  Raster SI:    mean=%.1f, median=%.1f\n",
            mean(plots$SI_current, na.rm = TRUE),
            median(plots$SI_current, na.rm = TRUE)))
cat(sprintf("  Predicted SI:  mean=%.1f, median=%.1f\n",
            mean(plots$SI_predicted_current, na.rm = TRUE),
            median(plots$SI_predicted_current, na.rm = TRUE)))
cat(sprintf("  Correlation:   r=%.3f\n",
            cor(plots$SI_current, plots$SI_predicted_current, use = "complete")))

# Step 5: Project future SI for each scenario
results <- data.table(
  plot_id   = plots$plot_id,
  STATECD   = plots$STATECD,
  COUNTYCD  = plots$COUNTYCD,
  PLOT      = plots$PLOT,
  LAT       = plots$LAT,
  LON       = plots$LON,
  SI_current = plots$SI_current
)

for (scenario_name in names(DELTAS)) {
  cat(sprintf("\nProjecting %s...\n", scenario_name))
  delta_list <- DELTAS[[scenario_name]]

  future_plots <- apply_deltas(plots, delta_list)
  future_derived <- add_derived(future_plots)
  si_future <- predict_si(future_derived, rf_model)

  si_col <- paste0("SI_", scenario_name)
  ratio_col <- paste0("ratio_", scenario_name)

  results[[si_col]] <- si_future
  results[[ratio_col]] <- si_future / plots$SI_current

  cat(sprintf("  Mean SI: %.1f m (current: %.1f m)\n",
              mean(si_future, na.rm = TRUE),
              mean(plots$SI_current, na.rm = TRUE)))
  cat(sprintf("  Mean ratio: %.3f, Median: %.3f\n",
              mean(results[[ratio_col]], na.rm = TRUE),
              median(results[[ratio_col]], na.rm = TRUE)))
}

# Step 6: Save results
out_file <- file.path(OUTPUT_DIR, "fia_plot_si_future_delta.csv")
fwrite(results, out_file)
cat(sprintf("\nSaved: %s\n", out_file))

# Step 7: Summary table
cat("\n=== SI Change Summary ===\n")
cat(sprintf("%-20s %8s %8s %8s %8s\n",
            "Scenario", "Mean SI", "Median", "Mean Ratio", "Med Ratio"))
cat(paste(rep("=", 56), collapse = ""), "\n")

for (scenario_name in names(DELTAS)) {
  si_col <- paste0("SI_", scenario_name)
  ratio_col <- paste0("ratio_", scenario_name)
  cat(sprintf("%-20s %8.1f %8.1f %8.3f %8.3f\n",
              scenario_name,
              mean(results[[si_col]], na.rm = TRUE),
              median(results[[si_col]], na.rm = TRUE),
              mean(results[[ratio_col]], na.rm = TRUE),
              median(results[[ratio_col]], na.rm = TRUE)))
}

# Step 8: Create a simplified ratio table for PERSEUS integration
# This maps plot_id -> SI ratio for each scenario/period combination
ratio_cols <- grep("^ratio_", names(results), value = TRUE)
ratio_table <- results[, c("plot_id", "STATECD", "COUNTYCD", "PLOT",
                            "LAT", "LON", "SI_current", ratio_cols),
                        with = FALSE]

ratio_file <- file.path(OUTPUT_DIR, "perseus_si_ratios.csv")
fwrite(ratio_table, ratio_file)
cat(sprintf("\nSaved PERSEUS SI ratio table: %s\n", ratio_file))
cat(sprintf("  %d plots x %d scenario columns\n",
            nrow(ratio_table), length(ratio_cols)))

cat("\n=== Done ===\n")

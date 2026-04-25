# =============================================================================
# Title: Project Future Climate Site Index Rasters
# Author: A. Weiskittel
# Date: 2026-04-17
# Description: Generates site index rasters under HadGEM2-ES RCP 4.5 and 8.5
#              for 2030s (2011-2040), 2060s (2041-2070), and 2090s (2071-2100)
#              using the fitted ClimateNA+ecoregion ranger model.
#
# Prerequisites:
#   1. Current climate SI raster and ranger model:
#      ~/SiteIndex/rf_NASI_ClimateNA_ECO_model.rds
#      ~/SiteIndex/ClimateNA_ECO_SI_m .tif
#   2. Current climate rasters (already present):
#      ~/SiteIndex/rasters/ClimateNA/Normal_1991_2020_bioclim/
#   3. Future climate rasters (NEED TO DOWNLOAD):
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp45_2030s/
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp45_2060s/
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp45_2090s/
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp85_2030s/
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp85_2060s/
#      ~/SiteIndex/rasters/ClimateNA/HadGEM2-ES_rcp85_2090s/
#
# How to obtain future rasters:
#   Option A: Download from ClimateNA (https://climatena.ca)
#     - Download ClimateNA desktop application
#     - Select HadGEM2-ES, RCP 4.5 / 8.5
#     - Select periods: 2011-2040, 2041-2070, 2071-2100
#     - Export bioclim rasters covering Maine extent
#     - Upload to Cardinal
#
#   Option B: Use climr R package (CMIP6 equivalent, SSP245/SSP585)
#     install.packages("climr")
#     Then use climr::downscale() with GCM = "HadGEM3-GC31-LL"
#     (CMIP6 successor to HadGEM2-ES)
#
#   Option C: Extract point-level values for FIA plot locations only
#     (avoids full raster download, see extract_plot_si() below)
#
# Usage:
#   module load gcc/12.3.0 R/4.4.0 proj/9.2.1 gdal/3.7.3 geos/3.12.0
#   Rscript project_future_si.R
# =============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(ranger)
  library(parallel)
})

# --- Configuration -----------------------------------------------------------
# All paths are env driven. Override SITEINDEX_DIR, SI_OUTPUT_DIR, and
# SLURM_CPUS_PER_TASK as appropriate for your HPC environment.
BASE_DIR    <- Sys.getenv("SITEINDEX_DIR",
                           file.path(Sys.getenv("HOME"), "SiteIndex"))
RASTER_DIR  <- file.path(BASE_DIR, "rasters/ClimateNA")
MODEL_FILE  <- file.path(BASE_DIR, "rf_NASI_ClimateNA_ECO_model.rds")
ECO_SHP     <- file.path(BASE_DIR, "NA_Eco_L3_WGS84.shp")
CURRENT_SI  <- file.path(BASE_DIR, "ClimateNA_ECO_SI_m .tif")
OUTPUT_DIR  <- Sys.getenv("SI_OUTPUT_DIR", file.path(BASE_DIR, "future_SI"))
N_CORES     <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "20"))

# Future scenario definitions
SCENARIOS <- list(
  list(rcp = "rcp45", period = "2030s", subdir = "HadGEM2-ES_rcp45_2030s",
       prefix = "HadGEM2-ES_rcp45_2011-2040"),
  list(rcp = "rcp45", period = "2060s", subdir = "HadGEM2-ES_rcp45_2060s",
       prefix = "HadGEM2-ES_rcp45_2041-2070"),
  list(rcp = "rcp45", period = "2090s", subdir = "HadGEM2-ES_rcp45_2090s",
       prefix = "HadGEM2-ES_rcp45_2071-2100"),
  list(rcp = "rcp85", period = "2030s", subdir = "HadGEM2-ES_rcp85_2030s",
       prefix = "HadGEM2-ES_rcp85_2011-2040"),
  list(rcp = "rcp85", period = "2060s", subdir = "HadGEM2-ES_rcp85_2060s",
       prefix = "HadGEM2-ES_rcp85_2041-2070"),
  list(rcp = "rcp85", period = "2090s", subdir = "HadGEM2-ES_rcp85_2090s",
       prefix = "HadGEM2-ES_rcp85_2071-2100")
)

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Load ranger model -------------------------------------------------------
cat("Loading ranger RF model...\n")
rf_model <- readRDS(MODEL_FILE)
cat(sprintf("  Model has %d trees, %d variables\n",
            rf_model$num.trees, length(rf_model$forest$independent.variable.names)))
cat(sprintf("  Required variables: %s\n",
            paste(head(rf_model$forest$independent.variable.names, 20), collapse = ", ")))

# --- Prediction wrapper (matches Predict_CNA_SI.r) --------------------------
ranger_pred_fun <- function(model, data, ...) {
  pred <- predict(model, data = data)
  return(pred$predictions)
}

# --- Build climate stack with derived variables ------------------------------
build_climate_stack <- function(raster_dir, prefix_pattern) {
  # List all raster files
  raster_files <- list.files(
    path = raster_dir,
    pattern = "\\.(tif|asc)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(raster_files) == 0) {
    stop(sprintf("No raster files found in %s", raster_dir))
  }

  climate_stack <- rast(raster_files)

  # Strip prefix to get clean variable names
  raw_names <- gsub("\\.(tif|asc)$", "", basename(raster_files), ignore.case = TRUE)
  # Remove any prefix up to and including the last underscore before the variable
  # For current: "Normal_1991_2020_MAT" -> "MAT"
  # For future: "HadGEM2-ES_rcp45_2011-2040_MAT" -> "MAT"
  clean_names <- gsub(paste0("^", prefix_pattern, "_?"), "", raw_names)
  names(climate_stack) <- clean_names

  cat(sprintf("  Loaded %d raster layers: %s\n",
              nlyr(climate_stack), paste(clean_names, collapse = ", ")))

  # --- Rename ClimateNA variables to match ranger model expectations ---------
  # (mapping from Predict_CNA_SI.r)
  rename_map <- c(
    "bFFP" = "sday", "eFFP" = "fday", "FFP" = "ffp",
    "EMT" = "mmin", "EXT" = "mmax", "DD1040" = "d100",
    "MAP" = "map", "MAT" = "mat", "MCMT" = "mtcm",
    "MSP" = "gsp", "MWMT" = "mtwm", "DD_0" = "dd0"
  )

  for (from in names(rename_map)) {
    to <- rename_map[[from]]
    if (from %in% names(climate_stack)) {
      climate_stack[[to]] <- climate_stack[[from]]
    }
  }

  # --- Derived variables (matching Predict_CNA_SI.r) -------------------------
  climate_stack$tdiff  <- climate_stack$MWMT - climate_stack$MCMT
  climate_stack$pratio <- climate_stack$MSP / climate_stack$MAP

  climate_stack$adi <- (climate_stack$DD5^0.5) / climate_stack$MAP
  climate_stack$sdi <- (climate_stack$DD5^0.5) / climate_stack$MSP

  climate_stack$adimindd0 <- climate_stack$adi * climate_stack$DD_0
  climate_stack$sdimindd0 <- climate_stack$sdi * climate_stack$DD_0

  climate_stack$dd5mtcm   <- climate_stack$DD5 * climate_stack$MCMT
  climate_stack$gspdd5    <- climate_stack$MSP * climate_stack$DD5
  climate_stack$mapdd5    <- climate_stack$MAP * climate_stack$DD5
  climate_stack$dd5dd0    <- climate_stack$DD5 * climate_stack$DD_0
  climate_stack$mmindd0   <- climate_stack$EMT * climate_stack$DD_0
  climate_stack$mapdd0    <- climate_stack$MAP * climate_stack$DD_0

  # Additional interactions
  if ("PPT_sm" %in% names(climate_stack)) {
    climate_stack$smrp <- climate_stack$PPT_sm
  }
  if ("PPT_sp" %in% names(climate_stack)) {
    climate_stack$sprp <- climate_stack$PPT_sp
  }

  return(climate_stack)
}

# --- Add spatial layers (lat/lon, ecoregions) --------------------------------
add_spatial_layers <- function(climate_stack, eco_shp_path) {
  # Lat/lon layers
  xy <- crds(climate_stack, df = FALSE)
  lon_rast <- climate_stack[[1]]
  lat_rast <- climate_stack[[1]]
  values(lon_rast) <- NA
  values(lat_rast) <- NA
  xy_df <- crds(climate_stack)
  lon_rast[!is.na(climate_stack[[1]])] <- xy_df[!is.na(values(climate_stack[[1]])), 1]
  lat_rast[!is.na(climate_stack[[1]])] <- xy_df[!is.na(values(climate_stack[[1]])), 2]

  climate_stack$LON <- lon_rast
  climate_stack$LAT <- lat_rast
  climate_stack$LAT_LON_P <- climate_stack$LAT + climate_stack$LON
  climate_stack$LAT_LON_M <- climate_stack$LAT - climate_stack$LON

  # Ecoregion layers
  if (file.exists(eco_shp_path)) {
    cat("  Adding ecoregion layers...\n")
    eco <- vect(eco_shp_path)

    for (level in c("NA_L3CODE", "NA_L2CODE", "NA_L1CODE")) {
      if (level %in% names(eco)) {
        eco_rast <- rasterize(eco, climate_stack[[1]], field = level)
        names(eco_rast) <- level
        climate_stack <- c(climate_stack, eco_rast)
      }
    }
  } else {
    cat("  WARNING: Ecoregion shapefile not found, skipping.\n")
  }

  return(climate_stack)
}

# --- Main prediction loop ----------------------------------------------------
predict_si_raster <- function(scenario, rf_model, eco_shp_path, n_cores) {

  raster_dir <- file.path(RASTER_DIR, scenario$subdir)

  if (!dir.exists(raster_dir)) {
    cat(sprintf("\n  SKIPPING %s %s: raster directory not found at %s\n",
                scenario$rcp, scenario$period, raster_dir))
    cat("  Please download ClimateNA rasters for this scenario.\n")
    return(NULL)
  }

  cat(sprintf("\n=== Predicting SI for %s %s ===\n", scenario$rcp, scenario$period))

  # Build climate stack
  cat("  Building climate stack...\n")
  climate_stack <- build_climate_stack(raster_dir, scenario$prefix)

  # Add spatial layers
  climate_stack <- add_spatial_layers(climate_stack, eco_shp_path)

  # Predict
  cat(sprintf("  Predicting with %d cores...\n", n_cores))
  si_rast <- terra::predict(
    climate_stack, rf_model,
    fun = ranger_pred_fun,
    na.rm = TRUE,
    cores = n_cores
  )

  # Save
  out_file <- file.path(OUTPUT_DIR,
                        sprintf("SI_%s_%s_m.tif", scenario$rcp, scenario$period))
  writeRaster(si_rast, out_file, overwrite = TRUE,
              datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
  cat(sprintf("  Saved: %s\n", out_file))

  return(out_file)
}

# --- Compute SI change ratios ------------------------------------------------
compute_si_ratios <- function(current_si_path, future_si_path, out_path) {
  current <- rast(current_si_path)
  future  <- rast(future_si_path)

  ratio <- future / current
  ratio <- clamp(ratio, lower = 0.5, upper = 2.0)

  writeRaster(ratio, out_path, overwrite = TRUE,
              datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
  cat(sprintf("  SI ratio saved: %s\n", out_path))

  # Summary stats
  vals <- values(ratio, na.rm = TRUE)
  cat(sprintf("    Median ratio: %.3f, Mean: %.3f, Range: [%.3f, %.3f]\n",
              median(vals), mean(vals), min(vals), max(vals)))
  return(out_path)
}

# =============================================================================
# ALTERNATIVE: Point-based approach for FIA plots
# (Does not require full raster download)
# =============================================================================
extract_plot_si <- function(fia_plots_csv, current_si_path, rf_model) {
  # fia_plots_csv should have columns: plot_id, lat, lon, elev
  plots <- read.csv(fia_plots_csv)
  current_si <- rast(current_si_path)

  # Extract current SI at plot locations
  pts <- vect(plots, geom = c("lon", "lat"), crs = "EPSG:4326")
  plots$current_si <- extract(current_si, pts)[, 2]

  cat(sprintf("Extracted current SI for %d plots\n", nrow(plots)))
  cat(sprintf("  Mean SI: %.1f m, Range: [%.1f, %.1f]\n",
              mean(plots$current_si, na.rm = TRUE),
              min(plots$current_si, na.rm = TRUE),
              max(plots$current_si, na.rm = TRUE)))

  return(plots)
}

# =============================================================================
# EXECUTION
# =============================================================================
cat("=== Future Climate Site Index Projection ===\n")
cat(sprintf("Base directory: %s\n", BASE_DIR))
cat(sprintf("Output directory: %s\n", OUTPUT_DIR))
cat(sprintf("Cores: %d\n\n", N_CORES))

# Check which scenario rasters are available
available <- character(0)
missing   <- character(0)

for (s in SCENARIOS) {
  raster_dir <- file.path(RASTER_DIR, s$subdir)
  if (dir.exists(raster_dir) && length(list.files(raster_dir, pattern = "\\.tif$")) > 0) {
    available <- c(available, sprintf("%s_%s", s$rcp, s$period))
    cat(sprintf("  FOUND: %s\n", s$subdir))
  } else {
    missing <- c(missing, sprintf("%s_%s", s$rcp, s$period))
    cat(sprintf("  MISSING: %s\n", s$subdir))
  }
}

if (length(missing) > 0) {
  cat(sprintf("\n%d of %d future climate raster sets are missing.\n",
              length(missing), length(SCENARIOS)))
  cat("\nTo download from ClimateNA:\n")
  cat("  1. Visit https://climatena.ca and download the desktop app\n")
  cat("  2. For each missing period, request HadGEM2-ES bioclim rasters\n")
  cat("  3. Upload to Cardinal under ~/SiteIndex/rasters/ClimateNA/\n")
  cat("\nAlternatively, install the climr R package for programmatic access:\n")
  cat("  install.packages('climr')\n")
  cat("  # Then use climr::downscale() with HadGEM3-GC31-LL (CMIP6 equiv)\n\n")
}

# Process available scenarios
si_files <- list()
for (s in SCENARIOS) {
  result <- predict_si_raster(s, rf_model, ECO_SHP, N_CORES)
  if (!is.null(result)) {
    si_files[[sprintf("%s_%s", s$rcp, s$period)]] <- result
  }
}

# Compute change ratios for completed predictions
if (length(si_files) > 0 && file.exists(CURRENT_SI)) {
  cat("\n=== Computing SI change ratios (future / current) ===\n")
  for (name in names(si_files)) {
    ratio_path <- file.path(OUTPUT_DIR, sprintf("SI_ratio_%s.tif", name))
    compute_si_ratios(CURRENT_SI, si_files[[name]], ratio_path)
  }
}

cat("\n=== Done ===\n")

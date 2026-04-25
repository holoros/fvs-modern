# =============================================================================
# Title: Extract Plot-Level Future SI Using climr Package
# Author: A. Weiskittel
# Date: 2026-04-17
# Description: Alternative to full raster projection. Extracts ClimateNA
#              bioclimatic variables at FIA plot locations using the climr
#              package, then predicts SI using the fitted ranger model.
#              Produces per-plot SI values for current and future periods.
#
# This approach avoids downloading full rasters and works at the plot level,
# which is what PERSEUS ultimately needs for per-plot SI adjustments.
#
# Prerequisites:
#   1. Install climr: install.packages("climr")
#   2. ranger model: ~/SiteIndex/rf_NASI_ClimateNA_ECO_model.rds
#   3. FIA plot locations with lat/lon/elev
#
# Usage:
#   module load gcc/12.3.0 R/4.4.0 proj/9.2.1 gdal/3.7.3 geos/3.12.0
#   Rscript extract_plot_si_climr.R
# =============================================================================

suppressPackageStartupMessages({
  library(ranger)
  library(sf)
  library(data.table)
})

# --- Configuration -----------------------------------------------------------
# All paths are env driven; set SITEINDEX_DIR and FIA_DATA_DIR before running
BASE_DIR     <- Sys.getenv("SITEINDEX_DIR",
                            file.path(Sys.getenv("HOME"), "SiteIndex"))
MODEL_FILE   <- file.path(BASE_DIR, "rf_NASI_ClimateNA_ECO_model.rds")
ECO_SHP      <- file.path(BASE_DIR, "NA_Eco_L3_WGS84.shp")
FIA_DATA_DIR <- Sys.getenv("FIA_DATA_DIR",
                            file.path(Sys.getenv("HOME"), "fia_data"))
OUTPUT_DIR   <- Sys.getenv("SI_OUTPUT_DIR", file.path(BASE_DIR, "future_SI"))

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Load ranger model -------------------------------------------------------
cat("Loading ranger model...\n")
rf_model <- readRDS(MODEL_FILE)
model_vars <- rf_model$forest$independent.variable.names
cat(sprintf("  Model expects %d variables\n", length(model_vars)))

# --- Get FIA plot locations ---------------------------------------------------
get_fia_plots <- function(state = "ME") {
  # Try rFIA first, fall back to CSV
  if (requireNamespace("rFIA", quietly = TRUE)) {
    cat("  Loading FIA data via rFIA...\n")
    fia <- rFIA::readFIA(FIA_DATA_DIR, states = state, tables = c("PLOT", "COND"))
    plots <- fia$PLOT |>
      dplyr::filter(!is.na(LAT), !is.na(LON)) |>
      dplyr::distinct(STATECD, COUNTYCD, PLOT, .keep_all = TRUE) |>
      dplyr::select(STATECD, COUNTYCD, PLOT, LAT, LON, ELEV)
  } else {
    cat("  Loading FIA PLOT table from CSV...\n")
    plot_file <- file.path(FIA_DATA_DIR, paste0(state, "_PLOT.csv"))
    if (!file.exists(plot_file)) {
      plot_file <- file.path(FIA_DATA_DIR, "PLOT.csv")
    }
    plots <- fread(plot_file)
    plots <- plots[!is.na(LAT) & !is.na(LON),
                   .(STATECD, COUNTYCD, PLOT, LAT, LON, ELEV)]
    plots <- unique(plots, by = c("STATECD", "COUNTYCD", "PLOT"))
  }

  cat(sprintf("  Found %d unique plot locations\n", nrow(plots)))
  return(as.data.frame(plots))
}

# --- Add ecoregion codes to plots --------------------------------------------
add_ecoregions <- function(plots, eco_shp_path) {
  if (!file.exists(eco_shp_path)) {
    cat("  Ecoregion shapefile not found, skipping.\n")
    return(plots)
  }

  cat("  Assigning ecoregion codes...\n")
  eco <- st_read(eco_shp_path, quiet = TRUE)

  plots_sf <- st_as_sf(plots, coords = c("LON", "LAT"), crs = 4326)
  eco <- st_transform(eco, 4326)

  joined <- st_join(plots_sf, eco[, c("NA_L3CODE", "NA_L2CODE", "NA_L1CODE")])
  plots$NA_L3CODE <- joined$NA_L3CODE
  plots$NA_L2CODE <- joined$NA_L2CODE
  plots$NA_L1CODE <- joined$NA_L1CODE

  return(plots)
}

# --- Compute derived variables from ClimateNA output -------------------------
compute_derived <- function(df) {
  # These match the Predict_CNA_SI.r derived variable calculations
  df$tdiff  <- df$MWMT - df$MCMT
  df$pratio <- df$MSP / df$MAP
  df$adi    <- sqrt(df$DD5) / df$MAP
  df$sdi    <- sqrt(df$DD5) / df$MSP

  # Interactions with cold stress
  df$adimindd0 <- df$adi * df$DD_0
  df$sdimindd0 <- df$sdi * df$DD_0
  df$dd5mtcm   <- df$DD5 * df$MCMT
  df$gspdd5    <- df$MSP * df$DD5
  df$mapdd5    <- df$MAP * df$DD5
  df$dd5dd0    <- df$DD5 * df$DD_0
  df$mmindd0   <- df$EMT * df$DD_0
  df$mapdd0    <- df$MAP * df$DD_0

  # Renamed aliases
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

  # Spatial
  df$LAT_LON_P <- df$LAT + df$LON
  df$LAT_LON_M <- df$LAT - df$LON

  return(df)
}

# --- Predict SI at plots using climr ----------------------------------------
predict_si_climr <- function(plots, rf_model, periods, gcms, ssps) {

  if (!requireNamespace("climr", quietly = TRUE)) {
    stop("climr package not installed. Install with: install.packages('climr')")
  }

  library(climr)

  results <- list()

  # Current climate normals
  cat("Fetching current climate normals (1991-2020)...\n")
  current <- climr::downscale(
    xyz = data.frame(
      lon = plots$LON,
      lat = plots$LAT,
      elev = plots$ELEV,
      id = seq_len(nrow(plots))
    ),
    which_normal = "normal_na",
    return_normal = TRUE,
    vars = c("MAT", "MWMT", "MCMT", "MAP", "MSP", "DD5", "DD_0",
             "EMT", "EXT", "bFFP", "eFFP", "FFP", "DD1040",
             "AHM", "CMD", "CMI", "Eref", "NFFD", "PAS", "RH", "SHM", "TD")
  )

  # Compute derived and predict
  current_df <- merge(plots, current, by.x = "row_id", by.y = "id")
  current_df <- compute_derived(current_df)
  current_df$si_current <- predict(rf_model, data = current_df)$predictions

  results[["current"]] <- current_df[, c("STATECD","COUNTYCD","PLOT","LAT","LON",
                                          "ELEV","si_current")]

  # Future periods
  for (period in periods) {
    for (gcm in gcms) {
      for (ssp in ssps) {
        label <- sprintf("%s_%s_%s", gcm, ssp, period)
        cat(sprintf("Fetching %s...\n", label))

        future <- tryCatch(
          climr::downscale(
            xyz = data.frame(
              lon = plots$LON,
              lat = plots$LAT,
              elev = plots$ELEV,
              id = seq_len(nrow(plots))
            ),
            which_normal = "normal_na",
            gcm_models = gcm,
            ssp = ssp,
            gcm_period = period,
            vars = c("MAT", "MWMT", "MCMT", "MAP", "MSP", "DD5", "DD_0",
                     "EMT", "EXT", "bFFP", "eFFP", "FFP", "DD1040",
                     "AHM", "CMD", "CMI", "Eref", "NFFD", "PAS", "RH", "SHM", "TD")
          ),
          error = function(e) {
            cat(sprintf("  ERROR fetching %s: %s\n", label, e$message))
            return(NULL)
          }
        )

        if (!is.null(future)) {
          future_df <- merge(plots, future, by.x = "row_id", by.y = "id")
          future_df <- compute_derived(future_df)
          si_col <- sprintf("si_%s", label)
          future_df[[si_col]] <- predict(rf_model, data = future_df)$predictions

          ratio_col <- sprintf("ratio_%s", label)
          future_df[[ratio_col]] <- future_df[[si_col]] / results[["current"]]$si_current

          results[[label]] <- future_df[, c("STATECD","COUNTYCD","PLOT",
                                             si_col, ratio_col)]
        }
      }
    }
  }

  # Merge all results
  final <- results[["current"]]
  for (name in names(results)) {
    if (name != "current") {
      final <- merge(final, results[[name]],
                     by = c("STATECD","COUNTYCD","PLOT"))
    }
  }

  return(final)
}

# =============================================================================
# EXECUTION
# =============================================================================
cat("=== Plot-Level Future SI Extraction ===\n\n")

# Get plots
plots <- get_fia_plots("ME")
plots <- add_ecoregions(plots, ECO_SHP)

# Check if climr is available
if (requireNamespace("climr", quietly = TRUE)) {
  cat("climr package found. Proceeding with programmatic climate data access.\n\n")

  si_table <- predict_si_climr(
    plots, rf_model,
    periods = c("2011_2040", "2041_2070", "2071_2100"),
    # HadGEM3-GC31-LL is CMIP6 successor to HadGEM2-ES
    gcms = c("HadGEM3-GC31-LL"),
    ssps = c("ssp245", "ssp585")  # Approx RCP 4.5 / 8.5
  )

  out_file <- file.path(OUTPUT_DIR, "fia_plot_si_future.csv")
  write.csv(si_table, out_file, row.names = FALSE)
  cat(sprintf("\nSaved plot-level SI table: %s\n", out_file))

} else {
  cat("climr package NOT found.\n")
  cat("To install: Rscript -e 'install.packages(\"climr\")'\n\n")
  cat("Alternative: use the raster-based approach (project_future_si.R)\n")
  cat("after downloading ClimateNA future rasters manually.\n\n")

  # For now, extract current SI from existing raster as a starting point
  if (requireNamespace("terra", quietly = TRUE)) {
    library(terra)
    current_si_path <- file.path(BASE_DIR, "ClimateNA_ECO_SI_m .tif")
    if (file.exists(current_si_path)) {
      cat("Extracting current SI at plot locations from existing raster...\n")
      si_rast <- rast(current_si_path)
      pts <- vect(plots, geom = c("LON", "LAT"), crs = "EPSG:4326")
      plots$si_current <- terra::extract(si_rast, pts)[, 2]

      out_file <- file.path(OUTPUT_DIR, "fia_plot_si_current.csv")
      write.csv(plots, out_file, row.names = FALSE)
      cat(sprintf("Saved current SI at %d plots: %s\n", nrow(plots), out_file))
      cat(sprintf("  Mean SI: %.1f m, Median: %.1f m\n",
                  mean(plots$si_current, na.rm = TRUE),
                  median(plots$si_current, na.rm = TRUE)))
    }
  }
}

cat("\n=== Done ===\n")

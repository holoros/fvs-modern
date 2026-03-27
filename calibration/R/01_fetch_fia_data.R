#!/usr/bin/env Rscript
# =============================================================================
# FVS Bayesian Calibration: FIA Data Fetch and Preparation
#
# Downloads or reads locally stored FIA data and prepares remeasurement pairs
# for calibrating tree level models (diameter growth, height, mortality, crown).
#
# Supports two modes:
#   1. Download via rFIA (default, for local workstation use)
#   2. Local FIA directory (for HPC with pre downloaded ENTIRE_FIA.zip)
#
# Usage:
#   Rscript calibration/R/01_fetch_fia_data.R --variant ne
#   Rscript calibration/R/01_fetch_fia_data.R --variant ne --fia-dir /path/to/fia
# =============================================================================

library(rFIA)
library(tidyverse)
library(data.table)
library(jsonlite)
library(logger)

# =============================================================================
# Parse Command Line Arguments
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
variant <- "ne"
fia_dir <- NULL

if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] == "--variant" & i < length(args)) {
      variant <- args[i + 1]
    }
    if (args[i] == "--fia-dir" & i < length(args)) {
      fia_dir <- args[i + 1]
    }
  }
}

# =============================================================================
# Configuration
# =============================================================================

project_root <- Sys.getenv("FVS_PROJECT_ROOT",
                           "/home/aweiskittel/Documents/Claude/fvs-modern")
calibration_dir <- file.path(project_root, "calibration")
processed_dir <- file.path(calibration_dir, "data", "processed", variant)
config_dir <- file.path(project_root, "config")

# If fia_dir not passed as arg, check environment variable
if (is.null(fia_dir)) {
  env_fia <- Sys.getenv("FVS_FIA_DATA_DIR", "")
  if (nchar(env_fia) > 0) fia_dir <- env_fia
}

dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(calibration_dir, "logs",
                      paste0("01_fetch_fia_data_", variant, ".log"))
dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
logger::log_appender(logger::appender_file(log_file), index = 1)
logger::log_info("Starting FIA data fetch for variant {variant}")
if (!is.null(fia_dir)) {
  logger::log_info("Using local FIA directory: {fia_dir}")
}

# =============================================================================
# FVS Variant to State Mapping
#
# Used to determine which state level FIA files to load. This is a rough
# geographic filter for efficiency; the precise variant assignment comes
# from the FIA PLOT table's variant column (preferred) or falls back to
# this mapping when that column is absent in older FIA vintages.
# =============================================================================

variant_states <- list(
  ne  = c("ME", "NH", "VT", "MA", "CT", "RI", "NY", "PA", "NJ", "DE", "MD", "WV"),
  ls  = c("MI", "MN", "WI"),
  cs  = c("IN", "IL", "IA", "MO", "OH", "KY"),
  sn  = c("VA", "NC", "SC", "GA", "FL", "AL", "MS", "TN", "AR", "LA", "TX", "OK"),
  ca  = c("CA"),
  oc  = c("CA"),
  pn  = c("WA", "OR"),
  wc  = c("WA", "OR"),
  nc  = c("WA", "OR"),
  ie  = c("ID", "MT"),
  ci  = c("ID", "MT"),
  kt  = c("MT"),
  em  = c("ID", "MT", "WY", "CO", "UT", "NV", "AZ", "NM"),
  cr  = c("CO", "AZ", "NM"),
  so  = c("OR", "CA"),
  bm  = c("OR"),
  ec  = c("WA", "OR"),
  tt  = c("UT", "NV"),
  ut  = c("UT", "NV"),
  ws  = c("CA", "OR", "WA"),
  op  = c("OR"),
  ak  = c("AK"),
  acd = c("ME", "NH", "VT"),
  bc  = c("ME"),   # Placeholder: BC uses Canadian NFI data
  on  = c("ME")    # Placeholder: ON uses Canadian NFI data
)

# FVS variant codes as they appear in FIA (uppercase two letter)
variant_upper <- toupper(variant)

states <- variant_states[[variant]]
if (is.null(states)) {
  logger::log_error("Unknown variant: {variant}")
  stop("Unknown variant: ", variant)
}

logger::log_info("Variant {variant} maps to states: {paste(states, collapse = ', ')}")
logger::log_info("Will filter on FIA variant column = '{variant_upper}' when available")

# =============================================================================
# Load FVS Config for Species Mapping and Bark Ratios
# =============================================================================

config_file <- file.path(config_dir, paste0(variant, ".json"))
if (!file.exists(config_file)) {
  logger::log_error("Config file not found: {config_file}")
  stop("Config file not found: ", config_file)
}

config <- fromJSON(config_file)

# Extract species definitions
fia_species_codes <- unlist(config$categories$species_definitions$FIAJSP)
n_species <- config$maxsp

# Bark ratio parameters (species indexed)
bkrat_params <- config$categories$bark_ratio$BKRAT
avg_bark_ratio <- mean(bkrat_params, na.rm = TRUE)

logger::log_info("Config: {n_species} species, avg bark ratio = {round(avg_bark_ratio, 3)}")

# =============================================================================
# Load FIA Data (Download or Read Local)
# =============================================================================

logger::log_info("Loading FIA data for {length(states)} states...")

# Determine FIA source directory
if (!is.null(fia_dir)) {
  # Local mode: ENTIRE_FIA.zip was unzipped to fia_dir
  # Expect state subdirectories like fia_dir/ME/, fia_dir/NH/, etc.
  # OR flat structure with state prefix files like ME_TREE.csv, ME_PLOT.csv
  fia_source_dir <- fia_dir
  logger::log_info("Reading from local FIA directory: {fia_source_dir}")
} else {
  # Download mode: use rFIA to fetch data

  fia_source_dir <- file.path(calibration_dir, "data", "fia_raw")
  dir.create(fia_source_dir, showWarnings = FALSE, recursive = TRUE)
  logger::log_info("Download mode: FIA data cached in {fia_source_dir}")
}

# Process each state
all_tree_data <- list()
all_cond_data <- list()
all_plot_data <- list()

for (state in states) {
  logger::log_info("Processing state: {state}")

  tryCatch({
    if (!is.null(fia_dir)) {
      # -----------------------------------------------------------------------
      # Local FIA mode
      # -----------------------------------------------------------------------
      # Check for state subdirectory first, then flat structure
      state_subdir <- file.path(fia_source_dir, state)

      if (dir.exists(state_subdir) && length(list.files(state_subdir)) > 0) {
        db <- readFIA(dir = state_subdir, common = TRUE)
      } else {
        # Try reading from the main directory (flat ENTIRE_FIA structure)
        db <- readFIA(dir = fia_source_dir, states = state, common = TRUE)
      }
    } else {
      # -----------------------------------------------------------------------
      # Download mode
      # -----------------------------------------------------------------------
      state_dir <- file.path(fia_source_dir, state)
      if (!dir.exists(state_dir) || length(list.files(state_dir)) == 0) {
        logger::log_info("Downloading FIA data for {state}...")
        getFIA(states = state, dir = fia_source_dir, load = FALSE)
      }
      db <- readFIA(dir = file.path(fia_source_dir, state), common = TRUE)
    }

    # Extract TREE table
    tree_df <- db$TREE %>%
      as_tibble() %>%
      filter(
        STATUSCD %in% c(1, 2),     # Live and recently dead
        DIA >= 1.0,                 # Minimum 1 inch DBH
        !is.na(TPA_UNADJ),
        TPA_UNADJ > 0
      ) %>%
      select(
        CN, PLT_CN, CONDID, SUBP, TREE, INVYR, CYCLE, SUBCYCLE,
        STATUSCD, SPCD, DIA, HT, CR, ACTUALHT,
        TPA_UNADJ, BHAGE, TOTAGE, PREV_TRE_CN
      ) %>%
      mutate(state = state)

    # Extract COND table
    cond_df <- db$COND %>%
      as_tibble() %>%
      select(
        PLT_CN, CONDID, INVYR,
        FORTYPCD, FLDTYPCD, STDAGE, STDSZCD, STDORGCD,
        SITECLCD, SICOND, SISP, SLOPE, ASPECT,
        BALIVE, LIVE_CANOPY_CVR_PCT
      )

    # Extract PLOT table (grab variant column if it exists)
    plot_raw <- db$PLOT %>% as_tibble()
    plot_cols <- c("CN", "INVYR", "LAT", "LON", "ELEV", "MEASYEAR")

    # FIA databases now include an FVS variant assignment per plot
    # Column names vary: VARIANT, FVS_VARIANT, FVSVARIANT, FVSVARIANTCD
    var_col <- intersect(
      names(plot_raw),
      c("VARIANT", "FVS_VARIANT", "FVSVARIANT", "FVSVARIANTCD",
        "variant", "fvs_variant")
    )
    if (length(var_col) > 0) {
      plot_cols <- c(plot_cols, var_col[1])
      logger::log_info("Found FIA variant column: {var_col[1]}")
    }

    plot_df <- plot_raw %>%
      select(any_of(plot_cols)) %>%
      rename(PLT_CN = CN)

    # Standardize variant column name to FIA_VARIANT
    if (length(var_col) > 0) {
      plot_df <- plot_df %>% rename(FIA_VARIANT = !!sym(var_col[1]))
    }

    all_tree_data[[state]] <- tree_df
    all_cond_data[[state]] <- cond_df
    all_plot_data[[state]] <- plot_df

    logger::log_info("State {state}: {nrow(tree_df)} tree records loaded")

  }, error = function(e) {
    logger::log_error("Failed to process state {state}: {e$message}")
  })
}

# Combine across states
tree_all <- bind_rows(all_tree_data)
cond_all <- bind_rows(all_cond_data)
plot_all <- bind_rows(all_plot_data)

logger::log_info("Combined: {nrow(tree_all)} tree records across {length(all_tree_data)} states")

# =============================================================================
# Join Tree + Condition + Plot
# =============================================================================

combined <- tree_all %>%
  left_join(cond_all, by = c("PLT_CN", "CONDID", "INVYR")) %>%
  left_join(plot_all, by = c("PLT_CN", "INVYR"))

# =============================================================================
# Filter to Plots Assigned to This FVS Variant
#
# If the FIA data contains a variant column (modern FIA databases do),
# use it for precise filtering. This is more accurate than the state
# mapping since variant boundaries do not follow state lines in the
# western US (e.g., Idaho contains both IE and CI plots).
# =============================================================================

has_fia_variant <- "FIA_VARIANT" %in% names(combined)

if (has_fia_variant) {
  n_before_filter <- nrow(combined)

  # Standardize: strip whitespace and compare case insensitively
  combined <- combined %>%
    mutate(FIA_VARIANT = trimws(toupper(FIA_VARIANT))) %>%
    filter(FIA_VARIANT == variant_upper)

  logger::log_info(
    "Filtered by FIA variant column: {n_before_filter} -> {nrow(combined)} tree records for {variant_upper}"
  )

  if (nrow(combined) == 0) {
    logger::log_error("No plots found with FIA_VARIANT == '{variant_upper}'")
    stop("No FIA plots matched variant ", variant_upper)
  }
} else {
  logger::log_warn(
    "No FIA variant column found in PLOT table; using state mapping only. ",
    "This is less precise for variants that share states (e.g., IE/CI in Idaho)."
  )
}

# =============================================================================
# Build Remeasurement Pairs (Same Tree, Two Inventories)
# =============================================================================

logger::log_info("Building remeasurement pairs from PREV_TRE_CN linkages...")

# Method 1: Use PREV_TRE_CN for exact tree linkage (preferred)
trees_with_prev <- combined %>%
  filter(!is.na(PREV_TRE_CN), PREV_TRE_CN > 0)

# Match current tree to its previous measurement
prev_trees <- combined %>%
  select(CN, PLT_CN, CONDID, INVYR, STATUSCD, SPCD, DIA, HT, CR,
         TPA_UNADJ, SLOPE, ASPECT, SICOND, SITECLCD, BALIVE,
         LAT, LON, ELEV, MEASYEAR) %>%
  rename_with(~ paste0(.x, "_t1"), .cols = c(INVYR, STATUSCD, DIA, HT, CR,
                                              TPA_UNADJ, MEASYEAR))

remeas <- trees_with_prev %>%
  rename(
    INVYR_t2 = INVYR,
    STATUSCD_t2 = STATUSCD,
    DIA_t2 = DIA,
    HT_t2 = HT,
    CR_t2 = CR,
    TPA_UNADJ_t2 = TPA_UNADJ,
    MEASYEAR_t2 = MEASYEAR
  ) %>%
  left_join(
    prev_trees,
    by = c("PREV_TRE_CN" = "CN")
  ) %>%
  filter(
    !is.na(DIA_t1), !is.na(DIA_t2),
    DIA_t1 > 0, DIA_t2 > 0
  )

# Compute measurement interval
remeas <- remeas %>%
  mutate(
    years_interval = INVYR_t2 - INVYR_t1
  ) %>%
  filter(
    years_interval > 0,
    years_interval <= 15
  )

logger::log_info("Found {nrow(remeas)} remeasurement pairs via PREV_TRE_CN")

# If PREV_TRE_CN yields too few pairs, fall back to tree_id matching
if (nrow(remeas) < 100) {
  logger::log_warn("Few PREV_TRE_CN pairs; falling back to tree_id matching")

  combined <- combined %>%
    mutate(tree_id = paste(PLT_CN, SUBP, TREE, sep = "_"))

  remeas <- combined %>%
    filter(STATUSCD == 1) %>%
    group_by(tree_id) %>%
    filter(n() >= 2) %>%
    arrange(INVYR) %>%
    mutate(
      DIA_t1 = DIA,
      DIA_t2 = lead(DIA),
      HT_t1 = HT,
      HT_t2 = lead(HT),
      CR_t1 = CR,
      CR_t2 = lead(CR),
      INVYR_t1 = INVYR,
      INVYR_t2 = lead(INVYR),
      STATUSCD_t1 = STATUSCD,
      STATUSCD_t2 = lead(STATUSCD),
      years_interval = lead(INVYR) - INVYR
    ) %>%
    filter(
      !is.na(DIA_t2),
      DIA_t1 > 0, DIA_t2 > 0,
      years_interval > 0, years_interval <= 15
    ) %>%
    ungroup()

  logger::log_info("Fallback yielded {nrow(remeas)} remeasurement pairs")
}

# =============================================================================
# Compute Inside Bark Diameters and Diameter Growth
# =============================================================================

# Map FIA species codes to FVS species indices for bark ratio lookup
fia_to_fvs <- setNames(seq_along(fia_species_codes), fia_species_codes)

remeas <- remeas %>%
  mutate(
    # Map species to FVS index (use 1 as fallback for unknown species)
    fvs_sp_idx = as.integer(fia_to_fvs[as.character(SPCD)]),
    fvs_sp_idx = ifelse(is.na(fvs_sp_idx), 1L, fvs_sp_idx),

    # Species specific bark ratio where available, otherwise average
    bark_ratio = ifelse(
      fvs_sp_idx <= length(bkrat_params) & bkrat_params[fvs_sp_idx] > 0,
      bkrat_params[fvs_sp_idx],
      avg_bark_ratio
    ),

    # Inside bark diameters
    DIB_t1 = DIA_t1 * bark_ratio,
    DIB_t2 = DIA_t2 * bark_ratio,

    # Squared diameter increment (FVS Wykoff model predicts ln(DDS))
    DDS = DIB_t2^2 - DIB_t1^2,

    # Annual height increment
    HT_annual = ifelse(
      !is.na(HT_t1) & !is.na(HT_t2) & HT_t2 > HT_t1,
      (HT_t2 - HT_t1) / years_interval,
      NA_real_
    ),

    # Annual crown ratio change
    CR_annual = ifelse(
      !is.na(CR_t1) & !is.na(CR_t2),
      (CR_t2 - CR_t1) / years_interval,
      NA_real_
    )
  )

# =============================================================================
# Compute Stand Level Covariates (BA, BAL, SDI, etc.)
# =============================================================================

logger::log_info("Computing stand level covariates...")

# Stand metrics from live trees at time 1
stand_metrics <- combined %>%
  filter(STATUSCD == 1, !is.na(DIA), DIA > 0) %>%
  group_by(PLT_CN, CONDID, INVYR) %>%
  summarise(
    BA_ft2ac = sum(TPA_UNADJ * 0.005454 * DIA^2, na.rm = TRUE),
    TPA = sum(TPA_UNADJ, na.rm = TRUE),
    QMD = sqrt(BA_ft2ac / TPA / 0.005454),
    SDI = TPA * (QMD / 10)^1.605,
    top_ht = {
      ht_sorted <- sort(HT[!is.na(HT)], decreasing = TRUE)
      if (length(ht_sorted) >= 5) mean(ht_sorted[1:min(5, length(ht_sorted))])
      else if (length(ht_sorted) > 0) mean(ht_sorted)
      else NA_real_
    },
    .groups = "drop"
  )

# Basal Area in Larger trees (BAL) for each tree
bal_data <- combined %>%
  filter(STATUSCD == 1, !is.na(DIA), DIA > 0) %>%
  group_by(PLT_CN, CONDID, INVYR) %>%
  arrange(desc(DIA)) %>%
  mutate(
    tree_ba = TPA_UNADJ * 0.005454 * DIA^2,
    BAL_ft2ac = cumsum(tree_ba) - tree_ba  # BA of trees larger than this one
  ) %>%
  select(PLT_CN, CONDID, INVYR, SUBP, TREE, SPCD, BAL_ft2ac) %>%
  ungroup()

# Site index: use SICOND from FIA condition table where available
site_index <- cond_all %>%
  filter(!is.na(SICOND), SICOND > 0) %>%
  distinct(PLT_CN, CONDID, INVYR, SICOND)

# =============================================================================
# Join Covariates to Remeasurement Pairs
# =============================================================================

# Use time 1 inventory year for covariate matching
remeas <- remeas %>%
  left_join(
    stand_metrics %>% rename(BA = BA_ft2ac),
    by = c("PLT_CN.y" = "PLT_CN", "CONDID" = "CONDID", "INVYR_t1" = "INVYR")
  ) %>%
  left_join(
    bal_data,
    by = c("PLT_CN.x" = "PLT_CN", "CONDID" = "CONDID",
           "INVYR_t2" = "INVYR", "SUBP" = "SUBP", "TREE" = "TREE", "SPCD" = "SPCD")
  ) %>%
  left_join(
    site_index,
    by = c("PLT_CN.y" = "PLT_CN", "CONDID" = "CONDID", "INVYR_t1" = "INVYR")
  ) %>%
  mutate(
    # Use SICOND as site index; fall back to top_ht if missing
    SI = coalesce(SICOND, top_ht),
    # Use BALIVE from condition table if BAL computation failed
    BAL = coalesce(BAL_ft2ac, BALIVE)
  )

# =============================================================================
# Compute Derived Model Variables
# =============================================================================

remeas <- remeas %>%
  mutate(
    # Slope / aspect transformations
    ASPECT_rad = ASPECT * pi / 180,
    SLOPE_SASP = SLOPE * sin(ASPECT_rad),
    SLOPE_CASP = SLOPE * cos(ASPECT_rad),
    ELEV_100ft = ELEV / 100,

    # Logarithmic transforms for Wykoff model
    ln_DBH = log(DIA_t1),
    DBH_sq = DIA_t1^2,
    ln_SI = log(pmax(SI, 1)),
    CR_pct = CR_t1 * 100,  # FIA stores 0 to 1, some models want 0 to 100

    # Annual DDS
    DDS_annual = DDS / years_interval,
    ln_DDS = log(pmax(DDS, 0.001))
  )

# =============================================================================
# Quality Filters
# =============================================================================

logger::log_info("Applying quality filters...")

n_before <- nrow(remeas)

remeas_clean <- remeas %>%
  filter(
    !is.na(DDS), DDS > 0,
    !is.na(DIA_t1), DIA_t1 >= 1.0,
    !is.na(SI), SI > 0,
    !is.na(BA), BA > 0,
    !is.na(SLOPE),
    !is.na(ELEV)
  )

logger::log_info("Quality filter: {n_before} -> {nrow(remeas_clean)} observations ({round(100 * nrow(remeas_clean)/n_before, 1)}% retained)")

# =============================================================================
# Save Diameter Growth Dataset
# =============================================================================

dg_out <- remeas_clean %>%
  transmute(
    PLT_CN = coalesce(PLT_CN.x, PLT_CN.y),
    tree_id = paste(PLT_CN, SUBP, TREE, sep = "_"),
    SPCD, fvs_sp_idx, variant = !!variant,
    # Measurements
    DIA_t1, DIA_t2, DIB_t1, DIB_t2, DDS, ln_DDS,
    HT_t1, HT_t2, HT_annual,
    CR_t1, CR_t2, CR_annual,
    years_interval,
    STATUSCD_t1, STATUSCD_t2,
    # Covariates
    ln_DBH, DBH_sq, ln_SI, SI,
    SLOPE, SLOPE_SASP, SLOPE_CASP,
    ELEV, ELEV_100ft,
    CR_pct, BAL, BA, SDI, QMD, TPA,
    # Location
    LAT, LON
  )

dg_file <- file.path(processed_dir, "diameter_growth.csv")
write_csv(dg_out, dg_file)
logger::log_info("Saved diameter growth data: {nrow(dg_out)} obs to {dg_file}")

# =============================================================================
# Save Height Growth Dataset (trees with valid height remeasurements)
# =============================================================================

ht_out <- remeas_clean %>%
  filter(!is.na(HT_t1), !is.na(HT_t2), HT_t1 > 4.5, HT_t2 > HT_t1) %>%
  transmute(
    PLT_CN = coalesce(PLT_CN.x, PLT_CN.y),
    tree_id = paste(PLT_CN, SUBP, TREE, sep = "_"),
    SPCD, fvs_sp_idx, variant = !!variant,
    DIA_t1, HT_t1, HT_t2, HT_annual,
    years_interval,
    ln_DBH, SI, BA, BAL, CR_pct,
    SLOPE, ELEV, LAT, LON
  )

ht_file <- file.path(processed_dir, "height_growth.csv")
write_csv(ht_out, ht_file)
logger::log_info("Saved height growth data: {nrow(ht_out)} obs to {ht_file}")

# =============================================================================
# Save Height Diameter Dataset (all live trees with HT and DIA)
# =============================================================================

hd_out <- combined %>%
  filter(STATUSCD == 1, !is.na(DIA), DIA >= 1.0, !is.na(HT), HT > 4.5) %>%
  left_join(site_index, by = c("PLT_CN", "CONDID", "INVYR")) %>%
  transmute(
    PLT_CN, CONDID, INVYR,
    SPCD,
    fvs_sp_idx = as.integer(fia_to_fvs[as.character(SPCD)]),
    fvs_sp_idx = ifelse(is.na(fvs_sp_idx), 1L, fvs_sp_idx),
    variant = !!variant,
    DIA, HT,
    SI = coalesce(SICOND, 50),  # Default SI = 50 if missing
    LAT, LON, ELEV
  )

hd_file <- file.path(processed_dir, "height_diameter.csv")
write_csv(hd_out, hd_file)
logger::log_info("Saved height diameter data: {nrow(hd_out)} obs to {hd_file}")

# =============================================================================
# Save Mortality Dataset (live trees + trees that died between measurements)
# =============================================================================

mort_out <- remeas %>%
  filter(
    STATUSCD_t1 == 1,          # Alive at time 1
    !is.na(DIA_t1), DIA_t1 >= 1.0,
    !is.na(SI), SI > 0,
    !is.na(BA), BA > 0,
    years_interval > 0
  ) %>%
  transmute(
    PLT_CN = coalesce(PLT_CN.x, PLT_CN.y),
    tree_id = paste(PLT_CN, SUBP, TREE, sep = "_"),
    SPCD, fvs_sp_idx, variant = !!variant,
    DIA = DIA_t1, HT = HT_t1, CR = CR_t1,
    died = as.integer(STATUSCD_t2 == 2),  # 1 = died, 0 = survived
    years_interval,
    ln_DBH = log(DIA_t1), SI, BA, BAL,
    CR_pct = CR_t1 * 100,
    SLOPE, ELEV, LAT, LON
  )

mort_file <- file.path(processed_dir, "mortality.csv")
write_csv(mort_out, mort_file)
logger::log_info("Saved mortality data: {nrow(mort_out)} obs ({sum(mort_out$died)} deaths)")

# =============================================================================
# Save Crown Ratio Change Dataset
# =============================================================================

cr_out <- remeas_clean %>%
  filter(!is.na(CR_t1), !is.na(CR_t2), CR_t1 > 0) %>%
  transmute(
    PLT_CN = coalesce(PLT_CN.x, PLT_CN.y),
    tree_id = paste(PLT_CN, SUBP, TREE, sep = "_"),
    SPCD, fvs_sp_idx, variant = !!variant,
    DIA = DIA_t1, DIA_sq = DIA_t1^2,
    CR_init = CR_t1, CR_final = CR_t2,
    delta_CR = CR_annual,
    years_interval,
    SI, BA, BAL,
    SLOPE, ELEV, LAT, LON
  )

cr_file <- file.path(processed_dir, "crown_ratio_change.csv")
write_csv(cr_out, cr_file)
logger::log_info("Saved crown ratio change data: {nrow(cr_out)} obs to {cr_file}")

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("==========================================\n")
cat("FIA Data Preparation Complete\n")
cat("==========================================\n")
cat("Variant:", variant, "\n")
cat("States:", paste(states, collapse = ", "), "\n")
cat("FIA source:", ifelse(!is.null(fia_dir), fia_dir, "rFIA download"), "\n")
cat("\nDatasets created:\n")
cat("  Diameter growth:", nrow(dg_out), "observations\n")
cat("  Height growth:  ", nrow(ht_out), "observations\n")
cat("  Height diameter:", nrow(hd_out), "observations\n")
cat("  Mortality:      ", nrow(mort_out), "observations",
    "(", sum(mort_out$died), "deaths )\n")
cat("  Crown ratio:    ", nrow(cr_out), "observations\n")
cat("\nOutput directory:", processed_dir, "\n\n")

logger::log_info("FIA data preparation complete for variant {variant}")

#!/usr/bin/env Rscript
# =============================================================================
# Title: NSBE/VTECO Volume and Biomass Equations for FVS Calibration
# Author: A. Weiskittel
# Date: 2026-04-04
# Description: Implements Westfall et al. (2024) NSBE (Northeast State-based
#   Equations) volume and biomass equations with VTECO coefficients.
#
#   Replaces simplified Jenkins approach with full species x ecodivision
#   coefficient hierarchy (SPCD_DIVISION > SPCD > JENKINS_SPGRPCD).
#
#   Functions:
#   - compute_nsbe_volume(trees, variant, ecodiv): Full NSBE with ecodivision
#   - compute_stand_volume_nsbe(trees): Simplified NSBE w/o ecodivision
#   - add_volume_to_trajectory(): Wrapper for projection engine output
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)

# --- Paths and Environment ---------------------------------------------------

base_dir <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                                 normalizePath(file.path(dirname(sys.frame(1)$ofile), "../.."), mustWork = FALSE)),
                      "calibration")
output_base <- file.path(base_dir, "output", "comparisons")
dir.create(output_base, showWarnings = FALSE, recursive = TRUE)

# NSBE data location: fallback hierarchy
# 1. NSBE_ROOT env variable (Cardinal/remote path)
# 2. Local copy in project data directory
# 3. Original source on development machine
nsbe_candidates <- c(
  Sys.getenv("NSBE_ROOT", ""),
  file.path(dirname(base_dir), "data", "NSBE"),
  Sys.getenv("FVS_NSBE_DATA_DIR", unset = file.path(project_root, "data/NSBE/Coefs/combined"))
)

nsbe_root <- NA_character_
for (path in nsbe_candidates) {
  if (path != "" & dir.exists(path)) {
    nsbe_root <- path
    break
  }
}

if (is.na(nsbe_root)) {
  stop("Cannot locate NSBE data. Set NSBE_ROOT or ensure data/NSBE exists in project.")
}

# =============================================================================
# SECTION 1: Load NSBE coefficient files and reference data
# =============================================================================

#' Load NSBE coefficients and reference tables
#'
#' @param nsbe_root Path to NSBE/VTECO_modified directory
#'
#' @return list with: ref_species, volib_coefs, volob_coefs, total_biomass,
#'         bark_biomass, branch_biomass, equation_forms
load_nsbe_coefficients <- function(nsbe_root) {

  # Reference species with properties
  ref_species <- fread(file.path(nsbe_root, "REF_SPECIES.csv"),
                       select = c("SPCD", "GENUS", "JENKINS_SPGRPCD",
                                  "WOOD_SPGR_GREENVOL_DRYWT", "BARK_SPGR_GREENVOL_DRYWT"))
  setkey(ref_species, SPCD)

  # Volume coefficients (inside-bark)
  volib_coefs <- fread(file.path(nsbe_root, "Coefs/combined/volib_coefs.csv"))
  setkey(volib_coefs, SPCD_DIVISION)

  # Volume coefficients (outside-bark)
  volob_coefs <- fread(file.path(nsbe_root, "Coefs/combined/volob_coefs.csv"))
  setkey(volob_coefs, SPCD_DIVISION)

  # Biomass coefficients
  total_biomass_coefs <- fread(file.path(nsbe_root, "Coefs/combined/total_biomass_coefs.csv"))
  setkey(total_biomass_coefs, SPCD_DIVISION)

  bark_biomass_coefs <- fread(file.path(nsbe_root, "Coefs/combined/bark_biomass_coefs.csv"))
  setkey(bark_biomass_coefs, SPCD_DIVISION)

  branch_biomass_coefs <- fread(file.path(nsbe_root, "Coefs/combined/branch_biomass_coefs.csv"))
  setkey(branch_biomass_coefs, SPCD_DIVISION)

  # Equation forms (form number -> R-evaluable string)
  equation_forms <- fread(file.path(nsbe_root, "Files/equation_forms_and_calls.csv"))

  list(
    ref_species = ref_species,
    volib_coefs = volib_coefs,
    volob_coefs = volob_coefs,
    total_biomass_coefs = total_biomass_coefs,
    bark_biomass_coefs = bark_biomass_coefs,
    branch_biomass_coefs = branch_biomass_coefs,
    equation_forms = equation_forms
  )
}

# Load at module initialization
.nsbe_data <- load_nsbe_coefficients(nsbe_root)

# =============================================================================
# SECTION 2: Coefficient lookup with hierarchical fallback
# =============================================================================

#' Get coefficient row using NSBE hierarchy: SPCD_DIVISION > SPCD > JENKINS_SPGRPCD
#'
#' @param coef_table data.table with columns: SPCD_DIVISION, SPCD, JENKINS_SPGRPCD
#' @param spcd FVS species code
#' @param jenkins_spgrpcd Jenkins species group code
#' @param spcd_div Optional SPCD_DIVISION string (e.g., "12 M330")
#'
#' @return Single row data.table with coefficients, or empty if not found
get_nsbe_coefficient <- function(coef_table, spcd, jenkins_spgrpcd, spcd_div = NULL) {

  # Try SPCD_DIVISION first (most specific)
  if (!is.null(spcd_div)) {
    result <- coef_table[SPCD_DIVISION == spcd_div]
    if (nrow(result) > 0) return(result[1])
  }

  # Try SPCD-level (second priority)
  result <- coef_table[!is.na(SPCD) & SPCD == spcd]
  if (nrow(result) > 0) return(result[1])

  # Fall back to JENKINS_SPGRPCD (least specific)
  result <- coef_table[!is.na(JENKINS_SPGRPCD) & JENKINS_SPGRPCD == jenkins_spgrpcd]
  if (nrow(result) > 0) return(result[1])

  # No match found
  data.table()
}

# =============================================================================
# SECTION 3: Equation implementation
# =============================================================================

#' Apply NSBE volume/biomass equation based on form and coefficients
#'
#' @param dbh DBH in inches
#' @param ht Height in feet
#' @param form Equation form number (3, 4, 5, 50, etc.)
#' @param coefs Named list of coefficients: a, b, c, b2, a0, b0, b1, a1, c1
#'
#' @return Computed value (cubic feet or tons)
apply_equation <- function(dbh, ht, form, coefs) {

  # All equations use DBH (inches) and HT (feet) as-is (FVS/FIA standard)

  switch(as.character(form),
    # Form 3: V = a * DBH^b * HT^c
    "3" = {
      a <- coefs$a
      b <- coefs$b
      c <- coefs$c
      if (is.na(a) | is.na(b) | is.na(c)) return(NA_real_)
      a * (dbh ^ b) * (ht ^ c)
    },

    # Form 4: Segmented DBH equation (two-part)
    # V = ifelse(DBH < k, a0 * DBH^b0 * HT^c, a0 * k^(b0-b1) * DBH^b1 * HT^c)
    "4" = {
      a0 <- coefs$a0
      b0 <- coefs$b0
      b1 <- coefs$b1
      c <- coefs$c
      if (is.na(a0) | is.na(b0) | is.na(b1) | is.na(c)) return(NA_real_)

      # For simplicity, use k = 15 (typical breakpoint in FVS)
      k <- 15

      if (dbh < k) {
        a0 * (dbh ^ b0) * (ht ^ c)
      } else {
        a0 * (k ^ (b0 - b1)) * (dbh ^ b1) * (ht ^ c)
      }
    },

    # Form 5: Variable exponent on DBH
    # V = a * DBH^(a1 * (1 - exp(-b1*DBH))^c1) * HT^c
    "5" = {
      a <- coefs$a
      a1 <- coefs$a1
      b1 <- coefs$b1
      c1 <- coefs$c1
      c <- coefs$c
      if (any(is.na(c(a, a1, b1, c1, c)))) return(NA_real_)

      exp_dbh <- a1 * (1 - exp(-b1 * dbh)) ^ c1
      a * (dbh ^ exp_dbh) * (ht ^ c)
    },

    # Form 50: Exponential decay term
    # V = a * DBH^b * HT^c * exp(-(b2 * DBH))
    "50" = {
      a <- coefs$a
      b <- coefs$b
      c <- coefs$c
      b2 <- coefs$b2
      if (is.na(a) | is.na(b) | is.na(c) | is.na(b2)) return(NA_real_)

      a * (dbh ^ b) * (ht ^ c) * exp(-(b2 * dbh))
    },

    # Unknown form
    NA_real_
  )
}

# =============================================================================
# SECTION 4: Main NSBE computation function
# =============================================================================

#' Compute NSBE volume and biomass from tree list
#'
#' @param trees data.table or data.frame with columns: dbh, ht, tpa, spcd
#'              (dbh in inches, ht in feet, tpa = trees per acre)
#' @param variant FVS variant code (for future ecodivision mapping)
#' @param ecodiv Optional ECOSUBCD for SPCD_DIVISION lookup
#'
#' @return list with per-acre metrics:
#'   - cuft_ib: cubic feet inside-bark
#'   - cuft_ob: cubic feet outside-bark
#'   - biomass_total_tons: total aboveground biomass (short tons)
#'   - biomass_bark_tons: bark biomass (short tons)
#'   - biomass_branch_tons: branch biomass (short tons)
#'   - carbon_tons: total carbon (short tons, 0.50 fraction)
compute_nsbe_volume <- function(trees, variant = NULL, ecodiv = NULL) {

  if (nrow(trees) == 0) {
    return(list(
      cuft_ib = 0,
      cuft_ob = 0,
      biomass_total_tons = 0,
      biomass_bark_tons = 0,
      biomass_branch_tons = 0,
      carbon_tons = 0
    ))
  }

  # Coerce to data.table
  trees_dt <- as.data.table(trees)

  # Initialize output columns
  trees_dt[, c("vol_ib", "vol_ob",
               "biomass_total", "biomass_bark", "biomass_branch") :=
           list(0.0, 0.0, 0.0, 0.0, 0.0)]

  # Get species reference data
  trees_dt <- trees_dt[.nsbe_data$ref_species,
                       on = .(spcd = SPCD),
                       nomatch = 0L]

  # Process each tree
  for (i in seq_len(nrow(trees_dt))) {

    spcd <- trees_dt$spcd[i]
    jenkins_spgrpcd <- trees_dt$JENKINS_SPGRPCD[i]
    dbh <- trees_dt$dbh[i]
    ht <- trees_dt$ht[i]

    if (is.na(dbh) | is.na(ht) | dbh < 1.0) next  # Skip invalid trees

    # Construct SPCD_DIVISION if ecodiv available
    spcd_div <- if (!is.null(ecodiv)) paste(spcd, ecodiv) else NULL

    # --- Inside-bark volume ---
    coef_ib <- get_nsbe_coefficient(.nsbe_data$volib_coefs, spcd, jenkins_spgrpcd, spcd_div)
    if (nrow(coef_ib) > 0) {
      form_ib <- coef_ib$equation[1]
      vol_ib <- apply_equation(dbh, ht, form_ib, coef_ib)
      if (!is.na(vol_ib)) trees_dt$vol_ib[i] <- vol_ib
    }

    # --- Outside-bark volume ---
    coef_ob <- get_nsbe_coefficient(.nsbe_data$volob_coefs, spcd, jenkins_spgrpcd, spcd_div)
    if (nrow(coef_ob) > 0) {
      form_ob <- coef_ob$equation[1]
      vol_ob <- apply_equation(dbh, ht, form_ob, coef_ob)
      if (!is.na(vol_ob)) trees_dt$vol_ob[i] <- vol_ob
    }

    # --- Total aboveground biomass ---
    coef_total_bio <- get_nsbe_coefficient(.nsbe_data$total_biomass_coefs,
                                           spcd, jenkins_spgrpcd, spcd_div)
    if (nrow(coef_total_bio) > 0) {
      form_bio <- coef_total_bio$equation[1]
      # Biomass equations typically output in kg
      biomass_kg <- apply_equation(dbh, ht, form_bio, coef_total_bio)
      if (!is.na(biomass_kg)) {
        # Convert kg to short tons (1 ton = 907.185 kg)
        trees_dt$biomass_total[i] <- biomass_kg / 907.185
      }
    }

    # --- Bark biomass ---
    coef_bark <- get_nsbe_coefficient(.nsbe_data$bark_biomass_coefs,
                                      spcd, jenkins_spgrpcd, spcd_div)
    if (nrow(coef_bark) > 0) {
      form_bark <- coef_bark$equation[1]
      biomass_bark_kg <- apply_equation(dbh, ht, form_bark, coef_bark)
      if (!is.na(biomass_bark_kg)) {
        trees_dt$biomass_bark[i] <- biomass_bark_kg / 907.185
      }
    }

    # --- Branch biomass ---
    coef_branch <- get_nsbe_coefficient(.nsbe_data$branch_biomass_coefs,
                                        spcd, jenkins_spgrpcd, spcd_div)
    if (nrow(coef_branch) > 0) {
      form_branch <- coef_branch$equation[1]
      biomass_branch_kg <- apply_equation(dbh, ht, form_branch, coef_branch)
      if (!is.na(biomass_branch_kg)) {
        trees_dt$biomass_branch[i] <- biomass_branch_kg / 907.185
      }
    }
  }

  # Calculate per-acre totals
  cuft_ib <- sum(trees_dt$vol_ib * trees_dt$tpa, na.rm = TRUE)
  cuft_ob <- sum(trees_dt$vol_ob * trees_dt$tpa, na.rm = TRUE)
  biomass_total <- sum(trees_dt$biomass_total * trees_dt$tpa, na.rm = TRUE)
  biomass_bark <- sum(trees_dt$biomass_bark * trees_dt$tpa, na.rm = TRUE)
  biomass_branch <- sum(trees_dt$biomass_branch * trees_dt$tpa, na.rm = TRUE)

  # Carbon = 0.50 fraction of dry biomass
  carbon_tons <- biomass_total * 0.50

  list(
    cuft_ib = cuft_ib,
    cuft_ob = cuft_ob,
    biomass_total_tons = biomass_total,
    biomass_bark_tons = biomass_bark,
    biomass_branch_tons = biomass_branch,
    carbon_tons = carbon_tons
  )
}

# =============================================================================
# SECTION 5: Simplified NSBE for projection engine (without ecodivision)
# =============================================================================

#' Simplified NSBE computation for projection engine without ecodivision
#'
#' Uses SPCD > JENKINS_SPGRPCD hierarchy. Suitable for forward projection
#' without detailed ecodivision information.
#'
#' @param trees data.table/data.frame with dbh, ht, tpa, spcd
#'
#' @return list with per-acre: cuft_ib, cuft_ob, biomass_total_tons,
#'         biomass_bark_tons, biomass_branch_tons, carbon_tons
compute_stand_volume_nsbe <- function(trees) {
  compute_nsbe_volume(trees, variant = NULL, ecodiv = NULL)
}

# =============================================================================
# SECTION 6: Integration with projection engine output
# =============================================================================

#' Add NSBE volumes to projection trajectory
#'
#' Takes output from script 17 (stand_projection_engine) and computes
#' volume/biomass for each year.
#'
#' @param trajectory data.table from stand_projection_engine with columns:
#'                   year, dbh, ht, tpa, spcd
#'
#' @return trajectory with added columns: vol_cuft_ib, vol_cuft_ob,
#'         bio_tons_total, bio_tons_bark, bio_tons_branch, carbon_tons
add_volume_to_trajectory <- function(trajectory) {

  if (is.null(trajectory) | nrow(trajectory) == 0) {
    return(trajectory)
  }

  trajectory_dt <- as.data.table(trajectory)

  # Initialize volume columns
  trajectory_dt[, c("vol_cuft_ib", "vol_cuft_ob",
                    "bio_tons_total", "bio_tons_bark", "bio_tons_branch",
                    "carbon_tons") :=
                list(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)]

  # Group by year and compute volumes
  years <- unique(trajectory_dt$year)

  for (yr in years) {

    trees_in_year <- trajectory_dt[year == yr]

    if (nrow(trees_in_year) == 0) next

    # Compute volumes
    vol_result <- compute_stand_volume_nsbe(trees_in_year)

    # Assign to all trees in this year
    trajectory_dt[year == yr, c("vol_cuft_ib", "vol_cuft_ob",
                                "bio_tons_total", "bio_tons_bark",
                                "bio_tons_branch", "carbon_tons") :=
                  list(vol_result$cuft_ib, vol_result$cuft_ob,
                       vol_result$biomass_total_tons, vol_result$biomass_bark_tons,
                       vol_result$biomass_branch_tons, vol_result$carbon_tons)]
  }

  setkey(trajectory_dt, year)
  trajectory_dt[]
}

# =============================================================================
# SECTION 7: Validation and diagnostics
# =============================================================================

#' Check NSBE data availability for species set
#'
#' @param spcds Vector of species codes to check
#'
#' @return data.table showing coefficient availability
check_nsbe_coverage <- function(spcds) {

  coverage <- data.table()

  for (spcd in unique(spcds)) {

    ref <- .nsbe_data$ref_species[SPCD == spcd]

    if (nrow(ref) == 0) {
      coverage <- rbind(coverage,
                        data.table(SPCD = spcd, has_volib = FALSE, has_volob = FALSE,
                                   has_biomass = FALSE, JENKINS_SPGRPCD = NA_integer_))
      next
    }

    jenkins_grp <- ref$JENKINS_SPGRPCD[1]

    has_volib <- nrow(.nsbe_data$volib_coefs[SPCD == spcd | JENKINS_SPGRPCD == jenkins_grp]) > 0
    has_volob <- nrow(.nsbe_data$volob_coefs[SPCD == spcd | JENKINS_SPGRPCD == jenkins_grp]) > 0
    has_biomass <- nrow(.nsbe_data$total_biomass_coefs[SPCD == spcd | JENKINS_SPGRPCD == jenkins_grp]) > 0

    coverage <- rbind(coverage,
                      data.table(SPCD = spcd, has_volib = has_volib, has_volob = has_volob,
                                 has_biomass = has_biomass, JENKINS_SPGRPCD = jenkins_grp))
  }

  coverage[]
}

# =============================================================================
# SECTION 8: Export functions for module use
# =============================================================================

# Export main functions
.GlobalEnv$compute_nsbe_volume <- compute_nsbe_volume
.GlobalEnv$compute_stand_volume_nsbe <- compute_stand_volume_nsbe
.GlobalEnv$add_volume_to_trajectory <- add_volume_to_trajectory
.GlobalEnv$check_nsbe_coverage <- check_nsbe_coverage

cat("NSBE volume equations module loaded successfully.\n")
cat("Available functions:\n")
cat("  - compute_nsbe_volume(trees, variant=NULL, ecodiv=NULL)\n")
cat("  - compute_stand_volume_nsbe(trees)\n")
cat("  - add_volume_to_trajectory(trajectory)\n")
cat("  - check_nsbe_coverage(spcds)\n")
cat("\nData source: ", nsbe_root, "\n", sep = "")

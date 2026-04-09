# =============================================================================
# Title: Download FIA Data for Calibration
# Author: A. Weiskittel
# Date: 2026-04-09
# Description: Downloads FIA state databases needed for the calibration
#   pipeline using the rFIA package. This script makes the calibration
#   reproducible from a fresh repository clone.
# Dependencies: rFIA package, internet connection
# =============================================================================

# --- Libraries ---------------------------------------------------------------
if (!requireNamespace("rFIA", quietly = TRUE)) {
  install.packages("rFIA", repos = "https://cloud.r-project.org")
}
library(rFIA)

# --- Configuration -----------------------------------------------------------
project_root <- Sys.getenv("FVS_PROJECT_ROOT",
                           unset = normalizePath(file.path(dirname(
                             sys.frame(1)$ofile), "../.."), mustWork = FALSE))
data_dir <- file.path(project_root, "calibration/data/raw_fia")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# All US states with forested land that contribute to FVS variant calibration
# Organized by FVS variant region for reference
states <- list(
  # Northeast (NE variant)
  NE = c("CT", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "PA", "RI", "VT"),
  # Lake States (LS variant)
  LS = c("MI", "MN", "WI"),
  # Central States (CS variant)
  CS = c("IA", "IL", "IN", "KS", "MO", "ND", "NE", "OH", "SD"),
  # Southern (SN variant)
  SN = c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "OK",
         "SC", "TN", "TX", "VA", "WV"),
  # Pacific Northwest (PN, WC, OC, OP variants)
  PNW = c("OR", "WA"),
  # California (CA variant)
  CA = c("CA"),
  # Inland Empire and related (IE, EM, BM, CI, KT, TT, UT, CR, SO, EC, WS)
  INTERIOR = c("AZ", "CO", "ID", "MT", "NM", "NV", "UT", "WY"),
  # Alaska (AK variant)
  AK = c("AK")
)

all_states <- sort(unique(unlist(states)))

cat("==========================================================\n")
cat("FIA DATA DOWNLOAD\n")
cat(sprintf("Target directory: %s\n", data_dir))
cat(sprintf("States to download: %d\n", length(all_states)))
cat("==========================================================\n\n")

# --- Download ----------------------------------------------------------------
# rFIA::getFIA downloads the SQLite database for each state
# These are large files (100 MB to 2+ GB per state)

for (st in all_states) {
  st_dir <- file.path(data_dir, st)

  # Skip if already downloaded
  if (dir.exists(st_dir) && length(list.files(st_dir, pattern = "\\.csv$")) > 5) {
    cat(sprintf("  %s: already downloaded, skipping\n", st))
    next
  }

  cat(sprintf("  %s: downloading...\n", st))
  tryCatch({
    getFIA(states = st, dir = data_dir,
           tables = c("PLOT", "COND", "TREE", "SUBPLOT", "SUBP_COND",
                       "SEEDLING", "SURVEY", "POP_EVAL",
                       "POP_EVAL_TYP", "POP_STRATUM",
                       "POP_PLOT_STRATUM_ASSGN"),
           load = FALSE)
    cat(sprintf("  %s: done\n", st))
  }, error = function(e) {
    cat(sprintf("  %s: FAILED (%s)\n", st, conditionMessage(e)))
  })
}

cat("\n==========================================================\n")
cat("DOWNLOAD COMPLETE\n")
cat(sprintf("Data directory: %s\n", data_dir))
cat(sprintf("Total size: %.1f GB\n",
            sum(file.info(list.files(data_dir, recursive = TRUE,
                                     full.names = TRUE))$size, na.rm = TRUE) / 1e9))
cat("==========================================================\n")
cat("\nNext step: Run calibration/R/01_fetch_fia_data.R to process\n")
cat("the raw FIA data into variant-specific training datasets.\n")

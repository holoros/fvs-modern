# =============================================================================
# Title: Composite %RMSE Benchmark Assessment & Spatial Maps
# Author: A. Weiskittel
# Date: 2026-04-08
# Description: Multi-attribute benchmark assessment using %RMSE (RMSE as percent
#   of observed mean) across BA, TPA, QMD, and VOL_CFGRS, weighted by
#   CONDPROP_UNADJ (proportion of plot area in each condition class, proxy for
#   forestland acres). This normalizes for:
#     (1) Regional differences in absolute stand metrics (PNW vs SE vs NE)
#     (2) Unequal representation of partial vs full conditions on FIA plots
#
#   The composite %RMSE = weighted mean of individual attribute %RMSEs,
#   with equal weights across BA, TPA, QMD, VOL unless otherwise specified.
#
#   OUTPUT FIGURES:
#     Fig A: Composite %RMSE by variant (paired bar, calibrated vs default)
#     Fig B: Per-attribute %RMSE comparison (faceted by attribute)
#     Fig C: %RMSE reduction dumbbell (composite, by variant)
#     Fig D: Spatial hex map of composite %Error (calibrated) across CONUS
#     Fig E: Spatial hex map of composite %Error reduction
#     Fig F: Spatial hex map of composite %Bias (calibrated)
#     Fig G: Variant centroid map of composite %RMSE reduction
#     Fig H: Composite 2x2 hero map figure for manuscript
#     Fig I: Volume %RMSE maps (bonus)
#
# Conventions:
#   - Okabe-Ito colorblind-safe palette for regions
#   - No hyphens in axis labels
#   - Expression notation for units
#   - Publication quality: 300 dpi, consistent fonts
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)

# --- Configuration -----------------------------------------------------------
project_root <- Sys.getenv("FVS_PROJECT_ROOT")
if (project_root == "") project_root <- getwd()

fia_root     <- Sys.getenv("FVS_FIA_DATA_DIR")
if (fia_root == "") fia_root <- "/users/PUOM0008/crsfaaron/FIA"

output_root  <- file.path(project_root, "calibration/output/comparisons")
fig_dir      <- file.path(output_root, "manuscript_figures")
table_dir    <- file.path(output_root, "manuscript_tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# Attribute weights for composite %RMSE (equal by default)
ATTR_WEIGHTS <- c(BA = 1, TPA = 1, QMD = 1, VOL = 1)
ATTR_WEIGHTS <- ATTR_WEIGHTS / sum(ATTR_WEIGHTS)  # normalize

cat("Composite %RMSE attribute weights:\n")
cat(sprintf("  BA=%.2f  TPA=%.2f  QMD=%.2f  VOL=%.2f\n",
  ATTR_WEIGHTS["BA"], ATTR_WEIGHTS["TPA"],
  ATTR_WEIGHTS["QMD"], ATTR_WEIGHTS["VOL"]))

# --- Publication theme -------------------------------------------------------
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray95", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, color = "black"),
    axis.text  = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 13, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text  = element_text(size = 9),
    legend.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray98", color = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

# --- Variant/region mapping --------------------------------------------------
variant_info <- data.table(
  variant = c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
              "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
              "OP", "SN", "CS", "LS", "ON", "NE", "ACD"),
  region = c("Pacific Northwest", "Pacific Northwest", "Pacific Northwest",
             "Pacific Northwest",
             "Pacific Southwest", "Pacific Southwest", "Pacific Southwest",
             "Intermountain", "Intermountain", "Intermountain",
             "Intermountain", "Intermountain", "Intermountain",
             "Northern Rockies", "Northern Rockies",
             "Southern Rockies", "Southern Rockies", "Southern Rockies",
             "Central Plains", "Southern", "Central States",
             "Lake States", "Lake States", "Northeast", "Northeast")
)

region_colors <- c(
  "Pacific Northwest" = "#0072B2",
  "Pacific Southwest" = "#56B4E9",
  "Intermountain"     = "#009E73",
  "Northern Rockies"  = "#F0E442",
  "Southern Rockies"  = "#E69F00",
  "Central Plains"    = "#D55E00",
  "Southern"          = "#CC79A7",
  "Central States"    = "#999999",
  "Lake States"       = "#66CCEE",
  "Northeast"         = "#882255"
)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("\nLoading validation data...\n")
val_file <- file.path(output_root, "intermediate/validation_data.csv")
stopifnot("validation_data.csv not found" = file.exists(val_file))
val <- fread(val_file)
val[, VARIANT := toupper(VARIANT)]
cat("  Loaded", nrow(val), "conditions\n")

# --- Join CONDPROP_UNADJ and STATECD from FIA ENTIRE_COND --------------------
cat("Joining CONDPROP_UNADJ and STATECD from FIA ENTIRE_COND...\n")
cond_file <- file.path(fia_root, "ENTIRE_COND.csv")
if (file.exists(cond_file)) {
  # Read CONDPROP_UNADJ + STATECD (for expansion factor estimation)
  cond_raw <- fread(cond_file, select = c("PLT_CN", "CONDID", "CONDPROP_UNADJ", "STATECD"))
  cond_raw[, PLT_CN := as.character(PLT_CN)]
  cond_raw[, CONDID := as.integer(CONDID)]

  # Join via PLT_CN_t2 (the observation-time plot)
  val[, PLT_CN_t2 := as.character(PLT_CN_t2)]
  val[, CONDID := as.integer(CONDID)]
  val <- merge(val, cond_raw, by.x = c("PLT_CN_t2", "CONDID"),
               by.y = c("PLT_CN", "CONDID"), all.x = TRUE)

  n_with_cp <- sum(!is.na(val$CONDPROP_UNADJ))
  cat("  CONDPROP_UNADJ matched:", n_with_cp, "/", nrow(val),
      sprintf("(%.1f%%)\n", 100 * n_with_cp / nrow(val)))

  # Fill missing with 1.0 (assume full-plot condition where unknown)
  val[is.na(CONDPROP_UNADJ), CONDPROP_UNADJ := 1.0]
  rm(cond_raw)
} else {
  cat("  WARNING: ENTIRE_COND.csv not found at", cond_file, "\n")
  cat("  Setting CONDPROP_UNADJ = 1 for all conditions (unweighted).\n")
  val[, CONDPROP_UNADJ := 1.0]
}

# --- Compute per-condition represented acres ----------------------------------
# Standard FIA Phase 2: ~6000 acres/plot (actually 5937.2 ac for 1:6000 grid)
# Some states use intensified sampling (e.g., MN, WI, ME) with denser grids.
# Without POP_STRATUM tables, approximate by using known state-level info.
# Weight per condition = CONDPROP_UNADJ * acres_per_plot(state)
#
# Known intensified states (approximate expansion factors):
#   MN (27): ~3000 ac/plot (2x intensity)
#   WI (55): ~3000 ac/plot
#   ME (23): ~3000 ac/plot
#   OR (41): ~3000 in some inventories
# Default for all others: 6000 ac/plot

ACRES_PER_PLOT_DEFAULT <- 6000
intensified_states <- data.table(
  STATECD = c(23L, 27L, 55L),
  acres_per_plot = c(3000, 3000, 3000)
)

if ("STATECD" %in% names(val)) {
  val <- merge(val, intensified_states, by = "STATECD", all.x = TRUE)
  val[is.na(acres_per_plot), acres_per_plot := ACRES_PER_PLOT_DEFAULT]
  val[, WEIGHT_ACRES := CONDPROP_UNADJ * acres_per_plot]
  cat("Computing per-condition represented acres (CONDPROP * acres/plot)...\n")
  cat("  Total represented area:", round(sum(val$WEIGHT_ACRES, na.rm = TRUE) / 1e6, 2), "million acres\n")
  cat("  States with intensified weighting:", paste(intensified_states$STATECD, collapse = ", "), "\n")
} else {
  cat("  STATECD not available; using constant", ACRES_PER_PLOT_DEFAULT, "acres/plot.\n")
  val[, WEIGHT_ACRES := CONDPROP_UNADJ * ACRES_PER_PLOT_DEFAULT]
}

# --- Join coordinates from raster lookup --------------------------------------
raster_file <- file.path(project_root, "calibration/data/plot_raster_lookup.csv")
if (file.exists(raster_file) && !"LAT" %in% names(val)) {
  cat("Joining coordinates from raster lookup...\n")
  raster_lookup <- fread(raster_file, colClasses = c(PLT_CN = "character"),
                         select = c("PLT_CN", "LAT", "LON"))
  val <- merge(val, raster_lookup, by.x = "PLT_CN_t2", by.y = "PLT_CN", all.x = TRUE)
  cat("  Geocoded:", sum(!is.na(val$LAT)), "/", nrow(val), "\n")
  rm(raster_lookup)
}

# =============================================================================
# STEP 1: Compute per-condition %errors for all attributes
# =============================================================================

cat("\n--- Computing per-condition errors for BA, TPA, QMD, VOL ---\n")

# BA
val[, BA_pctErr_calib   := fifelse(BA_t2 > 5, 100 * (BA_pred_calib - BA_t2) / BA_t2, NA_real_)]
val[, BA_pctErr_default := fifelse(BA_t2 > 5, 100 * (BA_pred_default - BA_t2) / BA_t2, NA_real_)]
val[, BA_absPctErr_calib   := abs(BA_pctErr_calib)]
val[, BA_absPctErr_default := abs(BA_pctErr_default)]

# TPA
val[, TPA_pctErr_calib   := fifelse(TPA_t2 > 5, 100 * (TPA_pred_calib - TPA_t2) / TPA_t2, NA_real_)]
val[, TPA_pctErr_default := fifelse(TPA_t2 > 5, 100 * (TPA_pred_default - TPA_t2) / TPA_t2, NA_real_)]
val[, TPA_absPctErr_calib   := abs(TPA_pctErr_calib)]
val[, TPA_absPctErr_default := abs(TPA_pctErr_default)]

# QMD
val[, QMD_pctErr_calib   := fifelse(QMD_t2 > 1, 100 * (QMD_pred_calib - QMD_t2) / QMD_t2, NA_real_)]
val[, QMD_pctErr_default := fifelse(QMD_t2 > 1, 100 * (QMD_pred_default - QMD_t2) / QMD_t2, NA_real_)]
val[, QMD_absPctErr_calib   := abs(QMD_pctErr_calib)]
val[, QMD_absPctErr_default := abs(QMD_pctErr_default)]

# VOL_CFGRS
val[, VOL_pctErr_calib   := fifelse(VOL_CFGRS_t2 > 50, 100 * (VOL_CFGRS_pred_calib - VOL_CFGRS_t2) / VOL_CFGRS_t2, NA_real_)]
val[, VOL_pctErr_default := fifelse(VOL_CFGRS_t2 > 50, 100 * (VOL_CFGRS_pred_default - VOL_CFGRS_t2) / VOL_CFGRS_t2, NA_real_)]
val[, VOL_absPctErr_calib   := abs(VOL_pctErr_calib)]
val[, VOL_absPctErr_default := abs(VOL_pctErr_default)]

# Composite absolute %error per condition (weighted mean across available attributes)
val[, composite_absPctErr_calib := {
  w <- numeric(0); e <- numeric(0)
  if (!is.na(BA_absPctErr_calib))  { w <- c(w, ATTR_WEIGHTS["BA"]);  e <- c(e, BA_absPctErr_calib) }
  if (!is.na(TPA_absPctErr_calib)) { w <- c(w, ATTR_WEIGHTS["TPA"]); e <- c(e, TPA_absPctErr_calib) }
  if (!is.na(QMD_absPctErr_calib)) { w <- c(w, ATTR_WEIGHTS["QMD"]); e <- c(e, QMD_absPctErr_calib) }
  if (!is.na(VOL_absPctErr_calib)) { w <- c(w, ATTR_WEIGHTS["VOL"]); e <- c(e, VOL_absPctErr_calib) }
  if (length(w) > 0) sum(w * e) / sum(w) else NA_real_
}, by = seq_len(nrow(val))]

val[, composite_absPctErr_default := {
  w <- numeric(0); e <- numeric(0)
  if (!is.na(BA_absPctErr_default))  { w <- c(w, ATTR_WEIGHTS["BA"]);  e <- c(e, BA_absPctErr_default) }
  if (!is.na(TPA_absPctErr_default)) { w <- c(w, ATTR_WEIGHTS["TPA"]); e <- c(e, TPA_absPctErr_default) }
  if (!is.na(QMD_absPctErr_default)) { w <- c(w, ATTR_WEIGHTS["QMD"]); e <- c(e, QMD_absPctErr_default) }
  if (!is.na(VOL_absPctErr_default)) { w <- c(w, ATTR_WEIGHTS["VOL"]); e <- c(e, VOL_absPctErr_default) }
  if (length(w) > 0) sum(w * e) / sum(w) else NA_real_
}, by = seq_len(nrow(val))]

val[, composite_improvement := composite_absPctErr_default - composite_absPctErr_calib]

cat("  Conditions with composite %Error (calib):", sum(!is.na(val$composite_absPctErr_calib)), "\n")
cat("  Conditions with composite %Error (default):", sum(!is.na(val$composite_absPctErr_default)), "\n")

# =============================================================================
# STEP 2: Compute CONDPROP-weighted %RMSE by variant
# =============================================================================

cat("\n--- Computing CONDPROP-weighted %RMSE by variant ---\n")

# Helper: weighted %RMSE = sqrt(weighted.mean(pctErr^2, w)) where w = CONDPROP
wpctrmse <- function(pctErr, w) {
  ok <- !is.na(pctErr) & !is.na(w) & w > 0
  if (sum(ok) < 5) return(NA_real_)
  sqrt(weighted.mean(pctErr[ok]^2, w[ok]))
}

# Helper: weighted mean
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 5) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

pctrmse_variant <- val[, .(
  n = .N,
  sum_acres = sum(WEIGHT_ACRES, na.rm = TRUE),

  # Individual attribute %RMSE (CONDPROP-weighted)
  BA_pctRMSE_calib    = wpctrmse(BA_pctErr_calib, WEIGHT_ACRES),
  BA_pctRMSE_default  = wpctrmse(BA_pctErr_default, WEIGHT_ACRES),
  TPA_pctRMSE_calib   = wpctrmse(TPA_pctErr_calib, WEIGHT_ACRES),
  TPA_pctRMSE_default = wpctrmse(TPA_pctErr_default, WEIGHT_ACRES),
  QMD_pctRMSE_calib   = wpctrmse(QMD_pctErr_calib, WEIGHT_ACRES),
  QMD_pctRMSE_default = wpctrmse(QMD_pctErr_default, WEIGHT_ACRES),
  VOL_pctRMSE_calib   = wpctrmse(VOL_pctErr_calib, WEIGHT_ACRES),
  VOL_pctRMSE_default = wpctrmse(VOL_pctErr_default, WEIGHT_ACRES),

  # Individual attribute %Bias (CONDPROP-weighted)
  BA_pctBias_calib    = wmean(BA_pctErr_calib, WEIGHT_ACRES),
  BA_pctBias_default  = wmean(BA_pctErr_default, WEIGHT_ACRES),
  TPA_pctBias_calib   = wmean(TPA_pctErr_calib, WEIGHT_ACRES),
  TPA_pctBias_default = wmean(TPA_pctErr_default, WEIGHT_ACRES),
  QMD_pctBias_calib   = wmean(QMD_pctErr_calib, WEIGHT_ACRES),
  QMD_pctBias_default = wmean(QMD_pctErr_default, WEIGHT_ACRES),
  VOL_pctBias_calib   = wmean(VOL_pctErr_calib, WEIGHT_ACRES),
  VOL_pctBias_default = wmean(VOL_pctErr_default, WEIGHT_ACRES),

  # Mean observed values (CONDPROP-weighted) for context
  mean_obs_BA  = wmean(BA_t2, WEIGHT_ACRES),
  mean_obs_TPA = wmean(TPA_t2, WEIGHT_ACRES),
  mean_obs_QMD = wmean(QMD_t2, WEIGHT_ACRES),
  mean_obs_VOL = wmean(VOL_CFGRS_t2, WEIGHT_ACRES)
), by = VARIANT]

# Composite %RMSE = weighted mean of the four attribute %RMSEs
pctrmse_variant[, composite_pctRMSE_calib := {
  vals <- c(BA_pctRMSE_calib, TPA_pctRMSE_calib, QMD_pctRMSE_calib, VOL_pctRMSE_calib)
  wts  <- ATTR_WEIGHTS[c("BA", "TPA", "QMD", "VOL")]
  ok <- !is.na(vals)
  if (sum(ok) > 0) sum(vals[ok] * wts[ok]) / sum(wts[ok]) else NA_real_
}, by = VARIANT]

pctrmse_variant[, composite_pctRMSE_default := {
  vals <- c(BA_pctRMSE_default, TPA_pctRMSE_default, QMD_pctRMSE_default, VOL_pctRMSE_default)
  wts  <- ATTR_WEIGHTS[c("BA", "TPA", "QMD", "VOL")]
  ok <- !is.na(vals)
  if (sum(ok) > 0) sum(vals[ok] * wts[ok]) / sum(wts[ok]) else NA_real_
}, by = VARIANT]

pctrmse_variant[, composite_pctRMSE_reduction := 100 * (1 - composite_pctRMSE_calib / composite_pctRMSE_default)]

# Add region
pctrmse_variant <- merge(pctrmse_variant, variant_info[, .(variant, region)],
  by.x = "VARIANT", by.y = "variant", all.x = TRUE)

# Compute overall row
overall <- val[, .(
  VARIANT = "OVERALL", n = .N, sum_acres = sum(WEIGHT_ACRES, na.rm = TRUE),
  BA_pctRMSE_calib    = wpctrmse(BA_pctErr_calib, WEIGHT_ACRES),
  BA_pctRMSE_default  = wpctrmse(BA_pctErr_default, WEIGHT_ACRES),
  TPA_pctRMSE_calib   = wpctrmse(TPA_pctErr_calib, WEIGHT_ACRES),
  TPA_pctRMSE_default = wpctrmse(TPA_pctErr_default, WEIGHT_ACRES),
  QMD_pctRMSE_calib   = wpctrmse(QMD_pctErr_calib, WEIGHT_ACRES),
  QMD_pctRMSE_default = wpctrmse(QMD_pctErr_default, WEIGHT_ACRES),
  VOL_pctRMSE_calib   = wpctrmse(VOL_pctErr_calib, WEIGHT_ACRES),
  VOL_pctRMSE_default = wpctrmse(VOL_pctErr_default, WEIGHT_ACRES),
  BA_pctBias_calib = wmean(BA_pctErr_calib, WEIGHT_ACRES),
  BA_pctBias_default = wmean(BA_pctErr_default, WEIGHT_ACRES),
  TPA_pctBias_calib = wmean(TPA_pctErr_calib, WEIGHT_ACRES),
  TPA_pctBias_default = wmean(TPA_pctErr_default, WEIGHT_ACRES),
  QMD_pctBias_calib = wmean(QMD_pctErr_calib, WEIGHT_ACRES),
  QMD_pctBias_default = wmean(QMD_pctErr_default, WEIGHT_ACRES),
  VOL_pctBias_calib = wmean(VOL_pctErr_calib, WEIGHT_ACRES),
  VOL_pctBias_default = wmean(VOL_pctErr_default, WEIGHT_ACRES),
  mean_obs_BA = wmean(BA_t2, WEIGHT_ACRES),
  mean_obs_TPA = wmean(TPA_t2, WEIGHT_ACRES),
  mean_obs_QMD = wmean(QMD_t2, WEIGHT_ACRES),
  mean_obs_VOL = wmean(VOL_CFGRS_t2, WEIGHT_ACRES)
)]
overall[, composite_pctRMSE_calib := mean(c(BA_pctRMSE_calib, TPA_pctRMSE_calib,
  QMD_pctRMSE_calib, VOL_pctRMSE_calib), na.rm = TRUE)]
overall[, composite_pctRMSE_default := mean(c(BA_pctRMSE_default, TPA_pctRMSE_default,
  QMD_pctRMSE_default, VOL_pctRMSE_default), na.rm = TRUE)]
overall[, composite_pctRMSE_reduction := 100 * (1 - composite_pctRMSE_calib / composite_pctRMSE_default)]
overall[, region := "All"]

cat("\nComposite %RMSE Summary (CONDPROP-weighted, 4 attributes):\n")
print(pctrmse_variant[order(composite_pctRMSE_calib), .(
  VARIANT, n,
  BA = round(BA_pctRMSE_calib, 1),
  TPA = round(TPA_pctRMSE_calib, 1),
  QMD = round(QMD_pctRMSE_calib, 1),
  VOL = round(VOL_pctRMSE_calib, 1),
  Composite_Calib = round(composite_pctRMSE_calib, 1),
  Composite_Default = round(composite_pctRMSE_default, 1),
  Reduction_pct = round(composite_pctRMSE_reduction, 1)
)])

cat(sprintf("\nOverall composite: Calibrated=%.1f%%, Default=%.1f%%, Reduction=%.1f%%\n",
  overall$composite_pctRMSE_calib, overall$composite_pctRMSE_default,
  overall$composite_pctRMSE_reduction))

# Save table
full_table <- rbind(pctrmse_variant, overall, fill = TRUE)
fwrite(full_table, file.path(table_dir, "fia_benchmark_composite_pctrmse.csv"))
cat("Saved: fia_benchmark_composite_pctrmse.csv\n")

# =============================================================================
# FIGURE A: Composite %RMSE by Variant (paired bar chart)
# =============================================================================

cat("\nFigure A: Composite %RMSE by variant...\n")

comp_long <- melt(pctrmse_variant,
  id.vars = c("VARIANT", "region"),
  measure.vars = c("composite_pctRMSE_calib", "composite_pctRMSE_default"),
  variable.name = "approach", value.name = "pctRMSE")
comp_long[, approach := fifelse(approach == "composite_pctRMSE_calib", "Calibrated", "Default")]

fig_a <- ggplot(comp_long,
  aes(x = reorder(VARIANT, pctRMSE), y = pctRMSE, fill = approach)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00")) +
  geom_hline(yintercept = c(25, 50), linetype = "dashed", color = "gray60", linewidth = 0.3) +
  labs(
    x = "FVS Variant",
    y = "Composite %RMSE",
    title = "Composite %RMSE: Calibrated vs Default (CONDPROP weighted)",
    subtitle = "Mean of BA, TPA, QMD, and VOL %RMSE; weighted by represented forestland acres (CONDPROP x ~6000 ac/plot)",
    fill = "Approach"
  ) +
  coord_flip() +
  theme_pub +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pctrmse_composite_by_variant.png"), fig_a,
       width = 22, height = 20, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "pctrmse_composite_by_variant.pdf"), fig_a,
       width = 22, height = 20, units = "cm", bg = "white")
cat("  Saved: pctrmse_composite_by_variant.png/pdf\n")

# =============================================================================
# FIGURE B: Per-attribute %RMSE (faceted)
# =============================================================================

cat("Figure B: Per-attribute %RMSE faceted...\n")

attr_long <- melt(pctrmse_variant,
  id.vars = c("VARIANT", "region"),
  measure.vars = c("BA_pctRMSE_calib", "BA_pctRMSE_default",
                    "TPA_pctRMSE_calib", "TPA_pctRMSE_default",
                    "QMD_pctRMSE_calib", "QMD_pctRMSE_default",
                    "VOL_pctRMSE_calib", "VOL_pctRMSE_default"))

attr_long[, attribute := fcase(
  grepl("^BA_",  variable), "Basal Area",
  grepl("^TPA_", variable), "Trees per Acre",
  grepl("^QMD_", variable), "Quadratic Mean Diameter",
  grepl("^VOL_", variable), "Gross Cubic Foot Volume"
)]
attr_long[, approach := fifelse(grepl("_calib$", variable), "Calibrated", "Default")]

fig_b <- ggplot(attr_long,
  aes(x = reorder(VARIANT, value), y = value, fill = approach)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00")) +
  facet_wrap(~attribute, scales = "free_x", ncol = 2) +
  coord_flip() +
  labs(
    x = "FVS Variant", y = "%RMSE (CONDPROP weighted)",
    title = "Per-Attribute %RMSE by Variant",
    subtitle = "Weighted by represented forestland acres; each attribute compared independently",
    fill = "Approach"
  ) +
  theme_pub +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pctrmse_per_attribute.png"), fig_b,
       width = 30, height = 30, units = "cm", dpi = 300, bg = "white")
cat("  Saved: pctrmse_per_attribute.png\n")

# =============================================================================
# FIGURE C: Composite %RMSE Reduction Dumbbell
# =============================================================================

cat("Figure C: Composite %RMSE reduction dumbbell...\n")

dumb <- pctrmse_variant[!is.na(composite_pctRMSE_calib)]
dumb[, VARIANT := factor(VARIANT, levels = dumb[order(composite_pctRMSE_reduction)]$VARIANT)]

fig_c <- ggplot(dumb, aes(y = VARIANT)) +
  geom_segment(aes(x = composite_pctRMSE_default, xend = composite_pctRMSE_calib,
                   yend = VARIANT), color = "gray60", linewidth = 0.6) +
  geom_point(aes(x = composite_pctRMSE_default, color = "Default"), size = 3) +
  geom_point(aes(x = composite_pctRMSE_calib, color = "Calibrated"), size = 3) +
  scale_color_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00"),
                     name = "Approach") +
  labs(
    x = "Composite %RMSE",
    y = NULL,
    title = "Calibration Effect on Composite %RMSE",
    subtitle = "Mean of BA, TPA, QMD, VOL; CONDPROP weighted; leftward = improvement"
  ) +
  theme_pub +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "pctrmse_composite_dumbbell.png"), fig_c,
       width = 22, height = 20, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "pctrmse_composite_dumbbell.pdf"), fig_c,
       width = 22, height = 20, units = "cm", bg = "white")
cat("  Saved: pctrmse_composite_dumbbell.png/pdf\n")

# =============================================================================
# SPATIAL MAPS (require geocoded conditions)
# =============================================================================

if ("LAT" %in% names(val) && sum(!is.na(val$LAT)) > 1000) {

  val_geo <- val[!is.na(LAT) & !is.na(LON) & !is.na(composite_absPctErr_calib)]
  conus <- val_geo[VARIANT != "AK"]
  cat("\nGeocoded CONUS conditions for maps:", nrow(conus), "\n")

  # US basemap
  us_states <- NULL
  if (requireNamespace("sf", quietly = TRUE) && requireNamespace("maps", quietly = TRUE)) {
    us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  }

  make_basemap <- function(p) {
    if (!is.null(us_states)) {
      p <- p + geom_sf(data = us_states, fill = "gray97", color = "gray70",
                        linewidth = 0.2, inherit.aes = FALSE)
    }
    p + coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326)
  }

  # =========================================================================
  # FIGURE D: Spatial hex of composite |%Error| (calibrated)
  # =========================================================================
  cat("Figure D: Spatial composite |%Error| hex map (calibrated)...\n")

  p_d <- ggplot(conus, aes(x = LON, y = LAT))
  p_d <- make_basemap(p_d)
  p_d <- p_d +
    stat_summary_hex(aes(z = composite_absPctErr_calib, fill = after_stat(value)),
                     fun = function(z) median(z, na.rm = TRUE),
                     bins = 80, alpha = 0.85) +
    scale_fill_viridis_c(
      option = "inferno", direction = -1,
      name = "Median composite |%Error|",
      limits = c(0, 60), oob = squish
    ) +
    labs(
      title = "Calibrated FVS: Composite |%Error| (BA + TPA + QMD + VOL)",
      subtitle = "Median per hex, weighted equally across attributes; lower = better",
      x = "Longitude", y = "Latitude"
    ) +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"))

  ggsave(file.path(fig_dir, "pctrmse_composite_spatial_calibrated.png"), p_d,
         width = 26, height = 16, units = "cm", dpi = 300, bg = "white")
  cat("  Saved: pctrmse_composite_spatial_calibrated.png\n")

  # =========================================================================
  # FIGURE E: Spatial hex of composite |%Error| REDUCTION
  # =========================================================================
  cat("Figure E: Spatial composite |%Error| reduction...\n")

  p_e <- ggplot(conus[!is.na(composite_improvement)], aes(x = LON, y = LAT))
  p_e <- make_basemap(p_e)
  p_e <- p_e +
    stat_summary_hex(aes(z = composite_improvement, fill = after_stat(value)),
                     fun = function(z) median(z, na.rm = TRUE),
                     bins = 80, alpha = 0.85) +
    scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
      name = "Median composite |%Error| reduction",
      limits = c(-20, 20), oob = squish
    ) +
    labs(
      title = "Where does calibration improve relative accuracy? (Composite metric)",
      subtitle = "Blue = calibration reduces |%Error|; Red = default was better; all 4 attributes",
      x = "Longitude", y = "Latitude"
    ) +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"))

  ggsave(file.path(fig_dir, "pctrmse_composite_reduction_spatial.png"), p_e,
         width = 26, height = 16, units = "cm", dpi = 300, bg = "white")
  cat("  Saved: pctrmse_composite_reduction_spatial.png\n")

  # =========================================================================
  # FIGURE F: Spatial hex of composite %Bias (calibrated, signed)
  # =========================================================================
  cat("Figure F: Spatial composite %Bias (calibrated)...\n")

  # Composite signed %error
  conus[, composite_pctBias_calib := {
    w <- numeric(0); e <- numeric(0)
    if (!is.na(BA_pctErr_calib))  { w <- c(w, ATTR_WEIGHTS["BA"]);  e <- c(e, BA_pctErr_calib) }
    if (!is.na(TPA_pctErr_calib)) { w <- c(w, ATTR_WEIGHTS["TPA"]); e <- c(e, TPA_pctErr_calib) }
    if (!is.na(QMD_pctErr_calib)) { w <- c(w, ATTR_WEIGHTS["QMD"]); e <- c(e, QMD_pctErr_calib) }
    if (!is.na(VOL_pctErr_calib)) { w <- c(w, ATTR_WEIGHTS["VOL"]); e <- c(e, VOL_pctErr_calib) }
    if (length(w) > 0) sum(w * e) / sum(w) else NA_real_
  }, by = seq_len(nrow(conus))]

  p_f <- ggplot(conus[!is.na(composite_pctBias_calib)], aes(x = LON, y = LAT))
  p_f <- make_basemap(p_f)
  p_f <- p_f +
    stat_summary_hex(aes(z = composite_pctBias_calib, fill = after_stat(value)),
                     fun = function(z) median(z, na.rm = TRUE),
                     bins = 80, alpha = 0.85) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
      name = "Median composite %Bias",
      limits = c(-30, 30), oob = squish
    ) +
    labs(
      title = "Spatial pattern of calibrated residuals (composite %Bias)",
      subtitle = "Blue = underprediction, Red = overprediction; mean of BA, TPA, QMD, VOL %errors",
      x = "Longitude", y = "Latitude"
    ) +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"))

  ggsave(file.path(fig_dir, "pctbias_composite_spatial.png"), p_f,
         width = 26, height = 16, units = "cm", dpi = 300, bg = "white")
  cat("  Saved: pctbias_composite_spatial.png\n")

  # =========================================================================
  # FIGURE G: Variant centroid map of composite %RMSE reduction
  # =========================================================================
  cat("Figure G: Variant centroid map (composite %RMSE reduction)...\n")

  variant_spatial <- conus[, .(
    LAT = mean(LAT, na.rm = TRUE),
    LON = mean(LON, na.rm = TRUE),
    n = .N,
    mean_obs_BA = wmean(BA_t2, WEIGHT_ACRES),
    comp_pctRMSE_calib = wpctrmse(
      {  # inline composite %error
        w <- numeric(0); e <- numeric(0)
        if (sum(!is.na(BA_pctErr_calib)) > 10)  { e <- c(e, wpctrmse(BA_pctErr_calib, WEIGHT_ACRES)) }
        if (sum(!is.na(TPA_pctErr_calib)) > 10) { e <- c(e, wpctrmse(TPA_pctErr_calib, WEIGHT_ACRES)) }
        if (sum(!is.na(QMD_pctErr_calib)) > 10) { e <- c(e, wpctrmse(QMD_pctErr_calib, WEIGHT_ACRES)) }
        if (sum(!is.na(VOL_pctErr_calib)) > 10) { e <- c(e, wpctrmse(VOL_pctErr_calib, WEIGHT_ACRES)) }
        mean(e, na.rm = TRUE)
      },
      rep(1, .N)  # dummy weights (already weighted inside)
    )
  ), by = VARIANT]

  # Simpler approach: just merge from the variant-level table
  variant_spatial2 <- merge(
    conus[, .(LAT = mean(LAT, na.rm = TRUE), LON = mean(LON, na.rm = TRUE), n = .N), by = VARIANT],
    pctrmse_variant[, .(VARIANT, composite_pctRMSE_calib, composite_pctRMSE_default,
                         composite_pctRMSE_reduction)],
    by = "VARIANT"
  )

  p_g <- ggplot(variant_spatial2, aes(x = LON, y = LAT))
  p_g <- make_basemap(p_g)
  p_g <- p_g +
    geom_point(aes(size = n, fill = composite_pctRMSE_reduction),
               shape = 21, color = "black", alpha = 0.9, stroke = 0.5) +
    geom_text(aes(label = sprintf("%s\n%.0f%%", VARIANT, composite_pctRMSE_reduction)),
              size = 2.5, fontface = "bold",
              nudge_y = 1.2, check_overlap = TRUE, lineheight = 0.85) +
    scale_fill_gradient2(
      low = "#B2182B", mid = "#FFFFBF", high = "#2166AC", midpoint = 0,
      name = "Composite %RMSE reduction",
      limits = c(-15, 35), oob = squish
    ) +
    scale_size_continuous(name = "n conditions", range = c(3, 16),
                          breaks = c(1000, 10000, 50000, 100000, 200000)) +
    labs(
      title = "FVS Variant: Composite %RMSE Reduction from Calibration",
      subtitle = "Mean of BA + TPA + QMD + VOL; CONDPROP-weighted; positive = calibration improved",
      x = "Longitude", y = "Latitude"
    ) +
    theme_pub +
    theme(legend.position = "right")

  ggsave(file.path(fig_dir, "pctrmse_composite_variant_map.png"), p_g,
         width = 28, height = 18, units = "cm", dpi = 300, bg = "white")
  cat("  Saved: pctrmse_composite_variant_map.png\n")

  # =========================================================================
  # FIGURE H: Composite 2x2 Hero Map
  # =========================================================================
  cat("Figure H: Composite 2x2 hero map...\n")

  p_d_s <- p_d +
    labs(title = "A) Calibrated Composite |%Error|", subtitle = NULL) +
    theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9),
          axis.text = element_text(size = 8), legend.title = element_text(size = 8),
          legend.text = element_text(size = 7), legend.key.width = unit(1.5, "cm"))

  p_e_s <- p_e +
    labs(title = "B) Composite |%Error| Reduction", subtitle = NULL) +
    theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9),
          axis.text = element_text(size = 8), legend.title = element_text(size = 8),
          legend.text = element_text(size = 7), legend.key.width = unit(1.5, "cm"))

  p_f_s <- p_f +
    labs(title = "C) Calibrated Composite %Bias", subtitle = NULL) +
    theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9),
          axis.text = element_text(size = 8), legend.title = element_text(size = 8),
          legend.text = element_text(size = 7), legend.key.width = unit(1.5, "cm"))

  p_g_s <- p_g +
    labs(title = "D) Composite %RMSE Reduction by Variant", subtitle = NULL) +
    theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9),
          axis.text = element_text(size = 8), legend.title = element_text(size = 8),
          legend.text = element_text(size = 7))

  fig_h <- (p_d_s + p_e_s) / (p_f_s + p_g_s) +
    plot_annotation(
      title = "Spatial Benchmark: Composite Normalized Performance (BA + TPA + QMD + VOL)",
      subtitle = "All metrics as %RMSE; weighted by represented forestland acres (CONDPROP x expansion); equal weight across attributes",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray30")
      )
    )

  ggsave(file.path(fig_dir, "pctrmse_composite_hero.png"), fig_h,
         width = 36, height = 28, units = "cm", dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, "pctrmse_composite_hero.pdf"), fig_h,
         width = 36, height = 28, units = "cm", bg = "white")
  cat("  Saved: pctrmse_composite_hero.png/pdf\n")

} else {
  cat("\nSkipping spatial maps: need geocoded conditions.\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("23_pctrmse_benchmark_maps.R COMPLETE (v2: composite + CONDPROP)\n")
cat(strrep("=", 70), "\n")
cat("\nFigures saved to:", fig_dir, "\n")
cat("Tables saved to:", table_dir, "\n")
cat("\nKey outputs:\n")
cat("  1. pctrmse_composite_by_variant.png   -- Composite %RMSE bar chart\n")
cat("  2. pctrmse_per_attribute.png          -- Per-attribute %RMSE (BA, TPA, QMD, VOL)\n")
cat("  3. pctrmse_composite_dumbbell.png     -- Composite %RMSE reduction dumbbell\n")
cat("  4. pctrmse_composite_spatial_calibrated.png -- Hex map composite |%Error|\n")
cat("  5. pctrmse_composite_reduction_spatial.png  -- Hex map composite reduction\n")
cat("  6. pctbias_composite_spatial.png      -- Hex map composite %Bias\n")
cat("  7. pctrmse_composite_variant_map.png  -- Variant centroid composite map\n")
cat("  8. pctrmse_composite_hero.png/pdf     -- 2x2 hero composite figure\n")
cat("  9. fia_benchmark_composite_pctrmse.csv -- Full summary table\n")
cat("\nDone.\n")

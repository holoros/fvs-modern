# =============================================================================
# Title: Spatial Performance Analysis of Calibrated vs Default FVS
# Author: A. Weiskittel
# Date: 2026-04-07
# Description: Generates spatial heatmaps showing where Bayesian calibration
#   improves FVS predictions most, and relates performance to ClimateSI,
#   SDIMAX, and other landscape factors.
# Dependencies: validation_data.csv, plot_raster_lookup.csv, US states shapefile
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggsci)
library(sf)
library(patchwork)

# --- Configuration -----------------------------------------------------------
project_root <- Sys.getenv("FVS_PROJECT_ROOT")
if (project_root == "") project_root <- getwd()

output_root  <- file.path(project_root, "calibration/output/comparisons")
fig_dir      <- file.path(output_root, "manuscript_figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Publication theme -------------------------------------------------------
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# --- Data --------------------------------------------------------------------
cat("Loading validation data...\n")
val <- fread(file.path(output_root, "intermediate/validation_data.csv"))
cat("  Validation records:", nrow(val), "\n")

cat("Loading raster lookup (coordinates + ClimateSI + SDIMAX)...\n")
raster_lookup <- fread(
  file.path(project_root, "calibration/data/plot_raster_lookup.csv"),
  colClasses = c(PLT_CN = "character")
)
cat("  Raster lookup records:", nrow(raster_lookup), "\n")

# Join coordinates to validation data via PLT_CN_t2
val[, PLT_CN_t2 := as.character(PLT_CN_t2)]
raster_lookup[, PLT_CN := as.character(PLT_CN)]
val <- merge(val, raster_lookup[, .(PLT_CN, LAT, LON)],
             by.x = "PLT_CN_t2", by.y = "PLT_CN", all.x = TRUE)

# If ClimateSI_ft already in val from benchmark, keep it; otherwise join
if (!"ClimateSI_ft" %in% names(val)) {
  val <- merge(val, raster_lookup[, .(PLT_CN, ClimateSI_ft)],
               by.x = "PLT_CN_t2", by.y = "PLT_CN", all.x = TRUE)
}

n_with_coords <- sum(!is.na(val$LAT) & !is.na(val$LON))
cat("  Records with coordinates:", n_with_coords, "/", nrow(val), "\n")

# Filter to records with valid coordinates and predictions
val <- val[!is.na(LAT) & !is.na(LON) & !is.na(BA_pred_calib) & !is.na(BA_pred_default)]

# --- Compute per-condition performance metrics --------------------------------
cat("Computing per-condition error metrics...\n")

# Absolute errors
val[, BA_ae_calib   := abs(BA_pred_calib - BA_t2)]
val[, BA_ae_default := abs(BA_pred_default - BA_t2)]
val[, BA_improvement := BA_ae_default - BA_ae_calib]  # positive = calibration better
val[, BA_improvement_pct := fifelse(BA_ae_default > 0,
  100 * (BA_ae_default - BA_ae_calib) / BA_ae_default, 0)]

# Signed errors (bias)
val[, BA_error_calib   := BA_pred_calib - BA_t2]
val[, BA_error_default := BA_pred_default - BA_t2]

# Volume errors (where available)
val[, VOL_ae_calib   := abs(VOL_CFGRS_pred_calib - VOL_CFGRS_t2)]
val[, VOL_ae_default := abs(VOL_CFGRS_pred_default - VOL_CFGRS_t2)]
val[, VOL_improvement := VOL_ae_default - VOL_ae_calib]
val[, VOL_improvement_pct := fifelse(VOL_ae_default > 0,
  100 * (VOL_ae_default - VOL_ae_calib) / VOL_ae_default, 0)]
val[, VOL_error_calib   := VOL_CFGRS_pred_calib - VOL_CFGRS_t2]
val[, VOL_error_default := VOL_CFGRS_pred_default - VOL_CFGRS_t2]

# Binary: did calibration improve this condition?
val[, calib_wins_BA  := BA_ae_calib < BA_ae_default]
val[, calib_wins_VOL := VOL_ae_calib < VOL_ae_default]

# --- US States basemap -------------------------------------------------------
cat("Loading US states basemap...\n")
# Use maps package for simple US outline
if (requireNamespace("maps", quietly = TRUE)) {
  us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
} else {
  us_states <- NULL
  cat("  Warning: 'maps' package not available; proceeding without basemap\n")
}

# Separate Alaska (mapped separately) from CONUS
ak_val <- val[VARIANT == "ak"]
conus_val <- val[VARIANT != "ak"]
cat("  CONUS conditions:", nrow(conus_val), "| Alaska conditions:", nrow(ak_val), "\n")

# --- Spatial binning for heatmap (hexagonal) ----------------------------------
cat("Generating spatial heatmaps...\n")

# Convert to sf for spatial ops
conus_sf <- st_as_sf(conus_val, coords = c("LON", "LAT"), crs = 4326)

# ===========================================================================
# FIGURE S1: Hexbin map of calibration improvement (BA)
# ===========================================================================
cat("Figure S1: Spatial BA improvement hexbin map...\n")

p_hex_ba <- ggplot(conus_val, aes(x = LON, y = LAT))
if (!is.null(us_states)) {
  p_hex_ba <- p_hex_ba +
    geom_sf(data = us_states, fill = "gray95", color = "gray70",
            linewidth = 0.2, inherit.aes = FALSE)
}
p_hex_ba <- p_hex_ba +
  stat_summary_hex(aes(z = BA_improvement, fill = after_stat(value)),
                   fun = mean, bins = 80, alpha = 0.85) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
    name = expression("Mean BA improvement (ft"^2~"ac"^{-1}*")"),
    limits = c(-30, 30), oob = scales::squish
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
  labs(title = "Where does Bayesian calibration improve FVS most?",
       subtitle = "Blue = calibration better, Red = default better",
       x = "Longitude", y = "Latitude") +
  theme_pub +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

ggsave(file.path(fig_dir, "S1_spatial_ba_improvement_hex.png"), p_hex_ba,
       width = 24, height = 16, units = "cm", dpi = 300, bg = "white")

# ===========================================================================
# FIGURE S2: Win rate map (% of conditions where calibration is better)
# ===========================================================================
cat("Figure S2: Calibration win rate hexbin map...\n")

p_hex_win <- ggplot(conus_val, aes(x = LON, y = LAT))
if (!is.null(us_states)) {
  p_hex_win <- p_hex_win +
    geom_sf(data = us_states, fill = "gray95", color = "gray70",
            linewidth = 0.2, inherit.aes = FALSE)
}
p_hex_win <- p_hex_win +
  stat_summary_hex(aes(z = as.numeric(calib_wins_BA), fill = after_stat(value) * 100),
                   fun = mean, bins = 80, alpha = 0.85) +
  scale_fill_gsea(name = "Calibration win rate (%)") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
  labs(title = "Calibration win rate by location",
       subtitle = "% of conditions where calibrated BA is closer to observed",
       x = "Longitude", y = "Latitude") +
  theme_pub +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

ggsave(file.path(fig_dir, "S2_spatial_calib_winrate_hex.png"), p_hex_win,
       width = 24, height = 16, units = "cm", dpi = 300, bg = "white")

# ===========================================================================
# FIGURE S3: By-variant performance map (faceted)
# ===========================================================================
cat("Figure S3: By-variant spatial performance...\n")

# Aggregate by variant + spatial hex is too complex for facets; use variant centroids
variant_perf <- conus_val[, .(
  LAT = mean(LAT, na.rm = TRUE),
  LON = mean(LON, na.rm = TRUE),
  n = .N,
  BA_RMSE_calib   = sqrt(mean((BA_pred_calib - BA_t2)^2, na.rm = TRUE)),
  BA_RMSE_default = sqrt(mean((BA_pred_default - BA_t2)^2, na.rm = TRUE)),
  BA_r2_calib     = {ss_r <- sum((BA_t2 - BA_pred_calib)^2); ss_t <- sum((BA_t2 - mean(BA_t2))^2);
                     if (ss_t > 0) max(1 - ss_r/ss_t, 0) else NA_real_},
  BA_r2_default   = {ss_r <- sum((BA_t2 - BA_pred_default)^2); ss_t <- sum((BA_t2 - mean(BA_t2))^2);
                     if (ss_t > 0) max(1 - ss_r/ss_t, 0) else NA_real_},
  win_rate = 100 * mean(BA_ae_calib < BA_ae_default, na.rm = TRUE),
  mean_improvement = mean(BA_ae_default - BA_ae_calib, na.rm = TRUE),
  mean_ClimateSI = mean(ClimateSI_ft, na.rm = TRUE)
), by = VARIANT]

variant_perf[, RMSE_reduction := 100 * (1 - BA_RMSE_calib / BA_RMSE_default)]

p_variant_map <- ggplot(variant_perf, aes(x = LON, y = LAT))
if (!is.null(us_states)) {
  p_variant_map <- p_variant_map +
    geom_sf(data = us_states, fill = "gray95", color = "gray70",
            linewidth = 0.2, inherit.aes = FALSE)
}
p_variant_map <- p_variant_map +
  geom_point(aes(size = n, color = RMSE_reduction), alpha = 0.85) +
  geom_text(aes(label = VARIANT), size = 2.5, fontface = "bold",
            nudge_y = 0.8, check_overlap = TRUE) +
  scale_color_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
    name = "BA RMSE reduction (%)"
  ) +
  scale_size_continuous(name = "n conditions", range = c(3, 15)) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
  labs(title = "FVS variant performance: Calibrated RMSE improvement",
       x = "Longitude", y = "Latitude") +
  theme_pub +
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "S3_variant_performance_map.png"), p_variant_map,
       width = 26, height = 16, units = "cm", dpi = 300, bg = "white")

# ===========================================================================
# FIGURE S4: ClimateSI relationship with calibration improvement
# ===========================================================================
cat("Figure S4: ClimateSI vs calibration improvement...\n")

si_plot <- conus_val[!is.na(ClimateSI_ft) & ClimateSI_ft > 0]
if (nrow(si_plot) > 0) {
  # Bin ClimateSI into deciles for cleaner visualization
  si_plot[, SI_bin := cut(ClimateSI_ft,
    breaks = quantile(ClimateSI_ft, probs = seq(0, 1, 0.1), na.rm = TRUE),
    include.lowest = TRUE, labels = FALSE)]
  si_plot[, SI_bin_label := cut(ClimateSI_ft,
    breaks = quantile(ClimateSI_ft, probs = seq(0, 1, 0.1), na.rm = TRUE),
    include.lowest = TRUE, dig.lab = 3)]

  # Aggregate by SI decile
  si_agg <- si_plot[, .(
    mean_SI = mean(ClimateSI_ft, na.rm = TRUE),
    BA_RMSE_calib   = sqrt(mean((BA_pred_calib - BA_t2)^2, na.rm = TRUE)),
    BA_RMSE_default = sqrt(mean((BA_pred_default - BA_t2)^2, na.rm = TRUE)),
    mean_improvement = mean(BA_ae_default - BA_ae_calib, na.rm = TRUE),
    win_rate = 100 * mean(BA_ae_calib < BA_ae_default, na.rm = TRUE),
    n = .N
  ), by = SI_bin]
  si_agg[, RMSE_reduction := 100 * (1 - BA_RMSE_calib / BA_RMSE_default)]

  # Panel A: Scatter of ClimateSI vs improvement
  pa <- ggplot(si_plot[sample(.N, min(.N, 20000))],
               aes(x = ClimateSI_ft, y = BA_improvement)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.05, size = 0.5, color = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, color = "#D55E00", linewidth = 0.8) +
    labs(x = "ClimateSI (ft)", y = expression("BA improvement (ft"^2~"ac"^{-1}*")"),
         title = "A) Site productivity vs calibration improvement") +
    theme_pub

  # Panel B: RMSE reduction by SI decile
  pb <- ggplot(si_agg, aes(x = mean_SI, y = RMSE_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_col(fill = "steelblue", alpha = 0.7, width = 3) +
    geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.3, size = 2.8) +
    labs(x = "Mean ClimateSI (ft)", y = "BA RMSE reduction (%)",
         title = "B) RMSE reduction by site productivity decile") +
    theme_pub

  # Panel C: Win rate by SI decile
  pc <- ggplot(si_agg, aes(x = mean_SI, y = win_rate)) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = n), color = "#0072B2") +
    geom_line(color = "#0072B2", linewidth = 0.5) +
    scale_size_continuous(name = "n", range = c(2, 8)) +
    labs(x = "Mean ClimateSI (ft)", y = "Calibration win rate (%)",
         title = "C) Win rate by site productivity decile") +
    theme_pub

  p_si <- pa / (pb + pc) +
    plot_annotation(
      title = "Calibration performance as a function of site productivity (ClimateSI)",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )

  ggsave(file.path(fig_dir, "S4_climatesi_vs_improvement.png"), p_si,
         width = 24, height = 22, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# FIGURE S5: Spatial map of ClimateSI colored by calibration improvement
# ===========================================================================
cat("Figure S5: ClimateSI spatial + improvement bivariate...\n")

if (nrow(si_plot) > 0) {
  # Create bivariate categories: high/low SI x better/worse calibration
  si_plot[, SI_class := fifelse(ClimateSI_ft >= median(ClimateSI_ft, na.rm = TRUE),
                                 "High SI", "Low SI")]
  si_plot[, perf_class := fifelse(BA_improvement > 0,
                                    "Calibration better", "Default better")]
  si_plot[, bivar_class := paste(SI_class, "|", perf_class)]

  bivar_colors <- c(
    "High SI | Calibration better" = "#2166AC",
    "High SI | Default better"     = "#67A9CF",
    "Low SI | Calibration better"  = "#D1E5F0",
    "Low SI | Default better"      = "#EF8A62"
  )

  p_bivar <- ggplot(si_plot, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_bivar <- p_bivar +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_bivar <- p_bivar +
    geom_point(aes(color = bivar_class), alpha = 0.15, size = 0.5) +
    scale_color_manual(values = bivar_colors, name = "") +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "Bivariate map: Site productivity x Calibration improvement",
         x = "Longitude", y = "Latitude") +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    theme_pub +
    theme(legend.position = "bottom")

  ggsave(file.path(fig_dir, "S5_bivariate_si_improvement_map.png"), p_bivar,
         width = 24, height = 16, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# FIGURE S6: SDIMAX raster relationship with performance
# ===========================================================================
if ("SDIMAX_imperial" %in% names(val)) {
  sdi_plot <- val[!is.na(SDIMAX_imperial) & SDIMAX_imperial > 0 &
                    !is.na(BA_improvement)]
  if (nrow(sdi_plot) > 100) {
    cat("Figure S6: SDIMAX vs calibration improvement...\n")

    sdi_plot[, SDI_bin := cut(SDIMAX_imperial,
      breaks = quantile(SDIMAX_imperial, probs = seq(0, 1, 0.1), na.rm = TRUE),
      include.lowest = TRUE, labels = FALSE)]

    sdi_agg <- sdi_plot[, .(
      mean_SDIMAX = mean(SDIMAX_imperial, na.rm = TRUE),
      RMSE_reduction = {
        rc <- sqrt(mean((BA_pred_calib - BA_t2)^2, na.rm = TRUE))
        rd <- sqrt(mean((BA_pred_default - BA_t2)^2, na.rm = TRUE))
        100 * (1 - rc / rd)
      },
      win_rate = 100 * mean(BA_ae_calib < BA_ae_default, na.rm = TRUE),
      n = .N
    ), by = SDI_bin]

    p_sdi_a <- ggplot(sdi_plot[sample(.N, min(.N, 20000))],
                 aes(x = SDIMAX_imperial, y = BA_improvement)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(alpha = 0.05, size = 0.5, color = "steelblue") +
      geom_smooth(method = "loess", se = TRUE, color = "#D55E00", linewidth = 0.8) +
      labs(x = expression("SDIMAX (trees ac"^{-1}*")"),
           y = expression("BA improvement (ft"^2~"ac"^{-1}*")"),
           title = "A) SDIMAX vs calibration improvement") +
      theme_pub

    p_sdi_b <- ggplot(sdi_agg, aes(x = mean_SDIMAX, y = RMSE_reduction)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_col(fill = "#009E73", alpha = 0.7, width = diff(range(sdi_agg$mean_SDIMAX))/12) +
      geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.3, size = 2.8) +
      labs(x = expression("Mean SDIMAX (trees ac"^{-1}*")"), y = "BA RMSE reduction (%)",
           title = "B) RMSE reduction by SDIMAX decile") +
      theme_pub

    p_sdi <- p_sdi_a + p_sdi_b +
      plot_annotation(
        title = "Calibration performance vs maximum stand density (Emmerson SDIMAX)",
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )

    ggsave(file.path(fig_dir, "S6_sdimax_vs_improvement.png"), p_sdi,
           width = 24, height = 12, units = "cm", dpi = 300, bg = "white")
  }
}

# ===========================================================================
# FIGURE S7: Residual spatial autocorrelation check
# ===========================================================================
cat("Figure S7: Residual spatial pattern (calibrated BA error)...\n")

p_resid_map <- ggplot(conus_val, aes(x = LON, y = LAT))
if (!is.null(us_states)) {
  p_resid_map <- p_resid_map +
    geom_sf(data = us_states, fill = "gray95", color = "gray70",
            linewidth = 0.2, inherit.aes = FALSE)
}
p_resid_map <- p_resid_map +
  stat_summary_hex(aes(z = BA_error_calib, fill = after_stat(value)),
                   fun = mean, bins = 80, alpha = 0.85) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
    name = expression("Mean BA bias (ft"^2~"ac"^{-1}*")"),
    limits = c(-20, 20), oob = scales::squish
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
  labs(title = "Spatial pattern of calibrated BA residuals (bias)",
       subtitle = "Blue = underprediction, Red = overprediction",
       x = "Longitude", y = "Latitude") +
  theme_pub +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

ggsave(file.path(fig_dir, "S7_spatial_ba_residual_bias.png"), p_resid_map,
       width = 24, height = 16, units = "cm", dpi = 300, bg = "white")

# ===========================================================================
# FIGURE S8: Volume improvement hexbin map (primary metric)
# ===========================================================================
cat("Figure S8: Spatial VOL improvement hexbin map (primary)...\n")

vol_conus <- conus_val[VOL_CFGRS_t2 > 0 & !is.na(VOL_ae_calib) & !is.na(VOL_ae_default)]
if (nrow(vol_conus) > 0) {
  p_hex_vol <- ggplot(vol_conus, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_hex_vol <- p_hex_vol +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_hex_vol <- p_hex_vol +
    stat_summary_hex(aes(z = VOL_improvement, fill = after_stat(value)),
                     fun = mean, bins = 80, alpha = 0.85) +
    scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
      name = expression("Mean volume improvement (ft"^3~"ac"^{-1}*")"),
      limits = c(-300, 300), oob = scales::squish
    ) +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "Where does Bayesian calibration improve FVS volume predictions?",
         subtitle = "Blue = calibration better, Red = default better (gross cubic foot volume)",
         x = "Longitude", y = "Latitude") +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

  ggsave(file.path(fig_dir, "S8_spatial_vol_improvement_hex.png"), p_hex_vol,
         width = 24, height = 16, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# FIGURE S9: Volume RMSE by hex (calibrated vs default side-by-side)
# ===========================================================================
cat("Figure S9: Volume RMSE hexbin maps (calibrated vs default)...\n")

if (nrow(vol_conus) > 0) {
  # Panel A: Calibrated volume RMSE by hex
  p_vol_rmse_c <- ggplot(vol_conus, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_vol_rmse_c <- p_vol_rmse_c +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_vol_rmse_c <- p_vol_rmse_c +
    stat_summary_hex(aes(z = (VOL_CFGRS_pred_calib - VOL_CFGRS_t2)^2,
                          fill = sqrt(after_stat(value))),
                     fun = mean, bins = 60, alpha = 0.85) +
    scale_fill_gradientn(
      colors = pal_gsea()(12),
      name = expression("RMSE (ft"^3~"ac"^{-1}*")"),
      limits = c(0, 1500), oob = scales::squish
    ) +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "A) Calibrated volume RMSE", x = "", y = "Latitude") +
    theme_pub + theme(legend.position = "right")

  # Panel B: Default volume RMSE by hex
  p_vol_rmse_d <- ggplot(vol_conus, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_vol_rmse_d <- p_vol_rmse_d +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_vol_rmse_d <- p_vol_rmse_d +
    stat_summary_hex(aes(z = (VOL_CFGRS_pred_default - VOL_CFGRS_t2)^2,
                          fill = sqrt(after_stat(value))),
                     fun = mean, bins = 60, alpha = 0.85) +
    scale_fill_gradientn(
      colors = pal_gsea()(12),
      name = expression("RMSE (ft"^3~"ac"^{-1}*")"),
      limits = c(0, 1500), oob = scales::squish
    ) +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "B) Default volume RMSE", x = "", y = "") +
    theme_pub + theme(legend.position = "right")

  p_vol_rmse_pair <- p_vol_rmse_c / p_vol_rmse_d +
    plot_annotation(
      title = "Spatial distribution of volume prediction error",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )

  ggsave(file.path(fig_dir, "S9_spatial_vol_rmse_calib_vs_default.png"), p_vol_rmse_pair,
         width = 24, height = 28, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# FIGURE S10: Volume win rate + ClimateSI relationship
# ===========================================================================
cat("Figure S10: Volume win rate hex + ClimateSI panels...\n")

if (nrow(vol_conus) > 0) {
  # Panel A: Volume win rate hexbin
  p_vol_win <- ggplot(vol_conus, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_vol_win <- p_vol_win +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_vol_win <- p_vol_win +
    stat_summary_hex(aes(z = as.numeric(calib_wins_VOL), fill = after_stat(value) * 100),
                     fun = mean, bins = 80, alpha = 0.85) +
    scale_fill_gsea(name = "Volume win rate (%)") +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "A) Calibration volume win rate by location",
         x = "Longitude", y = "Latitude") +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

  # Panel B: ClimateSI vs volume improvement
  vol_si <- vol_conus[!is.na(ClimateSI_ft) & ClimateSI_ft > 0]
  if (nrow(vol_si) > 100) {
    vol_si[, SI_bin := cut(ClimateSI_ft,
      breaks = quantile(ClimateSI_ft, probs = seq(0, 1, 0.1), na.rm = TRUE),
      include.lowest = TRUE, labels = FALSE)]

    vol_si_agg <- vol_si[, .(
      mean_SI = mean(ClimateSI_ft, na.rm = TRUE),
      VOL_RMSE_reduction = {
        rc <- sqrt(mean((VOL_CFGRS_pred_calib - VOL_CFGRS_t2)^2, na.rm = TRUE))
        rd <- sqrt(mean((VOL_CFGRS_pred_default - VOL_CFGRS_t2)^2, na.rm = TRUE))
        100 * (1 - rc / rd)
      },
      VOL_win_rate = 100 * mean(calib_wins_VOL, na.rm = TRUE),
      n = .N
    ), by = SI_bin]

    p_vol_si <- ggplot(vol_si_agg, aes(x = mean_SI, y = VOL_RMSE_reduction)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_col(fill = "steelblue", alpha = 0.7, width = 3) +
      geom_text(aes(label = sprintf("%.0f%%", VOL_win_rate)), vjust = -0.3, size = 3) +
      labs(x = "Mean ClimateSI (ft)",
           y = "Volume RMSE reduction (%)",
           title = "B) Volume RMSE reduction by site productivity (labels = win rate)") +
      theme_pub

    p_s10 <- p_vol_win + p_vol_si +
      plot_layout(widths = c(2, 1)) +
      plot_annotation(
        title = "Volume prediction improvement: spatial pattern and site productivity",
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else {
    p_s10 <- p_vol_win +
      plot_annotation(title = "Volume prediction improvement: spatial pattern")
  }

  ggsave(file.path(fig_dir, "S10_vol_winrate_and_climatesi.png"), p_s10,
         width = 30, height = 16, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# FIGURE S11: Volume residual bias spatial map
# ===========================================================================
cat("Figure S11: Volume residual bias spatial map...\n")

if (nrow(vol_conus) > 0) {
  p_vol_resid <- ggplot(vol_conus, aes(x = LON, y = LAT))
  if (!is.null(us_states)) {
    p_vol_resid <- p_vol_resid +
      geom_sf(data = us_states, fill = "gray95", color = "gray70",
              linewidth = 0.2, inherit.aes = FALSE)
  }
  p_vol_resid <- p_vol_resid +
    stat_summary_hex(aes(z = VOL_error_calib, fill = after_stat(value)),
                     fun = mean, bins = 80, alpha = 0.85) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
      name = expression("Mean volume bias (ft"^3~"ac"^{-1}*")"),
      limits = c(-500, 500), oob = scales::squish
    ) +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
    labs(title = "Spatial pattern of calibrated volume residuals (bias)",
         subtitle = "Blue = underprediction, Red = overprediction",
         x = "Longitude", y = "Latitude") +
    theme_pub +
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

  ggsave(file.path(fig_dir, "S11_spatial_vol_residual_bias.png"), p_vol_resid,
         width = 24, height = 16, units = "cm", dpi = 300, bg = "white")
}

# ===========================================================================
# Update variant performance table with volume metrics
# ===========================================================================
variant_perf_vol <- conus_val[VOL_CFGRS_t2 > 0 & !is.na(VOL_ae_calib), .(
  LAT = mean(LAT, na.rm = TRUE),
  LON = mean(LON, na.rm = TRUE),
  n = .N,
  BA_RMSE_calib   = sqrt(mean((BA_pred_calib - BA_t2)^2, na.rm = TRUE)),
  BA_RMSE_default = sqrt(mean((BA_pred_default - BA_t2)^2, na.rm = TRUE)),
  VOL_RMSE_calib   = sqrt(mean((VOL_CFGRS_pred_calib - VOL_CFGRS_t2)^2, na.rm = TRUE)),
  VOL_RMSE_default = sqrt(mean((VOL_CFGRS_pred_default - VOL_CFGRS_t2)^2, na.rm = TRUE)),
  VOL_r2_calib     = {ss_r <- sum((VOL_CFGRS_t2 - VOL_CFGRS_pred_calib)^2);
                      ss_t <- sum((VOL_CFGRS_t2 - mean(VOL_CFGRS_t2))^2);
                      if (ss_t > 0) max(1 - ss_r/ss_t, 0) else NA_real_},
  VOL_r2_default   = {ss_r <- sum((VOL_CFGRS_t2 - VOL_CFGRS_pred_default)^2);
                      ss_t <- sum((VOL_CFGRS_t2 - mean(VOL_CFGRS_t2))^2);
                      if (ss_t > 0) max(1 - ss_r/ss_t, 0) else NA_real_},
  BA_win_rate  = 100 * mean(BA_ae_calib < BA_ae_default, na.rm = TRUE),
  VOL_win_rate = 100 * mean(VOL_ae_calib < VOL_ae_default, na.rm = TRUE),
  mean_ClimateSI = mean(ClimateSI_ft, na.rm = TRUE)
), by = VARIANT]

variant_perf_vol[, BA_RMSE_reduction  := 100 * (1 - BA_RMSE_calib / BA_RMSE_default)]
variant_perf_vol[, VOL_RMSE_reduction := 100 * (1 - VOL_RMSE_calib / VOL_RMSE_default)]

# Replace original variant_perf with the enriched version
variant_perf <- variant_perf_vol

# ===========================================================================
# Summary table: variant-level spatial performance
# ===========================================================================
cat("\n", strrep("=", 60), "\n")
cat("Spatial Performance Summary by Variant\n")
cat(strrep("=", 60), "\n\n")

variant_perf <- variant_perf[order(-VOL_RMSE_reduction)]
for (i in 1:nrow(variant_perf)) {
  v <- variant_perf[i]
  cat(sprintf("  %s: n=%d | BA RMSE red=%.1f%% | VOL RMSE red=%.1f%% | VOL R2 c/d=%.3f/%.3f | VOL win=%.1f%% | SI=%.0f ft\n",
              v$VARIANT, v$n, v$BA_RMSE_reduction, v$VOL_RMSE_reduction,
              v$VOL_r2_calib, v$VOL_r2_default, v$VOL_win_rate, v$mean_ClimateSI))
}

# Save variant performance table
fwrite(variant_perf, file.path(output_root, "manuscript_tables/variant_spatial_performance.csv"))

cat("\n\nSpatial analysis complete. Figures saved to:", fig_dir, "\n")

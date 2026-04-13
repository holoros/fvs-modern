# =============================================================================
# Title: Ablation v3 Publication Figures
# Author: A. Weiskittel
# Date: 2026-04-09
# Description: Generate updated calibration figures from v3 ablation results
#   for the manuscript and GMUG slides. Includes:
#   1. Composite %RMSE bar chart by variant (calibrated vs default)
#   2. Ablation component contribution waterfall chart
#   3. Variant performance dumbbell chart
#   4. BA/Volume R-squared comparison heatmap
#   5. Spatial performance map (variant centroids)
# Dependencies: fia_benchmark_results_ablation_*.csv
# =============================================================================

# --- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(ggtext)
})

# --- Configuration -----------------------------------------------------------
project_root <- Sys.getenv("FVS_PROJECT_ROOT",
                           unset = normalizePath(file.path(dirname(
                             sys.frame(1)$ofile), "../.."), mustWork = FALSE))

tab_dir <- file.path(project_root, "calibration/output/comparisons/manuscript_tables")
fig_dir <- file.path(project_root, "calibration/output/comparisons/manuscript_figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Publication theme (consistent with r-analysis skill)
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9),
    strip.text       = element_text(size = 11, face = "bold"),
    plot.title       = element_text(size = 14, face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray30"),
    plot.margin      = margin(10, 10, 10, 10)
  )

# Variant geographic info (centroids for spatial map)
variant_info <- data.table(
  VARIANT = c("AK", "ACD", "BC", "BM", "CA", "CI", "CR", "CS", "EC", "EM",
              "IE", "KT", "LS", "NC", "NE", "OC", "ON", "OP", "PN", "SN",
              "SO", "TT", "UT", "WC", "WS"),
  region = c("Alaska", "Northeast", "Pacific NW", "Inland West", "Pacific SW",
             "Inland West", "Rocky Mountain", "Central", "Inland West", "Rocky Mountain",
             "Inland West", "Inland West", "Lake States", "Pacific SW", "Northeast",
             "Pacific NW", "Lake States", "Pacific NW", "Pacific NW", "Southeast",
             "Pacific SW", "Rocky Mountain", "Rocky Mountain", "Pacific NW", "Pacific SW"),
  lat = c(63.5, 45.0, 49.5, 44.5, 38.0, 46.5, 39.0, 40.0, 44.0, 47.0,
          46.5, 47.5, 46.0, 40.0, 43.0, 39.5, 44.5, 43.5, 46.0, 34.0,
          43.0, 40.5, 39.5, 47.5, 42.0),
  lon = c(-152, -69.0, -126, -114.5, -121, -115, -106, -89, -118, -112,
          -115, -115, -89, -123.5, -72, -122, -80, -121, -122, -83,
          -118, -111, -111.5, -122, -119)
)

# Region color palette (colorblind safe)
region_colors <- c(
  "Alaska"         = "#332288",
  "Northeast"      = "#117733",
  "Pacific NW"     = "#44AA99",
  "Pacific SW"     = "#88CCEE",
  "Inland West"    = "#DDCC77",
  "Rocky Mountain" = "#CC6677",
  "Central"        = "#AA4499",
  "Lake States"    = "#882255",
  "Southeast"      = "#E69F00"
)

# =============================================================================
# SECTION 1: Load ablation results
# =============================================================================

configs <- data.table(
  tag   = c("ablation_full", "ablation_no_csi", "ablation_no_sdi",
            "ablation_no_ig", "ablation_baseline", "ablation_baskerville"),
  label = c("Full Model", "No ClimateSI", "No SDIMAX",
            "No Ingrowth", "Baseline (Bayes only)", "Baskerville Mean DG"),
  order = 1:6
)

results_list <- list()
for (i in seq_len(nrow(configs))) {
  tag <- configs$tag[i]
  f <- file.path(tab_dir, paste0("fia_benchmark_results_", tag, ".csv"))
  if (!file.exists(f)) next
  dt <- fread(f)
  dt <- dt[VARIANT != "" & !is.na(VARIANT)]
  dt[, config := configs$label[i]]
  dt[, config_order := configs$order[i]]
  results_list[[tag]] <- dt
}

if (length(results_list) == 0) {
  cat("No ablation data found. Exiting.\n")
  quit(save = "no")
}

all_results <- rbindlist(results_list, fill = TRUE)
full <- results_list[["ablation_full"]]
cat("Loaded", length(results_list), "configs,", nrow(all_results), "total rows\n")

# =============================================================================
# FIGURE 1: Composite %RMSE by Variant (Calibrated vs Default)
# =============================================================================
cat("\nFigure 1: Composite %RMSE bar chart\n")

metrics <- c("BA", "QMD", "SDI", "VOL_CFGRS")

# Compute composite %RMSE for each variant
comp_data <- full[, {
  calib_vals <- unlist(.SD[, paste0(metrics, "_RMSE_pct_calib"), with = FALSE])
  def_vals   <- unlist(.SD[, paste0(metrics, "_RMSE_pct_default"), with = FALSE])
  list(
    pctrmse_calib   = mean(as.numeric(calib_vals), na.rm = TRUE),
    pctrmse_default = mean(as.numeric(def_vals), na.rm = TRUE),
    n_conditions    = n_conditions
  )
}, by = VARIANT]

comp_data[, reduction := pctrmse_default - pctrmse_calib]
comp_data[, pct_improvement := 100 * reduction / pctrmse_default]
setorder(comp_data, -pctrmse_default)

# Reshape for grouped bar chart
comp_long <- melt(comp_data, id.vars = c("VARIANT", "n_conditions"),
                  measure.vars = c("pctrmse_calib", "pctrmse_default"),
                  variable.name = "model", value.name = "pctrmse")
comp_long[, model := fifelse(model == "pctrmse_calib", "Calibrated (v3)", "FVS Default")]
comp_long[, VARIANT := factor(VARIANT, levels = comp_data$VARIANT)]

p1 <- ggplot(comp_long, aes(x = VARIANT, y = pctrmse, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = c("Calibrated (v3)" = "#2166AC", "FVS Default" = "#CCCCCC")) +
  labs(title = "Composite %RMSE by FVS Variant",
       subtitle = "Mean of BA, QMD, SDI, and Volume %RMSE",
       x = "FVS Variant", y = "Composite %RMSE",
       fill = NULL) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "v3_composite_pctrmse_by_variant.png"),
       p1, width = 22, height = 12, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "v3_composite_pctrmse_by_variant.pdf"),
       p1, width = 22, height = 12, units = "cm")
cat("  Saved: v3_composite_pctrmse_by_variant.png/pdf\n")

# =============================================================================
# FIGURE 2: Ablation Component Contribution Waterfall
# =============================================================================
cat("\nFigure 2: Ablation waterfall chart\n")

# Weighted average composite %RMSE across variants for each config
weighted_comp <- all_results[, {
  calib_vals <- unlist(.SD[, paste0(metrics, "_RMSE_pct_calib"), with = FALSE])
  list(pctrmse = mean(as.numeric(calib_vals), na.rm = TRUE),
       n = n_conditions)
}, by = .(config, config_order, VARIANT)]

# Weighted mean across variants
global_means <- weighted_comp[, .(
  wmean_pctrmse = weighted.mean(pctrmse, n, na.rm = TRUE)
), by = .(config, config_order)]
setorder(global_means, config_order)

# Also get default FVS %RMSE
default_pctrmse <- full[, {
  def_vals <- unlist(.SD[, paste0(metrics, "_RMSE_pct_default"), with = FALSE])
  list(pctrmse = mean(as.numeric(def_vals), na.rm = TRUE), n = n_conditions)
}, by = VARIANT][, weighted.mean(pctrmse, n, na.rm = TRUE)]

# Build waterfall data
waterfall <- data.table(
  component = c("FVS Default",
                "Baskerville -> Median DG",
                "+ Bayes Calibration",
                "+ Ingrowth",
                "+ ClimateSI",
                "+ SDIMAX"),
  order = 0:5
)

# The ablation configs remove one component at a time from the full model.
# Component contribution = full_pctrmse - no_component_pctrmse (negative = improvement)
bask_mean  <- global_means[config == "Baskerville Mean DG", wmean_pctrmse]
base_mean  <- global_means[config == "Baseline (Bayes only)", wmean_pctrmse]
no_ig_mean <- global_means[config == "No Ingrowth", wmean_pctrmse]
no_csi_mean <- global_means[config == "No ClimateSI", wmean_pctrmse]
no_sdi_mean <- global_means[config == "No SDIMAX", wmean_pctrmse]
full_mean  <- global_means[config == "Full Model", wmean_pctrmse]

waterfall[, value := c(
  default_pctrmse,
  -(default_pctrmse - bask_mean),      # Baskerville fix contribution
  -(bask_mean - base_mean),             # Bayes calibration
  -(base_mean - no_ig_mean),            # Approximate ingrowth from no_ig
  -(no_ig_mean - no_csi_mean),          # Approximate ClimateSI
  -(no_csi_mean - full_mean)            # Approximate SDIMAX
)]

waterfall[, cumulative := cumsum(c(value[1], value[-1]))]
waterfall[, ystart := c(0, head(cumulative, -1))]
waterfall[, yend := cumulative]
waterfall[, fill_type := fifelse(order == 0, "baseline",
                         fifelse(value < 0, "improvement", "degradation"))]
waterfall[, component := factor(component, levels = component)]

p2 <- ggplot(waterfall, aes(x = component)) +
  geom_rect(aes(xmin = as.numeric(component) - 0.35,
                xmax = as.numeric(component) + 0.35,
                ymin = ystart, ymax = yend,
                fill = fill_type)) +
  geom_text(aes(y = yend, label = sprintf("%.1f%%", value)),
            vjust = ifelse(waterfall$value < 0, 1.3, -0.3), size = 3.2) +
  scale_fill_manual(values = c("baseline" = "#999999",
                                "improvement" = "#2166AC",
                                "degradation" = "#B2182B"),
                    guide = "none") +
  labs(title = "v3 Ablation: Component Contributions to %RMSE Reduction",
       subtitle = "Weighted mean composite %RMSE across all variants",
       x = NULL, y = "Composite %RMSE") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(fig_dir, "v3_ablation_waterfall.png"),
       p2, width = 20, height = 12, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "v3_ablation_waterfall.pdf"),
       p2, width = 20, height = 12, units = "cm")
cat("  Saved: v3_ablation_waterfall.png/pdf\n")

# =============================================================================
# FIGURE 3: Dumbbell Chart (Improvement per Variant per Metric)
# =============================================================================
cat("\nFigure 3: Dumbbell chart\n")

metric_labels <- c(BA = "Basal Area", QMD = "QMD", SDI = "SDI",
                   VOL_CFGRS = "Volume (gross)")

dumbbell_data <- list()
for (m in metrics) {
  dt <- full[, .(
    VARIANT,
    metric = m,
    metric_label = metric_labels[m],
    calib = get(paste0(m, "_RMSE_pct_calib")),
    default = get(paste0(m, "_RMSE_pct_default"))
  )]
  dumbbell_data[[m]] <- dt
}
dumbbell_df <- rbindlist(dumbbell_data)
dumbbell_df[, metric_label := factor(metric_label, levels = metric_labels)]

# Order variants by mean improvement
var_order <- dumbbell_df[, .(mean_reduction = mean(default - calib, na.rm = TRUE)),
                          by = VARIANT]
setorder(var_order, mean_reduction)
dumbbell_df[, VARIANT := factor(VARIANT, levels = var_order$VARIANT)]

p3 <- ggplot(dumbbell_df, aes(y = VARIANT)) +
  geom_segment(aes(x = calib, xend = default, yend = VARIANT),
               color = "gray60", linewidth = 0.5) +
  geom_point(aes(x = default), color = "#CCCCCC", size = 2.5) +
  geom_point(aes(x = calib), color = "#2166AC", size = 2.5) +
  facet_wrap(~metric_label, scales = "free_x", nrow = 1) +
  labs(title = "Calibrated vs Default: %RMSE Improvement by Variant",
       subtitle = "Blue = calibrated (v3), gray = FVS default",
       x = "%RMSE", y = NULL) +
  theme_pub +
  theme(panel.spacing = unit(1, "lines"))

ggsave(file.path(fig_dir, "v3_dumbbell_pctrmse.png"),
       p3, width = 28, height = 18, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "v3_dumbbell_pctrmse.pdf"),
       p3, width = 28, height = 18, units = "cm")
cat("  Saved: v3_dumbbell_pctrmse.png/pdf\n")

# =============================================================================
# FIGURE 4: R-squared Heatmap (Variant x Metric)
# =============================================================================
cat("\nFigure 4: R-squared heatmap\n")

r2_data <- list()
for (m in metrics) {
  dt <- full[, .(
    VARIANT,
    metric = metric_labels[m],
    r2_calib = get(paste0(m, "_r2_calib")),
    r2_default = get(paste0(m, "_r2_default"))
  )]
  r2_data[[m]] <- dt
}
r2_df <- rbindlist(r2_data)

# Improvement in R2
r2_df[, r2_improvement := r2_calib - r2_default]

p4a <- ggplot(r2_df, aes(x = metric, y = VARIANT, fill = r2_calib)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.3f", r2_calib)), size = 2.8) +
  scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "A) Calibrated R\u00b2", x = NULL, y = NULL, fill = "R\u00b2") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4b <- ggplot(r2_df, aes(x = metric, y = VARIANT, fill = r2_improvement)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.3f", r2_improvement)), size = 2.8) +
  scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                       midpoint = 0, limits = c(-0.1, 0.1)) +
  labs(title = "B) R\u00b2 Improvement (v3 minus default)", x = NULL, y = NULL,
       fill = "\u0394R\u00b2") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4 <- p4a + p4b + plot_layout(widths = c(1, 1))

ggsave(file.path(fig_dir, "v3_r2_heatmap.png"),
       p4, width = 30, height = 18, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "v3_r2_heatmap.pdf"),
       p4, width = 30, height = 18, units = "cm")
cat("  Saved: v3_r2_heatmap.png/pdf\n")

# =============================================================================
# FIGURE 5: Spatial Performance Map (Variant Centroids)
# =============================================================================
cat("\nFigure 5: Spatial performance map\n")

# Merge variant info with performance
spatial_data <- merge(comp_data, variant_info, by = "VARIANT", all.x = TRUE)
spatial_data <- spatial_data[!is.na(lat)]  # Drop variants without coordinates

# Get US map
if (requireNamespace("maps", quietly = TRUE)) {
  us_map <- ggplot2::map_data("state")

  p5 <- ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = "gray95", color = "gray70", linewidth = 0.2) +
    geom_point(data = spatial_data,
               aes(x = lon, y = lat, size = n_conditions,
                   color = pct_improvement),
               alpha = 0.85) +
    geom_text(data = spatial_data,
              aes(x = lon, y = lat + 0.8, label = VARIANT),
              size = 2.5, fontface = "bold") +
    scale_color_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                          midpoint = median(spatial_data$pct_improvement, na.rm = TRUE),
                          name = "% Improvement\nin composite\n%RMSE") +
    scale_size_continuous(range = c(3, 12), labels = comma,
                          name = "FIA conditions") +
    coord_fixed(ratio = 1.3, xlim = c(-125, -65), ylim = c(25, 50)) +
    labs(title = "FVS Calibration v3: Spatial Performance",
         subtitle = "Point size = sample size, color = % improvement over default") +
    theme_pub +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "right")

  ggsave(file.path(fig_dir, "v3_spatial_performance_map.png"),
         p5, width = 25, height = 15, units = "cm", dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, "v3_spatial_performance_map.pdf"),
         p5, width = 25, height = 15, units = "cm")
  cat("  Saved: v3_spatial_performance_map.png/pdf\n")
} else {
  cat("  SKIP: 'maps' package not available\n")
}

# =============================================================================
# SUMMARY STATISTICS for GMUG slides
# =============================================================================
cat("\n--- SUMMARY STATISTICS ---\n")

# Weighted averages across all variants
n_total <- sum(full$n_conditions, na.rm = TRUE)
cat("Total conditions:", formatC(n_total, format = "d", big.mark = ","), "\n")
cat("Variants:", nrow(full), "\n\n")

for (m in c("BA", "QMD", "SDI", "VOL_CFGRS")) {
  r2_c <- weighted.mean(full[[paste0(m, "_r2_calib")]], full$n_conditions, na.rm = TRUE)
  r2_d <- weighted.mean(full[[paste0(m, "_r2_default")]], full$n_conditions, na.rm = TRUE)
  rmse_c <- weighted.mean(full[[paste0(m, "_RMSE_pct_calib")]], full$n_conditions, na.rm = TRUE)
  rmse_d <- weighted.mean(full[[paste0(m, "_RMSE_pct_default")]], full$n_conditions, na.rm = TRUE)
  cat(sprintf("  %10s: R2 calib=%.3f default=%.3f | %%RMSE calib=%.1f%% default=%.1f%%\n",
              m, r2_c, r2_d, rmse_c, rmse_d))
}

# Composite
comp_calib_wt <- weighted.mean(comp_data$pctrmse_calib, comp_data$n_conditions, na.rm = TRUE)
comp_def_wt <- weighted.mean(comp_data$pctrmse_default, comp_data$n_conditions, na.rm = TRUE)
cat(sprintf("\n  Composite: calib=%.1f%% default=%.1f%% (%.1f%% improvement)\n",
            comp_calib_wt, comp_def_wt, 100 * (comp_def_wt - comp_calib_wt) / comp_def_wt))

cat("\nAll figures saved to:", fig_dir, "\n")
cat("Done.\n")

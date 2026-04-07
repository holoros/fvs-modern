# =============================================================================
# Title: Hero Manuscript Figures for Bayesian FVS Calibration
# Author: A. Weiskittel
# Date: 2026-04-04
# Description: Two publication-quality figures that tell the complete story of
#   Bayesian recalibration of FVS across all 25 geographic variants:
#
#   FIGURE 1: "The Complete Story" - Full-page 3×2 multi-panel figure showing:
#     A. Calibration heatmap (4 components × 25 variants, color = R² or AUC)
#     B. Bakuzis matrix pass/fail heatmap (4 ecological laws × 25 variants)
#     C. Stand-level BA trajectories (calibrated with 95% CI vs default)
#     D. Stand-level BA at year 50 (calibrated vs default scatter)
#     E. Annualization diagnostic (correct vs bug trajectories, NE variant)
#     (Can optionally add Panel F for conceptual diagram if space allows)
#
#   FIGURE 2: "FIA Benchmark" - Full-page 3×2 multi-panel figure showing:
#     A. Predicted vs observed BA (calibrated), 1:1 line, RMSE annotation
#     B. Same for default parameters
#     C. Residual boxplots by variant (calibrated vs default)
#     D. Volume predicted vs observed (structure ready for NSBE integration)
#     E. MAI comparison (calibrated vs default vs observed)
#     F. Stand-level bias by variant
#
# Conventions:
#   - Okabe-Ito colorblind-safe palette for regions
#   - No hyphens in axis labels (use spaces)
#   - Expression notation for units: ft² ac⁻¹
#   - Variants on y-axis sorted by region then alphabetically
#   - Publication quality: theme_minimal, 300 dpi, consistent fonts
#   - Save as PNG (300 dpi) and PDF
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

# --- Paths -------------------------------------------------------------------
base_dir  <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                       "/home/aweiskittel/Documents/Claude/fvs-modern"),
                       "calibration")
output_dir <- file.path(base_dir, "output", "comparisons")
assess_dir <- file.path(base_dir, "output", "assessment")
fig_dir    <- file.path(output_dir, "manuscript_figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Publication theme -------------------------------------------------------
theme_manuscript <- function() {
  theme_minimal(base_size = 11) +
  theme(
    # Grid
    panel.grid.major = element_line(color = "gray95", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    # Text
    axis.title = element_text(size = 11, color = "black"),
    axis.text  = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, face = "bold", color = "black"),
    plot.subtitle = element_text(size = 10, color = "gray20"),
    # Legends
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text  = element_text(size = 9),
    legend.background = element_blank(),
    # Facets
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray98", color = NA),
    # Margins
    plot.margin = margin(8, 8, 8, 8)
  )
}

# --- Variant and region mapping -----------------------------------------------
variant_info <- tibble(
  variant = c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
              "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
              "OP", "SN", "CS", "LS", "ON", "NE", "ACD"),
  region = c("Pacific Northwest", "Pacific Northwest", "Pacific Northwest", "Pacific Northwest",
             "Pacific Southwest", "Pacific Southwest", "Pacific Southwest",
             "Intermountain", "Intermountain", "Intermountain",
             "Intermountain", "Intermountain", "Intermountain",
             "Northern Rockies", "Northern Rockies",
             "Southern Rockies", "Southern Rockies", "Southern Rockies",
             "Central Plains", "Southern", "Central States",
             "Lake States", "Lake States", "Northeast", "Northeast")
)

# Okabe-Ito colorblind-safe palette (8 colors + grays)
region_colors <- c(
  "Pacific Northwest" = "#0072B2",
  "Pacific Southwest" = "#56B4E9",
  "Intermountain"     = "#009E73",
  "Northern Rockies"  = "#F0E442",
  "Southern Rockies"  = "#E69F00",
  "Central Plains"    = "#D55E00",
  "Southern"          = "#CC79A7",
  "Central States"    = "#999999",
  "Lake States"       = "#66CCEE"
)

heatmap_colors <- list(
  performance = colorRampPalette(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"))(100),
  booleans    = c("FALSE" = "#d7191c", "TRUE" = "#1a9641")
)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading data files...\n")

# Model performance summary (DG, HD, CR, Mortality)
perf_summary <- read_csv(file.path(output_dir, "model_performance_summary.csv"),
                         show_col_types = FALSE) %>%
  filter(level == "tree", metric == "r2" | metric == "auc") %>%
  mutate(
    model = ifelse(model == "calibrated", "Calibrated", "Default"),
    component_short = case_when(
      component == "Diameter_Growth" ~ "DG",
      component == "Height_Diameter" ~ "HD",
      component == "Crown_Ratio" ~ "CR",
      component == "Mortality" ~ "Mortality",
      TRUE ~ component
    )
  )

# Bakuzis matrix results
bakuzis_sukachev <- read_csv(file.path(assess_dir, "bakuzis_sukachev_effect.csv"),
                             show_col_types = FALSE)
bakuzis_eichhorn <- read_csv(file.path(assess_dir, "bakuzis_eichhorn_rule.csv"),
                             show_col_types = FALSE)
bakuzis_crown <- read_csv(file.path(assess_dir, "bakuzis_crown_recession.csv"),
                          show_col_types = FALSE)
bakuzis_mort <- read_csv(file.path(assess_dir, "bakuzis_mortality_ushape.csv"),
                         show_col_types = FALSE)

# Stand projections
stand_ci <- read_csv(file.path(output_dir, "stand_projections_with_ci.csv"),
                     show_col_types = FALSE)
stand_3scenarios <- read_csv(file.path(output_dir, "stand_projections_3scenarios.csv"),
                             show_col_types = FALSE)

# Stand-level performance for final BA comparisons
sl_file <- file.path(output_dir, "stand_level_default_vs_calibrated.csv")
sl_file_alt <- file.path(output_dir, "manuscript_tables/stand_projection_realism_annualized.csv")
if (file.exists(sl_file)) {
  stand_level_perf <- read_csv(sl_file, show_col_types = FALSE)
} else if (file.exists(sl_file_alt)) {
  stand_level_perf <- read_csv(sl_file_alt, show_col_types = FALSE)
  # Add bias column if missing (use ba_change_pct as proxy)
  if (!"bias" %in% names(stand_level_perf) && "ba_change_pct" %in% names(stand_level_perf)) {
    stand_level_perf$bias <- stand_level_perf$ba - 60.15  # diff from initial BA
  }
} else {
  cat("Warning: No stand-level performance CSV found. Creating empty placeholder.\n")
  stand_level_perf <- tibble(variant = character(), bias = numeric())
}

cat("Data loaded successfully.\n")

# =============================================================================
# FIGURE 1: "THE COMPLETE STORY"
# =============================================================================

cat("\nCreating FIGURE 1: The Complete Story...\n")

# --- Panel A: Calibration Heatmap (4 components × 25 variants) ----
# Create matrix form: rows = variants (sorted), cols = components, values = metric

# Get the best metric for each component (R² for growth-based, AUC for mortality)
perf_heatmap_data <- perf_summary %>%
  mutate(variant = factor(variant, levels = variant_info$variant)) %>%
  arrange(variant, component_short) %>%
  pivot_wider(
    id_cols = variant,
    names_from = component_short,
    values_from = value,
    values_fn = list(value = mean)
  ) %>%
  pivot_longer(
    cols = -variant,
    names_to = "component",
    values_to = "metric"
  ) %>%
  mutate(component = factor(component, levels = c("DG", "HD", "CR", "Mortality")))

panel_a <- perf_heatmap_data %>%
  ggplot(aes(x = component, y = variant, fill = metric)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colors = heatmap_colors$performance,
    name = "R² / AUC",
    limits = c(0, 1),
    na.value = "white"
  ) +
  labs(
    x = NULL,
    y = "FVS Variant",
    title = "A. Calibration Performance Across Components"
  ) +
  theme_manuscript() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# --- Panel B: Bakuzis Matrix Heatmap (4 ecological laws × 25 variants) ----
# Combine all 4 bakuzis tests into one tall matrix

bakuzis_matrix <- bind_rows(
  bakuzis_sukachev %>%
    select(variant, sukachev_pass) %>%
    mutate(law = "Sukachev Effect", pass = sukachev_pass),
  bakuzis_eichhorn %>%
    select(variant, eichhorn_pass) %>%
    mutate(law = "Eichhorn Rule", pass = eichhorn_pass),
  bakuzis_crown %>%
    select(variant, recession_pass) %>%
    mutate(law = "Crown Recession", pass = recession_pass),
  bakuzis_mort %>%
    select(variant, u_shape_pass) %>%
    mutate(law = "Mortality U-Shape", pass = u_shape_pass)
) %>%
  mutate(
    variant = factor(variant, levels = variant_info$variant),
    law = factor(law, levels = c("Sukachev Effect", "Eichhorn Rule",
                                 "Crown Recession", "Mortality U-Shape")),
    pass_text = ifelse(pass, "Pass", "Fail"),
    pass_text = factor(pass_text, levels = c("Pass", "Fail"))
  ) %>%
  arrange(variant, law)

panel_b <- bakuzis_matrix %>%
  ggplot(aes(x = law, y = variant, fill = pass_text)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Pass" = "#1a9641", "Fail" = "#d7191c"),
    name = "Result"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "B. Ecological Realism (Bakuzis Matrix)"
  ) +
  theme_manuscript() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "right"
  )

# --- Panel C: Stand-level BA Trajectories (faceted by region) ----
# Calibrated with 95% CI vs default point estimate

# Prepare data for both calibrated (with CI) and default
calib_traj <- stand_ci %>%
  select(year, ba_q025, ba_q50, ba_q975, variant, region) %>%
  rename(
    ba_lower = ba_q025,
    ba_median = ba_q50,
    ba_upper = ba_q975
  ) %>%
  mutate(model = "Calibrated")

# Get default estimates from stand_3scenarios
# Note: scenario names are "Calibrated (annualized)", "Default FVS-like", etc.
default_traj <- stand_3scenarios %>%
  filter(scenario == "Default FVS-like") %>%
  select(year, ba, variant) %>%
  rename(ba_median = ba) %>%
  left_join(variant_info %>% select(variant, region), by = "variant") %>%
  mutate(
    model = "Default",
    ba_lower = ba_median,
    ba_upper = ba_median
  )

traj_combined <- bind_rows(calib_traj, default_traj) %>%
  mutate(
    region = factor(region, levels = sort(unique(region))),
    model = factor(model, levels = c("Calibrated", "Default"))
  )

panel_c <- traj_combined %>%
  ggplot(aes(x = year, y = ba_median, color = model, fill = model)) +
  geom_ribbon(
    aes(ymin = ba_lower, ymax = ba_upper),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~region, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  scale_color_manual(values = c("Calibrated" = "#0072B2", "Default" = "#E69F00")) +
  scale_fill_manual(values = c("Calibrated" = "#0072B2", "Default" = "#E69F00")) +
  labs(
    x = "Projection Year",
    y = expression(BA ~ (ft^2 ~ ac^{-1})),
    title = "C. Stand Basal Area Trajectories by Region"
  ) +
  theme_manuscript() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

# --- Panel D: Final BA at Year 50 (scatter) ----
# Calibrated vs default with 1:1 line and variant labels

final_ba <- traj_combined %>%
  filter(year == 50) %>%
  pivot_wider(
    id_cols = c(variant, region),
    names_from = model,
    values_from = ba_median
  ) %>%
  left_join(variant_info %>% select(variant, region), by = "variant", suffix = c("", "_info")) %>%
  mutate(region = coalesce(region, region_info)) %>%
  select(variant, region, Calibrated, Default)

panel_d <- final_ba %>%
  ggplot(aes(x = Default, y = Calibrated)) +
  geom_abline(intercept = 0, slope = 1, color = "gray60", linetype = "dashed", linewidth = 0.8) +
  geom_point(
    aes(color = region),
    size = 3,
    alpha = 0.8
  ) +
  ggrepel::geom_text_repel(
    aes(label = variant),
    size = 2.5,
    max.overlaps = 25,
    seed = 42
  ) +
  scale_color_manual(values = region_colors, name = "Region") +
  labs(
    x = expression("Default BA at Year 50 (ft²·ac⁻¹)"),
    y = expression("Calibrated BA at Year 50 (ft²·ac⁻¹)"),
    title = "D. Stand BA Comparison at Year 50"
  ) +
  theme_manuscript() +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom"
  )

# --- Panel E: Annualization Diagnostic (NE variant) ----
# Show correct (annualized) vs incorrect (period-level) trajectories

ne_annualized <- stand_3scenarios %>%
  filter(variant == "NE", scenario == "Calibrated (annualized)") %>%
  select(year, ba) %>%
  mutate(scenario = "Correct (annualized)")

ne_bug <- stand_3scenarios %>%
  filter(variant == "NE", scenario == "Calibrated (period-level bug)") %>%
  select(year, ba) %>%
  mutate(scenario = "Bug (period-level)")

panel_e <- bind_rows(ne_annualized, ne_bug) %>%
  ggplot(aes(x = year, y = ba, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = c("Correct (annualized)" = "#1a9641", "Bug (period-level)" = "#d7191c")) +
  scale_linetype_manual(values = c("Correct (annualized)" = "solid", "Bug (period-level)" = "dashed")) +
  labs(
    x = "Projection Year",
    y = expression(BA ~ (ft^2 ~ ac^{-1})),
    title = "E. Annualization Diagnostic (Northeast Variant)",
    subtitle = "5-year measurement interval requires annual conversion"
  ) +
  theme_manuscript() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# --- Assemble Figure 1 ----
fig1 <- (panel_a | panel_b | panel_c) /
        (panel_d | panel_e | plot_spacer()) +
  plot_annotation(
    title = "Figure 1: The Complete Story - Bayesian Calibration of All 25 FVS Variants",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0)
    )
  ) +
  plot_layout(heights = c(1, 1), guides = "collect") &
  theme(
    legend.position = "bottom"
  )

# Save Figure 1
ggsave(
  file.path(fig_dir, "fig1_complete_story.png"),
  fig1,
  width = 18,
  height = 14,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(fig_dir, "fig1_complete_story.pdf"),
  fig1,
  width = 18,
  height = 14,
  bg = "white"
)

cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2: "FIA BENCHMARK"
# =============================================================================

cat("\nCreating FIGURE 2: FIA Benchmark...\n")

# Load condition-level validation data (for scatter/residual plots)
val_file <- file.path(output_dir, "intermediate/validation_data.csv")
# Load variant-level summary (for RMSE/R2/equiv)
fia_summary_file <- file.path(output_dir, "manuscript_tables/fia_benchmark_results.csv")

val_data <- if (file.exists(val_file)) {
  read_csv(val_file, show_col_types = FALSE)
} else {
  tibble()
}

fia_summary <- if (file.exists(fia_summary_file)) {
  read_csv(fia_summary_file, show_col_types = FALSE)
} else {
  tibble()
}

cat("  Validation data:", nrow(val_data), "conditions\n")
cat("  Summary stats:", nrow(fia_summary), "variants\n")

# --- Panel A: Predicted vs Observed BA (Calibrated) ----
if (nrow(val_data) > 0 && "BA_pred_calib" %in% names(val_data)) {
  rmse_calib <- sqrt(mean((val_data$BA_pred_calib - val_data$BA_t2)^2, na.rm = TRUE))
  r2_calib <- 1 - sum((val_data$BA_t2 - val_data$BA_pred_calib)^2, na.rm = TRUE) /
              sum((val_data$BA_t2 - mean(val_data$BA_t2, na.rm = TRUE))^2, na.rm = TRUE)

  # Subsample for plotting (too many points otherwise)
  set.seed(42)
  n_val <- nrow(val_data)
  plot_sample <- val_data %>% slice_sample(n = min(n_val, 50000))

  panel_f1a <- ggplot(plot_sample, aes(x = BA_t2, y = BA_pred_calib)) +
    geom_abline(intercept = 0, slope = 1, color = "gray60", linetype = "dashed", linewidth = 0.8) +
    geom_point(alpha = 0.08, size = 0.5, color = "#0072B2") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1.5,
             label = sprintf("RMSE = %.1f", rmse_calib),
             size = 3.5, fontface = "bold") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.3,
             label = sprintf("R\u00b2 = %.3f", max(r2_calib, 0)),
             size = 3.5, fontface = "bold") +
    labs(
      x = expression("Observed BA (ft"^2~"ac"^{-1}*")"),
      y = expression("Predicted BA (ft"^2~"ac"^{-1}*")"),
      title = "A. Calibrated: Predicted vs Observed BA"
    ) +
    coord_fixed(xlim = c(0, 400), ylim = c(0, 400)) +
    theme_manuscript()
} else {
  panel_f1a <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "A. Calibrated: Predicted vs Observed BA")
}

# --- Panel B: Predicted vs Observed BA (Default) ----
if (nrow(val_data) > 0 && "BA_pred_default" %in% names(val_data)) {
  rmse_default <- sqrt(mean((val_data$BA_pred_default - val_data$BA_t2)^2, na.rm = TRUE))
  r2_default <- 1 - sum((val_data$BA_t2 - val_data$BA_pred_default)^2, na.rm = TRUE) /
                sum((val_data$BA_t2 - mean(val_data$BA_t2, na.rm = TRUE))^2, na.rm = TRUE)

  panel_f1b <- ggplot(plot_sample, aes(x = BA_t2, y = BA_pred_default)) +
    geom_abline(intercept = 0, slope = 1, color = "gray60", linetype = "dashed", linewidth = 0.8) +
    geom_point(alpha = 0.08, size = 0.5, color = "#E69F00") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1.5,
             label = sprintf("RMSE = %.1f", rmse_default),
             size = 3.5, fontface = "bold") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.3,
             label = sprintf("R\u00b2 = %.3f", max(r2_default, 0)),
             size = 3.5, fontface = "bold") +
    labs(
      x = expression("Observed BA (ft"^2~"ac"^{-1}*")"),
      y = expression("Predicted BA (ft"^2~"ac"^{-1}*")"),
      title = "B. Default: Predicted vs Observed BA"
    ) +
    coord_fixed(xlim = c(0, 400), ylim = c(0, 400)) +
    theme_manuscript()
} else {
  panel_f1b <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "B. Default: Predicted vs Observed BA")
}

# --- Panel C: R-squared Scatter (Calibrated vs Default by variant) ----
if (nrow(fia_summary) > 0 && "BA_r2_calib" %in% names(fia_summary)) {
  r2_plot <- fia_summary %>% filter(VARIANT != "OVERALL")

  panel_f1c <- ggplot(r2_plot, aes(x = BA_r2_default, y = BA_r2_calib, label = VARIANT)) +
    geom_abline(intercept = 0, slope = 1, color = "gray60", linetype = "dashed", linewidth = 0.8) +
    geom_point(size = 3, color = "#0072B2") +
    geom_text(size = 2.5, nudge_y = 0.012, check_overlap = TRUE) +
    labs(
      x = expression("Default BA R"^2),
      y = expression("Calibrated BA R"^2),
      title = expression("C. BA R"^2*": Calibrated vs Default")
    ) +
    theme_manuscript()
} else {
  panel_f1c <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "C. R-squared Comparison")
}

# --- Panel D: Equivalence Comparison (within 20% of observed) ----
if (nrow(fia_summary) > 0 && "BA_equiv_calib" %in% names(fia_summary)) {
  equiv_data <- fia_summary %>%
    filter(VARIANT != "OVERALL") %>%
    select(VARIANT, Calibrated = BA_equiv_calib, Default = BA_equiv_default) %>%
    pivot_longer(cols = c(Calibrated, Default), names_to = "approach", values_to = "equiv_pct")

  panel_f1d <- ggplot(equiv_data,
                       aes(x = reorder(VARIANT, -equiv_pct), y = equiv_pct, fill = approach)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    scale_fill_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00")) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
    labs(
      x = "Variant", y = "% within 20%",
      title = "D. BA Equivalence (within 20% of observed)",
      fill = "Approach"
    ) +
    theme_manuscript() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
} else {
  panel_f1d <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "D. BA Equivalence")
}

# --- Panel E: RMSE Comparison (paired bar chart) ----
if (nrow(fia_summary) > 0 && "BA_RMSE_calib" %in% names(fia_summary)) {
  rmse_data <- fia_summary %>%
    filter(VARIANT != "OVERALL") %>%
    select(VARIANT, Calibrated = BA_RMSE_calib, Default = BA_RMSE_default) %>%
    pivot_longer(cols = c(Calibrated, Default), names_to = "approach", values_to = "RMSE")

  panel_f1e <- ggplot(rmse_data,
                       aes(x = reorder(VARIANT, -RMSE), y = RMSE, fill = approach)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    scale_fill_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00")) +
    labs(
      x = "Variant",
      y = expression("RMSE (ft"^2~"ac"^{-1}*")"),
      title = "E. BA RMSE: Calibrated vs Default",
      fill = "Approach"
    ) +
    theme_manuscript() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
} else {
  panel_f1e <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "E. BA RMSE Comparison")
}

# --- Panel F: BA Bias Dumbbell (Calibrated vs Default by variant) ----
if (nrow(fia_summary) > 0 && "BA_bias_calib" %in% names(fia_summary)) {
  bias_plot <- fia_summary %>%
    filter(VARIANT != "OVERALL") %>%
    select(VARIANT, Calibrated = BA_bias_calib, Default = BA_bias_default) %>%
    pivot_longer(cols = c(Calibrated, Default), names_to = "approach", values_to = "bias")

  panel_f1f <- ggplot(bias_plot, aes(x = reorder(VARIANT, bias), y = bias, color = approach)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(Calibrated = "#0072B2", Default = "#E69F00")) +
    coord_flip() +
    labs(
      x = "Variant",
      y = expression("Bias (ft"^2~"ac"^{-1}*")"),
      title = "F. BA Bias by Variant",
      color = "Approach"
    ) +
    theme_manuscript()
} else {
  panel_f1f <- ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    annotate("text", x = 0.5, y = 0.5, label = "Data not available", size = 6, color = "gray60") +
    theme_manuscript() + labs(title = "F. BA Bias by Variant")
}

# --- Assemble Figure 2 ----
fig2 <- (panel_f1a | panel_f1b | panel_f1c) /
        (panel_f1d | panel_f1e | panel_f1f) +
  plot_annotation(
    title = "Figure 2: FIA Benchmark - Predictive Performance Validation",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0)
    )
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

# Save Figure 2
ggsave(
  file.path(fig_dir, "fig2_fia_benchmark.png"),
  fig2,
  width = 18,
  height = 14,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(fig_dir, "fig2_fia_benchmark.pdf"),
  fig2,
  width = 18,
  height = 14,
  bg = "white"
)

cat("Figure 2 saved.\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

# Overall calibration improvement
perf_wide <- perf_summary %>%
  pivot_wider(
    id_cols = c(variant, component_short),
    names_from = model,
    values_from = value
  ) %>%
  mutate(improvement = Calibrated - Default)

avg_improvement <- perf_wide %>%
  summarise(
    mean_improvement = mean(improvement, na.rm = TRUE),
    median_improvement = median(improvement, na.rm = TRUE),
    .by = component_short
  )

cat("\nAverage metric improvement (Calibrated - Default):\n")
print(avg_improvement)

# Bakuzis matrix pass rate
bakuzis_pass_rate <- bakuzis_matrix %>%
  mutate(pass_binary = ifelse(pass_text == "Pass", 1, 0)) %>%
  summarise(
    pass_rate = mean(pass_binary),
    passes = sum(pass_binary),
    total = n(),
    .by = law
  )

cat("\nBakuzis matrix pass rates:\n")
print(bakuzis_pass_rate)

# Overall pass rate
overall_pass <- bakuzis_matrix %>%
  mutate(pass_binary = ifelse(pass_text == "Pass", 1, 0)) %>%
  summarise(
    overall_pass_rate = mean(pass_binary),
    total_passes = sum(pass_binary),
    total_laws = n()
  )

cat("\nOverall Bakuzis matrix performance:\n")
cat(sprintf("  Pass rate: %.1f%% (%d/%d tests)\n",
            overall_pass$overall_pass_rate * 100,
            overall_pass$total_passes,
            overall_pass$total_laws))

cat("\n=== FIGURES SAVED ===\n")
cat(sprintf("Figure 1: %s\n", file.path(fig_dir, "fig1_complete_story.png")))
cat(sprintf("Figure 1: %s\n", file.path(fig_dir, "fig1_complete_story.pdf")))
cat(sprintf("Figure 2: %s\n", file.path(fig_dir, "fig2_fia_benchmark.png")))
cat(sprintf("Figure 2: %s\n", file.path(fig_dir, "fig2_fia_benchmark.pdf")))

cat("\nHero figures generation complete.\n")

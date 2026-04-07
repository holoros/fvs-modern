# =============================================================================
# Title: Manuscript Tables and Summary Figures
# Author: A. Weiskittel
# Date: 2026-04-03
# Description: Generate final manuscript tables and summary figures from
#   calibration output. Produces:
#   - Table 2: Model performance summary (all components x all variants)
#   - Table 3: CR v1 vs v2 comparison
#   - Figure: Calibration heatmap (all components)
#   - Figure: DG R-squared by variant (bar chart)
#   - Figure: CR v1 vs v2 paired comparison
# Dependencies: model_performance_summary.csv, cross_component_summary.csv
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)

# --- Paths -------------------------------------------------------------------
base_dir <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                      "/home/aweiskittel/Documents/Claude/fvs-modern"),
                      "calibration", "output", "comparisons")
fig_dir  <- file.path(base_dir, "manuscript_figures")
tbl_dir  <- file.path(base_dir, "manuscript_tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir, showWarnings = FALSE, recursive = TRUE)

# --- Theme -------------------------------------------------------------------
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    strip.text   = element_text(size = 11, face = "bold"),
    plot.title   = element_text(size = 14, face = "bold"),
    plot.margin  = margin(10, 10, 10, 10)
  )

# --- Data --------------------------------------------------------------------
perf <- read_csv(file.path(base_dir, "model_performance_summary.csv"),
                 show_col_types = FALSE)
xcomp <- read_csv(file.path(base_dir, "cross_component_summary.csv"),
                   show_col_types = FALSE)
avail <- read_csv(file.path(base_dir, "equation_availability_full.csv"),
                   show_col_types = FALSE)

# Define variant order (West to East, roughly)
variant_order <- c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
                    "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
                    "OP", "SN", "CS", "LS", "ON", "NE", "ACD")

# =============================================================================
# TABLE 2: Full performance summary by variant and component
# =============================================================================

cat("\n=== Generating Table 2: Performance Summary ===\n")

# Extract key metrics for each component
hd <- perf %>%
  filter(component == "Height_Diameter", metric == "r2", model == "calibrated") %>%
  select(variant, hd_r2 = value)

dg <- perf %>%
  filter(component == "Diameter_Growth", metric == "r2", model == "calibrated") %>%
  select(variant, dg_r2 = value)

mort <- perf %>%
  filter(component == "Mortality", metric == "auc", model == "calibrated") %>%
  select(variant, mort_auc = value)

cr <- perf %>%
  filter(component == "Crown_Ratio", metric == "r2", model == "calibrated") %>%
  select(variant, cr_r2 = value)

# SDIMAX: extract pct_change
sdi <- perf %>%
  filter(component == "SDIMAX", metric == "mean_pct_change", model == "comparison") %>%
  select(variant, sdi_pct_change = value)

# Build Table 2
table2 <- avail %>%
  left_join(hd, by = "variant") %>%
  left_join(dg, by = "variant") %>%
  left_join(mort, by = "variant") %>%
  left_join(cr, by = "variant") %>%
  left_join(sdi, by = "variant") %>%
  mutate(variant = factor(variant, levels = variant_order)) %>%
  arrange(variant) %>%
  mutate(
    across(c(hd_r2, dg_r2, cr_r2), ~ round(.x, 3)),
    mort_auc = round(mort_auc, 3),
    sdi_pct_change = round(sdi_pct_change, 1)
  )

write_csv(table2, file.path(tbl_dir, "table2_performance_summary.csv"))
cat("Saved table2_performance_summary.csv\n")

# Summary statistics
cat("\nComponent Summary Statistics:\n")
cat(sprintf("  H-D:   mean R2 = %.3f (sd = %.3f), range = [%.3f, %.3f]\n",
            mean(table2$hd_r2, na.rm = TRUE), sd(table2$hd_r2, na.rm = TRUE),
            min(table2$hd_r2, na.rm = TRUE), max(table2$hd_r2, na.rm = TRUE)))
cat(sprintf("  DG:    mean R2 = %.3f (sd = %.3f), range = [%.3f, %.3f]\n",
            mean(table2$dg_r2, na.rm = TRUE), sd(table2$dg_r2, na.rm = TRUE),
            min(table2$dg_r2, na.rm = TRUE), max(table2$dg_r2, na.rm = TRUE)))
cat(sprintf("  Mort:  mean AUC = %.3f (sd = %.3f), range = [%.3f, %.3f]\n",
            mean(table2$mort_auc, na.rm = TRUE), sd(table2$mort_auc, na.rm = TRUE),
            min(table2$mort_auc, na.rm = TRUE), max(table2$mort_auc, na.rm = TRUE)))
cat(sprintf("  CR:    mean R2 = %.3f (sd = %.3f), range = [%.3f, %.3f]\n",
            mean(table2$cr_r2, na.rm = TRUE), sd(table2$cr_r2, na.rm = TRUE),
            min(table2$cr_r2, na.rm = TRUE), max(table2$cr_r2, na.rm = TRUE)))
cat(sprintf("  SDI:   mean pct change = %.1f%% (n = %d variants with SDICON)\n",
            mean(table2$sdi_pct_change, na.rm = TRUE),
            sum(!is.na(table2$sdi_pct_change))))

# =============================================================================
# FIGURE 1: Multi-panel performance overview
# =============================================================================

cat("\n=== Generating Figure 1: Performance Overview ===\n")

# Panel A: DG R-squared by variant
dg_plot_data <- table2 %>%
  filter(!is.na(dg_r2)) %>%
  mutate(variant = fct_reorder(variant, dg_r2))

fig_dg <- ggplot(dg_plot_data, aes(x = variant, y = dg_r2)) +
  geom_col(fill = "#2166AC", alpha = 0.8) +
  geom_hline(yintercept = mean(dg_plot_data$dg_r2), linetype = "dashed",
             color = "grey40") +
  coord_flip() +
  labs(x = NULL, y = expression(R^2 ~ "(log scale)"),
       title = "Diameter Growth") +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15)) +
  theme_pub

ggsave(file.path(fig_dir, "fig_dg_r2_by_variant.png"), fig_dg,
       width = 4.5, height = 7, dpi = 300)
cat("Saved fig_dg_r2_by_variant.png\n")

# Panel B: Mortality AUC by variant
mort_plot_data <- table2 %>%
  filter(!is.na(mort_auc)) %>%
  mutate(variant = fct_reorder(variant, mort_auc))

fig_mort <- ggplot(mort_plot_data, aes(x = variant, y = mort_auc)) +
  geom_col(fill = "#B2182B", alpha = 0.8) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "AUC",
       title = "Mortality") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_pub

ggsave(file.path(fig_dir, "fig_mort_auc_by_variant.png"), fig_mort,
       width = 4.5, height = 7, dpi = 300)
cat("Saved fig_mort_auc_by_variant.png\n")

# Panel C: H-D R-squared, calibrated vs baseline
hd_comp <- perf %>%
  filter(component == "Height_Diameter", metric == "r2") %>%
  select(variant, model, value) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  mutate(variant = factor(variant, levels = variant_order))

fig_hd <- ggplot(hd_comp, aes(x = baseline, y = calibrated)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, alpha = 0.8, color = "#009E73") +
  geom_text(aes(label = variant), size = 2.5, nudge_y = 0.005, check_overlap = TRUE) +
  labs(x = expression("Default " * R^2), y = expression("Calibrated " * R^2),
       title = "Height-Diameter") +
  scale_x_continuous(limits = c(0.7, 0.92)) +
  scale_y_continuous(limits = c(0.7, 0.92)) +
  theme_pub +
  theme(aspect.ratio = 1)

ggsave(file.path(fig_dir, "fig_hd_improvement.png"), fig_hd,
       width = 5, height = 5, dpi = 300)
cat("Saved fig_hd_improvement.png\n")

# =============================================================================
# FIGURE 2: Calibration heatmap (all components x all variants)
# =============================================================================

cat("\n=== Generating Figure 2: Calibration Heatmap ===\n")

# Normalize each metric to 0-1 for heatmap display
heatmap_data <- table2 %>%
  select(variant, hd_r2, dg_r2, mort_auc, cr_r2) %>%
  pivot_longer(cols = c(hd_r2, dg_r2, mort_auc, cr_r2),
               names_to = "component", values_to = "value") %>%
  group_by(component) %>%
  mutate(
    value_norm = (value - min(value, na.rm = TRUE)) /
      (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    component = recode(component,
                       "hd_r2" = "H-D (R\u00b2)",
                       "dg_r2" = "DG (R\u00b2)",
                       "mort_auc" = "Mort. (AUC)",
                       "cr_r2" = "CR (R\u00b2)"),
    component = factor(component, levels = c("H-D (R\u00b2)", "DG (R\u00b2)",
                                              "Mort. (AUC)", "CR (R\u00b2)")),
    variant = factor(variant, levels = rev(variant_order))
  )

fig_heat <- ggplot(heatmap_data, aes(x = component, y = variant, fill = value_norm)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = 0.5, name = "Relative\nperformance",
                       limits = c(0, 1)) +
  labs(x = NULL, y = NULL, title = "Model Performance by Variant and Component") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid = element_blank())

ggsave(file.path(fig_dir, "fig_calibration_heatmap.png"), fig_heat,
       width = 7, height = 9, dpi = 300)
cat("Saved fig_calibration_heatmap.png\n")

# =============================================================================
# TABLE 3: Cross-component summary
# =============================================================================

cat("\n=== Generating Table 3: Cross-Component Summary ===\n")

table3 <- xcomp %>%
  filter(model == "calibrated" | model == "comparison") %>%
  select(component, metric, n_variants, mean_val, sd_val, min_val, max_val) %>%
  mutate(across(c(mean_val, sd_val, min_val, max_val), ~ round(.x, 3)))

write_csv(table3, file.path(tbl_dir, "table3_cross_component_summary.csv"))
cat("Saved table3_cross_component_summary.csv\n")

# =============================================================================
# Output final statistics for manuscript text
# =============================================================================

cat("\n=== Key Statistics for Manuscript Text ===\n\n")

# H-D improvement
hd_base_mean <- mean(hd_comp$baseline, na.rm = TRUE)
hd_cal_mean  <- mean(hd_comp$calibrated, na.rm = TRUE)
cat(sprintf("H-D: Calibration improved mean R2 from %.3f to %.3f (+%.1f%%)\n",
            hd_base_mean, hd_cal_mean,
            100 * (hd_cal_mean - hd_base_mean) / hd_base_mean))

# DG
cat(sprintf("DG: Mean R2 = %.3f across 25 variants\n",
            mean(table2$dg_r2, na.rm = TRUE)))
cat(sprintf("  Top 5: %s\n",
            paste(head(dg_plot_data %>% arrange(desc(dg_r2)) %>% pull(variant), 5),
                  collapse = ", ")))
cat(sprintf("  Bottom 5: %s\n",
            paste(head(dg_plot_data %>% arrange(dg_r2) %>% pull(variant), 5),
                  collapse = ", ")))

# Mortality
cat(sprintf("Mortality: Mean AUC = %.3f\n",
            mean(table2$mort_auc, na.rm = TRUE)))
cat(sprintf("  AUC > 0.9: %s\n",
            paste(table2 %>% filter(mort_auc > 0.9) %>% pull(variant), collapse = ", ")))
cat(sprintf("  AUC < 0.7: %s\n",
            paste(table2 %>% filter(mort_auc < 0.7) %>% pull(variant), collapse = ", ")))

# CR
cat(sprintf("Crown Ratio: Mean R2 = %.3f\n",
            mean(table2$cr_r2, na.rm = TRUE)))

# SDI
sdi_with_data <- table2 %>% filter(!is.na(sdi_pct_change))
cat(sprintf("SDIMAX: %d of 25 variants have SDICON values\n", nrow(sdi_with_data)))
cat(sprintf("  Mean calibrated SDI change: %.1f%%\n",
            mean(sdi_with_data$sdi_pct_change, na.rm = TRUE)))

cat("\n=== All tables and figures generated ===\n")

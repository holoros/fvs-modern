# =============================================================================
# Title: Crown Ratio v1 vs v2 Comparison Figure
# Author: A. Weiskittel
# Date: 2026-04-03
# Description: Paired comparison figure for CR v1 (delta model) vs v2 (Beta
#   regression on level). Produces a dot and arrow plot showing improvement
#   for all 25 variants.
# =============================================================================

library(tidyverse)
library(ggplot2)

base_dir <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                      "/home/aweiskittel/Documents/Claude/fvs-modern"),
                      "calibration", "output", "comparisons")
fig_dir  <- file.path(base_dir, "manuscript_figures")
tbl_dir  <- file.path(base_dir, "manuscript_tables")

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

cr_compare <- read_csv(file.path(tbl_dir, "cr_v1_v2_comparison.csv"),
                        show_col_types = FALSE)

# Order by v2 performance
cr_compare <- cr_compare %>%
  mutate(variant = fct_reorder(variant, cr_v2_r2))

# Dumbbell plot: v1 (dot) to v2 (dot) with connecting segment
fig_cr <- ggplot(cr_compare) +
  geom_segment(aes(x = cr_r2_v1, xend = cr_v2_r2, y = variant, yend = variant),
               color = "grey60", linewidth = 0.8,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(aes(x = cr_r2_v1, y = variant), color = "#D55E00", size = 2.5) +
  geom_point(aes(x = cr_v2_r2, y = variant), color = "#0072B2", size = 2.5) +
  labs(x = expression(R^2),
       y = NULL,
       title = "Crown Ratio: v1 (delta) vs v2 (Beta regression)") +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  annotate("text", x = 0.18, y = 1, label = "v1 (delta)", color = "#D55E00",
           size = 3.5, fontface = "bold") +
  annotate("text", x = 0.58, y = 1, label = "v2 (level, Beta)", color = "#0072B2",
           size = 3.5, fontface = "bold") +
  theme_pub

ggsave(file.path(fig_dir, "fig_cr_v1_v2_comparison.png"), fig_cr,
       width = 6, height = 7, dpi = 300)
cat("Saved fig_cr_v1_v2_comparison.png\n")

# Also make the mortality v1 vs v2 comparison for the 9 variants with v2
mort_v2 <- read_csv(file.path(base_dir, "mortality_v2_auc_full.csv"),
                     show_col_types = FALSE) %>%
  filter(!is.na(auc_v2))

perf <- read_csv(file.path(base_dir, "model_performance_summary.csv"),
                  show_col_types = FALSE)
mort_v1 <- perf %>%
  filter(component == "Mortality", metric == "auc", model == "calibrated") %>%
  select(variant, auc_v1 = value)

mort_compare <- mort_v2 %>%
  select(variant, auc_v2) %>%
  left_join(mort_v1, by = "variant") %>%
  mutate(
    change = auc_v2 - auc_v1,
    variant = fct_reorder(variant, auc_v2)
  )

write_csv(mort_compare, file.path(tbl_dir, "mortality_v1_v2_comparison.csv"))

fig_mort_v <- ggplot(mort_compare) +
  geom_segment(aes(x = auc_v1, xend = auc_v2, y = variant, yend = variant),
               color = "grey60", linewidth = 0.8,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(aes(x = auc_v1, y = variant), color = "#D55E00", size = 3) +
  geom_point(aes(x = auc_v2, y = variant), color = "#0072B2", size = 3) +
  labs(x = "AUC",
       y = NULL,
       title = "Mortality: v1 vs v2 (random slopes)") +
  scale_x_continuous(limits = c(0.6, 0.75), breaks = seq(0.6, 0.75, 0.05)) +
  annotate("text", x = 0.72, y = 1, label = "v1", color = "#D55E00",
           size = 3.5, fontface = "bold") +
  annotate("text", x = 0.64, y = 1, label = "v2", color = "#0072B2",
           size = 3.5, fontface = "bold") +
  theme_pub

ggsave(file.path(fig_dir, "fig_mortality_v1_v2_comparison.png"), fig_mort_v,
       width = 5, height = 5, dpi = 300)
cat("Saved fig_mortality_v1_v2_comparison.png\n")

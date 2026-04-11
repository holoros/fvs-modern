# =============================================================================
# Title: PERSEUS 100yr Projection with Uncertainty Bands
# Author: A. Weiskittel
# Date: 2026-04-11
# Description: Publication-ready figures for FVS-ACD and FVS-NE 100-year
#              projections from 1999-2004 FIA baseline with Bayesian
#              uncertainty bands from posterior parameter draws.
# Dependencies: mmt_point_estimates.csv, mmt_uncertainty_bands.csv
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)

# --- Paths -------------------------------------------------------------------
project_root <- Sys.getenv(
  "FVS_PROJECT_ROOT",
  default = here::here()
)
unc_dir <- file.path(
  project_root, "calibration", "output", "perseus",
  "uncertainty_1999_2004"
)
fig_dir <- file.path(project_root, "calibration", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Theme -------------------------------------------------------------------
theme_fvs <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95"),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

variant_colors <- c(
  "ACD_default"    = "#1b9e77",
  "ACD_calibrated" = "#d95f02",
  "NE_default"     = "#7570b3",
  "NE_calibrated"  = "#e7298a"
)

variant_labels <- c(
  "ACD_default"    = "FVS-ACD (default)",
  "ACD_calibrated" = "FVS-ACD (calibrated)",
  "NE_default"     = "FVS-NE (default)",
  "NE_calibrated"  = "FVS-NE (calibrated)"
)

# --- Data --------------------------------------------------------------------
point_mmt <- read_csv(
  file.path(unc_dir, "mmt_point_estimates.csv"),
  show_col_types = FALSE
) |>
  mutate(
    model_key = paste0(VARIANT, "_", CONFIG),
    model_label = variant_labels[model_key]
  )

unc_bands <- read_csv(
  file.path(unc_dir, "mmt_uncertainty_bands.csv"),
  show_col_types = FALSE
)

# --- Figure 1: Point estimates for all four model configurations -------------

# Compute approximate calendar year from PROJ_YEAR
# Median initial year is around 2001 for the 1999-2004 cohort
base_year <- 2001

p1 <- point_mmt |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  filter(PROJ_YEAR >= 0, PROJ_YEAR <= 100) |>
  ggplot(aes(x = calendar_year, y = MMT, color = model_key)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(
    values = variant_colors,
    labels = variant_labels,
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2110, by = 10)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "100-Year No-Harvest AGB Projection for Maine",
    subtitle = "FVS-ACD and FVS-NE with default and calibrated parameters (1999\u20132004 baseline)",
    x = "Year",
    y = "Aboveground Biomass (MMT)"
  ) +
  theme_fvs

ggsave(
  file.path(fig_dir, "perseus_uncertainty_point_estimates.png"),
  p1, width = 10, height = 6, dpi = 300
)

# --- Figure 2: Calibrated projections with uncertainty bands -----------------

p2_data <- point_mmt |>
  filter(CONFIG == "calibrated") |>
  mutate(calendar_year = base_year + PROJ_YEAR)

p2_bands <- unc_bands |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  filter(PROJ_YEAR >= 0, PROJ_YEAR <= 100)

p2 <- ggplot() +
  # 95% CI band
  geom_ribbon(
    data = p2_bands,
    aes(
      x = calendar_year,
      ymin = MMT_Q0025, ymax = MMT_Q0975,
      fill = VARIANT
    ),
    alpha = 0.15
  ) +
  # 90% CI band
  geom_ribbon(
    data = p2_bands,
    aes(
      x = calendar_year,
      ymin = MMT_Q0050, ymax = MMT_Q0950,
      fill = VARIANT
    ),
    alpha = 0.20
  ) +
  # 50% CI band
  geom_ribbon(
    data = p2_bands,
    aes(
      x = calendar_year,
      ymin = MMT_Q0250, ymax = MMT_Q0750,
      fill = VARIANT
    ),
    alpha = 0.25
  ) +
  # Median line
  geom_line(
    data = p2_bands,
    aes(x = calendar_year, y = MMT_Q0500, color = VARIANT),
    linewidth = 1.0
  ) +
  # Point estimate (MAP) for comparison
  geom_line(
    data = p2_data,
    aes(x = calendar_year, y = MMT, color = VARIANT),
    linewidth = 0.5, linetype = "dashed"
  ) +
  scale_color_manual(
    values = c("ACD" = "#d95f02", "NE" = "#e7298a"),
    labels = c("ACD" = "FVS-ACD", "NE" = "FVS-NE"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c("ACD" = "#d95f02", "NE" = "#e7298a"),
    labels = c("ACD" = "FVS-ACD", "NE" = "FVS-NE"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2110, by = 10)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "Calibrated FVS Projections with Bayesian Uncertainty",
    subtitle = paste0(
      "Shaded bands: 50%, 90%, and 95% credible intervals from ",
      max(unc_bands$N_DRAWS, na.rm = TRUE),
      " posterior draws"
    ),
    x = "Year",
    y = "Aboveground Biomass (MMT)"
  ) +
  theme_fvs +
  guides(fill = "none")

ggsave(
  file.path(fig_dir, "perseus_uncertainty_calibrated_bands.png"),
  p2, width = 10, height = 6, dpi = 300
)

# --- Figure 3: Coefficient of variation over time ---------------------------

p3 <- unc_bands |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  filter(PROJ_YEAR > 0, PROJ_YEAR <= 100) |>
  ggplot(aes(x = calendar_year, y = MMT_CV * 100, color = VARIANT)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c("ACD" = "#d95f02", "NE" = "#e7298a"),
    labels = c("ACD" = "FVS-ACD", "NE" = "FVS-NE"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2110, by = 10)) +
  labs(
    title = "Parameter Uncertainty Growth Over Projection Horizon",
    subtitle = "Coefficient of variation (%) of state-level AGB from posterior draws",
    x = "Year",
    y = "Coefficient of Variation (%)"
  ) +
  theme_fvs

ggsave(
  file.path(fig_dir, "perseus_uncertainty_cv_over_time.png"),
  p3, width = 8, height = 5, dpi = 300
)

# --- Figure 4: Default vs Calibrated divergence with uncertainty context -----

p4_default <- point_mmt |>
  filter(CONFIG == "default") |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  select(calendar_year, VARIANT, MMT_default = MMT)

p4_cal <- point_mmt |>
  filter(CONFIG == "calibrated") |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  select(calendar_year, VARIANT, MMT_cal = MMT)

p4_diff <- p4_default |>
  inner_join(p4_cal, by = c("calendar_year", "VARIANT")) |>
  mutate(
    pct_diff = (MMT_cal - MMT_default) / MMT_default * 100
  )

# Add uncertainty width for context
p4_unc_width <- unc_bands |>
  mutate(calendar_year = base_year + PROJ_YEAR) |>
  mutate(ci90_width_pct = (MMT_Q0950 - MMT_Q0050) / MMT_MEAN * 100)

p4 <- ggplot() +
  # Uncertainty width context (grey background)
  geom_ribbon(
    data = p4_unc_width,
    aes(
      x = calendar_year,
      ymin = -ci90_width_pct / 2,
      ymax = ci90_width_pct / 2,
      fill = VARIANT
    ),
    alpha = 0.15
  ) +
  # Default vs calibrated difference
  geom_line(
    data = p4_diff,
    aes(x = calendar_year, y = pct_diff, color = VARIANT),
    linewidth = 0.9
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c("ACD" = "#d95f02", "NE" = "#e7298a"),
    labels = c("ACD" = "FVS-ACD", "NE" = "FVS-NE"),
    name = "Calibration effect"
  ) +
  scale_fill_manual(
    values = c("ACD" = "#d95f02", "NE" = "#e7298a"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(2000, 2110, by = 10)) +
  labs(
    title = "Calibration Effect vs. Parameter Uncertainty",
    subtitle = "Line: % difference (calibrated vs default). Band: 90% CI width from posterior draws.",
    x = "Year",
    y = "Difference (%)"
  ) +
  theme_fvs

ggsave(
  file.path(fig_dir, "perseus_uncertainty_calibration_vs_uncertainty.png"),
  p4, width = 10, height = 6, dpi = 300
)

# --- Combined panel figure for manuscript ------------------------------------
p_combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "PERSEUS FVS Projections for Maine (1999\u20132004 Baseline)",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  )

ggsave(
  file.path(fig_dir, "perseus_uncertainty_combined_panel.png"),
  p_combined, width = 14, height = 10, dpi = 300
)

cat("Figures saved to:", fig_dir, "\n")

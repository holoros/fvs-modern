# =============================================================================
# Title: Maine State Level AGB (MMT) from PERSEUS FVS Projections
# Author: A. Weiskittel
# Date: 2026-04-10
# Description: Aggregate FIA plot level AGB projections to state level
#              megatonnes (MMT) and produce a figure showing FVS NE and ACD
#              default vs calibrated from ~2000 to 2100 by decade.
# Dependencies: perseus_combined_all.csv (PERSEUS output from corrected code)
# Notes: Previous PERSEUS output had two bugs now fixed in the Python code:
#        (1) SPCD string/int mismatch causing Jenkins group fallback
#        (2) NSBE docstring said kg but equations output pounds
#        Both bugs are resolved in perseus_100yr_projection.py as of
#        2026-04-10. The corrected rerun on Cardinal will produce clean data
#        that no longer requires the 4.4x correction factor.
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggtext)

# --- Configuration -----------------------------------------------------------
perseus_path <- file.path(
  dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
  "output", "perseus", "perseus_combined_all.csv"
)
# Fallback path for non-RStudio environments
if (!file.exists(perseus_path)) {
  perseus_path <- "calibration/output/perseus/perseus_combined_all.csv"
}

# Maine forested acres (FIA estimate, ~2020)
maine_forested_acres <- 17.6e6

# Conversion: short tons to metric megatonnes
# 1 short ton = 0.9072 metric tonnes; 1 MMT = 1e6 metric tonnes
tons_to_mmt <- 0.9072 / 1e6

# Publication theme
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.margin = margin(10, 15, 10, 10)
  )

# --- Data --------------------------------------------------------------------
raw <- read_csv(perseus_path, show_col_types = FALSE)

# --- Aggregation to decade x variant x config --------------------------------
# Bin YEAR to nearest decade for clean x axis
decade_summary <- raw |>
  mutate(decade = round(YEAR / 10) * 10) |>
  filter(decade >= 2000, decade <= 2100) |>
  group_by(decade, VARIANT, CONFIG) |>
  summarise(
    n_plots = n(),
    mean_agb_tons_ac = mean(AGB_TONS_AC, na.rm = TRUE),
    se_agb = sd(AGB_TONS_AC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    # Scale to state level MMT
    state_mmt = mean_agb_tons_ac * maine_forested_acres * tons_to_mmt,
    state_mmt_lo = (mean_agb_tons_ac - 1.96 * se_agb) * maine_forested_acres * tons_to_mmt,
    state_mmt_hi = (mean_agb_tons_ac + 1.96 * se_agb) * maine_forested_acres * tons_to_mmt
  )

# Create display labels
decade_summary <- decade_summary |>
  mutate(
    model_label = case_when(
      VARIANT == "NE"  & CONFIG == "default"    ~ "FVS-NE Default",
      VARIANT == "NE"  & CONFIG == "calibrated" ~ "FVS-NE Calibrated",
      VARIANT == "ACD" & CONFIG == "default"    ~ "FVS-ACD Default",
      VARIANT == "ACD" & CONFIG == "calibrated" ~ "FVS-ACD Calibrated",
      TRUE ~ paste(VARIANT, CONFIG)
    )
  )

# Print summary table
cat("\n=== Maine MMT by Decade ===\n")
decade_summary |>
  select(decade, model_label, n_plots, mean_agb_tons_ac, state_mmt) |>
  print(n = 50)

# --- Figure ------------------------------------------------------------------
# Color palette: paired colors for NE (warm) and ACD (cool), default vs calibrated
model_colors <- c(
  "FVS-NE Default"    = "#D55E00",  # vermillion
  "FVS-NE Calibrated" = "#0072B2",  # blue
  "FVS-ACD Default"   = "#E69F00",  # amber
  "FVS-ACD Calibrated" = "#009E73"  # teal
)

model_linetypes <- c(
  "FVS-NE Default"    = "dashed",
  "FVS-NE Calibrated" = "solid",
  "FVS-ACD Default"   = "dashed",
  "FVS-ACD Calibrated" = "solid"
)

model_shapes <- c(
  "FVS-NE Default"    = 1,   # open circle
  "FVS-NE Calibrated" = 16,  # filled circle
  "FVS-ACD Default"   = 2,   # open triangle
  "FVS-ACD Calibrated" = 17  # filled triangle
)

p <- ggplot(decade_summary, aes(x = decade, y = state_mmt,
                                 color = model_label,
                                 linetype = model_label,
                                 shape = model_label)) +
  geom_ribbon(aes(ymin = state_mmt_lo, ymax = state_mmt_hi,
                  fill = model_label),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_linetype_manual(values = model_linetypes) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(
    breaks = seq(2000, 2100, by = 10),
    limits = c(2000, 2100)
  ) +
  labs(
    title = "Maine Aboveground Biomass: FVS-NE vs FVS-ACD Projections",
    subtitle = "State total from 3,632 PERSEUS plots (NSBE biomass, default vs calibrated)",
    x = "Year",
    y = "Aboveground biomass (MMT)"
  ) +
  theme_pub +
  guides(
    color = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2),
    fill = "none"
  )

# --- Output ------------------------------------------------------------------
out_dir <- dirname(perseus_path)

ggsave(file.path(out_dir, "maine_mmt_projection.png"), plot = p,
       width = 22, height = 14, units = "cm", dpi = 300, bg = "white")

ggsave(file.path(out_dir, "maine_mmt_projection.pdf"), plot = p,
       width = 22, height = 14, units = "cm")

cat("\nFigure saved to:", file.path(out_dir, "maine_mmt_projection.png"), "\n")

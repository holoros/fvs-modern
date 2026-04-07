library(tidyverse)

# Read the 3 scenarios CSV
df <- read_csv("output/comparisons/stand_projections_3scenarios.csv", show_col_types = FALSE)

# Filter to Calibrated (annualized) scenario only
calib <- df %>% filter(scenario == "Calibrated (annualized)")

# Create a region mapping from variant codes
variant_to_region <- tribble(
  ~variant, ~region,
  "ACD", "East",
  "AK", "Alaska",
  "BC", "Canada",
  "BM", "South",
  "CA", "West",
  "CI", "South",
  "CR", "South",
  "CS", "South",
  "EC", "East",
  "EM", "East",
  "IE", "East",
  "KT", "West",
  "LS", "West",
  "NC", "East",
  "NE", "North",
  "OC", "Canada",
  "ON", "Canada",
  "OP", "West",
  "PN", "Pacific",
  "SN", "South",
  "SO", "South",
  "TT", "South",
  "UT", "West",
  "WC", "Canada",
  "WS", "West"
)

# Compute per-variant statistics for Calibrated scenario
realism_detail <- calib %>%
  group_by(variant) %>%
  arrange(year) %>%
  summarise(
    initial_ba = ba[year == 0],
    final_ba = ba[year == 50],
    initial_tpa = tpa[year == 0],
    final_tpa = tpa[year == 50],
    initial_qmd = qmd[year == 0],
    final_qmd = qmd[year == 50],
    ba_growth_ft2_ac_yr = (final_ba - initial_ba) / 50,
    tpa_decline_pct = ((initial_tpa - final_tpa) / initial_tpa) * 100,
    qmd_grew = final_qmd > initial_qmd,
    tpa_declined = final_tpa < initial_tpa,
    .groups = 'drop'
  ) %>%
  # Add region information
  left_join(variant_to_region, by = "variant") %>%
  # Classify as realistic based on criteria
  mutate(
    ba_in_range = final_ba >= 30 & final_ba <= 200,
    qmd_growth_ok = qmd_grew,
    tpa_thinning_ok = tpa_declined,
    realism_class = case_when(
      ba_in_range & qmd_growth_ok & tpa_thinning_ok ~ "Realistic",
      !ba_in_range ~ "BA out of range",
      !qmd_growth_ok ~ "QMD did not grow",
      !tpa_thinning_ok ~ "TPA did not decline",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(variant)

# Create output directory if it doesn't exist
dir.create("output/comparisons/manuscript_tables", showWarnings = FALSE, recursive = TRUE)

# Write detailed results table
write_csv(realism_detail, "output/comparisons/manuscript_tables/projection_realism_detail.csv")

# Print summary to console
cat("=== PROJECTION REALISM CHECK ===\n")
cat("Scenario: Calibrated (annualized)\n")
cat("Variants analyzed:", n_distinct(realism_detail$variant), "\n\n")

cat("=== REALISM CLASSIFICATION ===\n")
class_summary <- realism_detail %>%
  group_by(realism_class) %>%
  summarise(n = n(), .groups = 'drop')
print(class_summary)

cat("\n=== BA GROWTH RATES ===\n")
cat("Min:", round(min(realism_detail$ba_growth_ft2_ac_yr), 3), "ft2/ac/yr\n")
cat("Max:", round(max(realism_detail$ba_growth_ft2_ac_yr), 3), "ft2/ac/yr\n")
cat("Mean:", round(mean(realism_detail$ba_growth_ft2_ac_yr), 3), "ft2/ac/yr\n")
cat("SD:", round(sd(realism_detail$ba_growth_ft2_ac_yr), 3), "\n\n")

cat("=== FINAL BA STATISTICS ===\n")
cat("Min:", round(min(realism_detail$final_ba), 2), "ft2/ac (",
    realism_detail$variant[which.min(realism_detail$final_ba)], ")\n")
cat("Max:", round(max(realism_detail$final_ba), 2), "ft2/ac (",
    realism_detail$variant[which.max(realism_detail$final_ba)], ")\n")
cat("Mean:", round(mean(realism_detail$final_ba), 2), "ft2/ac\n")
cat("Range:", round(max(realism_detail$final_ba) - min(realism_detail$final_ba), 2), "\n\n")

# Dumbbell chart data preparation
# Create ordering by initial BA
variant_order <- realism_detail %>%
  arrange(initial_ba) %>%
  pull(variant)

dumbbell_data <- realism_detail %>%
  select(variant, region, initial_ba, final_ba) %>%
  mutate(variant = factor(variant, levels = variant_order)) %>%
  pivot_longer(
    cols = c(initial_ba, final_ba),
    names_to = "time_point",
    values_to = "ba"
  ) %>%
  mutate(
    time_point = factor(time_point, levels = c("initial_ba", "final_ba"),
                        labels = c("Year 0", "Year 50"))
  )

# Create lines data for dumbbell
line_data <- realism_detail %>%
  mutate(variant = factor(variant, levels = variant_order))

# Create publication-quality dumbbell chart
dir.create("output/comparisons/manuscript_figures", showWarnings = FALSE, recursive = TRUE)

p <- ggplot(dumbbell_data, aes(x = ba, y = variant, color = region)) +
  geom_segment(data = line_data,
               aes(x = initial_ba, xend = final_ba, y = variant, yend = variant),
               linewidth = 0.8, inherit.aes = FALSE, color = "grey50") +
  geom_point(aes(shape = time_point), size = 3) +
  scale_color_brewer(palette = "Set2", name = "Region") +
  scale_shape_manual(values = c(16, 17), name = "Time point") +
  labs(
    title = "Basal Area Trajectories by Variant (Calibrated Scenario)",
    subtitle = "50 year projections from initial stand density (200 TPA) to final state",
    x = "Basal Area (ft2/ac)",
    y = "Forest Variant"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("output/comparisons/manuscript_figures/fig_ba_dumbbell_calibrated.png",
       plot = p, width = 10, height = 8, dpi = 300, bg = "white")

cat("Outputs written:\n")
cat("  - output/comparisons/manuscript_tables/projection_realism_detail.csv\n")
cat("  - output/comparisons/manuscript_figures/fig_ba_dumbbell_calibrated.png\n")

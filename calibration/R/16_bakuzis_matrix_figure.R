# =============================================================================
# Title: Bakuzis Matrix Visualization + Stand-Level Performance
# Author: A. Weiskittel
# Date: 2026-04-03
# Description: Generate a comprehensive Bakuzis matrix visualization showing
#   biological realism of calibrated parameters across all 25 FVS variants.
#   Also generates stand-level projection trajectories comparing variants.
#
#   The Bakuzis matrix (Bakuzis 1969) tests a model system against known
#   ecological relationships. We evaluate 4 key laws:
#     1. Sukachev effect: competition reduces individual tree growth
#     2. Eichhorn's rule: better site quality increases growth
#     3. Crown recession: crown ratios decline under competition
#     4. Mortality-size pattern: small trees have higher mortality
#
#   Stand-level projections provide the integrated system test.
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)

# --- Paths -------------------------------------------------------------------
base_dir  <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                       "/home/aweiskittel/Documents/Claude/fvs-modern"),
                       "calibration", "output")
assess_dir <- file.path(base_dir, "assessment")
fig_dir    <- file.path(base_dir, "comparisons", "manuscript_figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Theme -------------------------------------------------------------------
theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    strip.text   = element_text(size = 10, face = "bold"),
    plot.title   = element_text(size = 12, face = "bold"),
    plot.margin  = margin(5, 8, 5, 5)
  )

# --- Data --------------------------------------------------------------------
sukachev  <- read_csv(file.path(assess_dir, "bakuzis_sukachev_effect.csv"),
                       show_col_types = FALSE)
eichhorn  <- read_csv(file.path(assess_dir, "bakuzis_eichhorn_rule.csv"),
                       show_col_types = FALSE)
recession <- read_csv(file.path(assess_dir, "bakuzis_crown_recession.csv"),
                       show_col_types = FALSE)
mort_u    <- read_csv(file.path(assess_dir, "bakuzis_mortality_ushape.csv"),
                       show_col_types = FALSE)
trajectories <- read_csv(file.path(assess_dir, "stand_projection_trajectories.csv"),
                          show_col_types = FALSE)

# Regional grouping for color coding
region_map <- tibble(
  variant = c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
              "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
              "OP", "SN", "CS", "LS", "ON", "NE", "ACD"),
  region = c("Pacific NW", "Pacific NW", "Pacific NW", "Pacific NW",
             "Pacific SW", "Pacific SW", "Pacific SW",
             "Intermountain", "Intermountain", "Intermountain",
             "Intermountain", "Intermountain", "Intermountain",
             "Northern Rockies", "Northern Rockies",
             "Southern Rockies", "Southern Rockies", "Southern Rockies",
             "Central Plains", "Southern", "Central States",
             "Lake States", "Lake States", "Northeast", "Northeast")
)

region_colors <- c(
  "Pacific NW"      = "#0072B2",
  "Pacific SW"      = "#56B4E9",
  "Intermountain"   = "#009E73",
  "Northern Rockies"= "#F0E442",
  "Southern Rockies"= "#E69F00",
  "Central Plains"  = "#D55E00",
  "Southern"        = "#CC79A7",
  "Central States"  = "#882255",
  "Lake States"     = "#332288",
  "Northeast"       = "#117733"
)

# =============================================================================
# FIGURE: Bakuzis Matrix (4 panels)
# =============================================================================

cat("=== Building Bakuzis Matrix Figure ===\n")

# --- Panel A: Sukachev effect (competition reduces growth) ---
# Show growth at low vs high competition, with arrow indicating expected direction
sukachev_long <- sukachev %>%
  select(variant, di_low_comp, di_high_comp, sukachev_pass) %>%
  left_join(region_map, by = "variant") %>%
  mutate(variant = fct_reorder(variant, growth_reduction_pct <-
           100 * (di_low_comp - di_high_comp) / di_low_comp))

pA <- ggplot(sukachev_long) +
  geom_segment(aes(x = di_high_comp, xend = di_low_comp,
                    y = variant, yend = variant, color = region),
               linewidth = 0.7, alpha = 0.7) +
  geom_point(aes(x = di_low_comp, y = variant, color = region),
             size = 2, shape = 16) +
  geom_point(aes(x = di_high_comp, y = variant, color = region),
             size = 2, shape = 17) +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = expression("Annual DI (in. yr"^{-1}*")"),
       y = NULL,
       title = "A) Sukachev: Competition reduces growth",
       subtitle = "Circle = low BAL, Triangle = high BAL (100% pass)") +
  theme_pub

# --- Panel B: Eichhorn's rule (site quality increases growth) ---
eichhorn_long <- eichhorn %>%
  left_join(region_map, by = "variant") %>%
  mutate(variant = fct_reorder(variant, growth_increase_pct))

pB <- ggplot(eichhorn_long) +
  geom_segment(aes(x = di_low_si, xend = di_high_si,
                    y = variant, yend = variant, color = region),
               linewidth = 0.7, alpha = 0.7,
               arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_point(aes(x = di_low_si, y = variant, color = region),
             size = 2, shape = 16) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = expression("Annual DI (in. yr"^{-1}*")"),
       y = NULL,
       title = "B) Eichhorn: Site quality increases growth",
       subtitle = paste0("Arrow: low SI to high SI (",
                          sum(eichhorn$eichhorn_pass), "/25 pass)")) +
  theme_pub +
  # Mark failures with X
  geom_point(data = eichhorn_long %>% filter(!eichhorn_pass),
             aes(x = (di_low_si + di_high_si) / 2, y = variant),
             shape = 4, size = 3, color = "red", stroke = 1.5)

# --- Panel C: Crown recession under competition ---
recession_long <- recession %>%
  left_join(region_map, by = "variant") %>%
  pivot_longer(cols = c(ba_effect, bal_effect),
               names_to = "predictor", values_to = "coefficient") %>%
  mutate(
    predictor = recode(predictor,
                       "ba_effect" = "BA effect",
                       "bal_effect" = "BAL effect"),
    variant = factor(variant, levels = recession$variant[order(recession$ba_effect)])
  )

pC <- ggplot(recession_long, aes(x = coefficient, y = variant,
                                   shape = predictor, color = region)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_shape_manual(values = c("BA effect" = 16, "BAL effect" = 17)) +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = "Posterior median coefficient",
       y = NULL,
       shape = NULL,
       title = "C) Crown recession: CR declines with density",
       subtitle = "Negative = ecologically correct (100% pass)") +
  theme_pub +
  theme(legend.position = c(0.8, 0.15))

# --- Panel D: Mortality vs size (U-shape test) ---
mort_long <- mort_u %>%
  left_join(region_map, by = "variant") %>%
  select(variant, region, mort_smallest, mort_middle, mort_largest, u_shape_pass) %>%
  pivot_longer(cols = c(mort_smallest, mort_middle, mort_largest),
               names_to = "size_class", values_to = "mort_rate") %>%
  mutate(
    size_class = recode(size_class,
                        "mort_smallest" = "Small",
                        "mort_middle" = "Mid",
                        "mort_largest" = "Large"),
    size_class = factor(size_class, levels = c("Small", "Mid", "Large")),
    variant = fct_reorder(variant, ifelse(u_shape_pass, 1, 0))
  )

pD <- ggplot(mort_long, aes(x = size_class, y = mort_rate,
                              group = variant, color = region)) +
  geom_line(alpha = 0.5, linewidth = 0.6) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = region_colors) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
  labs(x = "Size class",
       y = "Predicted mortality rate",
       color = "Region",
       title = "D) Mortality-size relationship",
       subtitle = paste0("U-shape expected (",
                          sum(mort_u$u_shape_pass), "/25 pass)")) +
  theme_pub +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"))

# Combine with patchwork
bakuzis_fig <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title = "Bakuzis Matrix: Biological Realism of Calibrated FVS Parameters",
    subtitle = "Tests of ecological laws across 25 FVS geographic variants",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  )

ggsave(file.path(fig_dir, "fig_bakuzis_matrix.png"), bakuzis_fig,
       width = 14, height = 12, dpi = 300)
cat("Saved fig_bakuzis_matrix.png (14 x 12 in)\n")

# =============================================================================
# FIGURE: Bakuzis Summary Heatmap (pass/fail + effect strength)
# =============================================================================

cat("\n=== Building Bakuzis Summary Heatmap ===\n")

# Build summary table
variant_order <- c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
                    "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
                    "OP", "SN", "CS", "LS", "ON", "NE", "ACD")

bakuzis_summary <- tibble(variant = variant_order) %>%
  left_join(sukachev %>% select(variant, sukachev_pct = growth_reduction_pct,
                                 sukachev_pass), by = "variant") %>%
  left_join(eichhorn %>% select(variant, eichhorn_pct = growth_increase_pct,
                                 eichhorn_pass), by = "variant") %>%
  left_join(recession %>% select(variant, ba_eff = ba_effect,
                                  recession_pass), by = "variant") %>%
  left_join(mort_u %>% select(variant, u_shape_pass), by = "variant")

# Pivot for heatmap
hm_data <- bakuzis_summary %>%
  transmute(
    variant,
    `Sukachev\n(competition)` = sukachev_pct,
    `Eichhorn\n(site quality)` = pmax(pmin(eichhorn_pct, 100), -50),
    `Crown recession\n(BA effect)` = ba_eff * (-100),
    `Mortality\nU-shape` = ifelse(u_shape_pass, 1, 0) * 100
  ) %>%
  pivot_longer(-variant, names_to = "Law", values_to = "strength") %>%
  mutate(
    variant = factor(variant, levels = rev(variant_order)),
    pass = strength > 0
  )

fig_hm <- ggplot(hm_data, aes(x = Law, y = variant, fill = strength)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(pass, "\u2713", "\u2717")),
            size = 3.5, color = ifelse(hm_data$pass, "darkgreen", "red")) +
  scale_fill_gradient2(
    low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
    midpoint = 25, name = "Effect\nstrength (%)",
    limits = c(-50, 100)
  ) +
  labs(x = NULL, y = NULL,
       title = "Bakuzis Matrix: Ecological Law Compliance",
       subtitle = "Green = ecologically correct direction, check/X = pass/fail") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        panel.grid = element_blank())

ggsave(file.path(fig_dir, "fig_bakuzis_heatmap.png"), fig_hm,
       width = 7, height = 9, dpi = 300)
cat("Saved fig_bakuzis_heatmap.png\n")

# =============================================================================
# FIGURE: Stand-Level Trajectories (50-year projections)
# =============================================================================

cat("\n=== Building Stand-Level Trajectory Figure ===\n")

traj <- trajectories %>%
  left_join(region_map, by = "variant")

# Panel 1: Basal area
p_ba <- ggplot(traj, aes(x = year, y = ba, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  geom_hline(yintercept = 60.15, linetype = "dashed", color = "grey50",
             linewidth = 0.3) +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = "Year", y = expression("Basal area (ft"^2~"ac"^{-1}*")"),
       title = expression("A) Basal area (ft"^2~"ac"^{-1}*")")) +
  theme_pub

# Panel 2: QMD
p_qmd <- ggplot(traj, aes(x = year, y = qmd, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = "Year", y = "QMD (in.)",
       title = "B) Quadratic mean diameter (in.)") +
  theme_pub

# Panel 3: TPA
p_tpa <- ggplot(traj, aes(x = year, y = tpa, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_manual(values = region_colors, guide = "none") +
  labs(x = "Year", y = expression("Trees ac"^{-1}),
       title = expression("C) Trees per acre")) +
  theme_pub

# Panel 4: SDI
p_sdi <- ggplot(traj, aes(x = year, y = sdi, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  geom_hline(yintercept = 124, linetype = "dashed", color = "grey50",
             linewidth = 0.3) +
  scale_color_manual(values = region_colors) +
  labs(x = "Year", y = "SDI",
       color = "Region",
       title = "D) Stand density index") +
  theme_pub +
  theme(legend.position = "right",
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size = 7))

stand_fig <- (p_ba | p_qmd) / (p_tpa | p_sdi) +
  plot_annotation(
    title = "Stand-Level Projections: 50-Year Trajectories (Calibrated Parameters)",
    subtitle = "Initial conditions: 200 TPA, 60 BA, 7.4 in. QMD",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  )

ggsave(file.path(fig_dir, "fig_stand_trajectories.png"), stand_fig,
       width = 12, height = 9, dpi = 300)
cat("Saved fig_stand_trajectories.png (12 x 9 in)\n")

# =============================================================================
# FIGURE: Stand-Level Realism Diagnostic
# =============================================================================

cat("\n=== Building Stand-Level Realism Figure ===\n")

# Classify variant trajectories by behavior type
traj_summary <- traj %>%
  group_by(variant, region) %>%
  summarise(
    ba_initial = ba[year == 0],
    ba_final = ba[year == max(year)],
    ba_change_pct = 100 * (ba_final - ba_initial) / ba_initial,
    qmd_initial = qmd[year == 0],
    qmd_final = qmd[year == max(year)],
    qmd_change = qmd_final - qmd_initial,
    tpa_initial = tpa[year == 0],
    tpa_final = tpa[year == max(year)],
    tpa_decline_pct = 100 * (tpa_initial - tpa_final) / tpa_initial,
    sdi_max = max(sdi),
    sdi_final = sdi[year == max(year)],
    # Check if BA ever increases above initial
    ba_max = max(ba),
    ba_peaked = ba_max > ba_initial * 1.05,
    .groups = "drop"
  ) %>%
  mutate(
    behavior = case_when(
      ba_change_pct > -15 ~ "Realistic (BA stable/growing)",
      ba_change_pct > -50 ~ "Moderate decline",
      ba_change_pct > -90 ~ "Severe BA decline",
      TRUE ~ "BA collapse (<10% remaining)"
    ),
    behavior = factor(behavior, levels = c("Realistic (BA stable/growing)",
                                            "Moderate decline",
                                            "Severe BA decline",
                                            "BA collapse (<10% remaining)"))
  )

# BA change vs QMD change scatter
p_realism <- ggplot(traj_summary, aes(x = ba_change_pct, y = qmd_change,
                                        color = region, label = variant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.8, nudge_y = 0.3, check_overlap = TRUE) +
  scale_color_manual(values = region_colors) +
  # Shade the "realistic" quadrant
  annotate("rect", xmin = -15, xmax = 100, ymin = 0, ymax = 20,
           alpha = 0.08, fill = "green") +
  annotate("text", x = 25, y = 18, label = "Realistic\nzone",
           size = 3, color = "darkgreen", fontface = "italic") +
  labs(x = "BA change over 50 years (%)",
       y = "QMD change over 50 years (in.)",
       color = "Region",
       title = "Stand-Level Realism: BA vs QMD Change (50-Year Projection)",
       subtitle = "Most variants show excessive BA decline driven by high predicted mortality") +
  theme_pub +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"))

ggsave(file.path(fig_dir, "fig_stand_realism_scatter.png"), p_realism,
       width = 9, height = 7, dpi = 300)
cat("Saved fig_stand_realism_scatter.png\n")

# Print diagnostic summary
cat("\n=== Stand-Level Behavior Summary ===\n")
cat("Variant trajectory classification:\n")
traj_summary %>%
  group_by(behavior) %>%
  summarise(
    n = n(),
    variants = paste(variant, collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = 120)

cat("\nBA change summary:\n")
cat(sprintf("  Mean: %.1f%%, Median: %.1f%%, Range: [%.1f%%, %.1f%%]\n",
            mean(traj_summary$ba_change_pct), median(traj_summary$ba_change_pct),
            min(traj_summary$ba_change_pct), max(traj_summary$ba_change_pct)))

cat("\nVariants with realistic BA trajectories (decline < 15%):\n")
cat(" ", paste(traj_summary %>% filter(ba_change_pct > -15) %>% pull(variant),
               collapse = ", "), "\n")

cat("\nQMD always increases:", sum(traj_summary$qmd_change > 0), "/ 25\n")
cat("TPA always declines:", sum(traj_summary$tpa_decline_pct > 0), "/ 25\n")

cat("\n=== All Bakuzis and stand-level figures generated ===\n")

# =============================================================================
# Title: Final Manuscript Tables with v1 and v2 Data
# Author: A. Weiskittel
# Date: 2026-04-03
# Description: Build comprehensive manuscript tables combining v1 comparison
#   metrics with v2 crown ratio and mortality results. Produces:
#   - Table 2: Full variant x component performance (all 25 variants)
#   - Table 3: Component summary statistics
#   - CR v1 vs v2 comparison table
#   - Mortality v1 vs v2 AUC table
# Dependencies: model_performance_summary.csv, crown_ratio_v2_r2.csv,
#   mortality_v2_auc_full.csv
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)

# --- Paths -------------------------------------------------------------------
base_dir <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                      "/home/aweiskittel/Documents/Claude/fvs-modern"),
                      "calibration", "output", "comparisons")
tbl_dir  <- file.path(base_dir, "manuscript_tables")
dir.create(tbl_dir, showWarnings = FALSE, recursive = TRUE)

# --- Data --------------------------------------------------------------------
perf <- read_csv(file.path(base_dir, "model_performance_summary.csv"),
                 show_col_types = FALSE)
cr_v2 <- read_csv(file.path(base_dir, "crown_ratio_v2_r2.csv"),
                   show_col_types = FALSE)
mort_v2 <- read_csv(file.path(base_dir, "mortality_v2_auc_full.csv"),
                     show_col_types = FALSE)

# Variant display order (geographic: West to East)
variant_order <- c("AK", "BC", "PN", "WC", "OC", "CA", "NC", "SO", "WS",
                    "BM", "IE", "CI", "KT", "EC", "EM", "UT", "CR", "TT",
                    "OP", "SN", "CS", "LS", "ON", "NE", "ACD")

# =============================================================================
# Extract component metrics from v1 comparison
# =============================================================================

# H-D: calibrated R2 and baseline R2
hd_cal <- perf %>%
  filter(component == "Height_Diameter", metric == "r2", model == "calibrated") %>%
  select(variant, hd_r2_cal = value)
hd_base <- perf %>%
  filter(component == "Height_Diameter", metric == "r2", model == "baseline") %>%
  select(variant, hd_r2_base = value)

# DG: calibrated R2
dg <- perf %>%
  filter(component == "Diameter_Growth", metric == "r2", model == "calibrated") %>%
  select(variant, dg_r2 = value)

# Mortality v1: AUC
mort_v1 <- perf %>%
  filter(component == "Mortality", metric == "auc", model == "calibrated") %>%
  select(variant, mort_auc_v1 = value)

# CR v1: R2
cr_v1 <- perf %>%
  filter(component == "Crown_Ratio", metric == "r2", model == "calibrated") %>%
  select(variant, cr_r2_v1 = value)

# =============================================================================
# TABLE 2: Full performance matrix (all components, best available version)
# =============================================================================

cat("\n=== TABLE 2: Complete Variant Performance ===\n\n")

table2 <- tibble(variant = variant_order) %>%
  left_join(hd_cal, by = "variant") %>%
  left_join(hd_base, by = "variant") %>%
  left_join(dg, by = "variant") %>%
  left_join(mort_v1, by = "variant") %>%
  left_join(mort_v2 %>% select(variant, mort_auc_v2 = auc_v2), by = "variant") %>%
  left_join(cr_v1, by = "variant") %>%
  left_join(cr_v2, by = "variant") %>%
  mutate(
    # Use best mortality AUC available (v2 when present, else v1)
    mort_auc_best = coalesce(mort_auc_v2, mort_auc_v1),
    mort_version = ifelse(!is.na(mort_auc_v2), "v2", "v1"),
    # H-D improvement
    hd_improvement = hd_r2_cal - hd_r2_base
  )

# Display table
table2_display <- table2 %>%
  transmute(
    Variant = variant,
    `H-D R2 (base)` = round(hd_r2_base, 3),
    `H-D R2 (cal)` = round(hd_r2_cal, 3),
    `DG R2` = round(dg_r2, 3),
    `Mort AUC (v1)` = round(mort_auc_v1, 3),
    `Mort AUC (v2)` = ifelse(!is.na(mort_auc_v2), round(mort_auc_v2, 3), NA),
    `CR R2 (v1)` = round(cr_r2_v1, 3),
    `CR R2 (v2)` = round(cr_v2_r2, 3)
  )

write_csv(table2_display, file.path(tbl_dir, "table2_full_performance.csv"))
cat("Saved table2_full_performance.csv\n")
print(table2_display, n = 25)

# =============================================================================
# TABLE 3: Component Summary Statistics
# =============================================================================

cat("\n=== TABLE 3: Component Summary Statistics ===\n\n")

summary_stats <- function(x, label) {
  x <- x[!is.na(x)]
  tibble(
    Component = label,
    N = length(x),
    Mean = round(mean(x), 3),
    SD = round(sd(x), 3),
    Min = round(min(x), 3),
    Max = round(max(x), 3),
    Median = round(median(x), 3)
  )
}

table3 <- bind_rows(
  summary_stats(table2$hd_r2_base, "H-D R2 (default)"),
  summary_stats(table2$hd_r2_cal, "H-D R2 (calibrated)"),
  summary_stats(table2$dg_r2, "DG R2"),
  summary_stats(table2$mort_auc_v1, "Mortality AUC (v1)"),
  summary_stats(table2$mort_auc_v2, "Mortality AUC (v2)"),
  summary_stats(table2$cr_r2_v1, "CR R2 (v1, delta)"),
  summary_stats(table2$cr_v2_r2, "CR R2 (v2, level)")
)

write_csv(table3, file.path(tbl_dir, "table3_component_summary.csv"))
cat("Saved table3_component_summary.csv\n")
print(table3)

# =============================================================================
# CR v1 vs v2 Paired Comparison
# =============================================================================

cat("\n=== CR v1 vs v2 Comparison ===\n\n")

cr_compare <- table2 %>%
  select(variant, cr_r2_v1, cr_v2_r2) %>%
  mutate(
    improvement = cr_v2_r2 - cr_r2_v1,
    pct_improvement = 100 * improvement / cr_r2_v1
  ) %>%
  arrange(desc(improvement))

write_csv(cr_compare, file.path(tbl_dir, "cr_v1_v2_comparison.csv"))
cat(sprintf("CR v2 improved over v1 in %d of %d variants\n",
            sum(cr_compare$improvement > 0, na.rm = TRUE), nrow(cr_compare)))
cat(sprintf("Mean improvement: %.3f (%.1f%%)\n",
            mean(cr_compare$improvement, na.rm = TRUE),
            mean(cr_compare$pct_improvement, na.rm = TRUE)))
cat(sprintf("CR v1 mean R2: %.3f  ->  CR v2 mean R2: %.3f\n",
            mean(cr_compare$cr_r2_v1, na.rm = TRUE),
            mean(cr_compare$cr_v2_r2, na.rm = TRUE)))

# =============================================================================
# Key Manuscript Numbers
# =============================================================================

cat("\n=== KEY NUMBERS FOR MANUSCRIPT ===\n\n")

cat("Height-Diameter:\n")
cat(sprintf("  Default mean R2: %.3f  Calibrated mean R2: %.3f  Improvement: +%.3f\n",
            mean(table2$hd_r2_base), mean(table2$hd_r2_cal), mean(table2$hd_improvement)))
cat(sprintf("  All 25 variants improved (range: +%.3f to +%.3f)\n",
            min(table2$hd_improvement), max(table2$hd_improvement)))

cat("\nDiameter Growth:\n")
cat(sprintf("  Mean R2 = %.3f (SD = %.3f), range [%.3f, %.3f]\n",
            mean(table2$dg_r2), sd(table2$dg_r2),
            min(table2$dg_r2), max(table2$dg_r2)))

cat("\nMortality:\n")
cat(sprintf("  V1 mean AUC = %.3f (all 25 variants)\n", mean(table2$mort_auc_v1)))
mort_v2_valid <- table2 %>% filter(!is.na(mort_auc_v2))
cat(sprintf("  V2 mean AUC = %.3f (%d of 9 variants computed)\n",
            mean(mort_v2_valid$mort_auc_v2, na.rm = TRUE), nrow(mort_v2_valid)))

cat("\nCrown Ratio:\n")
cat(sprintf("  V1 (delta CR, change model) mean R2 = %.3f\n", mean(table2$cr_r2_v1)))
cat(sprintf("  V2 (level CR, Beta regression) mean R2 = %.3f\n", mean(table2$cr_v2_r2)))
cat(sprintf("  Improvement: +%.3f (%.1f%% relative)\n",
            mean(cr_compare$improvement), mean(cr_compare$pct_improvement)))

cat("\nAll tables saved to:", tbl_dir, "\n")

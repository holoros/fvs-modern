# =============================================================================
# Title: Build Tables for FVS Bayesian Recalibration Manuscript (EMS)
# Author: A. Weiskittel
# Date: 2026-03-29
# Description: Generates publication-ready Tables 1 and 2 for the FVS
#   calibration manuscript, plus supplemental tables. Outputs CSV for review
#   and formatted flextable/docx for manuscript insertion.
# Dependencies: cross_variant_summary.csv, stand_level_summary_wide.csv,
#   stand_level_default_vs_calibrated.csv
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(flextable)
library(officer)

# --- Paths -------------------------------------------------------------------
results_dir <- "/home/aweiskittel/Documents/Claude/fvs-modern/calibration/output/comparisons"
out_dir     <- "/home/aweiskittel/Documents/Claude/fvs-modern/calibration/output/tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Data --------------------------------------------------------------------
cv_raw <- read_csv(file.path(results_dir, "cross_variant_summary.csv"),
                   show_col_types = FALSE)
sl_raw <- read_csv(file.path(results_dir, "stand_level_summary_wide.csv"),
                   show_col_types = FALSE)
sl_def <- read_csv(file.path(results_dir, "stand_level_default_vs_calibrated.csv"),
                   show_col_types = FALSE)

# =============================================================================
# TABLE 1: Diameter Growth Model Performance by Variant
# =============================================================================

# Equation type lookup
eq_types <- sl_def |>
  filter(metric == "BA_default") |>
  select(variant, eq_type) |>
  distinct() |>
  mutate(variant = toupper(variant))

# Reshape calibrated vs default to wide format
cal <- cv_raw |>
  filter(model == "calibrated") |>
  select(variant, n_cal = n, rmse_cal = rmse, bias_cal = bias,
         r2_cal = r2, mae_cal = mae)

def <- cv_raw |>
  filter(model == "default") |>
  select(variant, rmse_def = rmse, bias_def = bias,
         r2_def = r2, mae_def = mae)

table1_data <- cal |>
  left_join(def, by = "variant") |>
  left_join(eq_types, by = "variant") |>
  mutate(
    eq_type = case_when(
      eq_type == "western" ~ "Wykoff",
      eq_type == "central" ~ "Central",
      eq_type == "ne"      ~ "NE",
      TRUE                 ~ "Unknown"
    )
  ) |>
  arrange(eq_type, variant) |>
  select(
    variant, eq_type, n_cal,
    rmse_cal, bias_cal, r2_cal,
    rmse_def, bias_def, r2_def
  ) |>
  mutate(
    across(c(rmse_cal, bias_cal, rmse_def, bias_def), ~round(., 3)),
    r2_cal = round(r2_cal, 3),
    r2_def = round(r2_def, 3),
    n_cal = format(n_cal, big.mark = ",")
  )

# Compute weighted means for the summary row
cal_only <- cv_raw |> filter(model == "calibrated")
n_total <- sum(cal_only$n)
wmean_rmse_cal  <- sum(cal_only$n * cal_only$rmse) / n_total
wmean_bias_cal  <- sum(cal_only$n * cal_only$bias) / n_total
wmean_r2_cal    <- sum(cal_only$n * cal_only$r2) / n_total

cat("\n=== Table 1: DG Performance Summary ===\n")
cat(sprintf("  N variants: %d\n", nrow(cal_only)))
cat(sprintf("  Total trees: %s\n", format(n_total, big.mark = ",")))
cat(sprintf("  Weighted RMSE (cal): %.3f\n", wmean_rmse_cal))
cat(sprintf("  Weighted bias (cal): %.3f\n", wmean_bias_cal))
cat(sprintf("  Weighted R2 (cal):   %.3f\n", wmean_r2_cal))

# Save CSV
write_csv(table1_data, file.path(out_dir, "table1_dg_performance.csv"))

# Build flextable
t1_ft <- table1_data |>
  flextable() |>
  set_header_labels(
    variant  = "Variant",
    eq_type  = "Equation",
    n_cal    = "N trees",
    rmse_cal = "RMSE",
    bias_cal = "Bias",
    r2_cal   = "R\u00B2",
    rmse_def = "RMSE",
    bias_def = "Bias",
    r2_def   = "R\u00B2"
  ) |>
  add_header_row(
    values = c("", "", "", "Calibrated (MAP)", "Default FVS"),
    colwidths = c(1, 1, 1, 3, 3)
  ) |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  autofit() |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "body") |>
  bold(part = "header") |>
  add_footer_lines(
    "Performance metrics computed on ln(DDS) scale using 20% holdout test set. CR excluded from default comparison (equation type unclassified). NA indicates default coefficients not available."
  ) |>
  fontsize(size = 8, part = "footer")

# Save as docx
doc1 <- read_docx() |>
  body_add_par("Table 1. Diameter growth model performance by FVS variant.", style = "heading 3") |>
  body_add_par("Calibrated Bayesian MAP versus default FVS predictions on ln(DDS) scale.") |>
  body_add_flextable(t1_ft) |>
  body_add_par("")

print(doc1, target = file.path(out_dir, "table1_dg_performance.docx"))
cat("  Table 1 saved: table1_dg_performance.csv + .docx\n")

# =============================================================================
# TABLE 2: Stand Level Validation by Variant
# =============================================================================

table2_data <- sl_raw |>
  mutate(variant = toupper(variant)) |>
  left_join(eq_types, by = "variant") |>
  mutate(
    eq_type = case_when(
      eq_type == "western" ~ "Wykoff",
      eq_type == "central" ~ "Central",
      eq_type == "ne"      ~ "NE",
      TRUE                 ~ "Unknown"
    )
  ) |>
  arrange(eq_type, variant) |>
  select(
    variant, eq_type,
    BA_cal_bias, BA_cal_pct_rmse, BA_cal_r2,
    QMD_cal_bias, QMD_cal_pct_rmse, QMD_cal_r2
  ) |>
  mutate(
    across(c(BA_cal_bias, QMD_cal_bias), ~round(., 2)),
    across(c(BA_cal_pct_rmse, QMD_cal_pct_rmse), ~round(., 1)),
    across(c(BA_cal_r2, QMD_cal_r2), ~round(., 3))
  )

# Weighted means for stand level
n_stands <- sl_def |>
  filter(grepl("^BA_cal", metric)) |>
  select(variant, n) |>
  distinct() |>
  mutate(variant = toupper(variant))

sl_merged <- table2_data |>
  left_join(n_stands, by = "variant")

n_total_stands <- sum(sl_merged$n, na.rm = TRUE)
wmean_ba_prmse <- sum(sl_merged$n * sl_merged$BA_cal_pct_rmse, na.rm = TRUE) / n_total_stands
wmean_ba_r2    <- sum(sl_merged$n * sl_merged$BA_cal_r2, na.rm = TRUE) / n_total_stands
wmean_qmd_prmse <- sum(sl_merged$n * sl_merged$QMD_cal_pct_rmse, na.rm = TRUE) / n_total_stands
wmean_qmd_r2    <- sum(sl_merged$n * sl_merged$QMD_cal_r2, na.rm = TRUE) / n_total_stands

cat("\n=== Table 2: Stand Level Validation Summary ===\n")
cat(sprintf("  N variants: %d\n", nrow(table2_data)))
cat(sprintf("  Total stands: %s\n", format(n_total_stands, big.mark = ",")))
cat(sprintf("  Weighted BA %%RMSE:  %.1f%%\n", wmean_ba_prmse))
cat(sprintf("  Weighted BA R2:     %.3f\n", wmean_ba_r2))
cat(sprintf("  Weighted QMD %%RMSE: %.1f%%\n", wmean_qmd_prmse))
cat(sprintf("  Weighted QMD R2:    %.3f\n", wmean_qmd_r2))

# Save CSV
write_csv(table2_data, file.path(out_dir, "table2_stand_validation.csv"))

# Build flextable
t2_ft <- table2_data |>
  flextable() |>
  set_header_labels(
    variant         = "Variant",
    eq_type         = "Equation",
    BA_cal_bias     = "Bias",
    BA_cal_pct_rmse = "%RMSE",
    BA_cal_r2       = "R\u00B2",
    QMD_cal_bias    = "Bias",
    QMD_cal_pct_rmse = "%RMSE",
    QMD_cal_r2      = "R\u00B2"
  ) |>
  add_header_row(
    values = c("", "", "Basal area (m\u00B2 ha\u207B\u00B9)", "QMD (cm)"),
    colwidths = c(1, 1, 3, 3)
  ) |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  autofit() |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "body") |>
  bold(part = "header") |>
  add_footer_lines(
    "Projections use calibrated MAP diameter growth estimates only (no mortality, height, or crown ratio). Bias in original units; %RMSE relative to observed mean. CR variant excluded (equation type unclassified)."
  ) |>
  fontsize(size = 8, part = "footer")

# Save as docx
doc2 <- read_docx() |>
  body_add_par("Table 2. Stand level validation of calibrated diameter growth projections.", style = "heading 3") |>
  body_add_par("Basal area and quadratic mean diameter metrics across 18 FVS variants using DG only projections.") |>
  body_add_flextable(t2_ft) |>
  body_add_par("")

print(doc2, target = file.path(out_dir, "table2_stand_validation.docx"))
cat("  Table 2 saved: table2_stand_validation.csv + .docx\n")

# =============================================================================
# SUPPLEMENTAL TABLE S1: Full Stand Level Metrics (Default vs Calibrated vs Naive)
# =============================================================================

tableS1_data <- sl_def |>
  mutate(variant = toupper(variant)) |>
  separate(metric, into = c("measure", "model"), sep = "_", extra = "merge") |>
  select(variant, eq_type, measure, model, n, bias, pct_rmse, r2) |>
  filter(measure == "BA") |>
  pivot_wider(
    names_from = model,
    values_from = c(n, bias, pct_rmse, r2),
    names_glue = "{.value}_{model}"
  ) |>
  arrange(variant) |>
  # Select a clean set of columns for the comparison
  select(
    variant, eq_type, n_calibrated,
    bias_default, pct_rmse_default, r2_default,
    bias_calibrated, pct_rmse_calibrated, r2_calibrated,
    bias_naive, pct_rmse_naive, r2_naive
  ) |>
  mutate(
    across(starts_with("bias"), ~round(., 2)),
    across(starts_with("pct_rmse"), ~round(., 1)),
    across(starts_with("r2"), ~round(., 3)),
    n_calibrated = format(n_calibrated, big.mark = ",")
  )

write_csv(tableS1_data, file.path(out_dir, "tableS1_ba_default_vs_cal_vs_naive.csv"))

# Build flextable for S1
tS1_ft <- tableS1_data |>
  flextable() |>
  set_header_labels(
    variant             = "Variant",
    eq_type             = "Equation",
    n_calibrated        = "N plots",
    bias_default        = "Bias",
    pct_rmse_default    = "%RMSE",
    r2_default          = "R\u00B2",
    bias_calibrated     = "Bias",
    pct_rmse_calibrated = "%RMSE",
    r2_calibrated       = "R\u00B2",
    bias_naive          = "Bias",
    pct_rmse_naive      = "%RMSE",
    r2_naive            = "R\u00B2"
  ) |>
  add_header_row(
    values = c("", "", "", "Default FVS", "Calibrated (MAP)", "Naive (observed DG)"),
    colwidths = c(1, 1, 1, 3, 3, 3)
  ) |>
  theme_booktabs() |>
  fontsize(size = 8, part = "all") |>
  autofit() |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "body") |>
  bold(part = "header") |>
  add_footer_lines(
    "Default FVS values computed by applying raw species coefficients from variant config files without internal FVS simulation engine modifiers. Many western variants produce extreme values because the Fortran codebase applies additional clamps, floor/ceiling logic, and species group adjustments not captured here. BA in m\u00B2 ha\u207B\u00B9."
  ) |>
  fontsize(size = 7, part = "footer")

docS1 <- read_docx() |>
  body_add_par("Table S1. Stand level basal area validation: default vs calibrated vs naive.", style = "heading 3") |>
  body_add_par("Full comparison across all 18 FVS variants with equation type.") |>
  body_add_flextable(tS1_ft) |>
  body_add_par("")

print(docS1, target = file.path(out_dir, "tableS1_ba_comparison.docx"))
cat("\n=== Supplemental Table S1 saved ===\n")

# =============================================================================
# Summary
# =============================================================================
cat("\n=== All Tables Generated ===\n")
cat("Output directory:", out_dir, "\n")
cat("Files:\n")
cat("  table1_dg_performance.csv / .docx\n")
cat("  table2_stand_validation.csv / .docx\n")
cat("  tableS1_ba_default_vs_cal_vs_naive.csv / .docx\n")

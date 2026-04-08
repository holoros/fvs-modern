#!/usr/bin/env Rscript
# =============================================================================
# 21_ablation_comparison.R
# Ablation study comparison table for GMUG presentation
# Compares 5 configurations across 6 key stand metrics
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

project_root <- Sys.getenv("FVS_PROJECT_ROOT",
                           unset = normalizePath(file.path(dirname(sys.frame(1)$ofile), "../.."), mustWork = FALSE))
output_root  <- file.path(project_root, "calibration/output/comparisons")

cat("\n==========================================\n")
cat("ABLATION STUDY: COMPARISON TABLE\n")
cat("==========================================\n\n")

# --- Configuration labels ---------------------------------------------------
configs <- data.table(
  tag   = c("ablation_full", "ablation_no_csi", "ablation_no_sdi",
            "ablation_no_ig", "ablation_baseline"),
  label = c("Full Model", "No ClimateSI", "No SDIMAX",
            "No Ingrowth", "Baseline (Bayes only)"),
  csi   = c("ON", "OFF", "ON", "ON", "OFF"),
  sdi   = c("ON", "ON", "OFF", "ON", "OFF"),
  ig    = c("ON", "ON", "ON", "OFF", "OFF")
)

# --- Metrics of interest -----------------------------------------------------
metrics <- c("BA", "TPA", "QMD", "SDI", "VOL_CFGRS", "HT_top")
metric_labels <- c("BA (ft2/ac)", "TPA", "QMD (in)", "SDI",
                    "Volume (ft3/ac)", "Top Height (ft)")
names(metric_labels) <- metrics

# --- Read all ablation results -----------------------------------------------
results_list <- list()
for (i in seq_len(nrow(configs))) {
  tag <- configs$tag[i]
  f <- file.path(output_root, "manuscript_tables",
                 paste0("fia_benchmark_results_", tag, ".csv"))
  if (!file.exists(f)) {
    cat("  WARNING: Missing results for", tag, "\n")
    cat("    Expected:", f, "\n")
    next
  }
  dt <- fread(f)
  results_list[[tag]] <- dt
  cat("  Loaded:", tag, "  (", nrow(dt), "variants )\n")
}

if (length(results_list) == 0) {
  cat("\nNo ablation results found yet. Jobs may still be running.\n")
  quit(save = "no", status = 0)
}

cat("\n")

# --- Helper: compute overall weighted metrics from variant-level data --------
overall_metrics <- function(dt, metric) {
  # Weighted by n_conditions (or n_VOL_CFGRS_conditions for volume metrics)
  n_col <- if (grepl("VOL|HT_top", metric)) {
    paste0("n_", metric, "_conditions")
  } else {
    "n_conditions"
  }

  if (!n_col %in% names(dt)) n_col <- "n_conditions"

  # Filter to real variants (exclude empty VARIANT)
  dt2 <- dt[VARIANT != "" & !is.na(VARIANT)]

  n_vec <- dt2[[n_col]]
  if (is.null(n_vec) || all(is.na(n_vec))) n_vec <- rep(1, nrow(dt2))
  n_vec[is.na(n_vec)] <- 0
  total_n <- sum(n_vec)
  if (total_n == 0) return(list(rmse_c = NA, r2_c = NA, bias_c = NA,
                                 rmse_d = NA, r2_d = NA, bias_d = NA,
                                 rmse_pct_change = NA, n_improved = NA,
                                 n_variants = NA))

  # Calibrated
  rmse_c_col <- paste0(metric, "_RMSE_calib")
  r2_c_col   <- paste0(metric, "_r2_calib")
  bias_c_col <- paste0(metric, "_bias_calib")

  # Default
  rmse_d_col <- paste0(metric, "_RMSE_default")
  r2_d_col   <- paste0(metric, "_r2_default")
  bias_d_col <- paste0(metric, "_bias_default")

  get_wtd <- function(col) {
    if (!col %in% names(dt2)) return(NA_real_)
    vals <- dt2[[col]]
    sum(vals * n_vec, na.rm = TRUE) / total_n
  }

  rmse_c <- get_wtd(rmse_c_col)
  r2_c   <- get_wtd(r2_c_col)
  bias_c <- get_wtd(bias_c_col)
  rmse_d <- get_wtd(rmse_d_col)
  r2_d   <- get_wtd(r2_d_col)
  bias_d <- get_wtd(bias_d_col)

  # Count variants improved (lower RMSE)
  if (rmse_c_col %in% names(dt2) && rmse_d_col %in% names(dt2)) {
    n_improved <- sum(dt2[[rmse_c_col]] < dt2[[rmse_d_col]], na.rm = TRUE)
  } else {
    n_improved <- NA
  }

  rmse_pct_change <- if (!is.na(rmse_c) && !is.na(rmse_d) && rmse_d > 0) {
    (rmse_d - rmse_c) / rmse_d * 100
  } else NA_real_

  list(rmse_c = rmse_c, r2_c = r2_c, bias_c = bias_c,
       rmse_d = rmse_d, r2_d = r2_d, bias_d = bias_d,
       rmse_pct_change = rmse_pct_change,
       n_improved = n_improved,
       n_variants = nrow(dt2))
}

# --- Build the summary table -------------------------------------------------
rows <- list()
for (i in seq_len(nrow(configs))) {
  tag <- configs$tag[i]
  if (!tag %in% names(results_list)) next

  dt <- results_list[[tag]]
  for (m in metrics) {
    om <- overall_metrics(dt, m)
    rows[[length(rows) + 1]] <- data.table(
      config       = configs$label[i],
      ClimateSI    = configs$csi[i],
      SDIMAX       = configs$sdi[i],
      Ingrowth     = configs$ig[i],
      metric       = m,
      metric_label = metric_labels[m],
      RMSE_calib   = om$rmse_c,
      R2_calib     = om$r2_c,
      Bias_calib   = om$bias_c,
      RMSE_default = om$rmse_d,
      R2_default   = om$r2_d,
      Bias_default = om$bias_d,
      RMSE_pct_reduction = om$rmse_pct_change,
      n_improved   = om$n_improved,
      n_variants   = om$n_variants
    )
  }
}

summary_dt <- rbindlist(rows)

# --- Print summary table to console ------------------------------------------
cat("==========================================\n")
cat("ABLATION RESULTS: RMSE % Reduction vs Default FVS\n")
cat("==========================================\n\n")

# Pivot: configs as rows, metrics as columns
pivot <- dcast(summary_dt, config + ClimateSI + SDIMAX + Ingrowth ~ metric,
               value.var = "RMSE_pct_reduction")

# Reorder columns
col_order <- c("config", "ClimateSI", "SDIMAX", "Ingrowth", metrics)
col_order <- col_order[col_order %in% names(pivot)]
pivot <- pivot[, ..col_order]

# Reorder rows by config factor
pivot[, config := factor(config, levels = configs$label)]
setorder(pivot, config)

cat(format(pivot, digits = 3), sep = "\n")

cat("\n\n==========================================\n")
cat("ABLATION RESULTS: R-squared (Calibrated)\n")
cat("==========================================\n\n")

pivot_r2 <- dcast(summary_dt, config + ClimateSI + SDIMAX + Ingrowth ~ metric,
                  value.var = "R2_calib")
col_order_r2 <- c("config", "ClimateSI", "SDIMAX", "Ingrowth", metrics)
col_order_r2 <- col_order_r2[col_order_r2 %in% names(pivot_r2)]
pivot_r2 <- pivot_r2[, ..col_order_r2]
pivot_r2[, config := factor(config, levels = configs$label)]
setorder(pivot_r2, config)

cat(format(pivot_r2, digits = 3), sep = "\n")

cat("\n\n==========================================\n")
cat("ABLATION RESULTS: Variants Improved (out of N)\n")
cat("==========================================\n\n")

pivot_imp <- dcast(summary_dt, config + ClimateSI + SDIMAX + Ingrowth ~ metric,
                   value.var = "n_improved")
col_order_imp <- c("config", "ClimateSI", "SDIMAX", "Ingrowth", metrics)
col_order_imp <- col_order_imp[col_order_imp %in% names(pivot_imp)]
pivot_imp <- pivot_imp[, ..col_order_imp]
pivot_imp[, config := factor(config, levels = configs$label)]
setorder(pivot_imp, config)

cat(format(pivot_imp), sep = "\n")

cat("\n\n==========================================\n")
cat("ABLATION RESULTS: Bias (Calibrated)\n")
cat("==========================================\n\n")

pivot_bias <- dcast(summary_dt, config + ClimateSI + SDIMAX + Ingrowth ~ metric,
                    value.var = "Bias_calib")
col_order_bias <- c("config", "ClimateSI", "SDIMAX", "Ingrowth", metrics)
col_order_bias <- col_order_bias[col_order_bias %in% names(pivot_bias)]
pivot_bias <- pivot_bias[, ..col_order_bias]
pivot_bias[, config := factor(config, levels = configs$label)]
setorder(pivot_bias, config)

cat(format(pivot_bias, digits = 3), sep = "\n")

# --- Incremental contribution analysis --------------------------------------
cat("\n\n==========================================\n")
cat("INCREMENTAL CONTRIBUTION ANALYSIS\n")
cat("==========================================\n\n")

# Compare each removal config to the full model to quantify marginal contribution
if ("ablation_full" %in% names(results_list)) {
  for (m in metrics) {
    full_row <- summary_dt[config == "Full Model" & metric == m]
    base_row <- summary_dt[config == "Baseline (Bayes only)" & metric == m]

    if (nrow(full_row) == 0 || nrow(base_row) == 0) next

    cat(sprintf("--- %s ---\n", metric_labels[m]))
    cat(sprintf("  Baseline RMSE reduction:  %+.1f%%\n", base_row$RMSE_pct_reduction))
    cat(sprintf("  Full model RMSE reduction: %+.1f%%\n", full_row$RMSE_pct_reduction))

    total_gain <- full_row$RMSE_pct_reduction - base_row$RMSE_pct_reduction
    cat(sprintf("  Total gain from enhancements: %.1f pp\n", total_gain))

    # Marginal contributions (Shapley-style: difference when removed)
    for (j in 2:4) {
      tag_j <- configs$tag[j]
      label_j <- configs$label[j]
      removed <- summary_dt[config == label_j & metric == m]
      if (nrow(removed) > 0) {
        marginal <- full_row$RMSE_pct_reduction - removed$RMSE_pct_reduction
        cat(sprintf("  Marginal contribution of %s: %+.1f pp\n",
                    gsub("No ", "", label_j), marginal))
      }
    }
    cat("\n")
  }
}

# --- Save outputs ------------------------------------------------------------
fwrite(summary_dt, file.path(output_root, "manuscript_tables",
                              "ablation_comparison_summary.csv"))
cat("Saved: ablation_comparison_summary.csv\n")

# Also save the pivoted RMSE reduction table for easy use in presentations
fwrite(pivot, file.path(output_root, "manuscript_tables",
                         "ablation_rmse_reduction_pivot.csv"))
cat("Saved: ablation_rmse_reduction_pivot.csv\n")

# --- Figure: grouped bar chart of RMSE % reduction by config -----------------
plot_dt <- summary_dt[!is.na(RMSE_pct_reduction)]
plot_dt[, config := factor(config, levels = configs$label)]
plot_dt[, metric_label := factor(metric_label, levels = metric_labels)]

if (nrow(plot_dt) > 0) {
  p <- ggplot(plot_dt, aes(x = metric_label, y = RMSE_pct_reduction, fill = config)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
    scale_fill_manual(values = c("Full Model" = "#2166ac",
                                  "No ClimateSI" = "#67a9cf",
                                  "No SDIMAX" = "#d1e5f0",
                                  "No Ingrowth" = "#fddbc7",
                                  "Baseline (Bayes only)" = "#ef8a62")) +
    labs(title = "Ablation Study: RMSE Reduction vs Default FVS",
         subtitle = "Positive = improvement over default FVS; each bar removes one component",
         x = NULL, y = "RMSE Reduction (%)",
         fill = "Configuration") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          plot.title = element_text(face = "bold")) +
    guides(fill = guide_legend(nrow = 2))

  ggsave(file.path(output_root, "manuscript_figures",
                    "ablation_rmse_reduction_bars.png"),
         p, width = 10, height = 6, dpi = 300)
  cat("Saved: ablation_rmse_reduction_bars.png\n")

  # --- Figure: R-squared comparison ---
  p2 <- ggplot(plot_dt, aes(x = metric_label, y = R2_calib, fill = config)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("Full Model" = "#2166ac",
                                  "No ClimateSI" = "#67a9cf",
                                  "No SDIMAX" = "#d1e5f0",
                                  "No Ingrowth" = "#fddbc7",
                                  "Baseline (Bayes only)" = "#ef8a62")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Ablation Study: Calibrated R-squared by Configuration",
         x = NULL, y = expression(R^2),
         fill = "Configuration") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          plot.title = element_text(face = "bold")) +
    guides(fill = guide_legend(nrow = 2))

  ggsave(file.path(output_root, "manuscript_figures",
                    "ablation_r2_bars.png"),
         p2, width = 10, height = 6, dpi = 300)
  cat("Saved: ablation_r2_bars.png\n")
}

# --- Variant-level detail table (for appendix) -------------------------------
if (length(results_list) >= 2) {
  variant_detail <- list()
  for (i in seq_len(nrow(configs))) {
    tag <- configs$tag[i]
    if (!tag %in% names(results_list)) next
    dt <- results_list[[tag]]
    dt2 <- dt[VARIANT != "" & !is.na(VARIANT)]
    for (m in metrics) {
      rmse_c_col <- paste0(m, "_RMSE_calib")
      rmse_d_col <- paste0(m, "_RMSE_default")
      r2_c_col   <- paste0(m, "_r2_calib")

      if (!rmse_c_col %in% names(dt2)) next

      vdt <- dt2[, .(VARIANT,
                      RMSE_calib = get(rmse_c_col),
                      RMSE_default = get(rmse_d_col),
                      R2_calib = get(r2_c_col))]
      vdt[, RMSE_pct_reduction := (RMSE_default - RMSE_calib) / RMSE_default * 100]
      vdt[, config := configs$label[i]]
      vdt[, metric := m]
      variant_detail[[length(variant_detail) + 1]] <- vdt
    }
  }

  if (length(variant_detail) > 0) {
    variant_dt <- rbindlist(variant_detail)
    fwrite(variant_dt, file.path(output_root, "manuscript_tables",
                                  "ablation_variant_detail.csv"))
    cat("Saved: ablation_variant_detail.csv\n")
  }
}

cat("\n==========================================\n")
cat("ABLATION COMPARISON COMPLETE\n")
cat("==========================================\n")

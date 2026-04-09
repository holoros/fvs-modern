# =============================================================================
# Title: Stand-Level Projection Engine with Annualized Components
# Author: A. Weiskittel
# Date: 2026-04-03
# Description: Proper stand-level forward projection engine that:
#   (1) Correctly annualizes mortality (period survival -> annual survival)
#   (2) Uses calibrated DG Wykoff parameters with annualization
#   (3) Compares calibrated vs default FVS-like parameters
#   (4) Diagnoses the measurement interval issue
#
#   Key finding: the original assessment projections applied period-level
#   mortality (5-10 year survival) annually, causing catastrophic BA collapse.
#   FIA measurement intervals: NE=5yr, CA/BM=10yr, AK=11yr.
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)

# --- Paths -------------------------------------------------------------------
base_dir   <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                        normalizePath(file.path(dirname(sys.frame(1)$ofile), "../.."), mustWork = FALSE)),
                        "calibration")
output_base <- file.path(base_dir, "output", "variants")
data_base   <- file.path(base_dir, "data", "processed")
fig_dir     <- file.path(base_dir, "output", "comparisons", "manuscript_figures")
tbl_dir     <- file.path(base_dir, "output", "comparisons", "manuscript_tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir, showWarnings = FALSE, recursive = TRUE)

# --- Theme -------------------------------------------------------------------
theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    strip.text   = element_text(size = 10, face = "bold"),
    plot.title   = element_text(size = 12, face = "bold"),
    plot.margin  = margin(5, 8, 5, 5)
  )

ALL_VARIANTS <- c("acd", "ak", "bc", "bm", "ca", "ci", "cr", "cs", "ec",
                   "em", "ie", "kt", "ls", "nc", "ne", "oc", "on", "op",
                   "pn", "sn", "so", "tt", "ut", "wc", "ws")

# =============================================================================
# STEP 1: Determine measurement intervals by variant
# =============================================================================

cat("=== Step 1: Measurement intervals ===\n")

intervals <- list()
for (v in ALL_VARIANTS) {
  data_file <- file.path(data_base, v, "diameter_growth.csv")
  if (!file.exists(data_file)) next
  d <- read_csv(data_file, show_col_types = FALSE, n_max = 50000)
  if ("years_interval" %in% names(d)) {
    intervals[[v]] <- tibble(
      variant = toupper(v),
      mean_interval = mean(d$years_interval, na.rm = TRUE),
      median_interval = median(d$years_interval, na.rm = TRUE)
    )
  }
}
interval_df <- bind_rows(intervals)
cat("Measurement intervals:\n")
print(interval_df, n = 25)

# =============================================================================
# STEP 2: Load calibrated parameters for each variant
# =============================================================================

cat("\n=== Step 2: Loading calibrated parameters ===\n")

load_full_params <- function(v) {
  params <- list(variant = v)

  # Measurement interval
  int_row <- interval_df %>% filter(variant == toupper(v))
  params$meas_interval <- if (nrow(int_row) > 0) int_row$mean_interval[1] else 7

  # H-D parameters
  hd_file <- file.path(output_base, v, "height_diameter_samples.rds")
  if (file.exists(hd_file)) {
    draws <- tryCatch(readRDS(hd_file), error = function(e) NULL)
    if (!is.null(draws)) {
      d <- as_tibble(draws)
      params$hd <- list(
        a = median(d$b_a_Intercept, na.rm = TRUE),
        b = median(d$b_b_Intercept, na.rm = TRUE),
        c = median(d$b_c_Intercept, na.rm = TRUE)
      )
    }
  }

  # DG Wykoff parameters
  dg_file <- file.path(output_base, v, "diameter_growth_samples.rds")
  std_file <- file.path(output_base, v, "standardization_params.rds")
  if (file.exists(dg_file)) {
    draws <- tryCatch(readRDS(dg_file), error = function(e) NULL)
    if (!is.null(draws)) {
      d <- as_tibble(draws)
      get_par <- function(nm) {
        if (nm %in% names(d)) median(d[[nm]], na.rm = TRUE) else 0
      }
      params$dg <- list(
        mu_b0 = get_par("mu_b0"),
        betas = sapply(paste0("b", 1:13), get_par),
        sigma = get_par("sigma")
      )
      # Species intercepts
      sp_cols <- names(d)[grepl("^b0\\[", names(d))]
      params$dg$sp_b0 <- sapply(sp_cols, function(x) median(d[[x]], na.rm = TRUE))
    }
  }
  if (file.exists(std_file)) {
    params$dg_std <- tryCatch(readRDS(std_file), error = function(e) NULL)
  }

  # Mortality parameters
  mort_file <- file.path(output_base, v, "mortality_samples.rds")
  if (file.exists(mort_file)) {
    draws <- tryCatch(readRDS(mort_file), error = function(e) NULL)
    if (!is.null(draws)) {
      d <- as_tibble(draws)
      get_fe <- function(nm) {
        col <- paste0("b_", nm)
        if (col %in% names(d)) median(d[[col]], na.rm = TRUE) else 0
      }
      params$mort <- list(
        b0 = get_fe("Intercept"), b1 = get_fe("DBH_std"),
        b2 = get_fe("IDBH_stdE2"), b3 = get_fe("BAL_std"),
        b4 = get_fe("CR_std"), b5 = get_fe("SI_std"), b6 = get_fe("BA_std")
      )
    }
  }

  # SDIMAX
  cal_file <- file.path(output_base, v, "species_sdimax_calibrated.csv")
  if (file.exists(cal_file)) {
    sdi <- read_csv(cal_file, show_col_types = FALSE)
    sdi_col <- if ("sdimax_combined" %in% names(sdi)) "sdimax_combined" else "sdimax_bayes"
    if (sdi_col %in% names(sdi)) {
      params$sdimax <- mean(sdi[[sdi_col]], na.rm = TRUE)
    }
  }

  params
}

# =============================================================================
# STEP 3: Projection engine (corrected)
# =============================================================================

simulate_stand_v2 <- function(trees, params, si = 65, years = 50,
                               use_calibrated_dg = TRUE,
                               use_calibrated_mort = TRUE,
                               annualize = TRUE,
                               default_annual_mort_rate = 0.015,
                               default_annual_di = 0.10) {
  # trees: data.frame with: dbh, tpa, spcd
  # annualize: if TRUE, convert period-level predictions to annual
  # default_*: used when use_calibrated_* = FALSE

  meas_int <- params$meas_interval
  trajectory <- vector("list", years + 1)

  for (yr in 0:years) {
    # --- Compute stand metrics ---
    ba_tree <- pi * (trees$dbh / 24)^2
    ba  <- sum(trees$tpa * ba_tree, na.rm = TRUE)
    tpa <- sum(trees$tpa, na.rm = TRUE)
    qmd <- ifelse(!is.na(tpa) && tpa > 0 && !is.na(ba) && ba > 0,
                  sqrt(ba / tpa * 576 / pi), 0)
    sdi <- ifelse(qmd > 0, tpa * (qmd / 10)^1.605, 0)

    # --- H-D (calibrated always, just for top height) ---
    if (!is.null(params$hd)) {
      trees$ht <- 4.5 + pmax(params$hd$a, 1) *
        (1 - exp(-pmax(params$hd$b, 0.001) * trees$dbh / 20))^pmax(params$hd$c, 0.1)
    } else {
      trees$ht <- 4.5 + 100 * (1 - exp(-0.03 * trees$dbh))^1.3
    }
    top_ht <- if (nrow(trees) > 0 && any(is.finite(trees$ht))) {
      ht_sorted <- sort(trees$ht[is.finite(trees$ht)], decreasing = TRUE)
      mean(ht_sorted[1:min(5, length(ht_sorted))])
    } else 0

    trajectory[[yr + 1]] <- tibble(
      year = yr, tpa = tpa, ba = ba, qmd = qmd, sdi = sdi,
      top_ht = top_ht, n_trees = nrow(trees)
    )

    if (yr == years || nrow(trees) == 0) break

    # Sort for BAL (recompute ba_tree after sort to keep alignment)
    trees <- trees %>% arrange(desc(dbh))
    ba_tree <- pi * (trees$dbh / 24)^2
    trees$bal <- cumsum(trees$tpa * ba_tree) - trees$tpa * ba_tree
    if (!("cr" %in% names(trees))) trees$cr <- 0.40

    # Helper: safe standardization that won't NaN on constant/single values
    safe_scale <- function(x) {
      if (length(x) < 2) return(rep(0, length(x)))
      s <- sd(x, na.rm = TRUE)
      m <- mean(x, na.rm = TRUE)
      if (is.na(s) || s < 1e-10) return(rep(0, length(x)))
      (x - m) / s
    }

    # --- MORTALITY ---
    if (use_calibrated_mort && !is.null(params$mort)) {
      m <- params$mort
      dbh_s <- safe_scale(trees$dbh)
      bal_s <- safe_scale(trees$bal)
      cr_s  <- safe_scale(trees$cr)

      logit_surv_period <- m$b0 + m$b1 * dbh_s + m$b2 * dbh_s^2 +
                           m$b3 * bal_s + m$b4 * cr_s + m$b5 * 0 + m$b6 * 0
      logit_surv_period[!is.finite(logit_surv_period)] <- 2  # default high survival
      p_surv_period <- plogis(logit_surv_period)
      p_surv_period <- pmin(pmax(p_surv_period, 0.01), 0.999)

      if (annualize) {
        # Convert period survival to annual survival
        p_surv_annual <- p_surv_period^(1 / meas_int)
      } else {
        # Bug: applying period survival as if annual (the original error)
        p_surv_annual <- p_surv_period
      }
    } else {
      # Default FVS-like mortality: background rate modified by competition
      # Typical FVS produces 1-2% annual mortality with density-dependent amplification
      base_mort <- default_annual_mort_rate
      # Small tree mortality modifier (higher for saplings)
      size_mod <- ifelse(trees$dbh < 5, 1.5, ifelse(trees$dbh > 20, 1.2, 1.0))
      # Competition modifier (higher BA -> higher mortality)
      comp_mod <- 1 + 0.5 * pmin(ba / 200, 1)
      p_surv_annual <- 1 - base_mort * size_mod * comp_mod
      p_surv_annual <- pmin(pmax(p_surv_annual, 0.50), 0.999)
    }

    # Apply stochastic mortality
    set.seed(yr * 1000 + which(ALL_VARIANTS == tolower(params$variant))[1])
    alive <- runif(nrow(trees)) < p_surv_annual
    trees <- trees[alive, ]
    if (nrow(trees) == 0) break

    # --- DIAMETER GROWTH ---
    if (use_calibrated_dg && !is.null(params$dg) && !is.null(params$dg_std)) {
      std <- params$dg_std
      dg <- params$dg
      safe_std <- function(x, nm) {
        mn <- std[[paste0(nm, "_mean")]]; sd_val <- std[[paste0(nm, "_sd")]]
        if (!is.null(mn) && !is.null(sd_val) && !is.na(sd_val) && sd_val > 1e-10) {
          return((x - mn) / sd_val)
        }
        # Fallback: center on mean, no scaling (all constant -> 0)
        rep(0, length(x))
      }

      ln_dbh <- log(pmax(trees$dbh, 0.1))
      dbh_sq <- trees$dbh^2
      ln_si  <- log(pmax(si, 1))
      slope  <- 15  # assume moderate slope
      elev   <- 3000 # assume mid-elevation

      ln_dbh_s <- safe_std(ln_dbh, "ln_DBH")
      dbh_sq_s <- safe_std(dbh_sq, "DBH_sq")
      ln_si_s  <- safe_std(rep(ln_si, nrow(trees)), "ln_SI")
      slope_s  <- safe_std(rep(slope, nrow(trees)), "SLOPE")
      elev_s   <- safe_std(rep(elev, nrow(trees)), "ELEV")
      bal_s    <- safe_std(trees$bal, "BAL")
      ba_s     <- safe_std(rep(ba, nrow(trees)), "BA")
      cr_prop  <- trees$cr

      # Replace any remaining NaN/NA with 0
      ln_dbh_s[!is.finite(ln_dbh_s)] <- 0
      dbh_sq_s[!is.finite(dbh_sq_s)] <- 0
      ln_si_s[!is.finite(ln_si_s)]   <- 0
      slope_s[!is.finite(slope_s)]   <- 0
      elev_s[!is.finite(elev_s)]     <- 0
      bal_s[!is.finite(bal_s)]       <- 0
      ba_s[!is.finite(ba_s)]         <- 0
      cr_prop[!is.finite(cr_prop)]   <- 0.40

      # Species index: use mu_b0 as default since we don't know exact species
      sp_int <- ifelse(is.finite(dg$mu_b0), dg$mu_b0, 0)

      b <- dg$betas
      b[!is.finite(b)] <- 0  # guard against NA betas

      pred_ln_dds_period <- sp_int +
        b[1]*ln_dbh_s + b[2]*dbh_sq_s + b[3]*ln_si_s +
        b[4]*slope_s + b[5]*slope_s^2 + b[6]*0 + b[7]*0 +
        b[8]*elev_s + b[9]*elev_s^2 +
        b[10]*cr_prop + b[11]*cr_prop^2 + b[12]*bal_s + b[13]*ba_s

      # Guard against NaN in predictions
      pred_ln_dds_period[!is.finite(pred_ln_dds_period)] <- log(0.5)

      # Back-transform: DDS = exp(ln_DDS)
      # This DDS is over the measurement period
      sigma_sq <- ifelse(is.finite(dg$sigma), dg$sigma^2, 0)
      dds_period <- exp(pred_ln_dds_period + sigma_sq / 2)  # bias correction

      if (annualize) {
        # Annualize: DDS is change in squared diameter over period
        # For annual step: DDS_annual = DDS_period / interval
        dds_annual <- dds_period / meas_int
      } else {
        dds_annual <- dds_period
      }

      # Convert DDS to diameter increment: D2 = sqrt(D1^2 + DDS)
      dds_annual <- pmax(dds_annual, 0, na.rm = TRUE)  # no negative growth
      dds_annual[!is.finite(dds_annual)] <- 0
      d2 <- sqrt(trees$dbh^2 + dds_annual)
      trees$dbh <- pmax(d2, trees$dbh)  # trees don't shrink

    } else {
      # Default DG: simple annual increment
      # FVS-like: ~0.1 in/yr average, modified by competition and size
      competition_mod <- pmax(0.3, 1 - trees$bal / pmax(ba + 1, 10))
      size_mod <- pmax(0.5, 1 - (trees$dbh / 30)^0.5)
      trees$dbh <- trees$dbh + default_annual_di * competition_mod * size_mod
    }

    # CR change
    trees$cr <- pmax(0.10, pmin(0.95, trees$cr - 0.003 * pmin(ba / 150, 1)))

    # SDIMAX enforcement
    if (!is.null(params$sdimax) && nrow(trees) > 0) {
      current_tpa <- sum(trees$tpa, na.rm = TRUE)
      if (!is.na(current_tpa) && current_tpa > 0) {
        current_ba  <- sum(trees$tpa * pi * (trees$dbh / 24)^2, na.rm = TRUE)
        current_qmd <- sqrt(current_ba / current_tpa * 576 / pi)
        if (!is.na(current_qmd) && current_qmd > 0) {
          current_sdi <- current_tpa * (current_qmd / 10)^1.605
          if (!is.na(current_sdi) && current_sdi > params$sdimax) {
            trees$tpa <- trees$tpa * (params$sdimax / current_sdi)
          }
        }
      }
    }
  }

  bind_rows(trajectory)
}

# =============================================================================
# STEP 4: Run all three projection scenarios
# =============================================================================

cat("\n=== Step 3: Running projections (3 scenarios per variant) ===\n")

results_all <- list()

for (v in ALL_VARIANTS) {
  cat("  ", toupper(v), "... ")

  params <- load_full_params(v)
  if (is.null(params$hd) || is.null(params$mort)) {
    cat("skip (incomplete params)\n"); next
  }

  # Build initial stand (same as before)
  set.seed(42)
  n_init <- 200
  init_trees <- tibble(
    dbh = rlnorm(n_init, log(6), 0.5),
    tpa = 1,
    spcd = sample(c(97, 316), n_init, replace = TRUE)
  ) %>% filter(dbh >= 1, dbh <= 30)

  # Scenario 1: Calibrated + annualized (correct)
  t1 <- simulate_stand_v2(init_trees, params, si = 65, years = 50,
                           use_calibrated_dg = TRUE,
                           use_calibrated_mort = TRUE,
                           annualize = TRUE)
  t1$scenario <- "Calibrated (annualized)"

  # Scenario 2: Default FVS-like parameters
  t2 <- simulate_stand_v2(init_trees, params, si = 65, years = 50,
                           use_calibrated_dg = FALSE,
                           use_calibrated_mort = FALSE,
                           annualize = TRUE,
                           default_annual_mort_rate = 0.015,
                           default_annual_di = 0.10)
  t2$scenario <- "Default FVS-like"

  # Scenario 3: Calibrated but NOT annualized (original bug)
  t3 <- simulate_stand_v2(init_trees, params, si = 65, years = 50,
                           use_calibrated_dg = TRUE,
                           use_calibrated_mort = TRUE,
                           annualize = FALSE)
  t3$scenario <- "Calibrated (period-level bug)"

  combined <- bind_rows(t1, t2, t3) %>%
    mutate(variant = toupper(v))

  results_all[[v]] <- combined
  cat("BA final: cal=", round(tail(t1$ba, 1), 0),
      " default=", round(tail(t2$ba, 1), 0),
      " raw=", round(tail(t3$ba, 1), 0), "\n")
}

proj_all <- bind_rows(results_all)
write_csv(proj_all, file.path(base_dir, "output", "comparisons",
                               "stand_projections_3scenarios.csv"))

# =============================================================================
# STEP 5: Figures
# =============================================================================

cat("\n=== Step 4: Generating figures ===\n")

# Regional grouping
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

proj_all <- proj_all %>% left_join(region_map, by = "variant")

# --- Figure A: BA trajectories by scenario (faceted by scenario) ---
fig_ba_scenario <- ggplot(proj_all, aes(x = year, y = ba,
                                          group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  geom_hline(yintercept = 60.15, linetype = "dashed", color = "grey40",
             linewidth = 0.3) +
  facet_wrap(~ scenario, nrow = 1) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Projection year",
       y = expression("Basal area (ft"^2~"ac"^{-1}*")"),
       color = "Region",
       title = "Basal Area Trajectories: Calibrated vs Default Parameters",
       subtitle = "Initial: 200 TPA, 60 BA, ~6 in. QMD, SI = 65") +
  theme_pub +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"))

ggsave(file.path(fig_dir, "fig_ba_3scenarios.png"), fig_ba_scenario,
       width = 14, height = 5.5, dpi = 300)
cat("Saved fig_ba_3scenarios.png\n")

# --- Figure B: 6-panel stand metrics (calibrated annualized vs default) ---
# Exclude the bug scenario for the clean comparison
proj_clean <- proj_all %>% filter(scenario != "Calibrated (period-level bug)")

# BA
p1 <- ggplot(proj_clean %>% filter(scenario == "Calibrated (annualized)"),
             aes(x = year, y = ba, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "grey40") +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = expression("BA (ft"^2~"ac"^{-1}*")"),
       title = "Calibrated") +
  ylim(0, NA) + theme_pub

p2 <- ggplot(proj_clean %>% filter(scenario == "Default FVS-like"),
             aes(x = year, y = ba, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "grey40") +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = NULL, title = "Default") +
  ylim(0, NA) + theme_pub

# TPA
p3 <- ggplot(proj_clean %>% filter(scenario == "Calibrated (annualized)"),
             aes(x = year, y = tpa, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = expression("TPA (trees ac"^{-1}*")")) +
  ylim(0, NA) + theme_pub

p4 <- ggplot(proj_clean %>% filter(scenario == "Default FVS-like"),
             aes(x = year, y = tpa, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = NULL) +
  ylim(0, NA) + theme_pub

# QMD
p5 <- ggplot(proj_clean %>% filter(scenario == "Calibrated (annualized)"),
             aes(x = year, y = qmd, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Year", y = "QMD (in.)", color = "Region") +
  theme_pub + theme(legend.key.size = unit(0.4, "cm"))

p6 <- ggplot(proj_clean %>% filter(scenario == "Default FVS-like"),
             aes(x = year, y = qmd, group = variant, color = region)) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", guide = "none") +
  labs(x = "Year", y = NULL) +
  theme_pub

fig_6panel <- (p1 | p2) / (p3 | p4) / (p5 | p6) +
  plot_annotation(
    title = "Stand-Level Projections: Calibrated (Annualized) vs Default FVS",
    subtitle = "50-year projections from identical initial conditions (200 TPA, SI=65)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  )

ggsave(file.path(fig_dir, "fig_stand_calibrated_vs_default.png"), fig_6panel,
       width = 11, height = 11, dpi = 300)
cat("Saved fig_stand_calibrated_vs_default.png\n")

# --- Figure C: Final BA comparison (year 50) ---
final_ba <- proj_all %>%
  group_by(variant, scenario, region) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(variant, scenario, ba, tpa, qmd, sdi, region) %>%
  pivot_wider(names_from = scenario, values_from = c(ba, tpa, qmd, sdi),
              names_sep = "_")

fig_ba_scatter <- ggplot(final_ba,
       aes(x = `ba_Default FVS-like`,
           y = `ba_Calibrated (annualized)`,
           color = region, label = variant)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.5, nudge_y = 1.5, check_overlap = TRUE) +
  scale_color_brewer(palette = "Set2") +
  labs(x = expression("Default BA at year 50 (ft"^2~"ac"^{-1}*")"),
       y = expression("Calibrated BA at year 50 (ft"^2~"ac"^{-1}*")"),
       color = "Region",
       title = "Final BA: Calibrated (Annualized) vs Default",
       subtitle = "Points above 1:1 line = calibrated grows more") +
  theme_pub

ggsave(file.path(fig_dir, "fig_ba_final_scatter.png"), fig_ba_scatter,
       width = 8, height = 7, dpi = 300)
cat("Saved fig_ba_final_scatter.png\n")

# =============================================================================
# STEP 6: Summary statistics
# =============================================================================

cat("\n=== Summary Statistics ===\n\n")

scenario_summary <- proj_all %>%
  group_by(scenario) %>%
  filter(year == max(year)) %>%
  summarise(
    n_variants = n_distinct(variant),
    mean_ba = mean(ba, na.rm = TRUE),
    sd_ba = sd(ba, na.rm = TRUE),
    mean_tpa = mean(tpa, na.rm = TRUE),
    mean_qmd = mean(qmd, na.rm = TRUE),
    n_ba_positive = sum(ba > 30),
    .groups = "drop"
  )
print(scenario_summary)

# Realism check for calibrated annualized
cal_ann <- proj_all %>%
  filter(scenario == "Calibrated (annualized)") %>%
  group_by(variant) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    ba_change_pct = 100 * (ba - 60.15) / 60.15,
    realistic = ba > 30 & ba < 200
  )

cat("\nCalibrated (annualized) realism:\n")
cat(sprintf("  BA > 30 at year 50: %d / %d variants\n",
            sum(cal_ann$realistic), nrow(cal_ann)))
cat(sprintf("  Mean final BA: %.1f ft2/ac (initial: 60.2)\n",
            mean(cal_ann$ba)))

write_csv(cal_ann %>% select(variant, ba, tpa, qmd, sdi, ba_change_pct, realistic),
          file.path(tbl_dir, "stand_projection_realism_annualized.csv"))

cat("\n=== All projections and figures complete ===\n")

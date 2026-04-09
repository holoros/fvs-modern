#!/usr/bin/env Rscript
# =============================================================================
# Title: Uncertainty Propagation via Posterior Draws
# Author: A. Weiskittel
# Date: 2026-04-04
# Description: Propagates parameter uncertainty through stand projections by
#   sampling from posterior draws for diameter growth, mortality, and height-diameter.
#   Produces trajectory envelopes with credible intervals.
#
#   Key workflow:
#   (1) Load posterior draws for each variant (DG, mortality, H-D)
#   (2) Sample n_draws parameter sets from posterior
#   (3) Run full projection for each draw
#   (4) Compute percentile-based credible intervals
#   (5) Generate publication-ready figures with ribbons and facets
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(future)
library(future.apply)
library(data.table)

# --- Paths -------------------------------------------------------------------
base_dir   <- file.path(Sys.getenv("FVS_PROJECT_ROOT",
                        normalizePath(file.path(dirname(sys.frame(1)$ofile), "../.."), mustWork = FALSE)),
                        "calibration")
output_base <- file.path(base_dir, "output", "variants")
comp_base   <- file.path(base_dir, "output", "comparisons")
fig_dir     <- file.path(comp_base, "manuscript_figures")
tbl_dir     <- file.path(comp_base, "manuscript_tables")
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

# =============================================================================
# SECTION 1: Load and sample posterior draws
# =============================================================================

cat("=== Section 1: Load posterior draws ===\n")

load_posterior_draws <- function(variant, n_draws = 200, seed = 123) {
  # Load posterior samples for a single variant
  # Returns list with dg, mort, hd draw indices
  set.seed(seed + which(ALL_VARIANTS == tolower(variant)))

  params <- list(variant = variant)

  # Measurement interval
  data_file <- file.path(base_dir, "data", "processed", tolower(variant),
                         "diameter_growth.csv")
  if (file.exists(data_file)) {
    d <- tryCatch(read_csv(data_file, show_col_types = FALSE, n_max = 10000),
                  error = function(e) NULL)
    if (!is.null(d) && "years_interval" %in% names(d)) {
      params$meas_interval <- mean(d$years_interval, na.rm = TRUE)
    } else {
      params$meas_interval <- 7
    }
  } else {
    params$meas_interval <- 7
  }

  # Height-diameter samples
  hd_file <- file.path(output_base, tolower(variant), "height_diameter_samples.rds")
  if (file.exists(hd_file)) {
    hd_draws <- tryCatch(readRDS(hd_file), error = function(e) NULL)
    if (!is.null(hd_draws)) {
      hd_df <- as_tibble(hd_draws)
      n_hd <- nrow(hd_df)
      hd_indices <- sample(seq_len(n_hd), min(n_draws, n_hd), replace = FALSE)
      params$hd_samples <- hd_df[hd_indices, ]
      params$n_hd_draws <- length(hd_indices)
    }
  }

  # Diameter growth samples
  dg_file <- file.path(output_base, tolower(variant), "diameter_growth_samples.rds")
  if (file.exists(dg_file)) {
    dg_draws <- tryCatch(readRDS(dg_file), error = function(e) NULL)
    if (!is.null(dg_draws)) {
      dg_df <- as_tibble(dg_draws)
      n_dg <- nrow(dg_df)
      dg_indices <- sample(seq_len(n_dg), min(n_draws, n_dg), replace = FALSE)
      params$dg_samples <- dg_df[dg_indices, ]
      params$n_dg_draws <- length(dg_indices)
    }
  }

  # Mortality samples
  mort_file <- file.path(output_base, tolower(variant), "mortality_samples.rds")
  if (file.exists(mort_file)) {
    mort_draws <- tryCatch(readRDS(mort_file), error = function(e) NULL)
    if (!is.null(mort_draws)) {
      mort_df <- as_tibble(mort_draws)
      n_mort <- nrow(mort_df)
      mort_indices <- sample(seq_len(n_mort), min(n_draws, n_mort), replace = FALSE)
      params$mort_samples <- mort_df[mort_indices, ]
      params$n_mort_draws <- length(mort_indices)
    }
  }

  # Standardization params (for DG)
  std_file <- file.path(output_base, tolower(variant), "standardization_params.rds")
  if (file.exists(std_file)) {
    params$dg_std <- tryCatch(readRDS(std_file), error = function(e) NULL)
  }

  # SDIMAX
  cal_file <- file.path(output_base, tolower(variant), "species_sdimax_calibrated.csv")
  if (file.exists(cal_file)) {
    sdi <- tryCatch(read_csv(cal_file, show_col_types = FALSE),
                    error = function(e) NULL)
    if (!is.null(sdi)) {
      sdi_col <- if ("sdimax_combined" %in% names(sdi)) "sdimax_combined" else "sdimax_bayes"
      if (sdi_col %in% names(sdi)) {
        params$sdimax <- mean(sdi[[sdi_col]], na.rm = TRUE)
      }
    }
  }

  params
}

# =============================================================================
# SECTION 2: Extract single draw parameters
# =============================================================================

extract_draw_params <- function(posterior_list, draw_index) {
  # Extract the draw_index-th parameter set from posterior_list
  params <- list(
    variant = posterior_list$variant,
    meas_interval = posterior_list$meas_interval,
    sdimax = posterior_list$sdimax,
    dg_std = posterior_list$dg_std
  )

  # H-D parameters from draw
  if (!is.null(posterior_list$hd_samples) && nrow(posterior_list$hd_samples) >= draw_index) {
    hd_row <- posterior_list$hd_samples[draw_index, ]
    params$hd <- list(
      a = hd_row$b_a_Intercept[1],
      b = hd_row$b_b_Intercept[1],
      c = hd_row$b_c_Intercept[1]
    )
  }

  # DG parameters from draw
  if (!is.null(posterior_list$dg_samples) && nrow(posterior_list$dg_samples) >= draw_index) {
    dg_row <- posterior_list$dg_samples[draw_index, ]
    params$dg <- list(
      mu_b0 = dg_row$mu_b0[1],
      betas = sapply(paste0("b", 1:13), function(x) {
        if (x %in% names(dg_row)) dg_row[[x]][1] else 0
      }),
      sigma = dg_row$sigma[1]
    )
    # Species-specific intercepts if available
    sp_cols <- names(dg_row)[grepl("^b0\\[", names(dg_row))]
    if (length(sp_cols) > 0) {
      params$dg$sp_b0 <- sapply(sp_cols, function(x) dg_row[[x]][1])
    }
  }

  # Mortality parameters from draw
  if (!is.null(posterior_list$mort_samples) && nrow(posterior_list$mort_samples) >= draw_index) {
    mort_row <- posterior_list$mort_samples[draw_index, ]
    get_fe <- function(nm) {
      col <- paste0("b_", nm)
      if (col %in% names(mort_row)) mort_row[[col]][1] else 0
    }
    params$mort <- list(
      b0 = get_fe("Intercept"), b1 = get_fe("DBH_std"),
      b2 = get_fe("IDBH_stdE2"), b3 = get_fe("BAL_std"),
      b4 = get_fe("CR_std"), b5 = get_fe("SI_std"), b6 = get_fe("BA_std")
    )
  }

  params
}

# =============================================================================
# SECTION 3: Modified simulate_stand_v2 (from script 17)
# =============================================================================

simulate_stand_v2 <- function(trees, params, si = 65, years = 50,
                               use_calibrated_dg = TRUE,
                               use_calibrated_mort = TRUE,
                               annualize = TRUE,
                               default_annual_mort_rate = 0.015,
                               default_annual_di = 0.10) {

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

    # --- H-D ---
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
      year = yr, tpa = tpa, ba = ba, qmd = qmd, sdi = sdi, top_ht = top_ht
    )

    if (yr == years || nrow(trees) == 0) break

    # Sort for BAL
    trees <- trees %>% arrange(desc(dbh))
    ba_tree <- pi * (trees$dbh / 24)^2
    trees$bal <- cumsum(trees$tpa * ba_tree) - trees$tpa * ba_tree
    if (!("cr" %in% names(trees))) trees$cr <- 0.40

    # Safe scaling
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
      logit_surv_period[!is.finite(logit_surv_period)] <- 2
      p_surv_period <- plogis(logit_surv_period)
      p_surv_period <- pmin(pmax(p_surv_period, 0.01), 0.999)

      if (annualize) {
        p_surv_annual <- p_surv_period^(1 / meas_int)
      } else {
        p_surv_annual <- p_surv_period
      }
    } else {
      base_mort <- default_annual_mort_rate
      size_mod <- ifelse(trees$dbh < 5, 1.5, ifelse(trees$dbh > 20, 1.2, 1.0))
      comp_mod <- 1 + 0.5 * pmin(ba / 200, 1)
      p_surv_annual <- 1 - base_mort * size_mod * comp_mod
      p_surv_annual <- pmin(pmax(p_surv_annual, 0.50), 0.999)
    }

    set.seed(yr * 1000 + 42)
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
        rep(0, length(x))
      }

      ln_dbh <- log(pmax(trees$dbh, 0.1))
      dbh_sq <- trees$dbh^2
      ln_si  <- log(pmax(si, 1))
      slope  <- 15
      elev   <- 3000

      ln_dbh_s <- safe_std(ln_dbh, "ln_DBH")
      dbh_sq_s <- safe_std(dbh_sq, "DBH_sq")
      ln_si_s  <- safe_std(rep(ln_si, nrow(trees)), "ln_SI")
      slope_s  <- safe_std(rep(slope, nrow(trees)), "SLOPE")
      elev_s   <- safe_std(rep(elev, nrow(trees)), "ELEV")
      bal_s    <- safe_std(trees$bal, "BAL")
      ba_s     <- safe_std(rep(ba, nrow(trees)), "BA")
      cr_prop  <- trees$cr

      ln_dbh_s[!is.finite(ln_dbh_s)] <- 0
      dbh_sq_s[!is.finite(dbh_sq_s)] <- 0
      ln_si_s[!is.finite(ln_si_s)]   <- 0
      slope_s[!is.finite(slope_s)]   <- 0
      elev_s[!is.finite(elev_s)]     <- 0
      bal_s[!is.finite(bal_s)]       <- 0
      ba_s[!is.finite(ba_s)]         <- 0
      cr_prop[!is.finite(cr_prop)]   <- 0.40

      sp_int <- ifelse(is.finite(dg$mu_b0), dg$mu_b0, 0)
      b <- dg$betas
      b[!is.finite(b)] <- 0

      pred_ln_dds_period <- sp_int +
        b[1]*ln_dbh_s + b[2]*dbh_sq_s + b[3]*ln_si_s +
        b[4]*slope_s + b[5]*slope_s^2 + b[6]*0 + b[7]*0 +
        b[8]*elev_s + b[9]*elev_s^2 +
        b[10]*cr_prop + b[11]*cr_prop^2 + b[12]*bal_s + b[13]*ba_s

      pred_ln_dds_period[!is.finite(pred_ln_dds_period)] <- log(0.5)
      sigma_sq <- ifelse(is.finite(dg$sigma), dg$sigma^2, 0)
      dds_period <- exp(pred_ln_dds_period + sigma_sq / 2)

      if (annualize) {
        dds_annual <- dds_period / meas_int
      } else {
        dds_annual <- dds_period
      }

      dds_annual <- pmax(dds_annual, 0, na.rm = TRUE)
      dds_annual[!is.finite(dds_annual)] <- 0
      d2 <- sqrt(trees$dbh^2 + dds_annual)
      trees$dbh <- pmax(d2, trees$dbh)

    } else {
      competition_mod <- pmax(0.3, 1 - trees$bal / pmax(ba + 1, 10))
      size_mod <- pmax(0.5, 1 - (trees$dbh / 30)^0.5)
      trees$dbh <- trees$dbh + default_annual_di * competition_mod * size_mod
    }

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
# SECTION 4: Run uncertainty propagation
# =============================================================================

cat("\n=== Section 2: Running uncertainty propagation ===\n")

n_draws <- 200
results_with_ci <- list()

for (v in ALL_VARIANTS) {
  cat("  ", toupper(v), "... ")

  # Load posterior samples
  posterior_list <- load_posterior_draws(v, n_draws = n_draws)

  if (is.null(posterior_list$hd_samples) || is.null(posterior_list$mort_samples)) {
    cat("skip (incomplete posterior samples)\n"); next
  }

  n_available <- min(nrow(posterior_list$hd_samples),
                     nrow(posterior_list$mort_samples),
                     if (!is.null(posterior_list$dg_samples))
                       nrow(posterior_list$dg_samples) else n_draws)

  # Initial stand
  set.seed(42)
  n_init <- 200
  init_trees <- tibble(
    dbh = rlnorm(n_init, log(6), 0.5),
    tpa = 1,
    spcd = sample(c(97, 316), n_init, replace = TRUE)
  ) %>% filter(dbh >= 1, dbh <= 30)

  # Run projections in parallel for each draw
  plan(multisession, workers = min(4, n_available))

  trajectories <- future_lapply(
    seq_len(n_available),
    function(draw_idx) {
      params <- extract_draw_params(posterior_list, draw_idx)
      if (is.null(params$hd) || is.null(params$mort)) return(NULL)

      traj <- simulate_stand_v2(init_trees, params, si = 65, years = 50,
                                 use_calibrated_dg = TRUE,
                                 use_calibrated_mort = TRUE,
                                 annualize = TRUE)
      traj$draw = draw_idx
      traj
    },
    future.seed = TRUE
  )

  plan(sequential)

  # Bind results and compute quantiles
  traj_all <- bind_rows(trajectories) %>% filter(!is.na(ba))

  if (nrow(traj_all) > 0) {
    ci_data <- traj_all %>%
      group_by(year) %>%
      summarise(
        ba_q025 = quantile(ba, 0.025, na.rm = TRUE),
        ba_q25  = quantile(ba, 0.25, na.rm = TRUE),
        ba_q50  = quantile(ba, 0.50, na.rm = TRUE),
        ba_q75  = quantile(ba, 0.75, na.rm = TRUE),
        ba_q975 = quantile(ba, 0.975, na.rm = TRUE),
        tpa_q50 = quantile(tpa, 0.50, na.rm = TRUE),
        qmd_q50 = quantile(qmd, 0.50, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(variant = toupper(v))

    results_with_ci[[v]] <- ci_data
    cat("n_draws =", n_available, "\n")
  } else {
    cat("skip (no valid trajectories)\n")
  }
}

ci_all <- bind_rows(results_with_ci) %>%
  left_join(region_map, by = "variant")

write_csv(ci_all, file.path(comp_base, "stand_projections_with_ci.csv"))
cat("Saved stand_projections_with_ci.csv\n")

# =============================================================================
# SECTION 5: Figures with credible intervals
# =============================================================================

cat("\n=== Section 3: Generating uncertainty figures ===\n")

# --- Figure 1: Calibrated trajectories with 50% and 95% CIs (faceted by region) ---
fig_ci_by_region <- ggplot(ci_all, aes(x = year)) +
  geom_ribbon(aes(ymin = ba_q025, ymax = ba_q975, fill = "95 percent credible interval"),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = ba_q25, ymax = ba_q75, fill = "50 percent credible interval"),
              alpha = 0.4) +
  geom_line(aes(y = ba_q50, color = "Median"), linewidth = 0.7) +
  facet_wrap(~ region, ncol = 3) +
  scale_fill_manual(values = c("95 percent credible interval" = "steelblue",
                                "50 percent credible interval" = "darkblue")) +
  scale_color_manual(values = c("Median" = "darkred")) +
  labs(x = "Projection year",
       y = "Basal area (ft2 per ac)",
       fill = NULL,
       color = NULL,
       title = "Basal Area Trajectories with Posterior Uncertainty",
       subtitle = "Calibrated projections, n=200 posterior samples per variant") +
  theme_pub +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "fig_ci_by_region.png"), fig_ci_by_region,
       width = 14, height = 10, dpi = 300)
cat("Saved fig_ci_by_region.png\n")

# --- Figure 2: Single panel all variants with uncertainty bands ---
fig_ci_all <- ggplot(ci_all, aes(x = year, color = region, fill = region)) +
  geom_ribbon(aes(ymin = ba_q025, ymax = ba_q975),
              alpha = 0.15, color = NA) +
  geom_line(aes(y = ba_q50), linewidth = 0.6) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Projection year",
       y = "Basal area (ft2 per ac)",
       color = "Region",
       fill = "Region",
       title = "Basal Area Trajectories: All Variants with 95% Credible Intervals",
       subtitle = "Calibrated with posterior uncertainty propagation (n=200 draws per variant)") +
  theme_pub

ggsave(file.path(fig_dir, "fig_ci_all_variants.png"), fig_ci_all,
       width = 11, height = 6, dpi = 300)
cat("Saved fig_ci_all_variants.png\n")

# --- Figure 3: Comparison calibrated (with CI) vs default (point estimate) ---

# Get default projections (from script 17 output if available)
default_file <- file.path(comp_base, "stand_projections_3scenarios.csv")
if (file.exists(default_file)) {
  proj_default <- read_csv(default_file, show_col_types = FALSE) %>%
    filter(scenario == "Default FVS-like") %>%
    select(variant, year, ba) %>%
    mutate(scenario = "Default") %>%
    left_join(region_map, by = "variant")

  ci_subset <- ci_all %>%
    select(variant, year, ba_q50, region) %>%
    rename(ba = ba_q50) %>%
    mutate(scenario = "Calibrated")

  comp_data <- bind_rows(
    ci_subset,
    proj_default
  )

  fig_comparison <- ggplot(comp_data, aes(x = year, y = ba, color = scenario)) +
    geom_line(linewidth = 0.7) +
    geom_ribbon(data = ci_all,
                aes(x = year, ymin = ba_q025, ymax = ba_q975,
                    fill = "95 percent CI (calibrated)"),
                color = NA, alpha = 0.2, inherit.aes = FALSE) +
    facet_wrap(~ region, ncol = 3) +
    scale_color_manual(values = c("Calibrated" = "darkblue", "Default" = "darkred")) +
    scale_fill_manual(values = c("95 percent CI (calibrated)" = "steelblue")) +
    labs(x = "Projection year",
         y = "Basal area (ft2 per ac)",
         color = "Scenario",
         fill = NULL,
         title = "Calibrated (with uncertainty) vs Default FVS like Projections",
         subtitle = "Faceted by region; gray band = 95% credible interval for calibrated") +
    theme_pub +
    theme(legend.position = "bottom")

  ggsave(file.path(fig_dir, "fig_calibrated_vs_default_with_ci.png"), fig_comparison,
         width = 14, height = 10, dpi = 300)
  cat("Saved fig_calibrated_vs_default_with_ci.png\n")
}

# --- Figure 4: Trajectory envelope (all 200 draws) for a representative variant ---
sample_variant <- "NE"  # Northeast
sample_trajectories <- list()

sample_posterior <- load_posterior_draws("ne", n_draws = 200)
set.seed(42)
n_init <- 200
init_trees <- tibble(
  dbh = rlnorm(n_init, log(6), 0.5),
  tpa = 1,
  spcd = sample(c(97, 316), n_init, replace = TRUE)
) %>% filter(dbh >= 1, dbh <= 30)

plan(multisession, workers = 4)
all_draws_ne <- future_lapply(
  seq_len(min(200, nrow(sample_posterior$hd_samples))),
  function(draw_idx) {
    params <- extract_draw_params(sample_posterior, draw_idx)
    if (is.null(params$hd) || is.null(params$mort)) return(NULL)
    traj <- simulate_stand_v2(init_trees, params, si = 65, years = 50)
    traj$draw = draw_idx
    traj
  },
  future.seed = TRUE
)
plan(sequential)

all_draws_df <- bind_rows(all_draws_ne) %>% filter(!is.na(ba))

fig_envelope <- ggplot(all_draws_df, aes(x = year, y = ba, group = draw)) +
  geom_line(alpha = 0.05, color = "steelblue") +
  geom_quantile(aes(group = NULL, color = "50th percentile"),
                quantiles = 0.5, formula = y ~ x, method = "loess",
                linewidth = 1, alpha = 1) +
  geom_quantile(aes(group = NULL, color = "2.5th and 97.5th percentiles"),
                quantiles = c(0.025, 0.975), formula = y ~ x, method = "loess",
                linewidth = 0.8, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = c("50th percentile" = "darkred",
                                 "2.5th and 97.5th percentiles" = "darkred")) +
  labs(x = "Projection year",
       y = "Basal area (ft2 per ac)",
       color = NULL,
       title = "Basal Area Trajectory Envelope: Northeast (NE)",
       subtitle = "All 200 posterior draws (light blue), percentile curves (dark red)") +
  theme_pub

ggsave(file.path(fig_dir, "fig_trajectory_envelope_ne.png"), fig_envelope,
       width = 9, height = 6, dpi = 300)
cat("Saved fig_trajectory_envelope_ne.png\n")

# =============================================================================
# SECTION 6: Summary statistics
# =============================================================================

cat("\n=== Section 4: Uncertainty Summary ===\n\n")

uncertainty_summary <- ci_all %>%
  group_by(variant, region) %>%
  filter(year == max(year)) %>%
  summarise(
    ba_median = ba_q50,
    ba_lower_95 = ba_q025,
    ba_upper_95 = ba_q975,
    ci_width = ba_q975 - ba_q025,
    ci_width_pct = 100 * (ba_q975 - ba_q025) / ba_q50,
    .groups = "drop"
  ) %>%
  arrange(ci_width_pct)

write_csv(uncertainty_summary,
          file.path(tbl_dir, "uncertainty_summary_year50.csv"))

cat("Uncertainty Summary (Year 50):\n")
print(uncertainty_summary %>% select(variant, ba_median, ci_width_pct), n = 25)

cat("\nVariants with highest relative uncertainty:\n")
print(uncertainty_summary %>% arrange(desc(ci_width_pct)) %>% head(10))

cat("\n=== Uncertainty propagation complete ===\n")

// Gompit (Complementary Log Log) Mortality Model for CONUS wide FVS
// Based on ORGANON PM_SWO (Hann and Hanus, 2001) with exposure offset
// Extended with species x ecodivision hierarchy and climate covariates
//
// Link function: complementary log log (gompit/cloglog)
//   cloglog(P_mort_annual) = eta
//   P_mort_annual = 1 - exp(-exp(eta))
//
// With exposure offset for variable measurement intervals:
//   P_surv_T = exp(-exp(eta) * T)
//   P_mort_T = 1 - exp(-exp(eta) * T)
//
// Equivalent to Bernoulli with:
//   alive_T ~ Bernoulli(exp(-exp(eta) * T))
//
// Model form (linear predictor):
//   eta = b0[s,d] + b1*DBH + b2*DBH^2 + b3*CR + b4*SITE + b5*BAL + b6*CLIM1 + b7*sqrt(BA*RD)
//
// Hierarchy:
//   b0[s,d] = mu_b0 + sigma_sp * z_sp[s] + sigma_eco * z_eco[d]
//
// Units: DBH in cm, BA in m2/ha, BAL in m2/ha, site productivity in m (ClimateSI or BGI)

data {
  int<lower=1> N;                      // Number of tree period observations
  int<lower=1> N_species;              // Number of species
  int<lower=1> N_ecodiv;               // Number of ecodivisions

  // Response: survived (1) or died (0) over the measurement period
  array[N] int<lower=0, upper=1> alive;

  // Tree level covariates at measurement start (t1), metric units
  vector[N] dbh;                       // DBH at t1 in cm
  vector[N] dbh_sq;                    // DBH^2 at t1 in cm2
  vector[N] cr;                        // Crown ratio at t1 (0 to 1)
  vector[N] site;                      // ClimateSI or BGI in m
  vector[N] ln_bal;                    // log(BAL + 5.0) in m2/ha following ORGANON PM_SWO transform
  vector[N] rd;                        // Relative density (BA/QMD)
  vector[N] sqrt_ba_rd;                // sqrt(BA*RD) interaction covariate
  vector[N] clim1;                     // Climate variable (standardized)

  // Measurement interval (exposure)
  vector<lower=0>[N] log_years;        // log(measurement interval in years)

  // Group indices
  array[N] int<lower=1, upper=N_species> species_id;
  array[N] int<lower=1, upper=N_ecodiv> ecodiv_id;
}

parameters {
  // Hierarchical intercept
  real mu_b0;                          // Grand mean intercept
  real<lower=0.01> sigma_sp;           // SD of species intercepts
  real<lower=0.01> sigma_eco;          // SD of ecodivision intercepts
  vector[N_species] z_sp;             // Standardized species effects
  vector[N_ecodiv] z_eco;             // Standardized ecodivision effects

  // Fixed slope coefficients
  real b1;                             // DBH: linear in cm
  real b2;                             // DBH^2: quadratic in cm2 (U shape)
  real b3;                             // CR: crown ratio
  real b4;                             // SITE: site productivity in m
  real b5;                             // ln(BAL+5): competition (log scale)
  real b6;                             // CLIM1: climate
  real b7;                             // sqrt(BA*RD): relative density interaction (positive = denser = more mortality)
}

transformed parameters {
  vector[N_species] b0_sp = sigma_sp * z_sp;
  vector[N_ecodiv] b0_eco = sigma_eco * z_eco;
}

model {
  // ---- Priors ----

  // Hierarchical intercept
  mu_b0 ~ normal(-3.0, 2.0);          // Low annual base mortality
  sigma_sp ~ exponential(1);
  sigma_eco ~ exponential(2);
  z_sp ~ std_normal();
  z_eco ~ std_normal();

  // Slope priors
  // Note: b1 and b2 scaled for cm units
  // Original ORGANON prior was for inches; rescaled by 1/2.54 for linear, (1/2.54)^2 for quadratic
  b1 ~ normal(-0.06, 0.04);            // Decreasing mort with size initially (cm scale)
  b2 ~ normal(0.0005, 0.0003);         // Positive: U shape for large trees (cm2 scale)
  b3 ~ normal(-3.0, 1.5);              // Strong negative: healthy crown = low mort
  b4 ~ normal(0.01, 0.01);             // Weak positive site effect (m scale)
  b5 ~ normal(0.005, 0.003);           // Positive: more competition = more mort
  b6 ~ normal(0, 0.5);                 // Climate (weakly informative)
  b7 ~ normal(0.1, 0.1);               // Relative density: positive prior (denser = more mortality)

  // ---- Likelihood ----
  // Gompit (cloglog) with exposure offset
  // alive_i ~ Bernoulli(P_surv_T)
  // P_surv_T = exp(-exp(eta + log(T)))
  // P_surv_T = exp(-exp(eta) * T)
  {
    vector[N] eta;
    for (n in 1:N) {
      eta[n] = mu_b0 + b0_sp[species_id[n]] + b0_eco[ecodiv_id[n]]
             + b1 * dbh[n]
             + b2 * dbh_sq[n]
             + b3 * cr[n]
             + b4 * site[n]
             + b5 * ln_bal[n]
             + b6 * clim1[n]
             + b7 * sqrt_ba_rd[n]
             + log_years[n];           // Exposure offset
    }

    // Bernoulli with cloglog link:
    // P(alive=1) = 1 - (1 - exp(-exp(eta)))  = exp(-exp(eta))
    // P(alive=0) = 1 - exp(-exp(eta))
    //
    // Using log probability directly for numerical stability:
    for (n in 1:N) {
      real log_hazard = eta[n];        // Already includes log(T)
      real neg_cumhaz = -exp(log_hazard);

      if (alive[n] == 1) {
        // P(survive) = exp(-exp(eta))
        target += neg_cumhaz;          // log(P_surv) = -exp(eta)
      } else {
        // P(die) = 1 - exp(-exp(eta))
        target += log1m_exp(neg_cumhaz);
      }
    }
  }
}

generated quantities {
  // Posterior predictive and log likelihood (subsample)
  int M = min(N, 2000);
  array[M] int alive_rep;
  vector[M] log_lik;

  for (n in 1:M) {
    real eta_n = mu_b0 + b0_sp[species_id[n]] + b0_eco[ecodiv_id[n]]
               + b1 * dbh[n]
               + b2 * dbh_sq[n]
               + b3 * cr[n]
               + b4 * site[n]
               + b5 * ln_bal[n]
               + b6 * clim1[n]
               + b7 * sqrt_ba_rd[n]
               + log_years[n];
    real neg_cumhaz_n = -exp(eta_n);
    real p_surv = exp(neg_cumhaz_n);

    alive_rep[n] = bernoulli_rng(fmin(fmax(p_surv, 1e-10), 1.0 - 1e-10));

    if (alive[n] == 1) {
      log_lik[n] = neg_cumhaz_n;
    } else {
      log_lik[n] = log1m_exp(neg_cumhaz_n);
    }
  }
}

// Bayesian Individual Tree Mortality Model for FVS Calibration
// Hierarchical logistic regression with species and plot random effects
//
// Model form:
//   logit(P(dead)) = b0 + b1*DBH + b2*DBH^2 + b3*BAL + b4*CR + b5*SI + b6*BA
//                    + species_effect[species] + plot_effect[plot]
//
// The quadratic DBH term captures the U-shaped mortality pattern where
// very small and very large trees have elevated mortality rates.
// BAL (basal area in larger trees) captures one sided competition.
// Crown ratio reflects tree vigor: suppressed trees with small crowns
// have higher mortality.
//
// Annualization: the model predicts annual mortality probability.
// For multi-year intervals, the observation likelihood uses:
//   P(survive interval) = (1 - p_annual)^years

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species
  int<lower=0> N_plot;               // Number of plots

  array[N] int<lower=0, upper=1> died;  // 1 if tree died, 0 if survived
  vector[N] DBH;                     // Diameter at breast height
  vector[N] DBH_sq;                  // DBH squared
  vector[N] BAL;                     // Basal area in larger trees
  vector[N] CR;                      // Crown ratio
  vector[N] SI;                      // Site index
  vector[N] BA;                      // Stand basal area
  vector<lower=0>[N] years;          // Measurement interval (years)

  array[N] int species_id;           // Species index
  array[N] int plot_id;              // Plot index

  // Prior specifications from FVS config
  real prior_b1_mean;                // Prior for DBH coefficient
  real prior_b3_mean;                // Prior for BAL coefficient
  real prior_b4_mean;                // Prior for CR coefficient
}

parameters {
  // Fixed effects
  real b0;                           // Intercept
  real b1;                           // DBH (linear)
  real b2;                           // DBH^2 (quadratic)
  real b3;                           // BAL (competition)
  real b4;                           // CR (vigor)
  real b5;                           // SI (site quality)
  real b6;                           // BA (stand density)

  // Random effects
  vector[N_species] z_species;       // Standardized species effects
  vector[N_plot] z_plot;             // Standardized plot effects
  real<lower=0> sigma_species;       // SD of species random effect
  real<lower=0> sigma_plot;          // SD of plot random effect
}

transformed parameters {
  vector[N_species] effect_species = sigma_species * z_species;
  vector[N_plot] effect_plot = sigma_plot * z_plot;

  // Annual mortality probability on logit scale
  vector[N] logit_p_annual;
  for (n in 1:N) {
    logit_p_annual[n] = b0
                      + b1 * DBH[n]
                      + b2 * DBH_sq[n]
                      + b3 * BAL[n]
                      + b4 * CR[n]
                      + b5 * SI[n]
                      + b6 * BA[n]
                      + effect_species[species_id[n]]
                      + effect_plot[plot_id[n]];
  }
}

model {
  // ========== Priors ==========

  b0 ~ normal(-3, 1.5);              // Base mortality rate (low: ~5% annual)
  b1 ~ normal(prior_b1_mean, 0.5);   // DBH effect
  b2 ~ normal(0, 0.01);              // DBH^2 (small quadratic adjustment)
  b3 ~ normal(prior_b3_mean, 0.3);   // BAL (more competition -> more mortality)
  b4 ~ normal(prior_b4_mean, 0.5);   // CR (higher CR -> lower mortality, expect negative)
  b5 ~ normal(0, 0.3);               // SI effect
  b6 ~ normal(0, 0.3);               // BA effect

  // ========== Random effects ==========
  z_species ~ std_normal();
  z_plot ~ std_normal();
  sigma_species ~ exponential(2);
  sigma_plot ~ exponential(2);

  // ========== Likelihood ==========
  // Account for variable measurement intervals
  // P(die in interval) = 1 - (1 - p_annual)^years
  for (n in 1:N) {
    real p_annual = inv_logit(logit_p_annual[n]);
    real p_interval = 1 - pow(1 - p_annual, years[n]);
    died[n] ~ bernoulli(p_interval);
  }
}

generated quantities {
  // Posterior predictive and log likelihood
  array[N] int died_rep;
  vector[N] log_lik;
  vector[N] p_annual_out;

  for (n in 1:N) {
    real p_annual = inv_logit(logit_p_annual[n]);
    real p_interval = 1 - pow(1 - p_annual, years[n]);
    p_annual_out[n] = p_annual;
    died_rep[n] = bernoulli_rng(p_interval);
    if (died[n] == 1) {
      log_lik[n] = log(p_interval);
    } else {
      log_lik[n] = log1m(p_interval);
    }
  }
}

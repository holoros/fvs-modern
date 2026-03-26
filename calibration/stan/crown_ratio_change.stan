// Bayesian Crown Ratio Change Model for FVS Calibration
// Hierarchical linear model with species and plot random effects
//
// Model form:
//   delta_CR = b0 + b1*DBH + b2*DBH^2 + b3*BA + b4*BAL + b5*CR_init
//            + b6*SI + species_effect[species] + plot_effect[plot] + e
//
// Crown ratio change is modeled as annual change. Key biological
// expectations:
//   - Trees in denser stands (high BA, BAL) tend to lose crown
//   - Trees with initially high CR tend to lose crown (regression to mean)
//   - Trees on better sites (high SI) may maintain or gain crown
//   - Larger trees (high DBH) in competitive stands lose crown faster
//
// FVS uses BCR1-BCR4 parameters for crown ratio change. These
// serve as informative priors.

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species
  int<lower=0> N_plot;               // Number of plots

  vector[N] delta_CR;                // Annual change in crown ratio
  vector[N] DBH;                     // Diameter at breast height
  vector[N] DBH_sq;                  // DBH squared
  vector[N] BA;                      // Stand basal area
  vector[N] BAL;                     // Basal area in larger trees
  vector[N] CR_init;                 // Initial crown ratio
  vector[N] SI;                      // Site index

  array[N] int species_id;           // Species index
  array[N] int plot_id;              // Plot index

  // Prior specifications from FVS BCR parameters
  real prior_bcr1;                   // Prior mean for DBH effect
  real prior_bcr2;                   // Prior mean for BA effect
  real prior_bcr3;                   // Prior mean for BAL effect
  real prior_bcr4;                   // Prior mean for CR_init effect
}

parameters {
  // Fixed effects
  real b0;                           // Intercept
  real b1;                           // DBH (linear)
  real b2;                           // DBH^2 (quadratic)
  real b3;                           // BA (stand density)
  real b4;                           // BAL (competition)
  real b5;                           // CR_init (initial crown ratio)
  real b6;                           // SI (site quality)

  // Random effects
  vector[N_species] z_species;       // Standardized species effects
  vector[N_plot] z_plot;             // Standardized plot effects
  real<lower=0> sigma_species;       // SD of species random effect
  real<lower=0> sigma_plot;          // SD of plot random effect

  real<lower=0> sigma;               // Observation error SD
}

transformed parameters {
  vector[N_species] effect_species = sigma_species * z_species;
  vector[N_plot] effect_plot = sigma_plot * z_plot;

  vector[N] mu;
  for (n in 1:N) {
    mu[n] = b0
          + b1 * DBH[n]
          + b2 * DBH_sq[n]
          + b3 * BA[n]
          + b4 * BAL[n]
          + b5 * CR_init[n]
          + b6 * SI[n]
          + effect_species[species_id[n]]
          + effect_plot[plot_id[n]];
  }
}

model {
  // ========== Priors informed by FVS BCR parameters ==========

  b0 ~ normal(0, 2);                   // Intercept
  b1 ~ normal(prior_bcr1, 0.5);        // DBH effect
  b2 ~ normal(0, 0.1);                 // DBH^2 (small quadratic)
  b3 ~ normal(prior_bcr2, 0.3);        // BA: higher density -> crown loss
  b4 ~ normal(prior_bcr3, 0.3);        // BAL: more overtopping -> crown loss
  b5 ~ normal(prior_bcr4, 0.3);        // CR_init: regression toward mean
  b6 ~ normal(0, 0.3);                 // SI effect

  // ========== Random effects ==========
  z_species ~ std_normal();
  z_plot ~ std_normal();
  sigma_species ~ exponential(2);
  sigma_plot ~ exponential(2);

  // ========== Observation error ==========
  sigma ~ exponential(1);

  // ========== Likelihood ==========
  delta_CR ~ normal(mu, sigma);
}

generated quantities {
  vector[N] delta_CR_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    delta_CR_rep[n] = normal_rng(mu[n], sigma);
    log_lik[n] = normal_lpdf(delta_CR[n] | mu[n], sigma);
  }
}

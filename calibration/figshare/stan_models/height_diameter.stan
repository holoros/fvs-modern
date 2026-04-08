// Bayesian Chapman-Richards Height-Diameter Model for FVS Calibration
// Hierarchical nonlinear model with species level partial pooling
//
// Model form:
//   H = 4.5 + a[species] * (1 - exp(-b[species] * DBH))^c
//
// where:
//   a = asymptotic height above breast height (species specific)
//   b = rate of approach to asymptote (species specific)
//   c = shape/inflection parameter (shared across species)
//
// The 4.5 offset represents breast height (ft); use 1.3 for metric.
// Site index interaction is incorporated through the asymptote: better
// sites produce taller trees at any given diameter.

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species
  int<lower=0> N_plot;               // Number of plots

  vector<lower=0>[N] HT;            // Observed total height (ft or m)
  vector<lower=0>[N] DBH;           // Diameter at breast height (in or cm)
  vector<lower=0>[N] SI;            // Site index

  array[N] int species_id;           // Species index
  array[N] int plot_id;              // Plot index

  // Prior specifications
  real breast_height;                // 4.5 ft or 1.3 m
  real prior_a_mean;                 // Prior mean for asymptotic height (~80 ft or ~25 m)
  real prior_b_mean;                 // Prior mean for rate parameter (~0.03-0.05)
  real prior_c_mean;                 // Prior mean for shape parameter (~1.0-1.5)
}

parameters {
  // Species level asymptote with SI interaction
  vector<lower=0>[N_species] a_base;  // Base asymptotic height by species
  real a_si;                           // SI effect on asymptote

  // Species level rate parameter
  vector<lower=0>[N_species] b;       // Rate of approach to asymptote

  // Shared shape parameter
  real<lower=0.5, upper=5> c;         // Shape/inflection parameter

  // Random effects
  vector[N_plot] z_plot;              // Standardized plot effects
  real<lower=0> sigma_plot;           // SD of plot random effect

  // Hyperparameters for species partial pooling
  real<lower=0> mu_a;                 // Mean asymptotic height
  real<lower=0> tau_a;               // SD of asymptotic heights
  real<lower=0> mu_b;                 // Mean rate parameter
  real<lower=0> tau_b;               // SD of rate parameters

  real<lower=0> sigma;                // Observation error SD
}

transformed parameters {
  vector[N_plot] effect_plot = sigma_plot * z_plot;

  vector[N] mu;
  for (n in 1:N) {
    real a_eff = a_base[species_id[n]] + a_si * SI[n];
    mu[n] = breast_height
          + a_eff * pow(1 - exp(-b[species_id[n]] * DBH[n]), c)
          + effect_plot[plot_id[n]];
  }
}

model {
  // ========== Hyperpriors ==========

  mu_a ~ normal(prior_a_mean, 20);
  tau_a ~ exponential(0.1);
  a_base ~ normal(mu_a, tau_a);

  a_si ~ normal(0.5, 0.3);           // Better sites -> taller asymptote

  mu_b ~ normal(prior_b_mean, 0.02);
  tau_b ~ exponential(10);
  b ~ normal(mu_b, tau_b);

  c ~ normal(prior_c_mean, 0.5);

  // ========== Random effects ==========
  z_plot ~ std_normal();
  sigma_plot ~ exponential(1);

  // ========== Observation error ==========
  sigma ~ exponential(0.1);

  // ========== Likelihood ==========
  HT ~ normal(mu, sigma);
}

generated quantities {
  vector[N] HT_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    HT_rep[n] = normal_rng(mu[n], sigma);
    log_lik[n] = normal_lpdf(HT[n] | mu[n], sigma);
  }
}

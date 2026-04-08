// Bayesian Height Increment Model for FVS Calibration
// Hierarchical model with species group partial pooling
//
// Used for variants with explicit HG parameters (IE, CI, KT, BC, WS, EM)
// Other variants derive height growth from the H-D relationship + DG prediction
//
// Model form (log scale):
//   ln(HTG) = b0[species] + b1*ln(DBH) + b2*ln(HT) + b3*CR
//           + b4*SI + b5*BAL + b6*BA + b7*SLOPE + b8*cos(ASP)
//           + plot_effect[plot] + e
//
// The ln(DBH) and ln(HT) terms capture the biological allometry: taller
// trees at a given diameter have lower height growth potential, while
// diameter reflects the tree's capacity for resource acquisition.

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species
  int<lower=0> N_spgrp;             // Number of species groups (for HG curve shape)
  vector[N] ln_HTG;                  // Log annual height increment (ft/yr or m/yr)
  vector[N] ln_DBH;                  // Log diameter at breast height
  vector[N] ln_HT;                   // Log total height
  vector[N] CR;                      // Crown ratio (proportion or percent)
  vector[N] SI;                      // Site index
  vector[N] BAL;                     // Basal area in larger trees
  vector[N] BA;                      // Stand basal area
  vector[N] SLOPE;                   // Slope (proportion)
  vector[N] CASP;                    // cos(aspect)

  array[N] int species_id;           // Species index for each observation
  array[N] int spgrp_id;            // Species group index for each observation
  // Prior specifications from FVS config
  vector[N_species] prior_hgld;      // Prior for ln(DBH) coefficient by species
  vector[N_spgrp] prior_hghc;       // Prior for height curve shape by species group
  vector[N_spgrp] prior_hgldd;      // Prior for ln(DBH) within species group
  vector[N_spgrp] prior_hgh2;       // Prior for HT^2 coefficient by species group
}

parameters {
  // Species level intercepts
  vector[N_species] b0;              // Intercept (species specific)

  // Species specific ln(DBH) coefficient
  vector[N_species] b1;              // ln(DBH) effect on height growth

  // Shared fixed effects
  real b2;                           // ln(HT) coefficient
  real b3;                           // CR coefficient (expect positive)
  real b4;                           // SI coefficient (expect positive)
  real b5;                           // BAL coefficient (expect negative)
  real b6;                           // BA coefficient
  real b7;                           // SLOPE coefficient
  real b8;                           // cos(ASP) coefficient

  // Species group shape parameters (for height growth curve)
  vector[N_spgrp] gamma_shape;      // Species group shape modifier

  // Hyperparameters
  real mu_b0;                        // Mean of species intercepts
  real<lower=0> tau_b0;             // SD of species intercepts
  real mu_b1;                        // Mean of species ln(DBH) coefficients
  real<lower=0> tau_b1;             // SD of species ln(DBH) coefficients

  real<lower=0> sigma;               // Observation level error SD
}

transformed parameters {
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = b0[species_id[n]]
          + b1[species_id[n]] * ln_DBH[n]
          + b2 * ln_HT[n]
          + b3 * CR[n]
          + b4 * SI[n]
          + b5 * BAL[n]
          + b6 * BA[n]
          + b7 * SLOPE[n]
          + b8 * CASP[n]
          + gamma_shape[spgrp_id[n]] * ln_HT[n];  // Species group height curve shape
  }
}

model {
  // ========== Hyperpriors ==========

  // Species intercepts: partial pooling
  mu_b0 ~ normal(0, 2);
  tau_b0 ~ exponential(1);
  b0 ~ normal(mu_b0, tau_b0);

  // Species ln(DBH) coefficient: partial pooling with FVS prior
  mu_b1 ~ normal(0, 1);
  tau_b1 ~ exponential(2);
  b1 ~ normal(prior_hgld, tau_b1);

  // Fixed effects priors
  b2 ~ normal(-0.5, 0.5);       // ln(HT): taller trees grow less (negative)
  b3 ~ normal(0.5, 0.5);        // CR: larger crowns grow more (positive)
  b4 ~ normal(0.3, 0.3);        // SI: better sites grow more (positive)
  b5 ~ normal(-0.3, 0.3);       // BAL: more competition reduces growth (negative)
  b6 ~ normal(0, 0.3);          // BA: stand density effect
  b7 ~ normal(0, 0.3);          // SLOPE
  b8 ~ normal(0, 0.3);          // cos(ASP)

  // Species group shape parameters with FVS priors
  gamma_shape ~ normal(prior_hghc, 0.5);

  // ========== Observation error ==========
  sigma ~ exponential(1);

  // ========== Likelihood ==========
  ln_HTG ~ normal(mu, sigma);
}

generated quantities {
  // Posterior predictive distribution
  vector[N] ln_HTG_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    ln_HTG_rep[n] = normal_rng(mu[n], sigma);
    log_lik[n] = normal_lpdf(ln_HTG[n] | mu[n], sigma);
  }
}

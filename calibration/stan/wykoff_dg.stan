// Bayesian Wykoff Diameter Growth Model for FVS Calibration
// Hierarchical model with species-level partial pooling
// Random effects for plot and location (nested)
//
// Model form (log scale):
//   ln(DDS) = b0[species] + b1*ln(DBH) + b2*DBH^2 + b3*ln(SI) +
//             b4*SLOPE + b5*SLOPE^2 + b6*SLOPE*sin(ASP) + b7*SLOPE*cos(ASP) +
//             b8*ELEV + b9*ELEV^2 + b10*CR + b11*CR^2 + b12*BAL + b13*BA +
//             plot_effect[plot] + location_effect[location] + e

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species
  int<lower=0> N_plot;               // Number of plots
  int<lower=0> N_location;           // Number of locations

  vector[N] ln_DDS;                  // Log response: diameter increment squared
  vector[N] ln_DBH;                  // Log diameter at breast height
  vector[N] DBH_sq;                  // Diameter squared
  vector[N] ln_SI;                   // Log site index
  vector[N] SLOPE;                   // Slope (percent)
  vector[N] SLOPE_sq;                // Slope squared
  vector[N] SLOPE_SASP;              // Slope * sin(aspect)
  vector[N] SLOPE_CASP;              // Slope * cos(aspect)
  vector[N] ELEV;                    // Elevation (km)
  vector[N] ELEV_sq;                 // Elevation squared
  vector[N] CR;                      // Crown ratio
  vector[N] CR_sq;                   // Crown ratio squared
  vector[N] BAL;                     // Basal area larger (sq ft)
  vector[N] BA;                      // Stand basal area (sq ft/acre)

  array[N] int species_id;           // Species index for each observation
  array[N] int plot_id;              // Plot index for each observation
  array[N] int location_id;          // Location index for each observation

  // Prior specifications from FVS config
  vector[N_species] prior_b0;        // Prior mean for intercept by species
  vector[N_species] prior_b1;        // Prior mean for ln(DBH) coefficient
  vector[N_species] prior_b2;        // Prior mean for DBH^2 coefficient
  vector[N_species] prior_b3;        // Prior mean for ln(SI) coefficient

  // Hyperprior parameters for other coefficients
  real prior_b_mu;                   // Mean of coefficient hyperpriors
  real prior_b_sigma;                // SD of coefficient hyperpriors
}

parameters {
  // Fixed effects
  vector[N_species] b0;              // Intercept (species-specific)
  vector[N_species] b1;              // Coefficient: ln(DBH)
  vector[N_species] b2;              // Coefficient: DBH^2
  vector[N_species] b3;              // Coefficient: ln(SI)
  real b4;                           // Coefficient: SLOPE
  real b5;                           // Coefficient: SLOPE^2
  real b6;                           // Coefficient: SLOPE*sin(ASP)
  real b7;                           // Coefficient: SLOPE*cos(ASP)
  real b8;                           // Coefficient: ELEV
  real b9;                           // Coefficient: ELEV^2
  real b10;                          // Coefficient: CR
  real b11;                          // Coefficient: CR^2
  real b12;                          // Coefficient: BAL
  real b13;                          // Coefficient: BA

  // Random effects
  vector[N_plot] z_plot;             // Standardized plot effects
  vector[N_location] z_location;     // Standardized location effects
  real<lower=0> sigma_plot;          // SD of plot random effect
  real<lower=0> sigma_location;      // SD of location random effect

  real<lower=0> sigma;               // Observation-level error SD
}

transformed parameters {
  vector[N_plot] effect_plot = sigma_plot * z_plot;
  vector[N_location] effect_location = sigma_location * z_location;

  vector[N] mu;
  for (n in 1:N) {
    mu[n] = b0[species_id[n]]
          + b1[species_id[n]] * ln_DBH[n]
          + b2[species_id[n]] * DBH_sq[n]
          + b3[species_id[n]] * ln_SI[n]
          + b4 * SLOPE[n]
          + b5 * SLOPE_sq[n]
          + b6 * SLOPE_SASP[n]
          + b7 * SLOPE_CASP[n]
          + b8 * ELEV[n]
          + b9 * ELEV_sq[n]
          + b10 * CR[n]
          + b11 * CR_sq[n]
          + b12 * BAL[n]
          + b13 * BA[n]
          + effect_plot[plot_id[n]]
          + effect_location[location_id[n]];
  }
}

model {
  // ========== Hyperpriors for fixed effects ==========

  // Species-specific intercepts with FVS prior
  b0 ~ normal(prior_b0, 1.5);

  // Species-specific ln(DBH) coefficients
  b1 ~ normal(prior_b1, 0.5);

  // Species-specific DBH^2 coefficients
  b2 ~ normal(prior_b2, 0.1);

  // Species-specific ln(SI) coefficients
  b3 ~ normal(prior_b3, 0.5);

  // Shared slope/aspect/elevation coefficients with weakly informative priors
  b4 ~ normal(0, 0.5);      // SLOPE
  b5 ~ normal(0, 0.5);      // SLOPE^2
  b6 ~ normal(0, 0.5);      // SLOPE*sin(ASP)
  b7 ~ normal(0, 0.5);      // SLOPE*cos(ASP)
  b8 ~ normal(0, 0.5);      // ELEV
  b9 ~ normal(0, 0.5);      // ELEV^2

  // Crown and competition coefficients
  b10 ~ normal(0, 1.0);     // CR (expect positive)
  b11 ~ normal(0, 0.5);     // CR^2 (expect negative)
  b12 ~ normal(-0.5, 0.5);  // BAL (expect negative: larger trees reduce growth)
  b13 ~ normal(0.5, 0.5);   // BA (expect positive: higher stand density increases growth)

  // ========== Hyperpriors for random effects ==========

  // Plot-level effects
  z_plot ~ std_normal();
  sigma_plot ~ exponential(2);

  // Location-level effects
  z_location ~ std_normal();
  sigma_location ~ exponential(2);

  // Observation-level error
  sigma ~ exponential(1);

  // ========== Likelihood ==========
  ln_DDS ~ normal(mu, sigma);
}

generated quantities {
  // Posterior predictive distribution
  vector[N] ln_DDS_rep;
  vector[N] log_lik;  // Pointwise log-likelihood for LOO-IC

  for (n in 1:N) {
    ln_DDS_rep[n] = normal_rng(mu[n], sigma);
    log_lik[n] = normal_lpdf(ln_DDS[n] | mu[n], sigma);
  }
}

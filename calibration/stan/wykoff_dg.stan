// Bayesian Wykoff Diameter Growth Model for FVS Calibration
// Species intercepts + shared slope coefficients
// Simplified for tractable HMC sampling
//
// Model form (log scale):
//   ln(DDS) = b0[species] + b1*ln(DBH) + b2*DBH^2 + b3*ln(SI) +
//             b4*SLOPE + b5*SLOPE^2 + b6*SLOPE*sin(ASP) + b7*SLOPE*cos(ASP) +
//             b8*ELEV + b9*ELEV^2 + b10*CR + b11*CR^2 + b12*BAL + b13*BA + e
//
// All continuous predictors are standardized (mean 0, sd 1) except CR (0 to 1)

data {
  int<lower=0> N;                    // Number of observations
  int<lower=0> N_species;            // Number of species

  vector[N] ln_DDS;                  // Log response: diameter increment squared
  vector[N] ln_DBH;                  // Log DBH (standardized)
  vector[N] DBH_sq;                  // DBH^2 (standardized)
  vector[N] ln_SI;                   // Log site index (standardized)
  vector[N] SLOPE;                   // Slope (standardized)
  vector[N] SLOPE_sq;                // Slope^2 (from standardized SLOPE)
  vector[N] SLOPE_SASP;              // Slope * sin(aspect) (standardized)
  vector[N] SLOPE_CASP;              // Slope * cos(aspect) (standardized)
  vector[N] ELEV;                    // Elevation (standardized)
  vector[N] ELEV_sq;                 // Elevation^2 (from standardized ELEV)
  vector[N] CR;                      // Crown ratio (0 to 1)
  vector[N] CR_sq;                   // Crown ratio squared
  vector[N] BAL;                     // Basal area larger (standardized)
  vector[N] BA;                      // Stand basal area (standardized)

  array[N] int species_id;           // Species index for each observation

  // Prior specifications from FVS config
  vector[N_species] prior_b0;        // Prior mean for intercept by species
}

parameters {
  // Species intercepts
  real mu_b0;                        // Grand mean intercept
  real<lower=0> sigma_b0;            // SD of species intercepts
  vector[N_species] z_b0;            // Standardized species intercepts

  // Shared slope coefficients
  real b1;                           // ln(DBH)
  real b2;                           // DBH^2
  real b3;                           // ln(SI)
  real b4;                           // SLOPE
  real b5;                           // SLOPE^2
  real b6;                           // SLOPE*sin(ASP)
  real b7;                           // SLOPE*cos(ASP)
  real b8;                           // ELEV
  real b9;                           // ELEV^2
  real b10;                          // CR
  real b11;                          // CR^2
  real b12;                          // BAL
  real b13;                          // BA

  real<lower=0> sigma;               // Observation error SD
}

transformed parameters {
  // Non centered species intercepts
  vector[N_species] b0 = mu_b0 + sigma_b0 * z_b0;
}

model {
  // Hyperpriors for species intercepts
  mu_b0 ~ normal(0, 2);
  sigma_b0 ~ exponential(1);
  z_b0 ~ std_normal();

  // Shared coefficient priors (all predictors standardized)
  b1 ~ normal(0, 1);      // ln(DBH)
  b2 ~ normal(0, 1);      // DBH^2
  b3 ~ normal(0, 1);      // ln(SI)
  b4 ~ normal(0, 1);      // SLOPE
  b5 ~ normal(0, 1);      // SLOPE^2
  b6 ~ normal(0, 1);      // SLOPE*sin(ASP)
  b7 ~ normal(0, 1);      // SLOPE*cos(ASP)
  b8 ~ normal(0, 1);      // ELEV
  b9 ~ normal(0, 1);      // ELEV^2
  b10 ~ normal(0, 1);     // CR
  b11 ~ normal(0, 1);     // CR^2
  b12 ~ normal(0, 1);     // BAL
  b13 ~ normal(0, 1);     // BA

  // Observation error
  sigma ~ exponential(1);

  // Vectorized likelihood
  {
    vector[N] mu = b0[species_id]
                 + b1 * ln_DBH
                 + b2 * DBH_sq
                 + b3 * ln_SI
                 + b4 * SLOPE
                 + b5 * SLOPE_sq
                 + b6 * SLOPE_SASP
                 + b7 * SLOPE_CASP
                 + b8 * ELEV
                 + b9 * ELEV_sq
                 + b10 * CR
                 + b11 * CR_sq
                 + b12 * BAL
                 + b13 * BA;
    ln_DDS ~ normal(mu, sigma);
  }
}

generated quantities {
  // Posterior predictive samples (subset for memory)
  vector[min(N, 1000)] ln_DDS_rep;
  vector[min(N, 1000)] log_lik;

  {
    int M = min(N, 1000);
    for (n in 1:M) {
      real mu_n = b0[species_id[n]]
                + b1 * ln_DBH[n]
                + b2 * DBH_sq[n]
                + b3 * ln_SI[n]
                + b4 * SLOPE[n]
                + b5 * SLOPE_sq[n]
                + b6 * SLOPE_SASP[n]
                + b7 * SLOPE_CASP[n]
                + b8 * ELEV[n]
                + b9 * ELEV_sq[n]
                + b10 * CR[n]
                + b11 * CR_sq[n]
                + b12 * BAL[n]
                + b13 * BA[n];
      ln_DDS_rep[n] = normal_rng(mu_n, sigma);
      log_lik[n] = normal_lpdf(ln_DDS[n] | mu_n, sigma);
    }
  }
}

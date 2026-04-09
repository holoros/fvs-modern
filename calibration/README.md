# FVS Bayesian Calibration Pipeline

A complete Bayesian calibration system for FVS (Forest Vegetation Simulator) variant parameters using FIA (Forest Inventory and Analysis) data. This pipeline refits FVS model coefficients using modern Bayesian methods (MCMC via CmdStan) while maintaining fidelity to the original FVS model structure.

## Overview

This calibration pipeline:

1. **Fetches FIA data** for all relevant states via the rFIA package
2. **Extracts remeasurement pairs** of trees with known fates between measurements
3. **Fits Bayesian growth, mortality, and crown models** to FIA data
4. **Uses informative priors** based on the original FVS parameters
5. **Generates posterior distributions** for all model coefficients
6. **Converts posteriors to JSON** matching the FVS config schema
7. **Produces comprehensive diagnostics** for model validation

## Bayesian Approach

### Key Modeling Decisions

- **Wymoff Diameter Growth Model**: Log-linear model on squared inside-bark diameter increment (DDS)
  - Formula: `ln(DDS) = b0 + b1*ln(DBH) + b2*DBH^2 + b3*ln(SI) + b4*SLOPE + ... + error_plot + error_location + e`
  - 13 fixed effects + hierarchical random effects

- **Hierarchical Structure**: Species-level partial pooling with plot and location random effects
  - Reduces overfitting while allowing variant and species heterogeneity
  - Shares information across species and plots

- **Informative Priors**: Derived from extracted FVS parameters
  - Growth coefficients: FVS parameter values as prior means
  - Weakly informative for competition and site coefficients
  - Exponential priors (positive support) for variance parameters

- **MCMC Sampling**: CmdStan with dynamic Hamiltonian Monte Carlo
  - 4 chains × 2000 iterations (1000 warmup)
  - Adapt delta = 0.95 for improved geometry exploration
  - Automatic divergence detection and reporting

### Model Components

1. **Diameter Growth** (primary focus)
   - Response: ln(DDS) where DDS = DIB_t2^2 - DIB_t1^2
   - Predictors: DBH, SI, SLOPE, ASPECT, ELEVATION, CROWN RATIO, COMPETITION
   - Accounts for measurement interval (5 or 10 year periods)

2. **Height-Diameter** (Chapman-Richards equation)
   - Response: Height at breast height
   - Model: H = 4.5 + a*(1 - exp(-b*DBH))^c
   - Species-level varying parameters

3. **Mortality** (Logistic regression)
   - Response: Binary survival indicator
   - Predictors: DBH, competition, crown ratio, site quality
   - Plot random effects

4. **Crown Ratio** (Change in live crown)
   - Response: Annual change in crown ratio
   - Used to validate consistency with growth estimates

## Project Structure

```
calibration/
├── R/                          # R scripts for calibration pipeline
│   ├── 00_setup.R              # Environment setup and package installation
│   ├── 01_fetch_fia_data.R     # FIA data download and preprocessing
│   ├── 02_fit_diameter_growth.R # Main Wykoff DG model fitting
│   ├── 03_fit_height_diameter.R # Chapman-Richards HT/DBH fitting
│   ├── 04_fit_mortality.R      # Logistic mortality model
│   ├── 05_fit_crown_ratio.R    # Crown ratio change model
│   ├── 06_posterior_to_json.R  # Convert posteriors to FVS config JSON
│   └── 07_diagnostics.R        # Convergence diagnostics and plots
├── stan/
│   └── wykoff_dg.stan          # Stan model for diameter growth
├── slurm/
│   ├── submit_calibration.sh   # Master array job submission script
│   └── run_variant.sh          # Per-variant job script
├── data/
│   ├── raw_fia/                # Downloaded FIA data cache
│   └── processed/              # Processed data by variant
├── output/
│   └── variants/               # Output by variant
│       ├── {variant}/
│       │   ├── diameter_growth_samples.rds
│       │   ├── diameter_growth_posterior.csv
│       │   ├── *_summary.csv
│       │   └── diagnostics/    # Diagnostic plots and reports
├── logs/                        # Execution logs
└── README.md                    # This file
```

## Installation and Setup

### Prerequisites

- R 4.1+
- Access to OSC (Ohio Supercomputer Center) or compatible HPC
- Sufficient disk space for FIA cache (~50GB for full US dataset)

### Initial Setup

Run the setup script to install all required packages and configure the environment:

```bash
cd /path/to/Documents/Claude/fvs-modern
Rscript calibration/R/00_setup.R
```

This will:
- Install all required R packages
- Create necessary directory structure
- Configure CmdStan (compile if needed)
- Set up logging

### Required Packages

Core packages:
- `rFIA`: FIA data access
- `brms`: Bayesian regression interface
- `cmdstanr`: CmdStan R interface
- `tidyverse`: Data manipulation and visualization
- `posterior`, `bayesplot`: Posterior analysis
- `jsonlite`: JSON configuration handling
- `parallel`, `foreach`, `doParallel`: Parallelization
- `logger`: Logging framework

## Running the Calibration

### Option 1: Full Pipeline on OSC (Recommended)

Submit as a SLURM array job (one job per FVS variant):

```bash
cd /path/to/Documents/Claude/fvs-modern
sbatch calibration/slurm/submit_calibration.sh
```

This submits 25 array jobs (one per variant) running in parallel.

**Resource Allocation:**
- Nodes: 25 parallel jobs
- CPUs per job: 4 (for parallel MCMC chains)
- Memory per job: 16 GB
- Wall time: 24 hours

Check job status:
```bash
squeue -u $USER -n fvs-calibration
sacct -j <job_id> --format=JobID,JobName,State,Elapsed,MaxRSS
```

### Option 2: Single Variant Locally

Calibrate one variant interactively:

```bash
bash calibration/slurm/run_variant.sh ca
```

This runs the complete pipeline for the California variant.

### Option 3: Manual Step-by-Step

For development or troubleshooting:

```bash
# Setup
Rscript calibration/R/00_setup.R

# Fetch data (run once)
Rscript calibration/R/01_fetch_fia_data.R

# Fit models for specific variant
Rscript calibration/R/02_fit_diameter_growth.R --variant ca
Rscript calibration/R/03_fit_height_diameter.R --variant ca
Rscript calibration/R/04_fit_mortality.R --variant ca
Rscript calibration/R/05_fit_crown_ratio.R --variant ca

# Convert and diagnose
Rscript calibration/R/06_posterior_to_json.R --variant ca
Rscript calibration/R/07_diagnostics.R --variant ca
```

## Output Files

For each variant, the pipeline generates:

### Model Outputs
- `diameter_growth_samples.rds`: Raw posterior samples (8000 draws × ~50 parameters)
- `diameter_growth_posterior.csv`: Posterior medians, credible intervals, Rhat, ESS
- `{model}_summary.csv`: Detailed summary statistics for all models
- `{model}_ppc.pdf`: Posterior predictive checks

### Configuration
- `config/calibrated/{variant}.json`: Updated FVS parameters (ready to use in simulations)
- `config/calibrated/{variant}_OLD.json`: Backup of original parameters

### Diagnostics
- `diagnostics/convergence_summary.csv`: Rhat and ESS statistics
- `diagnostics/poor_convergence.csv`: Parameters needing attention
- `diagnostics/parameter_credible_intervals.pdf`: Posterior estimates visualization
- `diagnostics/parameter_correlations.pdf`: Correlation heatmap
- `diagnostics/DIAGNOSTICS_REPORT.md`: Comprehensive diagnostic report

### Logs
- `logs/variant_{variant}.log`: Execution log for each variant
- `logs/01_fetch_fia_data.log`: FIA data processing log
- `logs/{step}_{variant}.log`: Individual step logs

## Parameter Mapping: Stan to FVS

The Stan model uses this parameter mapping for diameter growth:

| Stan Parameter | FVS Parameter | Interpretation |
|---|---|---|
| b0[species] | (intercept by species) | Base growth rate |
| b1[species] | DGLD | ln(DBH) coefficient (positive) |
| b2[species] | DGDS | DBH^2 coefficient (negative) |
| b3[species] | DGSITE | ln(SI) coefficient (positive) |
| b4 | DGSLOP | SLOPE coefficient |
| b5 | DGSLSQ | SLOPE^2 coefficient |
| b6 | DGSASP | SLOPE × sin(ASPECT) |
| b7 | DGCASP | SLOPE × cos(ASPECT) |
| b8 | DGEL | ELEVATION coefficient |
| b9 | DGELSQ | ELEVATION^2 coefficient |
| b10 | DGCR | CROWN RATIO coefficient |
| b11 | DGCRSQ | CROWN RATIO^2 coefficient |
| b12 | DGBAL | BAL (basal area larger) coefficient |
| b13 | DGPCCF | BA or CCF coefficient |

## Interpretation of Results

### Convergence Diagnostics

**Rhat (Potential Scale Reduction Factor)**
- Values < 1.01: Good convergence
- Values 1.01-1.05: Acceptable (may need more iterations)
- Values > 1.05: Poor convergence (refit with more iterations)

**ESS (Effective Sample Size)**
- Bulk ESS > 400: Adequate for inference
- Rank ESS > 400: Adequate for tail quantiles
- Low ESS: Chain mixing issues; may indicate prior specification problems

### Posterior Credible Intervals

95% credible intervals (p2.5 to p97.5) provide the range of plausible parameter values given the data and priors.

Narrower intervals indicate more certainty. Very wide intervals may indicate:
- Weak data signal for that parameter
- Parameter near boundary (e.g., negative growth)
- Need for stronger priors

### Prior-Posterior Comparison

Comparing posteriors to the FVS prior values (original parameters):
- Large shifts suggest FVS parameter needed adjustment
- Small shifts suggest original parameters were well-calibrated to FVS model form
- Direction of shift indicates systematic bias (e.g., FVS overestimating growth)

## Model Validation

### Posterior Predictive Checks

PPC plots compare observed data to predictions from the posterior distribution:
- Good overlap: Model captures data distribution well
- Data outside posterior predictions: Possible model misspecification
- Systematic patterns: May indicate missing predictors or interactions

### Cross-validation

Leave-one-out cross-validation (LOO) scores can compare model fits:
```r
loo(fit)  # Requires log_lik from generated quantities
```

Lower LOO (more negative) indicates better predictive performance.

## Common Issues and Troubleshooting

### High Rhat values (> 1.05)

**Causes:**
- Chains not mixed
- Model misspecification
- Weak priors allowing extreme values

**Solutions:**
1. Increase iterations: `iter_warmup = 2000, iter_sampling = 4000`
2. Tighten priors on problematic parameters
3. Standardize predictors more carefully
4. Check for data errors (outliers, measurement errors)

### Low ESS relative to samples

**Causes:**
- High autocorrelation in chains
- Step size too small
- Complex posterior geometry

**Solutions:**
1. Decrease `adapt_delta` (use 0.80-0.90) for faster stepping
2. Reduce prior variance to improve identifiability
3. Check for collinearity among predictors

### Memory errors

**Solutions:**
- Reduce `iter_sampling` for shorter runs (test first)
- Use fewer Stan chains
- Process data in smaller batches (by region or species)

### Missing FIA data for a state

The rFIA package may have connectivity issues. Solutions:
1. Check internet connection
2. Increase timeout in `readFIA()` call
3. Manually download FIA data and place in `data/raw_fia/`

## Integration with fvs-modern

### Using Calibrated Parameters

Once calibration completes, use the calibrated configs in fvs-modern:

```r
# Load calibrated parameters
calibrated_config <- jsonlite::fromJSON("config/calibrated/ca.json")

# Use in FVS simulations...
```

### Parameter Replacement

The calibrated JSON files have the same structure as the original configs, so they are drop-in replacements:

```bash
# Backup original
cp config/ca.json config/ca_original.json

# Use calibrated version
cp config/calibrated/ca.json config/ca.json
```

### Comparing Predictions

Use diagnostic plots to compare FVS-original vs. calibrated predictions:

```r
source("calibration/R/07_diagnostics.R")
# Generates prior-posterior comparison plots
```

## Performance Notes

### Typical Runtimes (OSC, 4 cores per job)

- FIA data fetch (first time): 30-60 minutes
- Diameter growth fitting: 20-40 minutes per variant
- Height-diameter fitting: 10-20 minutes per variant
- Mortality & Crown models: 15-30 minutes per variant
- Total per variant: ~2-3 hours

With 25 parallel jobs, full calibration: 2-3 hours wall time

### Memory Usage

- Per-job peak: ~12-14 GB (mostly Stan samples)
- Total for all 25 jobs: ~350 GB (keep compressed outputs)

### Data Storage

- Raw FIA cache: ~50 GB (first time download)
- Processed data: ~500 MB per variant
- Posterior samples: ~200 MB per variant (uncompressed)
- Logs and diagnostics: ~100 MB per variant

## Citation and References

This pipeline implements Bayesian calibration methods for FVS as described in:

- Weiskittel, A. R., et al. (2011). Forest Growth and Yield Modeling. Wiley-Blackwell.
- Standish, B., & Savage, M. (2021). Bayesian calibration of climate-sensitive forest models.
- Wykoff, W. R. (1986). Supplement to: Optimizing lodgepole pine stand density...

Stan implementation follows:
- Gelman, A., et al. (2013). Bayesian Data Analysis (3rd ed.). Chapman and Hall/CRC.
- Betancourt, M. (2017). A Conceptual Introduction to Hamiltonian Monte Carlo.

## Future Extensions

Potential improvements and extensions:

1. **Climate Sensitivity**: Add climate variables and interaction effects
2. **Spatial Correlations**: Implement spatial random effects
3. **Genetic Algorithm**: Optimized prior specification from genetics data
4. **Multi-species Interactions**: Density-dependent competition terms
5. **Temporal Trends**: Incorporate long-term FIA remeasurement series
6. **Validation**: Compare predictions to independent test plots
7. **Uncertainty Quantification**: Propagate posterior uncertainty through simulations

## Contributing

Issues, improvements, and extensions welcome. Contact Aaron Weiskittel (aweiskittel@maine.edu).

## License

Code is part of fvs-modern project. See main project README for license details.

---

**Last Updated**: 2026-03-26
**Tested On**: R 4.3.0, CmdStan 2.33+, OSC with SLURM scheduler

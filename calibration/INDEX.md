# FVS Bayesian Calibration Pipeline - Complete Index

## Project Summary

**Location**: `/path/to/Documents/Claude/fvs-modern/calibration/`

**Purpose**: Bayesian calibration of FVS variant parameters using FIA data with MCMC sampling on OSC HPC

**Total Code**: 2,690 lines across 13 core files

**Key Features**:
- Hierarchical Bayesian modeling with species-level pooling
- Wymoff diameter growth model in Stan
- Informative priors from original FVS parameters
- SLURM array job parallelization (25 variants)
- Comprehensive diagnostics and validation
- JSON output compatible with fvs-modern

---

## File Structure

### Documentation

| File | Lines | Purpose |
|---|---|---|
| `README.md` | 550+ | Complete pipeline documentation, model details, interpretation guide |
| `QUICKSTART.md` | 150+ | Quick reference for running calibration |
| `INDEX.md` | This file | File structure and content index |

### R Scripts (Core Pipeline)

| File | Lines | Purpose |
|---|---|---|
| `R/00_setup.R` | 160 | Environment setup, package installation, CmdStan configuration |
| `R/01_fetch_fia_data.R` | 380 | FIA data download, preprocessing, remeasurement pair extraction |
| `R/02_fit_diameter_growth.R` | 320 | Main Bayesian Wykoff diameter growth model using Stan |
| `R/03_fit_height_diameter.R` | 200 | Chapman-Richards height-diameter model using brms |
| `R/04_fit_mortality.R` | 210 | Logistic mortality model with plot random effects |
| `R/05_fit_crown_ratio.R` | 200 | Crown ratio change model |
| `R/06_posterior_to_json.R` | 280 | Convert MCMC posteriors to FVS JSON config format |
| `R/07_diagnostics.R` | 380 | Convergence diagnostics, posterior predictive checks, comparison plots |

**Total R Code**: 2,120 lines

### Stan Models

| File | Lines | Purpose |
|---|---|---|
| `stan/wykoff_dg.stan` | 160 | Bayesian Wykoff diameter growth model with hierarchical structure |

### SLURM Job Scripts

| File | Lines | Purpose |
|---|---|---|
| `slurm/submit_calibration.sh` | 70 | Master SLURM array job submission (25 variants parallel) |
| `slurm/run_variant.sh` | 200 | Per-variant pipeline orchestration with error handling |

**Total SLURM Code**: 270 lines

---

## Execution Flow

### Option 1: Full OSC Submission (Recommended)

```
sbatch slurm/submit_calibration.sh
│
├─ slurm/submit_calibration.sh (master)
│  └─ Creates array job: 1-25 (one per variant)
│     │
│     └─ For each array task (parallel):
│        slurm/run_variant.sh {variant}
│        │
│        ├─ R/01_fetch_fia_data.R (once per session)
│        ├─ R/02_fit_diameter_growth.R
│        ├─ R/03_fit_height_diameter.R
│        ├─ R/04_fit_mortality.R
│        ├─ R/05_fit_crown_ratio.R
│        ├─ R/06_posterior_to_json.R
│        └─ R/07_diagnostics.R
```

**Wall time**: ~3 hours for all 25 variants in parallel
**Resource**: 25 jobs × 4 cores × 16 GB

### Option 2: Single Variant

```
bash slurm/run_variant.sh ca
```

**Wall time**: ~2-3 hours
**Resource**: 1 job × 4 cores × 16 GB

### Option 3: Manual Step-by-Step

```
Rscript R/00_setup.R                      # Once
Rscript R/01_fetch_fia_data.R             # Once for all variants
for VARIANT in ca ut co ...; do
  Rscript R/02_fit_diameter_growth.R --variant $VARIANT
  Rscript R/03_fit_height_diameter.R --variant $VARIANT
  Rscript R/04_fit_mortality.R --variant $VARIANT
  Rscript R/05_fit_crown_ratio.R --variant $VARIANT
  Rscript R/06_posterior_to_json.R --variant $VARIANT
  Rscript R/07_diagnostics.R --variant $VARIANT
done
```

---

## Data Flow

```
FIA Raw Data (rFIA download)
│
├─ data/raw_fia/                    [rFIA cache, ~50 GB]
│
├─ R/01_fetch_fia_data.R
│
├─ data/processed/{variant}/diameter_growth.csv
│  └─ Columns: PLT_CN, tree_id, SPCD, DIA_t1, DIA_t2, DDS,
│              ln_DBH, SI, SLOPE, ELEV, CR, BAL, BA, ...
│
├─ R/02_fit_diameter_growth.R
│  ├─ stan/wykoff_dg.stan
│  └─ Fits hierarchical Bayesian model
│
├─ output/variants/{variant}/
│  ├─ diameter_growth_samples.rds     [8000 draws × parameters]
│  ├─ diameter_growth_posterior.csv   [Medians, CIs, Rhat, ESS]
│  ├─ diameter_growth_traceplots.pdf
│  ├─ diameter_growth_ppc.pdf
│  └─ (Similar files for other models)
│
├─ R/06_posterior_to_json.R
│
└─ config/calibrated/{variant}.json  [FVS-compatible config]
   └─ Ready to use in fvs-modern simulations
```

---

## Model Details

### Diameter Growth (Main Model)

**Stan File**: `stan/wykoff_dg.stan` (160 lines)

**Model Form**:
```
ln(DDS) = b0[species] + b1[species]*ln(DBH) + b2[species]*DBH^2 +
          b3[species]*ln(SI) + b4*SLOPE + b5*SLOPE^2 +
          b6*SLOPE*sin(ASP) + b7*SLOPE*cos(ASP) +
          b8*ELEV + b9*ELEV^2 + b10*CR + b11*CR^2 +
          b12*BAL + b13*BA +
          effect_plot[plot] + effect_location[location] + error
```

**Features**:
- Hierarchical species-level pooling
- Plot and location random effects
- Informative priors from FVS parameters
- 13 fixed effects + variance components
- Stan: 4 chains × 2000 iterations

**Prior Specification**:
```
b0[s] ~ N(FVS_intercept, 1.5)
b1[s] ~ N(FVS_DGLD, 0.5)
b2[s] ~ N(FVS_DGDS, 0.1)
b3[s] ~ N(FVS_DGSITE, 0.5)
b4-b9 ~ N(0, 0.5)                # Weakly informative
b10 ~ N(0, 1.0)                  # Expect positive CR effect
b11 ~ N(0, 0.5)                  # Expect negative CR^2
b12 ~ N(-0.5, 0.5)               # Expect negative BAL
b13 ~ N(0.5, 0.5)                # Expect positive BA
sigma_* ~ Exponential(2)
sigma ~ Exponential(1)
```

### Height-Diameter Model

**Script**: `R/03_fit_height_diameter.R`

**Model Form** (Chapman-Richards):
```
H = 4.5 + a * (1 - exp(-b * DBH))^c
  with species-level varying parameters
```

### Mortality Model

**Script**: `R/04_fit_mortality.R`

**Model Form**:
```
logit(P_survive) = b0 + b1*DBH + b2*DBH^2 + b3*BAL +
                   b4*CR + b5*SI + b6*BA + plot_effect
```

### Crown Ratio Model

**Script**: `R/05_fit_crown_ratio.R`

**Model Form**:
```
delta_CR = b0 + b1*DBH + b2*DBH^2 + b3*BA + b4*BAL +
           b5*CR + b6*SI + plot_effect
```

---

## Output Structure

### Per Variant

```
output/variants/{variant}/
├── diameter_growth_samples.rds              [Raw MCMC draws]
├── diameter_growth_posterior.csv            [Summary: p50, p05, p95, Rhat, ESS]
├── diameter_growth_summary.csv              [Full posterior summary]
├── diameter_growth_traceplots.pdf           [MCMC chain diagnostics]
├── diameter_growth_ppc.pdf                  [Posterior predictive check]
├── height_diameter_samples.rds
├── height_diameter_summary.csv
├── height_diameter_ppc.pdf
├── height_diameter_effects.pdf
├── mortality_samples.rds
├── mortality_summary.csv
├── mortality_ppc.pdf
├── mortality_effects.pdf
├── crown_ratio_samples.rds
├── crown_ratio_summary.csv
├── crown_ratio_ppc.pdf
├── crown_ratio_effects.pdf
└── diagnostics/
    ├── DIAGNOSTICS_REPORT.md               [Comprehensive text report]
    ├── convergence_summary.csv
    ├── poor_convergence.csv                [If any issues]
    ├── parameter_credible_intervals.pdf
    ├── prior_posterior_comparison.pdf
    ├── parameter_correlations.pdf
    └── ess_efficiency.csv
```

### Calibrated Configs

```
config/calibrated/
├── acd.json, ak.json, bc.json, ...        [25 calibrated configs]
└── [One config per variant, FVS-compatible]
```

---

## Key Parameters and Priors

### Diameter Growth Parameters

| Parameter | FVS Name | Prior Mean | Prior SD | Interpretation |
|-----------|----------|-----------|----------|---|
| b0[s] | (intercept) | FVS_B0 | 1.5 | Intercept by species |
| b1[s] | DGLD | FVS_DGLD | 0.5 | ln(DBH) effect (positive) |
| b2[s] | DGDS | FVS_DGDS | 0.1 | DBH^2 effect (negative) |
| b3[s] | DGSITE | FVS_DGSITE | 0.5 | ln(SI) effect |
| b4 | DGSLOP | 0 | 0.5 | Slope effect |
| b5 | DGSLSQ | 0 | 0.5 | Slope^2 effect |
| b6 | DGSASP | 0 | 0.5 | Slope × sin(aspect) |
| b7 | DGCASP | 0 | 0.5 | Slope × cos(aspect) |
| b8 | DGEL | 0 | 0.5 | Elevation effect |
| b9 | DGELSQ | 0 | 0.5 | Elevation^2 effect |
| b10 | DGCR | 0 | 1.0 | Crown ratio (positive) |
| b11 | DGCRSQ | 0 | 0.5 | Crown ratio^2 (negative) |
| b12 | DGBAL | -0.5 | 0.5 | BAL effect (negative) |
| b13 | DGPCCF | 0.5 | 0.5 | BA/CCF effect (positive) |

---

## Resources Required

### Compute (on OSC)

**Per Variant (4 cores, 16 GB)**:
- Diameter growth: 20-40 min
- Height-diameter: 10-20 min
- Mortality: 15-30 min
- Crown ratio: 15-30 min
- **Total: 2-3 hours per variant**

**Full Pipeline (25 variants parallel)**:
- **Wall time: 2-3 hours**
- CPU-hours: 100-150
- Memory peak: ~350 GB total

### Storage

| Type | Size |
|---|---|
| Raw FIA cache | ~50 GB |
| Processed data | ~500 MB × 25 variants = 12.5 GB |
| Posterior samples (uncompressed) | ~200 MB × 25 variants = 5 GB |
| Logs & diagnostics | ~100 MB × 25 variants = 2.5 GB |
| Calibrated configs | ~5 MB × 25 variants = 125 MB |
| **Total** | **~70 GB** |

---

## Quality Assurance

### Convergence Criteria

All parameters must satisfy:
- Rhat < 1.01 (excellent convergence)
- ESS_bulk > 400 (adequate for inference)
- ESS_tail > 400 (adequate for tail quantiles)
- No divergent transitions

### Validation Checks

1. **Posterior Predictive Checks**: Observations vs posterior predictions
2. **Parameter Correlation**: Check for problematic identifiability
3. **Prior vs Posterior**: Ensure data informed the fit
4. **Biological Plausibility**: Growth parameters in reasonable ranges

### Model Comparison

- LOO-CV (leave-one-out cross validation) scores in generated quantities
- Comparison of original vs calibrated predictions on hold-out FIA data

---

## Integration with fvs-modern

### Using Calibrated Configs

```r
# Load calibrated parameters
calib <- jsonlite::fromJSON("config/calibrated/ca.json")

# Compare with original
orig <- jsonlite::fromJSON("config/ca.json")

# Use in simulations (e.g., via FVS Fortran via rFVS or similar)
# Parameters automatically applied to variant
```

### Backup Strategy

```bash
# Keep originals
cp config/*.json config/original_backup/

# Use calibrated
cp config/calibrated/*.json config/
```

---

## Troubleshooting Reference

| Issue | Location in README | Solution |
|---|---|---|
| High Rhat | Common Issues section | Increase iterations, tighten priors |
| Low ESS | Common Issues section | Adjust adapt_delta, standardize predictors |
| Memory errors | Common Issues section | Reduce iterations, fewer chains |
| FIA data missing | Common Issues section | Check internet, manual download |
| Stan compilation error | 00_setup.R | Re-run setup, check compiler |

---

## Development Checklist

- [x] Wykoff diameter growth model
- [x] Height-diameter Chapman-Richards model
- [x] Mortality logistic regression
- [x] Crown ratio change model
- [x] Informative prior from FVS configs
- [x] Hierarchical species pooling
- [x] Plot and location random effects
- [x] Posterior predictive checks
- [x] Convergence diagnostics
- [x] JSON output generation
- [x] SLURM array job setup
- [x] Per-variant orchestration script
- [x] Comprehensive logging
- [x] README documentation
- [x] Quick start guide

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-03-26 | Initial complete pipeline release |

---

## Contact and Support

**Project**: fvs-modern (FVS Bayesian Calibration)
**Author**: Aaron Weiskittel
**Email**: aweiskittel@maine.edu

---

**Last Updated**: 2026-03-26
**Code Status**: Production Ready
**Test Status**: Validated on OSC SLURM environment

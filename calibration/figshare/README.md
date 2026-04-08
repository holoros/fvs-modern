# Bayesian Calibration of the Forest Vegetation Simulator (FVS): Reproducibility Package

## Overview

This repository contains the R scripts, Stan models, and calibrated parameter files for a Bayesian recalibration of six core model components in the USDA Forest Service Forest Vegetation Simulator (FVS) across all 25 geographic variants using FIA remeasurement data.

## Citation

Weiskittel, A.R. [et al.]. (2026). Bayesian calibration of the Forest Vegetation Simulator using national forest inventory data. [Journal TBD].

## Model Components

Six model components were recalibrated per variant:

| Component | Model Form | Method | Variants |
|-----------|-----------|--------|----------|
| Height-Diameter | Chapman-Richards: H = 4.5 + a*(1-exp(-b*DBH/20))^c | brms + CmdStanR (HMC) | 25/25 |
| Diameter Growth | Wykoff: ln(DDS) ~ species intercepts + 13 fixed effects | Custom Stan (HMC) | 25/25 |
| Mortality | Logistic: survived ~ DBH + DBH^2 + BAL + CR + SI + BA + (1\|SPCD) | brms (HMC) | 25/25 |
| Crown Ratio Change | Linear: CR_change ~ DBH + DBH^2 + BA + BAL + CR + SI + (1\|SPCD) | brms (HMC) | 25/25 |
| Stand Density (SDIMAX) | Quantile regression (tau=0.97) + Bayesian hierarchical | quantreg + brms | 25/25 |
| Height Increment | ln(HTG) ~ species intercepts + ln(DBH) + ln(HT) + CR + SI + BAL + BA | Custom Stan (MAP) | 6/25 |

## Geographic Variants

All 25 FVS geographic variants: ACD, AK, BC, BM, CA, CI, CR, CS, EC, EM, IE, KT, LS, NC, NE, OC, ON, OP, PN, SN, SO, TT, UT, WC, WS.

Height increment calibration applies only to variants with explicit HG parameters: BC, CI, EM, IE, KT, WS.

## Repository Structure

```
fvs_figshare/
├── README.md                          # This file
├── DATA_DICTIONARY.md                 # Description of all output fields
├── 00_reproduce_calibration.R         # Master pipeline script
├── scripts/
│   ├── 01_fetch_fia_data.R            # FIA data acquisition and remeasurement pair building
│   ├── 02_fit_diameter_growth.R       # Wykoff DG model (custom Stan)
│   ├── 03_fit_height_diameter.R       # Chapman-Richards H-D (brms)
│   ├── 03b_fit_height_increment.R     # Height increment (custom Stan, 6 variants)
│   ├── 04_fit_mortality.R             # Logistic mortality (brms)
│   ├── 05_fit_crown_ratio.R           # Crown ratio change (brms)
│   ├── 06_posterior_to_json.R         # Posterior -> calibrated FVS JSON
│   ├── 08_fetch_stand_data.R          # Stand-level density data from FIA
│   ├── 09_fit_stand_density.R         # SDIMAX calibration (QR + Bayesian)
│   ├── 11_full_comparison.R           # Default vs calibrated comparison
│   └── 12_comprehensive_assessment.R  # Full model system evaluation
├── stan_models/
│   ├── wykoff_dg.stan                 # Diameter growth Stan model
│   ├── height_diameter.stan           # H-D Stan model
│   ├── height_increment.stan          # Height increment Stan model
│   ├── mortality.stan                 # Mortality Stan model
│   └── crown_ratio_change.stan        # Crown ratio change Stan model
├── calibrated_configs/                # One JSON per variant (25 files)
│   ├── acd.json
│   ├── ...
│   └── ws.json
├── summary_data/                      # Aggregated results (no raw FIA data)
│   ├── model_performance_summary.csv  # Per-component, per-variant fit statistics
│   ├── hd_tree_level_metrics.csv      # H-D RMSE, bias, R2 by variant
│   ├── mortality_tree_level_metrics.csv
│   ├── crown_ratio_tree_level_metrics.csv
│   ├── diameter_growth_tree_level_metrics.csv
│   ├── sdimax_stand_level_metrics.csv
│   ├── equation_availability_full.csv # Completeness heatmap data
│   ├── cross_component_summary.csv    # Aggregated across variants
│   └── assessment/                    # From 12_comprehensive_assessment.R
│       ├── bakuzis_matrix_summary.csv
│       ├── hd_equivalence_test.csv
│       ├── projection_realism_checks.csv
│       └── table1_overall_performance.csv
└── figures/                           # Publication-ready PNGs
    ├── fig_calibration_heatmap.png
    ├── fig_hd_r2_comparison.png
    ├── fig_mortality_auc.png
    └── ...
```

## Software Requirements

- R >= 4.2.0
- CmdStan >= 2.33 (install via `cmdstanr::install_cmdstan()`)
- R packages: tidyverse, data.table, brms, cmdstanr, posterior, bayesplot, tidybayes, jsonlite, quantreg, rFIA, logger, parallel, arrow (optional), sf, terra

## Data Requirements

FIA ENTIRE flat files (not included due to size):
- ENTIRE_TREE.csv (~8 GB)
- ENTIRE_PLOT.csv (~500 MB)
- ENTIRE_COND.csv (~1 GB)

Download from: https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html

FVS default configuration JSONs (included in `config/` directory of the GitHub repository).

## Reproduction Instructions

### On an HPC cluster with SLURM

```bash
# Clone the repository
git clone https://github.com/holoros/fvs-modern.git
cd fvs-modern

# Edit the OSC configuration
vi calibration/osc/config_osc.sh

# Download FIA data
# Place ENTIRE_TREE.csv, ENTIRE_PLOT.csv, ENTIRE_COND.csv in your FIA directory

# Submit all 25 variants as array job
bash calibration/osc/submit_cardinal.sh
```

### On a local workstation (single variant)

```bash
# Set environment variables
export FVS_PROJECT_ROOT="/path/to/fvs-modern"
export FVS_FIA_DATA_DIR="/path/to/FIA"

# Run one variant at a time
Rscript 00_reproduce_calibration.R --variant ne --fia-dir /path/to/FIA
```

Expected runtime per variant: 2-6 hours on a machine with 8+ cores and 32+ GB RAM.

### Running just the comparison/assessment

If you have the calibrated outputs already:

```bash
Rscript scripts/11_full_comparison.R --all
Rscript scripts/12_comprehensive_assessment.R --all
```

## Inference Strategy

All Bayesian models use a multi-strategy approach:

1. **MAP optimization** (LBFGS via CmdStan): provides starting values and point estimates
2. **HMC sampling** (4 chains, 1000-1500 iterations): primary inference method
3. **Variational inference** (ADVI meanfield): automatic fallback if HMC fails to converge

This ensures robust parameter estimation even for variants with challenging posterior geometries.

## Evaluation Framework

The assessment follows Weiskittel et al. (2011, Ch. 15-16) and includes:

- **Tree-level diagnostics**: RMSE, bias, R2 stratified by DBH size class and species functional group
- **Equivalence testing**: Reynolds (1984) method with 10% tolerance
- **Bakuzis matrix**: biological realism checks (Sukachev effect, Reineke's rule, Eichhorn's rule, mortality U-shape, crown recession)
- **Stand-level projection**: 50-year forward simulation using all components simultaneously
- **Species cross-variant consistency**: performance of shared species across multiple variants

## License

[To be determined by authors]

## Contact

Aaron R. Weiskittel
Center for Research on Sustainable Forests
University of Maine
aaron.weiskittel@maine.edu

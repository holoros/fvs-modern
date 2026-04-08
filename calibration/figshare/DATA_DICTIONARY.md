# Data Dictionary: FVS Bayesian Calibration Output Files

## Calibrated Configuration JSONs (`calibrated_configs/{variant}.json`)

Each JSON file matches the FVS variant configuration schema with updated parameter arrays. Key fields:

| Field | Description |
|-------|-------------|
| `categories.species_definitions.FIAJSP` | FIA species codes mapped to FVS species indices |
| `categories.height_diameter.HT_A` | Chapman-Richards asymptote parameter (a) per species |
| `categories.height_diameter.HT_B` | Chapman-Richards rate parameter (b) per species |
| `categories.height_diameter.HT_C` | Chapman-Richards shape parameter (c), shared |
| `categories.diameter_growth.DG_INTERCEPT` | Wykoff species intercepts (ln(DDS) scale) |
| `categories.diameter_growth.DG_SLOPES` | 13 shared fixed effects for DG model |
| `categories.mortality.MORT_INTERCEPT` | Logistic intercept per species |
| `categories.mortality.MORT_SLOPES` | 6 shared fixed effects for mortality |
| `categories.crown_ratio.CR_INTERCEPT` | CR change intercept per species |
| `categories.crown_ratio.CR_SLOPES` | 6 shared fixed effects for CR change |
| `categories.stand_density.SDICON` | Calibrated SDIMAX per species |

## Summary Data Files

### `model_performance_summary.csv`

Master table of all fit statistics across components and variants.

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code (uppercase) |
| component | character | Model component (Height_Diameter, Mortality, Crown_Ratio, Diameter_Growth, SDIMAX) |
| metric | character | Performance metric (rmse, bias, r2, auc, brier_score, etc.) |
| model | character | Model version (calibrated, baseline, comparison) |
| level | character | Assessment level (tree, stand) |
| value | numeric | Metric value |

### `hd_tree_level_metrics.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| n | integer | Number of observations |
| rmse_cal | numeric | RMSE of calibrated H-D model (ft) |
| bias_cal | numeric | Bias of calibrated model (ft, pred - obs) |
| r2_cal | numeric | R-squared of calibrated model |
| rmse_base | numeric | RMSE of baseline species power curve |
| bias_base | numeric | Bias of baseline model |
| r2_base | numeric | R-squared of baseline model |

### `mortality_tree_level_metrics.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| n | integer | Number of observations |
| overall_mort_rate | numeric | Observed annual mortality rate |
| auc | numeric | Area under ROC curve (discrimination) |
| brier_score | numeric | Brier score (calibration + discrimination) |
| n_species | integer | Number of species in variant |

### `crown_ratio_tree_level_metrics.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| n | integer | Number of observations |
| rmse_cal | numeric | RMSE of calibrated CR change model |
| bias_cal | numeric | Bias (predicted - observed CR change) |
| r2_cal | numeric | R-squared |

### `diameter_growth_tree_level_metrics.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| n | integer | Number of observations |
| rmse_log | numeric | RMSE on ln(DDS) scale |
| bias_log | numeric | Bias on ln(DDS) scale |
| r2_log | numeric | R-squared on ln(DDS) scale |

### `sdimax_stand_level_metrics.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| n_species | integer | Number of matched species |
| mean_default | numeric | Mean default SDIMAX across species |
| mean_calibrated | numeric | Mean calibrated SDIMAX |
| mean_pct_change | numeric | Mean percent change from default |
| correlation | numeric | Correlation between default and calibrated |

### `equation_availability_full.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| HD | logical | Height-diameter calibrated? |
| MORT | logical | Mortality calibrated? |
| CR | logical | Crown ratio calibrated? |
| DG | logical | Diameter growth calibrated? |
| SDI | logical | SDIMAX calibrated? |
| HI | logical | Height increment calibrated? |

## Assessment Output Files (from `12_comprehensive_assessment.R`)

### `bakuzis_matrix_summary.csv`

| Column | Type | Description |
|--------|------|-------------|
| law | character | Ecological law tested |
| pass_rate | numeric | Percent of variants passing the test |
| n_variants | integer | Number of variants evaluated |

Laws tested: Sukachev (competition reduces growth), Reineke (self-thinning exponent near 1.605), Eichhorn (site quality increases growth), Mortality_U_Shape (U-shaped mortality-size), Crown_Recession (density reduces crown ratio).

### `hd_equivalence_test.csv`

Reynolds (1984) equivalence test results per variant with 10% tolerance.

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| mean_diff | numeric | Mean prediction difference (pred - obs) |
| se_diff | numeric | Standard error of difference |
| ci_lower | numeric | Lower confidence interval bound |
| ci_upper | numeric | Upper confidence interval bound |
| tolerance | numeric | Equivalence tolerance (10% of mean obs) |
| equivalent | logical | Does the CI fall within the tolerance band? |

### `stand_projection_trajectories.csv`

50-year stand-level forward projections using coupled calibrated models.

| Column | Type | Description |
|--------|------|-------------|
| year | integer | Projection year (0 to 50) |
| tpa | numeric | Trees per acre |
| ba | numeric | Basal area (sq ft per acre) |
| qmd | numeric | Quadratic mean diameter (in.) |
| sdi | numeric | Stand density index (Reineke, imperial) |
| top_ht | numeric | Top height (ft) |
| n_trees | integer | Number of trees remaining in tree list |
| variant | character | FVS variant code |

### `projection_realism_checks.csv`

| Column | Type | Description |
|--------|------|-------------|
| variant | character | FVS variant code |
| ba_initial | numeric | Initial basal area |
| ba_final | numeric | Final basal area (year 50) |
| ba_growth_reasonable | logical | BA increased but stayed below 400 sq ft/ac |
| qmd_grows | logical | QMD increased over 50 years |
| tpa_declines | logical | TPA decreased (mortality exceeded ingrowth) |
| sdi_bounded | logical | SDI stayed below 1500 |
| all_checks_pass | logical | All realism tests passed |

## Variable Definitions

### Tree-level predictors (used in calibration models)

| Variable | Units | Description |
|----------|-------|-------------|
| DIA_t1 | inches | Diameter at breast height, time 1 |
| DIA_t2 | inches | Diameter at breast height, time 2 |
| HT_t1 | feet | Total tree height, time 1 |
| HT_t2 | feet | Total tree height, time 2 |
| CR_pct | percent | Crown ratio (0 to 100) |
| BA | sq ft/acre | Stand basal area |
| BAL | sq ft/acre | Basal area in larger trees (one-sided competition index) |
| SI | feet | Site index (base age 50, variant-specific equation) |
| SPCD | integer | FIA species code |
| SLOPE | proportion | Terrain slope (0 to 1) |
| ASPECT | degrees | Terrain aspect (0 to 360) |
| ELEV | feet | Elevation above sea level |
| ln_DDS | - | Natural log of change in squared diameter: ln(DIA_t2^2 - DIA_t1^2) |
| DBH_scaled | - | DIA_t1 / 20 (fixed scaling for Chapman-Richards) |

### Standardization

All _std suffixed variables in the mortality and crown ratio models are standardized using `scale()` (zero mean, unit variance) computed within each variant's training data. The diameter growth model uses externally saved standardization parameters stored in `standardization_params.rds` per variant.

## FIA Data Source

All models were fitted using the USDA Forest Service Forest Inventory and Analysis (FIA) national database, specifically the ENTIRE flat file format. Remeasurement pairs were built from trees measured on at least two occasions with positive growth intervals.

Panel years included: most recent complete cycle per state as of the 2024 data download.

States included per variant are defined in the variant-to-state mapping in `01_fetch_fia_data.R`.

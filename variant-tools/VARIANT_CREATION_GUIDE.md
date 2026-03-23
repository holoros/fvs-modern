# FVS Variant Creation and Calibration Guide

## Table of Contents

* Overview
* Understanding the FVS Variant System
* Data Requirements for Calibration
* Step-by-Step Variant Creation Process
* Recalibrating Existing Variants
* Model Equations and Fitting
* R Code Examples for Model Calibration
* Validation Procedures
* Submitting Your Variant

---

## Overview

This guide walks you through the process of creating a new Forest Vegetation Simulator (FVS) variant for a specific region or recalibrating an existing variant with new data. The FVS framework is modular, allowing researchers to customize growth and mortality models to their particular forest types and regions.

Creating a variant is a significant undertaking that typically requires:
* 1-3 months of data preparation and analysis
* Regional inventory data with multiple measurements per plot
* Expertise in forest growth modeling and statistics
* Familiarity with Fortran programming (for final implementation)
* Access to FVS source code and documentation

---

## Understanding the FVS Variant System

### What is an FVS Variant?

An FVS variant is a region-specific implementation of the FVS growth and mortality models. A variant consists of:

1. **Species List**: Mapping between FIA species codes and internal array indices
2. **Species Coefficients**: Calibrated parameters for each species in that region
3. **Model Equations**: The mathematical forms used for growth predictions
4. **Supporting Code**: Fortran modules that implement the models

### Variant Components

Each variant directory contains these key files:

* `crown.f90`: Crown ratio model (predicts what fraction of tree height is crown)
* `dgf.f90`: Diameter growth function (predicts annual diameter increment)
* `morts.f90` or `varmrt.f90`: Mortality model (predicts tree survival)
* `htdbh.f90`: Height-diameter relationship (predicts height from diameter)
* `sitset.f90`: Site index initialization
* `blkdat.f90`: Block data with species coefficients
* `PRGPRM.F90`: Program parameters (memory allocation, species count)

### Default Variants in FVS

FVS comes with 13+ regional variants:

* **AK** - Alaska
* **BM** - Blue Mountains (OR/WA)
* **CA** - California
* **CI** - Central Interior (CO/WY/UT)
* **CR** - Central Rockies (MT/CO)
* **NE** - Northeast (New England, NY, PA, NJ)
* **NC** - North Central (Great Lakes)
* **PN** - Pacific Northwest
* **SN** - South
* **UT** - Utah
* **WC** - West Cascades
* Plus specialized variants (plantation, insect/disease, etc.)

---

## Data Requirements for Calibration

### Minimum Data Needed

To calibrate a variant, you need regional forest inventory data with:

1. **Plot-level information**:
   * Geographic location (latitude, longitude, elevation)
   * Forest type classification
   * Plot size and sampling probability
   * Site variables (slope, aspect, soil type, if available)

2. **Tree-level measurements**:
   * Diameter at breast height (DBH) in inches
   * Total height in feet
   * Species identification (using FIA codes)
   * Crown ratio (percentage of tree height that is crown) - preferred but optional
   * Crown position (dominant, codominant, intermediate, overtopped)

3. **Repeated measurements**:
   * For growth models: measurements 5-10 years apart on same trees
   * For mortality: tracking which trees survived between measurements
   * Multiple plot clusters across your region's environmental gradient

### Recommended Data Volume

* Minimum: 500-1000 trees with repeated measurements
* Ideal: 5000+ trees, 50+ plots, spanning environmental range
* Multiple measurement intervals (don't cluster all intervals at same time span)

### Data Sources

**USDA Forest Service FIA Program**
* Publicly available FIA plot database
* 5-year measurement cycle
* Consistent sampling across regions
* Website: https://www.fia.fs.fed.us/

**State Forestry Agencies**
* State forest inventories
* May have more detailed regional data
* Contact state forestry service or GIS center

**Forest Certification Programs**
* PEFC, FSC certified forest monitoring data
* Typically have detailed measurements
* May require licensing agreement

**University Forestry Programs**
* May have research plot networks
* Often available for academic use

**Your Own Inventory Data**
* If you manage forests regionally
* Should be quality-controlled and representative

---

## Step-by-Step Variant Creation Process

### Phase 1: Project Setup and Data Preparation (1-2 weeks)

#### Step 1.1: Decide on Regional Scope

Define your variant's geographic and ecological scope:

* Geographic area covered (state, multi-state region, etc.)
* Forest types/biomes included
* Elevation and precipitation range
* Climate zones represented

Example: "Northeast variant covers forests from Maine to Pennsylvania,
elevations 0-6000 ft, mixed hardwood-softwood types"

#### Step 1.2: Compile Species List

Identify which species occur in your region and their FIA codes:

* Count total number of species (this sets MAXSP parameter)
* Create mapping of FIA code → common name → array index 1 to MAXSP
* Research shade tolerance classification for each species
* Assign maximum realistic DBH and height

Use the `Species_List` sheet in `variant_coefficients_template.xlsx` to organize this.

Sources:
* USDA Forest Service Regional Guides
* State forest type guides
* E-Flora or Flora databases for your region

#### Step 1.3: Compile and Clean Inventory Data

Obtain regional inventory data:

* Download FIA data from national database, filter to your region
* OR obtain from state forestry agency
* Quality control:
  * Remove outliers (DBH > 100", heights > 250')
  * Remove records with missing critical values
  * Verify species codes are valid
  * Check for measurement errors (e.g., height < 4.5 ft for DBH > 6")

Organize into a consistent format:

```
plot_id, tree_id, species_code, year, dbh, height, crown_ratio, crown_class
101,     1,       12,           2010, 14.5, 68.0,   0.45,         2
101,     1,       12,           2015, 16.2, 70.5,   0.47,         2
...
```

### Phase 2: Model Calibration (2-8 weeks depending on data availability)

#### Step 2.1: Crown Ratio Model

**Model equation**:
```
CR = 10 * (BCR1 / (1 + BCR2*BA) + BCR3 * (1 - exp(BCR4*DBH)))
```

Where:
* CR = crown ratio (percentage, 1-95)
* BA = stand basal area (sq ft/acre)
* DBH = diameter (inches)
* BCR1-BCR4 = species-specific coefficients to estimate

**Data needed**: Crown ratio measurements from inventory

**R Fitting Code** (see section 4.2 below for full example)

**Expected coefficient ranges**:
* BCR1: 3-8 (vertical intercept component)
* BCR2: 0.002-0.015 (BA sensitivity; positive = BA reduces crown)
* BCR3: -3 to 8 (asymptotic crown ratio)
* BCR4: -0.5 to 0.05 (diameter effect; usually negative)

**Validation**: Plot predicted vs. observed crown ratio across DBH and BA gradient

#### Step 2.2: Diameter Growth Model

**Model equation** (on log scale):
```
ln(DDS) = b0 + b1*ln(DBH) + b2*BA + b3*SI + COR(ispc)
```

Where:
* DDS = change in squared diameter
* DBH = initial diameter
* BA = basal area of larger trees (competition)
* SI = site index
* b0-b3 = coefficients
* COR = species bias correction

**Data needed**: Pairs of measurements on same trees 5+ years apart

**Statistical approach**: Mixed-effects regression accounts for:
* Repeated measurements on same trees
* Random plot effects
* Heterogeneous variances by species

**R Fitting Code** (see section 4.3 below)

**Expected coefficient ranges**:
* B1 (SI sensitivity): 0.0005-0.0015
* B2 (DBH sensitivity): 0.03-0.15
* COR: 0.8-1.2 (1.0 = no bias)

#### Step 2.3: Mortality/Survival Model

**Model equation** (logistic):
```
Annual Mortality = 1 / (1 + exp(-(b0 + b1*ln(DBH) + b2*BA + b3*Rel_Height)))
Multi-year Survival = (1 - Annual_Mortality)^years
```

**Data needed**: Tracking which trees died between measurements

**Statistical approach**: Logistic regression on binary outcome (alive/dead)

**Species-specific parameter**: Shade tolerance (VARADJ)
* Low (0.1-0.3): Shade-intolerant species (e.g., pines, aspens)
  * High mortality when overtopped
* Medium (0.4-0.6): Intermediate tolerance (e.g., many hardwoods)
* High (0.7-0.9+): Shade-tolerant species (e.g., hemlocks, spruces, maples)

**R Fitting Code** (see section 4.4 below)

#### Step 2.4: Height-Diameter Relationship

**Model equation** (Curtis-Arney):
```
H = 4.5 + exp(b0 + b1/DBH^b2)
```

Where:
* H = total height (feet)
* DBH = diameter (inches)
* b0, b1, b2 = species-specific coefficients

**Data needed**: Height and diameter pairs (no need for repeated measures)

**Statistical approach**: Non-linear least squares regression per species

**Expected coefficient ranges**:
* b0: -3.0 to -1.5
* b1: 10 to 20
* b2: 0.8 to 1.2

**Validation**: Plot predicted vs. observed height for full DBH range

#### Step 2.5: Site Index and Productivity

**Default site index**: Regional average productivity for each species
* Typical range: 40-100 feet at age 50
* Publish state-specific values if available
* Document assumed age base (typically 50 years)

**Site index curves**: If you have dominant-tree heights and ages:
* Fit Chapman-Richards curve: H = a*(1-exp(-b*Age))^c
* Solve for site index (height at reference age, e.g., 50 years)

**Stand Density Index (SDI)**: Maximum self-thinning density by species
* Used in mortality and density-dependent growth models
* Typical range: 200-800 (varies by species)
* Can often use published values rather than estimate

### Phase 3: Implementation in Fortran (1-2 weeks)

#### Step 3.1: Copy Template Directory

```bash
cp -r template_variant my_variant_xx
cd my_variant_xx
```

Where `xx` = your variant code (2 letters)

#### Step 3.2: Update Fortran Templates with Coefficients

For each Fortran file, replace the coefficient arrays with your calibrated values:

**crown.f90**:
```fortran
real, parameter :: BCR1(108) = [ & ... your values ... ]
real, parameter :: BCR2(108) = [ & ... your values ... ]
real, parameter :: BCR3(108) = [ & ... your values ... ]
real, parameter :: BCR4(108) = [ & ... your values ... ]
```

**dgf.f90**:
```fortran
real, parameter :: B1(108) = [ & ... your values ... ]
real, parameter :: B2(108) = [ & ... your values ... ]
real, parameter :: COR(108) = [ & ... your bias corrections ... ]
```

**morts.f90**:
```fortran
real, parameter :: VARADJ(108) = [ & ... shade tolerance values ... ]
real, parameter :: MORT_B0(108) = [ & ... logistic intercepts ... ]
real, parameter :: MORT_B1(108) = [ & ... logistic slopes ... ]
```

**htdbh.f90**:
```fortran
real, parameter :: COEFFS(3,108) = reshape([ &
  -2.53, 15.4, 1.05,  & ! Species 1: b0, b1, b2
  -1.92, 12.1, 1.00,  & ! Species 2
  ... all 108 species ...
], shape=[3, 108])
```

**blkdat.f90**:
```fortran
real :: BKRAT(MAXSP) = [ & ... bark ratios ... ]
real :: COR2(MAXSP) = [ & ... diameter growth corrections ... ]
```

**sitset.f90**:
```fortran
real :: SDICON(MAXSP) = [ & ... SDI maximums ... ]
```

**PRGPRM.F90**: Update only if changing species count or memory parameters
```fortran
integer, parameter :: MAXSP = XX  ! Your actual species count (or 108 for compatibility)
```

#### Step 3.3: Register Variant in Build System

Add your variant to CMakeLists.txt (or appropriate build configuration):

```cmake
# Add new variant source files
set(my_variant_xx_sources
  my_variant_xx/crown.f90
  my_variant_xx/dgf.f90
  my_variant_xx/morts.f90
  my_variant_xx/htdbh.f90
  my_variant_xx/blkdat.f90
  my_variant_xx/sitset.f90
)

# Include in variant library
target_sources(fvs_variants PRIVATE ${my_variant_xx_sources})
```

#### Step 3.4: Build and Test

```bash
cmake ..
make -j 4

# Run test with a small inventory
./fvs_simulation --variant xx --input test_data.txt --cycles 8
```

### Phase 4: Validation and Documentation (2-4 weeks)

#### Step 4.1: Independent Validation

Set aside 20-30% of your data as an independent test set (not used in fitting):

1. Run FVS with your variant on test inventory
2. Compare 5-year predictions to actual measurements
3. Calculate:
   * Bias: (Predicted - Observed) average
   * RMSE: sqrt(mean((Predicted - Observed)^2))
   * Correlation: scatter plot of predicted vs. observed
4. Document results (plots and summary statistics)

#### Step 4.2: Sensitivity Analysis

Test model robustness:

* Vary inputs ±10%: Do outputs change reasonably?
* Test on extreme conditions: Very dense stands, very old trees, etc.
* Compare growth patterns to published curves for your region
* Consult with regional forest ecologists

#### Step 4.3: Create Documentation

Document your variant in a README file including:

```markdown
# Variant XX - [Region Name]

## Geographic and Ecological Scope
* States/provinces included
* Elevation range
* Forest types represented
* Climate zones

## Species
Total: XX species
List with FIA codes and common names

## Data Sources
* Inventory data: [source, # of plots, # of years]
* Growth data: [source, # of tree-pairs, time interval]
* Mortality data: [source, # of observations]

## Model Calibration Summary
* Crown model: [R-squared, RMSE]
* Diameter growth: [conditional R-squared, RMSE]
* Mortality: [AIC, classification accuracy]
* Height-diameter: [R-squared by species]

## Validation Results
* Independent test set performance
* Bias and RMSE for 5-year projections
* Sensitivity analysis results

## References
Citations to published growth and yield studies, regional guides, etc.

## Maintainer
Your name and contact information
```

---

## Recalibrating Existing Variants

If you want to update an existing variant with new data:

### Step 1: Obtain Current Coefficients

From the existing variant's Fortran source files (crown.f90, dgf.f90, etc.)

### Step 2: Prepare New Data

* Compile new inventory data from recent remeasurements
* Combine with historical data if enhancing sample size
* Quality control as described in Phase 1.3

### Step 3: Refit Models

* Run same R regression scripts as new variant creation
* New data might improve coefficient precision
* Check if coefficients have changed substantially from old values
* If changes are > 10%, investigate why (data quality? real change?)

### Step 4: Limited Validation

* Test new variant on subset of old calibration data
* Verify it still performs reasonably on "historical" plots
* Only validate on truly independent, new data

### Step 5: Create New Release

* Version the coefficients (e.g., "Variant NE v2.0")
* Document changes from previous version
* Update the variant documentation
* Submit to FVS team with change log

---

## Model Equations and Fitting Details

### Crown Ratio Model - Detailed Derivation

**Conceptual Model**:
Crown ratio decreases with:
1. Basal area (more competition = smaller crowns)
2. Diameter (larger trees have proportionally smaller crowns)

Crown ratio increases with:
* Species characteristics (some species consistently larger-crowned)

**Functional Form**:
```
CR = 10 * (A / (1 + B*BA) + C*(1 - exp(D*DBH)))
```

Terms:
* `A/(1+B*BA)`: Intercept component, reduced by stand density
* `C*(1-exp(D*DBH))`: Diameter effect (asymptotic)
* Factor of 10: Scale to percentage

**Interpretation**:
* `BCR1 (A)`: Base crown ratio at BA=0
* `BCR2 (B)`: Sensitivity to competition (units: 1/sq ft BA)
* `BCR3 (C)`: Asymptotic crown ratio at large DBH
* `BCR4 (D)`: Rate of change with diameter (usually negative)

### Diameter Growth Model - Detailed Derivation

**Conceptual Model**:
Annual diameter growth depends on:
1. Site quality (SI): Better sites support faster growth
2. Diameter (DBH): Larger trees grow faster in absolute terms
3. Competition (BAL): Basal area in larger trees limits growth

**Functional Form** (potential basal area growth):
```
POTBAG = B1 * SI * (1 - exp(-B2 * DBH))
```

This says:
* Growth is proportional to site index
* Grows asymptotically with diameter (fast at small DBH, plateaus)

**Conversion to DDS** (diameter-squared change):
```
DDS = (new_dbh)^2 - (old_dbh)^2  [in units of (dbh)^2 increment per year]
ln(DDS) = b0 + b1*ln(DBH) + b2*BA + ... + COR
```

**Why log transformation?**
* DDS spans several orders of magnitude (0.01 to 100+)
* Log normalization makes residuals more normal
* Reduces influence of large trees' extreme values

**Bias Correction (COR)**:
* Due to Jensen's inequality: E[DDS] ≠ exp(E[ln(DDS)])
* COR adjusts back-transformation
* Typical values: 0.8-1.2

### Mortality Model - Detailed Derivation

**Conceptual Model**:
Mortality probability increases with:
1. Small diameter (small trees more vulnerable)
2. High competition (high BA)
3. Overtopping (relative height low)
4. Species shade-intolerance (less tolerant of shade = more death)

**Functional Form** (logistic):
```
P(death) = 1 / (1 + exp(-(b0 + b1*ln(DBH) + b2*BA + b3*Rel_HT)))
```

**Species-Specific Shade Tolerance (VARADJ)**:
* Multiplies the effect of relative height
* High VARADJ (tolerant): small effect of overtopping
* Low VARADJ (intolerant): large effect of overtopping

**Survival over multiple years**:
Assume annual rate is constant (conservative):
```
P(survival over n years) = (1 - P(annual death))^n
```

---

## R Code Examples for Model Calibration

### Setup and Data Preparation

```r
# Load required packages
library(tidyverse)
library(nlme)
library(lme4)
library(dplyr)

# Set random seed for reproducibility
set.seed(42)

# Load inventory data
inventory <- read.csv("regional_inventory.csv")
# Expected columns: plot_id, tree_id, species_code, year, dbh, height,
#                   crown_ratio, crown_class, ba

# Filter to one species for example
species_code <- 12  # Example: balsam fir
data_species <- inventory %>%
  filter(species == species_code)

print(paste("Total records for species", species_code, ":", nrow(data_species)))
print(summary(data_species))
```

### Crown Ratio Model Fitting

```r
# Crown ratio cross-sectional model
# Equation: CR = 10*(B1/(1+B2*BA) + B3*(1-exp(B4*DBH)))

# Cross-sectional data (don't need repeated measures)
crown_data <- inventory %>%
  filter(!is.na(crown_ratio), !is.na(ba), !is.na(dbh)) %>%
  filter(species == species_code)

# Non-linear least squares fitting
fit_crown <- nls(
  crown_ratio ~ 10 * (b1/(1 + b2*ba) + b3*(1 - exp(b4*dbh))),
  data = crown_data,
  start = list(b1 = 5.5, b2 = 0.008, b3 = 3.5, b4 = -0.05),
  algorithm = "port",
  lower = c(-100, -1, -100, -1),
  upper = c(100, 1, 100, 1),
  trace = TRUE
)

# Examine results
summary(fit_crown)
coef_crown <- coef(fit_crown)

# Validation: plot predicted vs. observed
crown_data$pred <- predict(fit_crown, crown_data)
plot(crown_data$crown_ratio, crown_data$pred,
  xlab = "Observed CR", ylab = "Predicted CR", main = "Crown Ratio Model")
abline(0, 1, col = "red")

# Calculate R-squared
ss_res <- sum((crown_data$crown_ratio - crown_data$pred)^2)
ss_tot <- sum((crown_data$crown_ratio - mean(crown_data$crown_ratio))^2)
rsq <- 1 - ss_res/ss_tot
print(paste("R-squared:", round(rsq, 3)))
```

### Diameter Growth Model Fitting

```r
# Repeated measures model
# Need pairs of measurements on same trees

# Create growth dataset
growth_data <- inventory %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  filter(n() >= 2) %>%  # Only trees with ≥ 2 measurements
  mutate(
    dbh_initial = lag(dbh),
    year_initial = lag(year),
    years_interval = year - year_initial,
    dds = (dbh^2 - dbh_initial^2) / years_interval,
    log_dds = log(pmax(dds, 0.001)),
    log_dbh = log(dbh_initial),
    log_ba = log(ba + 1)  # +1 to avoid log(0)
  ) %>%
  filter(!is.na(log_dds), !is.na(log_dbh)) %>%
  filter(species == species_code) %>%
  ungroup()

print(paste("Growth records:", nrow(growth_data)))

# Mixed-effects model (random intercept by plot)
fit_dg <- lme(
  log_dds ~ log_dbh + log_ba + si,
  random = ~1 | plot_id,
  data = growth_data,
  method = "REML"
)

summary(fit_dg)
coef_dg <- fixef(fit_dg)

# Extract log-scale parameters
b0 <- coef_dg["(Intercept)"]
b1 <- coef_dg["log_dbh"]
b2 <- coef_dg["log_ba"]
b3 <- coef_dg["si"]

print(paste("DG Model: ln(DDS) =", round(b0, 4), "+",
  round(b1, 4), "*ln(DBH) +", round(b2, 4), "*ln(BA)"))

# Bias correction factor (back-transformation correction)
# For lognormal: E[Y] ≈ exp(E[ln(Y)] + sigma^2/2)
residuals_dg <- residuals(fit_dg)
sigma_dg <- sqrt(var(residuals_dg))
cor_factor <- exp(sigma_dg^2 / 2)
print(paste("Bias correction factor:", round(cor_factor, 3)))

# Predictions on original scale
growth_data$pred_log <- predict(fit_dg, growth_data)
growth_data$pred_dds <- exp(growth_data$pred_log) * cor_factor

# Validation
plot(growth_data$dds, growth_data$pred_dds,
  xlab = "Observed DDS", ylab = "Predicted DDS",
  main = "Diameter Growth Model", log = "xy")
abline(0, 1, col = "red")

# Calculate correlation coefficient
print(paste("Correlation:", round(cor(growth_data$dds, growth_data$pred_dds), 3)))
```

### Mortality Model Fitting

```r
# Binary response: mortality indicator (0=survived, 1=died)

# Create mortality dataset
mort_data <- inventory %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  filter(n() >= 2) %>%
  mutate(
    dbh_initial = lag(dbh),
    ht_initial = lag(height),
    survived = !is.na(dbh)  # TRUE if tree measured in next interval
  ) %>%
  filter(!is.na(survived), species == species_code) %>%
  select(tree_id, plot_id, dbh = dbh_initial, height = ht_initial,
    ba, survived) %>%
  mutate(
    log_dbh = log(dbh),
    rel_height = height / mean(height, na.rm = TRUE),
    mort = as.numeric(!survived)  # 1 = died, 0 = survived
  ) %>%
  ungroup()

print(paste("Mortality observations:", nrow(mort_data)))
print(paste("Trees died:", sum(mort_data$mort)))
print(paste("Trees survived:", sum(!mort_data$mort)))

# Logistic regression
fit_mort <- glm(
  mort ~ log_dbh + ba + rel_height,
  family = binomial(link = "logit"),
  data = mort_data
)

summary(fit_mort)

# Extract coefficients
b0_mort <- coef(fit_mort)["(Intercept)"]
b1_mort <- coef(fit_mort)["log_dbh"]
b2_mort <- coef(fit_mort)["ba"]
b3_mort <- coef(fit_mort)["rel_height"]

print(paste("Mortality Model: ln(odds) =", round(b0_mort, 3), "+",
  round(b1_mort, 3), "*ln(DBH) +", round(b2_mort, 4), "*BA +",
  round(b3_mort, 3), "*Rel_Ht"))

# Predictions
mort_data$pred_prob <- predict(fit_mort, mort_data, type = "response")

# Validation: ROC curve or calibration plot
plot(mort_data$pred_prob, mort_data$mort + rnorm(nrow(mort_data), sd=0.02),
  xlab = "Predicted Mortality Probability", ylab = "Observed",
  main = "Mortality Model Calibration", ylim = c(-0.2, 1.2))
abline(0, 1, col = "red")
```

### Height-Diameter Model Fitting

```r
# Curtis-Arney equation: H = 4.5 + exp(b0 + b1/DBH^b2)

# Cross-sectional height-diameter data
ht_data <- inventory %>%
  filter(!is.na(height), !is.na(dbh), dbh > 0.1) %>%
  filter(species == species_code)

# Non-linear fit
fit_htdbh <- nls(
  height ~ 4.5 + exp(b0 + b1/dbh^b2),
  data = ht_data,
  start = list(b0 = -2.5, b1 = 15, b2 = 1.0),
  algorithm = "port",
  trace = TRUE
)

summary(fit_htdbh)
coef_ht <- coef(fit_htdbh)

# Prediction across full DBH range
dbh_seq <- seq(0.5, max(ht_data$dbh), length.out = 100)
pred_frame <- data.frame(dbh = dbh_seq)
pred_ht <- predict(fit_htdbh, pred_frame)

# Plot: observations and fitted curve
plot(ht_data$dbh, ht_data$height,
  xlab = "DBH (inches)", ylab = "Height (feet)",
  main = "Height-Diameter Relationship",
  alpha = 0.3)
lines(dbh_seq, pred_ht, col = "red", lwd = 2)

# R-squared
ht_data$pred <- predict(fit_htdbh, ht_data)
ss_res <- sum((ht_data$height - ht_data$pred)^2)
ss_tot <- sum((ht_data$height - mean(ht_data$height))^2)
rsq_ht <- 1 - ss_res/ss_tot
print(paste("R-squared:", round(rsq_ht, 3)))
```

---

## Validation Procedures

### Independent Test Set Validation

```r
# Set aside 30% of data for validation before fitting

set.seed(42)
test_indices <- sample(1:nrow(inventory), size = 0.3*nrow(inventory))
test_data <- inventory[test_indices, ]
train_data <- inventory[-test_indices, ]

# Fit models using train_data only
# Then predict on test_data and compare

# For growth model: 5-year predictions
test_growth <- test_data %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  filter(n() >= 2) %>%
  mutate(
    dbh_initial = lag(dbh),
    ba_initial = lag(ba),
    dbh_5yr = dbh,
    years_actual = year - lag(year)
  ) %>%
  filter(years_actual == 5, !is.na(dbh_initial)) %>%
  select(dbh_initial, ba_initial, dbh_5yr, species)

# Predict 5-year growth using fitted model
test_growth$dg_pred <- predict(fit_dg, test_growth)
test_growth$dbh_pred_5yr <- sqrt(test_growth$dbh_initial^2 +
                                  exp(test_growth$dg_pred) * 5)

# Compare
bias <- mean(test_growth$dbh_pred_5yr - test_growth$dbh_5yr)
rmse <- sqrt(mean((test_growth$dbh_pred_5yr - test_growth$dbh_5yr)^2))

print(paste("5-year DBH prediction bias:", round(bias, 3)))
print(paste("5-year DBH prediction RMSE:", round(rmse, 3)))

# Plot: prediction error vs tree size
test_growth$error <- test_growth$dbh_pred_5yr - test_growth$dbh_5yr
plot(test_growth$dbh_initial, test_growth$error,
  xlab = "Initial DBH", ylab = "Prediction Error (predicted - observed)",
  main = "Diameter Growth Validation", abline(h=0, col="red"))
```

### Sensitivity Analysis

```r
# Test robustness to small input changes

# Example: How sensitive is growth prediction to ±10% change in BA?
ba_nominal <- 80  # Typical stand BA
ba_low <- ba_nominal * 0.9
ba_high <- ba_nominal * 1.1

dbh_test <- 15  # Test tree
si_test <- 70   # Site index
dg_nominal <- predict_dg(dbh_test, ba_nominal, si_test)
dg_low <- predict_dg(dbh_test, ba_low, si_test)
dg_high <- predict_dg(dbh_test, ba_high, si_test)

sensitivity <- (dg_high - dg_low) / (ba_high - ba_low) / dg_nominal

print(paste("Diameter growth sensitivity to BA:",
  round(sensitivity, 3), "relative change per unit BA change"))

# Should be reasonable (not wild swings for small input changes)
```

---

## Submitting Your Variant

### Required Documentation

1. **README.md** - Overview and summary statistics
2. **SPECIES_LIST.txt** - All species with FIA codes
3. **COEFFICIENTS.txt** - All calibrated coefficients
4. **MODEL_EQUATIONS.md** - Equations used in variant
5. **CALIBRATION_REPORT.pdf** - Detailed statistical summary
6. **VALIDATION_RESULTS.pdf** - Independent test set performance

### Submission Process

Email to FVS Development Team:
* fvs-feedback@fs.fed.us
* Subject: "New FVS Variant Submission: XX [Region Name]"

Attach:
* Complete variant source code (all .f90 files)
* Documentation package
* Sample test data and outputs

Include cover letter with:
* Names and affiliations of developers
* Geographic/ecological scope
* Data sources and sample sizes
* Model R-squared and validation statistics
* Contact information for questions

---

## References and Resources

### FVS Documentation
* National FVS User Guide: https://www.fvs.nrs.fs.fed.us/
* FVS Code Repository: https://github.com/USDA-Forest-Service/ForestVegetationSimulator

### Growth and Yield Literature
* Clutter, J.L., Fortson, J.C., Pienaar, L.V., Brister, G.H., Bailey, R.L. (1983). Timber management: A quantitative approach.
* Pretzsch, H. (2009). Forest Dynamics, Growth and Yield.
* Curtis, R.O. (1967). Height-diameter and height-diameter-age equations for second-growth Douglas-fir. Forest Science 13: 365-375.

### Regional Forest Resources
* USDA NRCS Web Soil Survey: https://websoilsurvey.nrcs.usda.gov/
* USGS National Elevation Dataset: https://www.usgs.gov/
* Forest Type Guides: https://www.fs.fed.us/r8/active/tg/
* State Forestry Agency Regional Offices

### Statistical Modeling in R
* Pinheiro, J.C., Bates, D.M. (2000). Mixed-Effects Models in S and S-PLUS.
* Venables, W.N., Ripley, B.D. (2002). Modern Applied Statistics with S.
* Hastie, T., Tibshirani, R., Friedman, J. (2009). The Elements of Statistical Learning.

---

**Document Version**: 1.0
**Last Updated**: 2026-03-22
**Maintainer**: FVS Development Team

# FVS-CONUS Development Notes

Saved from the April 9, 2026 planning session. These items need to be addressed
when active development begins on the `conus-variant` branch.

## Computational Strategy for National Fit

The biggest technical risk is fitting a single Wykoff DDS model to ~4.5M trees
across CONUS with species random effects and ecoregion random effects. Plan for
a staged approach:

1. **ADVI initialization** to get rough posteriors (hours, not days)
2. **Targeted HMC on 10% stratified subsample** to refine posteriors
3. **Evaluate** whether full HMC on the complete dataset is necessary or if
   the subsample posteriors are sufficiently stable
4. Per-variant fits took 12 to 36 hours each on Cardinal; a national fit
   could take days even with ADVI

## Random Effects Structure

Key decision: **nested vs. crossed** random effects for species x ecoregion.

- **Nested** (species within ecoregion): More regional flexibility but many more
  parameters. Risk of overfitting in data-sparse ecoregion x species cells.
- **Crossed** (species + ecoregion independently): More parsimonious, shares
  strength better across regions. Likely the right default.
- FIA data density probably supports crossed for common species but may require
  nested (or at least ecoregion-level slope adjustments) for rare ones.
- Consider a hybrid: crossed RE for intercepts, nested for key slope terms
  (e.g., BAL effect varying by ecoregion within species).

## FIA Data Pipeline for CONUS

- Script 01 currently fetches data per variant. CONUS needs a national fetch.
- **Deduplication step** keyed on CN/PLT_CN is essential: many plots appear in
  multiple variants' geographic boundaries.
- `plot_raster_lookup.csv` (1.9M records) needs to cover ALL CONUS FIA plots,
  not just those in current variant boundaries. Verify ClimateNA extraction
  covers the full plot network.

## Crown Ratio Redesign

Crown ratio modeling is the weakest component (R-squared 0.13 to 0.38). For
CONUS, explore:

- Nonlinear alternatives to the current linear model
- Allometric crown models derived from airborne lidar (GEDI, 3DEP)
- Empirical lookup tables by species group and competition class
- Crown competition factor (CCF) as an alternative state variable

## Ingrowth Model

Three options for CONUS:

a) **Empirical FIA-based**: Observed ingrowth rates by species group and stand
   condition. Simple, defensible, not climate-sensitive. Currently implemented.
b) **Establishment model**: Climate niche models (SDMs) combined with gap/
   disturbance detection.
c) **Hybrid**: Empirical rates modulated by climate suitability scores.

Start with (a) for initial release, develop (c) as a research priority.

## Species Code Unification

Traditional FVS uses sequential integer species codes per variant (species 1,
2, 3...) that differ between variants. CONUS uses FIA SPCD directly. The NSBE
equations already work this way. Fortran integration needs to handle:

- Dynamic species array allocation (no fixed MAXSP COMMON blocks)
- SPCD-based coefficient lookup instead of sequential index
- Graceful handling of species not in the training data (Jenkins fallback)

## Soil Data Integration

gSSURGO data is not yet integrated. For CONUS, soil properties (available water
capacity, depth to restrictive layer, clay content) could improve site
productivity and drought response predictions. Consider:

- Pre-extracting soil properties for all FIA plot locations
- Adding soil covariates to the DG and mortality models
- Evaluating marginal improvement over climate-only models

## Validation Framework

1. **Within-sample**: Cross-validated RMSE, R-squared, bias by ecoregion
2. **Out-of-sample**: Hold-out 20% of FIA plots for independent validation
3. **Variant boundary test**: Compare projections for plots near variant
   borders processed by (a) assigned variant, (b) adjacent variant,
   (c) FVS-CONUS
4. **Long-term realism**: 50-year projections checked against ecological
   bounds (Reineke SDI limits, expected mortality patterns)
5. **Species range edges**: Test performance at range margins where
   variant-specific fits have limited data

# FVS-CONUS: Technical Design Document

## 1. Problem Statement

The current FVS architecture assigns each FIA plot to one of 22 US geographic
variants. Each variant has independently parameterized equations for diameter
growth, height, mortality, crown change, and regeneration. This creates:

- **Hard boundary artifacts**: Plots near variant borders receive discontinuous
  treatment depending on assignment.
- **Inconsistent species coverage**: The same species (e.g., red maple, SPCD 316)
  may have different coefficients in NE, LS, CS, and SN variants.
- **Maintenance burden**: Updating one equation (e.g., mortality) requires
  separate fitting and testing across 22 variants.
- **No climate sensitivity**: Variant boundaries are fixed; they cannot respond
  to shifting species ranges or changing climate.

## 2. Proposed Solution

A single CONUS variant with:

- **Nationally fit equations** using Bayesian hierarchical models
- **Species random effects** that capture species-specific growth behavior
- **Ecoregion-level partial pooling** to capture regional variation without
  hard boundaries
- **Continuous climate covariates** replacing discrete variant assignments
- **Dynamic species handling** via FIA SPCD codes

## 3. Equation Specifications

### 3.1 Diameter Growth

Current: Wykoff (1982) ln(DDS) model, 13 covariates, species random intercept.

Proposed CONUS extension:

```
ln(DDS) = beta_0 + b_0[species] + b_0[ecoregion] +
          beta_1 * ln(DBH) + beta_2 * DBH^2 +
          beta_3 * BAL + beta_4 * ln(BA + 1) +
          beta_5 * CR + beta_6 * SI_climate +
          beta_7 * SLOPE + beta_8 * cos(ASPECT) * SLOPE +
          beta_9 * sin(ASPECT) * SLOPE +
          beta_10 * ELEV +
          beta_11 * MAT + beta_12 * CMD +
          epsilon
```

Key changes from current per-variant fits:
- Add `b_0[ecoregion]` random intercept (Bailey's ecoregion divisions)
- Replace variant-specific SI with `SI_climate` (ClimateNA site index)
- Add explicit climate variables: MAT (mean annual temperature),
  CMD (climatic moisture deficit)
- Fit to ALL FIA remeasurement data simultaneously (~4.5M trees)

### 3.2 Mortality

Current: Logistic survival with species RE + competition.

Proposed CONUS extension:

```
logit(P_survive) = gamma_0 + g_0[species] + g_0[ecoregion] +
                   gamma_1 * DBH + gamma_2 * DBH^2 +
                   gamma_3 * BAL/BA + gamma_4 * RDI +
                   gamma_5 * CR +
                   gamma_6 * drought_index
```

Key changes:
- Add `g_0[ecoregion]` random intercept
- Replace threshold-based SDIMAX mortality with continuous RDI effect
- Add drought index as explicit climate driver
- Eliminate the binary onset/slope SDIMAX modifier

### 3.3 Height-Diameter

Current: Chapman-Richards with species RE.

Proposed CONUS extension:

```
HT = 4.5 + a[species, ecoregion] * (1 - exp(-b * DBH))^c
```

Key changes:
- Crossed random effects: species x ecoregion on asymptote parameter `a`
- Enables smooth interpolation between regions for the same species

### 3.4 Crown Ratio Change

Current: Linear with species RE (weakest component).

Proposed: Explore nonlinear alternatives or empirical lookup tables. Crown
ratio modeling is the weakest link in FVS and may warrant a fundamentally
different approach in CONUS (e.g., allometric crown models from lidar data).

### 3.5 Volume and Biomass

Use NSBE/VTECO equations (Westfall et al. 2024) which are already national
in scope and species x ecodivision specific. These are production ready
and implemented in `calibration/R/20_volume_equations.R`.

### 3.6 Site Productivity

Replace variant-specific site index curves with ClimateNA-derived site
productivity index. This provides continuous, climate-sensitive site
quality estimates across CONUS.

### 3.7 Ingrowth / Regeneration

The most challenging component. Options:

a) **Empirical FIA-based**: Use observed ingrowth rates by species group
   and stand condition. Simple, defensible, but not climate-sensitive.

b) **Establishment model**: Climate niche models (species distribution
   models) combined with gap/disturbance detection.

c) **Hybrid**: Empirical rates modulated by climate suitability scores.

Recommendation: Start with (a) for initial release, develop (c) as a
research priority.

## 4. Data Requirements

| Dataset | Source | Current Status |
|---------|--------|----------------|
| FIA remeasurement | USDA FIA DataMart | Available (9 GB processed) |
| ClimateNA normals | ClimateNA v7.40 | Available (raster) |
| Emmerson SDIMAX | Emmerson et al. | Available (raster) |
| Bailey's ecoregions | USFS | Available (shapefile) |
| Soil data | gSSURGO | Not yet integrated |
| Elevation/topo | SRTM 30m | Derived from FIA ELEV |

## 5. Implementation Strategy

### Phase 1: R Prototype
Refit all equations nationally in R using brms/CmdStan. Validate against
the existing per-variant fits using the FIA benchmark engine (script 19).
This is the primary deliverable for the current calibration paper.

### Phase 2: Fortran Integration
Translate the R-fitted coefficients into FVS Fortran subroutines.
Key decision: whether to create new Fortran source files (conus/*.f90)
or extend the existing base/ routines with a CONUS mode flag.

Recommendation: New source files in `src-converted/conus/` that override
the base routines, following the existing variant architecture. This
preserves backward compatibility with all 22 existing variants.

### Phase 3: Standalone Executable
Build FVS-CONUS as a standalone executable alongside the existing
variant executables. Users select FVS-CONUS when they want nationally
consistent projections without variant boundary effects.

## 6. Validation Framework

1. **Within-sample**: Cross-validated RMSE, R-squared, bias by ecoregion
2. **Out-of-sample**: Hold-out 20% of FIA plots for independent validation
3. **Variant boundary test**: Compare projections for plots near variant
   borders processed by (a) assigned variant, (b) adjacent variant,
   (c) FVS-CONUS
4. **Long-term realism**: 50-year projections checked against ecological
   bounds (Reineke SDI limits, expected mortality patterns)
5. **Species range edges**: Test performance for species at range margins
   where variant-specific fits have limited data

## 7. Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| National fit loses regional specificity | Medium | Ecoregion random effects + validation |
| Computational cost of fitting to 4.5M trees | High | ADVI initialization + targeted HMC |
| NSBE coverage gaps for rare species | Low | Jenkins fallback in NSBE hierarchy |
| User resistance to new variant | Medium | Maintain all 22 variants; CONUS is additive |
| Climate data availability on user machines | Medium | Bundled lookup tables, optional NetCDF |

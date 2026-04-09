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

## 3. Design Principles

FVS-CONUS departs from the traditional FVS equation architecture in three
fundamental ways:

1. **Annualized increment**: All growth equations predict annual increment
   rather than period increment. Variable FIA remeasurement intervals (5 to
   10+ years) are standardized to annual rates during fitting. This eliminates
   the measurement-period bias embedded in current Wykoff-style DDS models and
   simplifies projection logic (no period-length adjustment needed).

2. **Biologically consistent equation forms**: The Wykoff (1982) ln(DDS)
   model is replaced with ORGANON-family equation forms (Zumrawi & Hann 1993
   and descendants). These forms enforce proper biological behavior at
   diameter extremes, produce well-behaved predictions for small and large
   trees, and avoid the log-scale back-transformation issues that plagued
   the 22-variant calibration (Baskerville correction inflating predictions
   24 to 86%).

3. **Bayesian hierarchical fitting**: All equations are fit nationally with
   species and ecoregion random effects, enabling smooth partial pooling
   across geography. No hard boundaries.

## 4. Equation Specifications

### 4.1 Diameter Growth

Current (22-variant): Wykoff (1982) ln(DDS) model, 13 covariates, species
random intercept, variable-interval.

Proposed CONUS form (ORGANON-family, annualized):

```
delta_dbh = exp( beta_0 + b_0[species] + b_0[ecoregion(species)]
               + beta_1 * log((dbh + 1)^beta_3 / (cr * ht + 1)^beta_4)
               + beta_2 * (bal^beta_5 / (dbh + 2.7))
               + beta_6 * elev
               + beta_7 * EMT )
```

where `delta_dbh` is annual diameter increment (inches/year), `dbh` is
diameter at breast height, `cr` is crown ratio (0 to 1), `ht` is total
height, `bal` is basal area in larger trees, `elev` is elevation, and
`EMT` is extreme minimum temperature.

This form (Zumrawi & Hann 1993, as adapted by Johnson/Marshall) has been
fitted to 84 species across CONUS with >= 5,000 FIA observations each
(3.48M total records). Key advantages over Wykoff:

- Exponentiated form guarantees non-negative diameter growth
- Crown ratio and height enter as a ratio, capturing the competitive
  status of the live crown relative to tree size
- BAL effect is scaled by diameter, reflecting size-dependent competition
- No log-scale back-transformation needed (avoids Baskerville bias)
- Proper asymptotic behavior: growth approaches zero for very large trees
  and trees with minimal crown

Candidate extensions under evaluation:

- Additional climate covariates (MAT, CMD, growing degree days) beyond
  the current elevation + EMT approach
- ClimateSI as a continuous productivity proxy replacing site index
- Soil available water capacity from gSSURGO
- Slope/aspect interaction terms
- Nonlinear random slope terms on key parameters (e.g., BAL effect
  varying by ecoregion within species)

Annualization approach:

- Use ALL available remeasurement combinations from FIA (5-year, 10-year,
  and any other intervals that arise from the survey design)
- Annualize observed increment: delta_dbh_annual = delta_dbh_obs / interval
- Fitting on the full range of intervals improves coverage of growth
  conditions and avoids discarding valuable long-interval data
- Projection uses the annualized rate directly; no period-length adjustment

Random effects structure (nested, not crossed):

- Species as the top-level grouping (84+ species)
- Ecoregion nested within species: `b_0[ecoregion(species)]`
- This is more biologically defensible than crossed effects because
  a species' shade tolerance and growth strategy constrain how much
  ecoregion can shift its growth rate. A shade-tolerant species won't
  suddenly become fast-growing just because it's in a productive ecoregion.
- Reduces parameter count substantially (no species x ecoregion cross
  for every combination), improving computational tractability
- Species-level random slopes on competition terms (BAL effect) where
  data support it
- ADVI initialization followed by targeted HMC on stratified subsample

### 4.2 Mortality

Current (22-variant): Logistic survival with species RE + competition,
threshold-based SDIMAX mortality modifier (onset at RDI=0.55 to 0.70).

Proposed CONUS form (annualized survival probability):

```
logit(P_survive_annual) = gamma_0 + g_0[species] + g_0[ecoregion(species)]
                        + gamma_1 * DBH + gamma_2 * DBH^2
                        + gamma_3 * (BAL / BA)
                        + gamma_4 * RDI
                        + gamma_5 * CR
                        + gamma_6 * drought_index
```

Key changes from current architecture:

- Annualized: Predict annual survival probability, compound for projection
  period. Eliminates the variable-interval mortality adjustment.
- Continuous RDI effect replaces threshold-based SDIMAX modifier. The
  hard onset/slope modifier (v2: onset=0.55, slope=2.0; v3: onset=0.70,
  slope=0.5) is eliminated in favor of a smooth density-dependent term.
- Explicit drought index as climate driver for background mortality.
- Ecoregion random effects capture regional mortality patterns (e.g.,
  higher density-independent mortality in fire-prone western ecoregions).

### 4.3 Height-Diameter

Current (22-variant): Chapman-Richards with species RE.

Proposed CONUS form:

```
HT = 4.5 + a[species] * a_eco[ecoregion(species)] * (1 - exp(-b * DBH))^c
```

Key changes:

- Nested random effects: species-level asymptote `a[species]` with
  ecoregion modifier `a_eco[ecoregion(species)]` nested within species
- Consistent with the nested RE philosophy across all CONUS equations
- Enables smooth interpolation for the same species across regions
- Consider ORGANON-style height-diameter form as alternative:
  `HT = 4.5 + exp(a + b / (DBH + c))` with species/ecoregion RE on `a`

### 4.4 Height Growth

Current (22-variant): Derived from height-diameter relationship at
projected diameter.

Proposed CONUS: Direct annual height increment model, fitted independently
of the height-diameter relationship. This decouples height growth from
diameter growth, allowing trees to adjust their height-diameter allometry
over time in response to competition and environment.

### 4.5 Crown Ratio

Current (22-variant): Linear model with species RE (R-squared 0.13 to 0.38,
weakest component).

Proposed CONUS: Fundamental redesign. Three candidate approaches:

a) **Allometric crown models** from airborne lidar (GEDI, 3DEP). Crown
   dimensions predicted from tree size, stand density, and canopy structure.
b) **Nonlinear asymptotic form**: CR as a function of relative stand
   density, canopy position (BAL/BA), and species shade tolerance class.
c) **Crown competition factor (CCF)** as an alternative state variable,
   sidestepping direct crown ratio prediction.

The linear model used in the 22-variant architecture is inadequate. This
component is the highest priority for research in the CONUS development.

### 4.6 Volume and Biomass

Use NSBE/VTECO equations (Westfall et al. 2024) which are already national
in scope and species x ecodivision specific. These are production ready
and implemented in `calibration/R/20_volume_equations.R`. No changes
needed for CONUS.

### 4.7 Site Productivity

Replace variant-specific site index curves with a continuous productivity
surface. Candidates:

- ClimateSI (ClimateNA-derived site index, 99.6% FIA coverage in current
  pipeline)
- Climate and environmental variables directly (elevation, EMT, growing
  degree days, precipitation) as in the ORGANON DG form
- Embedded variables (PCA of climate + environment)
- Asymptotic above-ground biomass from remote sensing
- Hybrid: ClimateSI + explicit climate terms

Greg Johnson's preliminary CONUS fitting shows that elevation + EMT capture
much of the productivity gradient but do not fully resolve spatial
autocorrelation in residuals. Additional climate or remote sensing
covariates are likely needed.

### 4.8 Ingrowth / Regeneration

The most challenging component. Options:

a) **Empirical FIA-based**: Observed ingrowth rates by species group
   and stand condition. Simple, defensible, not climate-sensitive.
   Currently implemented in the 22-variant architecture.

b) **Establishment model**: Climate niche models (SDMs) combined with
   gap/disturbance detection.

c) **Hybrid**: Empirical rates modulated by climate suitability scores.

Recommendation: Start with (a) for initial release, develop (c) as a
research priority. Ingrowth will be annualized to match the growth/mortality
framework.

## 5. Data Requirements

| Dataset | Source | Current Status |
|---------|--------|----------------|
| FIA remeasurement | USDA FIA DataMart | 10.87M records pre-screening |
| FIA screened (>=5K obs/species) | Derived | 3.48M records, 84 species |
| ClimateNA normals | ClimateNA v7.40 | Available (raster) |
| Emmerson SDIMAX | Emmerson et al. | Available (raster) |
| Bailey's ecoregions | USFS | Available (shapefile) |
| Soil data | gSSURGO | Not yet integrated |
| Elevation/topo | SRTM 30m / FIA ELEV | Available |
| GEDI/3DEP lidar | NASA / USGS | For crown ratio redesign (not yet integrated) |

## 6. Implementation Strategy

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

## 7. Validation Framework

1. **Within-sample**: Cross-validated RMSE, R-squared, bias by ecoregion
2. **Out-of-sample**: Hold-out 20% of FIA plots for independent validation
3. **Variant boundary test**: Compare projections for plots near variant
   borders processed by (a) assigned variant, (b) adjacent variant,
   (c) FVS-CONUS
4. **Long-term realism**: 50-year projections checked against ecological
   bounds (Reineke SDI limits, expected mortality patterns)
5. **Species range edges**: Test performance for species at range margins
   where variant-specific fits have limited data

## 8. Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| National fit loses regional specificity | Medium | Ecoregion random effects + validation |
| Computational cost of fitting to 4.5M trees | High | ADVI initialization + targeted HMC |
| NSBE coverage gaps for rare species | Low | Jenkins fallback in NSBE hierarchy |
| User resistance to new variant | Medium | Maintain all 22 variants; CONUS is additive |
| Climate data availability on user machines | Medium | Bundled lookup tables, optional NetCDF |

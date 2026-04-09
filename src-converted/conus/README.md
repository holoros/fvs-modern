# FVS-CONUS: Continental United States Variant

## Overview

FVS-CONUS is a unified Forest Vegetation Simulator variant covering all forested
lands in the contiguous United States. Unlike the traditional FVS architecture,
which uses 22 geographically distinct US variants (each with separate equations,
species lists, and parameterizations), FVS-CONUS uses a single set of nationally
calibrated equations with continuous spatial covariates.

## Motivation

The current 22-variant architecture introduces hard boundaries between regions.
A plot near the border of the PN and WC variants may receive substantially
different projections depending on which variant processes it. FVS-CONUS
eliminates these boundary artifacts by:

1. Using continuous climate and site variables instead of discrete variant assignments
2. Fitting species-level equations across the full range of each species
3. Replacing variant-specific species codes with a unified FIA SPCD-based system
4. Incorporating spatially explicit covariates (climate normals, soil, topography)

## Architecture

### Equations (new development)

All equations are fit to FIA remeasurement data nationally, using Bayesian
hierarchical models with species random effects and spatial covariates.

| Component | Form | Status |
|-----------|------|--------|
| Diameter growth | Wykoff ln(DDS) with climate covariates | Calibrated (v3) |
| Mortality | Logistic survival with competition + climate | Calibrated (v3) |
| Height-diameter | Chapman-Richards with species RE | Calibrated (v3) |
| Crown ratio change | Linear with species RE | Calibrated (v2) |
| Height growth | TBD: likely Hossfeld IV or Richards | Planned |
| Ingrowth | TBD: mechanistic vs. empirical | Planned |
| Volume/Biomass | NSBE/VTECO equations (Westfall et al. 2024) | Production ready |
| Site productivity | ClimateSI raster (ClimateNA-based) | Available |
| Max stand density | Emmerson SDIMAX raster | Available |

### Species System

FVS-CONUS uses FIA SPCD (4-digit species codes) directly instead of the variant-
specific sequential integer codes used by traditional FVS variants. The species
mapping is maintained in `data/NSBE/REF_SPECIES.csv` which provides:

- FIA SPCD to common/scientific name mapping
- Jenkins species group codes for biomass fallback
- PLANTS code cross-reference
- Genus-level grouping for hierarchical models

### Key Design Decisions

1. **No COMMON block species arrays**: Traditional FVS uses fixed-size Fortran
   COMMON blocks with variant-specific MAXSP. FVS-CONUS dynamically allocates
   species arrays based on the species present in each stand.

2. **Climate as explicit input**: Rather than relying on geographic location to
   determine a variant (and its embedded climate assumptions), FVS-CONUS takes
   climate normal grids as direct inputs.

3. **Bayesian posterior-driven uncertainty**: All projections can optionally
   propagate parameter uncertainty through posterior draws, producing credible
   intervals on every output metric.

4. **Backward compatibility**: FVS-CONUS can ingest standard FVS StandInit/
   TreeInit databases and produce output in the same format as traditional
   variants.

## Relationship to Current Calibration

The `calibration/` directory contains the Bayesian calibration pipeline that
produces the equation parameters used by FVS-CONUS. The key scripts are:

- `02_fit_diameter_growth.R`: Wykoff ln(DDS) with 13 covariates + species RE
- `03_fit_height_diameter.R`: Chapman-Richards with species RE
- `04_fit_mortality.R`: Logistic survival with competition indices
- `05_fit_crown_ratio.R`: Linear crown ratio change model
- `06_posterior_to_json.R`: Export posteriors for Fortran/R consumption

These are currently fit per-variant. The CONUS variant will use a single
national fit with ecoregion/ecodivision-level hierarchical structure.

## Development Roadmap

### Phase 1: National equation refitting (Q2 2026)
- Refit diameter growth across all FIA data with ecoregion random effects
- Refit mortality with national climate covariates
- Refit H-D with latitude/longitude continuous trends
- Validate against variant-specific fits

### Phase 2: Fortran implementation (Q3 2026)
- Implement dynamic species array allocation
- Integrate climate normal reader (NetCDF or raster)
- Wire NSBE volume equations into Fortran tree processing loop
- Build/test standalone FVS-CONUS executable

### Phase 3: Validation and comparison (Q4 2026)
- Compare FVS-CONUS vs. traditional variants on FIA benchmarks
- Boundary discontinuity analysis
- Long-term (50-year) projection realism checks
- GMUG and user community feedback

### Phase 4: Integration and release (2027)
- Merge into main fvs-modern build system
- User documentation and tutorials
- Docker/cloud deployment support
- Potential submission to USDA Forest Service for adoption

# Known Issues

## Regression Test Failures

### iet03 segfault (Inland Empire variant, test 03)

The iet03 regression test produces a segmentation fault during execution.
This is a pre-existing issue inherited from the upstream USDA FVS source
(Open-FVS revision 3360). The segfault occurs in the establishment extension
module under specific stand initialization conditions. All other 66 regression
tests pass (98.5% pass rate).

**Status**: Known, not yet investigated. Does not affect core growth/mortality
projections or the Bayesian calibration pipeline.

**Workaround**: Avoid the specific stand initialization configuration used in
iet03. Normal FVS runs with standard StandInit/TreeInit inputs are unaffected.

## Calibration Pipeline

### Crown ratio model performance

The crown ratio change model (script 05/05b) has the lowest predictive
performance among calibrated components, with R-squared values ranging from
0.13 to 0.38 across variants. This is consistent with the difficulty of
predicting crown dynamics from standard inventory variables. Crown ratio
predictions should be interpreted with appropriate uncertainty bounds.

**Planned improvement**: Explore lidar-derived crown allometrics and nonlinear
model forms in the CONUS variant development (branch: `conus-variant`).

### Ingrowth model is empirical only

The ingrowth component uses observed FIA recruitment rates by variant rather
than a mechanistic regeneration model. This is adequate for short-term (5 to
15 year) projections but may not capture long-term regeneration dynamics or
climate-driven species composition changes.

**Planned improvement**: Develop climate-sensitive ingrowth submodel for
the CONUS variant.

### ClimateSI and SDIMAX raster coverage

The `plot_raster_lookup.csv` file provides climate site index (ClimateNA-based)
and Emmerson SDIMAX values for FIA plot locations. Coverage is:

- ClimateSI: 99.6% of FIA conditions matched
- SDIMAX: 69.0% of FIA conditions matched

Conditions without raster matches fall back to FIA SICOND (for site index)
or default variant-level SDIMAX values.

## Build System

### macOS compilation

On Apple Silicon Macs, the Homebrew gfortran installation may require explicit
library path configuration. See `deployment/scripts/setup_macos.sh` for the
recommended setup procedure.

### Windows native builds

FVS-modern is not tested on native Windows. Windows users should use WSL2
(see `deployment/scripts/setup_wsl.sh`) or Docker.

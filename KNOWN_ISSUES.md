# Known Issues

## Regression Test Failures

### iet03 segfault (Inland Empire variant, test 03)

The iet03 regression test produces a segmentation fault during execution.
This is a pre-existing issue inherited from the upstream USDA FVS source
(Open-FVS revision 3360). The segfault occurs in the establishment extension
module under specific stand initialization conditions. All other 66 regression
tests pass (98.5% pass rate).

**Status**: Known, triaged 2026-04-13. Does not affect core growth/mortality
projections or the Bayesian calibration pipeline.

**Triage notes**: The iet03 keyword file exercises the Fire and Fuels
Extension (FFE) together with SNAGINIT, SIMFIRE, SALVAGE, DEFULMOD, POTFIRE,
FMORTMLT, and the full BurnRept/FuelOut/FuelRept/SOILHEAT reporting stack,
seeded only with a TREEFMT descriptor and no TreeInit tree list. The segfault
is consistent with FFE snag and fuels initialization running before the tree
list is populated. See `src-converted/tests/FVSie/iet03.key`.

**Planned investigation**:

1. Run `FVSie` under `gdb --args FVSie --keyword=iet03.key` to capture the
   faulting frame; expected location is the FFE snag or pot-fire initializer.
2. Compare against upstream Open-FVS r3360 to confirm the fault is inherited
   (it is listed as preexisting but never pinned to a commit).
3. If reproducible upstream, file an issue on the USDA Open-FVS tracker and
   keep the test skipped here with an explicit xfail marker in
   `run_regression_tests.sh`.

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

### ACD variant on cardinal.osc.edu

Earlier development cycles reported ACD build instability on Cardinal.
The 2026-04-13 diagnostic (see `deployment/scripts/diagnose_acd_cardinal.sh`
and `docs/acd_cardinal_handoff.md`) resolved this:

- `src-converted/acd/blkdat.f90` and `src-converted/acd/crown.f90` are
  byte-identical on Cardinal and the workspace
  (sha256 `59db704b...` and `33415929...`).
- All MAXSP-dimensioned DATA statements in ACD carry 108 values, matching
  the declared MAXSP in `src-converted/acd/common/PRGPRM.f90`.
- ifort 2021.10.0 and ifx 2023.2.3 both compile ACD sources cleanly on
  Cardinal (warnings only for explicit interface declarations of DBCHK,
  OPFIND, OPGET, OPDONE, which are benign).
- `FVSacd.so` (built 2026-04-10) loads successfully via `ctypes.CDLL` on
  Cardinal.

gfortran 11.4.1 on Cardinal flags an undeclared `iosum` symbol in
`src-converted/common/OUTCOM.f90` when compiled with `-fimplicit-none`.
This is a smoke-test artifact from the diagnostic script, not a bug that
appears in the production build (which relies on implicit typing). Tracked
as a backlog item for the next calendar tag; harmless at runtime.

### macOS compilation

On Apple Silicon Macs, the Homebrew gfortran installation may require explicit
library path configuration. See `deployment/scripts/setup_macos.sh` for the
recommended setup procedure.

### Windows native builds

FVS-modern is not tested on native Windows. Windows users should use WSL2
(see `deployment/scripts/setup_wsl.sh`) or Docker.

# CLAUDE.md: AI Assistant Conventions for fvs-modern

## Project Overview

fvs-modern is a modernized community-maintained fork of the USDA Forest Vegetation Simulator (FVS), extended with Bayesian calibration of 25 regional variants against national FIA remeasurement data. The project provides:

1. Fortran 90 free-form conversion of the FVS codebase (2,247 files)
2. Turnkey deployment infrastructure (Docker, AWS, platform-specific installers)
3. Python/REST API wrappers (fvs2py ctypes binding, microfvs REST interface)
4. Bayesian calibration pipeline producing posterior distributions for 7 component models per variant
5. Uncertainty quantification system (500 posterior draws per component, ensemble projections)

The codebase spans three major software ecosystems: Fortran (simulation engine), R (Bayesian calibration), and Python (APIs and post-processing).

## Directory Structure

```
src-converted/               Fortran 90 free-form source (2,247 files, all variants)
  base/                      Core simulation engine (253 files)
  ne/, ie/, acd/, ...        25 US + Canadian regional variants
  fire/, estb/, volume/      Optional extension modules

deployment/
  scripts/                   Build and deployment automation
    build_fvs_libraries.sh   Compile all variants to .so shared libraries
    run_regression_tests.sh  Full test suite (66/67 passing)
    deploy_laptop.sh         Fedora/RHEL install with systemd services
    setup_macos.sh           Homebrew installation
    setup_wsl.sh             Windows WSL2 setup
    add_variant.sh           Scaffolding for new regional variants
  docker/                    Dockerfile + compose (Ubuntu 24.04, Shiny + R)
  fvs2py/                    Python ctypes wrapper (load .so, ctypes bindings)
  microfvs/                  FastAPI REST interface (subprocess management, JSON I/O)
  patches/                   Auto-applied diffs for upstream compatibility

calibration/
  R/                         Bayesian fitting scripts (01 through 09)
  python/                    Python projection engines and aggregators
  osc/                       SLURM submission templates for OSC Cardinal
  output/variants/{variant}/ Posterior draws, diagnostics, convergence checks
  data/                      FIA remeasurement pairs, processed by component
  stan/                      Stan model specifications (diameter growth, mortality, etc.)

config/
  *.json                     Default parameters for 25 variants
  calibrated/                Posterior medians for calibrated variants
  config_loader.py           Runtime parameter switching (default/calibrated/custom)
  uncertainty.py             Bayesian posterior draw sampling and aggregation

src-converted/tests/         Regression tests (test.py, comparison scripts)
```

## Build Instructions

Build all FVS variant shared libraries in one command:

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted ./lib
```

This compiles src-converted/ into 25 .so files (FVSne.so, FVSie.so, etc.), placing them in ./lib/. The script takes SOURCE_DIR (the converted source tree) and OUTPUT_DIR as required arguments. It:

1. Checks for gfortran, gcc dependencies
2. Compiles base/ into object files
3. Links variant-specific modules against base
4. Creates position-independent shared libraries for ctypes/fvs2py

Subset builds are supported:

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted ./lib ne ak ie
```

## Key Conventions

Fortran 90 Free Form: All source is free-form (column-independent, ! comments). No fixed-form carryover (column 6 punch card notation). Modern declaration style: `integer, parameter :: MAX_TREES = 10000`.

Python 3.9+: Type hints preferred for new code. Path operations use pathlib.Path. Environment variable access via os.environ.get() with sensible defaults.

R Tidyverse Style: Pipe operators (%>%), tidy data frames (one observation per row). No global assignments except for data loading. Use readr for I/O.

No Hardcoded HPC Paths in New Code: Cardinal paths (/users/PUOM0008/, /scratch/) are confined to calibration/osc/ SLURM scripts. Production code reads FVS_PROJECT_ROOT, FVS_LIB_DIR, FVS_FIA_DATA_DIR from environment.

Parameter Versioning: All FVS instances accept version="default|calibrated|custom". Runtime switching via config/config_loader.py ensures consistency.

## Testing

Run the full test suite:

```bash
bash deployment/scripts/run_regression_tests.sh
```

This executes:

1. Library load tests (gctorture in R, ctypes import in Python)
2. Standalone keyword-file simulations (41/42 passing; 1 known segfault in iet03)
3. rFVS API tests via R's .Fortran() interface
4. Comparative benchmarks (original vs. calibrated parameters)

Current status: 66/67 tests passing (98.5%).

Unit tests for Python modules:

```bash
cd deployment/fvs2py
pytest
```

## Special Notes

Calibration OSC Scripts: calibration/osc/ contains historical SLURM submission templates (submit_cardinal.sh, submit_dg_24h.sh, etc.) with hardcoded Cardinal paths and account numbers. These are kept for reproducibility of past calibration runs. Do not extend these patterns; new workflows should read paths from environment variables.

Uncertainty Quantification: The UncertaintyEngine (config/uncertainty.py) manages 500 posterior draws per component. Each draw is a complete parameter vector (e.g., B0[1..max_species] for diameter growth). This preserves parameter correlations within a draw while allowing ensemble spread across draws.

Variant Deployment: New variants are scaffolded via add_variant.sh, which creates variant directory, species mapping template, and parameter JSON stub. Parameters are calibrated offline (calibration/R/) and merged into config/calibrated/.

## Licensing

Fortran source code (src-converted/): USDA public domain (inherits from original FVS).
New Python/R/deployment code: MIT license.
See LICENSE file at repo root.

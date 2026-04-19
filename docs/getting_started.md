# Getting Started with fvs-modern

This walkthrough takes you from a fresh clone to running your first FVS projection in about 15 minutes. You will build the Northeast (NE) variant as a shared library, verify it loads, and then drive a projection from Python.

## Prerequisites

You need `gfortran`, `gcc`, and Python 3.9+. On Fedora/RHEL: `sudo dnf install gcc-gfortran gcc python3`. On Ubuntu/Debian: `sudo apt install gfortran gcc python3`. On macOS: `brew install gcc python`.

## Step 1: Clone the repository

```bash
git clone https://github.com/holoros/fvs-modern.git
cd fvs-modern
```

## Step 2: Build the NE variant

The build script compiles the Fortran source into a shared library (.so). Building a single variant takes about 2 minutes.

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted lib ne
```

Expected output:

```
================================================================
FVS Shared Library Builder
================================================================
Source:   src-converted
Output:   lib
Variants: ne
...
[ne] Building FVSne.so ... DONE (487 objects, 12 skipped)
================================================================
```

Verify the library was created:

```bash
ls -lh lib/FVSne.so
# -rwxr-xr-x 1 user user 5.7M ... lib/FVSne.so
```

## Step 3: Verify the library loads

The .so is loaded at runtime via Python ctypes (fvs2py) or R's `.Fortran()` interface (rFVS). Verify the API symbols are present:

```bash
python3 -c "
import ctypes, os
so = ctypes.CDLL(os.path.join('lib', 'FVSne.so'), mode=os.RTLD_LAZY)
print('FVSne.so loaded successfully')
print('API symbols present:')
print('  fvssetcmdline_:', so.fvssetcmdline_)
print('  fvssummary_:   ', so.fvssummary_)
print('  fvsdimsizes_:  ', so.fvsdimsizes_)
"
```

You should see all three symbols resolve without error.

## Step 4: Run a projection via fvs2py

The fvs2py wrapper provides a high level Python interface to FVS. From the repository root:

```bash
python3 -c "
import sys; sys.path.insert(0, 'deployment/fvs2py')
from fvs2py import FVS

fvs = FVS('lib/FVSne.so')
print('FVS-NE loaded via fvs2py')
print('Available methods:', [m for m in dir(fvs) if not m.startswith('_')])
"
```

For a full projection with a keyword file:

```python
import sys; sys.path.insert(0, 'deployment/fvs2py')
from fvs2py import FVS

fvs = FVS('lib/FVSne.so')
fvs.set_cmdline('--keywordfile=my_stand.key')
fvs.run()

# Access results
summary = fvs.summary          # pandas DataFrame: year, TPA, BA, volume
trees   = fvs.tree_attributes  # per-tree DBH, height, crown ratio
```

See `deployment/fvs2py/` for the full API documentation and example keyword files.

## Step 5: Deploy the web interface (optional)

For the full FVS-Online Shiny web interface with interactive projection tools:

```bash
# Docker (recommended)
cd deployment/docker && docker compose up --build
# FVS-Online available at http://localhost:3838

# Or install directly on Fedora/RHEL
bash deployment/scripts/deploy_laptop.sh
```

## Step 6: Build all US variants (optional)

To build all 22 US variants plus the 2 Canadian variants:

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted lib
```

This takes 10 to 20 minutes depending on your machine. The output will show each variant being compiled with an object count.

## Step 7: Use calibrated parameters (optional)

fvs-modern includes Bayesian calibrated parameters for all 25 variants, derived from FIA remeasurement data. To use them in a projection, reference the calibrated config:

```bash
# Point estimates (median of posterior)
ls config/calibrated/ne_calibrated.json

# The calibration pipeline is in calibration/R/ (Stan models)
# and calibration/python/ (FVS projection drivers)
```

See `CALIBRATION.md` for full details on the Bayesian pipeline.

## Next steps

- **REST API:** See `deployment/microfvs/` for the FastAPI wrapper with Swagger docs
- **Run the test suite:** `bash deployment/scripts/run_regression_tests.sh` (requires rFVS; 66/67 tests pass)
- **Create a new variant:** `bash deployment/scripts/add_variant.sh --name me --base ne`
- **Report issues:** https://github.com/holoros/fvs-modern/issues

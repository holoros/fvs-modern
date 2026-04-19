# Getting Started with fvs-modern

This walkthrough takes you from a fresh clone to running your first FVS projection in about 15 minutes. You will build the Northeast (NE) variant, run a standalone simulation, and then load the library from Python using fvs2py.

## Prerequisites

You need `gfortran`, `gcc`, and `make` installed. On Fedora/RHEL: `sudo dnf install gcc-gfortran gcc make`. On Ubuntu/Debian: `sudo apt install gfortran gcc make`. On macOS: `brew install gcc`.

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
Compiler: GNU Fortran (GCC) 12.3.1 ...
================================================================

[ne] Building FVSne.so ... DONE (487 objects, 12 skipped)
...
```

Verify the library was created:

```bash
ls -lh lib/FVSne.so
# -rwxr-xr-x 1 user user 9.6M ... lib/FVSne.so
```

## Step 3: Run a standalone simulation

The test suite includes keyword files with saved baselines. Run the NE test case:

```bash
cd src-converted/tests/FVSne

# Build the standalone executable first (one-time)
cd ../../../
gfortran -o lib/FVSne src-converted/base/main.f90 \
  -Llib -l:FVSne.so -Wl,-rpath,$(pwd)/lib \
  -ffree-form -fPIC -std=legacy -w 2>/dev/null

# Run the test
cd src-converted/tests/FVSne
../../../lib/FVSne --keywordfile=net01.key
```

Check the summary output:

```bash
head -20 net01.sum
```

You should see a stand summary table with columns for year, age, trees per acre, basal area, and volume. Compare against the saved baseline:

```bash
diff <(grep -E "^[0-9]{4}" net01.sum) <(grep -E "^[0-9]{4}" net01.sum.save)
# No output = exact match
```

## Step 4: Load from Python (fvs2py)

Return to the repository root and try the Python ctypes wrapper:

```bash
cd ../../..
python3 -c "
import ctypes, os

# Load the shared library
so = ctypes.CDLL(os.path.join('lib', 'FVSne.so'))

# Check a symbol is callable
print('FVSne loaded successfully')
print('fvs_ symbol:', so.fvs_)
"
```

For the full fvs2py API (which manages keyword files, tree lists, and summary extraction), see `deployment/fvs2py/`.

## Step 5: Build all US variants (optional)

To build all 22 US variants plus the 2 Canadian variants:

```bash
bash deployment/scripts/build_fvs_libraries.sh src-converted lib
```

This takes 10 to 20 minutes depending on your machine. The output will show each variant being compiled with an object count.

## Step 6: Use calibrated parameters (optional)

fvs-modern includes Bayesian calibrated parameters for all 25 variants, derived from FIA remeasurement data. To use them in a projection, reference the calibrated config:

```bash
# Point estimates (median of posterior)
ls config/calibrated/ne_calibrated.json

# The calibration pipeline is in calibration/R/ (Stan models)
# and calibration/python/ (FVS projection drivers)
```

See `CALIBRATION.md` for full details on the Bayesian pipeline.

## Next steps

- **FVS-Online web interface:** Run `bash deployment/scripts/deploy_laptop.sh` for the full Shiny deployment with rFVS
- **REST API:** See `deployment/microfvs/` for the FastAPI wrapper
- **Docker:** `cd deployment/docker && docker compose up --build` for a containerized deployment
- **Run the test suite:** `bash deployment/scripts/run_regression_tests.sh` (requires rFVS; 66/67 tests pass)
- **Create a new variant:** `bash deployment/scripts/add_variant.sh --name me --base ne`
- **Report issues:** https://github.com/holoros/fvs-modern/issues

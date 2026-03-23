# FVS GitHub Actions CI Configuration

## Overview

This directory contains the Continuous Integration (CI) workflow for the FVS (Forest Vegetation Simulator) codebase. The CI system automatically builds FVS and runs tests on every push and pull request, ensuring that code changes don't introduce regressions.

## What the CI Workflow Does

The GitHub Actions workflow (`fvs-build-test.yml`) performs the following steps:

### 1. Environment Setup
* Runs on `ubuntu-latest` (Ubuntu 22.04 LTS)
* Installs build dependencies:
  * gfortran (Fortran 77 compiler)
  * cmake (build configuration tool)
  * sqlite3 (database backend used by FVS)
* Configures the selected gfortran version (11 or 12)

### 2. Build Process
* Configures the FVS build with CMake
* Builds the selected FVS variant (default: NE - Northeastern)
* Compiles Fortran 77 fixed-form source files
* Links with SQLite3 library
* Generates executable (e.g., `FVSNE`)

### 3. Testing
* Runs existing test suite (legacy Python comparison tests in `tests/test.py`)
* Runs pFUnit unit tests (when available in `tests/fvs_tests`)
* Captures and archives test results

### 4. Artifacts
* Stores test results and build logs for later inspection
* Keeps artifacts for 30 days (configurable)

## Workflow Configuration

### File Location
```
.github/workflows/fvs-build-test.yml
```

### Key Parameters

#### Matrix Strategy
The workflow tests multiple configurations simultaneously:

* **gfortran-version**: [11, 12]
  * Tests with two major gfortran versions to catch compiler-specific issues
  * Can be extended to include gfortran 10, 13, etc.

* **fvs-variant**: [ne]
  * Currently only tests the NE (Northeastern) variant for speed
  * Can be expanded to [ne, ca, ak, ...] to test all variants
  * Each variant has its own growth equations and species coefficients

#### Environment Variables
None currently set; could be added for:
* Debug logging: `CMAKE_VERBOSE_MAKEFILE=ON`
* Optimization flags: `CMAKE_BUILD_TYPE=Release|Debug`
* Fortran compiler flags: custom `-ffixed-form -cpp` options

## Build Times

Typical build times on GitHub Actions runner:

* **NE variant**: ~2-3 minutes
  * Initial CMake configuration: ~30 seconds
  * Fortran compilation: ~90-120 seconds
  * Linking: ~30 seconds

* **All variants (ne, ca, ak, etc.)**: ~10-15 minutes
  * Each variant builds independently
  * Runs in parallel for all matrix combinations

* **With pFUnit tests**: add ~1-2 minutes
  * pFUnit preprocessing: ~30 seconds
  * Test compilation: ~60 seconds
  * Test execution: ~10-30 seconds

### Optimization
The workflow caches the CMake build directory:
```yaml
- uses: actions/cache@v3
  with:
    path: build
    key: fvs-build-${{ matrix.fvs-variant }}-gfortran-${{ matrix.gfortran-version }}-${{ github.sha }}
```

This cache is keyed by:
* FVS variant
* gfortran version
* Git commit hash (SHA)

Cache hit on re-run of same commit = ~30 seconds (quick sanity check).

## Adding New Variants to the Build Matrix

To test additional FVS variants (e.g., CA, AK, IM), modify the matrix strategy:

### Step 1: Update fvs-build-test.yml
```yaml
strategy:
  matrix:
    gfortran-version: [ 11, 12 ]
    fvs-variant: [ ne, ca, ak ]  # Add ca, ak, or other variants
```

### Step 2: Ensure CMakeLists.txt Supports Variant Selection
The CMake configuration must accept `-DFVS_VARIANT=<variant>`:
```cmake
if (NOT DEFINED FVS_VARIANT)
  set(FVS_VARIANT "ne")  # Default to NE
endif()
```

### Step 3: Verify Source Files Exist
Each variant must have its source list file:
```
bin/FVS<variant>_sourceList.txt
```

Example: `bin/FVSca_sourceList.txt` for California variant.

### Step 4: Commit and Push
Push changes to `.github/workflows/fvs-build-test.yml`. CI automatically uses the new configuration.

### Cost Consideration
* Each matrix combination is a separate job
* With 2 gfortran versions × 3 variants = 6 jobs per push
* GitHub provides 2,000 free CI minutes/month for public repos
* NE variant alone: ~3 min × 2 compiler × 30 pushes/month = 180 minutes

Add variants strategically; prioritize by usage frequency.

## Adding pFUnit Tests

pFUnit tests are automatically discovered and run if they exist:

### Step 1: Create tests/CMakeLists.txt

```cmake
find_package(PFUNIT REQUIRED)
enable_testing()

include_directories(
  ${CMAKE_SOURCE_DIR}/base
  ${CMAKE_SOURCE_DIR}/acd
  ${CMAKE_CURRENT_BINARY_DIR}
)

add_pfunit_executable(
    fvs_tests
    test_dense.pf
    test_dist.pf
    test_growth.pf
)

add_test(
    NAME FVSUnitTests
    COMMAND fvs_tests
)
```

### Step 2: Install pFUnit

The CI workflow assumes pFUnit is already installed on the runner. Add this step if needed:

```yaml
- name: Install pFUnit
  run: |
    git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
    cd pFUnit
    mkdir build && cd build
    cmake -DCMAKE_INSTALL_PREFIX=/usr/local/pfunit ..
    make -j$(nproc)
    sudo make install
    echo "export PATH=/usr/local/pfunit/bin:$PATH" >> $GITHUB_ENV
```

### Step 3: Add Test Files

Place .pf test files in `tests/`:
```
tests/
  test_dense.pf
  test_dist.pf
  test_growth.pf
```

### Step 4: Commit and Push

CI automatically discovers and runs tests in the next workflow run.

### Debugging Test Failures

If tests fail, check:

1. **Workflow run details**: GitHub Actions tab shows step-by-step logs
2. **Artifact artifacts**: Download test results and CMake logs
3. **Local reproduction**: Run same commands locally to debug

```bash
# Reproduce CI build locally
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j$(nproc)
ctest --output-on-failure
```

## Expected Build Times

A typical workflow run on GitHub Actions:

```
Job Setup & Dependencies:         ~1 min
  - Checkout code
  - Install gfortran, cmake, sqlite3
  - Set up gfortran symlinks

CMake Configure:                  ~30 sec
  - Parse CMakeLists.txt
  - Check compilers and libraries
  - Set up build rules

Fortran Compilation:              ~90-120 sec
  - Compile base/*.f files
  - Compile variant-specific *.f files
  - Compile C/C++ source (SQLite, FFE)

Linking:                          ~30 sec
  - Link shared libraries
  - Link main executable

Testing:                          ~1-2 min
  - Run legacy Python tests (if available)
  - Run pFUnit tests (if available)

Total per job:                    ~4-6 minutes
```

For 2 gfortran versions × 1 variant = 8-12 minutes total.

## Troubleshooting Build Failures

### Common Issues

#### 1. Fortran Compilation Error: "unexpected statement"
**Cause**: Fixed-form Fortran (.f) file has incorrect column positions
**Solution**: Check columns 1-6 (comment/label), ensure 6-72 are code

#### 2. Linker Error: "undefined reference to sqlite3_..."
**Cause**: libsqlite3-dev not installed or not linked
**Solution**: Ensure apt-get installs `libsqlite3-dev`

#### 3. CMake Error: "Could not find PFUNIT"
**Cause**: pFUnit not installed or not in PATH
**Solution**: Install pFUnit before running tests (see "Adding pFUnit Tests" section)

#### 4. Test Assertion Failure: "Expected X but got Y"
**Cause**: Code change broke expected behavior
**Solution**: Review diff in pull request; check if test expectation or code needs fixing

### Verbose Output

Enable verbose make output to see individual compilation commands:

```cmake
set (CMAKE_VERBOSE_MAKEFILE ON)
```

Or run make with `VERBOSE=1`:

```bash
make -j$(nproc) VERBOSE=1
```

The GitHub Actions workflow already includes `VERBOSE=1` in the build step.

## CI Integration with Pull Requests

### Checks Before Merge

GitHub can be configured to require CI checks before merging:

**Settings > Branches > Branch protection rule > Require status checks to pass**

Select:
* `FVS Build and Test (ne, gfortran-11)`
* `FVS Build and Test (ne, gfortran-12)`

This prevents merging code that breaks the build.

### Pull Request Workflow

1. Fork repository
2. Create branch: `git checkout -b feature/my-change`
3. Make changes to .f files
4. Commit and push to fork
5. Open Pull Request against `main` branch
6. GitHub automatically runs CI workflow
7. Monitor Actions tab for results
8. Fix any failures and push updates (CI runs again)
9. Once CI passes, maintainers can review and merge

## Extending CI for Multiple Operating Systems

Currently runs on `ubuntu-latest`. To add macOS and Windows:

```yaml
strategy:
  matrix:
    os: [ ubuntu-latest, macos-latest, windows-latest ]
    gfortran-version: [ 11, 12 ]
    fvs-variant: [ ne ]

runs-on: ${{ matrix.os }}
```

### OS-Specific Considerations

* **macOS**: Install gfortran via Homebrew; sqlite3 usually pre-installed
* **Windows**: Use MinGW or Visual Studio build tools; different path syntax

## Performance Optimization Tips

### Caching Build Artifacts
Currently cache CMake build directory. Could also cache:
* Compiled object files (`.o` files)
* Third-party dependencies (if pre-built)

### Parallel Compilation
Workflow already uses `-j$(nproc)` to use all available cores. GitHub runners typically have 2-4 cores.

### Incremental Builds
Only rebuild files that changed. Leverage CMake's dependency tracking.

## Monitoring CI Status

### Badge in README
Add CI status badge to project README:

```markdown
[![FVS Build Status](https://github.com/user/repo/workflows/FVS%20Build%20and%20Test/badge.svg)](https://github.com/user/repo/actions)
```

### Slack/Email Notifications
Configure GitHub Actions to notify on failure:

**Settings > Notifications > Workflow run notifications**

Select email address or integrate with Slack.

## Future Enhancements

### Short-term
1. Add CA and AK variants to matrix (currently only NE)
2. Install and run pFUnit tests
3. Add code coverage reporting (Gcov for Fortran)

### Medium-term
1. Add integration tests (multi-cycle simulations with validation)
2. Performance benchmarking (track compile/link time over commits)
3. Cross-platform testing (macOS, Windows MinGW)
4. Variant-specific coefficient validation

### Long-term
1. Automated refactoring checks (Fortran 77 → free-form migration)
2. Implicit typing detection and reporting
3. Memory leak detection (Valgrind)
4. Automated documentation generation from source

## References

* [pFUnit Documentation](https://github.com/Goddard-Fortran-Ecosystem/pFUnit/wiki)
* [GitHub Actions Documentation](https://docs.github.com/en/actions)
* [CMake Fortran Support](https://cmake.org/cmake/help/latest/language/Fortran/)
* [FVS User Guide](https://www.nrs.fs.fed.us/fvs/)

## Support

For questions about the CI workflow:

1. Check this README first
2. Review GitHub Actions logs for error messages
3. Consult FVS project maintainers
4. See pFUnit documentation for test framework questions

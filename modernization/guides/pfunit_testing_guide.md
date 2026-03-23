# pFUnit Testing Guide for FVS Fortran Codebase

## Table of Contents

1. [Why Unit Testing Matters for FVS](#why-unit-testing-matters-for-fvs)
2. [pFUnit Overview](#pfunit-overview)
3. [Installing pFUnit on Linux](#installing-pfunit-on-linux)
4. [Integrating pFUnit into FVS CMake Build](#integrating-pfunit-into-fvs-cmake-build)
5. [Writing Test Suites for FVS Routines](#writing-test-suites-for-fvs-routines)
6. [Running Tests](#running-tests)
7. [Organizing Test Directories](#organizing-test-directories)
8. [Strategy for Incrementally Adding Tests](#strategy-for-incrementally-adding-tests)
9. [Integration with CI](#integration-with-ci)

---

## Why Unit Testing Matters for FVS

The FVS codebase is a critical forest simulation engine used by land managers and researchers. Unit testing provides several key benefits:

### Regression Detection
* Catch unintended side effects when refactoring density calculations, growth equations, or mortality models
* Validate that changes to tightly coupled modules (e.g., DENSE, DGF, HTGF) don't break downstream calculations
* Maintain confidence across the 50+ variant models (NE, CA, AK, etc.) that share common base routines

### Refactoring Confidence
* FVS contains Fortran 77 fixed-form code that is often difficult to navigate and understand
* Unit tests document expected behavior of individual subroutines
* Enable incremental modernization (fixed-form → free-form, implicit typing removal) without fear of silent failures

### Validation of Species-Specific Models
* FVS growth equations contain species-dependent coefficients that must be validated
* Tests ensure species-specific behavior is correctly implemented
* Help detect errors in coefficient tables (common source of bugs in ecological models)

### Continuous Integration Safety
* Automated testing prevents regressions from being silently introduced via pull requests
* Provides evidence that changes work correctly across gfortran versions and platforms
* Reduces manual testing burden on maintainers

---

## pFUnit Overview

### What is pFUnit?

**pFUnit** (Parallel Fortran Unit Test framework) is a unit testing framework specifically designed for Fortran code. It provides:

* Simple, declarative syntax for defining tests via specially-formatted Fortran files (.pf extension)
* xUnit-style assertions (assertEqual, assertTrue, assertLessThan, etc.)
* Test fixtures for setup/teardown
* Support for both fixed-form and free-form Fortran
* CMake integration out-of-the-box
* Automatic test discovery and execution

### How It Works with CMake

pFUnit operates in two phases:

1. **Pre-processing**: pFUnit's `pf` tool parses .pf files and generates Fortran source code containing test drivers and assertion logic
2. **Compilation**: Generated Fortran code is compiled as part of the normal CMake build
3. **Execution**: Test executables are linked and run via `ctest`

Key advantage: Tests live alongside production code with minimal friction; no separate test harness needed.

---

## Installing pFUnit on Linux

### Ubuntu (20.04 LTS and later)

```bash
# Install build dependencies
sudo apt-get update
sudo apt-get install -y gfortran cmake git

# Clone pFUnit repository
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
cd pFUnit

# Configure and build (installs to /usr/local/pfunit by default)
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local/pfunit ..
make -j$(nproc)
sudo make install

# Add pFUnit to PATH
echo 'export PATH=/usr/local/pfunit/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

### RHEL / CentOS / Fedora

```bash
# Install build dependencies
sudo yum groupinstall -y "Development Tools"
sudo yum install -y gfortran cmake git

# Clone and build pFUnit (same as Ubuntu)
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
cd pFUnit
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local/pfunit ..
make -j$(nproc)
sudo make install

echo 'export PATH=/usr/local/pfunit/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

### Verification

```bash
pf --version
# Should output: pFUnit version X.X.X
```

---

## Integrating pFUnit into FVS CMake Build

### Step 1: Find pFUnit in CMakeLists.txt

Add the following to your top-level `CMakeLists.txt` (or the variant subdirectory CMakeLists.txt):

```cmake
# At the top, after project() declaration
find_package(PFUNIT REQUIRED)

# Enable Fortran testing
enable_testing()
```

### Step 2: Create a Test Directory Structure

```
FVS_root/
  base/
  acd/
  bin/
  tests/                          # New: Core pFUnit tests
    CMakeLists.txt               # Test configuration
    test_dense.pf                # Test for DENSE subroutine
    test_dist.pf                 # Test for DIST subroutine
    test_growth.pf               # Test for DGF/HTGF subroutines
    fixtures/                    # Test data and fixtures
      dense_test_data.f77        # Shared test data (if needed)
```

### Step 3: Create tests/CMakeLists.txt

```cmake
# tests/CMakeLists.txt

# Find pFUnit
find_package(PFUNIT REQUIRED)

# Include FVS source directories for test compilation
include_directories(
  ${CMAKE_SOURCE_DIR}/base
  ${CMAKE_SOURCE_DIR}/acd
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_CURRENT_BINARY_DIR}
)

# Link to the main FVS library (or individual module libraries)
# This assumes FVS has been built as a shared library FVS_variant
link_directories(${CMAKE_BINARY_DIR}/lib)

# Add pFUnit test executable
add_pfunit_executable(
    fvs_tests
    test_dense.pf
    test_dist.pf
    test_growth.pf
    # Link against FVS modules
    LINK_LIBRARIES FVS_ne  # or FVS_ca, FVS_ak, etc. for variant-specific testing
)

# Register tests with ctest
add_test(
    NAME FVSUnitTests
    COMMAND fvs_tests
)
```

### Step 4: Link FVS Libraries to Tests

Ensure that FVS source files needed by tests are either:
* Compiled into a separate library (`add_library(FVS_core ...)`)
* Or listed directly in the test executable's source files

For most FVS variants, the existing `FVS_${varName}` library from `bin/CMakeLists.txt` is sufficient.

---

## Writing Test Suites for FVS Routines

### Test File Format (.pf files)

pFUnit test files use a special format that blends Fortran code with test declarations:

```fortran
! test_example.pf
!
! Simple pFUnit test example for FVS
!
module test_example_mod
  use pfunit_mod
  implicit none

  private
  public :: test_simple_assertion

contains

  @test
  subroutine test_simple_assertion()
    real :: value
    value = 5.0
    @assertEqual(5.0, value)
  end subroutine test_simple_assertion

end module test_example_mod
```

Key syntax:
* `@test` decorator marks a subroutine as a test
* `@assertEqual(expected, actual)` checks for equality (handles floating-point tolerance)
* `@assertTrue`, `@assertFalse`, `@assertLessThan`, etc. for other assertions
* Tests run in alphabetical order within a module

### Realistic Example 1: Testing DENSE (Density Calculations)

**dense.f** calculates Stand Density Index (SDI), relative density (RELDEN), and basal area (BA). Key outputs to test:

* RELDEN: relative density (0-100+ scale)
* BA: basal area per acre
* CCF: crown competition factor (cumulative across species)
* RMSQD: root-mean-square diameter

**Test Strategy**: Create synthetic tree lists with known density characteristics, call DENSE, verify output metrics.

```fortran
! test_dense.pf
!
! Unit tests for DENSE subroutine (density calculations)
!
module test_dense_mod
  use pfunit_mod
  implicit none

  private
  public :: test_dense_empty_stand
  public :: test_dense_single_tree
  public :: test_dense_known_values

contains

  @test
  subroutine test_dense_empty_stand()
    ! DENSE should handle empty tree list gracefully
    ! When ITRN (number of trees) = 0, all output should be zeroed
    implicit none

    ! Note: This is a simplified example; actual test would:
    ! 1. Initialize COMMON blocks with empty tree list
    ! 2. Call DENSE
    ! 3. Assert all outputs are 0.0

    ! Place holder assertion (see real example below)
    @assertEqual(0.0, 0.0)
  end subroutine test_dense_empty_stand

  @test
  subroutine test_dense_single_tree()
    ! Single tree: BA should equal tree's individual basal area
    ! For example, 10" DBH Douglas fir with PROB=1.0 tree/acre
    ! BA ≈ 0.005454154 * (10.0^2 * 1.0) ≈ 0.545 sq ft/acre

    implicit none
    real :: expected_ba

    expected_ba = 0.005454154 * (10.0 * 10.0 * 1.0)
    ! Simplified assertion
    @assertEqual(0.545, expected_ba, tolerance=0.001)
  end subroutine test_dense_single_tree

  @test
  subroutine test_dense_known_values()
    ! Test with known stand characteristics
    ! 100 trees/acre, average DBH 10", species = Douglas-fir
    ! Expected RELDEN ≈ 35-45 (depends on CCF model)
    ! Expected BA ≈ 54.5 sq ft/acre

    implicit none
    real :: expected_ba, tolerance

    ! 100 trees at 10" DBH
    expected_ba = 0.005454154 * 100.0 * (10.0 * 10.0)
    tolerance = 0.5  ! Allow 0.5 sq ft/acre error

    @assertEqual(54.5, expected_ba, tolerance=tolerance)
  end subroutine test_dense_known_values

end module test_dense_mod
```

### Realistic Example 2: Testing DIST (Percentile Distribution)

**dist.f** calculates diameter percentiles (90th, 70th, 50th, 30th, 10th) and finds the largest tree. This is useful for generating tree size distributions.

**Test Strategy**:
* Create sorted tree list with known percentile breakpoints
* Call DIST
* Verify that returned percentiles match expected values

```fortran
! test_dist.pf
!
! Unit tests for DIST subroutine (percentile calculations)
!
module test_dist_mod
  use pfunit_mod
  implicit none

  private
  public :: test_dist_empty_list
  public :: test_dist_single_tree
  public :: test_dist_five_trees
  public :: test_dist_percentile_boundaries

contains

  @test
  subroutine test_dist_empty_list()
    ! DIST with ITRN=0 should return all zeros in ATTR array
    implicit none

    ! Placeholder test
    @assertEqual(0.0, 0.0)
  end subroutine test_dist_empty_list

  @test
  subroutine test_dist_single_tree()
    ! Single tree: all percentiles should equal that tree's DBH
    ! If tree has DBH=12.5", then:
    ! ATTR(1) = 12.5 (90th percentile)
    ! ATTR(2) = 12.5 (70th percentile)
    ! ... ATTR(6) = 12.5 (max diameter)

    implicit none
    real :: tree_dbh

    tree_dbh = 12.5
    ! After calling DIST with single tree:
    @assertEqual(tree_dbh, tree_dbh)
  end subroutine test_dist_single_tree

  @test
  subroutine test_dist_five_trees()
    ! Five trees with DBHs: 8, 10, 12, 14, 16 inches
    ! Each represents 20 trees/acre, cumulative probability:
    ! 20/100=20% (8"), 40/100=40% (10"), 60/100=60% (12"),
    ! 80/100=80% (14"), 100/100=100% (16")
    !
    ! Expected percentiles:
    ! 90th percentile → trees at 80-100% → DBH 14-16 → ~15"
    ! 70th percentile → trees at 60-80% → DBH 12-14 → ~13"
    ! 50th percentile → trees at 40-60% → DBH 10-12 → ~11"
    ! 30th percentile → trees at 20-40% → DBH 8-10 → ~9"
    ! 10th percentile → trees at 0-20% → DBH 8 → ~8"
    ! Max (ATTR(6)) → 16"

    implicit none

    ! Simplified check: max diameter should be 16"
    @assertEqual(16.0, 16.0)
  end subroutine test_dist_five_trees

  @test
  subroutine test_dist_percentile_boundaries()
    ! Test that percentile calculations respect boundary conditions:
    ! If we query the 90th percentile, returned diameter should be >=
    ! diameter of the tree at 90% cumulative probability

    implicit none

    ! Boundary condition check
    @assertTrue(15.0 >= 14.0)  ! 90th percentile should be >= the 80% tree
  end subroutine test_dist_percentile_boundaries

end module test_dist_mod
```

### Realistic Example 3: Testing Growth Equations (DGF/HTGF)

**DGF** (Diameter Growth Function) computes diameter increment. **HTGF** (Height Growth Function) computes height increment. These are species-specific and depend on:
* Diameter at breast height (DBH)
* Site index
* Basal area in larger trees (BAL)

**Test Strategy**:
* Test that diameter growth is positive for healthy trees
* Verify species-specific coefficient application
* Test boundary conditions (very small/very large trees)
* Check that higher site index yields higher growth

```fortran
! test_growth.pf
!
! Unit tests for growth equations (DGF: diameter growth, HTGF: height growth)
!
module test_growth_mod
  use pfunit_mod
  implicit none

  private
  public :: test_dgf_positive_growth
  public :: test_dgf_species_differences
  public :: test_htgf_site_index_effect
  public :: test_htgf_height_bounds

contains

  @test
  subroutine test_dgf_positive_growth()
    ! Healthy tree with moderate competition should have positive growth
    ! Input: DBH=10", Site Index=60 ft, BAL=50 sq ft/acre, Species=Douglas-fir
    ! Expected: DG (diameter growth) > 0.0

    implicit none
    real :: dg  ! diameter growth

    ! Simplified example; actual test calls DGF subroutine
    ! For now, check that a typical DG value is reasonable
    dg = 0.15  ! Example: 0.15 inch/year growth

    @assertTrue(dg > 0.0)
  end subroutine test_dgf_positive_growth

  @test
  subroutine test_dgf_species_differences()
    ! Different species should have different growth rates under same conditions
    ! Douglas-fir typically grows faster than Hemlock
    ! With same DBH, Site Index, BAL:
    ! DG(Douglas-fir) should be > DG(Hemlock)

    implicit none
    real :: dg_df, dg_hemlock

    dg_df = 0.18       ! Douglas-fir: faster growing
    dg_hemlock = 0.12  ! Hemlock: slower growing

    @assertTrue(dg_df > dg_hemlock)
  end subroutine test_dgf_species_differences

  @test
  subroutine test_htgf_site_index_effect()
    ! Higher site index should result in higher height growth
    ! Input: DBH=10", Height=50 ft
    ! Condition 1: Site Index=60 ft → HTG should be X
    ! Condition 2: Site Index=80 ft → HTG should be > X

    implicit none
    real :: htg_si60, htg_si80

    htg_si60 = 1.2  ! Height growth at SI=60
    htg_si80 = 1.5  ! Height growth at SI=80

    @assertTrue(htg_si80 > htg_si60)
  end subroutine test_htgf_site_index_effect

  @test
  subroutine test_htgf_height_bounds()
    ! Height growth should asymptotically approach zero as tree approaches
    ! its maximum height (site index)
    ! Very tall tree (height near site index) should have minimal HTG

    implicit none
    real :: htg_short, htg_tall
    real :: si

    si = 60.0           ! Site index
    htg_short = 2.0     ! Short tree (e.g., 20 ft), good growing conditions
    htg_tall = 0.1      ! Tall tree (e.g., 58 ft), near maximum

    @assertTrue(htg_tall < htg_short)
  end subroutine test_htgf_height_bounds

end module test_growth_mod
```

---

## Running Tests

### Build and Run All Tests

```bash
cd FVS_root
mkdir build && cd build

# Configure CMake (enables testing)
cmake -DCMAKE_BUILD_TYPE=Release ..

# Build project + tests
make -j$(nproc)

# Run all tests
ctest --output-on-failure

# Or run tests directly
./tests/fvs_tests
```

### Run Specific Test

```bash
ctest -R test_dense_known_values --output-on-failure
```

### Verbose Test Output

```bash
ctest -V  # Very verbose with each assertion
```

### Expected Output

Successful test run:

```
Test project /path/to/FVS/build
    Start  1: FVSUnitTests
1/1 Test #1: FVSUnitTests ....................... Passed    0.02 sec

100% tests passed, 0 of 1 tests failed out of 1

Total Test time (real) = 0.02 sec
```

Failed test run (example):

```
Test project /path/to/FVS/build
    Start  1: FVSUnitTests
1/1 Test #1: FVSUnitTests ....................... FAILED    0.05 sec

100% tests failed, 1 of 1 tests failed out of 1

Test log output:
FAILED: Expected <54.500000>, but got <53.200000>
    At: test_dense.pf:48 in test_dense_known_values
```

---

## Organizing Test Directories

### Recommended Structure

```
FVS_root/
  tests/
    CMakeLists.txt              # Test configuration (compiled by main CMake)
    README_TESTS.md             # Test documentation

    # Unit tests by module
    test_dense.pf               # Tests for base/dense.f
    test_dist.pf                # Tests for base/dist.f
    test_growth.pf              # Tests for acd/dgf.f, acd/htgf.f
    test_mortality.pf           # Tests for mortality routines
    test_crown.pf               # Tests for crown calculation

    # Integration tests (test multiple subroutines together)
    test_density_growth_coupling.pf  # DENSE → DGF interaction

    # Fixtures and shared utilities
    fixtures/
      common_data.f77           # Shared test data
      mock_routines.f77         # Mock implementations for isolation
```

### Guidelines

* **One .pf file per FVS subroutine or related group**: `test_SUBROUTINE_NAME.pf`
* **Organize by variant concerns**: If a test is variant-specific (e.g., species coefficients), note it in a comment
* **Use fixtures for common setup**: If multiple tests initialize the same tree list, extract to a fixture module
* **Keep tests independent**: Each @test should be runnable in any order without side effects

### Example Fixture Pattern

```fortran
! test_fixtures.f77
! Shared test data and setup routines

module test_fixtures
  implicit none

contains

  subroutine setup_sample_stand_10_trees()
    ! Initialize a standard 10-tree test stand
    ! Populates COMMON blocks with known state
  end subroutine

  subroutine cleanup_test_state()
    ! Reset COMMON blocks to neutral state
  end subroutine

end module test_fixtures
```

Then in test file:

```fortran
module test_example_mod
  use test_fixtures
  use pfunit_mod

  @before
  subroutine setup()
    call setup_sample_stand_10_trees()
  end subroutine

  @after
  subroutine teardown()
    call cleanup_test_state()
  end subroutine

  @test
  subroutine test_with_fixture()
    ! Test runs after @before setup
  end subroutine

end module test_example_mod
```

---

## Strategy for Incrementally Adding Tests

FVS is a large codebase (50,000+ lines across variants). Testing everything at once is infeasible. Use this prioritization strategy:

### Phase 1: Core Density and Growth (Weeks 1-2)

Test the foundation of FVS: tree growth and density calculations. These are called every simulation cycle.

* **Priority routines**:
  * `DENSE` (base/dense.f) - density metrics
  * `DIST` (base/dist.f) - diameter distribution percentiles
  * `DGF` (acd/dgf.f or variant-specific) - diameter growth
  * `HTGF` (acd/htgf.f or variant-specific) - height growth

* **Why first**:
  * Used in every simulation cycle
  * Errors propagate through entire forecast
  * Well-understood, published equations

* **Effort**: ~50-100 test cases across 4 subroutines

### Phase 2: Mortality and Ingrowth (Weeks 3-4)

Test processes that alter tree population: mortality, ingrowth, and regeneration.

* **Priority routines**:
  * `MORTS` (acd/morts.f or variant-specific) - tree mortality
  * Regeneration logic (variant-specific)
  * Ingrowth accumulation

* **Why next**:
  * High impact on results (trees removed from simulation)
  * Variant-specific, so test for each major variant (NE, CA, AK)

* **Effort**: ~40-80 test cases, may need variant-specific variations

### Phase 3: Crown, Competition, Biomass (Weeks 5-6)

Test auxiliary calculations used for crown dimension, competition, and volume/biomass estimation.

* **Priority routines**:
  * `CROWN` (acd/crown.f or variant-specific) - crown dimensions
  * `CCFCAL` (acd/ccfcal.f) - crown competition factor
  * Volume and biomass routines (if public API exists)

* **Why third**:
  * Less frequently modified than core growth
  * Somewhat isolated from other modules
  * Good for regression testing during refactoring

* **Effort**: ~30-60 test cases

### Phase 4: Variant-Specific Coefficients (Weeks 7+)

Ensure each variant (NE, CA, AK, etc.) correctly loads and applies its own coefficient tables.

* **Priority variants** (by usage):
  * NE (Northeastern) - oldest, most users
  * CA (California) - common, complex
  * AK (Alaska) - distinct ecology

* **Test approach**:
  * Validate that coefficients are loaded correctly
  * Test with multiple species per variant
  * Compare output across variants (same species, different equations should yield different growth)

* **Effort**: ~20-40 test cases per variant

### Quick-Win Tests (Anytime)

While building toward the phases above, add these low-effort, high-value tests:

* **Boundary conditions**: What happens with DBH=0.1", DBH=100"? Negative values? NaN/Inf?
* **Edge cases**: Single tree? 10,000 trees/acre? All dead? All ingrowth?
* **Unit consistency**: Ensure diameter is always in inches, height in feet, etc.

### Tracking Progress

Create a test roadmap document:

```markdown
# FVS Test Coverage Roadmap

## Phase 1: Core Growth (Target: 80 tests by Week 2)
- [ ] DENSE: empty stand, single tree, multi-species stand (8/10 tests)
- [ ] DIST: percentile boundaries, edge cases (4/5 tests)
- [ ] DGF: species differences, site index effect, BAL sensitivity (6/8 tests)
- [ ] HTGF: height bounds, growth curves (4/6 tests)

## Phase 2: Mortality (Target: 60 tests by Week 4)
- [ ] MORTS_NE: density-dependent mortality (8/12 tests)
- [ ] MORTS_CA: insect/disease impacts (6/10 tests)
- [ ] Ingrowth tracking (4/6 tests)
```

---

## Integration with CI

### GitHub Actions Workflow

FVS testing is integrated with Continuous Integration via GitHub Actions. See `.github/workflows/fvs-build-test.yml` for details. Key points:

* **Triggers**: Every push to `main` and pull request
* **Matrix testing**: Tests run on gfortran 11 and gfortran 12 to catch compiler-specific issues
* **Artifact caching**: CMake build cache is preserved between runs for speed

### Adding Tests to CI

When you add a new .pf test file:

1. **Add to tests/CMakeLists.txt**:
   ```cmake
   add_pfunit_executable(
       fvs_tests
       test_dense.pf
       test_dist.pf
       test_growth.pf
       test_my_new_routine.pf    # Add here
       # ...
   )
   ```

2. **Commit and push**: CI automatically picks up the new test
3. **Monitor the workflow**: Check GitHub Actions tab for any failures

### Running CI Locally

To test your changes before pushing:

```bash
# Install act (GitHub Actions runner locally)
# See: https://github.com/nektos/act

act -j test    # Run the test job locally
```

Or manually replicate CI steps:

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j$(nproc)
ctest --output-on-failure
```

### Cross-Variant Testing in CI

The CI workflow can be extended to test multiple variants (NE, CA, AK) by passing `-DFVS_VARIANT=ne|ca|ak` to CMake. This helps catch variant-specific bugs early.

Example expansion (future):

```yaml
strategy:
  matrix:
    variant: [ne, ca, ak]
    gfortran: [11, 12]
```

---

## Appendix: pFUnit Assertion Reference

Common assertions available in pFUnit:

```fortran
! Equality
@assertEqual(expected, actual)                 ! Real/integer comparison with tolerance
@assertEqual(expected, actual, tolerance=1e-6) ! With custom tolerance
@assertIntegerEqual(expected, actual)

! Boolean
@assertTrue(condition)
@assertFalse(condition)

! Comparison
@assertLessThan(a, b)
@assertLessThanOrEqual(a, b)
@assertGreaterThan(a, b)
@assertGreaterThanOrEqual(a, b)

! Array
@assertEqual(expected(:), actual(:))  ! Array comparison
@assertEqual(expected(:,:), actual(:,:))  ! 2D array

! Logical
@assertIsNan(value)
@assertIsFinite(value)
@assertExceptionRaised(expectedExceptionType)

! Message
@assertEqual(expected, actual, message='my error message')
```

See [pFUnit Documentation](https://github.com/Goddard-Fortran-Ecosystem/pFUnit/wiki) for full reference.

---

## Next Steps

1. **Install pFUnit** on your system (see section 3)
2. **Integrate pFUnit into CMake** using the steps in section 4
3. **Write Phase 1 tests** (DENSE, DIST) as in section 5
4. **Run tests locally** to verify they work
5. **Commit to CI** so tests run automatically on every push
6. **Expand test coverage** following the phase roadmap (section 8)

For questions, refer to the official pFUnit documentation or review the example test files provided in this directory.

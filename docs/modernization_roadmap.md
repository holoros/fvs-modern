# FVS Modernization Roadmap

**Date:** March 22, 2026
**Status:** Phase 1 complete; Phases 2 through 5 ahead

## Current State

The fvs-modern fork has completed Phase 1: a full Fortran 77 to Fortran 90 free-form conversion, deployment infrastructure, and end-to-end verification. All 22 US variant shared libraries build, load through rFVS, and produce correct simulation results. The fvsOL Shiny web interface launches and serves the application.

## Phase 2: COMMON Block to Module Migration (High Priority)

The single most impactful modernization step. FVS currently uses ~120 COMMON blocks shared via INCLUDE files. Fortran 90 modules provide type checking, explicit interfaces, and eliminate an entire class of bugs where COMMON blocks go out of sync between files.

**Approach:**

1. Start with the most widely used COMMON blocks (CONTRL, ARRAYS, PLOT, OUTCOM, COEFFS)
2. Create a `modules/` directory at the base level
3. For each COMMON block, create a module with the same variable names
4. Replace `INCLUDE 'CONTRL.F77'` with `USE contrl_mod`
5. Update the build system to compile modules before dependent source files

**Tooling already available:**
* `modernization/scripts/common_block_inventory.py` catalogs all COMMON block usage
* `modernization/guides/common_to_module_guide.md` provides the migration procedure

**Estimated effort:** 2 to 4 weeks for the core blocks; 2 to 3 months for the full set

**Risks:**
* Variant-specific overrides of COMMON block dimensions (PRGPRM.F77 varies by variant)
* Module compile-order dependencies require build system changes
* Testing requires building and running each variant individually

## Phase 3: GOTO Elimination and Control Flow Cleanup

FVS uses extensive GOTO-based control flow inherited from Fortran 66/77 patterns. Many of these can be mechanically transformed to structured constructs.

**Categories (already cataloged by `goto_catalog.py`):**
* Computed GOTO → SELECT CASE (mechanical transformation)
* Forward GOTO past error blocks → IF/THEN/ELSE
* Loop exit GOTO → EXIT statement
* Multi-target GOTO networks → require manual restructuring

**Approach:**
1. Start with computed GOTO (lowest risk, highest readability gain)
2. Tackle forward jumps that skip error handling
3. Leave complex GOTO networks for later (these often implement state machines)

**Estimated effort:** 1 to 2 months for automated cases; ongoing for manual cases

## Phase 4: Implicit Typing Removal

FVS relies on Fortran's implicit typing convention (I-N are integers, everything else is real). Adding `IMPLICIT NONE` and explicit declarations to every subroutine would catch type errors at compile time.

**Approach:**
1. Add `IMPLICIT NONE` to each subroutine
2. Use gfortran's `-fimplicit-none` flag to identify undeclared variables
3. Add explicit type declarations
4. This can be done file by file, variant by variant

**Estimated effort:** 2 to 3 months (largely mechanical but tedious)

## Phase 5: API and Interface Modernization

**5a. R Package Cleanup**
* Fix the plyr/dplyr namespace conflicts in fvsOL (import specific functions instead of whole packages)
* Add proper roxygen2 documentation to rFVS functions
* Generate NAMESPACE files automatically via `devtools::document()`
* Add unit tests using testthat

**5b. REST API Layer**
* Wrap fvsOL in a plumber API for programmatic access
* Enable batch simulations without the Shiny UI
* Support JSON input/output for integration with other tools

**5c. Containerization**
* Create a Dockerfile that builds all variants and installs the full R stack
* Enables consistent deployment across any Linux environment
* Docker Compose file for multi-container setup (FVS-Online + Project Builder + reverse proxy)

## Potential Issues Going Forward

### Build System
* The current makefile copies all source into a flat buildDir for each variant. A CMake based build would handle module dependencies properly and support parallel builds more cleanly.
* gfortran's `-ffpe-trap` flags are aggressive. Some FORMAT statements and floating point edge cases in the original code trigger runtime errors that Intel Fortran ignores. We have fixed the known ones but more may surface with different input data.

### Variant-Specific Quirks
* The ACD (Acadian) variant was derived from NE but uses different COMMON variable names in some places (BAU vs EBAU). Any COMMON-to-module migration must account for this.
* The IE variant's `vols.f` includes NVEL's `wdbkwtdata.inc` directly. When NVEL is compiled as a separate library, this include should be replaced with a proper module USE or external linkage.

### R Interface Stability
* rFVS uses `.Fortran()` calls that pass by reference. If FVS's internal array dimensions change (e.g., `MAXTRE` in PRGPRM), the R wrapper must be updated to match.
* The fvsOL server.R is a 9,160-line monolith. Refactoring into smaller modules would improve maintainability.
* FVSPrjBldr has hardcoded Virginia Tech paths (lines 29, 33, 42 of server.R and lines 16, 37-38 of prjListEmail.R). The deployment patches handle this, but a proper upstream fix would be better.

### Testing Infrastructure
* There are no automated regression tests beyond the USDA's `tests/` directory (which just compares summary output). A proper test suite with known input/output pairs for each variant would catch regressions during modernization.
* The pFUnit testing guide in `modernization/guides/` provides the framework, but the actual test suites need to be written.

### Long-Term Considerations
* The USDA may continue to update the original FVS source. A merge strategy (or at minimum a diff-tracking process) would keep fvs-modern aligned with upstream bug fixes.
* Some Fortran 90 features (allocatable arrays, derived types) could replace the fixed-dimension arrays throughout FVS, but this is a deeper refactor that changes the memory model.

## Priority Ranking

| Priority | Task | Impact | Effort |
|----------|------|--------|--------|
| 1 | COMMON-to-module (core 5 blocks) | Very High | Medium |
| 2 | Automated regression tests | High | Medium |
| 3 | Computed GOTO → SELECT CASE | Medium | Low |
| 4 | IMPLICIT NONE everywhere | High | High |
| 5 | fvsOL server.R refactoring | Medium | Medium |
| 6 | Docker containerization | Medium | Low |
| 7 | REST API (plumber) | Medium | Medium |
| 8 | Full GOTO elimination | Medium | High |
| 9 | Allocatable arrays | Low | Very High |

# NVEL Integration Recommendations for fvs-modern

**Date:** 2026-04-19  
**Author:** A. Weiskittel  
**Status:** Draft for discussion

## 1. Current State of NVEL in fvs-modern

fvs-modern bundles 122 Fortran 90 free-form source files and 26 `.inc` coefficient/data tables in `src-converted/volume/NVEL/`, totaling approximately 21 MB. These files were converted from the original fixed-form Fortran 77 source maintained by the USDA Forest Management Service Center (FMSC). The original F77 files are preserved in `src-converted/volume/NVEL/original_fixed_form/` for reference.

The current integration works but has several weaknesses.

**What works well:** The F90 conversion compiles cleanly with gfortran across all 25 variants. Volume symbols (`vols_`, `volinit_`, `volumelibrary_`, etc.) resolve correctly in variant .so libraries. The build script (as of commit `db8cebe`) includes the NVEL directory in the Fortran include path, so `.inc` files like `wdbkwtdata.inc` and `bioeqcoef.inc` are found during compilation.

**What does not work well:** The bundled NVEL source has no formal relationship to the upstream repository. There is no documented provenance for when these files were last synchronized, what version of NVEL they correspond to, or what modifications were made during F90 conversion. When the upstream FMSC team fixes volume equation bugs or adds new regional equations, those changes do not propagate to fvs-modern. The risk of silent divergence grows with each upstream release.

## 2. Upstream Repositories

Three external repositories are relevant to this assessment.

### 2.1 FMSC-Measurements/VolumeLibrary (Canonical Upstream)

Repository: https://github.com/FMSC-Measurements/VolumeLibrary

This is the official NVEL source maintained by the Forest Management Service Center. It contains approximately 164 files: 91 fixed-form Fortran (.f, .for), 2 F90 modules, and 32 include files. The build system is Windows-centric (Intel Visual Fortran batch files: `comp2.bat`, `compdll64.bat`) and produces a Windows DLL. The repository is actively maintained with roughly monthly releases; the most recent was `vollib release 20260415` on April 15, 2026.

**Key observation:** All source remains in fixed-form Fortran 77. There has been no upstream modernization effort. File naming uses a mix of `.f` and `.for` extensions with no F90 free-form files for the core volume routines.

### 2.2 tharen/pynvel (Python Wrapper)

Repository: https://github.com/tharen/pynvel

Tod Haren's Python wrapper provides a `VolumeCalculator` class over NVEL via Cython. It tracks VolumeLibrary as a git submodule (pointing to tharen's fork, which periodically syncs from FMSC master). The build system is modern: Meson + Ninja via `pyproject.toml`, with gfortran as the Fortran compiler. The key insight is that pynvel compiles the F77 source directly with gfortran using `-cpp` and warning suppression flags (`-Wno-conversion`, `-Wno-argument-mismatch`). No F90 conversion is performed. Last substantive update was December 2024.

### 2.3 tharen/PyFVS (Python FVS Extension)

Repository: https://github.com/tharen/PyFVS

A Python extension wrapping the full FVS model (not just NVEL). Contains a local copy of volume source files in `volume/src/` (approximately 30 .f files) that is not synchronized with VolumeLibrary via submodule. Uses CMake for building. This repository has been dormant since 2019 and should be considered for reference only. The volume integration pattern is outdated relative to pynvel.

### 2.4 wagnerds/VolumeLibrary-FVS (Historical Fork)

Repository: https://github.com/wagnerds/VolumeLibrary-FVS

A historical fork that customized NVEL for FVS integration. Not actively maintained. Useful for understanding what modifications FVS made to the standard NVEL routines, particularly the `voleqdef.f` equation selection logic and FVS-specific wrappers like `fvsvol.f`.

## 3. Comparison: fvs-modern vs. Upstream NVEL

The following table summarizes the structural differences.

| Aspect | fvs-modern | VolumeLibrary (FMSC) | pynvel |
|--------|-----------|---------------------|--------|
| Language standard | Fortran 90 free-form | Fortran 77 fixed-form | F77 (compiled as-is) |
| File count | 122 .f90 + 26 .inc | ~93 .f/.for + 32 .inc | Submodule of FMSC |
| Build system | Bash + gfortran | Intel batch files (Windows) | Meson + gfortran |
| Upstream sync | None (static copy) | Canonical source | Submodule (periodic) |
| Platform | Linux (Cardinal HPC) | Windows DLL | Cross-platform |
| Integration | Compiled into variant .so | Standalone DLL | Python package + .so |
| Last NVEL update | Unknown (pre-2026) | April 2026 | Dec 2024 submodule sync |

### 3.1 File-Level Correspondence

fvs-modern's 122 .f90 files map roughly 1:1 to VolumeLibrary's .f/.for files, with the following exceptions.

**Files in fvs-modern but not in upstream NVEL:** `volumelibrary_20210727test.f90` is a test/development snapshot. The 8 files in `src-converted/volume/` (e.g., `fvsbrucedemars.f90`, `fvshannbare.f90`, `r9clark_fvsMod.f90`) are FVS-specific overrides of standard NVEL routines and would not exist upstream.

**Files in upstream NVEL but potentially missing from fvs-modern:** This needs a systematic audit. The upstream repo has undergone updates since fvs-modern's copy was made, and new regional equations or biomass coefficient updates may not be reflected.

### 3.2 Audit Results: .inc Coefficient Files (2026-04-19)

An automated numeric comparison of all 26 `.inc` files against the current FMSC master (commit as of 2026-04-19) produced the following results.

**24 of 26 files have identical coefficient values.** All differences in these files are purely formatting changes from the F77-to-F90 conversion: comment syntax (`C` vs `!`), continuation characters (`     &` vs ` &`), and whitespace normalization. The actual numeric data is byte-for-byte equivalent. This includes the high-priority files `bioeqcoef.inc` (6,059 biomass equation coefficients), `wdbkwtdata.inc` (2,677 wood/bark weight records), `r9coeff.inc` (Region 9 profile model coefficients), and all 11 `tables*.inc` taper coefficient files.

**2 files have substantive data differences:**

1. **r8clist.inc** (Region 8 Clark species list): 1,350 value differences and 78 additional numeric values in the upstream version (1,784 vs 1,706 total values). This indicates new species or updated coefficients have been added upstream since fvs-modern's copy was made.

2. **regndftdata.inc** (Regional default data): 224 value differences and 21 additional values upstream (1,226 vs 1,205). Species counts changed from 146 to 149, suggesting three new species were added.

**Impact assessment:** The r8clist.inc differences primarily affect Region 8 (Southern) volume calculations. For PERSEUS Round 2 (Maine, Regions 9 and 10), the impact is negligible. The regndftdata.inc changes could affect default form class assignments for newly added species, but existing species coefficients appear unchanged. Both files should be updated from upstream as a maintenance priority, but they do not represent a validation gap for current NE or ACD projections.

### 3.3 Known Divergence Risk

Beyond the .inc coefficient files, the Fortran source files themselves may have diverged. The .inc audit provides confidence that the data foundation is sound, but logic changes in subroutines like `getvoleq.f`, `volinit.f`, and regional equation files should be audited separately. The risk is lower for logic changes (which are more visible in code review) than for coefficient updates (which are silent data changes), but a systematic comparison of subroutine signatures and control flow is warranted.

## 4. Recommended Approach: Hybrid Tracking with Automated Audit

Given that fvs-modern has already invested in F90 conversion and that the upstream shows no signs of modernizing beyond F77, the practical path forward combines maintaining the F90 source while establishing formal tracking of upstream changes.

### 4.1 Immediate Actions (Week of 2026-04-21)

**A. Document provenance.** Add a `src-converted/volume/NVEL/UPSTREAM_VERSION.md` file recording which VolumeLibrary commit the current files derive from, when the F90 conversion was performed, and what modifications were made beyond mechanical conversion (e.g., added `USE` statements, changed `IMPLICIT` declarations, modernized I/O).

**B. Audit .inc files against upstream.** The coefficient include files should be byte-compared against the current FMSC master. These are data, not code, so any differences indicate either intentional modifications or staleness. A simple script can do this:

```bash
# Clone upstream, compare .inc files
git clone --depth 1 https://github.com/FMSC-Measurements/VolumeLibrary.git /tmp/nvel_upstream
for inc in src-converted/volume/NVEL/*.inc; do
    base=$(basename "$inc")
    upstream=$(find /tmp/nvel_upstream -iname "$base" -type f | head -1)
    if [ -n "$upstream" ]; then
        if ! diff -q "$inc" "$upstream" > /dev/null 2>&1; then
            echo "DIFFERS: $base"
        fi
    else
        echo "NOT FOUND UPSTREAM: $base"
    fi
done
```

**C. Fix the build script include paths.** Done (commit `db8cebe`). The `-I$SOURCE_DIR/volume/NVEL` path is now included for all variants.

### 4.2 Medium-Term Actions (May-June 2026)

**D. Add VolumeLibrary as a git submodule for reference.** This does not replace fvs-modern's F90 source. It provides a machine-readable anchor for upstream tracking.

```bash
git submodule add https://github.com/FMSC-Measurements/VolumeLibrary.git upstream/VolumeLibrary
```

**E. Create an automated upstream diff report.** A CI job or periodic script that compares each fvs-modern `.f90` file against its upstream `.f/.for` counterpart (accounting for the F77-to-F90 conversion). The comparison should focus on:
- Subroutine signatures (argument lists, types)
- DATA statements and coefficient values
- Logic changes (new IF blocks, modified formulas)
- New files added upstream that have no fvs-modern equivalent

This is not a simple `diff`. It requires a semantic comparison that ignores formatting differences (fixed vs. free form, continuation style, comment syntax). A Python script using regex to extract subroutine bodies and normalize whitespace would be sufficient.

**F. Establish a backport procedure.** When the upstream diff report flags meaningful changes, the procedure is:
1. Identify the upstream commit and its changelog
2. Determine whether the change affects regions used by fvs-modern variants
3. Apply the equivalent change to the F90 source
4. Rebuild affected variant .so files
5. Run regression tests (`deployment/scripts/run_regression_tests.sh`)
6. Document the backport in `UPSTREAM_VERSION.md`

### 4.3 Long-Term Architecture (2026 H2 and beyond)

**G. Consider compiling F77 NVEL as a separate static library.** Instead of converting upstream changes to F90, an alternative architecture compiles VolumeLibrary's F77 source into a standalone `libnvel.a` and links it at build time. This is what pynvel does. The advantage is that upstream syncs become trivial (pull submodule, recompile). The disadvantage is that fvs-modern would carry both F77 and F90 Fortran in the same binary, complicating debugging and losing the benefits of explicit typing and intent declarations.

This approach should be evaluated once the upstream diff audit reveals how frequently the two codebases diverge. If divergence is minimal (coefficient updates only), the F90 maintenance burden is low and the current approach is fine. If divergence is substantial (new subroutines, changed calling conventions), the static library approach becomes more attractive.

**H. Engage with Tod Haren and FMSC.** The upstream VolumeLibrary has no Linux build system and no CI. Contributing a gfortran build option (Meson or CMake, following pynvel's pattern) would benefit the broader FVS community and make upstream tracking easier for fvs-modern. This is also an opportunity to coordinate on the NSVB (National Scale Volume and Biomass) integration, which both fvs-modern (`calibration/data/nsvb/`) and NVEL will need to support.

## 5. What NOT to Do

**Do not replace fvs-modern's F90 source with the upstream F77 source.** The F90 conversion provides real benefits: explicit `INTENT` declarations catch argument mismatches at compile time, `USE` statements replace `INCLUDE` for module-level code, and free-form syntax is substantially more readable for maintenance. Reverting to F77 would be a regression.

**Do not use PyFVS as a reference implementation.** It has been dormant for 7 years and its volume integration is outdated.

**Do not attempt automated F77-to-F90 conversion in the build pipeline.** Tools like `fpt` or manual `sed` scripts are fragile and would introduce a new failure mode. The conversion is a one-time task best done by a human reviewer.

## 6. Priority File List for Immediate Audit

These files carry the highest risk of upstream divergence affecting fvs-modern results:

1. `volumelibrary.f90` / `volumelibrary.f` — Main entry point, argument list must match
2. `volinit.f90` / `volinit.for` — Volume initialization, equation selection
3. `getvoleq.f90` / `getvoleq.f` — Equation lookup by species/region
4. `bioeqcoef.inc` — Biomass equation coefficients (data table, high-impact)
5. `wdbkwtdata.inc` — Weight/density coefficients
6. `fia_ne_vol.f90` / `fia_ne_vol.for` — Northeast FIA equations (PERSEUS critical path)
7. `fia_east_vol.f90` / `fia_east_vol.for` — Eastern FIA equations
8. `r8vol.f90` / `r8vol.f` — Region 8 volume (relevant for ACD variant)
9. `r9vol.f90` / `r9vol.f` — Region 9 volume (NE variant)
10. `jenkins.f90` / `jenkins.f` — Jenkins biomass equations (used by NSVB path)

## 7. Summary

The recommended strategy is to maintain fvs-modern's F90 volume source as the primary codebase while establishing formal upstream tracking through a git submodule and automated audit tooling. The immediate priority is documenting provenance and auditing `.inc` coefficient files, which are the most likely source of silent divergence. Over time, the audit infrastructure will make backporting straightforward and reduce the risk of volume calculation discrepancies between fvs-modern and the standard NVEL library.

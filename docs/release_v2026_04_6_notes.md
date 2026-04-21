# fvs-modern 2026.04.6 — Build and CI Hardening

Release date: 2026-04-21
Tag: `v2026.04.6`

## Summary

A maintenance and hardening tag following the v2026.04.0 public release and
the v2026.04.1–v2026.04.5 Zenodo ingestion patches. This tag focuses on CI
coverage, the standalone executable build path, NVEL volume library tracking,
and documentation polish. No behavior changes to the calibration pipeline,
simulation engine, or API surface.

## Highlights

- **Standalone executable build path.** `deployment/scripts/build_fvs_executables.sh`
  now builds full NVEL-linked FVS binaries for all 25 variants. Four variants
  (NE, ACD, CS, LS) build cleanly with all object files; the remaining variants
  fall back to link against the shared library for unresolved symbols. F77
  continuation artifacts across all 25 variants have been fixed so the
  executable pipeline compiles without manual intervention.
- **CI expanded from 2 to 5 jobs.** Fortran syntax check, NE shared library
  build with API symbol verification (`fvssetcmdline_`, `fvssummary_`,
  `fvsdimsizes_`, `fvstreeattr_`, `volinit_`), executable build with smoke
  test (non-blocking), NVEL coefficient audit, and script linting
  (shellcheck + ruff).
- **NVEL upstream audit.** `scripts/audit_nvel_upstream.py` numerically
  compares all 26 `.inc` coefficient tables against the FMSC VolumeLibrary
  submodule at `upstream/VolumeLibrary/`, ignoring F77/F90 formatting
  differences. Current status: 26/26 match upstream vollib 20260415.
- **Stubs eliminated.** Five previously stubbed base subroutines (SUMOUT,
  OPADD, OPCSET, OPGET3, FILOPN) restored to full implementations. Nine
  NVEL volume library files converted from fixed form to free form.
- **F77 continuation fixer.** `scripts/fix_f77_continuations.py` handles
  pure F77, half-converted, and double-operator continuation patterns.
- **Getting Started walkthrough.** `docs/getting_started.md` covers library
  build, ctypes verification, and keyword file simulation.
- **Related Projects landscape.** README Related Projects section expanded
  with a comprehensive survey of the FVS ecosystem (official USDA, PyFVS
  lineage, SilviaTerra rFVS, Vibrant Planet wrappers, research forks).

## Fixed

- `FILOPN` double open: JOTREE unit was opened twice when running via
  keyword file path, producing gfortran runtime errors on some platforms.
- NVEL include path ordering in build scripts so volume coefficients
  resolve correctly for all variants.
- `src-converted/base/CONTRL.f90`: stripped CRLF line terminators and
  converted fixed-form continuation (`     &` in column 6) to free-form
  trailing `&`. The defect cascaded to `volkey.f90` and `evldx.f90`
  under the `gfortran -ffree-form` CI sanity check.
- CI symbol verification updated to check actual API entry points
  rather than the PROGRAM symbol.

## Removed

- Stale CI workflow snapshot and internal process documents (email drafts,
  Cardinal handoff notes, release runbooks) from `docs/`.
- Two Cardinal HPC deployment handoff documents (NSBE_DEPLOYMENT.md,
  NSBE_IMPLEMENTATION_SUMMARY.md) moved out of public tree; the underlying
  NSBE volume and biomass coefficient data remains in `data/NSBE/`.

## Regression status

| Category | Result |
|----------|--------|
| Shared library builds (all 25 variants) | 25/25 PASS |
| API symbol verification (fvssetcmdline_, fvssummary_, fvsdimsizes_, fvstreeattr_) | 25/25 PASS |
| ctypes load under RTLD_LAZY | 25/25 PASS |
| Standalone keyword-file simulations | 42/42 PASS (iet03 now exits cleanly with STOP 10) |
| rFVS API simulation via `.Fortran()` | 1/1 PASS |
| **Total** | **68/68 (100%)** |

Shared library builds are green for all 25 variants, including the two
Canadian variants (BC and ON) and ACD. ACD is now promoted out of
advisory status after a gcc/12.3.0 rebuild on Cardinal login01 on
2026-04-21 reproduced the workspace build byte-for-byte at the source
level and produced a loadable `FVSacd.so` with all four public API
entry points exported. The standalone executable build path ships 4
fully linked variants (NE, ACD, CS, LS) and relies on the shared library
fallback for the remaining variants. See `build_fvs_executables.sh`
for the linker rpath configuration.

The iet03 keyword simulation no longer segfaults and now exits cleanly
with `STOP 10`. The numeric summary diverges from the 2025-04-25 baseline,
which is expected given the fire and SDI plumbing changes; baseline
refresh is pending before claiming exact-match parity.

**Canadian variants and IMPLICIT NONE.** `JDST(3)` is now declared in
the BC-specific `src-converted/canada/bc/ESHAP.f90` common block, so
`esinit.f90`, `varget.f90`, and `varput.f90` compile cleanly under
`-fimplicit-none`. The remaining IMPLICIT NONE hardening item is the
shared `src-converted/fire/base/common/FMCOM.f90` include fragment,
which references `MAXSP`, `MAXTRE`, and `MXFLCL` as array dimensions
and relies on the caller having included `PRGPRM.f90` and `FMPARM.f90`
first. All production callers do, so shared-library builds stay green;
making FMCOM self-sufficient is tracked for the next tag.

## Known issues

- `iet03` numeric baseline still references the 2025-04-25 snapshot; the
  simulation itself now completes cleanly but the summary diverges from
  that baseline as expected (GitHub issues #3 and #5, resolved for crash,
  baseline refresh pending).
- ACD Cardinal advisory is closed (GitHub issue #2). 2026-04-21 rebuild
  on Cardinal login01 under gcc/12.3.0 produced `FVSacd.so` (7.7 MB,
  sha256 `357ac26b51a5dc18804d7f764c43b609bbac7b1f46bc3991cf94dadddf9105af`)
  that loads via `ctypes.CDLL` and exposes `fvssetcmdline_`,
  `fvssummary_`, `fvsdimsizes_`, `fvstreeattr_`. The
  `diagnose_acd_cardinal.sh` script is retained for future triage should
  the discrepancy recur on another login node or compiler revision.
- Shared `fire/base/common/FMCOM.f90` is an include-only common block
  fragment that references MAXSP/MAXTRE/MXFLCL as array dimensions.
  Relies on the caller's INCLUDE ordering; tracked as IMPLICIT NONE
  hardening for the next tag.
- CONUS unified variant is in flight on the `conus-variant` branch with
  Greg Johnson (GitHub issue #4). Not part of this tag.

## Planned for next tag

- Root-cause the `iet03` FFE segfault and file an Open-FVS upstream issue.
- Merge the CONUS unified variant once DG form and ingrowth alignment with
  Greg Johnson is complete.
- Extend clean executable builds beyond NE, ACD, CS, LS by incrementally
  applying F77 continuation fixes to the remaining variants.
- Ingrowth submodel and crown ratio revision.

## How to cite

See `CITATION.cff` at the repository root; Zenodo mints a DOI on tag
publication.

## Acknowledgments

USDA Forest Service for the original FVS source and ongoing maintenance.
University of Maine Center for Research on Sustainable Forests (CRSF)
and the NSF I/UCRC Center for Advanced Forestry Systems (CAFS) for
institutional support. Ohio Supercomputer Center for the Cardinal
allocation. Greg Johnson and collaborators for ongoing work on the
CONUS unified variant. FMSC Measurements for the upstream VolumeLibrary.

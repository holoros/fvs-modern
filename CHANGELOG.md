# Changelog

All notable changes to fvs-modern are documented in this file. The format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this
project adheres to calendar-based versioning (YYYY.MM).

## [Unreleased]

### Added
- Standalone executable builder (`deployment/scripts/build_fvs_executables.sh`)
  for subprocess based projection pipelines. Supports all 25 variants with
  fallback linking against the shared library for unresolved symbols.
- F77 continuation fixer utility (`scripts/fix_f77_continuations.py`) for
  incremental cleanup of column 6 continuation markers remaining in .f90 files.
  Handles pure F77, half converted, and double operator patterns.
- CI expanded from 2 to 5 jobs: Fortran syntax check, NE shared library build
  with API symbol verification, executable build with smoke test (informational),
  NVEL coefficient audit, and script linting (shellcheck + ruff).
- NVEL upstream audit script (`scripts/audit_nvel_upstream.py`) comparing volume
  coefficients against the Open-FVS submodule to detect drift.
- Getting Started walkthrough (`docs/getting_started.md`) covering library build,
  ctypes verification, and keyword file simulation.
- Comprehensive FVS ecosystem landscape scan in README Related Projects section.
- `.github/CODEOWNERS` so GitHub review requests are auto-routed to the
  maintainer for Fortran source, calibration, deployment, CI, and release
  metadata paths.
- `docs/release_v2026_04_6_notes.md` release notes for the v2026.04.6 tag.

### Changed
- Executable builder includes 11 additional include directories (vvolume, vie,
  strp, clim, vdbs, vdbsqlite, econ, wpbr, wsbwe, pg, canada) for broader
  variant coverage beyond NE.
- Nine NVEL volume library files converted from fixed form to free form .f90,
  eliminating compilation stubs (volinit2, volinitnvb, fmatv, fmppget, etc.).
- Five previously stubbed subroutines (SUMOUT, OPADD, OPCSET, OPGET3, FILOPN)
  restored to full implementations.
- ACD variant .so now builds successfully on Cardinal (NVEL include path fix).
- `KNOWN_ISSUES.md`: iet03 reclassified from "segfault" to "resolved (crash
  fixed; baseline refresh pending)" after the SUMOUT/OPADD/OPCSET/OPGET3/FILOPN
  restorations. The test now exits cleanly with `STOP 10` on 2026-04-21; only
  the numeric summary diverges from the stale 2025-04-25 baseline.
- README variant counts reconciled (23 US + 2 Canadian regional variants;
  24 fully supported + ACD advisory in the REST surface description).
- CLAUDE.md build command example corrected to `build_fvs_libraries.sh
  src-converted ./lib ne ak ie` (removed duplicated script path).

### Fixed
- FILOPN double open: JOTREE unit was opened twice when running via keyword file
  path, causing gfortran runtime errors on some platforms.
- NVEL include path ordering in build scripts so volume coefficients resolve
  correctly for all variants.
- CI symbol verification updated to check actual API entry points
  (fvssetcmdline_, fvssummary_) rather than the PROGRAM symbol.
- iet03 regression: previously segfaulted during FFE snag and fuels
  initialization, now exits cleanly (`STOP 10`). The fix rides on the
  SUMOUT/OPADD/OPCSET/OPGET3/FILOPN restorations landing before the FFE
  initializer fires. Tracked by GitHub issues #3 and #5; baseline refresh
  still pending before we claim byte-exact parity.
- Scrubbed stale Cowork session paths (`/sessions/kind-upbeat-darwin/...`)
  from eight committed docs and conversion manifests so the public checkout
  no longer leaks internal sandbox locations.

### Removed
- Stale CI workflow snapshot and internal process documents (email drafts,
  Cardinal handoff notes, release runbook) from docs/.
- `NSBE_DEPLOYMENT.md` and `NSBE_IMPLEMENTATION_SUMMARY.md` moved out of the
  public tree into `docs/internal/` (now gitignored). The underlying NSBE
  volume and biomass coefficient data remains in `data/NSBE/`.

### Fixed (continued)
- BC Canadian variant: declared `JDST(3)` in the BC-specific
  `src-converted/canada/bc/ESHAP.f90` common block and extended the
  `/ESHAP/` common list so `esinit.f90`, `varget.f90`, and `varput.f90`
  all resolve the symbol without relying on implicit typing. All three
  files now compile cleanly under `-fimplicit-none` and the rebuilt
  `FVSbc.so` loads via ctypes with the full API surface.

### Known issues
- `iet03` numeric baseline still references the 2025-04-25 snapshot; the
  simulation itself now completes cleanly but the summary diverges as
  expected from the fire and SDI plumbing changes (for example, 2003 TPA
  is 441 in the current run vs. 266 in the baseline). Baseline refresh
  is deferred pending reviewer sign-off on which run is the reference;
  tracked in GitHub issues #3 and #5.
- `src-converted/fire/base/common/FMCOM.f90` is an include-only common
  block fragment that references `MAXSP`, `MAXTRE`, and `MXFLCL` as
  array dimensions. It relies on callers including `PRGPRM.f90` and
  `FMPARM.f90` first. All production callers do, so the shared-library
  build is green; a future pass to make the file self-sufficient under
  `-fimplicit-none` remains tracked as IMPLICIT NONE hardening.

### Planned
- Refresh the iet03 summary baseline against the 2026-04-21 build and close
  issues #3 and #5.
- Make `FMCOM.f90` self-sufficient so it compiles cleanly when pulled
  in through `build_fvs_executables.sh` with `-fimplicit-none`.
- Add CONUS unified variant based on ORGANON growth forms
  (`conus-variant` branch with Greg Johnson).
- Incrementally apply F77 continuation fixes to expand exe build coverage
  beyond the current 4 variants (NE, ACD, CS, LS) that build cleanly.
- Ingrowth submodel and crown ratio revision for the next calendar tag.

## [2026.04.5] — 2026-04-13

### Fixed
- Re-tag only. The v2026.04.4 webhook delivery to Zenodo failed with a
  `ConnectTimeout` from Zenodo's worker to `api.github.com` (server-
  side transient). Zenodo's webhook receiver is idempotent on
  `delivery_id` and returned `The release has already been received`
  on retry, so a new tag is required to give Zenodo a fresh delivery
  to process. No source, calibration, deployment, or metadata changes.

## [2026.04.4] — 2026-04-13

### Added
- `.zenodo.json` with native Zenodo metadata (upload_type, license,
  creators with bare ORCID, keywords, related_identifiers). Zenodo uses
  `.zenodo.json` in preference to CITATION.cff on GitHub release
  ingestion. After v2026.04.1, .2, and .3 all returned "Citation
  metadata load failed" with distinct error_ids despite cffconvert
  validating the CITATION.cff against CFF schema 1.2.0, switching to the
  native format bypasses whatever parser quirk was hitting the CFF path.

## [2026.04.3] — 2026-04-13

### Fixed
- CITATION.cff: removed `license-url` field and the machine-readable
  `references:` block. Zenodo's GitHub release ingestion continued to
  return "Citation metadata load failed" on v2026.04.1 and v2026.04.2
  despite the file validating cleanly under cffconvert 2.0 against CFF
  schema 1.2.0. Empirically, Zenodo's parser accepts only a minimal
  subset of CFF fields. The upstream Weiskittel et al. 2011 citation is
  preserved in README.md and the v3 manuscript.

## [2026.04.2] — 2026-04-13

### Fixed
- `src-converted/base/CONTRL.f90`: stripped CRLF line terminators and
  converted fixed-form continuation (`     &` in column 6) to free-form
  trailing `&`. The file is consumed by 40+ base modules via `INCLUDE`,
  so the defect cascaded to `volkey.f90` and `evldx.f90` under the
  `gfortran -ffree-form` CI sanity check.
- `src-converted/base/errgro.f90`: renamed legacy `INCLUDE 'PRGPRM.F77'`,
  `'CONTRL.F77'`, and `'PLOT.F77'` references to the actual `.f90`
  include targets in the tree, and applied the same CRLF and
  continuation fixes as CONTRL.
- CI `syntax-check` job in `.github/workflows/ci.yml` now passes on all
  six sanity-check base files (main, myopen, volkey, algevl, evldx,
  errgro). Preexisting CI red since the PR #118 upstream sync is
  cleared.

## [2026.04.1] — 2026-04-13

### Fixed
- CITATION.cff `license` field switched from a list (`MIT`, `CC0-1.0`) to a
  single SPDX identifier (`MIT`) plus `license-url`. Zenodo's CITATION.cff
  ingestion rejected the list form with "Citation metadata load failed" on
  the v2026.04.0 release, preventing DOI minting. Dual-license intent is
  preserved in the adjacent comment block and the LICENSE file.

## [2026.04.0] — 2026-04-13

### Added
- Bayesian calibration pipeline producing posterior distributions for seven
  component models across 25 regional variants.
- UncertaintyEngine with 500 posterior draws per component preserving
  within-draw parameter correlations.
- Python ctypes wrapper (`fvs2py`) and FastAPI REST interface (`microfvs`).
- Turnkey deployment for Docker, AWS, Fedora/RHEL, macOS, and WSL2.
- Upstream sync tooling (`check_upstream.sh`, `sync_upstream.sh`) with
  per-commit traceability.
- Code of Conduct, changelog, and funding metadata.

### Changed
- Synchronized 74 Fortran files from upstream commit 66a5e14e (PR #118),
  adding SDI reporting updates, modern gfortran FORMAT descriptor fixes,
  Reineke diameter plumbing in CONTRL globals, and NSVB decay class = 3.
- Dual-licensed release: CC0-1.0 for original USDA FVS Fortran source,
  MIT for new Python/R/deployment code. CITATION.cff reflects the SPDX list.

### Fixed
- FORMAT descriptor regressions (`A8'`, `A10'`, `I3'`) that broke under
  gfortran 15.2; patches upstreamed in PR #118 so local sed hooks are
  retired in favor of a no-op stub.
- Untracked file triage: 58 stale top-level variant directories, PERSEUS
  and Bakuzis output, and ephemeral slide exports are now gitignored.

### Known Issues
- Standalone `iet03` keyword file segfaults (1 of 67 regression tests).
  Root cause under investigation; does not affect library load or rFVS
  `.Fortran()` path.

## [2025.12] — initial public draft

### Added
- Free-form Fortran 90 conversion of 2,247 source files across base plus
  25 regional variants and optional extension modules (fire, estb, volume).
- Regression harness comparing library loads and keyword-file runs against
  upstream baselines.

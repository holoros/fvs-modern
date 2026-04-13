# Changelog

All notable changes to fvs-modern are documented in this file. The format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this
project adheres to calendar-based versioning (YYYY.MM).

## [Unreleased]

### Planned
- Resolve ACD variant build state on cardinal.osc.edu using
  `deployment/scripts/diagnose_acd_cardinal.sh` and promote ACD from
  advisory to supported in the next tag.
- Root-cause the `iet03` FFE segfault under gdb and, if reproducible
  upstream, file an Open-FVS issue.
- Add CONUS unified variant based on ORGANON growth forms
  (`conus-variant` branch with Greg Johnson).
- Ingrowth submodel and crown ratio revision for the next calendar tag.

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

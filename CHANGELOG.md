# Changelog

All notable changes to fvs-modern are documented in this file. The format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this
project adheres to calendar-based versioning (YYYY.MM).

## [Unreleased]

### Planned
- Deploy PR #118 Fortran baseline to OSC Cardinal once current PERSEUS
  uncertainty ensemble completes.
- Add CONUS unified variant based on ORGANON growth forms.

## [2026.04] — 2026-04-13

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

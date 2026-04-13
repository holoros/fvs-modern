# fvs-modern 2026.04.0 — First Public Tag

Release date: 2026-04-13
Tag: `v2026.04.0`

## Summary

This is the first public tag of `fvs-modern`, a community-maintained,
dual-licensed fork of the USDA Forest Vegetation Simulator that adds a
Bayesian calibration pipeline and uncertainty quantification on top of a
Fortran 90 free-form conversion of the full 2,247-file FVS codebase. The
tag serves as the reproducibility anchor for the v3 manuscript and the
baseline collaborators on Cardinal and Greg Johnson's CONUS work can pull.

## Highlights

- Fortran 90 free-form conversion of base plus 25 US and Canadian regional
  variants and the fire, estb, and volume extension modules.
- Upstream sync through PR #118 (commit 66a5e14e), covering SDI reporting
  updates, modern gfortran FORMAT descriptor fixes, Reineke diameter
  plumbing in CONTRL globals, and NSVB decay class = 3. Local sed patches
  are retired in favor of the upstream fixes.
- Bayesian calibration pipeline producing posterior distributions for seven
  component models across 25 variants, with Wykoff ln(DDS) basal area R²
  improving from 0.672 to 0.699 across the fleet.
- PERSEUS uncertainty projection pipeline driving ensemble projections of
  500 posterior draws per component, preserving within-draw parameter
  correlations for Maine-wide runs backing the v3 manuscript.
- Turnkey deployment: Docker compose on Ubuntu 24.04, Fedora/RHEL systemd
  installer, macOS Homebrew flow, WSL2 setup, fvs2py ctypes bindings, and
  the microfvs FastAPI REST interface.
- Dual licensing: CC0-1.0 for original USDA FVS Fortran source, MIT for
  new Python, R, and deployment code. CITATION.cff is SPDX-aligned.
- Governance: Code of Conduct, Contributing guide, CHANGELOG, and upstream
  sync tooling (`check_upstream.sh`, `sync_upstream.sh`) with per-commit
  traceability.

## Regression status

66 of 67 standalone keyword-file regression tests pass (98.5%). The one
failure is the long-standing `iet03` segfault, triaged in
`KNOWN_ISSUES.md` as an FFE snag and fuels initialization issue seeded
without a TreeInit tree list. Library load tests, fvs2py ctypes imports,
and the rFVS `.Fortran()` path are green across all 25 built variants.

## Known issues

- `iet03` segfault: preexisting in upstream, xfail marked here. Planned
  investigation under gdb and upstream Open-FVS issue in the next cycle.
- ACD variant builds clean in the workspace but has been reported unstable
  on cardinal.osc.edu. `deployment/scripts/diagnose_acd_cardinal.sh`
  captures source hashes, PRGPRM include resolution, and compiler flags
  so the discrepancy can be pinned without another round trip. This tag
  ships FVSacd.so as advisory; users on Cardinal should run the diagnostic
  before depending on ACD output.

## How to cite

See `CITATION.cff` at the repository root, plus the v3 manuscript
preprint reference (to be updated with the final DOI once the Zenodo
integration mints one for this tag).

## Acknowledgments

USDA Forest Service for the original FVS source and ongoing maintenance.
University of Maine Center for Research on Sustainable Forests (CRSF)
and the NSF I/UCRC Center for Advanced Forestry Systems (CAFS) for
institutional support. Ohio Supercomputer Center for the Cardinal
allocation that ran the calibration fleet. Greg Johnson for ongoing
collaboration on the CONUS unified variant, slated for a later tag.

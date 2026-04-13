# Release Plan: fvs-modern 2026.04 (first public tag)

Date drafted: 2026-04-13
Owner: Aaron Weiskittel
Status: Draft for sign-off

## Goal

Cut the first public tag of `fvs-modern` now that pre-release cleanup is
merged and the PERSEUS uncertainty ensemble that backs the v3 manuscript is
running. The tag acts as a reproducibility anchor for the v3 manuscript and
as the baseline collaborators (Greg Johnson, Cardinal users) can pull.

## Proposed version

`v2026.04.0`, matching the calendar-based scheme already used in
`CHANGELOG.md`. Subsequent patch releases before the next monthly cut go to
`v2026.04.1`, `v2026.04.2`, etc.

## Gate checks before tagging

1. `git status` clean on `main` and `origin/main` in sync. (Done 2026-04-13
   after push of commits `c217d10..68f39cb`.)
2. `bash deployment/scripts/run_regression_tests.sh` returns 66/67 with the
   documented `iet03` xfail. Run once more from a clean checkout to confirm
   reproducibility on the release commit.
3. `bash deployment/scripts/build_fvs_libraries.sh` produces all 25 `.so`
   files on the laptop and (separately) on Cardinal. ACD is allowed to be
   waived for this tag provided `KNOWN_ISSUES.md` is updated with the
   Cardinal diagnostic summary.
4. `pytest` in `deployment/fvs2py/` green.
5. License, CITATION.cff, CODE_OF_CONDUCT, CONTRIBUTING present at repo root.
6. CHANGELOG `[Unreleased]` section emptied; `[2026.04]` heading receives the
   tag date.

## Tag and release steps

```bash
# From a clean checkout on main at 68f39cb (or later)
git switch main
git pull --ff-only
git tag -a v2026.04.0 -m "fvs-modern 2026.04.0: first public tag"
git push origin v2026.04.0

# Then create the GitHub release
gh release create v2026.04.0 \
  --title "fvs-modern 2026.04.0" \
  --notes-file docs/release_v2026_04_notes.md
```

## Zenodo / DOI

If a DOI is wanted for the manuscript citation, enable the Zenodo GitHub
integration on `holoros/fvs-modern` before pushing the tag. Zenodo will mint
a DOI automatically from the tagged release and the CITATION.cff metadata.

## Release note outline (to be drafted separately)

- Summary: modernized, dual-licensed FVS with Bayesian calibration and
  uncertainty quantification.
- Highlights: PR #118 upstream sync, PERSEUS pipeline, 66/67 tests passing,
  25 variant .so build.
- Known issues: `iet03` segfault (see `KNOWN_ISSUES.md`), ACD build needs
  Cardinal verification (see `deployment/scripts/diagnose_acd_cardinal.sh`).
- How to cite: CITATION.cff plus manuscript preprint reference.
- Acknowledgments: USDA, CAFS, CRSF, OSC Cardinal allocation.

## Deferred to a later tag

- CONUS unified variant (Greg Johnson collaboration) will land on the
  `conus-variant` branch and be released separately.
- Ingrowth submodel and crown ratio revision scheduled for the next calendar
  tag.
- `iet03` root-cause fix. Tag does not depend on it.

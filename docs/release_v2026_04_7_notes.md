# fvs-modern 2026.04.7 — Metadata Hardening

Release date: 2026-04-21
Tag: `v2026.04.7`

## Summary

A small follow-up tag to v2026.04.6 that corrects stale citation metadata,
hardens `.gitignore` against a set of scratch tooling that accumulated at
the repo root during parallel manuscript and calibration work, and
documents the outstanding Zenodo webhook enablement as an operational
action. No changes to the Fortran sources, calibration pipeline,
deployment scripts, CI workflows, or API surface.

## Changes

### CITATION.cff bumped to 2026.04.7

The v2026.04.6 tag shipped with `CITATION.cff` still reading
`version: "2026.04.5"` and `date-released: "2026-04-13"`. The slip was
harmless for Zenodo ingestion (Zenodo prefers `.zenodo.json` since
v2026.04.4 and derives the version from the tag name) but misleading
for anyone who renders the GitHub "Cite this repository" widget or
runs cffconvert against the file. v2026.04.7 updates both fields.

### .gitignore hardening

Six new patterns added to keep scratch work out of the public tree:

- `src-converted/tests/**/*.sng` and `*.sum`: FFE and standard summary
  outputs regenerated on each keyword-file regression run.
- `/package.json`, `/package-lock.json`, `/node_modules/`: Node.js
  tooling accidentally initialized at the repo root during PNAS v6
  response preparation. These belong in a separate manuscript repo.
- `/apply_v6_patches.gs` and `/build_letters.js`: Google Apps Script
  and Node.js scratch for PNAS v6 figure and table restyling.
- `/pnas_v6_figures/`: empty placeholder directory from the same
  workflow.
- `docs/perseus_projection_status_*.md` and `docs/*_status_*.md`:
  dated internal status notes that reference Cardinal account codes
  (PUOM0008), unpublished projection results, and manuscript draft
  numbers. Not appropriate for the public source tree.

None of the newly ignored paths were tracked; this is pure defense
against future accidental commits of the same patterns.

### RELEASE_READINESS_2026-04-21.md corrected

Replaced the "Latest published tag is v2026.04.5" line (stale the
moment v2026.04.6 pushed on 2026-04-21) with a reference to v2026.04.6
and a pointer to v2026.04.7. Replaced the Zenodo ingestion sentence
with a pointer to the operational action described below.

## Outstanding operational action: enable Zenodo webhook

A comprehensive audit on 2026-04-21 queried the Zenodo public API
three ways (title match on "fvs-modern", creator match on Weiskittel,
related-identifier match on the GitHub URL) and found zero fvs-modern
records. Despite the narrative in the CHANGELOG suggesting v2026.04.4
and v2026.04.5 successfully minted DOIs, Zenodo has never ingested
any tag of this repo.

The root cause is almost certainly that the Zenodo GitHub integration
was never enabled in the Zenodo UI for this repository. This is a
per-repo opt-in that requires:

1. Log into `https://zenodo.org/` with the GitHub account that owns
   or has admin access to `holoros/fvs-modern` (i.e., @holoros).
2. Visit `https://zenodo.org/account/settings/github/`.
3. Locate `holoros/fvs-modern` in the repository list (click **Sync now**
   if it does not appear; Zenodo only lists repos visible to the
   authenticated GitHub user).
4. Toggle the switch from OFF to ON.

Once the toggle is on, the next tag to GitHub triggers Zenodo's
webhook, which downloads the source tarball, reads `.zenodo.json`
for metadata, and mints a DOI. The concept DOI is stable across
versions; each version receives its own version DOI.

Past tags (v2026.04.0 through v2026.04.6) do not backfill
automatically. If version DOIs for those historical tags are desired,
download the source tarball for each past tag from GitHub
(`https://github.com/holoros/fvs-modern/archive/refs/tags/<tag>.tar.gz`)
and upload manually through the Zenodo UI, once per version. For most
projects this level of historical fidelity is unnecessary — the
concept DOI from the first ingested tag forward is sufficient for
citation purposes.

## Regression status

Unchanged from v2026.04.6. All 25 shared library builds green. 68/68
regression tests pass. ACD advisory closed.

## Planned for next tag

No new work is planned specifically for a v2026.04.8. The next tag
should be a calendar-versioned minor bump (for example, v2026.05.0)
once one or more of the following lands:

- iet03 numeric baseline refresh (closes GitHub issues #3 and #5).
- `FMCOM.f90` self-sufficient under `-fimplicit-none`.
- CONUS unified variant merges from the `conus-variant` branch.
- Additional executable build coverage beyond NE, ACD, CS, LS.
- Ingrowth submodel and crown ratio revision.

## How to cite

See `CITATION.cff` at the repository root; Zenodo mints a DOI on tag
publication once the webhook is enabled.

## Acknowledgments

Same as v2026.04.6.

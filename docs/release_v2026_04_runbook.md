# Runbook: Cutting `v2026.04.0`

Companion to `docs/release_v2026_04_plan.md`. This is the sequence to run
on Aaron's laptop (and separately on Cardinal for the build check). Each
step is a gate — stop and triage if it fails rather than pushing past it.

## 0. Prereqs

```bash
cd ~/CRSF-Cowork/fvs-modern  # or wherever the working copy lives
git switch main
git pull --ff-only
git status                   # must be clean
```

## 1. Build check on laptop

```bash
rm -rf lib/*.so bin
bash deployment/scripts/build_fvs_libraries.sh
ls lib/FVS*.so | wc -l       # expect 25 (24 if ACD is waived)
ls bin/FVS* | wc -l           # standalone executables for regression tests
```

Record any variant that fails to build. ACD is allowed to fail for this
tag; every other variant must produce both a `.so` and a standalone
executable.

## 2. Regression suite

```bash
bash deployment/scripts/run_regression_tests.sh ./bin ./src-converted/tests
```

Expected: 66 passed, 1 skipped/xfail (`iet03`). If any other test fails,
do not tag.

## 3. fvs2py unit tests

```bash
cd deployment/fvs2py
pytest -q
cd -
```

Expect green.

## 4. Cardinal build check

From the laptop:

```bash
ssh crsfaaron@cardinal.osc.edu
cd /users/PUOM0008/crsfaaron/fvs-modern
git pull --ff-only
bash deployment/scripts/build_fvs_libraries.sh
bash deployment/scripts/diagnose_acd_cardinal.sh > /tmp/acd_cardinal.log 2>&1
```

Scp the ACD diagnostic log back to the laptop and paste the summary into
`KNOWN_ISSUES.md` under the ACD section before tagging.

## 5. Zenodo decision

Before tagging, decide whether to mint a DOI for the v3 manuscript
citation:

- If yes: enable the Zenodo GitHub integration on `holoros/fvs-modern`
  (Zenodo → Settings → Repositories → toggle `fvs-modern` on). Do this
  before pushing the tag so Zenodo captures the release.
- If no: note it and defer to the next tag.

## 6. Tag and push

```bash
git switch main
git pull --ff-only
git tag -a v2026.04.0 -m "fvs-modern 2026.04.0: first public tag"
git push origin v2026.04.0

gh release create v2026.04.0 \
  --title "fvs-modern 2026.04.0" \
  --notes-file docs/release_v2026_04_notes.md
```

## 7. Post-tag

- If Zenodo minted a DOI, add it to `CITATION.cff` under `doi:` and
  update the v3 manuscript citation.
- Open GitHub issues for the deferred items in the CHANGELOG `[Unreleased]`
  section so they have trackers before they fall off the radar.
- Announce on the CAFS mailing list and the FVS open-fvs discussion list.

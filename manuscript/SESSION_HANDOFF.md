# Session Handoff — 2026-04-26 (final update)

## Status: Manuscript submission-ready. DOI minted. v3 self-review complete.

Three releases shipped this session, one DOI minted, six targeted
manuscript revisions applied during v3 self-review, manuscript fully
updated with the citable archive. Local main at commit `4c8ec62`.

```
4c8ec62 Manuscript review v3: six targeted revisions plus self-review docx
c1b430f Update SESSION_HANDOFF for submission-ready state with DOI minted
d91227a Insert minted Zenodo DOI 10.5281/zenodo.19802673 across artifacts
9ba70af Simplify .zenodo.json to fix Zenodo InvenioRDM ingestion
6a71a67 Update SESSION_HANDOFF with v2026.05.2 tag
59612ab Release prep v2026.05.2: PN/SN/IE library build infrastructure
4da5168 Strengthen sections 5.4 and 5.5 with PN/SN/IE infrastructure work
d017700 Session wrap-up: FVS-PN/SN/IE all load, runtime EOF is final blocker
07ef4a8 Repair eccalc.f90 and ecin.f90 conversion bugs
98b9fb6 Repair more econ F77->F90 conversion bugs
8faf40b Fix FVS-PN library load: build include order + econ conversion bugs
245875d Release prep v2026.05.1: FIA-stand Bakuzis revision
4de8467 Apply FIA-stand Bakuzis results to manuscript section 4.6
```

Three GitHub Releases published, all visible on the repo:
- v2026.05.1 — FIA-stand Bakuzis evaluation
- v2026.05.2 — FVS-PN/SN/IE library build infrastructure recovery
- v2026.05.3 — Zenodo InvenioRDM metadata fix (this is the one with a DOI)

## DOI minted

**`10.5281/zenodo.19802673`** — resolves to the v2026.05.3 archive at
https://doi.org/10.5281/zenodo.19802673

Inserted in:
- `manuscript/fvs_combined_draft.md` Software availability block
- `manuscript/fvs_combined_draft.md` section 5.3 Reproducibility
- `CITATION.cff` identifiers block
- `README.md` Zenodo DOI badge

The manuscript is now complete and submission-ready.

## Headline scientific findings (FIA-mode Bakuzis)

Bakuzis biological law compliance at year 100, real FIA stands,
33 of 36 cells per variant:

|  Variant | Config         | Sukachev | Eichhorn | Density mortality |
|----------|----------------|---------:|---------:|------------------:|
|  ACD     | default        | 1.00     | 0.20     | 0.70              |
|  ACD     | calibrated MAP | 1.00     | 0.20     | 0.70              |
|  ACD     | posterior band | 0.90     | **0.50** | 0.70              |
|  NE      | default        | 0.00     | 0.44     | 1.00              |
|  NE      | calibrated MAP | 0.00     | 0.44     | 1.00              |
|  NE      | posterior band | 0.00     | 0.22     | 1.00              |

Headline: ACD posterior 50% Eichhorn vs 20% point estimates,
corroborated on real FIA stands. NE Sukachev 0% confirmed as a
variant property (not synthetic artifact).

## What got built this session (sequential summary)

1. **FIA-mode Bakuzis pipeline**: SLURM array on Cardinal,
   five real FIA plots per cell, 100 posterior draws, NE plus ACD
   variants. Fixed sawtooth bug from inventory-year mismatch via
   horizon-quantization in aggregator + `inv_year=2000` override
   in `fia_stand_generator.py`.

2. **Manuscript section 4.6 rewritten** with FIA numbers; sections
   3.3, 5.4, 5.5 updated to match. 7,255 → 7,617 words (after v3
   revisions). v2 self-review re-scored 3.7 → 4.5; v3 self-review
   scored 4.7/5.0 with recommendation Accept.

3. **FVS-PN/SN/IE library symbol resolution**: build script
   include-order fix in `deployment/scripts/build_fvs_libraries.sh`
   (variant-specific paths before `-Ibase`); seven F77→F90
   conversion-bug repairs in `src-converted/econ/` (ecvol.f90,
   ecinit.f90, echarv.f90, ecstatus.f90, eccalc.f90, ecsetp.f90,
   ecin.f90); twelve undefined symbols recovered. FVS-PN, FVS-SN,
   FVS-IE now build cleanly and load via Python ctypes.

4. **Marshall format adapter** `calibration/python/marshall_to_fia_csv.py`
   converts state-level FIA exports to standard FIA DataMart
   format. Used to convert OR, WA, AL, FL, GA, MS, SC, TN, ID, MT
   on Cardinal at `~/fia_data/`.

5. **Zenodo metadata fix**: identified InvenioRDM rejection of
   `creators[].orcid` translation. Simplified `.zenodo.json` to
   minimal valid metadata (no orcid, no related_identifiers).
   Result: DOI minted on first try.

6. **v3 self-review with six targeted revisions** (commit `4c8ec62`):
   R1: Abstract reframed "three of four" biological laws (not four).
   R2: §2.2 added posterior draw subsampling rationale (500 from 4,000).
   R3: §4.4 differentiated bootstrap envelope from posterior credible band.
   R4: §5.3 distinguished synthetic vs FIA-mode Bakuzis compute costs.
   R5: §3.1 added explicit site index treatment (SICOND, base age 50).
   R6: §3.1 cited Reynolds (1984) for equivalence metric foundation.
   v3 score: 4.7/5.0; recommendation: Accept.

## Open at the runtime layer

FVS-PN/SN/IE shared libraries load via ctypes successfully but the
Fortran runtime hits an EOF in `base/keyrdr.f90` line 47 when
reading any keyword file, including the canonical USDA upstream
`pnt01.key`. The standalone `lib/FVSpn` executable also reports
`NO STOP RECORD; RECORDS READ=0`. This is a deeper F77-to-F90
conversion regression in keyword-reader I/O semantics specific to
non-eastern variants. Documented in
`calibration/python/PN_SN_LIBRARY_DIAGNOSIS.md`. Beyond the scope
of a single sandbox session; needs USDA reference binary
comparison.

The Marshall format adapter and the SLURM template for PN/SN/IE
Bakuzis (`calibration/slurm/submit_bakuzis_fia_pnsnie.sh` on
Cardinal) are both ready to run once the runtime regression is
resolved.

## Critical paths

```
Repo root:            /sessions/cool-epic-bardeen/mnt/fvs-modern
GitHub:               git@github.com:holoros/fvs-modern.git
Cardinal:             crsfaaron@cardinal.osc.edu:fvs-modern/
Zenodo concept DOI:   10.5281/zenodo.19802673
SSH key (Cardinal):   /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519_cardinal
SSH key (GitHub):     /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519
```

## Submission checklist (manuscript)

- [x] Section 4.6 with real FIA Bakuzis numbers
- [x] Section 3.3 methods describes FIA stratified sampling path
- [x] Section 5.4 limitations honestly frames PN/SN/IE state
- [x] Section 5.5 future work lists runtime regression as item six
- [x] CRediT contributor statement
- [x] All six 2025 citations inserted (Crookston-Dixon, Itter, Joo,
      Premer, Crookston-Rehfeldt, Woodall)
- [x] Equation forms numbered (M1)
- [x] Spatial autocorrelation note (S2b)
- [x] Random seeds stated (G3)
- [x] Climate-FVS reference (GY5)
- [x] Comparative positioning (GY6)
- [x] Software availability with resolvable Zenodo DOI
- [x] CITATION.cff with DOI identifier
- [x] README with Zenodo badge
- [x] Site index treatment specified (R5, v3)
- [x] Posterior draw subsampling rationale (R2, v3)
- [x] Bootstrap vs posterior uncertainty distinction (R3, v3)
- [x] Synthetic vs FIA-mode compute cost distinction (R4, v3)
- [x] Abstract biological law claim corrected to three of four (R1, v3)
- [x] Reynolds (1984) cited in body (R6, v3)
- [ ] Co-author final review (after distribution)

Manuscript is ready for co-author distribution and EMS submission.

## Files of record

- `manuscript/fvs_combined_draft.md` and `.docx` (7,617 words, 11 figures)
- `manuscript/fvs_combined_SI.md` and `.docx`
- `manuscript/20260426_ems_self-review_fvs-modern_v3.md` and `.docx`
  (v3 self-review at 4.7/5.0; recommendation Accept; six revisions applied)
- `manuscript/20260426_ems_self-review_fvs-modern_v2.docx` (v2 self-review, superseded)
- `RELEASE_NOTES_v2026.05.1.md`
- `RELEASE_NOTES_v2026.05.2.md`
- `calibration/python/PN_SN_LIBRARY_DIAGNOSIS.md`

## SSH command patterns

Cardinal:
```
ssh -F /dev/null -i /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519_cardinal \
  -o UserKnownHostsFile=/dev/null -o GlobalKnownHostsFile=/dev/null \
  -o StrictHostKeyChecking=no crsfaaron@cardinal.osc.edu "<cmd>"
```

GitHub push:
```
GIT_SSH_COMMAND="ssh -F /dev/null -i /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519 \
  -o UserKnownHostsFile=/dev/null -o GlobalKnownHostsFile=/dev/null \
  -o StrictHostKeyChecking=no" git push origin main

# Session Handoff — 2026-04-25 20:11 EDT

## Status: FIA Bakuzis pipeline complete and pushed

The Bakuzis uncertainty SLURM array (job 8847841) ran end-to-end with
the FIA-mode runner. Aggregator and figures regenerated, manuscript
section 4.6 updated with real numbers, all changes committed and
pushed to `origin/main` as commit `4de8467`.

```
4de8467 Apply FIA-stand Bakuzis results to manuscript section 4.6
76b85de Add FIA-derived stand generator and INVENTORY keyfile pathway
89ecf0b v2026.05.0: Bakuzis uncertainty pipeline, real posterior runs, combined manuscript
```

Local main and origin main both at `4de8467`. Verified with
`git ls-remote origin main`.

## Key findings (FIA-mode, 33 of 36 cells populated per variant)

Bakuzis biological law compliance at year 100:

|  Variant | Config         | Sukachev | Eichhorn | Density mortality |
|----------|----------------|---------:|---------:|------------------:|
|  ACD     | default        | 1.00     | 0.20     | 0.70              |
|  ACD     | calibrated MAP | 1.00     | 0.20     | 0.70              |
|  ACD     | posterior band | 0.90     | **0.50** | 0.70              |
|  NE      | default        | 0.00     | 0.44     | 1.00              |
|  NE      | calibrated MAP | 0.00     | 0.44     | 1.00              |
|  NE      | posterior band | 0.00     | 0.22     | 1.00              |

Headline finding: ACD posterior achieves 50 percent Eichhorn vs 20
percent point estimates, a 2.5x uplift now corroborated on real FIA
stands. NE Sukachev 0 percent confirmed as variant property, not
synthetic artifact. Year 100 BA divergence on real stands is small
(within plus or minus 1 percent for most cells); only Oak Pine high
high reaches -9.6 percent.

## What got committed in `4de8467` (17 files changed)

Code:
- `calibration/python/fia_stand_generator.py` — INVYR forced to 2000
- `calibration/python/bakuzis_uncertainty_aggregate.py` — replicate
  pooling, horizon alignment with 5 year quantization
- `calibration/python/bakuzis_uncertainty_figures.py` — auto per
  variant rendering (NE and ACD both rendered without flag)
- `calibration/python/run_bakuzis_post_array.sh` — new wrapper
- `calibration/output/bakuzis/*.csv` — FIA mode summaries
- `calibration/output/comparisons/manuscript_figures/fig_bakuzis_*.png`
  (6 files) — regenerated with horizon alignment

Manuscript:
- `manuscript/fvs_combined_draft.md` — section 3.3, 4.6, 5.4 updated
- `manuscript/fvs_combined_draft.docx` — regenerated (7,255 words,
  11 images, 173 paragraphs)
- `manuscript/fvs_combined_SI.md` — M2 calibration architecture note
  (carried from prior session)
- `manuscript/section_4_6_fia_update_template.md` — anticipatory
  template (now applied)

## Untracked but uncommitted

```
?? PNAS_v6_Results_P1_revision.md
?? cardinal_temporal/                            # binary work
?? manuscript/20260425_ems_self-review_fvs-modern.docx
?? manuscript/20260425_ems_self-review_fvs-modern.md
?? manuscript/C2_FIA_GENERATOR_OUTLINE.md
?? manuscript/PNAS_v6_review_memo.md
?? calibration/patches/
 M manuscript/SESSION_HANDOFF.md       (this file)
 M manuscript/fvs_combined_SI.docx     (auto-regenerated)
```

The SI.docx update can be regenerated anytime via
`cd manuscript && pandoc fvs_combined_SI.md -o fvs_combined_SI.docx`.
The other untracked files are working notes from prior sessions.

## Remaining work for submission readiness

1. **Run Module 5 self-review again** on the revised manuscript. The
   prior self-review docx is at
   `manuscript/20260425_ems_self-review_fvs-modern.docx`.
   Expected score shifts:
   - D6 validation: 4 -> 5 (real FIA replicates remove synthetic stand caveat)
   - D8 interpretation: 4 -> 5 (real-stand Bakuzis is defensible)
   - Overall recommendation: Minor Revision -> probable Accept,
     pending D5 (Zenodo DOI)

2. **Zenodo webhook** — enable the GitHub-Zenodo integration so the
   next release tag mints a DOI. Manual step requiring
   github.com/holoros/fvs-modern -> Settings -> Integrations.

3. **Tag a release** (e.g., v2026.05.1) once Zenodo webhook is active
   so a DOI lands for citation in the manuscript Reproducibility
   section.

4. **Optional: extend Bakuzis to PN, SN, IE** by either downloading
   per state CSVs from FIA DataMart or converting consolidated CONUS
   RDS to per state CSVs. The FIA generator and runner support this
   without code changes.

5. **Optional: USDA reference binary diff** — the FVS-PN/SN/IE shared
   libraries built April 25 reject all input via fvs2py (canonical
   pnt01.key fails with RECORDS READ=0). This is a build script
   regression specific to non eastern variants, not a calibration
   issue. Diagnosing requires comparing against a USDA reference
   binary or stepping through the build script with verbose flags.

## Critical paths and file locations

```
Repo root:        /sessions/cool-epic-bardeen/mnt/fvs-modern
GitHub:           git@github.com:holoros/fvs-modern.git
Cardinal:         crsfaaron@cardinal.osc.edu:fvs-modern/
SSH key (Cardinal): /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519_cardinal
SSH key (GitHub):   /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519
```

Key Bakuzis files:
- `calibration/python/bakuzis_uncertainty_comparison.py` — main runner
- `calibration/python/fia_stand_generator.py` — FIA stratified sampler
- `calibration/python/bakuzis_uncertainty_aggregate.py` — replicate
  and draw pooling, horizon alignment
- `calibration/python/bakuzis_uncertainty_figures.py` — matplotlib
  figures
- `calibration/python/run_bakuzis_post_array.sh` — Cardinal post
  array wrapper
- `calibration/slurm/submit_bakuzis_fia.sh` — SLURM template (on
  Cardinal at `~/fvs-modern/calibration/slurm/`)

Manuscript files:
- `manuscript/fvs_combined_draft.md` — main EMS manuscript markdown
- `manuscript/fvs_combined_draft.docx` — pandoc output
- `manuscript/fvs_combined_SI.md` and `.docx` — supplementary material
- `manuscript/section_4_6_fia_update_template.md` — historical
  template (preserved for provenance, now applied)

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
```

## Aggregator algorithm notes (for future revisits)

The aggregator now handles two stand modes transparently:

Synthetic mode (no replicate column): one stand per cell. Default and
calibrated MAP rows pass through with sd=0; posterior groups across
draws to compute median and quantile bands.

FIA mode (replicate column present): up to 5 stands per cell.
- Each (variant, scenario, replicate, config, draw_id) combination
  has its own start year (FIA INVYR varies). Horizon is computed
  per replicate, then quantized to the nearest 5 year cycle so plots
  with INVYRs 2010, 2012, 2014 etc all align on horizons 0, 5, 10.
- Year column is reset to 2000 + horizon so downstream code
  (figures, benchmark table) works without modification.
- Default and calibrated MAP rows are aggregated across replicates
  so each cell yields a single median plus an SD across plots.
- Posterior rows are pooled across the cross product of draws and
  replicates (5 plots x 100 draws = 500 values per cell year), so
  the credible band reflects both parameter and stand to stand
  uncertainty.

The FIA generator forces inv_year=2000 for all sampled stands so
the calendar year axis aligns even before the aggregator's
horizon adjustment, providing belt-and-suspenders alignment.

## Open file paths preserved for next session

If a future session needs to re-derive:
- Raw FIA Bakuzis ensemble CSVs (Cardinal):
  `~/fvs-modern/calibration/output/bakuzis/bakuzis_uncertainty_{ne,acd}_n100_batch{00..05}.csv`
- Aggregated summaries (Cardinal and sandbox):
  `calibration/output/bakuzis/bakuzis_uncertainty_summary_long.csv`
  `calibration/output/bakuzis/bakuzis_benchmark_wide.csv`
  `calibration/output/bakuzis/bakuzis_laws_compliance.csv`
- Tarball pulled from Cardinal: `/tmp/bakuzis_fia_summaries.tar.gz`

## Provenance trail of this session

- Start: commit 76b85de pushed, FIA generator landed, array submitted
- Job 8847841 array submitted 19:24 EDT, completed 20:00 EDT
  (~36 min wall clock per array task, six tasks in parallel)
- Aggregator audited and patched for replicate dimension
  (load_ensemble keep list, summarize_ensemble pooling)
- Figures script extended for per variant rendering
- First aggregator run on Cardinal flagged sawtooth trajectories
  caused by FIA INVYR variation across replicates
- Second aggregator iteration added horizon alignment with 5 year
  quantization; trajectories cleaned up
- FIA generator patched to force inv_year=2000 for future runs
- Manuscript section 4.6, 3.3, 5.4 updated with FIA numbers and
  variant coverage caveats
- pandoc regenerated `fvs_combined_draft.docx` (7,255 words)
- Commit `4de8467` pushed to origin/main, verified via ls-remote

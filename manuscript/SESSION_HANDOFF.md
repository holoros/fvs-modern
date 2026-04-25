# Session Handoff — 2026-04-25 07:00 EDT

## Resume Instructions

Resume the fvs-modern v2026.04.7 follow up work. The Bakuzis uncertainty
pipeline is built, executed on Cardinal (job 8828820), and results are
back in the workspace. The combined Environmental Modelling and Software
manuscript and supplementary information are drafted with real posterior
figures embedded. The Perseus future SI and harvest scripts have been
refactored to env var conventions. Nothing has been git committed since
`b78e6e7` (Bakuzis pipeline), and nothing has been pushed to GitHub from
the sandbox. Next: stage one consolidated commit covering the Bakuzis
output, manuscript directory, and Perseus script refinements, then push
from your laptop and optionally tag as v2026.05.0.

## Project Context

- **Project:** fvs-modern (open source modernized FVS distribution)
- **Working directory:** `/home/aweiskittel/fvs-modern` (mounted in sandbox at `/sessions/cool-epic-bardeen/mnt/fvs-modern`)
- **Repository:** https://github.com/holoros/fvs-modern
- **Latest published tag:** v2026.04.7 (April 21, 2026)
- **Local commit ahead of origin:** `b78e6e7` (Bakuzis matrix uncertainty pipeline)
- **Related skills:** crsf-workspace, hpc-cardinal, forestry-modeling, manuscript-preparer, docx, pptx
- **Cardinal account:** PUOM0008, user crsfaaron
- **Co-authors on the EMS manuscript:** Aaron Weiskittel, Greg Johnson (GMUG), David Marshall (USDA FS PNW)

## Completed This Session

### Bakuzis uncertainty pipeline (committed at b78e6e7, transferred to Cardinal, executed)

1. `calibration/python/bakuzis_uncertainty_comparison.py` — runner, 36 scenarios x (1 default + 1 calibrated MAP + N posterior draws) per variant, with SLURM array batching
2. `calibration/python/bakuzis_uncertainty_aggregate.py` — collapses ensemble to summary long, benchmark wide, and law compliance tables
3. `calibration/python/bakuzis_uncertainty_figures.py` — matplotlib figures for both NE and ACD variants
4. `calibration/python/bakuzis_preview_figures.py` — preview figures from existing point estimate CSV
5. `calibration/R/17_bakuzis_uncertainty_figures.R` — publication ggplot2 figures
6. `calibration/slurm/submit_bakuzis_uncertainty.sh` — SLURM array template
7. `calibration/BAKUZIS_UNCERTAINTY_README.md` — workflow documentation

### Cardinal execution (job 8828820, completed 2026-04-24 21:59 EDT)

- 6 array tasks, all COMPLETED with exit code 0:0, 5 min 19 sec wall clock each
- Total compute: 7,344 FVS invocations across NE and ACD variants
- Output files synced back to sandbox:
  - 12 batch CSVs at `calibration/output/bakuzis/bakuzis_uncertainty_{ne,acd}_n100_batch{00..05}.csv`
  - `bakuzis_uncertainty_summary_long.csv` (36,288 rows of medians and bands)
  - `bakuzis_benchmark_wide.csv` (576 rows scenario level summaries)
  - `bakuzis_laws_compliance.csv` (Bakuzis law fractions)
- Real Bakuzis figures regenerated with NE for main text and ACD for SI

### EMS manuscript draft (manuscript/fvs_combined_draft.md and .docx)

- 6,047 words main text, 11 embedded figures, 4.6 MB docx
- Combines docs/fvs_manuscript.docx (March 22 assessment) and calibration/manuscript/fvs_calibration_manuscript.docx (Bayesian recalibration)
- Section 4.6 fully rewritten with real Cardinal results: ACD posterior 50% Eichhorn vs 17% MAP, NE posterior bands 5 to 15% of median at year 100
- 25 variant coverage table (Table 2) with correct names (fixed earlier errors)
- Calibration quality summary (Table 3)
- R code and Python code examples for calibrated config and uncertainty
- Computational resources subsection (5.3) documenting 3,800 CPU hours
- Citation keys converted from pandoc @ syntax to plain author year

### SI document (manuscript/fvs_combined_SI.docx)

- 1,242 words, 41 embedded figures, 29.8 MB docx
- Five parts: predicted vs observed, per variant performance, spatial distribution, posterior diagnostics, Bakuzis ACD variant
- Three new ACD only Bakuzis figures (SI.39 trajectories, SI.40 divergence, SI.41 band growth)

### Adjacent items outline (manuscript/adjacent_items_outline.md)

- Seven companion deliverables sized and prioritized: PNAS v6 review, extension short note, scientific poster, release notes blog, MEE alternative, teaching module, CRSF annual report

### Perseus and SiteIndex script refinements

- Replaced hardcoded `/users/PUOM0008/crsfaaron/...` paths with `Sys.getenv("SITEINDEX_DIR", file.path(Sys.getenv("HOME"), "SiteIndex"))` pattern in:
  - `calibration/R/project_future_si_delta.R`
  - `calibration/python/extract_plot_si_climr.R`
  - `calibration/python/project_future_si.R`
- Replaced hardcoded `crsfaaron@cardinal.osc.edu` with `OSC_USER` env var pattern in `calibration/slurm/deploy_perseus_harvest.sh`
- Defaulted `submit_perseus_*.sh` env vars to `${HOME}` instead of `/users/PUOM0008/crsfaaron/`
- Added env driven SITEINDEX_DIR to `calibration/osc/submit_future_si.sh`
- All scripts pass syntax checks (bash -n and Rscript parse)

## In Progress

Nothing currently in flight. All tasks 1 through 57 completed. Awaiting decision on consolidated commit.

## Pending Decisions

- [ ] Should I run the consolidated git commit now, or wait until you can review the manuscript and SI docx in person?
- [ ] Should the next tag be v2026.05.0 (calendar minor bump including Bakuzis pipeline) or stay on v2026.04.x?
- [ ] Should the Perseus future SI scripts go in this same commit or as a separate one?
- [ ] Has the Zenodo GitHub webhook been enabled at https://zenodo.org/account/settings/github/ ? If yes, the next pushed tag will mint the first DOI.
- [ ] EMS or MEE for primary submission target? Manuscript is currently EMS structured but MEE alternative is outlined.
- [ ] Should the synthetic stand generator be replaced with FIA derived stands to address NE Sukachev artifact, or document as a known limitation?

## Key Context

### Sandbox environmental constraints

- **SSH to outside hosts:** blocked by default in the sandbox due to permissions on `/etc/ssh/ssh_config.d/20-systemd-ssh-proxy.conf`. Workaround: pass `-F /dev/null -o UserKnownHostsFile=/dev/null -o GlobalKnownHostsFile=/dev/null` to bypass system config. This works for Cardinal SSH but **not** for git push to GitHub (no public key registered for sandbox).
- **FVS shared library runs:** fail in sandbox with Fortran EOF error in errgro.f90 line 55 even though the .so loads cleanly. Original `bakuzis_100yr_comparison.py` reproduces the same error, so this is environmental not code related. Cardinal and Aaron's laptop work fine.
- **R packages:** sandbox has base R but no tidyverse, ggplot2, or patchwork. Use Python matplotlib for figure rendering inside the sandbox.

### Cardinal connection details

```bash
SSH_OPTS="-F /dev/null \
    -i /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519_cardinal \
    -o UserKnownHostsFile=/dev/null \
    -o StrictHostKeyChecking=no \
    -o GlobalKnownHostsFile=/dev/null"
ssh $SSH_OPTS crsfaaron@cardinal.osc.edu '<command>'
```

### Active Cardinal jobs (as of 2026-04-24 22:00 EDT)

- 8 fvs_prod_* jobs running (production calibration)
- 1 perseus_unc_29 running
- 1 perseus_unc_agg pending on Dependency

### Directory inventory of new untracked files

```
M (modified, scripts now env-driven):
  calibration/R/project_future_si_delta.R
  calibration/python/extract_plot_si_climr.R
  calibration/python/project_future_si.R
  calibration/osc/submit_future_si.sh
  calibration/slurm/deploy_perseus_harvest.sh
  calibration/slurm/submit_perseus_climate.sh
  calibration/slurm/submit_perseus_harvest.sh

A (new, manuscript work):
  manuscript/fvs_combined_draft.md
  manuscript/fvs_combined_draft.docx
  manuscript/fvs_combined_SI.md
  manuscript/fvs_combined_SI.docx
  manuscript/adjacent_items_outline.md
  manuscript/SESSION_HANDOFF.md  <-- this file

A (new, Bakuzis output):
  calibration/output/bakuzis/bakuzis_uncertainty_*_n100_batch*.csv (12 files)
  calibration/output/bakuzis/bakuzis_uncertainty_summary_long.csv
  calibration/output/bakuzis/bakuzis_benchmark_wide.csv
  calibration/output/bakuzis/bakuzis_laws_compliance.csv

A (new, Bakuzis figures):
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_trajectories.png  (NE)
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_divergence.png    (NE)
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_laws.png          (NE+ACD)
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_band_growth.png   (NE)
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_trajectories_acd.png
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_divergence_acd.png
  calibration/output/comparisons/manuscript_figures/fig_bakuzis_band_growth_acd.png

Pre-existing untracked (not addressed this session, leave alone):
  PNAS_v6_Results_P1_revision.md
  cardinal_temporal/
  calibration/patches/
  calibration/figures/figure3_pipeline_architecture.png
```

### Headline Bakuzis findings

- **ACD Eichhorn law compliance:** posterior band 50%, default and MAP both 17%. Headline argument for posterior propagation: the credible band carries biological information that point estimates lose.
- **ACD Sukachev:** 92% default and MAP, 83% posterior. Density mortality 100% across.
- **NE Sukachev:** 0% all configurations. Synthetic stand generator artifact (called out in limitations).
- **NE BA bands at year 100:** 5 to 15% of posterior median. Narrower than the bootstrap demonstration suggested.
- **Year 100 BA divergence:** most NE scenarios within plus or minus 5% of zero. Outliers at high site high density Pine and Oak Pine where the SDImax revision triggers earlier self thinning.

### Aaron's standing preferences (relevant to the manuscript)

- R code preferred when possible, Python where it fits the toolchain
- No hyphens in prose (AI giveaway). Hyphens are OK in proper nouns and CLI flags.
- Citation format in this manuscript: plain author year `(Weiskittel et al. 2011)`, not pandoc `[@key]`
- Figure references inline as `Figure N (filename.png)` so reviewers can map back to source

## Next Steps (in priority order)

### Unblocked, can do anytime

1. **Consolidated git commit and push.** Stage the Perseus scripts, manuscript directory, and Bakuzis output as one or two commits. Push from Aaron's laptop. Optionally tag v2026.05.0 to mint the first Zenodo DOI (if webhook enabled).
   ```bash
   cd ~/fvs-modern && git pull
   git add manuscript/ \
           calibration/python/bakuzis_uncertainty_*.py \
           calibration/python/bakuzis_preview_figures.py \
           calibration/slurm/submit_bakuzis_uncertainty.sh \
           calibration/R/17_bakuzis_uncertainty_figures.R \
           calibration/BAKUZIS_UNCERTAINTY_README.md \
           calibration/output/bakuzis/bakuzis_uncertainty_*.csv \
           calibration/output/bakuzis/bakuzis_benchmark_wide.csv \
           calibration/output/bakuzis/bakuzis_laws_compliance.csv \
           calibration/output/comparisons/manuscript_figures/fig_bakuzis_*.png
   git add calibration/R/project_future_si_delta.R \
           calibration/python/extract_plot_si_climr.R \
           calibration/python/project_future_si.R \
           calibration/osc/submit_future_si.sh \
           calibration/slurm/deploy_perseus_harvest.sh \
           calibration/slurm/submit_perseus_climate.sh \
           calibration/slurm/submit_perseus_harvest.sh
   git commit -m "v2026.05.0: Bakuzis uncertainty pipeline, Perseus script refinements, combined manuscript draft"
   git tag -a v2026.05.0 -m "fvs-modern 2026.05.0 (Bakuzis uncertainty release)"
   git push origin main
   git push origin v2026.05.0
   ```

2. **PNAS v6 response review.** Read the three files at repo root and check for variant count consistency, citation correctness, and tone. Output as a 1 to 2 page review memo.

3. **Extension short note.** 800 word non technical summary for CRSF website using the CONUS improvement map and an NE trajectory figure.

### Blocked or deferred

4. Verify the EMS manuscript with a content review pass (read straight through, look for inconsistencies between sections, check that all 11 figures are referenced in the right places).
5. Build the scientific poster scaffold using the seven panel outline.
6. Run the FIA stratified Bakuzis evaluation to address the NE Sukachev synthetic stand artifact.

## Files to read first when resuming

1. This handoff: `manuscript/SESSION_HANDOFF.md`
2. The combined manuscript markdown: `manuscript/fvs_combined_draft.md`
3. The Bakuzis README: `calibration/BAKUZIS_UNCERTAINTY_README.md`
4. The CHANGELOG entry for v2026.04.7 in `CHANGELOG.md`
5. The release notes: `docs/release_v2026_04_7_notes.md`

## Cardinal verification on resume

Check whether the Bakuzis output directory is intact (it was synced back to the sandbox so a re-pull is not necessary, but verifying parity is good practice):

```bash
SSH_OPTS="-F /dev/null -i /sessions/cool-epic-bardeen/mnt/uploads/id_ed25519_cardinal -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o GlobalKnownHostsFile=/dev/null"
ssh $SSH_OPTS crsfaaron@cardinal.osc.edu \
    'ls ~/fvs-modern/calibration/output/bakuzis/bakuzis_uncertainty_*_n100_batch*.csv | wc -l'
# Expect: 12
```

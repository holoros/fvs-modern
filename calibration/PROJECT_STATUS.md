# FVS Bayesian Calibration: Project Status

**Last updated:** 2026-03-30 13:45 EDT
**Project lead:** Aaron Weiskittel
**Repository:** https://github.com/holoros/fvs-modern.git
**HPC cluster:** OSC Cardinal (account PUOM0008, user crsfaaron)

## Pipeline Completion

| Component | Equation Form | Variants | Status |
|-----------|--------------|----------|--------|
| Height-Diameter | Chapman-Richards (nlme) | 25/25 | HMC complete |
| Mortality | Logistic (species RE) | 25/25 | HMC complete |
| Crown Ratio | Linear mixed effects | 25/25 | HMC complete |
| SDIMAX | Quantile regression + Bayesian | 25/25 | Complete |
| Diameter Growth | Wykoff log-linear (custom Stan) | 8/25 | HMC running (job 7589313) |
| Height Increment | Custom Stan | 6/6 | MAP only |

## Active Jobs (Cardinal)

| Job ID | Script | Status | Wall Time |
|--------|--------|--------|-----------|
| 7589313 | submit_dg_24h.sh (array 0-17) | 8 HMC done, 17 running | 24h limit |
| 7589314 | submit_comparison_full.sh | PENDING (afterany:7589313) | 2h |
| 7589315 | submit_assessment.sh | PENDING (afterany:7589313) | 3h |

## Key Paths (Cardinal)

```
/users/PUOM0008/crsfaaron/fvs-modern/calibration/
  R/                  # All R scripts (00-12)
  stan/               # Stan model files
  osc/                # SLURM submission scripts
  output/variants/    # Per-variant calibration outputs
  output/comparisons/ # Cross-variant comparison results
  output/assessment/  # Comprehensive assessment (pending)
  logs/               # SLURM log files
```

## Key Paths (Local)

```
~/Documents/Claude/fvs-modern/calibration/
  manuscript/         # Calibration manuscript + tables
  figures/            # Publication figures (PDF + PNG)
  figshare/           # Reproducibility package for FigShare
  R/                  # R scripts (local copies)
  stan/               # Stan models (local copies)
  osc/                # SLURM scripts (local copies)
  output/             # Synced comparison + table outputs
  data/               # Input data (variant boundaries, NSVB coefficients)
```

## Deliverables

| Deliverable | Location | Status |
|-------------|----------|--------|
| Manuscript (docx) | calibration/manuscript/fvs_calibration_manuscript.docx | Draft, DG placeholder |
| FigShare draft | figshare.com item 31891714 | Metadata set, needs zip upload |
| FigShare zip | fvs_calibration_figshare.zip (top level) | 1.8 MB, missing DG configs |
| Comparison figures | calibration/output/comparisons/ | 131 files, 5 components |
| Assessment output | (pending) | Waiting on DG completion |

## Remaining Tasks

1. Wait for DG HMC to finish (17 variants, est. 4-12 hours)
2. Comparison (7589314) and assessment (7589315) jobs run automatically
3. Rsync final results from Cardinal
4. Update manuscript with DG metrics (replace placeholder text)
5. Rebuild FigShare zip with calibrated_configs/ and DG summary data
6. Upload zip to FigShare, select category, add co-authors
7. Git push to GitHub (needs auth token)

## Monitoring Command

```bash
ssh crsfaaron@cardinal.osc.edu "echo 'HMC:' && ls /users/PUOM0008/crsfaaron/fvs-modern/calibration/output/variants/*/diameter_growth_summary.csv 2>/dev/null | wc -l && echo '/25' && sacct -j 7589313 --format=JobID%15,State%12,Elapsed%10 | grep -c RUNNING && echo 'tasks still running'"
```

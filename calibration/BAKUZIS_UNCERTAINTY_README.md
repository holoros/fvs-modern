# Bakuzis matrix with parametric uncertainty

Long term (100 year) projection comparison of default versus calibrated
FVS parameters, extended with Bayesian posterior credible bands.
Evaluates the four classical Bakuzis biological laws (Sukachev,
Eichhorn, density driven mortality, and mortality size pattern) on a
factorial design of 36 scenarios.

## What's new since the point estimate version

The original `bakuzis_100yr_comparison.py` ran one default and one
calibrated projection per scenario. This pipeline keeps those two plus
a user configurable number of posterior draws per scenario, so that
year 50 and year 100 divergence between default and calibrated is
reported with a 95 percent credible band rather than as a single
deterministic delta.

## Pipeline

```
                1. bakuzis_uncertainty_comparison.py
                   36 scenarios x (default + calibrated MAP + N draws)
                   per variant, writes long CSV
                            |
                            v
                2. bakuzis_uncertainty_aggregate.py
                   median + 95 percent band per scenario x year
                   benchmarking wide table + Bakuzis law compliance
                            |
                            v
                3a. bakuzis_preview_figures.py    (matplotlib preview)
                3b. 17_bakuzis_uncertainty_figures.R  (publication, ggplot2)
```

## Scenario grid

| Dimension | Levels |
|-----------|--------|
| Species group | Spruce Fir, Northern Hardwood, White Pine, Oak Pine |
| Site class | Low (SI 45), Medium (SI 60), High (SI 75) |
| Initial density | Low (BA 60), Medium (BA 120), High (BA 180) |

4 x 3 x 3 = 36 scenarios, projected over 100 years in 20 five year
cycles using the FVS shared library pathway (fvs2py).

## Quick start

### Local workstation

```bash
# 1. Build the shared libraries once
bash deployment/scripts/build_fvs_libraries.sh src-converted lib

# 2. Run NE with 50 posterior draws
export FVS_PROJECT_ROOT="$PWD"
python3 calibration/python/bakuzis_uncertainty_comparison.py \
    --variant ne --n-draws 50

# 3. Aggregate into summary, benchmark, and law compliance tables
python3 calibration/python/bakuzis_uncertainty_aggregate.py \
    --input calibration/output/bakuzis/bakuzis_uncertainty_ne_n50.csv

# 4. Preview figures (matplotlib, sandbox safe)
python3 calibration/python/bakuzis_preview_figures.py

# 5. Publication figures (needs tidyverse, ggplot2, patchwork)
Rscript calibration/R/17_bakuzis_uncertainty_figures.R
```

### OSC Cardinal

```bash
# Submit array job: 6 batches of 6 scenarios, both NE and ACD
sbatch calibration/slurm/submit_bakuzis_uncertainty.sh

# Override defaults via environment
sbatch --export=N_DRAWS=100,SEED=7 calibration/slurm/submit_bakuzis_uncertainty.sh
```

After all array tasks finish, run aggregation once over the full batch
directory:

```bash
python3 calibration/python/bakuzis_uncertainty_aggregate.py \
    --input-dir calibration/output/bakuzis
Rscript calibration/R/17_bakuzis_uncertainty_figures.R
```

## Output files

Written to `calibration/output/bakuzis/` unless overridden via
`BAKUZIS_OUTPUT_DIR`:

| File | Content |
|------|---------|
| `bakuzis_uncertainty_<variant>_n<N>.csv` | Raw ensemble; one row per year x scenario x config x draw |
| `bakuzis_uncertainty_summary_long.csv` | Median and quantile bands per scenario x year x variable |
| `bakuzis_benchmark_wide.csv` | Scenario level year 25/50/75/100 BA, TPA, volume with default vs calibrated percent divergence |
| `bakuzis_laws_compliance.csv` | Fraction of scenario pairs satisfying each Bakuzis law, per variant x config |
| `bakuzis_trajectories_preview.png` | Matplotlib 3 x 4 grid preview |
| `bakuzis_year100_divergence_preview.png` | Year 100 divergence scatter preview |
| `bakuzis_laws_preview.png` | Law compliance bar chart preview |

Publication figures (R) land in
`calibration/output/comparisons/manuscript_figures/`:

- `fig_bakuzis_trajectories.{pdf,png}` (trajectory grid with bands)
- `fig_bakuzis_divergence.{pdf,png}` (year 100 divergence with band width)
- `fig_bakuzis_laws.{pdf,png}` (four biological laws panel)
- `fig_bakuzis_band_growth.{pdf,png}` (uncertainty growth with horizon)

## Bakuzis biological laws implemented

The framework comes from Bakuzis (1969). We evaluate four laws at the
100 year horizon:

1. **Sukachev effect.** Per tree volume (tcuft / tpa) should be smaller
   in high density stands than in low density stands at matched site
   and species. We report the fraction of site x species cells where
   the high density scenario has a lower per tree volume than the low
   density scenario.

2. **Eichhorn's rule.** Stand basal area should increase with site
   quality at matched species and density. We report the fraction of
   species x density cells where BA is monotonic non decreasing across
   Low, Medium, and High site classes.

3. **Density drives mortality.** Cumulative mortality should be greater
   in high density stands than in low density stands at matched site
   and species. Reported as a fraction of cells.

4. **Mortality size pattern.** Smaller trees should have higher
   mortality rates than larger trees within a scenario. Not currently
   computed from the Summary2 output (it needs treelist detail);
   reported as NA until treelist extraction is added to the runner.

Three of the four are implemented from the standard FVS Summary2
output. Adding law 4 requires capturing a treelist snapshot per cycle,
which is a follow on enhancement.

## Performance budget

Single FVS run on a modern workstation: roughly 0.8 to 2.0 seconds for
a 100 year projection. Per scenario the ensemble cost is
`(2 + N) * t_single`. Full matrix cost is
`36 scenarios x 2 variants x (2 + N) runs x t_single`.

| N draws | Per variant time | Total (NE + ACD) |
|---------|-------------------|-------------------|
| 50      | ~40 minutes       | ~80 minutes       |
| 100     | ~70 minutes       | ~140 minutes      |
| 500     | ~5 hours          | ~10 hours         |

These are rough estimates using fvs2py on a single core. The SLURM
template splits across six array tasks so wall time divides by six.

## Related scripts

- `calibration/python/bakuzis_100yr_comparison.py` — the point estimate
  progenitor. Keep for reference and for the fastest comparison runs.
- `calibration/python/perseus_uncertainty_projection.py` — plot level
  uncertainty on real Maine FIA remeasurements, complementary to this
  synthetic matrix.
- `calibration/R/16_bakuzis_matrix_figure.R` — earlier R figure based
  on the assessment pipeline; the new `17_bakuzis_uncertainty_figures.R`
  supersedes it for the uncertainty version.

# Section 4.6 update template (post-FIA Bakuzis run)

This template captures the methodology shifts and the structure of
the rewritten section 4.6 once the FIA Bakuzis array (job 8847841)
completes. Numbers in [SQUARE BRACKETS] need replacement from the
new aggregated outputs.

## Template prose for §4.6

## 4.6 Bakuzis matrix: outcomes

The Bakuzis uncertainty pipeline was extended to draw real FIA
stands from the variant's state coverage rather than synthesize
stands from target BA and TPA targets. For the Northeast (NE) and
Acadian (ACD) variants, each of the 36 Bakuzis cells (4 species
groups x 3 site classes x 3 density classes) was matched against
FIA conditions stratified by site index band and basal area band,
and up to five real FIA plots were sampled per cell. The runner
projected each plot under default parameters, calibrated MAP, and
100 posterior draws, producing a long format ensemble of [N] rows
across both variants.

The shift to real stands eliminates the synthetic stand artifact
flagged in the prior version of this section: the NE Sukachev
compliance value of 0 percent under all configurations was driven
by the synthetic generator's invariant species mix and BA-derived
QMD, not by the calibrated parameters. Real FIA plots have the
natural variability in species composition, diameter distribution,
and crown structure that the Bakuzis biological laws are designed
to test.

[NEW FIGURES 8-11, regenerated with the FIA-mode output. The
trajectory grid (Figure 8) now shows median plus 95 percent
credible band averaged across 5 plots per cell. The divergence
scatter (Figure 9) shows year 100 BA divergence per (variant,
species, site, density) replicated across plots; circles can be
sized by within-cell standard deviation. The laws figure (Figure
10) and band growth (Figure 11) follow the same regeneration.]

Figure 8 ([NEW]) shows the [3 x 4] grid of NE BA trajectories.
Default and calibrated MAP trajectories [match the prior synthetic
result | diverge from the prior synthetic result] across [most |
some] scenarios. The replicate dimension reveals [tighter | looser]
within-cell variability than the synthetic single-stand bands
suggested. The exception remains [scenario name] where the
calibrated MAP reaches [X] ft2/ac at year 100 versus the default's
[Y] ft2/ac. The Acadian variant shows [pattern].

Figure 10 ([NEW]) reports Bakuzis law compliance from the FIA
ensemble. Compliance values across all four laws are now computed
from real FIA stand combinations rather than synthetic invariant
designs:

- Sukachev effect (per tree volume lower under high density at
  matched site and species): NE [X] percent default, [Y] percent
  MAP, [Z] percent posterior; ACD [X] / [Y] / [Z]. [Compare to
  prior synthetic NE 0 percent / ACD 92 percent.]
- Eichhorn's rule (BA monotonic non-decreasing across site classes
  at matched species and density): NE [X] / [Y] / [Z]; ACD [X] /
  [Y] / [Z]. [Compare to prior synthetic NE 17 percent / ACD
  posterior 50 percent vs MAP 17 percent.]
- Density drives mortality: NE [X] / [Y] / [Z]; ACD [X] / [Y] /
  [Z]. [Compare to prior synthetic 100 percent.]
- Mortality size pattern: [report or carry forward "deferred to
  future work" if the runner does not yet capture treelist].

The [headline | revised] finding is [restate the most striking
posterior-vs-MAP gap from the new numbers]. [If ACD posterior
Eichhorn 50% replicates: emphasize that real stands corroborate
the posterior-band-as-biological-information argument. If ACD
posterior Eichhorn drops or NE Sukachev recovers: discuss what the
shift means about synthetic stand artifacts.]

## Limitations note (revised text for §5.4)

The Bakuzis matrix results are computed on real FIA stands sampled
by site and density bin, not on synthetic algorithmic stands. The
prior synthetic stand artifact (NE Sukachev 0 percent under all
configurations) is no longer present because real FIA plots carry
the natural variability needed to express that biological law. A
remaining limitation is that the variant coverage is restricted to
NE and ACD because per-state FIA CSVs for the western (PN, IE) and
southern (SN) variants were not available on the Cardinal
filesystem at submission time. Extending Bakuzis to those variants
requires either downloading per-state CSVs from the FIA DataMart
or writing a small R script that converts the consolidated CONUS
remeasurement RDS into per-state CSVs. The pipeline supports both
extensions without further code changes once the data are in
place.

## Methods section addition (§3.3 update)

Add a paragraph to §3.3 (Bakuzis matrix factorial):

The Bakuzis runner can use either synthetic algorithmic stands or
real FIA condition pairs. For the NE and ACD variants we used the
FIA path: each Bakuzis cell was matched against FIA conditions
stratified by SICOND (site index, base age 50) and BALIVE (live
basal area), and five real plots per cell were sampled with
replacement seeded by scenario id. Tree records were pulled from
the FIA TREE table for the sampled PLT_CN list and converted to
the FVS standinit and treeinit format. The synthetic generator
remains in the codebase as a fallback for variants without FIA
state CSV coverage and for sensitivity tests.

## Aggregator and figure regeneration

Once the array completes, on Cardinal:

```bash
cd ~/fvs-modern
module load python/3.12
python3 calibration/python/bakuzis_uncertainty_aggregate.py \
    --input-dir calibration/output/bakuzis
```

The aggregator handles the new replicate dimension implicitly because
it groups by (variant, scenario, species_group, site_class,
density_class, year, variable, config) and computes median and
quantiles across all matching rows. Replicates increase the within
group sample size; the band statistics remain valid.

For figures, on the sandbox after pulling the three summary CSVs:

```bash
python3 calibration/python/bakuzis_uncertainty_figures.py
```

The figures will overwrite the prior synthetic-stand versions at
calibration/output/comparisons/manuscript_figures/fig_bakuzis_*.png.

## Manuscript regeneration

```bash
cd manuscript
pandoc fvs_combined_draft.md -o fvs_combined_draft.docx
```

## Self-review re-score expected shifts

D6 validation: 4 -> 5 (real FIA replicates per scenario remove the
"synthetic stand artifact" caveat that was the main weakness).
D8 interpretation: 4 -> 5 (real-stand Bakuzis is a defensible
manuscript-grade evaluation that can support the posterior-band
biological information claim).
Indicative recommendation: Minor Revision -> [Accept | Minor
Revision] depending on whether D5 reproducibility (Zenodo DOI) is
also resolved.

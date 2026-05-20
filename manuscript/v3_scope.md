# v3 manuscript scope and sequencing

*Drafted 2026-05-16, looking forward from where the v2 manuscript stack
lands. Captures the v3 boundary: what graduates from v2 open threads
into v3 confirmed content, what fresh work v3 adds, and what stays out
of scope (held for v4 or operational notes).*

## Headline framing for v3

v2 establishes the species-free trait-driven pivot on pilot-scale fits
(HG-Organon fixedK at ~150K observations, DG-Kuehne at ~100K). v3 ships
the production-scale confirmation of the same hypothesis plus the
species-out predictive evaluation that the trait-driven architecture
makes possible. The v3 story is therefore: "the v2 pilot results
generalize at full data scale and to species held out at training time."

## What graduates from v2 open to v3 confirmed

Each v3 confirmed section closes one of the v2 open threads documented
in `v2_section5_discussion.md`.

| v2 open thread | v3 confirmation source | Section in v3 |
|---|---|---|
| Full-data B1 HG refit (1.98M obs) | Production fit, expected wall ~12h on Cardinal | New paragraph in 5.6 + headline figure |
| v6 + v4 LOO selection | Both fits land; LOO ELPD comparison | Section 5.8 update + Figure 5 |
| Held-out species predictive accuracy | `eval_80_holdout_species_fit.R` + `eval_80b_holdout_species_predict.R` | New Section 5.9 + Figure 2 |
| 5% functional-equivalence threshold citation | Paperpile pass | CITE TODO resolved across all sections |

## What v3 adds on top

Three new contributions beyond v2.

### 1. Production-scale species-free fit on HG (1.98M observations)

The HG-Organon fixedK B1 species-free pilot was 150K observations on 9
species. The v3 production fit uses the full 1.98M observation CONUS HG
training set (all 70+ species with sufficient remeasurement data). The
hypothesis is that the pilot-level σ-equivalence (0.26%) and trait
coefficient identifiability hold at production scale. If they do, the
trait-driven architecture is operationally validated for HG. If they
don't, the deviation patterns will inform either further model
refinement or a tighter scope claim.

### 2. Held-out species predictive evaluation

The species-free architecture's central promise is clean extrapolation
to species not in the training set. v3 quantifies that promise
empirically. For each component, the fitting set excludes a curated 20%
of species; the held-out species' annual increment is predicted from
the trait coefficients alone (no species random intercept available
because the species was never seen in training). The predictive R² and
RMSE on the held-out species relative to the in-training species set
quantifies the architecture's transferability.

The eval skeletons are at `calibration/R/eval_80_holdout_species_fit.R`
and `calibration/R/eval_80b_holdout_species_predict.R`. Each component
needs one fit run per fold (5-fold across 70+ species means 5 fits per
component, 30 fits total). Expected wall is ~3h per fit on Cardinal.

### 3. v6 production candidate result and LOO selection

The May 15 covariate exploration produced the v6_data_driven Stan
candidate (SICOND + is_plantation + ELEV + sdi_complexity). v3 runs
the v6 fit at production scale, runs the v4_full architecture variant
suite winner (quad / l1site / traitsite / full — pick one via LOO), and
compares v6 vs the v4 winner. The LOO ELPD difference picks the
manuscript candidate; the loser stays in the SI as a reference.

## What stays out of v3 scope

- bc and on Canadian variants (still don't load; multi-day stub work
  separate from the manuscript)
- Architecture variant suite cross-validation against an out-of-sample
  geographic holdout (separate methods development)
- Phase 5 calibration components beyond the six in CONUS Phase 4
- Operational tooling improvements (notebook templates, dashboard
  integrations) — these go in `fvs-modern` release notes, not the
  manuscript

## v3 figures (proposed numbering after v2 final)

| # | Description | Source | Status |
|---|---|---|---|
| Figure 1 | B1 vs B2 trait coefficients, HG + DG Kue | v2 (Aaron's a6a1f42) | merged |
| Figure 2 | Held-out species predictive accuracy | v3 new (eval_80) | scaffold in this PR |
| Figure 3 | CONUS modifier alpha coefficients across 6 components | v2 (PR #19) | merged |
| Figure 4 | Covariate importance from 2026-05-15 exploration | v2 (pending PR) | scaffold present |
| Figure 5 | LOO ELPD comparison: v6 vs v4 winner | v3 new | when fits land |
| Figure 6 | Full-data B1 HG production fit, posterior summary | v3 new | when fit lands |

## v3 dependencies and gating

```
Paperpile pass (independent) ────────────────────┐
                                                 │
HG full-data B1 fit (12h) ──────────► Fig 6 ─────┤
                                                 │
v6 fit (12h) ───────┐                            │
                    ├──► LOO compare ──► Fig 5 ──┤
v4 winner (varies) ─┘                            │
                                                 ▼
Held-out species fits (5 folds × 6 components)  v3 manuscript ready
   ────► eval_80 outputs ────► Fig 2 ───────────►
```

The Paperpile pass is independent and can happen anytime. The other
three paths gate on Cardinal compute. The held-out species path is the
longest (30 fits at ~3h each = ~90h sequential, ~6h elapsed if 15
parallel), so it should start first.

## Suggested v3 launch sequence

1. **Day 0**: Land all four v2 PRs on origin/main. Run the Paperpile pass.
2. **Day 0**: Submit the held-out species fits as a SLURM array (5
   folds × 6 components = 30 jobs).
3. **Day 0 or 1**: Submit the v6 production candidate fit.
4. **Day 1 or 2**: Submit the chosen v4 variant winner fit (after a
   quick LOO check on the existing v4 architecture suite outputs).
5. **Day 2 or 3**: Submit the full-data B1 HG production fit.
6. **Day 3+**: As fits land, run the corresponding figure scripts and
   append the new findings to Sections 5.6 / 5.7 / 5.8 / new 5.9.
7. **Day 5-7**: v3 manuscript ready for co-author review.

Total Cardinal compute estimate: ~150 CPU-hours plus ~$300 budget at
current pricing.

## Risk register

| Risk | Mitigation |
|---|---|
| Production HG fit reveals σ deviation > 5% at full scale (pilot was 0.26%) | Report the deviation honestly; v3 narrative becomes "pilot generalizes within 5% but production fit shows X% deviation explained by Y" |
| Held-out species predictions are systematically biased (e.g., always over-predict rare species) | Bias is itself a finding; characterize and add to Discussion |
| v6 + SICOND fit reveals new identification problem | Fall back to v4 winner; v6 stays in SI |
| Paperpile library missing one of the 9 sources | Add manually from the paper's reference list; minor inconvenience |

## What goes back into v4

Three threads that v3 deliberately holds back for future work:

1. **Species-trait extension beyond the 8 chosen traits.** The
   importance analysis on the production HG B1 fit will identify which
   subset of the 8 actually matters; v4 can then test whether adding
   more traits (e.g., wood ring porosity, mycorrhizal type, root depth)
   improves predictive accuracy.
2. **Canadian variant (bc, on) restoration.** Currently blocked on
   missing database routines and crown routines; needs upstream
   investigation separately from the manuscript track.
3. **Operational deployment guide.** Once v3 is in print, write a
   one-page operational guide for FVS users on choosing between v1
   per-variant fits, v2 species-free fits, and v3 production fits.

## Open questions for v3 design

1. Which 20% holdout species set for Figure 2? Three candidate
   schemes:
   - Random 20% per fold (5 folds, all 70+ species participate as
     holdouts at some point)
   - Species sparsity-based (hold out the 20% with fewest
     observations; the case where transferability matters most)
   - Geographic (hold out species whose primary range is in a specific
     ecoregion; tests geographic vs trait extrapolation)

   Recommend the sparsity-based scheme as primary, with random as a
   sensitivity check in the SI.

2. Which LOO comparison test is canonical for v6 vs v4 winner? The
   `loo` package's `loo_compare()` with default ELPD difference and
   SE is standard; the choice of "winning" rule (ELPD diff > 2 SE)
   should be pre-specified.

3. Does v3 need a v3.0 release tag on the `fvs-modern` repo, or does
   it stay on the v2026.05 calendar tag line? Recommend a fresh
   v2026.0X tag at v3 ship to mark the production-scale calibration
   release.

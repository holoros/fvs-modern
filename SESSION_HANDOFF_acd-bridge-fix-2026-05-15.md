# Session Handoff — acd-bridge-fix-2026-05-15

Date written: 2026-05-16
Author of session: Aaron Weiskittel (working with Claude assistance)
Branch: acd-bridge-fix-2026-05-15
Remote: github.com:holoros/fvs-modern.git
Latest commit at handoff: 44d1f0a (NE vs ACD runtime A/B) plus pending commits described below.

## TL;DR

ACD was a sub-variant of NE that behaved oddly when called. After
~15 commits this branch:

1. Fixes the ACD calibration post-pass (parse bug, NE param
   fallback, ingrowth_lookup cache).
2. Defaults NSVB volume/biomass/carbon equations to on across all
   seven Eastern variants (ACD, NE, CS, LS, SN, KT, EM) via the
   LFIANVB flag in grinit.f90.
3. Fixes seven F77 to F90 conversion bugs in the standalone FVS
   build path that were blocking native binary execution.
4. Delivers three runtime A/B comparisons proving the fork carries
   genuine variant divergence at three resolution levels: dispatch,
   estimator, and calibration.
5. Confirms with Bayesian posteriors and FIA benchmarks that NE
   currently outperforms ACD calibration on the Acadian footprint,
   with a clear fix path identified.

The fork is now in a state where calibrated ACD and calibrated NE
projections can be A/B tested at runtime against the same FIA input
deck, and the build pipeline is solid enough to support future
variant-level recalibration work without further F77/F90 hazard.

## Branch lineage

```
44d1f0a NE vs ACD runtime A/B: confirm subvariant divergence
cc00b92 NSVB vs CRM runtime A/B: definitive divergence demonstrated
26cc488 spctrn.f90: handle ACD variant in species fallback + guard JSPIN(0)
c75e576 build_fvs_executables.sh: shield variant PRGPRM/ESPARM from base/ shadows
6c029bd filopn.f90: open ISTDAT with <basename>.tre in non-interactive path
6a27c2b filopn.f90: init FVS unit numbers before OPEN — unblocks keyrdr.f90
3db614a fvs.f90: initialize FVS unit-number COMMON variables on entry
1cd784f errgro.f90: defensive JOSTND guard to avoid UNFORMATTED I/O abort
e0aeeec autopilot 2: extend NSVB to KT/EM, fix exe build, document divergence
3625a01 Extend NSVB default to NE / CS / LS / SN variants
254cce3 ACD: default FIA NSVB volume/biomass/carbon equations to ON
86a7110 ACD stand-level calibration factors + bias diagnostic suite
a8ee4e9 ACD post-pass: cache ingrowth_lookup so full-fidelity ingrowth is reusable
f5bb1bf ACD subvariant bridge: fix post-pass parse + relabel + NE param fallback
```

Plus pending commits at handoff time:
- sumout.f90: deactivate dead CS/LS/NE/SN CASE branches (fixes FVSne summary writer crash)
- calibrated_ne_vs_acd directory: SLURM script, CSV, README
- Updated ne_vs_acd README reflecting fixed FVSne output

## Key technical findings

### NSVB vs CRM (commit cc00b92)

Two FVSacd executables built from identical source except for the
LFIANVB default produce different .sum / .out / FVSOut.db. CFV
differs at cycle 0 (1530 NSVB vs 1526 CRM), BFV at cycle 0 is the
headline (292 NSVB vs 0 CRM because CRM applies stricter merch-spec
thresholds). TPA, BA, QMD remain bit-identical, confirming the
divergence is isolated to the volume estimator path. Pattern matches
Westfall et al. 2024 NSVB equations vs the Component Ratio Method.

### NE vs ACD runtime (commit 44d1f0a, then revised with sumout fix)

Stand S248112 UNTHINNED CONTROL at year 2090 (age 160):

| Metric | NE | ACD | Delta |
| --- | --- | --- | --- |
| TPA | 111 | 94 | NE +17% |
| BA  | 194 | 169 | NE +15% |
| QMD | 17.9 | 18.2 | ACD +1.7% |
| CFV | 7,638 | 6,727 | NE +13% |
| BFV | 43,258 | 38,540 | NE +12% |

ACD produces a lower-stocked, higher-QMD stand at maturity, the
regional spruce-fir signature expected from the Acadian variant. NE
keeps more trees; ACD has higher mortality but larger residuals.

### Calibrated NE vs Calibrated ACD on Acadian-footprint FIA

SLURM job 9718410 with FVS_ACD_RELABEL=TRUE. 30,146 ME/NH/VT
conditions (13,693 ACD + 16,453 NE post relabel).

| Variant | BA %RMSE | BA bias_pct |
| --- | --- | --- |
| ACD_acadian | 28.52 | +51.56 |
| NE_acadian  | 23.19 | +27.32 |

NE outperforms ACD on the Acadian footprint by 5.3 pp %RMSE and
~24 pp less positive bias. Driver: ACD diameter-growth HMC fit did
not converge (rhat 1.95, converged FALSE). Fix path: re-fit with
longer warmup, smaller step size, adaptive reparameterization.

All 2,261 common posterior variables between NE and ACD differ.
The posteriors are not aliases of each other; ACD just needs better
mixing.

## Files modified or added

### Source code (src-converted/)

- acd/grinit.f90, ne/grinit.f90, cs/grinit.f90, ls/grinit.f90,
  sn/grinit.f90, kt/grinit.f90, em/grinit.f90 — LFIANVB = .TRUE.
- base/errgro.f90 — defensive JOSTND guard
- base/fvs.f90 — unit-number init on entry
- base/filopn.f90 — unit-number init before OPEN, ISTDAT tree-file
  open in non-interactive path
- base/varver_stub.f90 — new stub for VARVER subroutine
- vls/spctrn.f90 — AC alias + JSPIN(0) guard
- vbase/sumout.f90 — deactivate dead CS/LS/NE/SN CASE branches (NEW)

### Build pipeline

- deployment/scripts/build_fvs_executables.sh — conditional
  econ_stubs inclusion, MAXSP shadow shield for variant builds

### Calibration pipeline

- calibration/R/19_fia_benchmark_engine.R — FVS_ACD_RELABEL env gate,
  ACD param fallback to NE, ingrowth_lookup cache
- calibration/R/27_acd_post_pass.R — marker-based slicing, fallback,
  ingrowth cache reading
- calibration/slurm/submit_benchmark_acd_relabel.sh — NEW

### Analysis artifacts (calibration/analysis/acd_stand_level_2026-05-16/)

- ne_vs_acd/ — runtime NE vs ACD outputs + README
- nsvb_runtime_ab/ — runtime NSVB vs CRM outputs + README
- calibrated_ne_vs_acd/ — calibrated NE vs ACD FIA benchmark CSV +
  SLURM script + README (NEW)
- ACD_bias_findings.md — diagnostic narrative
- stand_level_calibration.R — 81-row stand-level calibration factors
- acd_calibration_factors.csv + guidance

## How to resume

1. Pull the branch on Cardinal: `cd ~/fvs-modern-acdbridge && git
   pull origin acd-bridge-fix-2026-05-15`.
2. Rebuild executables: `bash deployment/scripts/build_fvs_executables.sh
   . lib acd` and `... lib ne`.
3. Smoke-test: run the four-corner integration test described in
   `calibration/analysis/acd_stand_level_2026-05-16/ne_vs_acd/README.md`.
4. Open PR or merge into main when ready.

## Open follow-ups (priority order)

1. Re-fit ACD diameter-growth HMC with longer warmup and smaller
   step size. Target: rhat < 1.05. The Stan model file is at
   calibration/stan/wykoff_dg/.
2. Widen ACD footprint to include New York Adirondack counties via
   county-level filter in 19_fia_benchmark_engine.R. FIPS county
   list documented in calibrated_ne_vs_acd/README.md.
3. Repeat calibrated A/B with the 81-row ACD post-pass calibration
   factors applied to see whether the stand-level corrections close
   the bias gap.
4. STOP 10 at the very end of standalone FVS runs — post-completion
   IC code, not a crash. Investigation: main.f90 SELECT CASE on
   fvsGetICCode. Low priority; doesn't affect output.
5. Consider merging acd-bridge-fix-2026-05-15 into main after the
   ACD HMC re-fit completes.

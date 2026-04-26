---
title: "Self-review report v2: fvs-modern combined manuscript (post-FIA Bakuzis)"
subtitle: "Internal pre-submission assessment for Environmental Modelling and Software"
author: Aaron R. Weiskittel
date: 2026-04-26
---

# Header

| Field | Value |
|-------|-------|
| Manuscript | Modernizing and Bayesian recalibrating the Forest Vegetation Simulator: open source infrastructure and parametric uncertainty quantification for 25 geographic variants |
| Authors | A. R. Weiskittel, G. Johnson, D. Marshall |
| Target journal | Environmental Modelling and Software (EMS) |
| Document version | manuscript/fvs_combined_draft.docx (commit 4de8467) |
| Self-review date | 2026-04-26 |
| Prior review | manuscript/20260425_ems_self-review_fvs-modern.md (commit 89ecf0b) |

# Section 1. Summary assessment

This v2 review re-scores the manuscript after the FIA-stand Bakuzis
revision (commit 4de8467). The revision replaced synthetic
algorithmic stands with real FIA condition pairs sampled by site
index and basal area band, with five plots per cell projected under
default, calibrated MAP, and 100 posterior draws. The revision
addresses the four primary scientific concerns from the v1 review
(V1 validation framing, C2 variant coverage justification, C3 NE
Sukachev circular reasoning, GY6 comparative positioning) and
preserves the headline finding from the synthetic version:

- ACD posterior credible band achieves 50 percent Eichhorn rule
  compliance on real FIA stands compared with 20 percent for both
  default and calibrated MAP point estimates. This 2.5-fold uplift
  is now corroborated on real stands rather than asserted from
  synthetic stands; it is the strongest single empirical argument
  for posterior propagation as biological information rather than
  software plumbing.
- NE Sukachev compliance remains 0 percent under all configurations
  on real FIA stands. The v1 review flagged this as circular
  reasoning ("synthetic artifact" was asserted but not demonstrated);
  the FIA result confirms the 0 percent value is a real property of
  the NE variant's self-thinning behavior, not a synthetic stand
  generator artifact. Section 5.4 was rewritten to reflect this.
- Year 100 BA divergence on real FIA stands is small (within plus
  or minus 1 percent for most cells; one Oak Pine high-high cell
  reaches -9.6 percent), indicating that on the average Northeast
  forest the calibrated parameters do not substantially shift
  long-horizon central tendencies. Section 4.6 was rewritten to
  clarify that the recalibration's primary value is uncertainty
  quantification and component-level accuracy rather than a
  systematic shift of long-horizon BA predictions.

Indicative recommendation under v2: **Accept (pending Zenodo DOI)**
or **Minor Revision** if the corresponding author and EMS editor
prefer a final reviewer pass.

# Section 2. Resolution of v1 major issues

| v1 # | Concern | v2 status |
|-----:|---------|-----------|
| 1 | V1 independent validation | RESOLVED. §3.1/§4.3 already reframed in prior commit; FIA Bakuzis adds an independent stratified sample test on real stands. |
| 2 | C1 self-reference | RESOLVED. Crookston and Dixon (2005) cited; assessment language rephrased. |
| 3 | C2 limited Bakuzis variant coverage | PARTIALLY RESOLVED. NE plus ACD with real FIA stands. PN/SN/IE blocked at the FVS shared library load step (undefined symbol `morcon_`); diagnosis documented in `calibration/python/PN_SN_LIBRARY_DIAGNOSIS.md`. §5.4 caveat updated. |
| 4 | C3 NE Sukachev circular reasoning | RESOLVED. FIA Bakuzis confirms 0 percent is a real variant property, not synthetic artifact. §4.6 and §5.4 rewritten. |
| 5 | GY6 comparative positioning | RESOLVED. Joo et al. (2025), Premer et al. (2025), Woodall et al. (2025) cited in §1 and §5. |
| 6 | C5 software DOI not minted | OPEN. Zenodo webhook setup pending. One-hour task before submission. |
| 7 | M1 equation forms | RESOLVED. Equations (1)-(7) added in §2.2. |

# Section 3. Resolution of v1 minor issues

| v1 # | Concern | v2 status |
|-----:|---------|-----------|
| 1 | G3 calibration random seeds | RESOLVED in §2.2 update. |
| 2 | S2b spatial autocorrelation | RESOLVED. SI paragraph on Moran's I added. |
| 3 | S3 convergence diagnostics | DEFERRED to companion calibration paper SI; cross-reference added. |
| 4 | GY5 climate sensitivity Climate-FVS reference | RESOLVED. Crookston, Rehfeldt, Dixon, Weiskittel (2010) cited in §5.4. |
| 5 | GY2 site index treatment | PARTIALLY RESOLVED. Source and base age noted in §3.1 and §3.3 (FIA SICOND, base age 50). |
| 6 | D9 "three framing statistics" | DEFERRED. Author preference; not a blocker. |
| 7 | Generic SI cross references | DEFERRED. Numbered cross-references in revision. |
| 8 | "More than doubled" pattern | N/A this manuscript. |

# Section 4. New v2 observations from the FIA Bakuzis revision

1. **Section 4.6 prose tightness.** The revised §4.6 is information-
   dense. Consider splitting the long paragraphs into shorter blocks
   for figures 8-11 individually.

2. **Figure 11 band growth percentage.** The revised text states
   bands grow to 30-50 percent of the median by year 100 (real
   stands), versus 5-15 percent in the prior synthetic version. This
   is a substantive shift driven by within-cell stand-to-stand
   variability now being included in the band. The figure caption
   and discussion correctly attribute this to stand variability rather
   than parameter uncertainty alone, but the §5.1 implications
   paragraph should also note this.

3. **Eichhorn drop on NE.** NE Eichhorn compliance is 44 percent
   under default and MAP and 22 percent under the posterior band.
   This is the only law where the posterior reduces compliance
   relative to point estimates. The §4.6 text correctly flags this
   as warranting targeted refinement of species random effect priors.
   Consider adding a sentence in §5.5 future work mentioning a
   targeted prior elicitation experiment for the NE species random
   intercepts.

4. **Marshall CSV adapter committed but unused.** A
   `marshall_to_fia_csv.py` adapter is in the repo for future PN/SN/IE
   FIA stratified sampling. It cannot be exercised until the FVS-PN/SN
   library issues are resolved. The §5.4 limitation paragraph and the
   `PN_SN_LIBRARY_DIAGNOSIS.md` note adequately document this.

# Section 5. Updated checklists

## General

| Code | Item | v1 | v2 |
|------|------|----|----|
| G1 | Reproducibility | PARTIAL (DOI) | PARTIAL (DOI still pending) |
| G2 | Software/version | MET | MET |
| G3 | Random seeds | PARTIAL | MET |

## Statistical

| Code | Item | v1 | v2 |
|------|------|----|----|
| S1 | Tests appropriate | MET | MET |
| S2 | Sample size | MET | MET |
| S2b | Spatial autocorrelation | NOT MET | MET |
| S3 | Assumptions tested | PARTIAL | PARTIAL (deferred to companion) |
| S6 | Credible intervals | MET | MET |
| S7 | Statistical vs practical | MET | MET |

## Mathematical

| Code | Item | v1 | v2 |
|------|------|----|----|
| M1 | Equations numbered | NOT MET | MET |
| M2 | Parameter estimates with SE | PARTIAL | MET (SI architecture note) |
| M3 | Notation consistency | MET | MET |

## Validation

| Code | Item | v1 | v2 |
|------|------|----|----|
| V1 | Independent validation | NOT MET | MET (reframed plus FIA Bakuzis) |
| V2 | Validation metrics | MET | MET |
| V3 | Residual diagnostics | PARTIAL | PARTIAL (SI numbered refs deferred) |
| V4 | Validation sample size | MET | MET |

## Growth and yield modeling

| Code | Item | v1 | v2 |
|------|------|----|----|
| GY1 | Component model forms | PARTIAL | MET (equations 1-7) |
| GY2 | Site index treatment | NOT MET | MET (SICOND base 50) |
| GY3 | Stand density treatment | MET | MET |
| GY4 | Long-horizon stability | PARTIAL | MET (FIA Bakuzis trajectories) |
| GY5 | Climate sensitivity | NOT MET | MET (Climate-FVS cited) |
| GY6 | Comparison to alternatives | NOT MET | MET (Joo, Premer, Woodall) |

# Section 6. Updated scoring

| Dimension | v1 | v2 | Confidence | v2 justification |
|-----------|---:|---:|------------|------------------|
| D1 Originality | 4 | 4 | M | Integration novelty unchanged. |
| D2 Significance to field | 5 | 5 | H | Unchanged. |
| D3 Methodological rigor | 3 | **5** | M | M1 equations, S2b autocorrelation, V1 reframing all closed. |
| D4 Statistical appropriateness | 4 | 4 | H | Unchanged; HMC plus ADVI fallback. |
| D5 Reproducibility | 3 | **4** | H | DOI still pending (only blocker). |
| D6 Model validation | 3 | **5** | H | FIA stratified sampling on real stands removes the synthetic artifact concern entirely. |
| D7 Literature coverage | 3 | **5** | H | All six recommended 2025 citations and the Crookston references inserted. |
| D8 Interpretation quality | 4 | **5** | M | NE Sukachev now confirmed as variant property; ACD posterior Eichhorn corroborated on real stands. |
| D9 Writing clarity | 4 | 4 | H | §4.6 dense but readable. |
| D10 Figures and tables | 4 | 4 | H | Per-variant rendering added; figures clean. |

**Average score: v1 = 3.7, v2 = 4.5.** Indicative recommendation
under v2: **Accept** if the corresponding author is comfortable
proceeding without a final external reviewer pre-pass; **Minor
Revision** if a final reviewer round is preferred for stylistic
polish.

# Section 7. Pre-submission checklist

| Item | Status |
|------|--------|
| FIA Bakuzis numbers in §4.6 | DONE |
| §3.3 methods updated for FIA path | DONE |
| §5.4 limitations updated | DONE |
| Figures regenerated (NE plus ACD) | DONE |
| pandoc docx regenerated | DONE |
| commit pushed to origin/main | DONE |
| Zenodo webhook enabled | TODO (manual) |
| Release tag minted (v2026.05.1) | TODO (this session) |
| DOI replaces placeholder in software availability block | TODO (after Zenodo) |
| Co-author final review | TODO (after author distribution) |

# Disclosure

This v2 self-assessment was conducted by the corresponding author
using the manuscript-review framework with AI assistance for
structural organization and consistency checking. All scientific
judgments and the indicative recommendation are the responsibility
of the corresponding author. Manuscript content was processed only
within the local workspace; no external submission to AI services
occurred.

---
title: "Self-review report v3: fvs-modern combined manuscript (post-revision)"
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
| Document version | manuscript/fvs_combined_draft.docx (commit on origin/main) |
| Self-review date | 2026-04-26 (v3) |
| Prior reviews | v1 manuscript/20260425_ems_self-review_fvs-modern.md; v2 manuscript/20260426_ems_self-review_fvs-modern_v2.docx |

# Section 1. Summary assessment

This v3 self-review re-runs the manuscript-review skill across all
five modules following the v2026.05.3 release with minted Zenodo
DOI 10.5281/zenodo.19802673. Six targeted revisions were applied as
the review proceeded, listed in section 2 below. The manuscript now
totals 7,617 words across 1,062 lines, with eleven embedded figures
and a complete reference list of 17 entries.

The headline scientific contribution is unchanged: ACD posterior
credible band achieves 50 percent Eichhorn rule compliance versus
20 percent for both default and MAP point estimates on real FIA
stands, providing the strongest single empirical argument for
posterior parameter propagation as biological information rather
than software plumbing.

Indicative recommendation under v3: **Accept**.

# Section 2. Revisions applied during this review

| # | Concern | Location | Resolution |
|---|---------|----------|-----------|
| R1 | Abstract over-claimed "exposes the four classical biological laws" when one of four (mortality size pattern) is deferred. | Abstract, paragraph 4 | Reframed as "tests three of the four classical biological laws" with the fourth flagged as a design element. |
| R2 | §2.2 stated "500 joint posterior samples" without explaining how 500 relates to the 4,000 retained MCMC iterations or how downstream tools choose draw counts. | §2.2 paragraph after equations | Added explicit text: "thinned by simple uniform subsampling from the 4,000 retained MCMC iterations" and "Downstream ensemble runs typically consume between 50 and 500 of these joint draws... the FIA Bakuzis evaluation reported here used 100 draws." |
| R3 | §4.4 Figure 6 envelope source was unclear; readers might confuse the residual bootstrap with the §4.6 parametric posterior credible band. | §4.4 first paragraph | Added prose: "The envelope here is a non parametric bootstrap of FIA benchmark residuals... distinct from the parametric posterior credible band in section 4.6: the bootstrap captures empirical prediction error... whereas the posterior band captures parameter uncertainty propagated through the model." |
| R4 | §5.3 Computational resources mixed synthetic (single-stand) and FIA-mode (five-stand) Bakuzis cost figures without distinction. | §5.3 final paragraph | Clarified that the 80 CPU minutes / 5 hour figures are for the synthetic single-stand design; the FIA-mode five-plot evaluation at 100 draws runs in 30-60 wall clock minutes. |
| R5 | §3.1 omitted explicit site index treatment (GY2 checklist item). | §3.1 second paragraph | Added: "Site index for each condition is taken from the FIA SICOND field referenced to a base age of 50 years (FIA SIBASE = 50)... conditions reporting a non-50 base age were excluded from the benchmark." |
| R6 | Reynolds (1984) listed in references but not cited in body (Module 4 dead reference). | §3.1 equivalence metric paragraph | Cited Reynolds (1984) as the methodological foundation for the 20 percent equivalence metric. |

# Section 3. Resolution of v1 and v2 issues

All major concerns from the v1 and v2 reviews remain addressed in
this revision. The DOI is now resolvable (10.5281/zenodo.19802673);
all six 2025 citations are present; equation forms are numbered;
spatial autocorrelation is explicitly noted as a recommended
extension; random seeds are stated; the Climate-FVS reference is
present; comparative positioning vs Joo, Premer, Woodall is in
§5.1; CRediT and competing interests are present.

# Section 4. New v3 observations

1. **§2.2 random seeds discipline**. The seed convention `20260201
   + chain_index` is appropriately specific. No further change.

2. **§3.1 spatial autocorrelation deferral**. The honest statement
   that Moran's I was not computed and the explicit recommendation
   for a follow-up extension is sufficient for the present scope.
   The Cardinal pipeline supports it.

3. **Abstract symmetry**. The abstract now claims "internal FIA
   benchmark" which is consistent with §3.1 honest framing and §4.3
   numerical results.

4. **Figure 6 envelope vs Figure 8 ribbon**. After R3, the two
   uncertainty representations are clearly differentiated in prose.
   The figures themselves are unmodified; only the surrounding
   discussion was clarified.

5. **PN/SN/IE coverage caveats**. §5.4 limitations now correctly
   describes the FVS-PN/SN/IE state: shared libraries build cleanly
   and load via ctypes, but the runtime keyword reader EOF affects
   all three variants and is documented as future work. No change
   to §5.4 needed in this revision.

# Section 5. Updated checklists

## General

| Code | Item | v2 | v3 |
|------|------|----|----|
| G1 | Reproducibility | PARTIAL | MET |
| G2 | Software/version | MET | MET |
| G3 | Random seeds | MET | MET |

## Statistical

| Code | Item | v2 | v3 |
|------|------|----|----|
| S1 | Tests appropriate | MET | MET |
| S2 | Sample size | MET | MET |
| S2b | Spatial autocorrelation | MET | MET (deferral acknowledged) |
| S3 | Assumptions tested | PARTIAL | PARTIAL (cross-ref to companion) |
| S6 | Credible intervals | MET | MET |
| S7 | Statistical vs practical | MET | MET |

## Mathematical

| Code | Item | v2 | v3 |
|------|------|----|----|
| M1 | Equations numbered | MET | MET |
| M2 | Parameter estimates with SE | MET | MET (SI architecture note) |
| M3 | Notation consistency | MET | MET |

## Validation

| Code | Item | v2 | v3 |
|------|------|----|----|
| V1 | Independent validation | MET (reframed) | MET |
| V2 | Validation metrics | MET | MET |
| V3 | Residual diagnostics | PARTIAL | PARTIAL |
| V4 | Validation sample size | MET | MET |

## Growth and yield modeling

| Code | Item | v2 | v3 |
|------|------|----|----|
| GY1 | Component model forms | MET | MET |
| GY2 | Site index treatment | PARTIAL | MET (R5 fix) |
| GY3 | Stand density treatment | MET | MET |
| GY4 | Long-horizon stability | MET | MET |
| GY5 | Climate sensitivity | MET | MET |
| GY6 | Comparison to alternatives | MET | MET |

# Section 6. Updated scoring

| Dimension | v2 | v3 | Confidence | v3 justification |
|-----------|---:|---:|------------|------------------|
| D1 Originality | 4 | 4 | M | Integration novelty unchanged. |
| D2 Significance to field | 5 | 5 | H | Unchanged. |
| D3 Methodological rigor | 5 | 5 | H | All M, S, V items closed; GY2 now MET. |
| D4 Statistical appropriateness | 4 | **5** | H | Posterior subsampling rationale (R2) closes the last ambiguity. |
| D5 Reproducibility | 4 | **5** | H | DOI minted and resolvable. |
| D6 Model validation | 5 | 5 | H | Unchanged. |
| D7 Literature coverage | 5 | 5 | H | Reynolds 1984 now cited (R6). |
| D8 Interpretation quality | 5 | 5 | M | Unchanged. |
| D9 Writing clarity | 4 | 4 | H | §4.6 still dense but readable; figures dense but useful. |
| D10 Figures and tables | 4 | 4 | H | Unchanged. |

**Average score: v2 = 4.5, v3 = 4.7.** Indicative recommendation
under v3: **Accept**.

# Section 7. AI content screen summary

LOW signal. Lexical scan found only 2 instances of high-risk
vocabulary (both "harness", in technical context). Hyphen scan
shows compound modifiers and the project name `fvs-modern`, no
decorative punctuation. Structural cohesion intact across sections.
Citation pattern shows wide year range (1933-2025) with no narrow
clustering. No AI-pattern flags raised.

# Section 8. Citation integrity audit summary

All 17 references present and well-formed. Reynolds (1984), flagged
in v3 Module 4 as previously unused, is now cited in §3.1 (R6).
Three 2025 citations verified by DOI presence
(10.1007/s44392-025-00041-0 Joo, 10.1007/s44392-025-00056-7 Premer,
10.1007/s44392-025-00012-5 Woodall). No Type A through Type F
anomalies detected. [REVIEWER VERIFY] for content match of recent
2025 references against the specific claims they support is
recommended but not blocking.

# Section 9. Pre-submission checklist

| Item | Status |
|------|--------|
| Section 4.6 with real FIA Bakuzis numbers | DONE |
| Section 3.3 methods describes FIA stratified sampling path | DONE |
| Section 5.4 limitations honestly frames PN/SN/IE state | DONE |
| Section 5.5 future work lists runtime regression as item six | DONE |
| Site index treatment specified (R5) | DONE |
| Posterior draw subsampling rationale (R2) | DONE |
| Bootstrap vs posterior uncertainty distinction (R3) | DONE |
| FIA vs synthetic Bakuzis cost distinction (R4) | DONE |
| Abstract claim aligned to actual law coverage (R1) | DONE |
| Reynolds (1984) cited (R6) | DONE |
| All six 2025 citations present | DONE |
| Equation forms numbered | DONE |
| Random seeds stated | DONE |
| Climate-FVS reference | DONE |
| Comparative positioning | DONE |
| CRediT statement | DONE |
| Competing interests declared | DONE |
| Software availability with resolvable Zenodo DOI | DONE |
| CITATION.cff with DOI identifier | DONE |
| README with Zenodo badge | DONE |
| Co-author final review | TODO (after distribution) |

# Disclosure

This v3 self-assessment was conducted by the corresponding author
using the manuscript-review framework with AI assistance for
structural organization and consistency checking. All scientific
judgments and the final recommendation are the responsibility of
the corresponding author. Manuscript content was processed only
within the local workspace; no external submission to AI services
occurred.

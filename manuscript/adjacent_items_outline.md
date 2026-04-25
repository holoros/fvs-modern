# Adjacent items outline

Companion deliverables that flow from the fvs-modern v2026.04.7
release and the combined manuscript. Each is sized for a specific
audience and channel. Deliverables are ordered from highest near
term impact (external visibility) to lower priority (long tail
outreach).

## 1. Extension and outreach short note (2 pages, markdown or PDF)

**Audience.** Forest managers, state and federal agency staff,
consulting foresters, research cooperators. Non technical.

**Distribution channels.**
  - CRSF website front page blog post
  - Northern Hardwoods Research Institute newsletter
  - SAF western and eastern chapter bulletins
  - University of Maine Center for Research on Sustainable Forests
    bi monthly update

**Key messages (maximum three).**
  - Calibrated FVS is more accurate across the entire country. Basal
    area predictions improved in 19 of 19 variants tested; volume in
    17 of 19. Calibrated RMSE is 4.9 percent lower overall.
  - Calibrated FVS now reports uncertainty. Every projection can
    include 95 percent credible bands computed from 500 posterior
    draws, so you can see how much parameter uncertainty affects
    your management decision.
  - Calibrated FVS is free and open source. Docker, Python, and R
    interfaces are provided alongside the original Fortran. Anyone
    can download it at github.com/holoros/fvs-modern.

**Structure.**
  - Hero stat callout (4.9 percent RMSE reduction, 500 draws, 25
    variants)
  - What changed (three short paragraphs)
  - Who this helps (carbon accounting, carbon markets, harvest
    scheduling, climate adaptation planning)
  - How to try it (one line install for Docker)
  - Citation and contact

**Target length.** 800 words, two figures (CONUS improvement map,
trajectory with band).

**Status.** Unblocked. Can draft today.

## 2. Scientific poster (A0, SAF or IUFRO)

**Audience.** Conference attendees at SAF national convention
(November), IUFRO world congress (next occurrence), or state G and
Y working group.

**Structure (seven panels).**
  1. Title plus author block plus institution logos
  2. Motivation (why modernize; three framing stats)
  3. Pipeline architecture (reuse Figure 1 from manuscript)
  4. FIA benchmark (Figure 2 plus spatial map)
  5. Bakuzis matrix results (one trajectory grid plus law panel)
  6. Try it (QR code to repo, Docker pull command)
  7. Acknowledgments plus references

**Assets already available.**
  - fig2_fia_benchmark.png
  - pctrmse_reduction_spatial.png
  - fig_bakuzis_trajectories.png (bootstrap; swap for real after
    Cardinal completes)
  - fig_bakuzis_laws.png
  - figure3_pipeline_architecture.png

**Status.** Can scaffold today with the available figures; swap real
Bakuzis panels after Cardinal returns.

## 3. Release notes blog post

**Audience.** Software users and contributors.

**Channel.** fvs-modern GitHub release page, CRSF website changelog,
cross post to the GMUG mailing list.

**Outline.**
  - What's new in v2026.04.7 (CITATION metadata, .gitignore hardening,
    Zenodo webhook note)
  - Key features since v2026.04.0 (list of calendar tags with one line
    summaries)
  - Highlights (Bayesian calibration, 25 variants, uncertainty engine)
  - Install options (Docker, Python pip, R devtools)
  - Known issues (iet03 baseline refresh pending, Canadian variant
    IMPLICIT NONE hardening pending, CONUS unified variant in flight)
  - Roadmap (v2026.05.0 targets)
  - Feedback channels (GitHub issues, email)

**Target length.** 600 words.

**Status.** Unblocked. Useful to publish alongside Zenodo DOI once
webhook is enabled.

## 4. PNAS v6 response review (second pair of eyes)

**Audience.** Aaron internally, before the PNAS resubmission.

**Scope.** Read PNAS_v6_Cover_Letter.docx, PNAS_v6_Response_to_Reviewer.docx,
and PNAS_v6_Results_P1_revision.md. Check for:
  - Internal consistency against the combined manuscript claims
  - Accuracy of variant counts (25, not 24) and component counts
    (seven, not six)
  - Citation format and completeness
  - Response to reviewer tone (professional, specific, grounded in
    evidence rather than assertion)
  - Any places where fvs-modern v2026.04.7 release evidence should be
    cited in the response

**Output.** Short review memo (1 to 2 pages) with line by line or
section level comments.

**Status.** Can be done any time; independent of Cardinal.

## 5. Methods in Ecology and Evolution alternative framing

**Audience.** Methodological readers. Useful if EMS bounces or if
Aaron wants parallel submission.

**Structure.** Same content as the EMS draft but with:
  - Heavier emphasis on the Bayesian inference strategy (HMC plus
    ADVI fallback)
  - Uncertainty propagation as the novel methodological contribution
  - Bakuzis matrix reframed as a formal model validation framework
  - Compact Methods section with more mathematical rigor

**Effort.** 4 to 6 hours of restructure from the existing EMS draft.

**Status.** Hold until EMS response.

## 6. Teaching module (graduate forest biometrics)

**Audience.** Aaron's graduate students at UMaine, and possibly
external shared material.

**Deliverables.**
  - One lab exercise (2 hours): students build the NE calibrated
    shared library, run a projection via fvs2py, and plot an
    uncertainty band with tidyverse
  - One problem set (take home): students compare default and
    calibrated projections across a gradient of site classes and
    interpret the Bakuzis law compliance
  - Lecture slides (10 to 15 slides): principles of Bayesian
    calibration for growth and yield, posterior propagation, and
    validation

**Status.** Low priority; build once the main manuscript is in
review.

## 7. CRSF annual report section

**Audience.** CRSF advisory board, UMaine administration, funding
agencies.

**Content.** Short paragraph in the 2026 annual report's Impacts
section noting the fvs-modern release, the NSF or USDA funding that
supported calibration, and the impact metrics (downloads, citations,
community contributors).

**Status.** Deferred until annual reporting cycle.

---

## Recommended priority order

Given current state (manuscript draft in hand, Bakuzis runs pending,
Zenodo webhook not yet enabled):

1. **PNAS v6 response review** now (independent of everything, fast)
2. **Extension short note** after PNAS to capitalize on the
   accompanying calibration paper release
3. **Release notes blog post** once Zenodo DOI is minted
4. **Scientific poster** for the next appropriate conference cycle
5. **Teaching module** during summer break
6. **MEE alternative** only if EMS bounces
7. **CRSF annual report** during the reporting cycle

The PNAS review is the only item that can meaningfully accelerate
other deliverables, since errors caught there propagate to the EMS
draft. The extension note benefits from the PNAS calibration paper
being in review or accepted when it publishes. The blog post should
follow the first minted Zenodo DOI.

---
title: "Co-author distribution — fvs-modern manuscript final draft"
date: 2026-04-26
---

To: Greg Johnson <gjohnson@growthmodel.org>; David Marshall <dmarshall@fs.fed.us>
From: Aaron Weiskittel <aaron.weiskittel@maine.edu>
Subject: fvs-modern manuscript final draft — review request before EMS submission

Greg and David,

The fvs-modern manuscript is at a complete pre-submission state and
I would value your review before I submit to Environmental Modelling
and Software. Target submission date is the week of May 5 if your
feedback lands by May 2.

Attached:

1. fvs_combined_draft.docx — main manuscript (7,617 words, 11 figures)
2. fvs_combined_SI.docx — supplementary materials (1,263 words, 41 figures)
3. cover_letter_ems.docx — draft cover letter
4. 20260426_ems_self-review_fvs-modern_v3.docx — internal self-review
   (manuscript-review framework, scoring 4.7/5.0, recommendation Accept)

Repository state:

- GitHub: https://github.com/holoros/fvs-modern
- Tag: v2026.05.3 (current; commit 4c8ec62 on origin/main)
- Zenodo DOI: 10.5281/zenodo.19802673
  (https://doi.org/10.5281/zenodo.19802673)

What changed since you last saw the manuscript:

The biggest substantive change is Section 4.6 (Bakuzis matrix
outcomes), now run on real FIA condition pairs sampled by site index
and basal area band rather than synthetic algorithmic stands. Five
real plots per cell, 100 posterior draws per scenario, NE and ACD
variants on OSC Cardinal. The headline finding is preserved and
strengthened: ACD posterior credible band achieves 50 percent
Eichhorn rule compliance on real stands compared with 20 percent for
both default and calibrated MAP point estimates. NE Sukachev 0
percent compliance is now confirmed as a real variant property
rather than a synthetic stand artifact.

Section 3.3 methods now describes the FIA stratified sampling path
explicitly. Section 5.4 limitations honestly frames the FVS-PN, SN,
IE state: shared libraries build cleanly and load via Python ctypes
after this release's repairs, but a Fortran runtime regression in
the keyword reader still affects all three variants. Section 5.5
future work lists this as the sixth outstanding item.

Six smaller revisions came out of an internal manuscript-review pass
this morning (R1 through R6 in the v3 self-review document). They
are: abstract reframed to "three of four" biological laws (since
mortality size pattern is deferred); Section 2.2 added rationale for
serializing 500 joint posterior samples thinned from 4,000 retained
MCMC draws; Section 4.4 distinguishes the residual bootstrap envelope
in Figure 6 from the parametric posterior credible band in Figure 8;
Section 5.3 distinguishes synthetic single stand and FIA mode five
plot Bakuzis compute costs; Section 3.1 added explicit site index
treatment (FIA SICOND, base age 50); and Reynolds (1984) is now
cited in Section 3.1 as the methodological foundation for the 20
percent equivalence metric.

What I would value most from each of you:

Greg, your perspective on the operational framing in Section 5
(particularly 5.1 scientific implications and 5.2 software
engineering implications) and on the comparative positioning against
Joo et al. (2025), Premer et al. (2025), and the Forest Carbon
Modeling Group statement (Woodall et al. 2025). Your review of
Section 4.6 Bakuzis findings would also be welcome given your
familiarity with the Acadian variant.

David, your review of Sections 2.1 through 2.3 (software
description) and Section 4.1 (modernization outcomes) given your
deep familiarity with FVS internals. I would also appreciate your
feedback on Section 5.4 limitations, specifically the framing of the
FVS-PN/SN/IE runtime regression.

If either of you wants to contribute additional material, the most
valuable additions would be (a) any prior performance results for
your respective variants that are missing from Section 4.3, or (b)
language tightening any of the discussion sections.

Authorship and acknowledgments:

Author order is alphabetical after Aaron (Johnson, Marshall) per
prior agreement. CRediT statement currently lists me as
conceptualization, methodology, calibration pipeline, software
engineering, formal analysis, writing original draft. Greg and David
are listed for software internals review, Pacific Northwest variant
expertise, and editing. If either of you would like a different
contribution allocation, please let me know.

The manuscript declares no competing interests for any of us. If
you want to add a specific funding acknowledgment beyond the existing
text, send the language and I will integrate it.

Practical:

Tracked changes are welcome. If easier, mark up the docx and send
back, or open issues against the GitHub repo. Please flag anything
that should not appear in the EMS submission, including any phrasing
you find inaccurate about your own contributions.

Thank you both for your work on this and for the review.

Aaron

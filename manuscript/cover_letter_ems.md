---
title: "Cover letter — Environmental Modelling and Software submission"
date: 2026-04-26
---

April 26, 2026

Dr. Anthony J. Jakeman
Editor-in-Chief
Environmental Modelling and Software

Dear Dr. Jakeman,

We are pleased to submit the enclosed manuscript, "Modernizing and
Bayesian recalibrating the Forest Vegetation Simulator: open source
infrastructure and parametric uncertainty quantification for 25
geographic variants," for consideration as a Software Description
in Environmental Modelling and Software.

The manuscript reports an integrated open source release that
combines three substantive advances in the FVS modeling ecosystem.
First, the full Fortran 77 source tree of FVS, the operational
standard for individual tree growth and yield projection across
federally managed United States forests, has been converted to
standards conformant Fortran 90. Second, all 25 geographic variants
have been recalibrated using Bayesian hierarchical models fit to
Forest Inventory and Analysis remeasurement data covering roughly
two decades of post 2005 panels. Third, a runtime uncertainty engine
propagates posterior parametric draws through stand projections,
producing credible bands rather than point estimates for the first
time in a production growth and yield system at this scale.

Three empirical applications motivate the release. An internal FIA
benchmark on 433,291 remeasurement conditions across 19 variants
shows that calibrated parameters improve basal area R squared from
0.672 to 0.704 and gross volume R squared from 0.783 to 0.818. An
ablation analysis attributes roughly 60 percent of that improvement
to the Wykoff diameter growth recalibration, with the Chapman
Richards height to diameter component contributing another 25
percent. A Bakuzis matrix factorial drawing real FIA condition pairs
demonstrates that the calibrated posterior credible band achieves 50
percent compliance with Eichhorn's rule on the Acadian variant
versus 20 percent for both default and calibrated MAP point
estimates, a 2.5 fold uplift that we interpret as posterior
parameter propagation carrying biological information rather than
software plumbing.

The submission fits Environmental Modelling and Software's scope on
several axes. Methodologically, the work integrates Bayesian
inference, software engineering, and quantitative model evaluation in
a single environmental modeling platform. Operationally, the release
addresses Woodall et al. (2025), who identify model uncertainty
quantification as the top community priority for the next decade in
forest carbon modeling, and complements Joo et al. (2025) and Premer
et al. (2025) on the comparative landscape of growth and yield
systems. Reproducibly, the full source code, calibrated parameter
files, and benchmarking scripts are archived at Zenodo with DOI
10.5281/zenodo.19802673 (https://doi.org/10.5281/zenodo.19802673),
indexed against tagged releases on GitHub at
https://github.com/holoros/fvs-modern, and built and tested via a
GitHub Actions continuous integration pipeline.

The manuscript is approximately 7,600 words across six sections plus
references, with eleven embedded figures. A supplementary materials
package provides additional per variant performance plots,
calibration diagnostics, posterior architecture notes, and Acadian
Bakuzis figures (41 figures, 1,300 words).

This work has not been published elsewhere and is not under review at
another journal. All authors have approved the submission. We
declare no competing interests. The manuscript was prepared with AI
assistance for structural organization and consistency checking; all
scientific judgments and the final text are the responsibility of
the authors.

We suggest the following reviewers based on relevant expertise:

- Dr. Malcolm Itter, North Carolina State University (Bayesian
  growth and yield calibration). Email: msitter@ncsu.edu
- Dr. Andrew Finley, Michigan State University (hierarchical models
  for forest data). Email: finleya@msu.edu
- Dr. John Coulston, USDA Forest Service Southern Research Station
  (FIA program leadership). Email: jcoulston@fs.fed.us
- Dr. Hailemariam Temesgen, Oregon State University (G&Y modeling
  in the Pacific Northwest). Email: hailemariam.temesgen@oregonstate.edu

We ask that the following reviewers not be approached due to recent
collaboration: Drs. Greg Johnson and David Marshall (manuscript
co-authors), and any current University of Maine faculty.

Thank you for considering this submission. We look forward to your
editorial decision.

Sincerely,

Aaron R. Weiskittel
Center for Research on Sustainable Forests
University of Maine
Orono, ME 04469
aaron.weiskittel@maine.edu
ORCID: 0000-0002-9249-1686

On behalf of co-authors:
Greg Johnson, Growth Model Users Group
David Marshall, USDA Forest Service Pacific Northwest Research Station

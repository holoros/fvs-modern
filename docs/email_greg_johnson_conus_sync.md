# Draft: CONUS sync email to Greg Johnson

**To:** greg@nosnhoj.org
**From:** aaron.weiskittel@maine.edu
**Subject:** fvs-modern v2026.04 tag and CONUS branch next steps

Greg,

We are about to cut the first public tag of fvs-modern (v2026.04.0) this
week as the reproducibility anchor for the v3 manuscript. That closes out
the regional variant calibration fleet at 66/67 tests passing and leaves
the CONUS unified variant as the headline item for the next tag.

A few things I'd like to align on before I push tag and open the
`conus-variant` branch in earnest:

1. DG form. You are fitting exp(b0 + b1*log((dbh+1)²/(cr*ht+1)^b3) +
   b2*bal^b4/log(dbh+2.7) + b5*elev + b6*EMT) species-by-species. Are you
   comfortable standardizing on that form for the unified variant, or do
   you want to revisit the ORGANON base (Hann et al. 2006) before we
   commit? I can go either way; I'd rather settle it before the branch
   gets real users.
2. Annualization. I am planning to use the Cao (2000)/Weiskittel et al.
   (2007) iterative approach on the FIA remeasurement pairs. Any concerns
   with that, or results from your own runs worth comparing against
   first?
3. Ingrowth. This is the biggest gap on my side. Do you have a draft
   equation I can fit against, or do we want to pull Bob Froese or
   someone else in to own it?
4. SN weakness and the mortality U-shape. I'd like to scope both for the
   next calendar tag. If you have time for a one-hour call in the next
   two weeks, I'll send a Doodle.

The v3 manuscript preprint is attached. Section 3.5 and the uncertainty
discussion are where your CONUS work would most naturally plug in for
a follow-on.

Thanks again,
Aaron

# Acadian annual calibration table: how it was produced

`acd_annual_calibration.csv` holds annual diameter increment multipliers
(`dDBH.mult`) for the Acadian model, fit against real ACD FIA remeasurement.
This is the table the bridge reads in place of the FVS-NE `baimult`.

## Method (v3, current)

`fit_acd_annual_calibration_v3.R` reads the ACD remeasurement records
(`fvs-conus/data/processed/acd/diameter_growth.csv`, 575,461 trees, 17,326
plots, 5 year remeasurement). For each tree it iterates the real Acadian
`dDBH_fun` annually over the tree's interval and finds the per species annual
multiplier that minimizes squared error between predicted and observed periodic
diameter growth. Species random effects come from `ddbh.fun.spp` in
`AcadianGY.R`.

Two corrections versus the first pass:

1. Units. The recorded BAL and BA columns are in ft2/acre (stand BA averages
   118.8 ft2/acre, about 27 m2/ha), but `dDBH_fun` expects BAL in m2/ha. The
   first pass fed ft2/acre straight in (about 4.35x too high), which over
   suppressed growth and inflated the multipliers. v3 converts recorded BAL to
   m2/ha (median 15.5) before fitting.

2. Competition split. The larger tree basal area is split into BAL.SW and
   BAL.HW using a per tree larger tree softwood fraction (softwood = SPCD < 300)
   reconstructed from the plot records, then applied to the converted BAL. This
   replaces the earlier total BAL as BAL.SW approximation.

## Result

34 Acadian species fitted; remaining valid codes filled with 1.0 so the bridge
always finds a row. Diameter growth RMSE drops from 1.081 cm to 1.008 cm, a 6.8
percent reduction. With the corrected competition scale, the uncalibrated
Acadian equations under predict annual diameter growth modestly for most species
(balsam fir multiplier 1.14, red spruce 1.12, eastern hemlock 1.22, white pine
1.10) with larger corrections for fast growing hardwoods (red oak 1.92, red
maple 1.50, quaking aspen 1.48, bigtooth aspen 1.61, American beech 1.47). A few
sit below 1 (silver maple 0.74, butternut 0.65, black oak 0.94, white oak 0.95).
Multiplier range 0.65 to 1.92.

## Height and mortality

`dHt.mult` and `mort.mult` are shipped as 1.0. A height fit was attempted but is
not trustworthy: the `HT_annual` column carries implausible outliers (up to 10.2
ft/yr) and a height proxy from DBH had to be used, so the optimizer saturated.
The `dHt_diag` column in the CSV records the (unshipped) height diagnostic for
transparency. Proper height calibration needs measured `HT_t1`/`HT_t2` with
outlier handling; mortality needs a survival fit on `STATUSCD`. Both are flagged
as the next refinement.

## Assumptions and caveats

1. Competition is held at the start of period value across the interval, matching
   how the model applies within period competition; BAL does not evolve year to
   year inside the fit.
2. The softwood fraction is weighted by `DBH^2 * TPA`; the absolute TPA scale is
   unreliable in these records but cancels in the fraction.
3. Climate site index is mapped from FVS site index in feet using the bridge map,
   defaulting to 12 when missing.
4. These are bridge calibration factors under the stated assumptions, not new
   biological parameters.

## How the bridge consumes it

`make_acd_calib_from_table.R` (tested, base R) reads this CSV and builds the
calib.spp data frame the model expects (SP, dDBH.mult, dHt.mult, mort.mult,
max.dbh, max.height), keeping the size caps from FVS. It defaults species absent
from the table to multiplier 1. See `bridge_patch_baimult_scale.md` for where it
slots into `make_fvs_calib()`.

# Fortran FVS-NE/ACD on Canadian MAGPlot: diagnosis and RESOLUTION

2026-05-21. Goal: run the Fortran FVS engine (FVS-NE and FVS-ACD, default and
calibrated) on Canadian (New Brunswick) MAGPlot tree lists. Reported as "blocked
by the fvs2py inventory issue."

## RESOLVED

MAGPlot tree lists now ingest and project through the Fortran FVS engine. The
working route is the standalone FVS binary (`lib/FVSne`, `lib/FVSacd`) with a
DATABASE/STANDSQL keyfile and a SQLite inventory DB, ONE stand per subprocess.
`magplot_fvs_runner.py` implements it and was validated on NB stands:

    [magplot] NB initial live trees 479,100  crosswalk coverage 100.0%
      30300001 ne/default : 63 trees  BA 34.9->62.0   TPA 1548->1526
      30300001 acd/default: 63 trees  BA 34.9->96.0   TPA 1548->1504
      30300002 ne/default : 56 trees  BA 23.9->38.0   TPA 830->818
      30300002 acd/default: 56 trees  BA 23.9->59.3   TPA 830->806
      30300003 ne/default : 55 trees  BA 30.8->48.0
      30300003 acd/default: 55 trees  BA 30.8->71.7
      30300004 ne/default : 69 trees  BA 46.7->70.0
      30300004 acd/default: 69 trees  BA 46.7->101.8

Species crosswalk coverage is 100 percent of NB stems. FVS-NE and FVS-ACD produce
clearly different projections (for example stand 30300001 horizon BA 62.0 vs 96.0),
which is the NE vs ACD divergence showing up on Canadian data.

## Why the fvs2py route was blocked (background)

The original attempt used the fvs2py Python wrapper, which has three problems:

1. fvs2py does not import on the cluster default Python 3.9 (it uses
   `enum.StrEnum`, 3.11+, and `typing.ParamSpec`, 3.10+). It imports under
   `module load python/3.12`.
2. Even under 3.12, fvs2py's `run()` did not drive a DATABASE keyfile run to
   completion: `restart_code` stayed 0 and no output tables were written, even
   for a clean synthetic stand. So the engine never produced results via fvs2py.
3. Multiple fvs2py runs in one process crash at `keyrdr.f90:47`
   ("Sequential READ after EOF") because FVS keeps unit 15 open across runs.

The standalone binary sidesteps all three: it needs no fvs2py (so no Python
version constraint), it drives itself to completion and writes the output DB, and
one stand per subprocess is always a fresh process. The known cosmetic STOP
traceback at the end of a standalone run does not affect the written results.

## The converter

`magplot_fvs_runner.py` (needs only sqlite3 + pandas + the FVS binaries):

- species crosswalk: MAGPlot botanical genus.species (ABIE.BAL, PICE.RUB, ...) to
  FIA SPCD, covering the NB species; generics and unknowns map to FVS "other".
- units: dbh cm to inches, height m to feet, stem/ha to trees per acre.
- initial measurement (meas_num 0), live trees only.
- builds a SQLite DB (fvs_standinit + fvs_treeinit), writes a DATABASE keyfile,
  runs `FVS{variant}` standalone per stand, reads FVS_Summary2 from the output DB.
- supports variants ne, acd and configs default, calibrated (the calibrated
  config injects FvsConfigLoader keywords before PROCESS).

## Run it (works on the cluster default Python 3.9)

    export FVS_PROJECT_ROOT=/users/PUOM0008/crsfaaron/fvs-modern
    export FVS_LIB_DIR=/users/PUOM0008/crsfaaron/fvs-modern/lib
    python3 magplot_fvs_runner.py --nstands 5 --variants ne,acd --configs default,calibrated

## Remaining polish (not blockers)

- Validate the calibrated configs actually load FvsConfigLoader keywords for
  ne/acd and that default vs calibrated diverge as expected on MAGPlot.
- Decide the plot unit (this run grouped by magp_site_id; switch to the finer
  site+plot key and per-plot meas_plot_size if per-plot stands are wanted).
- Set per-stand site index and elevation from magp_sites_nb_ns.csv instead of the
  current Acadian-region defaults (lat 46.5, lon -66.5, SI 45, elev ~200 m).
- The FVS-ACD run reported one extra summary cycle vs FVS-NE; confirm the cycle
  alignment when comparing default vs calibrated.

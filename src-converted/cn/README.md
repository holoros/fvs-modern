# CONUS unified variant (cn)

This variant is a coordinated bundle: 224-species coefficient table in
`config/cn.json` plus the Fortran subroutines that consume it. The
component growth equations come from Greg Johnson, David Marshall, and
Aaron Weiskittel's CONUS re-fitting effort (`gregjohnsonbiometrics/fvs_remodeling`),
re-fit on a 5.4M-row CONUS remeasurement panel under biological sign
constraints with ClimateNA EMT and TD covariates.

## Build state

The variant is structured in three layers:

### 1. CN-custom files (the new modeling work)

| File | Status | Source |
| --- | --- | --- |
| `dgf.f90` | **CN-custom** | Port of Greg's `est_dg` integrated annual increment, reading from `COMMON /CN_DG/` |
| `htgf.f90` | **CN-custom** | Port of Greg's `est_hg`, reading from `COMMON /CN_HG/` |
| `dgf_r_wrapper.f90` | **CN-custom** | Argument-passing R wrapper for testing DGF/HTGF outside the FVS runtime |
| `htdbh_cn.f90` | **CN-custom** | Wykoff HT-DBH per species, reads `COMMON /CN_HD/` (224 species) |
| `crown_cn.f90` | **CN-custom** | HTLC_INIT (logistic) + DHCB_STEP (annualized Hann-Hanus, no SI), reads `CN_HL` and `CN_DH` (182 / 163 species) |
| `morts_cn.f90` | **CN-custom** | cloglog mortality with per-year compounding inside the FVS time-step loop, reads `CN_MO` (200 species) |
| `load_cn_json.f90` | **scaffold** | Stub showing the expected JSON loader interface for the four new COMMON blocks. Real JSON parsing delegated to a community Fortran library wired in CMake. |

`config/cn.json` carries 8 categories under `categories.{species_definitions,
johnson_dg, johnson_hg, ht_dbh, htlc, dhcb, mortality, cr_change, ingrowth}`
keyed by a 224-species `JTYPE` index. Ingrowth is a hurdle (occurrence
logistic + zero-truncated NB count, both per-year via `log(periods)` offset).

### 2. NC-scaffold files still in place (will be replaced when build wires in CN-custom)

| File | Status | Notes |
| --- | --- | --- |
| `htdbh.f90` | **CN-custom (live)** | Wykoff form, reads `COMMON /CN_HD/`. NC scaffold archived under `_nc_scaffold/htdbh_nc.f90`. |
| `crown.f90` | **CN-custom (live)** | HTLC_INIT + DHCB_STEP. NC scaffold archived under `_nc_scaffold/crown_nc.f90`. |
| `morts.f90` | **CN-custom (live)** | cloglog with per-year compounding. NC scaffold archived under `_nc_scaffold/morts_nc.f90`. |
| `grinit.f90` | **patched** | Calls `LOAD_CN_JSON` at end so coefficient COMMONs are populated before any growth call. |
| `cratet.f90` | NC scaffold | future CR dynamics fit pending — `cr_parms.RDS` covers 74 species |
| `nwcmrt.f90` | NC scaffold | mortality companion, will be ported next |
| `sitset.f90` | NC scaffold | site index / climate productivity lookup pending — productivity v4 (job 8864212) testing 11 climate metrics |
| `ecocls.f90` | NC scaffold | ecological classification, NC-specific |
| `habtyp.f90` | NC scaffold | habitat type lookup, NC-specific |
| `forkod.f90` | NC scaffold | FOR code lookup, mostly portable |

### 3. CMake source list

`bin/FVScn_sourceList.txt` (821 lines) registers the variant. Cloned from
`FVSnc_sourceList.txt` with `nc/` -> `cn/` substitution and two new files
inserted after `cn/dgf.f90`: `cn/cn_coeffs_data.f90` and
`cn/load_cn_json.f90`. Drop into the upstream `src-converted/bin/`
alongside the other variant source lists; the existing CMakeLists.txt
glob picks it up automatically.

### 4. Coefficient initialization

Per-species coefficients live in `config/cn.json` (224 species union, 8
categories). At build time, `cn_json_to_fortran.py` (in the project root)
converts the JSON into `cn_coeffs_data.f90` — a Fortran BLOCK DATA file
that initializes the CN_HD / CN_HL / CN_DH / CN_MO / CN_CR / CN_ING
COMMON blocks at link time, before main() runs. No runtime JSON parser
is needed.

Workflow when coefficients change (e.g., new RDS files from Cardinal):

```bash
# 1. Refresh cn.json from RDS
Rscript build_cn_json_v2.R

# 2. Regenerate the Fortran BLOCK DATA
python3 cn_json_to_fortran.py

# 3. Rebuild fvs-modern
cd src-converted/bin && cmake . && make
```

`load_cn_json.f90` is now a no-op stub kept for the GRINIT call site, so
a runtime JSON parser can be wired in later without touching the
consumer routines (htdbh.f90, crown.f90, morts.f90).

### 3. Files that should work verbatim across variants

These are utility code that isn't really variant-specific:

* `bfvol.f90`, `bratio.f90`, `cubrds.f90`, `logs.f90` — volume / bark / log conversion
* `ccfcal.f90` — CCF calibration
* `essubh.f90`, `htcalc.f90`, `findag.f90`, `formcl.f90` — generic helpers
* `dgdriv.f90` — DGF driver loop, calls `DGF` so works for any variant once `DGF` is correct
* `htgr5.f90` — height growth driver, calls `HTGF`
* `dunn.f90`, `dubscr.f90`, `pvref5.f90`, `pvref6.f90`, `regent.f90`, `sichg.f90`, `grinit.f90`, `grohed.f90` — supporting code

## What's still needed before the variant compiles + runs end to end

1. **Update `RCON.f90` (in `src-converted/base/`)** to load `cn.json`
   into the `CN_DG` and `CN_HG` COMMON blocks at startup. The simplest
   approach: link a small JSON-parsing routine (e.g., the Fortran-Sax
   community module) and call it once from RCON when `IVAR == 'CN'`.
   Without this step, our `dgf.f90` and `htgf.f90` see uninitialised
   coefficient arrays and produce zero growth.

2. **Update `blkdat.f90`** to declare `MAXSP = 115` for the CN variant,
   or move the species table into `cn.json` and have RCON populate it.
   The current `blkdat.f90` is verbatim NC (12 species) and will not
   work for 115-species inputs.

3. **Replace `htdbh.f90`, `crown.f90`, `morts.f90`** with code that reads
   the per-species coefficients from JSON the same way DGF/HTGF do. The
   coefficient tables come from the SLURM-fit RDS files
   (`ht_dbh_parms.RDS`, `htlc_parms.RDS`, `mort_parms_all.RDS`) which
   we generate alongside the constrained DG/HG fits.

4. **Wire the variant into the CMake build** by adding a `cn` target
   and a test on `IVAR == 'CN'` in the variant dispatch.

5. **Smoke test** on a representative tree list. Suggested: a 50-year
   projection on a Maine red spruce / balsam fir plot, a Pacific
   Northwest Douglas-fir plot, and a Southeast loblolly plantation
   plot. Compare to the legacy NC, WC, and SN variant predictions for
   sanity.

## Build recipe (once items 1-4 are done)

```bash
gfortran -fPIC -shared -O2 -I src-converted/base \
    src-converted/base/*.f90 \
    src-converted/cn/*.f90 \
    -o libfvsmodern_cn.so
```

## References

* Greg Johnson, David Marshall, Aaron Weiskittel. FVS Remodeling Project Repository.
  https://github.com/gregjohnsonbiometrics/fvs_remodeling
* Constrained refit RDS files:
  https://github.com/holoros/fvs_remodeling/tree/constrained-refit-2026-04-25/rds
* Bound diagnostic + validation reports on the FVS Remodel Drive folder.

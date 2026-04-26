# FVS-PN/SN library runtime failure diagnosis

Date: 2026-04-25 (updated 2026-04-26 with deeper findings)

## Update 2026-04-26: progress and remaining work

The originally-reported `morcon_` undefined symbol has been resolved:

1. **Build script include order fixed** in
   `deployment/scripts/build_fvs_libraries.sh`. Variant-specific
   include directories (`-I$var`, `-I$var/common`) are now placed
   BEFORE `-Ibase` so that headers like `PRGPRM.f90` resolve to the
   variant's own copy. The previous order picked up
   `base/PRGPRM.f90` (with `MAXSP=23`) before `pn/common/PRGPRM.f90`
   (with `MAXSP=39`), causing `vwc/morts.f90` to fail compilation
   silently when its 39-element DATA arrays exceeded the array
   declarations sized to MAXSP=23.

2. **`econ/ecvol.f90`** F77→F90 conversion bug repaired. The
   original `C`-prefixed comment block on lines 2-19 had been
   mangled into continuation-of-SUBROUTINE-statement, breaking
   compilation. Restored from upstream USDA/FVS pattern.

3. **`econ/ecinit.f90`** F77→F90 conversion bug repaired with the
   same approach.

After these three fixes, FVSpn.so links cleanly with both `morcon_`
and `ecvol_` defined. However, `ctypes.CDLL` load now reports
`undefined symbol: ecinit_` despite ecinit.f90 compiling
successfully. Investigation pending; likely another econ/ or rd/
file with the same conversion-gap pattern.

## Pattern of the F77→F90 conversion bug

The conversion script was confused by Fortran 77 `C`-prefixed
comments that contained continuation-style content. Specifically:

- F77 source: `C   Initialize control variables`
- F90 expected: `! Initialize control variables`
- F90 actual: trailing ` &` on previous line plus `   nitialize control variables`

The script appears to have stripped the leading `C  ` thinking
it was a column-1-to-5 indent, then treated the resulting line as
a continuation of the previous Fortran statement (line ending with
`&`) by mistake. The `I` of "Initialize" was then read as the
column-6 continuation marker and stripped.

Files where this pattern is present and needs repair (audited 2026-04-26):
- `src-converted/econ/ecvol.f90`     [REPAIRED]
- `src-converted/econ/ecinit.f90`    [REPAIRED]
- `src-converted/econ/echarv.f90`    [identified, not yet repaired]
- `src-converted/rd/RDADD.f90`       [identified, INCLUDE-style file]
- `src-converted/rd/RDARRY.f90`      [identified, INCLUDE-style file]
- `src-converted/rd/RDCOM.f90`       [identified, INCLUDE-style file]
- `src-converted/rd/RDCRY.f90`       [identified, INCLUDE-style file]
- `src-converted/rd/RDPARM.f90`      [identified, INCLUDE-style file]

The `RD*.f90` files appear to be variable-declaration headers meant
to be `INCLUDE`d into another routine, not standalone compilation
units. They begin with stripped lines like `!ODE SEGMENT RDADD`
(originally `C ODE SEGMENT RDADD` perhaps) and have free-floating
declarations at file scope. Treating them as INCLUDE-only would
require updating the source list and adding the includes to the
parent routines that need the declarations. This is a separate
refactoring effort from the comment-conversion bug.

## Original symptom (preserved for reference below)


## Symptom

`ctypes.CDLL("lib/FVSpn.so", RTLD_GLOBAL)` raises:

```
OSError: undefined symbol: morcon_
```

Same for FVS-SN. FVS-NE, FVS-ACD, and FVS-IE load cleanly.

## Diagnosis

`morcon` is a Fortran ENTRY point inside each variant's `morts.f90`
file. NE, ACD, IE, SN, AK and others all carry their own
`morts.f90` in `src-converted/<variant>/`.

PN does not. The full PN source listing is:

```
blkdat.f90    bratio.f90    ccfcal.f90    common/
crown.f90     dgf.f90       ecocls.f90    forkod.f90
formcl.f90    grinit.f90    grohed.f90    habtyp.f90
htcalc.f90    htdbh.f90     pvref6.f90    sichg.f90
sitset.f90
```

Seventeen files, no mortality module. The Fortran 77 to Fortran 90
conversion missed `morts.f` for PN.

SN has `morts.f90` but the library still shows `U morcon_`,
indicating the build script did not include it in the link line for
SN. That is a build script bug separable from the PN source gap.

## Recommended fixes (out of scope for current session)

1. Locate the original FVS-PN `morts.f` in the upstream USDA
   repository (likely `src/pn/morts.f` in USDA-Forest-Service/ForestVegetationSimulator).
   Convert F77 to F90 with the same conversion pipeline used for
   the rest of the codebase.
2. Add the converted file to `src-converted/pn/`.
3. Audit `deployment/scripts/build_fvs_libraries.sh` to confirm SN
   includes `morts.f90` in its link line. If not, add it.
4. Rebuild: `bash deployment/scripts/build_fvs_libraries.sh src-converted ./lib pn sn`
5. Verify: `ctypes.CDLL("./lib/FVSpn.so", RTLD_GLOBAL)` should not
   raise. Run `pnt01.key` smoke test.

## Implication for this session's manuscript work

The PN/SN extensions of the FIA Bakuzis evaluation are blocked at
the library load stage. The Marshall CSV adapter
(`marshall_to_fia_csv.py`) is committed in this repo so that when
the PN/SN library issues are resolved, the FIA stratified sampling
path is ready to run for the western and southern variants without
further code changes.

The manuscript correctly notes this limitation in Section 5.4 by
stating that variant coverage is currently restricted to NE and
ACD because per-state FIA CSVs were not available; the deeper
truth is that the FVS-PN/SN libraries themselves do not load via
fvs2py and would block any extension regardless of FIA data
availability. The next manuscript revision could expand this caveat
or, preferably, the library issues could be fixed before the next
revision so the caveat can be removed.

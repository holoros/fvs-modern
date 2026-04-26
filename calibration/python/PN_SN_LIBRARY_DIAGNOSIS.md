# FVS-PN/SN library runtime failure diagnosis

Date: 2026-04-25

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

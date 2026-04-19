# NVEL Volume Library Provenance

## Current Status

The Fortran 90 free-form source files in this directory were converted from the
USDA Forest Management Service Center (FMSC) National Volume Estimator Library
(NVEL). The canonical upstream repository is:

  https://github.com/FMSC-Measurements/VolumeLibrary

## Upstream Reference

A read-only copy of the upstream VolumeLibrary is tracked as a git submodule at
`upstream/VolumeLibrary/` in the fvs-modern repository root. This submodule is
for reference and auditing only; fvs-modern compiles from the F90 source in this
directory, not from the submodule.

## Version Tracking

| Date       | Upstream Commit | Release Tag         | Notes |
|------------|-----------------|---------------------|-------|
| 2026-04-19 | 4fa39a00        | vollib 20260415     | Initial provenance documentation. Submodule added. All 26 .inc coefficient files audited numerically against upstream master: 23/26 match exactly, regndftdata.inc updated from upstream (species count 146→149). Three files show value-level differences: r9coeff.inc (56 diffs), wdbkwtdata.inc (408 diffs), wdbkwtdata_20190807.inc (411 diffs, archived). See audit script output for details. |

## Conversion Notes

The F90 files in this directory were mechanically converted from fixed-form
Fortran 77 with the following changes:

- Comment syntax: `C` in column 1 replaced with `!`
- Continuation: column-6 `&` replaced with trailing `&` in free-form style
- Line length: no column-72 limit; free-form line length up to 132 characters
- Declarations: some `IMPLICIT` statements retained for compatibility
- Include files (.inc): formatting converted but numeric data preserved exactly

No algorithmic or coefficient changes were made during conversion. The .inc
coefficient tables contain identical numeric values to the upstream source
(verified 2026-04-19) with two exceptions noted above.

## Audit Procedure

To verify coefficient synchronization with upstream:

```bash
# From the fvs-modern root directory
python3 scripts/audit_nvel_upstream.py
```

This script compares all .inc files numerically, ignoring F77/F90 formatting
differences. See `scripts/audit_nvel_upstream.py` for details.

## Files Not in Upstream NVEL

The following files in `src-converted/volume/` are FVS-specific overrides that
do not exist in the upstream VolumeLibrary:

- `fvsbrucedemars.f90` - Bruce/DeMars volume equations
- `fvshannbare.f90` - Hann/Bare volume equations
- `fvsoldfst.f90`, `fvsoldgro.f90`, `fvsoldsec.f90` - Legacy FVS equations
- `fvssierralog.f90` - Sierra log rules
- `r9clark_fvsMod.f90` - FVS-modified Region 9 Clark equations
- `voleqdef.f90` - FVS equation definition wrapper

These are maintained independently of NVEL upstream.

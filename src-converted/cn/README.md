# CONUS unified variant (cn)

This variant carries the Johnson, Marshall, and Weiskittel constrained
diameter and height growth equations re-fit on the full FIA CONUS panel
(8.22 M tree-level remeasurement pairs) with biological sign bounds.

Source coefficients live in `config/cn.json` keyed by FVS species index;
the SPCD-to-index mapping is in `categories.species_definitions.JTYPE`.

## Files

* `dgf_r_wrapper.f90` — thin Fortran wrapper exposing DGF and HTGF
  through an argument-passing R interface. It populates the COMMON
  blocks DGF and HTGF read, calls the legacy subroutines, and extracts
  WK2 / HTG. Build it alongside one of the other variant DGF + HTGF
  implementations (NC, CS, etc.) until the Johnson DGF / HTGF Fortran
  versions land.

## Status

The CN variant is currently a **coefficient-only** drop: the JSON carries
the new constrained parameters but the actual DGF / HTGF Fortran code
that consumes them needs porting from the R `est_dg` / `est_hg` integrated
fits in `gregjohnsonbiometrics/fvs_remodeling/scripts/`. Until the port
lands, the CN variant compiles against any other CONUS variant's `dgf.f90`
and `htgf.f90`; the JSON-driven coefficient tables are wired to be read
by the wrapper.

## References

* Constrained refit RDS files: `gregjohnsonbiometrics/fvs_remodeling@constrained-refit-2026-04-25`
* Bound diagnostic and validation: see drive folder
  `https://drive.google.com/drive/folders/1k2mSX6hty1u-z09tOIDDp8UeanLjjK3W`

! =============================================================================
! src-converted/cn/load_cn_json.f90
!
! Coefficient-table loader for the CN variant. As of 2026-04-26 the loader
! is a no-op: cn.json values are baked at build time into cn_coeffs_data.f90
! (auto-generated BLOCK DATA), which initializes the per-species COMMON
! blocks before main() runs. This wrapper exists so GRINIT.f90 has a stable
! call site if a runtime JSON parser is ever added (e.g., to support hot
! re-loading of cn.json without a recompile).
!
! Companion files:
!   cn_coeffs_data.f90  auto-generated BLOCK DATA from cn.json
!                       (regenerate with cn_json_to_fortran.py after edits)
!   ../config/cn.json   source of truth, hand-edits should round-trip via
!                       the build_cn_json_v2.R generator
!
! COMMON blocks initialized at link time:
!   /CN_DG/  Johnson DG coefficients
!   /CN_HG/  Johnson HG coefficients
!   /CN_HD/  Wykoff HT-DBH coefficients (224 species)
!   /CN_HL/  HTLC logistic coefficients (182 species)
!   /CN_DH/  Hann-Hanus annualized Δ HCB coefficients (163 species)
!   /CN_MO/  cloglog mortality coefficients (200 species)
!   /CN_CR/  CR change linear coefficients (74 species)
!   /CN_ING/ ingrowth hurdle: occurrence + conditional count
!
! Each L*OK flag is .TRUE. iff every coefficient for that species is
! non-null in cn.json; consumer routines fall back to pooled CONUS values
! when the flag is false.
! =============================================================================
SUBROUTINE LOAD_CN_JSON
  IMPLICIT NONE

  ! No-op: BLOCK DATA in cn_coeffs_data.f90 has already initialized
  ! the per-species COMMON blocks. Reserved for future runtime JSON support.

  RETURN
END SUBROUTINE LOAD_CN_JSON

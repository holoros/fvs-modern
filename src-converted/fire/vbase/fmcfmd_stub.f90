! fmcfmd_stub.f90 - No-op stub for FMCFMD subroutine.
! Used by variants (CA, NC, WS) whose fire variant fmcfmd.f90 uses
! the non-standard LOC() intrinsic that GCC gfortran rejects.
! This stub resolves the link-time symbol without affecting runtime
! behavior since FMCFMD is only called in fire simulation paths.

SUBROUTINE FMCFMD(IYR, FMD)
IMPLICIT NONE
INTEGER IYR
REAL FMD
FMD = 0.0
RETURN
END

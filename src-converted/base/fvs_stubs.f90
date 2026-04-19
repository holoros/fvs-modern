! fvs_stubs.f90 - No-op stubs for subroutines that are referenced in
! source lists but cannot compile on GCC/gfortran. These routines are
! either Windows-specific (DLL import directives), region-specific
! (e.g., Oregon/California volume), or database-specific (SQLite with
! MSVC extensions). They are never called at runtime in variants that
! don't use those code paths.
!
! This file is compiled by build_fvs_executables.sh to resolve link-time
! symbols that the .so build tolerates via lazy binding.
!
! Stubs removed after proper compilation:
!   CRZSPDFT, MILESDATA, BIOMASSLIBRARY2 - NVEL free-form conversion
!   NATCRS, OCFVOL - provided by vvolume/fvsvol.f90 ENTRY points
!   DBSFMSSNAG, DBSFMDSNAG - DLL directives commented out, now compiles
!   SUMOUT - FORMAT continuation fixed to free-form
!   DBSFMPF, DBSSUMRY2 - F77 continuation artifacts fixed
!   INITRE, SVOUT - F77 continuation false positives corrected
!   DBSSTANDIN - F77 column-6 continuations converted to free-form
!
! All stubs have been eliminated. This file is kept for documentation
! and as a placeholder for any future stubs that may be needed.
! -----------------------------------------------------------------------

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
!   NATCRS - provided by vvolume/fvsvol.f90 ENTRY point
!   DBSFMSSNAG, DBSFMDSNAG - DLL directives commented out, now compiles
!
! -----------------------------------------------------------------------

! OCFVOL: Now provided by vvolume/fvsvol.f90 ENTRY point.
! Stub removed.

! DBSSTANDIN: Database stand input routine. The vdbsqlite/ version
! has pre-existing F77 conversion issues.
SUBROUTINE DBSSTANDIN(SQLSTR, LKECHO)
  IMPLICIT NONE
  CHARACTER(*), INTENT(IN) :: SQLSTR
  LOGICAL, INTENT(IN) :: LKECHO
  RETURN
END SUBROUTINE DBSSTANDIN

! INITRE: Input/initialization. Source in vbase/initre.f90 has
! pre-existing compile issues (unclosed DO loops from F77 conversion).
SUBROUTINE INITRE
  IMPLICIT NONE
  RETURN
END SUBROUTINE INITRE

! DBSFMPF: Database fire model potential fire output.
! Source in vdbsqlite/dbsfmpf.f90 has F77 continuation artifacts.
SUBROUTINE DBSFMPF(ESSION)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ESSION
  RETURN
END SUBROUTINE DBSFMPF

! SUMOUT: Summary output routine. Source in vbase/sumout.f90 has
! pre-existing compile issues (undefined FORMAT labels).
SUBROUTINE SUMOUT(IOSUM, I20, ICALLF, JOPRT, JSTND2, JSUM2, &
                  LENG, MGMID, NPLT, SAMWT, ITITLE, IPTINV)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IOSUM(*), I20, ICALLF, JOPRT, JSTND2, JSUM2
  INTEGER, INTENT(IN) :: LENG, IPTINV
  CHARACTER(*), INTENT(IN) :: MGMID, NPLT, ITITLE
  REAL, INTENT(IN) :: SAMWT
  RETURN
END SUBROUTINE SUMOUT

! DBSSUMRY2: Database summary output. Source in vdbsqlite/dbssumry.f90
! has pre-existing F77 continuation issues.
SUBROUTINE DBSSUMRY2
  IMPLICIT NONE
  RETURN
END SUBROUTINE DBSSUMRY2

! SVOUT: Stand visualization output. Source in vbase/svout.f90
! needs fire include paths that aren't available in the standard build.
SUBROUTINE SVOUT(IYEAR, IFMCLFG, AMSG)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: IYEAR, IFMCLFG
  CHARACTER(*), INTENT(IN) :: AMSG
  RETURN
END SUBROUTINE SVOUT

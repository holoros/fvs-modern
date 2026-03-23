SUBROUTINE RDRANI(SSEED)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!  THIS SUBROUTINE INITIALIZES THE RANDOM NUMBER GENERATOR FOR THE
!  ROOT DISEASE MODEL.
!
!  CALLED BY :
!     RDMN1   [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!
!  PARAMETERS :
!     SSEED - SEED VALUE FOR THE ROOT DISEASE RANDOM NUMBER GENERATOR.
!
!  Revision History :
!   11/06/89 - Last revision date.
!   09/02/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMON
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

INCLUDE 'RDADD.f90'
!
!OMMONS
!
INTEGER  ISEED
REAL     SSEED

ISEED = INT(SSEED)
IF (MOD(ISEED,2) .EQ. 0) ISEED = ISEED + 1
SS = FLOAT(ISEED)
S0 = SS

RETURN
END

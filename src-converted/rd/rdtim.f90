SUBROUTINE RDTIM
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR SUMMING UP OPROB FOR TIM
!
!  CALLED BY :
!     RDTREG  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  Revision History:
!   03/01/95 - Last revision date.
!   09/04/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'RDADD.f90'
!
!OMMONS
!
INTEGER I, IDI

RRGEN(1,1) = 0.0
RRGEN(2,1) = 0.0
IF (ITRN .LT. 1) RETURN

IDI = MAXRR
DO 1000 I = 1,ITRN
   IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
!
!     If non-host species skip operation (RNH May98)
!
IF (IDI .LE. 0) GO TO 1000
!
RRGEN(IDI,1) = RRGEN(IDI,1) + PROB(I)
1000 CONTINUE

RETURN
END

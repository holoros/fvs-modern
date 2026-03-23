SUBROUTINE RDSSIZ(ISP,A,STCUT,ISL,ISPS,IRTSPC)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR ASSIGNING SIZE CLASS TO STUMPS.
!
!  CALLED BY :
!     RDDAM   [ROOT DISEASE]
!     RDEND   [ROOT DISEASE]
!     RDIN    [ROOT DISEASE]
!     RDSADD  [ROOT DISEASE]
!     RDSTR   [ROOT DISEASE]
!
!  CALLS :
!     NONE
!
!  PARAMETERS :
!     ISP    -
!     A      -
!     STCUT  -
!     ISL    -
!     ISPS   -
!     IRTSPC -
!
!  Revision History :
!   03/05/95 - Last revision date.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!
!.... PARAMETER INCLUDE FILES
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
!
INTEGER ISL, ISP, ISPS(ITOTSP), IRTSPC(MAXSP), J
REAL    A, STCUT(5)
INTEGER IDANUW
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = IRTSPC(1)
IDANUW = ISP
IDANUW = ISPS(1)
!
DO 100 J = 2,5
   IF (A .LT. STCUT(J-1) .OR. A .GT. STCUT(J)) GOTO 100
   ISL = J - 1
   RETURN
100 CONTINUE

ISL = 5

RETURN
END


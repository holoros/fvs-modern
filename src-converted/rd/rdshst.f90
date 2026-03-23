SUBROUTINE RDSHST
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR UPDATING THE STUMP HISTORY ARRAYS
!
!  CALLED BY :
!     RDMN2   [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  Revision History :
!   08/14/12 - Last revision date.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'RDPARM.f90'

INCLUDE 'RDCOM.f90'

INCLUDE 'ARRAYS.f90'

INCLUDE 'CONTRL.f90'
!
!OMMONS
!
INTEGER I, J, K, KK

DO 501 I = 1,IRRTRE
   DO 502 J = 1,4
      K = 4 - J + 1
      KK = K - 1
      IF (J .EQ. 4) GOTO 503
      ROOTH(K,I) = ROOTH(KK,I)
      XMTH(K,I) = XMTH(KK,I)
      GOTO 502

503       CONTINUE
      IF (I .GT. ITRN) GOTO 504
      ROOTH(1,I) = RROOTT(I)
      XMTH(1,I) = WK22(I)
      GOTO 505

504       CONTINUE
      ROOTH(1,I) = -1.0
      XMTH(1,I) = -1.0

505       CONTINUE
      RROOTT(I) = 0.0
      WK22(I) = 0.0
502    CONTINUE
501 CONTINUE

RETURN
END

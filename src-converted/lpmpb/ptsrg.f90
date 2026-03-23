FUNCTION PTSRG(X,BASNO)
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!
! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
INTEGER J
REAL BASNO, C, PTSRG, X

DO 1 J=4,12,4
C=J
PTSRG=C*BASNO
IF(PTSRG .GE. X) RETURN
1 CONTINUE
RETURN
END

FUNCTION EMERG(BY,T,INCRS)
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
!
!
! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!   08/22/14 Lance R. David (FMSC)
!     Function name was used as variable name.
!     changed variable INT to INCRS
!----------
DOUBLE PRECISION C
INTEGER T, INCRS
REAL EMERG,BY

IF(T .NE. 0) GO TO 1
C = 1.0D00/2**INCRS
GO TO 2
1 C = C*(INCRS-T+1)/T
2 EMERG = BY*C
RETURN
END

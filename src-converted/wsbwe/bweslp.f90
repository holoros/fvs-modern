FUNCTION BWESLP(XX,X,Y,N)
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!
!     A LINEAR-INTERPOLATION FUNCTION
!
!     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
!     INT-MOSCOW FORESTRY SC. LAB. AUG 1980
!
!     DESCRIPTION :
!
!     SUPPLIED WITH THE BWMOD AS WRITTEN AT THE MODELING
!     WORKSHOP.  THE NAME IS CHANGED FROM 'SLP' TO 'BWESLP'.
!     THE FUNCTION RETURNS THE VALUE OF A LINEAR SEGMENTED
!     FUNCTION DEFINED BY THE PAIRS X(I), Y(I) EVALUATED AT
!     X=XX FOR X INCLUDED IN THE INTERVAL ?X(1),X(N)!
!     IF XX < X(1), F(XX)=X(1).  IF XX > X(N), XX=X(N).
!
!     PARAMETERS :
!
!     XX - POINT AT WHICH FUNCTION IS TO BE EVALUATED.
!     X  - ARRAY OF X(I), SEGMENT ENDPOINTS FOR X VARIABLE.
!     Y  - ARRAY OF Y(I), SEGMENT ENDPOINTS FOR Y VARIABLE.
!     N  - SIZE OF ARRAYS X AND Y
!
! Revision History:
!   05-MAY-00 Lance David (FHTET)
!      .The last occurence of SLP had been translated to BWSLP instead
!       of BWESLP. Fixed it.
!      .Added weather and outbreak random number seeds WSEEDR and OBSEER.
!    14-JUL-2010 Lance R. David (FMSC)
!       Added IMPLICIT NONE and declared variables as needed.
!----------
INTEGER I, N, NN
REAL    XX, X(N), Y(N), BWESLP

!     WRITE (16,*) 'FUNCTION BWESLP: XX=',XX,' N=',N               ! TEMP DEBUG
!     WRITE (16,*) '                  X=',X                        ! TEMP DEBUG
!     WRITE (16,*) '                  Y=',Y                        ! TEMP DEBUG

NN=N-1
DO 30 I=1,NN
IF(XX.LT.X(I).OR.XX.GT.X(I+1)) GO TO 30
BWESLP=Y(I)+((Y(I+1)-Y(I))/(X(I+1)-X(I)))*(XX-X(I))
GOTO 9000
30 CONTINUE
BWESLP=Y(N)
IF (XX.LT.X(1)) BWESLP=Y(1)

9000 CONTINUE

!     WRITE (16,*) 'FUNCTION BWESLP=',BWESLP                       ! TEMP DEBUG

RETURN
END

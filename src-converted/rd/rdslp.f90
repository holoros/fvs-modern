REAL FUNCTION RDSLP(X,XX,YY,N)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Given two arrays XX and YY, each of dimension N, representing
!  a series of line segments (XX strictly increasing), and an X-
!  coordinate, return its Y-coordinate on the line.
!
!  CALLED BY :
!     RDGROW  [ROOT DISEASE]
!     RDINOC  [ROOT DISEASE]
!     RDSPRD  [ROOT DISEASE]
!     ANCARY  [ROOT DISEASE]
!     RDMORT  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  PARAMETERS :
!     X      - (I ) X-coordinate of desired point.
!     XX     - (I ) X points on series of line segments.
!     YY     - (I ) Y points on series of line segments.
!     N      - (I ) number of points in XX and YY arrays.
!
!  Revision History:
!    21-MAR-00 Lance David (FHTET)
!       Reduced RETURN statements to 1 at the end of routine.
!       Modified Debug code.
!    06-AUG-01 Lance R. David (FHTET)
!       Changed dimensions on XX and YY arrays from 5 to *.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... PARAMETER INCLUDE FILES
!
INCLUDE 'PRGPRM.f90'
!
!.... COMMON INCLUDE FILES
!
INCLUDE 'CONTRL.f90'
!
!.... Local variables
!
LOGICAL DEBUG
INTEGER I, N
REAL    X, XX(*), YY(*)
!
!.... Check for DEBUG
!
CALL DBCHK(DEBUG,'RDSLP',5,ICYC)

IF      (X .LT. XX(1)) THEN
   RDSLP = YY(1)
ELSE IF (X .GT. XX(N)) THEN
   RDSLP = YY(N)
ELSE
   I = 1
10    IF (X .GT. XX(I+1)) THEN
      I = I + 1
      GOTO 10
   ENDIF
!
!....    Now X in the range XX(I) to XX(I+1); find Y.
!
   RDSLP = YY(I) + (YY(I+1) - YY(I)) / &
               (XX(I+1) - XX(I)) * (X-XX(I))
ENDIF

IF (DEBUG) THEN
   WRITE (JOSTND,910) ' IN RDSLP:  N= ', N
   WRITE (JOSTND,900) ' IN RDSLP: XX= ', (XX(I),I=1,5), X
   WRITE (JOSTND,900) ' IN RDSLP: YY= ', (YY(I),I=1,5), RDSLP
900 FORMAT (A15, 5F10.3, ';  -> ', F10.3)
910 FORMAT (A15, I5)
ENDIF
RETURN
END

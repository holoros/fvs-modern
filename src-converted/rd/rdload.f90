SUBROUTINE RDLOAD(A,B,N)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR COPYING ONE ARRAY INTO ANOTHER.
!
!  CALLED BY :
!     RDTREG  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  PARAMETERS :
!     A      - DESTINATION ARRAY OF COPY.
!     B      - SOURCE ARRAY OF COPY.
!     N      - NUMBER OF ELEMENTS IN THE 2 ARRAYS.
!
!  Revision History :
!   12/15/87 - Last revision date.
!   08/29/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------

INTEGER  I, N
REAL     A(N),B(N)

IF (N .GT. 0) THEN
   DO 1000 I=1,N
      A(I) = B(I)
1000    CONTINUE
ENDIF

RETURN
END

SUBROUTINE RDSUM(N,A1,A2,IYEND)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE TO SUM UP THE ARRAY PROBI(1350,41)
!
!  CALLED BY :
!     RDBBDO  [ROOT DISEASE]
!     RDEND   [ROOT DISEASE]
!     RDESTB  [ROOT DISEASE]
!     RDGROW  [ROOT DISEASE]
!     RDINF   [ROOT DISEASE]
!     RDINSD  [ROOT DISEASE]
!     RDMN2   [ROOT DISEASE]
!     RDMORT  [ROOT DISEASE]
!     RDSETP  [ROOT DISEASE]
!     RDSTR   [ROOT DISEASE]
!     RDTREG  [ROOT DISEASE]
!
!
!  CALLS     :
!     NONE
!
!  PARAMETERS :
!     N      - NUMBER OF TREE RECORDS TO SUM UP OVER
!     A1     - ARRAY TO SUM UP INTO
!     A2     - ARRAY TO BE SUMMED UP
!     IYEND  - NUMBER OF YEARS TO SUM UP OVER
!
!  Revision History :
!   11/23/93 - Last revision date.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'RDPARM.f90'
!
!OMMONS
!

INTEGER I, IYEND, J, N
REAL    A1(IRRTRE), A2(IRRTRE,41,2)

IF (N .LE. 0) RETURN

DO 1000 I = 1,N
   A1(I) = 0.0

   DO 900 J = 1,IYEND
      A1(I) = A1(I) + A2(I,J,1) + A2(I,J,2)
900    CONTINUE
1000 CONTINUE

RETURN
END

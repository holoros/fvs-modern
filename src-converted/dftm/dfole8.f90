SUBROUTINE DFOLE8
IMPLICIT NONE
!----------
! DFTM $Id$
!----------
!
!  DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.
!
!******************************************************
!
!  *** S(0) MODULE V3.1 DFTM MODEL ***
!
!******************************************************
!  REVISION HISTORY:
!    01-APR-2013 Lance R. David (FMSC)
!      A few variables defined locally were already defined
!      in a common block. Local declaration removed.
!----------
!
!OMMONS
!
INCLUDE 'LIMITS.f90'
!
INCLUDE 'DFOL.f90'
!
INCLUDE 'ICOND.f90'
!
!OMMONS
!
INTEGER II,I

II = K(3) - 1

DO 10 I=1,ICOUNT
  G19OUT(I,II) = G19(I)
  DPCENT(I,II) = 100.0 * (1.0 - X6(I) / Z3(I))
10 CONTINUE

RETURN
END

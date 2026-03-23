SUBROUTINE G0COMP
IMPLICIT NONE
!----------
! DFTM $Id$
!----------
!
!     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.
!
!OMMONS
!
INCLUDE 'ICOND.f90'
!
INCLUDE 'GPASS.f90'
!
INCLUDE 'UPPER.f90'
!
INCLUDE 'LIMITS.f90'
!
!  REVISION HISTORY:
!    01-APR-2013 Lance R. David (FMSC)
!      A few variables defined locally were already defined
!      in a common block. Local declaration removed.
!----------
!
!OMMONS
!
INTEGER INUM,IP,J,I,KPP

IF (K(3) .EQ. K(1)) RETURN
INUM = IC(2) - IC(1) + 1
KP = K(3) - 1
IP = KP + 1

CALL REDIST(KP, INUM)

DO 10 J=1,INUM
  G3(J) = AMIN1((1.0 - Z2(J) / 100.0) * Z3(J), X6(J))
10 CONTINUE

DO 120 I=1,INUM
  X6(I) = G3(I)
  KPP = K(3) + 56
  X7(I) = (1.0 - B0(KPP)) * G2(I)
120 CONTINUE

RETURN
END

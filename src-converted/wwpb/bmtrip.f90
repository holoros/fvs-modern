SUBROUTINE BMTRIP(ITFN,I,WEIGHT)
!----------
! WWPB $Id$
!----------
INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'
INCLUDE 'BMCOM.f90'
!..the following line commented out 7/1/99 (AJM)
!      GRFDEN(ITFN)=  GRFDEN(I)
LBMDAM(ITFN) = LBMDAM(I)

!... samples from triple.
!
!      PROB(ITFN)=PROB(I)*WEIGHT
!      WK1(ITFN)=WK1(I)

RETURN
END

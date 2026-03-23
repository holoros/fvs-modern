FUNCTION FMBRKT(DBH,ISP)
IMPLICIT NONE
!----------
! FIRE-NC $Id$
!----------
!
!     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
!     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
!
!OMMONS
!
INTEGER ISP
REAL    DBH,FMBRKT
REAL    B1(MAXSP)
!
DATA B1/ &
        0.063, &  !1  other conifers - use Douglas-fir
        0.072, &  !2  sugar pine
        0.063, &  !3  Douglas-fir
        0.048, &  !4  white fir
        0.060, &  !5  madrone
        0.060, &  !6  incense cedar
        0.030, &  !7  California black oak
        0.052, &  !8  tanoak
        0.039, &  !9  red fir
        0.063, &  !10 ponderosa pine
        0.052, &  !11 other hardwood - use tanoak
        0.081/    !12 coast redwood - use giant sequoia/redwood from WS
!
FMBRKT = DBH*B1(ISP)
!
RETURN
END

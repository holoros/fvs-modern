FUNCTION FMBRKT(DBH,ISP)
IMPLICIT NONE
!----------
!  **FMBRKT  FIRE-NI-DATE OF LAST REVISION:  02/03/03
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
        0.035, &  !1  white pine
        0.063, &  !2  western larch
        0.063, &  !3  Douglas-fir
        0.046, &  !4  grand fir
        0.040, &  !5  western hemlock
        0.035, &  !6  western redcedar
        0.028, &  !7  lodgepole pine
        0.036, &  !8  Engelmann spruce
        0.041, &  !9  subalpine fir
        0.063, &  !10 ponderosa pine
        0.040/    !11 other (mountain hemlock)
!
FMBRKT = DBH*B1(ISP)
!
RETURN
END

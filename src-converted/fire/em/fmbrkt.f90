FUNCTION FMBRKT(DBH,ISP)
IMPLICIT NONE
!----------
! FIRE-EM $Id$
!----------
!
!     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
!     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)
!----------
!OMMONS
!
INCLUDE 'PRGPRM.f90'
!
!OMMONS
!----------
INTEGER ISP
REAL    FMBRKT,DBH
REAL    B1(MAXSP)
!----------
!  SPECIES ORDER:
!   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
!  17=PB, 18=OS, 19=OH
!----------
!
DATA B1/ &
        0.030, &  !1  whitebark pine
        0.063, &  !2  larch
        0.063, &  !3  Douglas-fir
        0.030, &  !4  limber pine, use IE limber pine
        0.050, &  !5  subalpine larch, use IE subalpine larch
        0.025, &  !6  Rocky Mountain juniper, use ie RM juniper
        0.028, &  !7  lodgepole pine
        0.036, &  !8  Englemann spruce
        0.041, &  !9  subalpine fir
        0.063, &  !10 ponderosa pine
        0.038, &  !11 green ash, use IE cottonwood
        0.044, &  !12 quaking aspen, use IE quaking aspen
        0.038, &  !13 black cottonwood, use IE cottonwood
        0.038, &  !14 balsam poplar, use IE cottonwood
        0.038, &  !15 plains cottonwood, use IE cottonwood
        0.038, &  !16 narrowleaf cottonwood, use IE cottonwood
        0.027, &  !17 paper birch, use IE paper birch
        0.025, &  !18 other softwoods - use western juniper
        0.038/    !19 other hardwoods, use IE cottonwood
!
FMBRKT = DBH*B1(ISP)
!
RETURN
END

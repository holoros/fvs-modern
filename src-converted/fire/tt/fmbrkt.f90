FUNCTION FMBRKT(DBH,ISP)
IMPLICIT NONE
!----------
! FIRE-TT $Id$
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
!----------
INTEGER ISP
REAL    DBH,FMBRKT
REAL    B1(MAXSP)
!
!----------
! SPECIES ORDER FOR TETONS VARIANT:
!
!  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
!
! VARIANT EXPANSION:
! BS USES ES EQUATIONS FROM TT
! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
! PP USES PP EQUATIONS FROM CI
! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
! MM USES MM EQUATIONS FROM IE
! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
! OS USES OT (OTHER SP.) EQUATIONS FROM TT
!----------
DATA B1/ &
        0.030, &  !1  whitebark pine
        0.030, &  !2  limber pine
        0.063, &  !3  Douglas-fir
        0.030, &  !4  singleleaf pinyon - use Pinus sp
        0.036, &  !5  blue spruce
        0.044, &  !6  quaking aspen
        0.028, &  !7  lodgepole pine
        0.036, &  !8  Engelmann spruce
        0.041, &  !9  subalpine fir
        0.063, &  !10 ponderosa pine - use CI (NI) PP
        0.025, &  !11 Utah juniper - use UT western juniper
        0.025, &  !12 Rocky Mountain juniper - use UT western juniper
        0.024, &  !13 bigtooth maple - use SO big leaf maple
        0.040, &  !14 Rocky Mountain maple - use IE MM (NI-mtn. hemlock)
        0.038, &  !15 narrowleaf cottonwood - use CR Populus sp
        0.044, &  !16 curlleaf mountain-mahogany - use SO (WC-other(AS))
        0.030, &  !17 other softwoods - use whitebark pine
        0.038/    !18 other hardwoods - use CR Populus sp
!
FMBRKT = DBH*B1(ISP)
!
RETURN
END

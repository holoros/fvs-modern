FUNCTION FMBRKT(DBH,ISP)
IMPLICIT NONE
!----------
! FIRE-CI $Id$
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
REAL    DBH,FMBRKT
REAL    B1(MAXSP)
!----------
!     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
!     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
!     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
!     4 = GRAND FIR (GF)                   ABIES GRANDIS
!     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
!     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
!     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
!     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
!     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
!    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
!    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
!    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
!    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
!    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
!    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
!    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
!    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
!    18 = OTHER SOFTWOODS (OS)
!    19 = OTHER HARDWOODS (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE IE VARIANT:
!      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
!      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
!      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
!      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
!
!  FROM THE UT VARIANT:
!      USE 12(WJ) FOR 14(WJ)
!      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
!                                                  REALLY WC39=OT)
!----------
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
        0.030, &  !11 whitebark pine uses UT WB/LM = pinus spp.
        0.025, &  !12 pacific yew uses IE PY
        0.044, &  !13 quaking aspen uses UT AS
        0.025, &  !14 western juniper uses UT WJ
        0.044, &  !15 curl-leaf mt. mahog.- uses WC-other (AS)
        0.030, &  !16 limber pine uses UT WB/LM = pinus spp.
        0.038, &  !17 cottonwood, uses populus spp.
        0.040, &  !18 other softwoods (mountain hemlock)
        0.038/    !19 other hardwoods, uses populus spp.
!
FMBRKT = DBH*B1(ISP)
!
RETURN
END

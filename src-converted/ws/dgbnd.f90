SUBROUTINE DGBND (ISPC,DBH,DDG)
IMPLICIT NONE
!----------
! WS $Id$
!----------
!  THIS SUBROUTINE IS USED TO INSURE THAT A MAXIMUM VALUE FOR DG
!  IS NOT EXCEEDED.  EQUATION AND COEFFICIENTS ARE FROM DOLPH AND
!  DIXON (1993), WEST.J.APPL.FOR 8(1):24-27.
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
!OMMONS
!----------
EXTERNAL RANN
INTEGER ISPC
REAL DGAMAX(MAXSP),DGBMAX(MAXSP),DGCMAX(MAXSP),DDG,DBH,DGMAX
!----------
!     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
!
!     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
!     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
!     3 = WHITE FIR (WF)                    ABIES CONCOLOR
!     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
!     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
!     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
!     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
!     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
!     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
!    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
!    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
!    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
!    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
!    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
!    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
!    16 = COULTER PINE (CP)                 PINUS COULTERI
!    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
!    18 = MONTEREY PINE (MP)                PINUS RADIATA
!    19 = GRAY PINE (GP)                    PINUS SABINIANA
!         (OR CALIFORNIA FOOTHILL PINE)
!    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
!    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
!    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
!    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
!    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
!    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
!    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
!    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
!    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
!    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
!    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
!    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
!    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
!         (OR CALIFORNIA WHITE OAK)
!    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
!    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
!    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
!    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
!    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
!    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
!    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
!    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
!    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
!    42 = OTHER SOFTWOODS (OS)
!    43 = OTHER HARDWOODS (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!    FROM EXISTING WS EQUATIONS --
!      USE 1(SP) FOR 11(WP) AND 24(MH)
!      USE 2(DF) FOR 22(BD)
!      USE 3(WF) FOR 13(SF)
!      USE 4(GS) FOR 23(RW)
!      USE 8(PP) FOR 18(MP)
!      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
!      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
!                     43(OH)
!
!    FROM CA VARIANT --
!      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
!                       25(WJ), 26(WJ), AND 27(CJ)
!      USE CA12(LP) FOR 9(LP) AND 10(WB)
!
!    FROM SO VARIANT --
!      USE SO30(MC) FOR 41(MC)
!
!    FROM UT VARIANT --
!      USE UT17(GB) FOR 21(GB)
!----------
DATA DGAMAX/ &
      4.9094,   1.8388,   3.8681,   4.9094,   1.9557, &
      3.7202,   3.8681,   3.7202,       0.,       0., &
      4.9094,       0.,   3.8681,       0.,       0., &
          0.,       0.,   3.7202,       0.,       0., &
          0.,   1.8388,   4.9094,   4.9094,       0., &
          0.,       0.,   1.9557,   1.9557,   1.9557, &
      1.9557,   1.9557,   1.9557,   1.9557,   1.9557, &
      1.9557,   1.9557,   1.9557,   1.9557,   1.9557, &
          0.,   3.7202,   1.9557/
!
DATA DGBMAX/ &
      0.4678,   0.7319,   0.5043,   0.4678,   0.4655, &
      0.3860,   0.5043,   0.3860,       0.,       0., &
      0.4678,       0.,   0.5043,       0.,       0., &
          0.,       0.,   0.3860,       0.,       0., &
          0.,   0.7319,   0.4678,   0.4678,       0., &
          0.,       0.,   0.4655,   0.4655,   0.4655, &
      0.4655,   0.4655,   0.4655,   0.4655,   0.4655, &
      0.4655,   0.4655,   0.4655,   0.4655,   0.4655, &
          0.,   0.3860,   0.4655/
!
DATA DGCMAX/ &
    -0.04365, -0.05194, -0.04492, -0.04365, -0.05613, &
    -0.03852, -0.04492, -0.03852,       0.,       0., &
    -0.04365,       0., -0.04492,       0.,       0., &
          0.,       0., -0.03852,       0.,       0., &
          0., -0.05194, -0.04365, -0.04365,       0., &
          0.,       0., -0.05613, -0.05613, -0.05613, &
    -0.05613, -0.05613, -0.05613, -0.05613, -0.05613, &
    -0.05613, -0.05613, -0.05613, -0.05613, -0.05613, &
          0., -0.03852, -0.05613/
!----------
! MAX DG CHECK (ONLY APPLIES TO SPECIES USING WS VARIANT EQUATIONS
!----------
SELECT CASE (ISPC)
CASE(1:3,5:8,11,13,18,22,24,28:40,42:43)
  IF(DBH.LT.3.)GO TO 900
  DGMAX = DGAMAX(ISPC) * (DBH**DGBMAX(ISPC)) * &
             EXP(DGCMAX(ISPC)*DBH)
  IF(DDG .GT. DGMAX) DDG = DGMAX
  IF(DDG .LT. 0.0) DDG = 0.0
END SELECT
!----------
! CHECK FOR SIZE CAP COMPLIANCE.
!----------
900 CONTINUE
IF((DBH+DDG).GT.SIZCAP(ISPC,1) .AND. SIZCAP(ISPC,3).LT.1.5)THEN
  DDG=SIZCAP(ISPC,1)-DBH
  IF(DDG .LT. 0.01) DDG=0.01
ENDIF
!
RETURN
END

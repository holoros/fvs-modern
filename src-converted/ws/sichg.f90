SUBROUTINE SICHG(ISISP,SSITE,SIAGE)
IMPLICIT NONE
!----------
! WS $Id$
!----------
! THIS ROUTINE TAKES THE SITE INFORMATION AND CALCULATES AN AGE FOR
! THAT CURVE, GIVEN THE SITE, WHERE A NEW HT (OR PROXY AGE) MAY ME
! LOOKED UP.
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
!OMMONS
!----------
CHARACTER*1 ISILOC,REFLOC(MAXSP)
INTEGER DIFF,REFAGE(MAXSP)
INTEGER I,ISISP
REAL A(MAXSP),B(MAXSP),SIAGE(MAXSP)
REAL SSITE,AGE2BH
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
!      ALEXANDER 1967 RM32 USED AS A SITE REFERENCE; COEFFICIENTS WERE
!      COPIED FROM THE SO VARIANT SPECIES 8=ES FOR THIS ROUTINE AND
!      **HTCALC**.
!----------
!  DATA STATEMENTS
!----------
DATA A/ &
       10.83,    13.27,    22.67,   29.286,    22.67, &
      18.667,     25.0,   13.789,     10.0,     10.0, &
       10.83,     10.0,    22.67,     10.0,     10.0, &
        10.0,     10.0,   13.789,     10.0,     10.0, &
    33.72545,    13.27,   29.286,    10.83,     10.0, &
        10.0,     10.0,   10.976,   10.976,   10.976, &
      10.976,   10.976,   10.976,   19.763,   19.763, &
      19.763,   19.763,   19.763,   19.763,   10.976, &
    11.56252,    13.92,   10.976/
!
DATA B/ &
       -0.10,    -0.04,    -0.17,   -0.071,    -0.17, &
      -0.100,   -0.125,   -0.050,      0.0,      0.0, &
       -0.10,      0.0,    -0.17,      0.0,      0.0, &
         0.0,      0.0,   -0.050,      0.0,      0.0, &
   -0.274509,    -0.04,   -0.071,    -0.10,      0.0, &
         0.0,      0.0,   -0.071,   -0.071,   -0.071, &
      -0.071,   -0.071,   -0.071,   -0.080,   -0.080, &
      -0.080,   -0.080,   -0.080,   -0.080,   -0.071, &
    -0.05586,    -0.13,   -0.071/
!
DATA REFLOC/ &
         'T',      'T',      'T',      'T',      'T', &
         'T',      'B',      'T',      'T',      'T', &
         'T',      'T',      'T',      'T',      'T', &
         'T',      'T',      'T',      'T',      'T', &
         'B',      'T',      'T',      'T',      'T', &
         'T',      'T',      'B',      'B',      'B', &
         'B',      'B',      'B',      'B',      'B', &
         'B',      'B',      'B',      'B',      'B', &
         'B',      'T',      'B'/
!
DATA REFAGE/ &
          50,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
         100,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
          50,       50,       50,       50,       50, &
         100,       50,       50/
!----------
! ISILOC IS THE PLACE WHERE THE AGE FOR THE SITE IS TAKEN
!----------
ISILOC = REFLOC(ISISP)
DO 100 I = 1,MAXSP
!----------
!  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
! LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE
!----------
IF(ISILOC .EQ. 'T' .AND. REFLOC(I) .EQ. 'B')DIFF=-1
IF(ISILOC .EQ. REFLOC(I))DIFF=0
IF(ISILOC .EQ. 'B' .AND. REFLOC(I) .EQ. 'T')DIFF=1
AGE2BH=0.0
IF(DIFF .LT. 0 .OR. DIFF .GT. 0) AGE2BH= A(I) + B(I)*SSITE
SIAGE(I) = REFAGE(I) + AGE2BH*DIFF
100 CONTINUE
!
RETURN
END

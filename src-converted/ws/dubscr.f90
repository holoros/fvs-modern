SUBROUTINE DUBSCR(ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
IMPLICIT NONE
!----------
! WS $Id$
!----------
!  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
!  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
!  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
!  MEASUREMENTS AND ARE LESS THAN 5.0 INCHES DBH.  FINALLY, IT IS
!  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
!  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
!  THIS ROUTINE WAS MOVED FROM THE SO11 SOURCE DIRECTORY TO EC
!  WHEN THE SO11 MODEL WAS RETIRED IN JUNE 2010. IT IS USED BY
!  THE EC, NC, AND WS VARIANTS
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PDEN.f90'
!
!
!OMMONS
!----------
EXTERNAL RANN
LOGICAL DEBUG
INTEGER ISPC
REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),BCR5(MAXSP), &
    BCR6(MAXSP),BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP),CRSD(MAXSP)
REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO,PRD,QMDPLT,HDR
REAL RDANUW
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
!  DATA STATEMENTS
!----------
DATA BCR0/ &
    -1.669490, -0.426688, -0.426688, -0.426688, -0.426688, &
    -0.426688, -0.426688, -1.669490,  6.489813,  6.489813, &
    -1.669490,  6.489813, -0.426688,  6.489813,  6.489813, &
     6.489813,  6.489813, -1.669490,  6.489813,  6.489813, &
     6.489813, -0.426688, -0.426688, -1.669490,  6.489813, &
     6.489813,  6.489813, -1.669490, -1.669490, -1.669490, &
    -1.669490, -1.669490, -1.669490,  -2.19723,  -2.19723, &
     -2.19723,  -2.19723,  -2.19723,  -2.19723, -1.669490, &
          5.0, -1.669490, -1.669490/
!
DATA BCR1/ &
    -0.209765, -0.093105, -0.093105, -0.093105, -0.093105, &
    -0.093105, -0.093105, -0.209765,       0.0,       0.0, &
    -0.209765,       0.0, -0.093105,       0.0,       0.0, &
          0.0,       0.0, -0.209765,       0.0,       0.0, &
          0.0, -0.093105, -0.093105, -0.209765,       0.0, &
          0.0,       0.0, -0.209765, -0.209765, -0.209765, &
    -0.209765, -0.209765, -0.209765,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, -0.209765, &
          0.0, -0.209765, -0.209765/
!
DATA BCR2/ &
          0.0,  0.022409,  0.022409,  0.022409,  0.022409, &
     0.022409,  0.022409,       0.0, -0.029815, -0.029815, &
          0.0, -0.029815,  0.022409, -0.029815, -0.029815, &
    -0.029815, -0.029815,       0.0, -0.029815, -0.029815, &
    -0.029815,  0.022409,  0.022409,       0.0, -0.029815, &
    -0.029815, -0.029815,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0/
!
DATA BCR3/ &
     0.003359,  0.002633,  0.002633,  0.002633,  0.002633, &
     0.002633,  0.002633,  0.003359, -0.009276, -0.009276, &
     0.003359, -0.009276,  0.002633, -0.009276, -0.009276, &
    -0.009276, -0.009276,  0.003359, -0.009276, -0.009276, &
    -0.009276,  0.002633,  0.002633,  0.003359, -0.009276, &
    -0.009276, -0.009276,  0.003359,  0.003359,  0.003359, &
     0.003359,  0.003359,  0.003359,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,  0.003359, &
          0.0,  0.003359,  0.003359/
!
DATA BCR5/ &
     0.011032,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,  0.011032,       0.0,       0.0, &
     0.011032,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,  0.011032,       0.0,       0.0, &
          0.0,       0.0,       0.0,  0.011032,       0.0, &
          0.0,       0.0,  0.011032,  0.011032,  0.011032, &
     0.011032,  0.011032,  0.011032,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,  0.011032, &
          0.0,  0.011032,  0.011032/
!
DATA BCR6/ &
          0.0, -0.045532, -0.045532, -0.045532, -0.045532, &
    -0.045532, -0.045532,       0.0,       0.0,       0.0, &
          0.0,       0.0, -0.045532,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0, -0.045532, -0.045532,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0/
!
DATA BCR8/ &
     0.017727,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,  0.017727,       0.0,       0.0, &
     0.017727,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,  0.017727,       0.0,       0.0, &
          0.0,       0.0,       0.0,  0.017727,       0.0, &
          0.0,       0.0,  0.017727,  0.017727,  0.017727, &
     0.017727,  0.017727,  0.017727,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,  0.017727, &
          0.0,  0.017727,  0.017727/
!
DATA BCR9/ &
    -0.000053,  0.000022,  0.000022,  0.000022,  0.000022, &
     0.000022,  0.000022, -0.000053,       0.0,       0.0, &
    -0.000053,       0.0,  0.000022,       0.0,       0.0, &
          0.0,       0.0, -0.000053,       0.0,       0.0, &
          0.0,  0.000022,  0.000022, -0.000053,       0.0, &
          0.0,       0.0, -0.000053, -0.000053, -0.000053, &
    -0.000053, -0.000053, -0.000053,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, -0.000053, &
          0.0, -0.000053, -0.000053/
!
DATA BCR10/ &
     0.014098, -0.013115, -0.013115, -0.013115, -0.013115, &
    -0.013115, -0.013115,  0.014098,       0.0,       0.0, &
     0.014098,       0.0, -0.013115,       0.0,       0.0, &
          0.0,       0.0,  0.014098,       0.0,       0.0, &
          0.0, -0.013115, -0.013115,  0.014098,       0.0, &
          0.0,       0.0,  0.014098,  0.014098,  0.014098, &
     0.014098,  0.014098,  0.014098,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,  0.014098, &
          0.0,  0.014098,  0.014098/
!
DATA CRSD/ &
          0.5,    0.6957,    0.6957,      0.15,    0.9310, &
       0.6957,    0.6957,    0.4942,    2.0426,    2.0426, &
          0.5,    2.0426,    0.6957,    2.0426,    2.0426, &
       2.0426,    2.0426,    0.4942,    2.0426,    2.0426, &
       2.0426,    0.6957,    0.15,      0.5,       2.0426, &
       2.0426,    2.0426,    0.6124,    0.6124,    0.6124, &
       0.6124,    0.6124,    0.6124,       0.2,       0.2, &
          0.2,       0.2,       0.2,       0.2,    0.6124, &
          0.5,       0.5,    0.6124/
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
RDANUW = TPCT
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
!----------
!  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
!  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
!  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
!  FOR GS, CROWN RATIO IS A FUNCTION OF DBH, HEIGHT-DIAMETER RATIO,
!  RELATIVE DENSITY, AND RELATIVE DIAMETER (D/QMD)
!----------

!  CALCULATE HEIGHT DIAMETER RATIO
HDR = (H*12)/D

! RW CR CALCULATION
IF(ISPC .EQ. 4 .OR. ISPC .EQ. 23) THEN
  CR = -1.021064 + 0.309296*LOG(HDR) + &
           0.869720*PRD - 0.116274*(D/QMDPLT)

! RW DEBUG
  IF(DEBUG)WRITE(JOSTND,*)'IN DUBSCR - RW/GS DEBUG',' D=',D, &
     ' H=',H,' PRD=',PRD,' QMDPLT=',QMDPLT,' HDR=', HDR,' CR=',CR

! ALL OTHER SPECIES CR CALCULATION
ELSE
  CR = BCR0(ISPC) &
      + BCR1(ISPC)*D &
      + BCR2(ISPC)*H &
      + BCR3(ISPC)*BA &
      + BCR5(ISPC)*TPCCF &
      + BCR6(ISPC)*(AVH/H) &
      + BCR8(ISPC)*AVH &
      + BCR9(ISPC)*(BA*TPCCF) &
      + BCR10(ISPC)*RMAI
ENDIF
!----------
!  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
!  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
!  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
!  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.
!----------
SD=CRSD(ISPC)
10 FCR=BACHLO(0.0,SD,RANN)
IF(ABS(FCR).GT.SD) GO TO 10
!
SELECT CASE (ISPC)
CASE (4,23)
  CR=1.0/(1.0+EXP(CR+FCR))
CASE (9:10,12,14:17,19:20,25:27)
  CR=CR+FCR
  CR=((CR-1.0)*10.0 + 1.0)/100.
CASE (41)
  CR=((CR-1.0)*10.0 + 1.0)/100.
CASE DEFAULT
  IF(ABS(CR+FCR).GE.86.)CR=86.
  CR=1.0/(1.0+EXP(CR+FCR))
END SELECT
!
IF(CR .GT. .95) CR = .950
IF(CR .LT. .05) CR=.05
IF(DEBUG)WRITE(JOSTND,*)' IN DUBSCR','CR=',CR,' FCR=',FCR
!     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
! 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
!    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
!    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
!
!
RETURN
END

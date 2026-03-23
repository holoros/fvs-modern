SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
IMPLICIT NONE
!----------
! EC $Id$
!----------
!  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
!  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
!  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
!  MEASUREMENTS AND ARE LESS THAN 1.0 INCH DBH.
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
REAL TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP), &
    CRSD(MAXSP),BCR5(MAXSP),BCR6(MAXSP), &
    BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
REAL RDANUW
!----------
!  SPECIES LIST FOR EAST CASCADES VARIANT.
!
!   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
!   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
!   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
!   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
!   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
!   6 = GRAND FIR               (GF)    ABIES GRANDIS
!   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
!   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
!   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
!  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
!  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
!  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
!  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
!  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
!  15 = NOBLE FIR               (NF)    ABIES PROCERA
!  16 = WHITE FIR               (WF)    ABIES CONCOLOR
!  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
!  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
!  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
!  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
!  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
!  22 = RED ALDER               (RA)    ALNUS RUBRA
!  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
!  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
!  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
!  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
!  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
!  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
!  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
!  30 = WILLOW SPECIES          (WI)    SALIX sp.
!  31 = OTHER SOFTWOODS         (OS)
!  32 = OTHER HARDWOODS         (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE EC VARIANT:
!      USE 6(GF) FOR 16(WF)
!      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
!
!  FROM THE WC VARIANT:
!      USE 19(WH) FOR 11(WH)
!      USE 33(PY) FOR 13(PY)
!      USE 31(WB) FOR 14(WB)
!      USE  7(NF) FOR 15(NF)
!      USE 30(LL) FOR 17(LL)
!      USE  8(YC) FOR 18(YC)
!      USE 29(WJ) FOR 19(WJ)
!      USE 21(BM) FOR 20(BM) AND 21(VN)
!      USE 22(RA) FOR 22(RA)
!      USE 24(PB) FOR 23(PB)
!      USE 25(GC) FOR 24(GC)
!      USE 34(DG) FOR 25(DG)
!      USE 26(AS) FOR 26(AS) AND 32(OH)
!      USE 27(CW) FOR 27(CW)
!      USE 28(WO) FOR 28(WO)
!      USE 36(CH) FOR 29(PL)
!      USE 37(WI) FOR 30(WI)
!----------
DATA BCR0/ &
    -1.669490, -1.669490,  -.426688,  -.426688,  -.426688, &
     -.426688, -1.669490,  -.426688,  -.426688, -1.669490, &
     7.558538,  -2.19723,  6.489813,  6.489813,  8.042774, &
     -.426688,  6.489813,  7.558538,       9.0,       5.0, &
          5.0,       5.0,       5.0,       5.0,       5.0, &
          5.0,       5.0,       5.0,       5.0,       5.0, &
     -2.19723,       5.0/
!
DATA BCR1/ &
     -.209765,  -.209765,  -.093105,  -.093105,  -.093105, &
     -.093105,  -.209765,  -.093105,  -.093105,  -.209765, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     -.093105,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR2/ &
          0.0,       0.0,   .022409,   .022409,   .022409, &
      .022409,       0.0,   .022409,   .022409,       0.0, &
    -0.015637,       0.0, -0.029815, -0.029815,  0.007198, &
      .022409, -0.029815, -0.015637,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR3/ &
      .003359,   .003359,   .002633,   .002633,   .002633, &
      .002633,   .003359,   .002633,   .002633,   .003359, &
    -0.009064,       0.0, -0.009276, -0.009276, -0.016163, &
      .002633, -0.009276, -0.009064,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR5/ &
      .011032,   .011032,       0.0,       0.0,       0.0, &
          0.0,   .011032,       0.0,       0.0,   .011032, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR6/ &
          0.0,       0.0,  -.045532,  -.045532,  -.045532, &
     -.045532,       0.0,  -.045532,  -.045532,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     -.045532,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR8/ &
      .017727,   .017727,       0.0,       0.0,       0.0, &
          0.0,   .017727,       0.0,       0.0,   .017727, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR9/ &
     -.000053,  -.000053,   .000022,   .000022,   .000022, &
      .000022,  -.000053,   .000022,   .000022,  -.000053, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
      .000022,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA BCR10/ &
      .014098,   .014098,  -.013115,  -.013115,  -.013115, &
     -.013115,   .014098,  -.013115,  -.013115,   .014098, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     -.013115,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA CRSD/ &
       0.5000,    0.5000,    0.6957,    0.6957,    0.6957, &
       0.9310,    0.6124,    0.6957,    0.6957,    0.4942, &
       1.9658,       0.2,    2.0426,    2.0426,    1.3167, &
       0.9310,    2.0426,    1.9658,       0.5,       0.5, &
          0.5,       0.5,       0.5,       0.5,       0.5, &
          0.5,       0.5,       0.5,       0.5,       0.5, &
          0.2,       0.5/
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
RDANUW = TPCT
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
!----------
SELECT CASE (ISPC)
!-----------
!  SPECIES USING EC VARIANT EQUATIONS
!----------
CASE(1:10,12,16,31)
!----------
!  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
!  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
!  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
!----------
  CR = BCR0(ISPC) &
      + BCR1(ISPC)*D &
      + BCR2(ISPC)*H &
      + BCR3(ISPC)*BA &
      + BCR5(ISPC)*TPCCF &
      + BCR6(ISPC)*(AVH/H) &
      + BCR8(ISPC)*AVH &
      + BCR9(ISPC)*(BA*TPCCF) &
      + BCR10(ISPC)*RMAI
!----------
!  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
!  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
!  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
!  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.
!----------
  SD=CRSD(ISPC)
10   FCR=BACHLO(0.0,SD,RANN)
  IF(ABS(FCR).GT.SD) GO TO 10
  IF(ABS(CR+FCR).GE.86.)CR=86.
  CR=1.0/(1.0+EXP(CR+FCR))
  IF(CR .GT. .95) CR = .950
  IF(CR .LT. .05) CR=.05
!       IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
! 600   FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
!    &   ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
!    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
!
!-----------
!  SPECIES USING WC VARIANT EQUATIONS
!----------
CASE(11,13:15,17:30,32)
!----------
!  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, HEIGHT,
!  AND BASAL AREA. THE EQUATION RETURNS A CROWN CODE VALUE 0-9
!----------
  CR=BCR0(ISPC) + BCR2(ISPC)*H + BCR3(ISPC)*BA
!----------
!  ASSIGN A RANDOM ERROR TO THE CROWN RATIO PREDICTION
!  VALUES OF CRSD ARE THE STANDARD ERROR VALUES FROM REGRESSION
!----------
  SD=CRSD(ISPC)
20   FCR=BACHLO(0.0,SD,RANN)
  IF(ABS(FCR).GT.SD) GO TO 20
  IF(DEBUG)WRITE(JOSTND,*)' IN DUBSCR H,BA,CR,FCR= ', &
     H,BA,CR,FCR
  CR=CR+FCR
!----------
!  CONVERT CODE VALUE TO A CROWN RATIO 0-100.
!----------
  CR=((CR-1.0)*10.0 + 1.0) / 100.
  IF(CR .GT. .95) CR=.95
  IF(CR .LT. .05) CR=.05
  IF(DEBUG)WRITE(JOSTND,602)ISPC,H,BA,CR
602   FORMAT(' IN DUBSCR, ISPC= ',I2,' H= ',F5.1,' BA= ',F9.4, &
     ' CR= ',F4.3)
!
END SELECT
!
RETURN
END

SUBROUTINE DGF(DIAM)
IMPLICIT NONE
!----------
! EC $Id$
!----------
!  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
!  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
!  WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
!  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
!  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
!  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
!  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
!  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
!  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
!  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CALCOM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'COEFFS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'OUTCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'PDEN.f90'
!
!
!OMMONS
!----------
!  DIMENSIONS FOR INTERNAL VARIABLES.
!
!     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
!             ARGUEMENT).
!     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
!             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
!             SPECIES).
!     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
!             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
!             EACH SPECIES).
!   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
!             RATIO SQUARED TERM IN THE DDS MODEL (ONE
!             COEFFICIENT FOR EACH SPECIES).
!    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
!             LARGER TREES TERM IN THE DDS MODEL
!             (ONE COEFFICIENT FOR EACH SPECIES).
!   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
!             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
!             COEFFICIENT PER SPECIES).
!    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
!             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
!             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).
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
LOGICAL DEBUG
INTEGER ISPC,I1,I2,I3,I,IPCCF
INTEGER MAPDSQ(7,MAXSP),MAPLOC(7,MAXSP)
REAL BARK,BRATIO,CONST,DIAGR
REAL CONSPP,D,CR,BAL,DUMMY,RELHT,DDS,SASP,XSITE,TEMEL
REAL DIAM(MAXTRE),DGLD(MAXSP),DGDUM(MAXSP),DGCR(MAXSP), &
      DGCRSQ(MAXSP),DGDBAL(MAXSP),DGHCCF(MAXSP),DGFOR(6,MAXSP), &
      DGDS(4,MAXSP),DGEL(MAXSP),DGEL2(MAXSP),DGSASP(MAXSP), &
      DGCASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),DGCCFA(MAXSP), &
      OBSERV(MAXSP),DGSITE(MAXSP),DGPCCF(MAXSP),SL0DUM(MAXSP), &
      DGBA(MAXSP)
!----------
DATA DGLD/ &
      1.32610,  0.609098,  0.855516,  0.980383,   0.58705, &
     1.042583,  0.554261,  0.823082,  0.816917,  0.665401, &
     0.722462,  0.580156,  0.879338,  0.879338,  0.904253, &
     1.042583,  0.879338,  0.816880,  0.889596,  1.024186, &
     1.024186,       0.0,  0.889596,  0.889596,  0.889596, &
     0.889596,  0.889596,   1.66609,  0.889596,  0.889596, &
     0.580156,  0.889596/
!
DATA DGCR/ &
      1.29730,  1.158355,  2.009866,  1.709846,   1.29360, &
     2.182084,  1.423849,  1.263610,  1.119493,  1.671186, &
     2.160348,  1.212069,  1.970052,  1.970052,  4.123101, &
     2.182084,  1.970052,  2.471226,  1.732535,  0.459387, &
     0.459387,       0.0,  1.732535,  1.732535,  1.732535, &
     1.732535,  1.732535,       0.0,  1.732535,  1.732535, &
     1.212069,  1.732535/
!
DATA DGCRSQ/ &
          0.0,       0.0,  -0.44082,       0.0,       0.0, &
    -0.843518,       0.0,       0.0,       0.0,       0.0, &
    -0.834196,       0.0,       0.0,       0.0, -2.689340, &
    -0.843518,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA DGSITE/ &
      0.86756,  0.351929,  1.119725,  0.323625,       0.0, &
     0.782092,  0.458662,  0.290959,  0.231960,  0.921987, &
     0.380416,  0.346907,  0.252853,  0.252853,  0.684939, &
     0.782092,  0.252853,  0.244694,  0.227307,  1.965888, &
     1.965888,       0.0,  0.227307,  0.227307,  0.227307, &
     0.227307,  0.227307,   0.14995,  0.227307,  0.227307, &
     0.346907,  0.227307/
!
DATA DGDBAL/ &
     -0.00239, -0.004253, -0.003075, -0.000261,  -0.02284, &
    -0.001323, -0.004803, -0.005163, -0.000702, -0.008065, &
    -0.004065,       0.0, -0.004215, -0.004215, -0.006368, &
    -0.001323, -0.004215, -0.005950, -0.001265, -0.010222, &
    -0.010222,       0.0, -0.001265, -0.001265, -0.001265, &
    -0.001265, -0.001265,       0.0, -0.001265, -0.001265, &
          0.0, -0.001265/
!
DATA DGDUM/ &
          0.0,       0.0,       0.0, -0.799079,       0.0, &
     0.522079,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     0.522079,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA DGHCCF/ &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,  0.156459,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     0.156459,       0.0/
!
DATA DGPCCF/ &
     -0.00044, -0.000568, -0.000441, -0.000643,  -0.00094, &
    -0.001574, -0.000627, -0.000883, -0.001102,   0.00112, &
          0.0, -0.001221,       0.0,       0.0, -0.000471, &
    -0.001574,       0.0,       0.0,       0.0, -0.000757, &
    -0.000757,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.001221,       0.0/
!----------
!  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
!  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
!  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
!  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR
!  CALIBRATION).
!----------
DATA  OBSERV/ &
        1185.,      591.,     6249.,     1210.,      100., &
        1950.,     1478.,      652.,      723.,     4021., &
        4836.,     1370.,      475.,      475.,     1467., &
        1950.,      475.,       0.0,      220.,       78., &
          78.,      125.,      220.,      220.,      220., &
         220.,      220.,     2144.,      220.,      220., &
        1370.,      220./
!----------
!  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
!  HABITAT CLASS.
!----------
DATA DGCCFA/ &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, -0.003183, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!----------
!  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
!  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
!----------
DATA MAPLOC/ &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,2,1,2,1,1,1, &
    1,2,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,2,1,1,1, &
    1,1,2,1,1,1,1, &
    1,1,1,2,2,1,1, &
    1,1,1,2,1,1,1, &
    1,1,1,1,1,1,1, &
    1,2,2,2,1,1,1, &
    1,2,3,3,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,2,2,2,1,1,1, &
    1,1,1,2,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,2,3,3,1,1,1, &
    1,1,1,1,1,1,1/
!
DATA DGFOR/ &
     -4.64535,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.605649,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -4.081038, -3.965956,       0.0,       0.0,       0.0,       0.0, &
    -0.441408, -0.538987,       0.0,       0.0,       0.0,       0.0, &
      1.49419,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -3.811100, -3.673109,       0.0,       0.0,       0.0,       0.0, &
    -1.084679, -1.172470,       0.0,       0.0,       0.0,       0.0, &
    -0.098284,  0.117987,       0.0,       0.0,       0.0,       0.0, &
    -0.420205, -0.312955,       0.0,       0.0,       0.0,       0.0, &
    -3.102028,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.147675, -0.298310,       0.0,       0.0,       0.0,       0.0, &
    -1.407548, -1.131934, -1.539078,       0.0,       0.0,       0.0, &
    -1.310067,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -1.310067,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -1.127977, -1.401865,       0.0,       0.0,       0.0,       0.0, &
    -3.811100, -3.673109,       0.0,       0.0,       0.0,       0.0, &
    -1.310067,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -1.277664,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -7.753469,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -7.753469,       0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
     -1.33299,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0, &
    -1.407548, -1.131934, -1.539078,       0.0,       0.0,       0.0, &
    -0.107648,       0.0,       0.0,       0.0,       0.0,       0.0/
!----------
!  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
!  IN THE DIAMETER INCREMENT MODELS    ARRAYED BY FOREST BY
!  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
!  COEFFICIENT.
!----------
DATA MAPDSQ/ &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1, &
    1,1,1,1,1,1,1/
!
DATA DGDS/ &
          0.0,       0.0,       0.0,       0.0, &
   -0.0001683,       0.0,       0.0,       0.0, &
    -0.000261,       0.0,       0.0,       0.0, &
   -0.0002189,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
   -0.0003694,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
   -0.0002039,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
   -0.0002468,       0.0,       0.0,       0.0, &
   -0.0001546,       0.0,       0.0,       0.0, &
   -0.0000192,       0.0,       0.0,       0.0, &
   -0.0001323,       0.0,       0.0,       0.0, &
   -0.0001323,       0.0,       0.0,       0.0, &
   -0.0003996,       0.0,       0.0,       0.0, &
   -0.0003694,       0.0,       0.0,       0.0, &
   -0.0001323,       0.0,       0.0,       0.0, &
   -0.0002536,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
   -0.0001737,       0.0,       0.0,       0.0, &
   -0.0001737,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
     -0.00154,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0, &
   -0.0000192,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0/
!----------
!  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
!  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
!  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
!  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
!  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
!  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
!  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
!  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
!  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
!  MODELS.  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.
!----------
DATA DGCASP/ &
      0.38002, -0.156235, -0.092151, -0.059062,  -0.06625, &
    -0.239156, -0.064328, -0.055587, -0.049761, -0.181022, &
          0.0, -0.097288,       0.0,       0.0, -0.374512, &
    -0.239156,       0.0, -0.023186,  0.085958,       0.0, &
          0.0,       0.0,  0.085958,  0.085958,  0.085958, &
     0.085958,  0.085958,       0.0,  0.085958,  0.085958, &
    -0.097288,  0.085958/
!
DATA DGSASP/ &
     -0.17911,  0.258712,  0.029947, -0.128126,   0.05534, &
    -0.185520, -0.142328,  0.216231,  0.002810, -0.149848, &
          0.0,  0.037062,       0.0,       0.0, -0.207659, &
    -0.185520,       0.0,  0.679903, -0.863980,       0.0, &
          0.0,       0.0, -0.863980, -0.863980, -0.863980, &
    -0.863980, -0.863980,       0.0, -0.863980, -0.863980, &
     0.037062, -0.863980/
!
DATA DGSLOP/ &
     -0.81780, -0.635704, -0.309511,  0.240178,   0.11931, &
     1.466089, -0.097297, -0.000577,  1.160345, -0.252705, &
     0.421486,  0.089774,       0.0,       0.0,  0.400223, &
     1.466089,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
     0.089774,       0.0/
!
DATA DGSLSQ/ &
      0.84368,       0.0,       0.0,  0.131356,       0.0, &
    -1.817050,  0.094464,       0.0, -1.740114,       0.0, &
    -0.693610,       0.0,       0.0,       0.0,       0.0, &
    -1.817050,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0/
!
DATA DGEL/ &
          0.0,  0.004379, -0.021091, -0.015087,  -0.00175, &
     0.023020, -0.001124, -0.014944, -0.009430, -0.005345, &
    -0.040067,  0.012082,       0.0,       0.0, -0.069045, &
     0.023020,       0.0,       0.0, -0.075986, -0.012111, &
    -0.012111,       0.0, -0.075986, -0.075986, -0.075986, &
    -0.075986, -0.075986,       0.0, -0.075986, -0.075986, &
     0.012082, -0.075986/
!
DATA DGEL2/ &
          0.0,       0.0,  0.000225,       0.0, -0.000067, &
    -0.000364,       0.0,       0.0,       0.0,       0.0, &
     0.000395,       0.0,       0.0,       0.0,  0.000608, &
    -0.000364,       0.0,       0.0,  0.001193,       0.0, &
          0.0,       0.0,  0.001193,  0.001193,  0.001193, &
     0.001193,  0.001193,       0.0,  0.001193,  0.001193, &
          0.0,  0.001193/
!
! DUMMY FOR SLOPE EQ 0
!
 DATA SL0DUM/ &
          0.0, -0.290174,       0.0, -0.174404,       0.0, &
    -0.360203,       0.0,       0.0, -0.278601,       0.0, &
          0.0, -0.099908,       0.0,       0.0,       0.0, &
    -0.360203,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
    -0.099908,       0.0/
!----------
!  BA COEFFICIENT FOR SPECIES USING EQNS FROM THE WC VARIANT
!----------
DATA DGBA/ &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0,       0.0,       0.0,       0.0, &
          0.0,       0.0, -0.000173, -0.000173,       0.0, &
          0.0, -0.000173, -0.000147, -0.000981,       0.0, &
          0.0,       0.0, -0.000981, -0.000981, -0.000981, &
    -0.000981, -0.000981,  -0.00204, -0.000981, -0.000981, &
          0.0, -0.000981/
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'DGF',3,ICYC)
IF(DEBUG) WRITE(JOSTND,3)ICYC
3 FORMAT(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
!----------
!  DEBUG OUTPUT: MODEL COEFFICIENTS.
!----------
IF(DEBUG) WRITE(JOSTND,*) 'IN DGF,HTCON=',HTCON, &
   'ELEV=',ELEV,'RELDEN=',RELDEN
IF(DEBUG) &
    WRITE(JOSTND,9000) DGCON,DGDSQ
9000 FORMAT(/11(1X,F10.5))
!----------
!  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
!  DEPENDENT
!----------
DO 20 ISPC=1,MAXSP
I1=ISCT(ISPC,1)
IF(I1.EQ.0) GO TO 20
I2=ISCT(ISPC,2)
CONSPP= DGCON(ISPC) + COR(ISPC)
IF(ISPC.EQ.12 .OR. ISPC.EQ.31)CONSPP=CONSPP-0.000021*RMAI*RELDEN
SELECT CASE (ISPC)
CASE(4,6,16)
  DUMMY = 1.0
CASE DEFAULT
  DUMMY = 0.0
END SELECT
!----------
!  BEGIN TREE LOOP WITHIN SPECIES ISPC.
!----------
DO 10 I3=I1,I2
I=IND1(I3)
D=DIAM(I)
IF (D.LE.0.0) GOTO 10
CR=ICR(I)*0.01
BAL = (1.0 - (PCT(I)/100.)) * BA
IPCCF=ITRE(I)
RELHT = 0.0
IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
IF(RELHT .GT. 1.5)RELHT=1.5
!
SELECT CASE (ISPC)
!----------
!  SPECIES USING EQUATIONS FROM THE EC VARIANT
!----------
CASE(1:10,12,16,31)
  DDS = CONSPP + DGLD(ISPC)*ALOG(D) &
      + DGDUM(ISPC)*DUMMY + CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC)) &
      + DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0)) &
      + DGPCCF(ISPC)*PCCF(IPCCF) &
      + DGHCCF(ISPC)*RELHT*PCCF(IPCCF)/100.0 &
      + DGCCFA(ISPC) *PCCF(IPCCF)*PCCF(IPCCF)/1000.0
  IF(ISPC.EQ.1)DDS=DDS+0.49649*RELHT
!----------
!  SPECIES USING EQUATIONS FROM THE WC VARIANT
!----------
CASE(11,13:15,17:21,23:30,32)
!----------
!  THIS FUNCTION OCCASIONALLY GIVES AN UNDERFLOW ERROR. SPLITTING
!  IT UP INTO TWO PARTS IS A TEMPORARY SOLUTION WHICH WORKS. GD 2/20/97
!----------
  DDS = CONSPP + DGLD(ISPC)*ALOG(D) &
      + CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC)) &
      + DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0)) &
      + DGPCCF(ISPC)*PCCF(IPCCF) + DGBA(ISPC)*BA
  IF(ISPC.EQ.11) DDS = DDS - 0.000358*RELHT
  IF(ISPC.EQ.28) DDS = DDS - 0.00326*BAL
!----------
!  RED ALDER
!  FUNCTION BOTTOMS OUT AT D=18. DECREASE LINERALY AFTER THAT TO
!  DG=0 AT D=28, AND LIMIT TO .1 ON LOWER END.  GED 4-15-93.
!----------
CASE(22)
  BARK=BRATIO(ISPC,D,HT(I))
  CONST=3.250531 - 0.003029*BA
  IF(D .LE. 18.) THEN
    DIAGR = CONST - 0.166496*D + 0.004618*D*D
  ELSE
    DIAGR = CONST - (CONST/10.)*(D-18.)
  ENDIF
  IF(DIAGR .LT. 0.1) DIAGR=0.1
  DDS = ALOG(DIAGR*(2.0*D*BARK+DIAGR))+ALOG(COR2(ISPC))+COR(ISPC)
!
END SELECT
!
IF(DEBUG) WRITE(JOSTND,8000) &
   I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH,DDS
8000 FORMAT(1H0,'IN DGF 8000F',2I5,10F11.4)
!---------
IF(DDS.LT.-9.21) DDS=-9.21
WK2(I)=DDS
!----------
!  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
!----------
IF(DEBUG)THEN
WRITE(JOSTND,9001) I,ISPC,DDS
9001 FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  LN(DDS)=',F7.4)
ENDIF
10 CONTINUE
!----------
!  END OF SPECIES LOOP.
!----------
20 CONTINUE
IF(DEBUG) WRITE(JOSTND,100)ICYC
100 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
!
RETURN
!
!
ENTRY DGCONS
!----------
!  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
!  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
!----------
!  CHECK FOR DEBUG.
!----------
CALL DBCHK (DEBUG,'DGF',3,ICYC)
!----------
!  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
!----------
DO 30 ISPC=1,MAXSP
ISPFOR=MAPLOC(IFOR,ISPC)
ISPDSQ=MAPDSQ(IFOR,ISPC)
SASP = &
                     (DGSASP(ISPC) * SIN(ASPECT) &
                    + DGCASP(ISPC) * COS(ASPECT) &
                    + DGSLOP(ISPC)) * SLOPE &
                    + DGSLSQ(ISPC) * SLOPE * SLOPE
!
SELECT CASE (ISPC)
CASE(1:10,12,16,31)
  IF(SLOPE .EQ. 0.0)SASP=SL0DUM(ISPC)
END SELECT
!
XSITE=SITEAR(ISPC)
SELECT CASE (ISPC)
CASE (12)
  XSITE=XSITE*3.281
CASE (28)
  XSITE=1-(XSITE/114.24569)**.4444
  IF (XSITE.LE.0) THEN
    XSITE=125.
  ELSE
    XSITE=-37.60812*ALOG(XSITE)
  ENDIF
CASE (31)
  XSITE=XSITE*3.281
END SELECT
!
TEMEL=ELEV
SELECT CASE (ISPC)
CASE (19,23:27,29,30,32)
  IF(TEMEL.GT.30.)TEMEL=30.
END SELECT
!
DGCON(ISPC) = &
                      DGFOR(ISPFOR,ISPC) &
                    + DGEL(ISPC) * TEMEL &
                    + DGEL2(ISPC) * TEMEL * TEMEL &
                    + DGSITE(ISPC)*ALOG(XSITE) &
                    + SASP
!
! DUMMY FOR SUBALPINE FIR
!
IF(ISPC .EQ. 9) DGCON(ISPC)=DGCON(ISPC) + 0.3835
!
DGDSQ(ISPC)=DGDS(ISPDSQ,ISPC)
ATTEN(ISPC)=OBSERV(ISPC)
SMCON(ISPC)=0.
IF(DEBUG)WRITE(JOSTND,9030)DGFOR(ISPFOR,ISPC), &
   DGEL(ISPC),ELEV,DGEL2(ISPC),DGSASP(ISPC),ASPECT, &
   DGCASP(ISPC),DGSLOP(ISPC),SLOPE,DGSITE(ISPC), &
   SITEAR(ISPC),DGCON(ISPC),SASP,XSITE
9030 FORMAT(' IN DGF 9030',14F9.5)
!----------
!  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
!  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
!  INITIALIZED TO 1.0 IN BLKDATA.
!----------
IF (LDCOR2.AND.COR2(ISPC).GT.0.0) DGCON(ISPC)=DGCON(ISPC) &
      + ALOG(COR2(ISPC))
30 CONTINUE
!
RETURN
!
END

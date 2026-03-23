SUBROUTINE ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER, &
     GENTIM,TRAGE)
IMPLICIT NONE
!----------
! CI $Id$
!----------
!  ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
!  CREATED BY THE ESTABLISHMENT MODEL.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ESPARM.f90'
!
!
INCLUDE 'ESCOMN.f90'
!
!
INCLUDE 'ESCOM2.f90'
!
!
!OMMONS
!----------
INTEGER IHTSER,I,N,ITIME,IAGE
REAL TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,AGE,AGELN,BNORM,PN
REAL UPRE(4,MAXSP),UHAB(5,MAXSP),UPHY(5,MAXSP)
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
!----------
!     UHAB CONTAINS COEFFICIENT FOR SUBSEQUENT HEIGHTS
!     BY SPECIES AND H.T. GROUP
!     H.T.GROUP--> WET DF  DRY D-FIR  GRAND F   WRC/WH     SAF
!----------
DATA UHAB/ &
           0.0,      0.0,      0.0,      0.0,       0.0, &
      -0.01541, -0.03814,  0.11409,  0.35334,       0.0, &
      -0.21858, -0.03354,  0.22756,  0.51988,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
      -0.29969, -0.15449,  0.04545, -0.00601,       0.0, &
           0.0,      0.0,  0.18740,  0.26511,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
      -0.02287, -0.14710,  0.19278,  0.13817,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0, &
           0.0,      0.0,      0.0,      0.0,       0.0/
!----------
!     UPRE HOLDS COEFFICIENTS FOR SUBSEQUENT HEIGHTS BY PREP/SPECIES
!     SITE PREP--> NONE      MECH       BURN      ROAD
!----------
DATA UPRE/ &
           0.0,      0.0,      0.0,      0.0, &
           0.0, -0.11310, -0.06246, 0.009632, &
           0.0,  0.06961,  0.19508,  0.17952, &
           0.0, -0.08010,  0.01032, -0.05975, &
           0.0,      0.0,      0.0,      0.0, &
           0.0, -0.41961, -0.22326,  0.15608, &
           0.0,  0.11502,  0.02486,  0.13080, &
           0.0,  0.10587,  0.27072,  0.16240, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,  0.20729,  0.18491,  0.11864, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0/
!----------
!     UPHY HOLDS COEF'S FOR SUBS HEIGHTS BY PHYS & H.T. GROUP
!     PHYS.POS-->  BOTTOM    LOWER      MID      UPPER   RIDGE
!----------
DATA UPHY/ &
      -0.18731, -0.48682, -0.32160, -0.16113,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
      -0.27801, -0.20433, -0.12317, -0.26736,      0.0, &
      -0.06976, -0.16483, -0.10900, -0.15873,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
       0.32401,  0.14743,  0.22165,  0.24559,      0.0, &
       0.41120,  0.01164,  0.22217,  0.15834,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0, &
           0.0,      0.0,      0.0,      0.0,      0.0/
!----------
!     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
!     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
!     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
!     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
!     THE TREE.
!----------
N=INT(DELAY + 0.5)
IF(N.LT.-3) N=-3
DELAY=FLOAT(N)
ITIME=INT(TIME + 0.5)
IF(N.GT.ITIME) DELAY=TIME
AGE=TIME-DELAY-GENTIM
IAGE=INT(AGE + 0.5)
IF(IAGE.LT.1) IAGE=1
AGE=AGE+TRAGE
IF(AGE.LT.1.0) AGE=1.0
AGELN=ALOG(AGE)
BNORM=BNORML(IAGE)
TRAGE=TIME-DELAY
!
SELECT CASE (I)
!----------
!  HT OF TALLEST SUBS. WWP. 6JAN88 CARLSON
!----------
CASE(1)
  PN= -1.51302 +1.24537*AGELN -0.003052*BAA +UPHY(IPHY,1)
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.46010)
!----------
!  HT OF TALLEST SUBSEQUENT WESTERN LARCH. 7JAN88 CARLSON
!----------
CASE(2)
  PN= -1.36257 +1.21548*AGELN -0.003797*BAA +UHAB(IHTSER,2) &
         +UPRE(IPREP,2)
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.52668)
!----------
!  HT OF TALLEST SUBS. D-FIR. 8JAN88 CARLSON
!----------
CASE(3)
  PN= -2.16416 +1.28151*AGELN -0.0031363*BAA +UHAB(IHTSER,3) &
         +UPRE(IPREP,3) +UPHY(IPHY,3) -0.09626*XCOS &
         -0.23946*XSIN -0.14589*SLO
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.55942)
!----------
!  HT OF TALLEST SUBS. GRAND FIR. 6JAN88 CARLSON
!----------
CASE(4)
  PN= -2.62001 +1.19408*AGELN -0.0035489*BAA +UPRE(IPREP,4) &
         +UPHY(IPHY,4) +0.01871*XCOS +0.09002*XSIN -0.37365*SLO &
         +0.05070*ELEV -0.000736*ELEV*ELEV
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.52958)
!----------
!  HT OF TALLEST SUBS. W. HEMLOCK. 7JAN88 CARLSON
!----------
CASE(5)
  PN= -2.42379 +1.52366*AGELN -0.003256*BAA
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.54116)
!----------
!  HT OF TALLEST SUB. W.CEDAR.
!----------
CASE(6)
  PN= -0.89895 +1.08584*AGELN -0.00205*BAA +UPRE(IPREP,6) &
         -0.01594*ELEV
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.56107)
!----------
!  HT OF TALLEST SUBS. LPP. 7JAN88 CARLSON
!----------
CASE(7)
  PN= -0.27105 +1.32027*AGELN -0.008208*BAA +UPRE(IPREP,7) &
         +UPHY(IPHY,7) +UHAB(IHTSER,7) -0.15385*XCOS +0.04156*XSIN &
         -0.49186*SLO -0.04744*ELEV +0.0003511*ELEV*ELEV &
         +0.01105*BWAF +0.02588*BWB4
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.47557)
!----------
!  HT OF TALLEST SUBS. E. SPRUCE. 7JAN88 CARLSON
!----------
CASE(8)
  PN= -2.93213 +1.43503*AGELN -0.002504*BAA +UPRE(IPREP,8) &
         +UPHY(IPHY,8) +UHAB(IHTSER,8)
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.48951)
!----------
!  HT OF TALLEST SUBS. SAF.  6JAN88 CARLSON
!----------
CASE(9)
  PN= -2.06377 +1.18184*AGELN -0.0044465*BAA +0.06615*XCOS &
         +0.03085*XSIN -0.37402*SLO
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.56740)
!----------
!  HEIGHT OF TALLEST SUBSEQUENT PONDEROSA PINE.
!----------
CASE(10)
  PN= -1.99480 +1.53946*AGELN -0.00402*BAA +UHAB(IHTSER,10) &
         +UPRE(IPREP,10) -0.01155*ELEV
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.49076)
!----------
!  HT OF TALLEST SUBS. WHITEBARK PINE
!----------
CASE(11)
  PN = 0.0
  HHT = 0.5
!----------
!  HT OF TALLEST SUBS. PACIFIC YEW
!----------
CASE(12)
  PN = 0.0
  HHT = 0.5
!----------
!  HT OF TALLEST SUBS. QUAKING ASPEN
!----------
CASE(13)
  PN = 0.0
  HHT = 5.0
!----------
!  HT OF TALLEST SUBS. WESTERN JUNIPER
!----------
CASE(14)
  PN = 0.0
  HHT = 0.5
!----------
!  HT OF TALLEST SUBS. CURLLEAF MOUNTAIN-MAHOGANY
!----------
CASE(15)
  PN = 0.0
  HHT = 0.5
!----------
!  HT OF TALLEST SUBS. LIMBER PINE
!----------
CASE(16)
  PN = 0.0
  HHT = 0.5
!----------
!  HT OF TALLEST SUBS. BLACK COTTONWOOD
!----------
CASE(17)
  PN = 0.0
  HHT = 5.0
!----------
!  HT OF TALLEST SUBS. OTHER SOFTWOODS (USES WH EQUATION).
!----------
CASE(18)
  PN= -2.42379 +1.52366*AGELN -0.003256*BAA
  HHT = EXP(PN +EMSQR*DILATE*BNORM*0.54116)
!----------
!  HT OF TALLEST SUBS. OTHER HARDWOODS
!----------
CASE(19)
  PN = 0.0
  HHT = 5.0
!
END SELECT
!
RETURN
END

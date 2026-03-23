SUBROUTINE DUBSCR(ISPC,D,H,PRD,QMDPLT,CR)
IMPLICIT NONE
!----------
! AK $Id$
!----------
!  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
!  THE REGENERATION ESTABLISHMENT MODEL. IT ALSO DUBS CROWN RATIOS
!  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
!  MEASUREMENTS AND MEET CERTAIN CRITERIA (DEAD OR DBH < 1 INCH).
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PDEN.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'CRCOEF.f90'
!
!
!OMMONS
!
!----------
!  VARIABLE DECLARATIONS:
!----------
LOGICAL DEBUG
!
EXTERNAL RANN
!
INTEGER ISPC
!
REAL BACHLO,CR,D,FCR,H,SD,PRD,QMDPLT
!
REAL CRSD(MAXSP)

!----------
!  DATA STATEMENTS:
!
!----------
! SPECIES LIST FOR ALASKA VARIANT.
!
! Number Code  Common Name         FIA  PLANTS Scientific Name
!   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
!   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
!   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
!   4     TA   tamarack            071  LALA   Larix laricina
!   5     WS   white spruce        094  PIGL   Picea glauca
!   6     LS   Lutz�s spruce            PILU   Picea lutzii
!   7     BE   black spruce        095  PIMA   Picea mariana
!   8     SS   Sitka spruce        098  PISI   Picea sitchensis
!   9     LP   lodgepole pine      108  PICO   Pinus contorta
!  10     RC   western redcedar    242  THPL   Thuja plicata
!  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
!  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
!  13     OS   other softwoods     298  2TN
!  14     AD   alder species       350  ALNUS  Alnus species
!  15     RA   red alder           351  ALRU2  Alnus rubra
!  16     PB   paper birch         375  BEPA   Betula papyrifera
!  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
!  18     BA   balsam poplar       741  POBA2  Populus balsamifera
!  19     AS   quaking aspen       746  POTR5  Populus tremuloides
!  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
!  21     WI   willow species      920  SALIX  Salix species
!  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
!  23     OH   other hardwoods     998  2TB
!
!----------
!
!    STANDARD ERROR OF REGRESSIONS FOR EACH SPECIES
DATA CRSD/ &
     0.1719, 0.1719, 0.159,  0.1645, 0.1542, &
     0.1542, 0.1894, 0.1719, 0.1568, 0.1811, &
     0.1582, 0.155,  0.1542, 0.1447, 0.1447, &
     0.144,  0.144,  0.1605, 0.1356, 0.1605, &
     0.0938, 0.0938, 0.1605/
!
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK(DEBUG,'DUBSCR',6,ICYC)

!----------
!  CALCULATE CROWN RATIO USING THE FOLLOWING LOGISITC EQUATION
!  FORM:

!  CR = 1/(1 + EXP(X)))
!  X = B1 + B2*HDR + B3*PRD + B4*DBH/QMD

!  WHERE
!  CR: CROWN RATIO
!  HDR: HEIGHT DIAMETER RATIO OF TREE RECORD
!  PRD: POINT LEVEL RELATIVE DENSITY
!  DBH: DIAMETER AT BREAST HEIGHT
!  QMDPLT: POINT LEVEL QUADRATIC MEAN DIAMETER.
!----------

CR = CRINT(ISPC) &
      + CRHDR(ISPC) * LOG((H*12/D)) &
      + CRRD(ISPC) * PRD &
      + CRDQMD(ISPC) * (D/QMDPLT)

!----------
!  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
!  PRIOR TO THE LOGISTIC TRANSFORMATION. THE ELEMENTS OF CRSD
!  ARE THE STANDARD ERRORS FROM THE FINAL CROWN RATIO EQUATIONS.
!----------
!     EXTRACT STANDARD ERROR AND DETERMINE RANDOM DRAW FROM
!     NORMAL DISTRIBUTION
SD=CRSD(ISPC)
10 FCR=BACHLO(0.0,SD,RANN)
IF(ABS(FCR).GT.SD) GO TO 10
!
!     CALCULATE CROWN RATIO
CR=1.0/(1.0+EXP(CR + FCR))
!
!     DETERMINE IF CROWN RATIO SHOULD BE CONSTRAINED
IF(CR .GT. .95) CR = 0.95
IF(CR .LT. .05) CR= 0.05

!  DO SOME DEBUG
IF(DEBUG)WRITE(JOSTND,*)'IN DUBSCR',' ISPC=',ISPC,' D=',D,' H=',H, &
     ' PRD=',PRD,' QMDPLT=',QMDPLT,' FCR=',FCR,' CR=',CR

!     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
! 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
!    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
!    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
!
RETURN
END

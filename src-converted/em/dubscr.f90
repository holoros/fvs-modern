SUBROUTINE DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
IMPLICIT NONE
!----------
! EM $Id$
!----------
!  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
!  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
!  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
!  MEASUREMENTS AND ARE LESS THAN 1.0 (3.0 FOR SOME SPECIES) INCHES DBH.
!  FINALLY, IT ISUSED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
!  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.
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
REAL D,H,CR,TPCT,TPCCF,BACHLO,FCR,SD
REAL BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP), &
    CRSD(MAXSP),BCR5(MAXSP),BCR6(MAXSP), &
    BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
REAL RDANUW
!----------
!  SPECIES ORDER:
!   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
!  17=PB, 18=OS, 19=OH
!
!  SPECIES EXPANSION
!  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
!  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
!  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
!  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
!  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
!----------
!  NOTE: THE ORIGINAL SOURCE OF MOST OF THESE EQUATIONS GOES BACK
!  TO THE 11 SPECIES VERSIONS OF THE SO AND NI VARIANTS.
!  LL IS THE NI AF EQUATION;
!  RM USES THE UT JU EQUATION;
!  ALL OTHER SPECIES USE EQUATIONS FROM SO.
!
!  ALSO NOTE THAT THE STANDARD DEVIATIONS HAVE BEEN MANIPULATED TO
!  GET SOME SPECIES DIFFERENCES (FOR EXAMPLE, IN SO, WB, WL, LP, AND
!  PP ALL USE THE SAME EQUATION BUT THE STANDARD DEVIATIONS HAVE BEEN
!  MODIFIED TO BETTER REPRESENT THE SPECIES.)
!----------
DATA BCR0/ &
    -1.669490, -1.669490,  -.426688,  -1.66949,   -.89014,        0., &
    -1.669490,  -.426688,  -.426688, -1.669490,        0.,  -.426688, &
           0.,        0.,        0.,        0.,  -.426688,  -2.19723, &
           0./
DATA BCR1/ &
     -.209765,  -.209765,  -.093105,  -.209765,  -0.18026,        0., &
     -.209765,  -.093105,  -.093105,  -.209765,        0.,  -.093105, &
           0.,        0.,        0.,        0.,  -.093105,        0., &
           0./
DATA BCR2/ &
           0.,        0.,   .022409,        0.,    .02233,        0., &
           0.,   .022409,   .022409,        0.,        0.,   .022409, &
           0.,        0.,        0.,        0.,   .022409,        0., &
           0./
DATA BCR3/ &
      .003359,   .003359,   .002633,   .003359,    .00614,        0., &
      .003359,   .002633,   .002633,   .003359,        0.,   .002633, &
           0.,        0.,        0.,        0.,   .002633,        0., &
           0./
DATA BCR5/ &
      .011032,   .011032,        0.,   .011032,        0.,        0., &
      .011032,        0.,        0.,   .011032,        0.,        0., &
           0.,        0.,        0.,        0.,        0.,        0., &
           0./
DATA BCR6/ &
           0.,        0.,  -.045532,        0.,        0.,        0., &
           0.,  -.045532,  -.045532,        0.,        0.,  -.045532, &
           0.,        0.,        0.,        0.,  -.045532,        0., &
           0./
DATA BCR8/ &
      .017727,   .017727,        0.,   .017727,        0.,        0., &
      .017727,        0.,        0.,   .017727,        0.,        0., &
           0.,        0.,        0.,        0.,        0.,        0., &
           0./
DATA BCR9/ &
     -.000053,  -.000053,   .000022,  -.000053,        0.,        0., &
     -.000053,   .000022,   .000022,  -.000053,        0.,   .000022, &
           0.,        0.,        0.,        0.,   .000022,        0., &
           0./
DATA BCR10/ &
      .014098,   .014098,  -.013115,   .014098,        0.,        0., &
      .014098,  -.013115,  -.013115,   .014098,        0.,  -.013115, &
           0.,        0.,        0.,        0.,  -.013115,        0., &
           0./
DATA CRSD/ &
    .5000, .5000, .6957, .5000, .8871,    0., .6124, .6957, .6957, &
    .4942,    0., .9310,    0.,    0.,    0.,    0., .9310,  .200, &
       0./
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
RDANUW = TPCT
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)
IF(DEBUG) WRITE(JOSTND,3)ICYC
3 FORMAT(' ENTERING SUBROUTINE DUBSCR  CYCLE =',I5)
!----------
!  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
!  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
!  AND RETURNS A VALUE BETWEEN ZERO AND ONE.
!----------
CR=BCR0(ISPC) + BCR1(ISPC)*D + BCR2(ISPC)*H + BCR3(ISPC)*BA &
      + BCR5(ISPC)*TPCCF + BCR6(ISPC)*(AVH/H) + BCR8(ISPC)*AVH &
      + BCR9(ISPC)*(BA*TPCCF) + BCR10(ISPC)*RMAI
!----------
!  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
!  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
!  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
!  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.
!----------
SD=CRSD(ISPC)
10 CONTINUE
FCR=0.0
IF (DGSD.GE.1.0) FCR=BACHLO(0.0,SD,RANN)
IF(ABS(FCR).GT.SD) GO TO 10
IF(ABS(CR+FCR).GE.86.)CR=86.
CR=1.0/(1.0+EXP(CR+FCR))
IF(CR .GT. .95) CR = .950
IF(CR .LT. .05) CR=.05
!     IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI,TAVH
! 600 FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
!    & ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
!    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
RETURN
END

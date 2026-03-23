SUBROUTINE ESPXCS (IFT)
IMPLICIT NONE
!----------
! AK $Id$
!----------
!     PREDICTS THE PROBABILITY OF EXCESS SPECIES.
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
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'PDEN.f90'
!
!
INCLUDE 'ESHAP.f90'
!
!
INCLUDE 'ESHAP2.f90'
!
!
!OMMONS
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!
!  IFT      -- STAND FOREST TYPE CATEGORY WHERE:
!                1 = 122
!                2 = 125
!                3 = 270
!                4 = 271
!                5 = 281
!                6 = 301
!                7 = 304
!                8 = 305
!                9 = 703
!               10 = 901
!               11 = 902
!               12 = 904
!               13 = 911
!               14 = OTHER (NO ADVANCED REGENERATION)
!  MAXSP    -- MAXIMUM NUMBER OF SPECIES, PASSED IN PRGPRM.F77
!  PNFT     -- SPECIES X FOREST TYPE COEFFICIENT (B1)
!  PNRDA    -- RELATIVE DENSITY COEFFICIENT (B2)
!  PBAPER   -- SPECIES BASAL AREA PERCENTAGE COEFFICIENT (B3)
!  PNEVEL   -- ELEVATION COEFFICIENT (B4)
!  PNSLO    -- SLOPE COEFFICIENT (B5)
!  PNTLAT   -- STAND LATTITUDE COEFFICIENT (B6)
!  PN       -- USED IN DETERMINING SPECIES PROBABILITY
!  XPRD     -- PLOT RELATIVE DENSITY >= REGNBK, PASSED IN AS
!              PRDA(MAXPLOT) IN PDEN.F77
!  BAPER    -- PLOT SPECIES BASAL AREA PROPORTION>= REGNBK,
!              PASSED IN AS OVER(MAXSP,MAXPLOT) IN PDEN.F77
!  ELEV     -- STAND ELEVATION (100S OF FEET), PASSED IN PLOT.F77
!  SLO      -- PLOT SLOPE IN PROPORITON, PASSED IN ESCOMN.F77
!  TLAT     -- STAND LATTITUDE, PASSED IN PLOT.F77
!  NNID     -- BASE MODEL PLOT NUMBER, PASSE IN ESCOMN.F77
!  OCURFT   -- SPECIES OCCURANCE BY FOREST TYPE, 0=NO, 1=YES,
!              PASSED IN ESCOMN.F77
!  XESMLT   -- SPECIES MULTIPLIER, PASSED IN ESHAP.F77
!  PXCS     -- SPECIES PROBABILITY, PASSED TO ESCOM2.F77
!----------
!  VARIABLE DECLARATIONS:
!----------
!
INTEGER IFT, I, J
!
REAL PN,PNFT(MAXSP,14),PNRDA(MAXSP),PBAPER(MAXSP),PNELEV(MAXSP), &
    PNSLO(MAXSP),PNTLAT(MAXSP),B1,B2,B3,B4,B5,B6,XPRD,BAPER,DENOM, &
    ADJSLO, ADJELV
!
!----------
DATA ((PNFT(I,J),I=1,MAXSP),J=1,14)/ &
      0.0,         0.0,       0.0,       -4.335977, -0.768725, &  ! 122 WHITE SPRUCE
      0.0,       -13.023483,  0.0,        0.0,       0.0, &
      0.0,       -14.636196,  0.0,        0.0,       0.0, &
      4.543671,    0.0,      -0.129163, -10.517939,  0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,       -4.335977, -1.793084, &  ! 125 BLACK SPRUCE
      0.0,       -10.509385,  0.0,        0.0,       0.0, &
      0.0,       -15.787063,  0.0,        0.0,       0.0, &
      3.846221,    0.0,      -4.067106, -11.048545,  0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      17.052464,   0.0,      -3.104481, &  ! 270 MOUNTAIN HEMLOCK
      0.0,         0.0,      -1.727407,  13.303874, 77.557037, &
      23.185698, -10.383977,  0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      17.998179,   0.0,       0.0, &  ! 271 ALASKA CEDAR
      0.0,         0.0,      -1.900489,  14.426165, 79.436554, &
      23.405695,  -9.457501,  0.0,        0.0,      35.036460, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      18.116574,   0.0,       0.0, &  ! 281 LODGEPOLE PINE
      0.0,         0.0,      -3.220587,  14.50099,  79.452263, &
      22.762266, -10.122027,  0.0,        0.0,      35.036460, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      15.963877,   0.0,       0.0, &  ! 301 WESTERN HEMLOCK
      0.0,         0.0,      -1.224259,  12.070894, 78.000985, &
      24.451408, -11.165193,  0.0,        0.0,      35.036460, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      17.094312,   0.0,       0.0, &  ! 304 WESTERN REDCEDAR
      0.0,         0.0,      -1.434335,  13.561585, 79.462069, &
      23.941341, -10.180572,  0.0,        0.0,      35.036460, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,      14.997227,   0.0,       0.0, &  ! 305 SITKA SPRUCE
      0.0,         0.0,      -0.563591,  11.217778, 78.007480, &
      22.942334, -11.404114,  0.0,        0.0,      35.036460, &
      -0.265927,   0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.231061, &  ! 703 COTTONWOOD
      0.0,       -14.506330, -2.473404,   0.0,       0.0, &
      21.694368, -14.517376,  0.0,        0.0,       0.0, &
      3.879223,    0.0,       0.0,      -10.664679, -2.676858, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,       -4.335977,  0.65022, &  ! 901 ASPEN
      0.0,       -12.961809,  0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      3.691336,    0.0,       0.107820, -11.026581,  0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,       -4.335977, -0.090381, &  ! 902 PAPER BIRCH
      0.0,       -12.383669, -5.003714,   0.0,       0.0, &
      20.687934, -15.353202,  0.0,        0.0,       0.0, &
      5.542455,    0.0,      -0.743739, -10.160502,  0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,       -4.335977, -0.768725, &  ! 904 BALSAM POPLAR (MAP TO WHITE SPRUCE)
      0.0,       -13.023483,  0.0,        0.0,       0.0, &
      0.0,       -14.636196,  0.0,        0.0,       0.0, &
      4.543671,    0.0,      -0.129163, -10.517939,  0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &  ! 911 RED ALDER
      0.0,         0.0,      -0.042003,   0.0,       0.0, &
      21.940216,   0.0,       0.0,        0.0,      35.03646, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &  ! OTHER F.T.
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0,        0.0,       0.0, &
      0.0,         0.0,       0.0 /
DATA PNRDA / &
      0.0,       0.0,      -2.727570,  0.0,        0.774395, &
      0.0,       1.660146, -0.838168, -3.839624,  -1.443889, &
      1.739987, -0.620028,  0.0,       0.0,        0.851525, &
     -1.846654,  0.0,      -0.464799, -0.397800,  -0.464799, &
      0.0,       0.0,       0.0 /
DATA PBAPER / &
      0.0,      0.0,      3.266453, 0.0,      2.917051, &
      0.0,     19.545809, 0.891633, 2.472698, 1.074455, &
      5.056352, 3.062509, 0.0,      0.0,      5.115524, &
      0.957204, 0.0,      5.337844, 3.757893, 5.337844, &
      0.0,      0.0,      0.0 /
DATA PNELEV / &
      0.0,      0.0,       0.000346,   0.0,       0.000580, &
      0.0,      0.000381, -0.000293,  -0.000699, -0.001340, &
     -0.000998, 0.000542,  0.0,        0.0,      -0.004470, &
     -0.000526,  0.0,     -0.000588,  -0.000492, -0.000588, &
      0.0,      0.0,       0.0 /
DATA PNSLO / &
      0.0,       0.0,     -0.003355,  0.0,      -0.020237, &
      0.0,      -0.044745, 0.003523, -0.002279,  0.006463, &
      0.007062, -0.007500, 0.0,       0.0,      -0.027259, &
      0.021042,  0.0,      0.019300,  0.029324,  0.019300, &
      0.0,       0.0,      0.0 /
DATA PNTLAT / &
      0.0,       0.0,      -0.314786,  0.0,        -0.025084, &
      0.0,       0.156958,  0.023200, -0.253624,   -1.427424, &
     -0.415727,  0.162942,  0.0,       0.0,        -0.707129, &
     -0.077977,  0.0,      -0.035312,  0.145927,   -0.035312, &
      0.0,       0.0,       0.0 /

!     CALCULATE PLOT DENSITIES
XPRD = PRDA(NNID)
DENOM = BAAA(NNID)
IF(BAAA(NNID).LE.0.) DENOM=1.
!     ADJUST PLOT SLO-BASED VARIABLES TO PERCENT SLOPE.
ADJSLO = SLO*100
ADJELV = ELEV*100

!----------
!  PREDICT PROBABILITY OF EXCESS SPECIES
!
!  PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + B5 * ADJSLO +
!  B6 * TLAT
!
!  VARIABLES DEFINED IN SECTION ABOVE
!  PN IS CONVERTED TO PROBABILITY OF EXCESS SPECIES BY
!  EXP(PN)/(1 + EXP(PN))
!----------
!  IF THE FOREST TYPE IS UNDEFINED (IFT=14), THE PROBABILITY CAN NOT
!  BE CALCULATED AND PADV VALUE WILL BE ZERO.
!
IF(IFT .EQ. 14) THEN
  DO J=1,MAXSP
    PXCS(J) = 0.0
  ENDDO
ELSE

!     CYCLE THROUGH SPECIES TO COMPUTE SPECIES PROBABILITIES
  DO J=1,MAXSP
    IF(OCURFT(J,IFT) .EQ. 0.0) THEN
      PXCS(J) = 0.0
    ELSE
!           SET COEFFICIENTS FOR COMPUTING PN
      B1 = PNFT(J,IFT)
      B2 = PNRDA(J)
      B3 = PBAPER(J)
      B4 = PNELEV(J)
      B5 = PNSLO(J)
      B6 = PNTLAT(J)
      BAPER = OVER(J,NNID)/DENOM
      PN = B1 + B2 * XPRD + B3 * BAPER + B4 * ADJELV + &
              B5 * ADJSLO + B6 * TLAT
      PXCS(J)=(EXP(PN)/(1 + EXP(PN))) * OCURFT(J,IFT) * XESMLT(J)

!           MAKE SURE SPECIES PROBABILITIES ARE BETWEEN 0 AND 1.
      IF(PXCS(J) .LT. 0.0) PXCS(J) = 0.0
      IF(PXCS(J) .GT. 1.0) PXCS(J) = 1.0
    ENDIF
  ENDDO
ENDIF

RETURN
END

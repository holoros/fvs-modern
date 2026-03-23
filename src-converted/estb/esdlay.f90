SUBROUTINE ESDLAY (ISPE,IAS,DRAW,DELAY)
IMPLICIT NONE
!----------
! ESTB $Id$
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
!
!     CONTAINS WEIBULL MAXIMUM LIKLIHOOD FUNCTIONS FOR DETERMINING
!     THE NUMBER OF YEARS BETWEEN LAST PLOT DISTURBANCE AND
!     GERMINATION OF BEST TREES.
!
REAL BSUB(3,MAXSP),CSUB(3,MAXSP),BBW(3,4),CBW(3,4), &
     BADV(2,MAXSP),CADV(2,MAXSP),BBW1(2,4),CBW1(2,4),BBW2(2,4), &
     CBW2(2,4)
REAL DELAY,DRAW,BB,CC
INTEGER IAS,ISPE,IT,IBW,IB,IBAA,I
!
!     PLOT AGE (YRS): 2 THRU 7   8 THRU 12   13 THRU 20
!
DATA BSUB/      3.52946,   7.62339,    12.79801, &
                   5.23792,   7.38005,    10.42350, &
                   4.34376,   6.55916,     9.16226, &
                   4.17909,   5.88262,     8.49857, &
                   4.33094,   6.30802,     8.63060, &
                   4.16284,   7.52536,    10.20937, &
                   5.33757,   6.78727,     9.45827, &
                   5.36466,   7.44468,     9.69507, &
                   3.45725,   6.34975,     8.65545, &
                   3.81610,   5.74622,     9.36345, &
                   4.33094,   6.30802,     8.63060/
DATA BBW/       4.01218,   5.86172,    10.61297, &
                   3.67409,   6.12256,     8.74195, &
                   5.92251,   8.62548,     8.76074, &
                   4.11555,   6.19124,     8.82962/
!
!     PLOT AGE (YRS): 3 THRU 7   8 THRU 12   13 THRU 20
!
DATA CSUB/      1.71621,   2.72466,     3.98359, &
                   3.11598,   3.04038,     2.87196, &
                   2.33194,   2.63560,     2.21663, &
                   2.47058,   2.25957,     2.00065, &
                   1.97408,   2.35053,     2.00997, &
                   2.03892,   3.12279,     2.68340, &
                   4.16994,   3.59937,     2.39138, &
                   2.89777,   2.80504,     3.27745, &
                   2.06804,   2.79933,     2.28687, &
                   3.01975,   2.09376,     1.80925, &
                   1.97408,   2.35053,     2.00997/
DATA CBW/       2.16561,   2.16010,     2.44953, &
                   2.19139,   2.38522,     2.03146, &
                   3.31325,   5.21269,     2.30270, &
                   1.98007,   2.84149,     2.46665/
!
!     OVERSTORY BA:  0 TO 25  26+ SQ.FT./A.
!     NO BUDWORM BEFORE HARVEST
!
DATA BADV/    6.699826, 13.431179, &
                 9.768223, 27.100242, &
                13.121021, 22.186540, &
                11.269182, 18.245664, &
                13.604594, 19.605485, &
                17.779381, 24.241344, &
                 7.358880, 32.955809, &
                13.990273, 21.779362, &
                21.962337, 32.176727, &
                 8.986115, 10.312660, &
                13.604594, 19.605485/
!
!     BUDWORM 1-3 YEARS BEFORE HARVEST
!
DATA BBW1/   16.856252, 18.601097, &
                11.393035, 19.994331, &
                 8.388204, 18.023409, &
                20.207660, 30.052681/
!
!     BUDWORM 4-5 YEARS BEFORE HARVEST
!
DATA BBW2/   18.833076, 17.660965, &
                16.154050, 13.936793, &
                10.587442, 16.554662, &
                25.512363, 20.555202/
!
!     NO BUDWORM BEFORE HARVEST
!
DATA CADV/    1.262533,  1.279302, &
                 1.152577,  1.319692, &
                 1.043254,  1.215651, &
                 1.122139,  1.082805, &
                 1.267057,  1.287445, &
                 1.337217,  1.540663, &
                 0.912295,  1.230540, &
                 1.051296,  1.416222, &
                 1.101266,  1.317022, &
                 1.068472,  1.470405, &
                 1.267057,  1.287445/
!
!     BUDWORM 1-3 YEARS BEFORE HARVEST
!
DATA CBW1/    1.165449,  1.182513, &
                 1.137030,  1.355774, &
                 1.332778,  1.502281, &
                 1.660504,  1.832537/
!
!     BUDWORM 4-5 YEARS BEFORE HARVEST
!
DATA CBW2/    1.307195,  1.938481, &
                 1.330297,  1.311782, &
                 1.470496,  1.207402, &
                 1.871949,  1.248784/
!
IT=1
IF(TIME.GT.7.5.AND.TIME.LT.12.5) IT=2
IF(TIME.GT.12.5) IT=3
IBW=INT(BWB4+BWAF+0.5)
IB=1
IF(IBW.GT.2) IB=2
IBAA=1
IF(BAA.GT.25.5) IBAA=2
IF(IAS.EQ.2) GO TO 20
!
!     ADVANCE REGENERATION
!
BB=BADV(IBAA,ISPE)
CC=CADV(IBAA,ISPE)
IF(BWB4.LT.0.5) GO TO 40
IF(ISPE.NE.3.AND.ISPE.NE.4.AND.ISPE.NE.8.AND.ISPE.NE.9) &
     GO TO 40
IF(ISPE.EQ.3) I=1
IF(ISPE.EQ.4) I=2
IF(ISPE.EQ.8) I=3
IF(ISPE.EQ.9) I=4
IF(BWB4.GT.3.5) GO TO 45
BB=BBW1(IBAA,I)
CC=CBW1(IBAA,I)
GO TO 40
45 CONTINUE
BB=BBW2(IBAA,I)
CC=CBW2(IBAA,I)
40 CONTINUE
DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
DELAY= (DELAY+3.0) *(-1.0)
GO TO 50
20 CONTINUE
!
!     SUBSEQUENT REGENERATION
!
BB=BSUB(IT,ISPE)
CC=CSUB(IT,ISPE)
IF(IB.NE.2) GO TO 30
IF(ISPE.NE.3.AND.ISPE.NE.4.AND.ISPE.NE.8.AND.ISPE.NE.9) &
     GO TO 30
IF(ISPE.EQ.3) I=1
IF(ISPE.EQ.4) I=2
IF(ISPE.EQ.8) I=3
IF(ISPE.EQ.9) I=4
BB=BBW(IT,I)
CC=CBW(IT,I)
30 CONTINUE
DELAY=((-(ALOG(1.0-DRAW)))**(1.0/CC))*BB
DELAY=DELAY-4.0
50 CONTINUE
!
IF(DELAY .GE. 10.)THEN
  DELAY=10.
ELSEIF(DELAY .LE. 0.)THEN
  DELAY=0.
ENDIF
!
RETURN
END

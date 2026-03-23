SUBROUTINE DMCW(DMTRCW)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMCW --NISI Date of last revision: 08/05/94
! This module has been made by modifing the COVER MODEL module CVCW
!  Modified for East Cascades.
!--------------------------------------------------------------------
! Purpose:
!   Computes crown width for individual trees. Widths are measured
! in feet. Further documentation can be found in: Moeur, Melinda.
! 1981. Crown width and foliage weight of northern Rocky Mountain
! Confifers. USDA Forest Service Res. Pap. INT-283.
!--------------------------------------------------------------------
!
! Called by:
!
!     DMMTRX
!
! Other routines called:
!
!     DBCHK
!
! Argument list definitions:
!
!     REAL    DMTRCW (O)  Predicted maximum crown width (feet).
!
! Local variable definitions (not complete):
!
!     REAL    BH1         COEFFICIENTS FOR HEIGHT TERM FOR CROWN
!                          WIDTH FUNCTION FOR TREES LESS THAN
!                          3.5 INCHES DBH.
!     REAL    BINT2       INTERCEPTS FOR CROWN WIDTH FUNCTION FOR.
!                          TREES 3.5 INCHES AND LARGER
!     REAL    BH2         COEFFICIENTS FOR HEIGHT TERM FOR TREES
!                          .GE. 3.5 INCHES.
!     REAL    BAREA       STAND BASAL AREA (sq feet?).
!     REAL    CL          CROWN LENGTH (feet).
!     REAL    D           TREE DBH (inches).
!     REAL    H           TREE HEIGHT (feet).
!
! Common block variables and parameters:
!
!     [none related to NISI; FVS commons are not documented]
!
!********************************************************************C

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'DMCOM.f90'

! Argument list variables

REAL      DMTRCW

DIMENSION DMTRCW(MAXTRE)

! Local variables.

LOGICAL   DEBUG
DIMENSION BH1(MAXSP)
DIMENSION BINT2(MAXSP)
DIMENSION BH2(MAXSP)

! Data assignments.

DATA BH1   / 0.37031,  0.23846,  0.32874,  0.38503, &
                0.46452,  0.38503,  0.26342,  0.33089, &
                0.33722,  0.36380,  0.07049  /

DATA BINT2 / 4.30800,  2.31359,  3.02271,  2.20611, &
                2.79784,  2.20611,  1.06804,  3.76535, &
                1.74558,  1.62365, -0.91984  /

DATA   BH2 /-1.37265, -0.80919, -1.00486, -0.76936, &
               -0.89666, -0.76936, -0.55987, -1.18257, &
               -0.73972, -0.68098, -0.07299  /

!
!  CHECK FOR DEBUG.
!
CALL DBCHK(DEBUG,'DMCW',4,ICYC)
IF (DEBUG) WRITE (JOSTND,9000) ICYC
9000 FORMAT (/' **CALLING DMCW, CYCLE = ',I2 / '         I      ISPI', &
    '         D         H        CL     BAREA   DMTRCW')
!
!     RETURN IF NOTREES OPTION IN EFFECT.
!
IF (ITRN .GT. 0) GOTO 5
IF (DEBUG) WRITE (JOSTND,9001) ITRN
9001 FORMAT (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
RETURN
5 CONTINUE
!
!  USE PRE-THIN DENSITY STATISTICS IF A THINNING HAS JUST OCCURRED.
!
BAREA = BA
ALOGBA = ALOG(BAREA)
!
!  ENTER TREE LOOP
!
DO 100 I = 1,ITRN
  D = DBH(I)
  H = HT(I)
  ISPI = ISP(I)
  IICR = ICR(I)
  CL = FLOAT(IICR)*H/100.
!
!  BRANCH ON DBH
!
  IF (D .LT. 3.5) GOTO 10
!
!  COMPUTE CROWN WIDTH FOR TREES 3.5 INCHES AND LARGER
!
  DMTRCW(I) = EXP (BINT2(ISPI) +  1.08137*ALOG(D) + &
                   BH2(ISPI)*ALOG(H) + 0.29786*ALOG(CL))
!
!  CORRECT ESTIMATE FOR NEGATIVE BIAS. (BASKERVILLE 1972)
!  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP (.5*.04898)
!
  DMTRCW(I) = DMTRCW(I) * 1.02479
  GOTO 90
!
!  COMPUTE CROWN WIDTH FOR TREES LESS THAN 3.5 INCHES
!
10   CONTINUE

  DMTRCW(I) = EXP (BH1(ISPI)*ALOG(H) + 0.28283 &
                   *ALOG(CL) + 0.04032*ALOGBA)
!----------
!  CORRECT ESTIMATE FOR NEGATIVE BIAS
!  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP(.5*.06036)
!----------
  DMTRCW(I) = DMTRCW(I)*1.03064
!----------
90   CONTINUE
!----------
  IF (DEBUG) WRITE (JOSTND,9002) I,ISPI,D,H,CL,BAREA, &
                   DMTRCW(I)
9002   FORMAT (2I10,5F10.1)
!
100   CONTINUE

RETURN
END

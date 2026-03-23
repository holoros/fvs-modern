SUBROUTINE DMCW(DMTRCW)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMCW --NISI Date of last revision: 15-July-2008
! This module has been made by modifing the COVER MODEL module CVCW
!  Modified for Northern Idaho.
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
!  ENTER TREE LOOP
!
DO 100 I = 1,ITRN
  DMTRCW(I) = CRWDTH(I)
  IF (DEBUG) WRITE (JOSTND,9002) I,ISPI,D,H,CL,BAREA, &
                   DMTRCW(I)
9002   FORMAT (2I10,5F10.1)
!
100   CONTINUE

RETURN
END

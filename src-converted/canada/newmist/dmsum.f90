SUBROUTINE DMSUM(DMTRCW, IDMSHP)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMSUM --     DATE OF LAST REVISION:  02/23/96
! This code was adapted from the CVSUM subroutine of the COVER model.
!----------
!  Purpose:
!    This routine computes the crown radius and volume (in MESH
! units) for each treelist record. Further documentation can be found
! in: Moeur, Melinda. 1981. Crown width and foliage weight of
! northern Rocky Mountain Confifers. USDA Forest Service Res. Pap.
! INT-283.
!
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
!     REAL    DMTRCW  (I) Predicted maximum crown width (feet).
!     INTEGER IDMSHP  (I) Tree crown shape category.
!
! Local variable definitions (not complete):
!
!     REAL    HC          HEIGHT OF CROWN (CROWN LENGTH).
!     REAL    BASE        HEIGHT AT CROWN BASE (BOTTOM IF SHAPE =
!                          CONE, PARABOLOID OR NEILOID; MID-CROWN
!                          IF SHAPE = SPHERE OR ELLIPSOID).
!     REAL    RAD         CROWN RADIUS AT BASE.
!     REAL    BOT         HEIGHT AT CROWN BOTTOM.
!     INTEGER ITOP        INDEX TO HT CLASS CONTAINING CROWN TOP.
!     INTEGER IBOT        INDEX TO HT CLASS CONTAINING CROWN BOTTOM.
!     REAL    H1          DISTANCE FROM BASE TO LOWER PLANE OF
!                          FRUSTRUM.
!     REAL    H2          THICKNESS OF FRUSTRUM.
!     REAL    R1          RADIUS OF LOWER PLANE OF FRUSTRUM.
!     REAL    R2          RADIUS OF UPPER PLANE OF FRUSTRUM.
!     REAL    Y1          LOWER LIMIT OF INTEGRATION.
!     REAL    Y2          UPPER LIMIT OF INTEGRATION.
!     REAL    CNOP(1)     HEIGHT AT LOWER LIMIT OF CROWN CLASS.
!     REAL    CNOP(2)     HEIGHT AT UPPER LIMIT OF CROWN CLASS.
!     REAL    UPLIM       HEIGHT AT UPPER PLANE OF CROWN FRUSTRUM.
!     REAL    LOWLIM      HEIGHT AT LOWER PLANE OF CROWN FRUSTRUM.
!     REAL    PAREA       PROFILE AREA OF FRUSTRUM.
!
!     REAL    MSCL        Height increment (MESH scale).
!     INTEGER DMSHAP      DM shape category
!
! Common block variables and parameters:
!
!     DMRDMX  DMCOM
!     FPM     DMCOM
!     MESH    DMCOM
!     MXHT    DMCOM
!     PIE     DMCOM
!
!     [FVS commons are not documented]
!
!********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'DMCOM.f90'

! Argument list variables.

REAL    DMTRCW
INTEGER IDMSHP

DIMENSION DMTRCW(MAXTRE)
DIMENSION IDMSHP(MAXTRE)

! Local variables.

LOGICAL DEBUG
INTEGER DMSHAP, I, J, IBOT, ITOP, IICR, J1, K
REAL    LOWLIM, MSCL, BOT, HC, BASE, H1, H2, R1, R2
REAL    RAD, CNOP, Y1, Y2, B1, Z1, Z2, CONST, FRUST, PAREA
REAL    UPLIM, RAD1, RAD2

DIMENSION CNOP(2)

! The original model seems to work in units of feet. The unit we want
! to use here is MESH (normally 2 meters). 'MSCL' performs the scale
! conversion. Multiplying by 'MSCL' gives the value in feet; dividing
! by 'MSCL' gives a value in MESH.

MSCL = FPM * MESH
!
!     CHECK FOR DEBUG.
!
CALL DBCHK(DEBUG, 'DMSUM', 4, ICYC)
!
!     RETURN IF NOTREES OPTION IN EFFECT.
!
IF (ITRN .GT. 0) GO TO 60
IF (DEBUG) WRITE (JOSTND, 9001) ITRN
9001 FORMAT (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
RETURN
60 CONTINUE

IF (DEBUG) WRITE (JOSTND, 9010)
9010 FORMAT ('      I  J     FRUST    ', &
   'TREVOL   CVOLUM    PAREA    TREAR    CAREA    BASE   UPLIM', &
   '  LOWLIM      H1      H2      Y1      Y1')

do I=1,ITRN
  do J=1,MXHT
    DMRDMX(I, J, RADIUS) = 0.
    DMRDMX(I, J, VOLUME) = 0.
  enddo
enddo
!
!     ENTER TREE LOOP.
!
DO 300 I = 1, ITRN
!
!     COMPUTE HEIGHT CLASS (MESH BAND) OF TOP OF TREE.
!
ITOP = IFIX(HT(I)/(MSCL+0.0001)) + 1
IF (ITOP .GT. MXHT) ITOP = MXHT
!
!     COMPUTE HEIGHT CLASS (MESH BAND) OF CROWN BOTTOM.
!
BOT = HT(I) - (HT(I)*ICR(I)/100.)
IBOT = IFIX (BOT/MSCL) + 1
!
!     COMPUTE DIMENSIONS USED IN HEIGHT CLASS CALCULATIONS.
!
DMSHAP = IDMSHP(I)
IICR = ICR(I)
HC = FLOAT(IICR) * HT(I) / 100.
BASE = BOT
IF ((DMSHAP.EQ.1) .OR. (DMSHAP.EQ.5)) BASE = BOT + HC / 2.
RAD = DMTRCW(I) / 2.
!
!  SET INITIAL CANOPY HEIGHTS FOR CURRENT TREE.
!
CNOP(1) = MSCL * IBOT - (2. * MSCL)
CNOP(2) = CNOP(1) + MSCL
!
!  ENTER HEIGHT CLASS LOOP WITHIN CURRENT TREE.  J1 IS USED TO
!  DECREMENT THE INDEX BY HEIGHT CLASS IF THE SHAPE IS NEILOID
!  (I.E. DO IT UPSIDE-DOWN).
!
J1 = ITOP + 1
DO 290 J = IBOT, ITOP

IF (DMSHAP .NE. 3) J1 = J
IF (DMSHAP .EQ. 3) J1 = J1 - 1
DO 230 K = 1, 2
CNOP(K) = CNOP(K) + MSCL
230 CONTINUE
UPLIM = AMIN1(HT(I), CNOP(2))
IF (J .EQ. ITOP) UPLIM = HT(I)
LOWLIM = AMAX1(BOT, CNOP(1))
H1 = LOWLIM - BASE
H2 = UPLIM - LOWLIM
R1 = (1. - H1 / HC) * RAD
R2 = (1. - (H1 + H2) / HC) * RAD
Y1 = LOWLIM - BASE
Y2 = UPLIM - BASE
IF (Y1 .GE. HC) Y1 = HC
IF (Y2 .GE. HC) Y2 = HC
IF (Y1 .LT. -HC) Y1 = -HC
IF (Y2 .LT. -HC) Y2 = -HC
!
!  BRANCH ON SHAPE TO COMPUTE FRUSTRUM AND RADIUS
!
GO TO (241, 242, 243, 244, 245), DMSHAP
!
!  DMSHAPE=1   SOLID FORM=SPHERE    PLANE FORM=CIRCLE
!
241 CONTINUE
IF (BASE.GE.LOWLIM) H1 = BASE - UPLIM
B1 = HC / 2.
IF (Y1 .GT. B1) Y1 = B1
IF (Y1 .LT. -B1) Y1 = -B1
IF (Y2 .GT. B1) Y2 = B1
IF (Y2 .LT. -B1) Y2 = -B1
Z1=B1*B1-Y1*Y1
Z2=B1*B1-Y2*Y2
IF(Z1.LT.0.0) Z1=0.0
IF(Z2.LT.0.0) Z2=0.0
CONST = 1.04720 * H2 * RAD * RAD / (HC * HC)
FRUST = CONST* (3 * HC * HC - 12*H1*H1 - 12*H1*H2 - 4*H2*H2)
PAREA = RAD/B1*(Y2*SQRT(Z2)+B1**2*ASIN(Y2/B1))- &
           RAD/B1*(Y1*SQRT(Z1)+B1**2*ASIN(Y1/B1))
GO TO 280
!
!  DMSHAPE=2   SOLID FORM=CONE   PLANE FORM=TRIANGLE
!
242 CONTINUE
CONST = 1.04720*H2
FRUST = CONST*(R1*R1+R2*R2+R1*R2)
PAREA = (R1+R2)*H2
GO TO 280
!
!  DMSHAPE=3   SOLID FORM=NEILOID   PLANE FORM=NEILOID
!
243 CONTINUE
FRUST = (PIE*RAD*RAD*H2) - &
           (HLFPIE*H2*RAD**2/HC)*(2*HC-2*H1-H2)
PAREA = RAD*((Y2+.66667*HC*(1-Y2/HC)**1.5) - &
                (Y1+.66667*HC*(1-Y1/HC)**1.5))
GO TO 280
!
!  DMSHAPE=4   SOLID FORM=PARABOLOID   PLANE FORM=PARABOLA
!
244 CONTINUE
FRUST = (HLFPIE*H2*RAD**2/HC)*(2*HC-2*H1-H2)
PAREA = -RAD*1.33333*HC*((1-(Y2/HC))**1.5 - (1-(Y1/HC))**1.5)
GO TO 280
!
!  DMSHAPE=5   SOLID FORM=ELLIPSOID   PLANE FORM=ELLIPSE
!
245 CONTINUE
IF (BASE.GE.LOWLIM) H1 = BASE - UPLIM
B1 = HC/2.
IF (Y1 .GT. B1) Y1 = B1
IF (Y1 .LT. -B1) Y1 = -B1
IF (Y2 .GT. B1) Y2 = B1
IF (Y2 .LT. -B1) Y2 = -B1
Z1=B1*B1-Y1*Y1
Z2=B1*B1-Y2*Y2
IF(Z1.LT.0.0) Z1=0.0
IF(Z2.LT.0.0) Z2=0.0
CONST = 1.04720*H2*RAD*RAD/(HC*HC)
FRUST = CONST*(3*HC*HC - 12*H1*H1 - 12*H1*H2 - 4*H2*H2)
PAREA = RAD/B1*(Y2*SQRT(Z2)+B1**2*ASIN(Y2/B1))- &
           RAD/B1*(Y1*SQRT(Z1)+B1**2*ASIN(Y1/B1))
280 CONTINUE

!  Fill the RADIUS and VOLUME parts of 'DMRDMX()'.
!   J - is Height Class
!   I - is a Tree Record
!  RADIUS and VOLUME measures are in MESH units.

RAD1 = PAREA / (H2 * 2)
RAD2 = SQRT(FRUST / (PIE * H2))

!     if (j .gt. 4) then
!        jjj=0
!      endif

DMRDMX(I, J, RADIUS) = RAD2 / MSCL
DMRDMX(I, J, VOLUME) = FRUST / MSCL**3
!
!  DEBUG OUTPUT
!
!      WRITE(*, 4000) I, IDMSHP(I), RAD1, RAD2
! 4000 FORMAT (I4, I4, 2F10.1)

IF (DEBUG) WRITE (JOSTND, 7000) I, J1, UPLIM, LOWLIM, H1, H2, &
    BASE
7000 FORMAT (I6, I3, F7.2, 5F10.2, 7F8.2)
!
!  END LAYER LOOP
!
290 CONTINUE
!
!  END TREE LOOP
!
300 CONTINUE
RETURN
END

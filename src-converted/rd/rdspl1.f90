SUBROUTINE RDSPL1(RCROSS,STNEW,IDI,DIAM,RTD)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE TO CREATE NEW STUMPS AND DISEASE CENTERS FROM
!  SPORE INFECTED STUMPS THAT HAVE CHANGED DISEASE TYPE
!
!  CALLED BY :
!     RDSPOR  [ROOT DISEASE]
!
!  CALLS     :
!     RDRANN  (FUNCTION)  [ROOT DISEASE]
!
!  PARAMETERS :
!     RCROSS -
!     STNEW  -
!     IDI    -
!     DIAM   -
!     RTD    -
!
!  Revision History :
!   01/01/91 - Last revision date.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'RDPARM.f90'

INCLUDE 'CONTRL.f90'

INCLUDE 'RDCOM.f90'

INCLUDE 'RDADD.f90'
!
!OMMONS
!
INTEGER  II, IDI, JJ, NCROSS
REAL     DIAM, R, R1, R2, R3, RCROSS, RDRANN, REMCRO, RMX, &
            RTD, STNEW, XC, YC, XDIF, YDIF, DIS

STNEW = 0.0
IF (DIAM .LT. SPDBH(IDI)) GOTO 300

DIMEN = SQRT(SAREA) * 208.7

!
!     GET STUMPS THAT CHANGE DISEASE TYPE
!
NCROSS = IFIX(RCROSS)
REMCRO = RCROSS - NCROSS
R = RDRANN(0)
IF (R .LT. REMCRO) NCROSS = NCROSS + 1

IF (NCROSS .LT. 1) RETURN

DO 350 II=1,NCROSS
!
!        CHECK TO SEE IF STUMP IS INSIDE CENTERS
!
   XC = RDRANN(0) * DIMEN
   YC = RDRANN(0) * DIMEN
   JJ = 0

400    CONTINUE
   JJ = JJ + 1
   IF (JJ .GT. NCENTS(IDI)) GOTO 600

   XDIF = PCENTS(IDI,JJ,1) - XC
   YDIF = PCENTS(IDI,JJ,2) - YC
   DIS = SQRT((XDIF * XDIF) + (YDIF * YDIF))

   IF (DIS .GT. PCENTS(IDI,JJ,3)) GOTO 400

!
!        STUMP IS INSIDE OF OTHER TYPE'S CENTERS.
!        SO IGNORE IT
!
   GOTO 350

600    CONTINUE
!
!        STUMP IS OUTSIDE OF OTHER TYPE'S CENTERS
!        SO IT BECOMES A NEW CENTER

   STNEW = STNEW + 1.0

!        SO ADD IT TO THE PATCH LIST.
!
!        Perhaps IDI should be ID2 (the actual disease type instead of the other) --Sarah

   IF (NCENTS(IDI) .GE. 100) GOTO 550

   NCENTS(IDI) = NCENTS(IDI) + 1
   ICENSP(IDI,NCENTS(IDI)) = 1
   PCENTS(IDI,NCENTS(IDI),1) = XC
   PCENTS(IDI,NCENTS(IDI),2) = YC

!
!        PATCH SIZE (RADIUS) IS 50% OF STUMP ROOT DIAMETER
!
   PCENTS(IDI,NCENTS(IDI),3) = 0.25 * RTD

   GOTO 350

550    CONTINUE
!
!        ADD PATCH AREA TO LAST PATCH
!
   R1 = PCENTS(IDI,NCENTS(IDI),3)
   R2 = (0.25 * RTD)
   R3 = SQRT((R1 * R1) + (R2 * R2))

!        Should the following line be .5 * DIMEN instead (half of one side of a square stand)?
!           (If so, also change line in RDSPL2) -- Sarah

   RMX = 2*DIMEN
   IF (R3 .LT. RMX) PCENTS(IDI,NCENTS(IDI),3) = R3
350 CONTINUE

300 CONTINUE

RETURN
END

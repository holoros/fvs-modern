SUBROUTINE FMMOIS (FMOIS, MOIS)
IMPLICIT NONE
!----------
! FIRE-WS $Id$
!----------
!
!     CALLED FROM: FMBURN
!----------------------------------------------------------------------
!  PURPOSE:
!     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS
!----------------------------------------------------------------------
!
!  CALL LIST DEFINITIONS:
!     FMOIS:   MOISTURE MODEL NUMBER
!     MOIS:    MOISTURE VALUES
!
!  LOCAL VARIABLE DEFINITIONS:
!     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
!           (2,*):LIVE STUFF
!
!  COMMON BLOCK VARIABLES AND PARAMETERS:
!

!.... PARAMETER STATEMENTS.

!.... PARAMETER INCLUDE FILES.

!.... COMMON INCLUDE FILES.
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'


!     LOCAL VARIABLE DECLARATIONS

INTEGER  FMOIS
REAL     MOIS(2,5)
LOGICAL  DEBUG
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMMOIS',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC
7 FORMAT(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)

!     BEGIN ROUTINE

IF (FMOIS .EQ. 0) THEN
   RETURN

ELSEIF (FMOIS .EQ. 1) THEN

!         "very low moisture" / wildfire

    MOIS(1,1) = 0.03
    MOIS(1,2) = 0.04
    MOIS(1,3) = 0.05
    MOIS(1,4) = 0.10
    MOIS(1,5) = 0.15
    MOIS(2,1) = 0.70 ! live woody
    MOIS(2,2) = 0.70 ! live herb

ELSEIF (FMOIS .EQ. 2) THEN

!         "low moisture"

    MOIS(1,1) = 0.08
    MOIS(1,2) = 0.08
    MOIS(1,3) = 0.10
    MOIS(1,4) = 0.15
    MOIS(1,5) = 0.50
    MOIS(2,1) = 1.10
    MOIS(2,2) = 1.10

ELSEIF (FMOIS .EQ. 3) THEN

!         "moderate moisture"

    MOIS(1,1) = 0.12
    MOIS(1,2) = 0.12
    MOIS(1,3) = 0.14
    MOIS(1,4) = 0.25
    MOIS(1,5) = 1.25
    MOIS(2,1) = 1.50
    MOIS(2,2) = 1.50

ELSEIF (FMOIS .EQ. 4) THEN

!         "high moisture"

    MOIS(1,1) = 0.12
    MOIS(1,2) = 0.12
    MOIS(1,3) = 0.14
    MOIS(1,4) = 0.25
    MOIS(1,5) = 1.25
    MOIS(2,1) = 1.50
    MOIS(2,2) = 1.50

ENDIF

RETURN
END


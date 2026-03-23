SUBROUTINE FMMOIS (FMOIS, MOIS)
IMPLICIT NONE
!----------
! FIRE-NI $Id: fmmois.f 0000 2018-02-14 00:00:00Z gedixon $
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
LOGICAL DEBUG
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

    MOIS(1,1) = .04    ! 1hr, 0-.25"
    MOIS(1,2) = .04    ! 10hr, .25-1"
    MOIS(1,3) = .05    ! 100hr, 1-3"
    MOIS(1,4) = .10    ! 3+
    MOIS(1,5) = .15    ! Duff
    MOIS(2,1) = .7     ! Live woody
    MOIS(2,2) = .7     ! Live herb

ELSEIF (FMOIS .EQ. 2) THEN

!         "low moisture"

    MOIS(1,1) = .08
    MOIS(1,2) = .08
    MOIS(1,3) = .1
    MOIS(1,4) = .15
    MOIS(1,5) = .5
    MOIS(2,1) = 1.1
    MOIS(2,2) = 1.1

ELSEIF (FMOIS .EQ. 3) THEN

!         "moderate moisture"

    MOIS(1,1) = .12
    MOIS(1,2) = .12
    MOIS(1,3) = .14
    MOIS(1,4) = .25
    MOIS(1,5) = 1.25
    MOIS(2,1) = 1.5
    MOIS(2,2) = 1.5

ELSEIF (FMOIS .EQ. 4) THEN

!         "high moisture"

    MOIS(1,1) = .16
    MOIS(1,2) = .16
    MOIS(1,3) = .18
    MOIS(1,4) = .50
    MOIS(1,5) = 2.0
    MOIS(2,1) = 1.5
    MOIS(2,2) = 1.5

ENDIF

RETURN
END


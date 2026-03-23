SUBROUTINE FMMOIS (FMOIS, MOIS)
IMPLICIT NONE
!----------
! FIRE-SN $Id$
!----------
!  CALLED FROM: FMBURN, FMPOFL, FMIN, FMTRET
!----------
!  PURPOSE:
!     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS.
!     THE MOISTURE VALUES USED FOR SEVERE AND MODERATE POTENTIAL FIRES
!     (CATEGORIES 1 $ 3) WERE PROVIDED BY BENNIE TERRELL AND GREGG
!     VICKERS.  CATEGORY 2 IS APPROX. HALF WAY BETWEEN CAT 1 & 3. CAT 4
!     IS THE SAME AS IN NI.
!     DUFF MOISTURE VALUES ARE THE DEFAULTS IN FOFEM.
!----------
!  CALL LIST DEFINITIONS:
!     FMOIS:   MOISTURE MODEL NUMBER
!     MOIS:    MOISTURE VALUES
!
!  LOCAL VARIABLE DEFINITIONS:
!     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
!           (2,*):LIVE STUFF
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
!OMMONS
!----------
!  LOCAL VARIABLE DECLARATIONS
!----------
INTEGER  FMOIS
REAL     MOIS(2,5)
LOGICAL DEBUG
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMMOIS',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC
7 FORMAT(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)
!----------
!  BEGIN ROUTINE
!----------
IF (FMOIS .EQ. 0) THEN
   RETURN
!
ELSEIF (FMOIS .EQ. 1) THEN
!----------
!  VERY DRY
!----------
    MOIS(1,1) = .05    ! 1hr, 0-.25"
    MOIS(1,2) = .07    ! 10hr, .25-1"
    MOIS(1,3) = .12    ! 100hr, 1-3"
    MOIS(1,4) = .17    ! 3+
    MOIS(1,5) = .40    ! Duff
    MOIS(2,1) = .55    ! Live woody
    MOIS(2,2) = .55    ! Live herb
!
ELSEIF (FMOIS .EQ. 2) THEN
!----------
!  DRY
!----------
    MOIS(1,1) = .06
    MOIS(1,2) = .08
    MOIS(1,3) = .13
    MOIS(1,4) = .18
    MOIS(1,5) = .75
    MOIS(2,1) = .80
    MOIS(2,2) = .80
!
ELSEIF (FMOIS .EQ. 3) THEN
!----------
!  WET
!----------
    MOIS(1,1) = .07
    MOIS(1,2) = .09
    MOIS(1,3) = .14
    MOIS(1,4) = .20
    MOIS(1,5) = 1.0
    MOIS(2,1) = 1.0
    MOIS(2,2) = 1.0
!
ELSEIF (FMOIS .EQ. 4) THEN
!----------
!  VERY WET
!----------
    MOIS(1,1) = .16
    MOIS(1,2) = .16
    MOIS(1,3) = .18
    MOIS(1,4) = .50
    MOIS(1,5) = 1.75
    MOIS(2,1) = 1.5
    MOIS(2,2) = 1.5
!
ENDIF
!
RETURN
END


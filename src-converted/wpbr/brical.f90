SUBROUTINE BRICAL
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRIBA calculates the Rust Index based on exposure time.
!
!  The functions used to determine the rust index from exposure time were
!  provided by Geral McDonald, Intermountain Research Station, Moscow,
!  ID.
!
!  Rust Index (RI) is calculated once per cycle. The function has four
!  parameters and utilizes stand age as the exposure time variable.
!  The parameters are:
!     MINRI - minumum Rust Index
!     MAXRI - maximum Rust Index
!     PKAGE - stand age (exposure time) when Rust Index peaks
!     RISHP - defines shape of curve about the peak
!
!  Called from:
!     BRTREG depending on RI assignment method.
!
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'CONTRL.f90'

!.... Local variable declarations.

REAL BRI
INTEGER NYEAR, NAGE
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRICAL',6,ICYC)
IF(DEBUG) WRITE(JOSTND,10) ICYC
10 FORMAT('Entering subroutine BRICAL: cycle = ',I2)

!.... Get current year and age.

NYEAR = IY(ICYC+1)
NAGE = IAGE + NYEAR - IY(1)

IF (RIMETH.EQ.3) THEN
!....    Calculate Rust Index with Gaussian equation.
!....    This equation provides a normal curve around the peak age.
   BRI = MINRI + MAXRI * EXP(-0.5 * ((NAGE - PKAGE)/PKSHP)**2)

ELSEIF (RIMETH.EQ.4) THEN
!....    Calculate Rust Index with log function.
!....    This equation provides a skewed curve around the peak age
!....    so that a greater number of cankers can be attained early.
   BRI = MINRI + MAXRI * EXP(-0.5*(LOG(NAGE - PKAGE)/PKSHP)**2)

ENDIF

RIDEF=BRI

IF(DEBUG) WRITE(JOSTND,60) RIDEF,RIMETH,NAGE,ICYC
60 FORMAT(27X,'RIDEF = ',F10.8,' RIMETH=',I1,' NAGE=',I4,/, &
          ' Leaving subroutine BRICAL: cycle = ',I2)
RETURN
END

SUBROUTINE BRPR
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRPR performs the following functions:
!     1) calls BRCSTA to assign canker status when a compression
!        has been performed
!     2) calls BRTSTA to calculate the status of a tree
!     3) calls BRSTAT to calculate Blister Rust statistics
!     4) calls BRSUM to load the summary information
!     5) calls BRCOUT to write detailed canker output
!     6) calls BRTOUT to write detailed tree output
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
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

LOGICAL BRGO, DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRPR',4,ICYC)
IF(DEBUG) WRITE(JOSTND,10) ICYC
10 FORMAT('Entering subroutine BRPR: cycle = ',I2)

!.... See if Blister Rust model is being used in this simulation.
!.... If not, then return.

CALL BRATV(BRGO)
IF(.NOT.BRGO) GO TO 90

!.... When a compression has been performed, canker statuses must be
!.... assigned.

IF(BRCMP) THEN
   CALL BRCSTA
   BRCMP=.FALSE.
ENDIF

!.... Call BRTSTA to calcualte status for tree based on the tree's
!.... worst canker.

CALL BRTSTA

!.... Call BRSTAT to calculate statistics. BRSTAT is called from BRSETP
!.... during the initialization phase.

IF(.NOT.LSTART) CALL BRSTAT

!.... Call BRSUM to load summary statistics arrays

CALL BRSUM

!.... Call BRCOUT to print list of cankers of requested.

IF(BRCL) CALL BRCOUT

!.... Call BRTOUT to print list of trees if requested.

IF(BRTL) CALL BRTOUT

!.... Common return.

90 CONTINUE
IF(DEBUG) WRITE(JOSTND,100) ICYC
100 FORMAT('Leaving subroutine BRPR: cycle = ',I2)
RETURN
END

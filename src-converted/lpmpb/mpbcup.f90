SUBROUTINE MPBCUP
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     INTERFACING PROGRAM TO CALL EITHER MPBDRV OR COLDRV.
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'MPBCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
!
!OMMONS
!
!************************* EXECUTION BEGINS ***************************
!
IF (LPOPDY) THEN
   CALL MPBDRV
ELSE
   CALL COLDRV
ENDIF
RETURN
END

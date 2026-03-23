SUBROUTINE BRATV(L)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!**********************************************************************
!  **BRATV        DATE OF LAST REVISION:  06/05/2014
!----------------------------------------------------------------------
!  Purpose:
!  BRATV returns L=.TRUE. to indicate that the Blister Rust model is
!  available from the current link and is actually being called.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Common include files

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations

LOGICAL L

L=BRYES

!.... Common return

RETURN
END

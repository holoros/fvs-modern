SUBROUTINE MISGET(ITREE,IDMR)
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Gets dwarf mistletoe rating (stored in array IMIST) for the
!  current tree record.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ITREE:  (I) Current tree record number.
!     IDMR:   (O) Dwarf mistletoe rating for current tree record.
!
!  Common block variables and parameters:
!     IMIST:  From MISCOM; array containing tree record DMR's.
!
!**********************************************************************
IMPLICIT NONE

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'MISCOM.f90'

INTEGER IDMR,ITREE

!.... Get dwarf mistletoe rating for current tree.

IDMR=IMIST(ITREE)

RETURN
END

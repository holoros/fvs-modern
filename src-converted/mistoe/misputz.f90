SUBROUTINE MISPUTZ(ITREE,IDMR)
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Sets dwarf mistletoe rating (stored in array IMIST) for the
!  current tree record to the value passed in IDMR, and zeros
!  elements in 3 DM arrays.
!     Entry point MISPUT omits zeroing DM arrays, and is the same
!  as the previous (before Sept. 1993) MISPUT code.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ITREE:  (I) Current tree record number.
!     IDMR:   (I) Mistletoe rating to put into IMIST for current tree.
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

!.... Variable declarations.

INTEGER IDMR,ITREE

!.... Set zeros for initial values of DM variables.
!.... At this time, MISPUTZ and MISPUT are equivalent.

ENTRY MISPUT(ITREE,IDMR)

!.... Check for valid dwarf mistletoe rating.

IF(IDMR.GE.0.AND.IDMR.LE.6) THEN

!....    Put value into IMIST for current tree record number.

   IMIST(ITREE)=IDMR

ELSE

!....    Print error message and return.

   PRINT*,' *MISPUT* Invalid dwarf mistletoe rating (', &
             IDMR, ') input for tree ', ITREE
ENDIF

RETURN
END

SUBROUTINE DMMDMR(New, Old)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMMDMR -- NISI  Date of last revision: April 9 1994
!----------------------------------------------------------------------
! Purpose:
!   In order to keep the interim model spread equations distinct from
! the NISI, a different DMR array is used for the NISI. This routine
! copies the NISI value into the Interim model array, so that the
! Interim model routines can process the results in a consistent way.
!----------------------------------------------------------------------
!
! Called by:
!
!     MISTOE
!
! Other routines called:
!
!     [none]
!
! Argument list definitions:
!
!     INTEGER New     (I) DMR computed by the NISI
!     INTEGER Old     (O) DMR array used by the Interim model
!
! Local variable definitions:
!
!     INTEGER i           Loop counter for the treelist records
!
! Common block variables and parameters:
!
!     MAXTRE  PRGPRM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'

INTEGER New
INTEGER Old

DIMENSION New(MAXTRE)
DIMENSION Old(MAXTRE)

INTEGER i

DO 100 i = 1, ITRN
   Old(i) = New(i)
100 CONTINUE

RETURN
END

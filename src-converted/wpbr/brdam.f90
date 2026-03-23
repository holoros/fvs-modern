SUBROUTINE BRDAM(II,ICODES)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!   This subroutine processes damage codes to determine whether
!   the tree in question is infected with Blister Rust.
!   The Region 1 code for White Pine Blister Rust is 36.
!   ************
!   *** NOTE ***
!   ************
!   At this point nothing is done with this information from the
!   treelist damage codes.
!----------------------------------------------------------------------
!
!   Parameters:
!      II     - current tree ID
!      ICODES - integer array (size 6) holding 3 pairs of damage
!               codes and severity ratings
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  14-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Changed literal value for rust damage code to variable IBRDAM.
!
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

INTEGER ICODES(6), II, J, ISEVRT

!.... Process damage codes.

DO 100 J=1,5,2
   IF(ICODES(J).EQ.IBRDAM) THEN

!....       Get the severity rating code.

      ISEVRT=ICODES(J+1)
      GO TO 110
   ENDIF
100 CONTINUE

!.... Common return.

110 CONTINUE
RETURN
END

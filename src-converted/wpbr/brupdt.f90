SUBROUTINE BRUPDT
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Updates tree ground diameter and converts it to centimeters
!  for each cycle in the Blister Rust model.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  15-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 45) and species temp index variable (I3)
!     are new.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM.
!
!**********************************************************************

!.... Common include files

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'

!.... Local variables.
INTEGER I1, I2, I3, J, K

!.... If no tree records then return.

IF(ITRN.EQ.0) GO TO 50

!.... Process the host pines, if none then return.
!.... Start species loop.

DO 45 I3 = 1, MAXSP

IF (BRSPM(I3) .EQ. 0) GO TO 45

I1=ISCT(I3,1)
IF(I1.EQ.0) GO TO 45
I2=ISCT(I3,2)
DO 22 J=I1,I2
   K=IND1(J)

!....    Update the ground diameter of this tree.

   BRGD(K)=BRGD(K)+DG(K)*2.54
22 CONTINUE

!.... End species loop.
45 CONTINUE

!.... Common return.

50 CONTINUE
RETURN
END

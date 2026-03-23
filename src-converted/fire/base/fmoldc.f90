SUBROUTINE FMOLDC
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!
!     CALLED FROM: FMMAIN
!
!  Purpose:
!     This subroutine records some crown size information for use in
!     calculating the amount of litterfall due to crown lifting in the
!     next cycle.  The proportion of total crown weight that will fall
!     in the next cycle is equal to ratio of the increase in the bottom
!     height of the crown to the total length of the crown (i.e., [new
!     bottom - old bottom] / old length).  The material to fall will be
!     added to down debris at a constant rate throughout the next FVS
!     cycle.  (In 'reality', some of this material would not fall until
!     later cycles, and some of the slow-falling material from earlier
!     cycles would fall during the current cycle.  The two effects
!     partially cancel each other out, so simply falling all of the
!     dead material derived from the current crown lift in the current
!     cycle is not grossly inaccurate).
!
!
!  Local variable definitions:
!
!  Common block variables and parameters:
!
!

!.... Parameter statements.

!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'FMCOM.f90'

!.... Variable declarations.

INTEGER I, J

!.... Begin routine.

!     Loop through the tree list, recording the current height, crown
!     length, and crown weights of each record.

DO I=1,ITRN
   OLDHT(I) = HT(I)
   OLDCRL(I) = HT(I) * (FLOAT(FMICR(I)) / 100.0)
   DO J=0,5
      OLDCRW(I,J) = CROWNW(I,J)
   ENDDO
ENDDO
RETURN
END


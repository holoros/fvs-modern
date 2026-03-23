REAL FUNCTION RDRANN(J)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  THIS FUNCTION RETURNS A UNIFORM RANDOM NUMBER TO THE
!  ROOT DISEASE MODEL CALLING ROUTINE.
!
!  CALLED BY :
!     RDCENT  [ROOT DISEASE]
!     RDCLOC  [ROOT DISEASE]
!     RDINSD  [ROOT DISEASE]
!     RDSPRD  [ROOT DISEASE]
!     RDSPL1  [ROOT DISEASE]
!     RDSPL2  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  PARAMETERS :
!     J      -
!
!  Revision History :
!   11/06/89 - Last revision date.
!   09/02/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

INCLUDE 'RDADD.f90'
!
!OMMONS
!
INTEGER J
INTEGER IDANUW
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = J
!
!
S1 = DMOD(16807D0*S0,2147483647D0)
RDRANN = REAL(S1 / 2147483648D0)
S0 = S1
!
RETURN
END

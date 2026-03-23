SUBROUTINE ESPREP (PNONE,PMECH,PBURN)
IMPLICIT NONE
!----------
! AK $Id$
!----------
!     PREDICT DEFAULT SITE PREP PROBABILITIES.
!
!----------
!  VARIABLE DECLARATIONS:
!----------
!
REAL PBURN,PMECH,PNONE
!
!----------
!     PROB(NO SITE PREP)
!----------
PNONE = 1.0
!----------
!     PROB(MECH SITE PREP)
!----------
PMECH = 0.0
!----------
!     PROB(BURN SITE PREP)
!----------
PBURN = 0.0
!
RETURN
END

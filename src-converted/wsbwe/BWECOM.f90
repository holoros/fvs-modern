!ODE SEGMENT BWECOM
! ----------
! WSBWE $Id$
! ----------
!
!  --> VARIABLES FOR THE BUDWORM DAMAGE MODEL
!
! Revision History:
!   03-MAY-00 Lance David (FHTET)
!     Reduced size of array TABLE from 111 to 25, the actual number
!     of Defoliation Model keywords.
!   12-MAY-00  Lance David (FHTET)
!     Change variable ISIZE to TABSZ because ISIZE was used
!     elsewhere in the model for size classes. The was a problem for serial
!     processing of multiple stands, second set of budworm keywords failed.
!   16-AUG-01 Lance R. David (FHTET)
!     Added DSEEDD and DSEEDR variables for Damage model random number seed.
!   10-AUG-2006 Lance R. David (FHTET)
!     Reorganizing and removing obsolete variable in preparation for
!     linkage with PPE.
!   01-28-09 - Lance R. David (FMSC)
!      Common blocks added for character and double precision variables
!      that were in common BWECOM due to compiler complaint (mixed
!      variable types in common) reported by Don Robinson (ESSA).
!   07/09/10 Lance R. David (FMSC)
!      Declared BWFINT for IMPLICIT NONE addition to subroutines.
!---------------------------------------------------------------------------

CHARACTER*8 TABLE(25)
INTEGER  IBWSPM(MAXSP),IBWYR1,IBWYR2,ICUMYR,IPRBYR,ITODO, &
            IYRCUR,JOWSBW,NCROWN,NCUMYR,NHOSTS,NTODO,TABSZ
REAL  BWFINT,DSEEDR
DOUBLE PRECISION DSEEDD
LOGICAL LBWDAM,LBWPDM,LCALBW,LDEFOL,LTOPK

COMMON /BWECOM/ BWFINT,DSEEDR,IBWSPM,IBWYR1,IBWYR2, &
      ICUMYR,IPRBYR,ITODO,IYRCUR,JOWSBW,LBWDAM,LBWPDM,LCALBW, &
      LDEFOL,LTOPK,NCROWN,NCUMYR,NHOSTS,NTODO,TABSZ

COMMON /BWECOMC/ TABLE

COMMON /BWECOMD/ DSEEDD
!
!   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
!   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
!   LTOPK =  TRUE IF TOP-KILL IS TO BE CALCULATED & APPLIED
!   LBWDAM = TRUE IF "DAMAGE" OUTPUT IS TO BE PRINTED
!   LBWPDM = TRUE IF "PERDAM" OUTPUT IS TO BE PRINTED
!
!   IBWSPM =  [set to 11 for Blue Mountains variant]
!   TABLE =  LOOKUP TABLE FOR BUDWORM KEYWORDS
!   TABSZ =  NUMBER OF BUDWORM KEYWORDS (dimension of array TABLE)
!   NHOSTS = NUMBER OF HOST SPECIES
!   NCROWN = NUMBER OF CROWN LEVELS
!   JOWSBW = FILE NUMBER FOR BUDWORM OUTPUT
!   IBWYR1 = FIRST YEAR IN BUDWORM SIMULATION
!   IBWYR2 = LAST YEAR IN BUDWORM SIMULATION
!   IYRCUR = CURRENT YEAR
!   IPRBYR =
!   ICUMYR =
!   NCUMYR =
!   ITODO  =
!   NTODO  =
!   BWFINT = NUMBER OF YEARS IN THE BUDWORM PORTION OF A CYCLE
!   DSEEDD = RANDOM NUMBER SEED VALUE FOR DAMAGE MODEL -DOUBLE PRECISION-
!   DSEEDR = RANDOM NUMBER SEED VALUE FOR DAMAGE MODEL -REAL-
! --------END SEGMENT

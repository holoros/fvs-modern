SUBROUTINE RDSADD(I,TP)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  ADD TO THE STUMP LIST.
!
!  CALLED BY :
!     RDSTR   [ROOT DISEASE]
!
!  CALLS     :
!     RDSSIZ  (SUBROUTINE)  [ROOT DISEASE]
!     RDSTP   (SUBROUTINE)  [ROOT DISEASE]
!     RDDBUG  (SUBROUTINE)  [ROOT DISEASE]
!
!  PARAMETERS :
!     I      - (I ) Tree record number.
!     TP     - (I ) Proportion of trees in current tree record not
!                   cut by (Prognosis) subroutine CUTS.
!
!  COMMON BLOCK VARIABLES :
!
!  LOCAL VARIABLES :
!     DEN
!     JJ
!
!  Revision History :
!   04/07/93 - Last revision date.
!   09/02/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... PARAMETER INCLUDE FILES
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
!
!.... COMMON INCLUDE FILES
!
INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'RDADD.f90'
INCLUDE 'ARRAYS.f90'
!
!.... Local variables
!
INTEGER I, ISL, JJ
REAL TP, DEN

DEN = PROBIT(I) * (1.0 - TP)
JJ = ISP(I)

CALL RDSSIZ (JJ,DBH(I),STCUT,ISL,ISPS,IRTSPC)
CALL RDSTP  (ISL,JJ,DEN,DBH(I),ROOTL(I))

RETURN
END

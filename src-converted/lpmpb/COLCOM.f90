!ODE SEGMENT COLCOM
!----------
! LPMPB $Id$
!----------
!
! Revision History
!   06/09/89 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Declared variables for Addition of IMPLICIT NONE to subroutines.
!----------
REAL CURRMR(10),DEAD(10,10),DEADCL(10),GREEN(10,10),GREINF(10), &
        PRNOIN(10),START(10),STOTAL,TDEAD(10),TGREEN(10), &
        ZINMOR(10)
LOGICAL LCURMR, LDAM
COMMON /COLCOM/ CURRMR,DEAD,DEADCL,GREEN,GREINF,LCURMR,LDAM, &
          PRNOIN,START,STOTAL,TDEAD,TGREEN,ZINMOR
!
!-----END SEGMENT

!ODE SEGMENT PT
!----------
! LPMPB $Id$
!----------
!
! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Declared variables for Addition of IMPLICIT NONE to subroutines.
!----------
CHARACTER*1 IPTLET(5,10)
INTEGER IPTVAR(5),IPTSPL(5),IPTCOD(5,10)
REAL    PTMAX(5,10),PTMIN(5,10),PTU(5,10),PTL(5,10)

COMMON/PTCHR/ IPTLET
COMMON/PT/ PTMAX,PTMIN,PTU,PTL,IPTVAR,IPTSPL,IPTCOD
!
!-----END SEGMENT

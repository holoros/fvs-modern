!ODE SEGMENT MULTCM
!----------
! COMMON $Id$
!----------
!
REAL   XDMULT(MAXSP),XHMULT(MAXSP),XMDIA1(MAXSP),XMDIA2(MAXSP), &
          XMMULT(MAXSP),XRDMLT(MAXSP),XRHMLT(MAXSP)
!
COMMON /MULTCM/ XDMULT,XHMULT,XMDIA1,XMDIA2,XMMULT,XRDMLT,XRHMLT
!
!----------
!  VARIABLE DEFINITIONS:
!----------
!  Species specifice multipers:
!  XDMULT -- diameter growth
!  XHMULT -- height growth
!  XMDIA1 -- lower diameter limit for mortality multipier
!  XMDIA2 -- upper diameter limit for mortality multipier
!  XMMULT -- mortality
!  XRDMLT -- small tree diameter growth
!  XRHMLT -- small tree height growth
!
!-----END SEGMENT

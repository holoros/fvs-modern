SUBROUTINE DMMTRX
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!--------------------------------------------------------------------
! **DMMTRX -- NISI Date of last revision: April 9, 1994
!--------------------------------------------------------------------
! Purpose:
!   This routine controls the three routines taken from the COVER
! model. Together, they estimate the radius and volume of treelist
! records. This routine calls the COVER model routines that compute
! the crown geometry needed to estimate opacity effect and spread and
! intensification. The basic information is crown radius (in MESH
! units; normally 2 meters) and volume in MESH height slices. The
! method is based on Moeur's COVER model.
!--------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     DMCW
!     DMSHAP
!     DMSUM
!
!     Note that DMCW and DMSHAP have modified *file* names, since
!     their actual contents vary according to the variant for which
!     they have modified. For example, the Northern Idaho variant
!     has DMCWNI and DMSHAPNI.
!
! Argument list definitions:
!
!     [none]
!
! Local variable definitions:
!
!     INTEGER   IDMSHP  Tree crown shape category
!     REAL      DMTRCW   Estimated tree crown diameter (feet)
!
! Common block variables and parameters:
!
!     MAXTRE    PRGPRM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'

! Local variables.

REAL    DMTRCW(MAXTRE)
INTEGER IDMSHP(MAXTRE)

! Find the tree crown width (feet) for every element of the treelist.
! Calling argument array 'DMTRCW' is filled on return.

CALL DMCW(DMTRCW)

! Find the crown shape category of each element of the treelist. The
! calling argument array 'IDMSHP' is filled on return.

CALL DMSHAP(DMTRCW, IDMSHP)

! Compute the radius and volume of each element of the treelist. This
! is estimated for each MESH-heigh band of each element, and computes
! in units of MESH Radius and MESH**3 volume. MESH is normally 2
! meters.

CALL DMSUM(DMTRCW, IDMSHP)

RETURN
END

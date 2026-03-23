SUBROUTINE DMBSHD (iCS, iz, ix, ivLen, ivCnt)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
!  **DMBSHD -- NISI  Date of last revision: April 7 1994
!----------------------------------------------------------------------
! Purpose:
!   The source tree produces a field of trajectories emanating from
! the position of an infection. These trajectories are recorded
! using a target tree reference frame in which the infection
! originates from (X,Z) cell (1,10). The X dimension increases going
! away from the tree and the Z dimension increases from the ground
! up. After adjusting for the different reference frame of the
! target, each MESH band of the  target exists in some position in
! the reference frame of the source. For example, this could be in
! (5,2), which would mean that a portion of the crown of the target
! is 5 MESH units lateral to the piece of source producing the
! infection, and 8 units below. The job of this routine is to provide
! a list of all the trajectories that pass through (5,2), and a
! record of what (X,Z) positions they passed through to get there.
! These allow the intensification, between-tree loss and spread to
! be computed. This is done by using the 'ShdPtr()' to see if any
! paths went through this (X,Z). If so, then the 'iCS()' matrix is
! loaded all the path and path-weight information, to be used by the
! calling routine. The use of 0:28 in the 2nd position of the 'iCS()'
! matrix simply reflects an empirical determination of the longest
! path ever observed when MESH was 1 meter. Most are shorter, and
! since MESH is usually 2 meters, much shorter.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMADLV
!
! Other routines called:
!
!     [none]
!
! Argument list definitions:
!
!     INTEGER    iCS   (O)  The path information, consisting of a
!                            triply indexed array:
!                            Index 1: for each unique path.
!                            Index 2: for each gridcell that the
!                                      path passes through.
!                            Index 3: for the (X,Z) location of
!                                      each cell.
!     INTEGER    iz    (I)  The z-position (source tree's reference
!                            frame) of the target crown.
!     INTEGER    ix    (I)  The x-position (etc.) of the target crown
!     INTEGER    ivLen (O)  The length of each path passing through
!                            (iz,ix); this corresponds to the relevant
!                            length of iCS(-,2,-)
!     INTEGER    ivCnt (O)  The number of paths passing through (iz,ix)
!                            This corresponds to the relevant length
!                            of iCS(1,-,-)
!
! Local variable definitions:
!
!      INTEGER   i          Loop counter for the each unique path
!      INTEGER   j          Loop counter for the length of each path
!      INTEGER   StrtVl     Pointer to position of first cell in path
!      INTEGER   EndVal     Pointer to position of last cell in path
!      INTEGER*4 Ptr        Index of position in encoded paths
!
! Common block variables and parameters:
!
!     MXTRAJ     DMCOM
!     TOP1       DMCOM
!     XX         DMCOM
!     ZZ         DMCOM
!     ShdPtr     DMCOM
!     Shd1       DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER    iCS
INTEGER    iz
INTEGER    ix
INTEGER    ivLen
INTEGER    ivCnt

DIMENSION  iCS(MXTRAJ, 0:28, 2)
DIMENSION  iVLen(MXTRAJ)

! Local variables

INTEGER    i, j
INTEGER    StrtVl, EndVal
INTEGER*4  Ptr

! If no trajectories occur in this grid cell, 'Ptr' is zero.

Ptr = ShdPtr(iz, ix)

IF (Ptr .EQ. 0) THEN
   ivCnt = 0

ELSE IF ((Ptr .GE. 1) .AND. (Ptr .LT. TOP1)) THEN

   ivCnt = Shd1(Ptr)
   Ptr = Ptr + 1

   DO 100 i = 1, ivCnt

      iCS(i, 0, XX) = Shd1(Ptr)
      Ptr = Ptr + 1

      StrtVl = Shd1(Ptr)
      Ptr = Ptr + 1

      EndVal = INT(Shd1(Ptr)/2)
      Ptr = Ptr + 1

      DO 300 j = StrtVl, EndVal
         iCS(i, j, XX) = Shd1(Ptr)
         Ptr = Ptr + 1
         iCS(i, j, ZZ) = Shd1(Ptr)
         Ptr = Ptr + 1
300       CONTINUE

    ivLen(i) = EndVal

100     CONTINUE

END IF

RETURN
END

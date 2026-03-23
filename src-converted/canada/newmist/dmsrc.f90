SUBROUTINE DMSRC (Sp, D, Ptr, Index, P, SrcI, SrcCD, SPtr)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
!  **DMSRC -- NISI  Date of last revision April 14 1994
!--------------------------------------------------------------------
!  Purpose:
!    The model requires the selection of source trees to surround
! each target. Further, it needs to select these trees from each DM
! category based on the DM category's density and the autocorellation
! that may be present. All these require a quick way to select these
! trees. This routine serves that purpose, returning three objects.
! The first, 'SPtr()', holds six array positions that are the break
! points between the DM categories. Within these breakpoint groups,
! 'SrcCD()' holds the cumulative distribution of treelist records
! within the DM category. This cumulative distribution can be used
! with a random number to select trees at random. Last, 'SrcI()'
! holds the treelist record itself, allowing the unique attributes
! (e.g.: height, crown and DM rating) of an individual tree to be
! used. This routine may have the dubious distinction of having the
! highest comment/code ratio!
!--------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     [none]
!
! Argument list definitions:
!
!     INTEGER Sp      (I) The species code being processed
!     REAL    D       (I) The density of each DM category of species
!                          'Sp'.
!     INTEGER Ptr     (I) Matrix of **pointers to treelist, sorted
!                          by species and DM class:
!                          Index 1: Species code.
!                          Index 2: DM rating.
!                          Index 3: FST is first position in
!                                   'Index()'.
!                          array; LST is last position in array. This
!                          mapping is analagous to the 'IND1()' array
!                          of the base model, but with two levels of
!                          indirection.
!     INTEGER Index   (I) Array containing pointers to the treelist.
!                          It is sorted by species and DM rating and
!                          is an analogue of the 'IND1()' array of
!                          the base model.
!     REAL    P       (I) The density (trees/acre) of each record of
!                          the treelist.
!     INTEGER SrcI    (O) Pointer to the treelist index occupying
!                          corresponding to the cumulative
!                          probability within the 'SrcCD()' array.
!                          These records are sorted by DM category
!                          into groups marked by the 'Sptr()' array.
!     REAL    SrcCD   (O) The cumulative probability of each DM group
!                          is computed by taking the relative
!                          density (trees/acre) of each treelist
!                          record and forming a cumulative
!                          distribution.
!     INTEGER Sptr    (O) Breakpoints demarcating the DM categories
!                          ordered within the 'SrcI() and 'SrcCD()'
!                          arrays. Each value marks the *last* entry
!                          in that category: e.g.: 'Sptr(3)' contains
!                          the position of the last position with DM
!                          rating 3; 'Sptr(2)+1' contains the first.
!
! Local variable definitions:
!
!     INTEGER i           Loop over DM categories.
!     INTEGER j           Loop over elements of each DM category.
!     INTEGER k           Counter for absolute position in 'SrcI()'
!                          and 'SrcCD()' arrays.
!     REAL    x           Weighting term for each treelist record,
!                          based on total density (trees/acre) within
!                          each DM category.
!     REAL    y           The current term for the cumulative
!                          distribution
!
! Common block variables and parameters:
!
!     MAXSP     PRGPRM
!     MAXTRE    PRGPRM
!     FST       DMCOM
!     LST       DMCOM
!     DMTINY    DMCOM
!
!********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER Sp
REAL    D
INTEGER Ptr
INTEGER Index
REAL    P
INTEGER SrcI
REAL    SrcCD
INTEGER Sptr

DIMENSION D(0:6)
DIMENSION Ptr(MAXSP, 0:6, LST)
DIMENSION Index(MAXTRE)
DIMENSION P(MAXTRE)
DIMENSION SrcI(MAXTRE)
DIMENSION SrcCD(MAXTRE)
DIMENSION Sptr(0:6)

! Local variables.

INTEGER i, j, k
REAL    x, y

k = 0
DO 100 i = 0, 6
  IF (Ptr(Sp, i, FST) .GT. 0) THEN
    x = 1.0 / (D(i) + DMTINY)
    y = 0.0
    DO 200 j = Ptr(Sp, i, FST), Ptr(Sp, i, LST)
      k = k + 1
      y = y + P(Index(j)) * x
      SrcI(k) = Index(j)
      SrcCD(k) = y
200     CONTINUE
  END IF
SPtr(i) = k
100 CONTINUE

RETURN
END

SUBROUTINE DMFINF (Ptr, Index)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMFINF --  DATE OF LAST REVISION: 02/16/96
!----------
! Purpose:
!  The model frequently needs to access the treelist according to
! species and DM rating. This routine generates two objects: a triply
! indexed array, 'Ptr()', that holds references to the positions in
! a second array, 'Index()', containing the relevant treelist
! records. A call to OPLIST generates the 'Index()' array.
! Subsequently, the key positions of 'Index()' are determined and
! stored in 'Ptr()'.
!
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     OPSORT
!
! Argument list definitions:
!
!     INTEGER Ptr     (O) Matrix of **pointers to treelist, sorted
!                          by species and DM class:
!                          Index 1: species code.
!                          Index 2: DM rating.
!                          Index 3: FST is first position in 'Index()'
!                                   array; LST is last position in
!                                   array. This mapping is analagous
!                                   to the 'IND1()' array of the base
!                                   model, but with two levels of
!                                   indirection.
!     INTEGER Index   (O) Array containing pointers to the treelist.
!                          It is sorted by species and DM rating and
!                          is an analogue of the 'IND1()' array of
!                          the base model.
!
! Local variable definitions:
!
!     INTEGER i           Loop counter for species and for walk
!                          along 'Index()' array.
!     INTEGER j           Loop counter for DM rating.
!     INTEGER k           Loop counter for third index of 'Ptr()'
!                          and for position in 'Index()' array.
!     INTEGER Sp          Current species code.
!     INTEGER DMR         Current DM rating.
!     INTEGER PrSp        Previous species code.
!     INTEGER PrDMR       Previous DM rating.
!
!
! Common block variables and parameters:
!
!     MAXSP   PRGPRM
!     MAXTRE  PRGPRM
!     FST     DMCOM
!     LST     DMCOM
!     ITRN    CONTRL
!     ISP     ARRAYS
!     DMRATE  DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER   Ptr
INTEGER   Index

DIMENSION Ptr(MAXSP, 0:6, LST)
DIMENSION Index(MAXTRE)

! Local variables.

INTEGER i, j, k
INTEGER Sp, DMR
INTEGER PrSp, PrDMR

do i = 1, MAXTRE
  Index(i) = 0
enddo

! Zero pointer matrix

DO 100 i = 1, MAXSP
  DO 110 j = 0, 6
    DO 120 k = FST, LST
      Ptr(i, j, k) = 0
120     CONTINUE
110   CONTINUE
100 CONTINUE

! Load the two variables into the sorting array. The 'Index()' array
! returned by OPSORT is sorted first by species 'ISP()' then by DM
! rating 'DMRATE()'.

CALL OPSORT(ITRN, ISP, DMRATE, Index, .TRUE.)

! Walk down the sorted list, assigning the first and last from each
! category, as they are found. Changes in species code and DM rate
! are used to trigger assignments to the 'Ptr()' array.

k = Index(1)
Sp = ISP(k)
DMR = DMRATE(k)
PrSp = Sp
PrDMR = DMR

Ptr(Sp, DMR, FST) = 1
DO 300 i = 2, ITRN
  k = Index(i)
  Sp = ISP(k)
  DMR = DMRATE(k)

  IF (Sp .NE. PrSp) THEN
    Ptr(PrSp, PrDMR, LST) = i - 1
    Ptr(Sp, DMR, FST) = i
    PrSp = Sp
    PrDMR = DMR
  ELSE
    IF (DMR .NE. PrDMR) THEN
      Ptr(Sp, PrDMR, LST) = i - 1
      Ptr(Sp, DMR, FST) = i
      PrDMR = DMR
    END IF
  END IF

300 CONTINUE
Ptr(Sp, DMR, LST) = ITRN

RETURN
END

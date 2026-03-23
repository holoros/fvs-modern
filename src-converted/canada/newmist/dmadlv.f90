SUBROUTINE DMADLV(SrcInd, Cnt, SFld, IFld, MshHt, Dist, &
                         Level, Shd, Shd0, II, EB)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
!  **DMADLV --  DATE OF LAST REVISION: 02/26/96
!----------
!  Purpose:
!    Computes part (one of many "levels", hence the name) of the DM
!  Intensification vector of the infection source tree and the Spread
!  vector of the infection target tree. This involves accounting for
!  the different frames of reference of the two trees, as well as
!  slope and height-dependent opacity effects. The Spread and
!  Intensification vectors (ie: heights in each tree) are returned to
!  the calling routine, where they are further adjusted to account
!  for canopy size and distance between the two. As calculated here,
!  the units are not "complete" until the final geometrical
!  adjustments and summations take place in the calling routine.
!
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     DMBSHD
!     DMRANN
!
! Argument list definitions:
!
!     INTEGER SrcInd  (I) Treelist index for the source tree.
!     INTEGER Cnt     (I) Number of these trees. This will only be
!                          greater than 1 in dense small treelists.
!     REAL    SFld    (0) Array into which the computed amount of
!                          spread (amount/MESH**3) is placed.
!     REAL    IFld    (0) Array into which the computed amount of
!                          intensification (amount/MESH**3) is
!                          placed.
!     INTEGER MshHt   (I) Height (MESH) from which the source
!                          infection is acting.
!     INTEGER Dist    (I) Dstnce (MESH) between the source and
!                          target trees.
!     REAL    Level   (I) Infection (amount/cubic MESH) originating
!                          from the source tree.
!     REAL    Shd     (I) Nbr stand shading array
!     REAL    Shd0    (I) Index stand shading array
!     INTEGER II      (I) 0 if the Src-Target pair are in the same stand;
!                         otherwise 1
!     INTEGER EB      (I) Edge-band in which Target is found
!
! Local variable definitions:
!
!     INTEGER h       Height of source (MESH), source reference
!                      frame.
!     INTEGER i       Loop counter, fixed ref frame (z-axis).
!     INTEGER j       Loop counter, fixed ref frame (x-axis).
!     INTEGER k       Loop counter,  list of trajectories into
!                      (u,v) coordinate space.
!     INTEGER m       Loop counter, members of 'k' list.
!     INTEGER n       Number of unique trajectories into (u,v).
!     INTEGER u       Loop counter, tree ref frame (z-axis).
!     INTEGER v       Loop counter, tree ref frame (x-axis).
!     INTEGER HSZInd  Upper bound, tree ref frame (z-axis).
!     INTEGER LSZInd  Lower bound, tree ref frame (z-axis).
!     INTEGER HSXInd  Upper bound, tree ref frame (x-axis).
!     INTEGER LSXInd  Lower bound, tree ref frame (x-axis).
!     INTEGER HFZInd  Upper bound, fixed ref frame (z-axis).
!     INTEGER LFZInd  Lower bound, fixed ref frame (z-axis).
!     INTEGER HFXInd  Upper bound, fixed ref frame (x-axis).
!     INTEGER LFXInd  Lower bound, fixed ref frame (x-axis).
!     INTEGER CShd    List of x- and z-positions taken along each
!                      precomputed trajectory;
!                      Index 1: element in list.
!                      Index 2: array position in element.
!                      Index 3: x- and z-position.
!     INTEGER VecLen  Length of each 'CShd' list element.
!     REAL    x       Intermediate calculation.
!     REAL    y       Intermediate calculation.
!     REAL    Op      Opacity (/MESH) of host species.
!     REAL    Rad     Radius (MESH) of source tree.
!     REAL    VecWt   Scalar for amount of infection along this
!                      trajectory.
!     REAL    Loss    Intermediate calculation of infection loss
!                      via intensification or shading.
!
! Common block variables and parameters:
!
!     DMOPQ2  DMCOM
!     DMRDMX  DMCOM
!     MXHT    DMCOM
!     MAXOFF  DMCOM
!     MXTRAJ  DMCOM
!     MXTHRX  DMCOM
!     MXTHRZ  DMCOM
!     ORIGIN  DMCOM
!     RADIUS  DMCOM
!     XX      DMCOM
!     ZZ      DMCOM
!     ISP     ARRAYS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER   SrcInd
INTEGER   Cnt
REAL      SFld
REAL      IFld
INTEGER   MshHt
INTEGER   Dist
REAL      Level
REAL      Shd(MXHT)
REAL      Shd0(MXHT)
INTEGER   II
INTEGER   EB

DIMENSION SFld(MXHT)
DIMENSION IFld(MXHT)

! Local variables.

INTEGER   h, i, j, k, m, n, u, v, w

! The precomputed matrix, represented as row and column-specific
! lists in the call to DMBSHD, assumes a fixed matrix of
! MXTHRX by MXTHRZ for the "archetype" infection field. Since
! the actual location of an infection depends on its position in the
! source tree, the two coordinate systems must be reconciled by
! horizontal and vertical translations. These boundaries of the two
! coordinate systems are referenced by 'HSZInd. etc.' and
! 'HFZInd, etc.' for the "fixed" coordinate system and the
! "tree" coordinate systems, respectively.

INTEGER HSZInd, LSZInd
INTEGER HSXInd, LSXInd

INTEGER HFZInd, LFZInd
INTEGER HFXInd, LFXInd

! Shading function data is mapped into 'CShd'. The maximum
! values are derived as follows: Each row (1st index) of the matrix
! is a set of cells through which the trajectory passes. Inspection
! of the data set used to derive the function shows that for any
! (x,z) target cell, there are at most MXTRAJ unique trajectories.
! The value of MXTRAJ (and the underlying data) will change if the
! MESH definition is changed (for example, from 2 meters to 1 meter)
! or if the number of retained vectors is changed. The default (1993)
! data uses 40% of the vectors, to speed up the computation time.
! The columns (2nd index) are the maximum number of cells ((x,z)
! pairs) through which any trajectory will pass. The choice of 28 is
! slightly arbitrary, but large enough to accomodate a 1 meter MESH
! resolution for the longest-length trajectory list, based on
! inspection of the files that created 'dmdata.inc'. The 3rd index
! records the X (=XX=1) or Z (=ZZ=2) position of the trajectory.
! 'VecLen' records the actual number of elements in each row of the
! matrix.

INTEGER CShd(MXTRAJ, 0:28, 2), VecLen(MXTRAJ)

REAL    x, y, Op, Rad
REAL    VecWt, Loss

! Find the opacity of the tree's foliage, in units of P(shading)
! per MESH**3.

Op = DMOPQ2(ISP(SrcInd))

! Find the upper and lower Z-indices of the 'SP()' array. If 'MshHt'
! is quite high or low on the tree, then parts of the distribution
! will be truncated when the 'SP()' array is mapped to the 'SFld()'
! array. This insures that the operations to the 'SFld()' array will
! always be within bounds. Note that 'MshHt' is measured in MESH
! units. Also, the calling subroutine prevents 'MshHt' from being
! greater than MXHT.

! Determine the lowest z-index for the tree coordinate system.

LSZInd = 1
i = ORIGIN - MshHt + 1
IF (i .GT. LSZInd) LSZInd = i

! Determine the highest z-index for the tree coordinate system.

HSZInd = MXTHRZ
i = ORIGIN - MshHt + MXHT
IF (i .LT. HSZInd) HSZInd = i

! Determine the lowest z-index for the tree coordinate system.
! This does not account for "seed rain" when the trajectories from
! the fixed system "run out" before hitting ground. This possibility
! is handled in the code.

LFZInd = 1
i = MshHt - ORIGIN + 1
IF (i .GT. LFZInd) LFZInd = i

! Determine the highest z-index for the tree coordinate system.

HFZInd = MshHt - ORIGIN + MXTHRZ
i = MXHT
IF (i .LT. HFZInd) HFZInd = i

! The origignal prototype allowed the x-dimension of the two matrices
! to shift. The method is now different, and the simplest code change
! involved processing only the x-value at fixed distance 'Dist'. ie:
! "process only one sampling ring". Other sampling rings are examined
! via other calls to this routine. The old loop structure is retained
! because "it ain't broke"; however, the J-loop is only passed
! through one time.

LSXInd = Dist
HSXInd = Dist

!     LSXInd = 1
!     HSXInd = MXTHRX

LFXInd = LSXInd
HFXInd = HSXInd

! Increment the infection field array with the crownthird DMR level,
! accounting for horizontal shading and the number of trees at
! distance 'Dist'. The 'u' and 'v' variables index into the fixed
! coordinate system as the tree coordinate system (the 'i' and 'j'
! indices) "moves" up the target tree.

u = LSZInd
DO i = LFZInd, HFZInd

   IF (Shd(i) .GT. 0.0) THEN

    v = LSXInd
    DO j = LFXInd, HFXInd

! Create the 'ChellShd' list (disguised as a 3-index array) of
! trajectory paths arriving at position (u,v) in the fixed reference
! frame of the source infection. 'n' returns the number of items
! (unique trajectories) in the list.

      CALL DMBSHD(CShd, u, v, VecLen, n)

! Walk through the matrix computing actual shade-corrected
! transfer to the cell. The value in 'CShd(m,k,-)' is the
! number of occurences of the trajectory 'k' at height 'm'.
! Note that the 0'th column holds 'VecWt' on return, and that
! 'Shade' must be offset to provide the true height.
!
! Here is the dynamic: If the z-position is above 50 m (unlikely)
! then the information is not saved for the higher positions. If
! the x-position is less than the radius the trajectory contributes
! to intensification. If the x-position is on a cell straddling the
! radius of the tree, then intensification is a linear proportion of
! the amount straddled (Talk about dotting i's and crossing t's). If
! the x-position is greater than or equal to the radius, then it
! contributes to the spread field, if and only if is in the proper
! sampling ring; otherwise infection field is diminished by the
! presence of shading.

! Two issues are not addressed here: Shading loss is computed using
! the height 'h' of the source tree. The actual shading is probably
! closer to the average of the source and target tree heights. (All
! this is important *only* on slopes.) Second, the stochastic effect
! of multiple instances of a particular source tree record within a
! sampling ring are not included. At *very* high densities it is
! possible to have two (or more) source trees in a ring, derived
! from the same record. The model will put give all these trees the
! same offset relative to the target.

      DO k = 1, n
        VecWt = FLOAT(CShd(k, 0, XX)) * Level
        DO m = 1, VecLen(k)
          h = MshHt + CShd(k, m, ZZ) - ORIGIN
          IF ((h .GE. 1) .AND. (h .LE. MXHT)) THEN
            Rad = DMRDMX(SrcInd, h, RADIUS)
            x = FLOAT(CShd(k, m, XX))
            y = x - Rad
            IF (x .LE. Rad) THEN
              Loss = VecWt * Op
              VecWt = VecWt - Loss
              IFld(h) = IFld(h) + Loss
            ELSE IF ((y .GT. 0.0) .AND. (y .LT. 1.0)) THEN
              Loss = VecWt * Op * y
              VecWt = VecWt - Loss
              IFld(h) = IFld(h) + Loss
            ELSE
              IF (CShd(k, m, XX) .EQ. Dist) THEN
                Loss = VecWt * Op
                SFld(h) = SFld(h) + Cnt * Loss
              ELSE
                IF (II .EQ. 1 .AND. &
                       CShd(k, m, XX) .LT. (Dist-EB)) THEN
                  Loss = VecWt * Shd(h)
                ELSE
                  Loss = VecWt * Shd0(h)
                ENDIF
                VecWt = VecWt - Loss
              END IF
            END IF
          END IF

! If the trajectory walk does not end at or before the ground (h=1),
! then "rain down" the remainder based on the last XX position 'x'.
! The value of 'x' is carried over from the last available position.

          IF ((m .EQ. VecLen(k)) .AND. (i .EQ. LFZInd) &
                 .AND. (LFZInd .GT. 1)) THEN
            DO w = i, 1, -1
              Rad = DMRDMX(SrcInd, w, RADIUS)
              y = x - Rad
              IF (y .LE. Rad) THEN
                Loss = VecWt * Op
                VecWt = VecWt - Loss
                IFld(w) = IFld(w) + Loss
              ELSE IF ((y .GT. 0.) .AND. (y .LT. 1.)) THEN
                Loss = VecWt * Op * y
                VecWt = VecWt - Loss
                IFld(w) = IFld(w) + Loss
              ELSE
                IF (CShd(k, m, XX) .EQ. Dist) THEN
                  Loss = VecWt * Op
                  SFld(w) = SFld(w) + Cnt * Loss
                ELSE
                IF (II .EQ. 1 .AND. &
                       CShd(k, m, XX) .LT. (Dist-EB)) THEN
                    Loss = VecWt * Shd(h)
                  ELSE
                    Loss = VecWt * Shd0(h)
                  ENDIF
                  VecWt = VecWt - Loss
                END IF
              END IF
            ENDDO
          END IF
        ENDDO
      ENDDO

    v = v + 1
    ENDDO

  END IF
  u = u + 1

ENDDO

RETURN
END

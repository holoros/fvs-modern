SUBROUTINE DMSAMP (TotD, D, CNB, Prop, S)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMSAMP -- NISI  Date of last revision: April 12 1994
!----------------------------------------------------------------------
! Purpose:
!   Determine the sample size 'S' to be used, based on a random selection
! from the cumulative distribution function. 'CNB()' is the cumulative
! density function for trees of all species. 'TotD' is the total density
! of all species in the stand. 'D' is the (autocorrelated) density within
! the ring.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     DMRANN
!
! Argument list definitions:
!
!     REAL      TotD  (I) The total density (trees/acre) of all trees
!                          of all species.
!     REAL      D     (I) The adjusted density (trees/acre) for the
!                          Species and DMR category from which a
!                          sample may be drawn.
!     REAL      CNB   (I) Array containing the cumulative
!                          distribution function describing the
!                          likelihood of drawing 'n' trees of any
!                          species or DMR category from within a
!                          sampling ring.
!     REAL      Prop  (I) Scalar the modifies the number drawn from
!                          the sample, depending on how much of the
!                          ring is in the stand.
!     INTEGER   S     (O) The sample size obtained from a random
!                          draw from within the appropriate species
!                          and DM class: the sample may be zero.
!
! Local variable definitions:
!
!     INTEGER   BigS      A random sample of the number of trees that
!                          are found in the sample ring. They may be
!                          of any species or DM class.
!     INTEGER   j         Loop counter for the cumulative
!                          distribution.
!     SINGLE    RND       A unform random number.
!     SINGLE    x         The proportion of all trees that will be in
!                          the appropriate species and DM class for
!                          this sample.
!
! Common block variables and parameters:
!
!     DSTLEN   DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Argument list variables.

REAL      TotD
REAL      D
REAL      CNB
REAL      Prop
INTEGER   S

DIMENSION CNB(0:DSTLEN)

! Local variables.

INTEGER   BigS, j
REAL      RND, x

! Draw a sample of trees, based on the cumulative distribution
! function that describes tree clumping. This larger sample is
! 'BigS'.

CALL DMRANN(RND)
BigS = 0
DO j = 0, DSTLEN
  IF (RND .LE. CNB(j)) THEN
    BigS = j
    GOTO 201
  END IF
ENDDO

! From the larger sample representing all trees in the stand,
! determine how many will be of the required DMR, based on the ratio
! of the density of the appropriate tree class to the total tree
! density. This is stochastic: summed over all the DMR classes, the
! value of 'S' is not compelled to be 'BigS * D / TotD', but will
! tend to that value.

201 S = 0
IF (BigS .GT. 0) THEN
  x = Prop * (D / TotD)
  DO j = 1, BigS
    CALL DMRANN(RND)
    IF (RND .LE. x) S = S + 1
  ENDDO
END IF

RETURN
END

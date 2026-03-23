SUBROUTINE DMNB (RQ, D, CNB)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMNB -- NISI  Date of last revision: 12/19/03
!----------------------------------------------------------------------
! Purpose:
!   Create a frequency distribution based upon the binomial family of
! distributions and the variance/mean ratio. This allows a wide range
! of dispersion, from binomial to poisson to negative binomial. The
! distributions are accurate until the mean and variance are both
! very small. This occurs in cases where the mean is less than about
! 0.1 trees/acre and the dispersion is less than 0.1 In these
! situations the distribution is biased so that both the variance and
! the mean are larger than requested. One solution to this would be
! to constrain the variance as the mean dropped, since it is arguably
! non-binomial when the mean is low.
!   The routine begins by creating cumulative distributions for
! sources around target in each quadrat ring. The answer is returned
! in an array containing the cumulative distribution for the number
! ofC trees in the ring. If densities higher than 10,000/acre occur,
! the length of the array will have to be enlarged or the method
! changed, because the range of (the upper tail of) the distribution
! will extend beyond the end of the array. If this occurs, those
! sample sizes will never be chosen.
!   The next step computes the distribution function for trees in an
! annulus formed by overlaying the inner circle over the outer
! circle. The annuli have certain properties: there is always at
! least one tree in the sample: the target tree itself, and; there
! are forbidden observations that result from prior observations in
! the inner circle. These two constraints require the distribution to
! be reweighted.The algorithm is a bit wasteful, since the
! distribution of the inner circle quadrat must always be
! recomputed on each pass. However, storage requirements get large
! any other way.
!
! PS: The name 'DMNB' is an artefact of the earlier method, in which
! only the Negative Binomial distribution was possible.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     BNDIST
!
! Argument list definitions:
!
!     INTEGER RQ      (I) The sampling ring number for which the
!                          cumulative distribution function is to be
!                          calculated.
!     REAL    D       (I) Total density of trees of all species
!                          (trees/ac).
!     REAL    CNB     (0) Array containing the *cumulative*
!                          distribution for trees in the sampling
!                          ring, regardless of species or DM status.
!
! Local variable definitions:
!
!     LOGICAL erflg       Unused, could set exit status for base FVS.
!     INTEGER i           General loop counter.
!     INTEGER j           Ring loop counter (inner, outer)
!     INTEGER k           General loop counter.
!     INTEGER CUR         Flag for current ring being processed.
!     INTEGER PRV         Flag for previous ring just processed.
!     INTEGER EMark       Location of the last non-zero probability
!                          in the upper tail.
!     INTEGER TopEnd      The last non-zero probability for the
!                          current (outer) ring.
!     REAL    mu          The requested mean of the distribution.
!     REAL    var         The requested variance of the distribution.
!     REAL    NB          The array holding the distribution for
!                          observations in the inner and the outer
!                          rings.
!
! Common block variables and parameters:
!
!     DSTLEN   DMCOM
!     CrArea   DMCOM
!     DMCLMP   DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Subroutine argument list.

INTEGER  RQ
REAL     D, x
REAL     CNB

DIMENSION CNB(0:DSTLEN)

! Local variables.

LOGICAL  erflg
INTEGER  i, j, k
INTEGER  CUR, PRV
INTEGER  EMark(2), TopEnd
REAL     mu, var
REAL     NB(0:DSTLEN, 2)
DOUBLE PRECISION YD

!     ** A NOTE ON 'NB()' **
!
! 'NB()' holds the probability of observing 'k' individuals at
! density 'mu' with variance 'var' Two vectors are generated: PRV is
! the vector of probabilities for the circle which is interior to the
! INNER radius of the annulus; CUR is the probability vector for the
! circle which is interior to the OUTER radius of the annulus. The
! expected number of trees 'mu' in each of the circles is given by
! the area 'CrArea()' multiplied by the total trees/acre density.
! The number of trees in the annulus (the non-intersection of the two
! circles) is given by the sum of products. For example, the
! likelihood of observing exactly 2 Source trees in the annulus is:
!
!    {P(0,i) * P(2,o)} + { P(1,i) * P(3,o) } + {P(2,i) * P(4,o) } + ...
!
! where 'i' indexes the inner circle and 'o' indexes the outer circle.
!

! Zero the distribution.

DO 10 i = 0, DSTLEN
  CNB(i) = 0.
10 CONTINUE

! Find the distribution for the inner and outer circles that define
! the ring quadrat. If RQ=1, then j=0 and the center quadrat is being
! done. In this case the inner ring is a point containing the target
! tree. The point has only 1 likelihood: P(1)= 1.0.

CUR = 1
DO 100 j = RQ - 1, RQ

  IF (j .EQ. 0) THEN

    NB(CUR,1) = 1.0
    EMark(CUR) = 1

  ELSE

    mu = CrArea(j) * D
    var = DMCLMP * mu

    CALL BNDIST(mu, var, erflg, DSTLEN, NB(0,CUR), &
                    EMark(CUR))

  END IF

  CUR = 2
  PRV = 1

100 CONTINUE

! Reweight to account for absence of observations of the n=0 class.
! This is because there is always at least a sample of 1 (the target
! tree itself) for both the inner and outer disks.

DO 1000 i = 1, 2
  x = 1.0 / (1.0 - NB(0, i))
  NB(0, i) = 0.
  DO 1001 j = 1, EMark(i)
    NB(j, i) = NB(j, i) * x
1001   CONTINUE
1000 CONTINUE

! Do the sum of products described above. 'PRV' indexes the inner
! disk, 'CUR' the outer. Recall that in the RQ=1 case, the 'PRV' disk
! is a point.

  TopEnd = EMark(CUR)

  DO 210 k = 0,TopEnd
    x = 0.
    DO 220 j = k,MIN0(k+EMark(PRV),DSTLEN)
      YD = DBLE(NB(j, CUR)) * DBLE(NB(j-k, PRV))
      IF (YD .GT. 1.0E-25) x = x + REAL(YD,4)
220     CONTINUE
    CNB(k) = x
210   CONTINUE

! Create the cumulative probability by sums.

DO 300 k = 1, TopEnd
  CNB(k) = CNB(k-1) + CNB(k)
300 CONTINUE

! Adjust distribution to account for forbidden values. ie: make it
! all sum to 1.

x = 1.0 / CNB(TopEnd)
DO 400 k = 0, TopEnd
  CNB(k) = CNB(k) * x
400 CONTINUE

RETURN
END

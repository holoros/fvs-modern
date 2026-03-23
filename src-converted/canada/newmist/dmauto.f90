SUBROUTINE DMAUTO (TrgDMR, RQ, D, S)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMAUTO -- NISI  Date of last revision: April 7 1994
!--------------------------------------------------------------------
! Purpose:
!   The model allows a positive spatial autocorrelation to exist
! between trees of similar DMR classes along with a negative spatial
! autocorrelation for unlike classes. This means that the stand-
! average density of trees in each DMR class varies in the
! neighborhood of each different DMR target tree. This routine
! adjusts those densities using a reweighting scheme that preserves
! the overall density of each DMR class. In effect, it makes it less
! likely that certain DMR classes will be neighbors. The values that
! are used as weights are held in the matrix 'SF(DMR_s,DMR_t)', which
! contains values derived from a double exponential function computed
! in DMINIT (and possibly recalculated in DMOPTS). The function has
! the form:
!
!  f(A,B,D,DMR_t,DMR_s)= EXP(A*ABS(DMR_s-DMR_t)*EXP(B*D)))
!
! where A and B are user-defined coefficients (through the DMAUTO
! keyword), D is the sampling ring from which the target sample is to
! be drawn, and DMR_t and DMR_s are the DMR classes of the target and
! source trees respectively.
!
! Within the ring 'D' containing a target tree with a DMR of 'DMR_t',
! the following relationship exists, with 'x' unknown, looping over
! the 0-6 'i' DMR  categories:
!
!              i
!   D_total = SUM { x * SF(DMR_t,D) * D_s(i) }
!
! where:  SF(T,S) is the scaling function value based on the
!                 DMR-difference of Target 'T' compared to a Source
!                 'S'
!         D_s(i)  is the trees/acre for the DMR_s in category 'i'
!         D_total is the total stems/acre for the stand
!         D       is the index to the sample ring
!
! 'x' can be solved directly:
!
!                  i
!   x = D_total / SUM { SF(DMR_t,D) * D_s(i) }
!
! Then the adjusted valued of D_s can be computed directly, giving
! back the adjusted trees/acre for each ring.
!
!   D_s'(i)= SF(DMR_t,D) * x * D_s(i)
!
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
!     INTEGER TrgDMR  (I) The DMR of the target tree that lies at the
!                          centre of the local neighborhood
!     INTEGER RQ      (I) The sampling ring number for which the
!                          neighborhood density of each DMR class is
!                          computed. A value of 1 is the innermost
!                          ring (actually a circular disk of MESH
!                          radius)
!     REAL    D       (I) Array containing the unadjusted density
!                          (trees/ac) for each DMR category within a
!                          species.
!     REAL    S       (O) Array containing the adjusted density
!                          (trees/ac) for each DMR category within a
!                          species.
!
! Local variable definitions:
!
!     INTEGER i           Loop counter.
!     REAL    ds          Sum of weighted densities across all DMRs.
!     REAL    DTot        Trees/acre for all members of the species.
!
! Common block variables and parameters:
!
!     SF      DMCOM
!     DMRDFF  DMCOM
!
!********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER   TrgDMR
INTEGER   RQ
REAL      D
REAL      S

DIMENSION D(0:6)
DIMENSION S(0:6)

! Local variables.

INTEGER   i
REAL      ds, DTot

! Find the total trees/acre of all DMR classes. If there are no trees
! of this species, then blow this popstand. This is slightly inefficient,
! since it will be calculated repeatedly when the call is made within a
! time step. Perhaps there is already a calculation of each DMR density
! done somewhere else?

DTot = 0.
DO 200 i = 0, 6
  S(i) = 0.0
  DTot = DTot + D(i)
200 CONTINUE

IF (DTot .LE. 0.0) GOTO 500

! Loop over the delta-DMR categories. For DMR=0 and 6, there will be
! 6 of these. For DMR=3 only 3 delta-DMR values will be used (obvious
! once you think about it). Then compute the appropriate sum of
! weighted densities for each DMR class. 'ds' is a weighted sum for
! all classes in the quadrat ring: each 'D()' Source DMR-class
! density is weighted by the value of 'SF()' appropriate to the DMR
! difference between Target 'j' and Source 'k' implied by the
! 'DMRDFF' matrix.

    ds = 0.
    DO 300 i = 0, 6
      ds =  ds + D(i) * SF(DMRDFF(i, TrgDMR), RQ)
300     CONTINUE

! 'DTot' changes meaning slightly here, and is scaled by 'ds' to give
! a multiplier for the adjusted density of each DMR class.

    DTot = DTot / ds
    DO 400 i = 0, 6
      S(i) = DTot * D(i) * SF(DMRDFF(i, TrgDMR), RQ)
400     CONTINUE

500 CONTINUE

RETURN
END

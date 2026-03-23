SUBROUTINE DMOTHR (Sp)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMOTHR -- NISI  Date of last revision: April 12 1994
!----------------------------------------------------------------------
! Purpose:
!   Spread and intensification are modelled in an abstract way. Real
! world S&I is based on seeds being thrown, sticking, germinating,
! etc. This routine uses some rough guesses about the probability of
! some of these events, combined with guesses about seed production
! per female plant, and the relationship between DM rating and the
! density of plants required to achieve that rating. These *guesses*
! merely get the number of new infections in the ball park. Fine
! tuning is based on user-supplied scaling terms applied through the
! DMSTUN, DMITUN and DMETUN keywords. Once the model is more
! mature, I hope that the terms put in those keywords are hardwired,
! so that they will behave sensibly with their default values of 1.0
!
! For the record, here are the guesses:
!
!  P(adhering successfully to a branch tip): 0.05
!  P(germinating):                           0.15
!  P(being female):                          0.5
!
! Other factors accounted for here are:
!  1/  actual seed production per plant:    10.    (a guess)
!  2/  plants/DMR/observed cubic meter       0.25  (a guess)
!  3/  cubic meters per cubic MESH        MESH**3
!  4/  weight of each observation            0.001 (1e3 trajectories)
!----------------------------------------------------------------------
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
!     INTEGER   Sp    (I) Species code for the tree species being
!                          processed.
!
! Local variable definitions:
!
!     INTEGER   i         Pointer to treelist record referenced by
!                          'IND()' array.
!     INTEGER   j         Loop counter for crown thirds.
!     INTEGER   jj        Loop counter for treelist records.
!     INTEGER   i1        Pointer to the first treelist record for
!                          the species being processed.
!     INTEGER   i2        Pointer to the last treelist record...
!     REAL      Factor    Gross model adjustment for all processes
!                          leading to successful infection.
!     REAL      FacS      Fine model adjustment for spread.
!     REAL      FacI      Fine model adjustment for intensification.
!
! Common block variables and parameters:
!
!     ISCT      CONTRL
!     MESH      DMCOM
!     TRAJWT    DMCOM
!     DMETUN    DMCOM
!     DMSTUN    DMCOM
!     DMITUN    DMCOM
!     IND1      ARRAYS
!     CRTHRD    DMCOM
!     NewSpr    DMCOM
!     NewInt    DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'DMCOM.f90'

! Subroutine arguments.

INTEGER   Sp

! Local variables.

INTEGER   i, j, jj, i1, i2
REAL      Factor, FacS, FacI

i1 = ISCT(Sp, 1)
i2 = ISCT(Sp, 2)

IF (i1 .EQ. 0) RETURN

! General adjustment based on guesses for the processes.

Factor = .05 * .15 * .5 * 10. * .25 * (MESH ** 3) * TRAJWT

! Fine-tuning for user-supplied overall establishment (DMETUN),
! Spread (DMSTUN) and Intensification (DMITUN).

FacS = DMETUN(Sp) * DMSTUN(Sp) * Factor
FacI = DMETUN(Sp) * DMITUN(Sp) * Factor

DO 100 jj = i1, i2
  i = IND1(jj)
  DO 200 j = 1, CRTHRD
    NewSpr(i, j) = NewSpr(i, j) * FacS
    NewInt(i, j) = NewInt(i, j) * FacI
200     CONTINUE
100   CONTINUE

RETURN
END

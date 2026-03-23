SUBROUTINE DMNDMR
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMNDMR -- NISI  Date of last revision: 04/06/04
!----------------------------------------------------------------------
! Purpose:
!   The most common measure of infection status is Hawksworth's DMR.
! This routine transforms the projected infection levels from each
! crown third into the 0-6 Hawksworth scale. From the life history
! pools there are two components that contribute to the DMR: the
! ACTIVE (flower producing) and suprsd (formerly flower producing,
! now quiescent or suppressed). By this point, fecundity scalings
! (DMITUN, etc.) have already happened, and the pools represent DMR
! density for the crown the infection resdides in. A random process
! is used to determine what to do with the fractional parts of
! infection. For example, if the sum of the ACTIVE+SUPRSD pools is
! 1.71 in the lower crown third, there is a 71% chance of the crown
! third being given a 2 rating, and a 29% chance of being rated a 1.
! The crown thirds are then added up to give the tree rating.
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
!     [none]
!
! Local variable definitions:
!
!     INTEGER   i         Loop counter for treelist records.
!     INTEGER   j         Loop counter for crown thirds.
!     INTEGER   k         Incremental counter for DMR in each third.
!     REAL      x         Sum of the ACTIVE and SUPRSD pools.
!     REAL      Frac      Fractional remainder
!     REAL      RND       Uniform random number.
!
! Common block variables and parameters:
!
!     ITRN      CONTRL
!     DMRATE    DMCOM
!     CRTHRD    DMCOM
!     DMINF     DMCOM
!     ACTIVE    DMCOM
!     SUPRSD    DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DMCOM.f90'

INTEGER   i,j,k,L
REAL      x,Frac,RND

! WALK THROUGH TREELIST AND ASSIGN NEW DMR BASED ON RANDOM TRUNCATION OF
! PROJECTED DMR WITHIN EACH CROWN THIRD.

DO I = 1,ITRN
  k = 0
  DMRATE(i) = 0
  DO J = 1,CRTHRD

    x = DMINF(i,j,ACTIVE) + DMINF(i,j,SUPRSD)
    DO L = 1,MAXBC
      x = x + DMINF_BC(i,j,ACTIVE,L) + DMINF_BC(i,j,SUPRSD,L)
    ENDDO
    x = x + DMINF(i,j,DEAD_BC)

    IF (x .GT. 2.0) THEN
      k = 2
    ELSE
      k = INT(x)
      Frac = x - FLOAT(k)
      CALL DMRANN(RND)
      IF (RND .LE. Frac) k = k + 1
    END IF
    DMRATE(i) = DMRATE(i) + k
  ENDDO
ENDDO

RETURN
END

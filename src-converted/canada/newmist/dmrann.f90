SUBROUTINE DMRANN (SEL)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMRANN -- NISI  Date of last revision: April 12 1994
!----------------------------------------------------------------------
! Purpose:
!   This routine is based on the uniform random number found in
! ESRANN, coded by Nicholas Crookston. It is used in many places
! to simulate random processes.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMINIT    [ENTRY DMRNSD]
!     MISIN     [ENTRY DMRNSD]
!     DMFSHD
!     DMNDMR
!     DMSAMP
!     DMSLOP
!     DMSLST
!     DMTLST
!
! Other routines called:
!
!     [none]
!
! Argument list (and ENTRY point) definitions:
!
!     INTEGER   SEL   (O) Uniform random number.
!     LOGICAL   LSET  (I) Logical value specifying whether the
!                          generator is being reseeded (.TRUE.).
!     REAL      SEED  (I) Value to reseed the random
!                          sequence.
!
! Local variable definitions:
!
!     [none]
!
! Common block variables and parameters:
!
!     DMS0      DMCOM
!     DMS1      DMCOM
!     DMSS      DMCOM
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'DMCOM.f90'

! Argument list (including ENTRY point) variables.

REAL      SEL
LOGICAL   LSET
REAL      SEED

DMS1=DMOD(16807D0*DMS0,2147483647D0)
SEL=REAL(DMS1/2147483648D0,4)
DMS0=DMS1
RETURN

! You may reseed the generator by supplying an real-valued odd
! number with LSET = .TRUE. If LSET = .FALSE. a call to DMRNSD will
! cause the random number generator to start over.

ENTRY DMRNSD (LSET, SEED)
IF (LSET) GOTO 10
SEED=DMSS
DMS0=SEED
RETURN
10 CONTINUE
IF (AMOD(SEED,2.0).EQ.0.) SEED=SEED+1
DMSS=SEED
DMS0=SEED
RETURN
END

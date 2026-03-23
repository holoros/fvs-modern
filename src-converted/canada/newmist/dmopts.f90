SUBROUTINE DMOPTS
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMOPTS -- NISI  Date of last revision: April 10 1994
!----------------------------------------------------------------------
! Purpose:
!  Users are provided three date-sensitive options in the model:
! DMCLMP, DMALPH and DMBETA. This routine checks to see if any of
! those keywords has been scheduled for this cycle, and performs
! necessary calculations if required. The calculation of the 'SF()'
! array mirrors the calculation performed in DMINIT.
!----------------------------------------------------------------------
!
! Called by:
!
!     DMTREG
!
! Other routines called:
!
!     OPFIND
!     OPGET
!     OPDONE
!
! Argument list definitions:
!
!     [none]
!
! Local variable definitions:
!
!     INTEGER   i         Loop counter for various things: options,
!                          trees, plots and DM categories.
!     INTEGER   j         Loop counter for sampling rings.
!     INTEGER   n         Counter for actual number of sample points
!                          encountered.
!     INTEGER   p         Sample plot number.
!     INTEGER   NTODO     The number of activities scheduled for the
!                          option in the current cycle.
!     INTEGER   IDATE     Dummy value containing the date for which
!                          the OP call is scheduled.
!     INTEGER   IACTK     Dummy value holding the completion status
!                          of the OP call.
!     INTEGER   NPAR      Dummy value containing the number of
!                          parameters returned from the OP call.
!     REAL      Parm      The array in which option parameters are
!                          returned.
!     REAL      Mean      The mean density of trees.
!     REAL      Var       The variance of the density, computed
!                          either by the Dispersion index or by
!                          the variance of the sample points.
!     REAL      x         Scalar used in calculation of sample ring
!                          modified density; also used to hold
!                          trees/acre density of individual records.
!     REAL      tmp       Temporary calculation for sample ring
!                          density scaling.
!     REAL      PltDns   Dnsty (trees/acre) for each sample point.
!
! Common block variables and parameters:
!
!     MXTHRX    DMCOM
!     MAXPLT    PRGPRM
!     IY        CONTRL
!     ICYC      CONTRL
!     DMALPH    DMCOM
!     DMBETA    DMCOM
!     SF        DMCOM
!     IPTINV    PLOT
!     ITRE      ARRAYS
!     PROB      ARRAYS
!     PI        PLOT
!
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'DMCOM.f90'

! Local variables.

INTEGER   i, n, p
INTEGER   NTODO, IDATE, IACTK, NPAR
INTEGER   MYACTS(2)
REAL      Parm(2)
REAL      Mean, Var, x
REAL      PltDns(MAXPLT)

DATA MYACTS /2008,2009/

! See if options have been specified for DMALPH and/or DMBETA. A
! Parm() value of -999 means "don't change the existing value".

NTODO = 0
CALL OPFIND(1, MYACTS(2), NTODO)
IF (NTODO .NE. 0) THEN
  DO 8 I = 1,NTODO
    CALL OPGET(I, 2, IDATE, IACTK, NPAR, Parm)
    CALL OPDONE(I,IY(ICYC))
    IF(Parm(1) .NE. -999.0) DMALPH = Parm(1)
    IF(Parm(2) .NE. -999.0) DMBETA = Parm(2)
8   CONTINUE
  CALL DMNAC(DMALPH,DMBETA)
ENDIF


! Determine if DMCLMP has been specified. If not, the stems/acre
! dispersion among plots is used. Plots are not assumed to be in any
! order, even though I suspect that they probably are.

NTODO = 0
CALL OPFIND(1, MYACTS(1), NTODO)
IF (NTODO .NE. 0) THEN
  DO 19 I = 1, NTODO
    CALL OPGET(I, 1, IDATE, IACTK, NPAR, Parm)
    CALL OPDONE(I, IY(ICYC))
    DMCLMP = Parm(1)
19   CONTINUE

ELSE

  IF (IPTINV .GT. 1) THEN

    DO 20 i=1, MAXPLT
      PltDns(i) = 0.0
20     CONTINUE

    Mean = 0.0
    DO 21 i = 1, ITRN
      p = ITRE(i)
      x = PROB(i)
      PltDns(p) = PltDns(p) + x
      Mean = Mean + x
21     CONTINUE
    Mean = Mean / PI

    Var = 0.0
    n = 0
    DO 22 i = 1, MAXPLT
      IF (PltDns(i) .GT. 0.0) THEN
        Var = Var + (PltDns(i) - Mean)**2
        n = n + 1
      END IF
22     CONTINUE

    IF (n .GT. 1) THEN
      Var = Var / (FLOAT(n) - 1.0)
      DMCLMP = Var / Mean
    END IF

  END IF

ENDIF

RETURN
END

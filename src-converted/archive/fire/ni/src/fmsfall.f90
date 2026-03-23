Subroutine FMSFALL(IYR,KSP,D,ORIGDEN,DENTTL,ISWTCH, &
                      RSOFT,RSMAL,DFALLN)
IMPLICIT NONE
!----------
! FIRE_NI $Id: fmsfall.f 0000 2018-02-14 00:00:00Z gedixon $
!----------
!
!     SNAG FALL PREDICTION
!
!   Purpose:
!     This routine calculates the base fall rates for snags, based
!     on species, DBH (large vs small), and whether we are down to the
!     last 5% of the snags original stem/ac representation.
!
!     The logic in this routine was extracted from its original
!     location in FMSNAG, in order to generalize the logic for use
!     with both the FFE snag pools, and the base FVS model snag records.
!
!   Called from: FMSNAG to compute fall rate for a given FFE snag pool.
!                SVSNAGE to compute fall rate for a given FVS snag record.
!
!   Local variable definitions:
!     BASE:    Base rate-of-fall for snags of this size (I.E., rate for
!              snags with FALLX=1)
!     D:       Diameter of current snag pool/record.
!     DENTTL:  If called from FMSNAG:
!                 Sum of DENIH + DENIS for current snag pool (sum of
!                 hard + soft snags/ac).
!              If called from SVSNAGE:
!                 The total number of SVS snag records still standing
!                 that were generated from the same source tree, in
!                 the same year, as the current snag record.
!     DFALLN:  Target density of snags to fall under normal conditions
!              (where hard and soft snags fall at the same rate).
!     DZERO:   Density level (#/acre), at which snag is considered
!              equal to ZERO.
!     FALLM2:  Rate-of-fall for last 5% of lrg snags in current record
!     ISWTCH:  =1 if FMSNAG called this subroutine.
!              =2 if SVSNAGE called this subroutine.
!     KSP:     Species number for current snag pool/record.
!     ORIGDEN: If called from FMSNAG:
!                 The density (stems per acre) of snags at the time of
!                 death, for the current snag pool.
!              If called from SVSNAGE:
!                 The total number of SVS snag records that were
!                 generated from the same source tree, in the same
!                 year, as the current snag record.
!     RSMAL:   Rate of snag fall implied by PBSMAL and PBTIME
!     RSOFT:   Rate of snag fall implied by PBSOFT and PBTIME
!
!   Common variable definitions:
!     ALLDWN:  Time by which the last 5% of lrg snags in each group
!              have all fallen.
!     FALLX:   Rate-of-FALL correction factors for each species.
!              Adjusts fall rates for each species, relative to rate
!              predicted by base equation for a single base spp (PP in
!              NI variant).
!              Internal FALLX values can be overridden by the user, via
!              the SNAGFALL keyword.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
!OMMONS
!
INTEGER ISWTCH, IYR, KSP
REAL    BASE, D, DENTTL, DFALLN, DZERO, FALLM2, ORIGDEN, &
           RSOFT, RSMAL, X
INTEGER IDANUW
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = ISWTCH
!
!
!     In the first year after a fire, some work is required to determine
!     what fall rates to use in the coming years.  First, calculate
!     RSOFT and RSMAL.  These rates are the constant proportion
!     of snags that must fall each year in order for a total proportion
!     PBSOFT (or PBSMAL) of snags to have fallen after PBTIME (e.g.,
!     28% of remaining snags must fall every year in order for 90% of
!     the initial number to have fallen in 7 years:  (1-0.28)**7 = 0.1).
!     This could be done outside the snag loop except when either PBSOFT
!     of PBSMAL equals 1, which may often be the case.  So its done here.

DFALLN = 0.
RSOFT = 0.0
RSMAL = 0.0
IF (DENTTL .LE. 0) RETURN
IF ((IYR - BURNYR) .LT. PBTIME) THEN
  DZERO = NZERO / 50.0

  IF (PBSOFT .GT. 0.0) THEN
    IF (PBSOFT .LT. 1.0) THEN
      RSOFT = 1 - EXP( LOG(1-PBSOFT) / PBTIME )
    ELSE
      RSOFT = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
    ENDIF
  ENDIF

  IF (PBSMAL .GT. 0.0) THEN
    IF (PBSMAL .LT. 1.0) THEN
      RSMAL = 1 - EXP( LOG(1-PBSMAL) / PBTIME )
    ELSE
      RSMAL = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
    ENDIF
  ENDIF
ENDIF

!     Calculate the density of snags in this record that would fall under
!     normal conditions.  This depends on species, dbh and whether 5% are
!     left.

BASE = -0.001679 * D + 0.064311
IF (BASE .LT. 0.01) BASE = 0.01

IF (D .LT. 18.0) THEN
  DFALLN = BASE * FALLX(KSP) * ORIGDEN
ELSE

!     Near 5%, fall at least as many lg. snags as should fall at
!     final rate-of-fall, but be sure not to fall more than this
!     many below 5% all at once.  This requires calculating FALLM2.
!     First, find the time X at which 5% of snags will be left
!     (proportion standing = -BASE*FALLX*time + 1).

  X = (0.05 - 1) / (-BASE*FALLX(KSP))

!     Then find the slope of the line that passes from
!     5% at this time to 0% at ALLDWN (adjusted for the different
!     FALLX values).  If ALLDWN <= time at which 5% left, assign
!     a slope of -2 (removes last 5% of snags immediately).  The
!     negative of this slope is the rate FALLM2.

  IF (ALLDWN(KSP) .LE. X) THEN
    FALLM2 = 2
  ELSE
    FALLM2 = 0.05 / (ALLDWN(KSP) - X)
  ENDIF

!     Now proceed to calculate how many snags should normally fall.

  IF (DENTTL .LE. (0.05*ORIGDEN)) THEN
    DFALLN = FALLM2 * ORIGDEN
  ELSE
    DFALLN = BASE * FALLX(KSP) * ORIGDEN
    IF (DENTTL .LT. (DFALLN + 0.05*ORIGDEN)) THEN
      DFALLN = DENTTL  - (ORIGDEN * (0.05 - FALLM2))
    ENDIF
  ENDIF
ENDIF
!
RETURN
END


SUBROUTINE FMSNGDK(VAR,KSP,D,DKTIME)
IMPLICIT NONE
!----------
! FIRE-VBASE $Id$
!----------
!
!     SNAG DECAY PREDICTION
!
!   Purpose:
!     This routine calculates the number of years, since death, for a
!     snag to become soft, based on species, DBH, and plant association.
!
!     The base logic in this routine was extracted from its original
!     location in FMSNAG, FMSCRO, and SVSNAGE, in order to
!     structure/generalize the logic for use with both the
!     FFE snag pools, and the base FVS model snag records.
!
!   Called from: FMSNAG to compute decay time for a given FFE snag pool.
!                FMSCRO to compute decay time for a given FFE snag pool.
!                SVSNAGE to compute decay time for a given FVS snag record.
!
!   Local variable definitions:
!     D:       Diameter of current snag pool/record.
!     DKTIME:  Years, since death, for snag to become soft.
!     KSP:     Species number for current snag pool/record.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
!OMMONS
!
CHARACTER VAR*2
INTEGER JADJ, JSML, JYRSOFT, KSP
REAL    D, DKTIME, XMOD


!----------
!  Call SVHABT to get habitat-based multiplier for decay rate.
!  (NOTE: SVHABT currently returns a constant value of 1.0.
!         The call to SVHABT is preserved here for potential future use.
!         The XMOD multiplier is not applied to decay rates computed in
!         the FMR6SDCY routine, since it specifically addresses plant
!         association.)
!----------

CALL SVHABT(XMOD)

!----------
!  Calculate years, since death, for snag to become soft.
!----------

SELECT CASE (VAR)
  CASE('LS','ON')
    DKTIME = 0.65 * DECAYX(KSP) * D
    DKTIME = DKTIME * XMOD
  CASE('PN', 'WC', 'BM', 'EC', 'AK', 'OP')
    CALL FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
    DKTIME = JYRSOFT * DECAYX(KSP)
  CASE('SO')
    SELECT CASE (KODFOR)
    CASE (601,602,620,799)                     !OREGON
      CALL FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
      DKTIME = JYRSOFT * DECAYX(KSP)
    CASE DEFAULT                               !CALIFORNIA
      DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
      DKTIME = DKTIME * XMOD
    END SELECT
  CASE DEFAULT
    DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
    DKTIME = DKTIME * XMOD
END SELECT
!
RETURN
END


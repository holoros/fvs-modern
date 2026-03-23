SUBROUTINE FMSNGHT(VAR,KSP,HTD,HTCURR,IHRD,HTSNEW)
IMPLICIT NONE
!----------
! FIRE-VBASE $Id$
!----------
!
!     SNAG HEIGHT PREDICTION
!
!   Purpose:
!     This routine predicts snag height loss due to top breakage.
!
!     The base logic in this routine was extracted from its original
!     location in FMSNAG, in order to structure/generalize the logic
!     for use with both the FFE snag pools, and the base FVS model
!     snag records.
!
!   Called from: FMSNAG to compute height loss for a given FFE snag pool.
!                SVSNAGE to compute height loss for a given FVS snag record.
!
!   Local variable definitions:
!     IHRD:    Indicates if current height loss prediction is for a hard
!              or soft snag. Used to reference appropriate HTX array elements.
!     HTCURR:  Current height of snag pool/record.
!     HTSNEW:  Updated height for snag pool/record.
!     HTD:     Height of current snag pool/record, at time of death.
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
INTEGER HTINDX1, HTINDX2, IHRD, KSP
REAL    HTCURR, HTD, HTSNEW, SFTMULT, X2
!
!----------
!  Set array indices for retrieving hard/soft height loss coefficients.
!----------
!
IF ( IHRD .EQ. 1 ) THEN
  HTINDX1 = 1
  HTINDX2 = 2
  SFTMULT = 1.0
ELSE
  HTINDX1 = 3
  HTINDX2 = 4
  SFTMULT = HTXSFT
ENDIF
!
!----------
!  Decrease the height of snags according to current height and
!  whether they were initially hard or soft. If height gets to
!  be less than 1.5 foot, call it 0 (the 'snag' is considered to be
!  'fuel' now instead).
!----------
!
SELECT CASE (VAR)
  CASE('CI')
!       CI variant has special rules for WP & RC

    SELECT CASE (KSP)
      CASE (1,6)          ! WP, RC STOP LOSING HEIGHT AT 75%
        IF (HTCURR .GT. (0.75 * HTD)) THEN
          HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1) &
                      * SFTMULT)**NYRS
        ELSE
          HTSNEW = HTCURR
        ENDIF
      CASE DEFAULT
        IF (HTCURR .GT. (0.5 * HTD)) THEN
          HTSNEW = HTCURR * (1.0 - HTR1 * HTX(KSP,HTINDX1) &
                      * SFTMULT)**NYRS
        ELSE
          HTSNEW = HTCURR * (1.0 - HTR2 * HTX(KSP,HTINDX2) &
                      * SFTMULT)**NYRS
        ENDIF
    END SELECT

  CASE('PN', 'WC', 'BM', 'EC', 'OP')
!       First, get the height loss rate from fmr6htls. But if the
!       height loss is adjusted by user (snagbrk keyword), make sure
!       you use their values.

    CALL FMR6HTLS(KSP,X2)
    IF (HTCURR .GT. (0.5 * HTD)) THEN
      IF ((HTX(KSP,HTINDX1) .GT. 1.01) .OR. &
             (HTX(KSP,HTINDX1) .LT. 0.99)) THEN
        HTSNEW = HTCURR * &
                      (1.0 - HTR1 * HTX(KSP,HTINDX1)*SFTMULT)**NYRS
      ELSE
        HTSNEW = HTCURR * (1.0 - X2)**NYRS
      ENDIF
    ELSE
      IF ((HTX(KSP,HTINDX2) .GT. 1.01) .OR. &
             (HTX(KSP,HTINDX2) .LT. 0.99)) THEN
        HTSNEW = HTCURR * &
                      (1.0 - HTR2 * HTX(KSP,HTINDX2)*SFTMULT)**NYRS
      ELSE
        HTSNEW = HTCURR * (1.0 - X2)**NYRS
      ENDIF
    ENDIF

  CASE('SO')
    SELECT CASE (KODFOR)
    CASE(505,506,509,511,701,514)                  ! CALIFORNIA

      IF (HTCURR .GT. (0.5 * HTD)) THEN
        HTSNEW = HTCURR * &
                      (1.0 - HTR1 * HTX(KSP,HTINDX1) *SFTMULT)**NYRS
      ELSE
        HTSNEW = HTCURR * &
                      (1.0 - HTR2 * HTX(KSP,HTINDX2) *SFTMULT)**NYRS
      ENDIF
!
    CASE DEFAULT                                   ! OREGON

!         First, get the height loss rate from fmr6htls. But if the
!         height loss is adjusted by user (snagbrk keyword), make sure
!         you use their values.

      CALL FMR6HTLS(KSP,X2)
      IF (HTCURR .GT. (0.5 * HTD)) THEN
        IF ((HTX(KSP,HTINDX1) .GT. 1.01) .OR. &
               (HTX(KSP,HTINDX1) .LT. 0.99)) THEN
          HTSNEW = HTCURR * &
                       (1.0 - HTR1 *HTX(KSP,HTINDX1)*SFTMULT)**NYRS
        ELSE
          HTSNEW = HTCURR * (1.0 - X2)**NYRS
        ENDIF
      ELSE
        IF ((HTX(KSP,HTINDX2) .GT. 1.01) .OR. &
               (HTX(KSP,HTINDX2) .LT. 0.99)) THEN
          HTSNEW = HTCURR * &
                        (1.0 - HTR2 *HTX(KSP,HTINDX2)*SFTMULT)**NYRS
        ELSE
          HTSNEW = HTCURR * (1.0 - X2)**NYRS
        ENDIF
      ENDIF
    END SELECT

  CASE DEFAULT
    IF (HTCURR .GT. (0.5 * HTD)) THEN
      HTSNEW = HTCURR * &
                   (1.0 - HTR1 * HTX(KSP,HTINDX1) * SFTMULT)**NYRS
    ELSE
      HTSNEW = HTCURR * &
                    (1.0 - HTR2 * HTX(KSP,HTINDX2) * SFTMULT)**NYRS
    ENDIF

END SELECT

IF (HTSNEW .LT. 1.5) HTSNEW = 0.0

RETURN
END


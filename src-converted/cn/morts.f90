! =============================================================================
! src-converted/cn/morts_cn.f90
!
! CONUS unified variant background mortality. Replaces the NC clone.
!
! Form (annualized cloglog GLM, fit per species on David Marshall's CHANGEdata):
!
!     log( -log(1 - p_period) ) = MOB0 + MOB1*DBH + MOB2*CR + MOB3*BAL
!                                 + log(periods)
!
! With offset(log(periods)) the linear predictor is on a per-year basis, so
! the per-year mortality probability is:
!
!     eta_yr      = MOB0 + MOB1*DBH + MOB2*CR + MOB3*BAL
!     p_yr        = 1 - exp( -exp(eta_yr) )
!
! Cycle (multi-year) mortality compounds:
!
!     p_cycle     = 1 - (1 - p_yr)^FINT
!
! Coefficients per species are read from cn.json categories.mortality
! .MOB0 .. MOB3 into the CN_MO COMMON block populated upstream by RCON.
!
! Inputs (per tree, from VARCOM / ARRAYS):
!   ISPC          variant species index
!   DBH(I)        starting diameter, inches
!   ICR(I)        crown ratio, integer percent (we divide by 100)
!   PTBAA         basal area in larger trees, sq ft per acre
!   FINT          cycle length in years (CONTRL.f90)
!   PROB(I)       per-tree probability weight
!
! Output:
!   WK2(I)        cycle probability of dying for tree I (FVS convention,
!                 same as legacy NC morts.f90); returned via VARCOM
! =============================================================================
SUBROUTINE MORTS
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'
  INCLUDE 'CALCOM.f90'
  INCLUDE 'ARRAYS.f90'
  INCLUDE 'COEFFS.f90'
  INCLUDE 'CONTRL.f90'
  INCLUDE 'OUTCOM.f90'
  INCLUDE 'PLOT.f90'
  INCLUDE 'PDEN.f90'
  INCLUDE 'VARCOM.f90'

  ! CN_MO COMMON block populated from cn.json categories.mortality
  REAL    MOB0, MOB1, MOB2, MOB3
  LOGICAL LMOOK
  COMMON /CN_MO/ MOB0(MAXSP), MOB1(MAXSP), MOB2(MAXSP), MOB3(MAXSP), &
                  LMOOK(MAXSP)

  INTEGER :: I, ISPCT, NPRD
  REAL    :: B0, B1, B2, B3, ETA_YR, PYR, PCYC, CR_FRAC, BAL_USE, DBH_USE

  IF (FINT <= 0.0) THEN
    NPRD = 1
  ELSE
    NPRD = NINT(FINT)
    IF (NPRD < 1) NPRD = 1
  END IF

  DO I = 1, ITRN
    WK2(I) = 0.0
    IF (PROB(I) <= 0.0) CYCLE
    ISPCT = ISP(I)
    IF (ISPCT < 1 .OR. ISPCT > MAXSP) THEN
      ! pooled CONUS fallback (intercept giving ~1.5%/yr at average covariates)
      B0 = -4.6; B1 = -0.02; B2 = -0.7; B3 = 0.005
    ELSE IF (.NOT. LMOOK(ISPCT)) THEN
      B0 = -4.6; B1 = -0.02; B2 = -0.7; B3 = 0.005
    ELSE
      B0 = MOB0(ISPCT); B1 = MOB1(ISPCT)
      B2 = MOB2(ISPCT); B3 = MOB3(ISPCT)
    END IF

    DBH_USE = DBH(I)
    CR_FRAC = REAL(ICR(I)) / 100.0
    IF (CR_FRAC < 0.01) CR_FRAC = 0.01
    IF (CR_FRAC > 1.00) CR_FRAC = 1.00
    BAL_USE = PTBAA(ITRE(I))

    ETA_YR = B0 + B1*DBH_USE + B2*CR_FRAC + B3*BAL_USE
    IF (ETA_YR >  10.0) ETA_YR =  10.0
    IF (ETA_YR < -20.0) ETA_YR = -20.0

    PYR  = 1.0 - EXP( -EXP(ETA_YR) )
    PCYC = 1.0 - (1.0 - PYR)**REAL(NPRD)

    IF (PCYC < 0.0) PCYC = 0.0
    IF (PCYC > 1.0) PCYC = 1.0
    WK2(I) = PCYC
  END DO

  RETURN
END SUBROUTINE MORTS

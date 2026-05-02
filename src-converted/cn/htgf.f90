! =============================================================================
! src-converted/cn/htgf.f90
!
! CONUS unified variant height growth function. Implements Greg Johnson's
! Chapman-Richards-style integrated annual increment from
! https://github.com/gregjohnsonbiometrics/fvs_remodeling
! ported from R (est_hg in scripts/height_growth/Height_Growth_Equations_for_CONUS.qmd).
!
! Equation form (per year, per tree):
!
!     dHT = HT_MAX * B1 * B2 * CR^B3
!           * exp( -B1*HT - B4*CCFL - B8*sqrt(CCH)
!                  - B5*ELEV + B6*sqrt(TD) + B7*EMT )
!           * (1 - exp(-B1*HT))^(B2 - 1)
!
! Coefficients are read from config/cn.json categories.johnson_hg.
! JHGB0 = species-specific maximum height (feet).
!
! Inputs (via COMMON blocks):
!     ISPC          variant species index
!     HT(I)         starting total height, feet
!     ICR(I)        crown ratio, integer percent
!     PCT(I)        % of plot BA in larger trees (proxy for CCFL)
!     PTBAA(ITRE(I)) basal area in larger trees, sq ft / acre
!     CCH(I)        crown closure at tree tip, fraction (HTCAL.f90)
!     ELEV          stand elevation, feet
!     TD            stand annual temperature difference, deg C (PLOT.f90)
!     EMT           stand extreme minimum temperature, deg C (PLOT.f90)
!     FINT          cycle length, years
!
! Output:
!     HTG(I)        height growth over the cycle, feet (matches legacy HTGF)
! =============================================================================
SUBROUTINE HTGF
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'
  INCLUDE 'CALCOM.f90'
  INCLUDE 'ARRAYS.f90'
  INCLUDE 'COEFFS.f90'
  INCLUDE 'CONTRL.f90'
  INCLUDE 'OUTCOM.f90'
  INCLUDE 'PLOT.f90'
  INCLUDE 'MULTCM.f90'
  INCLUDE 'HTCAL.f90'
  INCLUDE 'PDEN.f90'
  INCLUDE 'VARCOM.f90'

  REAL :: JHGB0(MAXSP), JHGB1(MAXSP), JHGB2(MAXSP), JHGB3(MAXSP)
  REAL :: JHGB4(MAXSP), JHGB5(MAXSP), JHGB6(MAXSP), JHGB7(MAXSP), JHGB8(MAXSP)
  COMMON /CN_HG/ JHGB0, JHGB1, JHGB2, JHGB3, JHGB4, JHGB5, JHGB6, JHGB7, JHGB8

  INTEGER :: I, ISPC, IYR, NYRS
  REAL    :: HT_C, CR_C, CCFL_C, CCH_C
  REAL    :: HT_MAX, B1, B2, B3, B4, B5, B6, B7, B8
  REAL    :: INC, ARG, ONE_MINUS, FACTOR
  REAL    :: CN_EMT_LOC, CN_TD_LOC, CN_CSI_LOC, CN_BGI_LOC
  LOGICAL :: LCLIM_LOC
  REAL, PARAMETER :: EPS = 1.0E-12

  CALL GETCLIM(CN_EMT_LOC, CN_TD_LOC, CN_CSI_LOC, CN_BGI_LOC, LCLIM_LOC)

  NYRS = MAX(1, NINT(FINT))

  DO I = 1, ITRN
    IF (PROB(I) <= 0.0) THEN
      HTG(I) = 0.0
      CYCLE
    END IF

    ISPC = ISP(I)
    IF (ISPC < 1 .OR. ISPC > MAXSP) THEN
      HTG(I) = 0.0
      CYCLE
    END IF

    HT_MAX = JHGB0(ISPC)
    B1 = JHGB1(ISPC); B2 = JHGB2(ISPC); B3 = JHGB3(ISPC); B4 = JHGB4(ISPC)
    B5 = JHGB5(ISPC); B6 = JHGB6(ISPC); B7 = JHGB7(ISPC); B8 = JHGB8(ISPC)

    ! CN-variant defensive defaults: when JHGB0 is zero or implausible,
    ! the constrained refit didn't produce a usable fit for this species.
    ! Use a generic conifer-shaped Chapman-Richards: max height ~120 ft,
    ! site-rate parameter B1=0.04/yr, allometric coupling B2=1.5.
    !
    ! Window is [30, 400] ft to accommodate the genuinely tall species:
    ! Doug-fir (325), redwood (~360), Sitka spruce (~310), giant sequoia
    ! (~310). Anything above 400 ft is a refit failure.
    IF (HT_MAX < 30.0 .OR. HT_MAX > 400.0) HT_MAX = 120.0
    IF (ABS(B1) < 1.0E-6) B1 = 0.04
    IF (ABS(B2) < 1.0E-6) B2 = 1.5
    IF (ABS(B3) < 1.0E-6) B3 = 0.5

    HT_C   = HT(I)
    CR_C   = REAL(ICR(I)) / 100.0
    IF (CR_C <= 0.0) CR_C = 0.05
    CCFL_C = PCT(I)
    CCH_C  = 0.0  ! CCH not wired yet; placeholder
    IF (CCH_C < 0.0) CCH_C = 0.0

    HTG(I) = 0.0
    DO IYR = 1, NYRS
      ARG = -B1 * HT_C - B4 * CCFL_C - B8 * SQRT(CCH_C) - B5 * ELEV
      IF (CN_TD_LOC > 0.0) ARG = ARG + B6 * SQRT(CN_TD_LOC)
      ARG = ARG + B7 * CN_EMT_LOC

      ONE_MINUS = 1.0 - EXP(-B1 * HT_C)
      IF (ONE_MINUS < EPS) ONE_MINUS = EPS

      FACTOR = HT_MAX * B1 * B2 * (CR_C**B3) * EXP(ARG) * (ONE_MINUS**(B2 - 1.0))

      ! Cap height increment at 5 ft/year to prevent runaway growth from
      ! pathological coefficient combinations. 5 ft/yr is at the extreme
      ! end of biological growth (e.g. fast-grown plantation pine).
      INC = MAX(FACTOR, 0.0)
      IF (INC > 5.0) INC = 5.0
      HTG(I) = HTG(I) + INC
      HT_C   = HT_C + INC
    END DO
  END DO

  RETURN
END SUBROUTINE HTGF

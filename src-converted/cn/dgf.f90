! =============================================================================
! src-converted/cn/dgf.f90
!
! CONUS unified variant diameter growth function. Implements Greg Johnson's
! integrated annual increment equation from
! https://github.com/gregjohnsonbiometrics/fvs_remodeling
! ported from R (est_dg in scripts/diameter_growth/diameter_growth_equations_for_conus.qmd).
!
! Equation form (per year, per tree):
!
!     dDBH = exp( B0
!                 + B1 * log( (DBH+1)^2 / (CR*HT+1)^B3 )
!                 + B2 * BAL^B4 / log(DBH+2.7)
!                 + B5 * ELEV
!                 )
!
! Coefficients are read from config/cn.json categories.johnson_dg into the
! JDGB0..JDGB6 arrays (one entry per variant species index).
!
! Inputs (via COMMON blocks populated upstream by RCON / DGDRIV):
!     ISPC          variant species index for this tree
!     DBH(I)        starting diameter, inches
!     HT(I)         starting total height, feet
!     ICR(I)        crown ratio, integer percent (we divide by 100 here)
!     PTBAA(ITRE(I)) basal area in larger trees, sq ft per acre
!     ELEV          stand elevation, feet (PLOT.f90)
!     EMT           stand extreme minimum temperature, deg C (PLOT.f90)
!     FINT          cycle length in years (CONTRL.f90)
!     PROB(I)       per-tree probability weight (>0 means tree is alive)
!
! Output:
!     WK2(I)        LN(change in squared diameter) over the cycle (matches
!                   legacy DGF convention)
!
! Author   Aaron Weiskittel
! License  Public domain (matches USFS FVS), GPL 2.0 for Greg's contribution
! =============================================================================
SUBROUTINE DGF(DIAM)
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

  REAL, INTENT(IN) :: DIAM(MAXTRE)

  ! Coefficient arrays loaded from cn.json by RCON
  REAL :: JDGB0(MAXSP), JDGB1(MAXSP), JDGB2(MAXSP), JDGB3(MAXSP)
  REAL :: JDGB4(MAXSP), JDGB5(MAXSP), JDGB6(MAXSP)
  COMMON /CN_DG/ JDGB0, JDGB1, JDGB2, JDGB3, JDGB4, JDGB5, JDGB6

  INTEGER :: I, ISPC, IYR, NYRS
  REAL    :: DBH_C, HT_C, CR_C, BAL_C, BAL_END, BAL_DELTA
  REAL    :: SIZE_NUM, SIZE_DEN, SIZE_LOG
  REAL    :: BAL_DEN, BAL_TERM, INC, DDS_TOT
  REAL    :: B0, B1, B2, B3, B4, B5, B6
  REAL    :: CN_EMT_LOC, CN_TD_LOC, CN_CSI_LOC, CN_BGI_LOC
  LOGICAL :: LCLIM_LOC
  REAL, PARAMETER :: EPS = 1.0E-12

  ! Read CN climate stash (defaults are CONUS-pooled)
  CALL GETCLIM(CN_EMT_LOC, CN_TD_LOC, CN_CSI_LOC, CN_BGI_LOC, LCLIM_LOC)

  ! Convert FINT (cycle length) to integer year count
  NYRS = MAX(1, NINT(FINT))

  DO I = 1, ITRN
    IF (PROB(I) <= 0.0) THEN
      WK2(I) = 0.0
      CYCLE
    END IF

    ISPC = ISP(I)
    IF (ISPC < 1 .OR. ISPC > MAXSP) THEN
      WK2(I) = 0.0
      CYCLE
    END IF

    ! Pull coefficients
    B0 = JDGB0(ISPC); B1 = JDGB1(ISPC); B2 = JDGB2(ISPC); B3 = JDGB3(ISPC)
    B4 = JDGB4(ISPC); B5 = JDGB5(ISPC); B6 = JDGB6(ISPC)

    ! Initialise per-tree state from input
    DBH_C = DIAM(I)
    HT_C  = HT(I)
    CR_C  = REAL(ICR(I)) / 100.0   ! ICR is integer percent, equation expects fraction
    IF (CR_C <= 0.0) CR_C = 0.05    ! guard for missing CR
    BAL_C = PTBAA(ITRE(I))

    ! BAL endpoint not directly available here; treat as constant within the
    ! cycle. (FVS legacy DGF also computes BAL once per cycle.) For multi-year
    ! cycles we hold BAL constant; the shape of the integrated fit was derived
    ! under the same assumption.
    BAL_END   = BAL_C
    BAL_DELTA = 0.0

    ! Annual loop
    DDS_TOT = 0.0
    DO IYR = 1, NYRS
      SIZE_NUM = (DBH_C + 1.0)**2
      SIZE_DEN = (CR_C * HT_C + 1.0)**B3
      IF (SIZE_DEN < EPS) SIZE_DEN = EPS
      SIZE_LOG = LOG(SIZE_NUM / SIZE_DEN)

      BAL_DEN = LOG(MAX(DBH_C + 2.7, 1.01))
      BAL_TERM = (MAX(BAL_C, 0.0)**B4) / BAL_DEN

      INC = EXP(B0 + B1 * SIZE_LOG + B2 * BAL_TERM + B5 * ELEV + B6 * CN_EMT_LOC)

      ! Accumulate change-in-squared-diameter (legacy DDS)
      DDS_TOT = DDS_TOT + (DBH_C + INC)**2 - DBH_C**2
      DBH_C = DBH_C + INC
      BAL_C = BAL_C + BAL_DELTA
    END DO

    ! Legacy expects WK2 = LN(DDS) over the full cycle
    IF (DDS_TOT > EPS) THEN
      WK2(I) = LOG(DDS_TOT)
    ELSE
      WK2(I) = -8.52   ! same floor used by the NC variant DGF
    END IF
  END DO

  RETURN
END SUBROUTINE DGF

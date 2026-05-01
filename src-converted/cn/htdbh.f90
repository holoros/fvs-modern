! =============================================================================
! src-converted/cn/htdbh_cn.f90
!
! CONUS unified variant height-diameter relationship. Replaces the NC clone
! that hardcoded Klamath / Sugar Pine / Douglas-Fir species.
!
! Curtis form (per species, three-parameter):
!
!     HT = 4.5 + exp( B0 + B1 * DBH**B2 )
!
! Equivalently:
!
!     DBH = ( (log(HT - 4.5) - B0) / B1 ) ** (1 / B2)
!
! Coefficients per species are read from config/cn.json
! categories.ht_dbh.HDB0 / HDB1 / HDB2 into the CN_HD COMMON block populated
! upstream by RCON.
!
! Inputs:
!   IFOR - forest code (unused for CN; species variation drives prediction)
!   ISPC - variant species index (1..MAXSP)
!   D    - diameter at breast height (inches)
!   H    - total height (feet)
!   MODE - 0 if D is provided and H is desired (HTDBH dubbing)
!          1 if H is provided and D is desired (REGENT inversion)
!
! Output:
!   H if MODE=0, D if MODE=1
!
! Falls back to a CONUS-wide pooled (B0=6.5, B1=-5.0, B2=-0.25) when the
! per-species coefficient is null OR implausible. The pooled fallback
! produces ~50 ft at DBH=10, ~67 ft at DBH=20, ~83 ft at DBH=30 — a
! reasonable generic conifer/hardwood profile.
!
! Sanity check: HDB0 outside [0, 8] is treated as a refit failure and the
! pooled fallback is used. The constrained refit emitted absurd intercepts
! (e.g. HDB0=99, 169, even -57) for ~8 species; without this guard those
! species would predict 10^43-foot heights, propagating NaN through dgf.
! See outputs/cn_mortality_audit.md for the list.
! =============================================================================
SUBROUTINE HTDBH (IFOR, ISPC, D, H, MODE)
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'

  INTEGER, INTENT(IN)  :: IFOR, ISPC, MODE
  REAL,    INTENT(INOUT) :: D, H

  ! CN_HD COMMON block populated from cn.json by RCON
  ! HDB0(MAXSP), HDB1(MAXSP) per-species; LHDOK(MAXSP) flags species presence.
  REAL    HDB0, HDB1, HDB2
  LOGICAL LHDOK
  COMMON /CN_HD/ HDB0(MAXSP), HDB1(MAXSP), HDB2(MAXSP), LHDOK(MAXSP)

  REAL :: B0, B1, B2, ARG

  ! ---------- coefficient selection -------------------------------------------
  IF (ISPC < 1 .OR. ISPC > MAXSP) THEN
    ! Out-of-range species: pooled CONUS fallback (Curtis form)
    B0 = 6.5
    B1 = -5.0
    B2 = -0.25
  ELSE IF (.NOT. LHDOK(ISPC)) THEN
    ! Species exists but no HD fit: pooled fallback
    B0 = 6.5
    B1 = -5.0
    B2 = -0.25
  ELSE IF (HDB0(ISPC) > 8.0 .OR. HDB0(ISPC) < 0.0) THEN
    ! Implausible Curtis intercept (refit failure). 8 species in the
    ! constrained refit emitted HDB0 values 49-169 or even negative.
    ! Without this guard those species predict heights of 10^21-10^43 ft,
    ! propagating NaN through dgf via SIZE_LOG.
    B0 = 6.5
    B1 = -5.0
    B2 = -0.25
  ELSE
    B0 = HDB0(ISPC)
    B1 = HDB1(ISPC)
    B2 = HDB2(ISPC)
    IF (ABS(B2) < 1.0E-6) B2 = -0.25
  END IF

  ! ---------- forward (D -> H) or inverse (H -> D) ---------------------------
  IF (MODE == 0) THEN
    ! H = 4.5 + exp(B0 + B1 * D**B2)  (3-parameter Curtis form, matches cn.json)
    IF (D <= 0.0) THEN
      H = 4.5
      RETURN
    END IF
    ARG = B0 + B1 * (D**B2)
    ! protect against extreme arguments
    IF (ARG > 12.0)  ARG = 12.0
    IF (ARG < -3.0)  ARG = -3.0
    H = 4.5 + EXP(ARG)
  ELSE
    ! D = ((LOG(H - 4.5) - B0) / B1) ** (1/B2)  (Curtis inverse)
    IF (H <= 4.5) THEN
      D = 0.1
      RETURN
    END IF
    ARG = (LOG(H - 4.5) - B0) / B1
    IF (ARG <= 0.0) THEN
      D = 0.1
    ELSE
      D = ARG ** (1.0 / B2)
      IF (D < 0.1) D = 0.1
      IF (D > 200.0) D = 200.0
    END IF
  END IF

  RETURN
END SUBROUTINE HTDBH

! =============================================================================
! src-converted/cn/htdbh_cn.f90
!
! CONUS unified variant height-diameter relationship. Replaces the NC clone
! that hardcoded Klamath / Sugar Pine / Douglas-Fir species.
!
! Wykoff form (per species, two-parameter):
!
!     HT = 4.5 + exp( B0 + B1 / (DBH + 1) )
!
! Equivalently:
!
!     DBH = 1 / ( (log(HT - 4.5) - B0) / B1 ) - 1
!
! Coefficients per species are read from config/cn.json
! categories.ht_dbh.HDB0 / HDB1 into the CN_HD COMMON block populated upstream
! by RCON.
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
! Falls back to a CONUS-wide pooled (B0=4.5, B1=-6.5) when the per-species
! coefficient is null in the JSON, matching how dgf.f90 falls back.
! =============================================================================
SUBROUTINE HTDBH (IFOR, ISPC, D, H, MODE)
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'

  INTEGER, INTENT(IN)  :: IFOR, ISPC, MODE
  REAL,    INTENT(INOUT) :: D, H

  ! CN_HD COMMON block populated from cn.json by RCON
  ! HDB0(MAXSP), HDB1(MAXSP) per-species; LHDOK(MAXSP) flags species presence.
  REAL    HDB0, HDB1
  LOGICAL LHDOK
  COMMON /CN_HD/ HDB0(MAXSP), HDB1(MAXSP), LHDOK(MAXSP)

  REAL :: B0, B1, ARG

  ! ---------- coefficient selection -------------------------------------------
  IF (ISPC < 1 .OR. ISPC > MAXSP) THEN
    ! Out-of-range species: pooled CONUS fallback
    B0 = 4.5
    B1 = -6.5
  ELSE IF (.NOT. LHDOK(ISPC)) THEN
    ! Species exists but no HD fit: pooled fallback
    B0 = 4.5
    B1 = -6.5
  ELSE
    B0 = HDB0(ISPC)
    B1 = HDB1(ISPC)
  END IF

  ! ---------- forward (D -> H) or inverse (H -> D) ---------------------------
  IF (MODE == 0) THEN
    ! H = 4.5 + exp(B0 + B1 / (D + 1))
    IF (D <= 0.0) THEN
      H = 4.5
      RETURN
    END IF
    ARG = B0 + B1 / (D + 1.0)
    ! protect against extreme arguments
    IF (ARG > 12.0)  ARG = 12.0
    IF (ARG < -3.0)  ARG = -3.0
    H = 4.5 + EXP(ARG)
  ELSE
    ! D = 1 / ((log(H - 4.5) - B0)/B1) - 1
    IF (H <= 4.5) THEN
      D = 0.1
      RETURN
    END IF
    ARG = LOG(H - 4.5) - B0
    IF (ABS(ARG) < 1.0E-6) THEN
      D = 100.0    ! asymptotic
    ELSE
      D = B1 / ARG - 1.0
      IF (D < 0.1) D = 0.1
    END IF
  END IF

  RETURN
END SUBROUTINE HTDBH

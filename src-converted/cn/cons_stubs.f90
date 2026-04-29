! =============================================================================
! src-converted/cn/cons_stubs.f90
!
! No-op coefficient-loader stubs for the CN variant. In legacy FVS variants
! (NC, IE, EM, etc.) the routines DGCONS, HTCONS, MORCON, CRCONS, REGCON are
! ENTRY points inside dgf.f90 / htgf.f90 / morts.f90 / crown.f90 / regent.f90
! that RCON calls once at simulation startup to load site-dependent
! coefficient arrays.
!
! In CN we don't need them. cn_coeffs_data.f90 (auto-generated BLOCK DATA from
! cn.json) populates the per-species CN_HD / CN_HL / CN_DH / CN_MO / CN_CR /
! CN_ING COMMON blocks at link time, before main() runs. The CONS routines
! are still called by base/grincr.f90 -> ie/rcon.f90 -> CALL DGCONS, so we
! provide no-op stubs to satisfy the linker.
!
! If a future cn variant ever needs runtime coefficient loading (e.g., to
! support hot-reloadable cn.json without recompile), these stubs become the
! call sites where LOAD_CN_JSON would dispatch.
! =============================================================================

SUBROUTINE DGCONS
  IMPLICIT NONE
  ! Diameter growth coefficients are populated at link time by BLOCK DATA
  ! CN_COEFFS_DATA (cn_coeffs_data.f90) into COMMON /CN_DG/ JDGB0..6.
  ! No runtime work required.
  RETURN
END SUBROUTINE DGCONS

SUBROUTINE HTCONS
  IMPLICIT NONE
  ! Height growth coefficients are populated at link time into COMMON /CN_HG/
  ! JHGB0..8. No runtime work required.
  RETURN
END SUBROUTINE HTCONS

SUBROUTINE MORCON
  IMPLICIT NONE
  ! Mortality coefficients populated at link time into COMMON /CN_MO/
  ! MOB0..3 + LMOOK flags.
  RETURN
END SUBROUTINE MORCON

SUBROUTINE CRCONS
  IMPLICIT NONE
  ! Crown / CR-change coefficients populated at link time into COMMON /CN_HL/
  ! (HTLC), /CN_DH/ (Δ HCB), /CN_CR/ (CR change). No runtime work.
  RETURN
END SUBROUTINE CRCONS

! Note: REGCON is provided as ENTRY REGCON inside cn/regent.f90 (NC scaffold).
! No stub needed here -- defining one would cause "multiple definition" at link.

SUBROUTINE CROWN
  IMPLICIT NONE
  ! Per-cycle crown-update entry called from base/grincr.f90 each simulation
  ! step. The CN-custom crown work happens via HTLC_INIT (initialization,
  ! crown.f90) and DHCB_STEP (annualized Hann-Hanus, crown.f90), called by
  ! dgdriv / htgr5 with per-tree state. This top-level CROWN is a no-op
  ! placeholder until the per-cycle dispatcher is wired in.
  RETURN
END SUBROUTINE CROWN


! Appended from climstash.f90 (CMakeLists list-parser dropped that filename)
! =============================================================================
! src-converted/cn/cn_clim_stash.f90
!
! CN climate stash. Holds the per-stand climate covariates used by the
! Greg Johnson DG and HG equations. Filled in at simulation startup either
! from a CLIMATE keyword block, from an external lookup against LAT/LON,
! or from CONUS-wide pooled defaults if neither is available.
!
! COMMON /CN_CLIM/ values:
!   CN_EMT     extreme minimum temperature, deg C (5-yr, 30-yr normal)
!   CN_TD      temperature difference (max - min), deg C
!   CN_CSI     Climate Site Index, dimensionless productivity proxy
!   CN_BGI     Brokaw growth index, alternate productivity proxy
!   LCLIM_OK   .TRUE. if the stand has a valid climate value loaded
!
! BLOCK DATA initializes to CONUS-wide pooled defaults:
!   CN_EMT = -23.6  (CONUS-wide median EMT, FIA panel)
!   CN_TD  =  31.4  (CONUS-wide median TD)
!   CN_CSI =  85.0  (CONUS-wide median Climate SI)
!   CN_BGI =  60.0  (CONUS-wide median BGI)
!   LCLIM_OK = .FALSE. (flag stays false until real values loaded)
!
! Routine SETCLIM may be called from KEYRDR-driven code path, or from a
! per-stand initialization step that looks up climate by LAT/LON.
! =============================================================================

BLOCK DATA CN_CLIM_DATA
  IMPLICIT NONE

  REAL    CN_EMT, CN_TD, CN_CSI, CN_BGI
  LOGICAL LCLIM_OK
  COMMON /CN_CLIM/ CN_EMT, CN_TD, CN_CSI, CN_BGI, LCLIM_OK

  DATA CN_EMT   /-23.6/
  DATA CN_TD    / 31.4/
  DATA CN_CSI   / 85.0/
  DATA CN_BGI   / 60.0/
  DATA LCLIM_OK / .FALSE. /

END BLOCK DATA CN_CLIM_DATA


SUBROUTINE SETCLIM (EMT_VAL, TD_VAL, CSI_VAL, BGI_VAL, LOK)
  ! Load per-stand climate covariates into the CN_CLIM stash. Called by
  ! grinit / RCON / a CLIMATE keyword handler at simulation startup.
  IMPLICIT NONE
  REAL,    INTENT(IN) :: EMT_VAL, TD_VAL, CSI_VAL, BGI_VAL
  LOGICAL, INTENT(IN) :: LOK

  REAL    CN_EMT, CN_TD, CN_CSI, CN_BGI
  LOGICAL LCLIM_OK
  COMMON /CN_CLIM/ CN_EMT, CN_TD, CN_CSI, CN_BGI, LCLIM_OK

  CN_EMT  = EMT_VAL
  CN_TD   = TD_VAL
  CN_CSI  = CSI_VAL
  CN_BGI  = BGI_VAL
  LCLIM_OK = LOK
  RETURN
END SUBROUTINE SETCLIM


SUBROUTINE GETCLIM (EMT_VAL, TD_VAL, CSI_VAL, BGI_VAL, LOK)
  ! Read the current CN_CLIM stash. Called by dgf / htgf when assembling
  ! the climate term in the integrated annual increment.
  IMPLICIT NONE
  REAL,    INTENT(OUT) :: EMT_VAL, TD_VAL, CSI_VAL, BGI_VAL
  LOGICAL, INTENT(OUT) :: LOK

  REAL    CN_EMT, CN_TD, CN_CSI, CN_BGI
  LOGICAL LCLIM_OK
  COMMON /CN_CLIM/ CN_EMT, CN_TD, CN_CSI, CN_BGI, LCLIM_OK

  EMT_VAL = CN_EMT
  TD_VAL  = CN_TD
  CSI_VAL = CN_CSI
  BGI_VAL = CN_BGI
  LOK     = LCLIM_OK
  RETURN
END SUBROUTINE GETCLIM

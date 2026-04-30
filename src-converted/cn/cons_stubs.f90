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



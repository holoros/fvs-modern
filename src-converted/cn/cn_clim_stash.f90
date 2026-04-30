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

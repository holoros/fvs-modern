! =============================================================================
! src-converted/cn/crown_cn.f90
!
! CONUS unified variant crown logic. Two routines:
!
!   HTLC_INIT    initialize height to live crown base from a logistic form
!                  HTLC = HT * 1 / (1 + exp(B0 + B1*HT + B2*CCFL
!                                            + B3*log(BAPA) + B4*DBH/HT
!                                            + B5*SI))
!                Coefficients per species from cn.json categories.htlc.HLB0..HLB5
!                via the CN_HL COMMON block.
!
!   DHCB_STEP    annualized Δ HCB from Hann-Hanus 2004 Eq 10 form. Called once
!                per year inside the FVS time-step loop. No site / age term --
!                Aaron's directive (no SICOND, no STDAGE).
!                  delta_yr = (HT * CR + delta_HT) /
!                             (1 + exp( b0 + b1*log(CR) + b2*CR
!                                       + b4*log(CCF) + b5*CR/CCF ))
!                Coefficients per species from cn.json categories.dhcb.DHB0..5
!                via the CN_DH COMMON block.
!
! These are the CN replacements for the NC-cloned crown.f90 placeholder.
! =============================================================================

! -----------------------------------------------------------------------------
SUBROUTINE HTLC_INIT(ISPC, DBH, HT, CCFL, BAPA, SI, HTLC)
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'

  INTEGER, INTENT(IN)  :: ISPC
  REAL,    INTENT(IN)  :: DBH, HT, CCFL, BAPA, SI
  REAL,    INTENT(OUT) :: HTLC

  REAL    HLB0, HLB1, HLB2, HLB3, HLB4, HLB5
  LOGICAL LHLOK
  COMMON /CN_HL/ HLB0(MAXSP), HLB1(MAXSP), HLB2(MAXSP), &
                  HLB3(MAXSP), HLB4(MAXSP), HLB5(MAXSP), LHLOK(MAXSP)

  REAL :: B0, B1, B2, B3, B4, B5, ETA, BAPA_USE

  IF (ISPC < 1 .OR. ISPC > MAXSP .OR. .NOT. LHLOK(ISPC)) THEN
    ! pooled CONUS fallback
    B0 = -1.0; B1 = 0.02; B2 = 0.005
    B3 = 0.30; B4 = 0.50; B5 = -0.005
  ELSE
    B0 = HLB0(ISPC); B1 = HLB1(ISPC); B2 = HLB2(ISPC)
    B3 = HLB3(ISPC); B4 = HLB4(ISPC); B5 = HLB5(ISPC)
  END IF

  IF (HT <= 4.5) THEN
    HTLC = 0.0
    RETURN
  END IF
  BAPA_USE = MAX(BAPA, 1.0)

  ETA = B0 + B1*HT + B2*CCFL + B3*LOG(BAPA_USE) &
       + B4*(DBH / MAX(HT, 1.0)) + B5*SI

  ! protect against overflow
  IF (ETA > 30.0)  ETA = 30.0
  IF (ETA < -30.0) ETA = -30.0

  HTLC = HT / (1.0 + EXP(ETA))
  IF (HTLC < 0.0)  HTLC = 0.0
  IF (HTLC > HT)   HTLC = HT
  RETURN
END SUBROUTINE HTLC_INIT


! -----------------------------------------------------------------------------
SUBROUTINE DHCB_STEP(ISPC, HT, CR, CCF, DELTA_HT, DELTA_HCB)
  ! Annualized Hann-Hanus crown recession. Called once per simulation year
  ! with running HT, CR (fraction 0-1), CCF, and the per-year delta in HT.
  IMPLICIT NONE

  INCLUDE 'PRGPRM.f90'

  INTEGER, INTENT(IN)  :: ISPC
  REAL,    INTENT(IN)  :: HT, CR, CCF, DELTA_HT
  REAL,    INTENT(OUT) :: DELTA_HCB

  REAL    DHB0, DHB1, DHB2, DHB4, DHB5
  LOGICAL LDHOK
  COMMON /CN_DH/ DHB0(MAXSP), DHB1(MAXSP), DHB2(MAXSP), &
                  DHB4(MAXSP), DHB5(MAXSP), LDHOK(MAXSP)

  REAL :: b0, b1, b2, b4, b5, CR_USE, CCF_USE, DENOM, ETA

  IF (ISPC < 1 .OR. ISPC > MAXSP .OR. .NOT. LDHOK(ISPC)) THEN
    ! pooled CONUS fallback (matches Hann-Hanus published Doug-fir starts)
    b0 = -5.77; b1 = -4.65; b2 = 6.97; b4 = 0.053; b5 = 126.0
  ELSE
    b0 = DHB0(ISPC); b1 = DHB1(ISPC); b2 = DHB2(ISPC)
    b4 = DHB4(ISPC); b5 = DHB5(ISPC)
  END IF

  CR_USE  = MAX(CR,  1.0E-6)
  CCF_USE = MAX(CCF, 1.0E-6)

  ETA = b0 + b1*LOG(CR_USE) + b2*CR_USE &
        + b4*LOG(CCF_USE) + b5*(CR_USE / CCF_USE)
  IF (ETA >  30.0) ETA =  30.0
  IF (ETA < -30.0) ETA = -30.0

  DENOM = 1.0 + EXP(ETA)

  DELTA_HCB = (HT * CR_USE + DELTA_HT) / DENOM
  IF (DELTA_HCB < 0.0)              DELTA_HCB = 0.0
  IF (DELTA_HCB > HT * CR_USE)      DELTA_HCB = HT * CR_USE
  RETURN
END SUBROUTINE DHCB_STEP

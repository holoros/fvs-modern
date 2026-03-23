!ODE SEGMENT BGCCOM
!----------
!  **BGCCOM -- BGC  DATE OF LAST REVISION: 10/15/99
!              Revised 11/12/02.  removing PPE includes. This change is in
!              conjunction with changes made to BGCGO, BGCGROW, BGCIN,
!              BGCINT, BGCFVS & BINITIAL. Variables IBGC, LBGCON and IBCYC
!              are no longer dimensioned by MXSTND.  The FVS-BGC extension
!              is now, once again, a single stand model.
!----------
!
!     LBGCON -- TRUE IF THE BGC EXTENSION IS ACTIVE, FALSE OTHERWISE
!
!      INCLUDE 'PPEPRM.F77' ! removed 11/02 ajm
REAL BGC_DG(1400), BGC_HG(1400), BGC_CR(1400), BGC_EXP(1400)
!      INTEGER IBGC(MXSTND), IBCYC(MXSTND)              ! removed 11/02 ajm
!      LOGICAL LBGCON(MXSTND)                           ! ditto
INTEGER IBGC, IBCYC
LOGICAL LBGCON
COMMON /BGCCOM/ LBGCON, IBGC, BGC_DG, BGC_HG, BGC_CR, BGC_EXP, &
                   IBCYC
!
!     END SEGMENT

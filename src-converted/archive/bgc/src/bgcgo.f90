 SUBROUTINE BGCGO(LBGCGO)
!----------
!  **BGCGO  BGC--DATE OF LAST REVISION: 05/29/01
!                Revised 11/12/02.  Removing index ISTND, and removing PPE
!                                   common "includes" (PPEPRM, PPCNTL, &
!                                   PRGPRM).  AJM
!                These changes--also made in BGCFVS, BGCGROW, BGCIN, BGCINT,
!                BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
!                The FVS-BGC code is now, once again, a single stand model.
!----------
!
!     TELLS FVS WHETHER OR NOT BGC IS ACTIVE (REGARDLESS OF WHETHER OR NOT
!     INCREMENTS WILL BE PASSED FROM BGC TO FVS).  PASSES LBGCON (AS LBGCGO) TO FVS.
!     WHETHER OR NOT BGC's INCREMENTS WILL BE PASSED TO FVS WILL BE DEALT WITH
!     INTERNALLY WITHIN BGC IN SUBROUTINE FVSBGC (VIA FLAG IBGC).
!
!     CALLED FROM: GRINCR
!
!OMMONS
!
!
INCLUDE 'BGCCOM.f90'
!      INCLUDE 'PPCNTL.F77'                       ! removed ajm 11/02
!      INCLUDE 'PRGPRM.F77'                       ! removed ajm 11/02
!
!----------------------
!
LOGICAL LBGCGO
!      IF(LBGCON(ISTND)) LBGCGO = .TRUE.          !removed 11/02 ajm
IF(LBGCON) LBGCGO = .TRUE.

RETURN
END

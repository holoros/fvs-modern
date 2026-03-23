SUBROUTINE BGCGRO(IY1,IY2)
!----------
!  **BGCGROW  BGC--DATE OF LAST REVISION: 3/13/00
!    Revised 11/12/02.  Removing index ISTND, and removing PPE common
!                       "includes" (PPEPRM, PPCNTL, & PRGPRM).  AJM
!           These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
!           BGCIN, and BGCCOM.f77--remove all PPE funtionality.
!           The FVS-BGC code is now, once again, a single stand model.
!----------
!
!     RUNS THE BGC EXTENSION
!
!     CALLED FROM: GRINCR
!
!OMMONS
!
INCLUDE 'BGCCOM.f90'
INCLUDE 'PRGPRM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PDEN.f90'
INCLUDE 'OUTCOM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CVCOM.f90'
!      INCLUDE 'PPCNTL.F77'                         ! removed 11/02 ajm
!
!OMMONS
!
!      print *, 'in BGCGROW, LBGCON=',LBGCON(ISTND)
!      IF(.NOT.LBGCON(ISTND)) RETURN                ! removed 11/02 ajm
IF(.NOT.LBGCON) RETURN
!----------------------------------------------------------------------
!  LOAD BGC ENTITY ARRAYS WITH FVS OUTPUT
!
!  SKIP CALLS TO BENTYLOAD AND BSITELOAD IF BGC INCREMENTS ARE TO
!  BE USED AND FVS CYCLE IS GREATER THAN ONE. INSTEAD CALL BENTYUPDT
!  WHICH WILL ADD NEW TREES FROM REGEN ESTAB MODEL AND UPDATE PROB
!  IN CASE THINNING HAS OCCURRED.
!----------------------------------------------------------------------
!      IF(IBGC(ISTND).EQ.1 .AND. ICYC.GT.1) THEN   ! removed 11/02 ajm
!        IBCYC(ISTND)=1                            ! ditto
IF(IBGC.EQ.1 .AND. ICYC.GT.1) THEN
  IBCYC=1
  CALL BENTYUPDT(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE, &
                    CLOW,CMED,CTALL,MAXCY1)
ELSE
!        IBCYC(ISTND)=0 !removed 11/02 ajm
  IBCYC=0
CALL BENTYLOAD(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE, &
                  CLOW,CMED,CTALL,MAXCY1)
CALL BSITELOAD(SLOPE,ASPECT,ELEV,TLAT,KODTYP,KODFOR)
END IF
!---------------------------------
!  RUN STAND-BGC
!---------------------------------
NUMYRS=IY2-IY1
CALL BSTNDBGCN(NUMYRS,ICYC)
RETURN
END

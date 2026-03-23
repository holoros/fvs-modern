SUBROUTINE BGCFVS(X)
!--------------------
!     BGCFVS BGC--DATE OF LAST REVISION: 05/29/01
!                 Revised 11/12/02.  Removing index ISTND, and removing PPE
!                                    common "includes" (PPEPRM, PPCNTL, &
!                                    PRGPRM).  AJM
!                 These changes--also made in BGCGO, BGCGROW, BGCIN, BGCINT,
!                 BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
!                 The FVS-BGC code is now, once again, a single stand model.
!                 Revised 6/3/03 AJM.  Added line re-setting WK2(I) to 0.0 if
!                                      there is no BGC-projected mortality
!--------------------
!
!     REPLACES DIAMETER, HEIGHT, CR AND PROB, IF USER INVOKES INTERACTIVE MODE
!     (i.e. IF KEYWORD BGCGROW IS USED).
!     CHANGES ARE MADE FROM HERE DIRECTLY TO FVS's COMMON BLOCK VARIABLES
!     THIS ROUTINE IS CALLED TWICE,  FIRST CALL DEALS WITH HT/DIAM/PROB; SECOND CALL
!     DEALS WITH CROWN RATIO.
!
!     IBGC(ISTND)[note: IBGC is no longer dimensioned!--11/02 AJM ] IS SET
!     TO 1 IN BGCIN IF BGC INCREMENTS ARE TO BE USED IN FVS.
!     X IS FLAG PASSED FROM FVS INDICATING WHICH INCREMENTS ARE READY TO BE UPDATED.
!     WHEN X=1, HT, DIAM, AND PROB ARE UPDATED; WHEN X=2, CROWN RATIOS ARE UPDATED.
!
!     CALLED FROM: GRADD OF FVS BASE CODE
!
!OMMONS
INCLUDE 'ENTITY.f90'
INCLUDE 'BGCCOM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
!      INCLUDE 'PPCNTL.F77'                            ! removed 11/02 ajm
INCLUDE 'PRGPRM.f90'
!
INTEGER X     ! FLAG FROM FVS DICTATING WHICH INCREMENT IT IS READY TO HAVE BGC UPDATE.
!     REAL BGC_DG(*), BGC_HG(*)
!---------------------
!     LOAD BGC INCREMENTS INTO FVS ARRAYS. VALUES ARE IN ENGLISH UNITS
!---------------------
!      IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 1) THEN       !removed ajm 11/02
IF (IBGC .EQ. 1 .AND. X .EQ. 1) THEN
   DO 100 I=1,ITRN
      DG(I)=-99.
      DO 200 J=1, NT
         IF(IDTREE(I).EQ.TREENO(J)) THEN
           DG(I)=PASSDG(J)
           HTG(I)=PASSHG(J)
           WK2(I)=0.0
           GO TO 100
         END IF
200       CONTINUE
   IF (DG(I) .LT. 0.0) THEN
      DG(I)=0.0
      HTG(I)=0.0
      WK2(I)=PROB(I)
   END IF
100    CONTINUE

!      ELSE IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 2) THEN  !removed ajm 11/02
ELSE IF (IBGC .EQ. 1 .AND. X .EQ. 2) THEN
  DO 300 I=1,ITRN
    DO 400 J=1, NT
      IF(IDTREE(I).EQ.TREENO(J)) THEN
        ICR(I)=IFIX(CR(J)*100)
        GO TO 300
      END IF
400     CONTINUE
300   CONTINUE
!
END IF
!
RETURN
END

SUBROUTINE BRTSTA
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRTSTA determines the status of each tree based on the worst
!  canker for the tree.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  15-SEP-2000 Lance R. David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 53) and species temp index variable (I3)
!     are new.
!  16-MAR-2001 Lance R. David (FHTET)
!     Corrected tree status (ITSTAT) assignment so that lowest status
!     for trees that have a total canker count (ITCAN) greater than 0
!     is 1 (non-lethal).
!  21-MAR-2001 Lance R. David (FHTET)
!     Added handling of escape tree status code 9. Once a tree is tagged
!     as an escape/reserve tree, it will never become infected.
!  01-MAY-2001 Lance R. David (FHTET)
!     Expanded tree category scalar variables to arrays.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM. Instead of just being and indicator of a
!     species being a host, BRSPM holds the array index value for that
!     species and is used to access all species-specific BR arrays.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

INTEGER I1, I2, I3, I4, II, J, KT, MAXNO, NTIM
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRTSTA',6,ICYC)
IF(DEBUG) WRITE(JOSTND,20) ICYC
20 FORMAT('Entering subroutine BRTSTA: cycle = ',I2)

!.... If no trees, return.
IF (ITRN .EQ. 0) GO TO 55

!.... Process host pines in the treelist.
!.... Start species loop

DO 53 I3 = 1, MAXSP

IF(BRSPM(I3) .EQ. 0) GO TO 53
!.... Set blister rust species index
I4=BRSPM(I3)

I1=ISCT(I3,1)
IF(I1.EQ.0) GO TO 53
I2=ISCT(I3,2)
DO 50 J=I1,I2
   KT=IND1(J)

!....    Initialize worst canker variable.
!....    Minimum status for trees with any cankers is 1 non-lethal.

   IF(ITCAN(KT) .GT. 0) THEN
      MAXNO=1
   ELSE
      MAXNO=0
   ENDIF

!....    Loop through cankers for the tree and find the worst one.
!....    Canker categories: 0=clean, 1=non-lethal, 2=prunable,
!....    3=excisable, 4=non-salvable or lethal, 5=topkilled,
!....    7=tree killed.

   DO 30 NTIM=1,ILCAN(KT)
      IF(ISTCAN(NTIM,KT).GT.MAXNO) THEN
         MAXNO=ISTCAN(NTIM,KT)
      ENDIF
30    CONTINUE

!....    Trees are not killed/topkilled on initialization.
!....    Highest status code possible is 4 (non-salvable).
!....    Status codes of 9 for reserved excape trees are not changed.
   IF(ICYC.EQ.0.AND.MAXNO.GT.4) MAXNO=4
   IF(IBRSTAT(KT).NE.9) IBRSTAT(KT)=MAXNO

!....    Add tree count to total number in that category. If tree has
!....    lethal cankers then reset tree class code.

   IF(IBRSTAT(KT).EQ.0 .OR. IBRSTAT(KT).EQ.9) THEN

!....       Tree is clean (no cankers) or reserved escape, add count

      TBRCLN(I4)=TBRCLN(I4)+PROB(KT)

!....       Loop through 2-inch DBH categories; add to the total for
!....       clean trees; if DBH not < 19", must be => 19"

      DO 31 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2CLN(II)=D2CLN(II)+PROB(KT)
            GO TO 32
         ENDIF
31       CONTINUE
      D2CLN(10)=D2CLN(10)+PROB(KT)
32       CONTINUE
   ELSE IF(IBRSTAT(KT).EQ.1) THEN

!....       Status of worst canker on tree is non-lethal, add count

      TBRNOL(I4)=TBRNOL(I4)+PROB(KT)

!....       Loop through 2-inch DBH categories; add to the total for
!....       non-lethal trees; if DBH not < 19", must be > 19"

      DO 33 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2NOL(II)=D2NOL(II)+PROB(KT)
            GO TO 34
         ENDIF
33       CONTINUE
      D2NOL(10)=D2NOL(10)+PROB(KT)
34       CONTINUE
   ELSE IF(IBRSTAT(KT).EQ.2) THEN

!....       Status of worst canker on tree is prunable, add count

      TBRPRN(I4)=TBRPRN(I4)+PROB(KT)

!....       Loop through 2-inch DBH categories; add to the total for
!....       prunable trees; if DBH not < 19", must be > 19"

      DO 35 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2PRN(II)=D2PRN(II)+PROB(KT)
            GO TO 36
         ENDIF
35       CONTINUE
      D2PRN(10)=D2PRN(10)+PROB(KT)
36       CONTINUE
   ELSE IF(IBRSTAT(KT).EQ.3) THEN

!....       Status of worst canker on tree is excisable, add count

      TBREXC(I4)=TBREXC(I4)+PROB(KT)

!....       Loop through 2-inch DBH categories; add to the total for
!....       excisable trees; if DBH not < 19", must be > 19"

      DO 37 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2EXC(II)=D2EXC(II)+PROB(KT)
            GO TO 38
         ENDIF
37       CONTINUE
      D2EXC(10)=D2EXC(10)+PROB(KT)
38       CONTINUE
   ELSE IF(IBRSTAT(KT).EQ.4) THEN

!....       Status of worst canker on tree is non-salvable, add count

      TBRNOS(I4)=TBRNOS(I4)+PROB(KT)

!....       Loop through 2-inch DBH categories; add to the total for
!....       non-salvable trees; if DBH not < 19", must be > 19"

      DO 39 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2NOS(II)=D2NOS(II)+PROB(KT)
            GO TO 40
         ENDIF
39       CONTINUE
      D2NOS(10)=D2NOS(10)+PROB(KT)
40       CONTINUE
   ELSE IF(IBRSTAT(KT).EQ.5) THEN

!....       Status of worst canker on tree is topkilled, add count

      TBRGIR(I4)=TBRGIR(I4)+PROB(KT)
      IMC(KT)=3

!....       Loop through 2-inch DBH categories; add to the total for
!....       topkill trees; if DBH not < 19", must be > 19"

      DO 41 II=1,9
         IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
            D2GIR(II)=D2GIR(II)+PROB(KT)
            GO TO 42
         ENDIF
41       CONTINUE
      D2GIR(10)=D2GIR(10)+PROB(KT)
42       CONTINUE
   ENDIF

!....    After a compression, it is possible for a record to represent
!....    both live and dead trees; therefore, BRPB must be added for
!....    all trees.  BRPB will be 0 if the tree is not dead.

   TBRMRT(I4)=TBRMRT(I4)+BRPB(KT)

!....    Loop through 2-inch DBH categories; add to the total for
!....    dead trees; if DBH not < 19" or less, must be > 19"

   DO 43 II=1,9
      IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
         D2DED(II)=D2DED(II)+BRPB(KT)
         GO TO 44
      ENDIF
43    CONTINUE
   D2DED(10)=D2DED(10)+BRPB(KT)
44    CONTINUE

!....    Calculate total number of trees per acre that are white pine.
!....    This should be the sum of all the canker status categories
!....    listed above excluding trees killed by blister rust this cycle.

   TBRHST(I4)=TBRHST(I4)+PROB(KT)+BRPB(KT)

!....    Loop through 2-inch DBH categories; add to the total for
!....    all white pines; if DBH not < 19" or less, must be > 19"

   DO 45 II=1,9
      IF(DBH(KT).LT.(FLOAT(II*2+1))) THEN
         D2WP(II)=D2WP(II)+PROB(KT)
         GO TO 46
      ENDIF
45    CONTINUE
   D2WP(10)=D2WP(10)+PROB(KT)
46    CONTINUE
50 CONTINUE

!.... End species loop.
53 CONTINUE

!.... Common return.

55 CONTINUE
IF(DEBUG) WRITE(JOSTND,60) ICYC
60 FORMAT('Leaving subroutine BRTSTA: cycle = ',I2)
RETURN
END

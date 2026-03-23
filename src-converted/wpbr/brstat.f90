SUBROUTINE BRSTAT
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRSTAT calculates value for proportion of trees infected with
!  cankers; average number cankers per tree.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  20-MAY-1999 Lance David
!     Added debug code.
!  14-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 33) and species temp index variable (I3)
!     are new.
!  21-MAR-2001 Lance David (FHTET)
!     Increased limit on PITCA from 0.99 to 0.999 so reported value
!     rounds to 1.00 and log function on BRTARG still will not fail.
!  03-MAY-2001 Lance R. David (FHTET)
!     expanded processing for arrays with species dimension.
!     * * * The 2 inch DBH class tables include all host species
!     * * * and have not expanded to be species-specific like the
!     * * * summary statistics table.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM. Instead of just being and indicator of a
!     species being a host, BRSPM holds the array index value for that
!     species and is used to access all species-specific BR arrays.
!  11-MAY-2001 Lance R. David (FHTET)
!     Changed calculation of PITCA and PILCA to include TBRHMR because
!     these values should reflect historical mortality that may no longer
!     be carried in FVS due to a compression of the tree list.
!  10-MAY-2006 Lance R. David (FHTET)
!     Added debug.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'

!.... Local variable declarations.

INTEGER I1, I2, I3, I4, II, ILLCAN, ISC, J, K, M, NLCAN
LOGICAL DEBUG
REAL TPROB2(10),TRELN2(10),TRETN2(10),SUMNL2(10),SUMNT2(10), &
        SUMEC2(10),TRELN(NBRSP),SUMNL(NBRSP),SUMNT(NBRSP), &
        SUMEC(NBRSP),CURDBH, PROBJ

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRSTAT',6,ICYC)
IF(DEBUG) WRITE(JOSTND,111) ICYC
111 FORMAT('Entering subroutine BRSTAT: cycle = ',I2)

!.... Initialize variables for this cycle.

DO I4 = 1, NBRSP
   THPROB(I4)=0.0
   TRETN(I4)=0.0
   TRELN(I4)=0.0
   SUMNT(I4)=0.0
   SUMNL(I4)=0.0
   SUMEC(I4)=0.0
   PITCA(I4)=0.0
   PILCA(I4)=0.0
   AVTCPT(I4)=0.0
   AVLCPT(I4)=0.0
   AVECPT(I4)=0.0
END DO

DO 5 II=1,10
   TPROB2(II)=0.0
   TRELN2(II)=0.0
   TRETN2(II)=0.0
   SUMNL2(II)=0.0
   SUMNT2(II)=0.0
   SUMEC2(II)=0.0
   D2PIT(II)=0.0
   D2PIL(II)=0.0
   D2AVT(II)=0.0
   D2AVL(II)=0.0
   D2AVE(II)=0.0
5 CONTINUE

!.... If no tree records, then return.

IF(ITRN.EQ.0) GO TO 100

!.... Process host trees. If none, then return.  If tree
!.... is dead, then don't process it.

!.... Start species loop

DO 33 I3 = 1, MAXSP

IF (BRSPM(I3) .EQ. 0) GO TO 33
I1=ISCT(I3,1)

!.... Set blister rust species index
I4=BRSPM(I3)

IF(I1.EQ.0) GO TO 32
I2=ISCT(I3,2)

DO 30 K=I1,I2

!....    Set index and prob for calculations; get current DBH - needed
!....    for 2" DBH class table output.

   J=IND1(K)
   PROBJ=PROB(J)
   CURDBH=DBH(K)

   IF(DEBUG) WRITE(JOSTND,*) 'IN BRSTAT: J=',J, &
      ' BRSPC=',BRSPC(I4),' PROBJ=',PROBJ,' CURDBH =',CURDBH, &
      ' ESTCAN=',ESTCAN(J),' ILCAN=',ILCAN(J), &
      ' IBRSTAT=',IBRSTAT(J)

!....    If dead, skip out.

   IF(IBRSTAT(J).EQ.7) GO TO 30

!....    Sum total number of trees and total trees within each 2" DBH
!....    class; if current DBH not < 19", must be => 19"

   THPROB(I4)=THPROB(I4)+PROBJ
   DO 10 II=1,9
      IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
         TPROB2(II)=TPROB2(II)+PROBJ
         GO TO 11
      ENDIF
10    CONTINUE
   TPROB2(10)=TPROB2(10)+PROBJ
11    CONTINUE

!....    Sum total number of estimated cankers per tree and total
!....    estimated cankers within each 2" DBH class; if DBH not < 19",
!....    must be => 19"

   SUMEC(I4)=SUMEC(I4)+(ESTCAN(J)*PROBJ)
   DO 12 II=1,9
      IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
         SUMEC2(II)=SUMEC2(II)+(ESTCAN(J)*PROBJ)
         GO TO 13
      ENDIF
12    CONTINUE
   SUMEC2(10)=SUMEC2(10)+(ESTCAN(J)*PROBJ)
13    CONTINUE


!....    If no lethal cankers, skip out.

   ILLCAN=0
   NLCAN=ILCAN(J)
   IF(NLCAN.EQ.0) GO TO 30

!....    If tree has potentially lethal cankers (i.e. the ILCAN array),
!....    process canker array to accumulate truly lethal cankers for
!....    current tree. Canker status of 4 and 5 are truly lethal.

   DO 20 M=1,NLCAN
      ISC=ISTCAN(M,J)
      IF(ISC.EQ.4.OR.ISC.EQ.5) THEN

!....          Increment sum of lethal cankers for the tree.

         ILLCAN=ILLCAN+1
      ENDIF
20    CONTINUE

!....    Sum total number of trees with lethal cankers and total trees
!....    within each 2" DBH class with lethal cankers

   IF(ILLCAN.GT.0) THEN
      TRELN(I4)=TRELN(I4)+PROBJ
      DO 21 II=1,9
         IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
            TRELN2(II)=TRELN2(II)+PROBJ
            GO TO 22
         ENDIF
21       CONTINUE
      TRELN2(10)=TRELN2(10)+PROBJ
22       CONTINUE
   ENDIF

!....    Sum total number of trees with lethal and non-lethal cankers
!....    and total within each 2" DBH class with both canker types

   IF(ITCAN(J).GT.0) THEN
      TRETN(I4)=TRETN(I4)+PROBJ
      DO 23 II=1,9
         IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
            TRETN2(II)=TRETN2(II)+PROBJ
            GO TO 24
         ENDIF
23       CONTINUE
      TRETN2(10)=TRETN2(10)+PROBJ
24       CONTINUE
   ENDIF

!....    Sum total number of lethal cankers per tree and total lethal
!....    cankers within each 2" DBH class

   SUMNL(I4)=SUMNL(I4)+FLOAT(ILLCAN)*PROBJ
   DO 25 II=1,9
      IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
         SUMNL2(II)=SUMNL2(II)+(FLOAT(ILLCAN)*PROBJ)
         GO TO 26
      ENDIF
25    CONTINUE
   SUMNL2(10)=SUMNL2(10)+(FLOAT(ILLCAN)*PROBJ)
26    CONTINUE

!....    Sum total number of lethal and non-lethal cankers per tree and
!....    total of both cankers within each 2" DBH class

   SUMNT(I4)=SUMNT(I4)+FLOAT(ITCAN(J))*PROBJ
   DO 27 II=1,9
      IF(CURDBH.LT.(FLOAT(II*2+1))) THEN
         SUMNT2(II)=SUMNT2(II)+(FLOAT(ITCAN(J))*PROBJ)
         GO TO 28
      ENDIF
27    CONTINUE
   SUMNT2(10)=SUMNT2(10)+(FLOAT(ITCAN(J))*PROBJ)
28    CONTINUE

!.... End of tree loop within species loop.
30 CONTINUE

IF(THPROB(I4).GT.0) THEN

!....    Calculate proportion of trees infected with lethal and
!....    total (lethal + nonlethal) cankers.
!....    PITCA has a upper limit of 0.999 imposed so the calculation of
!....    the stand deviation factor (DFACT) in subroutine BRTARG will
!....    not error on the log function.

   PITCA(I4)=(TRETN(I4)+TBRHMR(I4))/(THPROB(I4)+TBRHMR(I4))
   IF(PITCA(I4).GT.0.999) PITCA(I4)=0.999
   PILCA(I4)=(TRELN(I4)+TBRHMR(I4))/(THPROB(I4)+TBRHMR(I4))

!....    Calculate average number of cankers per tree per acre
!....    (all cankers, lethal cankers, and expected cankers).

   AVTCPT(I4)=SUMNT(I4)/THPROB(I4)
   AVLCPT(I4)=SUMNL(I4)/THPROB(I4)
   AVECPT(I4)=SUMEC(I4)/THPROB(I4)
ENDIF

32 CONTINUE

IF(DEBUG) THEN
   WRITE(JOSTND,37) BRSPC(I4),THPROB(I4),TBRHMR(I4),PITCA(I4), &
            PILCA(I4),AVTCPT(I4),AVLCPT(I4),AVECPT(I4)
37    FORMAT('IN BRSTAT: BRSPC=',A4,' THPROB=',F7.2, &
            ' TBRHMR=',F7.2,' PITCA=',F5.3, &
            ' PILCA=',F5.3,' AVTCPT=',F7.3,' AVLCPT=',F5.3, &
            ' AVECPT=',F5.3)
ENDIF

!.... End of species loop
33 CONTINUE

!.... Loop through the 10 2" DBH classes.
!.... * * * This 2 inch class table is not species specific. * * *

DO 50 II=1,10
   IF(TPROB2(II).GT.0) THEN

!...        Calculate proportion of trees infected with lethal and
!....       lethal + non-lethal cankers by 2" DBH class.

      D2PIT(II)=TRETN2(II)/TPROB2(II)
      D2PIL(II)=TRELN2(II)/TPROB2(II)

!....       Calculate average number of cankers per tree (total,
!....       lethal, and expected) by 2" DBH classes.

      D2AVT(II)=SUMNT2(II)/TPROB2(II)
      D2AVL(II)=SUMNL2(II)/TPROB2(II)
      D2AVE(II)=SUMEC2(II)/TPROB2(II)
   ENDIF
50 CONTINUE

!.... Common return.

100 CONTINUE
IF(DEBUG) WRITE(JOSTND,113) ICYC
113 FORMAT('Leaving subroutine BRSTAT: cycle = ',I2)
RETURN
END

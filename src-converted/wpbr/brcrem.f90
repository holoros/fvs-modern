SUBROUTINE BRCREM
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRCREM removes cankers that have been classified as inactive
!  or non-lethal.  Also removes excisable cankers when excising
!  is scheduled and prunable cankers when pruning is scheduled.
!  The tree's height to base of crown is reset for trees that
!  have been pruned.
!  Crown ratio carried by Prognosis is not changed.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  24-MAY-1999 Lance David
!     Added debug code to track pruning.
!  14-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 200) and species temp index variable (I3)
!     are new.
!  05-MAR-2001 Lance David (FHTET)
!     Added pathological pruning control so that crown base height
!     will remain unchanged when a branch canker is pruned off.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM.
!  09-MAY-2006 Lance R. David (FHTET)
!     Changed IND2 to ICNDX.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'

!.... Local variable declarations.

INTEGER I1, I2, I3, ICNDX(10), ITSTAT, IVAC, J, K, M, NLCAN, &
           ICANT
REAL HITE, PRHT, PRHTST, VALUE
LOGICAL DEBUG,PRUNED

!.... Is debug requested?

CALL DBCHK(DEBUG,'BRCREM',6,ICYC)
IF(DEBUG) WRITE(JOSTND,111) ICYC
111 FORMAT('Entering subroutine BRCREM: cycle = ',I2)

!.... If there are no trees, exit subroutine.

IF (ITRN .EQ. 0) GO TO 300

!.... Process host species, if any.
!.... Start species loop

DO 200 I3 = 1, MAXSP

IF (BRSPM(I3) .EQ. 0) GO TO 200

I1=ISCT(I3,1)
IF(I1.EQ.0) GO TO 200
I2=ISCT(I3,2)

DO 30 K=I1,I2
   J=IND1(K)
   PRUNED=.FALSE.
   IVAC=0
   HITE=HT(J)
   NLCAN=ILCAN(J)

!....    Get tree's status, based on the worst canker on the tree.

   ITSTAT=IBRSTAT(J)
   IF(ITSTAT.EQ.7) THEN

!....       Tree status is dead (7).  No treatment is performed.

      GO TO 30
   ELSE IF(ITSTAT.EQ.0.AND.LPRGO.AND.LCLEN) THEN

!....       Prune clean tree.

       PRUNED=.TRUE.
   ELSE

!....       Process cankers for this tree.
!....       Tree status is anything from clean (0) to non-salvable (5).
!....       Clear array that will hold canker indexes to be removed.

      DO 10 M=1,10
         ICNDX(M)=0
10       CONTINUE

      DO 40 M=1,NLCAN
         ICNDX(M)=M

!....          Get this canker's status.

         ICANT=ISTCAN(M,J)
         IF(ICANT.EQ.-1.OR.ICANT.EQ.1) THEN

!....             Canker is inactive or non-lethal, signal vacancy
!....             in array.  Remove inactive or non-lethal cankers for
!....             all tree statuses (except dead, of course) because
!....             even on non-salvable (4) and topkill (5) trees the
!....             non-lethal cankers continue to grow and soon become
!....             "not" non-lethal.

            IVAC=IVAC+1
            ICNDX(M)=-M
         ELSE IF(ICANT.EQ.3.AND.LEXGO.AND.ITSTAT.LE.3) THEN

!....             This is an excisable canker and excising is scheduled.
!....             Don't excise non-salvable (4) and topkill (5) trees.
!....             If random number is less than excising success rate,
!....             the canker will be removed.  If random number is
!....             greater than or equal to the success rate, the canker
!....             has escaped the excising process and the percent
!....             girdle is reset to 1 percent.

!....             First check to see if there are multiple excisable
!....             cankers on the tree in which case no excise will
!....             happen at all (LEXMLT is determined in BRCGRO and
!....             and BRTSTA).

            IF(.NOT.LEXMLT(J)) THEN

!....                This must be the only excisable canker on this
!....                tree; go ahead and test against a random number.

               CALL BRANN(VALUE)
               IF(VALUE.LT.SRATE(2)) THEN
                  IVAC=IVAC+1
                  ICNDX(M)=-M
               ELSE
                  GIRDL(M,J)=1.0
               ENDIF
            ENDIF

            IF(LPRGO.AND.LCLEN) THEN

!....                Pruning is scheduled and pruning of clean
!....                trees is also specified. Because pruning is
!....                done on cankerless trees, it will also be
!....                done on trees that have excisable cankers.

               PRUNED=.TRUE.
            ENDIF
         ELSE IF(ICANT.EQ.2.AND.LPRGO.AND.ITSTAT.LE.3) THEN

!....             This is a prunable canker and pruning is scheduled.
!....             Don't prune non-salvable (4) and topkill (5) trees.
!....             If random number is less than pruning success rate,
!....             the canker will be removed.

            PRUNED=.TRUE.
            CALL BRANN(VALUE)
            IF(VALUE.LT.SRATE(1)) THEN
               IVAC=IVAC+1
               ICNDX(M)=-M
            ENDIF
         ENDIF
40       CONTINUE
   ENDIF

   IF(PRUNED .AND. .NOT. LPATPR) THEN

!....       Pathological pruning (LPATPR = true) will not change
!....       the height to base of crown for a tree. If set false,
!....       reset the tree's height to base of crown, if it is
!....       not already higher than the absolute pruning height.
!....       Absolute pruning height for this tree must be
!....       calculated.
!....       NOTE: units are in centimeters

      PRHT=HTMAX(1)
      PRHTST=HITE*HTPRPR*30.48
      IF(PRHTST.GT.PRHT) PRHTST=PRHT
      IF(DEBUG) THEN
         WRITE(JOSTND,*) &
            ' tree pruned: BRHTBC old=',BRHTBC(J), &
            ' new(if>)=',PRHTST
      ENDIF
      IF(BRHTBC(J).LT.PRHTST) BRHTBC(J)=PRHTST
   ENDIF

   IF(IVAC.GT.0) THEN

!....       Vacancies have been created in this tree's canker
!....       arrays by removing cankers.
!....       Call BRCDEL to reorganize arrays.

      CALL BRCDEL(IVAC,ICNDX,J)
   ENDIF
30 CONTINUE

!.... End of species loop
200 CONTINUE

!.... Common return.

300 CONTINUE
IF(DEBUG) WRITE (JOSTND,333) ICYC
333 FORMAT('Leaving subroutine BRCREM: cycle = ',I2)
RETURN
END

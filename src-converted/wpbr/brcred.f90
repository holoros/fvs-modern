SUBROUTINE BRCRED
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRCRED reduces the crowns of white pines that have been girdled
!  by Blister Rust at initialization from canker data provided or
!  during the current growth cycle.
!  Reduction in crowns during the initialization phase of FVS tree data
!  is accomplished in CROWN.
!  This subroutine is called by:
!     BRSETP
!     BRTREG
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  11-MAY-1999 Lance David
!     Added debug for calculation of new crown ratio.
!  13-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 95) and species temp index variable (I3)
!     are new.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

INTEGER I, I1, I2, I3, ICCR, IICI, K
REAL CL, HD, HN
LOGICAL DEBUG,BRGO

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRCRED',6,ICYC)
IF(DEBUG) WRITE(JOSTND,111) ICYC
111 FORMAT('Entering subroutine BRCRED: cycle = ',I2)

!.... If the Blister Rust model is not active, then return.

CALL BRATV(BRGO)
IF(.NOT.BRGO) GO TO 100

!.... Process host species. If no trees, return.

IF (ITRN .EQ. 0) GO TO 100

!.... Start species loop

DO 95 I3 = 1, MAXSP

IF (BRSPM(I3) .EQ. 0) GO TO 95

I1=ISCT(I3,1)
IF(I1.EQ.0) GO TO 95
I2=ISCT(I3,2)
DO 90 K=I1,I2
   I=IND1(K)
   ICCR=ICR(I)

!....    If tree is dead, go to the next one.

   IF(IBRSTAT(I).EQ.7) GO TO 90

!....    If tree is not girdled, go to the next one.

   IF(ITRUNC(I).EQ.0.OR.ICRED(I).EQ.0) GO TO 90

!....    Reduce crowns of trees flagged as top-killed.
!....    NORMHT and ITRUNC are integer representing 1/100s feet.
!....    Min and Max crown ratio is set to 5 and 95 percent.
!....    HN is normal height (feet)
!....    HD is length of top that is dead (feet)

   HN=NORMHT(I)/100.0
   HD=HN-(ITRUNC(I)/100.0)
   CL=(FLOAT(ICCR)/100.)*HN-HD
   IICI=IFIX((CL*100./HN)+.5)
   IF(IICI.LT.5) IICI=5
   IF(IICI.GT.95) IICI=95
   ICR(I) = IICI
   ICRED(I)=0

!....    Write debug information.
   IF(DEBUG) THEN
      WRITE (JOSTND,85) I,IDTREE(I),NORMHT(I),ITRUNC(I),CL, &
                           ICCR,ICR(I)
85       FORMAT ('I=',I4,' IDTREE=',I5,' NORMHT=',I5,' ITRUNC=',I5, &
                 ' CL=',F5.1,' OLDCR=',I3,' NEWCR=',I3)
   ENDIF
90 CONTINUE

!.... End species loop
95 CONTINUE

!.... Common return.

100 CONTINUE
IF(DEBUG) WRITE(JOSTND,114) ICYC
114 FORMAT('Leaving subroutine BRCRED: cycle = ',I2)
RETURN
END

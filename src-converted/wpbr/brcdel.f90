SUBROUTINE BRCDEL(IVACT,INDXBR,J)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  This routine deletes cankers by moving them from the bottom of the
!  list to fill in empty canker record locations at the top of the list.
!  Vacant records were labelled in the calling routine by setting the
!  sign of those spots in the index negative.  The value of IVACT is
!  passed in as the number of negative members of the index.
!  This routine is called by BRCREM.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  10-MAY-1999 Lance R. David
!     Changed variable INDEX (which is a function name) to INDXBR.
!  08-NOV-2002 Lance R. David (FHTET)
!     Removed subtraction of deleted cankers from the total canker count.
!  09-MAY-2006 Lance R. David (FHTET)
!     Change INDXBR to inherit dimension. Initilization was missing (I)
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

LOGICAL DEBUG
INTEGER INDXBR(*), J, IR, IREC, IV, IVAC, IVACT

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRCDEL',6,ICYC)
IF(DEBUG) WRITE(JOSTND,9) ICYC
9 FORMAT('Entering subroutine BRCDEL: cycle = ',I2)

!.... Sort the index list such that vacancy pointers are at the top
!.... of the list.  The absolute values of these pointers point to
!.... the vacancies in descending order.

CALL IQRSRT(INDXBR,ILCAN(J))

!.... Initialize the pointers to the indices of vacancies and
!.... trees.  IVACT points to the end of the vacancy pointers.

IV=IVACT+1
IR=ILCAN(J)+1

!.... Top of the tree/vacancy loop.

10 CONTINUE

!.... Decrement the index to the vacancy pointers.
!.... If there are no more vacancies, skip out.

IV=IV-1
IF(IV.LT.1) GO TO 20

!.... Decrement the index to tree pointers.
!.... If there are no more trees, skip out.

IR=IR-1
IF(IR.LE.IVACT) GO TO 20

!.... Load the pointers to the vacancy and the tree records.
!.... If the vacancy pointer is greater than the tree pointer, skip out.

IVAC=-INDXBR(IV)
IREC=INDXBR(IR)
IF(IVAC.GT.IREC) GO TO 20

!.... Move the data from position IREC to position IVAC.

DUP(IVAC,J)=DUP(IREC,J)
DOUT(IVAC,J)=DOUT(IREC,J)
GIRDL(IVAC,J)=GIRDL(IREC,J)
ISTCAN(IVAC,J)=ISTCAN(IREC,J)

!.... Go on to the next tree/vacancy.

GO TO 10

!.... Update the lethal and total canker counts.

20 CONTINUE
ILCAN(J)=ILCAN(J)-IVACT

!.... Total canker count represents a cummulative value, so deleted
!.... cankers should not be subtracted per Geral Mcdonald 08-NOV-2002
!.... Lance David
!.... ITCAN(J)=ITCAN(J)-IVACT

!.... Common return.

IF(DEBUG) WRITE(JOSTND,22) ICYC
22 FORMAT('Leaving subroutine BRCDEL: cycle = ',I2)
RETURN
END

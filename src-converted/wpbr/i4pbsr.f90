SUBROUTINE I4PBSR (N,A,IORD,F,IP)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  This is an INTEGER*4 keyed binary search.
!  Part of the Stand Prognisis Model (Forest Vegetation Simulator)
!  adapted from INTEGER search - NL Crookston - Jan. 1981 - Moscow, ID
!
!  Binary search routine:  finds the subscript location of F in
!  array A, if IORD is filled with the ascending order indices of
!  array A, rather than the descending order indices.  Array A need
!  not be in ascending order as long as the keys in IORD are in
!  ascending order over A.
!----------------------------------------------------------------------
!
!  Local variable definitions:
!    N    = the length of A.
!    A    = a list of INTEGER*4 values
!    IORD = an array of keys sorted in ascengin order over A.
!    F    = an INTEGER*4 value which you wish to find in A.
!    IP   = the position in A where F was found; or 0 when F is not
!           a member of A.
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Local variable declarations.

INTEGER*4 A,F
INTEGER IORD,ITOP,IBOT,IMID,IP,I1,N,IB,IM,IN,IT
DIMENSION A(1),IORD(1)

!.... Initializations.

IMID=1
I1=IORD(1)

!.... If the value we search for is less than the smallest value in the
!.... array, then skip out.

IF(F.LE.A(I1)) GO TO 40
IMID=N
IN=IORD(N)

!.... If the value we search for is greater tha  the largest value in
!.... the array, then skip out.

IF(F.GE.A(IN)) GO TO 40

!.... Initialize the top and bottom of the search partition to the top
!.... and bottom of the array.

ITOP=1
IBOT=N

!.... Test the middle value of the partition.

20 CONTINUE
IMID=(IBOT+ITOP)/2
IM=IORD(IMID)

!.... If the middle value is greater than or equal to the search value,
!.... set the top of the partition to the middle value plus one.

IF(F.GT.A(IM)) GO TO 30

!.... If the middle value is less than what we search for, set the
!.... bottom of the partition to the middle value minus one.

IBOT=IMID-1
IB=IORD(IBOT)

!.... If the value of the bottom of the partition is greater than or
!.... equal to the search value, then skip out.

IF(F.GT.A(IB)) GO TO 40

!.... Re-search the partition.

GO TO 20

!.... Set the top of the partition to the middle value plus one.

30 CONTINUE
ITOP=IMID+1
IT=IORD(ITOP)

!.... If the value of the top of the partition is less than or equal
!.... to the search value, the skip out.

IF(F.LT.A(IT)) GO TO 40

!.... Re-search the partition.

GO TO 20

!.... This is the exit point for the routine.  If we got here, we
!.... either found the search value as the middle of a partition,
!.... or partitioned to the point where the search value could
!.... not be contained in the partition.  At this point, we don't know
!.... which so we make a final check, and return zero if not found.

40 CONTINUE
IP=IORD(IMID)
IF(F.NE.A(IP)) IP=0
RETURN
END

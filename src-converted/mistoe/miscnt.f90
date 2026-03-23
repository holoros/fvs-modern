SUBROUTINE MISCNT(MSPCNT)
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Counts the number of tree records with any mistletoe infection
!  greater than 0 and returns the values by species in array MSPCNT.
!  Note: MSPCNT species are ordered according to IREF, i.e. the order
!  of occurance of species in the treelist.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     MSPCNT: (O) Array of number of DM infected records by species.
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on or off.
!     I:      Loop counter.
!     ISPTR:  Pointer to current tree record's species number in
!                species ordered sort list.
!
!  Common block variables and parameters:
!     ICYC:   From CONTRL; cycle index number.
!     IMIST:  From MISCOM; array containing tree record DMR's.
!     IREF:   From CONTRL; index to order of occurance of species.
!     ISP:    From ARRAYS; species code number.
!     ITRN:   From CONTRL; current number of tree records.
!     JOSTND: From CONTRL; unit number of stand output.
!     MAXSP:  From PRGPRM; maximum number of species.
!
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'MISCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG
INTEGER I,ISPTR,MSPCNT(MAXSP)

!.... Data statements.

!.... Check for debug.

CALL DBCHK(DEBUG,'MISCNT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,100) ICYC
100 FORMAT(' Begin MISCNT: Cycle = ',I5)

!.... Clear the MSPCNT array.

DO 150 I=1,MAXSP
   MSPCNT(I)=0
150 CONTINUE

!.... Loop through the treelist counting mistletoe infected records.
!.... Save counts by species according to IREF (ordered index based
!.... on occurance of species in species ordered sort list).

DO 200 I=1,ITRN
   IF(IMIST(I).GT.0) THEN
      ISPTR=IREF(ISP(I))
      MSPCNT(ISPTR)=MSPCNT(ISPTR)+1
   ENDIF
200 CONTINUE

!.... Common return.

IF(DEBUG) WRITE(JOSTND,9010) ICYC
9010 FORMAT(' End MISCNT: Cycle = ',I5)

RETURN
END

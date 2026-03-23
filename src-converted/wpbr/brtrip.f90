SUBROUTINE BRTRIP(INUT,I,WT)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Triples the Blister Rust records.
!----------------------------------------------------------------------
!
!  Parameters passed:
!     INUT - array index of new tree (record) being created.
!     I    - array index of tree (record) from which new tree is being
!            created.
!     WT   - Weight being assigned to new tree to determine number of
!            trees being represented by this new record. When value is
!            0.6, the index values INUT and I should be the same because
!            the original tree PROB (number of tree represented) is
!            being adjusted.
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!  20-APR-1999 Lance David
!     Comments for code and parameters only.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable definitions.

INTEGER I, INUT, J, NNCAN
REAL    WT
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRTRIP',6,ICYC)
IF(DEBUG) WRITE(JOSTND,112) ICYC
112 FORMAT ('Entering subroutine BRTRIP: cycle = ',I2)

!.... The WT value of 0.6 the weight at which the original record is set
!.... when tripling occurs. This line should not be necessary. 20-APR-1999
!.... IF(WT.EQ.0.6) INUT=I

UPMARK(INUT)=UPMARK(I)
BRHTBC(INUT)=BRHTBC(I)
GI(INUT)=GI(I)
RI(INUT)=RI(I)
BRGD(INUT)=BRGD(I)
BRPB(INUT)=BRPB(I)*WT
ESTCAN(INUT)=ESTCAN(I)
TSTARG(INUT)=TSTARG(I)
ITCAN(INUT)=ITCAN(I)
ILCAN(INUT)=ILCAN(I)
BRAGE(INUT)=BRAGE(I)
ISTOTY(INUT)=ISTOTY(I)
IBRTID(INUT)=IBRTID(I)
ICRED(INUT)=ICRED(I)
IBRSTAT(INUT)=IBRSTAT(I)

!.... Load cankers for new tree.

NNCAN=ILCAN(I)

DO 77 J=1,NNCAN
   DUP(J,INUT)=DUP(J,I)
   DOUT(J,INUT)=DOUT(J,I)
   GIRDL(J,INUT)=GIRDL(J,I)
   ISTCAN(J,INUT)=ISTCAN(J,I)
77 CONTINUE

IF(DEBUG) WRITE(JOSTND,114) ICYC
114 FORMAT ('Leaving subroutine BRTRIP: cycle = ',I2)
RETURN
END

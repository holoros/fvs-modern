SUBROUTINE COLDBH
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     INITIALIZATION ROUTINE FOR COLE'S MPB MODEL
!
! Revision History
!   05/31/00 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'COLCOM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'MPBCOM.f90'
!
!
!OMMONS
!
INTEGER I,J,I1,I2,INDEX
!
!     INITIALIZE ARRAYS
!
STOTAL=0.0
DO 5 I=1,10
  DEADCL(I)=0.0
  START(I)=0.0
  TGREEN(I)=0.0
  TDEAD(I)=0.0
  DO 4 J=1,10
    DEAD(I,J)=0.0
    GREEN(I,J)=0.0
4   CONTINUE
5 CONTINUE
!
!     DETERMINE NUMBER OF TREES PER DBH CLASS IN THE STAND
!
!     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
!     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
!
!      I1=ISCT(7,1)
I1=ISCT(IDXLP,1)
!
IF (I1.EQ.0) GOTO 20
!
!     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
!     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
!
!      I2=ISCT(7,2)
I2=ISCT(IDXLP,2)
DO 10 I=I1,I2
  J=IND1(I)
!
!       LOAD THE STARTING NUMBER OF TREES WITH THE
!       NUMBER OF TREES PER ACRE THAT THIS TREE RECORD
!       REPRESENTS.
!
  CALL COLIND (DBH(J),INDEX)
  START(INDEX)=START(INDEX)+PROB(J)
10 CONTINUE
20 CONTINUE
RETURN
END









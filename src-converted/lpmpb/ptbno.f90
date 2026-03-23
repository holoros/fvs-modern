FUNCTION PTBNO(X)
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     FIND LARGEST INTEGER POWER OF 10 LESS THAN X
!     IF X IS LESS THAN 3.5*10**I, THEN PTBNO=10**I/2
!
!
! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
REAL P10, PTBNO, X, XRND, XX

P10=10.
XX=X
IF(XX .LT. 0. ) XX=-XX
XRND=0.1005*XX
IF(XRND .LE. 10.) GO TO 1
!
2 P10=10.*P10
        IF(P10.LE. XRND) GO TO 2
        GO TO 3
!
1 P10=P10/10.
        IF(P10 .GT. XRND) GO TO 1
        P10=10.*P10
3 PTBNO=P10
IF(3.5*P10 .GE. X) PTBNO=.5*P10
RETURN
END

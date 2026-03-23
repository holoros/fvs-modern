SUBROUTINE GRPSUM (NRECS,IPT,ATR,P,WT)
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     CALLED FROM GARBEL OR COMPRS    N.L. CROOKSTON JAN 1978
!     MODIFIED MAY 1980 JAN 1981
!
!          NRECS = LENGTH OF POINTER ARRAY
!          IPT   = POINTER ARRAY
!          ATR   = ATTRIBUTE TO BE PROCESSED (REAL)
!          WT    = WEIGHT OF ATTRIBUTE
!          P     = SUM (P(I) * WT * ATR(I))
!          PROB  = WEIGHT OF EACH MEMBER OF ATR
!
!
! Revision History
!   04/03/02 Lance R. David (FHTET)
!     Arrays dimensioned "1" changed to "*" so that array size is
!     inherited from calling routine.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
INTEGER NRECS, IPT(*), II, I
REAL    WT, P(*), ATR(*), AVE, STDV, XN

AVE = 0.0
STDV = 0.0
XN = FLOAT(NRECS)
!
DO 17 II = 1, NRECS
I = IPT(II)
AVE = AVE + ATR(I)
STDV=STDV + ATR(I) ** 2
17 CONTINUE
!
!     USE THE MAXIMUM LIKLIHOOD ESTIMATE OF THE VARIANCE.
!
STDV=(STDV-AVE*AVE/XN)/XN
IF ( STDV .GT. 0.000000001 ) GO TO 18
STDV = 1.0
GO TO 21
18 CONTINUE
STDV = SQRT ( STDV )
21 CONTINUE
AVE = AVE/XN
!
DO 30 II= 1, NRECS
I = IPT(II)
P(I) = P(I) + WT * (ATR(I) - AVE ) / STDV
30 CONTINUE
!
RETURN
END

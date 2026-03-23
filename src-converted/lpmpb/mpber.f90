SUBROUTINE MPBER (NOER)
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!
!     NOER  = TRUE WHEN MINIMUM CONDITIONS MET FOR AN OUTBREAK
!             IE NOT AN 'ERROR'
!
!
! Revision History
!   05/31/00 GEB
!     Adding variable IDXLP, the index of Lodgepole pine in arrays
!     Defined in mpblkd.f, which is variant dependent
!   12/01/98 RNH
!     Adapated to 24 species (CR variant)
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!----------
LOGICAL NOER
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'MPBCOM.f90'
!
!
!OMMONS

INTEGER I, I1, I2, II
REAL P, D
!
!     CHECK FOR EXECUTION-ERROR CAUSING CONDITIONS.
!
NOER=.TRUE.
ILP = 0
CNTLP = 0.0
BALPP = 0.0
A45DBH = 0.0
TLP45 = 0.0
!
!     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
!     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
!
I1 = ISCT(IDXLP,1)
!      I1 = ISCT(7,1)
!
!
IF ( I1 .EQ. 0 ) GO TO 90
!
!     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
!     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
!
I2 = ISCT(IDXLP,2)
!      I2 = ISCT(7,2)
!
ILP = I2-I1+1
IF ( NCLASS .LE. 0 ) GO TO 90
IF ( ILP .LT. 1 ) GO TO 90
!
DO 80 II = I1, I2
I = IND1(II)
P=PROB(I)
D=DBH(I)
CNTLP = CNTLP + P
BALPP = BALPP + 0.005454154 * D * D * P
IF (D .LT. 4.5 )  GO TO 80
TLP45 = TLP45 + P
A45DBH = A45DBH + D * P
80 CONTINUE
IF ( CNTLP .LT. .01 ) GO TO 90
IF ( TLP45 .LT. 1. ) GO TO 90
!
!     PROPORTION BASAL AREA IN LPP
!
PBALPP = BALPP / BA
!
!     AVERAGE LODGEPOLE DBH:
!
A45DBH = A45DBH  / TLP45
GO TO 100
!
90 CONTINUE
NOER=.FALSE.
100 CONTINUE
RETURN
END

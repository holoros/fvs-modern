SUBROUTINE BMCBET(ABETA, MINSIZE, MAXSIZE, BETA)
!----------
! WWPB $Id$
!----------
!     CALLED BY: BMISTD
!     CALLS: FUNCTIONS ALNGAM AND BETAIN
!*****************************************************************************
!       BMCBET  Date of last revision APRIL 22, 1998, By Shaoang
!       REVISED: 9/8/99 ajm.  Made A a function of BKP/Acre.
!*****************************************************************************
!     PURPOSE:
!       A SUBROUTINE TO CALCULATE PROBABILITY WITHIN EACH INTERVAL FOR A BETA
!       RANDOM VARIATE (IN THIS CASE, THE RANDOM VARIATE IS dbh). IT IS ADOCPTED
!       FROM ALGORITHM AS226 APPL. ATATIST. (1987) VOL. 36, NO. 2
!
!     CALL LIST DEFINITIONS:
!       MINSIZE: THE MINIMUM SIZE CLASS THE BEETLE CAN ATTACK
!       MAXSIZE: THE MAXIMUM SIZE CLASS THAT BEETLE CAN ATTACK
!       BETA: ARRAY CONTAINING THE BETA DISTRIBUTION VALUES FOR EACH SIZE CLASS
!
!     LOCAL VARIABLE DEFINITIONS:
!       A: PARAMETER ALPHA IN THE BETA DISTRIBUTION
!       B: PARAMETER BETA IN THE BETA DISTRIBUTION
!       IFAULT: FLAG VARIABLE
!       X: SIZE CLASS FALLING IN THE RANGE OF MINSIZE AND MAXSIZE
!       TEMP: TEMPORARY VARIABLES USED TO STORE CUMULATIVE BETA VALUES
!*********************************************************************************


!     PARAMETER INCLUDE FILE

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!     COMMON INCLUDE FILES

INCLUDE 'CONTRL.f90'
INCLUDE 'BMCOM.f90'

!     FUNCTION DEFINITIONS

REAL ALNGAM, BETAIN

!     ARG LIST

INTEGER MINSIZE
INTEGER MAXSIZE
REAL BETA

DIMENSION BETA(NSCL)

!     VARIABLE DECLARATIONS

INTEGER I
REAL A, X, MINX, MAXX, LOGBETA,TEMP0, TEMP1

!     **************************************************************************
!     Beta distribution parameters a and b is set to be 2.0 and 2.0 based on the
!     experience and sample simulation for beta distribution with different beta
!     parameters. For beta(x,2,2), probability increases as x increases. If it is
!     desired to make the parameters change with the maximum size class (MAXSIZE)
!     used in this subroutine, two functions: a=f(maxsize) and b=g(maxsize) can
!     be put in here instead of using data statement to define their values
!     ***************************************************************************

!      DATA A,B /2.0,2.0/
DATA B /2.0/
!
A = ABETA
!
!     Set A equal to ABETA (above), and declared A as REAL (above).
!     ABETA is defined in subroutine BMISTD and is a function of BKP/A.

DATA ZERO, HALF, ONE /0.0, 0.5, 1.0/
!
IF (LBMDEB) WRITE(JBMBPR,10)
10 FORMAT(' Begin BMCBET:')


!     ZERO ARRAY (actually set to a small number so that all size classes
!                       could potentially be attacked)

DO 12 I=1, NSCL
  BETA(I)=1E-5
12 CONTINUE
!...

MINX=REAL(MINSIZE)-HALF
MAXX=REAL(MAXSIZE)+HALF
LOGBETA=ALNGAM(A,IFAULT) + ALNGAM(B, IFAULT) - ALNGAM(A+B,IFAULT)

TEMP0=ZERO
DO 15 I=MINSIZE, MAXSIZE
  X=((REAL(I)+HALF)-MINX)/(MAXX-MINX)

  IF (X .LE. ZERO) X=0.001
  IF (X .EQ. ONE) X=ONE - 0.001

  IF (X .LT. ZERO .OR. X .GT. ONE) THEN
    IFAULT=3
    RETURN
  ENDIF

  IF (X .EQ. ZERO .OR. X .EQ. ONE) THEN
    IFAULT=0
    RETURN
  ENDIF

  TEMP1=BETAIN(X,A,B,LOGBETA,IFAULT)
  BETA(I) = TEMP1 - TEMP0
  TEMP0=TEMP1

15 CONTINUE

IF(LBMDEB) WRITE(JBMBPR,99) A, B
99 FORMAT ('End BMCBET: A= ', F4.1, 'B= ', F4.1)

RETURN
END

!
!********************************************************************************
! This file is copy from Statlib on the WWW with address
! http://lib.stat.cmu.edu/apstat/245. It is incoporated into BMCBET to calculate
! the weight to determine the probablity of beetle attacks by diameter classes.
! Adoption date: 4/22/98
!*********************************************************************************
!
! This file contains Algorithm AS 245 is gives an accuracy of about
! 10-12 significant decimal digits except for small regions around X = 1 and
! X = 2, where the function goes to zero.
!
!
!
REAL FUNCTION ALNGAM(XVALUE, IFAULT)
!
!     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2
!
!     Calculation of the logarithm of the gamma function
!
INTEGER IFAULT
REAL XVALUE
REAL ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9), &
             R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XLGST, &
             Y, ZERO
!***  DOUBLE PRECISION XVALUE
!
!     Coefficients of rational functions
!
DATA R1/-2.6668551E0, -2.4438753E1, &
           -2.1969895E1,  1.1166754E1, &
            3.1306054E0,  6.0777138E-1, &
            1.1940090E1,  3.1469011E1, &
            1.5234687E1/
DATA R2/-7.8335929E1, -1.4204629E2, &
            1.3751941E2,  7.8699492E1, &
            4.1643892E0,  4.7066876E1, &
            3.1339921E2,  2.6350507E2, &
            4.3340002E1/
DATA R3/-2.1215957E5,  2.3066151E5, &
            2.7464764E4, -4.0262111E4, &
           -2.2966072E3, -1.1632849E5, &
           -1.4602593E5, -2.4235740E4, &
           -5.7069100E2/
DATA R4/ 2.7919531791E-1, 4.9173176105E-1, &
            6.9291059929E-2, 3.3503438150E0, &
            6.0124592597E0/
!
!     Fixed constants
!
DATA ALR2PI/9.1893853320E-1/, FOUR/4.E0/, HALF/0.5E0/, &
        ONE/1.E0/, ONEP5/1.5E0/, TWELVE/12.E0/, ZERO/0.E0/
!
!     Machine-dependant constants.
!     A table of values is given at the top of page 399 of the paper.
!     These values are for the IEEE double-precision format for which
!     B = 2, t = 53 and U = 1023 in the notation of the paper.
!
DATA XLGE/5.10E6/, XLGST/1.E+38/
!
X = XVALUE
ALNGAM = ZERO
!
!     Test for valid function argument
!
IFAULT = 2
IF (X .GE. XLGST) RETURN
IFAULT = 1
IF (X .LE. ZERO) RETURN
IFAULT = 0
!
!     Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined
!
IF (X .LT. ONEP5) THEN
  IF (X .LT. HALF) THEN
    ALNGAM = -LOG(X)
    Y = X + ONE
!
!     Test whether X < machine epsilon
!
    IF (Y .EQ. ONE) RETURN
  ELSE
    ALNGAM = ZERO
    Y = X
    X = (X - HALF) - HALF
  END IF
  ALNGAM = ALNGAM + X * ((((R1(5)*Y + R1(4))*Y + R1(3))*Y &
                   + R1(2))*Y + R1(1)) / ((((Y + R1(9))*Y + R1(8))*Y &
                   + R1(7))*Y + R1(6))
  RETURN
END IF
!
!     Calculation for 1.5 <= X < 4.0
!
IF (X .LT. FOUR) THEN
  Y = (X - ONE) - ONE
  ALNGAM = Y * ((((R2(5)*X + R2(4))*X + R2(3))*X + R2(2))*X &
                 + R2(1)) / ((((X + R2(9))*X + R2(8))*X + R2(7))*X &
                 + R2(6))
  RETURN
END IF
!
!     Calculation for 4.0 <= X < 12.0
!
IF (X .LT. TWELVE) THEN
  ALNGAM = ((((R3(5)*X + R3(4))*X + R3(3))*X + R3(2))*X + R3(1)) / &
               ((((X + R3(9))*X + R3(8))*X + R3(7))*X + R3(6))
  RETURN
END IF
!
!     Calculation for X >= 12.0
!
Y = LOG(X)
ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
IF (X .GT. XLGE) RETURN
X1 = ONE / X
X2 = X1 * X1
ALNGAM = ALNGAM + X1 * ((R4(3)*X2 + R4(2))*X2 + R4(1)) / &
                 ((X2 + R4(5))*X2 + R4(4))
RETURN
END



REAL FUNCTION BETAIN(X, P, Q, BETA, IFAULT)
IMPLICIT REAL (A-H, O-Z)

!***      implicit double precision (a-h, o-z)
!
!    ***************************************************************************
!    This is an algorithm copied from Statlib on the internet. The web site
!    for this algorithm is http://lib.stat.cmu.edu/apstat/63
!
!     algorithm as 63  appl. statist. (1973), vol.22, no.3
!
!     computes incomplete beta function ratio for arguments
!     x between zero and one, p and q positive.
!     log of complete beta function, beta, is assumed to be known
!
!     **************************************************************************

LOGICAL INDX

REAL X,P,Q,BETA
INTEGER IFAULT
!
!     define accuracy and initialise
!
DATA ZERO/0.0E0/, ONE/1.0E0/, ACU/0.1E-14/
BETAIN=X
!
!     test for admissibility of arguments
!
IFAULT=1
IF(P.LE.ZERO .OR. Q.LE.ZERO) RETURN
IFAULT=2
IF(X.LT.ZERO .OR. X.GT.ONE) RETURN
IFAULT=0
IF(X.EQ.ZERO .OR. X.EQ. ONE) RETURN
!
!     change tail if necessary and determine s
!
PSQ=P+Q
CX=ONE-X
IF(P.GE.PSQ*X) GOTO 1
XX=CX
CX=X
PP=Q
QQ=P
INDX=.TRUE.
GOTO 2
1 XX=X
PP=P
QQ=Q
INDX=.FALSE.
2 TERM=ONE
AI=ONE
BETAIN=ONE
NS=QQ+CX*PSQ
!
!     user soper's reduction formulae.
!
RX=XX/CX
3 TEMP=QQ-AI
IF(NS.EQ.0) RX=XX
4 TERM=TERM*TEMP*RX/(PP+AI)
BETAIN=BETAIN+TERM
TEMP=ABS(TERM)
IF(TEMP.LE.ACU .AND. TEMP.LE.ACU*BETAIN) GOTO 5
AI=AI+ONE
NS=NS-1
IF(NS.GE.0) GOTO 3
TEMP=PSQ
PSQ=PSQ+ONE
GOTO 4
!
!     calculate result
!
5 BETAIN=BETAIN*EXP(PP*LOG(XX)+(QQ-ONE)*LOG(CX)-BETA)/PP
IF(INDX) BETAIN=ONE-BETAIN
RETURN
END

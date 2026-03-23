SUBROUTINE RDSTR (IT,PREM,PREPRB)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR DECREASING INFESTED TREES WHEN CUTTING OR
!  THINNING
!
!  CALLED BY :
!     CUTS    [PROGNOSIS]
!
!  CALLS     :
!     DBCHK   (SUBROUTINE)   [PROGNOSIS]
!     RDSADD  (SUBROUTINE)   [ROOT DISEASE]
!     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
!     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
!     RDSPUP  (SUBROUTINE)   [ROOT DISEASE]
!
!  PARAMETERS :
!     IT     -
!     PREM   -
!     PREPRB -
!
!  REVISION HISTORY:
!    01-JUL-2002 Lance R. David (FHTET)
!      Previous date might have been June 1998.
!      Root radius for live trees was only being set when spore infection
!      was active. This value is needed elsewhere regardless of spore
!      infection processes.
!    16-JUL-2002 Lance R. David (FHTET)
!      Modified comment and debug statements.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!   03/01/2016 Lance R. David (FMSC)
!     Moved check to exit if not active, no trees or no disease area
!     to top.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'RDPARM.f90'

INCLUDE 'RDCOM.f90'

INCLUDE 'RDARRY.f90'

INCLUDE 'ARRAYS.f90'

INCLUDE 'CONTRL.f90'

INCLUDE 'RDADD.f90'
!
!OMMONS
!

LOGICAL DEBUG
INTEGER I, IDI, IRG, ISL, IT, JJ
REAL    ANS, PREM, PREPRB, TESTAG, TP, TPAREA, XXX

!
!     SEE IF WE NEED TO DO SOME DEBUG.
!
CALL DBCHK (DEBUG,'RDSTR',5,ICYC)
IF (DEBUG) WRITE (JOSTND,100) ICYC
100 FORMAT (' ENTER RDSTR: CYCLE=',I5)

IF (IROOT .EQ. 0 .OR. PREPRB .EQ. 0.0) GOTO 1100

TPAREA = 0.0
DO 101 IDI=MINRR,MAXRR
   TPAREA = TPAREA + PAREA(IDI)
101 CONTINUE

TP = (1.0 - (PREM / PREPRB))
!
!     IF THE ROOT ARRAYS HAVE NOT YET BEEN CREATED (BECAUSE THERE IS
!     NO INFECTION IN THE STAND OR BECAUSE A CUT OCCURS IN THE FIRST
!     TIMESTEP) THEN DO SO.
!
JJ = ISP(IT)
IDI = MAXRR
IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(JJ))

!--------------
!     This block of code is replaced below. Why array that is suppose
!     to hold root radius for all each tree records is only calculated
!     when spore infection is active does not make sense.
!     lrd 01-jul-02
!      IF (ROOTL(IT) .GT. 0.0 .OR. SPINF(IDI) .LE. 0.0) GOTO 300
!
!          CALL RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)),
!     &                RSLOP(IRTSPC(JJ)),HT(IT))
!          ROOTL(IT) = ANS
!
!  300 CONTINUE
!---------------

IF (ROOTL(IT) .EQ. 0.0) THEN
    CALL RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)), &
                   RSLOP(IRTSPC(JJ)),HT(IT))
    ROOTL(IT) = ANS
ENDIF
!
!     WHY EQUAL WEIGHTS FOR OUTSIDE AND INSIDE ON A PER ACRE BASIS ?
!
XXX = TP * PREPRB
RROOTT(IT) = (RROOTT(IT) * WK22(IT) + &
                ROOTL(IT) * XXX) / (XXX + WK22(IT) + 0.0001)

!
!     CALL THE SPORE MODEL.
!
!      IF (SPINF(IDI) .GT. 0.0) THEN
!
!     Only call spore model if both IDI > 0, and SPINF(IDI) > 0
!     (RNH June98)
!
IF ((IDI .GT. 0) .AND. (SPINF(IDI) .GT. 0.0)) THEN
!
    CALL RDSSIZ(JJ,DBH(IT),STCUT,ISL,ISPS,IRTSPC)
    CALL RDSPUP(IT,ISL,JJ,TP,DBH(IT),ROOTL(IT))
ENDIF
!
!     REDUCE TREE DENSITY VARIABLES
!
PROBIU(IT) = PROBIU(IT) * TP
FPROB(IT) = FPROB(IT) * TP
WK22(IT) = WK22(IT) + XXX

DO 500 I = 1,ISTEP
   PROBI(IT,I,1) = PROBI(IT,I,1) * TP
   PROBI(IT,I,2) = PROBI(IT,I,2) * TP
500 CONTINUE

!     ADD THE ALREADY INFECTED STUMPS TO THE STUMP LIST
CALL RDSADD(IT,TP)

CALL RDSUM (IT,PROBIT,PROBI,ISTEP)
TESTAG = FLOAT (IY(ICYC))

IF (DEBUG) WRITE (JOSTND,1010) TESTAG, AGECUR, INFLAG
1010 FORMAT(' IN RDSTR: TESTAG AGECUR INFLAG=',2F6.0,I5)

IF ((TESTAG .EQ. AGECUR) .AND. (INFLAG .NE. 0)) INFLAG = 3
IF (INFLAG .NE. 0) GOTO 1100

!
!     SET THE CUTTING FLAG ON FOR ROOT DISEASE MODEL
!
INFLAG = 1
IF (DEBUG) WRITE (JOSTND,1020)
1020 FORMAT (' IN RDSTR: SET INFLAG=1')

!
!     SET UP AGE FOR THE CARRYOVER MODEL CALL TIMING
!
IRG = ICYC + IRGEN(1)
IF (IRG .LE. NCYC) AGECUR = IY(IRG)
IF (DEBUG) WRITE (JOSTND,1030) AGECUR
1030 FORMAT(' IN RDSTR: AGECUR=',F6.0)

1100 CONTINUE
IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDSTR'
RETURN
END

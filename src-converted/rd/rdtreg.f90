SUBROUTINE RDTREG
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This root disease model subroutine is called from FVS
!     GRADD every cycle.  It calls the other agents model,
!     then calls the master root disease model.  Finally,
!     it calls various subroutines to produce output files.
!
!  Called By :
!     GRADD  [FVS]
!
!  Calls :
!     DBCHK   (SUBROUTINE)   [FVS]
!     RDLOAD  (SUBROUTINE)   [ROOT DISEASE]
!     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
!     RDOAGM  (SUBROUTINE)   [ROOT DISEASE]
!     RDTIM   (SUBROUTINE)   [ROOT DISEASE]
!     RDPUSH  (SUBROUTINE)   [ROOT DISEASE]
!     RDCNTL  (SUBROUTINE)   [ROOT DISEASE]
!     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
!     RDEND   (SUBROUTINE)   [ROOT DISEASE]
!     RDRDEL  (SUBROUTINE)   [ROOT DISEASE]
!     RDGROW  (SUBROUTINE)   [ROOT DISEASE]
!     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
!     ANWRITE (SUBROUTINE)   [ROOT DISEASE]
!
!  Revision History :
!    03-APR-97 Matt Thompson (FHTET)
!              Changed the code that tests to see if a cut occurred
!              so that if the disease area is 0 but there is a chance
!              of spore infection, Annosus can occur.
!    14-JAN-00 Lance David (FHTET)
!              Replaced the literals in option processor calls with
!              references to array MYACT.
!    06-AUG-01 Lance R. David (FHTET)
!              Added initialization of array DIFF.
!    29-JUL-02 Lance R. David (FHTET)
!              Modified and added debug statements. Changed two return
!              statements to "GOTO 300" so routine has single point of exit.
!   09/04/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!   03/01/16 Lance R. David (FMSC)
!     Moved check to exit routine if RD not in use. IF (IROOT .EQ. 0) GOTO 300
!-----------------------------------------------------------------------------
!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDADD.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'RDCRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'

!.... Local variable declarations.

LOGICAL DEBUG
INTEGER I, IACTK, IDI, ITR, J, KDT, MYACT(1), NPS, NTODO
REAL    ANS, DIFF(ITOTRR), PRMS(2), TPAREA

DATA MYACT / 2430 /

!.... See if we need to do some debug.

CALL DBCHK (DEBUG,'RDTREG',6,ICYC)

IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDTREG'

IF (IROOT .EQ. 0) GOTO 300
DO 10 IDI=1,ITOTRR
   DIFF(IDI) = 0.0
10 CONTINUE

TPAREA = 0.0
DO 850 IDI=MINRR,MAXRR
   TPAREA = TPAREA + PAREA(IDI)
850 CONTINUE

!.... Changed the following two lines so that the annosus model will be run if
!.... there is no area but a cut did occur (LSPFLG(1..3) = .TRUE.).
!.... mt 04-03-97

IF ((TPAREA .EQ. 0) .AND. &
      (.NOT. LSPFLG(1) .AND. &
       .NOT. LSPFLG(2) .AND. &
       .NOT. LSPFLG(3))) GOTO 300

!.... Check for time to death and infection probability.

IF (IY(ICYC) .GE. IRGEN(8)) IRHAB = 2
IF (IY(ICYC) .NE. IRGEN(9)) GOTO 1000

IDI = MAXRR
DO 900 I = 1,MAXSP
   IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(I))
   PNINF(IRTSPC(I),IDI) = SSSFAC(IRTSPC(I),IDI) * &
                               PNINF(IRTSPC(I),IDI)
900 CONTINUE

1000 CONTINUE

!.... Copy the borax option processing information into the next
!.... cycle so it will be there if there is a cut.
!     (Activity Code 2430)

IF (LBORAX(1)) THEN
   IF (.NOT. LBORAX(2)) THEN

      IF (LBORAX(1)) CALL OPFIND (1,MYACT(1),NTODO)

      IF (NTODO .GT. 0) THEN

         CALL OPGET (1,2,KDT,IACTK,NPS,PRMS)

         BOTRT = PRMS(1)
         BODBH = PRMS(2)

         CALL OPDONE (1,IY(ICYC))
      ENDIF

   ELSE
      LBORAX(2) = .FALSE.
   ENDIF

ENDIF

IF (ITRN .LE. 0) GOTO 9999

!.... Calculate outside density.

DO 839 IDI=MINRR,MAXRR
   DIFF(IDI) = SAREA - PAREA(IDI)
   IF (DIFF(IDI) .GT. 0.0) GOTO 841
839 CONTINUE
GOTO 844

841 CONTINUE
IDI = MAXRR

DO 843 I=1,ITRN
!
IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
!
!     Exit loop if not a host species (RNH 28May98)
!
IF (IDI .LE. 0) GO TO 843
!
   IF (DIFF(IDI) .GE. 1E-6) THEN
      FPROB(I) = (PROB(I) * SAREA - PROBIU(I) - PROBIT(I)) / &
                    DIFF(IDI)
      IF (FPROB(I) .LE. 1E-6) FPROB(I) = 0.0
   ELSE
      FPROB(I) = 0.0
   ENDIF

843 CONTINUE

844 CONTINUE

!.... Load PROB into PROBL (Density of all live trees in stand is PROB).

CALL RDLOAD (PROBL,PROB,ITRN)

!.... Calculate root radii.

DO 1001 ITR = 1,ITRN
   I = ISP(ITR)
   CALL RDROOT (I,DBH(ITR), ANS, PROOT(IRTSPC(I)), &
         RSLOP(IRTSPC(I)), HT(ITR))
   ROOTL(ITR) = ANS
   IF (DEBUG) WRITE (JOSTND,775) ITR, PROB(ITR), PROBI(ITR,1,1), &
                 PROBIU(ITR), FPROB(ITR),ITR,HT(ITR),DBH(ITR),ANS
775    FORMAT (' IN RDTREG: I PROB PROBI PROBIU FPROB',I5,4(1X,F8.2), &
      ' ITR HT DBH ROOTL ', I4,3(1X,F7.3))
1001 CONTINUE

!.... Call the other agents submodels.

CALL RDOAGM

CALL RDTIM

9999 CONTINUE

!.... Call subroutine to push stumps.

CALL RDPUSH
CALL RDCNTL
IF (ITRN .LE. 0) GOTO 10000

!.... Call subroutine to update stump list with natural,
!.... other agent, and root disease killed trees.
!....
!.... Root disease extensions simulated, redefine PROB
!.... after losses to other agents

CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)
CALL RDEND

!.... New code added.  Call RDRDEL to delete tree records which
!.... have all prob values equal to zero, IE PROBI, PROBIU, PROBIT
!.... and FPROB.  RDRDEL called only if model initialized from treelist.

IF (RRTINV) CALL RDRDEL

!.... Call routine which reduces the growth factor due to root disease.

CALL RDGROW

!.... Update growing space factor based on root disease patches.

10000 CONTINUE
IF (INFLAG .EQ. 0) GOTO 260
IF (INFLAG .GT. 1) GOTO 260
INFLAG = 2

DO 212 IDI=MINRR,MAXRR
   DO 210 I = 1,2
      DO 205 J = 1,5
         CRNSTO(IDI,I,J) = PROBD(IDI,I,J)
205       CONTINUE
210    CONTINUE
212 CONTINUE

260 CONTINUE

!.... Call subroutine to decay the stump root radius
!.... (decrease inoculum).

CALL RDINOC (.FALSE.)

300 CONTINUE
IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDTREG'
RETURN
END

SUBROUTINE RDOWIN
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Subroutine that simulates windthrow.
!
!  Called By :
!     RDOAGM  [ROOT DISEASE]
!
!  Calls :
!     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
!     OPFIND  (SUBROUTINE)   [PROGNOSIS]
!     OPGET   (SUBROUTINE)   [PROGNOSIS]
!     OPDONE  (SUBROUTINE)   [PROGNOSIS]
!     OPREDT  (SUBROUTINE)   [PROGNOSIS]
!     OPINCR  (SUBROUTINE)   [PROGNOSIS]
!
!  Arguments :
!
!  Local Variables :
!     <incomplete>
!
!  Common Block Variables Used :
!     IIEND  - (RDCOM)   (O)
!     ILEND  - (RDCOM)   (O)
!     ISTEMI - (RDARRY)  (O)
!     ISTEML - (RDARRY)  (O)
!     STEMSI - (RDCOM)   (O)
!     STEMSL - (RDCOM)   (O)
!     <incomplete>
!
!  Revision History :
!    06/12/96 - Matthew K. Thompson
!       Created this subroutine from the entry point that
!       was in RDOWI.
!    26-MAR-2002 Lance R. David (FHTET)
!       Changed dimension on PROBUN from 1350 to FVS parameter MAXTRE.
!    14-DEC-2007 Lance R. David (FHTET)
!       Changed MYACT to MYACT(1) in call to OPREDT (from Don Robinson).
!   08/29/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations.

INTEGER  I, I1, I2, IACTK, IFL, IK, IP, ISPI, J, KDT, &
            MYACT(1), NPS, NREDTS, NTODO, RRTYPE

REAL     CRASH, THRESH, PRMS(2), PROBUN(MAXTRE), REMNO, RN, &
            RRTEMP, RRTEMS, SNAP, TBPROB, TT, TTIU, TTO, ZAREA

!.... Data statements.

DATA MYACT /2414/


IF (ITRN .LE. 0) RETURN

!.... Check to see if windthrow is scheduled.

CALL OPFIND (1,MYACT,NTODO)
IF (NTODO .LE. 0) RETURN

!.... Get windthrow parameters from the event monitor.

CALL OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
CRASH = PRMS(1)
THRESH = PRMS(2)

ILEND = 0
IIEND = 0
DO 22 ISPI = 1,MAXSP
   DO 21 IK = 1,4
      WINDSP(ISPI,IK) = 0.0
21    CONTINUE
22 CONTINUE

DO 30 ISPI = 1,MAXSP
   STEMSL(ISPI) = 0
   STEMSI(ISPI) = 0
   ROCRL(ISPI)  = 0
   ROCRI(ISPI)  = 0

   IF (ISCT(ISPI,1) .EQ. 0) GOTO 30

!....    NB: IF PCT N/A THEN HEIGHT SORT HERE

   I1 = ISCT(ISPI,1)
   I2 = ISCT(ISPI,2)
   RRTYPE = MAXRR

   DO 29 J = I1,I2
      I = IND1(J)
      IF (PCT(I) .LT. ROWDOM(IRTSPC(ISPI))) GOTO 29

!....       LIVE UNINFECTED STEMS (AS A NUMBER OF STEMS)

      IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
!
!     Exit loop for non-host trees (RNH June98)
!
IF (RRTYPE .LE. 0) GO TO 29
!
      ZAREA = SAREA - PAREA(RRTYPE)
      PROBUN(I) = PROBIU(I) + FPROB(I) * ZAREA

      IF (PROBUN(I) .EQ. 0.0) GOTO 25
      ILEND = ILEND + 1
      ISTEML(ILEND) = I
      STEMSL(ISPI) = STEMSL(ISPI) + PROBUN(I)
      WINDWL(I) = ICR(I) * PROBUN(I)
      ROCRL(ISPI) = ROCRL(ISPI) + WINDWL(I)

25       CONTINUE

!....       INFECTED STEMS. Susceptibility is increased by the
!....       proportion of root system infected: 1+PROPI(I,IK)

      IFL = 0
      RRTEMS = 0

      DO 27 IK = 1,ISTEP
         DO 26 IP=1,2
            IF (PROBI(I,IK,IP) .EQ. 0) GOTO 26
            IFL = 1
            STEMSI(ISPI) = STEMSI(ISPI) + PROBI(I,IK,IP)
            RRTEMP = ICR(I) * PROBI(I,IK,IP) * &
                        (1 + PROPI(I,IK,IP))
            RRTEMS = RRTEMS + RRTEMP
26          CONTINUE
27       CONTINUE

      IF (IFL .EQ. 0) GOTO 29
      IIEND = IIEND + 1
      ISTEMI(IIEND) = I
      ROCRI(ISPI) = ROCRI(ISPI) + RRTEMS
      WINDWI(I) = RRTEMS
29    CONTINUE
30 CONTINUE

!.... End of species loop.

TSTEMS = 0.0
TBPROB = 0.0

DO 35 ISPI = 1,MAXSP
   WINDSP(ISPI,3) = STEMSL(ISPI) + STEMSI(ISPI)
   TSTEMS = TSTEMS + WINDSP(ISPI,3)
   WINDSP(ISPI,3) = WINDSP(ISPI,3) * ROWIBP(IRTSPC(ISPI),2)
   TBPROB = TBPROB + WINDSP(ISPI,3)
   WINDSP(ISPI,1) = 0.0
   WINDSP(ISPI,2) = 0.0
35 CONTINUE

!.... Total number of stems to windthrow

WINDN = TSTEMS * CRASH
IF (WINDN .GT. TSTEMS) WINDN = TSTEMS

!.... If the number of stems windthrown is less then the
!.... specified minimum then the event does not occur.

IF ((WINDN / SAREA) .GE. THRESH) GOTO 38

DO 37 ISPI = 1,MAXSP
   DO 36 IK = 1,4
      WINDSP(ISPI,IK) = 0.0
36    CONTINUE
37 CONTINUE

WINDN = 0.0
GOTO 777

38 CONTINUE
CALL OPDONE(NTODO,IY(ICYC))

!.... Allocate to species.

DO 40 ISPI = 1,MAXSP
   WINDSP(ISPI,3) = WINDN * (WINDSP(ISPI,3) / TBPROB)
   IF (ROCRL(ISPI) .EQ. 0 .AND. ROCRI(ISPI) .EQ. 0) GOTO 40
   WINDSP(ISPI,1) = WINDSP(ISPI,3) * (ROCRL(ISPI) / (ROCRL(ISPI) &
                       + ROCRI(ISPI)))
   WINDSP(ISPI,2) = WINDSP(ISPI,3) - WINDSP(ISPI,1)
40 CONTINUE

!.... Now allocate these proportions back to the tree lists
!....
!.... Live uninfected trees.

RRTYPE = MAXRR
DO 50 J = 1,ILEND
   I = ISTEML(J)
   ISPI = ISP(I)
   TT = WINDSP(ISPI,1) * WINDWL(I) / ROCRL(ISPI)

!....    INSIDE UNINFECTED TREES - THESE ARE A NUMBER

   TTIU = TT * PROBIU(I) / PROBUN(I)
   IF (TTIU .GT. PROBIU(I) * 0.95) TTIU = PROBIU(I) * 0.95
   OAKL(DSIU,I) = OAKL(DSIU,I) + TTIU

!....    OUTSIDE (UNINFECTED) TREES - FPROB IS A DENSITY
!....           BUT OAKL IS KEPT AS A NUMBER

   IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
!
!     If non-host trees exit loop
!
IF (RRTYPE .LE. 0) GO TO 50
!
   ZAREA = SAREA - PAREA(RRTYPE)
!
!     When SAREA = PAREA, ZAREA > 0.0 and causes devide by zero error
!     in statement below.  Therefore, assign ZAREA a small number
!     when SAREA = PAREA (RNH, MAR98)
!
IF (ZAREA .LE. 0.0) ZAREA = 1.0E-6
!
   TTO = (TT - TTIU) / ZAREA
   IF (TTO .GT. FPROB(I) * 0.95) TTO = FPROB(I) * 0.95
   OAKL(DSO,I) = OAKL(DSO,I) + TTO * ZAREA

!....    UPDATE FFPROB FOR THE EFFECTS OF WINDTHROW

   IF (ZAREA .GT. 1E-6) &
          FFPROB(I,2) = FFPROB(I,2) - (OAKL(DSO,I) / ZAREA)

50 CONTINUE

!.... INFECTED TREES. NOTE THAT THE ACTUAL REMOVAL OF INFECTED TREES TAKES PLACE
!.... IN RDOAGM AFTER THE BARK BEETLES HAVE ACTED (SINCE THEY WANT
!.... TO ASSUME THAT THESE TREES ARE STILL ALIVE)

DO 70 J = 1,IIEND
   I = ISTEMI(J)
   ISPI = ISP(I)
   SNAP = 0.0

   DO 65 IK = 1,ISTEP
      DO 60 IP=1,2
         IF (PROBI(I,IK,IP) .EQ. 0) GOTO 60
         RN = PROBI(I,IK,IP)

!....          REMNO - NUMBER OF STEMS TO REMOVE
!....          SNAP  - NUMBER OF STEMS WHICH WERE BROKEN OFF?

         REMNO = WINDSP(ISPI,2) * WINDWI(I) / ROCRI(ISPI)
         IF (REMNO .GT. RN * .95) REMNO = RN * 0.95
         SNAP = SNAP + (REMNO * (1 - PROPI(I,IK,IP)))
         PROBI(I,IK,IP) = RN - REMNO
         RRKILL(I) = RRKILL(I) + REMNO
         OAKL(DSII,I) = OAKL(DSII,I) + REMNO
60       CONTINUE
65    CONTINUE

70 CONTINUE

ROWIND = 0

GOTO 888

!.... If no windthrow then reschedule event for next cycle.

777 CONTINUE
CALL OPREDT (MYACT(1),IY(ICYC),IY(ICYC+1),NREDTS)
CALL OPINCR (IY,ICYC,NCYC)

888 CONTINUE
RETURN
END

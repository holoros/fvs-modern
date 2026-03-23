SUBROUTINE RDSPUP (I,ISL,IISP,TP,DIAM,RTD)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Adds spore infected stumps to spore stump arrays (If they are
!     big enough to be centers). Boraxing also occurs here.
!
!  Called By :
!     RDSTR   [ROOT DISEASE]
!
!  Calls :
!     DBCHK   (SUBROUTINE)  [PROGNOSIS]
!
!  Arguments :
!     I      -
!     ISL    -
!     IISP   -
!     TP     -
!     DIAM   -
!     RTD    -
!
!  Local Variables :
!
!  Common Block Variables  Used :
!     xxxxx:   From ANCOM;
!
!  Revision History :
!     04/03/97 Matt Thompson (FHTET)
!               Modified the code that stores the information about
!               when the stand entry occurred.
!     14-JAN-00 Lance R. David (FHTET)
!               Removed literals from option processor calls and replaced
!               with references to MYACT array. Expanded MYACT array to
!               include activity code 2431.
!     08-AUG-01 Lance R. David (FHTET)
!               Declaration and initialization of variable NTODO.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations.

LOGICAL DEBUG
INTEGER I, IACTK, ICASE, IDI, IISP, IJ, IS, ISL, JRRTYP, KDT, &
           MYACT(2), NPS, NTODO
REAL    ADJBOR, DENOUT, DENUIN, DIAM, &
           OUTTOT, PRMS(5), RTD, TP, UINTOT

!.... Data statements.

DATA MYACT / 2430, 2431 /

CALL DBCHK(DEBUG,'RDSPUP',6,ICYC)

NTODO = 0
IDI = MAXRR
IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(IISP))
IS  = ISPS(IRTSPC(IISP))
ADJBOR = 0.0
DENUIN = 0.0
DENOUT = 0.0
OUTTOT = 0.0
UINTOT = 0.0

!.... Set flags to specify that a stand entry occurred and the year
!.... in which it occured.

LSPFLG(1) = .TRUE.
ISDATE(1) = IY(ICYC)

!.... Proportion of stumps to receive borax.
!
!.... Only call the option processor the first time this routine is
!.... called.  LBORAX is set to .FALSE. within the root disease model
!.... after this routine has been called. The first time in a cycle
!.... that the option processor is called LBORAX is set to .TRUE. to
!.... indicate the option processor has been called already and the
!.... variables have been set for this cycle.
!     (ACTIVITY CODE 2430)

IF (.NOT. LBORAX(2)) THEN

   IF (LBORAX(1)) CALL OPFIND (1,MYACT(1),NTODO)
   LBORAX(2) = .TRUE.

   IF (NTODO .GT. 0) THEN
      IF (NTODO .GT. 1) THEN

         DO 13 ICASE = 2, NTODO
            CALL OPDEL1(ICASE)
13          CONTINUE
      ENDIF

      CALL OPGET (1,2,KDT,IACTK,NPS,PRMS)

      BOTRT = PRMS(1)
      BODBH = PRMS(2)

      CALL OPDONE (1,IY(ICYC))

   ENDIF

!....    Also call the option processor to see if there are any
!....    changes to the spore variables (We just need SPDBH at this
!....    point, this call will be repeated elsewhere in case the user
!....    wanted to change some parameter in a year that there was no
!....    harvest)
!        (ACTIVITY CODE 2431)

   CALL OPFIND (1,MYACT(2),NTODO)

   IF (NTODO .GT. 0) THEN

      DO 23 IJ = 1, NTODO
         CALL OPGET (IJ,5,KDT,IACTK,NPS,PRMS)
         JRRTYP = INT(PRMS(5))
         SPINF(JRRTYP) = PRMS(1)
         SPDBH(JRRTYP) = PRMS(2)
23       CONTINUE
   ENDIF
ENDIF

IF (LBORAX(1) .AND. DIAM .GE. BODBH) ADJBOR = BOTRT

!.... Only spore infect those stumps that are big enough to
!.... become centers.

IF (DIAM .LT. SPDBH(IDI)) GOTO 70

!.... Update outside spore stump arrays.

DENOUT = FPROB(I) * (1.0-TP) * (SAREA-PAREA(IDI)) * &
            SPINF(IDI) * (1.0-ADJBOR)

IF (DENOUT .LE. 1E-4) GOTO 50

OUTTOT = DENOUT + STOUT(IDI,IS,ISL,1)

DBHOUT(IDI,IS,ISL,1) = (DBHOUT(IDI,IS,ISL,1) * &
         STOUT(IDI,IS,ISL,1) + DIAM * DENOUT) / OUTTOT
RTOUT(IDI,IS,ISL,1) = (RTOUT(IDI,IS,ISL,1) * &
         STOUT(IDI,IS,ISL,1) + RTD * DENOUT) / OUTTOT
STOUT(IDI,IS,ISL,1) = STOUT(IDI,IS,ISL,1) + DENOUT

!.... Update inside spore stump arrays.

50 CONTINUE
DENUIN = PROBIU(I) * (1.0-TP) * SPINF(IDI) * (1.0-ADJBOR)

IF (DENUIN .LE. 1E-4) GOTO 70

UINTOT = DENUIN + STUIN(IDI,IS,ISL,1)

DBHUIN(IDI,IS,ISL,1) = (DBHUIN(IDI,IS,ISL,1) * &
         STUIN(IDI,IS,ISL,1) + DIAM * DENUIN) / UINTOT
RTUIN(IDI,IS,ISL,1) = (RTUIN(IDI,IS,ISL,1) * &
         STUIN(IDI,IS,ISL,1) + RTD * DENUIN) / UINTOT
STUIN(IDI,IS,ISL,1) = STUIN(IDI,IS,ISL,1) + DENUIN

70 CONTINUE
IF (.NOT. DEBUG) GOTO 101
   WRITE(JOSTND,*) 'SAREA PAREA FPROB PROBIU',SAREA,PAREA(IDI), &
                  FPROB(I), PROBIU(I)
   WRITE(JOSTND,*) 'BOTRT ADJBOR SPINF', BOTRT, ADJBOR, SPINF(IDI)
   WRITE(JOSTND,*) 'I ISL IISP TP DIAM RTD'
   WRITE(JOSTND,*) I,ISL,IISP,TP,DIAM,RTD
   WRITE(JOSTND,*) 'IDI IS ISL', IDI, IS, ISL
   WRITE(JOSTND,*) 'DENOUT OUTTOT DENUIN UINTOT',DENOUT,OUTTOT, &
                 DENUIN, UINTOT
   WRITE(JOSTND,*) 'DBHOUT', DBHOUT(IDI,IS,ISL,1)
   WRITE(JOSTND,*) 'DBHUIN', DBHUIN(IDI,IS,ISL,1)
   WRITE(JOSTND,*) 'RTOUT', RTOUT(IDI,IS,ISL,1)
   WRITE(JOSTND,*) 'RTUIN', RTUIN(IDI,IS,ISL,1)
   WRITE(JOSTND,*) 'STOUT', STOUT(IDI,IS,ISL,1)
   WRITE(JOSTND,*) 'STUIN', STUIN(IDI,IS,ISL,1)
101 CONTINUE

RETURN
END

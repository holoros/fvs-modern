REAL FUNCTION MISHGF(ITREE,ISPC)
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Determines the proportion of height growth lost periodically
!  due to dwarf mistletoe infection based on tree species and the
!  intensity of infection (individual tree record DMR) and adjusts
!  the normal height growth accordingly (called from DGDRIV once per
!  tree record). Also processes the MISTHMOD keyword.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ISPC:   (I) Current tree species.
!     ITREE:  (I) Current tree record number.
!     MISHGF: (O) Returns the 10 year proportion of potential height
!                growth due to mistletoe infection.
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on or off.
!     IACTK:  Passed back from OPGET (unused).
!     IDATE:  Passed back from OPGET (unused).
!     IDMR:   Dwarf mistletoe rating for current tree record (0-6).
!     IKSPC:  Species of current tree record (0-11) used for keyword.
!     NP:     Passed back from OPGET (unused).
!     NTODO:  Number of actions to perform in a cycle.
!     PRMS:   Array containing MISTHMOD keyword values.
!
!  Common block variables and parameters:
!     HGPDMR: From MISCOM; height growth potentials based on species
!                and DMR (0-6).
!     FINT:   From PLOT; current cycle length.
!     ICYC:   From CONTRL; cycle index number.
!     IMIST:  From MISCOM; individual tree mistletoe rating.
!     IY:     From CONTRL; inventory year.
!     JOSTND: From CONTRL; unit number for stand output.
!     MAXSP:  From PRGPRM; maximum number species.
!     MISCYC: From MISCOM; logical flag to process MISTHMOD keyword
!                only once per cycle.
!     MISFIT: From MISCOM; which species affected by DM (see MISINT).

!
!  Revision History :
!     30-FEB-2010 Lance R. David (FMSC)
!        Subroutine created.
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'MISCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG
REAL    PRMS(7)
INTEGER MYACTS(1)
INTEGER I,IACTK,IDATE,IDMR,IKSPC,ISPC,ITREE,NP,NTODO
!
!     DATA STATEMENTS
!
DATA MYACTS(1) / 2004 /

!.... Check for debug.

CALL DBCHK(DEBUG,'MISHGF',6,ICYC)

IF(DEBUG) WRITE(JOSTND,100)ICYC,MISCYC(2,ICYC)
100 FORMAT(' Begin MISHGF: Cycle = ',I5,' MISCYC = ',L)

!.... Initializations.

MISHGF=1.0

!.... Check MISCYC; this flag set to TRUE if the MISTHMOD keyword has
!.... been processed this cycle (i.e. this will happen only once per
!.... cycle even though this function is called for every tree record).

IF(.NOT.MISCYC(2,ICYC)) THEN
   MISCYC(2,ICYC)=.TRUE.

!....    Process MISTHMOD keyword (activity code 2004) if scheduled.

   NTODO=0
   CALL OPFIND(1,MYACTS(1),NTODO)
!
   IF(NTODO.GT.0) THEN
      DO 200 I=1,NTODO
         CALL OPGET(I,7,IDATE,IACTK,NP,PRMS)
         CALL OPDONE(I,IY(ICYC))

!....          Check for an individual species number.
!....          Get mistletoe height growth proportions from keyword;
!....          A value of 1.0 causes no height growth potential impact
!....          and values approaching 0.0 will increasingly affect DM
!....          height growth impact (height growth reduction).
!....          MISFIT insures that only species of this variant that
!....          are affected by mistletoe will get height growth
!....          modifiers, others will be set to 1.0 which is no affect.

         IKSPC=IFIX(PRMS(1))
         IF(IKSPC.NE.0) THEN
            DO 120 IDMR=2,7
               IF(MISFIT(IKSPC).EQ.1) THEN
                  HGPDMR(IKSPC,IDMR)=PRMS(IDMR)
               ELSE
                  HGPDMR(IKSPC,IDMR)=1.0
               ENDIF
120             CONTINUE
         ELSE

!....             Otherwise new growth modifiers applied to all
!....             affected species.

            DO 150 IKSPC=1,MAXSP
               DO 140 IDMR=2,7
                  IF(MISFIT(IKSPC).EQ.1) THEN
                     HGPDMR(IKSPC,IDMR)=PRMS(IDMR)
                  ELSE
                     HGPDMR(IKSPC,IDMR)=1.0
                  ENDIF
140                CONTINUE
150             CONTINUE
         ENDIF
200       CONTINUE
   ENDIF
ENDIF

!.... Set mistletoe height growth proportion including effects
!.... of user supplied proportions based on species and DMR.

IDMR=IMIST(ITREE)
MISHGF=HGPDMR(ISPC,IDMR+1)

!.... Force normal growth for uninfected trees.

IF(IDMR.EQ.0) MISHGF=1.0

!.... Force upper and lower bounds on height growth.

IF(MISHGF.GT.1.0) MISHGF=1.0
IF(MISHGF.LT.0.0) MISHGF=0.0

!.... Common return.

IF(DEBUG) WRITE(JOSTND,9010) ICYC, ITREE, IMIST(ITREE), ISPC, &
      MISHGF
9010 FORMAT(' End MISHGF: Cycle, Tree record, IMIST, SPC, MISHGF = ', &
      I5,I7,I3,I5,F6.2)

RETURN
END

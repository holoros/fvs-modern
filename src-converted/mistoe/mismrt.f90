SUBROUTINE MISMRT(MFLAG)
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!    Processes the MISTMORT keyword.  Calculates the periodic mortality
!  rate for each tree record due to dwarf mistletoe infection (based on
!  the severity of the infection).  If MFLAG is true and the dwarf
!  mistletoe induced mortality is greater than the background
!  mortality, reduces the number of trees in the record by a percentage
!  equal to the calculated rate.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     MFLAG:  Logical flag to update WK2 with mistletoe mortality.
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on or off.
!     DMMORT: Periodic mortality rate induced by dwarf mistletoe.
!     I1:     Beginning tree record pointer for a species.
!     I2:     Ending tree record pointer for a species.
!     I3:     Tree record loop counter.
!     IACTK:  Passed back from OPGET (unused).
!     IDATE:  Passed back from OPGET (unused).
!     IDMR:   Dwarf mistletoe rating for current tree record (0-6).
!     ISPC:   Species loop counter.
!     ITREE:  Current tree record pointer.
!     NP:     Passed back from OPGET (unused).
!     NTODO:  Number of actions to perform in a cycle.
!     PRM:    Array containing MISTMORT keyword values.
!     PTPA:   Trees per acre.
!     WKI:    TPA to die this cycle due to dwarf mistletoe infection.
!
!  Common block variables and parameters:
!     DBH:    From ARRAYS; current tree DBH.
!     DMMMLT: From MISCOM; dwarf mistletoe mortality multiplier.
!     DMMTPA: From MISCOM; array containing TPA mortality from DM.
!     FINT:   From PLOT; current cycle length.
!     ICYC:   From CONTRL; cycle index number.
!     IMIST:  From MISCOM; individual tree mistletoe rating.
!     IND1:   From ARRAYS; tree list access.
!     ISCT:   From CONTRL; species ordered list pointer.
!     IY:     From CONTRL; inventory year.
!     JOSTND: From CONTRL; unit number for stand output.
!     MAXSP:  From PRGPRM; maximum number species.
!     MAXTRE: From PRGPRM; maximum number tree records.
!     MISFIT: From MISCOM; which species affected by DM (see MISINT).
!     PMCSP:  From MISCOM; percent mortality coefficients by species.
!     PROB:   From ARRAYS; Trees per acre per tree record.
!     WK2:    From ARRAYS; array containing TPA to die this cycle.
!
! Revision History:
!     02-AUG-99; Lance R. David (FHTET-FC)
!        Added definition and data statements for MYACTS array
!        and replaced the activity code literal in the CALL OPFIND
!        statements.
!        This change was provided by Bob Havis (FMSC) to eliminate
!        LF95 FORTRAN compiler warnings.
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'MISCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG,MFLAG
INTEGER COUNT
REAL    PRM(3),MRTSM1,MRTSM2,MRTSM3,DMMORT,PTPA,WKI
INTEGER MYACTS(1)
INTEGER I,I1,I2,I3,IACTK,IDATE,IDMR,ISPC,ITREE,NP,NTODO

!.... DATA STATEMENTS
DATA MYACTS(1) / 2003 /

!.... Check for debug.

CALL DBCHK(DEBUG,'MISMRT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,50)ICYC,FINT,MFLAG
50 FORMAT(' Begin MISMRT: Cycle, length = ',I5,F5.1,L8)

!.... Process MISTMORT keyword (activity code 2003).

NTODO=0
CALL OPFIND(1,MYACTS(1),NTODO)
IF(NTODO.NE.0) THEN
   DO 150 I=1,NTODO
      CALL OPGET(I,3,IDATE,IACTK,NP,PRM)
      CALL OPDONE(I,IY(ICYC))

!....       Get mistletoe mortality multipliers from keyword; > 1.0 will
!....       increase mortality rate, < 1.0 will decrease mortality rate.
!....       Check for a particular species.
!....       MISFIT insures that only species of this variant that
!....        are affected by mistletoe will get mort. multipliers -
!....        others will be set to 0.

      ISPC=IFIX(PRM(1))
      IF(ISPC.NE.0) THEN
         DMMMLT(ISPC)=PRM(2)*MISFIT(ISPC)
      ELSE

!....          Otherwise default to all species.

         DO 100 ISPC=1,MAXSP
            DMMMLT(ISPC)=PRM(2)*MISFIT(ISPC)
100          CONTINUE
      ENDIF
150    CONTINUE
ENDIF

!.... Loop through for each species.

COUNT=0
MRTSM1=0.0
MRTSM2=0.0
MRTSM3=0.0

DO 300 ISPC=1,MAXSP
   I1=ISCT(ISPC,1)

!....    If there are no trees of this species, skip out.

   IF(I1.EQ.0) GO TO 300
   I2=ISCT(ISPC,2)

!....    Loop through for each tree.

   DO 200 I3=I1,I2
      ITREE=IND1(I3)
      PTPA=PROB(ITREE)
      IDMR=IMIST(ITREE)

!....       If there are no trees represented by this record,
!....       or if there is no mistletoe infection, skip out.

      IF(PTPA.LE.0.OR.IDMR.EQ.0) THEN
         DMMTPA(ITREE)=0.0
         GO TO 200
      ENDIF

!....       Calculate the mortality rate based on current DMR.

      DMMORT=PMCSP(ISPC,1)+PMCSP(ISPC,2)*IDMR+ &
            PMCSP(ISPC,3)*IDMR**2

!....       Multiply by user input multiplier.

      DMMORT=DMMORT*DMMMLT(ISPC)

!....       Check for small trees (less than 9" DBH) and increase
!....       mortality rate accordingly.

      IF(DBH(ITREE).LT.9.0) DMMORT=DMMORT*1.2

!....       Force normal growth for uninfected trees.

      IF(IDMR.EQ.0) DMMORT=0.0

!....       Force upper and lower bounds on rate based on diameter.

      IF(DMMORT.LT.0.0) DMMORT=0.0
      IF(DBH(ITREE).LT.9.0) THEN
         IF(DMMORT.GT.0.71) DMMORT=0.71
      ELSE
         IF(DMMORT.GT.0.5) DMMORT=0.5
      ENDIF

!....       Scale the 10 yr. mortality rate to FINT cycle length.
!....        Using exponential functional form.

      DMMORT=1.0-(1.0-DMMORT)**(FINT/10.0)

      WKI=PTPA*DMMORT

!....       Compare dwarf mistletoe induced mortality with background
!....       mortality and store the larger one in WK2, if MFLAG is
!....       true.

      IF(MFLAG.AND.WK2(ITREE).LT.WKI) WK2(ITREE)=WKI

!....       Save mistletoe mortality (in TPA) to array.

      DMMTPA(ITREE)=WKI
      COUNT=COUNT+1
      MRTSM1=MRTSM1+DMMTPA(ITREE)
      MRTSM2=MRTSM2+CFV(ITREE)
      MRTSM3=MRTSM3+DMMTPA(ITREE)*CFV(ITREE)

!....    End tree loop.

200    CONTINUE

!.... End species loop.

300 CONTINUE

!.... Common return.

IF(DEBUG) WRITE(JOSTND,9010)ICYC,COUNT,MRTSM1,MRTSM2,MRTSM3
9010 FORMAT(' End MISMRT: Cycle, Count, dmmtpa, cfv, mrt =', &
          I5,I5,3F10.2)

RETURN
END

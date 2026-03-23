SUBROUTINE MISDAM(ITREE,ICODES)
!**********************************************************************
! MISTOE
!----------------------------------------------------------------------
!  Purpose:
!     Processes damage codes to determine whether the tree in
!  question has dwarf mistletoe. The codes are as follows:
!     30 Dwarf Mistletoe
!     31 Lodgepole Pine Mistletoe
!     32 Western Larch Mistletoe
!     33 Douglas Fir Mistletoe
!     34 Ponderosa Pine Mistletoe
!  Code 30 is recorded if mistletoe is found on a species other
!  than LP, WL, DF or PP.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ICODES: (I) Array of mistletoe damage codes.
!     ITREE:  (I) Current tree record number.
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on or off.
!     J:      Loop counter.
!
!  Common block variables and parameters:
!     ICYC:   From CONTRL; cycle index number.
!     IMIST:  From MISCOM; tree mistletoe rating (Hawksworth 0-6).
!     JOSTND: From CONTRL; unit number of stand output.
!     MISFLG: From MISCOM; logical flag to turn DM effects on or off.
!
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'MISCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG
INTEGER ICODES(6),ITREE,J

!.... Data statements.

!.... Check for debug.

CALL DBCHK(DEBUG,'MISDAM',6,ICYC)

!.... Initializations.

IMIST(ITREE)=0

!.... Check mistletoe processing option; if MISFLG is FALSE then
!.... process without the effects of dwarf mistletoe (skip out of
!.... this routine).

IF(.NOT.MISFLG) GO TO 9000

!.... Process damage codes.

DO 100 J=1,5,2
   IF((ICODES(J).GE.30 .AND. ICODES(J).LE.34)) THEN
      IMIST(ITREE)=ICODES(J+1)
      IF(IMIST(ITREE).GT.6) IMIST(ITREE)=6
      IF(IMIST(ITREE).LT.0) IMIST(ITREE)=0

!....       PROCESS FIA DWARF MISTLETO DAMAGE CODES.
!....       WHEN FIA CODES FOR ARCEUTHOBIUM ARE FOUND, THE
!....       SEVERITY CODES 1 - 6 WILL BE RETAINED. OTHERWISE,
!....       SEVERITY CODE 3 IS ASSIGNED.
!....
!....    These codes are from the FIA website document 8/25/2023
!....    FIA Database Description and User Guide for Phase 2 (version 9.0.1)
!....    pages H-29 and H-30
!....
   ELSEIF (ICODES(J).EQ.23005 .OR. ICODES(J).EQ.23006 &
         .OR. ICODES(J).EQ.23007 .OR. ICODES(J).EQ.23008 &
         .OR. ICODES(J).EQ.23009 .OR. ICODES(J).EQ.23010 &
         .OR. ICODES(J).EQ.23011 .OR. ICODES(J).EQ.23012 &
         .OR. ICODES(J).EQ.23013 .OR. ICODES(J).EQ.23014 &
         .OR. ICODES(J).EQ.23015 .OR. ICODES(J).EQ.23016 &
         .OR. ICODES(J).EQ.23017 .OR. ICODES(J).EQ.23021 &
         .OR. ICODES(J).EQ.23023 .OR. ICODES(J).EQ.23024) THEN

      IF(ICODES(J+1).GE.1 .AND. ICODES(J+1).LE.6) THEN
         IMIST(ITREE)=ICODES(J+1)
      ELSE
         IMIST(ITREE)=3
      ENDIF
   ENDIF
100 CONTINUE

IF(DEBUG) WRITE(JOSTND,9010)ICYC,ITREE,IMIST(ITREE)
9010 FORMAT(' MISDAM: Cycle = ',I5,'  IMIST(',I4,')= ',I2)

!.... Common return.

9000 CONTINUE

RETURN
END

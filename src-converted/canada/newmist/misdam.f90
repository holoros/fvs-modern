SUBROUTINE MISDAM(ITREE,ICODES)
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
!  **MISDAM--MS  Date of last revision:  07/15/94
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
!     DMRATE: From DMCOM;
!     ICYC:   From CONTRL; cycle index number.
!     IMIST:  From MISCOM; tree mistletoe rating (Hawksworth 0-6).
!     JOSTND: From CONTRL; unit number of stand output.
!     MISFLG: From MISCOM; logical flag to turn DM effects on or off.
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'MISCOM.f90'
INCLUDE 'DMCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG
INTEGER ITREE, J
INTEGER ICODES(6)

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
   IF(ICODES(J).EQ.30.OR.ICODES(J).EQ.31.OR.ICODES(J).EQ.32 .OR. &
         ICODES(J).EQ.33.OR.ICODES(J).EQ.34) THEN
      IMIST(ITREE)=ICODES(J+1)
      IF(IMIST(ITREE).GT.6) IMIST(ITREE)=6
      IF(IMIST(ITREE).LT.0) IMIST(ITREE)=0

!           Copy value of IMIST to new S+I variable

      DMRATE(ITREE) = IMIST(ITREE)

   ENDIF
100 CONTINUE

IF(DEBUG) WRITE(JOSTND,9010)ICYC,ITREE,IMIST(ITREE)
9010 FORMAT(' MISDAM: Cycle = ',I5,'  IMIST(',I4,')= ',I2)

!.... Common return.

9000 CONTINUE

RETURN
END

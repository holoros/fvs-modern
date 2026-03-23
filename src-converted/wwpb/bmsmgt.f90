SUBROUTINE BMSMGT (IYR)
!----------
! WWPB $Id$
!----------
!     CALLED FROM BMDRV
!**********************************************************************
! **BMSMGT    Date of last revision:  June 22, 1994
!
!  Routine to remove fresh slash (created from FVS-level harvest or BM-
!  model salvage cut (but not from sanititation cut)). Model removes
!  user-defined proportion of DOWNED (only) dead wood from host and non-
!  host pools.
!
!  Definitions:
!     ISC:    Loop counter over size classes
!     JSC:    Size class < 3 or > 3
!     PSLREM: Proportion of slash to remove that is < 3 or >3 in
!                 -given by keyword.
!
!  Common block variables and parameters:
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'PPCNTL.f90'
INCLUDE 'BMPRM.f90'
INCLUDE 'BMCOM.f90'

!.... Variable declarations.

INTEGER ISC, JSC
INTEGER SCNT, MYLST(MXSTND)
LOGICAL LOK
REAL    PRMS(2)
REAL    PSLREM(2)

SAVE

IF(LBMDEB) WRITE(JBMBPR,10) IYR
10 FORMAT(' Begin BMSMGT: Year= ',I5)

!     Initializations

IYR1 = 0
NPRMS = 2
IYR1 = IYR
CALL GPGET2 (315,IYR1,7,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
IF (LOK) THEN
  PSLREM(1) = PRMS(1)
  PSLREM(2) = PRMS(2)

  DO 200 I = 1, SCNT

    ISTD = MYLST(I)
    IF (.NOT.STOCK(ISTD) .OR. ISTD .LE. 0) GOTO 200

!         Remove some dead wood from downed dead wood, but only for the
!         current year.
!         (remember that DWPHOS is all > 3in, but is in 3 size classes and
!          DDWP is only <3 or > 3in)

    DO 45 ISC= 1, MXDWHZ
      JSC = MIN0(ISC,MXDWSZ)

      DWPHOS(ISTD,2,ISC) = DWPHOS(ISTD,2,ISC) * (1 - PSLREM(2))

      IF (ISC .LE. MXDWSZ) &
             DDWP(ISTD,ISC,1) = DDWP(ISTD,ISC,1) * (1 - PSLREM(JSC))

45     CONTINUE

200   CONTINUE
ENDIF

IF(LBMDEB) WRITE(JBMBPR,99) IYR
99 FORMAT(' End BMSMGT: Year= ',I5)

RETURN
END

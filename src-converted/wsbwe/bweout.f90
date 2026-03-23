SUBROUTINE BWEOUT
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!
!     COPIES PRINT DATA FROM FILE JOWSBW TO JOSTND.
!
!     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
!     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983
!
!     minor changes by K.Sheehan 7/96 to remove LBWDEB
!
!     CALLED FROM :
!
!       MAIN   - MAIN PROGRAM FOR THE SINGLE STAND PROGNOSIS MODEL.
!
!  Revision History:
!    07-JUN-00 Lance R. David (FHTET)
!      .Changed final REWIND of JOWSBW to CLOSE so that output from
!       from a previous stand is not mistaken as output for the current
!       FVS stand in a multiple stand serial run. This routine interprets
!       an open JOWSBW as the defoliation model being active.
!    16-OCT-2006 Lance R. David (FHTET)
!       Changed local variable name from RECORD to RECRD.
!    14-JUL-2010 Lance R. David (FMSC)
!       Added IMPLICIT NONE and declared variables as needed.
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BWESTD.f90'
INCLUDE 'BWECOM.f90'
INCLUDE 'BWEBOX.f90'
!
!OMMONS
!
LOGICAL LOPEN
CHARACTER*133 RECRD
INTEGER ISTLNB
!
IF (JOWSBW.EQ.JOSTND) GOTO 1000
!
!     FIND OUT IF THE FILE IS OPENED...IF NOT BYPASS.
!
INQUIRE (UNIT=JOWSBW,OPENED=LOPEN)
IF (.NOT.LOPEN) GOTO 1000
!
!     REWIND THE TEMPORARY OUTPUT FILE.
!
!REWIND JOWSBW
REWIND (JOWSBW, ERR=1000) ! temporary patch to allow stop/start
!
!     COPY THE FILE TO THE PRINTER.
!
10 CONTINUE
READ (JOWSBW,'(A)',END=40) RECRD
WRITE (JOSTND,'(A)') RECRD(1:MAX0(1,ISTLNB(RECRD)))
GOTO 10
40 CONTINUE
!
!     PREPARE THE FILE FOR THE NEXT STAND.
!

!     REWIND JOWSBW
CLOSE (JOWSBW)
1000 CONTINUE
!
!  PRINT THE SPECIAL EVENTS TABLE
!
IF (LP4) CALL BWEP4(2)
!
RETURN
END

SUBROUTINE TMHED (IOUT, NPLT, MGMID)
IMPLICIT NONE
!----------
! DFTM $Id$
!----------
!
! Revision History:
!     23-DEC-99; Lance R. David (FHTET-FC)
!        Updated for expansion of FVS stand id (variable NPLT)
!        from 8 to 26 characters.
!
!**********************************************************************

CHARACTER*26 NPLT
CHARACTER*4 MGMID

INTEGER IOUT
!
!     WRITES HEADING(S) FOR INSECT MODEL OUTPUT
!
WRITE (IOUT,10) NPLT, MGMID
10 FORMAT (' * PRE-RELEASE *  DOUGLAS-FIR TUSSOCK MOTH IN ', &
          'DOUGLAS-FIR AND GRAND FIR:  DFTM VERSION 3.1;  ', &
          'PROGNOSIS (INLAND EMPIRE) 4.0'// &
           T19,' STAND ID= ',A26,'; MANAGEMENT ID= ',A4/)

RETURN
END

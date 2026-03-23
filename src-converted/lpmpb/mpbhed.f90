SUBROUTINE MPBHED
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     WRITES HEADING FOR MPB MODEL OUTPUT
!
!     Writes a warning for the cix, ttx or bmx multi-pest model
!     (RNH June 98),  if the logical variable LXNOTE is set in
!     mpbin.f
!
! Revision History
!   12/23/99 Lance R. David (FHTET-FC)
!     Updated for expansion of FVS stand id (variable NPLT)
!     from 8 to 26 characters.
!   11/1100 Lance R. David (FHTET-FC)
!     Removed use of Hollerith constants from FORMAT.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!
!**********************************************************************
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'PLOT.f90'

INCLUDE 'MPBCOM.f90'
!
!OMMONS
!
WRITE (JOMPB,10)  NPLT,MGMID

IF (LXNOTE) THEN
WRITE(JOMPB, 9991)
LXNOTE = .FALSE.
ENDIF

10 FORMAT (/,T34,'MOUNTAIN PINE BEETLE IN LODGE', &
           'POLE PINE          VERSION 2.0',//, &
           'STAND ID: ',A26,'; MANAGEMENT ID: ',A4,/)

9991 FORMAT (' *=================================================', &
    '===========================*',/, &
    ' *---> Note:  The combined insect and pathogen models (in ', &
    'one executable) <---*',/, &
    ' *---> should NOT be used without close consultation with ', &
    'the forest''s    <---*',/, &
    ' *---> pathologist and entomologist.  Because of the ', &
    'potential for more   <---*',/, &
    ' *---> than one insect and/or pathogen acting on the same ', &
    'tree species,   <---*',/, &
    ' *---> the interpretation of the results of the combined ', &
    'model can be     <---*',/, &
    ' *---> inaccurate without appropriate model knowledge ', &
    'and/or experience.  <---*',/, &
    ' *==========================================================', &
    '==================*',/)

RETURN
END

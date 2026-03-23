SUBROUTINE DFBHED
IMPLICIT NONE
!----------
! DFB $Id$
!----------
!
!  WRITES HEADING FOR DFB MODEL OUTPUT
!
!     Writes a warning for the cix, ttx or bmx multi-pest model
!     (RNH June 98),  if the logical variable LXNOTE is set in
!     dfbin.f
!
!  CALLED BY :
!     DFBOUT  [DFB]
!
!  CALLS :
!     NONE
!
!  COMMON BLOCK VARIABLES USED :
!     JODFB  - (DFBCOM)  INPUT
!     MGMID  - (PLOT)    INPUT
!     NPLT   - (PLOT)    INPUT
!
!
! Revision History:
!     23-DEC-99; Lance R. David (FHTET-FC)
!        Updated for expansion of FVS stand id (variable NPLT)
!        from 8 to 26 characters.
!
!**********************************************************************
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'DFBCOM.f90'
!
!
!OMMONS
!
WRITE (JODFB,100) NPLT, MGMID

IF (LXNOTE) THEN
WRITE(JODFB, 9991)
LXNOTE = .FALSE.
ENDIF

100 FORMAT (//,T16,'DOUGLAS-FIR BEETLE IN DOUGLAS-FIR', &
           '             VERSION 1.0',//, &
           ' STAND ID:  ',A26,'; MANAGEMENT ID:  ',A4,/)

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
    ' *========================================================', &
    '====================*',/)

RETURN
END

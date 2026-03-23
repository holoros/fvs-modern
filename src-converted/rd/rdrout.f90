SUBROUTINE RDROUT
IMPLICIT NONE
!----------
! RD $Id$
!----------
!  ****** NOTICE ********* NOTICE ******* NOTICE ****** NOTICE *******
!  ***                                                             ***
!  *** 3/24/2015 Implementation of General Report Writer facility  ***
!  *** has made this subroutined obsolete. It is no longer needed. ***
!  *******************************************************************
!
!  WRITES FINAL ROOT DISEASE SUMMARY OUTPUT.  ALSO WRITES DETAILED REPORT IF
!  THE RRDOUT KEYWORD IS USED.
!
!  CALLED BY :
!     MAIN    [PROGNOSIS]
!
!  CALLS     :
!     NONE
!
!  Revision History :
!   11/06/89 - Last revision date.
!   09/02/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'RDPARM.f90'

INCLUDE 'CONTRL.f90'

INCLUDE 'RDCOM.f90'
!
!OMMONS
!
CHARACTER*132 DSTRNG

IF (IROOT .EQ. 0 .OR. IRUNIT .EQ. JOSTND) RETURN

!
!     WRITE END OF FILE MARKER THEN REWIND IRUNIT
!
ENDFILE IRUNIT
REWIND IRUNIT

125 CONTINUE
READ (IRUNIT,130,END=200) DSTRNG
130 FORMAT(A132)
WRITE (JOSTND,130) DSTRNG
GOTO 125

200 CONTINUE
REWIND IRUNIT
!
!     WRITE DETAILED OUPUT IF ASKED FOR.
!
IF (IRDOUT .EQ. 1) THEN
!
!        WRITE END OF FILE MARKER THEN REWIND IOUNIT.
!
   ENDFILE IOUNIT
   REWIND  IOUNIT
!
!        WRITE IOUNIT (DETAILED REPORT) TO END OF JOSTND.
!
300    CONTINUE
   READ (IOUNIT, 130, END=400) DSTRNG
   WRITE (JOSTND, 130) DSTRNG
   GOTO 300

400    CONTINUE
   REWIND IOUNIT
ENDIF

RETURN
END

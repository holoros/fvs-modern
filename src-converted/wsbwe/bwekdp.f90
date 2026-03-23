SUBROUTINE BWEKDP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!  Revision History:
!    14-JUL-2010 Lance R. David (FMSC)
!       Previous note revision date 1986
!       Added IMPLICIT NONE and declared variables as needed.
!----------
CHARACTER*10 KARD(12)
CHARACTER*8 KEYWRD
INTEGER IOUT, IRECNT
REAL ARRAY(12)
WRITE (IOUT,70) IRECNT,KEYWRD,ARRAY,KARD
70 FORMAT (/,'CARD NUM =',I5,'; KEYWORD FIELD = ''',A8,''''/, &
           '     PARAMETERS ARE:',7F14.7,/, &
           '     COL 11 TO 80 =''',7A10,'''')
!
RETURN
END

SUBROUTINE SVGRND (NOUT,KYLAST,KYFRST,IFIREFLG)
IMPLICIT NONE
!----------
! BASE $Id$
!----------
!     SVS BASIC GROUND DEFINITION GENERATION
!     D.L.GAMMEL    -- SEM         -- JUNE 2002
!     D. ROBINSON   -- ESSA        -- MAY 2005
!     S.SCHAROSCH   -- Abacus      -- MAR 2008
!
!     INPUT:
!     NOUT - THE OUTPUT FILE REFERENCE NUMBER
!     KYFRST - THE INDEX FOR THE FIRST CHAR NEEDED FROM
!              FROM THE KEYWORD FOR GENERATING THE FILE NAME
!     KYLAST - THE INDEX OF THE LAST CHAR NEEDED FOR THE
!              FILE NAME
!     IFIREFLG- THE IDENTIFIER OF THE CALLING CONTEXT
!               0=INVENTORY TIME
!               1=START OF CYCLE
!               2=AFTER CUTS
!               3=END OF PROJECTION (CALLED FROM MAIN)
!               4=CALLED FROM FIRE MODEL, INCLUDES FLAMES
!               5=CALLED FROM FIRE MODEL, NO FLAMES
!
!     BASIC GROUND COLOR CODES (SET VIA SVS KEYWORD FIELD 5):
!     ICOLIDX  COLOR SCHEME      COLOR CODES
!              BLACK               0
!              BLUE                1
!              GREEN               2
!              CYAN                3
!              RED                 4
!              MAGENTA             5
!              BROWN               6
!              GRAY                7
!              DARK GRAY           8
!              LIGHT BLUE          9
!              LIGHT GREEN        10
!              LIGHT CYAN         11
!              LIGHT RED          12
!              LIGHT MAGENTA      13
!              YELLOW             14
!              WHITE              15
!        1     REDS/ORANGES       16 to 27
!        2     TANS               28 to 39
!        3     REDDISH BROWNS     40 to 51
!        4     LT GREYS           52 to 63
!        5     LIME GREENS        64 to 75
!        6     DARK GREENS        76 to 87
!        7     MEDIUM BLUEGREENS  88 to 99
!        8     DARK GREENS       100 to 111
!        9     DARK GREYS        112 to 123
!       10     LIGHT BROWNS      124 to 135
!       11     KHAKI/BROWNS      136 to 147
!       12     LIGHT GREENS      148 to 159
!       13     DARK BLUEGREENS   160 to 171
!       14     CHOCOLATE BROWNS  172 to 183
!       15     DARK TANS         184 to 195
!       16     YELLOWS           196 to 207
!       17     MEDIUM GREYS      208 to 219
!       18     BROWNS            220 to 231
!       19     GREENS            232 to 243
!       20     DARK BROWNS       244 to 255

!OMMONS

INCLUDE 'PRGPRM.f90'

INCLUDE 'SVDATA.f90'

INCLUDE 'CONTRL.f90'

!OMMONS

CHARACTER*256 GRIDLINE
CHARACTER*100  FILENAME
CHARACTER*30 COLORCODE
INTEGER ICOLOR,GRNDOUT,NOUT,I,J,X,Y,INTCOLOR,KYFRST,KYLAST,KODE, &
           IFIREFLG
LOGICAL LMONO
REAL    K,FLINE,FLNY

DIMENSION FLINE(256)
DIMENSION FLNY(20)

ICOLOR = ICOLIDX*12+4
GRNDOUT = 222
COLORCODE = "ABCDEFGHIJ0123456789QRSTUVWXYZ"

!     If the base color set was input as a negative value,
!     interpret it as a request to generate a monochrome ground
!     surface of the indicated color (so CWD objects are more visible)

IF ( ICOLIDX .LT. 0 ) THEN
  LMONO = .TRUE.
  ICOLIDX = (-1) * ICOLIDX
  ICOLOR = ICOLIDX*12+14
ELSE
  LMONO = .FALSE.
ENDIF

!     CREATE THE GROUND FILE NAME

IF(IFIREFLG.GT.3) THEN
  IF (NIMAGE.LT.1000) THEN
    WRITE(FILENAME,10)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
10     FORMAT(A,'_g',I3.3,'_c',I3.3,'_',I3.3,'.grd')
  ELSE
    WRITE(FILENAME,20)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
20     FORMAT(A,'_g',I3.3,'_c',I3.3,'_',I6.6,'.grd')
  ENDIF
ELSE
  WRITE(FILENAME,'(A,''_g'',I3.3,''_c'',I3.3,''.grd'')') &
           KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX
ENDIF

!     ADD GROUNDFILE LINE TO SVS FILE

WRITE(NOUT, '(''#GROUNDFILE '',A/)') TRIM(FILENAME)

!     IF THIS IS NOT A FIRE FILE OR THE BEGINNING OF THE INVENTORY
!     THEN WE ARE FINISHED

IF(IFIREFLG.GE.1.AND.IFIREFLG.LE.3) RETURN

!     TRY OPENING FILE WITH PATH NAME INCLUDED

CALL MYOPEN(GRNDOUT,TRIM(KWDFIL(:KYLAST)//'/'//FILENAME), &
     5,120,0,1,1,0,KODE)

!     IF OPEN FAILS THEN TRY OPENING FILE W/OUT THE PATH

IF(KODE.GT.0) THEN
  CALL MYOPEN(GRNDOUT,TRIM(FILENAME),5,120,0,1,1,0,KODE)
  IF(KODE.GT.0) THEN
   IGRID=0
   RETURN
  ENDIF
ENDIF

!     PRINT OUT THE BASE COLOR SET
!     The base color set is comprised of:
!       1) User-specified color scheme from the SVS keyword
!       2) Colors 16-25 (red/orange): used for red fire line
!       3) Colors 113-122 (dark greys): used for black fire line

WRITE(GRNDOUT, '(2I5)') IGRID, IGRID
IF ( LMONO ) THEN
  DO I = 1, 10
    WRITE(GRNDOUT,30) COLORCODE(I:I),(ICOLOR)
  ENDDO
ELSE
  DO I = 1, 10
    WRITE(GRNDOUT,30) COLORCODE(I:I),(ICOLOR+I-0)
  ENDDO
30   FORMAT(A1,I3)
ENDIF

DO I = 11, 20
  WRITE(GRNDOUT,30) COLORCODE(I:I),(I+5)
ENDDO
DO I = 21, 30
  WRITE(GRNDOUT,30) COLORCODE(I:I),(I+92)
ENDDO
WRITE(GRNDOUT, '(''* 7'')')

!     IF THERE IS A FIRE THEN GRAB THE FIRE LINE

IF(IFIREFLG.EQ.4) THEN
  CALL FMGETFL(20, FLNY)
  DO I = 1,IGRID
    J = INT(I*20/IGRID+1)
    IF(J.GT.20) J = 20

    IF (IMETRIC.EQ.0) THEN
      FLINE(I)=IGRID - INT((FLNY(J) / 208.7 * IGRID) + 0.5)
    ELSE
      FLINE(I)=IGRID - INT((FLNY(J) / 100.0 * IGRID) + 0.5)
    ENDIF

  ENDDO
ENDIF

!     RANDOMLY GENERATE THE GROUND FILE

DO Y = 1, IGRID
  DO X = 1, IGRID
    CALL SVRANN(K)

!         IF THERE IS A FIRE THEN GENERATE THE FIRE EFFECTS

    IF(IFIREFLG.EQ.4) THEN
!           CREATE RED LINE
      IF(Y.GT.FLINE(X).AND.Y.LE.(FLINE(X)+(IGRID/10))) THEN
        INTCOLOR = INT(K * 10) + 11
!           CREATE RED OR BLACK LINE
      ELSEIF(Y.GT.(FLINE(X)+IGRID/10).AND. &
                Y.LE.(FLINE(X)+IGRID/5)) THEN
        INTCOLOR = INT(K * 20) + 11
!           CREATE BLACK LINE
      ELSEIF(Y.GT.(FLINE(X)+IGRID/5)) THEN
        INTCOLOR = INT(K * 10) + 21
      ELSE
        INTCOLOR = INT(K * 10) + 1
      ENDIF

!         NO FIRE

    ELSEIF(IFIREFLG.EQ.5) THEN
!           AFTER A FIRE
      INTCOLOR = INT(K * 10) + 21
    ELSE
!           NO FIRE EFFECTS
      INTCOLOR = INT(K * 10) + 1
    ENDIF
    GRIDLINE(X:X)=COLORCODE(INTCOLOR:INTCOLOR)
  ENDDO
  WRITE(GRNDOUT, '(A)') GRIDLINE(:IGRID)
ENDDO
CLOSE(GRNDOUT)
END


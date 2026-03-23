SUBROUTINE FMSSEE (IT,JSP,D,H,SNUM,ITYP,DEBUG,IOUT)
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
! PURPOSE:
!     SEES WHAT RANGE OF SNAG HEIGHTS WILL OCCUR IN EACH SPECIES+DBH
!     CLASS OF NEWLY CREATED SNAGS (HARVESTED OR KILLED MATERIAL), SO THAT
!     FMSADD WILL KNOW WHICH CLASSES NEED TO BE SPLIT INTO 2 HEIGHT CLASSES.
!     ALSO KEEPS TRACK OF TOTAL SNAG DENSITY EXPECTED IN EACH SPECIES+DBH
!     CLASS.
!-----------
!     CALLED FROM:  FMSCUT
!                   FMKILL
!                   CRATET
!-----------
!     Local variable definitions:
!
!     IT:   tree record
!     JSP:  species
!     D:    dbh
!     H:    height
!     SNUM: potential number of snags
!     ITYP: Code of where snags are coming from (0=fmscut,  1=kill,
!                                                3=initial, 4=keyword)
!
!OMMONS
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

INCLUDE 'CONTRL.f90'
INCLUDE 'FMCOM.f90'
!
!OMMONS

INTEGER DBHCL
LOGICAL DEBUG
INTEGER IOUT,ITYP,JSP,IT
REAL    SNUM,H,D

!     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

IF (.NOT. LFMON) RETURN

IF (DEBUG) WRITE (IOUT,10) IT,JSP,D,H,SNUM,ITYP
10 FORMAT (' IN FMSSEE, IT=',I4,' JSP=',I3,' D=',F7.3, &
            ' H=',F7.3,' SNUM=',F7.3,' ITYP=',I3)

!     If this was called after a stand entry set the harvest year
!     even if no snags were created.
!     IF NO NEW SLASH IS CREATED WE DON'T WANT TO SET HARVYR
!     ASSIGNMENT MOVED FROM HERE TO **FMSCUT**
!     SB: MARCH 2002
!
!sng  IF (ITYP .EQ. 0) HARVYR = IY(ICYC)
!     IF (ITYP .EQ. 0) HARVYR = IY(ICYC)

!     If there are no snags, then return

IF (SNUM .LE. 0.0) RETURN

SNGNEW(IT) = SNUM

!     Find the dbh-class of the new snags

IF (D .GE. 36.0) THEN
  DBHCL = 19
ELSE
  DBHCL = INT( (D/2.0) + 1.0)
END IF

!     Next, find the maximum and minimum height of each class, and
!     sum the number of snags in it.

IF (H .GT. MAXHT(JSP,DBHCL)) MAXHT(JSP,DBHCL) = H
IF (H .LT. MINHT(JSP,DBHCL)) MINHT(JSP,DBHCL) = H
DSPDBH(JSP,DBHCL) = DSPDBH(JSP,DBHCL) + SNGNEW(IT)

IF (ITYP .EQ. 4) SNGNEW(IT) = 0.0

RETURN
END


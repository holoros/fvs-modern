SUBROUTINE FMSDIT
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!
!     PART OF THE FIRE MODEL EXTENSION. THIS ROUTINE IS ENTERED
!     AT THE START OF EACH CYCLE. VALUES FOR SCCF, AND FIRKIL
!     ARE ZEROED HERE,
!
!     Called from GRINCR
!
!  Local Variable Definitions:
!     CYCLEN: length in years of the current FVS cycle
!     NEWBOT: the height of the bottom of the crown in the current cycle
!     OLDBOT: the height of the bottom of the crown in the previous cycle
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'

INTEGER I, J
REAL    CYCLEN, NEWBOT, OLDBOT
LOGICAL DEBUG
REAL    TONREM, X

!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMSDIT',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
7 FORMAT(' ENTERING ROUTINE FMSDIT CYCLE = ',I2,' LFMON=',L2)

!     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE

IF (.NOT. LFMON) RETURN

!     SET IFMYR1 TO -1 TO SIGNAL THAT THE FIRE SIMULATION
!     HAS NOT STARTED...THEN SET SOME FIRE MODEL EVENT
!     MONITOR VARIABLES TO UNSET.

IFMYR1=-1

CALL EVUST4(22)  ! CROWNIDX
CALL EVUST4(26)  ! CRBASEHT
CALL EVUST4(27)  ! TORCHIDX
CALL EVUST4(28)  ! CRBULKDN

!
!     SET INITIAL VALUES TO -1 (SEE **EVTSTV** FOR FULL LIST)
!     421 MINSOIL   PERCENTAGE OF MINERAL SOIL EXPOSURE (FM)
!     423 FIREYEAR  CALENDAR YEAR OF LAST FIRE; -1 OTHERWISE (FM)
!     420 FIRE      = 1 IF THERE WAS A FIRE THE PREVIOUS CYCLE;
!                   = 0 OTHERWISE (FM)
!
IF (ICYC .EQ. 1) THEN
  CALL EVSET4(21, -1.0)
  CALL EVSET4(23, FLOAT(BURNYR))
  CALL EVSET4(20, 0.0)
ENDIF

FMKOD=KODTYP

FMSLOP   = SLOPE
SCCF = 0.0
TONREM = 0.0

CYCLEN = FLOAT( ( IY(ICYC+1)) - IY(ICYC) )

!     If we are on the first cycle, then the old crown is not
!     known for the tree records.

DO I=1,ITRN
  FMPROB(I) = PROB(I)
  FMICR(I)  = ICR(I)
ENDDO
DO I=1,MAXTRE
  FIRKIL(I) = 0.0
ENDDO
!
TONRMS=0.0
TONRMH=0.0
TONRMC=0.0
!
IF (ICYC.GT.1) THEN
  DO I = 1, ITRN

!       Also calculate the annual amount of old crown material to fall
!       as a result of crown lifting in the current FVS cycle.  See
!       documentation of FMOLDC for further explanation.
!       Apply a minimum value of 0.001 ounce check on OLDCRW to avoid a
!       floating point error when call is not mortality reconciliation.
!       LRD 09/19/24

    OLDBOT = OLDHT(I) - OLDCRL(I)
    NEWBOT = HT(I) - (HT(I) * FLOAT(ICR(I)) / 100.0)

    IF ((OLDCRL(I) .GT. 0.001).AND.(NEWBOT-OLDBOT.GT.0.0)) THEN
      X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / CYCLEN
      DO J=0,5
        IF (OLDCRW(I,J) .LT. 0.0000625) THEN
          OLDCRW(I,J) = 0.0
        ELSE
          OLDCRW(I,J) = X * OLDCRW(I,J)
        ENDIF
      ENDDO
    ELSE
      DO J=0,5
        OLDCRW(I,J) = 0.0
      ENDDO
    ENDIF

   ENDDO
ENDIF

!     Calculate the weight of crown components, which several FM
!     routines will want. This only needs to be done once each
!     cycle because it depends mostly on ht and dbh.

CALL FMCROW

!     If this is the first year of the simulation, add in the
!     snags that are picked up during the inventory.  Set the year
!     of death equal to the mortality measurement period.  These
!     trees include "OLD" dead and "RECENT" mortlity.

IF (LFMON2) CALL FMSADD (IY(1)-IFIX(FINTM),3)

RETURN
END


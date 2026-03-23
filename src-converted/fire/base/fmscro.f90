SUBROUTINE FMSCRO (I,SP,DEADYR,DSNAGS,ICALL)
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!     SINGLE-STAND VERSION
!     CALLED FROM: FMSADD
!
!  Purpose:
!     This subroutine divides the crowns of newly created snags into
!     pools that will become down debris at specified years in the
!     future.  The weight of crown material that each snag has in each
!     size class at the time of death is taken from CROWNW.  The time by
!     which all of the material of a given size will have fallen from
!     the dead snag is extrapolated from data in (**Al's reference to
!     the old Division of Forest Economics report (1961).  The model
!     assumes that a constant proportion of the material will fall in
!     each future year up to that time.
!----------------------------------------------------------------------
!
!  Local variable definitions:
!     ANNUAL:  amount of material in a size class that will come down
!              each year
!     DEADYR:  YEAR of Death of current snag (note:  during initializ-
!              ation, this may be more than one year before the next
!              year that will be simulated).
!     DSNAGS:  Density of SNAGS to use in calculating total weight of
!              crown components (depends on whether FMSCRO has been
!              called to deal with fire-killed snags or other snags).
!     DKCL:    decay class
!     ILIFE:   lifespan of the current class of crown material, as an
!              integer value
!     ICALL:   Where was this called from? 1=after a fire,2=after a cut
!     FALLYR:  position in CWD2B array that corresponds to the year in
!              which a pool of material should fall.
!     RLIFE:   lifespan of the current class of crown material, as a
!              real value
!     SP:      snag species
!     TSOFT:   time for snag of this species and size to become soft
!     YNEXTY:  the number of years between the year of death and the
!              next year that will be simulated.
!     YRSCYC:  YeaRS left in current FVS CYCle (including current year)
!
!  Common block variables and parameters:
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.
INCLUDE 'FMCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'

!.... Variable declarations.
INTEGER I, SP, DEADYR, SIZE, DKCL, IYR, ILIFE, YNEXTY,FALLYR
REAL    DSNAGS, TSOFT, RLIFE, ANNUAL, NEWBOT, OLDBOT, X
INTEGER ICALL
REAL    YRSCYC
LOGICAL  DEBUG
!
!     CHECK FOR DEBUG.
!
CALL DBCHK (DEBUG,'FMSCRO',6,ICYC)

!.... Begin routine.
IF (DSNAGS .LE. 0.0) RETURN
DKCL = DKRCLS(SP)

YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )
!sng  YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )

!     find out how long it will be between the year of death and the
!     next year simulated, so that only crown material to fall in that
!     year or later is added to CWD2B2 or CWD2B.

!sng  IF (DEADYR .LT. IY(1)) THEN
!sng    YNEXTY = IY(1) - DEADYR

IF (DEADYR .LT. IY(1)) THEN
   YNEXTY = IY(1) - DEADYR
ELSE
   YNEXTY = 1
ENDIF

!     You can skip everything else if all material will fall before
!     the simulation begins.

IF (YNEXTY .GT. TFMAX) GOTO 101

!     Call FMSNGDK to predict years, since death, for snag to become
!     soft.

CALL FMSNGDK(VARACD,SP,DBH(I),TSOFT)
IF (DEBUG) WRITE(JOSTND,7) ICYC, TSOFT, KODFOR, VARACD
7 FORMAT(' FMSCRO CYCLE=',I2,' TSOFT=',F10.1,' KODFOR=',I5, &
          ' VARACD=',A2)

!     If called from CUTS, then OLDCRW will be holding last year's
!     crown info, not the dead part of the crown. Thus, we need to do
!     some additional calculations (which are normally done in FMSDIT).

X = 1.0
IF (ICALL .EQ. 2) THEN
   OLDBOT = OLDHT(I) - OLDCRL(I)
   NEWBOT = HT(I) - (HT(I) * FLOAT(FMICR(I)) / 100.0)
   IF (OLDCRL(I) .GT. 0.0 .AND. (NEWBOT-OLDBOT) .GT. 0.0) THEN
      X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / YRSCYC
   ELSE
      X = 0.0
   ENDIF
ENDIF

!     Divide each class of crown material equally among all the CWD2B2
!     pools between next year and the shorter of TSOFT or TFALL.

DO SIZE=0,5

   RLIFE = TFALL(SP,SIZE)
   IF (RLIFE .GT. TSOFT) RLIFE = TSOFT
   ILIFE = INT(RLIFE)

!        next line asks whether ILIFE had to be truncated (or is zero),
!        and rounds it up to the next highest integer if so.

   IF (REAL(ILIFE) .LT. RLIFE .OR. ILIFE .LE.0) ILIFE =ILIFE+1
   RLIFE = REAL(ILIFE)

!        don't forget to consider the OLDCRW material as well as CROWNW.
!        but only do this if it's not mortality reconciliation time (icall =4)
!        since then oldcrw is no longer holding the dead part of the crown
!        that is to fall each year, but is carrying the crown weight instead.
!        SAR 11/20/12
!        Apply a minimum value of 0.001 ounce check on OLDCRW to avoid a
!        floating point error when call is not mortality reconciliation.
!        LRD 09/19/24


   ANNUAL = CROWNW(I,SIZE)
   IF (ICALL .NE. 4) THEN
     IF (OLDCRW(I,SIZE) .LT. 0.0000625) OLDCRW(I,SIZE) = 0.0
     IF (SIZE .GT. 0) ANNUAL = ANNUAL + YRSCYC*OLDCRW(I,SIZE)*X
   ENDIF
   ANNUAL = ANNUAL * DSNAGS / RLIFE

   IF (DEBUG) WRITE(JOSTND,*) 'annual=',annual,' yrscyc=',yrscyc, &
      ' oldcrw=',OLDCRW(I,SIZE),' x=',x,' i=',i,' size=',size, &
      ' CROWNW=', CROWNW(I,SIZE),' dsnags =',dsnags,' rlife=',rlife

   IF (ANNUAL .GT. 0.0) THEN
     FALLYR = 0
     DO IYR=YNEXTY,ILIFE

       FALLYR = IYR + 1 - YNEXTY

!            Normally, we want to put the stuff into CWD2B2, but if this is
!            called during mortality reconciliation, CWD2B2 has already been
!            copied to CWD2B in preparation for the next cycle, so that is
!            where we need to put the stuff.

       IF (ICALL .NE. 4) THEN
         CWD2B2(DKCL,SIZE,FALLYR) = CWD2B2(DKCL,SIZE,FALLYR) &
                                      + ANNUAL
       ELSE
         CWD2B(DKCL,SIZE,FALLYR) = CWD2B(DKCL,SIZE,FALLYR) &
                                      + ANNUAL
       ENDIF

     ENDDO
  ENDIF
ENDDO

101 CONTINUE

RETURN
END


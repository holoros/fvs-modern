SUBROUTINE SVSNAGE(IYEAR,IDEAD,SNCRDI,SNCRTO,SNHT,SNDI)
IMPLICIT NONE
!----------
! BASE $Id$
!----------
!
!     STAND VISUALIZATION GENERATION
!     J.J.MARCHINEK -- RMRS MOSCOW -- MAY 1999
!     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
!     L.R. DAVID    -- FHTET FORT COLLINS-- JULY 2005
!     S.N.SCHAROSCH -- ABACUS -- MARCH 2008
!
!     USED FOR PROCESSING SNAG AGING INFORMATION
!
!
!     Called from:
!        SVSNAD to age newly-added snags.
!        SVOUT  to age snags prior to writing visualization data.
!
!
!     IYEAR = CURRENT YEAR
!     IDEAD = CURRENT SNAG BEING AGED
!     SNCRDI= CURRENT SNAG CROWN DIAMETER
!     SNCRTO= CURRENT SNAG CROWN RATIO
!     SNHT  = CURRENT SNAG HEIGHT
!     SNDI  = CURRENT SNAG DIAMETER
!
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'SVDEAD.f90'
!
!
INCLUDE 'SVDATA.f90'
!
!
!OMMONS

INTEGER IDEAD, ISTMLT, ITIDIF, ITSTYR, ICURYR, &
           IYEAR, J, KSP, NFALLD
REAL    DFALLN, DKTIME, EXPFALL, FALLNUM, FALRATE, &
           HTSNEW, PFALL, RSMAL, RSOFT, SNCRDI, SNCRTO, SNDI, &
           SNHT, X, X1, XHMOD, XPROB
DOUBLE PRECISION SAVESO
LOGICAL DEBUG, LDOWN
DATA ITSTYR / -999 /
SAVE ITSTYR

!----------
!  Check for debug, initialize variables:
!----------

CALL DBCHK (DEBUG,'SVSNAGE',7,ICYC)

IF (NDEAD.EQ.0) RETURN

CALL SVHABT(XHMOD)

!----------
!  If this is an open slot, zero out snag attributes and return.
!----------

IF (ISTATUS(IDEAD) .EQ. 0) THEN
  SNCRDI = 0.
  SNCRTO = 0.
  SNHT   = 0.
  SNDI   = 0.
  RETURN
ENDIF

!----------
!  This is an active snag record--calculate elapsed yrs since tree death.
!----------

IF (IYEAR .GT. IYRCOD(IDEAD)) THEN
  ITIDIF = IYEAR - IYRCOD(IDEAD)
ELSE
  ITIDIF = 0
ENDIF

!----------
!  Update snag crown diameter, and snag crown ratio
!----------

SNHT = SNGLEN(IDEAD)
SNDI = SNGDIA(IDEAD)
SNCRDI = CRNDIA(IDEAD)*(0.90**(ITIDIF))
IF (ICYC .EQ. 0) THEN
  SNCRTO = CRNRTO(IDEAD)
ELSE
  IF (SNHT .GT. 0.5) THEN
    SNCRTO = (OLEN(IDEAD)/SNHT)*(CRNRTO(IDEAD)*.01-1)+1
    SNCRTO = SNCRTO * 100.
  ELSE
    SNCRTO = 0.
  ENDIF
ENDIF
IF (SNCRTO .LE. 0. .OR. SNCRDI .LE. 0.) THEN
  SNCRDI = 0.
  SNCRTO = 0.
ENDIF

!----------
!  If snag predictions are already up-to-date, then
!    Return
!  Else
!    Initialize snag variables and proceed.
!----------

IF (IYEAR .LE. ILYEAR .OR. ITIDIF .LE. 0) THEN
  RETURN
ENDIF

NFALLD = 0
KSP = ISNSP(IDEAD)
IF (FALLDIR(IDEAD).EQ.-1) THEN
  LDOWN = .FALSE.
ELSE
  LDOWN = .TRUE.
ENDIF

!----------
!  Status codes will be negative for snags that have been flagged for
!  salvage, but are being temporarily held for display in the post
!  thinning/salvage SVS outputs. For such snags, temporarily reverse
!  the sign on the status code--it will be set back at the end of this
!  routine.
!----------
IF ( ISTATUS(IDEAD) .LT. 0 ) THEN
   ISTMLT = -1
ELSE
   ISTMLT = 1
ENDIF
ISTATUS(IDEAD) = ISTATUS(IDEAD) * ISTMLT

IF ( .NOT. LDOWN .OR. SPROBS(IDEAD,3) .GT. 0.0 ) THEN

!----------
!  Predict annual snag fall rate, and apply--stochastically.
!  This loop is executed if:
!     1) the snag is still standing, or,
!     2) the snag has previously fallen, but we need to continue
!        decrementing the SPROBS(IDEAD,3) value for the expected
!        snag expansion factor.
!  Loop by years since last snag update to predict snagfall.
!  If current year is prior to snag death year, skip falldown predictions.
!----------

  DO ICURYR=ILYEAR+1,IYEAR
    IF ( IYRCOD(IDEAD).GE.ICURYR ) CYCLE
    IF ( FALLDIR(IDEAD).NE.-1 .AND. SPROBS(IDEAD,3).EQ.0.0) EXIT

!----------
!  Call FMSFALL to:
!    1) Compute special post-burn fall rates if this is the first year
!       after a fire.
!    2) Calculate the density of snags in this record that would fall
!       under normal conditions. This depends on species, dbh and
!       whether 5% are left.
!
!  If the snag has already fallen, and we are calling FMSFALL just to
!  update the SPROBS(IDEAD,3) value (the expected snag expansion factor
!  if deterministic snagfall were used), then pass in 1 as the number
!  of snags still standing.
!----------

    IF ( FALLDIR(IDEAD).NE.-1 .AND. SPROBS(IDEAD,2).EQ.0.0 ) THEN
      CALL FMSFALL(ICURYR,KSP,SNGDIA(IDEAD),SPROBS(IDEAD,1), &
                      1.0,2,RSOFT,RSMAL,DFALLN)
    ELSE
      CALL FMSFALL(ICURYR,KSP,SNGDIA(IDEAD),SPROBS(IDEAD,1), &
                      SPROBS(IDEAD,2),2,RSOFT,RSMAL,DFALLN)
    ENDIF

    IF (DEBUG) THEN
      WRITE(JOSTND,1020) IDEAD, RSOFT, RSMAL, &
                            DFALLN, SPROBS(IDEAD,1)
1020       FORMAT(' ','IN SVSNAGE, IDEAD=',I4,', RSOFT=',F6.3, &
                    ', RSMAL=',F6.3,', DFALLN=',F6.3, &
                    ', ORIGDEN=',F6.3)
    ENDIF

!----------
!  If applicable, store the post-burn fall rate (PBFALL) that will be
!  in effect for the next PBYEAR years.
!  Different rates apply to small snags and snags that are soft
!  AT TIME OF FIRE (whether initially hard or soft): if both apply,
!  use whichever rate is greater. Fires do not affect the fall rate of
!  large snags that are hard at the time of the fire.
!----------

    IF ((ICURYR - BURNYR) .EQ. 1) THEN
      IF ( ISTATUS(IDEAD) .EQ. 4 ) THEN
        PBFALL(IDEAD) = RSOFT
      ELSE
        PBFALL(IDEAD) = 0.0
      ENDIF

      IF (SNGDIA(IDEAD) .LT. PBSIZE) THEN
        PBFALL(IDEAD) = RSMAL
        IF ( ISTATUS(IDEAD) .EQ. 4 ) THEN
          PBFALL(IDEAD) = MAX(RSOFT,RSMAL)
        ENDIF
      ENDIF
    ENDIF

!----------
!  Set FALLNUM according to whichever density is largest - the density
!  falling under normal conditions, or the density to fall under
!  post-burn conditions if these are applicable.
!----------

    FALLNUM = DFALLN
    IF (((ICURYR-BURNYR) .LE. PBTIME) .AND. &
            (IYRCOD(IDEAD) .LE. BURNYR)) THEN
      IF (FALLNUM .LT. PBFALL(IDEAD)*SPROBS(IDEAD,2)) THEN
        FALLNUM = PBFALL(IDEAD)*SPROBS(IDEAD,2)
      ENDIF
    ENDIF

!----------
!  FALLNUM is, at this point, the number of snags that need to
!  fall in the current year, across all snag records that still
!  remain standing from the original source tree.
!  For predicting fall of each individual snag, scale FALLNUM
!  down to a single stem probability.
!----------

    IF ( SPROBS(IDEAD,2).GT.0.0 ) THEN
      FALRATE = FALLNUM/SPROBS(IDEAD,2)
    ELSE
      FALRATE = 0.0
    ENDIF

!----------
!  Compute expected tree/acre representation of current snag, if
!  fallrate was applied in a deterministic fashion instead of stochastic.
!  SPROBS(IDEAD,3) is not adjusted for remaining snag cohort count
!  since it's meant to tally expected remaing snag tpa--independent
!  of stochastic falling.
!
!  Note: the fall probabilities computed by year within each cycle are not
!  entirely correct. That's because we loop by year, by snag, and the number
!  of snags fallen so far influences the probability of additional snags
!  falling. So snag 1 has already been updated to the end of the cycle
!  before snag 2 has its fall probability predicted for the first year
!  of the current cycle. The end result is that snags at the start of the
!  list have a slightly higher fall probability than those at the end.
!  The impact is probably negligible; looping sequence can't be easily
!  changed since SVSNAGE is called when looping by SVS object in SVSOUT.
!----------

    IF ( SPROBS(IDEAD,3) .GT. 0.0 ) THEN
      EXPFALL = FALLNUM / SPROBS(IDEAD,1)
      IF ( SPROBS(IDEAD,3) .GT. EXPFALL ) THEN
        SPROBS(IDEAD,3) = SPROBS(IDEAD,3) - EXPFALL
      ELSE
        SPROBS(IDEAD,3) = 0.0
      ENDIF
    ENDIF
    PFALL = FALRATE

!----------
!  Now actually fall the snag.
!  Snag records each represent one tree/ac; but FALRATE is fractional.
!  Therefore, we need to stochastically fell snag records using a
!  random number generation/test.
!----------

    IF ( FALRATE .GT. 0.0 .AND. FALLDIR(IDEAD) .EQ. -1 ) THEN
      CALL SVRANN(X)
      IF (X .LT. PFALL) THEN
        X1=X
        CALL SVRANN(X)
        FALLDIR(IDEAD) = IFIX(360. *X +.5)
        IF (DEBUG) WRITE(JOSTND,1070) ICURYR, X1, IDEAD
1070         FORMAT(' ','IN SVSNAGE (1070), ICURYR:',I4, &
                      ', X1=',F5.3,', FELLED SNAG:',I4)
      ENDIF

!----------
!  If snag fell, update "remaining snag" count.
!----------

      IF (FALLDIR(IDEAD) .GE. 0) THEN
        NFALLD=1
        XPROB = 0.0
        DO J=1,NDEAD
          IF ( OIDTRE(J)  .EQ. OIDTRE(IDEAD) .AND. &
                  IYRCOD(J)  .EQ. IYRCOD(IDEAD) .AND. &
                  FALLDIR(J) .EQ. -1 ) XPROB=XPROB+1
        ENDDO
        DO J=1,NDEAD
          IF ( OIDTRE(J) .EQ. OIDTRE(IDEAD) .AND. &
                  IYRCOD(J) .EQ. IYRCOD(IDEAD) ) THEN
            SPROBS(J,2) = XPROB
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDDO
ENDIF

!----------
!  For standing snags, loop by year to predict annual height loss.
!  Update hard/soft status in process, since height loss predictions
!  depend on hard/soft status.
!  Rannget and Rannput are used to not disrupt the random number
!  sequence, since rann can be called in the snag height loss
!  routine (fmR6htls).
!----------

IF (FALLDIR(IDEAD) .EQ. -1) THEN
  CALL RANNGET(SAVESO)
  DO ICURYR=MAX(ILYEAR+1,IYRCOD(IDEAD)+1),IYEAR
    IF ( ISTATUS(IDEAD) .EQ. 4 ) THEN
      CALL FMSNGHT(VARACD,KSP,OLEN(IDEAD),SNGLEN(IDEAD), &
                      0,HTSNEW)
    ELSE
      CALL FMSNGHT(VARACD,KSP,OLEN(IDEAD),SNGLEN(IDEAD), &
                      1,HTSNEW)
    ENDIF
    SNHT = HTSNEW
    SNGLEN(IDEAD) = HTSNEW

!----------
!  Adjust snag status for normal aging (may be subsequently overridden).
!----------

    SELECT CASE (ISTATUS(IDEAD))
      CASE(1,2)                          !Green/red hard snags
        IF (ITIDIF .LT. 2) THEN

!               Tree is moving from green to red snag, or staying red.
          ISTATUS(IDEAD) = 2
        ELSE

!               Tree is moving from green/red snag to hard grey snag.
          ISTATUS(IDEAD) = 3
        ENDIF
      CASE (90,91,92)
        IF (ITIDIF .LT. 2) THEN

!               WWPB kill, one year or less old--remains red snag.
          CONTINUE
        ELSEIF (ITIDIF .LT. 3) THEN

!               Advance to dark beetle kill.
          ISTATUS(IDEAD) = 91

        ELSEIF (ITIDIF .LT. 5) THEN

!               Advance to grey WWPB kill.
          ISTATUS(IDEAD) = 92

        ELSEIF (ITIDIF .LT. 6) THEN

!               Advance to grey hard snag.
          ISTATUS(IDEAD) = 3
        ELSE

!               Advance to grey soft snag.
          ISTATUS(IDEAD) = 4
        ENDIF
      CASE (5)
        IF (ITIDIF .GE. 2) THEN

!               This snag is progressing from a recently burned snag
!               to an older burned snag with no crown.
          ISTATUS(IDEAD) = 6
        ENDIF
    END SELECT

!----------
!  Call FMSNGDK to predict years, since death, for snag to become soft.
!  If time since tree death exceeds hard-to-soft decay time, change
!  status to soft snag.
!  (Includes beetle-caused and fire-caused snags)
!----------

    IF ( ISTATUS(IDEAD) .NE. 4 ) THEN
      CALL FMSNGDK(VARACD,KSP,SNGDIA(IDEAD),DKTIME)
      IF ((ICURYR - IYRCOD(IDEAD)) .GE. DKTIME) THEN
        ISTATUS(IDEAD) = 4
      ENDIF
    ENDIF
  ENDDO
  CALL RANNPUT(SAVESO)

!----------
!  No diameter change for standing snags.
!----------

  SNGDIA(IDEAD) = ODIA(IDEAD)
  SNDI = ODIA(IDEAD)
ENDIF

IF (DEBUG) THEN
  IF ( ITSTYR .NE. IYEAR ) THEN
    WRITE (JOSTND,1040) ICYC, IYEAR, &
                           NDEAD, NSVOBJ
1040   FORMAT (' ','IN SVSNAGE, ICYC=',I2,', IYEAR=',I4,':', / , &
             ' ',T5,'NDEAD=',I4,', NSVOBJ=',I5,//, &
             ' ',T5,'SNAG LIST AND FALL PROBABILITIES:',//, &
             ' ',T5,'IDEAD SPP ODIA OLEN IYRCOD ST ODEN CDEN -EXP- ', &
                    'PBFALL FRATE FALLDIR',/, &
             ' ',T5,'----- --- ---- ---- ------ -- ---- ---- ----- ', &
                    '----- ------ -------')
!                       XXXXX XXX XX.X XXX. XXXXXX XX XXX. XXX. X.XXX
!                       X.XXX X.XXXX XXXXX.X
    ITSTYR = IYEAR
  ENDIF
  WRITE(JOSTND,1050) IDEAD, ISNSP(IDEAD), ODIA(IDEAD), &
                        OLEN(IDEAD), IYRCOD(IDEAD), &
                        ISTATUS(IDEAD), SPROBS(IDEAD,1), &
                        SPROBS(IDEAD,2), SPROBS(IDEAD,3), &
                        PBFALL(IDEAD), FALRATE, &
                        FALLDIR(IDEAD)
1050   FORMAT(' ',T5,I5,1X,I3,1X,F4.1, &
                1X,F4.0,1X,I6, &
                1X,I2,1X,F4.0, &
                1X,F4.0,1X,F5.3, &
                1X,F5.3,1X,F6.4, &
                1X,F7.1)
ENDIF

!----------
!  Snag is down.
!  If age of snag is greater than zero, progress status, diameter and height;
!  otherwise, just adjust diameter and height.
!----------

IF (FALLDIR(IDEAD) .GE. 0) THEN

!       Force all downed snags to be grey, unless they are salvage snags.

  IF ( ISTMLT .EQ. 1 ) THEN
     IF ( ISTATUS(IDEAD) .LE.  2 ) ISTATUS(IDEAD)=3
     IF ( ISTATUS(IDEAD) .EQ. 90 ) ISTATUS(IDEAD)=91
  ENDIF

  IF (ISTATUS(IDEAD) .EQ. 1 .OR. ISTATUS(IDEAD) .EQ. 2) THEN
    IF (ITIDIF .LT. 2) THEN
      ISTATUS(IDEAD) = 2
    ELSE
      ISTATUS(IDEAD) = 3
    ENDIF
  ENDIF

!       FOR FIRE-CAUSED SNAGS ONLY.  PROGRESS FROM ISTATUS OF 5
!       TO ISTATUS OF 6.

  IF (ISTATUS(IDEAD).EQ.5 .AND. ITIDIF.GE.2) THEN

!         THIS SNAG IS PRORESSING FROM A RECENTLY BURNED SNAG
!         TO AN OLDER BURNED SNAG WITH NO CROWN.

    ISTATUS(IDEAD) = 6
  ELSEIF (ISTATUS(IDEAD) .GE. 90 .AND. &
             ISTATUS(IDEAD) .LE. 92) THEN
    IF (ITIDIF .LT. 2) THEN
!           THIS IS A NEW WWPB KILL ONE OR LESS YEARS OLD.
!           NO CHANGE IN STATUS, REMAINS RED.
      CONTINUE
    ELSEIF (ITIDIF .LT. 3) THEN
!           ADVANCE TO DARK WWPB KILL.
      ISTATUS(IDEAD) = 91
    ELSEIF (ITIDIF .LT. 5) THEN
!           ADVANCE TO GREY WWPB KILL.
      ISTATUS(IDEAD) = 92
    ELSEIF (ITIDIF .LT. 6) THEN
!           ADVANCE TO GREY HARD SNAG.
      ISTATUS(IDEAD) = 3
    ELSE
!           ADVANCE TO GREY SOFT SNAG.
      ISTATUS(IDEAD) = 4
    ENDIF
  ENDIF

!----------
!  For downed snags:
!    1) Predict diameter decrease using fixed rate.
!       Don't allow negative diameter--snags with diam lt 1 get
!       removed from snag arrays in SVOUT
!    2) Predict current length, using ratio of original dia to current.
!       Don't allow length to increase from what it was before snag fell.
!----------

  IF (ISTATUS(IDEAD) .EQ. 4) THEN
    SNDI = ODIA(IDEAD) - .11*ITIDIF
    IF ( SNDI .LT. 0.0 ) SNDI = 0.0
    SNGDIA(IDEAD) = SNDI
    SNHT = OLEN(IDEAD) * SNDI/ODIA(IDEAD)
    IF ( SNHT .GT. SNGLEN(IDEAD) ) SNHT = SNGLEN(IDEAD)
    SNGLEN(IDEAD) = SNHT
  ELSE
    SNDI = ODIA(IDEAD) - .1*ITIDIF
    IF ( SNDI .LT. 0.0 ) SNDI = 0.0
    SNGDIA(IDEAD) = SNDI
    SNHT = OLEN(IDEAD) * SNDI/ODIA(IDEAD)
    IF ( SNHT .GT. SNGLEN(IDEAD) ) SNHT = SNGLEN(IDEAD)
    SNGLEN(IDEAD) = SNHT
  ENDIF
ENDIF

!----------
!  Return the snag status code to a negative value for salvaged snags.
!----------

ISTATUS(IDEAD) = ISTATUS(IDEAD) * ISTMLT

RETURN
END

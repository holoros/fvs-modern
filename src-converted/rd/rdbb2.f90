SUBROUTINE RDBB2
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Determines how many races of bark beetle type 2 are eligible to
!     act in this growth cycle under current stand conditions.  The
!     mortality rates etc. specified for each beetle race that is
!     active are reported back to RDOAGM. By default, each race that
!     was scheduled for this time period is rescheduled for the next
!     time period (whether it was active this time or not, unless the
!     user specifies otherwise).
!
!     Keyword:  BBTYPE2
!     Inputs:  1 = first outbreak year    2 = host tree species
!              3 = DBH limit              4 = threshold density of
!                                             lg. enough windfallen host
!              5 = "KILL Factor"          6 = 0 for reschedule,
!                                             1 for don't
!
!  Called By :
!     RDOAGM  [ROOT DISEASE]
!
!  Calls :
!     OPFIND  (SUBROUTINE)   [FVS]
!     OPGET   (SUBROUTINE)   [FVS]
!     OPDONE  (SUBROUTINE)   [FVS]
!     OPREDT  (SUBROUTINE)   [FVS]
!     OPCOPY  (SUBROUTINE)   [FVS]
!     OPINCR  (SUBROUTINE)   [FVS]
!
! REVISION HISTORY:
!    06-AUG-01 Lance R. David (FHTET)
!       Change MYACT to IACTK in call to OPCOPY because of array to scalar
!       mismatch reported by LF95.
!       (previous date of last revision was 12/7/93)
!    10-DEC-07 Lance R. David (FHTET)
!       Update to argument list in call to OPCOPY, added variable
!       DONE for debug/tracking (from Don Robinson).
!   08/26/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!....................................................................

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations.

INTEGER   I, J, I1, I2, ISPI, ONLY1X, NOCOPY, RACE, DONE, IACTK
INTEGER   KDT, KODE, MYACT(1), NCOPYS, NPS, NTODO
REAL      KILLF, CLSUML, CLSUMI, SPSUMI, SPSUML, CLCNT, CLCNTA

REAL      DBHLIM, THRESH, MORT, TSTEM, TDENS, CLKILL
REAL      PRMS(5)

IF (ITRN .LE. 0) RETURN
NOCOPY = 0
DONE = 0

!.... Ask OPFIND how many races of this beetle type are scheduled for
!.... this time period.

DATA MYACT /2416/
CALL OPFIND (1,MYACT,NTODO)
IF (NTODO .LE. 0) RETURN

!.... For each race, first get the parameter values, and determine the
!.... first (I1) and last (I2) records of the host species.  Check that
!.... there is at least one record of the host species, and that some
!.... were windthrown.  NOTE that in this case PRMS(4) is NOT the
!.... mortality rate applied to eligible trees in a beetle outbreak.
!.... Rather, it is the number of standing trees that will die from
!.... bark beetles FOR EACH TREE KILLED BY WINDTHROW.  The default
!.... is that, for each tree killed by windthrow, 0.875 trees will
!.... die from beetles (from McNamee et al.1991).

DO 999 RACE = 1, NTODO
   CALL OPGET (RACE,5,KDT,IACTK,NPS,PRMS)

   ISPI = INT(PRMS(1))
   DBHLIM = PRMS(2)
   THRESH = PRMS(3)
   KILLF = PRMS(4)
   ONLY1X = INT(PRMS(5))

   I1 = ISCT(ISPI,1)
   I2 = ISCT(ISPI,2)

   IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
   IF (WINDSP(ISPI,3) .LE. 0.0) GOTO 888

!....    Second, go through the host tree records and determine the
!....    number of windfallen stems larger than the user's size
!....    criteria.  This is done by some conversion of the proportion
!....    of the total crown area that is due to trees of this size.
!....    The 'L' subscript means "Clean", not "Live".

!....    Zero out class & species accumulators.
!....    'class' is defined as greater than specified DBH.

   CLSUML = 0
   CLSUMI = 0
   SPSUML = 0
   SPSUMI = 0

!     GET PROP'NS OF FALLEN STEMS IN SIZE CLASS
!     FROM SUM OF WEIGHT COEFFS FOR ALL FALLEN IN
!     PROP TO THOSE WHICH QUALIFY
!     NB. WEIGHT COEFFICIENTS ARE THOSE USED TO CALCULATE
!     SUSCEPTIBLITY TO WINDFALL IN SUBROUTINE RROWIN

   DO 10 J = 1,ILEND
      I = ISTEML(J)
      IF (ISP(I) .NE. ISPI) GOTO 10
      SPSUML = SPSUML + WINDWL(I)
      IF (DBH(I) .LT. DBHLIM) GOTO 10
      CLSUML = CLSUML + WINDWL(I)
10    CONTINUE

!....    Infected stems.

   DO 20 J = 1,IIEND
      I = ISTEMI(J)
      IF (ISP(I) .NE. ISPI) GOTO 20
      SPSUMI = SPSUMI + WINDWI(I)
      IF (DBH(I) .LT. DBHLIM) GOTO 20
      CLSUMI = CLSUMI + WINDWI(I)
20    CONTINUE

!....    Number of fallen stems in size class.

   IF (SPSUML .LT. 1E-3) SPSUML = 1E-3
   IF (SPSUMI .LT. 1E-3) SPSUMI = 1E-3
   CLCNT = (WINDSP(ISPI,1) * CLSUML / SPSUML) + &
              (WINDSP(ISPI,2) * CLSUMI / SPSUMI)
   CLCNTA = CLCNT / SAREA


! Compare CLCNTA, the density of windfallen stems that met the user's size criteria, to
! the user's threshold for an active beetle outbreak.

   IF (CLCNTA .LT. THRESH) GOTO 888

! If the beetle is active, call OPDONE to signal the outbreak, increment NUMBB and
! store the specified parameter values for later use in killing trees.  Calculate the
! mortality rate needed from KILLF and the number of windfallen trees.

   CALL OPDONE (RACE,IY(ICYC))
   DONE = 1

   NUMBB = NUMBB + 1
   IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
   HOST(NUMBB) = ISPI
   MINDBH(NUMBB) = DBHLIM
   MININF(NUMBB) = 0.0

!....    target number of trees to kill

   CLKILL = CLCNT * KILLF

!....    number of live trees (note:  this is total before windthrow occurred)

   TDENS = 0.0

   DO 30 J = I1,I2
      I = IND1(J)
      IF (DBH(I) .LT. DBHLIM) GOTO 30
      TDENS = TDENS + PROBL(I)
30    CONTINUE

   TSTEM = TDENS * SAREA

!....    mortality rate = number to kill / number living

   IF (CLKILL .LT. TSTEM) THEN
      MORT = CLKILL / TSTEM
   ELSE
      MORT = 1.0
   ENDIF

   ORATE(NUMBB) = MORT
   OFRATE(NUMBB) = MORT
   IURATE(NUMBB) = MORT
   IIRATE(NUMBB) = MORT

! Check whether the Type 2 beetle races are to remain potentially active after the
! current growth cycle.

888    CONTINUE

   IF (ONLY1X .EQ. 1) NOCOPY = 1

999 CONTINUE

! If the Type 2 beetle races are to remain active, then copy them to the next growth
! cycle.

IF (NOCOPY .EQ. 1) GOTO 1000
   CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
   CALL OPINCR (IY,ICYC,NCYC)

1000 CONTINUE

RETURN
END

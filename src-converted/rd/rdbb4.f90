SUBROUTINE RDBB4
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Determines how many races of bark beetle type 4 are eligible to
!     act in this growth cycle under current stand conditions.  The
!     mortality rates etc. specified for each beetle race that is
!     active are reported back to RDOAGM.  By default, each race that
!     was scheduled for this time period is rescheduled for the next
!     time period (whether it was active this time or not, unless the
!     user specifies otherwise).
!
!     Keyword :  BBTYPE4
!     Inputs : 1 = first outbreak year
!              2 = host tree species
!              3 = DBH limit
!              4 = threshold density of lg.enough sick-enough host
!                  (live or newly dead)
!              5 = infected mortality rate
!              6 = proportion-of-roots-infected limit
!              7 = inside uninf. mort. rate
!              8 = fringe mortality rate
!              9 = outside center+fringe mortality rate
!             10 = 0 for reschedule, 1 for don't
!
!  Called By :
!     RDOAGM  [ROOT DISEASE]
!
!  Calls :
!     OPFIND  (SUBROUTINE)   [FVS]
!     OPGET   (SUBROUTINE)   [FVS]
!     OPDONE  (SUBROUTINE)   [FVS]
!     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
!     OPCOPY  (SUBROUTINE)   [FVS]
!     OPINCR  (SUBROUTINE)   [FVS]
!     OPREDT  (SUBROUTINE)   [FVS]
!
!  Revision History :
!     06/12/96 - Matthew K. Thompson
!                Moved the declaration of DSII to the parameter
!                include file RDPARM.
!
!     03/04/98 - Robert N. Havis
!                Increased size of arrays PREVA and PREVB from 2 to 4
!                to accomodate 4 types of root diseases. Increase
!                corresponding number of loops in statement 500 and 501
!    06-AUG-01 Lance R. David (FHTET)
!       Change MYACT to IACTK in call to OPCOPY because of array to scalar
!       mismatch reported by LF95.
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

INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'RDADD.f90'
INCLUDE 'PLOT.f90'

!.... Local variable declarations.

INTEGER  I, J, IK, I1, I2, ISPI, ONLY1X, NOCOPY, DONE, &
            RRTYPE, RACE, IACTK, MYACT(1)

INTEGER  IP, KDT, KODE, NCOPYS, NPS, NTODO

REAL     DBHLIM, THRESH, MORTII, MORTIU, MORTF, MORTO, STEMS

REAL     PRMS(9), PREVA(4), PREVB(4), RROTEX, XXXX

NOCOPY = 0
DONE = 0
IF (ITRN .LE. 0) RETURN

!.... Ask OPFIND how many races of this beetle type are scheduled for
!.... this time period.

DATA MYACT /2432/
CALL OPFIND (1,MYACT,NTODO)
IF (NTODO .LE. 0) RETURN

!.... For each race, first get the parameter values, determine which
!.... root rot type affects the beetle host species, and determine the
!.... first (I1) and last (I2) tree records of the host species.  Check
!.... that vulnerable host exists before proceeding.

DO 999 RACE = 1, NTODO
   CALL OPGET (RACE,9,KDT,IACTK,NPS,PRMS)

   ISPI = INT(PRMS(1))
   DBHLIM = PRMS(2)
   THRESH = PRMS(3)
   MORTII = PRMS(4)
   RROTEX = PRMS(5)
   MORTIU = PRMS(6)
   MORTF  = PRMS(7)
   MORTO  = PRMS(8)
   ONLY1X = INT(PRMS(9))

   RRTYPE = MAXRR
   IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
   I1 = ISCT(ISPI,1)
   I2 = ISCT(ISPI,2)

   IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
   IF (PAREA(RRTYPE) .LE. 0.0) GOTO 888
   IF (PCOLO(IRTSPC(ISPI),RRTYPE) .LT. RROTEX) GOTO 888

!....    Second, go through the host tree records and count the number
!....    of living stems PLUS stems that died in the last two years
!....    that meet the user's criteria for size and % of roots infected
!....    with annosus.  Determine whether the density of these stems
!....    exceeds the user's threshold for an active beetle outbreak.

   STEMS = 0.0

   DO 6 J = I1,I2
      I = IND1(J)
      IF (DBH(I) .LT. DBHLIM) GOTO 6

      DO 5 IK = 1,ISTEP
         DO 4 IP = 1,2
            IF (PROPI(I,IK,IP) .LT. RROTEX) GOTO 4
            STEMS = STEMS + ((2/FINT) * PROBI(I,IK,IP))
4          CONTINUE
5       CONTINUE

      STEMS = STEMS + ((2/FINT) * (PROAKL(DSII,I) + PRANKL(I)))
6    CONTINUE

   IF ((STEMS / PAREA(RRTYPE)) .LT. THRESH) GOTO 888

!....    If the beetle is active, call OPDONE to signal the outbreak,
!....    increment NUMBB and store the specified mortality rates for
!....    later use in killing trees.

   CALL OPDONE (RACE,IY(ICYC))
   DONE = 1

   NUMBB = NUMBB + 1
   IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
   HOST(NUMBB) = ISPI
   MINDBH(NUMBB) = DBHLIM
   MININF(NUMBB) = RROTEX
   ORATE(NUMBB) = MORTO
   OFRATE(NUMBB) = MORTF
   IURATE(NUMBB) = MORTIU
   IIRATE(NUMBB) = MORTII

!....    Now, if it's not already been done for a previous beetle race,
!....    calculate the "fringe" area that is within 1 root diameter of
!....    an infection center in the stand - RDBBDO will need this to
!....    know how many trees to kill at the higher rate that applies in
!....    this area.

   IF (FRINGE(RRTYPE) .GT. 0.0) GOTO 888

!....    This procedure is pasted from RRJUMP, to compute the areas in
!....    SAREA and PAREA() at the next timestep.  Then a bunch of
!....    values have to be set back, so that the "true" call to RRJUMP
!....    can do them. The future area of the uninfected stand is called
!....    NSAREA; the area of the new torus around existing patches is
!....    called FRINGE().

  IF (NCENTS(RRTYPE) .EQ. 0) GOTO 888

!....   Save PAREA() and OOAREA() so that they can be restored at the
!....   end.
!
!     Set number of loops to 4 rather than 2 to correspond to total
!     number of RR diseases available (4). RNH(FEB98)
!

  DO 500 I=1,4
     PREVA(I) = PAREA(I)
     PREVB(I) = OOAREA(I)
500   CONTINUE

  XXXX = 2.0 * RRJINC(RRTYPE)

  DO 700 I=1,NCENTS(RRTYPE)
     IF (PCENTS(RRTYPE,I,3) .LE. 0) GOTO 700
     PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) + XXXX
700   CONTINUE

  IRRSP = RRTYPE
  CALL RDAREA

  FRINGE(RRTYPE) = PAREA(RRTYPE) - PREVA(RRTYPE)
  FRINGE(RRTYPE) = AMAX1(0.0,FRINGE(RRTYPE))

!....   Put PAREA() and OOAREA() back to their earlier state.
!
!     Set number of loops to 4 rather than 2 to correspond to total
!     number of RR diseases available (4). RNH(FEB98)
!
  DO 501 I=1,4
     PAREA(I) = PREVA(I)
     OOAREA(I) = PREVB(I)
501   CONTINUE

!....   Put PCENTS back to its earlier state.

  DO 701 I=1,NCENTS(RRTYPE)
     IF (PCENTS(RRTYPE,I,3) .LE. 0) GOTO 701
     PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) - XXXX
701   CONTINUE

!....   Check whether the Type 4 beetle races are to remain potentially
!....   active after the current growth cycle.

888   CONTINUE

  IF (ONLY1X .EQ. 1) NOCOPY = 1

999 CONTINUE

!.... If the Type 4 beetle races are to remain active, then copy them
!.... to the next growth cycle.

IF (NOCOPY .EQ. 1) GOTO 1000
CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
CALL OPINCR (IY,ICYC,NCYC)

1000 CONTINUE

RETURN
END

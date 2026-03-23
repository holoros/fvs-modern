SUBROUTINE RDBB3
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     Determines how many races of bark beetle type 3 are eligible to
!     act in this growth cycle under the current stand conditions.
!     The mortality rates etc. specified for each beetle race that is
!     active are reported back to RDOAGM.  By default, each race that
!     was scheduled for this time period is rescheduled for the next
!     time period (whether it was active this time or not, unless the
!     user specifies otherwise).
!
!     Keyword:  BBTYPE3
!     Inputs:  1 = first outbreak year    2 = host tree species
!              3 = DBH limit              4 = threshold density of lg.
!                                             enough sick-enough host
!              5 = mortality rate         6 = proportion of roots
!                                             infected limit
!              7 = 0 for reschedule, 1 for don't
!
!  Called By :
!     RDOAGM  [ROOT DISEASE]
!
!  Calls :
!     OPFIND  (SUBROUTINE)   [FVS]
!     OPGET   (SUBROUTINE)   [FVS]
!     OPDONE  (SUBROUTINE)   [FVS]
!     OPCOPY  (SUBROUTINE)   [FVS]
!     OPINCR  (SUBROUTINE)   [FVS]
!     OPREDT  (SUBROUTINE)   [FVS]
!
! REVISION HISTORY:
!    06-AUG-01 Lance R. David (FHTET)
!       Change MYACT to IACTK in call to OPCOPY because of array to scalar
!       mismatch reported by LF95.
!       (previous date of last revision was March 1, 1995)
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

!.... Local variable declarations.

INTEGER  I, J, IK , I1, I2, ISPI, ONLY1X, NOCOPY, RRTYPE, RACE, &
            DONE, IACTK
INTEGER  IP, KDT, KODE, MYACT(1), NCOPYS, NPS, NTODO

REAL     DBHLIM, THRESH, MORT, RROTEX, STEMS

REAL     PRMS(6)

IF (ITRN .LE. 0) RETURN
NOCOPY = 0
DONE = 0

!.... Ask OPFIND how many races of this beetle type are scheduled for
!.... this time period.

DATA MYACT /2417/
CALL OPFIND (1,MYACT,NTODO)
IF (NTODO .LE. 0) RETURN

!.... For each race, first get the parameter values, determine which
!.... root rot type affects the beetle's host species, and determine
!.... the first (I1) and last (I2) records of the host species.  Before
!.... proceeding, check that vulnerable host exists.

DO 999 RACE = 1, NTODO
   CALL OPGET (RACE,6,KDT,IACTK,NPS,PRMS)

   ISPI = INT(PRMS(1))
   DBHLIM = PRMS(2)
   THRESH = PRMS(3)
   MORT = PRMS(4)
   RROTEX = PRMS(5)
   ONLY1X = INT(PRMS(6))

   RRTYPE = MAXRR
   IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))
   I1 = ISCT(ISPI,1)
   I2 = ISCT(ISPI,2)

   IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
   IF (PAREA(RRTYPE) .LE. 0.0) GOTO 888

!....    Second, go through the host tree records and count the number
!....    of live stems that meet the user's criteria for size and % of
!....    roots infected with annosus.  Determine whether the density of
!....    these stems exceeds the user's threshold for an active beetle
!....    outbreak.

   STEMS = 0.0
   DO 6 J = I1,I2
      I = IND1(J)
      IF (DBH(I) .LT. DBHLIM) GOTO 6

      DO 5 IK = 1,ISTEP
         DO 4 IP = 1,2
            IF (PROPI(I,IK,IP) .LT. RROTEX) GOTO 4
            STEMS = STEMS + PROBI(I,IK,IP)
4          CONTINUE
5       CONTINUE
6    CONTINUE

   IF ((STEMS / PAREA(RRTYPE)) .LT. THRESH) GOTO 888

!....    If the beetle is active, call OPDONE to signal the outbreak,
!....    increment NUMBB and store the specified mortality rates for
!....    later use in killing trees.  Reschedule the beetle race by
!....    copying it to the next growth cycle, unless restricted by
!....    the user.

   CALL OPDONE (RACE,IY(ICYC))
   DONE = 1

   NUMBB = NUMBB + 1
   IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
   HOST(NUMBB) = ISPI
   MINDBH(NUMBB) = DBHLIM
   MININF(NUMBB) = RROTEX
   ORATE(NUMBB) = 0.0
   OFRATE(NUMBB) = 0.0
   IURATE(NUMBB) = 0.0
   IIRATE(NUMBB) = MORT

!....    Check whether the Type 3 beetle races are to remain
!....    potentially active after the current growth cycle.

888    CONTINUE

   IF (ONLY1X .EQ. 1) NOCOPY = 1

999 CONTINUE

!.... If the Type 3 beetle races are to remain active, then copy them
!.... to the next growth cycle.

IF (NOCOPY .EQ. 1) GOTO 1000
CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
CALL OPINCR (IY,ICYC,NCYC)

1000 CONTINUE

RETURN
END

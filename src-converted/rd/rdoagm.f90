SUBROUTINE RDOAGM
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This subroutine calls "OTHER AGENT" sub-models.
!
!  Called By :
!     RDTREG  [ROOT DISEASE]
!
!  Calls :
!     RDOWI   (SUBROUTINE)   [ROOT DISEASE]
!     RDOWIN  (SUBROUTINE)   [ROOT DISEASE]
!     RDBB1   (SUBROUTINE)   [ROOT DISEASE]
!     RDBB2   (SUBROUTINE)   [ROOT DISEASE]
!     RDBB3   (SUBROUTINE)   [ROOT DISEASE]
!     RDBB4   (SUBROUTINE)   [ROOT DISEASE]
!
!  Common Block Variables Used :
!     NUMBB  - Number of Bark Beetles eligible to act in this time
!              period, which cannot exceed 3*MAXSP
!     FRINGE(ITOTRR) - The area in the stand that is within an average
!              root radius of a root rot infection center, for each
!              root rot type.  This area may experience greater
!              mortality due to bark beetles.
!     HOST(3*MAXSP) - Host tree species on which each beetle acts
!     MINDBH(3*MAXSP) - MINimum host DBH at which trees become
!              susceptible to each BB.
!     MININF(3*MAXSP) - Minimum proportion of roots infected at which
!              IIRATE applies.
!     ORATE(3*MAXSP) - Mortality rate outside infection centers (and
!              any associated 'fringe') caused by each BB.
!     OFRATE(3*MAXSP) - Mortality rate outside infection centers in the
!              'Fringe' area that may experience greater mortality.
!     IURATE(3*MAXSP) - Mortality rate inside infection centers of
!              uninfected trees, for each BB.
!     IIRATE(3*MAXSP) Mortality rate inside infection centers of
!              infected trees, for each BB.
!
!  Revision History :
!     08/07/97 - Matthew K. Thompson
!                Removed section of code that modified PROBI due to
!                windthrow.  Decided that windthrow mortality needed
!                to be removed from PROBI in the subroutine RDOWIN.
!   08/29/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDADD.f90'
INCLUDE 'RDARRY.f90'

INTEGER  I, J
REAL     DIFF

IF (ICYC .EQ. 1) CALL RDOWI

!.... Update FFPROB:  If this is the first time cycle, set FFPROB(I,1)
!.... to FPROB(I).  Otherwise, set it to the lower of FFPROB(I,2) or
!.... FPROB(I).  Set FFPROB(I,2) to the current FPROB(I). For numerical
!.... reasons, calculate DIFF, which will be negative if FPROB < FFPROB

DO 10 I=1,ITRN

   DIFF = FPROB(I) - FFPROB(I,2)

   IF ((DIFF .LE. 1E-4) .OR. (ICYC .EQ. 1)) THEN
      FFPROB(I,1) = FPROB(I)
   ELSE
      FFPROB(I,1) = FFPROB(I,2)
   ENDIF

   FFPROB(I,2) = FPROB(I)
10 CONTINUE

!.... Copy dead standing array OAKL() from previous timestep to
!.... PROAKL(), then zero out OAKL(), BBKILL() AND RROBNK() for use in
!.... current iteration.

DO 200 J=1,ITRN
   PRANKL(J) = RDKILL(J)

   DO 100 I=1,3
      PROAKL(I,J) = OAKL(I,J)
      OAKL(I,J) = 0.0
100    CONTINUE

   DO 133 I=1,4
      BBKILL(I,J) = 0.0
133    CONTINUE
200 CONTINUE

DO 233 I = 1,MAXSP
   RROBNK(I) = 0.0
233 CONTINUE

!.... Cycle processing.

DO 500 I=1,MAXSP
   DO 400 J=1,4
      WINDSP(I,J) = 0
400    CONTINUE
500 CONTINUE

!.... Call windthrow.

CALL RDOWIN

!.... Calls to bark beetle models.  First, determine which bark beetles
!.... are eligible to act.  Then apply the appropriate mortality to
!.... each tree record.  Note that if more than 3*MAXSP beetles are
!.... eligible to act, only the first (3*MAXSP - 1) plus the last one
!.... will actually be done.

NUMBB = 0

DO 750 IRRSP=MINRR,MAXRR
   FRINGE(IRRSP) = 0.0
750 CONTINUE

CALL RDBB1
CALL RDBB2
CALL RDBB3
CALL RDBB4

IF (NUMBB .GT. 0) CALL RDBBDO
IF (IBBOUT .GT. 0) CALL RDBOUT

DO 700 I = 1,ITRN

!....    Check to see that FPROB is not negative.

   IF (FFPROB(I,2) .LT. 0.0) FFPROB(I,2) = 0.0
700 CONTINUE

CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

RETURN
END

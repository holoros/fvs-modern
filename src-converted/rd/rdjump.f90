SUBROUTINE RDJUMP
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!    This subroutine expands the area in root disease when the
!    stand is clearcut.
!
!  Called By :
!    RDCNTL  [ROOT DISEASE]
!
!  Calls :
!    RDAREA  (SUBROUTINE)   [ROOT DISEASE]
!    RDINF   (SUBROUTINE)   [ROOT DISEASE]
!
!  Local Variables :
!    <incomplete>
!
!  Common Block Variables Used :
!    <incomplete>
!
!  Revision History
!    06/10/96 - Matthew K. Thompson
!               Changed the summing of trees in tree records.
!               Changed from an INTEGER summation to a REAL summation.
!   08/29/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... Parameter include files

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files

INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations

INTEGER  I, I1, I2, IDI, J, KSP
REAL     SPCEN, TOTCEN, TRENUM, XTRENU, XJPINC


IF (PAREA(IRRSP) .EQ. 0.0) RETURN

!.... Jump out if a cut occurred

IF (INFLAG .NE. 1) GOTO 650

SPPROP(IRRSP) = 0.0
IF (NCENTS(IRRSP) .EQ. 0) GOTO 650
IF (TNJUMP(IRRSP) .LE. 0.0) GOTO 650

SPCEN = 0.0
TOTCEN = 0.0
DO 700 I=1, NCENTS(IRRSP)
   IF (PCENTS(IRRSP,I,3) .LE. 0) GOTO 700

!....    Expand centers by the diameter of the uninfected root systems
!....    present at the time of the cut (given by 2*RRJINC)

   PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) + &
                          2 * RRJINC(IRRSP) * TNJUMP(IRRSP)
   IF (ICENSP(IRRSP,I) .NE. 0) SPCEN = SPCEN + &
                                  PCENTS(IRRSP,I,3)**2
   TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2
700 CONTINUE
SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)


CALL RDAREA

AREANU(IRRSP) = PAREA(IRRSP) - OOAREA(IRRSP)
IF (PAREA(IRRSP) .LE. 0.0) AREANU(IRRSP) = 0.0
IF (AREANU(IRRSP) .LT. 0.0) AREANU(IRRSP) = 0.0
OOAREA(IRRSP) = PAREA(IRRSP)

!.... Infect new trees due to the expansion of the centers

CALL RDINF

!.... Loop over tree list if there are any infection centers.
!.... Calculate the mean jump increment to use next year.
!
!     TRENUM - Number of trees outside patch area represented by the
!              record (so will weight roots by number of trees).
!     XTRENU - Sum of number of trees outside patch area
!     XJPINC - Sum of uninfected root radii in appropriate records
!     RRJINC - Mean uninfected root radii (used to increment the
!              radii of centers next year)

650 CONTINUE

RRJINC(IRRSP) = 0
XJPINC = 0.0
XTRENU = 0.0
TRENUM = 0.0

IF (ITRN .EQ. 0) RETURN

IDI = IRRSP
DO 500 KSP=1, MAXSP
   IF (ISCT(KSP,1) .EQ. 0) GOTO 500
   IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
   IF (IDI .NE. IRRSP) GOTO 500

   I1 = ISCT(KSP,1)
   I2 = ISCT(KSP,2)

   DO 400 J=I1, I2
      I = IND1(J)
      TRENUM = FPROB(I) * (SAREA - PAREA(IDI))
      XJPINC = XJPINC + ROOTL(I) * TRENUM
      XTRENU = XTRENU + TRENUM
400    CONTINUE
500 CONTINUE

RRJINC(IRRSP) = XJPINC / (XTRENU + 1E-6)

RETURN
END

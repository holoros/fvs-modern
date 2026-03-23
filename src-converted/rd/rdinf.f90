SUBROUTINE RDINF
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This subroutine takes a newly infected area and converts it
!     into the equivalent number of infected trees of each tree
!     class in the tree list.
!
!  Called By :
!     RDCNTL  [ROOT DISEASE]
!     RDJUMP  [ROOT DISEASE]
!
!  Calls :
!     DBCHK   (SUBROUTINE)   [PROGNOSIS]
!     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
!
!  Revision History :
!     06/12/96 - Matthew K. Thompson
!                Moved the declaration of DSO, DSII, and DSIU to the
!                parameter include file RDPARM.
!   08/28/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declarations.

LOGICAL DEBUG

INTEGER  I, I1, I2, IDI, J, KSP
REAL     ADDINF, NUINSD, OAMOVE(3), PNSP, PROPN, RDRANP

!.... See if we need to do some debug.

CALL DBCHK (DEBUG,'RDINF',5,ICYC)

!.... Don't bother doing anything if there is no new area.

IF (AREANU(IRRSP) .LE. 0.0) RETURN

IDI = MAXRR
DO 500 KSP=1,MAXSP
   IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
!
!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 500
!
   PNSP = PNINF(IRTSPC(KSP),IDI)

!....    First, modify probability of infection to account for the
!....    number of years in a cycle.

   PNSP = 1 - ((1 - PNSP) ** (FINT/PINT))

!....    Modify probability of infection given root to root contact
!....    based on the proportion of centers that are spore initiated
!....    (SPPROP).

   PNSP = PNSP * ((SPPROP(IDI) * SPTRAN(IDI)) + (1 - SPPROP(IDI)))

   IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 500

   I1 = ISCT(KSP,1)
   I2 = ISCT(KSP,2)

   DO 400 J=I1, I2
      I = IND1(J)

      IF (FPROB(I) .LE. 0.0) GOTO 400

      NUINSD = AREANU(IRRSP) * FPROB(I)
      ADDINF = NUINSD * PNSP

!           IF (ICYC .GT. 1) THEN
!              PROPI(I,ISTEP) = .001
!           ENDIF

!....       Get a random proportion of roots infected based around a
!....       mean proportion of roots infected for a new infection
!....       mean = 0.001 (Annosus), 0.05 (Armillaria, Phellinus)

      PROPI(I,ISTEP,2) = RDRANP(RRNEW(IDI))
      PROBI(I,ISTEP,2) = PROBI(I,ISTEP,2) + ADDINF
      PROBIU(I) = PROBIU(I) + NUINSD * (1 - PNSP)

!....       Add the new infection resulting from the center area
!....       expanding.
!....       (,1)=# trees infected, (,2)=total # new trees in area.

      EXPINF(IDI,1) = EXPINF(IDI,1) + ADDINF
      EXPINF(IDI,2) = EXPINF(IDI,2) + NUINSD

!....       Make sure that OAKL continues to contain the right number
!....       of killed trees. Assume that some proportion of those trees
!....       that were outside killed are now inside killed (either
!....       infected or uninfected) (eg. If 10% of the outside trees
!....       were killed from BB or windthrow then 10% of those trees
!....       just becoming inside trees were killed from BB or windthrow)
!....
!....       NUINSD -- Number of new trees inside the center.
!....       OAMOVE -- Amount to move from one OAKL slot to another.

      IF (OAKL(DSO,I) .GT. 0.0) THEN
         PROPN = OAKL(DSO,I) / (FPROB(I) * (SAREA - PAREA(IRRSP) + &
                    AREANU(IRRSP)))

         OAMOVE(DSO) = PROPN * NUINSD
         OAMOVE(DSII) = PROPN * ADDINF
         OAMOVE(DSIU) = PROPN * (NUINSD - ADDINF)

         CALL RDMREC (2,I,KSP,OAMOVE)

      ENDIF

      IF (DEBUG) WRITE(JOSTND,401) PROBI(I,ISTEP,2), PROBIU(I)
401       FORMAT(' IN RDINF PROBI PROBIU',2F10.2)
400    CONTINUE
500 CONTINUE

!.... Call RDSUM to calculate new value of PROBIT.

CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

RETURN
END

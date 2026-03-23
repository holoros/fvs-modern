SUBROUTINE RDIPRP(PROPN,NOPLOT,PRAN)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This subroutine calculates multipliers for each species for
!     distributing the user-defined number of infected trees in a
!     center.  The proportion of each species is defined by the
!     infection probability and the average time to death for the
!     species.
!
!  Called By :
!     RDSETP  (SUBROUTINE)  [ROOT DISEASE]
!
!  Calls :
!     RDSLP  (FUNCTION)   [ROOT DISEASE]
!
!  Arguments :
!     PROPN  -
!
!     NOPLOT -
!
!     PRAN   -
!
!  Local Variables :
!     <incomplete>
!
!  Common Block Variables Used :
!     <incomplete>
!
!  Revision History :
!  10/16/98 (RNH)
!    MOdificaion to more accurately calculate the number of trees
!    in each species to be inititially infected with root disease
!    with RRINIT keyword
!  02-AUG-01 Lance R. David (FHTET)
!    Changed initialization control loop from MINRR,MAXRR to 1,ITOTRR.
!  08/29/14 Lance R. David (FMSC)
!    Added implicit none and declared variables.
!------------------------------------------------------------------
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

!.... Argument variable declarations.

!.... Local variable declarations.

LOGICAL  YESSP(MAXSP), FULL(MAXSP)
INTEGER  I, IDI, II, INUMSP(ITOTRR), KSP, NOPLOT, NUMFUL(ITOTRR)
REAL     ADDON, AVGYTK, HABSP, OVER, PRAN(ITOTRR), &
            PROPN(MAXTRE), RDSLP, RELVAL(MAXSP), &
            SSTVAL(ITOTRR), TMPPRP(MAXSP), TOTINF(ITOTRR), &
            TOTNUM, TOTREE(ITOTRR), TOTSP(MAXSP), TOTVAL(ITOTRR), &
            TOTYTK(MAXSP), TREENO(MAXSP)
!
!     the array SSTVAL(MAXSP) is used in the calculation of initial
!     root disease in each species (RNH NOV98)
!
!.... Initializations.

AVGYTK = 0.0
OVER = 0.0

DO 99 IDI=1,ITOTRR
   TOTVAL(IDI) = 0.0
   TOTINF(IDI) = 0.0
   TOTREE(IDI) = 0.0
   INUMSP(IDI) = 0
   SSTVAL(IDI) = 0.0
   NUMFUL(IDI) = 0
99 CONTINUE

DO 100 KSP = 1, MAXSP
   YESSP(KSP) = .FALSE.
   FULL(KSP) = .FALSE.
   TOTSP(KSP) = 0.0
   TOTYTK(KSP) = 0.0
   TREENO(KSP) = 0.0
   RELVAL(KSP) = 0.0
   TMPPRP(KSP) = 0.0
100 CONTINUE

!.... Total the number of trees in center by species (TOTSP).
!.... Figure out the "TOTAL YEARS TO KILL" trees so we can get an
!.... average for the species in the next set of loops.

IDI = MAXRR

DO 1500 I= 1, ITRN
   IF (((IDPLOT(I) .EQ. NOPLOT) .OR. (NOPLOT .EQ. 0)) .AND. &
           (ISP(I) .NE. 0)) THEN
      KSP = ISP(I)
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
!
!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 1500
!
      TOTNUM = PROB(I) * PAREA(IDI)
      TOTSP(KSP) = TOTSP(KSP) + TOTNUM

      HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)
      YTKILL = RDSLP(DBH(I),XXINF,YYINF,NNINF)

      IF (YTKILL .GT. XMINKL(IDI)) THEN
         YTKILL = (YTKILL - XMINKL(IDI)) * HABSP * &
                     RRPSWT(IRTSPC(KSP)) + XMINKL(IDI)
      ENDIF

      TOTYTK(KSP) = TOTYTK(KSP) + YTKILL * TOTNUM

      IF (.NOT. YESSP(KSP) .AND. TOTSP(KSP) .GT. 0.0) &
            YESSP(KSP) = .TRUE.
   ENDIF
1500 CONTINUE

!.... Calculate the average year to kill of a particular species
!.... (AVGYTK).  Use the AVGTYK and the probability of infection for
!.... that species to find a relative value for the species (for
!.... calculating relative proportions of species killed)

!.... **Note that we are currently only using the defined probability
!....   and not correcting it for the number of years in a cycle.
!....   That would give slightly different results, but the cycle length
!....   should not affect the initialization (I Think).
!....   (Sarah, march 20/95)

IDI = MAXRR

DO 1600 KSP = 1, MAXSP
   IF (TOTSP(KSP) .GT. 0.0) THEN

      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))

!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 1600
!
      AVGYTK = TOTYTK(KSP) / TOTSP(KSP)
      RELVAL(KSP) = PNINF(IRTSPC(KSP),IDI) * AVGYTK

      TOTVAL(IDI) = TOTVAL(IDI) + RELVAL(KSP)

      TOTREE(IDI) = TOTREE(IDI) + TOTSP(KSP)
!
      IF (YESSP(KSP)) INUMSP(IDI) = INUMSP(IDI) + 1

ENDIF
1600 CONTINUE

!.... Calculate number to be infected in each species.
!.... TOTINF is the total number of trees to be infected in each
!.... disease type.  TREENO is the total number if trees to be infected
!.... by species, based on the relative likelihoods (calculated from
!.... YTK and PROB infection)
!.... PRAN is the proportion of infection in stand or plot.

DO 1699 IDI=MINRR,MAXRR
   TOTINF(IDI) = PRAN(IDI) * TOTREE(IDI)
!++++++
  DO 1698 KSP= 1, MAXSP

!     The following calculation was nested in this loop to for the
!     new initial ization method used in loop 1700
!     New array (RNH NOV98)
!
IF (TOTVAL(IDI) .GT. 0.0) &
    SSTVAL(IDI)= SSTVAL(IDI) + RELVAL(KSP)*TOTSP(KSP)/ &
                               TOTVAL(IDI)
!
1698   CONTINUE
!++++++
!
1699 CONTINUE

IDI = MAXRR

DO 1700 KSP = 1, MAXSP
!
IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
!
!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 1700
!
!     Commented out the following 2 lines and added a new formula to
!     calculate the total number of trees infected per species
!     (RNH Nov 98)
!
!            IF (TOTSP(KSP) .GT. 0.0 .AND. TOTVAL(IDI) .GT. 0.0)
!     &      TREENO(KSP) = TOTINF(IDI) * (RELVAL(KSP) / TOTVAL(IDI))
!
 IF (TOTSP(KSP) .GT. 0.0 .AND. TOTVAL(IDI) .GT. 0.0 .AND. &
       (SSTVAL(IDI) .GT. 0.0)) &
!
    TREENO(KSP) = TOTINF(IDI)*TOTSP(KSP)*RELVAL(KSP)/SSTVAL(IDI)/ &
    TOTVAL(IDI)
!
!
!
1700 CONTINUE

!.... Turn number of trees to be infected of each species into a
!.... proportion of the available trees within the center. This
!.... proportion may be greater than one if there are few trees but
!.... the probability of being infected is high.  If greater than one,
!.... then set to one and increase the others (If not also one) by
!.... dividing the remainder equally.

1750 CONTINUE

DO 1900 IRRSP=MINRR,MAXRR
   IF (INUMSP(IRRSP) .LE. NUMFUL(IRRSP)) GOTO 1850
   ADDON = OVER / (INUMSP(IRRSP) - NUMFUL(IRRSP))
   OVER = 0.0

   IDI = IRRSP
   DO 1800 KSP = 1, MAXSP
      IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
      IF (IDI .NE. IRRSP) GOTO 1800

      IF (YESSP(KSP) .AND. .NOT. FULL(KSP)) THEN
         TREENO(KSP) = TREENO(KSP) + ADDON
         TMPPRP(KSP) = TREENO(KSP) / TOTSP(KSP)

         IF (TMPPRP(KSP) .GT. 1.0) THEN
            TMPPRP(KSP) = 1.0
            FULL(KSP) = .TRUE.
            NUMFUL(IRRSP) = NUMFUL(IRRSP) + 1

            OVER = OVER + TREENO(KSP) - TOTSP(KSP)
         ENDIF
      ENDIF
1800    CONTINUE

   IF (NUMFUL(IRRSP) .EQ. INUMSP(IRRSP)) GOTO 1850
   IF (OVER .GT. 0.0 .AND. NUMFUL(IRRSP) .LT. INUMSP(IRRSP)) &
          GOTO 1750

1850    CONTINUE
   OVER = 0.0
1900 CONTINUE

DO 2000 II = 1,ITRN
   IF ((IDPLOT(II) .EQ. NOPLOT) .OR. (NOPLOT .EQ. 0)) THEN
      KSP = ISP(II)
      PROPN(II) = TMPPRP(KSP)
   ENDIF
2000 CONTINUE

RETURN
END

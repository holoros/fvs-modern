SUBROUTINE RDSPRD
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  THIS SUBROUTINE CALCULATES THE RATE OF SPREAD OF DISEASE CENTER
!  BY SIMULATING THE SPREAD THROUGH A SMALL STAND IN WHICH THE
!  TREES ARE REPRESENTED EXPLICITLY.
!
!  CALLED BY :
!     RDCNTL  [ROOT DISEASE]
!
!  CALLS     :
!     DBCHK   (SUBROUTINE)   [PROGNOSIS]
!     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
!     RDSLP   (FUNCTION)     [ROOT DISEASE]
!     RDRANN  (FUNCTION)     [ROOT DISEASE]
!
!  NOTES:
!    PNINF is here taken to be the probability of infection PER infected
!       tree contacted FOR PINT years. (where PINT is currently set to
!       10 years)
!    While the infection is growing into the tree center, the radius of
!       infection - RADINF - is negative and indicates how far the
!       infection is from the center.  RADINF is positive once the
!       infections starts spreading outwards.  RADINF will be positive
!       at the time of infection. Infection is from a neighbour whose
!       radius of infection overlaps the stump of the focal tree.
!
! SOME VARIABLES:
!   DISTNC(i,j)  the distance between the centers of each pair of trees
!   EFFSDI(1)    the effect of Stand Density Index for tree species 1,
!                equal to the proportion of the species-typical root
!                radius actually realized, given the total stand density
!   FINT         growth cycle used in the Annosus model
!   IRRSP        RR species to be modelled
!   IRSNYR       number of time periods for which to model RR spread
!   IRSTYP       1 = place trees randomly on sample plot
!                0 = regularly space trees on sample plot
!   IRTSPC(1)    maps Annosus tree species codes to FVS species codes
!   MCRATE(NMONT)  the spread rates determined by each Monte Carlo
!                simulation
!   NMONT        number of times to repeat MonteCarlo simulation of
!   NOW, NEW     pointers that indicate the present and future RADINF
!   NRSTEP       number of years in each time period
!   NTREES       actual number of trees used in the simulation
!                spread rate
!   NUMPAL(I), IDPAL(I, J)  Respectively, the number of trees with
!                which each focal tree 'i' has root contact ("pals"),
!                and the identity of those contact trees, for each
!                focal tree
!   PAREA(1)     area of the stand infected with RR species 1, in acres
!   PCOLO(1)     proportion of roots colonized after death of tree of
!                species1
!   PKILLS(1)    proportion of roots infected at death of tree of
!                species 1
!   PNINF(1)     probability of infection of tree species 1
!   RKILLS(i)    the radius of infection at which tree 'i' will die
!   ROOM         0/1 to indicate whether infection has crossed the
!                sample plot.  If it has, the rate of spread is
!                determined from the extent of infection at the end of
!                the previous period
!   RRGEN(IRRSP,2) target number of trees to put on sample plot
!   RRRATE(IRRSP), SDRATE(IRRSP)  the average and the s.d. of the
!   RRSARE       area of the sample plot used in the simulation
!   RRSFRN       the amount of random variation when trees are evenly
!                spaced
!   RRSMEN -
!   RRSRAD(i)    the species-typical root radius for a tree with the
!                DBH of tree 'i'
!   SAREA        stand area in acres
!   SFPROB       the density of uninfected trees outside desease
!                centers for the stand as a whole
!   SICK(i)      0/1 to indicate whether tree 'i' is infected
!   SINE(i)      the SIN of the angle of contact with infection for
!                each tree
!   TRURAD(i)    the realized root radius of each tree 'i', given EFFSDI
!   UPDATE       1 = update RADINF when infection from a neighbour has
!                    spread beyond the infection of the focal tree
!                0 = don't update
!   UPLTD        (UPDATE LIMITED) 1 = RADINF cannot be increased past
!                a tree's center by later contact with an infected
!                neighbour, 0 = it can be.  Where the increase in
!                RADINF is limited to the center, an increment of
!                between 0 and NRSTEP-1 years internal spread is added
!                (based on a random number).  This is because infection
!                will have been present for this long, on average,
!                before it is noted by the program
!                spread rates
!   YTK(i)       the years required for rr to kill a tree with the DBH
!                of tree 'i'.  We don't know whether this is time from
!                first infection, of time from infection of the center
!   YTKX         a multiplier of the YTK values calculated from a
!                linear equation
!
!  Revision History:
!    21-MAR-00 Lance R. David (FHTET)
!       Reduced RETURN statements to 1 at the end of routine.
!       Added Debug code.
!    07-AUG-01 Lance R. David (FHTET)
!       Initialization of variable YFORWD near statement lable 300.
!    19-AUG-14 Lance R. David (FMSC)
!       Variable NTREES declared locally is in RDCOM.F77. Local
!       declaration removed.
!   09/12/14 Lance R. David (FMSC)
!     Added implicit none and declared variables. The implicit type for
!     variable NEWDEN was not consistent with its use, declared REAL.
!
!----------------------------------------------------------------------

!.... PARAMETER INCLUDE FILES

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... COMMON INCLUDE FILES

INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

LOGICAL DEBUG
INTEGER GCENTS, I, I1, I2, ICEN, IDI, IDPAL(50,50), II, IN, &
           IRSSP(50), IRSTEP, IRSTP, IT, ITM, ITREES, J, JT, &
           JTIME, JYEARS, K, KSP, KT, NEW, NOW, NUMPAL(50), &
           ROOM, SICK(50), UPDATE, UPLTD
REAL    DIFF, DISTNC(50,50), EFFSDI(ITOTSP), HABSP, NEWDEN, &
           PNIN, PNSP, R, RDRANN, RDSLP, RKILLS(50), RPINT, RRSDIM, &
           RRSMEN, SFPROB, SINE(50), TRURAD(50), YDMAX, YFORWD, &
           YTK(50), YTKX

!.... SEE IF WE NEED TO DO SOME DEBUG.

CALL DBCHK (DEBUG,'RDSPRD',6,ICYC)

IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDSPRD'


!.... INITIALIZE CONSTANTS THAT MAY ONE DAY DEPEND ON USER INPUTS
!....
!.... IF YTKX = .5 THEN YEARS TO KILL IS FROM INFECTION TO DEATH
!.... IF YTKX = 1 THEN IT IS TIME FROM CENTER TO DEATH

YTKX = 1

DO 12 I = 1,ITOTSP
   EFFSDI(I) = 1.0
12 CONTINUE

UPDATE = 1
UPLTD = 1
NOW = 1
NEW = 2

!.... ZERO INFECTION ARRAYS

RRRATE(IRRSP) = 0.0
SDRATE(IRRSP) = 0.0

DO 5 JTIME = 1,NMONT
   MCRATE(IRRSP,JTIME) = 0.0
   MCTREE(IRRSP,JTIME) = 0
   SPRQMD(IRRSP,JTIME) = 0.0
5 CONTINUE

GCENTS = NCENTS(IRRSP) - NSCEN(IRRSP)

IF (GCENTS .LE. 0 .OR. PAREA(IRRSP) .EQ. 0.0) THEN

!....    don't need to zero out rrates any more since if GCENTS=0
!....    rrates should be filled with shrinking values.

   IF (NCENTS(IRRSP) .GT. 0.0) THEN
      DO 10 ICEN= 1,NCENTS(IRRSP)
         RRRATE(IRRSP) = RRRATE(IRRSP) + RRATES(IRRSP,ICEN)
10       CONTINUE

      RRRATE(IRRSP) = RRRATE(IRRSP) / NCENTS(IRRSP)
   ENDIF

   GOTO 2000   !RETURN
ENDIF

DIFF = SAREA - PAREA(IRRSP)
IF (DIFF .LE. 1E-3) GOTO 2000    !RETURN

!.... IF STAND IS RUN AS ONE CENTER THEN THERE IS NO SPREAD TO
!.... OUTSIDE AREAS. IF MANUAL INITIALIZATION CALCULATE SPREAD.

IF (LONECT(IRRSP) .EQ. 1) GOTO 2000     !RETURN


IF (IY(ICYC) .GE. IRGEN(4)) THEN

!....    CHANGE THE TREE SPATIAL DISTRIBUTION TO THE OTHER TYPE
!....    (RANDOM <-> REGULAR)

   ITM = IRSTYP
   IF (ITM .EQ. 0) IRSTYP = 1
   IF (ITM .EQ. 1) IRSTYP = 0
ENDIF

!.... THIS CALCULATION IS BASED ON DENSITY OUTSIDE OF DISEASED AREAS
!.... ONLY.  NOTE THAT ALL CACULATIONS ARE BASED ON THE LESSER OF LAST
!.... YEAR'S FRINGE DENSITY AFTER BARK BEETLES AND WINDTHROW HAVE BEEN
!.... APPLIED AND THIS YEAR'S OUTSIDE DENSITY. THE COMPARISON IS DONE
!.... IN RDOAGM EVERY YEAR (EVEN IF THERE ARE NO BARK BEETLES ACTIVE)

SFPROB = 0.0
IDI = IRRSP

DO 30 I = 1,ITRN
   IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
   IF (IDI .EQ. IRRSP) SFPROB = SFPROB + FFPROB(I,1)
30 CONTINUE

NEWDEN = SFPROB

!.... GET LINEAR DIMENSIONS IN FEET

IF (IRSTYP .EQ. 0) THEN
   RRSARE = RRGEN(IRRSP,2) / (NEWDEN + 1E-9)
ELSE
   ITREES = IFIX(SQRT(RRGEN(IRRSP,2)))
   RRSARE = (ITREES ** 2) / (NEWDEN + 1E-9)
ENDIF

IF (NEWDEN .LE. 0) GOTO 1020

RRSDIM = SQRT(RRSARE) * 208.7

IF (DEBUG) &
     WRITE (JOSTND,884) RRSARE,RRGEN(IRRSP,2),RRGEN(IRRSP,1)
884 FORMAT (' RRSARE RRGEN(IRRSP,2),RRGEN(IRRSP,1)',4F10.2)

DO 1000 JTIME = 1,NMONT

!....    SELECT TREES FOR THE AREA TO BE SIMULATED

   NTREES = 0
   IDI = IRRSP

   DO 100 KSP = 1,MAXSP
      IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(KSP))
!
!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 100
!
      HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)

!....       MODIFY THE TIME TO DEATH MULTIPLIER (HABSP) BASED ON THE
!....       PROPORTION OF CENTERS THAT ARE SPORE INITIATED (SPPROP).

      HABSP = HABSP * ((SPPROP(IDI) * SPYTK(IDI)) + &
                 (1 - SPPROP(IDI)))

      IF ((ISCT(KSP,1) .EQ. 0) .OR. (IDI .NE. IRRSP)) GOTO 100

      I1 = ISCT(KSP,1)
      I2 = ISCT(KSP,2)

      DO 90 J = I1,I2
         I = IND1(J)

!....          CALCULATE NUMBER OF TREES OF CLASS IN A
!....          IF TREE IS NOT OUTSIDE INFECTED AREA THEN DELETE

         IF (DEBUG) WRITE (JOSTND,778) PROBI(I,1,1), PROBIU(I), &
                       PROBIT(I), FFPROB(I,1)
778          FORMAT(' PROBI PROBIU PROBIT FFPROB',4F10.2)

         IF (FFPROB(I,1) .EQ. 0.0) GOTO 90

         RRSMEN = FFPROB(I,1) * RRSARE
         NUMTRE = IFIX(RRSMEN)
         PTRE = RRSMEN - NUMTRE
         R = RDRANN(0)
         IF (R .LE. PTRE) NUMTRE = NUMTRE + 1

         IF (DEBUG) WRITE (JOSTND,885) FFPROB(I,1), NUMTRE
885          FORMAT (' FFPROB=',F10.2,'NUMTRE=',I4)

         IF (NUMTRE .EQ. 0) GOTO 90

!....          GET TREE INFORMATION FROM TREE LIST

         DO 80 K = 1,NUMTRE
            NTREES = NTREES + 1

            IF (NTREES .GT. 50) THEN
               NTREES = 50
               GOTO 90
            ENDIF

            RRSDBH(NTREES) = DBH(I)
            RRSRAD(NTREES) = ROOTL(I)
            IRSSP(NTREES) = KSP
            YTK(NTREES) = RDSLP(RRSDBH(NTREES),XXINF,YYINF,NNINF)

            IF (DEBUG) WRITE (JOSTND,328) RRSDBH(NTREES), &
                          YTK(NTREES)
328             FORMAT(' RRSDBH,YTK=',2F10.2)

            YTK(NTREES) = (YTK(NTREES) - XMINKL(IRRSP)) * HABSP * &
                             RRPSWT(IRTSPC(KSP)) + XMINKL(IRRSP)

80          CONTINUE
90       CONTINUE
100    CONTINUE

!....    MODIFY THE ROOT RADIUS TO ACCOUNT FOR CROWDING (SDI)
!....    AND (POSSIBLY) CHANGE THE YTK TO REFLECT # YEARS FROM
!....    INFECTION TO DEATH ALSO CALCULATE THE QUADRATIC MEAN DIAMETER
!....    OF THE TREES INVOLVED IN THE SIMULATION

   DO 105 I = 1,NTREES
      TRURAD(I) = RRSRAD(I) * EFFSDI(IRTSPC(IRSSP(I)))
      RKILLS(I) = TRURAD(I) * PKILLS(IRTSPC(IRSSP(I)),IRRSP)
      YTK(I) = YTKX * YTK(I)

      SPRQMD(IRRSP,JTIME) = SPRQMD(IRRSP,JTIME) + RRSDBH(I) ** 2.0
105    CONTINUE

   SPRQMD(IRRSP,JTIME) = SQRT(SPRQMD(IRRSP,JTIME) / FLOAT(NTREES))
   MCTREE(IRRSP,JTIME) = NTREES

!....    SET UP TREE POSITIONS


   IF (IRSTYP .NE. 1) THEN

!....       RANDOM

      DO 150 IT = 1,NTREES
         XRRS(IT) = RDRANN(0) * RRSDIM
         YRRS(IT) = RDRANN(0) * RRSDIM
150       CONTINUE

   ELSE

!....       REGULAR

      ITREES = IFIX(SQRT(FLOAT(NTREES)))
      KT = 0

      DO 175 IT = 1,ITREES
         DO 170 JT = 1,ITREES
            KT = KT + 1
            XRRS(KT) = (IT - 0.5) * RRSDIM / FLOAT(ITREES) + &
                          (RDRANN(0) * 2 - 1) * RRSFRN
            YRRS(KT) = (JT - 0.5) * RRSDIM / FLOAT(ITREES) + &
                          (RDRANN(0) * 2 - 1) * RRSFRN
170          CONTINUE
175       CONTINUE

      NTREES = KT
   ENDIF

!....    DETERMINE DISTANCE BETWEEN EVERY PAIR OF TREES

   DO 205 I = 1, NTREES
      DO 200 J = (I + 1), NTREES
         DISTNC(I,J) = SQRT((XRRS(I) - XRRS(J)) ** 2 + &
                          (YRRS(I) - YRRS(J)) ** 2)
         DISTNC(J,I) = DISTNC(I, J)
200       CONTINUE
205    CONTINUE

!....    (RE)INITIALIZE TREE VARIABLES

   DO 210 I = 1, NTREES
      SICK(I) = 0
      RADINF(I, 1) = -TRURAD(I)
      RADINF(I, 2) = -TRURAD(I)
      NUMPAL(I) = 0
210    CONTINUE

!....    FIND ALL POSSIBLE CONTACT-TREES OF EACH TREE

   DO 225 I = 1, NTREES
      DO 220 J = (I + 1), NTREES
         IF (TRURAD(I) + TRURAD(J) .GE. DISTNC(I, J)) THEN
            NUMPAL(I) = NUMPAL(I) + 1
            NUMPAL(J) = NUMPAL(J) + 1
            IDPAL(I, NUMPAL(I)) = J
            IDPAL(J, NUMPAL(J)) = I
         ENDIF
220       CONTINUE
225    CONTINUE

!....    LOOP OVER TIME STEPS OF SIMULATION

   ROOM = 1
   JYEARS = 0

   RPINT = PINT

   DO 300 IRSTEP = 1, IRSNYR
      IF (ROOM .GT. 0) THEN
         JYEARS = JYEARS + NRSTEP

         DO 290 IN = 1, NTREES
            KSP = IRSSP(IN)

            IF (RADINF(IN, NOW) .LT. RKILLS(IN)) THEN
               PNSP = PNINF(IRTSPC(KSP),IRRSP)

!....                DETERMINE WHETHER UNINFECTED TREES ON THE BASELINE
!....                HAVE BECOME INFECTED

               IF ((SICK(IN) .EQ. 0) .AND. &
                      (YRRS(IN) .LE. TRURAD(IN))) THEN
                  R = RDRANN(0)

                  IF (IRSTEP .EQ. 1) THEN
                     PNIN = PNSP
                  ELSE
                     PNIN = 1 - ((1 - PNSP) ** (NRSTEP / RPINT))
                  ENDIF

                  IF (R .LE. PNIN) THEN
                     SICK(IN) = 1
                     RADINF(IN, NEW) = -YRRS(IN)
                     SINE(IN) = 1
                  ENDIF
               ENDIF

!....                DETERMINE WHETHER UNINFECTED TREES BECOME INFECTED,
!....                AND, IF UPDATE = 1, WHETHER INFECTED TREES BECOME
!....                MORE INFECTED, THROUGH CONTACT WITH NEIGHBOURS
!....                WHOSE INFECTION IS SPREADING.  IF UPTLD=1, THEN THE
!....                INCREASE IN INFECTION IS LIMITED TO THE CENTER
!....                OF THE TREE, WITH AN INCREMENT FOR SPREAD DURING
!....                THE 0 TO (NRSTEP-1) YEARS SINCE INFECTION AT THE
!....                CENTER WILL HAVE OCCURRED.

               IF ((IRSTEP .GT. 1) .AND. &
                      ((SICK(IN) .EQ. 0) .OR. &
                      ((UPDATE .EQ. 1) .AND. &
                      ((UPLTD .EQ. 0) .OR. &
                      (RADINF(IN, NOW) .LT. 0))))) THEN

                  DO 320 II = 1, NUMPAL(IN)
                     IT = IDPAL(IN, II)

                     IF (RADINF(IT, NOW) .GT. 0) THEN
                        IF (RADINF(IN, NEW) .LT. &
                               -(DISTNC(IT, IN) - &
                              RADINF(IT, NOW))) THEN

                           R = RDRANN(0)

                           PNIN = (1 - (1 - PNSP) ** &
                                     (NRSTEP / FINT))

                           IF (R .LE. PNIN) THEN
                              SICK(IN) = 1
                              RADINF(IN,NEW) = -(DISTNC(IT,IN) - &
                                                  RADINF(IT,NOW))

                              IF (DISTNC(IN, IT) .GT. 0) THEN
                                 SINE(IN) = (YRRS(IN) - &
                                               YRRS(IT)) / &
                                               DISTNC(IT, IN)
                              ELSE
                                 SINE(IN) = 1
                              ENDIF
                           ENDIF

                        ENDIF
                     ENDIF
320                   CONTINUE

                  IF ((UPLTD .EQ. 1) .AND. &
                         (RADINF(IN,NEW) .GT.0)) THEN

                     IRSTP = IFIX(RDRANN(0) * FLOAT(NRSTEP))

                     IF (IRSTP .EQ. NRSTEP) IRSTP = NRSTEP - 1
                     RADINF(IN, NEW) = FLOAT(IRSTP) * RRSRAD(IN) * &
                                          PKILLS(IRTSPC(KSP),IRRSP) / &
                                          YTK(IN)
                  ENDIF

               ENDIF

!....                SPREAD ROT THROUGH INFECTED ROOTS

               IF (SICK(IN) .EQ. 1) THEN
                  RADINF(IN,NEW) = RADINF(IN, NEW) + NRSTEP * &
                                      RRSRAD(IN) * &
                                      PKILLS(IRTSPC(KSP),IRRSP) / &
                                      YTK(IN)

                  IF (RADINF(IN, NEW) .GE. RKILLS(IN)) THEN
                     RADINF(IN,NEW) = PCOLO(IRTSPC(KSP),IRRSP) * &
                                         TRURAD(IN)
                  ENDIF
               ENDIF

               IF ((YRRS(IN) + RADINF(IN,NEW)) .GT. RRSDIM) &
                      ROOM = 0
            ENDIF

290          CONTINUE
      ENDIF

!....       UPDATE THE PRESENT RADII OF INFECTION, AND BEGIN NEXT
!....       TIME STEP

      IF (ROOM .GT. 0) THEN
         DO 295 I = 1, NTREES
            RADINF(I, NOW) = RADINF(I, NEW)
295          CONTINUE
      ENDIF

300    CONTINUE

!....    DETERMINE MAXIMUM SPREAD OF INFECTION.  IF INFECTION HAS
!....    CROSSED THE WHOLE SAMPLE PLOT, CALCULATE THE RATE FROM ITS
!....    EXTENT IN THE PREVIOUS TIME PERIOD (SUBSTITUTING THE PLOT
!....    HEIGHT IF INFECTION CROSSED THE SAMPLE PLOT IN THE FIRST
!....    TIME PERIOD).

   YDMAX = 0.0
   YFORWD = 0.0

   DO 330 IN = 1, NTREES
      IF (SICK(IN) .GT. 0) THEN

         IF (RADINF(IN, NOW) .GT. 0) THEN
            YFORWD = YRRS(IN) + RADINF(IN, NOW)
         ELSE
            IF (SINE(IN) .GT. 0) THEN
               YFORWD = YRRS(IN) - (SINE(IN)*(-RADINF(IN,NOW)))
            ENDIF
         ENDIF

         IF (YFORWD .GT. YDMAX) YDMAX = YFORWD

      ENDIF
330    CONTINUE

   IF (ROOM .GT. 0) THEN
      MCRATE(IRRSP,JTIME) = YDMAX / JYEARS
   ELSE
      IF (JYEARS .GT. NRSTEP) THEN
         MCRATE(IRRSP,JTIME) = YDMAX / (JYEARS - NRSTEP)
      ELSE
         MCRATE(IRRSP,JTIME) = RRSDIM / JYEARS
      ENDIF
   ENDIF

   RRRATE(IRRSP) = RRRATE(IRRSP) + MCRATE(IRRSP,JTIME)

1000 CONTINUE

RRRATE(IRRSP) = RRRATE(IRRSP) / (FLOAT(NMONT))

DO 1010 I = 1, NMONT
   SDRATE(IRRSP) = SDRATE(IRRSP) + (MCRATE(IRRSP,I) - &
                      RRRATE(IRRSP)) ** 2
1010 CONTINUE

SDRATE(IRRSP) = SQRT(SDRATE(IRRSP) / FLOAT(NMONT - 1))

1020 CONTINUE

IF (IRSPTY .EQ. 1) THEN

!....    DETERMINE WHAT SPREAD RATE TO ASSIGN TO EACH CENTER.
!....    AVERAGE (rrrate) IS RECALCULATED FROM APPLIED RATES.

   CALL RDRATE (NMONT)
ENDIF

2000 CONTINUE
IF (DEBUG) WRITE (JOSTND,2) RRRATE(IRRSP)
2 FORMAT (' EXIT RDSPRD:  RRRATE=',F9.2)

RETURN
END


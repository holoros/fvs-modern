SUBROUTINE RDSETP
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  SUBROUTINE FOR CALCULATING VALUES OF ROOT DISEASE VARIABLES
!  IN INITIALIZATION PHASE. ALSO CALLS RDAREA TO CALCULATE INITIAL
!  ROOT DISEASE AREA AND RDPR TO PRINT REPORT OF ROOT DISEASE
!  CONDITIONS. VALUES FOR THESE VARIABLES ARE ZEROED IN RDINIT.
!  PROGNOSIS VARIABLE PROB MAY BE SET IN THIS ROUTINE IF SAMPLING
!  ROOT DISEASE FROM TWO POPULATIONS (IE DISEASED AND NON-DISEASED
!  SUB-PLOTS.) ROOT DISEASE MODEL VARIABLE PROBL IS THEN LOADED
!  FROM PROB.
!
!  Called By :
!     RDMN1   [ROOT DISEASE]
!
!  Calls :
!     DBCHK   (SUBROUTINE)   [FVS]
!     RDCLOC  (SUBROUTINE)   [ROOT DISEASE]
!     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
!     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
!     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
!     RDIPRP  (SUBROUTINE)   [ROOT DISEASE]
!
!  Local Variables :
!  <incomplete>
!
!  Common Block Variables Used :
!     DIMEN  - (RDCOM)   (O)
!     FPROB  - (RDARRY)  (O)
!     I1, I2 - (RDCOM)   (O)
!     ICYC   - (CONTRL)  (I)
!     IND1   - (ARRAYS)  (I)
!     IPCFLG - (RDCOM)   (I)
!     IRDPLT - (RDADD)   (I)
!     IREC1  - (CONTRL)  (I)
!     ISCT   - (CONTRL)  (I)
!     JOSTND - (CONTRL)  (I)
!     LONECT - (RDADD)   (O)
!     MAXSP  - (PRGPRM)  (I)
!     PAREA  - (RDCOM)   (I)
!     PI     - (PLOT)    (I)
!     PRINF  - (RDCOM)   (O)
!     PRKILL - (RDCOM)   (I)
!     PROB   - (PROB)    (I/O)
!     PROBI  - (RDARRY)  (O)
!     PROBIT - (RDARRY)  (I)
!     PROBIU - (RDARRY)  (O)
!     PROBL  - (RDARRY)  (O)
!     PROPI  - (RDARRY)  (O)
!     PRPTOT - (RDCOM)   (O)
!     PRUN   - (RDCOM)   (I)
!     RRGEN  - (RDCOM)   (O)
!     RRINCS - (RDCOM)   (I)
!     RRTINV - (RDADD)   (I)
!     SAREA  - (RDCOM)   (I)
!     <incomplete>
!
!  Revision History :
!     05/03/97 - Matthew K. Thompson (FHTET)
!                Substituted the variable IREC1 for ITRN everywhere
!                in the subroutine.  When RDSETP is called the value
!                of ITRN (number of tree records) still includes
!                recently dead trees.  IREC1 is the number of
!                projectable records.
!
!     05/13/97 - Matthew K. Thompson (FHTET)
!                Changed code so that if number of plots equals number
!                of disease plots than patch area equals stand area.
!                Re-numbered statement labels.  Fixed call to RDIPRP,
!                an element of the array TMP was being passed to RDIPRP
!                not the whole array.  Removed the section of code that
!                recalculates PROB.
!     06/02/98 - Robert N. Havis (FHTET)
!                Branched out of tree loop in several places
!                when non-host tree record was beeing analysed
!     17-JUL-2002 Lance R. David (FHTET)
!                Modified/added debug code.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!------------------------------
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

LOGICAL DEBUG
INTEGER I, I1, I2, IDI, IIPI, IK, J, K, KSP, NDPLTS(ITOTRR)
REAL    PROPN(MAXTRE), RDRANP, TMP(ITOTRR), TPROP

!.... See if we need to do some debug.

CALL DBCHK (DEBUG,'RDSETP',6,ICYC)
IF (DEBUG) WRITE (JOSTND,100) ICYC,MINRR,MAXRR
100 FORMAT(' ENTER RDSETP: CYCLE=',I4,' MINRR=',I1,' MAXRR=',I1)

TPROP = 0.0
DO IDI=1,ITOTRR
   NDPLTS(IDI) = 0
   TMP(IDI)    = 0.0
END DO

!.... Calculate number of diseased sub-plots.  If user has specified
!.... diseased and non-diseased sub-plots and number of diseased plots
!.... equals the number of total plots in the inventory then stand is
!.... considered as one center.

DO 300 IDI=MINRR,MAXRR
   DO 200 K = 1,50
      IF (IRDPLT(IDI,K) .NE. 0) THEN
         NDPLTS(IDI) = NDPLTS(IDI) + 1
      ENDIF
200    CONTINUE
300 CONTINUE

IIPI = INT(PI)
DO 303 IDI=MINRR,MAXRR
   IF (IIPI .EQ. NDPLTS(IDI)) THEN
      LONECT(IDI) = 1
      PAREA(IDI) = SAREA
   ENDIF

   IF (LONECT(IDI) .EQ. 1) NDPLTS(IDI) = 1
303 CONTINUE

!.... Initialize the variables for the SDI effect on roots.

IF (SDISLP .EQ. 0.0 .OR. SDNORM .EQ. 0.0) THEN
   YINCPT = 1.0
ELSE
   YINCPT = 1.0 - (SDNORM * SDISLP)
ENDIF

!.... Set square dimension of the stand.

DIMEN = 0.0
IF (SAREA .NE. 0.0) THEN
   DIMEN = SQRT(SAREA) * 208.7
ENDIF

!.... Calculate the area of the disease patches.

DO 600 IRRSP=MINRR,MAXRR
   IF (IPCFLG(IRRSP) .EQ. 1) THEN

!....       Disease centers are assigned by the user.

      CALL RDAREA
   ELSE

!....       DISEASE CENTERS ARE ASSIGNED RANDOMLY.

      IF (PAREA(IRRSP) .GT. 0.0) THEN
         CALL RDCLOC
         CALL RDAREA
      ELSE
         PAREA(IRRSP) = SAREA * NDPLTS(IRRSP) / PI
         CALL RDCLOC
      ENDIF
   ENDIF

   IF (DEBUG) WRITE(JOSTND,400) PAREA(1), PAREA(2), PAREA(3), &
                                   PAREA(4)
400    FORMAT (' IN RDSETP: PAREA=',4F10.2)

   SPPROP(IRRSP) = 0.0

!....    If not initialized from the tree list, use manual esitmate of
!....    number of infected and uninfected trees in disease pockets??
!....    PRKILL and PRUN are densities of trees inside centers. They
!....    are changed into proportions here, using RRGEN, which is
!....    then set to 1.0.

   IF (.NOT. RRTINV .AND. .NOT. LPLINF) THEN
      RRGEN(IRRSP,1) = PRKILL(IRRSP) + PRUN(IRRSP)

      IF (RRGEN(IRRSP,1) .GT. 0.0) THEN
         PRKILL(IRRSP) = PRKILL(IRRSP) / RRGEN(IRRSP,1)
         PRUN(IRRSP) = PRUN(IRRSP) / RRGEN(IRRSP,1)
         RRGEN(IRRSP,1) = 1.0
      ENDIF

   ELSEIF (.NOT. RRTINV .AND. LPLINF) THEN
      RRGEN(IRRSP,1) = 1.0
   ENDIF

   IF (DEBUG) WRITE (JOSTND,500) PI, NDPLTS(1), NDPLTS(2), &
                                    NDPLTS(3), NDPLTS(4)
500    FORMAT (' IN RDSETP: PI NDPLTS=',F3.0,4I5)
600 CONTINUE

!.... Figure out what proportion of each record becomes infected.
!.... PROPN is by tree record.  If the PLOTINF keyword is used, the
!.... same species may have different infection rates.

IF (.NOT. RRTINV .AND. .NOT. LPLINF) THEN
   CALL RDIPRP(PROPN,0,PRKILL)
ENDIF

IF (.NOT. RRTINV .AND. LPLINF) THEN

!....    Initialized via the PLOTINF keyword

   DO 700 IDI=MINRR,MAXRR
      TMP(IDI) = 0.0
700    CONTINUE

   DO 900 IDI=MINRR,MAXRR
      IF (IDI .GT. MINRR) TMP(IDI-1) = 0.0

      DO 800 IK=1,50
         IF (IANPLT(IDI,IK) .LE. 0) GOTO 900
         TMP(IDI) = PLPROP(IDI,IK)

         CALL RDIPRP(PROPN,IANPLT(IDI,IK),TMP)

800       CONTINUE
900    CONTINUE
ENDIF

!.... Load the initial tree list from FVS into the root disease
!.... tree list arrays that characterize the trees as inside and
!.... outside of patches  (also infected and uninfected).
!
!     Note

IDI = MAXRR
DO 2100 I = 1, IREC1

!.....   If Annosus then select S-type or P-type.

!         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I))) ! changed 11/24/2015

!....    If both P and S type Annosus is being used, set disease area
!....    to that of the disease type of the host tree species; otherwise,
!....    use the disease area of the one disease being simulated even though
!....    current tree may not be specified as host species.

   IF (MAXRR .LT. 3 .AND. MINRR .NE. MAXRR) THEN
      IDI = IDITYP(IRTSPC(ISP(I)))
   ENDIF

!
!     Branch outof loop for non-host tree ((RNH June98)
!
IF (IDI .LE. 0) GO TO 2100
!
!
   IF (RRTINV .AND. .NOT. LPLINF) THEN

!....       The model is being initialized from the tree list.

      IF (IPRFL(I) .EQ. 1) THEN

!....          This tree record has a severity code 1, tree within
!....          30 feet of infected tree.  This tree is given an infection
!....          level around 10% of the tree species infection level at
!....          death.

         IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 1.'

         TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.1
         PROPI(I,1,1) = RDRANP(TPROP)

      ELSEIF (IPRFL(I) .EQ. 2) THEN

!....          This tree record has a severity code 2, pathogen or
!....          diagnostic symptoms detected.  This tree is given an
!....          infection level around 65% of the tree species infection
!....          level at death.

         IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 2.'

         TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.65
         PROPI(I,1,1) = RDRANP(TPROP)

      ELSEIF (IPRFL(I) .EQ. 3) THEN

!....          This tree record has a severity code 3, crown
!....          deterioration detected.  This tree is given an infection
!....          level around 75% of the tree species infection level at
!....          death.

         IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 3.'

         TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.75
         PROPI(I,1,1) = RDRANP(TPROP)

      ELSE

!....          Model is initialized from the tree list.  This tree
!....          record does not show infection (no root disease damage
!....          code).  The tree record is given a random proportion
!....          of roots infected for new infection. MEAN = 0.001.
!....
!....          I don't see why we initialize root disease level in
!....          a tree with no root disease. It doesn't matter at any
!....          point but seems useless.
!              PROPI(I,1,1) = RDRANP(0.001)

         IF (DEBUG) WRITE (JOSTND,*) '# No Infection.'

      ENDIF

!....       Scale PROB variables based on how patches are being
!....       defined.

      IF (LONECT(IDI) .EQ. 1 .OR. &
                (SAREA .EQ. PAREA(IDI))) THEN

!....          Stand being run as one disease center so scale PROB
!....          variables by value of patch area.

         IF (DEBUG) WRITE (JOSTND,1400)
1400          FORMAT(' # Run as 1 center, set PROBIU and PROBI.')

         PROBIU(I) = PROBIU(I) * PAREA(IDI)
         PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)
         FPROB(I) = 0.0

      ELSEIF (LONECT(IDI) .EQ. 0) THEN

!....          Stand being run without knowledge of the patches so
!....          scale PROB variables by value of patch area.

         IF (DEBUG) WRITE (JOSTND,1500)
1500          FORMAT(' # No knowledge of patches, ', &
                   'set PROBIU and PROBI.')

         PROBIU(I) = PROBIU(I) * PAREA(IDI)
         PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)

         IF (PAREA(IDI) .EQ. SAREA) THEN

!....             If patch area equals stand area then there are no
!....             trees outside patches.

            IF (DEBUG) WRITE (JOSTND,1525)
1525             FORMAT(' # No knowledge of patches, PAREA = SAREA.')

            FPROB(I) = 0.0

         ELSE

!....             Calculate trees/acre outside of the patches.

            IF (DEBUG) WRITE (JOSTND,1550)
1550             FORMAT(' # No knowledge of patches, PAREA /= SAREA.')

            FPROB(I) = (PROB(I) * SAREA - (PROBIU(I) + &
                          PROBI(I,1,1))) / (SAREA - PAREA(IDI))
         ENDIF

      ELSEIF (LONECT(IDI) .EQ. 2) THEN

!....          Stand being run as multiple plots so scale PROB
!....          variables by value of the stand area.

         IF (DEBUG) WRITE (JOSTND,1600)
1600          FORMAT(' # Run as multiple plots, ', &
                   'set PROBIU, PROBI, and FPROB.')

         PROBIU(I)  = PROBIU(I) * SAREA
         PROBI(I,1,1) = PROBI(I,1,1) * SAREA
         FPROB(I)   = FPROB(I) * (SAREA / (SAREA - PAREA(IDI)))
      ENDIF

      IF (DEBUG) WRITE (JOSTND,1650) I, ISP(I), PROB(I), &
                                        PROBI(I,1,1), PROBIU(I), &
                                        FPROB(I)
1650       FORMAT (' IN RDSETP: BEFORE-1 SCALE: I ISP PROB PROBI ', &
                 'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

   ELSE

!....       Model is initialized manually from the RRINIT keyword
!....       and/or the model is initialized through the PLOTINF keyword.
!....

      IF ((.NOT. LPLINF .OR. IDPLOT(I) .GT. 0) .AND. &
             IDI .NE. 0) THEN

!....          If (not using PLOTINF or tree records are inside
!....          infected plots) and the tree species is a host then
!....
!....          The proportion of infection for each tree record is taken
!....          from the random proportion function with the mean being
!....          the value entered by the user in field 5 of the RRINIT
!....          keyword.

         IF (DEBUG) WRITE (JOSTND,1700)
1700          FORMAT(' # Using only RRINIT, or tree record inside ', &
                   'infected plot specified by PLOTINF ', &
                   'set PROBIU, PROBI, and FPROB.')

         IF (PAREA(IDI) .GT. 0.0) &
               PROPI(I,1,1) = RDRANP(RRINCS(IDI))

         PROBI(I,1,1) = PROPN(I) * PROB(I) * PAREA(IDI) / &
                           (RRGEN(IDI,1) + 0.00001)
         PROBIU(I) = (1.0 - PROPN(I)) * PROB(I) * &
                         PAREA(IDI) / (RRGEN(IDI,1) + 0.00001)
         FPROB(I) = PROB(I)

      ELSE

!....          Initialized by PLOTINF keyword, but this record
!....          belongs to a plot which is outside disease centers

!
!     If non-host species exit loop (RNH May98)
!
IF (IDI .LE. 0) GO TO 2100
!
IF (DEBUG) WRITE (JOSTND,1800)
1800          FORMAT(' # Using PLOTINF, this tree is in plot which ', &
                   'is outside disease centers, set FPROB.')

         PROBI(I,1,1) = 0.0
         PROBIU(I) = 0.0
         FPROB(I)  = (PROB(I) * SAREA - PROBIU(I) - &
                        PROBI(I,1,1)) / (SAREA - PAREA(IDI))

      ENDIF

      IF (DEBUG) WRITE (JOSTND,1900) I, ISP(I), PROB(I), &
                                        PROBI(I,1,1), PROBIU(I), &
                                        FPROB(I)
1900       FORMAT (' IN RDSETP: BEFORE-2 SCALE: I ISP PROB PROBI ', &
                 'PROBIU FPROB ', I4, I4, 4(2X,F10.2))


   ENDIF

!....    If stand is being modeled as diseased and non-diseased
!....    subplots then modify PROB to recognize the effect of stand
!....    being the sum of two populations.

!M       IF (LONECT(IDI) .EQ. 2) THEN
!M          PROB(I) = (PROBI(I,1,1) + PROBIU(I) + FPROB(I) *
!M   &                (SAREA - PAREA(IDI))) / (SAREA + 1E-6)
!M       ENDIF

!....    Now load PROBL after PROB has been modified.

   PROBL(I) = PROB(I)

   IF (DEBUG) WRITE(JOSTND,2000) I,ISP(I),PROB(I),PROBI(I,1,1), &
                 PROBIU(I),FPROB(I)
2000    FORMAT (' IN RDSETP: AFTER    SCALE: I ISP PROB PROBI ', &
              'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

2100 CONTINUE

!.... End tree loop.

CALL RDSUM (IREC1,PROBIT,PROBI,ISTEP)

!.... Decay the root system of stumps initialized through STREAD or
!.... the treelist since the stumps may have been there for longer than
!.... just the inventory year.

CALL RDINOC (.TRUE.)

!.... Accumulate proportion of root systems infected inside patches
!.... for output in inventory year.
!....
!.... (Remember that PRINF & PRPTOT contain RR info for first ITOTRR
!.... slots and tree species info after that.)

DO 2200 IDI=1,ITOTRR
   PRPTOT(IDI) = 0.0
   PRINF(IDI) = 0.0
2200 CONTINUE

IDI = MAXRR

DO 2400 KSP = 1,MAXSP
   IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
!
!     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
!     If non-host species then IDI = 0, and loop should be skipped to
!     prevent array out of bounds error
!
IF (IDI .LE. 0) GO TO 2400
!
   IF (ISCT(KSP,1) .EQ. 0) GOTO 2400
   I1 = ISCT(KSP,1)
   I2 = ISCT(KSP,2)

   PRINF(KSP+ITOTRR)  = 0.0
   PRPTOT(KSP+ITOTRR) = 0.0

   DO 2300 J = I1,I2
      I = IND1(J)

!           If IND1(j) is pointing to a dead tree (bottom part of tree
!           list), then skip it. Also skip if PROPI < 0

      IF (I .GT. IREC1) GOTO 2300

      IF (PROPI(I,1,1) .GT. 0.0) THEN
         PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) + PROBI(I,1,1) * &
                                 PROPI(I,1,1)
         PRINF(IDI) = PRINF(IDI) + PROBI(I,1,1) * PROPI(I,1,1)
      ENDIF

      PRPTOT(KSP+ITOTRR) = PRPTOT(KSP+ITOTRR) + PROBI(I,1,1)
      PRPTOT(IDI)   = PRPTOT(IDI) + PROBI(I,1,1)
2300    CONTINUE

   PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) / &
                          (PRPTOT(KSP+ITOTRR) + 1E-6)

2400 CONTINUE

DO 2500 IDI=MINRR,MAXRR
   PRINF(IDI) = PRINF(IDI) / (PRPTOT(IDI) + 1E-6)
2500 CONTINUE

IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDSETP'

RETURN
END

SUBROUTINE BWEPDM
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!
!     PERIODIC DAMAGE.  TRANSLATES ANNUAL DAMAGE ACCUMULATORS INTO
!     DAMAGE DONE TO EACH TREE.
!
!     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
!     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984
!
!     minor changes by K.Sheehan 7/96 to remove LBWDEB,JBWPL4
!
!     CALLED FROM :
!
!       BWECUP - SINGLE STAND BUDWORM MODEL LINK TO PROGNOSIS.
!
!     FUNCTIONS CALLED :
!
!       BWERNP  - GENERATE A BETA VARIATE WITH PARAMETERS A FUNCTION
!                OF THE MEAN.
!
!     SUBROUTINES CALLED :
!
!       BWECRC - DETERMINE CROWN CLASS OF TREE.
!
!  Revision History:
!
!     28-DEC-1999  Lance R. David (FHTET)
!       Update for expansion of FVS stand id (variable NPLT)
!       from 8 to 26 characters.
!     01-SEP-2000  Lance R. David (FHTET)
!       SDI-based mortality until stand reaches 10 inch QMD update
!       to FVS leaves BAMAX variable at zero, during these times.
!       BAMAX is needed by this subroutine and will be calculated from
!       FVS variable SDIMAX when variable BAMAX is zero. Look for local
!       variable TBAMAX to find the few lines of code for this update.
!     10-NOV-2000  Lance R. David (FHTET)
!       Added comments and species lits for IFIR array.
!     13-JUN-2001  Lance R. David (FHTET)
!       Added debug handling.
!       Random variations applied to height and diameter growth values
!       in the growth adjustments section sometimes resulted in negative
!       values larger than the current DBH and/or HT. This was specifically
!       encountered with natural regen trees that were very small (i.e.
!       0.1 DBH and 1 foot tall). A check was added to set any negative
!       increment values to zero.
!     16-AUG-2001 Lance R. David (FHTET)
!       Added calls to BWERPT and BWERGT to place and retrieve the damage
!       model random number seed into the generator so that random
!       numbers used in function BWERNP and subroutine BWEBET are from
!       the damage model random number series and not from one of the
!       other two random number series used in the GenDefol model,
!       weather and outbreak scheduling.
!     15-APR-2003 Lance R. David (FHTET)
!       Replaced old mortality process with new equations based on
!       Mike Marsden's analysis of Bruce Hostetler's data. Kathy
!       Sheehan interpreted Mike's memos and created the equation
!       put into the code. Several new category variables used in the
!       new mortality equation were also added. Those variables are
!       for topkill, missing foliage top, and missing foliage middle.
!       Values for basal area of all trees and basal area of host trees
!       are also computed from begining of cycle FVS values for each
!       sample point (or plot) in the data set. Note that the mortality
!       rate applied is limited to 0.98 and this limit was already in
!       place before this update.
!     28-OCT-2003 Lance R. David (FHTET)
!       New topkill proportion function and coefficients from Mike's
!       analysis put into routine. Note that this is just the amount of
!       topkill and that the probability of topkill is unchanged.
!       There is a deviation from the analysis regarding the topkill
!       variable in the function. The analysis specifies topkill year
!       prior, but this routine does not cycle on a annual time step.
!       If this model is active and defoliation has occured, a tree's
!       existing topkill is assumed to have been the result of
!       defoliation. What can not be determined is the length of time
!       that has past since topkill or if the current truncated height
!       is the result of multiple topkill events.
!       Added include for dwarf mistletoe model common file MISCOM.F77
!       because dwarf mistletoe rating is utilized in the topkill
!       proportion function.
!
!    21-DEC-2005 Lance R. David (FHTET)
!       Set surrogate species coefficients from existing DF and GF
!       coefficients.
!    14-JUL-2010 Lance R. David (FMSC)
!       Added IMPLICIT NONE and declared variables as needed.
!
!    03/27/2012 Lance R. David
!       Added requirement that missing foliage categories for top and
!       middle crown must be GT 0.0 for topkill and mortality values to
!       be calculated.
!       Extremely high mortality is calculated when MFT and/or MFM is 0.0
!----------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'COEFFS.f90'
INCLUDE 'BWESTD.f90'
INCLUDE 'BWECOM.f90'
INCLUDE 'MISCOM.f90'
!
!OMMONS
!
INTEGER I, IBWYR, IC, ICRC1, ICRC2, IFIR(6), IHOST, II, IOD, &
           ISPI, ISZI, ITRC1, ITRC2, ITREE, NEW, NOBWYR
REAL AF, AVDEF, BARK, BASE, BRATIO, BWERNP, CN, D, DBHI, DDS, &
        DGI, DTK, FA, FTKILL, H, HTGI, ORGHT, PART, PRTOPK, SDEF, &
        STREES, TOPH, X, XD, XH
REAL    TBAMAX, PNTBA(MAXPLT), PNTHBA(MAXPLT), &
           B0(6), B1(6), B2(6), B3(6), B4(6), B5(6), B6(6), B7(6), &
           TK0(6), TK1(6), TK2(6), TK3(6), TK4(6), TK5(6), &
           TK6(6), TK7(6), TK8(6), &
           KTK, MFM, MFT, PR, XPR, PCTK

LOGICAL DEBUG

!     fir/non-fir tree species index
!               WF DF GF AF ES WL
DATA IFIR/ 0, 1, 0, 0, 1, 1/

!     DEFOLIATION MODEL SPECIES INDICES:
!       1 = WF - White fir      --> uses GF coefficients
!       2 = DF - Douglas fir    --> original coefficient from analysis
!       3 = GF - Grand fir      --> original coefficient from analysis
!       4 = AF - Subalpine fir  --> uses GF coefficients
!       5 = ES - Engelmann spruce --> uses DF coefficients
!       6 = WL - Western Larch  --> uses DF coefficients
!       7 = Not a host

!
!     Survival equation coefficients indexed by specie
!
! intercept
DATA B0 &
    /46.27900, 57.75010, 46.27900, 46.27900, 57.75010, 57.75010/
! elevation
DATA B1 &
    /-1.80810, -2.28210, -1.80810, -1.80810, -2.28210, -2.28210/
! elevation2
DATA B2 &
    / 0.01930,  0.02430,  0.01930,  0.01930, 0.02430,   0.02430/
! basal_area_at_point
DATA B3 &
    / 0.00550,      0.0,  0.00550,  0.00550,     0.0,       0.0/
! host_basal_area_at_point
DATA B4 &
    /-0.00808, -0.00793, -0.00808, -0.00808, -0.00793, -0.00793/
! missing_foliage_top
DATA B5 &
    / 0.57450,  0.92870,  0.57450,  0.57450,  0.92870,  0.92870/
! topkill category
DATA B6 &
    /-0.24050, -0.22330, -0.24050, -0.24050, -0.22330, -0.22330/
! missing_foliage_top * missing_foliage_middle
DATA B7 &
    /-0.09640, -0.13180, -0.09640, -0.09640, -0.13180, -0.13180/

!
!     Topkill equation coefficients indexed by specie
!
! intercept
DATA TK0 &
    /21.89880, 17.76920, 21.89880, 21.89880, 17.76920, 17.76920/
! elevation
DATA TK1 &
    /-0.00714, -0.00535, -0.00714, -0.00714, -0.00535, -0.00535/
! elevation2
DATA TK2 &
    / 6.99E-6,  5.22E-6,  6.99E-6,  6.99E-6,  5.22E-6,  5.22E-6/
! dwarf mistletoe rating
DATA TK3 &
    /     0.0, -0.01270,      0.0,      0.0, -0.01270, -0.01270/
! percent topkill for the tree in the year prior.
DATA TK4 &
    /-5.26520, -7.94630, -5.26520, -5.26520, -7.94630, -7.94630/
! missing foliage top
DATA TK5 &
    /-0.13630, -0.20360, -0.13630, -0.13630, -0.20360, -0.20360/
! missing foliage middle
DATA TK6 &
    /  0.28030,  0.12690,  0.28030,  0.28030,  0.12690,  0.12690/
! missing foliage middle X percent topkill last year
DATA TK7 &
    /  0.14030,  0.38010,  0.14030,  0.14030,  0.38010,  0.38010/
! missing foliage top X missing foliage middle
DATA TK8 &
    / -0.03770, -0.04120, -0.03770, -0.03770, -0.04120, -0.04120/


!
!.... Check for DEBUG
!
CALL DBCHK(DEBUG,'BWEPDM',6,ICYC)

IF (DEBUG) WRITE (JOSTND,*) 'ENTER BWEPDM: ICYC = ',ICYC
!
!
!     ********************** EXECUTION BEGINS **************************
!
!     COMPUTE FA TO BE THE FRACTION OF THE PERIOD WHERE THE BWMODEL
!     HAS NOT RUN AND IBWYR TO BE THE NUMBER OF YEARS THE MODEL HAS
!     RUN.  THESE VALUES WILL BE USED TO MODIFY THE GROWTH LOSS RATES.
!
IF (IBWYR2+1.LT.IY(ICYC+1).AND.IBWYR2.GE.0) THEN
   NOBWYR=IY(ICYC+1)-(IBWYR2+1)
   IBWYR=IFINT-NOBWYR
   FA=FLOAT(NOBWYR)/FINT
ELSE
   NOBWYR=0
   IBWYR=IFINT
   FA=0.0
ENDIF
!
!     IF THE CYCLE IS LONGER THAN THE OUTBREAK, THEN: ADD THE
!     FRACTIONAL PART OF THE PERIOD TO THE EXPECTED INCREMENTS...
!     PEDDS AND PEHTG.
!
IF (FA.GT.0.0) THEN
   DO 20 IHOST=1,6
      IF (IFHOST(IHOST).EQ.0) GOTO 20
      DO 10 ISZI=1,3
         PEDDS(IHOST,ISZI)=PEDDS(IHOST,ISZI)+FA
         PEHTG(IHOST,ISZI)=PEHTG(IHOST,ISZI)+FA
10       CONTINUE
20    CONTINUE
ENDIF

IF (DEBUG) THEN
   WRITE(JOSTND,*) 'IN BWEPDM: IBWYR=',IBWYR,' IBWYR2=',IBWYR2, &
      ' NOBWYR=',NOBWYR,' FA=',FA
   WRITE(JOSTND,*) 'IN BWEPDM: PEDDS=',PEDDS
   WRITE(JOSTND,*) 'IN BWEPDM: PEHTG=',PEHTG
ENDIF
!
!     RESTORE THE DAMAGE MODEL RANDOM NUMBER SEED TO THE GENERATOR.
!
CALL BWERPT (DSEEDD)

!
!     APPLY THE DAMAGE TO THE TREE RECORDS.
!
!     write header for periodic damage table output if requested.
!
IF (LBWPDM) WRITE (JOWSBW,30) IY(ICYC+1)-1,NPLT,MGMID
30 FORMAT (/'PERDAM: ',I4,'; ',A26,'; ',A4,' MORTALITY',T51, &
           'DBH GROWTH',T65,'HT GROWTH',T79,'PROB',T86,'TOP'/ &
           'TREE',T14,'DBH  ORG HT',T27,'TREES',T35,'BASE', &
           T44,'USED',T51,'BASE',T58,'USED',T65,'BASE',T72,'USED', &
           T78,'OF TOP',T86,'KILL'/ ' NO.',T7,'SP.', &
           T13,'(IN)',T20,'(FT)',T27,'/ACRE',T34,'(/ACRE)', &
           T42,'(/ACRE)',T51,'(IN)',T58,'(IN)',T65,'(FT)',T72,'(FT)', &
           T79,'KILL',T86,'(FT)'/ &
           '----  --- ',2(' ------'),3(' -------'),6(' ------'))
!
!     Run through tree list to sum basal area for all trees and
!     and basal area for host trees 3+ inch DBH on each sample point.
!     FVS array ITRE() holds point (or plot) number.
!
!     Note from Bruce indicated that Tommy included all trees when he
!     provided the point basal area values to Mike. So, condition is
!     commented out. 19-MAR-2003
!
DO I = 1,IPTINV
   PNTBA(I) = 0.0
   PNTHBA(I) = 0.0
ENDDO

DO 40 I = 1,ITRN
!CC      IF (DBH(I) .GE. 3.0) THEN      ----see note above----
!           sum basal area for all trees on point
      PNTBA(ITRE(I)) = PNTBA(ITRE(I)) &
                        + DBH(I)*DBH(I)*0.005454154

      IF (IBWSPM(ISP(I)) .LT. 6) THEN
!              sum basal area for bw host trees on point
         PNTHBA(ITRE(I)) = PNTHBA(ITRE(I)) &
                            + DBH(I)*DBH(I)*0.005454154
      ENDIF
!CC      ENDIF                          ----see note above----
40 CONTINUE
!
!     DO FOR ALL TREES.
!
PRTOPK=0.0
FTKILL=0.0
IF (ITRN.LE.0.OR.IBWYR2.EQ.-1) GOTO 70

DO 60 ISPI=1,MAXSP
   IF (ISCT(ISPI,1).EQ.0) GOTO 60
!
!        IF THE TREE IS NON-HOST OR LARCH, THEN: BYPASS THE CALCULATIONS.
!
   IHOST=IBWSPM(ISPI)
   IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: IHOST=',IHOST
   IF (IHOST.GE.6) GOTO 60

   DO 50 II=ISCT(ISPI,1),ISCT(ISPI,2)
      I=IND1(II)
      H=HT(I)
      ORGHT=H
      BARK=BRATIO(ISPI,DBH(I),H)
!
!           FIND THE HEIGHT AND CROWN CLASS INDICIES FOR THE TREE RECORD.
!
      CALL BWECRC(H,ISZI,ICRC1,ICRC2)
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: ICRC1=',ICRC1
!
!           Calculate variables use in the proportion of topkill and
!           probability of mortality equations.
!
!           Compute missing foliage categories (1-9) for top and middle
!           crown as average of all age classes from proportion of
!           retained biomass.
!
      MFT = 10.0 - IFIX((((PRBIO(IHOST,ICRC1,1) * 0.25) &
                    +(PRBIO(IHOST,ICRC1,2) * 0.25) &
                    +(PRBIO(IHOST,ICRC1,3) * 0.25) &
                    +(PRBIO(IHOST,ICRC1,4) * 0.25)) * 10.0) + 0.5)

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: MFT=',MFT,' PRBIO=', &
                    PRBIO(IHOST,ICRC1,1),PRBIO(IHOST,ICRC1,2), &
                    PRBIO(IHOST,ICRC1,3),PRBIO(IHOST,ICRC1,4)
!
!           Set index for middle crown third for current size class.
!
      IC = ICRC1 + 1
      MFM = 10.0 - IFIX((((PRBIO(IHOST,IC,1) * 0.25) &
                    +(PRBIO(IHOST,IC,2) * 0.25) &
                    +(PRBIO(IHOST,IC,3) * 0.25) &
                    +(PRBIO(IHOST,IC,4) * 0.25)) * 10.0) + 0.5)

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: MFM=',MFM,' PRBIO=', &
                    PRBIO(IHOST,IC,1),PRBIO(IHOST,IC,2), &
                    PRBIO(IHOST,IC,3),PRBIO(IHOST,IC,4)
!
!           The calculated variable in Michael Marsden's analysis
!           was percent topkill year prior. This routine does not
!           process on a annual basis, but once per cycle. So the
!           existing topkill is being vaguely assumed to have
!           occurred in the cycle prior if the model is active and
!           defoliation has taken place.
!
      IF (ITRUNC(I) .GT. 0) THEN
         PCTK = ITRUNC(I)/NORMHT(I)
      ELSE
         PCTK = 0.0
      ENDIF
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: CURRENT PCTK=',PCTK, &
                    ' LTOPK=', LTOPK
!           initialize topkill variables
      FTKILL=0.0
      PRTOPK=0.0
      HTGI=HTG(I)
!
!           Calculate and apply topkill if option is on. (actually,
!           it is initialized "on" with no user option to turn it off)
!

      IF (LTOPK) THEN

!              COMPUTE TOP KILL.  THE TOPKILL MODELS FOR LARGE TREES ARE BASED
!              ON IDEAS ONLY...NOT DATA.  N.CROOKSTON, TOMMY GREG, AND BRUCE
!              HOSTETLER CAME UP WITH THEM.  FOR SMALL TREES, SEE FERGUSON
!              1988 RESEARCH PAPER INT-393.
!

         AVDEF=BWERNP(AVYRMX(IHOST,ISZI),.06)
         IF (ISZI.EQ.1) THEN
            X=H
            IF (H.GT.10.) X=10.
            PRTOPK=1./(1.+EXP(-(-2.5817-.027635*FLOAT(ICR(I))+ &
                      3.709*SQRT(AVDEF)+.0488*X)))
         ELSE
            IF (IFIR(IHOST).EQ.0) THEN
               PRTOPK=.96*(1.-EXP(-((.65*(AVDEF+1.))**14.)))
            ELSE
               PRTOPK=.90*(1.-EXP(-((.60*(AVDEF+1.))**11.)))
            ENDIF
         ENDIF

         CALL BWERAN (X)
!              write (JOSTND,*) 'in bwepdm: damage random number: ',x  ! TEMP DEBUG

         IF (DEBUG) WRITE (JOSTND,*) 'IN BWEPDM: I=',I,' HT=',H, &
            ' HTG=',HTGI,' ICR=',ICR(I),' ISZI=',ISZI,' CRC1=', &
            ICRC1,' CRC2=',ICRC2,' AVDEF=',AVDEF,' PRTOPK=',PRTOPK, &
            ' RANX=',X
!
!              IF THE RANDOM NUMBER IS LESS THAN THE PRTOPK, SIMULATE THE
!              TOPKILLING.
!
         IF (X.LT.PRTOPK) THEN
!                 *************
!                 ************* begin original topkill proportion "PART"
!                 *************
!
!                 TOPKILL THESE TREES.  PROPORTION KILLED EQUATIONS OP.CITE.
!
!X                IF (ISZI.EQ.1) THEN
!X                   CALL BWERAN(X)
!X                   PART = .05684*((-ALOG(X))**1.7036)
!
!                    IF X<.0046, PART>1.0 SO BOUND PART TO .9 ... NOTE
!                    THAT .9<PART<1.0 OCCURS 0.6346 PERCENT OF THE TIME.
!                    THE BOUNDING OCCURS AFTER THE FOLLOWING MODELS, SO
!                    THEY ARE BOUNDED TOO.
!
!X                ELSE
!X                   IF (IFIR(IHOST).EQ.0) THEN
!X                     PART=.0960*(1.-EXP(-((.717*(AVDEF+1.))**7.92)))
!X                   ELSE
!X                     PART=.2545*(1.-EXP(-((.641*(AVDEF+1.))**6.60)))
!X                   ENDIF
!X                ENDIF
!                 *************
!                 ************* end original topkill proportion "PART"
!                 *************

!                 *************
!                 ************* begin new topkill proportion "PART"
!                 *************

!                 The function and coefficients for the proportion of
!                 topkill are the results of 2002 analysis by Michael
!                 Marsden of data in Bruce Hostetler's western spruce
!                 budworm study in the Blue Mountains.
!                 During the analysis it was necessary for Michael to
!                 bound the proportion between 0.01 and 0.99. That
!                 bounding was not carried forward here.
!
!           03/27/2012 Lance David
!           Added requirement that missing foliage categories for top
!           and middle must be GT 0.0 for topkill value to be calculated.
!
            IF (MFT .GT. 0.0 .AND. MFM .GT. 0.0) THEN

               PART = 1.0/(1.0 + EXP(TK0(IHOST) &
                     +(TK1(IHOST)*ELEV)+(TK2(IHOST)*ELEV*ELEV) &
                     +(TK3(IHOST)*IMIST(I))+(TK4(IHOST)*PCTK) &
                     +(TK5(IHOST)*MFT)+(TK6(IHOST)*MFM) &
                     +(TK7(IHOST)*MFM*PCTK)+(TK8(IHOST)*MFT*MFM)))
            ELSE
               PART = 0.0
            ENDIF

            IF (DEBUG) WRITE (JOSTND,*) 'IN BWEPDM: PART=',PART

!                 *************
!                 ************* end new topkill proportion "PART"
!                 *************

!                 Limiting the proportion of topkill to 0.9 is retained
!                 from the origingal topkill process.
!
            IF (PART.GT.0.9) PART=.9

!                 The application of the topkill to the tree record is retained
!                 from the original process coded by Nick Crookston.
!
            IF (PART.GT.0.0) THEN
               FTKILL=H*PART
!
!                    TOP KILL THE TREE. USE LOGIC LIKE THAT FOUND IN HTGSTP.
!
               TOPH=H-FTKILL
               ITRC2=IFIX(TOPH*100.+.5)
               ITRC1=ITRUNC(I)
               IF (ITRC1.GT.0) THEN
                  IF (ITRC1.GT.ITRC2) ITRUNC(I)=ITRC2
                  HT(I)=TOPH
               ELSE
                  D=DBH(I)*BARK
                  IF (H.GE. 25 .AND. D.GE.6.0) THEN
                     AF=CFV(I)/(.00545415*D*D*H)
                     AF=.44244-(.99167/AF)-(1.43237*ALOG(AF))+ &
                           (1.68581*SQRT(AF))-(.13611*AF*AF)
                     DTK=FTKILL/H
                     DTK=(DTK/((AF*DTK)+(1.-AF)))*D

                     IF (DTK.GT. 4.0) THEN
                        ITRUNC(I)=ITRC2
                        NORMHT(I)=IFIX(H*100.+.5)
                        IMC(I)=3
                     ELSE
                        IF (DTK.GT. 2.0 .AND. IMC(I).LT.2) &
                           IMC(I)=2
                     ENDIF
                  ENDIF
                  HT(I)=TOPH
                  IOD=ICR(I)
                  IF (IOD.GE.0) THEN
                     CN=(FLOAT(IOD)/100.*H)-H+TOPH
                     NEW=IFIX(CN/TOPH*100.+.5)
                     IF (NEW.LT.5) NEW=5
                     ICR(I)=-NEW
                  ENDIF
               ENDIF
            ENDIF
            HTG(I)=0.0

            IF (DEBUG) WRITE(JOSTND,*) &
               'IN BWEPDM: -TOPKILL- I=',I,' ISP=',ISPI,' H=',H, &
               ' HT=',HT(I),' NORMHT=',NORMHT(I), &
               ' ITRUNC=',ITRUNC(I),' DBH=',DBH(I),' BARK=',BARK, &
               ' PART=',PART,' FTKILL=',FTKILL

         ENDIF
      ENDIF
!
!           MODIFY THE DIAMETER GROWTH.
!           FIRST CONVERT DG TO INSIDE BARK DELTA DIAMETER SQUARED.
!
      DBHI=DBH(I)
      DGI=DG(I)
      DDS=DGI*(2.0*BARK*DBHI+DGI)
!
!           REDUCE DDS, THEN CONVERT BACK TO DG.
!           DO NOT ALLOW CALCULATED GROWTH VALUE < 0.
!
      XD=BWERNP(PEDDS(IHOST,ISZI),.03)
      DDS=DDS*XD
      DG(I)=SQRT((DBHI*BARK)**2+DDS)-BARK*DBHI
      IF (DG(I) .LT. 0.0) DG(I)=0.0

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: DG(OLD)=',DGI, &
                    '  DG(NEW)=',DG(I)

!
!           REDUCE HEIGHT GROWTH, UNLESS THE TREE HAS BEEN TOPKILLED AS
!           INDICATED BY HTG IF 0. DO NOT ALLOW CALCULATED GROWTH VALUE < 0.
!
      IF (HTG(I).GT.0.0) THEN
!
!              COMPUTE THE AMOUNT OF DEVIATION BASED ON THE PEDDS RANDOM
!              VARIABLE.
!
         IF (PEDDS(IHOST,ISZI).LT.0.99) THEN
            XH=PEHTG(IHOST,ISZI)+((1.-PEHTG(IHOST,ISZI))/ &
               (1.-PEDDS(IHOST,ISZI))*(XD-PEDDS(IHOST,ISZI)))
         ELSE
            XH=BWERNP(PEHTG(IHOST,ISZI),.03)
         ENDIF
         HTG(I)=HTG(I)*XH
         IF (HTG(I) .LT. 0.0) HTG(I)=0.0
      ENDIF

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: I=',I,' ISP=',ISP(I), &
         ' DBH,DG=',DBH(I),DG(I),' HT,HTG=',HT(I),HTG(I)
!*************
!************* beginning of original mortality process
!*************

!
!           APPLY MORTALITY. BWMXCD HOLDS MAXIMUM CUM DEFOLIATION PERCENT
!           FOR THE PERIOD WITHIN THE CYCLE THAT THE OUTBREAK WAS IN
!           PROGRESS.  USE CROOKSTON'S MORTALITY MODEL (A SET OF IDEAS
!           THAT WERE PARTLY DUE TO READING ALFARO'S CAN. J. FOR. RES.
!           (12:780-787) PAPER.  LET PR BE THE BUDWORM MODEL MORTALITY
!           RATE AND BASE BE THE BASE MODEL RATE.  RBAL IS THE BASAL AREA
!           IN LARGER TREES DIVIDED BY THE BAMAX.  A VALUE OF 1.0 SAYS
!           A LOT OF BASAL AREA 'ABOVE' IS ABOVE A TREE, AND THE STAND AT
!           MAX FOR THE SITE.  A VALUE OF 0.0 SAYS THAT THE TREE HAS NO
!           BA ABOVE IT (A DOMINATE TREE), REGARDLESS OF THE STANDS DENSITY.
!
!           COMPUTE A MODIFER OF THE BASE MORTALITY FUNCTION AS A FUNCTION
!           OF RBAL AND THE TREE DIAMETER.
!
!           If bamax has not been calculated by FVS, calculate value
!           from sdimax (lrd 01-sep-00).
!X
!X          IF (BAMAX .EQ. 0.0) THEN
!X             TBAMAX = SDIMAX * 0.5454154
!X          ELSE
!X             TBAMAX = BAMAX
!X          ENDIF

!X          RBAL = (1.0 - (PCT(I) / 100.0)) * BA / TBAMAX
!X          IF (RBAL .GT. 1.0) RBAL = 1.0
!X          D = DBH(I)
!X          IF (D .GT. 20.0) D = 20.0
!X          XM = 0.02 * D + (0.5 + 0.03 * D)*(1.0 - RBAL)
!X          IF (XM .GT. 1.0) XM = 1.0
!
!           COMPUTE THE CONSTANTS OF THE MORTALITY MODEL AS FUNCTIONS OF XM.
!
!X          C = XM * XM
!X          B = 0.00575 - 0.00645 * XM + 0.0031 * C
!X          C = 2.27 + 2.94 * XM + 1.84 * C
!
!           COMPUTE AN UPPER ASYMPOTE AS A FUNCTION OF DIAMETER.
!
!X          ASYM = .98 - 0.01 * D
!
!           COMPUTE PR...SCALED TO A 1-YEAR SURVIVAL RATE. THE RANDOM DEFOL
!           PROCESS IS INSERTED HERE.  SCALE THE MAX CUM DEF TO A ONE YEAR
!           DEFOLIATION PROPORTION.  GET THE RANDOM VARIABLE AND SCALE IT
!           BACK TO A 5 YEAR CUM.
!
!X          XD = BWERNP(BWMXCD(IHOST,ISZI) * 0.002, 0.03) * 500.0
!X          PR = ASYM * (1.0 - EXP(-((B * (XD + 1.0))**C)))
!X          PR = (1.0 - PR) ** 0.1
!*************
!************* end of original mortality process
!*************

!*************
!************* beginning of new mortality process
!*************

!           The mortality equation implemented here is from Mike Marsden's
!           analysis of Bruce Hostetler's WSB impact study data. The results
!           were reported in a memo dated 23-JUL-2003 and interpreted by
!           Kathy Sheehan in January, 2003 for use in the budworm model
!
!           Set up final computed variables used in mortality equation.
!           Topkill category - integer value 0 (no tk) to 10 (total tk)
!
      IF (ITRUNC(I) .EQ. 0.0 .OR. NORMHT(I) .EQ. 0.0) THEN
         KTK = 0.0
      ELSE
         KTK = 10.0 - IFIX( &
                 (REAL(ITRUNC(I))/REAL(NORMHT(I)) * 10.0) + 0.5)
      ENDIF

!
!           Compute probability of mortality.
!
!           03/27/2012 Lance David
!           Added requirement that missing foliage categories for top
!           and middle must be GT 0.0 for mortality value to be calculated.
!           Extremely high mortality is calculated when MFT and/or MFM is 0.0
!
      IF (MFT .GT. 0.0 .AND. MFM .GT. 0.0) THEN
         PR = 1.0/(1.0 + EXP( &
                B0(IHOST)+(B1(IHOST)*ELEV)+ &
               (B2(IHOST)*ELEV*ELEV)+(B3(IHOST)*PNTBA(ITRE(I)))+ &
               (B4(IHOST)*PNTHBA(ITRE(I)))+(B5(IHOST)*MFT)+ &
               (B6(IHOST)*KTK)+(B7(IHOST)*MFT*MFM)) )
      ELSE
         PR = 0.0
      ENDIF

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: KTK=',KTK, &
         ' ITRUNC=',ITRUNC(I),' NORMHT=',NORMHT(I),' MFT=',MFT, &
         ' MFM=',MFM,' PR=',PR
!*************
!************* end of new mortality process
!*************

!
!           BASE IS THE BASE MODEL MORTALITY RATE.  USE IT FOR THE PART OF
!           THE CYCLE WHEN BW WAS NOT ACTIVE.
!
! ?         This piece of code is part of the original mortality process
! ?         that was actually a survival rate. Since this is where accounting
! ?         for bw outbreak years within a FVS cycle takes place, it is a
! ?         necessary detail to keep. Question is, is this a correct way to
! ?         do it even if we convert the new mortality rate back to survival
! ?         rate for this little piece of code?
! ?         LRD 21-APR-03
!
      BASE = WK2(I) / PROB(I)

!           translate the mortailty rate to a survival rate for
!           following equations. 10/31/03
      PR = 1.0 - PR

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: BASE=',BASE, &
         ' NOBWYR=',NOBWYR,' IBWYR=',IBWYR,' FA=',FA,' PR=',PR

      IF (NOBWYR .GT. 0) THEN
         PR = (PR ** IBWYR)*((1.0 - BASE) ** FA)
      ELSE
         PR = PR ** IBWYR
      ENDIF
      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: PR=',PR

!*************
!           CONVERT SURVIVAL RATE TO MORTALITY RATE.
!
      PR=1.0-PR
!           The rate calculated by the new process is mortality,
!           not survival so no conversion is necessary. LRD 17-APR-03
!           This conversion reactivated 10/31/03. see conversion above
!*************

!
!           IF THE NUMBER DIEING IS LESS THAN THE BACK GROUND RATE FOR
!           THIS TREE RECORD, USE THE BACK GROUND RATE.
!
!***        FA=WK2(I)        *** disabled 03/27/12 LRD
!***                         *** FA is fraction of period, why set to mort prob???

      IF (BASE.GT.PR) THEN
         PR=BASE
      ELSE
         IF (PR.GT. 0.98) PR=0.98
         PR=PROB(I)*PR
         WK2(I)=PR
      ENDIF
!
!           WRITE DAMAGE OUTPUT, IF REQUESTED.
!
      IF (LBWPDM) WRITE (JOWSBW,46) I,NSP(ISPI,IMC(I)),DBH(I), &
         ORGHT,PROB(I),FA,WK2(I),DGI,DG(I),HTGI,HTG(I),PRTOPK, &
         FTKILL
46       FORMAT (I4,2X,A3,1X,2F7.2,3F8.3,5F7.3,F7.1)
50    CONTINUE
60 CONTINUE
70 CONTINUE
!
!     COMPUTE AVERAGE DEFOLIATION WEIGHTED BY TREES PER
!     HECTARE, AND PASS IT BACK TO THE EVENT MONITOR.
!
STREES=0.
SDEF=0.

DO 90 IHOST=1,6
   IF (IFHOST(IHOST).NE.1) GOTO 90
   DO 80 ITREE=1,3
      STREES=STREES+BWTPHA(IHOST,ITREE)
      SDEF=SDEF+CDEF(IHOST,ITREE)*BWTPHA(IHOST,ITREE)
80    CONTINUE
90 CONTINUE
IF (STREES.GT.0.0001) THEN
   SDEF=SDEF/STREES
ELSE
   SDEF=0.0
ENDIF

IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: CALL EVSET4; SDEF=',SDEF
CALL EVSET4 (5,SDEF)
!
!     RETRIEVE THE DAMAGE MODEL RANDOM NUMBER SEED FROM THE GENERATOR.
!
CALL BWERGT (DSEEDD)

IF (DEBUG) WRITE (JOSTND,*) 'EXIT BWEPDM: ICYC= ',ICYC

RETURN
END

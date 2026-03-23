SUBROUTINE BRECAN(IBRN,HITE,SSTAR,SSTHT,PROP,PIMX,EXPC)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRECAN calculates the number of new cankers expected for a
!  tree for one year.  When a new canker is placed into the
!  arrays which hold the information of cankers tracked by the
!  model, distance up (DUP array) is randomly placed within the
!  crown and distance out on branch (DOUT) is calulated based
!  on the distance up.
!----------------------------------------------------------------------
!
!  Parameters Passed:
!        IBRN - index for current tree
!        HITE  - total tree height at end of cycle in meters for
!                current tree. (current height  +  height growth)
!        RINDX - rust index value for current tree
!        SSTAR - summed target area for current tree
!        SSTHT - this years height in meters for current tree
!                (current height  +  proportion of height growth)
!        PROP  - proportion of full cycle represented as of current year
!        PIMX  - proportion trees infected maximum for the stand
!     Returned:
!        EXPC  - expected number of cankers for current tree (record)
!                this year.
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  19-MAY-1999 Lance David
!     Added debug code.
!  15-MAR-2001 Lance R. David (FHTET)
!     Updated DFACT variable to array by species and stock type.
!  22-MAR-2001 Lance R. David (FHTET)
!     Added PIMX parameter and control for assigning escape status
!     code 9 to trees. Once a tree is tagged as escape, it will never
!     become infected.
!  03-MAY-2001 Lance R. David (FHTET)
!     Added species dimension to variables (arrays).
!  10-MAY-2006 Lance R. David (FHTET)
!     Added debug.
!  14-MAY-2014 Lance R. David (FMSC)
!     Removed RINDX parmeter (third in list) because it is in common.
!     RINDX changed to RI(IBRN) in processing.
!
!**********************************************************************
!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.
INTEGER IBRN,NUMTIM,I3,ICANB,J
REAL    HITE,SSTAR,SSTHT,PROP,PIMX,EXPC,RITEM,TNEWC, &
           CRLEN,PLI,TOUT,TUP,PLETH,XBRAN,PIEXP
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRECAN',6,ICYC)
IF(DEBUG) WRITE(JOSTND,22) ICYC, &
    IBRN,ISP(IBRN),HITE,RI(IBRN),SSTAR,SSTHT,PROP, &
    PIMX,IBRSTAT(IBRN),ILCAN(IBRN),ITCAN(IBRN)
22 FORMAT('Entering subroutine BRECAN: cycle = ',I2,/, &
    'IBRN=',I4,' ISP=',I2,' HITE=',F10.7,' RI=',F10.7, &
    ' SSTAR=',F10.4,' SSTHT=',F10.7,' PROP=',F10.7, &
    ' PIMX=',F10.7,' IBRSTAT=',I2,' ILCAN=',I2,' ITCAN=',I3)

EXPC = 0.0

!.... set species code for index.
I3 = BRSPM(ISP(IBRN))
IF(DEBUG) WRITE(JOSTND,*) 'I3=',I3,' PROB=',PROB(IBRN), &
    ' PITCA=',PITCA(I3),' TRETN=',TRETN(I3),' THPROB=',THPROB(I3)

IF(IBRSTAT(IBRN).EQ.0) THEN
!....    If the current tree is a clean tree and stand infection max has
!....    not been met, need to update proportion of infected trees.
!....    Set tree status code so that this happens only once.
   IF(PITCA(I3) .LT. PIMX) THEN
      IBRSTAT(IBRN) = 1
      TRETN(I3) = TRETN(I3) + PROB(IBRN)
      PITCA(I3) = TRETN(I3)/THPROB(I3)
      IF(DEBUG) WRITE(JOSTND,*) ' NEW TREE - ',IDTREE(IBRN), &
         'PITCA=',PITCA(I3),'TRETN=',TRETN(I3),'PROB=',PROB(IBRN), &
         'THPROB=',THPROB
   ELSE IF(PITCA(I3) .GE. PIMX) THEN
!....       Infection level has been met.
!....       Reserve this escape tree with IBRSTAT code 9.
!....       During previous Years of cycle, expected cankers may
!....       have accumulated for this tree. Zero them out.
      IBRSTAT(IBRN) = 9
      ESTCAN(IBRN) = 0.0
      ITCAN(IBRN) = 0
      IF(DEBUG) WRITE(JOSTND,*) ' ESCAPE TREE - ', &
                  IDTREE(IBRN),ITCAN(IBRN)
      GO TO 35
   ENDIF
ENDIF

!.... Calculate temporary rust index variable based on height at the
!.... end of the cycle.

IF(HITE.GT.25.0) THEN
   RITEM=RI(IBRN)*0.1
ELSE IF(HITE.GT.15.0) THEN
   RITEM=RI(IBRN)*(1.0-(0.09*(HITE-15.0)))
ELSE
   RITEM=RI(IBRN)
ENDIF
IF(DEBUG) WRITE(JOSTND,*) 'RITEM=',RITEM,' RI=',RI(IBRN)

!.... Calculate number of expected cankers for current year
!.... (total and "potentially lethal") based on tree's target area.

TNEWC=RITEM*SSTAR
EXPC=TNEWC
IF(DEBUG) WRITE(JOSTND,*) 'EXPC=',EXPC

!.... Calculate crown length in centimeters for this year and the
!.... probability of infection for the tree.

CRLEN=(SSTHT*100.0)-BRHTBC(IBRN)
PIEXP=EXP(-(TNEWC/(1+TNEWC*DFACT(I3,ISTOTY(IBRN)))))
PLI=1.0-PIEXP
NUMTIM=INT(TNEWC)+1
IF(DEBUG) WRITE(JOSTND,*) 'PLI=',PLI,' NUMTIM=',NUMTIM

!.... Loop through for number of expected cankers.

DO 30 J=1,NUMTIM

!....    If probability of infection >= random number then add canker.

   CALL BRANN(XBRAN)
   IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN

   IF(PLI.GE.XBRAN) THEN

!....       Add a canker to total number of cankers for this tree.
!....       Running total number of cankers is kept but only "lethal"
!....       ones are processed during cycling.

      ITCAN(IBRN)=ITCAN(IBRN)+1

!....       Generate up and out positions for added canker.

      CALL BRANN(XBRAN)
      TUP=(100*SSTHT-CRLEN)+CRLEN*XBRAN
      TOUT=(35*SQRT(SSTHT)*(100*SSTHT-TUP))/CRLEN

      IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN, &
         ' TUP=',TUP,' TOUT=',TOUT

!....       If out < 50 then canker possibly lethal (different
!....       probability than if the canker is farther out than 50 cm).

      IF(TOUT.LT.50.0) THEN
         PLETH=0.97-0.0158*TOUT
      ELSE
         PLETH=35.4/TOUT**(1+(0.35*TOUT/50))
      ENDIF
      IF(PLETH.LT.0.0) PLETH=0.0

      IF(DEBUG) WRITE(JOSTND,*) 'PLETH=',PLETH

!....       Call random number generator. If PLETH >= number then create
!....       a bole canker, otherwise create a branch canker. If array is
!....       full then skip adding a canker altogether.

      CALL BRANN(XBRAN)
      IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN

      IF(ILCAN(IBRN).LT.10) THEN
         ILCAN(IBRN)=ILCAN(IBRN)+1
         ICANB=ILCAN(IBRN)
         IF(PLETH.GE.XBRAN) THEN
            DOUT(ICANB,IBRN)=0.0
         ELSE
            DOUT(ICANB,IBRN)=TOUT
         ENDIF
         DUP(ICANB,IBRN)=TUP
         GIRDL(ICANB,IBRN)=0.0
         ISTCAN(ICANB,IBRN)=0

         IF(DEBUG) WRITE(JOSTND,*) &
            ' DOUT=',DOUT(ICANB,IBRN),' DUP=',DUP(ICANB,IBRN)
     ENDIF
   ENDIF
30 CONTINUE
35 CONTINUE

!.... Common return.
IF(DEBUG) THEN
WRITE (JOSTND,38) IBRN,HITE,RI(IBRN),SSTAR,SSTHT,PROP,EXPC
38    FORMAT(' IBRN=',I4,' HITE=',F5.1,' RI=',F7.4, &
      ' SSTAR=',F7.1,' SSTHT=',F5.1,' PROP=',F4.1,' EXPC=',F3.1)
ENDIF

IF(DEBUG) WRITE(JOSTND,40) ICYC
40 FORMAT('Leaving subroutine BRECAN: cycle = ',I2)
RETURN
END

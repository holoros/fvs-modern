SUBROUTINE BMKILL
!----------
! WWPB $Id$
!----------
!
!     CONVERT THE MORTALITY ESTIMATES PRODUCED BY THE PINE
!     BEETLE MODEL INTO MODIFIED PROGNOSIS MODEL RATES.
!
!     AN EXTENSION OF THE PARALLEL PROCESSING EXTENSION (PROGNOSIS)
!     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--MAY 1987
!
!     CALLED FROM:  PPMAIN
!
! Revision History:
!   08/05/98 Robert N. Havis (FHTET)
!      Modified to track Salvage removals in PPE (RNH)
!   07/19/05 Lance R. David (FHTET)
!      Added call to SVMORT and SVOUT for generation of Stand Visualization
!      images.
!   09/22/05 Andrew McMahan (FHTET)
!      Changed variable name used in OCVREM calculation at end of routine.
!      This is in parallel with new output generation, which "stole" variable
!      "VOLREM" (heretofore used herein).  We need volume removed over the
!      course of the cycle, now kept in CVOLREM.
!   09/30/05 Lance R. David (FHTET)
!      Added BTKL array to hold host mortality for call to svmort.
!   11/08/05 Lance R. David (FHTET)
!      Changed how mortality is submitted to SVS when WWPB mortality does not
!      exceed FVS base mortality, enabling WWPB to get partial credit in SVS
!      images. Added new local variable BASMRT for this process.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'
INCLUDE 'PPCNTL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'OUTCOM.f90'

INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BMCOM.f90'
!
!OMMONS
!
LOGICAL LX
INTEGER IBMSTD
INTEGER K
INTEGER ISPC
INTEGER UPAGE(MXDWAG)
INTEGER IAG, ISC, IPC, JSC
!  ** DECLARED DIF AS INTEGER (MJOMarch98), parameter array NRSC (RNHAug98)
INTEGER DIF
REAL    MFAST, MSLOW, MPRG, MBTL, MSUM
REAL    MEX, SMX, DMX
REAL    X, BTKL(MAXTRE), BASMRT

DATA UPAGE /0,1,5,10,300/
!
!     IF BMSTND IS ZERO, NO DISPERSAL WAS RUN, BRANCH TO EXIT.
!     Change test to use IBMYR1 instead of BMSTND.  ajm dec 2005
!
 IF(IBMYR1 .EQ. 0) GO TO 150
!      IF (BMSTND .LE. 0) GOTO 150
!
!     FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP
!
IBMSTD= 0
CALL OPBISR (BMSTND,BMSDIX,ISTND,IBMSTD)
!
!     ajm i don't see how, if we got to here, a stand could NOT be in the dispersal group.
!     all stands in ppe run are in dispersal group!.
!     unless it is marked nonstocked, in which case this call will not help us.
!

!     IF IBMSTD IS ZERO, THEN BRANCH TO EXIT.
!     OR, IF IBMSTD IS NON-STOCKED! (NEW CONDITION ADDED 12/05.  AJM)
!
!      IF (IBMSTD .LE. 0) GOTO 150
IF (IBMSTD .LE. 0 .OR. (.NOT.STOCK(IBMSTD))) GOTO 150

!     For Stand Visualization process, initialize array that will hold
!     host tree mortaility.
!
DO I = 1, MAXTREE
   BTKL(I)= 0.0
ENDDO

DO 20 ISPC= 1,MAXSP
  IF (ISCT(ISPC,1) .EQ. 0) GOTO 20

!       Determine whether tree species is host or non-host so mortality will be applied
!       appropriately.

  LX= .FALSE.
  IF (  (((PBSPEC .EQ. 1) .OR. (PBSPEC .EQ. 4)) &
            .AND. (HSPEC(1,ISPC) .EQ. 1)) &
      .OR. (((PBSPEC .EQ. 2) .OR. (PBSPEC .EQ. 4)) &
            .AND. (HSPEC(2,ISPC) .EQ. 1)) &
      .OR. ((PBSPEC .EQ. 2) .AND. (HSPEC(2,ISPC) .EQ. 1)) &
      .OR. ((PBSPEC .EQ. 3) .AND. (HSPEC(3,ISPC) .EQ. 1)) ) &
           LX= .TRUE.


!     Fetch original TPA for each size class, and compute mortality for size class based on
!     the DEAD/LIVE ratio. Note that while not likely, MSUM could be greater than PROB().

  DO 10 II=ISCT(ISPC,1),ISCT(ISPC,2)

    I= IND1(II)
!
    CALL BMDBHC(DBH(I),K)

    IF (LX) THEN
       IF (OTPA(IBMSTD,K,1) .GT. 1.0E-9) THEN
          X= 1.0 / OTPA(IBMSTD,K,1)
       ELSE
          X= 1.0E9
       ENDIF

       MFAST= AMIN1(TPBK(IBMSTD,K,1,1) * X, 1.0) * PROB(I)
       MSLOW= AMIN1(TPBK(IBMSTD,K,1,2) * X, 1.0) * PROB(I)
       MBTL=  AMIN1(TPBK(IBMSTD,K,1,3) * X, 1.0) * PROB(I)

    ELSE
       IF (OTPA(IBMSTD,K,2) .GT. 1.0E-9) THEN
          X= 1.0 / OTPA(IBMSTD,K,2)
       ELSE
          X= 1.0E9
       ENDIF

       MFAST= AMIN1(TPBK(IBMSTD,K,2,1) * X, 1.0) * PROB(I)
       MSLOW= AMIN1(TPBK(IBMSTD,K,2,2) * X, 1.0) * PROB(I)
       MBTL=  AMIN1(TPBK(IBMSTD,K,2,3) * X, 1.0) * PROB(I)
    ENDIF

    MSUM= MFAST + MSLOW + MBTL
    MPRG= WK2(I)
    BASMRT = WK2(I)

!         If the total model predicted mortality from all sources is less
!         than Prognosis predicts, nothing is done and this IF/ENDIF is
!         bypassed. If model-predicted mortality is greater, reconciliation
!         is done.

    IF (MSUM .GT. MPRG) THEN

!           IF fast and beetle mortality are less than Prognosis predicts,
!           adjust slow mortality so that the total agrees with Prognosis.
!           ELSE, adjust Prognosis mortality with fast and beetle
!           mortality, and ignore slow altogether.

      IF ((MFAST + MBTL) .LT. MPRG) THEN
        MSLOW= MSLOW - (MSUM - MPRG)
      ELSE
        MPRG= MFAST + MBTL
      ENDIF

    ELSE

!           If Prognosis predicts higher mortality then the extra mortality needs to
!           be added to the dead tree lists. Assume that part all goes into standing
!           dead wood and that it is evenly divided between all age classes less than
!           the master cycle length old.

!         MEX:    The amount of mortality predicted by prognosis that was not
!                 predicted by the beetle model (in total CU FT)

!           DIF = MIY(ICYC) - MIY(ICYC-1)
      DIF=  IBMMRT
      MEX = (MPRG - MSUM) * CFV(I)
      SMX = MEX / DIF

      IAG = 1
      ISC = L2D(K) + 1
      JSC = MIN0(ISC,2)
      IPC = ISPFLL(ISPC)

      DO 200 J = 1, DIF
        IF (J .GT. UPAGE(IAG)) IAG = IAG + 1

        SMX = SMX * SDECRT(JSC)

! *********** Bound DMX to keep from getting too small (MJO March 1998)
        IF (DMX .LT. 1.0E-6) DMX = 1.0E-6

        DMX = DMX * DDECRT(JSC)
        DMX = DMX + SMX * FALLRT(IPC) * V2T
        SMX = SMX * (1 - FALLRT(IPC))

        SDWP(IBMSTD,IPC,ISC,IAG) = SDWP(IBMSTD,IPC,ISC,IAG) + SMX
        DDWP(IBMSTD,JSC,IAG) = DDWP(IBMSTD,JSC,IAG) + DMX
200       CONTINUE
    ENDIF

!         Transfer the (possibly) adjusted values back to their usual
!         places, and let any output printing use those values. Note that
!         only slow mortality and Prognosis mortality are adjusted.

    WK2(I)= MPRG

    IF ((PROB(I) - WK2(I)) .LT. 1.0E-6) &
            WK2(I)= PROB(I) - 1.0E-6
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     The following updates the WK2 array to account for Sanitaiton
!     removals (RNH Aug98)
!
!     Was Sanitization ndone this cycle?
!
IF (LOKS) THEN
!     Set LOKS1 to FALSE to prevent cycles from being updated when
!     no Sanitization was done
!
LOKS1 = .FALSE.
!
!     Is the current tree a host tree?
!
 IF (LX) THEN
!
!     Check to see if current stand is subject to sanitization
!
DO 300 ISTD1= 1,BMSTND
!
  IF(IBMSTD .EQ. MYLSTS(ISTD1)) THEN
!
 DO 250 ISIZ1= MINSCS, MAXSCS
!
!     Check to see if tree is in size class that was samitized
!
   IF((K .GE. MINSCS) .AND. (K .LE. MAXSCS)) THEN
!
!     Adjust WK2 array
!
!      IF ((ATREEI(IBMSTD,ISIZ1) - AREMS(IBMSTD,ISIZ1)) .LE. 1.E-6) THEN
!      WK2(I) = PROB(I) - 1.0E-6
!      GO TO 400
!      ENDIF
!
!       WK2(I)= WK2(I) + AREMS(IBMSTD,ISIZ1)*PROB(I)
!
!
!      WK2(I)= 1./((ATREEI(IBMSTD,ISIZ1) - AREMS(IBMSTD,ISIZ1))/
!     1        ATREEI(IBMSTD,ISIZ1))*PROB(I)
!
IF (ATREEI(IBMSTD,K) .LE. 1.0E-6) GO TO 400
!      IF (NRSC(K) .LE. 0) GO TO 400
!
!      WK2(I)= WK2(I)+AREMS(IBMSTD,K)/ATREEI(IBMSTD,K)*PROB(I)
!     1        /NRSC(K)*3.
!
WK2(I)= WK2(I)+AREMS(IBMSTD,K)/ATREEI(IBMSTD,K)*PROB(I)
!
!      write(29,*) ' bmstnd= ', bmstnd,' k=' ,k,' istd1= ', istd1,
!     1  ' NRSC(K)= ',NRSC(K),' K= ',K,' MPRG= ',MPRG
!      write(29,*) ' wk2= ', wk2(i),' arems= ', arems(ibmstd,isiz1),
!     1  ' atreei= ', atreei(ibmstd,isiz1)
!      write(29,*) ' I= ',I,' prob(i)= ', prob(i)
!
!     Check that removals pls mortality is less than total number of trees
!
IF((PROB(I)-WK2(I)) .LE. 1.0E-5) THEN
WK2(I) = PROB(I) - 1.0E-6
GO TO 400
ENDIF
!
!       WK2(k) has been adjusted so branch out of logical structure
!
  GO TO 400
!
   ENDIF
250  CONTINUE
  ENDIF
300 CONTINUE
400 CONTINUE
 ENDIF
ENDIF

!
!       The SVS mortality processing for this tree record.
!       If it is a host species, retain value for svmort (SVS) process.
!       When the overall WWPB mortality is greater than FVS base mortality,
!       WWPB gets credit for all mortality. If FVS base mortality is greater,
!       WWPB gets credit for the portion of total WWPB mortality specifically
!       attributed to beetles
!
  IF (LX) THEN
     IF (BASMRT .LE. WK2(I)) THEN
        BTKL(I) = WK2(I)
     ELSE
        BTKL(I) = MBTL
     ENDIF
  ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
10   CONTINUE
20 CONTINUE

!     DEBUG STATEMENT
!     WRITE (*,*) ' IN BMKILL: CALL SVMORT - 2  YEAR=',MIY(ICYC)

!     Stand Visualization process. Call svmort to add incremental mortality
!     attributed to WWPB.
CALL SVMORT (2, BTKL, MIY(ICYC))

!     Attempt to put our volume removed information into the total volume
!     removed array from prognosis. I think OCVREM(1-6) are percentile classes.
!     Also note that since we're only putting our information in one place
!     (total vol) that if there were other volumes from normal thinning, this
!     total vol would be greater than would be expected from the printed
!     merchatable vols.

OCVREM(7) = OCVREM(7) + CVOLREM(IBMSTD,1) + CVOLREM(IBMSTD,2) !USE NEW CYCLE-ACCUMULATOR VARS AJM 9/05

150 CONTINUE
IF (LBMDEB) WRITE (JBMBPR,160) ISTND,BMSTND,IBMSTD
160 FORMAT (/' IN BMKILL: ISTND=',I4,' BMSTND=',I4,' IBMSTD=',I4)

RETURN
END

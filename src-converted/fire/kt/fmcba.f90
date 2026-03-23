SUBROUTINE FMCBA (IYR,ISWTCH)
IMPLICIT NONE
!----------
! FIRE-KT $Id$
!----------
!     SINGLE-STAND VERSION
!     CALLED FROM: FMMAIN
!
!  Purpose:
!     Find the dominant species (by basal area). Set the initial live
!     and dead fuel values as well. The dead fuels are only initialized
!     in the first year, but COVTYP and the live fuels must be done
!     each year.
!----------------------------------------------------------------------
!
!  Local variable definitions:
!     BAMOST:  The highest basal area in a single species
!     CAREA:   The area covered by the crown at its widest point (sqft)
!     COVINI:  The seral cover type to be used for initiating fuels in
!              bare stands
!     CRL:     Crown length
!     CWIDTH:  The maximum width of the crowns (ft)
!     FUINIE:  The initial fuel loadings for established stands (from JBrown)
!     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
!     FULIVE:  The herb/shrub/regen for established stands (from JBrown)
!     FULIVI:  The herb/shrub/regen for initiating stands (from JBrown)
!     ISWTCH:  =1 if called by SVSTART
!              =0 if called by any other subroutine (FMMAIN, FMPPHV)
!     TOTBA:   The total basal area in the stand (used in the fuels calcs)
!     TOTCRA:  The sum of the area of the crowns, per acre (sqft)
!
!  Common block variables and parameters:
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'

!.... Variable declarations.

REAL TBA(MAXSP)
REAL BAMOST, BA1, TOTCRA, CWIDTH
INTEGER COVINI(30)
REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
REAL PRMS(12), FOTOVALS(9)
LOGICAL DEBUG

INTEGER NDRY
PARAMETER (NDRY=16)

INTEGER MAPDRY(2,NDRY),IHB

INTEGER MYACT(3)

INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
!
!     CLASSIFICATION OF HABITATS TO BE DRY GRASSY (IDRYB=1),
!     DRY SHRUBBY (IDRYB=2), OR OTHER (IDRYB=0)
!     THE FOLLOWING TYPE CAN BE ADDED ONCE WE KNOW THEIR CODES:
!     ?,1  !  PIPO/ARUV
!     ?,2  !  PIPO/AMAL
!     ?,2  !  PIPO/PYMA
!     ?,2  !  PSME/SYOC
!
DATA MAPDRY/ &
        130,1, &  ! PIPO/AGSP
        140,1, &  ! PIPO/FEID
        210,1, &  ! PSME/AGSP
        220,1, &  ! PSME/FEID
        230,1, &  ! PSME/FESC
        161,2, &  ! PIPO/PUTR
        170,2, &  ! PIPO/SYAL
        171,2, &  ! PIPO/SYAL-SYAL
        172,2, &  ! PIPO/SYAL-BERE
        180,2, &  ! PIPO/PRVI
        181,2, &  ! PIPO/PRVI-PRVI
        182,2, &  ! PIPO/PRVI-SHCA
        310,2, &  ! PSME/SYAL
        311,2, &  ! PSME/SYAL-AGSP
        312,2, &  ! PSME/SYAL-CARU
        313,2/     ! PSME/SYAL-SYAL
!
!                  herbs, shrubs
DATA FULIVE /0.15,  0.1, &
                0.2,   0.2, &
                0.2,   0.2, &
                0.15,  0.1, &
                0.2,   0.2, &
                0.2,   0.2, &
                0.2,   0.1, &
                0.15,  0.2, &
                0.15,  0.2, &
                0.2,   0.25, &
                0.15,  0.2/

DATA FULIVI /0.30, 2.0, &
                0.4,  2.0, &
                0.4,  2.0, &
                0.30, 2.0, &
                0.4,  2.0, &
                0.4,  2.0, &
                0.4,  1.0, &
                0.30, 2.0, &
                0.30, 2.0, &
                0.25, 0.10, &
                0.30, 2.0/

!                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
DATA FUINIE /1.0, 1.0, 1.6, 10.0,10.0,10.0, 0.0,0.0,0.0, 0.8,30.0, &
                0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0, &
                0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0, &
                0.7 ,0.7, 3.0,  7.0, 7.0, 0.0, 0.0,0.0,0.0, 0.6,25.0, &
                2.2, 2.2, 5.2, 15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0, &
                2.2, 2.2, 5.2, 15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0, &
                0.9 ,0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0, 0.6,15.0, &
                1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0, &
                1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0, &
                0.7 ,0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0, 1.4, 5.0, &
                1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0/

!                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
DATA FUINII /0.6, 0.6, 0.8,  6.0, 6.0, 6.0, 0.0,0.0,0.0, 0.4,12.0, &
                0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0, &
                0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0, &
                0.5, 0.5, 2.0,  2.8, 2.8, 0.0, 0.0,0.0,0.0, 0.3,12.0, &
                1.6, 1.6, 3.6,  6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0, &
                1.6, 1.6, 3.6,  6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0, &
                0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0, 0.3, 7.0, &
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0, &
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0, &
                0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0, 0.5, 0.8, &
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0/
DATA COVINI /10,10, 3, 3, 5, 4, 3, 3, 3, 8, &
                 8, 4, 4, 6, 6, 6, 5, 9, 9, 9, &
                 9, 9, 9, 9, 9, 9, 9, 9, 9, 9/

DATA MYACT / 2521, 2548, 2553 /
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC
7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
!
!.... BEGIN ROUTINE.
!
!     ZERO OUT THE CUMMULATIVE VARIABLES
!
COVTYP = 0
PERCOV = 0.0
BIGDBH = 0.0
TOTBA  = 0.0
!
!     LOOP THROUGH THE TREE LIST
!
IF (ITRN .GT. 0) THEN
!
!       ZERO OUT THE CUMMULATIVE VARIABLES
!
  BAMOST = 0.0
  TOTCRA = 0.0

  DO KSP=1,MAXSP
    TBA(KSP) = 0.0
  ENDDO

  DO I=1,ITRN
    IF (FMPROB(I) .GT. 0.0) THEN

      KSP = ISP(I)

      BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
      TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)

      IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
!
!           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
!           ENCOMPASSED BY ALL TREES
!
      CWIDTH=CRWDTH(I)
      CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
      TOTCRA = TOTCRA + CAREA*FMPROB(I)
    ENDIF
!
!         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
!
    CURKIL(I) = 0.0
  ENDDO
!
!        DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
!        -> THAT WILL BE THE COVER TYPE
!
  DO KSP=1,MAXSP
    IF (TBA(KSP) .GT. BAMOST) THEN
      BAMOST = TBA(KSP)
      COVTYP = KSP
    ENDIF
    TOTBA = TOTBA + TBA(KSP)
  ENDDO
!
!       USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
!       OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
!       CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:
!
!       PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))
!
  PERCOV = 1.0 - EXP(-TOTCRA/43560.)
  PERCOV = PERCOV * 100.0

ENDIF
!
!     IF THERE ARE NO TREES (COVTYP=0) THEN USE THE HABITAT TYPE
!     (INDEX=ITYPE) TO DETERMINE A SURROGATE
!
IF (COVTYP .EQ. 0) COVTYP = COVINI(ITYPE)
!
!     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
!     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
!
XCOV(1)=10.
XCOV(2)=60.
DO I=1,2
  YLOAD(1)=FULIVI(I,COVTYP)
  YLOAD(2)=FULIVE(I,COVTYP)
  FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
ENDDO

IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=', &
          2F6.3)
!
!     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
!
IF (IYR .EQ. IY(1)) THEN

!sng      IF (IYR .EQ. IY(1)) THEN
!ppe      IF (IYR .EQ. MIY(1)) THEN

!        ASSUME THE FUELS ARE UNPILED AND HARD. [**THIS WILL CHANGE LATER]
!
!        LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
!        STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
!
   XCOV(1) = 10.0
   XCOV(2) = 60.0
   DO ISZ = 1,MXFLCL
     YLOAD(1)=FUINII(ISZ,COVTYP)
     YLOAD(2)=FUINIE(ISZ,COVTYP)
     STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
     STFUEL(ISZ,1) = 0
   ENDDO

!       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

  CALL OPFIND(1,MYACT(2),J)
  IF (J .GT. 0) THEN
    CALL OPGET(J,2,JYR,IACTK,NPRM,PRMS)
    IF ((PRMS(1) .GE. 0) .AND. (PRMS(2) .GE. 0)) THEN
      CALL FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL, &
                         FOTOVALS)
      DO I = 1, MXFLCL
        IF (FOTOVAL(I) .GE. 0) STFUEL(I,2) = FOTOVAL(I)
        IF (I .LE. 9) STFUEL(I,1) = FOTOVALS(I)
      ENDDO

!           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
!           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
!           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

      IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)

    ELSE
      WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ', &
         'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
         'REQUIRED.',/1X)")
      CALL RCDSET (2,.TRUE.)
    ENDIF
  ENDIF

!        CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
!       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

  CALL OPFIND(1,MYACT(1),J)
  IF (J .GT. 0) THEN
    CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
    IF (PRMS(2) .GE. 0) STFUEL(3,2) = PRMS(2)
    IF (PRMS(3) .GE. 0) STFUEL(4,2) = PRMS(3)
    IF (PRMS(4) .GE. 0) STFUEL(5,2) = PRMS(4)
    IF (PRMS(5) .GE. 0) STFUEL(6,2) = PRMS(5)
    IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
    IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
    IF (PRMS(8) .GE. 0) STFUEL(1,2) = PRMS(8)
    IF (PRMS(9) .GE. 0) STFUEL(2,2) = PRMS(9)
    IF (PRMS(1) .GE. 0) THEN
      IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .LT. 0)) THEN
        STFUEL(1,2) = PRMS(1) * 0.5
        STFUEL(2,2) = PRMS(1) * 0.5
      ENDIF
      IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .GE. 0)) THEN
        STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
      ENDIF
      IF ((PRMS(8) .GE. 0) .AND. (PRMS(9) .LT. 0)) THEN
        STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
      ENDIF
    ENDIF
    IF (PRMS(10) .GE. 0) STFUEL(7,2) = PRMS(10)
    IF (PRMS(11) .GE. 0) STFUEL(8,2) = PRMS(11)
    IF (PRMS(12) .GE. 0) STFUEL(9,2) = PRMS(12)

!          DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
!          NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

     IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

  ENDIF

  CALL OPFIND(1,MYACT(3),J)
  IF (J .GT. 0) THEN
    CALL OPGET(J,9,JYR,IACTK,NPRM,PRMS)
    IF (PRMS(1) .GE. 0) STFUEL(1,1) = PRMS(1)
    IF (PRMS(2) .GE. 0) STFUEL(2,1) = PRMS(2)
    IF (PRMS(3) .GE. 0) STFUEL(3,1) = PRMS(3)
    IF (PRMS(4) .GE. 0) STFUEL(4,1) = PRMS(4)
    IF (PRMS(5) .GE. 0) STFUEL(5,1) = PRMS(5)
    IF (PRMS(6) .GE. 0) STFUEL(6,1) = PRMS(6)
    IF (PRMS(7) .GE. 0) STFUEL(7,1) = PRMS(7)
    IF (PRMS(8) .GE. 0) STFUEL(8,1) = PRMS(8)
    IF (PRMS(9) .GE. 0) STFUEL(9,1) = PRMS(9)

!         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
!         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

    IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

  ENDIF

!        DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
!        OF BASAL AREA IN THE STAND.

  DO ISZ = 1,MXFLCL
    IF (TOTBA .GT. 0.0) THEN
      DO KSP = 1,MAXSP
        IF (TBA(KSP) .GT. 0.0) THEN
          DO J = 1,2
            PRCL = TBA(KSP) / TOTBA
            IDC = DKRCLS(KSP)
            ADD = PRCL * STFUEL(ISZ,J)
            CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
          ENDDO
        ENDIF
      ENDDO
    ELSE
      IDC = DKRCLS(COVTYP)
      DO J = 1,2
        CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
      ENDDO
    ENDIF
  ENDDO
!
!       SET IDRYB FOR THIS HABITAT TYPE; TO BE USED IN **FMCFMD**
!       NOTE IDRYB IS USED IN OTHER VARIANTS TO SIGNAL START/END YEARS
!       FOR DROUGHT; UNUSED IN THIS VARIANT, SO REUSED TO HOLD THIS
!       CATEGORY
!
  IDRYB = 0
  DO I = 1,NDRY
    IF (MAPDRY(1,I) .EQ. FMKOD) THEN
      IDRYB = MAPDRY(2,I)
      EXIT
    ENDIF
  ENDDO

ENDIF

!     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODE 621
IF (IYR.EQ.IY(1) .AND. KODFOR.EQ.621) ICHABT = 2

RETURN
!
!     HOOK TO ALLOW THE HABITAT CATEGORY TO BE READ BY **FMCFMD*
!
ENTRY NIFMHAB(IHB)
IHB = IDRYB
RETURN

ENTRY SNGCOE

!     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
!     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

RETURN
END

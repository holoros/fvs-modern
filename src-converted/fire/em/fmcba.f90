SUBROUTINE FMCBA (IYR,ISWTCH)
IMPLICIT NONE
!----------
! FIRE-EM $Id$
!----------
!     SINGLE-STAND VERSION
!     CALLED FROM: FMMAIN
!
!  Purpose:
!     Find the dominant species (by basal area). Set the initial live
!     and dead fuel values as well. The dead fuels are only initialized
!     in the first year, but COVTYP and the live fuels must be done
!     each year.
!
!  Local variable definitions:
!     BAMOST:  The highest basal area in a single species
!     CAREA:   The area covered by the crown at its widest point (sqft)
!     CRL:     Crown length
!     CWIDTH:  The maximum width of the crowns (ft)
!     FUINIE:  The initial fuel loadings for established stands (from JBrown)
!     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
!     FULIVE:  The herb/shrub for established stands (from JBrown)
!     FULIVI:  The herb/shrub for initiating stands (from JBrown)
!     ISWTCH:  =1 if called by SVSTART
!              =0 if called by any other subroutine (FMMAIN, FMPPHV)
!     TOTBA:   The total basal area in the stand (used in the fuels calcs)
!     TOTCRA:  The sum of the area of the crowns, per acre (sqft)
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'EMCOM.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
!OMMONS
!----------
!  LOCAL VARIABLES DECLARATIONS
!----------
INTEGER MD1(122), MD2(122), M1, M2
REAL    TBA(MAXSP)
REAL    BAMOST, BA1, TOTCRA, CWIDTH
REAL    FULIVE(2,MAXSP), FULIVI(2,MAXSP)
REAL    FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
REAL    STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
REAL    PRMS(12), FOTOVALS(9)
LOGICAL DEBUG
INTEGER MYACT(3)
INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
!----------
!  SPECIES ORDER:
!   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
!  17=PB, 18=OS, 19=OH
!----------
!     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
!
!                  herbs, shrubs
!----------
DATA FULIVE /  0.2,   0.05, &  !WB USE LP
                  0.2,    0.1, &  !WL
                  0.2,    0.1, &  !DF
                  0.2,   0.25, &  !LM USE IE LM
                 0.15,    0.2, &  !LL USE IE LL
                  0.2,   0.25, &  !RM USE IE RM
                  0.2,   0.05, &  !LP
                 0.15,   0.10, &  !ES
                 0.15,   0.10, &  !AF
                  0.2,  0.125, &  !PP
                 0.25,   0.25, &  !GA USE QA Ottmar and others 2000b
                 0.25,   0.25, &  !AS Ottmar and others 2000b
                 0.25,   0.25, &  !CW USE QA Ottmar and others 2000b
                 0.25,   0.25, &  !BA USE QA Ottmar and others 2000b
                 0.25,   0.25, &  !PW USE QA Ottmar and others 2000b
                 0.25,   0.25, &  !NC USE QA Ottmar and others 2000b
                 0.25,   0.25, &  !PB USE QA Ottmar and others 2000b
                 0.14,   0.35, &  !OS (JUNIPER) (Ottmar, Volume I)
                 0.25,   0.25/   !OH USE QA Ottmar and others 2000b
!----------
!     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
!
!                  herbs, shrubs
!----------
DATA FULIVI /  0.4,   0.5, &  !WB USE LP
                  0.4,   1.0, &  !WL
                  0.4,   1.0, &  !DF
                 0.25,   0.1, &  !LM USE IE LM
                  0.3,   2.0, &  !LL USE IE LL
                 0.25,   0.1, &  !RM USE IE RM
                  0.4,   0.5, &  !LP
                  0.3,   1.0, &  !ES
                  0.3,   1.0, &  !AF
                 0.25,  0.05, &  !PP
                 0.18,  1.32, &  !GA USE QA Ottmar and others 2000b
                 0.18,  1.32, &  !AS Ottmar and others 2000b
                 0.18,  1.32, &  !CW USE QA Ottmar and others 2000b
                 0.18,  1.32, &  !BA USE QA Ottmar and others 2000bO
                 0.18,  1.32, &  !PW USE QA Ottmar and others 2000b
                 0.18,  1.32, &  !NC USE QA Ottmar and others 2000b
                 0.18,  1.32, &  !PB USE QA Ottmar and others 2000b
                  0.1,  2.06, &  !OS (JUNIPER) (Ottmar, Volume I)
                 0.18,  1.32/   !OH USE QA Ottmar and others 2000b
!----------
!     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
!     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
!
!                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
!----------
DATA FUINIE /0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, &  !WB USE LP
                0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, &  !WL
                0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, &  !DF
                0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, &  !LM USE IE LM
                1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, &  !LL USE IE LL
                0.7 ,0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, &  !RM USE IE RM
                0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, &  !LP
                1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, &  !ES
                1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, &  !AF
                0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, &  !PP
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !GA - QA - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !AS - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !CW - QA - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !BA - QA - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !PW - QA - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !NC - QA - ottmar and others 2000b
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, &  !PB - QA - ottmar and others 2000b
                0.1, 0.2, 0.4,  0.5, 0.8, 1.0, 0.0,0.0,0.0,0.1, 0.0, &  !OS (juniper - Ottmar, Volume I)
                0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8/ !OH - QA - ottmar and others 2000b
!----------
!     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
!     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
!
!                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
!----------
DATA FUINII /0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, &  !WB USE LP
                0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, &  !WL
                0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, &  !DF
                0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, &  !LM USE IE LM
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, &  !LL USE IE LL
                0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, &  !RM USE IE RM
                0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, &  !LP
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, &  !ES
                0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, &  !AF
                0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, &  !PP
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !GA - QA - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !AS - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !CW - QA - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !BA - QA - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !PW - QA - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !NC - QA - ottmar and others 2000b
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, &  !PB - QA - ottmar and others 2000b
                0.2, 0.4, 0.2,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.2, 0.0, &  !OS (juniper - Ottmar, Volume I)
                0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6/ !OH - QA - ottmar and others 2000b
!----------
!     FIRE MODEL TO USE WHEN COVER < 20%
!     USES ITYPE INDEX TO JTYPE() IN **BLKDAT**
!----------
DATA (MD1(I), I=   1,  50) / &
     8,  1,  2,  1,  1,  1,  1,  1,  1,  1, &
     1,  1,  1,  1,  1,  1,  2,  2,  2,  2, &
     2,  2,  2,  1,  1,  1,  1,  2,  2,  2, &
     2,  2,  2,  2,  2,  2,  2,  2,  2,  2, &
     2,  2,  2,  2,  2,  2,  2,  2,  2,  2/
DATA (MD1(I), I=  51, 100) / &
     2,  2,  2,  2,  2,  2,  5,  5,  5,  5, &
     5,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
     5,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
     5,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
     5,  5,  5,  5,  5,  5,  5,  5,  5,  5/
DATA (MD1(I), I= 101, 122) / &
     5,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
     5,  5,  5,  5,  5,  5,  5,  5*8/
!----------
!     FIRE MODEL TO USE WHEN COVER > 0%
!     USES ITYPE INDEX TO JTYPE() IN **BLKDAT**
!----------
DATA (MD2(I), I=   1,  50) / &
     8,  2,  6,  2,  2,  2,  2,  2,  2,  2, &
     2,  2,  2,  2,  2,  9,  9,  9,  9,  6, &
     6,  6,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8/
DATA (MD2(I), I=  51, 100) / &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8/
DATA (MD2(I), I= 101, 122) / &
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8, &
     8,  8,  8,  8,  8,  8,  8,  5*8/
!
DATA MYACT / 2521, 2548, 2553 /
!-----------
!  CHECK FOR DEBUG.
!-----------
CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC
7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
!----------
!  BEGIN ROUTINE.
!
!  ZERO OUT THE CUMMULATIVE VARIABLES
!----------
COVTYP = 0
PERCOV = 0.0
BIGDBH = 0.0
TOTBA  = 0.0
!----------
!  LOOP THROUGH THE TREE LIST
!----------
IF (ITRN.GT.0) THEN
!----------
!  ZERO OUT THE CUMMULATIVE VARIABLES
!----------
   BAMOST = 0.0
   TOTCRA = 0.0
!
   DO KSP=1,MAXSP
      TBA(KSP) = 0.0
   ENDDO
!
   DO I=1,ITRN
      IF (FMPROB(I) .GT. 0.0) THEN
!
         KSP = ISP(I)
!
         BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
         TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)
!
         IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
!----------
!  CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
!  ENCOMPASSED BY ALL TREES
!----------
         CWIDTH=CRWDTH(I)
         CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
         TOTCRA = TOTCRA + CAREA*FMPROB(I)
      ENDIF
!----------
!  USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
!----------
      CURKIL(I) = 0.0
   ENDDO
!----------
!  DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
!  -> THAT WILL BE THE COVER TYPE
!----------
   DO KSP=1,MAXSP
      IF (TBA(KSP) .GT. BAMOST) THEN
         BAMOST = TBA(KSP)
         COVTYP = KSP
      ENDIF
      TOTBA = TOTBA + TBA(KSP)
   ENDDO
!----------
!  USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
!  OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
!  CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:
!
!  PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))
!----------
   PERCOV = 1.0 - EXP(-TOTCRA/43560.)
   PERCOV = PERCOV * 100.0
!
ENDIF
!----------
!  IF THERE ARE NO TREES (COVTYP=0) THEN USE THE HABITAT TYPE
!  (INDEX=ITYPE) TO DETERMINE A SURROGATE
!
!        DEFINE COVTYP (IT IS A SPECIES CODE) AS A FUNCTION OF THE HABITAT
!        TYPE ENTERED FOR THE RUN. WE COMPUTE COVTYP AS A FUNCTION OF FMKOD
!----------
IF (COVTYP.EQ.0) THEN
   IF (FMKOD.LT.200) THEN
      COVTYP = 10         ! PP
   ELSEIF (FMKOD.LT.400) THEN
      COVTYP = 3          ! DF
   ELSEIF (FMKOD.LT.500) THEN
      COVTYP = 8          ! ES
   ELSEIF (FMKOD.LT.900) THEN
      COVTYP = 9          ! AF
   ELSE
      COVTYP = 7          ! LP
   ENDIF
ENDIF
!----------
!     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
!     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
!----------
XCOV(1)=10.
XCOV(2)=60.
DO I=1,2
   YLOAD(1)=FULIVI(I,COVTYP)
   YLOAD(2)=FULIVE(I,COVTYP)
   FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
ENDDO
!
IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=', &
          2F6.3)
!----------
!     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
!----------
IF (IYR .EQ. IY(1)) THEN
!----------
!  ASSUME THE FUELS ARE UNPILED AND HARD. [**THIS WILL CHANGE LATER]
!
!  LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
!  STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
!----------
  XCOV(1)=10.
  XCOV(2)=60.
  DO ISZ = 1,MXFLCL
    YLOAD(1)=FUINII(ISZ,COVTYP)
    YLOAD(2)=FUINIE(ISZ,COVTYP)
    STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
    STFUEL(ISZ,1) = 0
  ENDDO
!----------
!       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
!----------
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
!----------
!  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
!  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
!  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
!----------
      IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
    ELSE
      WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ', &
         'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
         'REQUIRED.',/1X)")
      CALL RCDSET (2,.TRUE.)
    ENDIF
  ENDIF
!----------
!  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
!  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
!----------
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
!----------
!  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
!  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
!----------
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

!----------
!  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
!  OF BASAL AREA IN THE STAND.
!----------
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
ENDIF
RETURN
!
!----------
!     HOOK TO ALLOW THE MD1() AND MD2() ARRAYS TO BE READ BY **FMCFMD*
!----------
ENTRY EMMD(M1,M2)
M1 = MD1(IEMTYP)
M2 = MD2(IEMTYP)
RETURN
!
!----------
!     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
!     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
!----------
ENTRY SNGCOE
RETURN
!
END

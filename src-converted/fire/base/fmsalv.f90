SUBROUTINE FMSALV (IYR,SALVTPA)
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!     SINGLE-STAND VERSION
!     CALLED FROM: CUTS
!     CALLS:       FMSVOL
!                  CWD1
!                  SVSALV
!----------------------------------------------------------------------
!  Purpose:
!     This subroutine removes existing snags as specified by the
!     SALVAGE keyword. It keeps track of both the total volume of the
!     snags that are removed, and their number. It also calculates
!     CWDCUT, which is used to remove a proportion of future
!     snag debris from CWD2B and add it to current debris.
!----------------------------------------------------------------------
!
!  Local variable definitions:
!      CUTDIS:  Density of Initially-Soft snags CUT in this salvage
!      CUTDIH:  Density of Initially-Hard snags CUT in this salvage
!      CUTVOL:  cumulative VOLume of snags CUT in this cycle's salvage(s)
!      IHARDV:  Volume of each Initially-HARD snag in the current record
!      ISOFTV:  Volume of each Initially-SOFT snag in the current record
!      IHARDV2  Merch volume of each Initially-HARD snag in the current record
!      ISOFTV2: Merch volume of each Initially-SOFT snag in the current record
!      KEY:     the identity number of the salvage KEYword specifications
!               that apply to this stand in this year.
!      MAXDBH:  MAXimum DBH of snags to be salvaged
!      MAXAGE:  MAXimum AGE (years since death) of snags to be salvaged
!      MINDBH:  MINimum DBH of snags to be salvaged
!      OKSOFT:  whether it's OK to salvage SOFT snags
!               (OKSOFT: 0=all snags, 1=hard only, 2=soft only)
!      PROP:    PROPortion of the eligible snags to actually salvage
!      PROPLV:  PROPortion of the eligible snags to LeaVe in the stand
!               as down material (add to CWD).
!      THISRM:  volume of snags removed in each salvage operation
!      TOTVOL:  TOTal VOLume of all snags before salvage
!
!  Common block variables and parameters:
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'
INCLUDE 'FMPROP.f90'
INCLUDE 'CONTRL.f90'

!.... Variable declarations.

INTEGER I, J, K, KEY, OKSOFT
REAL    CUTDIH, CUTDIS
REAL    CUTVOL, MAXDBH, MAXAGE, MINDBH, PROP, PROPLV
REAL    IHARDV, ISOFTV, IHARDV2, ISOFTV2
REAL    SALVTPA, TOTVOL, THISRM
REAL    X, XNEG1
REAL    PRMS(7)
INTEGER MYACT(2)
DATA    MYACT/2501,2520/
INTEGER IYR,JDO,NPRM,IACTK,JYR
INTEGER IGRP,IULIM,IG
LOGICAL LINCL,LMERCH,DEBUG
REAL    DOWN, PDOWN
INTEGER SIZE, DKCL, KYR

!.... Begin routine.
!     Initialize some outputs.  If a salvage is not requested in this stand
!     in this year (KEY<0).
!
CALL DBCHK (DEBUG,'FMSALV',6,ICYC)
IF(DEBUG)WRITE(JOSTND,*)' ENTERING FMSALV-ICYC= ',ICYC
!
CWDCUT = 0.0
CUTVOL = 0.0
SALVTPA = 0.0
TONRMS = 0.0
NSNAGSALV=NSNAG
DO I = 1,NSNAG
  SALVSPA(I,1) = 0.
  SALVSPA(I,2) = 0.
  HTIHSALV(I)=HTIH(I)
  HTISSALV(I)=HTIS(I)
  SPSSALV(I)=SPS(I)
  DBHSSALV(I)=DBHS(I)
  HARDSALV(I)=HARD(I)
  HTDEADSALV(I)=HTDEAD(I)
ENDDO
!
CALL OPFIND(2,MYACT,KEY)
IF (KEY .LE. 0) RETURN

!     COMPUTE TOTAL VOLUME OF SNAGS PRIOR TO ANY SALVAGE.
!     THIS IS USED FOR CALCULATING THE PROPORTION VOL CUT, USED
!     FOR CWD CALCS LATER. THUS, EAST/WEST VARIANT IS NOT IMPORTANT.

TOTVOL = 0.0
DEBUG  = .FALSE.

LMERCH = .FALSE.
IF (LVWEST) LMERCH = .TRUE.

DO I = 1,NSNAG
  IF ((DENIS(I) + DENIH(I)) .GT. 0.0) THEN
    IF (DENIS(I) .GT. 0.0) THEN
      ISOFTV  = 0.0
      CALL FMSVOL(I, HTIS(I), ISOFTV,.FALSE.,0)
      TOTVOL = TOTVOL + DENIS(I) * ISOFTV
    ENDIF

    IF (DENIH(I) .GT. 0.0) THEN
      IHARDV  = 0.0
      CALL FMSVOL(I, HTIH(I), IHARDV,.FALSE.,0)
      TOTVOL = TOTVOL + DENIH(I) * IHARDV
    ENDIF
  ENDIF
ENDDO
!
DO JDO = 1,KEY

  CALL OPGET(JDO,6,JYR,IACTK,NPRM,PRMS)
!
!       IF THIS IS A SALVSP SETTING, PROCESS IT
!
  IF(IACTK .EQ. 2501) THEN
    ISALVS = INT(PRMS(1))
    ISALVC = INT(PRMS(2))
    CALL OPDONE(JDO,IYR)
    GO TO 200
  ENDIF
!
!       Get the parameter values for this salvage operation
!       and initialize counter. Check ranges again (in case
!       of call through PARMS)
!
  MINDBH = MAX(  0.0, PRMS(1))
  MAXDBH = MIN(999.0, PRMS(2))
  MAXAGE = MAX(  0.0, PRMS(3))
  IF (PRMS(4) .GT. 2.0 .OR. PRMS(4) .LT. 0.0) PRMS(4) = 0.0
  OKSOFT = INT(PRMS(4))
  PROP   = MIN(1.0,MAX(0.0,PRMS(5)))
  PROPLV = MIN(1.0,MAX(0.0,PRMS(6)))
!
!       Loop over all snag records that are in use.
!
  THISRM = 0.0
  DO 100 I = 1,NSNAG

  LINCL = .FALSE.
  IF(ISALVS.EQ.0 .OR. ISALVS.EQ.SPS(I))THEN
    LINCL = .TRUE.
  ELSEIF(ISALVS.LT.0)THEN
    IGRP = -ISALVS
    IULIM = ISPGRP(IGRP,1)+1
    DO 90 IG=2,IULIM
    IF(SPS(I) .EQ. ISPGRP(IGRP,IG))THEN
      LINCL = .TRUE.
      GO TO 91
    ENDIF
90     CONTINUE
  ENDIF
91   CONTINUE

!         Skip this record if there are no snags in it.

    IF ((DENIS(I) + DENIH(I)) .LE. 0.0) GOTO 100
!
!         Skip the rest of the routine if none of these snags are eligible
!         for salvage.
!            (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
!
    IF(ISALVC.EQ.0 .AND. .NOT.LINCL) &
         GO TO 100
    IF(ISALVC.EQ.1 .AND. LINCL) &
         GO TO 100
    IF ( DENIH(I) .LE. 0.0 .AND. OKSOFT .EQ. 1 ) GOTO 100
    IF ( DENIS(I) .LE. 0.0 .AND. OKSOFT .EQ. 2 ) GOTO 100
    IF ( (IYR-YRDEAD(I)) .GT. MAXAGE .OR. &
            DBHS(I) .GE. MAXDBH .OR. &
            DBHS(I) .LT. MINDBH ) GOTO 100
!
!         COMPUTE VOLUME OF SOFT AND HARD SNAGS:
!           ISOFTV,IHARDV:   TOTAL VOLUME OF SNAGS
!           ISOFTV2,IHARDV2: MERCHANTABLE VOLUME OF SNAGS (FOR C HARV REPT.)
!
!           Note: call to FMSVL2 is changed so that routine appropriately
!                 recognizes original height of snags and calculates volume
!                 as if it is a top kill.
!
    XNEG1  = -1.0
    ISOFTV = 0.0
    ISOFTV2 = 0.0
    IF (DENIS(I) .GT. 0.0) THEN
      CALL FMSVOL(I, HTIS(I), ISOFTV,DEBUG,0)
      CALL FMSVL2(SPS(I),DBHS(I),HTDEAD(I),HTIS(I),ISOFTV2,0, &
                     'D',LMERCH,DEBUG,JOSTND)
!            CALL FMSVL2(SPS(I),DBHS(I),HTIS(I),XNEG1,ISOFTV2,
!     >        LMERCH,DEBUG,JOSTND)
    ENDIF

    XNEG1  = -1.0
    IHARDV2 = 0.0
    IHARDV = 0.0
    IF (DENIH(I) .GT. 0.0) THEN
      CALL FMSVOL(I, HTIH(I), IHARDV,DEBUG,0)
      CALL FMSVL2(SPS(I),DBHS(I),HTDEAD(I),HTIH(I),IHARDV2,0, &
                     'D',LMERCH,DEBUG,JOSTND)
!            CALL FMSVL2(SPS(I),DBHS(I),HTIH(I),XNEG1,IHARDV2,
!     >        LMERCH,DEBUG,JOSTND)
    ENDIF
!
!         Target some of the initially-hard snags, if there are any.
!         Note that these will either all still be hard or have all
!         gone soft. If they're all soft, only cut them if soft
!         snags are eligible for salvage.
!            (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
!
    CUTDIH = 0.0
    IF ( (DENIH(I) .GT. 0.0) .AND. &
            ( (HARD(I) .AND. (OKSOFT .NE. 2)) .OR. &
              ((.NOT. HARD(I)) .AND. (OKSOFT .NE. 1)) )) &
         CUTDIH = PROP * DENIH(I)
!
!         Target some of the initially-soft snags, if there are any
!         and if soft snags are eligible.
!
    CUTDIS = 0.0
    IF ((DENIS(I) .GT. 0.0) .AND. (OKSOFT .NE. 1)) &
         CUTDIS = PROP * DENIS(I)
!
!         Remove the snags and increment the removal counters.
!
    DENIS(I) = DENIS(I) - CUTDIS
    DENIH(I) = DENIH(I) - CUTDIH
!
!         Set some common variables for use with the SalvVol EM function
!
    SALVSPA(I,1) = SALVSPA(I,1) + CUTDIH*(1.0 - PROPLV)
    SALVSPA(I,2) = SALVSPA(I,2) + CUTDIS*(1.0 - PROPLV)
!
    IF (DENIS(I) .LE. 0.0) DENIS(I) = 0.0
    IF (DENIH(I) .LE. 0.0) DENIH(I) = 0.0
!
!         INCREMENT CUT VOLUME AND REMOVED VOLUME
!
    CUTVOL = CUTVOL + (CUTDIS*ISOFTV + CUTDIH*IHARDV)
    SALVTPA = SALVTPA + CUTDIS + CUTDIH
    THISRM = THISRM + (CUTDIS*ISOFTV + CUTDIH*IHARDV) &
                 * (1.0 - PROPLV)
!
!         LEAVE BEHIND A PROPORTION OF THE SALVAGED SNAGS
!         AND PLACE THE MATERIAL IN THE CWD POOLS
!
    CALL CWD1(I, CUTDIH*PROPLV, CUTDIS*PROPLV)
!
!         THE REMAINDER IS MARKED AS REMOVED FOR REPORTING PURPOSES
!
    TONRMS = TONRMS + &
          (CUTDIS * ISOFTV + CUTDIH * IHARDV) * V2T(SPS(I)) &
          * (1.0 - PROPLV)
!
!         ADD SALVAGE TO C-ACCOUNTING HARVEST POOLS
!         TOTAL SNAG VOLUME IS ALWAYS CALCULATED BY THE FFE METHOD;
!         NEVER WITH JENKINS, USING THE MERCHANTABILITY CRITERIA
!         APPROPRIATE TO THE EAST/WEST VARIANTS.

    X = (CUTDIS*ISOFTV2 + CUTDIH*IHARDV2) * V2T(SPS(I)) &
          * (1.0 - PROPLV)
    K = 1
    IF (BIOGRP(SPS(I)) .GT. 5) K = 2
    J = 1
    IF (DBHS(I) .GT. CDBRK(K))  J = 2
    FATE(J, K, ICYC) = FATE(J, K, ICYC) + X

100   CONTINUE
!
!       RECORD THE VOLUME REMOVED FROM THE STAND FOR THE ACTIVITY SUMMARY
!
  PRMS(7) = THISRM
  CALL OPCHPR(JDO,7,PRMS)
  CALL OPDONE(JDO,IYR)

!
!       CALL SVSALV TO REMOVE SNAGS FROM THE BASE FVS SNAG ARRAYS, AND
!       DELETE THE ASSOCIATED SVS SNAG OBJECTS.
!
  CALL SVSALV(IYR,MINDBH,MAXDBH,MAXAGE,OKSOFT,PROP,PROPLV)


200   CONTINUE

ENDDO
!
!     Calculate CWDCUT based on the volume proportion
!     of cut snags
!
IF (TOTVOL .GT. 0.0) CWDCUT = CWDCUT + CUTVOL / TOTVOL

!     new section for snag crowns left as slash during salvage.  separated
!     from fmcadd (sar april 2014).

!     Here we are accounting for crowns from salvaged trees left as slash.
!     To approximate this, remove a proportion of material from every
!     year pool - 1 to TFMAX.  NOTE:  this is a slight mis-usage of CWDCUT,
!     because CWD2B also contains dead crown material from live-but-
!     burned trees (not much per tree, or the tree would have died).

  DO KYR=1,TFMAX

    PDOWN = CWDCUT

!         Repeat for each decay class:

    DO DKCL=1,4

!          First add the litterfall to down debris.

      DOWN = PDOWN * CWD2B(DKCL,0,KYR)
      CWD(1,10,2,DKCL) = CWD(1,10,2,DKCL) + DOWN / 2000.0
      CWDNEW(1,10) = CWDNEW(1,10) + DOWN / 2000.0
      CWD2B(DKCL,0,KYR) = CWD2B(DKCL,0,KYR) - DOWN

!          Then all the sizes of woody material.

      DO SIZE=1,5
          DOWN = PDOWN * CWD2B(DKCL,SIZE,KYR)
          CWD(1,SIZE,2,DKCL) = CWD(1,SIZE,2,DKCL) + DOWN / 2000.0
          CWDNEW(1,SIZE) = CWDNEW(1,SIZE) + DOWN / 2000.0
          CWD2B(DKCL,SIZE,KYR) = CWD2B(DKCL,SIZE,KYR) - DOWN

      ENDDO
    ENDDO
  ENDDO


RETURN
END

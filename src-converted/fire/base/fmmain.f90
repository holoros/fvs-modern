SUBROUTINE FMMAIN
IMPLICIT NONE
!----------
! FIRE-BASE $Id$
!----------
!
!     THIS ROUTINE IS THE 'MAIN' FIRE ROUTINE. IT LOOPS OVER
!     THE YEARS WITHIN A CYCLE, AND LOOPS OVER EACH STAND, EACH
!     YEAR. IT CALLS MANY OF THE DIFFERENT PARTS OF THE FIRE MODEL.
!     FMD IS DOMINANT FUEL MODEL.
!
!     CALLED FROM -- GRADD  (SINGLE-STAND)
!     CALLS: EVSET4
!            FMCBA
!            FMBURN
!            FMCADD
!            FMCWD
!            FMDOUT
!            FMGSTD
!            FMSNAG
!            FMSOUT
!            FMSSUM
!            FMTRET
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'
!
!OMMONS
!
LOGICAL DEBUG
INTEGER I,IYR,IL,ISZ,IDC,ITM,IRTNCD
INTEGER FMD
!     Variables that support the use of FMORTMLT
INTEGER  MYACTS(1),NTODO,ITODO,NPRM,IACT,IDSP
REAL     PRMS(4)
DATA     MYACTS/2554/

!     CHECK FOR DEBUG.

CALL DBCHK (DEBUG,'FMMAIN',6,ICYC)
IF (DEBUG) WRITE(JOSTND,6) ICYC,LFMON
6 FORMAT(' ENTERING FMMAIN CYCLE = ',I2,' LFMON=',L2)

!     RETURN IF THE FIRE MODEL IS NOT ACTIVE

IF (.NOT. LFMON) RETURN

!     SET EVENT MONITOR VARIABLES (FROM **EVTSTV**)
!     420 FIRE 0 IF STAND HAS NO FIRE, 1 IF FIRE OCCURS (FM)

CALL EVSET4(20, 0.0)
LFIRE=.FALSE.

!     Calculate the number of years in this cycle so that the decomposition
!     rates can be adjusted correctly for variable cycle lengths.
!     This is necessary as we move from the FFE working on annual timesteps
!     to cycle timesteps. (note: this value is the same as IFINT)

NYRS = IY(ICYC+1) - IY(ICYC)
!
!     Loop over the years within the cycle
!
IFMYR1 = IY(ICYC)
IFMYR2 = IY(ICYC+1) - 1
IF (DEBUG) WRITE(JOSTND,7) IFMYR1,IFMYR2, BURNYR, ITRN
7 FORMAT(' IN FMMAIN IFMYR1 IFMYR2 BURNYR ITRN= ',5I5)

!     Process FMORTMLT

FMORTMLT = 1.
CALL OPFIND(1,MYACTS,NTODO)
IF (NTODO.GT.0) THEN
  DO ITODO=1,NTODO
    CALL OPGET(ITODO,4,IDSP,IACT,NPRM,PRMS)
    CALL OPDONE(ITODO,IY(ICYC))
    IDSP = IFIX(PRMS(2))
    DO I=1,ITRN
      IF (IDSP .NE. 0 .AND. ISP(I) .NE. IDSP) CYCLE
      IF (DBH(I).GE.PRMS(3) .AND. DBH(I).LT.PRMS(4)) &
            FMORTMLT(I) = PRMS(1)
    ENDDO
    IF (DEBUG) WRITE(JOSTND,8) PRMS(1),IDSP,PRMS(3),PRMS(4)
8     FORMAT(' FMORTMLT SET TO',F10.4,' FOR SPECIES I=',I3, &
         ' MIND=',F6.2,' MAXD=',F7.1)
  ENDDO
ENDIF

! REMOVE THE LOOP FOR RUNNING THIS JUST ON CYCLE BOUNDARIES...
!     and set IYR to be the cycle year.
IYR = IFMYR1
!      DO IYR = IFMYR1,IFMYR2

   IF (DEBUG) WRITE(JOSTND,9) IYR,BURNYR
9    FORMAT(' IN FMMAIN IYR BURNYR= ',2I5)

!        INITIALIZE THE CROWN RATIO IF ON THE FIRST YEAR
!        OF THE CYCLE.  THIS IS REQUIRED BECAUSE CUTS MAY HAVE
!        CHANGED THE CROWN RATIO IN SUPPORT OF THE PRUNE KEYWORD
!        AND REGENT MAY CHANGE IT IN THE NI VARIANT.
!         NOTE: cycle-boundary version no longer needs to check year
!
!         IF (IYR.EQ.IFMYR1) THEN
      DO I=1,ITRN
         FMPROB(I) = PROB(I)
         FMICR(I)  = ICR(I)
         FIRKIL(I) = 0.0
      ENDDO
!         ELSE
!            TONRMS=0.0
!            TONRMH=0.0
!            TONRMC=0.0
!         ENDIF

!        Initialize some Key variables. (R&C 07/09/96)

   SMOKE(1) = 0.0
   SMOKE(2) = 0.0
   CRBURN = 0.0
   BURNCR = 0.0
   PBRNCR = 0.0
   DO IL = 1,MXFLCL
      BURNED(1,IL) = 0.0
      BURNED(2,IL) = 0.0
      BURNED(3,IL) = 0.0
   ENDDO
   BURNLV(1) = 0.0
   BURNLV(2) = 0.0

!        Calculate the dominant cover type (as basal area)
!        Note that in the single stand case, this only needs to be
!        done once per cycle unless a burn has occurred during the cycle

   CALL FMCBA (IYR,0)

!        This resets the value of cwdcut, which is based on salvage
!        removal, back to zero in all but the first year of a cycle.
!        This is necessary since the call to fmsalv was moved to fmsdit,
!        which is only called at the beginning of each cycle.  since
!        salvage cuts now only occur on cycle breaks, cwdcut should
!        always be zero in all other years.
!         NOTE: cycle-bouncary version no longer needs this, since
!                 IYR=IFMYR1 always

!         IF (IYR .NE. IFMYR1) CWDCUT = 0.
!
!     END OF INITIALIZATION PART OF ROUTINE
!
!     DO VARIOUS ACTIVITIES AND TREATMENTS
!
!        Do fuel treatment (jackpot burns and pile burns).

   CALL FMTRET (IYR)

!        Do FuelMove keyword (formerly in FMCWD)

   CALL FMFMOV(IYR)

!        Check on user-specified fm definitions and
!        process any fueltret keywords.

   CALL FMUSRFM (IYR, FMD)

!        Simulate actual fires
   CALL FMBURN (IYR, FMD, .TRUE.)

!
!     PRINT ALL OUTPUT FILES
!
!        Print out the current snag list (if requested)

   CALL FMSOUT (IYR)
   CALL FMSSUM (IYR)

!        Potential fire report

!----------
!  CALL FMPOCR SO THE CANOPY FUELS PROFILE TABLE IS PRINTED
!  AT THE CORRECT TIME.
!  CALL NEW ROUTINE TO LOAD INFORMATION FOR CALCULATING THE
!  FUEL MODEL VARIABLES, BUT ONLY CALL THIS TIME IF A FIRE OCCURRED.
!----------
   CALL FMPOCR(IYR,2)
   IF (BURNYR .EQ. IYR) THEN
      CALL FMCFMD3(IYR, FMD)
   ENDIF

!----------
!  CALCULATE AND PRINT THE POTENTIAL FLAME LENGTH REPORT
!----------
   CALL FMPOFL (IYR, FMD, .TRUE.)
   CALL fvsGetRtnCode(IRTNCD)
   IF (IRTNCD.NE.0) RETURN

!        Print the stand-level fuel output table

   CALL FMDOUT (IYR)

!        Print the stand-level main carbon report

   CALL FMCRBOUT (IYR)

!        Print the stand-level harvest products carbon report

   CALL FMCHRVOUT (IYR)

!        If this is the first year, call evtstv to compute user-defined
!        variables. there will not be any to compute unless some are a
!        function of previously undefined variables. We set all the fire-
!        related variables to undefined so any compute expressions that
!        are functions of them will be undefined...this call attempts to
!        compute them after they have been defined. There may be unintended
!        side effects of this code!

   CALL EVTSTV (iyr)

!
!        UPDATE SNAG AND CWD POOLS
!
!        temporarlily reset teh value of NYRS to 1 so that we don't need to change the code again
   NYRS = 1

   DO IYR = IFMYR1,IFMYR2

!          Update conditon of existing snags for the current year.

     CALL FMSNAG (IYR, IY(1))

!          Update coarse woody debris pools

     CALL FMCWD(IYR)

!          Add this year's litterfall, crown breakage, and snag-crown-fall
!          to the CWD pools.

     CALL FMCADD

!          Copy CWD2B2 onto CWD2B (i.e., add debris from all snags
!          killed in the previous year to the pools of material
!          scheduled to fall in the upcoming years), and zero out CWD2B2.
!          (This used to be in FMSDIT and was moved so that it occurs
!          before any cuts that may occur next cycle.)

     DO ISZ = 0,5
        DO IDC = 1,4
           DO ITM = 1,TFMAX
              CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM) &
                      + CWD2B2(IDC,ISZ,ITM)
              CWD2B2(IDC,ISZ,ITM) = 0.0
           ENDDO
        ENDDO
     ENDDO

   ENDDO

!        change NYRS back to its original value
   NYRS = IY(ICYC+1) - IY(ICYC)

!        In the last year of each cycle, record some information about
!        crown size for use in determining litterfall in the next cycle.

!        IF (IYR .EQ. IFMYR2) CALL FMOLDC
   CALL FMOLDC

CALL FMSVSYNC

RETURN
END

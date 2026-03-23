SUBROUTINE BWEINT
IMPLICIT NONE
!---------
! WSBWE $Id$
!---------
!
! INITIALIZE BW DEFOLIATION MODEL VARIABLES
!    COMBINES PORTIONS OF BWLINT & BWGINT FROM POP.DYN.MODEL
!    READS WEATHER STATION NAMES; INIT.S EVENT SUMMARY ARRAY
!
! K.A.SHEEHAN, USFS, R6-FID, PORTLAND, OR
!
! CALLED FROM: INITRE
!
! SUBROUTINES CALLED: MYOPEN
!
! PARAMETERS:    (ALL IN BWEBOX.F77)
!   STNAME - NAME OF FILE CONTAINING LIST OF WEATHER STATIONS
!   JOWE - FILE NUMBER ASSIGNED TO STNAME
!   NUMSTN(10) - NUMBER OF WEATHER STATIONS BY STATE
!   WSLOOK(100,10) - STORES WEATHER STATION NAMES BY STATE
!   NEVENT - NUMBER OF BW SPECIAL EVENTS TO DATE
!   IEVENT(250,5) - BW SPECIAL EVENTS SUMMARY ARRAY
!
! Revision History:
!   05-MAY-00 Lance David (FHTET)
!      .Added debug handling.
!      .Added weather and outbreak random number seeds WSEEDR and OBSEER.
!   15-MAY-00 Lance David (FHTET)
!       Move initialization for non-static variables from BWEBK??.F
!       block data routine to this BWEINT.F so variables will get
!       reinitialized between stands for a multiple stand serial run.
!   22-MAY-00 Lance David (FHTET)
!       Added initialization of more variables from common blocks.
!    16-JUN-00 Lance David (FHTET)
!      .Reading of the weather station data file (STATIONS.DAT) moved
!       from BWEINT to BWEIN so that the process will only occur if
!       budworm keywords are present. Subroutine BWEINT is called by
!       FVS subroutine INITRE regardless.
!    16-AUG-01 Lance R. David (FHTET)
!       Added initialization of random number generator with Damage Model
!       random number seed.
!    21-NOV-2002 Lance R. David (FHTET)
!       Changed variable ISTATE to ISTNUM.
!    01-APR-2004 Lance R. David (FHTET)
!       Initialization of variables MGMIDB and ITITLB.
!    10-AUG-2006 Lance R. David (FHTET)
!       removed obsolete varaibles KFNAME and MOPT.
!    30-AUG-2006 Lance R. David (FHTET)
!       Changed array orientation of IEVENT from (4,250) to (250,4).
!    06-SEP-2006 Lance R. David (FHTET)
!       Moved init of variables variable from block data because they
!       are not static (FOLDVY, FOLWTY, IOUT6A)
!    14-JUL-2010 Lance R. David (FMSC)
!       Added IMPLICIT NONE and declared variables as needed.
!    23-JUN-2011 Lance R. David (FMSC)
!       Added BWPRMS array for RAWS daily weather processing to BLCOM3.
!   28-AUG-2013 Lance R. David (FMSC)
!      Modified initialization of IEVENT array.
!------------------------------------------------------------------------
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BWESTD.f90'
INCLUDE 'BWECOM.f90'
INCLUDE 'BWECM2.f90'
INCLUDE 'BWEBOX.f90'

INTEGER I, I1, I2, I3, I4, IHOST, ITREE, J
LOGICAL DEBUG, LTRU

!
!.... Check for DEBUG
!
CALL DBCHK(DEBUG,'BWEINT',6,ICYC)

IF (DEBUG) WRITE (JOSTND,*) 'ENTER BWEINT: ICYC = ',ICYC
!
!     INITIALIZE RANDOM NUMBER GENERATOR WITH DEFAULT SEEDS
!     FOR DAMAGE MODEL, WEATHER MODEL AND OUTBREAK SCHEDULING.
!
LTRU = .TRUE.
DSEEDR = 55329.0
!     DSEEDD = DBLE(DSEEDR)
CALL BWERSD(LTRU,DSEEDR)
CALL BWERGT(DSEEDD)
WSEEDR = 25523.0
CALL BWERSD(LTRU,WSEEDR)
CALL BWERGT(WSEED)
OBSEER = 42917.0
CALL BWERSD(LTRU,OBSEER)
CALL BWERGT(OBSEED)

IF (DEBUG) WRITE(JOSTND,*) 'IN BWEINT: DSEEDR=',DSEEDR, &
      ' DSEEDD=',DSEEDD,' WSEEDR=',WSEEDR,' WSEED=',WSEED, &
      ' OBSEER=',OBSEER,' OBSEED=',OBSEED

!
!     INITIALIZE ANNUAL DAMAGE VARIABLES RELATING TO RELATIVE GROWTH,
!     PROPORTIONAL GROWTH, AND TREE MORTALITY.
!
DO 25 IHOST=1,6
  DO 24 ITREE=1,3
    RDDSM1(IHOST,ITREE)=1.0
    RHTGM1(IHOST,ITREE)=1.0
    PEDDS(IHOST,ITREE)=0.0
    PEHTG(IHOST,ITREE)=0.0
    BWMXCD(IHOST,ITREE)=0.0
24   CONTINUE
25 CONTINUE
!
!     INITIALIZE SCALARS.....
!
NCUMYR = 0
ICUMYR = 0
LBWDAM = .FALSE.
LBWPDM = .FALSE.
LTOPK  = .TRUE.
IBWYR1 = -999
ILOBYR = 1950
IOBDUR = 0
LOWYRS = 0
IBUDYR = 0
IYRCUR = 0
IWYR   = 0
HOSTST = 0.0

!     Initialize Foliage arrays.
!     dimensions are: I1 host, I2 crown third, I3 age
!
DO I1 = 1, 6
  DO I2 = 1, 9
    DO I3 = 1, 4
      FOLPOT(I1,I2,I3) = 0.0
      FOLADJ(I1,I2,I3) = 0.0
    END DO
  END DO
END DO

!     dimensions are: I1 crown, I2 host
DO I1 = 1, 9
  FOLNH(I1) = 0.0
  DO I2 = 1, 6
    ACTNEW(I1,I2) = 0.0
    BW(I1,I2) = 0.0
    FNEW(I1,I2) = 0.0
    FREM(I1,I2) = 0.0
    FOLD1(I1,I2) = 0.0
    FOLD2(I1,I2) = 0.0
  END DO
END DO

!     Foliage quality effects on larval development time parameters
!     FQUALDEV keyword.

FOLDVY(1) = 1.0
FOLDVY(2) = 1.0
FOLDVY(3) = 1.1
FOLDVY(4) = 1.1

!     Foliage quality effects on pupal weight parameters
!     FQUALWT keyword.

FOLWTY(1) = 1.0
FOLWTY(2) = 1.0
FOLWTY(3) = 0.9
FOLWTY(4) = 0.9

!     Initialize Cummulative Defoliation arrays.
!     dimensions are: I1 host, I2 size class, I3 year,
!                     I4 type (1-tree top, 2-whole tree)
DO I1 = 1, 6
  DO I2 = 1, 3
    DO I3 = 1, 5
      CUMDEF(I1,I2,I3) = 0.0

      DO I4 = 1, 2
!             AVERAGE CUMULATIVE PROPORTION OF RETAINED BIOMASS
        APRBYR(I1,I2,I4,I3) = 0.0
      END DO
    END DO
  END DO
END DO

!     Host goes to 7 because of inclusion of nonhost category.
!     dimensions are: I1 host, I2 size class
DO I1 = 1, 7
  IFHOST(I1) = 0
  DO I2 = 1, 3
    BWTPHA(I1,I2) = 0.0
  END DO
END DO

IOBACT = 0
ISTNUM = 7
ISTN   = 1
IWOPT  = 1
IOBLOC = 2
IYRECV = 1
IYRSRC = 6
KRECVR = 0
LDEFOL = .FALSE.
LREGO  = .FALSE.
LCALBW = .FALSE.
LBUDL  = .FALSE.
LFIRST = .FALSE.
LSPRAY = .FALSE.
IDEFPR = 0
NUMCOL = 0
DEFLAB = '        '
IOBOPT = 1
NOBDON = 0
EGGS   = 0.0
NOBSCH = 0
DO I1 = 1, 4
  DO I2 = 1, 3
    NEMULT(I1,I2) = 1.0
  END DO
END DO
DO I1 = 1, 3
  DO I2 = 1, 2
    IOBSCH(I1,I2) = 0
  END DO
END DO


DO I1 = 1, 5
  DLABS(I1) = '        '
  IDEFSP(I1) = 0
END DO

DO I1 = 1, 9
  DO I2 = 1, 6
    ANTDEN(I1,I2) = 1.0
    BIRDEN(I1,I2) = 1.0
    MYS1DN(I1,I2) = 0.0
    MYS2DN(I1,I2) = 0.0
    MYS3DN(I1,I2) = 0.0
  END DO
END DO

DO I = 1, 3
  M1PRED(I) = 0.0
  M2PRED(I) = 0.0
  M3PRED(I) = 0.0
  WRAINA(I) = 1.0
  WRAINB(I) = 1.0
  WRAIN1(I) = 1.0
  WRAIN2(I) = 1.0
  WRAIN3(I) = 1.0
END DO

!     Logical unit numbers for I/O
!
JOWSBW = 26
JOWE   = 40
JOBWP1 = 41
JOBWP2 = 42
JOBWP3 = 43
JOBWP4 = 44
JOBWP5 = 45
JOBWP6 = 46
JOBWP7 = 47
JOBWP8 = 48

!     Indicators and file names for output tables and reports.
!     These correspond with the JOBWP? unit numbers above.
!
LP1 = .FALSE.
LP2 = .FALSE.
LP3 = .FALSE.
LP4 = .FALSE.
LP5 = .FALSE.
LP6 = .FALSE.
LP7 = .FALSE.
LP8 = .FALSE.
OUTNAM(1) = 'WITHIN.TAB'
OUTNAM(2) = 'CANOPY.TAB'
OUTNAM(3) = 'DEFSUM.TAB'
OUTNAM(4) = 'PARAMS.TAB'
OUTNAM(5) = 'ANNUAL.TAB'
OUTNAM(6) = 'DEFOL.TAB'
OUTNAM(7) = 'DYNAMICS.TAB'
OUTNAM(8) = 'EFFECTS.TAB'

MGMIDB = ' '
ITITLB = 'GENERAL DEFOLIATION MODEL'

IOUT6A(1) = '   '
IOUT6A(2) = '   '
IOUT6A(3) = '   '

!     OUT1(9,6,17)
DO I1 = 1, 9
  DO I2 = 1, 6
    DO I3 = 1, 17
      OUT1(I1,I2,I3) = 0.0
    END DO
  END DO
END DO

!     OUT2(6,3,8)
DO I1 = 1, 6
  DO I2 = 1, 3
    DO I3 = 1, 8
      OUT2(I1,I2,I3) = 0.0
    END DO
  END DO
END DO

!     OUT3(9,6,20)
DO I1 = 1, 9
  DO I2 = 1, 6
    DO I3 = 1, 20
      OUT3(1,I2,I3) = 0.0
    END DO
  END DO
END DO

WCOLDW = 1.0
WRAIND = 1.0
WHOTF  = 1.0
TREEDD = 350.0
DISPMR = 0.25

DO I1 = 1, 6
  DEFYRS(I1) = 0.0
END DO

OBPHAS(1,1) = 0.0
OBPHAS(2,1) = 0.13
OBPHAS(3,1) = 0.128
OBPHAS(1,2) = 0.0
OBPHAS(2,2) = 0.375
OBPHAS(3,2) = 0.364

!     Variables used in spray events.
!
ISPRAY = 0
ISPVAR = 0
INSTSP = 1
SPEFF  = 0.0
DEVEL = .373
DEVELS(1) = 0.0
DEVELS(2) = 0.20
DEVELS(3) = 0.69
DO I1 = 1, 5
  ISPYR(I1) = 0
END DO
DO I1 = 1, 10
  SPEFFS(I1) = 0.0
  SPINST(I1) = 0.0
END DO
TRIGGR = 0.0
NSPRAY = 0
DEFLYR = 0.0
LIMITS = 1
NUMAPP = 0

!     Weather variables.
!
WFNAME = 'WEATHER.DAT'
STNAME = 'STATIONS.DAT'
IWSRC  = 1

!     Array loaded from RAWS daily weather data in subroutine BWERAWS.f
!     11 parameters, 50 years.
!
DO I = 1, 11
  DO J=1, 50
    BWPRMS(I,J) = 0.0
  END DO
END DO


!     Some temp variables used in keyword, reporting and weather processing (?)
!
ITEMP(1) = 3
ITEMP(2) = 2
ITEMP(3) = 10
ITEMP(4) = 7
ITEMP(5) = 20
LTEMP1(1) = .FALSE.
LTEMP1(2) = .TRUE.
LTEMP1(3) = .TRUE.
LTEMP1(4) = .TRUE.

IPRBYR=0
IBWYR2=-1
!
! SET THE SPECIAL EVENTS TABLE TO ZERO
!
NEVENT=0
DO 70 I=1,250
DO 70 J=1,5
IEVENT(I,J)=0
70 CONTINUE
!
IF (DEBUG) WRITE (JOSTND,*) 'EXITING BWEINT: ICYC = ',ICYC
RETURN
END

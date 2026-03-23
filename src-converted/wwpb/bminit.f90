SUBROUTINE BMINIT (JOPRT)
!----------
! WWPB $Id$
!----------
!  Purpose:
!      Initialize variables for the Pine Beetle Model
!----------------------------------------------------------------------
!
!  Called from: BMPPIN
!
!  Call list definitions:
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on and off.
!     ISIZ:   Integer counter
!     LOW:    diameter of (i-1) class
!     MID:    midpoint of diameter class
!     X:      premultiply factor
!
!  Common block variables and parameters:
!     ICYC:   From CONTRL; cycle index number.
!     JOSTND: From CONTRL; logical unit number for stand output.
!     MSBA:   From BMCOM; avg basal area in each dbh size class
!     WPBA:   From BMCOM; avg basal area in each dead pool dbh size class
!     NBGEN:  From BMCOM; the number of MBP/WPB generations/yr
!     NIBGEN: From BMCOM; the number of IPS generations/yr
!     UPSIZ:  From BMCOM; upper size limits for dbh size classes
!     WPSIZ:  From BMCOM; upper size limits for dead pool dbh size classes
!
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'BMCOM.f90'
INCLUDE 'BMRRCM.f90'
INCLUDE 'BMPCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG, LRET
INTEGER ISIZ, LOW, I
REAL    MID, X

!.... Data statements.
!     Data statements have been moved to BLOCK DATA subprogram, BMBLKD.FOR.
!          8/16/94 Lance R. David
!
!     REMOVED LFAC PARAMETER CALCULATION, APPRENTLY NOT USED ANYWHERE.
!     AJM 8/05

!.... Check for debug.

CALL DBCHK(DEBUG,'BMINIT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,10) ICYC
10 FORMAT(' Begin BMINIT: Cycle = ',I5)

!     Set flag indicating that BMINIT subroutine has been called.
!
LBMINT = .FALSE.

X= PIE / (24. * 24.)
DO 100 ISIZ = 1,NSCL
    IF (ISIZ .EQ. 1) THEN
        LOW= 0
    ELSE
        LOW= UPSIZ(ISIZ-1)
    ENDIF

    MID = FLOAT(UPSIZ(ISIZ) + LOW) * 0.5
    MSBA(ISIZ) = MID * MID * X
    UPBA(ISIZ) = UPSIZ(ISIZ) * UPSIZ(ISIZ) * X
100 CONTINUE


!     Reproductive "increase" amounts for each beetle. Take the average
!     of the reproductive amount at each dbh.
!     These values can be changed by keyword

!      RSLOPE = 1.0/15.0
!      REPMAX = 3.0
!     Note! if these parameters are altered, check for possible negative
!     values in smaller size classes ajm9/05.
RSLOPE = 1.0/10.0
REPMAX = 4.0
DBHMAX = 36.0
REPLAC = 6.0
B = 1 - (RSLOPE * REPLAC)

LOW= 0
DO 110 I= 1,NSCL
IF (I .GT. 1) LOW = UPSIZ(I-1)
  DBHMID= (UPSIZ(I) + LOW) / 2.0

  IF (DBHMID .LT. DBHMAX) THEN
     INC(1,I) = (RSLOPE * DBHMID) + B
  ELSE
     INC(1,I) = REPMAX
  ENDIF
!
! INC(2,-) is provisional (we still need to look up the data in Lindhal thesis)

  INC(2,I)= INC(1,I)

!       Ips only attacks the part of a tree that is less than 3"dbh so that should
!       be the reproductive factor. But, Ips does even less well (this may be too
!       extreme)

 INC(3,I)= INC(1,1) * 0.1

110 CONTINUE

! DEAD WOODY POOL PARAMETER INITIALIZATIONS
!
! Defaults: breakpoints for the dead woody pool (host) DBH size classes.
!           There can be no more than MXDWHZ of these. The calculation
!           that follows computes another rough basal area.

DO 200 ISIZ = 1,MXDWSZ
  IF (ISIZ .EQ. 1) THEN
      LOW= 3
  ELSE
      LOW= WPSIZ(ISIZ-1)
  ENDIF
  MID = FLOAT(WPSIZ(ISIZ) + LOW) * 0.5
  WPBA(ISIZ) = MID * MID * X
200 CONTINUE

!     Calculate the translation variable from living to dead size classes
!         Some fiddling is done here so that the living class is assigned to the
!         dead class that contains most of the living class.
JSIZ = 1
DO 215 ISIZ= 1,NSCL
  IF (UPSIZ(ISIZ) .LE. 3) THEN
     L2D(ISIZ)= 0
  ELSE
    DIF = (UPSIZ(ISIZ) - WPSIZ(JSIZ)) + &
                            (UPSIZ(ISIZ-1) - WPSIZ(JSIZ))

    IF (UPSIZ(ISIZ) .LE. WPSIZ(JSIZ)) THEN
        L2D(ISIZ)= JSIZ
    ELSEIF (DIF .LE. 0.0) THEN
        L2D(ISIZ) = JSIZ
    ELSE
        JSIZ = JSIZ + 1
        L2D(ISIZ) = JSIZ
    ENDIF
  ENDIF
215 CONTINUE

!     These decay rates come from values in the notes for the fire model
!     Standing dead wood, small and large decay rates: Small is <3, large is everything else

SDECRT(1) = EXP(-.08)
SDECRT(2) = EXP(-.05)

!     Downed dead wood, small and large decay rates:

DDECRT(1) = EXP(-.06)
DDECRT(2) = EXP(-.03)


!     OTHER PARAMETER INITIALIZATIONS

DENS= .002       !LIGHTNING DENSITY

!     Number of years prior to first master cycle, from which any
!     inventoried mountain pine beetle records will be used to
!     initialize the run. By default, inventory will NOT be used.

DEDYRS = 2
LINVON = .FALSE.

!     Root Disease and Stem Rust default spread rates.
!     Changed default condition; they are now OFF by default.  3/00 ajm

BMRRSR = 0.10
BMSRSR = 0.001
!      LRRON = .TRUE.
!      LSRON = .TRUE.
LRRON = .FALSE.
LSRON = .FALSE.

! Windthrow default values: values can be changed by Keyword.

WTHRSH= 1.0
CRASH= 0.4

! Drought model initializations

WASDRY = .FALSE.

!     OUTSIDE WORLD initialization.

!     Attractiveness constant for outside world begins by being "not done"
ACDone= .FALSE.
!     outside world is not floating
UFLOAT   = 1
URMAX(1) = 15
URMAX(2) = 15
URMAX(3) = 15
STOCKO   = 100
RVOND    = 1.0
RVOD     = 1.0
!     outside world is on
OUTOFF = .FALSE.

CBAO      = -1
CRVOND    = -1
CBKPO(1)  = -1
CBAHO(1)  = -1
CBASPO(1) = -1
CSPO(1)   = -1
CBKPO(2)  = -1
CBAHO(2)  = -1
CBASPO(2) = -1
CSPO(2)   = -1


!     These are attractiveness parameters that could have different
!     defaults for each beetle type.
DO 201 I= 1,3
!     "C" in the attractiveness eqn. (field 4 in keyword ATTRACT)
!...CHANGED DEFAULT 3/00 ajm
   USERC(I) = 100.0
!       USERC(I) = 1.0
!     "n" in the attractiveness eqn. (field 5)
   SELFA(I) = 1.0
!     "a" in attractiveness eqn. (field 3)
   USERA(I) = 1.0
201 CONTINUE
!
!     OUTPUT UNITS

JBMBPR= JOPRT  ! MAIN FVS OUTPUT UNIT FOR DEBUG OUTPUT

JBMCYC= 25     !ADDED 8/05 AJM REPLACES JBMVOL
JBMLPY= 29     ! LANDSCAPE AVERAGE OUTPUT FILE

JBMAIN= 26     !MAIN DETAILED--DBS ENABLED
JBMTRE= 27     !TREE x SC DETAILED ---DBS ENABLED
JBMBKP= 28     !BKP DETAILED  - DBS ENABLED
JBMVOL= 30     !VOL x SC DETAILED--DBS ENABLED
!
!     OUTPUT LOGICALS  SET TO TRUE BY KEYWORD IF USER REQUESTS OUTPUT

LBMDEB= .FALSE.

LBMCYC= .FALSE.  ! REPLACES LBMVOL
LBMLPY= .FALSE.  ! LANDSCAPE AVERAGE OUTPUT FILE
!
LBMAIN= .FALSE.
LBMTRE= .FALSE.
LBMBKP= .FALSE.
LBMVOL= .FALSE.

!     OUTPUT FILE HEADERS (SET TO TRUE WHEN HEADER IS PRINTED)

LPRHDBML=.FALSE. ! BMLandscape (bml)
LPRHDBMC=.FALSE. ! BMCycle     (bmc)
LPRHD1=.FALSE.   ! BMMAIN      (bmm)
LPRHD2=.FALSE.   ! BMTREE      (bmt)
LPRHD3=.FALSE.   ! BMBMK       (bmb)
LPRHD4=.FALSE.   ! BMVOL       (bmv)

LBMSPR= .FALSE.  ! USED IN BMSETP. TRUE WHEN LOC/AREA DATA FOUND IN SPlaaR
LDBINIT=.FALSE.  !USED IN BMSETP. TRUE ONCE THE STAND-LEVEL DB-WRITING FLAGS HAVE BEEN INITIALIZED
LBMSETP=.FALSE.
BMSTND= 0
NSSTND= 0
IBMYR1= 0
IBMYR2= 0
IBMYRT= 0
JCNT=0
LCDENS= .TRUE.

CALL DBCHK(DEBUG,'BMINIT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,99)ICYC
99 FORMAT(' End BMINIT: Cycle = ',I5)

RETURN
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc!ADD AJM 8/05
ENTRY BMLNKD(LRET)
!----------
!  RETURNS TRUE IF THE WWPB MODEL IS LINKED
!  EXBM RETURNS FALSE.
!----------
LRET = .TRUE.
RETURN
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
END

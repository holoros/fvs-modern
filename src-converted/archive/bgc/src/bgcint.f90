SUBROUTINE BGCINT
!----------
!  **BGCINIT  BGC -- DATE OF LAST REVISION: 5/15/00
!                 Revised 11/12/02.  Removing index ISTND, and removing PPE
!                                    common "includes" (PPEPRM, PPCNTL, &
!                                    PRGPRM).  AJM
!                 These changes--also made in BGCFVS, BGCGROW, BGCIN, BGCGO,
!                 BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
!                 The FVS-BGC code is now, once again, a single stand model.
!----------
!
!    BGCINIT IS CALLED FROM INITRE TO SIMPLY SET LBGCON TO FALSE...
!    WHEN THE FIRST BGC KEYWORD IS USED, BGCIN CALLES BGCINITALL
!    TO SET BGC DEFAULT VALUES FOR PARAMETERS, SITE, AND CONTROL VARIABLES
!    NEEDED TO EXECUTE MODEL.
!
!OMMONS
!
!
INCLUDE 'BGCCOM.f90'
INCLUDE 'SITE.f90'
INCLUDE 'ENTITY.f90'
!      INCLUDE 'PRGPRM.F77'                             ! removed ajm 11/02
!      INCLUDE 'PPCNTL.F77'                             ! removed ajm 11/02
!
!
!OMMONS
!
!      LBGCON(ISTND) = .FALSE.                          ! removed ajm 11/02
LBGCON = .FALSE.
RETURN

ENTRY BGCINITALL
!
!     INITIALIZE THE MAJORITY OF THE BGC STATE VARIALBES HERE.
PRINT *,'In BgcInitall, AT OPEN STATEMENTS'
OPEN(60,FILE='CLIMATE.CLM',STATUS='OLD')
OPEN(61,FILE='BETA.DAT',STATUS='OLD')
OPEN(62,FILE='SITE.DAT',STATUS='OLD')
open(63,file='siteout.dat',status='unknown')
!----------------------------------------------------------------------
!  DEFAULT SITE ATTRIBUTES. READ FROM FILE *SITE.DAT* AND CALCULATED
!  IN SUBROUTINE **SOILH2O**
!----------------------------------------------------------------------
READ(62,'(F10.5)',END=5) (S(L),L=1,16)
5 write(63,7) (s(l),l=1,16)
!***************************************************************************
! Am changing format of siteout.dat output file.  11/02 ajm
!  And again 6/03 ajm

!    7 format (4(f6.1,4x),f8.6,2x,11(f6.1,4x))

7 format('Initial SWC = ',    T18, f6.1,/ &
          'Soil depth = ',     T18, f6.1,/ &
          'Max VWC = ',        T18, f6.1,/ &
          'Initial snow = '    T18, f6.1,/ &
          'Snowmelt coeff = ', T21, f8.6,/ &
          'Albedo = ',         T18, f6.1,/ &
          'Crn Zn Flag = ',    T18, f6.1,/ &
          'Crn Zn Depth = ',   T18, f6.1,/ &
          '% Bare Grnd = ',    T18, f6.1,/ &
          'NOT USED',          T18, f6.1,/ &
          'Start Yr = ',       T18, f6.1,/ &
          '% Sand = ',         T18, f6.1,/ &
          '% Silt = ',         T18, f6.1,/ &
          '% Clay = ',         T18, f6.1,/ &
          'PPT Multiplier = ', T18, f6.2,/ &
          'PSN Multiplier = ', T18, f6.2)
!***************************************************************************
!      print *, 'in bgcinitall', 's(3)= ',s(3),'LBGCON=',LBGCON(ISTND) !removed 11/02 ajm
print *, 'in bgcinitall', 's(3)= ',s(3),'LBGCON=',LBGCON
!      IF(S(3).EQ.0.0) THEN  !if maximum volumetric water content = 0
  CALL SOILH2O        ! then calculate from texture
!      ENDIF
! definitions and defaults read from *SITE.DAT*
!      S(1)=1450.0      !    INITIAL SOIL WATER CONTENT (m3/ha)
!      S(2)=1.0         !    SOIL DEPTH (m)  1.3
!      S(3)=0.25        !    MAX VOLUMETRIC WATER CONTENT (m3/m3) -- 0.25
!      S(4)=500.0       !    INITIAL SNOWPACK (m3/ha)
!      S(5)=0.00065     !    SNOWMELT COEFFICIENT (m/degree C/day)
!      S(6)=0.2         !    ALBEDO
!      S(7)=1.0         !    CROWN ZONE FLAG (1.=DEPTH METHOD,  0.=BY TREE)
!      S(8)=1.0         !    CROWN ZONE DEPTH (m)
!      S(9)=0.0         !    % OF HECTARE IN BARE, UNOCCUPIED GROUND
!      S(10)=0.0        !    NOT USED
!      S(11)=1.0        !    STARTING YEAR (USE 1.)
!      S(12)=40.0       !    PERCENT SAND    40  55
!      S(13)=40.0       !    PERCENT SILT  !NOT CURRENTLY USED 40  15
!      S(14)=20.0       !    PERCENT CLAY 20  30
!      S(15)=1.0        !    PPT MULTIPLIER VARIABLE, SET AT 1.0 WHEN NOT USED
!      S(16)=1.0        !    PSN MULTIPLIER FOR SCALING
!----------------------------------------------------------------------
!  DEFAULT PHYSIOLOGICAL PARAMETERS IN EXTERNAL FILE *BETA.DAT*
!----------------------------------------------------------------------
READ(61,10, END=15) ((B1(I,J),J=1,3),I=1,13) ! life form specific
10 FORMAT(3(F8.4,1X))
15 READ(61,'(F8.0)',END=20) (B2(K),K=1,22)     ! common to all lifeforms
!
! definitions and default values from *BETA.DAT*
!
!  000.0016,000.0016,000.006  B1(1)    MAX LEAF CONDUCTANCE (m/s) B(7) .0016,.0025,.005
!   0.5   ,  0.5   ,  0.5     B1(2)    MINIMUM LWP (-MPa) B(8)
!    .1   ,   .1   ,   .01    B1(3)    BOUNDARY LAYER CONDUCTANCE (m/s) B(11)
!    .0002,   .0004,   .0044  B1(4)    LEAF RESP. (kgC/day at 0 C) B(13) .0002,.0004,.0004
!    .0002,   .0002,   .0044  B1(5)    STEM RESP. (kgC/day at 0 C) B(14) .0002
!    .0002,   .0002,   .0003  B1(6)    COARSE ROOT RESP. (kgC/x(10)/day at 0 C) B(15) .0002
!    .0004,   .0011,   .0044  B1(7)    FINE ROOT RESP. (kgC/x(11)/day at 0 C) B(16)  .0004
!   4.0   ,  4.0   ,  6.0     B1(8)    MAX PSN RATE (umol/m2/s), 0 from leaf N B(20) 10
!  33.0   ,100.0   ,100.0     B1(9)    LEAF TURNOVER (%/yr) B(44)
!   0.0   , 20.0   , 98.0     B1(10)   STEM TURNOVER (%/yr) B(45) 2.0,
!  40.0   , 80.0   , 50.0     B1(11)   ROOT TURNOVER (%/yr) B(47) 40
!  33.0   , 18.0   , 17.0     B1(12)   LEAF LIGNIN CONCENTRATION (%) B(77)
!  25.0   , 35.0   , 25.0     B1(13)   SPECIFIC LEAF AREA (m2/kgC)
! 0.0005         B2(1)    INTERCEPTION COEFF. (m/lai/day) B(4)
! -0.5           B2(2)    CANOPY LIGHT EXT. COEFF. B(5)
! 1.65           B2(3)    LWP AT STOMATAL CLOSURE (-MPa) B(9)
! 25.0           B2(4)    VPD AT STOMATAL CLOSURE (mbar) B(10)
! 20.0           B2(5)    OPTIMUM TEMP PSN (degree C) B(21) 17,23
! 45.0           B2(6)    MAX TEMP. PSN (degree C) B(22)
! 0.35           B2(7)    LEAF GROWTH RESP. FRACTION B(40)
! 0.30           B2(8)    STEM GROWTH RESP. FRACTION B(41) 0.30
! 0.30           B2(9)    COARSE ROOT GROWTH RESP. FRACTION B(42)
! 0.35           B2(10)   FINE ROOT GROWTH RESP. FRACTION B(43)
! 0.15           B2(11)   LEAF CARBON ALLOC. FRACTION B(52)  .15
! 0.35           B2(12)   STEM CARBON ALLOC. FRACTION B(53) .45
! 0.05           B2(13)   COARSE ROOT CARBON ALLOC. FRACTION B(54)
! 0.50           B2(14)   FINE ROOT CARBON ALLOC. FRACTION B(55) .40
! 2.3            B2(15)   RATIO ALL-SIDED LAI TO 1-SIDED LAI B(3)
! 0.01           B2(16)   SLOPE OF GS vs PAR ( (mm/s) / (uE/m2/s) ) B(6)
! 0.069          B2(17)   COEFF. FOR MAINTENANCE RESP. (Q10 2.0) B(17)
! 0.50           B2(18)   FRACTION OF C IN DRY MATTER (kgC/kg drywt) B(38)
! 0.66           B2(19)   MAX RATIO OF LEAFC/(LEAFC + FINE_ROOTC)   B(68)
! 0.19           B2(20)   WATER STRESS INTEGRAL FRACTION    B(78)=.17
! 0.85           B2(21)   STEM/COARSE ROOT ALLOCATION RATIO B(69)=0.85
! 0.25           B2(22)   FRACTION OF BRANCHES IN TOTAL STEM CARBON
!
!----------------------------------------------------------------------
!  DEFAULT BGC VEG CONTROL PARAMETERS
!----------------------------------------------------------------------
20 CONTINUE
THINYR=0.0         ! no thinning
PTREMOVE=1.0
PSREMOVE=1.0
PGREMOVE=1.0
!
!----------------------------------------------------------------------
!  LEAVE CLIMATE IN EXTERNAL FILE FOR NOW
!----------------------------------------------------------------------
!
!  Initialization of flag IBGC added here AJM 8/25/00.  If BGC increments
!  will be used in FVS, this flag will be set to 1 in BGCIN (this
!  subroutine's calling subroutine.)
!      IBGC(ISTND)=0                                    ! removed 11/02 ajm
IBGC=0
RETURN
END
!----------------------------------------------------------------------
SUBROUTINE SOILH2O
!----------------------------
!  calculates maximum volumetric soil water content from sand, silt
!  and clay fractions.  Maximum volumetric content can also be read
!  in input. Called once for a site.
!    PSAND=percent sand.     S(12) in BIOME-BGC
!    PSILT=percent silt.     S(13) in BIOME-BGC (NOT USED)
!    PCLAY=percent clay.     S(14) in BIOME-BGC
!    VOLMAX=maximum volumetric water content. S(3) in BIOME=BGC
!    AA and BB are texture coefficients
!----------------------------
REAL PSAND, PCLAY
INCLUDE 'SITE.f90'

PSAND=S(12)
PCLAY=S(14)
!     VOLMAX=S(3)
!  calculate texture coefficients
AA=100.*EXP(-4.396-0.0715*PCLAY-4.880E-4*PSAND**2. &
              -4.285E-5*PSAND**2.*PCLAY)
BB=-3.14-0.00222*PCLAY**2.-3.484E-5*PSAND**2.*PCLAY
!  calculate maximum volumetric water content.
!   *** Am changing the algorithm.  From personal communication w/ K Milner
!   I understand that the VOLMAX=EXP((LOG(33.)-AA)/BB) eqn is from Hunt.
!   It appears to over estimate VOLMAX; and while it uses the AA and BB parameters
!   from Saxton et al 1986, the eqn itself is not from there.  I am now going to
!   insert the actual Saxton eqn for VOLMAX which does not use AA and BB.  [AA and BB
!   are texture dependent, and are used in the SWP calculations (as well as in the
!   newly-inserted SWC algorithm in BWATER which reduces SWC to field capacity after
!   one day at saturation)].  AJM 5/01
!
!       VOLMAX=EXP((LOG(33.)-AA)/BB)
 VOLMAX = 0.332 - 7.251E-4 * PSAND + 0.1276 * LOG10(PCLAY)
 IF(S(3).EQ.0.0) S(3)=VOLMAX
 print *, 'in soilh2o','volmax= ',volmax
!      WRITE(*,*) 'WATER: ',AA,BB,VOLMAX
RETURN
END


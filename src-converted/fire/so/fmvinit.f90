SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-SO $Id$
!----------
!  Purpose:
!      Initialize variant-specific variables for the Fire Model
!----------------------------------------------------------------------
!
!  Called from: INITRE
!
!  Call list definitions:
!
!  Local variable definitions:
!
!**********************************************************************
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'

INTEGER I,J

LVWEST    = .TRUE.  ! WESTERN VARIANT

!
!     LEAF RETENTION (YEARS).
!
LEAFLF(1)  = 4.0          ! White pine
LEAFLF(2)  = 3.0          ! Sugar pine
LEAFLF(3)  = 5.0          ! Douglas-fir
LEAFLF(4)  = 7.0          ! White fir
LEAFLF(5)  = 4.0          ! Mountain hemlock
LEAFLF(6)  = 5.0          ! Incense cedar
LEAFLF(7)  = 3.0          ! Lodgepole pine
LEAFLF(8)  = 6.0          ! Englemann spruce
LEAFLF(9)  = 7.0          ! Red fir
LEAFLF(10) = 4.0          ! Ponderosa pine
LEAFLF(11) = 4.0          ! Western juniper
LEAFLF(12) = 7.0          ! Grand fir
LEAFLF(13) = 7.0          ! Subalpine fir
LEAFLF(14) = 7.0          ! Pacific silver fir
LEAFLF(15) = 7.0          ! Noble fir
LEAFLF(16) = 3.0          ! Whitebark pine
LEAFLF(17) = 1.0          ! Western larch
LEAFLF(18) = 5.0          ! Western redcedar
LEAFLF(19) = 5.0          ! Western hemlock
LEAFLF(20) = 7.0          ! Pacific yew
LEAFLF(21) = 1.0          ! White alder
LEAFLF(22) = 1.0          ! Red alder
LEAFLF(23) = 1.0          ! Bigleaf maple
LEAFLF(24) = 1.0          ! Quaking aspen
LEAFLF(25) = 1.0          ! Black cottonwood
LEAFLF(26) = 1.0          ! Bitter cherry
LEAFLF(27) = 1.0          ! Oregon white oak
LEAFLF(28) = 1.0          ! Willow
LEAFLF(29) = 1.0          ! Giant chinkapin
LEAFLF(30) = 1.0          ! Curl-leaf mt. mahogany
LEAFLF(31) = 1.0          ! Birch-leaf mt. mahogany
LEAFLF(32) = 5.0          ! Other softwoods
LEAFLF(33) = 1.0          ! Other hardwoods
!
CANCLS(1)=5.0
CANCLS(2)=17.5
CANCLS(3)=37.5
CANCLS(4)=75.0

CORFAC(1)=0.5
CORFAC(2)=0.3
CORFAC(3)=0.2
CORFAC(4)=0.1

SNPRCL(1)=0
SNPRCL(2)=12
SNPRCL(3)=18
SNPRCL(4)=24
SNPRCL(5)=30
SNPRCL(6)=36

LOWDBH(1)=0.0
LOWDBH(2)=5.0
LOWDBH(3)=10.0
LOWDBH(4)=20.0
LOWDBH(5)=30.0
LOWDBH(6)=40.0
LOWDBH(7)=50.0
!----------
!  SET POTENTIAL FIRE TEMPERATURES AND WINDSPEEDS
!----------
PREWND(1)=20.
PREWND(2)=6.
POTEMP(1)=70.
POTEMP(2)=70.
!
!     LOG MINERALIZATION RATES, BASED ON HARMON ET AL. (DATE?)
!     ADV. ECOL. RES. 15. THERE ARE FOUR DEFAULT RATES, AND EACH OF
!     THOSE SPECIES TAKES ONE OF THE FOUR RATES. THE ABSOLUTE RATES
!     ARE ALSO MAPPED DIRECTLY ONTO EACH OF THE 1-6 SIZE CATEGORIES.
!     THIS IS MORE COMPLICATED THAN NECESSARY, BUT ALLOWS SIZE-DEPENDENT
!     RATES TO BE ADDED EASILY.
!
!sb      DKRDEF(1)=.007
!sb      DKRDEF(2)=.010
!sb      DKRDEF(3)=.011
!sb      DKRDEF(4)=.012
!
!     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!
DKRCLS(1)=1  ! White pine
DKRCLS(2)=1  ! Sugar pine
DKRCLS(3)=1  ! Douglas-fir
DKRCLS(4)=3  ! White fir
DKRCLS(5)=2  ! Mountain hemlock
DKRCLS(6)=1  ! Incense cedar
DKRCLS(7)=2  ! Lodgepole pine
DKRCLS(8)=2  ! Englemann spruce
DKRCLS(9)=3  ! Red fir
DKRCLS(10)=3 ! Ponderosa pine
DKRCLS(11)=1 ! Western juniper
DKRCLS(12)=3 ! Grand fir
DKRCLS(13)=3 ! Subalpine fir
DKRCLS(14)=3 ! Pacific silver fir
DKRCLS(15)=3 ! Noble fir
DKRCLS(16)=1 ! Whitebark pine
DKRCLS(17)=1 ! Western larch
DKRCLS(18)=1 ! Western redcedar
DKRCLS(19)=2 ! Western hemlock
DKRCLS(20)=1 ! Pacific yew
DKRCLS(21)=4 ! White alder
DKRCLS(22)=4 ! Red alder
DKRCLS(23)=4 ! Bigleaf maple
DKRCLS(24)=4 ! Quaking aspen
DKRCLS(25)=4 ! Black cottonwood
DKRCLS(26)=4 ! Bitter cherry
DKRCLS(27)=3 ! Oregon white oak
DKRCLS(28)=4 ! Willow
DKRCLS(29)=3 ! Giant chinkapin
DKRCLS(30)=4 ! Curl-leaf mt. mahogany
DKRCLS(31)=4 ! Birch-leaf mt. mahogany
DKRCLS(32)=1 ! Other softwoods
DKRCLS(33)=4 ! Other hardwoods
!
!     SET DECAY VARIABLES TO AN UNSET STATE
!
!     IN SO-FFE VARIANT DECAY RATE DEFAULTS ARE ASSIGNED IN CYCLE 1
!     IN **FMCBA**
!
DO I = 1,MXFLCL
  DO J = 1,4
    DKR(I,J)    = -1.
  ENDDO
ENDDO
DO I = 1,MXFLCL
  DO J = 1,4
    PRDUFF(I,J) = -1.
    TODUFF(I,J) =  0.
  ENDDO
ENDDO
!
!     SET THE CONVERSION FROM CUFT TO TONS.
!     THE DATA IS IN LB/CUFT, BY SPECIES (AS GIVEN BELOW)
!     SOURCE: WOOD HANDBOOK 1999
!
V2T(1)  = 22.5 ! White pine
V2T(2)  = 21.2 ! Sugar pine
V2T(3)  = 28.7 ! Douglas-fir
V2T(4)  = 23.1 ! White fir
V2T(5)  = 26.2 ! Mountain hemlock
V2T(6)  = 21.8 ! Incense cedar
V2T(7)  = 23.7 ! Lodgepole pine
V2T(8)  = 20.6 ! Englemann spruce
V2T(9)  = 22.5 ! Red fir
V2T(10) = 23.7 ! Ponderosa pine
V2T(11) = 34.9 ! Western juniper
V2T(12) = 21.8 ! Grand fir
V2T(13) = 19.3 ! Subalpine fir
V2T(14) = 24.9 ! Pacific silver fir
V2T(15) = 23.1 ! Noble fir
V2T(16) = 22.5 ! Whitebark pine
V2T(17) = 29.9 ! Western larch
V2T(18) = 19.3 ! Western redcedar
V2T(19) = 26.2 ! Western hemlock
V2T(20) = 26.2 ! Pacific yew
V2T(21) = 23.1 ! White alder
V2T(22) = 23.1 ! Red alder
V2T(23) = 27.4 ! Bigleaf maple
V2T(24) = 21.8 ! Quaking aspen
V2T(25) = 19.3 ! Black cottonwood
V2T(26) = 29.3 ! Bitter cherry
V2T(27) = 37.4 ! Oregon white oak
V2T(28) = 22.5 ! Willow
V2T(29) = 36.2 ! Giant chinkapin
V2T(30) = 21.8 ! Curl-leaf mt. mahogany
V2T(31) = 21.8 ! Birch-leaf mt. mahogany
V2T(32) = 28.7 ! Other softwoods
V2T(33) = 21.8 ! Other hardwoods
!
!     NOW CONVERT V2T SO THAT IT WILL TAKE THE CUFT INTO TONS.
!
DO I = 1,MAXSP
  V2T(I) = V2T(I) / 2000.0
ENDDO
!
!     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
!     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
!
NZERO  = 0.01
LIMBRK = 0.01
HTXSFT = 1.0
HTR1   = 0.03406
HTR2   = 0.01
DO I= 1,MAXSP
  PSOFT(I)  =  0.0
ENDDO
!
!     SET SNAG VARIABLES TO AN UNSET STATE
!
!     IN SO-FFE VARIANT DECAY SNAG DEFAULTS ARE ASSIGNED IN CYCLE 1
!     IN **FMCBA**
!
DO I = 1,MAXSP
  ALLDWN(I) = -1.0
  DECAYX(I) = -1.0
  FALLX(I)  = -1.0
  DO J= 1,4
    HTX(I,J) =  -1.0
  ENDDO
  LSW(I) = .FALSE.
ENDDO
!
!     PARAMETERS FOR POST-BURN SNAG FALL RATES:
!     THOSE SET TO -1 ARE SET IN FMCBA INSTEAD.
!
PBSCOR =  0.0
PBSOFT = -1.0
PBSMAL = -1.0
PBSIZE = 12.0
PBTIME =  7.0
!
!     THE LODGEPOLE PINE COVER TYPE METAGROUP (NOT USED IN SO-FFE)
!
OLDICT = 0
!
!     DROUGHT START AND END YEARS
!
IDRYB  = 0
IDRYE  = 0
!
!     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
!
SLCRIT = 10.0

RETURN
END

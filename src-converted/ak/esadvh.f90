SUBROUTINE ESADVH (I,HHT,DELAY,GENTIM,TRAGE,JOSTND,DEBUG)
IMPLICIT NONE
!----------
! AK $Id$
!----------
!     CALCULATES HEIGHTS OF ADVANCE TREES CREATED BY THE
!     ESTABLISHMENT MODEL.
!     ALL SPECIES EXCEPT OTHER SOFTWOODS AND OTHER HARDWOODS USE
!     ORIGINAL EQUATION TO ESTIMATE HEIGHT BASED ON TIME AND A
!     RANDOM NUMBER.
!     OTHER SOFTWOODS AND OTHER HARDWOODS SET TO MINIMUM HEIGHT.
!----------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'ESPARM.f90'

INCLUDE 'ESCOM2.f90'

INCLUDE 'ESCOMN.f90'

INCLUDE 'ESHAP.f90'
!
!OMMONS
!
!----------
!  VARIABLE DEFINITIONS:
!----------
! SPECIES LIST FOR ALASKA VARIANT.
!
! Number Code  Common Name         FIA  PLANTS Scientific Name
!   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
!   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
!   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
!   4     TA   tamarack            071  LALA   Larix laricina
!   5     WS   white spruce        094  PIGL   Picea glauca
!   6     LS   Lutz�s spruce            PILU   Picea lutzii
!   7     BE   black spruce        095  PIMA   Picea mariana
!   8     SS   Sitka spruce        098  PISI   Picea sitchensis
!   9     LP   lodgepole pine      108  PICO   Pinus contorta
!  10     RC   western redcedar    242  THPL   Thuja plicata
!  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
!  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
!  13     OS   other softwoods     298  2TN
!  14     AD   alder species       350  ALNUS  Alnus species
!  15     RA   red alder           351  ALRU2  Alnus rubra
!  16     PB   paper birch         375  BEPA   Betula papyrifera
!  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
!  18     BA   balsam poplar       741  POBA2  Populus balsamifera
!  19     AS   quaking aspen       746  POTR5  Populus tremuloides
!  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
!  21     WI   willow species      920  SALIX  Salix species
!  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
!  23     OH   other hardwoods     998  2TB
!
!  AGE      -- AGE OF BEST TREE
!    I      -- SPECIES NUMBER
!   BB      -- INTERMEDIATE VARIABLE
!    X      -- RANDOM NUMBER DRAW
!  HHT      -- HEIGHT OF BEST TREE
!----------
!  VARIABLE DECLARATIONS:
!----------
!
LOGICAL DEBUG
!
INTEGER I,N,JOSTND,MODE1
!
REAL AGE,DELAY,GENTIM,HHT,TRAGE
REAL H,HTGR,HTMAX
!
!----------
N = INT(DELAY+0.5)
IF(N.GT.2) N=1
DELAY=REAL(N)
TRAGE=3.0-DELAY
AGE=3.0-DELAY-GENTIM
IF(AGE.LT.1.0) AGE=1.0

!  INITALIZE VARIABLES NEEDED FOR HTCALC
MODE1= 1
HTGR=0.0
H=0.0
HTMAX=0.0

!  CALL HTCALC TO RETRIEVE A HEIGHT BASED ON INPUT AGE AND SITE INDEX
!  SITE INDEX IS EXTRACTED WITHIN HTCALC
CALL HTCALC(MODE1,I,AGE,H,HTMAX,HTGR,JOSTND,DEBUG)
HHT=H

IF(DEBUG)WRITE(JOSTND,*)' IN ESADVH',' DELAY=',DELAY,' N=',N, &
    ' TRAGE=',TRAGE,' GENTIM=',GENTIM,' AGE=',AGE,' TIME=',TIME, &
    ' HHT=',HHT

!----------
!  HEIGHTS TO TALL, TEMPORARY FIX 12-21-20 MC.
!  SET HTT TO VALUE IN XMIN
!----------
IF(HHT.LT.XMIN(I))HHT=XMIN(I)
RETURN
END

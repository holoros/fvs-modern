SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-CS $Id$
!----------
!  PURPOSE:
!      INITIALIZE VARIANT-SPECIFIC VARIABLES FOR THE FIRE MODEL
!----------
!  CALLED FROM: FMINIT
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
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'FMFCOM.f90'
!
!
!OMMONS
!
INTEGER I,J,TFALLCLS(MAXSP),SNAGCLS(MAXSP)

LVWEST    = .FALSE.  ! EASTERN VARIANT

!
CANCLS(1) =  5.0
CANCLS(2) = 17.5
CANCLS(3) = 37.5
CANCLS(4) = 75.0
!
CORFAC(1) =  0.5
CORFAC(2) =  0.3
CORFAC(3) =  0.2
CORFAC(4) =  0.1
!
SNPRCL(1) =  0
SNPRCL(2) = 12
SNPRCL(3) = 18
SNPRCL(4) = 24
SNPRCL(5) = 30
SNPRCL(6) = 36
!
LOWDBH(1) =  0.0
LOWDBH(2) =  5.0
LOWDBH(3) = 10.0
LOWDBH(4) = 20.0
LOWDBH(5) = 30.0
LOWDBH(6) = 40.0
LOWDBH(7) = 50.0
!----------
!  SET POTENTIAL FIRE TEMPERATURES AND WINDSPEEDS
!----------
PREWND(1)=20.
PREWND(2)=8.
POTEMP(1)=70.
POTEMP(2)=60.
!----------
!  DECAY RATES FROM ABBOTT AND CROSSLEY / BARBER AND VANLEAR
!----------
DKR(1,1)  = 0.11
DKR(2,1)  = 0.11
DKR(3,1)  = 0.09
DKR(4,1)  = 0.07
DKR(5,1)  = 0.07
DKR(6,1)  = 0.07
DKR(7,1)  = 0.07
DKR(8,1)  = 0.07
DKR(9,1)  = 0.07
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1)
  ENDDO
ENDDO
!----------
!  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
!  LITTER RATES FROM SHARPE ET AL. AND WITKAMP
!----------
DO J = 1,4
  DKR(10,J) = 0.65
  DKR(11,J) = 0.002
ENDDO
!----------
!  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
!  DECAY RATE: 'DKR'.
!----------
DO I = 1,MXFLCL
  DO J = 1,4
    PRDUFF(I,J) = 0.02
    TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
  ENDDO
ENDDO
!----------
!  SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
!  ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
!
!  NO SNAG HEIGHT LOSS
!----------
NZERO  =  0.01
LIMBRK =  0.01
HTXSFT =  2.0
HTR1   =  0.01  !from bm fmvinit, the way to get 0 ht. loss is
HTR2   =  0.01  !to set these low (can't be 0) and set htx to 0
DO I= 1,MAXSP
  PSOFT(I) = 0.0
ENDDO
!----------
!  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
!----------
!  V2T() - DERIVED BY USING TABLE 4-3A OF THE
!  'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.
!  AS WELL AS 'HARDWOODS OF NORTH AMERICA' 1995. FPL-GTR-83 AND
!  JENKINS ET. AL. 2004. GTR-NE-319.
!
!  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES - TAKEN FROM SN-FFE
!  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
!  OF DENDROLOGY, 9TH EDITION
!  EXCEPTIONS:
!    EASTERN REDCEDAR FOUND IN MICHIGAN TREES, BARNES AND WAGNER
!
!  TFALL() - TIME TO FALL FOR CROWN COMPONENTS;
!  CATEGORIES ARE
!  0 -  FOLIAGE
!  1 -   0 - 0.25 IN
!  2 -   0.25 - 1.00 IN
!  3 -   1.00 - 3.00 IN
!  4 -   3.00 - 6.00 IN
!  5 -   6.00 - 12.00 IN
!
!  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS, AS DEFINED
!  AT THE SN-FFE WORKSHOP.  SPECIES NOT IN SN-FFE WERE CLASSES AS 5,
!  EXCEPT OSAGE-ORANGE, WHICH WAS SET TO 4.
!
!  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
!
!  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
!  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE
!  VARIABLES FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
!  1 - SPECIES DECAYS AND FALLS FASTER THAN AVERAGE
!  2 - SPECIES DECAYS AND FALLS AT AN AVERAGE RATE
!  3 - SPECIES DECAYS AND FALLS SLOWER THAN AVERAGE
!  SNAGCLS WAS TAKEN FROM SN-FFE.
!  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLES:
!  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
!  DECAYX() - DECAY RATE MULTIPLIER
!  FALLX()  - FALL RATE MULTIPLIER
!  HTX()    - HEIGHT-LOSS RATE MULTIPLIER
!
!  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!
!  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
!  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
!  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
!  YOUNG GROWTH WAS ASSUMED.
!  SPECIES NOT LISTED IN THE BOOK WERE ASSIGNED BASED ON INFO
!  PROVIDED AT THE SN WORKSHOP
!----------
DO I= 1,MAXSP
!
  SELECT CASE (I)
!----------
!  EASTERN REDCEDAR
!----------
    CASE (1)
      V2T(I)     =  27.4
      TFALLCLS(I) = 1
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  JUNIPER SPECIES
!----------
    CASE (2)
      V2T(I)     =  27.4
      TFALLCLS(I) = 1
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SHORTLEAF PINE
!----------
    CASE (3)
      V2T(I)     =  29.3
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  VIRGINIA PINE
!----------
    CASE (4)
      V2T(I)     =  28.1
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  LOBLOLLY PINE
!----------
    CASE (5)
      V2T(I)     =  29.3
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  OTHER SOFTWOOD SPECIES
!----------
    CASE (6)
      V2T(I)     =  21.2
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  EASTERN WHITE PINE
!----------
    CASE (7)
      V2T(I)     =  21.2
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BLACK WALNUT
!----------
    CASE (8)
      V2T(I)     =  31.8
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  BUTTERNUT
!----------
    CASE (9)
      V2T(I)     =  22.5
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  TUPELO SPECIES
!----------
    CASE (10)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SWAMP TUPELO
!----------
    CASE (11)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  WATER TUPELO
!----------
    CASE (12)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  BLACKGUM / BLACK TUPELO
!----------
    CASE (13)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SELECT HICKORY
!----------
    CASE (14)
      V2T(I)     =  39.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  SHAGBARK HICKORY
!----------
    CASE (15)
      V2T(I)     =  39.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  SHELLBARK HICKORY
!----------
    CASE (16)
      V2T(I)     =  38.7
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  MOCKERNUT HICKORY
!----------
    CASE (17)
      V2T(I)     =  39.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  PIGNUT HICKORY
!----------
    CASE (18)
      V2T(I)     =  41.2
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  HICKORY SPECIES
!----------
    CASE (19)
      V2T(I)     =  39.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  WATER HICKORY
!----------
    CASE (20)
      V2T(I)     =  38.0
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  BITTERNUT HICKORY
!----------
    CASE (21)
      V2T(I)     =  37.4
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  PECAN
!----------
    CASE (22)
      V2T(I)     =  37.4
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  BLACK HICKORY
!----------
    CASE (23)
      V2T(I)     =  39.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  AMERICAN BEECH
!----------
    CASE (24)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  BLACK ASH
!----------
    CASE (25)
      V2T(I)     =  28.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  PUMPKIN ASH
!----------
    CASE (26)
      V2T(I)     =  29.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  BLUE ASH
!----------
    CASE (27)
      V2T(I)     =  33.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  EASTERN COTTONWOOD
!----------
    CASE (28)
      V2T(I)     = 23.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  RED MAPLE
!----------
    CASE (29)
      V2T(I)     =  30.6
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  BOXELDER
!----------
    CASE (30)
      V2T(I)     =  25.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  SILVER MAPLE
!----------
    CASE (31)
      V2T(I)     =  27.4
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  BLACK CHERRY
!----------
    CASE (32)
      V2T(I)     =  29.3
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  AMERICAN ELM
!----------
    CASE (33)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SUGARBERRY
!----------
    CASE (34)
      V2T(I)     =  30.6
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  HACKBERRY
!----------
    CASE (35)
      V2T(I)     =  30.6
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  WINGED ELM
!----------
    CASE (36)
      V2T(I)     =  37.4
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  ELM SPECIES
!----------
    CASE (37)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SIBERIAN ELM
!----------
    CASE (38)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SLIPPERY ELM
!----------
    CASE (39)
      V2T(I)     =  29.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  ROCK ELM
!----------
    CASE (40)
      V2T(I)     =  35.6
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  YELLOW-POPLAR
!----------
    CASE (41)
      V2T(I)     =  24.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  AMERICAN BASSWOOD
!----------
    CASE (42)
      V2T(I)     =  20.0
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SUGAR MAPLE
!----------
    CASE (43)
      V2T(I)     =  34.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  ASH SPECIES
!----------
    CASE (44)
      V2T(I)     =  33.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  WHITE ASH
!----------
    CASE (45)
      V2T(I)     =  34.3
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  GREEN ASH
!----------
    CASE (46)
      V2T(I)     =  33.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  WHITE OAK
!----------
    CASE (47)
      V2T(I)     =  37.4
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  NORTHERN RED OAK
!----------
    CASE (48)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  SOUTHERN RED OAK
!----------
    CASE (49)
      V2T(I)     =  32.4
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  BLACK OAK
!----------
    CASE (50)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  SCARLET OAK
!----------
    CASE (51)
      V2T(I)     =  37.4
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  BLACKJACK OAK
!----------
    CASE (52)
      V2T(I)     =  34.9
      TFALLCLS(I) = 2
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  CHINKAPIN OAK
!----------
    CASE (53)
      V2T(I)     =  37.4
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SWAMP WHITE OAK
!----------
    CASE (54)
      V2T(I)     =  39.9
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  BUR OAK
!----------
    CASE (55)
      V2T(I)     =  36.2
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SWAMP CHESTNUT OAK
!----------
    CASE (56)
      V2T(I)     =  37.4
      TFALLCLS(I) = 3
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  POST OAK
!----------
    CASE (57)
      V2T(I)     =  37.4
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  DELTA POST OAK
!----------
    CASE (58)
      V2T(I)     =  37.4
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  CHESTNUT OAK
!----------
    CASE (59)
      V2T(I)     =  35.6
      TFALLCLS(I) = 3
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  PIN OAK
!----------
    CASE (60)
      V2T(I)     =  36.2
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  CHERRYBARK OAK
!----------
    CASE (61)
      V2T(I)     =  38.0
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  SHINGLE OAK
!----------
    CASE (62)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  OVERCUP OAK
!----------
    CASE (63)
      V2T(I)     =  35.6
      TFALLCLS(I) = 3
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  WATER OAK
!----------
    CASE (64)
      V2T(I)     =  34.9
      TFALLCLS(I) = 3
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  NUTTALL OAK
!----------
    CASE (65)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  WILLOW OAK
!----------
    CASE (66)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  SHUMARD OAK
!----------
    CASE (67)
      V2T(I)     =  34.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  OTHER UPLAND HARDWOODS
!----------
    CASE (68)
      V2T(I)     =  37.4
      TFALLCLS(I) = 2
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  SASSAFRAS
!----------
    CASE (69)
      V2T(I)     =  26.2
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  OHIO BUCKEYE
!----------
    CASE (70)
      V2T(I)     =  20.6
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  CATALPA
!----------
    CASE (71)
      V2T(I)     =  23.7
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  2
!----------
!  COMMON PERSIMMON
!----------
    CASE (72)
      V2T(I)     =  39.9
      TFALLCLS(I) = 4
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  HONEYLOCUST
!----------
    CASE (73)
      V2T(I)     =  37.4
      TFALLCLS(I) = 2
      DKRCLS(I)  =  2
      SNAGCLS(I) =  3
!----------
!  BALSAM POPLAR
!----------
    CASE (74)
      V2T(I)     =  19.3
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BIGTOOTH ASPEN
!----------
    CASE (75)
      V2T(I)     =  22.5
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  QUAKING ASPEN
!----------
    CASE (76)
      V2T(I)     =  21.8
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BLACK LOCUST
!----------
    CASE (77)
      V2T(I)     =  41.2
      TFALLCLS(I) = 2
      DKRCLS(I)  =  1
      SNAGCLS(I) =  3
!----------
!  OTHER LOWLAND SPECIES
!----------
    CASE (78)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  SYCAMORE
!----------
    CASE (79)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  BALDCYPRESS
!----------
    CASE (80)
      V2T(I)     =  26.2
      TFALLCLS(I) = 1
      DKRCLS(I)  =  3
      SNAGCLS(I) =  3
!----------
!  RIVER BIRCH
!----------
    CASE (81)
      V2T(I)     =  30.6
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SWEETGUM
!----------
    CASE (82)
      V2T(I)     =  28.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  WILLOW SPECIES
!----------
    CASE (83)
      V2T(I)     =  22.5
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BLACK WILLOW
!----------
    CASE (84)
      V2T(I)     =  22.5
      TFALLCLS(I) = 6
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  NON-COMMERCIAL HARDWOODS
!----------
    CASE (85)
      V2T(I)     =  39.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  AMERICAN HORNBEAM
!----------
    CASE (86)
      V2T(I)     =  36.2
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  EASTERN REDBUD
!----------
    CASE (87)
      V2T(I)     =  36.2
      TFALLCLS(I) = 5
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  FLOWERING DOGWOOD
!----------
    CASE (88)
      V2T(I)     =  39.9
      TFALLCLS(I) = 5
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  HAWTHORN SPECIES
!----------
    CASE (89)
      V2T(I)     =  38.7
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  KENTUCKY COFFEETREE
!----------
    CASE (90)
      V2T(I)     =  33.1
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  OSAGE-ORANGE
!----------
    CASE (91)
      V2T(I)     =  47.4
      TFALLCLS(I) = 4
      DKRCLS(I)  =  1
      SNAGCLS(I) =  2
!----------
!  CUCUMBERTREE
!----------
    CASE (92)
      V2T(I)     =  27.4
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  SWEETBAY
!----------
    CASE (93)
      V2T(I)     =  26.2
      TFALLCLS(I) = 4
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  MULBERRY SPECIES
!----------
    CASE (94)
      V2T(I)     =  36.8
      TFALLCLS(I) = 5
      DKRCLS(I)  =  1
      SNAGCLS(I) =  2
!----------
!  EASTERN HOPHORNBEAM
!----------
    CASE (95)
      V2T(I)     =  39.3
      TFALLCLS(I) = 4
      DKRCLS(I)  =  3
      SNAGCLS(I) =  2
!----------
!  SOURWOOD
!----------
    CASE (96)
      V2T(I)     =  31.2
      TFALLCLS(I) = 5
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!
  END SELECT
!
  SELECT CASE (I)
    CASE (1:7)
      LSW(I) = .TRUE.
    CASE DEFAULT
      LSW(I) = .FALSE.
  END SELECT
!
  SELECT CASE (I)
    CASE (1,2)
      LEAFLF(I) = 5
    CASE (3)
      LEAFLF(I) = 4
    CASE (4,5)
      LEAFLF(I) = 3
    CASE (6,7)
      LEAFLF(I) = 2
    CASE DEFAULT
      LEAFLF(I) = 1
  END SELECT
!
  IF (I .LE. 2) THEN !redcedar /juniper species
    TFALL(I,0) = 3.0  ! foliage
  ELSE
    TFALL(I,0) = 1.0
  ENDIF
!
  SELECT CASE (TFALLCLS(I))
!
    CASE (1)  ! baldcypress/redcedar group
      TFALL(I,1) = 5.0
      TFALL(I,3) = 10.0
      TFALL(I,4) = 25.0
!
    CASE (2)  ! hickory/blackjack oak group
      TFALL(I,1) = 3.0
      TFALL(I,3) = 6.0
      TFALL(I,4) = 12.0
!
    CASE (3)  ! white oak group
      TFALL(I,1) = 2.0
      TFALL(I,3) = 5.0
      TFALL(I,4) = 10.0
!
    CASE (4)  ! red oak group
      TFALL(I,1) = 1.0
      TFALL(I,3) = 4.0
      TFALL(I,4) = 8.0
!
    CASE (5)  ! ash/elm/cottonwood group
      TFALL(I,1) = 1.0
      TFALL(I,3) = 3.0
      TFALL(I,4) = 6.0
!
    CASE (6)  ! pines
      TFALL(I,1) = 1.0
      TFALL(I,3) = 2.0
      TFALL(I,4) = 4.0
!
  END SELECT
!
  TFALL(I,2) = TFALL(I,1)
  TFALL(I,5) = TFALL(I,4)
!
  SELECT CASE (SNAGCLS(I))
!
    CASE (1)  ! pines and others
              ! this group decays and falls faster than average
      DECAYX(I)  = 0.07 ! 12 inch tree is soft in 2 years
      FALLX(I)   = 7.17 ! 95% of 12-inchers are down in 3 years
      ALLDWN(I)  = 6.0
      IF ((I .GE. 3) .AND. (I .LE. 7)) THEN ! pines
        ALLDWN(I) = 50.0
      ENDIF
!
    CASE (2)  ! black oak and others
              ! this group decays and falls at average rate
      DECAYX(I)  = 0.21  ! 12 inch tree is soft in 6 years
      FALLX(I)   = 3.07  ! 95% of 12-inchers are down in 7 years
      ALLDWN(I)  = 15.0
!
    CASE (3)  ! white oak, redcedar, and others
              ! this group decays and falls slower than average
      DECAYX(I)  = 0.35  ! 12 inch tree is soft in 10 years
      FALLX(I)   = 1.96  ! 95% of 12-inchers are down in 11 years
      ALLDWN(I)  = 25.0
      IF (I .LE. 2) THEN  ! redcedar
        ALLDWN(I) = 100.0
      ENDIF
!
  END SELECT
!
  DO J= 1,4
    HTX(I,J) =   0.0 ! no height loss
  ENDDO
!----------
!  CONVERT LB/FT**3 TO TONS/FT**3
!----------
  V2T(I) = V2T(I) / 2000.0
!
ENDDO
!----------
!  PARAMETERS FOR POST-BURN SNAG FALL RATES
!----------
PBSCOR =  0.0
PBSOFT =  1.0
PBSMAL =  0.9
PBSIZE = 12.0
PBTIME =  7.0 !may need to make this shorter??
!----------
!  PARAMETERS FOR FUEL MODEL SELECTION
!  COVER TYPE
!----------
OLDICT = 0
!----------
!  DROUGHT START AND END YEARS
!----------
IDRYB  = 0
IDRYE  = 0
!----------
!  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS
!----------
SLCRIT = 10.0
!
RETURN
END

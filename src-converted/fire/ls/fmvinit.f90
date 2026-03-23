SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-LS $Id$
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
PREWND(1)=25.
PREWND(2)=15.
POTEMP(1)=80.
POTEMP(2)=50.
!----------
!  DECAY RATES - FROM ABBOTT AND CROSSLEY, ECOLOGY, 1982
!                     ALBAN AND PASTOR, CAN J FOR RES, 1993
!                     TYRRELL AND CROW, CAN J FOR RES, 1994
!----------
DKR(1,1)  = 0.11
DKR(2,1)  = 0.11
DKR(3,1)  = 0.09
DKR(4,1)  = 0.06
DKR(5,1)  = 0.06
DKR(6,1)  = 0.02
DKR(7,1)  = 0.02
DKR(8,1)  = 0.02
DKR(9,1)  = 0.02
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1)
  ENDDO
ENDDO
!----------
!  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
!  FOR LITTER DECAY - PERALA AND ALBAN SUGGESTS 0.18, BUT THIS INCLUDES
!  TWIGS, SO USED INFO FROM MELILLO (NH) INSTEAD.  THIS IS CONSISTENT WITH
!  DISCUSSIONS WITH COELI HOOVER.
!----------
DO J = 1,4
  DKR(10,J) = 0.31
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
!----------
NZERO  =  0.01
LIMBRK =  0.01
HTXSFT =  2.0
HTR1   =  0.1  ! based on LS workshop info
HTR2   =  0.01
DO I= 1,MAXSP
  PSOFT(I) = 0.0
ENDDO
!----------
!  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
!----------
!  V2T() - DERIVED BY USING THE SPECIFIC GRAVITY VALUES IN TABLE 4-3A IN
!  THE 'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.
!
!  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
!  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
!  OF DENDROLOGY, 9TH EDITION (EASTERN REDCEDAR AND NORTHERN WHITE CEDAR
!  FOUND IN MICHIGAN TREES, BARNES AND WAGNER)
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
!  TFALL IS ASSIGNED FOR 4 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS FROM
!  THE SN VARIANT, SINCE WE FORGOT TO DISCUSS IT AT THE LS WORKSHOP.  SOME
!  CHANGES WERE MADE BASED ON THE RELATIVE FALL RATES OF VARIOUS SPECIES
!  AND GENERAL LS WORKSHOP NOTES.
!
!  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
!  SNAGCLS (1 - 6) FOR EACH SPECIES, AND THEN SETTING THE
!  VARIABLES FOR THE 6 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
!  1 - ASPEN, BIRCH, SPRUCE, FIR, POPLAR, BASSWOOD (FASTEST FALLERS)
!  2 - JACK PINE
!  3 - WHITE PINE
!  4 - RED PINE
!  5 - ASH, MAPLE, BEECH, ELM
!  6 - CEDAR, TAMARACK, OAK, HICKORY, HEMLOCK (SLOWEST FALLERS)
!  SPECIES WERE PUT IN SNAGCLS 5 BY DEFAULT
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
!  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN
!  WOOD HANDBOOK
!----------
DO I= 1,MAXSP
!
  SELECT CASE (I)
!----------
!  JACK PINE
!----------
    CASE (1)
      V2T(I)     =  24.9
      TFALLCLS(I) = 4
      LEAFLF(I)  =  2.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  2
!----------
!  SCOTCH PINE
!----------
    CASE (2)
      V2T(I)     =  25.6
      TFALLCLS(I) = 4
      LEAFLF(I)  =  3.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  4
!----------
!  RED PINE (NATURAL)
!----------
    CASE (3)
      V2T(I)     =  25.6
      TFALLCLS(I) = 4
      LEAFLF(I)  =  4.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  4
!----------
!  RED PINE PLANTATION
!----------
    CASE (4)
      V2T(I)     =  25.6
      TFALLCLS(I) = 4
      LEAFLF(I)  =  4.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  4
!----------
!  EASTERN WHITE PINE
!----------
    CASE (5)
      V2T(I)     =  21.2
      TFALLCLS(I) = 4
      LEAFLF(I)  =  2.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  3
!----------
!  WHITE SPRUCE
!----------
    CASE (6)
      V2T(I)     =  23.1
      TFALLCLS(I) = 4
      LEAFLF(I)  =  8.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  NORWAY SPRUCE
!----------
    CASE (7)
      V2T(I)     =  23.1
      TFALLCLS(I) = 4
      LEAFLF(I)  =  8.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BALSAM FIR
!----------
    CASE (8)
      V2T(I)     =  20.6
      TFALLCLS(I) = 4
      LEAFLF(I)  =  8.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BLACK SPRUCE
!----------
    CASE (9)
      V2T(I)     =  23.7
      TFALLCLS(I) = 4
      LEAFLF(I)  =  8.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  TAMARACK
!----------
    CASE (10)
      V2T(I)     =  30.6
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  3
      SNAGCLS(I) =  6
!----------
!  NORTHERN WHITE-CEDAR
!----------
    CASE (11)
      V2T(I)     =  18.1
      TFALLCLS(I) = 1
      LEAFLF(I)  =  2.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  EASTERN HEMLOCK
!----------
    CASE (12)
      V2T(I)     =  23.7
      TFALLCLS(I) = 1
      LEAFLF(I)  =  3.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  6
!----------
!  OTHER SOFTWOOD
!----------
    CASE (13)
      V2T(I)     =  27.4
      TFALLCLS(I) = 1
      LEAFLF(I)  = 5.0 !used eastern redcedar
      DKRCLS(I)  = 2 !used eastern redcedar
      SNAGCLS(I) = 6 !used eastern redcedar
!----------
!  EASTERN REDCEDAR
!----------
    CASE (14)
      V2T(I)     =  27.4
      TFALLCLS(I) = 1
      LEAFLF(I)  =  5.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  BLACK ASH
!----------
    CASE (15)
      V2T(I)     =  28.1
      TFALLCLS(I) = 3
      LEAFLF(I)  = 1.0
      DKRCLS(I)  = 4
      SNAGCLS(I) = 5
!----------
!  GREEN ASH
!----------
    CASE (16)
      V2T(I)     =  33.1
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  EASTERN COTTONWOOD
!----------
    CASE (17)
      V2T(I)     =  23.1
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SILVER MAPLE
!----------
    CASE (18)
      V2T(I)     =  27.4
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  RED MAPLE
!----------
    CASE (19)
      V2T(I)     =  30.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BLACK CHERRY
!----------
    CASE (20)
      V2T(I)     =  29.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  5
!----------
!  AMERICAN ELM
!----------
    CASE (21)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  SLIPPERY ELM
!----------
    CASE (22)
      V2T(I)     =  29.9
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  ROCK ELM
!----------
    CASE (23)
      V2T(I)     =  35.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  YELLOW BIRCH
!----------
    CASE (24)
      V2T(I)     =  34.3
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  AMERICAN BASSWOOD
!----------
    CASE (25)
      V2T(I)     = 20.0
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  SUGAR MAPLE
!----------
    CASE (26)
      V2T(I)     =  34.9
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BLACK MAPLE
!----------
    CASE (27)
      V2T(I)     =  32.4
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  AMERICAN BEECH
!----------
    CASE (28)
      V2T(I)     =  34.9
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  WHITE ASH
!----------
    CASE (29)
      V2T(I)     =  34.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  WHITE OAK
!----------
    CASE (30)
      V2T(I)     =  37.4
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  SWAMP WHITE OAK
!----------
    CASE (31)
      V2T(I)     =  39.9
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  BUR OAK
!----------
    CASE (32)
      V2T(I)     =  36.2
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  CHINKAPIN OAK
!----------
    CASE (33)
      V2T(I)     =  37.4
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  6
!----------
!  NORTHERN RED OAK
!----------
    CASE (34)
      V2T(I)     =  34.9
      TFALLCLS(I) = 2
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used white oak
      SNAGCLS(I) =  6
!----------
!  BLACK OAK
!----------
    CASE (35)
      V2T(I)     =  34.9
      TFALLCLS(I) = 2
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used white oak
      SNAGCLS(I) =  6
!----------
!  NORTHERN PIN OAK
!----------
    CASE (36)
      V2T(I)     =  36.2
      TFALLCLS(I) = 2
      LEAFLF(I)  = 1.0
      DKRCLS(I)  = 2 ! used white oak
      SNAGCLS(I) = 6
!----------
!  BITTERNUT HICKORY
!----------
    CASE (37)
      V2T(I)     =  37.4
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  6
!----------
!  PIGNUT HICKORY
!----------
    CASE (38)
      V2T(I)     =  41.2
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  6
!----------
!  SHAGBARK HICKORY
!----------
    CASE (39)
      V2T(I)     =  39.9
      TFALLCLS(I) = 1
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  6
!----------
!  BIGTOOTH ASPEN
!----------
    CASE (40)
      V2T(I)     =  22.5
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  QUAKING ASPEN
!----------
    CASE (41)
       V2T(I)     =  21.8
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  BALSAM POPLAR
!----------
    CASE (42)
      V2T(I)     =  19.3
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  PAPER BIRCH
!----------
    CASE (43)
      V2T(I)     =  29.9
      TFALLCLS(I) = 4
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  1
!----------
!  COMMERICAL HARDWOOD
!----------
    CASE (44)
      V2T(I)     =  31.8
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BUTTERNUT
!----------
    CASE (45)
      V2T(I)     =  22.5
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BLACK WALNUT
!----------
    CASE (46)
      V2T(I)     =  31.8
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  5
!----------
!  EASTERN HOPHORNBEAM
!----------
    CASE (47)
      V2T(I)     =  31.8
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BLACK LOCUST
!----------
    CASE (48)
      V2T(I)     =  41.2
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  1
      SNAGCLS(I) =  5
!----------
!  NONCOMMERCIAL HARDWOOD
!----------
    CASE (49)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BOXELDER
!----------
    CASE (50)
      V2T(I)     =  30.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  STRIPED MAPLE
!----------
    CASE (51)
      V2T(I)     =  30.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  MOUNTAIN MAPLE
!----------
    CASE (52)
      V2T(I)     =  30.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  AMERICAN HORNBEAM / MUSCLEWOOD
!----------
    CASE (53)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  AMERICAN CHESTNUT
!----------
    CASE (54)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  5
!----------
!  HACKBERRY
!----------
    CASE (55)
       V2T(I)     =  30.6
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  FLOWERING DOGWOOD
!----------
    CASE (56)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  HAWTHORN
!----------
    CASE (57)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  APPLE
!----------
    CASE (58)
      V2T(I)     =  29.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used black cherry
      SNAGCLS(I) =  5
!----------
!  BLACKGUM
!----------
    CASE (59)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  AMERICAN SYCAMORE
!----------
    CASE (60)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  PIN CHERRY
!----------
    CASE (61)
      V2T(I)     =  29.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used black cherry
      SNAGCLS(I) =  5
!----------
!  COMMON CHOKECHERRY
!----------
    CASE (62)
      V2T(I)     =  29.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used black cherry
      SNAGCLS(I) =  5
!----------
!  CHERRY AND PLUM SPECIES
!----------
    CASE (63)
      V2T(I)     =  29.3
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2 ! used black cherry
      SNAGCLS(I) =  5
!----------
!  WILLOW
!----------
    CASE (64)
      V2T(I)     =  22.5
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  BLACK WILLOW
!----------
    CASE (65)
      V2T(I)     =  22.5
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  DIAMOND WILLOW
!----------
    CASE (66)
      V2T(I)     =  22.5
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!----------
!  SASSAFRAS
!----------
    CASE (67)
      V2T(I)     =  26.2
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  2
      SNAGCLS(I) =  5
!----------
!  AMERICAN MOUNTAIN ASH
!----------
    CASE (68)
      V2T(I)     =  28.7
      TFALLCLS(I) = 3
      LEAFLF(I)  =  1.0
      DKRCLS(I)  =  4
      SNAGCLS(I) =  5
!
  END SELECT
!
  SELECT CASE (I)
    CASE (1:14)
      LSW(I) = .TRUE.
    CASE DEFAULT
      LSW(I) = .FALSE.
  END SELECT
!
  SELECT CASE (TFALLCLS(I))
    CASE (1)  ! hickory/white oak/hemlock/cedar/tamarack group
      TFALL(I,1) = 2.0
      TFALL(I,3) = 5.0
      TFALL(I,4) = 10.0
!
    CASE (2)  ! red oak group
      TFALL(I,1) = 1.0
      TFALL(I,3) = 4.0
      TFALL(I,4) = 8.0
!
    CASE (3)  ! ash/elm/maple/beech/other group
      TFALL(I,1) = 1.0
      TFALL(I,3) = 3.0
      TFALL(I,4) = 6.0
!
    CASE (4)  ! pine/spruce/fir/aspen/poplar/birch/basswood
      TFALL(I,1) = 1.0
      TFALL(I,3) = 2.0
      TFALL(I,4) = 4.0
!
  END SELECT
!
  TFALL(I,0) = 1.0
  TFALL(I,2) = TFALL(I,1)
  TFALL(I,5) = TFALL(I,4)
!
  SELECT CASE (SNAGCLS(I))
!
    CASE (1)  ! aspen, birch, spruce, fir, poplar, basswood
              ! this group decays and falls really fast
      DECAYX(I)  = 0.4
      FALLX(I)   = 1.66
      ALLDWN(I)  = 10
      DO J= 1,4
        HTX(I,J) =   3.0
      ENDDO
!
    CASE (2)  ! jack pine
      DECAYX(I)  = 0.8
      FALLX(I)   = 1.33
      ALLDWN(I)  = 30
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
!
    CASE (3)  ! white pine
      DECAYX(I)  = 1.0
      FALLX(I)   = 1.16
      ALLDWN(I)  = 50
      DO J= 1,4
        HTX(I,J) =   0.0 ! no height loss
      ENDDO
!
    CASE (4)  ! red pine
      DECAYX(I)  = 1.2
      FALLX(I)   = 1.0
      ALLDWN(I)  = 50
      DO J= 1,4
        HTX(I,J) =   0.0 ! no height loss
      ENDDO
!
    CASE (5)  ! ash, maple, beech, elm
      DECAYX(I)  = 1.5
      FALLX(I)   = .83
      ALLDWN(I)  = 50
      DO J= 1,4
        HTX(I,J) =   0.65
      ENDDO
!
    CASE (6)  ! cedar, tamarack, oak, hickory, hemlock
              ! this group decays and falls very slowly
      DECAYX(I)  = 2.3
      FALLX(I)   = .53
      ALLDWN(I)  = 50
      DO J= 1,4
        HTX(I,J) =   0.45
        IF (I .EQ. 12) HTX(I,J) = 0.0 ! hemlock gets no ht loss
      ENDDO
  END SELECT
!
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
OLDICT = 3
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

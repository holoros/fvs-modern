SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-NE $Id$
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
!  DECAY RATES - FROM FOSTER AND LANG, CAN J FOR RES, 1982
!                     ARTHUR, TRITTON, AND FAHEY, CAN J FOR RES, 1993
!                     FAHEY, HUGHES, PU, AND ARTHUR, FOREST SCIENCE, 1988
!  THIS IS BASED ON DISCUSSIONS WITH COELI HOOVER.
!----------
DKR(1,1)  = 0.19 ! Fahey et. al.
DKR(2,1)  = 0.19 ! Fahey et. al.
DKR(3,1)  = 0.11 ! Arthur et. al. and Fahey et. al.
DKR(4,1)  = 0.07 ! gradient
DKR(5,1)  = 0.03 ! Foster and Lang
DKR(6,1)  = 0.03 ! Foster and Lang
DKR(7,1)  = 0.03 ! Foster and Lang
DKR(8,1)  = 0.03 ! Foster and Lang
DKR(9,1)  = 0.03 ! Foster and Lang
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1)
  ENDDO
ENDDO
!----------
!  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
!  FOR LITTER DECAY - USED INFO FROM MELILLO, ECOLOGY, 1982.  THIS IS
!  BASED ON DISCUSSIONS WITH COELI HOOVER.
!  *** MAY 2006 - I BUMPED UP THE LITTER DECAY RATE SINCE TOO MUCH LITTER WAS
!  ACCUMULATING OVER TIME - SAR
!----------
DO J = 1,4
  DKR(10,J) = 0.40
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
HTR1   =  0.015  ! ht loss info from Coeli Hoover & Linda Heath
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
!  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS BASED
!  ON THE SN AND LS VARIANTS, SINCE I FORGOT TO DISCUSS IT AT THE MEETING
!  WITH COELI HOOVER AND LINDA HEATH.
!
!  THE SNAG DECAY RATE IS SET BY FIRST DEFINING A
!  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE DECAY RATE
!  VARIABLE FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
!  1 - SPECIES DECAYS FASTER THAN AVERAGE
!  2 - SPECIES DECAYS AT AN AVERAGE RATE
!  3 - SPECIES DECAYS SLOWER THAN AVERAGE
!  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLE:
!  DECAYX() - DECAY RATE MULTIPLIER
!  THESE CLASSES AND THE DECAY RATES WERE TAKEN FROM THE SN-FFE (WITH SOME
!  MODIFICATION) BECAUSE LINDA THOUGHT THEY SEEMED REASONABLE.
!
!  THE FOLLOWING VARIABLES ARE ALSO SET, BUT ARE NOT SPECIES-SPECIFIC
!  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
!  FALLX()  - FALL RATE MULTIPLIER
!  HTX()    - HEIGHT-LOSS RATE MULTIPLIER
!  THESE WERE BASED ON DISCUSSIONS WITH COELI HOOVER AND LINDA
!  HEATH AND A PAPER BY YAMASAKI AND LEAK (IN PRESS, NJAF)

!  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!
!  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
!  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
!  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
!  YOUNG GROWTH WAS ASSUMED.
!  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN THE WOOD HANDBOOK
!----------
DO I= 1,MAXSP
!
  SELECT CASE (I)
!----------
!  BALSAM FIR
!----------
 CASE (1)
   V2T(I)     = 20.6
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  TAMARACK
!----------
 CASE (2)
   V2T(I)     = 30.6
   TFALLCLS(I) = 1
   LEAFLF(I)  = 1
   DKRCLS(I)  = 3
   SNAGCLS(I) = 3
!----------
!  WHITE SPRUCE
!----------
 CASE (3)
   V2T(I)     =  23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  RED SPRUCE
!----------
 CASE (4)
   V2T(I)     = 23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  NORWAY SPRUCE
!----------
 CASE (5)
   V2T(I)     = 23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  BLACK SPRUCE
!----------
 CASE (6)
   V2T(I)     = 23.7
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  OTHER SPRUCE
!----------
 CASE (7)
   V2T(I)     = 23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 8
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  RED PINE
!----------
 CASE (8)
   V2T(I)     =  25.6
   TFALLCLS(I) = 6
   LEAFLF(I)  = 4
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  EASTERN WHITE PINE
!----------
 CASE (9)
   V2T(I)     = 21.2
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  LOBLOLLY PINE
!----------
 CASE (10)
   V2T(I)     = 29.3
   TFALLCLS(I) = 6
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  VIRGINIA PINE
!----------
 CASE (11)
   V2T(I)     = 28.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  NORTHERN WHITE CEDAR
!----------
 CASE (12)
   V2T(I)     = 18.1
   TFALLCLS(I) = 1
   LEAFLF(I)  = 2
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  ATLANTIC WHITE CEDAR
!----------
 CASE (13)
   V2T(I)     = 19.3
   TFALLCLS(I) = 1
   LEAFLF(I)  = 3
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  EASTERN REDCEDAR
!----------
 CASE (14)
   V2T(I)     = 27.4
   TFALLCLS(I) = 1
   LEAFLF(I)  = 5
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  JUNIPER SPECIES
!----------
 CASE (15)
   V2T(I)     = 27.4
   TFALLCLS(I) = 1
   LEAFLF(I)  = 5
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  EASTERN HEMLOCK
!----------
 CASE (16)
   V2T(I)     = 23.7
   TFALLCLS(I) = 3
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  HEMLOCK SPECIES
!----------
 CASE (17)
   V2T(I)     = 26.2
   TFALLCLS(I) = 3
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  OTHER PINE
!----------
 CASE (18)
   V2T(I)     = 25.6
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  JACK PINE
!----------
 CASE (19)
   V2T(I)     = 24.9
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SHORTLEAF PINE
!----------
 CASE (20)
   V2T(I)     = 29.3
   TFALLCLS(I) = 6
   LEAFLF(I)  = 4
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
! TABLE MOUNTAIN PINE
!----------
 CASE (21)
   V2T(I)     = 28.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  PITCH PINE
!----------
 CASE (22)
   V2T(I)     = 29.3
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  POND PINE
!----------
 CASE (23)
   V2T(I)     = 31.8
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SCOTCH PINE
!----------
 CASE (24)
   V2T(I)     =  25.6
   TFALLCLS(I) = 6
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  OTHER SOFTWOOD SPECIES
!----------
 CASE (25)
   V2T(I)     = 25.6
   TFALLCLS(I) = 6
   LEAFLF(I)  = 2
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  RED MAPLE
!----------
 CASE (26)
   V2T(I)     = 30.6
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SUGAR MAPLE
!----------
 CASE (27)
   V2T(I)     = 34.9
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BLACK MAPLE
!----------
 CASE (28)
   V2T(I)     = 32.4
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SILVER MAPLE
!----------
 CASE (29)
   V2T(I)     = 27.4
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  YELLOW BIRCH
!----------
 CASE (30)
   V2T(I)     = 34.3
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SWEET BIRCH
!----------
 CASE (31)
   V2T(I)     = 37.4
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  RIVER BIRCH
!----------
 CASE (32)
   V2T(I)     = 29.9
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  PAPER BIRCH
!----------
 CASE (33)
   V2T(I)     = 29.9
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  GRAY BIRCH
!----------
 CASE (34)
   V2T(I)     = 29.9
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  HICKORY SPECIES
!----------
 CASE (35)
   V2T(I)     = 39.9
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  PIGNUT HICKORY
!----------
 CASE (36)
   V2T(I)     = 41.2
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  SHELLBARK HICKORY
!----------
 CASE (37)
   V2T(I)     = 38.7
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  SHAGBARK HICKORY
!----------
 CASE (38)
   V2T(I)     = 39.9
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  MOCKERNUT HICKORY
!----------
 CASE (39)
   V2T(I)     = 39.9
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  AMERICAN BEECH
!----------
 CASE (40)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  ASH SPECIES
!----------
 CASE (41)
   V2T(I)     = 33.1
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  WHITE ASH
!----------
 CASE (42)
   V2T(I)     = 34.3
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BLACK ASH
!----------
 CASE (43)
   V2T(I)     = 28.1
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  GREEN ASH
!----------
 CASE (44)
   V2T(I)     = 33.1
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  PUMPKIN ASH
!----------
 CASE (45)
   V2T(I)     = 33.1
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  YELLOW-POPLAR
!----------
 CASE (46)
   V2T(I)     = 24.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SWEETGUM
!----------
 CASE (47)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  CUCUMBERTREE
!----------
 CASE (48)
   V2T(I)     = 27.4
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  QUAKING ASPEN
!----------
 CASE (49)
   V2T(I)     = 21.8
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  BALSAM POPLAR
!----------
 CASE (50)
   V2T(I)     = 19.3
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  EASTERN COTTONWOOD
!----------
 CASE (51)
   V2T(I)     = 23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  BIGTOOTH ASPEN
!----------
 CASE (52)
   V2T(I)     = 22.5
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SWAMP COTTONWOOD
!----------
 CASE (53)
   V2T(I)     = 23.1
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  BLACK CHERRY
!----------
 CASE (54)
   V2T(I)     = 29.3
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  WHITE OAK
!----------
 CASE (55)
   V2T(I)     = 37.4
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  BUR OAK
!----------
 CASE (56)
   V2T(I)     = 36.2
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  CHINKAPIN OAK
!----------
 CASE (57)
   V2T(I)     = 37.4
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  POST OAK
!----------
 CASE (58)
   V2T(I)     = 37.4
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  OAK SPECIES
!----------
 CASE (59)
   V2T(I)     = 37.4
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  SCARLET OAK
!----------
 CASE (60)
   V2T(I)     = 37.4
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  SHINGLE OAK
!----------
 CASE (61)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  WATER OAK
!----------
 CASE (62)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  NORTHERN PIN OAK
!----------
 CASE (63)
   V2T(I)     = 36.2
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  CHESTNUT OAK
!----------
 CASE (64)
   V2T(I)     = 35.6
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  SWAMP WHITE OAK
!----------
 CASE (65)
   V2T(I)     = 39.9
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  SWAMP CHESTNUT OAK
!----------
 CASE (66)
   V2T(I)     = 37.4
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 3
!----------
!  NORTHERN RED OAK
!----------
 CASE (67)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  SOUTHERN RED OAK
!----------
 CASE (68)
   V2T(I)     = 32.4
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  BLACK OAK
!----------
 CASE (69)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  CHERRYBARK OAK
!----------
 CASE (70)
   V2T(I)     = 38.0
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  OTHER HARDWOODS
!----------
 CASE (71)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BUCKEYE SPECIES
!----------
 CASE (72)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  YELLOW BUCKEYE
!----------
 CASE (73)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  WATER BIRCH
!----------
 CASE (74)
   V2T(I)     = 29.9
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  COMMON HACKBERRY
!----------
 CASE (75)
   V2T(I)     = 30.6
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  COMMON PERSIMMON
!----------
 CASE (76)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  AMERICAN HOLLY
!----------
 CASE (77)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 3
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BUTTERNUT
!----------
 CASE (78)
   V2T(I)     = 22.5
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BLACK WALNUT
!----------
 CASE (79)
   V2T(I)     = 31.8
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  OSAGE-ORANGE
!----------
 CASE (80)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 1
   SNAGCLS(I) = 2
!----------
!  MAGNOLIA
!----------
 CASE (81)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SWEETBAY
!----------
 CASE (82)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  APPLE SPECIES
!----------
 CASE (83)
   V2T(I)     = 29.3
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  WATER TUPELO
!----------
 CASE (84)
   V2T(I)     = 28.7
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  BLACK GUM
!----------
 CASE (85)
   V2T(I)     = 28.7
   TFALLCLS(I) = 3
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 3
!----------
!  SOURWOOD
!----------
 CASE (86)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  PAULOWNIA
!----------
 CASE (87)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  =  1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SYCAMORE
!----------
 CASE (88)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  WILLOW OAK
!----------
 CASE (89)
   V2T(I)     = 34.9
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  BLACK LOCUST
!----------
 CASE (90)
   V2T(I)     = 41.2
   TFALLCLS(I) = 2
   LEAFLF(I)  = 1
   DKRCLS(I)  = 1
   SNAGCLS(I) = 3
!----------
!  BLACK WILLOW
!----------
 CASE (91)
   V2T(I)     = 22.5
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SASSAFRAS
!----------
 CASE (92)
   V2T(I)     = 26.2
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  AMERICAN BASSWOOD
!----------
 CASE (93)
   V2T(I)     = 20.0
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  WHITE BASSWOOD
!----------
 CASE (94)
   V2T(I)     = 20.0
   TFALLCLS(I) = 6
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  ELM SPECIES
!----------
 CASE (95)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  AMERICAN ELM
!----------
 CASE (96)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  SLIPPERY ELM
!----------
 CASE (97)
   V2T(I)     = 29.9
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 1
!----------
!  NON-COMMERCIAL HARDWOODS
!----------
 CASE (98)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  BOX ELDER
!----------
 CASE (99)
   V2T(I)     = 30.6
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  STRIPED MAPLE
!----------
 CASE (100)
   V2T(I)     = 30.6
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  TREE OF HEAVEN - AILANTHUS
!----------
 CASE (101)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  SERVICEBERRY
!----------
 CASE (102)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  AMERICAN HORNBEAM
!----------
 CASE (103)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  FLOWERING DOGWOOD
!----------
 CASE (104)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  HAWTHORN SPECIES
!----------
 CASE (105)
   V2T(I)     = 28.7
   TFALLCLS(I) = 5
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  HOPHORNBEAM
!----------
 CASE (106)
   V2T(I)     = 28.7
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 4
   SNAGCLS(I) = 2
!----------
!  PLUM SPECIES
!----------
 CASE (107)
   V2T(I)     = 29.3
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!----------
!  PIN CHERRY
!----------
 CASE (108)
   V2T(I)     = 29.3
   TFALLCLS(I) = 4
   LEAFLF(I)  = 1
   DKRCLS(I)  = 2
   SNAGCLS(I) = 2
!
  END SELECT
!
  SELECT CASE (I)
    CASE (1:25)
      LSW(I) = .TRUE.
    CASE DEFAULT
      LSW(I) = .FALSE.
  END SELECT
!
  IF ((I .GE. 12) .AND. (I .LE. 15)) THEN !cedars
    TFALL(I,0) = 3.0  ! foliage
  ELSE
    TFALL(I,0) = 1.0
  ENDIF
!
  SELECT CASE (TFALLCLS(I))
!
    CASE (1)  ! cedar / tamarack group
      TFALL(I,1) = 5.0
      TFALL(I,3) = 10.0
      TFALL(I,4) = 25.0
!
    CASE (2)  ! hickory / black locust group
      TFALL(I,1) = 3.0
      TFALL(I,3) = 6.0
      TFALL(I,4) = 12.0
!
    CASE (3)  ! white oak group / hemlock / other
      TFALL(I,1) = 2.0
      TFALL(I,3) = 5.0
      TFALL(I,4) = 10.0
!
    CASE (4)  ! red oak group / other
      TFALL(I,1) = 1.0
      TFALL(I,3) = 4.0
      TFALL(I,4) = 8.0
!
    CASE (5)  ! ash/elm/maple/other group
      TFALL(I,1) = 1.0
      TFALL(I,3) = 3.0
      TFALL(I,4) = 6.0
!
    CASE (6)  ! pine/spruce/fir/aspen/poplar/birch/basswood
      TFALL(I,1) = 1.0
      TFALL(I,3) = 2.0
      TFALL(I,4) = 4.0
!
  END SELECT
!
  TFALL(I,2) = TFALL(I,1)
  TFALL(I,5) = TFALL(I,4)
!
  FALLX(I)   = 1.0
  ALLDWN(I)  = 50.0

  SELECT CASE (SNAGCLS(I))
!
    CASE (1)  ! pines (except white), fir, spruce and others
              ! this group decays faster than average
      DECAYX(I)  = 0.07 ! 12 inch tree is soft in 2 years
!
    CASE (2)  ! black oak and others
              ! this group decays at an average rate
      DECAYX(I)  = 0.21  ! 12 inch tree is soft in 6 years

!
    CASE (3)  ! white oak, redcedar, and others
              ! this group decays slower than average
      DECAYX(I)  = 0.35  ! 12 inch tree is soft in 10 years
  END SELECT
!
  DO J= 1,4
    HTX(I,J) =   1.0 ! all species get base rate of
                     ! 1.5% ht. loss/year
  ENDDO
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
!  COVER TYPE  - CURRENTLY NOT USED IN NE-FFE
!----------
OLDICT = 0
!----------
!  DROUGHT START AND END YEARS - CURRENTLY NOT USED IN NE-FFE
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

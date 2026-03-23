SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-OC $Id$
!----------
!  Purpose:
!      Initialize variant-specific variables for the Fire Model

!
!  Called from: INITRE
!
!  Call list definitions:
!
!  Local variable definitions:
!
!**********************************************************************
!OMMONS

INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'

!OMMONS

INTEGER I,J

LVWEST    = .TRUE.  ! WESTERN VARIANT

CANCLS(1) =  5.0
CANCLS(2) = 17.5
CANCLS(3) = 37.5
CANCLS(4) = 75.0

CORFAC(1) =  0.5
CORFAC(2) =  0.3
CORFAC(3) =  0.2
CORFAC(4) =  0.1

SNPRCL(1) =  0
SNPRCL(2) = 12
SNPRCL(3) = 18
SNPRCL(4) = 24
SNPRCL(5) = 30
SNPRCL(6) = 36

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
PREWND(2)=6.
POTEMP(1)=70.
POTEMP(2)=70.
!
!     SET DECAY VARIABLES TO AN UNSET STATE
!
!     IN CA-FFE VARIANT DECAY RATE DEFAULTS ARE ASSIGNED IN CYCLE 1
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

!     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
!     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.

!     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
!     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0

NZERO  =  0.01
LIMBRK =  0.01
HTXSFT = 10.0
DO I= 1,MAXSP
  PSOFT(I)  =  0.0
ENDDO

!     VALUE OF HTR1 BASED ON 50% HEIGHT LOSS IN 20 YEARS; ALL SPECIES
!     VALUE OF HTR2 BASED ON HEIGHT LOSS CEASING AFTER 50% LOSS
!        NOTE: HTR2 CAN NO LONGER BE 0 (FEB 2002) SO WAS RESET TO DEFAULT 0.01
!              THE CEASING OF HEIGHT-LOSS AFTER 50% IS NOW GIVEN BY HTX (BELOW)

HTR1   =  0.03406
HTR2   =  0.01

!     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

!     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
!     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
!     1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.

!     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]

!     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES

!     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
!     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
!     [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
!     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.

!     0 :  FOLIAGE
!     1 : <0.25"
!     2 :  0.25" -   1"
!     3 :  1"    -   3"
!     4 :  3"    -   6"
!     5 :  6"    -  12"

!     IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
!     **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
!     INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
!     THAT THEY BEHAVE PROPERLY.

!     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
!     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
!     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
!     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)

!     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

!     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

DO I = 1,MAXSP

   SELECT CASE (I)

!         Port Orford cedar (borrows from western redcedar)
    CASE (1)
      V2T(I)     =  24.3
      LEAFLF(I)  =   4.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      FALLX(I)   =   1.235 ! 20", 95% down @ 25 yrs
      LSW(I)     = .TRUE.

!         incense-cedar
    CASE (2)
      V2T(I)     =  21.8
      LEAFLF(I)  =   5.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.687
      LSW(I)     = .TRUE.

!         western redcedar (uses DF for some RC attributes)
    CASE (3)
      V2T(I)     =  19.3
      LEAFLF(I)  =   5.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      FALLX(I)   =   1.235
      LSW(I)     = .TRUE.

!         grand fir (uses SO California grand fir settings)
    CASE (4)
      V2T(I)     =  21.8
      LEAFLF(I)  =   7.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.882
      LSW(I)     = .TRUE.

!         California red fir, Shasta red fir
    CASE (5,6)
      V2T(I)     =  22.5
      LEAFLF(I)  =   7.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.882
      LSW(I)     = .TRUE.

!         Douglas-fir
    CASE (7)
      V2T(I)     =  28.7 ! interior west DF in Wood Handbook
      LEAFLF(I)  =   5.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.882
      LSW(I)     = .TRUE.

!         western hemlock
    CASE (8)
      V2T(I)     =  26.2
      LEAFLF(I)  =   5.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   1.235
      LSW(I)     = .TRUE.

!         mountain hemlock
    CASE (9)
      V2T(I)     =  26.2
      LEAFLF(I)  =   4.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   1.235
      LSW(I)     = .TRUE.

!         pines: whitebark (10), knobcone (11), lodgepole (12),
!         Coulter (13), limber (14), Jeffrey (15), sugar (16),
!         western white (17), ponderosa (18), Monterey (19),
!         gray (20), other softwoods (25)
    CASE (10:20,25)
      SELECT CASE (I)
        CASE (10,14,17)
          V2T(I)  =  22.5 ! white
        CASE (11,12,13,18,19,20,25)
          V2T(I)  =  23.7 ! lodgepole/ponderosa
        CASE (15,16)
          V2T(I)  =  21.2 ! sugar
      END SELECT
      SELECT CASE (I)
        CASE (11)
          LEAFLF(I)  =   4.0
        CASE DEFAULT
          LEAFLF(I)  =   3.0
      END SELECT
      TFALL(I,0) =   3.0
      SELECT CASE (I)
        CASE (18,25)
          TFALL(I,3) =  10.0
        CASE DEFAULT
          TFALL(I,3) =  15.0
      END SELECT
      ALLDWN(I)  = 100.0
      FALLX(I)   =   1.235
      LSW(I)     = .TRUE.

!         western juniper
    CASE (21)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      FALLX(I)   =   0.687
      LSW(I)     = .TRUE.

!         Brewer spruce
    CASE (22)
      V2T(I)     =  20.6 ! Engelmann
      LEAFLF(I)  =   8.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.687
      LSW(I)     = .TRUE.

!         giant sequoia and redwood
    CASE (23,50)
      V2T(I)     =  21.2 ! use redwood
      LEAFLF(I)  =   5.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      FALLX(I)   =   0.687
      LSW(I)     = .TRUE.

!         Pacific yew
    CASE (24)
      V2T(I)     =  26.2 ! use baldcypress
      LEAFLF(I)  =   7.0
      TFALL(I,0) =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      FALLX(I)   =   0.687
      LSW(I)     = .TRUE.

!         oaks: coast live oak (26), canyon live oak (27),
!         blue oak (28), Engelmann oak (29), Oregon white
!         oak (30), California black oak (31), valley white
!         oak (32), interior live oak (33), other hardwoods (49)

    CASE (26:33,49)
      SELECT CASE (I)
        CASE (26,27,33)
          V2T(I)  =  49.9  ! live oak
        CASE (28,29,30,32)
          V2T(I)  =  37.4  ! white oak
        CASE (31,49)
          V2T(I)  =  34.9  ! black oak
      END SELECT
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         bigleaf maple
    CASE (34)
      V2T(I)     =  27.4
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         California buckeye
    CASE (35)
      V2T(I)     =  37.4 ! use white oak
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         red alder
    CASE (36)
      V2T(I)     =  23.1
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         Pacific madrone (37)
    CASE (37)
      V2T(I)     =  36.2 ! tanoak
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         Golden chinkapin (38), tanoak (42)
    CASE (38,42)
      V2T(I)     =  36.2 ! tanoak
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         Pacific dogwood
    CASE (39)
      V2T(I)     =  27.4 ! bigleaf maple
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         Oregon ash
    CASE (40)
      V2T(I)     =  31.2
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         walnut
    CASE (41)
      V2T(I)     =  31.8
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         California sycamore
    CASE (43)
      V2T(I)     =  28.7 ! American sycamore
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         quaking aspen
    CASE (44)
      V2T(I)     =  21.8
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         black cottonwood
    CASE (45)
      V2T(I)     =  19.3
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         willow
    CASE (46)
      V2T(I)     =  22.5 ! black willow
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         California nutmeg
    CASE (47)
      V2T(I)     =  34.9 ! hickory-nutmeg
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

!         California laurel
    CASE (48)
      V2T(I)     =  36.2 ! tanoak
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      FALLX(I)   =   1.545
      LSW(I)     = .FALSE.

  END SELECT
!
!       SET THE DECAY RATE CLASS (DKRCLS)
!       DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!       CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!       CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  SELECT CASE (I)

!         some pines, doug-fir, cedars
    CASE (1:3,7,10,14,16,17,21,23,24,50)
      DKRCLS(I)  =   1

!         lodgepole, spruce, hemlock
    CASE (8,9,11,12,19,22)
      DKRCLS(I)  =   2

!         firs, some pines, oak
    CASE (4:6,13,15,18,20,25:33,37,38,42,49)
      DKRCLS(I)  =   3

!         aspen, cottonwood, other hardwoods
    CASE (34:36,39:41,43:48)
      DKRCLS(I)  =   4

  END SELECT

!       HARD SNAGS NEVER BECOME SOFT
!       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
!       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)

  DECAYX(I) = 999.0
  HTX(I,1)  =   1.0  ! 20 yrs for all species
  HTX(I,3)  =   1.0
  HTX(I,2)  =   0.0
  HTX(I,4)  =   0.0

!       TIME-TO-FALL FOR OTHER CROWN CATEGORIES

  TFALL(I,1) = 10.0
  TFALL(I,2) = 15.0
  TFALL(I,4) = TFALL(I,3)
  TFALL(I,5) = TFALL(I,3)

!       DEAD LEAF FALL CANNOT BE > LIVE

  TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))

!       TFALL(I,3) CANNOT BE < TFALL(I,2)

  IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
    TFALL(I,2) = TFALL(I,3)
  ENDIF

!       CONVERT LB/FT**3 TO TONS/FT**3

  V2T(I) = V2T(I) / 2000.0

ENDDO

!     parameters for post-burn snag fall rates:

PBSCOR =  0.0
PBSOFT =  1.0
PBSMAL =  0.9
PBSIZE = 12.0
PBTIME =  7.0

!     PARAMETERS FOR FUEL MODEL SELECTION

!     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
OLDICT = 0

!     DROUGHT START AND END YEARS
IDRYB  = 0
IDRYE  = 0

!     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
SLCRIT = 10.0

!     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY
CCCRIT = 10.0

RETURN
END

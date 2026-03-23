SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-UT $Id$
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
!
!
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'FMFCOM.f90'
!
!OMMONS
!----------
INTEGER I,J
!----------
!  SET FLAG FOR WESTERN VARIANT
!----------
LVWEST    = .TRUE.
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
PREWND(2)=6.
POTEMP(1)=70.
POTEMP(2)=70.
!----------
!     LOG MINERALIZATION RATES, BASED ON HARMON ET AL. (DATE?)
!     ADV. ECOL. RES. 15. THERE ARE FOUR DEFAULT RATES, AND EACH OF
!     THOSE SPECIES TAKES ONE OF THE FOUR RATES. THE ABSOLUTE RATES
!     ARE ALSO MAPPED DIRECTLY ONTO EACH OF THE 1-6 SIZE CATEGORIES.
!     THIS IS MORE COMPLICATED THAN NECESSARY, BUT ALLOWS SIZE-DEPENDENT
!     RATES TO BE ADDED EASILY.
!
!SB      DKRDEF(1)=.007
!SB      DKRDEF(2)=.010
!SB      DKRDEF(3)=.011
!SB      DKRDEF(4)=.012
!
!     NEW DECAY RATES (ADDED 2/97) BASED ON VALUES FROM ABBOTT AND
!     CROSSLEY, ECOLOGY 1982. THESE ARE SIZE CLASS DEPENDENT, BUT
!     DO NOT HAVE DIFFERENT DECAY RATES FOR DIFFERENT SPECIES.
!----------
DKR(1,1) = 0.12
DKR(2,1) = 0.12
DKR(3,1) = 0.09
DKR(4,1) = 0.015
DKR(5,1) = 0.015
DKR(6,1) = 0.015
DKR(7,1) = 0.015
DKR(8,1) = 0.015
DKR(9,1) = 0.015
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1)
  ENDDO
ENDDO
!----------
!     LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
!----------
DO J = 1,4
  DKR(10,J) = 0.5
  DKR(11,J) = 0.002
ENDDO
!----------
!     MODIFER FOR CR/UT - NO SPECIES MODIFIER
!     THIS AGREES WITH PUBLISHED VALUES FOR LARGE CWD
!     LP LOGS AT 100 YR: 0.0067/YR
!     SOURCE: BROWN ET AL. 1998. CJFR 28:932-936
!----------
DO I = 1,MXFLCL
  DO J = 2,4
    DKR(I,J) = DKR(I,J) * 0.45
  ENDDO
ENDDO
!----------
!     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
!     DECAY RATE: 'DKR'.
!----------
DO I = 1,MXFLCL
  DO J = 1,4
    PRDUFF(I,J) = 0.02
    TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
  ENDDO
ENDDO
!----------
!     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
!     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
!
!     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
!     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0
!----------
NZERO  =  0.01
LIMBRK =  0.01
HTXSFT = 10.0
HTR1   =  0.0228
HTR2   =  0.01
DO I= 1,MAXSP
   PSOFT(I)  =  0.0
ENDDO
!----------
!     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
!
!     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
!     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
!     1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.
!
!     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
!
!     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
!
!     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
!     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
!     [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
!     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
!
!     0 :  FOLIAGE
!     1 : <0.25"
!     2 :  0.25" -   1"
!     3 :  1"    -   3"
!     4 :  3"    -   6"
!     5 :  6"    -  12"
!
!     IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
!     **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
!     INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
!     THAT THEY BEHAVE PROPERLY.
!
!     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
!     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
!     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
!     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)
!
!     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
!
!     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!----------
DO I= 1,MAXSP
!
  SELECT CASE (I)
!
!         whitebark & limber pine, other softwoods
!
    CASE (1,2,23)
      V2T(I)     =  22.5 ! use white pine
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0978
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         Douglas-fir
!
    CASE (3)
      V2T(I)     =  26.8  ! use DF South from Wood Handbook
      LEAFLF(I)  =   5.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.1
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.9
      ENDDO
      DKRCLS(I)  =   3
      LSW(I)     = .TRUE.
!
!         white fir
!
    CASE (4)
      V2T(I)     =  23.1
      LEAFLF(I)  =   7.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  40.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   2.5
      DO J= 1,4
        HTX(I,J) =  1.494
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         Blue & Engelmann spruce
!
    CASE (5,8)
      V2T(I)     =  20.6
      LEAFLF(I)  =   6.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0462
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         quaking aspen
!
    CASE (6)
      V2T(I)     =  21.8
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =   5.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   4.0
      DO J= 1,4
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!
!         lodgepole pine
!
    CASE (7)
      V2T(I)     =  23.7
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0462
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         subalpine fir
!
    CASE (9)
      V2T(I)     =  19.3
      LEAFLF(I)  =   7.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  40.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   2.5
      DO J= 1,4
        HTX(I,J) =  1.494
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         ponderosa pine
!
    CASE (10)
      V2T(I)     =  23.7
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0978
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         common pinyon, singleleaf pinyon
!
    CASE (11,14)
      V2T(I)     =  31.8
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0978
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         western juniper, RM juniper, Utah juniper
!
    CASE (12,15,16)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =  0.0978
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .TRUE.
!
!         Gambel oak, other hardwoods
!
    CASE (13,24)
      V2T(I)     =  39.6
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  40.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   2.5
      DO J= 1,4
        HTX(I,J) =  1.494
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .FALSE.
!
!         GB bristlecone pine (based on LP)
!
    CASE (17)
      V2T(I)     =  23.7
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 999.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   0.001
      DO J= 1,4
        HTX(I,J) =   0.0462
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!
!         narrowleaf cottonwood, Fremont cottonwood, box elder
!
    CASE (18,19,22)
      V2T(I)  =  19.3
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =   5.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   4.0
      DO J= 1,4
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!
!         curlleaf mtn mahogany, bigtooth maple
!
    CASE (20,21)
      SELECT CASE (I)
      CASE(20)
        V2T(I)  = 21.8
      CASE(21)
        V2T(I)  = 27.4
      END SELECT
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!
  END SELECT
!
  SELECT CASE (I)

!----------
!       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
!----------
  CASE (20,21)
    TFALL(I,1) =   5.0
    TFALL(I,2) =   5.0
    TFALL(I,4) = TFALL(I,3)
    TFALL(I,5) = TFALL(I,3)
!----------
!       DEAD LEAF FALL CANNOT BE > LIVE
!----------
    TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
!
  CASE DEFAULT
    TFALL(I,1) = 10.0
    TFALL(I,2) = 15.0
    TFALL(I,4) = TFALL(I,3)
    TFALL(I,5) = TFALL(I,3)
!----------
!       DEAD LEAF FALL CANNOT BE > LIVE
!----------
    TFALL(I,0) = MIN(2.0, LEAFLF(I))
!
  END SELECT
!----------
!       TFALL(I,3) CANNOT BE < TFALL(I,2)
!----------
  IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
    TFALL(I,2) = TFALL(I,3)
  ENDIF
!----------
!       CONVERT LB/FT**3 TO TONS/FT**3
!----------
  V2T(I) = V2T(I) / 2000.0

ENDDO
!----------
!     PARAMETERS FOR POST-BURN SNAG FALL RATES:
!----------
PBSCOR =  0.0
PBSOFT =  1.0
PBSMAL =  0.9
PBSIZE = 12.0
PBTIME =  7.0
!----------
!     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
!----------
OLDICT = 6
!----------
!     DROUGHT START AND END YEARS
!----------
IDRYB  = 0
IDRYE  = 0
!----------
!     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
!----------
SLCRIT = 10.0
!
RETURN
END

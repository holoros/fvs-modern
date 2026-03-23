SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-EM $Id$
!----------
!  Purpose:
!  Initialize variant-specific variables for the Fire Model
!  Called from: FMINIT
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
!----------
!  VARIABLE DECLARATIONS
!----------
INTEGER I,J
!----------
!  SPECIES ORDER:
!   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
!  17=PB, 18=OS, 19=OH
!----------
LVWEST    = .TRUE.  ! WESTERN VARIANT
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

!  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
DO J = 1,4
  DKR(10,J) = .5
  DKR(11,J) = .002
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
!----------
NZERO  = 0.01
LIMBRK = 0.01
HTXSFT = 2.0
HTR1   = 0.0228
HTR2   = 0.01
DO I= 1,MAXSP
  PSOFT(I) = 0.0
ENDDO
!----------
!  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
!----------
!  V2T() - UNITS ARE LB/CUFT BY SPECIES FROM 'WOOD HANDBOOK' USDA
!  FOREST PRODUCTS AG. HANDBOOK 72.  1974. DENSITY OF PINYON, JUNIPER,
!  GAMBEL FROM CHOJNACKY 1992.
!
!  FOR THE ORIGINAL 8 SPECIES 1-3, 7-10, AND 18, DATA IS
!  FROM JIM BROWN. 1977. HANDBOOK FOR PREDICTING SLASH WEIGHTS OF
!  WESTERN CONIFERS.
!
!  [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
!
!  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
!
!  TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
!  IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
!  TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
!
!  0 :  FOLIAGE
!  1 : <0.25"
!  2 :  0.25" -   1"
!  3 :  1"    -   3"
!  4 :  3"    -   6"
!  5 :  6"    -  12"
!
!  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
!
!  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
!  DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0)
!  FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0)
!  HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0)
!
!  ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
!     DEFAULT SNAG RATE PARAMETERS ARE SET RELATIVE TO THE VALUES FOR
!     PP (SPECIES 10), WHICH WAS ORIGINALLY IN THE 'MEDIUM' SPECIES
!     GROUP. ALLDWN & DECAYX VALUES:
!     OTHER SOFTWOODS = PP
!     WHITE BARK PINE, WESTERN LARCH, DOUGLAS-FIR = 1.1 * PP
!     LODGEPOLE PINE, ENGELMANN SPRUCE, SUBALPINE FIR = 0.9 * PP
!
!  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!
!  LOG MINERALIZATION RATES, BASED ON HARMON ET AL. (DATE?)
!  ADV. ECOL. RES. 15. THERE ARE FOUR DEFAULT RATES, AND EACH OF
!  THOSE SPECIES TAKES ONE OF THE FOUR RATES. THE ABSOLUTE RATES
!  ARE ALSO MAPPED DIRECTLY ONTO EACH OF THE 1-6 SIZE CATEGORIES.
!  THIS IS MORE COMPLICATED THAN NECESSARY, BUT ALLOWS SIZE-DEPENDENT
!  RATES TO BE ADDED EASILY.
!
!----------
DO I= 1,MAXSP

  SELECT CASE (I)
!----------
!  WHITEBARK PINE
!----------
    CASE (1)
      V2T(I)     =  22.5    ! uses IE white pine
      LEAFLF(I)  =   7.0    ! uses EM subalpine fir
      TFALL(I,3) =  15.0    ! uses IE white pine
      ALLDWN(I)  = 110.0    ! uses IE white pine
      DECAYX(I)  =   1.1    ! uses IE white pine
      FALLX(I)   =   0.9    ! uses IE white pine
      DO J= 1,4
        HTX(I,J) =   0.9    ! uses IE white pine
      ENDDO
      DKRCLS(I)  =   4      ! uses IE white pine
      LSW(I)     = .TRUE.   ! uses IE white pine
!----------
!  WESTERN LARCH (USES SIMILARITY TO PP FOR SOME WL ATTRIBUTES)
!----------
    CASE (2)
      V2T(I)     =  29.9
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 110.0
      DECAYX(I)  =   1.1
      FALLX(I)   =   0.9
      DO J= 1,4
        HTX(I,J) =   0.9
      ENDDO
      DKRCLS(I)  =   3
      LSW(I)     = .TRUE.
!----------
!  DOUGLAS-FIR
!----------
    CASE (3)
      V2T(I)     =  28.1    ! use DF north; Wood Handbook
      LEAFLF(I)  =   5.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 110.0
      DECAYX(I)  =   1.1
      FALLX(I)   =   0.9
      DO J= 1,4
        HTX(I,J) =   0.9
      ENDDO
      DKRCLS(I)  =   3
      LSW(I)     = .TRUE.
!----------
!  LIMBER PINE -- USE IE LIMBER PINE
!----------
    CASE (4)
      V2T(I)     =  22.5    !w. white pine
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  SUBALPINE LARCH -- USE IE SUBALPINE LARCH
!----------
    CASE (5)
      V2T(I)     =  19.3    !subalpine fir
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  ROCKY MOUNTAIN JUNIPER -- USE IE ROCKY MTN JUNIPER
!----------
    CASE (6)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .TRUE.
!----------
!  LODGEPOLE PINE
!----------
    CASE (7)
      V2T(I)     =  23.7
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.1
      DO J= 1,4
        HTX(I,J) =   1.1
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  ENGELMANN SPRUCE
!----------
    CASE (8)
      V2T(I)     =  20.6
      LEAFLF(I)  =   6.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.1
      DO J= 1,4
        HTX(I,J) =   1.1
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  SUBALPINE FIR
!----------
    CASE (9)
      V2T(I)     =  19.3
      LEAFLF(I)  =   7.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.1
      DO J= 1,4
        HTX(I,J) =   1.1
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  PONDEROSA PINE
!----------
    CASE (10)
      V2T(I)     =  23.7
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!  GREEN ASH -- USE IE COTTONWOOD
!----------
    CASE (11)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  QUAKING ASPEN -- USE IE QUAKING ASPEN
!----------
    CASE (12)
      V2T(I)     =  21.8
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  BLACK COTTONWOOD -- USE IE COTTONWOOD
!----------
    CASE (13)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  BALSAM POPLAR -- USE IE COTTONWOOD
!----------
    CASE (14)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  PLAINS COTTONWOOD -- USE IE COTTONWOOD
!----------
    CASE (15)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  NARROWLEAF COTTONWOOD -- USE IE COTTONWOOD
!----------
    CASE (16)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  PAPER BIRCH -- USE IE PAPER BIRCH
!----------
    CASE (17)
      V2T(I)     =  29.9
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!  OTHER SOFTWOODS
!----------
    CASE (18)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0    ! other softwoods (juniper)
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .TRUE.
!----------
!  OTHER HARDWOODS -- USE IE COTTONWOOD
!----------
    CASE (19)
      V2T(I)     =  19.3    !black cottonwood
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
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
  TFALL(I,1) = 5.0
  TFALL(I,2) = TFALL(I,1)
  TFALL(I,4) = TFALL(I,3)
  TFALL(I,5) = TFALL(I,3)
!----------
!  DEAD LEAF FALL CANNOT BE > LIVE
!----------
  TFALL(I,0) = MIN(2.0, LEAFLF(I))
!----------
!  TFALL(I,3) CANNOT BE < TFALL(I,2)
!----------
  IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
    TFALL(I,2) = TFALL(I,3)
  ENDIF
!----------
!  CONVERT LB/FT**3 TO TONS/FT**3
!----------
  V2T(I) = V2T(I) / 2000.0
!
ENDDO
!----------
!     PARAMETERS FOR POST-BURN SNAG FALL RATES:
!----------
PBSCOR = 0.0
PBSOFT = 1.0
PBSMAL = 0.9
PBSIZE = 10.0
PBTIME = 7.0
!----------
!     THE LODGEPOLE PINE COVER TYPE METAGROUP (NOT USED IN EM-FFE)
!----------
OLDICT = 0
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

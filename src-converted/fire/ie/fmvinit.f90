SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-IE $Id$
!----------
!  PURPOSE:
!  INITIALIZE VARIANT-SPECIFIC VARIABLES FOR THE FIRE MODELC
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
INTEGER I,J

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
!
DKR(1,1)  = 0.12
DKR(2,1)  = 0.12
DKR(3,1)  = 0.09
DKR(4,1)  = 0.015
DKR(5,1)  = 0.015
DKR(6,1)  = 0.015
DKR(7,1)  = 0.015
DKR(8,1)  = 0.015
DKR(9,1)  = 0.015
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1)
  ENDDO
ENDDO
!----------
!  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
!----------
DO J = 1,4
  DKR(10,J) = 0.5
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
!  VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
!  AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0
!----------
NZERO  =  0.01
LIMBRK =  0.01
HTXSFT =  2.0
HTR1   =  0.0228
HTR2   =  0.01
DO I= 1,MAXSP
  PSOFT(I) = 0.0
ENDDO
!----------
!  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
!----------
!  V2T() - UNITS ARE LB/CUFT BY SPECIES FRIN 'WOOD HANDBOOK' USDA
!  FOREST PRODUCTS AG. HANDBOOK 72.  1974. DENSITY OF PINYON, JUNIPER,
!  GAMBEL FROM CHOJNACKY 1992.
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
!
!  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
!  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
!  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
!----------
DO I= 1,MAXSP

  SELECT CASE (I)
!----------
!  WESTERN WHITE PINE (USES PP FOR MOST WP ATTRIBUTES)
!----------
    CASE (1)
      V2T(I)     =  22.5
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 110.0
      DECAYX(I)  =   1.1
      FALLX(I)   =   0.9
      DO J= 1,4
        HTX(I,J) =   0.9
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
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
      V2T(I)     =  28.1
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
!  GRAND FIR
!----------
    CASE (4)
      V2T(I)     =  21.8
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
!  WESTERN HEMLOCK
!----------
    CASE (5)
      V2T(I)     =  26.2
      LEAFLF(I)  =   5.0
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
!  WESTERN REDCEDAR (USES DF FOR SOME RC ATTRIBUTES)
!----------
    CASE (6)
      V2T(I)     =  19.3
      LEAFLF(I)  =   5.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.1
      DO J= 1,4
        HTX(I,J) =   1.1
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
!  MOUNTAIN HEMLOCK
!----------
    CASE (11)
      V2T(I)     =  26.2
      LEAFLF(I)  =   4.0
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
!  WHITEBARK PINE
!----------
    CASE (12)
      V2T(I)     =  22.5 !w. white pine
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
!  LIMBER PINE
!----------
    CASE (13)
      V2T(I)     =  22.5  !w. white pine
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
!  SUBALPINE LARCH
!----------
    CASE (14)
      V2T(I)     =  19.3  !subalpine fir
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
!  PINYON PINE
!----------
    CASE (15)
      V2T(I)     =  31.8
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
!  ROCKY MOUNTAIN JUNIPER
!----------
    CASE (16)
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
!  PACIFIC YEW
!----------
    CASE (17)
      V2T(I)     =  26.2 ! bald cypress
      LEAFLF(I)  =   7.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   1
      LSW(I)     = .TRUE.
!----------
!  QUAKING ASPEN
!----------
    CASE (18)
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
!  COTTONWOOD
!----------
    CASE (19)
      V2T(I)     =  19.3 !black cottonwood
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
!  MOUNTAIN MAPLE
!----------
    CASE (20)
      V2T(I)     =  30.6 !red maple
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
!  PAPER BIRCH
!----------
    CASE (21)
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
!  OTHER HARDWOODS
!----------
    CASE (22)
      V2T(I)     =  23.1
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
    CASE (23)
      V2T(I)     =  26.2
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,4
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
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
!  PARAMETERS FOR POST-BURN SNAG FALL RATES:
!----------
PBSCOR =  0.0
PBSOFT =  1.0
PBSMAL =  0.9
PBSIZE = 12.0
PBTIME =  7.0
!----------
!  THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
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

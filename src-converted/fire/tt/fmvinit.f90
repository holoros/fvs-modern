SUBROUTINE FMVINIT
IMPLICIT NONE
!----------
! FIRE-TT $Id$
!----------
!  Purpose:
!      Initialize variant-specific variables for the Fire Model
!      Draws heavily from FFE-UT
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
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'FMFCOM.f90'
!
!OMMONS
!----------
INTEGER I,J
!
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
DKR(7,1) =  0.015
DKR(8,1) =  0.015
DKR(9,1) =  0.015
!
DO I = 1,9
  DO J = 2,4
    DKR(I,J) = DKR(I,1) * 0.45 ! same as UT
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
HTXSFT =  2.0
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
!     1974.
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
! SPECIES ORDER FOR TETONS VARIANT:
!
!  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
!
! VARIANT EXPANSION:
! BS USES ES EQUATIONS FROM TT
! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
! PP USES PP EQUATIONS FROM CI
! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
! MM USES MM EQUATIONS FROM IE
! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
! OS USES OT (OTHER SP.) EQUATIONS FROM TT
!----------
DO I= 1,MAXSP
!
  SELECT CASE (I)
!----------
!         whitebark pine, limber pine, other softwoods
!----------
    CASE (1,2,17)
      V2T(I)     =  22.5  ! from white pine
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   0.41
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         Douglas-fir (NI)
!----------
    CASE (3)
      V2T(I)     =  28.1  ! north DF in Wood Handbook
      LEAFLF(I)  =   5.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.1
      FALLX(I)   =   0.81 ! 95% of 15" down in 30 yr
      HTX(I,1)   =   1.01 ! hard, first 50%
      HTX(I,2)   =   4.61 ! hard, second 50%
      DKRCLS(I)  =   3
      LSW(I)     = .TRUE.
!----------
!         ponderosa pine
!----------
    CASE (10)
      V2T(I)     =  23.7
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0   ! NI default
      DO J= 1,2
        HTX(I,J) =   0.001 ! no height loss
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         singleleaf pinyon
!----------
    CASE (4)
      V2T(I)     =  31.8
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   0.0978
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         quaking aspen
!         special rules following burn **FMSNAG**
!----------
    CASE (6)
      V2T(I)     =  21.8
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =  15.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   2.4
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!         lodgepole pine
!----------
    CASE (7)
      V2T(I)     =  23.7
      LEAFLF(I)  =   3.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  50.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.60 ! 95% of 15" down in 15 yr
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         Engelmann spruce, Blue spruce
!         special handling of FALLX(8) in **FMSNAG**
!----------
    CASE (5,8)
      V2T(I)     =  20.6
      LEAFLF(I)  =   6.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   0.61 !! 95% of 15" down in 40 yr
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         subalpine fir
!----------
    CASE (9)
      V2T(I)     =  19.3
      LEAFLF(I)  =   7.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  40.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   0.81  !! 95% of 15" down in 30 yr
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .TRUE.
!----------
!         Utah juniper
!----------
    CASE (11)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   0.0978
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .TRUE.
!----------
!         Rocky Mtn juniper
!----------
    CASE (12)
      V2T(I)     =  34.9
      LEAFLF(I)  =   4.0
      TFALL(I,3) =  20.0
      ALLDWN(I)  = 150.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   0.0978
      ENDDO
      DKRCLS(I)  =   2
      LSW(I)     = .TRUE.
!----------
!         bigtooth maple
!----------
    CASE (13)
      V2T(I)     =  27.4
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!         Rocky Mtn maple
!----------
    CASE (14)
      V2T(I)     =  30.6  ! red maple
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  = 100.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!         narrowleaf cottonwood
!----------
    CASE (15)
      V2T(I)     =  19.3
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =   5.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   4.0
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!         curlleaf mountain-mahogany
!----------
    CASE (16)
      V2T(I)     =  21.8
      LEAFLF(I)  =   1.0
      TFALL(I,0) =   1.0
      TFALL(I,3) =  15.0
      ALLDWN(I)  =  90.0
      DECAYX(I)  =   1.0
      FALLX(I)   =   1.0
      DO J= 1,2
        HTX(I,J) =   1.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.
!----------
!         other hardwoods
!----------
    CASE (18)
      V2T(I)     =  19.3
      LEAFLF(I)  =   1.0
      TFALL(I,3) =  10.0
      ALLDWN(I)  =   5.0
      DECAYX(I)  =   0.9
      FALLX(I)   =   4.0
      DO J= 1,2
        HTX(I,J) =   0.0
      ENDDO
      DKRCLS(I)  =   4
      LSW(I)     = .FALSE.

  END SELECT
!----------
!       HEIGHT LOSS RATE FOR SOFT SNAGS IS THE SAME AS HARD SNAGS
!       IN BOTH THE FIRST 50% OF HT LOSS AND THE 50-95% OF HT LOSS
!----------
  HTX(I,3) = HTX(I,1)
  HTX(I,4) = HTX(I,2)
!----------
!       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
!----------
  SELECT CASE (I)
!
  CASE (13,16)
    TFALL(I,1) =   5.0
    TFALL(I,2) =   5.0
    TFALL(I,4) = TFALL(I,3)
    TFALL(I,5) = TFALL(I,3)
!----------
!       DEAD LEAF FALL CANNOT BE > LIVE
!----------
    TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
!
  CASE (4,11,12,15,18)
    TFALL(I,1) = 10.0
    TFALL(I,2) = 15.0
    TFALL(I,4) = TFALL(I,3)
    TFALL(I,5) = TFALL(I,3)
!----------
!       DEAD LEAF FALL CANNOT BE > LIVE
!----------
    TFALL(I,0) = MIN(2.0, LEAFLF(I))
!
  CASE DEFAULT
    TFALL(I,1) = 5.0
    TFALL(I,2) = TFALL(I,1)
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
!
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

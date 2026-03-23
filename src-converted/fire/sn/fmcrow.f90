SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-SN $Id$
!----------
!     CALLED FROM: FMSDIT, FMPRUN
!     CALLS:
!
!  PURPOSE:
!     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
!     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
!     RECORD IN THE CURRENT STAND.
!----------
!  LOCAL VARIABLE DEFINITIONS:
!     D:        DBH
!     H:        HEIGHT
!     IC:       LENGTH OF LIVE CROWN
!     SP:       SPECIES
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
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'SNCOM.f90'
!
!
!OMMONS
!----------
!  VARIABLE DECLARATIONS
!----------
LOGICAL DEBUG
INTEGER I,J,IC,ISPMAP(MAXSP),SPI
REAL    D,H,SG,XV(0:5)


!     INDEX TO THE CROWN EQUATIONS USED IN FMCROWE. EASTERN EQUATIONS ARE
!     BASED ON LS-FFE SPECIES NUMBERING;  IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
!
!     I   NAME                     MAPS TO          FMCROWE
! --------------------------------------------------------------
!     1 = fir sp.                  balsam fir           8
!     2 = redcedar                      -              14
!     3 = spruce sp.               black spruce         9
!     4 = sand pine                red pine natural     3
!     5 = shortleaf pine           red pine natural     3
!     6 = slash pine               red pine natural     3
!     7 = spruce pine              red pine natural     3
!     8 = longleaf pine            red pine natural     3
!     9 = table mountain pine      red pine natural     3
!    10 = pitch pine               red pine natural     3
!    11 = pond pine                red pine natural     3
!    12 = eastern white pine            -               5
!    13 = loblolly pine            red pine natural     3
!    14 = virginia pine            red pine natural     3
!    15 = baldcypress              eastern redcedar    14
!    16 = pondcypress              eastern redcedar    14
!    17 = hemlock                       -              12
!    18 = Florida maple            sugar maple         26
!    19 = boxelder                      -              50
!    20 = red maple                     -              19
!    21 = silver maple                  -              18
!    22 = sugar maple                   -              26
!    23 = buckeye/horsechestnut    comm. hardwoods     44
!    24 = birch sp.                yellow birch        24
!    25 = sweet birch              yellow birch        24
!    26 = american hornbeam             -              53
!    27 = hickory sp.              shagbark hickory    39
!    28 = catalpa                  comm. hardwoods     44
!    29 = hackberry sp.                 -              55
!    30 = eastern redbud           comm. hardwoods     44
!    31 = flowering dogwood             -              56
!    32 = common persimmon         comm. hardwoods     44
!    33 = american beech                -              28
!    34 = ash sp.                  green ash           16
!    35 = white ash                     -              29
!    36 = black ash                     -              15
!    37 = green ash                     -              16
!    38 = honeylocust              comm. hardwoods     44
!    39 = loblolly-bay             comm. hardwoods     44
!    40 = silverbell               comm. hardwoods     44
!    41 = american holly           comm. hardwoods     44
!    42 = butternut                     -              45
!    43 = black walnut                  -              46
!    44 = sweet gum                comm. hardwoods     44
!    45 = yellow-poplar            comm. hardwoods     44
!    46 = magnolia sp.             comm. hardwoods     44
!    47 = cucumbertree             comm. hardwoods     44
!    48 = southern magnolia        comm. hardwoods     44
!    49 = sweetbay                 comm. hardwoods     44
!    50 = bigleaf magnolia         comm. hardwoods     44
!    51 = apple sp.                     -              58
!    52 = mulberry sp.             apple sp.           58
!    53 = water tupelo                  -              59
!    54 = black gum                     -              59
!    55 = swamp tupelo                  -              59
!    56 = e. hophornbeam                -              47
!    57 = sourwood                 comm. hardwoods     44
!    58 = redbay                   comm. hardwoods     44
!    59 = sycamore                      -              60
!    60 = cottonwood                    -              17
!    61 = bigtooth aspen                -              40
!    62 = black cherry                  -              20
!    63 = white oak                     -              30
!    64 = scarlet oak              northern red oak    34
!    65 = southern red oak         northern red oak    34
!    66 = cherrybark oak           northern red oak    34
!    67 = turkey oak               northern red oak    34
!    68 = laurel oak               northern red oak    34
!    69 = overcup oak              white oak           30
!    70 = blackjack oak            northern red oak    34
!    71 = swamp chestnut oak       white oak           30
!    72 = chinkapin oak                 -              33
!    73 = water oak                northern red oak    34
!    74 = chestnut oak             white oak           30
!    75 = northern red oak              -              34
!    76 = shumard oak              northern red oak    34
!    77 = post oak                 white oak           30
!    78 = black oak                     -              35
!    79 = live oak                 white oak           30
!    80 = black locust                  -              48
!    81 = willow                        -              64
!    82 = sassafras                     -              67
!    83 = basswood                      -              25
!    84 = elm sp.                  slippery elm        22
!    85 = winged elm               slippery elm        22
!    86 = american elm                  -              21
!    87 = slippery elm                  -              22
!    88 = softwoods, misc.         eastern redcedar    14
!    89 = hardwoods, misc.         comm. hardwoods     44
!    90 = unknown/not listed       comm. hardwoods     44


DATA ISPMAP /8,14, 9, 3, 3, 3, 3, 3, 3, 3, &
                3, 5, 3, 3,14,14,12,26,50,19, &
               18,26,44,24,24,53,39,44,55,44, &
               56,44,28,16,29,15,16,44,44,44, &
               44,45,46,44,44,44,44,44,44,44, &
               58,58,59,59,59,47,44,44,60,17, &
               40,20,30,34,34,34,34,34,30,34, &
               30,33,34,30,34,34,30,35,30,48, &
               64,67,25,22,22,21,22,14,44,44/

!
!----------
!  CHECK FOR DEBUG
!----------
CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)
!
IF (ITRN.EQ.0) RETURN
!
DO 999 I = 1,ITRN
!----------
!  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
!  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
!  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
!----------
  IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
  IF (GROW(I) .LT. 1) GOTO 999
!----------
!  ARGUMENTS TO PASS
!----------
  SPI = ISPMAP(ISP(I))
  D   = DBH(I)
  H   = HT(I)
  IC  = ICR(I)
  SG  = V2T(ISP(I))
!----------
!  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
!  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
!----------
  DO J = 0,5
    XV(J) = 0.0
  ENDDO
!
  CALL FMCROWE(SPI,ISP(I),D,H,IC,SG,XV)
!----------
!  COPY TEMPORARY VALUES TO FFE ARRAY
!----------
  DO J = 0,5
    CROWNW(I,J) = XV(J)
    IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J, &
       ' CROWNW=',CROWNW(I,J)
  ENDDO
!
999 CONTINUE
!
RETURN
END

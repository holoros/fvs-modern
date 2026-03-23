SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-WC $Id$
!----------
!     CALLED FROM: FMSDIT, FMPRUN
!     CALLS        RDPSRT
!                  PCTILE
!  Purpose:
!     This subroutine calculates CROWNW(tree,size), the weight of
!     various sizes of crown material that is associated with each tree
!     record in the current stand.  These weights depend on tree
!     species, diameter, height, and crown ratio according to the
!     relationships described in Brown & Johnston, 1976, 'Debris
!     Prediction System', Fuel Science RWU 2104, which is itself
!     based primarily on Res Paper INT-197.
!
!     NOTE:  The allocation between crown size class 4 and 5 is
!            somewhat arbitrary, with class 5 currently not receiving any.
!
!
!  Local variable definitions:
!
!     D;        Dbh
!     H:        Height
!     HPCT:     Height PerCenTile ranking of each tree list element
!     HPOINT:   Height POINTer: specifies which tree list element has
!               the tallest trees, next tallest, and so on.
!     IC:       length of live Crown
!     SP:       SPecies
!
!  Common block variables and parameters:
!
!**********************************************************************

!.... Parameter include files.
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'

!.... Common include files.
INCLUDE 'FMCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'

!.... Parameter statements.

LOGICAL DEBUG
INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
INTEGER ITR,SPIW,SPIE
REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)

!     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
!     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
!     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
!     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF

!     I   NAME                     MAPS TO          WEST   EAST
! --------------------------------------------------------------
!     1 = PACIFIC SILVER FIR       grand fir         4
!     2 = WHITE FIR                grand fir         4
!     3 = GRAND FIR                -                 4
!     4 = SUBALPINE FIR            -                 1
!     5 = CALIFORNIA RED FIR/      grand fir         4
!         SHASTA RED FIR
!     6 = ---                      -                         0
!     7 = NOBLE FIR                grand fir         4
!     8 = ALASKA CEDAR/W LARCH     w larch           8
!     9 = INCENSE-CEDAR            -                20
!    10 = ENGELMANN SPRUCE/        -                18
!         SITKA SPRUCE
!    11 = LODGEPOLE PINE           -                11
!    12 = JEFFREY PINE             w white pine     15
!    13 = SUGAR PINE               w white pine     15
!    14 = WESTERN WHITE PINE       -                15
!    15 = PONDEROSA PINE           -                13
!    16 = DOUGLAS-FIR              -                 3
!    17 = COAST REDWOOD            giant sequoia    19
!    18 = WESTERN REDCEDAR         -                 7
!    19 = WESTERN HEMLOCK          -                 6
!    20 = MOUNTAIN HEMLOCK         -                24
!    21 = BIGLEAF MAPLE            -                 5
!    22 = RED ALDER                -                23
!    23 = WH. ALDER/PAC.MADRONE    -                10
!    24 = PAPER BIRCH              -                        43
!    25 = GIANT CHINKAPIN/TANOAK   tanoak           17
!    26 = QUAKING ASPEN            -                        41
!    27 = BLACK COTTONWOOD         eastern cottonwood       17
!    28 = OR.WH OAK/CA.BL OAK      tanoak           17
!    29 = WESTERN JUNIPER          R.M. juniper     16
!    30 = SUBALPINE LARCH          subalpine fir     1
!    31 = WHITEBARK PINE           -                14
!    32 = KNOBCONE PINE            lodgepole pine   11
!    33 = PACIFIC YEW              western redcedar  7
!    34 = PACIFIC DOGWOOD          flowering dogwood        56
!    35 = HAWTHORN                 hawthorn sp              57
!    36 = BITTER CHERRY            pin cherry               61
!    37 = WILLOW                   willow sp                64
!    38 = ---                      -                         0
!    39 = OTHER                    quaking aspen            41
! --------------------------------------------------------------

DATA ISPMAP / 4, 4, 4, 1, 4, 0, 4, 8,20,18, &
                11,15,15,15,13, 3,19, 7, 6,24, &
                 5,23,10,43,17,41,17,17,16, 1, &
                14,11, 7,56,57,61,64, 0,41/

!     CHECK FOR DEBUG

CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)

IF (ITRN.EQ.0) RETURN

!     YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
!     TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
!     IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:

CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)

!     NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
!     NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
!     EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
!     ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
!     PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
!     INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.

CALL PCTILE (ITRN, HPOINT, PROB, HPCT, JUNK)

DO 999 I = 1,ITRN

!       INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
!       TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
!       IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.

  IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
  IF (GROW(I) .LT. 1) GOTO 999

!       ARGUMENTS TO PASS

  SPIW = ISP(I)
  SPIE = ISPMAP(SPIW)

  D   = DBH(I)
  H   = HT(I)
  IC  = ICR(I)
  ITR = ITRUNC(I)
  HP  = HPCT(I)
  SG  = V2T(SPIW)

!       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
!       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

  DO J = 0,5
    XV(J) = 0.0
  ENDDO

  SELECT CASE (SPIW)
    CASE (24,26,27,34,35,36,37,39)
      CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
    CASE DEFAULT
      CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
  END SELECT

!       COPY TEMPORARY VALUES TO FFE ARRAY

  DO J = 0,5
    CROWNW(I,J) = XV(J)
    IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J, &
       ' CROWNW=',CROWNW(I,J)
  ENDDO

999 CONTINUE

RETURN
END



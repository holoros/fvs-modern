SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-CA $Id$
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
!     1 = PORT ORFORD CEDAR        western redcedar  7
!     2 = INCENSE CEDAR            -                20
!     3 = WESTERN REDCEDAR         -                 7
!     4 = WHITE FIR                grand fir         4
!     5 = CALIFORNIA RED FIR       grand fir         4
!     6 = SHASTA RED FIR           grand fir         4
!     7 = DOUGLAS-FIR              -                 3
!     8 = WESTERN HEMLOCK          -                 6
!     9 = MOUNTAIN HEMLOCK         -                24
!    10 = WHITEBARK PINE           -                14
!    11 = KNOBCONE PINE            lodgepole pine   11
!    12 = LODGEPOLE PINE           -                11
!    13 = COULTER PINE             lodgepole pine   11
!    14 = LIMBER PINE              lodgepole pine   11
!    15 = JEFFREY PINE             w white pine     15
!    16 = SUGAR PINE               w white pine     15
!    17 = WESTERN WHITE PINE       -                15
!    18 = PONDEROSA PINE           -                13
!    19 = MONTEREY PINE            ponderosa pine   13
!    20 = GRAY PINE                lodgepole pine   11
!    21 = WESTERN JUNIPER          R.M. juniper     16
!    22 = BREWER SPRUCE            Engelmann spruce 18
!    23 = GIANT SEQUOIA            -                19
!    24 = PACIFIC YEW              western redcedar  7
!    25 = OTHER SOFTWOODS          ponderosa pine   13
!    26 = COAST LIVE OAK           tanoak           17
!    27 = CANYON LIVE OAK          tanoak           17
!    28 = BLUE OAK                 Cal black oak    21
!    29 = ENGELMANN OAK            tanoak           17
!    30 = OREGON WHITE OAK         Cal black oak    21
!    31 = CALIFORNIA BLACK OAK     -                21
!    32 = VALLEY WHITE OAK         Cal black oak    21
!    33 = INTERIOR LIVE OAK        tanoak           17
!    34 = BIGLEAF MAPLE            -                 5
!    35 = CALIFORNIA BUCKEYE       comm. hardwoods          44
!    36 = RED ALDER                -                23
!    37 = PACIFIC MADRONE          -                10
!    38 = GOLDEN CHINKAPIN         tanoak           17
!    39 = PACIFIC DOGWOOD          flowering dogwood        56
!    40 = OREGON ASH               white ash                29
!    41 = WALNUT                   black walnut             46
!    42 = TANOAK                   -                17
!    43 = CALIFORNIA SYCAMORE      sycamore                 60
!    44 = QUAKING ASPEN            -                        41
!    45 = BLACK COTTONWOOD         eastern cottonwood       17
!    46 = WILLOW                   willow sp                64
!    47 = CALIFORNIA NUTMEG        tanoak           17
!    48 = CALIFORNIA LAUREL        tanoak           17
!    49 = OTHER HARDWOODS          Cal black oak    21
!    50 = COAST REDWOOD            -                19
DATA ISPMAP / 7,20, 7, 4, 4, 4, 3, 6,24,14, &
                11,11,11,11,15,15,15,13,13,11, &
                16,18,19, 7,11,17,17,21,17,21, &
                21,21,17, 5,44,23,10,17,56,29, &
                46,17,60,41,17,64,17,17,21,19/


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
    CASE (35,39,40,41,43,44,45,46)
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



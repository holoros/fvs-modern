SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-CR $Id$
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
!------------------------------------------------------------------------
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
INCLUDE 'PLOT.f90'

!.... Parameter statements.

LOGICAL DEBUG
INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
INTEGER ITR,SPIW,SPIE
REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
!
!     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
!     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
!     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
!     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
!
!     I   NAME                     MAPS TO          WEST   EAST
! --------------------------------------------------------------
!     1 = SUBALPINE FIR            -                 1
!     2 = CORKBARK FIR             subalpine         1
!     3 = DOUGLAS-FIR              -                 3
!     4 = GRAND FIR                -                 4
!     5 = WHITE FIR                grand fir         4
!     6 = MOUNTAIN HEMLOCK         -                24
!     7 = WESTERN REDCEDAR         -                 7
!     8 = WESTERN LARCH            -                 8
!     9 = BRISTLECONE PINE         -                 9
!    10 = LIMBER PINE              lodgepole pine   11
!    11 = LODGEPOLE PINE           -                11
!    12 = PINYON PINE              -                12
!    13 = PONDEROSA PINE           -                13/25
!    14 = WHITEBARK PINE           -                14
!    15 = SOUTHWESTERN WHITE PINE  w white pine     15
!    16 = UTAH JUNIPER             -                16
!    17 = BLUE SPRUCE              Engelmann spruce 18
!    18 = ENGELMANN SPRUCE         -                18
!    19 = WHITE SPRUCE             Engelmann spruce 18
!    20 = QUAKING ASPEN            -                       41
!    21 = NARROWLEAF COTTONWOOD    eastern cottonwood      17
!    22 = PLAINS COTTONWOOD        eastern cottonwood      17
!    23 = GAMBEL OAK               Gambel oak       22
!    24 = ARIZONA WHITE OAK        Gambel oak       22
!    25 = EMORY OAK                Gambel oak       22
!    26 = BUR OAK                  Gambel oak       22
!    27 = SILVERLEAF OAK           Gambel oak       22
!    28 = PAPER BIRCH              -                       43
!    29 = ALLIGATOR JUNIPER        Utah juniper     16
!    30 = ROCKY MOUNTAIN JUNIPER   Utah juniper     16
!    31 = ONESEED JUNIPER          Utah juniper     16
!    32 = EASTERN REDCEDAR         Utah juniper     16
!    33 = SINGLELEAF PINYON        pinyon pine      12
!    34 = BORDER PINYON            pinyon pine      12
!    35 = ARIZONA PINYON           pinyon pine      12
!    36 = CHIHUAHUA PINE           ponderosa pine   13
!    37 = OTHER SOFTWOODS          lodgepole pine   11
!    38 = OTHER HARDWOODS          eastern cottonwood      17
!
DATA ISPMAP /  1,  1,  3,  4,  4, 24,  7,  8,  9, 11, &
                 11, 12, 13, 14, 15, 16, 18, 18, 18, 41, &
                 17, 17, 22, 22, 22, 22, 22, 43, 16, 16, &
                 16, 16, 12, 12, 12, 13, 11, 17/

!     MAKE CHANGE FOR PONDEROSA PINE IN THE BLACK HILLS OR NEBRASKA NF
IF ((KODFOR .EQ. 203) .OR. (KODFOR .EQ. 207)) ISPMAP(13) = 25


!     CHECK FOR DEBUG.

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

CALL PCTILE (ITRN,HPOINT,PROB,HPCT,JUNK)

DO 999 I = 1,ITRN
!
!       INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
!       TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
!       IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
!
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

!
!       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
!       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
!
  DO J = 0,5
    XV(J) = 0.0
  ENDDO

  SELECT CASE (SPIW)
    CASE (20:22,28,38)
      CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
    CASE DEFAULT
      CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
  END SELECT
!
!       COPY TEMPORARY VALUES TO FFE ARRAY
!
  DO J = 0,5
    CROWNW(I,J) = XV(J)
    IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J, &
       ' CROWNW=',CROWNW(I,J)
  ENDDO

999 CONTINUE

RETURN
END

!     PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**

SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
IMPLICIT NONE

INTEGER I10,I11,I12
REAL    X10,X11

I10 = 0
I11 = 0
I12 = 0

X10 = 0.0
X11 = 0.0

RETURN
END



SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-SO $Id$
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

!.... Parameter statements.

LOGICAL DEBUG
INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
INTEGER ITR,SPIW,SPIE
REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5),CRTOT
!
!     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
!     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
!     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
!     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
!
!     I   NAME                     MAPS TO          WEST   EAST
! --------------------------------------------------------------
!     1 = WESTERN WHITE PINE       -                15
!     2 = SUGAR PINE               w white pine     15
!     3 = DOUGLAS-FIR              -                 3
!     4 = WHITE FIR                grand fir         4
!     5 = MOUNTAIN HEMLOCK         -                24
!     6 = INCENSE CEDAR            -                20
!     7 = LODGEPOLE PINE           -                11
!     8 = ENGELMANN SPRUCE         -                18
!     9 = RED FIR                  grand fir         4
!    10 = PONDEROSA PINE           -                13
!    11 = WESTERN JUNIPER          R.M. juniper     16
!    12 = GRAND FIR                                  4
!    13 = SUBALPINE FIR                              1
!    14 = PACIFIC SILVER FIR       grand fir         4
!    15 = NOBLE FIR                grand fir         4
!    16 = WHITEBARK PINE                            14
!    17 = WESTERN LARCH                              8
!    18 = WESTERN REDCEDAR                           7
!    19 = WESTERN HEMLOCK                            6
!    20 = PACIFIC YEW              western redcedar  7
!    21 = WHITE ALDER              red alder        23
!    22 = RED ALDER                                 23
!    23 = BIG LEAF MAPLE                             5
!    24 = QUAKING ASPEN            -                         41
!    25 = BLACK COTTONWOOD         eastern cottonwood        17
!    26 = BITTER CHERRY            pin cherry                61
!    27 = OREGON WHITE OAK         tanoak           17
!    28 = WILLOW                   willow sp.                64
!    29 = GIANT CHINKAPIN          tanoak           17
!    30 = CURL-LF. MT. MAHOG.      quaking aspen             41
!    31 = BIRCHLEAF MT. MAHOG.     quaking aspen             41
!    32 = OTHER SOFTWOODS          Douglas-fir       3
!    33 = OTHER HARDWOODS          quaking aspen             41
!
DATA ISPMAP /15,15, 3, 4,24,20,11,18, 4,13, &
                16,4,1,4,4,14,8,7,6,7, &
                23,23,5,41,17,61,17,64,17,41, &
                41,3,41/

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

!        ARGUMENTS TO PASS

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
    CASE (24,25,26,28,30,31,33)
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

  CRTOT = 0
  DO J = 0,5
    CRTOT = CROWNW(I,J) + CRTOT
  ENDDO
  IF (DEBUG) WRITE(JOSTND,*) 'ISP=',SPIW,' D=',D,' CRTOT=',CRTOT

999 CONTINUE

RETURN
END


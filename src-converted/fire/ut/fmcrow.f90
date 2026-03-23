SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-UT $Id$
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
!----------
!OMMONS
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
!OMMONS
!----------
LOGICAL DEBUG
INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
INTEGER ITR,SPIW,SPIE
REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
!----------
!     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
!     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
!     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
!     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
!
!     I   NAME                     MAPS TO          WEST   EAST
! --------------------------------------------------------------
!     1 = WHITEBARK PINE           -                14
!     2 = LIMBER PINE              lodgepole pine   11
!     3 = DOUGLAS-FIR              -                 3
!     4 = WHITE FIR                grand fir         4
!     5 = BLUE SPRUCE              Engelmann spruce 18
!     6 = QUAKING ASPEN            -                        41
!     7 = LODGEPOLE PINE           -                11
!     8 = ENGELMANN SPRUCE         -                18
!     9 = SUBALPINE FIR            -                 1
!    10 = PONDEROSA PINE           -                13
!    11 = COMMON PINYON            -                12
!    12 = WESTERN JUNIPER          R Mtn juniper    16
!    13 = GAMBEL OAK               Gambel oak       22
!    14 = SINGLELEAF PINYON        common pinyon    12
!    15 = ROCKY MTN JUNIPER        -                16
!    16 = UTAH JUNIPER             R Mtn juniper    16
!    17 = GB BRISTLECONE PINE      -                 9
!    18 = NARROWLEAF COTTONWOOD    eastern cottonwood       17
!    19 = FREMONT COTTONWOOD       eastern cottonwood       17
!    20 = CURLLEAF MTN MAHOGANY    quaking aspen            41
!    21 = BIGTOOTH MAPLE           bigleaf maple     5
!    22 = BOX ELDER                -                        50
!    23 = OTHER SOFTWOODS          whitebark pine   14
!    24 = OTHER HARDWOODS          Gambel oak       22
!
!----------
DATA ISPMAP / &
    14, 11,  3,  4, 18, 41, 11, 18,  1, 13, &
    12, 16, 22, 12, 16, 16,  9, 17, 17, 41, &
     5, 50, 14, 22/


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
    CASE (6,18:20,22)
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



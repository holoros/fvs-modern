SUBROUTINE FMCROW
IMPLICIT NONE
!----------
! FIRE-AK $Id$
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
!     BASED ON LS-FFE; IN THE TABLE BELOW, A '-' IN THE
!     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
!
!                                  MAPPPED TO
!     I   SPECIES                  WEST   EAST
! --------------------------------------------------------------
!     1   pacific silver fir         4
!     2   subalpine fir              1
!     3   alaska-cedar               8
!     4   tamarack                         10
!     5   white spruce                      6
!     6   Lutz's spruce                     6
!     7   black spruce                      9
!     8   sitka spruce              18
!     9   lodgepole pine            11
!    10   western redcedar           7
!    11   western hemlock            6
!    12   mountain hemlock          24
!    13   other softwoods                   6
!    14   alder species             23
!    15   red alder                 23
!    16   paper birch                      43
!    17   Alaska birch                     43
!    18   balsam poplar                    42
!    19   quaking aspen                    41
!    20   black cottonwood                 17
!    21   willow species                   64
!    22   Scouler's willow                 64
!    23   other hardwoods                  17
! --------------------------------------------------------------

DATA ISPMAP / 4,  1, 8, 10,  6,  6,  9, 18, 11,  7, &
                 6, 24, 6, 23, 23, 43, 43, 42, 41, 17, &
                64, 64, 17 /

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
!
!         SPECIES THAT USE CROWN ROUTINE FOR THE EAST
    CASE (4:7,13,16:)
      CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
!
!         SPECIES THAT USE CROWN ROUTINE FOR THE WEST
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
!
!
!----------
!  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
!----------
!      SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
!      IMPLICIT NONE
!
!      INTEGER I10,I11,I12
!      REAL    X10,X11
!
!      I10 = 0
!      I11 = 0
!      I12 = 0
!
!      X10 = 0.0
!      X11 = 0.0
!
!      RETURN
!      END


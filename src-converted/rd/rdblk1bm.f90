BLOCK DATA RDBLK1
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This block data file initializes constants in the Root Disease
!     extension to FVS.
!
!  Previous revision date 06/05/09
!
!OMMONS
!

!.... PARAMETER INCLUDE FILES

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
INCLUDE 'METRIC.f90'

!.... COMMON INCLUDE FILES

INCLUDE 'RDCOM.f90'
INCLUDE 'RDCRY.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'RDADD.f90'


!.... The array IRTSPC is used to index the species dependent arrays
!.... HABFAC, PNINF, PKILLS, RRJSP, ISPS, DBIFAC, HTIFAC, PROOT,
!.... RSLOP, ROWDOM, ROWIBP, RRPSWT, SSSFAC, IDITYP, PCOLO.
!.... In the root disease model, the defaults for these variables
!.... are indexed as follows :
!....
!.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
!.... Species  | WP | WL | DF | GF | WH | RC | LP | ES | AF | PP |
!....
!.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
!.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
!....
!.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
!.... Species  | CB | WB | LM | CO | WS | JU | OC | GS | BO | OTH|
!....
!.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
!.... Species  | JP | TO | PI | YC | RW | LL | KP | PY | NF | NH |
!....
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.
!
! RD
! INDX SP   SPECIES LIST FOR BLUE MOUNTAINS VARIANT.
!
!   1  WP   1 = WESTERN WHITE PINE   119 WP  PIMO3  PINUS MONTICOLA
!   2  WL   2 = WESTERN LARCH        073 WL  LAOC   LARIX OCCIDENTALIS
!   3  DF   3 = DOUGLAS-FIR          202 DF  PSME   PSEUDOTSUGA MENZIESII
!   4  GF   4 = GRAND FIR            017 GF  ABGR   ABIES GRANDIS
!  11  MH   5 = MOUNTAIN HEMLOCK     264 MH  TSME   TSUGA MERTENSIANA
!  26  JU   6 = WESTERN JUNIPER      064 WJ  JUOC   JUNIPERUS OCCIDENTALIS
!   7  LP   7 = LODGEPOLE PINE       108 LP  PICO   PINUS CONTORTA
!   8  ES   8 = ENGLEMANN SPRUCE     093 ES  PIEN   PICEA ENGELMANNII
!   9  AF   9 = SUBALPINE FIR        019 AF  ABLA   ABIES LASIOCARPA
!  10  PP  10 = PONDEROSA PINE       122 PP  PIPO   PINUS PONDEROSA
!  22  WB  11 = WHITEBARK PINE       101 WB  PIAL   PINUS ALBICAULIS
!  23  LM  12 = LIMBER PINE          113 LM  PIFL2  PINUS FLEXILIS
!  38  PY  13 = PACIFIC YEW          231 PY  TABR2  TAXUS BREVIFOLIA
!  34  YC  14 = ALASKA YELLOW CEDAR  042 YC  CHNO   CHAMAECYPARIS NOOTKATENSIS
!  19  AS  15 = QUAKING ASPEN        746 AS  POTR5  POPULUS TREMULOIDES
!  24  CO  16 = BLACK COTTONWOOD     747 CW  POBAT  POPULUS BALSAMIFERA
!  17  OS  17 = OTHER SOFTWOODS      298 OS  2TN
!  18  OH  18 = OTHER HARDWOODS      998 OH  2TB
!
!....
!.... The following IRTSPC is used with variant BM 18 species :

DATA IRTSPC / 1,  2,  3,  4, 11, 26,  7,  8,  9, &
                10, 22, 23, 38, 34, 19, 24, 17, 18/

DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

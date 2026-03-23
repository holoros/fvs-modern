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
!  Previous revision date 04/30/09
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

!.... The following IRTSPC is used with variant WC :
!.... WC has 39 species.
! RD
! INDX SP   SPECIES LIST FOR WEST CASCADES VARIANT
!
!  16  SF     1 = PACIFIC SILVER FIR (SF)      ABIES AMABILIS
!  13  WF     2 = WHITE FIR (WF)               ABIES CONCOLOR
!   4  GF     3 = GRAND FIR (GF)               ABIES GRANDIS
!   9  AF     4 = SUBALPINE FIR (AF)           ABIES LASIOCARPA
!  15  RF     5 = CALIFORNIA RED FIR (RF)/     ABIES MAGNIFICA
!                 SHASTA RED FIR
!  40  NH     6 = ---
!  39  NF     7 = NOBLE FIR (NF)               ABIES PROCERA
!  34  YC     8 = ALASKA CEDAR (YC)/           CHAMAECYPARIS NOOTKATENSIS
!                 WESTERN LARCH                LARIX OCCIDENTALIS
!  14  IC     9 = INCENSE CEDAR (IC)           LIBOCEDRUS DECURRENS
!   8  ES    10 = ENGELMANN SPRUCE (ES)/       PICEA ENGELMANNII
!                 SITKA SPRUCE                 PICEA SITCHENSIS
!   7  LP    11 = LODGEPOLE PINE (LP)          PINUS CONTORTA
!  31  JP    12 = JEFFREY PINE (JP)            PINUS JEFFREYI
!  12  SP    13 = SUGAR PINE (SP)              PINUS LAMBERTIANA
!   1  WP    14 = WESTERN WHITE PINE (WP)      PINUS MONTICOLA
!  10  PP    15 = PONDEROSA PINE (PP)          PINUS PONDEROSA
!   3  DF    16 = DOUGLAS-FIR (DF)             PSEUDOTSUGA MENZIESII
!  35  RW    17 = COAST REDWOOD (RW)           SEQUOIA SEMPERVIRENS
!   6  RC    18 = WESTERN REDCEDAR (RC)        THUJA PLICATA
!   5  WH    19 = WESTERN HEMLOCK (WH)         TSUGA HETEROPHYLLA
!  11  MH    20 = MOUNTAIN HEMLOCK (MH)        TSUGA MERTENSIANA
!  40  NH    21 = BIGLEAF MAPLE (BM)           ACER MACROPHYLLUM
!  40  NH    22 = RED ALDER (RA)               ALNUS RUBRA
!  40  NH    23 = WHITE ALDER (WA) /           ALNUS RHOMBIFOLIA
!                 PACIFIC MADRONE              ARBUTUS MENZIESII
!  40  NH    24 = PAPER BIRCH (PB)             BETULA PAPYRIFERA
!  40  NH    25 = GIANT CHINKAPIN (GC) /       CASTANOPSIS CHRYSOPHYLLA
!                 TANOAK                       LITHOCARPUS DENSIFLORUS
!  19  AS    26 = QUAKING ASPEN (AS)           POPULUS TREMULOIDES
!  40  NH    27 = BLACK COTTONWOOD (CW)        POPULUS TRICHOCARPA
!  40  NH    28 = OREGON WHITE OAK (WO) /      QUERCUS GARRYANA
!                 CALIFORNIA BLACK OAK         QUERCUS KELLOGGII
!  26  JU    29 = WESTERN JUNIPER (WJ)         JUNIPERUS OCCIDENTALIS
!  36  LL    30 = SUBALPINE LARCH (LL)         LARIX LYALLII
!  22  WB    31 = WHITEBARK PINE (WB)          PINUS ALBICAULIS
!  37  KP    32 = KNOBCONE PINE (KP)           PINUS ATTENUATA
!  38  PY    33 = PACIFIC YEW (PY)             TAXUS BREVIFOLIA
!  40  NH    34 = PACIFIC DOGWOOD (DG)         CORNUS NUTTALLII
!  40  NH    35 = HAWTHORN (HT)                CRATAEGUS sp.
!  40  NH    36 = BITTER CHERRY (CH)           PRUNUS EMARGINATA
!  40  NH    37 = WILLOW (WI)                  SALIX sp.
!  40  NH    38 = ---
!  40  NH    39 = OTHER (OT)
!

DATA IRTSPC /    16,     13,      4,      9,     15, &
                    40,     39,     34,     14,      8, &
                     7,     31,     12,      1,     10, &
                     3,     35,      6,      5,     11, &
                    40,     40,     40,     40,     40, &
                    19,     40,     40,     26,     36, &
                    22,     37,     38,     40,     40, &
                    40,     40,     40,     40/


DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

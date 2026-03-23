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
!     This version of RDBLK1 is specific to the Inland CA, Southern
!     CAscades (ICASCA)(CA) FVS Variant representing 49 tree species.
!     Created by Lance R. David 25-JUN-2001
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
!.... are indexed as follows:
!....
!.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
!.... Species  | WP | WL | DF | GF | WH |  C | LP |  S | AF | PP |
!....
!.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
!.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
!....
!.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
!.... Species  | CB | WB | LM | CW | WS | J  | OC | GS | BO | OTH|
!....
!.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
!.... Species  | JP | TO | P  | YC | RW | LL | KP | PY | NF | NH |
!....
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.
!
!     Species present in the CA variant but not in the RD model
!     are assigned to a surrogate RD species.
!
!     SPECIES LIST FOR (ICASCA) CA VARIANT
! Index and (specie code)
!    RD       FVS
! -------   -------
! _______    1 (PC) PORT ORFORD CEDAR     CHAMAECYPARIS LAWSONIANA
! 14 (IC)    2 (IC) INCENSE CEDAR         LIBOCEDRUS DECURRENS
!  6 (C )    3 (RC) WESTERN REDCEDAR      THUJA PLICATA
! 13 (WF)    4 (WF) WHITE FIR             ABIES CONCOLOR
! 15 (RF)    5 (RF) CALIFORNIA RED FIR    ABIES MAGNIFICA (MAGNIFICA)
! _______    6 (SH) SHASTA RED FIR        ABIES MAGNIFICA (SHASTENSIS)
!  3 (DF)    7 (DF) DOUGLAS-FIR           PSEUDOTSUGA MENZIESII
!  5 (WH)    8 (WH) WESTERN HEMLOCK       TSUGA HETEROPHYLLA
! 11 (MH)    9 (MH) MOUNTAIN HEMLOCK      TSUGA MERTENSIANA
! 22 (WB)   10 (WB) WHITEBARK PINE        PINUS ALBICAULIS
! 37 (KP)   11 (KP) KNOBCONE PINE         PINUS ATTENUATA
!  7 (LP)   12 (LP) LODGEPOLE PINE        PINUS CONTORTA
! _______   13 (CP) COULTER PINE          PINUS COULTERI
! 23 (LM)   14 (LM) LIMBER PINE           PINUS FLEXILIS (FLEXILIS)
! 31 (JP)   15 (JP) JEFFREY PINE          PINUS JEFFREYI
! 12 (SP)   16 (SP) SUGAR PINE            PINUS LAMBERTIANA
!  1 (WP)   17 (WP) WESTERN WHITE PINE    PINUS MONTICOLA
! 10 (PP)   18 (PP) PONDEROSA PINE        PINUS PONDEROSA
! _______   19 (MP) MONTEREY PINE         PINUS RADIATA
! _______   20 (GP) GRAY PINE             PINUS SABINIANA
! 26 (J )   21 (JU) WESTERN JUNIPER       JUNIPERUS OCCIDENTALIS
! _______   22 (BR) BREWER SPRUCE         PICEA BREWERIANA
! 28 (GS)   23 (GS) GIANT SEQUOIA         SEQUOIADENDRON GIGANTEUM
! 38 (PY)   24 (PY) PACIFIC YEW           TAXUS BREVIFOLIA
! 17 (OS)   25 (OS) OTHER SOFTWOODS
! _______   26 (LO) COAST LIVE OAK        QUERCUS AGRIFOLIA
! _______   27 (CY) CANYON LIVE OAK       QUERCUS CHRYSOLEPSIS
! _______   28 (BL) BLUE OAK              QUERCUS DOUGLASII
! _______   29 (EO) ENGELMANN OAK         QUERCUS ENGELMANNI
! _______   30 (WO) OREGON WHITE OAK      QUERCUS GARRYANA
! 29 (BO)   31 (BO) CALIFORNIA BLACK OAK  QUERCUS KELLOGGII
! _______   32 (VO) VALLEY WHITE OAK      QUERCUS LOBATA
! _______   33 (IO) INTERIOR LIVE OAK     QUERCUS WISLIZENII
! _______   34 (BM) BIGLEAF MAPLE         ACER MACROPHYLLUM
! _______   35 (BU) CALIFORNIA BUCKEYE    AESCULUS CALIFORNICA
! _______   36 (RA) RED ALDER             ALNUS RUBRA
! _______   37 (MA) PACIFIC MADRONE       ARBUTUS MENZIESII
! 32 (TO)   38 (GC) GOLDEN CHINKAPIN      CASTANOPSIS CHRYSOPHYLLA
! _______   39 (DG) PACIFIC DOGWOOD       CORNUS NUTTALLII
! _______   40 (OA) OREGON ASH            FRAXINUS LATIFOLIA
! _______   41 (WN) WALNUT                JUGLANS sp.
! 32 (TO)   42 (TO) TANOAK                LITHOCARPUS DENSIFLORUS
! _______   43 (SY) CALIFORNIA SYCAMORE   PLATANUS RACEMOSA
! 19 (AS)   44 (AS) QUAKING ASPEN         POPULUS TREMULOIDES
! 24 (CW)   45 (CW) BLACK COTTONWOOD      POPULUS TRICHOCARPA
! _______   46 (WI) WILLOW                SALIX sp.
! _______   47 (CN) CALIFORNIA NUTMEG     TORREYA CALIFORNICA
! _______   48 (CL) CALIFORNIA LAUREL     UMBELLULARIA CALIFORNICA
! 18 (OH)   49 (OH) OTHER HARDWOODS
!
!.... The following IRTSPC is used with CA variant and corresponds with
!.... column 1 of the preceding table.
!
DATA IRTSPC /    41,     14,      6,     13,     15, &
                    42,      3,      5,     11,     22, &
                    37,      7,     43,     23,     31, &
                    12,      1,     10,     44,     45, &
                    26,     46,     28,     38,     17, &
                    19,     19,     19,     99,     19, &
                    19,     19,     19,     19,     19, &
                    19,     19,     19,     19,     19, &
                    19,     19,     19,     19,     24, &
                    19,     19,     19,     18/


DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

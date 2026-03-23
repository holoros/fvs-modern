BLOCK DATA BRBLKD
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Block data for the Blister Rust model.
!----------
!     SPECIES LIST FOR Southern Oregon/Northeast California (SO) VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)            PINUS MONTICOLA
!     2 = SUGAR PINE (SP)                    PINUS LAMBERTIANA
!     3 = DOUGLAS-FIR (DF)                   PSEUDOTSUGA MENZIESII
!     4 = WHITE FIR (WF)                     ABIES CONCOLOR (SO - WF/GF)
!     5 = MOUNTAIN HEMLOCK (MH)              TSUGA MERTENSIANA
!     6 = INCENSE CEDAR (IC)                 LIBOCEDRUS DECURRENS
!     7 = LODGEPOLE PINE (LP)                PINUS CONTORTA
!     8 = ENGELMANN SPRUCE (ES)              PICEA ENGELMANNII
!     9 = SHASTA RED FIR (SH)                ABIES MAGNIFICA (SHASTENSIS)(FROM CA)
!    10 = PONDEROSA PINE (PP)                PINUS PONDEROSA
!    11 = WESTERN JUNIPER (WJ)               JUNIPERUS OCCIDENTALIS
!    12 = GRAND FIR (GF)                     ABIES GRANDIS (SO - WF/GF)
!    13 = SUBALPINE FIR (AF)                 ABIES LASIOCARPA
!    14 = PACIFIC SILVER FIR (SF)            ABIES AMABILIS (FROM EC)
!    15 = NOBLE FIR (NF)                     ABIES PROCERA (FROM WC)
!    16 = WHITEBARK PINE (WB)                PINUS ALBICAULIS (FROM TT)
!    17 = WESTERN LARCH (WL)                 LARIX OCCIDENTALIS (FROM EC)
!    18 = WESTERN REDCEDAR (RC)              THUJA PLICATA (FROM EC)
!    19 = WESTERN HEMLOCK (WH)               TSUGA HETEROPHYLLA (FROM WC)
!    20 = PACIFIC YEW (PY)                   TAXUS BREVIFOLIA (FROM WC)
!    21 = WHITE ALDER (WA)                   ALNUS RHOMBIFOLIA (FROM WC)
!    22 = RED ALDER (RA)                     ALNUS RUBRA (FROM WC)
!    23 = BIGLEAF MAPLE (BM)                 ACER MACROPHYLLUM (FROM WC)
!    24 = QUAKING ASPEN (AS)                 POPULUS TREMULOIDES (FROM UT)
!    25 = BLACK COTTONWOOD (CW)              POPULUS TRICHOCARPA (FROM WC)
!    26 = BITTER CHERRY (CH)                 PRUNUS EMARGINATA (FROM WC)
!    27 = OREGON WHITE OAK (WO)              QUERCUS GARRYANA (FROM CA)
!    28 = WILLOW (WI)                        SALIX sp. (FROM WC)
!    29 = GIANT CHINQUAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA (FROM WC)
!    30 = CURL-LEAF MOUNTAIN MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS (FROM WC)
!    31 = BIRCHLEAF MOUNTAIN MAHOGANY (MB)   CERCOCARPUS ALNIFOLIUS (FROM WC)
!    32 = OTHER SOFTWOODS (OS)               DOUGLAS-FIR (DF) (FROM SO)
!    33 = OTHER HARDWOODS (OH)               MISCELLANEOUS HARDWOOD (FROM WC)
!
!----------------------------------------------------------------------
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  13-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Initialize ISPBR array and IBRDAM variable.
!  30-MAR-2001 Lance R. David (FHTET)
!     Changed canker file default format and added stock type.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR array to BRSPM. Added BR alpha species code array,
!     BRSPC.
!  08-MAY-2006 Lance R. David (FHTET)
!     Changed random number seed variable names to unique variables
!     BRS0, BRSS.
!  02-JUN-2006 Lance R. David (FHTET)
!     Changed I/O units from 25, 26, 27 to units 55, 56, 57
!  12-JUN-2006 Lance R. David (FHTET)
!     Moved RIBUS initilization to brinit.
!  02-APR-2013 Lance R. David (FMSC)
!     Updated the species mapping. May have removed the wrong version
!     of this file when the 11 species SO was taken out of service.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'

!.... Data statements.

DATA RSF/2.3,1.0,0.64/
DATA BRPI/3.14159/
DATA BRS0/55329D0/, BRSS/55329./

!.... Input canker data file format. Order of variables is:
!.... TreeID, StockType, TreeAge, DistUp, DistOut, %Gird, TotalCount
!.... Col 1-7      9       11-13   15-19   21-25   27-30    32-35

DATA ICFMT/'(I7,1X,I1,1X,F3.0,1X,F5.1,1X,F5.1,1X,F4.0,1X,F4.0)'/
DATA ICIN/55/, IDTOUT/56/, IDCOUT/57/

!.... Blister Rust Species Map.
!.... WPBR Model species/indices are: WP/1, SP/2
!.... Western White Pine and Sugar Pine set as host to Blister Rust

DATA BRSPM/1,2,0,0,0,0,0,0,0,0,0, &
              0,0,0,0,0,0,0,0,0,0,0, &
              0,0,0,0,0,0,0,0,0,0,0/

!.... Blister Rust model alpha species codes.
DATA BRSPC/'WP  ','SP  '/

!.... Blister Rust damage code
DATA IBRDAM/36/

END

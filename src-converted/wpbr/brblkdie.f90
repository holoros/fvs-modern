BLOCK DATA BRBLKD
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Block data for the Blister Rust model.
!----------
!     SPECIES LIST FOR INLAND EMPIRE (NI) VARIANT.
!
!     SPECIES LIST FOR INLAND EMPIRE (IE) VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA (FR0M NI)
!     2 = WESTERN LARCH (WL)             LARIX OCCIDENTALIS (FR0M NI)
!     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII (FR0M NI)
!     4 = GRAND FIR (GF)                 ABIES GRANDIS (FR0M NI)
!     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA (FR0M NI)
!     6 = WESTERN REDCEDAR (RC)          THUJA PLICATA (FR0M NI)
!     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA (FR0M NI)
!     8 = ENGLEMAN SPRUCE (ES)           PICEA ENGELMANNII (FR0M NI)
!     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA (FR0M NI)
!    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA (FR0M NI)
!    11 = MOUNTAIN HEMLOCK (MH)          TSUGA MERTENSIANA (OT FR0M NI)
!    12 = WHITEBARK PINE (WB)            PINUS ALBICAULIS (WL FR0M NI)
!    13 = LIMBER PINE (LM)               PINUS FLEXILIS (FR0M TT)
!    14 = SUBALPINE LARCH (LL)           LARIX LYALLII (AF FR0M NI)
!    15 = PINYON PINE (PI)               PINUS EDULIS (FR0M UT)
!    16 = ROCKY MOUNTAIN JUNIPER (RM)    JUNIPERUS SCOPULORUM (FR0M UT)
!    17 = PACIFIC YEW (PY)               TAXUS BREVIFOLIA (LM FR0M TT)
!    18 = QUAKING ASPEN (AS)             POPULUS TREMULOIDES (FR0M UT)
!    19 = COTTONWOOD (CO)                POPULUS SPP. (OH FR0M CR)
!    20 = MOUNTAIN MAPLE (MM)            ACER GLABRUM (AS FROM UT)
!    21 = PAPER BIRCH (PB)               BETULA PAPYRIFERA (AS FROM UT)
!    22 = OTHER HARDWOODS (OH)           (OH FR0M CR)
!    23 = OTHER SOFTWOODS (OS)           (FR0M NI)
!----------
!
!----------------------------------------------------------------------
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  14-MAR-2014 Lance David (FMSC)
!     Created this file from the NI version for the IE varaint.
!     -------- Note for link to IE variant --------
!     Original host species for the model was Western white pine and
!     sugar pine when linked to NI varaint. NI variant only represented WP
!     and no other blister rust host. IE variant represents WP and limber
!     pine (LM) which is a blister rust host. For IE variant link, sugar
!     pine representation in the WPBR model is changed to LM representation.
!
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
!.... WPBR Model species/indices are: WP/1, LM/2
!.... Western White Pine set as host to Blister Rust model species 1.
!.... Limber Pine set as host to Blister Rust model species 2.
!....

DATA BRSPM/1,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0/

!.... Blister Rust model alpha species codes.
DATA BRSPC/'WP  ','LM  '/

!.... Blister Rust damage code
DATA IBRDAM/36/

END

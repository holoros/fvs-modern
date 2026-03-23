BLOCK DATA BRBLKD
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!**********************************************************************
!  **BRBLKD--NI   DATE OF LAST REVISION:  06/05/2014
!----------------------------------------------------------------------
!  Purpose:
!  Block data for the Blister Rust model.
!----------
!     SPECIES LIST FOR INLAND EMPIRE (NI) VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)        PINUS MONTICOLA
!     2 = WESTERN LARCH (L)              LARIX OCCIDENTALIS
!     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
!     4 = GRAND FIR (GF)                 ABIES GRANDIS
!     5 = WESTERN HEMLOCK (WH)           TSUGA HETEROPHYLLA
!     6 = WESTERN REDCEDAR (C)           THUJA PLICATA
!     7 = LODGEPOLE PINE (LP)            PINUS CONTORTA
!     8 = ENGLEMAN SPRUCE (S)            PICEA ENGELMANNII
!     9 = SUBALPINE FIR (AF)             ABIES LASIOCARPA
!    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
!    11 = OTHER (OT)  grown as MOUNTAIN HEMLOCK   TSUGA MERTENSIANA
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
!.... Western White Pine set as host to Blister Rust model species 1.

DATA BRSPM/1,0,0,0,0,0,0,0,0,0,0/

!.... Blister Rust model alpha species codes.
DATA BRSPC/'WP  ','SP  '/

!.... Blister Rust damage code
DATA IBRDAM/36/

END

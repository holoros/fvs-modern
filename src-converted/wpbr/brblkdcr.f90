BLOCK DATA BRBLKD
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  Block data for the Blister Rust model.
!----------
!     SPECIES LIST FOR CENTRAL ROCKIES VARIANT.
!
!    AL COMMON                  FIA SCIENTIFIC
!  # CD NAME                    CD  NAME
! -- -- ---------------------   --- -----------------------------------
!  1 AF SUBALPINE FIR           019 ABIES LASIOCARPA var. LASIOCARPA
!  2 CB CORKBARK FIR            018 ABIES LASIOCARPA var. ARIZONICA
!  3 DF DOUGLAS-FIR             202 PSEUDOTSUGA MENZIESII
!  4 GF GRAND FIR               017 ABIES GRANDIS
!  5 WF WHITE FIR               015 ABIES CONCOLOR
!  6 MH MOUNTAIN HEMLOCK        264 TSUGA MERTENSIANA
!  7 RC WESTERN REDCEDAR        242 THUJA PLICATA
!  8 WL WESTERN LARCH           073 LARIX OCCIDENTALIS
!  9 BC BRISTLECONE PINE        102 PINUS ARISTATA
! 10 LM LIMBER PINE             113 PINUS FLEXILIS var. FLEXILIS
! 11 LP LODGEPOLE PINE          108 PINUS CONTORTA
! 12 PI COMMON PINYON           106 PINUS EDULIS
! 13 PP PONDEROSA PINE          122 PINUS PONDEROSA
! 14 WB WHITEBARK PINE          101 PINUS ALBICAULIS
! 15 SW SOUTHWESTERN WHITE PINE 114 PINUS STROBIFORMUS
! 16 UJ UTAH JUNIPER            065 JUNIPERUS OSTEOSPERMA
! 17 BS BLUE SPRUCE             096 PICEA PUNGENS
! 18 ES ENGELMANN SPRUCE        093 PICEA ENGELMANNII
! 19 WS WHITE SPRUCE            094 PICEA GLAUCA
! 20 AS QUAKING ASPEN           746 POPULUS TREMULOIDES
! 21 NC NARROWLEAF COTTONWOOD   749 POPULUS ANGUSTIFOLIA
! 22 PW PLAINS COTTONWOOD       745 POPULUS DELTOIDES var. MONOLIFERA
! 23 GO GAMBEL OAK              814 QUERCUS GAMBELII
! 24 AW ARIZONA WHITE OAK       803 QUERCUS ARIZONICA
! 25 EM EMORY OAK               810 QUERCUS EMORYI
! 26 BK BUR OAK                 823 QUERCUS MACROCARPA
! 27 SO SILVERLEAF OAK          843 QUERCUS HYPOLEUCOIDES
! 28 PB PAPER BIRCH             375 BETULA PAPYRIFERA
! 29 AJ ALLIGATOR JUNIPER       063 JUNIPERUS DEPPEANA
! 30 RM ROCKY MOUNTAIN JUNIPER  066 JUNIPERUS SCOPULORUM
! 31 OJ ONESEED JUNIPER         069 JUNIPERUS MONOSPERMA
! 32 ER EASTERN REDCEDAR        068 JUNIPERUS VIRGINIANA
! 33 PM SINGLELEAF PINYON       133 PINUS MONOPHYLLA
! 34 PD BORDER PINYON           134 PINUS DISCOLOR
! 35 AZ ARIZONA PINYON PINE     143 PINUS MONOPHYLLA var. FALLAX
! 36 CI CHIHUAHUA PINE          118 PINUS LEIOPHYLLA var. CHIHUAHUANA
! 37 OS OTHER SOFTWOODS         298
! 38 OH OTHER HARDWOODS         998
!
!----------------------------------------------------------------------
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  23-FEB-2006 Lance David (FHTET)
!     Created this Central Rockies version from NI for purpose of testing
!     suitability to use WPBR model to represent Comandra Blister Rust on
!     lodgepole and ponderosa pines.
!  08-MAY-2006 Lance R. David (FHTET)
!     Changed random number seed variable names to unique variables
!     BRS0, BRSS.
!  02-JUN-2006 Lance R. David (FHTET)
!     Changed I/O units from 25, 26, 27 to units 55, 56, 57
!  12-JUN-2006 Lance R. David (FHTET)
!     Moved RIBUS initilization to brinit.
!  03-JUN-2014 Lance R. David (FMSC)
!     Modified for the CR 38 species representation.
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
!.... WPBR Model species/indices are: LP/1, PP/2
!.... Lodgepole and Ponderosa Pine set as host to Blister Rust
!.... (comandrae), both species use the original coefficients
!.... and default values of white pine blister rust with NI variant.
!....
!.... FVS Central Rockies Species list for all model types:
!....              1   2   3   4   5   6   7   8   9  10  11  12  -- FVS index
!....             AF  CB  DF  GF  WF  MH  RC  WL  BC  LM  LP  PI  -- FVS species
DATA BRSPM / 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0, &
                2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
!....             25  26  27  28  29  30  31  32  33  34  35  36  -- FVS index
!....             EM  BK  SO  PB  AJ  RM  OJ  ER  PM  PD  AZ  CI  -- FVS species
                0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
!....             37  38                                          -- FVS index
!....             OS  OH                                          -- FVS species
                0,  0/

!.... Blister Rust model alpha species codes.
DATA BRSPC/'LP  ','PP  '/

!.... Blister Rust damage code
DATA IBRDAM/36/

END

BLOCK DATA MPBLKD
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     MOUNTAIN PINE BEETLE --
!     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
!
! Revision History
!   06/08/09 LANCE DAVID (FMSC)
!     CREATED THIS 18 SPECIES BLUE MOUNTAINS VARIANT VERSION TO
!     ACCOMODATE THE 18 SPECIES REPRESENTED. SURROGATE SPECIES
!     ASSIGNMENTS ARE BASED ON THOSE MADE IN THE SORNEC AND
!     CENTRAL ROCKIES VARIANTS.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!   08/22/14 Lance R. David (FMSC)
!     Function name was used as variable name.
!     changed variable INT to INCRS
!----------------------------------------------------------------------
!
!OMMONS

INCLUDE 'PRGPRM.f90'

INCLUDE 'MPBCOM.f90'
!
!OMMONS
!
DATA  JOMPB  / 7 /

DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /, &
        INCRS/ 10 /

!----------
! vv---- MPB surface area calculation surrogate specie (surfce.f)
! !!
! !!   SPECIES LIST FOR BM VARIANT.
! !!   # CD NAME                  CD  NAME
! !!  -- -- --------------------- --- -----------------------------------
! WP   1 WP WESTERN WHITE PINE    119 PINUS MONTICOLA
! WL   2 WL WESTERN LARCH         073 LARIX OCCIDENTALIS
! DF   3 DF DOUGLAS-FIR           202 PSEUDOTSUGA MENZIESII
! DF   4 GF GRAND FIR              17 ABIES GRANDIS
! WL   5 MH MOUNTAIN HEMLOCK      264 TSUGA MERTENSIANA
! WL   6 WJ WESTERN JUNIPER       064 JUNIPERUS OCCIDENTALIS
! LP   7 LP LODGEPOLE PINE        108 PINUS CONTORTA
! WP   8 ES ENGELMANN SPRUCE      093 PICEA ENGELMANNII
! DF   9 AF SUBALPINE FIR         019 ABIES LASIOCARPA
! PP  10 PP PONDEROSA PINE        122 PINUS PONDEROSA
! WL  11 WB WHITEBARK PINE        101 PINUS ALBICAULIS
! LP  12 LM LIMBER PINE           113 PINUS FLEXILIS
! WL  13 PY PACIFIC YEW           231 TAXUS BREVIFOLIA
! WL  14 YC ALASKA CEDAR           42 CHAMAECYPARIS NOOTKATENSIS
! DF  15 AS QUAKING ASPEN         746 POPULUS TREMULOIDES
! DF  16 CW BLACK COTTONWOOD      747 POPULUS BALSAMIFERA
! WL  17 PB OTHER SOFTWOODS       298
! DF  18 OS OTHER HARDWOODS       998
!
!----------

!     Specie indexing for Blue Mountains 18 species
!
DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

!     Assign surrogate species for calculations in surfce.f
DATA MPBSPM/ &
       1,  2,  3,  3,  2,  2,  7,  1,  3, 10,  2, &
!        12  13  14  15  16  17  18                 -- EM index
!        LM  PY  YC  AS  CW  OS  OH                 -- EM species
!        WL  WL  WL  DF  DF  WL  DF                 -- surfce surrogate
       2,  2,  2,  3,  3,  2,  3/

END

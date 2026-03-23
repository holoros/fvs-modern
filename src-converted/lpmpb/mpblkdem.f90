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
!   04/24/09 LANCE DAVID (FMSC)
!     CREATED THIS 19 SPECIES EASTERN MONTANA VARIANT VERSION TO
!     ACCOMODATE THE 19 SPECIES REPRESENTED. SURROGATE SPECIES
!     ASSIGNMENTS ARE BASED ON THOSE MADE IN THE NORTHERN IDAHO AND
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
! !!   SPECIES LIST FOR EM VARIANT.
! !!   # CD NAME                  CD  NAME
! !!  -- -- --------------------- --- -----------------------------------
! WL   1 WB WHITEBARK PINE        101 PINUS ALBICALA
! WL   2 WL WESTERN LARCH         073 LARIX OCCIDENTALIS
! DF   3 DF DOUGLAS-FIR           202 PSEUDOTSUGA MENZIESII
! WL   4 LM LIMBER PINE           113 PINUS FLEXILIS
! WL   5 LL SUBALPINE LARCH       072 LARIX LYALLII
! WL   6 RM ROCKY MTN JUNIPER     064 JUNIPERUS OCCIDENTALIS
! LP   7 LP LODGEPOLE PINE        108 PINUS CONTORTA
! WP   8 ES ENGELMANN SPRUCE      093 PICEA ENGELMANNII
! DF   9 AF SUBALPINE FIR         019 ABIES LASIOCARPA
! PP  10 PP PONDEROSA PINE        122 PINUS PONDEROSA
! DF  11 GA GREEN ASH             544 FRAXINUS PENNSYLVANICA
! DF  12 AS QUAKING ASPEN         746 POPULUS TREMULOIDES
! DF  13 CW BLACK COTTONWOOD      747 POPULUS BALSAMIFERA VAR TRICHOCARPA
! DF  14 BA BALSAM POPLAR         741 POPULUS BALSAMIFERA
! DF  15 PW PLAINS COTTONWOOD     745 POPULUS DELTOIDES VAR MONOLIFERA
! DF  16 NC NARROWLEAF COTTONWOOD 749 POPULUS ANGUSTIFOLIA
! DF  17 PB PAPER BIRCH           375 BETULA PAPYRIFERA
! WL  18 OS OTHER SOFTWOODS       298
! DF  19 OH OTHER HARDWOODS       998
!----------

!     Specie indexing. EM variant does not have White Pine (WP), so
!                      WP is indexed to Whitebark Pine (WB).
DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

!     Assign surrogate species for calculations in surfce.f
DATA MPBSPM/ &
       2,  2,  3,  2,  2,  2,  7,  1,  3, 10,  3, &
!        12  13  14  15  16  17  18  19             -- EM index
!        AS  CW  BA  PW  NC  PB  OS  OH             -- EM species
!        DF  DF  DF  DF  DF  DF  WL  DF             -- surfce surrogate
       3,  3,  3,  3,  3,  3,  2,  3 /

END

BLOCK DATA MPBLKD
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     MOUNTAIN PINE BEETLE --
!     SEE MPBCUP OR MPBMOD FOR VARIABLE DISCRIPTIONS.
!
! REVISION HISTORY:
!   03/16/12 Lance R. David (FMSC)
!     Created this 32 species East Cascades variant version to
!     accomodate the species expansion of the variant. Surrogate species
!     assignments are based on those made in the SO, BM and IE variants.
!   08/22/14 Lance R. David (FMSC)
!     Function name was used as variable name.
!     changed variable INT to INCRS
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'MPBCOM.f90'

!OMMONS
!
DATA  JOMPB  / 7 /

DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /, &
        INCRS/ 10 /

!----------
! vv---- MPB surface area calculation surrogate specie (surfce.f)
! !!
! !!   SPECIES LIST FOR EC VARIANT.
! !!  FVS Alpha
! !!   #   Code Common Name             Scientific Name
! !! ---- ----- ----------------------- ------------------------------------
! WP   1    WP  WESTERN WHITE PINE      PINUS MONTICOLA
! WL   2    WL  WESTERN LARCH           LARIX OCCIDENTALIS
! DF   3    DF  DOUGLAS-FIR             PSEUDOTSUGA MENZIESII
! DF   4    SF  PACIFIC SILVER FIR      ABIES AMABILIS
! DF   5    RC  WESTERN REDCEDAR        THUJA PLICATA
! DF   6    GF  GRAND FIR               ABIES GRANDIS
! LP   7    LP  LODGEPOLE PINE          PINUS CONTORTA
! WP   8    ES  ENGELMANN SPRUCE        PICEA ENGELMANNII
! DF   9    AF  SUBALPINE FIR           ABIES LASIOCARPA
! PP  10    PP  PONDEROSA PINE          PINUS PONDEROSA
! DF  11    WH  WESTERN HEMLOCK         TSUGA HETEROPHYLLA
! WL  12    MH  MOUNTAIN HEMLOCK        TSUGA MERTENSIANA
! WL  13    PY  PACIFIC YEW             TAXUS BREVIFOLIA
! WL  14    WB  WHITEBARK PINE          PINUS ALBICAULIS
! DF  15    NF  NOBLE FIR               ABIES PROCERA
! DF  16    WF  WHITE FIR               ABIES CONCOLOR
! WL  17    LL  SUBALPINE LARCH         LARIX LYALLII
! WL  18    YC  ALASKA CEDAR            CHAMAECYPARIS NOOTKATENSIS
! WL  19    WJ  WESTERN JUNIPER         JUNIPERUS OCCIDENTALIS
! DF  20    BM  BIGLEAF MAPLE           ACER MACROPHYLLUM
! DF  21    VN  VINE MAPLE              ACER CIRCINATUM
! WL  22    RA  RED ALDER               ALNUS RUBRA
! DF  23    PB  PAPER BIRCH             BETULA PAPYRIFERA
! DF  24    GC  GOLDEN CHINKAPIN        CASTANOPSIS CHRYSOPHYLLA
! DF  25    DG  PACIFIC DOGWOOD         CORNUS NUTTALLII
! DF  26    AS  QUAKING ASPEN           POPULUS TREMULOIDES
! DF  27    CW  BLACK COTTONWOOD        POPULUS BALSAMIFERA var. TRICHOCARPA
! DF  28    WO  OREGON WHITE OAK        QUERCUS GARRYANA
! DF  29    PL  CHERRY AND PLUM SPECIES PRUNUS sp.
! DF  30    WI  WILLOW SPECIES          SALIX sp.
! WL  31    OS  OTHER SOFTWOODS
! DF  32    OH  OTHER HARDWOODS
!----------


DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

!     Use appropriate surrogate species for the calculations in surfce.f
!                  WP  L DF GF WH  C LP  S AF PP WH
DATA MPBSPM / 1, 2, 3, 3, 3, 3, 7, 1, 3,10, 3, &
                 2, 2, 2, 3, 3, 2, 2, 2, 3, 3, 2, &
!                  PB GC DG AS CW WO PL WI OS OH
                 3, 3, 3, 3, 3, 3, 3, 3, 2, 3 /
END

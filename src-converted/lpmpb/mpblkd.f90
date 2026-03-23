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
!     May-June, 2000 Glenn E. Brink
!       Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
!       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
!       Ponderosa Pine respectively.  Added to common block in file
!       MPBCOM.F77.
!       Added array MPBSPM to govern the computations in surfce.f by
!       species using an IF block as opposed to the old COMPUTED GO TO,
!       since the array allows the definition to be made in mpblkd.f,
!       instead of always having to change the COMPUTED GO TO.
!       Added to common block in file MPBCOM.F77.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
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

!     SPECIES LIST FOR INLAND EMPIRE VARIANT.
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


DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/1,2,3,7,10/

!     Use appropriate surrogate species for the calculations in surfce.f
!                   WP, L,DF,GF,WH, C,LP, S,AF,PP,OT
DATA MPBSPM /  1, 2, 3, 3, 3, 3, 7, 1, 3,10, 3/

END

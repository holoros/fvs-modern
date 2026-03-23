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
!   07/09/09 - Lance R. David (FMSC)
!     Update to Central Rockies expansion to 38 species.
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

DATA  JOMPB  / 7 /

DATA IPLTNO/ 1 /,IMPROB/ 1 /,NATR/ 2 /, KEYMPB/ 2,3,6*0,1 /, &
        INCRS/ 10 /

!----------
!   CENTRAL ROCKIES VARIANT 38 SPECIES LIST.
!
!   #  Code  Common Name             Scientific Name
!   1  AF    subalpine fir           Abies lasiocarpa var. lasiocarpa
!   2  CB    corkbark fir            Abies lasiocarpa var. arizonica
!   3  DF    Douglas-fir             Pseudotsuga menziesii
!   4  GF    grand fir               Abies grandis
!   5  WF    white fir               Abies concolor
!   6  MH    mountain hemlock        Tsuga mertensiana
!   7  RC    western redcedar        Thuja plicata
!   8  WL    western larch           Larix occidentalis
!   9  BC    bristlecone pine        Pinus aristata
!   10 LM    limber pine             Pinus flexilis var. flexilis
!   11 LP    lodgepole pine          Pinus contorta
!   12 PI    common pinyon pine      Pinus edulis
!   13 PP    ponderosa pine          Pinus ponderosa
!   14 WB    whitebark pine          Pinus albicaulis
!   15 SW    Southwestern white pine Pinus strobiformis
!   16 UJ    Utah juniper            Juniperus osteosperma
!   17 BS    blue spruce             Picea pungens
!   18 ES    Engelmann spruce        Picea engelmannii
!   19 WS    white spruce            Picea glauca
!   20 AS    quaking aspen           Populus tremuloides
!   21 NC    narrowleaf cottonwood   Populus angustifolia
!   22 PW    plains cottonwood       Populus delltoides var. monolifera
!   23 GO    Gambel oak              Quercus gambelii
!   24 AW    Arizona white oak       Quercus arizonica
!   25 EM    Emory oak               Quercus emoryi
!   26 BK    Bur Oak                 Quercus macrocarpa
!   27 SO    silverleaf oak          Quercus hypoleucoides
!   28 PB    paper birch             Betula papyrifera
!   29 AJ    Alligator juniper       Juniperus deppeana
!   30 RM    Rocky Mountain juniper  Juniperus scopulorum
!   31 OJ    Oneseed juniper         Juniperus monosperma
!   32 ER    Eastern Redcedar        Juniperus virginiana
!   33 PM    Singleleaf pinyon       Pinus monophylla
!   34 PD    Border pinyon           Pinus discolor
!   35 AZ    Arizona pinyon pine     Pinus monophylla var. fallax
!   36 CI    Chihuahua pine          Pinus leiophylla var. chihuahuana
!   37 OS    other softwoods
!   38 OH    other hardwoods

DATA IDXWP,IDXWL,IDXDF,IDXLP,IDXPP/15,8,3,11,13/

!     Use appropriate surrogate species for the calculations in surfce.f
!                 AF CB DF GF WF MH RC WL BC LM LP PI PP WB SW  -- CR species
DATA MPBSPM/ 3, 3, 3, 3, 3, 8, 8, 8, 8, 8,11, 8,13, 8,15, &
                8,15,15,15, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, &
!                 OJ ER PM PD AZ CI OS OH  -- CR species
                8, 8, 8, 8, 8,13, 8, 3/

END

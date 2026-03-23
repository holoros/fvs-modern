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
!  Revision History:
!  05-OCT-11 Lance David (FMSC)
!    .FVS CI variant expanded to 19 species, this block data created.
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
!.... are indexed as follows :
!....
!.... RD Model species:
!....   #  Code  Name                   #  Code  Name
!....   1   WP   WHITE PINE            21   CB   CORKBARK FIR
!....   2   WL   WESTERN LARCH         22   WB   WHITEBARK PINE
!....   3   DF   DOUGLAS-FIR           23   LM   LIMBER PINE
!....   4   GF   GRAND FIR             24   CO   COTTONWOOD
!....   5   WH   WESTERN HEMLOCK       25   WS   WHITE SPRUCE
!....   6   RC   W. REDCEDAR           26   JU   JUNIPER
!....   7   LP   LODGEPOLE PINE        27   OC   OTHER CONIFERS
!....   8   ES   ENGELMANN SPRUCE      28   GS   GIANT SEQUOIA
!....   9   AF   SUBALPINE FIR         29   BO   BLACK OAK
!....  10   PP   PONDEROSA PINE        30   OTH  OTHER
!....  11   MH   MOUNTAIN HEMLOCK      31   JP   JEFFREY PINE
!....  12   SP   SUGAR PINE            32   TO   TANOAK/CHINKAPIN
!....  13   WF   WHITE FIR             33   PI   PINYON PINE
!....  14   IC   INCENSE CEDAR         34   YC   YELLOW CEDAR
!....  15   RF   RED FIR               35   RW   REDWOOD
!....  16   SF   P. SILVER FIR         36   LL   SUBALPINE LARCH
!....  17   OS   OTHER SOFTWOOD        37   KP   KNOBCONE PINE
!....  18   OH   OTHER HARDWOOD        38   PY   PACIFIC YEW
!....  19   AS   ASPEN                 39   NF   NOBLE FIR
!....  20   BS   BLUE SPRUCE           40   NH   NON-HOST
!....
!.... SPECIES LIST FOR CENTRAL IDAHO VARIANT. ***** 19 species *****
!....
!....  ------FVS UT VARIANT-------   WRD MODEL SPECIES    ANNOSUS
!....   # CD COMMON NAME             OR SURROGATE SP.     TYPE
!....  -- -- --------------------- ---------------------  --------
!....   1 WP WESTERN WHITE PINE          WHITE PINE  (1)  P-TYPE
!....   2 WL WESTERN LARCH            WESTERN LARCH  (2)  S-TYPE
!....   3 DF DOUGLAS-FIR                DOUGLAS-FIR  (3)  S-TYPE
!....   4 GF GRAND FIR                    GRAND FIR  (4)  S-TYPE
!....   5 WH WESTERN HEMLOCK        WESTERN HEMLOCK  (5)  S-TYPE
!....   6 RC WESTERN REDCEDAR      WESTERN REDCEDAR  (6)  S-TYPE
!....   7 LP LODGEPOLE PINE          LODGEPOLE PINE  (7)  P-TYPE
!....   8 ES ENGLEMANN SPRUCE      ENGELMANN SPRUCE  (8)  S-TYPE
!....   9 AF SUBALPINE FIR            SUBALPINE FIR  (9)  S-TYPE
!....  10 PP PONDEROSA PINE          PONDEROSA PINE (10)  P-TYPE
!....  11 WB WHITEBARK PINE          WHITEBARK PINE (22)  P-TYPE
!....  12 PY PACIFIC YEW                PACIFIC YEW (38)  P-TYPE
!....  13 AS QUAKING ASPEN                    ASPEN (19)  NON-HOST
!....  14 WJ WESTERN JUNIPER                JUNIPER (26)  P-TYPE
!....  15 MC CURLLEAF MOUNTAIN-            NON-HOST (40)  NON-HOST
!....        MAHOGANY
!....  16 LM LIMBER PINE                LIMBER PINE (23)  P-TYPE
!....  17 CW BLACK COTTONWOOD            COTTONWOOD (24)  NON-HOST
!....  18 OS OTHER SOFTWOODS         OTHER SOFTWOOD (17)  P-TYPE
!....  19 OH OTHER HARDWOODS         OTHER HARDWOOD (18)  NON-HOST
!....
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.
!....
!.... The following IRTSPC is for the CI 19 species variant.
!....              1   2   3   4   5   6   7   8   9   10  11  12  -- FVS index
!....              WP  WL  DF  GF  WH  RC  LP  ES  AF  PP  WB  PY  -- FVS species
DATA IRTSPC /1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 22, 38, &
                19, 26, 40, 23, 24, 17, 18 /

DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

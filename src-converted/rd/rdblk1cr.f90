BLOCK DATA RDBLK1
IMPLICIT NONE
!----------
! RD $Id$
!
!     The IRTSPC array is now initialized using a data statement in this
!     subroutine rather than in the RDINCR subroutine (RNH Dec98)
!
!----------
!
!  Purpose :
!     This block data file initializes constants in the Root Disease
!     extension to FVS for the CR variant.
!
!  Revisions:
!     16-MAR-2000 Lance R. David
!       Changed mapping of Bristlecone Pine from Limber Pine to Pinyon.
!     07-JUL-2009 Lance R. David
!       Updated species mapping for Central Rockies expanded species
!       list. Did not receive response from field on request to assist
!       with new species surrogate RD model species assignment so just
!       used my best judgement on suitable surrogates.
!----------
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
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease.
!....
!.... For this data block, CR variant:
!.... Bristlecone pine (BC) is mapped as Pinyon (PI 33);
!.... Utah Juniper (UJ), alligator juniper (AJ), Rocky Mountain juniper (RM),
!.... oneseed juniper (OJ) and Eastern redcedar (ER) is mapped as Juniper (JU 26);
!.... Gamble oak (GO) and Emory oak (EM) is mapped as other hardwood (OH 18);
!.... Arizona white oak (AW), Bur oak (BK) and silverleaf oak (SO) is mapped
!.... to black oak (BO 29)
!....
!.... The following IRTSPC is for the CR 38 species variant.
!.... Species codes from the FVS CR variant JSP array in blkdat.f
!....
!.... FVS Central Rockies Species list for all model types:
!....               1   2   3   4   5   6   7   8   9  10  11  12  -- FVS index
!....              AF  CB  DF  GF  WF  MH  RC  WL  BC  LM  LP  PI  -- FVS species
DATA IRTSPC / 9, 21,  3,  4, 13, 11,  6,  2, 33, 23,  7, 33, &
                10, 22,  1, 26, 20,  8, 25, 19, 24, 24, 18, 29, &
!....              25  26  27  28  29  30  31  32  33  34  35  36  -- FVS index
!....              EM  BK  SO  PB  AJ  RM  OJ  ER  PM  PD  AZ  CI  -- FVS species
                18, 29, 29, 40, 26, 26, 26, 26, 33, 33, 33, 10, &
!....              37  38                                          -- FVS index
!....              OS  OH                                          -- FVS species
                17, 18/

DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

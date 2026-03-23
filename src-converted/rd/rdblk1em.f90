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
!  Previous revision date 04/22/09
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
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.
!....
!.... The following IRTSPC is for the EM variant.
!.... Species codes from the FVS EM variant JSP array in blkdat.f
!....              WB, WL, DF, LM, LL, RM, LP, ES, AF, PP,
!....              GA, AS, CW, BA, PW, NC, PB, OS, OH

DATA IRTSPC /22,  2,  3, 23, 36, 26,  7,  8,  9, 10, &
                18, 19, 24, 24, 24, 24, 18, 17, 18/

DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

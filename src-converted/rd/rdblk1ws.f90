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
!  Previous revision date 04/25/11
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
!.... SPECIES LIST FOR WESTERN SIERRAS VARIANT. ***** 43 species *****
!....
!.... ----------FVS WS VARIANT-----------   WRD MODEL SPECIES    ANNOSUS
!....  # CD COMMON NAME                     OR SURROGATE SP.     TYPE
!.... -- -- ----------------------------- ---------------------  --------
!....  1 SP SUGAR PINE                          SUGAR PINE (12)  P-TYPE
!....  2 DF DOUGLAS-FIR                        DOUGLAS-FIR (3)   S-TYPE
!....  3 WF WHITE FIR                            WHITE FIR (13)  S-TYPE
!....  4 GS GIANT SEQUOIA                    GIANT SEQUOIA (28)  S-TYPE
!....  5 IC INCENSE CEDAR                    INCENSE CEDAR (14)  P-TYPE
!....  6 JP JEFFREY PINE                      JEFFREY PINE (31)  P-TYPE
!....  7 RF CALIFORNIA RED FIR                     RED FIR (15)  S-TYPE
!....  8 PP PONDEROSA PINE                  PONDEROSA PINE (10)  P-TYPE
!....  9 LP LODGEPOLE PINE                  LODGEPOLE PINE (7)   P-TYPE
!.... 10 WB WHITEBARK PINE                  WHITEBARK PINE (22)  P-TYPE
!.... 11 WP WESTERN WHITE PINE                  WHITE PINE (1)   P-TYPE
!.... 12 PM SINGLELEAF PINYON                  PINYON PINE (33)  P-TYPE
!.... 13 SF PACIFIC SILVER FIR               P. SILVER FIR (16)  S-TYPE
!.... 14 KP KNOBCONE PINE                    KNOBCONE PINE (37)  P-TYPE
!.... 15 FP FOXTAIL PINE                     KNOBCONE PINE (37)  P-TYPE
!.... 16 CP COULTER PINE                     KNOBCONE PINE (37)  P-TYPE
!.... 17 LM LIMBER PINE                        LIMBER PINE (23)  P-TYPE
!.... 18 MP MONTEREY PINE                   PONDEROSA PINE (10)  P-TYPE
!.... 19 GP GRAY PINE                        KNOBCONE PINE (37)  P-TYPE
!....       (OR CALIFORNIA FOOTHILL PINE)
!.... 20 WE WASHOE PINE                      KNOBCONE PINE (37)  P-TYPE
!.... 21 GB GREAT BASIN BRISTLECONE PINE       PINYON PINE (33)  P-TYPE
!.... 22 BD BIGCONE DOUGLAS-FIR                DOUGLAS-FIR (3)   S-TYPE
!.... 23 RW REDWOOD                                REDWOOD (35)  S-TYPE
!.... 24 MH MOUNTAIN HEMLOCK              MOUNTAIN HEMLOCK (11)  S-TYPE
!.... 25 WJ WESTERN JUNIPER                        JUNIPER (26)  P-TYPE
!.... 26 UJ UTAH JUNIPER                           JUNIPER (26)  P-TYPE
!.... 27 CJ CALIFORNIA JUNIPER                     JUNIPER (26)  P-TYPE
!.... 28 LO CALIFORNIA LIVE OAK                  BLACK OAK (29)  NON-HOST
!.... 29 CY CANYON LIVE OAK                      BLACK OAK (29)  NON-HOST
!.... 30 BL BLUE OAK                             BLACK OAK (29)  NON-HOST
!.... 31 BO CALIFORNIA BLACK OAK                 BLACK OAK (29)  NON-HOST
!.... 32 VO VALLEY OAK                           BLACK OAK (29)  NON-HOST
!....       (OR CALIFORNIA WHITE OAK)
!.... 33 IO INTERIOR LIVE OAK                    BLACK OAK (29)  NON-HOST
!.... 34 TO TANOAK                        TANOAK/CHINKAPIN (32)  NON-HOST
!.... 35 GC GIANT CHINKAPIN               TANOAK/CHINKAPIN (32)  NON-HOST
!.... 36 AS QUAKING ASPEN                            ASPEN (19)  NON-HOST
!.... 37 CL CALIFORNIA-LAUREL                     NON-HOST (40)  NON-HOST
!.... 38 MA PACIFIC MADRONE                       NON-HOST (40)  NON-HOST
!.... 39 DG PACIFIC DOGWOOD                       NON-HOST (40)  NON-HOST
!.... 40 BM BIGLEAF MAPLE                         NON-HOST (40)  NON-HOST
!.... 41 MC CURLLEAF MOUNTAIN-MAHOGANY            NON-HOST (40)  NON-HOST
!.... 42 OS OTHER SOFTWOODS                 OTHER SOFTWOOD (17)  P-TYPE
!.... 43 OH OTHER HARDWOODS                 OTHER HARDWOOD (18)  NON-HOST
!....
!....
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.

!.... The following IRTSPC is for the WS variant 43 species.

DATA IRTSPC /12,  3, 13, 28, 14, 31, 15, 10,  7, 22,  1, &
                33, 16, 37, 37, 37, 23, 10, 37, 37, 33,  3, &
                35, 11, 26, 26, 26, 29, 29, 29, 29, 29, 29, &
                32, 32, 19, 40, 40, 40, 40, 40, 17, 18 /

DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

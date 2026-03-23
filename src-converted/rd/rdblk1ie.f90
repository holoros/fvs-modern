BLOCK DATA RDBLK1
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This block data file initializes constants in the Root Disease
!     extension to FVS variant Inland Empire 23 species.
!
!  This file created by Lance David 08/07/2003
!
!  Previous revision date 04/30/09
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
!.... In the Root Disease model, the defaults for these variables
!.... are indexed as follows :
!....
!.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
!.... Species  | WP | WL | DF | GF | WH | RC | LP | ES | AF | PP |
!....
!.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
!.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
!....
!.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
!.... Species  | CB | WB | LM | CO | WS | JU | OC | GS | BO | OTH|
!....
!.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
!.... Species  | JP | TO | PI | YC | RW | LL | KP | PY | NF | NH |
!....
!.... IRTSPC can be modified for different variants of FVS so
!.... that species match between FVS and the root disease
!.... model.
!....
!.... The following IRTSPC is used with variant IE 23 species.
!.... IE variant species Mountain maple (20) and Paper birch (21)
!.... do not have matching species in the RD models and are mapped
!.... to RD Other species (30).
!....
!....      1   2   3   4   5   6   7   8   9  10  11  12
!....     WP  WL  DF  GF  WH  RC  LP  ES  AF  PP  MH  WB
!....
!....     13  14  15  16  17  18  19  20  21  22  23
!....     LM  LL  PI  JU  PY  AS  CO  MM  PB  OH  OS
!....

DATA IRTSPC / &
        1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 22, &
       23, 36, 33, 26, 38, 19, 24, 30, 30, 18, 17/


DATA DICLAS /0.0, 5.0, 12.0, 24.0/
DATA DSFAC  /1.0, 0.75/

DATA IOUNIT /22/
DATA IRUNIT /18/

END

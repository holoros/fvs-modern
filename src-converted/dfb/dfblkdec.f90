BLOCK DATA DFBLKD
IMPLICIT NONE
!----------
! DFB $Id$
!----------
!
!     DOUGLAS-FIR BEETLE MODEL BLOCK DATA
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'DFBCOM.f90'
!
!
!OMMONS
!

!.... WINSUC IS THE SPECIES DEPENDENT VALUES FOR WINDTHROW
!.... SUCEPTIBILITY.
!....
!....    DFB              DFB              DFB
!....  IDX SP  WINSUC   IDX SP  WINSUC   IDX SP  WINSUC
!....   01 WP   0.028    14 IC   0.111    27 OC   0.028
!....   02 WL   0.083    15 RF   0.056    28 GS   0.056
!....   03 DF   0.056    16 SF   0.098    29 BO   0.0
!....   04 GF   0.139    17 OS   0.028    30 OTH  0.042
!....   05 WH   0.111    18 OH   0.056    31 JP   0.056
!....   06 RC   0.111    19 AS   0.056    32 TO   0.0
!....   07 LP   0.028    20 BS   0.139    33 PI   0.0
!....   08 ES   0.139    21 CB   0.139    34 YC   0.111
!....   09 AF   0.139    22 WB   0.0      35 RW   0.056
!....   10 PP   0.056    23 LM   0.0      36 LL   0.056
!....   11 MH   0.111    24 CW   0.056    37 KP   0.0
!....   12 SP   0.042    25 WS   0.139    38 PY   0.0
!....   13 WF   0.139    26 JU   0.0      39 NF   0.139
!....

DATA WINSUC / 0.028, 0.083, 0.056, 0.139, 0.111, &
                 0.111, 0.028, 0.139, 0.139, 0.056, &
                 0.111, 0.042, 0.139, 0.111, 0.056, &
                 0.098, 0.028, 0.056, 0.056, 0.139, &
                 0.139,   0.0,   0.0, 0.056, 0.139, &
                   0.0, 0.028, 0.056,   0.0, 0.042, &
                 0.056,   0.0,   0.0, 0.111, 0.056, &
                 0.056,   0.0,   0.0, 0.139  /

!.... THE ARRAY IFVSSP IS USED TO INDEX THE SPECIES DEPENDENT
!.... ARRAY WINSUC. IFVSSP CAN BE MODIFIED FOR DIFFERENT VARIANTS
!.... OF FVS SO THAT SPECIES MATCH BETWEEN FVS AND THE DOUGLAS-FIR
!.... BEETLE MODEL.
!.... This IFVSSP is for the variant EC (32 species)

!....              WP  WL  DF  SF  RC  GF  LP  ES  AF  PP  WH
DATA IFVSSP / 1,  2,  3, 16,  6,  4,  7,  8,  9, 10,  5, &
                11, 38, 22, 39, 13, 36, 34, 26, 18, 18, 18, &
!....              PB  GC  DG  AS  CW  WO  PL  WI  OS  OH
                18, 32, 18, 19, 24, 29, 18, 30, 17, 18 /

DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

DATA ROWDOM / MAXSP*80.0 /

!.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
!.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

DATA IDFSPC / 3 /

!.... Set the variables for random number generator

DATA S0 / 55329D0 /, SS / 55329.0 /

END

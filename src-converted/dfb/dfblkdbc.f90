BLOCK DATA DFBLKD
!----------
! DFB $Id$
!----------
!
!     DOUGLAS-FIR BEETLE MODEL BLOCK DATA FOR
!     THE BC (BC) 15 SPECIES FVS VARIANT.
!
!     Created by Don Robinson 20-Jan-2010 from IE variant
!     template
!
!------------------------------------
!
!OMMONS

INCLUDE 'PRGPRM.f90'
INCLUDE 'DFBCOM.f90'

!.... WINSUC IS THE SPECIES DEPENDENT VALUES FOR WINDTHROW
!.... SUCEPTIBILITY.
!....
!....        WP(01), WL(02), DF(03), GF(04), WH(05),
!....         C(06), LP(07),  S(08), AF(09), PP(10),
!....        MH(11), SP(12), WF(13), IC(14), RF(15),
!....        SF(16), OS(17), OH(18), AS(19), BS(20),
!....        CB(21), WB(22), LM(23), CW(24), WS(25),
!....         J(26), OC(27), GS(28), BO(29), OTH(30),
!....        JP(31), TO(32),  P(33), YC(34), RW(35),
!....        LL(36), KP(37), PY(38), NF(39)

DATA WINSUC / 0.028, 0.083, 0.056, 0.139, 0.111, &
                 0.111, 0.028, 0.139, 0.139, 0.056, &
                 0.111, 0.042, 0.139, 0.111, 0.056, &
                 0.098, 0.028, 0.056, 0.056, 0.139, &
                 0.139,   0.0,   0.0, 0.056, 0.139, &
                   0.0, 0.028, 0.056,   0.0, 0.042, &
                 0.056,   0.0,   0.0, 0.111, 0.056, &
                 0.056,   0.0,   0.0, 0.139  /

!.... THE ARRAY IFVSSP IS USED TO INDEX THE SPECIES DEPENDENT ARRAY
!.... WINSUC. IFVSSP CAN BE MODIFIED FOR DIFFERENT VARIANTS OF FVS SO
!.... THAT SPECIES MATCH BETWEEN FVS AND THE DOUGLAS-FIR BEETLE MODEL.

!.... This IFVSSP is for the BC variant 15 species.

!     1 = WESTERN WHITE PINE (WP)     [PW]
!     2 = WESTERN LARCH (WL)          [LW]
!     3 = DOUGLAS-FIR (DF)            [FD]
!     4 = GRAND FIR (GF)              [BG]
!     5 = WESTERN HEMLOCK (WH)        [HW]
!     6 = WESTERN REDCEDAR (RC)       [CW]
!     7 = LODGEPOLE PINE (LP)         [PL]
!     8 = ENGELMANN SPRUCE (ES)       [SE]
!     9 = SUBALPINE FIR (AF)          [BL]
!    10 = PONDEROSA PINE (PP)         [PY]
!    11 = BIRCH (PB)                  [EP] 19 in WINSUC
!    12 = ASPEN (AS)                  [AT] 19 in WINSUC
!    13 = COTTONWOOD (CW)             [AC] 24 in WINSUC
!    14 = OTHER CONIFER (OC)          [  ] ! FD
!    15 = OTHER HARDWOOD (OH)         [  ] ! EP

DATA IFVSSP /  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, &
                 19, 19, 24,  3, 19  /

DATA PERDD  / 0.30, 0.80, 0.95, 1.00 /

DATA ROWDOM / MAXSP*80.0 /

!.... IDFSPC IS THE SPECIES NUMBER FOR DOUGLAS-FIR IN FVS.
!.... THIS VALUE MAY BE DIFFERENT FOR DIFFERENT VARIANTS OF FVS.

DATA IDFSPC / 3 /

!.... Set the variables for random number generator

DATA S0 / 55329D0 /, SS / 55329.0 /

END

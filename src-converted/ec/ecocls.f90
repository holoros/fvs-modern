SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
IMPLICIT NONE
!----------
! EC $Id$
!
!  SETS DEFAULT MAX SDI VALUES, SITE INDICIES, AND SITE SPECIES
!  BY PLANT ASSOCIATION (ECOCLASS CODE)
!
!  CALLED FROM SITSET
!----------
! DEFINITION OF VARIABLES AND TERMS:
!  APASS     ECOCLASS ALPHA CODE                           (RECEIVED)
!  ASPEC     ALPHA SPECIES CODE                            (RETURNED)
!  SDI       MAXIMUM SDI FOR THIS SPECIES                  (RETURNED)
!  SITE      SITE INDEX FOR THIS SPECIES                   (RETURNED)
!  ISIFLG    SITE SPECIES FLAG                             (RETURNED)
!            0 = NOT THE SITE SPECIES    1 = SITE SPECIES
!  NUM       NUMBER OF SPECIES SPECIFIED FOR THIS ECOCLASS (RETURNED)
!  INDEX     ARRAY INDEX OF THE VALUES BEING RETURNED      (RECEIVED &
!                                                             RETURNED)
!  ISEQ      FVS SEQUENCE NUMBER FOR THIS SPECIES          (RETURNED)
!
!  GBA       GROWTH BASAL AREA USED FOR COMPUTING MAX SDI.
!            MAX SDI = GBA * 1.5 * 1.84
!            (Personal comunication with Fred Hall, R6/NR)
!
!  PA(i)     ECOCLASS CODE FOR PLANT ASSOCIATION i
!  SPC(j)    SPECIES j FOR PLANT ASSOCIATION i
!  SITE(j)   SITE INDEX FOR SPECIES j
!            (BASED ON THE SITE REFERENCE USED IN THIS VARIANT)
!  SDIMX(j)  MAXIMUM SDI FOR SPECIES j
!  NUMBR(j)  NUMBER OF SPECIES SPECIFIED FOR PLANT ASSOCIATION i
!  FVSSEQ(j) FVS SEQUENCE NUMBER FOR SPECIES j
!
!  THE MAX SDI VALUES FOR THE FOLLOWING PLANT ASSOCIATIONS ARE FROM
!  AN R6 ALL CVS DATA STUDY, JUNE 2008: 1,2,5-8,11-13,15,17,21,22,
!  24-33,36-40,42,43,45-54,56,57,59-62,64-72,74,76-78,80,82,84-86,
!  88-95,97-99,101,102,104,106,107,109-111,114,117-119,123,125,126,
!  129-140,142-146,148-150,152,153
!----------
INTEGER NENTRY
PARAMETER (NENTRY=155)
CHARACTER*4 SPC(NENTRY),ASPEC
CHARACTER*6 SCIEN(NENTRY)
CHARACTER*8 APASS,PA(NENTRY)
INTEGER FVSSEQ(NENTRY)
REAL SITE(NENTRY),SDIMX(NENTRY),RSI,RSDI
INTEGER NUMBR(NENTRY),IFLAG(NENTRY),ISEQ,INDEX,NUM,ISFLAG
INTEGER I,K
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=1,15) / &
!-----------------------------------------------------------------------
!      ALPHA     SCIEN         ALPHA        NUM  SITE FVS  PLANT
!       ECO       SITE     MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
!      CLASS      SPEC     SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
!-----------------------------------------------------------------------
!    1 = PIAL/CARU                            GBA: 101   p. 262
!    Whitebark pine/pinegrass                            PNW-GTR-360
!
   'CAG112  ','PSME  ', 625.,'DF  ',  25.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    2 = PIAL/VASC/LUHI                       GBA:  65   p. 248
!    Whitebark pine/grouse huckleberry/smooth woodrush   PNW-GTR-359
!
   'CAS311  ','ABLA2 ', 700.,'AF  ',  45.,   1,   1,   9, &
!----------------------------------------------------------------------
!    3 = THPL-ABGR/ACTR                       GBA: 308   p. 115
!    Western redcedar-grand fir/vanilla leaf             R6 E TP-004-88
!
   'CCF211  ','PSME  ', 850.,'DF  ',  72.,   1,   1,   3, &
!----------------------------------------------------------------------
!    4 = THPL/ACTR                            GBA: 368   p. 93
!    Western redcedar/vanilla leaf                       R6 E TP-006-88
!
   'CCF212  ','ABGR  ',1016.,'GF  ',  71.,   1,   1,   6, &
!----------------------------------------------------------------------
!    5 = THPL/CLUN                            GBA: 317   p. 246
!    Western redcedar/queencup beadily                   PNW-GTR-360
!
   'CCF221  ','PSME  ', 840.,'DF  ',  64.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    6 = THPL/ARNU3                           GBA: 380   p. 240
!    Western redcedar/wild sarsaparilla                  PNW-GTR-360
!
   'CCF222  ','PSME  ', 670.,'DF  ',  69.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    7 = THPL/OPHO                            GBA: 473   p. 251
!    Western redcedar/devil's club                       PNW-GTR-360
!
   'CCS211  ','THPL  ', 775.,'RC  ',  96.,   1,   1,   5, &
!-----------------------------------------------------------------------
!    8 = THPL/VAME                            GBA: 180   p. 256
!    Western redcedar/big huckleberry                    PNW-GTR-360
!
   'CCS311  ','PSME  ', 815.,'DF  ',  63.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    9 = PSME/PEFR3                           GBA:  83   p. 82
!    Douglas-fir/shrubby penstemon                       PNW-GTR-359
!
   'CDF411  ','PSME  ', 229.,'DF  ',  58.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   10 = PSME/ARUV-OKAN                       GBA: 120   p. 27
!    Douglas-fir/bearberry (Okanogan)                    R6 E 132b-83
!
   'CDG123  ','PSME  ', 331.,'DF  ',  38.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   11 = PSME/CARU-O&C                        GBA: 173   p. 49
!    Douglas-fir/pinegrass (Okanogan & Colville)         PNW-GTR-360
!
   'CDG131  ','PSME  ', 530.,'DF  ',  58.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   12 = PSME/CAGE-WEN                        GBA: 220   p. 60
!    Douglas-fir/elk sedge (Wenatchee)                   PNW-GTR-359
!
   'CDG132  ','PSME  ', 550.,'DF  ',  69.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   13 = PSME/CARU-AGSP                       GBA: 115   p. 64
!    Douglas-fir/pinegrass-bluebunch wheatgrass          PNW-GTR-359
!
   'CDG134  ','PSME  ', 430.,'DF  ',  61.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   14 = PSME/CAGE                            GBA: 160   p. 51
!    Douglas-fir/elk sedge                               R6 E TP-004-88
!
   'CDG141  ','PSME  ', 442.,'DF  ',  55.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   15 = PIPO-PSME/AGSP                       GBA:  73   p. 44
!    Ponderosa pine-Douglas-fir/bluebunch wheatgrass     PNW-GTR-360
!
   'CDG311  ','PIPO  ', 270.,'PP  ',  79.,   1,   1,  10/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=16,30) / &
!-----------------------------------------------------------------------
!   16 = PSME/FEOC                            GBA: 235   p. 55
!    Douglas-fir/western fescue                          R6 E TP-004-88
!
   'CDG321  ','PSME  ', 649.,'DF  ',  67.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   17 = PSME/AGSP-WEN                        GBA:  79   p. 58
!    Douglas-fir/bluebunch wheatgrass (Wenatchee)        PNW-GTR-359
!
   'CDG322  ','PSME  ', 235.,'DF  ',  39.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   18 = PSME/AGSP-ASDE                       GBA:  68   p. 80
!    Douglas-fir/bluebunch wheatgrass-podfern            PNW-GTR-359
!
   'CDG323  ','PSME  ', 188.,'DF  ',  58.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   19 = PSME/HODI/CAGE                       GBA: 245   p. 59
!    Douglas-fir/oceanspray/elk sedge                    R6 E TP-004-88
!
   'CDS231  ','PSME  ', 676.,'DF  ',  80.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   20 = PSME/ACCI/FEOC                       GBA: 261   p. 45
!    Douglas-fir/vine maple/western fescue               R6 E TP-006-88
!
   'CDS241  ','PSME  ', 720.,'DF  ',  76.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   21 = PSME/PAMY-OKAN                       GBA: 225   p. 41
!    Douglas-fir/pachistima (Okanogan)                   R6 E 132b-83
!
   'CDS411  ','PSME  ', 630.,'DF  ',  59.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   22 = PSME/PAMY/CARU                       GBA: 173   p. 81
!    Douglas-fir/pachistima/pinegrass                    PNW-GTR-359
!
   'CDS412  ','PSME  ', 450.,'DF  ',  57.,   1,   1,   3, &
!----------------------------------------------------------------------
!   23 = PSME/ARUV-PUTR                       GBA:  84   p. 24
!    Douglas-fir/bearberry-bitterbrush                   R6 E 132b-83
!
   'CDS631  ','PSME  ', 232.,'DF  ',  45.,   1,   1,   3, &
!----------------------------------------------------------------------
!   24 = PSME/SYOR-O&C                        GBA: 126   p. 71
!    Douglas-fir/Mt. snowberry (Okanogan and Colville)   PNW-GTR-360
!
   'CDS632  ','PSME  ', 400.,'DF  ',  54.,   1,   1,   3, &
!----------------------------------------------------------------------
!   25 = PSME/SYAL                            GBA: 234   p. 66
!    Douglas-fir/common snowberry                        PNW-GTR-360
!
   'CDS633  ','PSME  ', 475.,'DF  ',  81.,   1,   1,   3, &
!---------------------------------------------------------------------
!   26 = PSME/SYAL-WEN                        GBA: 244   p. 72
!    Douglas-fir/common snowberry  (Wenatchee)           PNW-GTR-359
!
   'CDS636  ','PSME  ', 580.,'DF  ',  80.,   1,   1,   3, &
!---------------------------------------------------------------------
!   27 = PSME/SYAL/AGSP                       GBA: 123   p. 74
!    Douglas-fir/common snowberry/bluebunch wheatgrass   PNW-GTR-359
!
   'CDS637  ','PSME  ', 325.,'DF  ',  67.,   1,   1,   3, &
!---------------------------------------------------------------------
!   28 = PSME/SYAL/CARU                       GBA: 208   p. 76
!    Douglas-fir/common snowberry/pinegrass              PNW-GTR-359
!
   'CDS638  ','PSME  ', 425.,'DF  ',  77.,   1,   1,   3, &
!---------------------------------------------------------------------
!   29 = PSME/SPBEL/CARU                      GBA: 177   p. 70
!    Douglas-fir/shiny-leaf spirea/pinegrass             PNW-GTR-359
!
   'CDS639  ','PSME  ', 550.,'DF  ',  65.,   1,   1,   3, &
!---------------------------------------------------------------------
!   30 = PSME/SPBEL                           GBA: 151   p. 82
!    Douglas-fir/shiny-leaf spirea                       PNW-GTR-359
!
   'CDS640  ','PSME  ', 555.,'DF  ',  68.,   1,   1,   3/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=31,45) / &
!---------------------------------------------------------------------
!   31 = PSME/ARUV-WEN                        GBA: 173   p. 80
!    Douglas-fir/bearberry  (Wenatchee)                  PNW-GTR-359
!
   'CDS653  ','PSME  ', 460.,'DF  ',  37.,   1,   1,   3, &
!---------------------------------------------------------------------
!   32 = PSME/ARUV-PUTR                       GBA:  67   p. 81
!    Douglas-fir/bearberry-bitterbrush                   PNW-GTR-359
!
   'CDS654  ','PSME  ', 375.,'DF  ',  51.,   1,   1,   3, &
!---------------------------------------------------------------------
!   33 = PSME/ARUV/CARU                       GBA: 103   p. 80
!    Douglas-fir/bearberry/pinegrass                     PNW-GTR-359
!
   'CDS655  ','PSME  ', 370.,'DF  ',  40.,   1,   1,   3, &
!----------------------------------------------------------------------
!   34 = PSME/SYAL-MTH                        GBA: 278   p. 67
!    Douglas-fir/common snowberry (Mt Hood)              R6 E TP-004-88
!
   'CDS661  ','PSME  ', 767.,'DF  ',  84.,   1,   1,   3, &
!----------------------------------------------------------------------
!   35 = PSME/ARNE                            GBA: 405   p. 63
!    Douglas-fir/pinemat manzanita                       R6 E TP-004-88
!
   'CDS662  ','PSME  ',1118.,'DF  ',  51.,   1,   1,   3, &
!----------------------------------------------------------------------
!   36 = PSME/PUTR                            GBA: 101   p. 82
!    Douglas-fir/bitterbursh                             PNW-GTR-359
!
   'CDS673  ','PSME  ', 525.,'DF  ',  50.,   1,   1,   3, &
!----------------------------------------------------------------------
!   37 = PSME/PUTR/AGSP                       GBA: 145   p. 66
!    Douglas-fir/bitterbursh/bluebunch wheatgrass        PNW-GTR-359
!
   'CDS674  ','PSME  ', 305.,'DF  ',  62.,   1,   1,   3, &
!----------------------------------------------------------------------
!   38 = PSME/PUTR/CARU                       GBA: 153   p. 68
!    Douglas-fir/bitterbrush/pinegrass                   PNW-GTR-359
!
   'CDS675  ','PSME  ', 370.,'DF  ',  58.,   1,   1,   3, &
!---------------------------------------------------------------------
!   39 = PSME/PHMA-O&C                        GBA: 220   p. 55
!    Douglas-fir/ninebark (Okanogan & Colville)          PNW-GTR-360
!
   'CDS715  ','PSME  ', 470.,'DF  ',  63.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   40 = PSME/PHMA-LIBOL                      GBA: 178   p. 61
!    Douglas-fir/ninebark-twinflower                     PNW-GTR-360
!
   'CDS716  ','PSME  ', 600.,'DF  ',  60.,   1,   1,   3, &
!-----------------------------------------------------------------------
!   41 = PSME/VACCI                           GBA: 144   p. 33
!    Douglas-fir/huckleberry                             R6 E 132b-83
!
   'CDS811  ','PSME  ', 397.,'DF  ',  51.,   1,   1,   3, &
!----------------------------------------------------------------------
!   42 = PSME/VACA-COL                        GBA: 191   p. 76
!    Douglas-fir/dwarf huckleberry (Colville)            PNW-GTR-360
!
   'CDS813  ','LAOC  ', 600.,'WL  ',  66.,   1,   1,   2, &
!---------------------------------------------------------------------
!   43 = PSME/VAME-COLV                       GBA: 185   p. 82
!    Douglas-fir/big huckleberry (Colville)              PNW-GTR-360
!
   'CDS814  ','PSME  ', 585.,'DF  ',  66.,   1,   1,   3, &
!----------------------------------------------------------------------
!   44 = PSME/VACA                            GBA: 131   p. 82
!    Douglas-fir/dwarf huckleberry                       PNW-GTR-359
!
   'CDS831  ','PSME  ', 362.,'DF  ',  60.,   1,   1,   3, &
!----------------------------------------------------------------------
!   45 = PSME/VAME-WEN                        GBA: 117   p. 83
!    Douglas-fir/big huckleberry (Wenatchee)             PNW-GTR-359
!
   'CDS832  ','PSME  ', 530.,'DF  ',  53.,   1,   1,   3/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=46,60) / &
!----------------------------------------------------------------------
!   46 = PSME/VAMY/CARU                       GBA: 166   p. 83
!    Douglas-fir/low huckleberry/pinegrass               PNW-GTR-359
!
   'CDS833  ','PSME  ', 265.,'DF  ',  48.,   1,   1,   3, &
!----------------------------------------------------------------------
!   47 = ABLA2/XETE                           GBA: 222   p. 178
!    Subalpine fir/beargrass                             PNW-GTR-360
!
   'CEF111  ','ABLA2 ', 905.,'AF  ',  54.,   1,   1,   9, &
!----------------------------------------------------------------------
!   48 = ABLA2/LIBOL-O&C                      GBA: 202   p. 141
!    Subalpine fir/twinflower (Okanogan & Colville)      PNW-GTR-360
!
   'CEF211  ','ABLA2 ', 685.,'AF  ',  80.,   1,   1,   9, &
!----------------------------------------------------------------------
!   49 = ABLA2/LIBOL-WEN                      GBA: 298   p. 234
!    Subalpine fir/twinflower (Wenatchee)                PNW-GTR-359
!
   'CEF222  ','PIEN  ', 700.,'ES  ',  90.,   1,   1,   8, &
!----------------------------------------------------------------------
!   50 = ABLA2/CLUN                           GBA: 278   p. 131
!    Subalpine fir/queencup beadily                      PNW-GTR-360
!
   'CEF421  ','ABLA2 ', 650.,'AF  ',  87.,   1,   1,   9, &
!----------------------------------------------------------------------
!   51 = ABLA2/TRCA3                          GBA: 242   p. 157
!    Subalpine fir/false bugbane                         PNW-GTR-360
!
   'CEF422  ','ABLA2 ', 745.,'AF  ',  87.,   1,   1,   9, &
!----------------------------------------------------------------------
!   52 = ABLA2/COCA                           GBA: 199   p. 136
!    Subalpine fir/bunchberry dogwood                    PNW-GTR-360
!
   'CEF423  ','ABLA2 ', 675.,'AF  ',  75.,   1,   1,   9, &
!----------------------------------------------------------------------
!   53 = ABLA2/ARLA-POPU                      GBA: 261   p. 214
!    Subalpine fir/broadleaf arnica-skunkleaf polemonium PNW-GTR-359
!
   'CEF424  ','ABLA2 ', 880.,'AF  ',  65.,   1,   1,   9, &
!----------------------------------------------------------------------
!   54 = ABLA2/LUHI-WEN                       GBA: 264   p. 218
!    Subalpine fir/smooth woodrush (Wenatchee)           PNW-GTR-359
!
   'CEG121  ','ABLA2 ', 785.,'AF  ',  65.,   1,   1,   9, &
!----------------------------------------------------------------------
!   55 = ABLA2/CARU-WEN                       GBA: 199   p. 216
!    Subalpine fir/pinegrass  (Wenatchee)                PNW-GTR-359
!
   'CEG310  ','ABLA2 ', 549.,'AF  ',  73.,   1,   1,   9, &
!----------------------------------------------------------------------
!   56 = ABLA2/CARU-O&C                       GBA: 199   p. 126
!    Subalpine fir/pinegrass (Okanogan & Colville)       PNW-GTR-360
!
   'CEG311  ','ABLA2 ', 655.,'AF  ',  77.,   1,   1,   9, &
!---------------------------------------------------------------------
!   57 = PIEN/EQAR                            GBA: 191   p. 184
!    Engelmann spruce/horsetail                          PNW-GTR-360
!
   'CEM211  ','PIEN  ', 535.,'ES  ',  72.,   1,   1,   8, &
!---------------------------------------------------------------------
!   58 = ABLA2/PAMY-OKAN                      GBA: 138   p. 52
!    Subalpine fir/pachistima (Okanogan)                 R6 E 132b-83
!
   'CES111  ','ABLA2 ', 381.,'AF  ',  90.,   1,   1,   9, &
!---------------------------------------------------------------------
!   59 = ABLA2/PAMY-WEN                       GBA: 254   p. 234
!    Subalpine fir/pachistima (Wenatchee)                PNW-GTR-359
!
   'CES113  ','PIEN  ', 820.,'ES  ', 111.,   1,   1,   8, &
!--------------------------------------------------------------------
!   60 = ABLA2/RHAL-XETE                      GBA: 211   p. 152
!    Subalpine fir/Cascades azalea-beargrass             PNW-GTR-360
!
   'CES210  ','ABLA2 ', 790.,'AF  ',  56.,   1,   1,   9/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=61,75) / &
!-------------------------------------------------------------------
!   61 = ABLA2/RHAL                           GBA: 176   p. 220
!    Subalpine fir/Cascade azalea                        PNW-GTR-359
!
   'CES211  ','ABLA2 ', 790.,'AF  ',  52.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   62 = ABLA2/RHAL/LUHI                      GBA: 198   p. 222
!    Subalpine fir/Cascade azalea/smooth woodrush        PNW-GTR-359
!
   'CES213  ','ABLA2 ', 665.,'AF  ',  60.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   63 = ABLA2/VACCI                          GBA: 185   p. 46
!    Subalpine fir/huckleberry                           R6 E 132b-83
!
   'CES312  ','ABLA2 ', 511.,'AF  ', 102.,   1,   1,   9, &
!----------------------------------------------------------------------
!   64 = ABLA2/VAME-COLV                      GBA: 259   p. 168
!    Subalpine fir/big huckleberry (Colville)            PNW-GTR-360
!
   'CES313  ','ABLA2 ', 700.,'AF  ',  76.,   1,   1,   9, &
!----------------------------------------------------------------------
!   65 = ABLA2/VAME-WEN                       GBA: 265   p. 235
!    Subalpine fir/big huckleberry (Wenatchee)           PNW-GTR-359
!
   'CES342  ','PSME  ', 810.,'DF  ',  73.,   1,   1,   3, &
!---------------------------------------------------------------------
!   66 = ABLA2/VASC-O&C                       GBA: 239   p. 173
!    Subalpine fir/grouse huckleberry (Okan & Colv)      PNW-GTR-360
!
   'CES412  ','ABLA2 ', 780.,'AF  ',  63.,   1,   1,   9, &
!---------------------------------------------------------------------
!   67 = ABLA2/VASC/CARU-OKAN                 GBA: 133   p. 236
!    Subalpine fir/grouse huckleberry/pinegrass (Okan)   PNW-GTR-359
!
   'CES413  ','PIEN  ', 670.,'ES  ',  62.,   1,   1,   8, &
!---------------------------------------------------------------------
!   68 = ABLA2/VACA                           GBA:  96   p. 235
!    Subalpine fir/dwarf huckleberry                     PNW-GTR-359
!
   'CES422  ','PICO  ', 620.,'LP  ',  94.,   1,   1,   7, &
!----------------------------------------------------------------------
!   69 = ABLA2/RULA                           GBA: 276   p. 224
!    Subalpine fir/dwarf bramble                         PNW-GTR-359
!
   'CES423  ','ABLA2 ', 785.,'AF  ',  90.,   1,   1,   9, &
!----------------------------------------------------------------------
!   70 = ABLA2/VASC/ARLA                      GBA: 249   p. 230
!    Subalpine fir/grouse huckleberry/broadleaf arnica   PNW-GTR-359
!
   'CES424  ','ABLA2 ', 785.,'AF  ',  51.,   1,   1,   9, &
!---------------------------------------------------------------------
!   71 = ABLA2/VASC/LUHI                      GBA: 146   p. 232
!    Subalpine fir/grouse huckleberry/smooth woodrush    PNW-GTR-359
!
   'CES425  ','ABLA2 ', 720.,'AF  ',  65.,   1,   1,   9, &
!---------------------------------------------------------------------
!   72 = ABLA2/VASC-WEN                       GBA: 423   p. 228
!    Subalpine fir/grouse huckleberry (Wenatchee)        PNW-GTR-359
!
   'CES426  ','PSME  ', 720.,'DF  ',  69.,   1,   1,   3, &
!---------------------------------------------------------------------
!   73 = ABAM/TITRU                           GBA: 447   p. 168
!    Pacific silver fir/coolwort foamflower              PNW-GTR-359
!
   'CFF162  ','ABAM  ',1234.,'SF  ', 143.,   1,   1,   4, &
!---------------------------------------------------------------------
!   74 = ABAM/ACTR-WEN                        GBA: 294   p. 158
!    Pacific silver fir/vanilla leaf  (Wenatchee)        PNW-GTR-359
!
   'CFF254  ','ABAM  ', 935.,'SF  ', 112.,   1,   1,   4, &
!--------------------------------------------------------------------
!   75 = ABAM/VAAL-WEN                        GBA: 316   p. 170
!    Pacific Silver fir/Alaska huckleberry (Wenatchee)   PNW-GTR-359
!
   'CFS232  ','ABAM  ', 872.,'SF  ', 104.,   1,   1,   4/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=76,90) / &
!---------------------------------------------------------------------
!   76 = ABAM/VAME/CLUN-WEN                   GBA: 254   p. 172
!    Silver fir/big huckleberry/queencup beadlily (Wen)  PNW-GTR-359
!
   'CFS233  ','ABAM  ',1070.,'SF  ',  79.,   1,   1,   4, &
!---------------------------------------------------------------------
!   77 = ABAM/VAME-PYSE                       GBA: 241   p. 174
!    Pacific silver fir/big huckleberry-sidebells pyrola PNW-GTR-359
!
   'CFS234  ','ABAM  ', 840.,'SF  ',  62.,   1,   1,   4, &
!--------------------------------------------------------------------
!   78 = ABAM/MEFE-WEN                        GBA: 342   p. 160
!    Pacific silver fir/rusty menziesia (Wenatchee)      PNW-GTR-359
!
   'CFS542  ','ABAM  ', 915.,'SF  ',  84.,   1,   1,   4, &
!---------------------------------------------------------------------
!   79 = ABAM/RHAL-OKAN                       GBA: 234   p. 75
!    Pacific silver fir/Cascade azalea (Okanogan)        R6 E 132b-83
!
   'CFS553  ','ABAM  ', 646.,'SF  ',  45.,   1,   1,   4, &
!---------------------------------------------------------------------
!   80 = ABAM/RHAL-VAME-WEN                   GBA: 268   p. 164
!    Pac silver fir/Cascade azalea-big huckleberry (Wen) PNW-GTR-359
!
   'CFS556  ','ABLA2 ', 940.,'AF  ',  40.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   81 = ABAM/PAMY                            GBA: 281   p. 75
!    Pacific silver fir/pachistima                       R6 E 132b-83
!
   'CFS558  ','PSME  ', 776.,'DF  ',  65.,   1,   1,   3, &
!----------------------------------------------------------------------
!   82 = ABAM/ACCI                            GBA: 306   p. 156
!    Pacific silver fir/vine maple                       PNW-GTR-359
!
   'CFS621  ','ABAM  ', 550.,'SF  ', 104.,   1,   1,   4, &
!----------------------------------------------------------------------
!   83 = TSHE-ABGR/CLUN                       GBA: 289   p. 111
!    Western hemlock-grand fir/queencup beadlily         R6 E TP-004-88
!
   'CHC311  ','ABGR  ', 798.,'GF  ',  81.,   1,   1,   6, &
!----------------------------------------------------------------------
!   84 = TSHE/ACTR-WEN                        GBA: 271   p. 138
!   Western hemlock/vanilla leaf (Wenatchee)             PNW-GTR-359
!
   'CHF223  ','PSME  ', 675.,'DF  ',  73.,   1,   1,   3, &
!---------------------------------------------------------------------
!   85 = TSHE/CLUN                            GBA: 285   p. 204
!   Western hemlock/queencup beadlily                    PNW-GTR-360
!
   'CHF311  ','PSME  ', 835.,'DF  ',  69.,   1,   1,   3, &
!--------------------------------------------------------------------
!   86 = TSHE/ARNU3                           GBA: 287   p. 199
!    Western hemlock/wild sarsaparilla                   PNW-GTR-360
!
   'CHF312  ','PSME  ', 775.,'DF  ',  75.,   1,   1,   3, &
!--------------------------------------------------------------------
!   87 = TSHE/ASCA3                           GBA: 454   p. 142
!    Western hemlock/wild ginger                         PNW-GTR-359
!
   'CHF313  ','PSME  ',1253.,'DF  ',  85.,   1,   1,   3, &
!----------------------------------------------------------------------
!   88 = TSHE/GYDR                            GBA: 506   p. 209
!    Western hemlock/oak-fern                            PNW-GTR-360
!
   'CHF422  ','PSME  ', 900.,'DF  ',  83.,   1,   1,   3, &
!---------------------------------------------------------------------
!   89 = TSHE/XETE-COLV                       GBA: 362   p. 226
!    Western hemlock/beargrass (Colville)                PNW-GTR-360
!
   'CHF521  ','PIEN  ', 830.,'ES  ',  90.,   1,   1,   8, &
!----------------------------------------------------------------------
!   90 = TSHE/BENE-WEN                        GBA: 214   p. 144
!    Western hemlock/Cascade Oregon grape (Wenatchee)    PNW-GTR-359
!
   'CHS142  ','PSME  ', 810.,'DF  ',  82.,   1,   1,   3/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=91,105) / &
!---------------------------------------------------------------------
!   91 = TSHE/PAMY/CLUN                       GBA: 230   p. 146
!    Western hemlock/pachistima/queencup beadlily        PNW-GTR-359
!
   'CHS143  ','PSME  ', 855.,'DF  ',  74.,   1,   1,   3, &
!---------------------------------------------------------------------
!   92 = TSHE/ARNE                            GBA: 147   p. 140
!    Western hemlock/pinemat manzanita                   PNW-GTR-359
!
   'CHS144  ','PSME  ', 705.,'DF  ',  52.,   1,   1,   3, &
!---------------------------------------------------------------------
!   93 = TSHE/ACCI/ACTR-WEN                   GBA: 183   p. 132
!    Western hemlock/vine maple/vanilla leaf (Wenatchee) PNW-GTR-359
!
   'CHS225  ','PSME  ', 565.,'DF  ',  87.,   1,   1,   3, &
!-------------------------------------------------------------------
!   94 = TSHE/ACCI/ASCA3                      GBA: 315   p. 134
!    Western hemlock/vine maple/wild ginger              PNW-GTR-359
!
   'CHS226  ','PSME  ', 720.,'DF  ',  86.,   1,   1,   3, &
!------------------------------------------------------------------
!   95 = TSHE/ACCI/CLUN                       GBA: 249   p. 136
!    Western hemlock/vine maple/queencup beadlily        PNW-GTR-359
!
   'CHS227  ','ABGR  ', 630.,'GF  ',  86.,   1,   1,   6, &
!----------------------------------------------------------------
!   96 = TSHE/RUPE                            GBA: 409   p. 221
!    Western hemlock/five-leaved bramble                 PNW-GTR-360
!
   'CHS411  ','PIEN  ',1129.,'ES  ', 103.,   1,   1,   8, &
!--------------------------------------------------------------------
!   97 = TSHE/MEFE                            GBA: 310   p. 215
!    Western hemlock/rusty menziesia                     PNW-GTR-360
!
   'CHS711  ','PSME  ', 765.,'DF  ',  71.,   1,   1,   3, &
!---------------------------------------------------------------------
!   98 = PICO/SHCA                            GBA: 162   p. 267
!    Lodgepole pine/russet buffaloberry                  PNW-GTR-360
!
   'CLS521  ','PICO  ', 530.,'LP  ',  96.,   1,   1,   7, &
!--------------------------------------------------------------------
!   99 = TSME/XETE-VAMY                       GBA: 245   p. 202
!    Mountain hemlock/beargrass-low huckleberry          PNW-GTR-359
!
   'CMF131  ','TSME  ', 775.,'OT  ',  23.,   1,   1,  11, &
!--------------------------------------------------------------------
!  100 = TSME/LUHI                            GBA: 197   p. 184
!    Mountain hemlock/smooth woodrush                    PNW-GTR-359
!
   'CMG221  ','TSME  ', 544.,'OT  ',  24.,   1,   1,  11, &
!--------------------------------------------------------------------
!  101 = TSME/VASC/LUHI                       GBA: 530   p. 200
!    Mountain hemlock/grouse huckleberry/smooth woodrush PNW-GTR-359
!
   'CMS121  ','TSME  ', 650.,'OT  ',  23.,   1,   1,  11, &
!---------------------------------------------------------------------
!  102 = TSME/RULA                            GBA: 257   p. 194
!    Mountain hemlock/dwarf bramble                      PNW-GTR-359
!
   'CMS122  ','ABAM  ', 940.,'SF  ',  79.,   1,   1,   4, &
!-----------------------------------------------------------------------
!  103 = TSME/MEFE-VAAL                       GBA: 269   p. 186
!    Mountain hemlock/rusty menziesia-Alaska huckleberry PNW-GTR-359
!
   'CMS256  ','ABAM  ', 742.,'SF  ',  94.,   1,   1,   4, &
!-----------------------------------------------------------------------
!  104 = TSME/MEFE-VAME                       GBA: 302   p. 188
!    Mountain hemlock/rusty menziesia-big huckleberry    PNW-GTR-359
!
   'CMS257  ','ABAM  ',1115.,'SF  ', 102.,   1,   1,   4, &
!----------------------------------------------------------------------
!  105 = TSME/VAAL-WEN                        GBA: 410   p. 196
!    Mountain hemlock/Alaska huckleberry (Wenatchee)     PNW-GTR-359
!
   'CMS258  ','TSME  ',1132.,'OT  ',  28.,   1,   1,  11/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=106,120) / &
!-----------------------------------------------------------------------
!  106 = TSME/VAME-WEN                        GBA: 226   p. 198
!    Mountain hemlock/big huckleberry (Wenatchee)        PNW-GTR-359
!
   'CMS259  ','TSME  ', 885.,'OT  ',  20.,   1,   1,  11, &
!-----------------------------------------------------------------------
!  107 = TSME/PHEM-VADE                       GBA: 161   p. 190
!    Mtn hemlock/red mountain heath-Cascade huckleberry  PNW-GTR-359
!
   'CMS354  ','ABLA2 ', 780.,'AF  ',  53.,   1,   1,   9, &
!---------------------------------------------------------------------
!  108 = TSME/RHAL-VAAL                       GBA: 196   p. 204
!    Mountain hemlock/Cascade azalea-Alaska huckleberry  PNW-GTR-359
!
   'CMS355  ','TSME  ', 541.,'OT  ',  26.,   1,   1,  11, &
!----------------------------------------------------------------------
!  109 = TSME/RHAL-VAME                       GBA: 242   p. 192
!    Mountain hemlock/Cascades azalea-big huckleberry    PNW-GTR-359
!
   'CMS356  ','TSME  ', 935.,'OT  ',  20.,   1,   1,  11, &
!---------------------------------------------------------------------
!  110 = PIPO/AGSP-WEN                        GBA:  67   p. 42
!    Ponderosa pine/bluebunch wheatgrass (Wenatchee)     PNW-GTR-359
!
   'CPG141  ','PIPO  ', 200.,'PP  ',  81.,   1,   1,  10, &
!---------------------------------------------------------------------
!  111 = PIPO/CARU-AGSP                       GBA:  65   p. 44
!    Ponderosa pine/pinegrass-bluebunch wheatgrass       PNW-GTR-359
!
   'CPG231  ','PIPO  ', 420.,'PP  ',  49.,   1,   1,  10, &
!---------------------------------------------------------------------
!  112 = PIPO-QUGA/BASA                       GBA: 119   p. 43
!    Ponderosa pine-Or white oak/arrowleaf balsamroot    R6 E TP-004-88
!
   'CPH211  ','PIPO  ', 328.,'PP  ',  65.,   1,   1,  10, &
!--------------------------------------------------------------------
!  113 = PIPO-QUGA/PUTR                       GBA: 124   p. 47
!    Ponderosa pine-Oregon white oak/bitterbrush         R6 E TP-004-88
!
   'CPH212  ','PIPO  ', 342.,'PP  ',  63.,   1,   1,  10, &
!---------------------------------------------------------------------
!  114 = PIPO/PUTR/AGSP                       GBA:  86   p. 46
!    Ponderosa pine/bitterbursh/bluebunch wheatgrass     PNW-GTR-359
!
   'CPS241  ','PIPO  ', 210.,'PP  ',  75.,   1,   1,  10, &
!----------------------------------------------------------------------
!  115 = ABGR-PIEN/SMST                       GBA: 352   p. 107
!    Grand fir-Engelmann spruce/starry solomonseal       R6 E TP-004-88
!
   'CWC511  ','ABGR  ', 972.,'GF  ',  90.,   1,   1,   6, &
!---------------------------------------------------------------------
!  116 = ABGR/LIBO2                           GBA: 257   p. 87
!    Grand fir/twinflower                                R6 E TP-004-88
!
   'CWF321  ','ABGR  ', 709.,'GF  ',  83.,   1,   1,   6, &
!-------------------------------------------------------------------
!  117 = ABGR/ARCO                            GBA: 272   p. 102
!    Grand fir/heartleaf arnica                          PNW-GTR-359
!
   'CWF444  ','ABGR  ', 785.,'GF  ',  72.,   1,   1,   6, &
!---------------------------------------------------------------------
!  118 = ABGR/TRLA2                           GBA: 337   p. 83
!    Grand fir/starflower                                R6 E TP-004-88
!
   'CWF521  ','ABGR  ', 810.,'GF  ',  91.,   1,   1,   6, &
!---------------------------------------------------------------------
!  119 = ABGR/ACTR                            GBA: 298   p. 95
!    Grand fir/vanillaleaf                               R6 E TP-004-88
!
   'CWF522  ','ABGR  ', 710.,'GF  ', 100.,   1,   1,   6, &
!------------------------------------------------------------------
!  120 = ABGR/POPU                            GBA: 346   p. 103
!    Grand fir/skunk-leaved polemonium                   R6 E TP-004-88
!
   'CWF523  ','ABGR  ', 955.,'GF  ',  90.,   1,   1,   6/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=121,135) / &
!-------------------------------------------------------------------
!  121 = ABGR/ACTR-WEN                        GBA: 349   p. 100
!   Grand fir/vanilla leaf (Wenatchee)                   PNW-GTR-359
!
   'CWF524  ','ABGR  ', 963.,'GF  ',  86.,   1,   1,   6, &
!------------------------------------------------------------------
!  122 = ABGR/CAGE                            GBA: 258   p. 71
!    Grand fir/elk sedge                                 R6 E TP-004-88
!
   'CWG121  ','ABGR  ', 712.,'GF  ', 104.,   1,   1,   6, &
!-------------------------------------------------------------------
!  123 = ABGR/CAGE-GP                         GBA: 509   p. 53
!    Grand fir/elk sedge (Gifford Pinchot)               R6 E TP-006-88
!
   'CWG122  ','ABGR  ', 810.,'GF  ', 100.,   1,   1,   6, &
!-------------------------------------------------------------------
!  124 = ABGR/CARU                            GBA: 641   p. 49
!    Grand fir/pinegrass                                 R6 E TP-006-88
!
   'CWG123  ','ABGR  ',1769.,'GF  ', 112.,   1,   1,   6, &
!-------------------------------------------------------------------
!  125 = ABGR/CARU-WEN                        GBA: 175   p. 110
!    Grand fir/pinegrass (Wenatchee)                     PNW-GTR-359
!
   'CWG124  ','ABGR  ', 635.,'GF  ',  85.,   1,   1,   6, &
!-------------------------------------------------------------------
!  126 = ABGR/CARU-LUPIN                      GBA: 214   p. 112
!    Grand fir/pinegrass-lupine                          PNW-GTR-359
!
   'CWG125  ','PSME  ', 750.,'DF  ',  58.,   1,   1,   3, &
!-------------------------------------------------------------------
!  127 = ABGR/VAME/CLUN-COL                   GBA: 361   p. 110
!    Grand fir/big huckleberry/queencup beadlily (Colv)  PNW-GTR-360
!
   'CWS214  ','ABGR  ', 996.,'GF  ',  86.,   1,   1,   6, &
!--------------------------------------------------------------------
!  128 = ABGR/VAME/LIBO2                      GBA: 281   p. 85
!    Grand fir/big huckleberry/twinflower                R6 E TP-006-88
!
   'CWS221  ','ABGR  ', 776.,'GF  ', 100.,   1,   1,   6, &
!--------------------------------------------------------------------
!  129 = ABGR/VAME/CLUN                       GBA: 344   p. 89
!    Grand fir/big huckleberry/queencup beadlily         R6 E TP-006-88
!
   'CWS222  ','ABGR  ', 745.,'GF  ', 103.,   1,   1,   6, &
!----------------------------------------------------------------------
!  130 = ABGR/RUPA/DIHO                       GBA: 332   p. 81
!    Grand fir/thimbleberry/fairy bells                  R6 E TP-006-88
!
   'CWS223  ','ABGR  ', 455.,'GF  ', 108.,   1,   1,   6, &
!---------------------------------------------------------------------
!  131 = ABGR/BENE/ACTR                       GBA: 264   p. 73
!    Grand fir/dwarf Oregon grape/vanillaleaf            R6 E TP-006-88
!
   'CWS224  ','PSME  ', 650.,'DF  ',  69.,   1,   1,   3, &
!---------------------------------------------------------------------
!  132 = ABGR/BENE                            GBA: 249   p. 106
!    Grand fir/Cascade Oregon grape                      PNW-GTR-359
!
   'CWS225  ','ABGR  ', 845.,'GF  ',  77.,   1,   1,   6, &
!---------------------------------------------------------------------
!  133 = ABGR/BENE/CARU-WEN                   GBA: 242   p. 108
!    Grand fir/Cascade Oregon grape/pinegrass-Wenatchee  PNW-GTR-359
!
   'CWS226  ','ABGR  ', 745.,'GF  ',  85.,   1,   1,   6, &
!-----------------------------------------------------------------------
!  134 = ABGR/SYMPH                           GBA: 279   p. 79
!    Grand fir/snowberry                                 R6 E TP-004-88
!
   'CWS331  ','ABGR  ', 695.,'GF  ',  90.,   1,   1,   6, &
!----------------------------------------------------------------------
!  135 = ABGR/SYMO/ACTR                       GBA: 392   p. 65
!    Grand fir/creeping snowberry/vanillaleaf            R6 E TP-006-88
!
   'CWS332  ','ABGR  ', 870.,'GF  ', 108.,   1,   1,   6/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=136,150) / &
!----------------------------------------------------------------------
!  136 = ABGR/SPEBL/PTAQ                      GBA: 215   p. 116
!    Grand fir/shiny-leaf spirea/bracken fern            PNW-GTR-359
!
   'CWS335  ','ABGR  ', 655.,'GF  ',  74.,   1,   1,   6, &
!---------------------------------------------------------------------
!  137 = ABGR/SYAL/CARU                       GBA: 260   p. 118
!    Grand fir/common snowberry/pinegrass                PNW-GTR-359
!
   'CWS336  ','ABGR  ', 580.,'GF  ',  76.,   1,   1,   6, &
!---------------------------------------------------------------------
!  138 = ABGR/SYOR                            GBA: 233   p. 120
!    Grand fir/mountain snowberry                        PNW-GTR-359
!
   'CWS337  ','PSME  ', 360.,'DF  ',  70.,   1,   1,   3, &
!---------------------------------------------------------------------
!  139 = ABGR/ARNE                            GBA: 156   p. 104
!    Grand fir/pinemat manzanita                         PNW-GTR-359
!
   'CWS338  ','PSME  ', 575.,'DF  ',  49.,   1,   1,   3, &
!---------------------------------------------------------------------
!  140 = ABGR/PHMA                            GBA: 240   p. 100
!    Grand fir/ninebark                                  PNW-GTR-360
!
   'CWS421  ','PSME  ', 575.,'DF  ',  79.,   1,   1,   3, &
!--------------------------------------------------------------------
!  141 = ABGR/ACGLD/CLUN                      GBA: 456   p. 95
!    Grand fir/Douglas maple/queencup beadlilly          PNW-GTR-360
!
   'CWS422  ','ABGR  ',1259.,'GF  ',  73.,   1,   1,   6, &
!--------------------------------------------------------------------
!  142 = ABGR/HODI                            GBA: 405   p. 75
!    Grand fir/oceanspray                                R6 E TP-004-88
!
   'CWS531  ','ABGR  ', 860.,'GF  ',  95.,   1,   1,   6, &
!---------------------------------------------------------------------
!  143 = ABGR/ACCI/ACTR                       GBA: 264   p. 91
!    Grand fir/vine maple/vanillaleaf                    R6 E TP-004-88
!
   'CWS532  ','ABGR  ', 780.,'GF  ',  98.,   1,   1,   6, &
!--------------------------------------------------------------------
!  144 = ABGR/CACH                            GBA: 214   p. 99
!    Grand fir/chinquapin                                R6 E TP-004-88
!
   'CWS533  ','PSME  ', 690.,'DF  ',  57.,   1,   1,   3, &
!-------------------------------------------------------------------
!  145 = ABGR/HODI-GP                         GBA: 358   p. 61
!    Grand fir/oceanspray (Gifford Pinchot)              R6 E TP-006-88
!
   'CWS534  ','ABGR  ', 585.,'GF  ', 104.,   1,   1,   6, &
!------------------------------------------------------------------
!  146 = ABGR/ACCI-BEAQ/TRLA2                 GBA: 461   p. 57
!    Grand fir/vine maple-tall Oregongrape/starflower    R6 E TP-006-88
!
   'CWS535  ','ABGR  ', 520.,'GF  ', 116.,   1,   1,   6, &
!------------------------------------------------------------------
!  147 = ABGR/COCO2/ACTR                      GBA: 499   p. 69
!    Grand fir/California hazel/vanillaleaf              R6 E TP-006-88
!
   'CWS536  ','ABGR  ',1377.,'GF  ', 116.,   1,   1,   6, &
!-------------------------------------------------------------------
!  148 = ABGR/CONU/ACTR                       GBA: 329   p. 77
!    Grand fir/pacific dogwood/vanillaleaf               R6 E TP-006-88
!
   'CWS537  ','PSME  ', 650.,'DF  ',  64.,   1,   1,   3, &
!-------------------------------------------------------------------
!  149 = ABGR/ACCI-WEN                        GBA: 511   p. 94
!    Grand fir/vine maple (Wenatchee)                    PNW-GTR-359
!
   'CWS551  ','ABGR  ', 740.,'GF  ', 109.,   1,   1,   6, &
!--------------------------------------------------------------------
!  150 = ABGR/ACCI-CHUM                       GBA: 393   p. 96
!    Grand fir/vine maple-western prince's pine          PNW-GTR-359
!
   'CWS552  ','ABGR  ', 695.,'GF  ', 100.,   1,   1,   6/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=151,155) / &
!--------------------------------------------------------------------
!  151 = ABGR/ACCI/CLUN                       GBA: 395   p. 98
!    Grand fir/vine maple/queencup beadlily              PNW-GTR-359
!
   'CWS553  ','ABGR  ',1090.,'GF  ', 104.,   1,   1,   6, &
!--------------------------------------------------------------------
!  152 = ABGR/HODI/CARU                       GBA: 258   p. 114
!    Grand fir/ocean-spray/pinegrass                     PNW-GTR-359
!
   'CWS554  ','PSME  ', 545.,'DF  ',  70.,   1,   1,   3, &
!--------------------------------------------------------------------
!  153 = ABGR/VACA                            GBA: 205   p. 105
!    Grand fir/dwarf huckleberry                         PNW-GTR-360
!
   'CWS821  ','PSME  ', 560.,'DF  ',  74.,   1,   1,   3, &
!--------------------------------------------------------------------
!  154 = POTR/CARU                            GBA: 189   p. 75
!    Quaking aspen/pinegrass                             R6 E 132b-83
!
   'HQG111  ','PICO  ', 522.,'LP  ',  84.,   1,   1,   7, &
!--------------------------------------------------------------------
!  155 = POTR/SYAL                            GBA: 120   p. 75
!    Quaking aspen/common snowberry                      R6 E 132b-83
!
   'HQS211  ','LAOC  ', 331.,'WL  ',  68.,   1,   1 ,  2/
!-----------------------------------------------------------------------
!  IF INDEX IS POSITIVE, THERE ARE MULTIPLE SPECIES FOR THE PLANT
!  ASSOCIATION, SO JUST RETURN THOSE VALUES.
!----------
IF(INDEX .GT. 0) THEN
  ASPEC  = SPC(INDEX)
  RSDI   = SDIMX(INDEX)
  RSI    = SITE(INDEX)
  ISFLAG = IFLAG(INDEX)
  ISEQ   = FVSSEQ(INDEX)
!----------
!  FIRST OCCURANCE FOR THIS PLANT ASSOCIATION. GO THROUGH LIST, LOCATE
!  THE PLANT ASSOCIATION, AND RETURN THE APPROPRIATE VALUES.
!----------
ELSE
  DO 10 K=1,NENTRY
  IF(APASS .EQ. PA(K)) THEN
    ASPEC  = SPC(K)
    RSDI   = SDIMX(K)
    RSI    = SITE(K)
    ISFLAG = IFLAG(K)
    ISEQ   = FVSSEQ(K)
    NUM    = NUMBR(K)
    INDEX  = K
    GO TO 20
  ENDIF
10   CONTINUE
!----------
!  PLANT ASSOCIATION WAS NOT FOUND.
!----------
  ASPEC  = '    '
  RSDI   = 0.
  RSI    = 0.
  ISFLAG = 0
  ISEQ   = 0
  NUM    = 0
  INDEX  = 0
ENDIF
!----------
!  RETURN TO CALLING PROGRAM.
!----------
20 CONTINUE
RETURN
END

SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
IMPLICIT NONE
!----------
! SO $Id$
!----------
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
!  AN R6 ALL CVS DATA STUDY, JUNE 2008: 1-3,9-21,25,26,31-40,43-62,
!  64-71,73-75,78-80,82-89
!----------
INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
REAL RSI,RSDI
PARAMETER (NENTRY=92)
CHARACTER*4 SPC(NENTRY),ASPEC
CHARACTER*6 SCIEN(NENTRY)
CHARACTER*8 APASS,PA(NENTRY)
INTEGER FVSSEQ(NENTRY)
REAL SITE(NENTRY),SDIMX(NENTRY)
INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=1,15) / &
!-----------------------------------------------------------------------
!      ALPHA     SCIEN         ALPHA        NUM  SITE FVS  PLANT
!       ECO       SITE     MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
!      CLASS      SPEC     SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
!-----------------------------------------------------------------------
!
!    1 = CDS612 = PSME-ABCO/SYAL/LIBO         GBA: 258   p. 78
!    Mixed conifer/snowberry/twinflower                  R6 E 104-85
!
   'CDS612  ','PSME  ', 755.,'DF  ',  85.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    2 = CDS613 = PSME-ABCO/SYAL/FORB         GBA: 359   p. 77
!    Mixed conifer/snowberry/forb                        R6 E 104-85
!
   'CDS613  ','ABCO  ', 810.,'WF  ',  90.,   1,   1,   4, &
!-----------------------------------------------------------------------
!    3 = CDS614 = PSME-ABCO/SYAL/CARU         GBA: 161   p. 76
!    Mixed conifer/snowberry/pinegrass                   R6 E 104-85
!
   'CDS614  ','PSME  ', 615.,'DF  ',  78.,   1,   1,   3, &
!-----------------------------------------------------------------------
!    4 = CEM111 = PIEN/CAEU                   GBA: 230   p. 55
!    Engelmann spruce/widefruit sedge                    R6 E TP-279-87
!
   'CEM111  ','PIEN  ', 635.,'ES  ',  80.,   1,   1,   8, &
!-----------------------------------------------------------------------
!    5 = CEM221 = PIEN/EQAR-STRO              GBA: 258   p. 57
!    Engelmann spruce/common horsetail-twistedstalk      R6 E TP-279-87
!
   'CEM221  ','PIEN  ', 712.,'ES  ',  90.,   1,   1,   8, &
!-----------------------------------------------------------------------
!    6 = CEM222 = PIEN/CLUN                   GBA: 305   p. 49
!    Engelmann spruce/queencup beadlily                  R6 E TP-279-87
!
   'CEM222  ','PIEN  ', 842.,'ES  ', 105.,   1,   1,   8, &
!-----------------------------------------------------------------------
!    7 = CEM311 = PIEN/VAOC2-FORB             GBA: 233   p. 51
!    Engelmann spruce/bog blueberry/forb                 R6 E TP-279-87
!
   'CEM311  ','PIEN  ', 643.,'ES  ',  85.,   1,   1,   8, &
!-----------------------------------------------------------------------
!    8 = CEM312 = PIEN/VAOC2/CAEU             GBA: 161   p. 53
!    Engelmann spruce/bog blueberry/widefruit sedge      R6 E TP-279-87
!
   'CEM312  ','PIEN  ', 444.,'ES  ',  76.,   1,   1,   8, &
!-----------------------------------------------------------------------
!    9 = CLC111 = PICO-PIAL/PELA              GBA:  99   p. 19
!    Lodgepole pine-Whitebark pine/Gay penstemon         R6 E 79-004
!
   'CLC111  ','PICO  ', 625.,'LP  ',  30.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   10 = CLC112 = PICO-PIAL/ARCO2             GBA:  84   p. 20
!    Lodgepole pine-Whitebark pine-W white pine/sandwort R6 E 79-004
!
   'CLC112  ','PICO  ', 690.,'LP  ',  25.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   11 = CLF111 = PICO/FORB                   GBA:  94   p. 11
!    Lodgepole pine/forb                                 R6 E 79-005
!
   'CLF111  ','PICO  ', 365.,'LP  ',  43.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   12 = CLG311 = PICO/STOC-BASIN             GBA:  68   p. 42
!    Lodgepole pine/needlegrass basins                   R6 E 104-85
!
   'CLG311  ','PICO  ', 480.,'LP  ',  38.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   13 = CLG313 = PICO/STOC-LUCA-LINU         GBA: 126   p. 49
!    Lodgepole pine/needlegrass-lupine-linanthastrum     R6 E 104-85
!
   'CLG313  ','PICO  ', 395.,'LP  ',  45.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   14 = CLG314 = PICO/STOC-LUCA              GBA: 106   p. 48
!    Lodgepole pine/needlegrass-lupine                   R6 E 104-85
!
   'CLG314  ','PICO  ', 660.,'LP  ',  52.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   15 = CLG315 = PICO/FRVI-FEID              GBA: 135   p. 21
!    Lodgepole pine/strawberry-fescue                    R6 E 79-004
!
   'CLG315  ','PICO  ', 510.,'LP  ',  44.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=16,30) / &
!-----------------------------------------------------------------------
!   16 = CLG411 = PICO/CAPE-LUCA              GBA: 183   p. 46
!    Lodgepole pine/sedge-lupine                         R6 E 104-85
!
   'CLG411  ','PICO  ', 680.,'LP  ',  49.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   17 = CLG412 = PICO/CAPE-LUCA-PEEU         GBA: 206   p. 47
!    Lodgepole pine/sedge-lupine-penstemon               R6 E 104-85
!
   'CLG412  ','PICO  ', 635.,'LP  ',  50.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   18 = CLG413 = PICO/CAPE-STOC-BASIN        GBA:  49   p. 43
!    Lodgepole pine/sedge-needlegrass basins             R6 E 104-85
!
   'CLG413  ','PICO  ', 590.,'LP  ',  37.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   19 = CLG415 = PICO/SIHY-CAPE              GBA:  79   p. 22
!    Lodgepole pine/squirreltail-long-stolon sedge       R6 E 79-004
!
   'CLG415  ','PICO  ', 540.,'LP  ',  40.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   20 = CLH111 = PICO/POTR/FRVI              GBA: 180   p. 23
!    Lodgepole pine/quaking aspen/strawberry             R6 E 79-004
!
   'CLH111  ','PICO  ', 345.,'LP  ',  48.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   21 = CLM111 = PICO/CANE-ELGL-WET          GBA: 168   p. 32
!    Lodgepole pine/sedge-grass wetland                  R6 E 104-85
!
   'CLM111  ','PICO  ', 540.,'LP  ',  51.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   22 = CLM112 = PICO/POPR                   GBA: 195   p. 29
!    Lodgepole pine/Kentucky bluegrass                   R6 E TP-279-87
!
   'CLM112  ','PICO  ', 538.,'LP  ',  55.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   23 = CLM113 = PICO/CAEU                   GBA: 178   p. 41
!    Lodgepole pine/widefruit sedge                      R6 E TP-279-87
!
   'CLM113  ','PICO  ', 491.,'LP  ',  57.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   24 = CLM114 = PICO/CAAQ                   GBA: 199   p. 43
!    Lodgepole pine/aquatic sedge                        R6 E TP-279-87
!
   'CLM114  ','PICO  ', 549.,'LP  ',  45.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   25 = CLM211 = PICO/ARUV                   GBA: 114   p. 31
!    Lodgepole pine/bearberry                            R6 E TP-279-87
!
   'CLM211  ','PICO  ', 585.,'LP  ',  48.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   26 = CLM311 = PICO/VAOC2/FORB             GBA: 151   p. 37 p. 33
!    Lodgepole pine/blueberry/forb                       R6 E TP-279-87
!                                                        R6 E 104-85
   'CLM311  ','PICO  ', 570.,'LP  ',  47.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   27 = CLM312 = PICO/VAOC2/CAEU             GBA: 169   p. 39
!    Lodgepole pine/bog blueberry/widefruit sedge        R6 E TP-279-87
!
   'CLM312  ','PICO  ', 466.,'LP  ',  54.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   28 = CLM313 = PICO/SPDO/FORB              GBA: 202   p. 33
!    Lodgepole pine/Douglas spiraea/forb                 R6 E TP-279-87
!
   'CLM313  ','PICO  ', 558.,'LP  ',  51.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   29 = CLM314 = PICO/SPDO/CAEU              GBA: 188   p. 35
!    Lodgepole pine/Douglas spiraea/widefruit sedge      R6 E TP-279-87
!
   'CLM314  ','PICO  ', 519.,'LP  ',  59.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   30 = CLM411 = PICO/XETE                   GBA: 194   p. 52
!    Lodgepole pine/beargrass                            R6 E 104-85
!
   'CLM411  ','PICO  ', 535.,'LP  ',  56.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=31,45) / &
!-----------------------------------------------------------------------
!   31 = CLM911 = PICO/PIEN/ELPA2             GBA:  76   p. 45
!    Lodgepole pine-Engel spruce/few-flowered spikerush  R6 E TP-279-87
!
   'CLM911  ','PICO  ', 495.,'LP  ',  35.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   32 = CLS112 = PICO/ARTR-RHYO              GBA:  83   p. 36
!    Lodgepole pine/sagebrush (rhyolite)                 R6 E 104-85
!
   'CLS112  ','PICO  ', 180.,'LP  ',  41.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   33 = CLS211 = PICO/PUTR/STOC              GBA:  97   p. 40
!    Lodgepole pine/bitterbrush/needlegrass              R6 E 104-85
!
   'CLS211  ','PICO  ', 405.,'LP  ',  46.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   34 = CLS212 = PICO/PUTR/CAPE              GBA: 165   p. 44
!    Lodgepole pine/bitterbrush/sedge                    R6 E 104-85
!
   'CLS212  ','PICO  ', 405.,'LP  ',  52.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   35 = CLS213 = PICO/PUTR/FORB              GBA: 105   p. 35
!    Lodgepole pine/bitterbrush/forb                     R6 E 104-85
!
   'CLS213  ','PICO  ', 400.,'LP  ',  43.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   36 = CLS214 = PICO/PUTR/FEID              GBA: 128   p. 39
!    Lodgepole pine/bitterbrush/fescue                   R6 E 104-85
!
   'CLS214  ','PICO  ', 400.,'LP  ',  45.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   37 = CLS215 = PICO/RICE-PUTR/STOC         GBA:  92   p. 41
!    Lodgepole pine/current-bitterbrush/needlegrass      R6 E 104-85
!
   'CLS215  ','PICO  ', 370.,'LP  ',  41.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   38 = CLS216 = PICO/PUTR-RHYO              GBA: 111   p. 38
!    Lodgepole pine/bitterbrush (rhyolite pumice)        R6 E 104-85
!
   'CLS216  ','PICO  ', 345.,'LP  ',  36.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   39 = CLS311 = PICO/ARNE                   GBA:  55   p. 50
!    Lodgepole pine/pinemat manzanita                    R6 E 104-85
!
   'CLS311  ','PICO  ', 575.,'LP  ',  31.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   40 = CLS412 = PICO/VASC                   GBA: 126   p. 51
!    Lodgepole pine/grouse huckleberry                   R6 E 104-85
!
   'CLS412  ','PICO  ', 865.,'LP  ',  45.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   41 = CLS413 = PICO/VASC-FORB              GBA: 161   p. 12
!    Lodgepole pine/grouse huckleberry/forb              R6 E 79-005
!
   'CLS413  ','PICO  ', 444.,'LP  ',  55.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   42 = CLS414 = PICO/VASC/CAPE              GBA: 105   p. 13
!    Lodgepole pine/grouse huckleberry/long-stolon sedge R6 E 79-005
!
   'CLS414  ','PICO  ', 290.,'LP  ',  43.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   43 = CLS911 = PICO/CEVE-ARPA              GBA: 109   p. 45
!    Lodgepole pine/snowbrush-manzanita                  R6 E 104-85
!
   'CLS911  ','PICO  ', 575.,'LP  ',  44.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   44 = CMS111 = TSME/VASC-DES               GBA: 189   p. 24 p. 80
!    Mountain hemlock/grouse huckleberry, Deschutes      R6 E 79-005
!                                                        R6 E 104-85
   'CMS111  ','TSME  ', 895.,'MH  ',  30.,   1,   1,   5, &
!-----------------------------------------------------------------------
!   45 = CPC211 = PIPO-JUOC/CELE/FEID         GBA: 108   p. 24
!    Ponderosa-juniper/mahogany-bitterb-big sage/fescue  R6 E 79-004
!
   'CPC211  ','PIPO  ', 345.,'PP  ',  82.,   1,   1,  10/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=46,60) / &
!-----------------------------------------------------------------------
!   46 = CPF111 = PIPO/WYMO                   GBA: 100   p. 27
!    Ponderosa pine/wooly wyethia                        R6 E 79-004
!
   'CPF111  ','PIPO  ', 510.,'PP  ',  84.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   47 = CPG212 = PIPO/CAPE-FEID-LALA2        GBA: 189   p. 66
!    Ponderosa pine/sedge-fescue-peavine                 R6 E 104-85
!
   'CPG212  ','PIPO  ', 575.,'PP  ',  97.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   48 = CPH311 = PIPO-POTR/PONE              GBA: 124   p. 28
!    Ponderosa pine/quaking aspen/bluegrass              R6 E 79-004
!
   'CPH311  ','PIPO  ', 485.,'PP  ',  84.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   49 = CPS111 = PIPO/PUTR-ARTR/FEID         GBA:  91   p. 56
!    Ponderosa pine/bitterbrush sagebrush/fescue         R6 E 104-85
!
   'CPS111  ','PIPO  ', 285.,'PP  ',  70.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   50 = CPS112 = PIPO/PUTR-ARTR/SIHY         GBA:  55   p. 55
!    Ponderosa pine/bitterbrush-sage/squirreltail (Rhyo) R6 E 104-85
!
   'CPS112  ','PIPO  ', 335.,'PP  ',  75.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   51 = CPS121 = PIPO/ARTR/PONE              GBA:  99   p. 29
!    Ponderosa pine/mtn big sagebrush/bluegrass          R6 E 79-004
!
   'CPS121  ','PIPO  ', 450.,'PP  ',  82.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   52 = CPS211 = PIPO/PUTR/FEID              GBA: 122   p. 57
!    Ponderosa pine/bitterbrush/fescue                   R6 E 104-85
!
   'CPS211  ','PIPO  ', 460.,'PP  ',  81.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   53 = CPS212 = PIPO/PUTR/STOC              GBA: 108   p. 60b
!    Ponderosa pine/bitterbrush/needlegrass              R6 E 104-85
!
   'CPS212  ','PIPO  ', 440.,'PP  ',  85.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   54 = CPS213 = PIPO/PUTR-ARPA/STOC         GBA:  95   p. 61
!    Ponderosa pine/bitterbrush-manzanita/needlegrass    R6 E 104-85
!
   'CPS213  ','PIPO  ', 345.,'PP  ',  81.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   55 = CPS214 = PIPO/PUTR-ARPA/CAPE         GBA:  65   p. 64
!    Ponderosa pine/bitterbrush-manzanita/sedge          R6 E 104-85
!
   'CPS214  ','PIPO  ', 450.,'PP  ',  88.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   56 = CPS215 = PIPO/PUTR/CAPE              GBA: 100   p. 63
!    Ponderosa pine/bitterbrush/sedge                    R6 E 104-85
!
   'CPS215  ','PIPO  ', 425.,'PP  ',  89.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   57 = CPS216 = PIPO/PUTR/FEID-AGSP         GBA:  85   p. 53
!    Ponderosa pine/bitterbrush/bunchgrass               R6 E 104-85
!
   'CPS216  ','PIPO  ', 225.,'PP  ',  77.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   58 = CPS217 = PIPO/PUTR-ARPA/FEID         GBA: 123   p. 58
!    Ponderosa pine/bitterbrush-manzanita/fescue         R6 E 104-85
!
   'CPS217  ','PIPO  ', 425.,'PP  ',  76.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   59 = CPS218 = PIPO/PUTR/SIHY-RHYO         GBA: 111   p. 54
!    Ponderosa pine/bitterbrush/squirreltail (rhyolite)  R6 E 104-85
!
   'CPS218  ','PIPO  ', 385.,'PP  ',  80.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   60 = CPS311 = PIPO/PUTR-CEVE/STOC         GBA: 143   p. 62
!     Ponderosa pine/bitterbrush-snowbrush/needlegrass   R6 E 104-85
!
   'CPS311  ','PIPO  ', 550.,'PP  ',  85.,   1,   1,  10/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=61,75) / &
!-----------------------------------------------------------------------
!   61 = CPS312 = PIPO/PUTR-CEVE/CAPE         GBA:  97   p. 65
!    Ponderosa pine/bitterbrush-snowbrush/sedge          R6 E 104-85
!
   'CPS312  ','PIPO  ', 345.,'PP  ',  89.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   62 = CPS314 = PIPO/PUTR-CEVE/FEID         GBA: 145   p. 59
!    Ponderosa pine/bitterbrush-snowbrush/fescue         R6 E 104-85
!
   'CPS314  ','PIPO  ', 365.,'PP  ',  90.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   63 = CPS511 = PIPO/SYAL-FLOOD             GBA: 187   p. 27
!    Ponderosa pine/common snowberry-floodplain          R6 E TP-279-87
!
   'CPS511  ','PIPO  ', 516.,'PP  ', 101.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   64 = CRG111 = ABMAS/CAPE                  GBA: 288   p. 22
!    Shasta red fir/long-stolon sedge                    R6 E 79-005
!
   'CRG111  ','ABMAS ', 745.,'RF  ', 120.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   65 = CRS111 = ABMAS/ARNE                  GBA: 128   p. 72
!    Mixed conifer/manzanita                             R6 E 104-85
!
   'CRS111  ','ABMAS ', 725.,'RF  ',  79.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   66 = CRS112 = ABMAS-TSME/ARNE/CAPE        GBA: 215   p. 23
!    Shasta red fir-Mtn hemlock/pinemat manzanita/sedge  R6 E 79-005
!
   'CRS112  ','ABMAS ', 910.,'RF  ',  99.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   67 = CRS311 = ABMAS-ABCO/CACH-CHUM/CAPE   GBA: 277   p. 21
!    Shasta red fir-white fir/chink-prince's pine/sedge  R6 E 79-005
!
   'CRS311  ','ABMAS ', 775.,'RF  ', 114.,   1,   1,   9, &
!-----------------------------------------------------------------------
!   68 = CWC111 = ABCO-PIPO-CADE/AMAL         GBA: 265   p. 34
!    White fir-ponderosa pine-incense cedar/serviceberry R6 E 79-004
!
   'CWC111  ','ABCO  ', 665.,'WF  ',  58.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   69 = CWC211 = ABCO/CEVE-CACH/PTAQ         GBA: 159   p. 75
!    Mixed conifer/snowbrush-chinkquapin/brackenfern     R6 E 104-85
!
   'CWC211  ','PIPO  ', 675.,'PP  ',  96.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   70 = CWC212 = ABCO/CEVE-CACH/CARU         GBA: 175   p. 74
!    Mixed conifer/snowbrush-chinkquapin/pinegrass       R6 E 104-85
!
   'CWC212  ','PIPO  ', 630.,'PP  ',  95.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   71 = CWC213 = ABCO/CEVE/CAPE-PTAQ         GBA: 146   p. 69
!    Mixed conifer/snowbrush/sedge-bracken               R6 E 104-85
!
   'CWC213  ','PIPO  ', 645.,'PP  ',  87.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   72 = CWC215 = ABCO-PSME/CEVE-ARUV         GBA: 243   p. 17
!    Mixed conifer/snowbrush-bearberry                   R6 E 79-005
!
   'CWC215  ','ABCO  ', 671.,'WF  ',  78.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   73 = CWC311 = ABCO-PICO/CAPE5-STOC        GBA: 207   p. 30
!    White fir-lodgepole pine/long-stolon sedge-needlegr R6 E 79-004
!
   'CWC311  ','ABCO  ', 770.,'WF  ',  54.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   74 = CWC411 = ABCO-PIPO-PILA/RIVI         GBA: 226   p. 35
!    White fir-ponderosa pine-white pine/sticky currant  R6 E 79-004
!
   'CWC411  ','ABCO  ', 910.,'WF  ',  56.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   75 = CWC412 = ABCO-PIPO-PILA/ARPA         GBA: 241   p. 33
!    White fir-ponderosa pine-sugar pine/manzanita       R6 E 79-004
!
   'CWC412  ','ABCO  ', 510.,'WF  ',  68.,   1,   1,   4/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=76,90) / &
!-----------------------------------------------------------------------
!   76 = CWC911 = PIEN-BOTTOMS                GBA: 247   p. 79
!    Engelmann spruce bottomlands                        R6 E 104-85
!
   'CWC911  ','PIEN  ', 682.,'ES  ',  86.,   1,   1,   8, &
!-----------------------------------------------------------------------
!   77 = CWF431 = ABCO/CLUN                   GBA: 316   p. 47
!    White fir/queencup beadlily                         R6 E TP-279-87
!
   'CWF431  ','ABCO  ', 872.,'WF  ',  81.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   78 = CWH111 = ABCO/CEVE-CACH              GBA: 140   p. 73
!    Mixed conifer/snowberry-chinquapin                  R6 E 104-85
!
   'CWH111  ','PIPO  ', 675.,'PP  ',  91.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   79 = CWH112 = ABCO/CACH-PAMY-CHUM         GBA: 237   p. 20
!    White fir/chinquapin-boxwood-prince's pine          R6 E 79-005
!
   'CWH112  ','ABCO  ', 750.,'WF  ',  84.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   80 = CWH211 = ABCO-PIPO-POTR/CAPE         GBA: 136   p. 36
!    White fir-ponderosa pine-aspen/long-stolon sedge    R6 E 79-004
!
   'CWH211  ','PIPO  ', 270.,'PP  ',  84.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   81 = CWM111 = ABCO/ALTE                   GBA: 220   p. 16
!    White fir-alder/shrub meadow                        R6 E 79-005
!
   'CWM111  ','ABCO  ', 607.,'WF  ',  81.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   82 = CWS112 = ABCO/CEVE-ARPA              GBA: 137   p. 70
!    Mixed conifer/snowbrush-manzanita                   R6 E 104-85
!
   'CWS112  ','PIPO  ', 660.,'PP  ',  85.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   83 = CWS113 = ABCO/CEVE-ARPA/CAPE-PEEU    GBA: 254   p. 71
!    M conifer/manzanita-snowbrush/sedge-penstemon       R6 E 104-85
!
   'CWS113  ','PIPO  ', 660.,'PP  ',  88.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   84 = CWS114 = ABCO/CEVE                   GBA: 118   p. 67
!    Mixed conifer/snowbrush                             R6 E 104-85
!
   'CWS114  ','PIPO  ', 725.,'PP  ',  86.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   85 = CWS115 = ABCO/CEVE/CAPE              GBA: 149   p. 68
!    Mixed conifer/snowbrush/sedge                       R6 E 104-85
!
   'CWS115  ','PIPO  ', 790.,'PP  ',  88.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   86 = CWS116 = ABCO/CEVE-CEPR/FRVI         GBA:  81   p. 18
!    Mixed conifer/snowbrush-squawcarpet/strawberry      R6 E 79-005
!
   'CWS116  ','ABCO  ', 560.,'WF  ',  65.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   87 = CWS117 = ABCO-PIPO/ARPA-BERE         GBA: 103   p. 32
!    White fir-ponderosa pine/manzanita-Oregon grape     R6 E 79-004
!
   'CWS117  ','PIPO  ', 570.,'PP  ',  86.,   1,   1,  10, &
!-----------------------------------------------------------------------
!   88 = CWS312 = ABCO/SYAL/FRVI              GBA: 128   p. 19
!    White fir/snowberry/strawberry                      R6 E 79-005
!
   'CWS312  ','ABCO  ', 485.,'WF  ',  69.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   89 = CWS313 = ABCO-PIPO/SYAL/STJA         GBA: 240   p. 31
!    White fir-ponderosa pine/snowberry/starwort         R6 E 79-004
!
   'CWS313  ','ABCO  ', 810.,'WF  ',  63.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   90 = HQM121 = POTR/ELGL                   GBA: 168   p. 61
!    Quaking aspen/blue wildrye                          R6 E TP-279-87
!
   'HQM121  ','PICO  ', 464.,'LP  ',  55.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=91,92) / &
!-----------------------------------------------------------------------
!   91 = HQM411 = POTR-PICO/SPDO/CAEU         GBA: 232   p. 63
!    Q aspen-lodgepole pine/Doug spiraea/widefruit sedge R6 E TP-279-87
!
   'HQM411  ','PICO  ', 640.,'LP  ',  59.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   92 = HQS221 = POTR/SYAL/ELGL              GBA: 216   p. 59
!    Quaking aspen/common snowberry/blue wildrye         R6 E TP-279-87
!
   'HQS221  ','PIPO  ', 596.,'PP  ', 101.,   1,   1,  10/
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

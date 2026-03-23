SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
IMPLICIT NONE
!----------
! WC $Id$
!----------
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
!  AN R6 ALL CVS DATA STUDY, JUNE 2008: 8-14,16-21,23-39,41-45,49-52,
!  54,56-60,62-67,69-74,76-83,86-89,91-103,105,107-139
!----------
INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
PARAMETER (NENTRY=139)
CHARACTER*4 SPC(NENTRY),ASPEC
CHARACTER*6 SCIEN(NENTRY)
CHARACTER*8 APASS,PA(NENTRY)
INTEGER FVSSEQ(NENTRY)
REAL SITE(NENTRY),SDIMX(NENTRY)
INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
REAL RSI,RSDI
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=1,15) / &
!-----------------------------------------------------------------------
!      ALPHA    SCIEN          ALPHA        NUM  SITE FVS  PLANT
!       ECO      SITE      MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
!      CLASS     SPEC      SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
!-----------------------------------------------------------------------
!    1 = TSME-ABLA2/PONE4                     GBA: 253   p. 31
!    Mountain hemlock-subalpine fir/Newberry's knotweed  R6 TP-08-95
!
   'CAF211  ','TSME  ', 698.,'MH  ',  14.,   1,   1,  20, &
!-----------------------------------------------------------------------
!    2 = TSME-ABLA2/ASLE2                     GBA: 196   p. 19
!    Mountain hemlock-subalpine fir/Cascades aster       R6 TP-08-95
!
   'CAF311  ','TSME  ', 541.,'MH  ',  15.,   1,   1,  20, &
!----------------------------------------------------------------------
!    3 = TSME-ABLA2/FEVI                      GBA: 135   p. 23
!    Mountain hemlock-subalpine fir/green fescue         R6 TP-08-95
!
   'CAG211  ','TSME  ', 373.,'MH  ',  12.,   1,   1,  20, &
!----------------------------------------------------------------------
!    4 = TSME/LUHI                            GBA: 297   p. 35
!    Mountain hemlock/Hitchcock's woodrush               R6 TP-08-95
!
   'CAG311  ','TSME  ', 820.,'MH  ',  17.,   1,   1,  20, &
!----------------------------------------------------------------------
!    5 = TSME-PIAL/LUHI                       GBA: 257   p. 47
!    Mountain hemlock-whitebark pine/Hitchc woodrush     R6 TP-08-95
!
   'CAG312  ','TSME  ', 709.,'MH  ',  13.,   1,   1,  20, &
!-----------------------------------------------------------------------
!    6 = TSME/PHEM-VADE                       GBA: 269   p. 43
!    Mountain hemlock/red mtn heather-delicious huckleb  R6 TP-08-95
!
   'CAS211  ','TSME  ', 742.,'MH  ',  16.,   1,   1,  20, &
!-----------------------------------------------------------------------
!    7 = TSME-ABLA2/JUOC4                     GBA: 466   p. 27
!    Mountain hemlock-subalpine fir/mtn juniper          R6 TP-08-95
!
   'CAS411  ','TSME  ',1286.,'MH  ',  12.,   1,   1,  20, &
!-----------------------------------------------------------------------
!    8 = PSME-TSHE/BENE                       GBA: 400   p. 78
!    Douglas-fir-western hemlock/dwarf Oregon grape      R6 E 257-B-86
!
   'CDC711  ','PSME  ', 810.,'DF  ', 145.,   1,   1,  16, &
!-----------------------------------------------------------------------
!    9 = PSME-TSHE/RHMA                       GBA: 317   p. 82
!    Douglas-fir-western hemlock/rhododendron            R6 E 257-B-86
!
   'CDC712  ','PSME  ', 785.,'DF  ', 133.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   10 = PSME-TSHE/GASH                       GBA: 404   p. 86
!    Douglas-fir-western hemlock/salal                   R6 E 257-B-86
!
   'CDC713  ','PSME  ', 685.,'DF  ', 138.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   11 = PSME/HODI-BENE                       GBA: 311   p. 62
!    Douglas-fir/oceanspray-dwarf Oregon grape           R6 E 257-B-86
!
   'CDS211  ','PSME  ', 770.,'DF  ', 115.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   12 = PSME/HODI/GRASS                      GBA: 312   p. 66
!    Douglas-fir/oceanspray/grass                        R6 E 257-B-86
!
   'CDS212  ','PSME  ', 565.,'DF  ', 121.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   13 = PSME/HODI-WHMO                       GBA: 290   p. 70
!    Douglas-fir/oceanspray-whipple vine                 R6 E 257-B-86
!
   'CDS213  ','PSME  ', 670.,'DF  ', 106.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   14 = PSME/SYMO-WIL                        GBA: 496   p. 74
!    Douglas-fir/snowberry (Willamette)                  R6 E 257-B-86
!
   'CDS641  ','PSME  ', 740.,'DF  ', 123.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   15 = ABAM-TSHE/RHMA-GASH                  GBA: 276   p. 49
!    Pac silver fir-W. hemlock/rhododendron-salal        R6 E 100-82
!
   'CFC251  ','PSME  ', 762.,'DF  ', 101.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=16,30) / &
!-----------------------------------------------------------------------
!   16 = ABAM-ABGR/SMST                       GBA: 496   p. 98
!    Pac silver fir-grand fir/false solomonseal          R6 E 257-B-86
!
   'CFC311  ','PSME  ', 935.,'DF  ', 133.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   17 = ABAM/TIUN                            GBA: 315   p. 61
!    Pac silver fir/coolwort foamflower                  R6 E 130a-83
!
   'CFF152  ','ABAM  ',1095.,'SF  ', 120.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   18 = ABAM/OXOR                            GBA: 500   p. 33
!   Pac silver fir/oxalis                                R6 E 100-82
!
   'CFF153  ','ABPR  ',1050.,'NF  ', 135.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   19 = ABAM/TIUN-STRO                       GBA: 501   p. 100
!    Pac silver fir/foamflower-rosy twisted stalk        R6 E TP-028-91
!
   'CFF154  ','ABAM  ', 960.,'SF  ', 134.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   20 = ABAM/ACTR-MBS                        GBA: 410   p. 84
!    Pac silver fir/vanilla leaf (Mt Baker/Snoq)         R6 E TP-028-91
!
   'CFF250  ','PSME  ', 900.,'DF  ', 155.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   21 = ABAM/ACTR-CLUN                       GBA: 415   p. 57
!    Pac silver fir/vanilla leaf-queencup beadlily       R6 E 130a-83
!
   'CFF253  ','ABPR  ', 955.,'NF  ', 134.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   22 = ABAM/XETE-MBS                        GBA: 507   p. 132
!   Pac silver fir/beargrass (Mt Baker/Snoq)             R6 E TP-028-91
!                                                        & Devlin memo
   'CFF312  ','ABPR  ',1399.,'NF  ', 117.,   1,   1,   7, &
!----------------------------------------------------------------------
!   23 = ABAM/RUPE-BLSP                       GBA: 627   p. 98
!    Pac silver fir/five-leaved bramble-deerfern         R6 E TP-028-91
!
   'CFF450  ','ABAM  ',1110.,'SF  ', 142.,   1,   1,   1, &
!----------------------------------------------------------------------
!   24 = ABAM/LYAM                            GBA: 744   p. 90
!    Pac silver fir/skunkcabbage                         R6 E TP-028-91
!
   'CFM111  ','ABAM  ', 715.,'SF  ', 134.,   1,   1,   1, &
!----------------------------------------------------------------------
!   25 = ABAM/BENE-MBS                        GBA: 242   p. 86
!    Pac silver fir/Oregon grape (Mt Baker/Snoqualamie)  R6 E TP-028-91
!
   'CFS110  ','ABPR  ', 820.,'NF  ', 109.,   1,   1,   7, &
!---------------------------------------------------------------------
!   26 = ABAM/BENE                            GBA: 274   p. 56
!   Pac silver fir/dwarf Oregon grape                    R6 E 130a-83
!
   'CFS151  ','TSHE  ',1035.,'WH  ',  64.,   1,   1,  19, &
!---------------------------------------------------------------------
!   27 = ABAM/GASH-GP                         GBA: 324   p. 55
!   Pac silver fir/Salal (Giff Pinchot)                  R6 E 130a-83
!
   'CFS152  ','ABAM  ',1035.,'SF  ', 108.,   1,   1,   1, &
!---------------------------------------------------------------------
!   28 = ABAM/GASH-BENE                       GBA: 210   p. 88
!   Pac silver fir/salal-Oregon grape                    R6 E TP-028-91
!
   'CFS154  ','ABAM  ',1040.,'SF  ', 115.,   1,   1,   1, &
!---------------------------------------------------------------------
!   29 = ABAM/VAAL-BENE                       GBA: 410   p. 104
!   Pac silver fir/Alaska huckleberry-Oregon grape       R6 E TP-028-91
!
   'CFS216  ','ABAM  ',1015.,'SF  ', 124.,   1,   1,   1, &
!---------------------------------------------------------------------
!   30 = ABAM/VAME-VASI                       GBA: 442   p. 128
!   Pac silver fir/big huckleberry-Sitka valerian        R6 E TP-028-91
!
   'CFS221  ','ABAM  ', 900.,'SF  ',  99.,   1,   1,   1/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=31,45) / &
!---------------------------------------------------------------------
!   31 = ABAM/VAME-STRO                       GBA: 546   p. 124
!   Pac silver fir/big huckleberry-rosy twisted stalk    R6 E TP-028-91
!
   'CFS222  ','ABAM  ', 975.,'SF  ', 118.,   1,   1,   1, &
!---------------------------------------------------------------------
!   32 = ABAM/VAME-VAAL                       GBA: 302   p. 126
!   Pac silver fir/big huckleberry-Alaska huckleberry    R6 E TP-028-91
!
   'CFS223  ','ABAM  ', 935.,'SF  ', 102.,   1,   1,   1, &
!---------------------------------------------------------------------
!   33 = ABAM/VAME                            GBA: 241   p. 120
!   Pac silver fir/big huckleberry                       R6 E TP-028-91
!
   'CFS224  ','ABAM  ',1100.,'SF  ', 100.,   1,   1,   1, &
!----------------------------------------------------------------------
!   34 = ABAM/VAAL-MADI2                      GBA: 643   p. 110
!   Pac silver fir/Ak huckleberry-false lily-of-the-val  R6 E TP-028-91
!
   'CFS225  ','ABAM  ', 945.,'SF  ', 126.,   1,   1,   1, &
!----------------------------------------------------------------------
!   35 = ABAM/VAAL-TIUN-MBS                   GBA: 517   p. 116
!   Pac silver fir/Alaska huckleberry-foamflower         R6 E TP-028-91
!
   'CFS226  ','ABAM  ',1030.,'SF  ', 136.,   1,   1,   1, &
!----------------------------------------------------------------------
!   36 = ABAM/VAME-PYSE                       GBA: 299   p. 122
!   Pac silver fir/big huckleberry-sidebells pyrola      R6 E TP-028-91
!
   'CFS229  ','ABAM  ',1110.,'SF  ', 108.,   1,   1,   1, &
!----------------------------------------------------------------------
!   37 = ABAM/VAAL-GASH-MBS                   GBA: 476   p. 108
!   Pac silver fir/Alaska huckleberry-salal (Mt B/Snoq)  R6 E TP-028-91
!
   'CFS230  ','ABAM  ', 830.,'SF  ', 101.,   1,   1,   1, &
!----------------------------------------------------------------------
!   38 = ABAM/VAAL-POMU                       GBA: 955   p. 112
!   Pac silver fir/Alaska huckleberry-swordfern          R6 E TP-028-91
!
   'CFS231  ','ABAM  ',1035.,'SF  ', 148.,   1,   1,   1, &
!---------------------------------------------------------------------
!   39 = ABAM/VAME/XETE                       GBA: 286   p. 66
!   Pac silver fir/big huckleberry/beargrass             R6 E 130a-83
!
   'CFS251  ','ABAM  ', 955.,'SF  ',  94.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   40 = ABAM/VAME-XETE-MBS                   GBA: 386   p. 130
!   Pac silver fir/big huckleberry-beargrass (Mt B/Snoq) R6 E TP-028-91
!                                                        & Devlin memo
   'CFS252  ','ABAM  ',1065.,'SF  ',  94.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   41 = ABAM/VAAL/COCA                       GBA: 407   p. 45
!   Pac silver fir/Alaska huckleberry/dogwood bunchberry R6 E 100-82
!
   'CFS253  ','ABPR  ', 975.,'NF  ', 110.,   1,   1,   7, &
!----------------------------------------------------------------------
!   42 = ABAM/MEFE                            GBA: 278   p. 64
!   Pac silver fir/fool's huckleberry                    R6 E 130a-83
!
   'CFS254  ','ABAM  ',1035.,'SF  ', 103.,   1,   1,   1, &
!---------------------------------------------------------------------
!   43 = ABAM/VAAL-GASH                       GBA: 294   p. 60
!   Pac silver fir/Alaska huckleberry-salal              R6 E 130a-83
!
   'CFS255  ','ABAM  ', 880.,'SF  ', 113.,   1,   1,   1, &
!----------------------------------------------------------------------
!   44 = ABAM/VAME/CLUN                       GBA: 243   p. 65
!   Pac silver fir/big huckleberry/queencup beadlily     R6 E 130a-83
!
   'CFS256  ','ABAM  ', 980.,'SF  ', 113.,   1,   1,   1, &
!----------------------------------------------------------------------
!   45 = ABAM/VAAL                            GBA: 250   p. 59
!   Pac silver fir/Alaska huckleberry                    R6 E 130a-83
!
   'CFS257  ','ABAM  ', 985.,'SF  ', 111.,   1,   1,   1/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=46,60) / &
!----------------------------------------------------------------------
!   46 = ABAM/VAAL-MBS                        GBA: 366   p. 102
!   Pac silver fir/Alaska huckleberry (Mt Baker/Snoq)    R6 E TP-028-91
!                                                        & Devlin memo
   'CFS258  ','ABAM  ',1010.,'SF  ', 116.,   1,   1,   1, &
!----------------------------------------------------------------------
!   47 = ABAM/VAAL-XETE-MBS                   GBA: 227   p. 118
!   Pac silver fir/Alaska huckleberry-beargrass (MB/SQ)  R6 E TP-028-91
!                                                        & Devlin memo
   'CFS259  ','ABAM  ', 626.,'SF  ',  94.,   1,   1,   1, &
!----------------------------------------------------------------------
!   48 = ABAM/VAAL-CLUN-MBS                   GBA: 556   p. 106
!   Pac silver fir/AK huckleberry-queen's cup (MB/SQ)    R6 E TP-028-91
!                                                        & Devlin memo
   'CFS260  ','ABAM  ',1535.,'SF  ', 128.,   1,   1,   1, &
!----------------------------------------------------------------------
!   49 = ABAM/OPHO                            GBA: 370   p. 62
!   Pac silver fir/devil's club                          R6 E 130a-83
!
   'CFS351  ','ABAM  ', 825.,'SF  ', 130.,   1,   1,   1, &
!----------------------------------------------------------------------
!   50 = ABAM/OPHO-VAAL                       GBA: 585   p. 92
!   Pac silver fir/devil's club-Alaska huckleberry       R6 E TP-028-91
!
   'CFS352  ','ABAM  ',1030.,'SF  ', 133.,   1,   1,   1, &
!----------------------------------------------------------------------
!   51 = ABAM/RHAL-GP                         GBA: 214   p. 63
!   Pac silver fir/Cascades azalea (Gifford Pinchot)     R6 E 130a-83
!
   'CFS550  ','ABAM  ',1120.,'SF  ', 102.,   1,   1,   1, &
!----------------------------------------------------------------------
!   52 = ABAM/RHAL/XETE                       GBA: 282   p. 37
!   Pac silver fir/Cascades azalea/beargrass             R6 E 100-82
!
   'CFS551  ','PSME  ', 815.,'DF  ',  73.,   1,   1,  16, &
!----------------------------------------------------------------------
!   53 = ABAM/RHAL/CLUN                       GBA: 282   p. 35
!   Pac silver fir/Cascades azalea/queencup beadlily     R6 E 100-82
!
   'CFS552  ','PSME  ', 778.,'DF  ',  73.,   1,   1,  16, &
!----------------------------------------------------------------------
!   54 = ABAM/RHAL-VAME                       GBA: 241   p. 96
!   Pac silver fir/white rhododendron-big huckleberry    R6 E TP-028-91
!
   'CFS554  ','ABAM  ', 995.,'SF  ',  93.,   1,   1,   1, &
!----------------------------------------------------------------------
!   55 = ABAM/RHAL-VAAL                       GBA: 259   p. 94
!   Pac silver fir/white rhododendron-Alaska huckleberry R6 E TP-028-91
!
   'CFS555  ','ABAM  ', 715.,'SF  ',  98.,   1,   1,   1, &
!----------------------------------------------------------------------
!   56 = ABAM/ACCI/TIUN                       GBA: 505   p. 43
!   Pac silver fir/vine maple/coolwort foamflower        R6 E 100-82
!
   'CFS651  ','ABPR  ',1030.,'NF  ', 140.,   1,   1,   7, &
!---------------------------------------------------------------------
!   57 = ABAM/RHMA-BENE                       GBA: 296   p. 55
!   Pac silver fir/rhododendron-dwarf Oregon grape       R6 E 100-82
!
   'CFS652  ','PSME  ',1010.,'DF  ', 104.,   1,   1,  16, &
!---------------------------------------------------------------------
!   58 = ABAM/RHMA/XETE                       GBA: 501   p. 57
!   Pac silver fir/rhododendron/beargrass                R6 E 100-82
!
   'CFS653  ','ABPR  ', 910.,'NF  ',  96.,   1,   1,   7, &
!---------------------------------------------------------------------
!   59 = ABAM/RHMA-VAAL/COCA                  GBA: 347   p. 47
!   Pac silver fir/rhododendron-Ak huckleb/dogwood bunch R6 E 100-82
!
   'CFS654  ','PSME  ', 995.,'DF  ',  97.,   1,   1,  16, &
!--------------------------------------------------------------------
!   60 = TSHE-PSME/HODI                       GBA: 372   p. 102
!   Western hemlock-Douglas-fir/oceanspray               R6 E 230A-86
!
   'CHC212  ','PSME  ', 675.,'DF  ', 120.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=61,75) / &
!-------------------------------------------------------------------
!   61 = TSHE-PSME-ARME                       GBA: 385   p. 105
!   Western hemlock-Douglas-fir-madrone                  R6 E 230A-86
!
   'CHC213  ','PSME  ',1063.,'DF  ', 105.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   62 = TSHE/OXOR-WILL                       GBA: 467   p. 202
!   Western hemlock/Oregon oxalis (Willamette)           R6 E 257-86
!
   'CHF111  ','PSME  ', 800.,'DF  ', 158.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   63 = TSHE/POMU-MTH                        GBA: 466   p. 73
!   Western hemlock/swordfern  (Mt Hood)                 R6 E 232A-86
!
   'CHF123  ','TSHE  ', 770.,'WH  ',  95.,   1,   1,  19, &
!----------------------------------------------------------------------
!   64 = TSHE/POMU-OXOR                       GBA: 527   p. 75
!   Western hemlock/swordfern-oxalis                     R6 E 232A-86
!
   'CHF124  ','TSHE  ', 905.,'WH  ', 102.,   1,   1,  19, &
!----------------------------------------------------------------------
!   65 = TSHE/POMU-GP                         GBA: 431   p. 82
!   Western hemlock/swordfern (Gifford Pinchot)          R6 E 230A-86
!
   'CHF125  ','TSHE  ', 740.,'WH  ',  96.,   1,   1,  19, &
!---------------------------------------------------------------------
!   66 = TSHE/POMU-GASH                       GBA: 311   p. 54
!   Western hemlock/swordfern-salal                      R6 E TP-028-91
!
   'CHF133  ','PSME  ',1005.,'DF  ', 151.,   1,   1,  16, &
!---------------------------------------------------------------------
!   67 = TSHE/POMU-BENE                       GBA: 543   p. 52
!   Western hemlock/swordfern-Oregon grape               R6 E TP-028-91
!
   'CHF134  ','PSME  ',1090.,'DF  ', 154.,   1,   1,  16, &
!---------------------------------------------------------------------
!   68 = TSHE/POMU-TITR-MBS                   GBA: 555   p. 56
!   Western hemlock/swordfern-foamflower                 R6 E TP-028-91
!                                                        & Devlin memo
   'CHF135  ','TSHE  ',1532.,'WH  ', 123.,   1,   1,  19, &
!----------------------------------------------------------------------
!   69 = TSHE/POMU-WILL                       GBA: 402   p. 234
!   Western hemlock/swordfern (Willamette)               R6 E 257-86
!
   'CHF151  ','PSME  ', 870.,'DF  ', 159.,   1,   1,  16, &
!----------------------------------------------------------------------
!   70 = TSHE/ACTR                            GBA: 402   p. 90
!   Western hemlock/vanilla leaf                         R6 E 230A-86
!
   'CHF221  ','PSME  ', 960.,'DF  ', 147.,   1,   1,  16, &
!---------------------------------------------------------------------
!   71 = TSHE/TITR                            GBA: 564   p. 80
!   Western hemlock/coolwort foamflower                  R6 E 230A-86
!
   'CHF222  ','PSME  ', 975.,'DF  ', 170.,   1,   1,  16, &
!---------------------------------------------------------------------
!   72 = TSHE/TITR-GYDR                       GBA: 1121  p. 58
!   Western hemlock/foamflower-oak fern                  R6 E TP-028-91
!
   'CHF250  ','PSME  ', 965.,'DF  ', 164.,   1,   1,  16, &
!---------------------------------------------------------------------
!   73 = TSHE/LIBO2                           GBA: 525   p. 238
!   Western hemlock/twinflower                           R6 E 257-86
!
   'CHF321  ','PSME  ',1020.,'DF  ', 148.,   1,   1,  16, &
!---------------------------------------------------------------------
!   74 = TSHE/ATFI                            GBA: 601   p. 72
!   Western hemlock/ladyfern                             R6 E 230A-86
!
   'CHF421  ','PSME  ', 880.,'DF  ', 174.,   1,   1,  16, &
!--------------------------------------------------------------------
!   75 = TSHE/LYAM                            GBA: 408   p. 68
!   Western hemlock/American yellow skunkcabbage         R6 E 232A-86
!
   'CHM121  ','PSME  ',1126.,'DF  ', 128.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=76,90) / &
!---------------------------------------------------------------------
!   76 = TSHE/GASH-WILL                       GBA: 334   p. 230
!   Western hemlock/salal (Willamette)                   R6 E 257-86
!
   'CHS111  ','PSME  ', 740.,'DF  ', 137.,   1,   1,  16, &
!---------------------------------------------------------------------
!   77 = TSHE/BENE/OXOR                       GBA: 514   p. 190
!   Western hemlock/dwarf Oregon grape/Oregon oxalis     R6 E 257-86
!
   'CHS113  ','PSME  ', 770.,'DF  ', 159.,   1,   1,  16, &
!--------------------------------------------------------------------
!   78 = TSHE/BENE/ACTR                       GBA: 476   p. 198
!   Western hemlock/dwarf Oregon grape/vanilla leaf      R6 E 257-86
!
   'CHS114  ','PSME  ',1010.,'DF  ', 158.,   1,   1,  16, &
!---------------------------------------------------------------------
!   79 = TSHE/BENE-GASH                       GBA: 440   p. 62
!   Western hemlock/dwarf Oregon grape-salal             R6 E 232A-86
!
   'CHS124  ','TSHE  ', 845.,'WH  ',  93.,   1,   1,  19, &
!---------------------------------------------------------------------
!   80 = TSHE/BENE                            GBA: 424   p. 93
!   Western hemlock/dwarf Oregon grape                   R6 E 230A-86
!
   'CHS125  ','TSHE  ',1020.,'WH  ',  82.,   1,   1,  19, &
!-----------------------------------------------------------------------
!   81 = TSHE/BENE/POMU                       GBA: 380   p. 64
!   Western hemlock/dwarf Oregon grape/swordfern         R6 E 232A-86
!
   'CHS126  ','TSHE  ', 835.,'WH  ',  89.,   1,   1,  19, &
!----------------------------------------------------------------------
!   82 = TSHE/BENE-GASH-GP                    GBA: 381   p. 95
!   Western hemlock/dwarf Oregon grape-salal (Giff Pin)  R6 E 230A-86
!
   'CHS127  ','PSME  ', 925.,'DF  ', 134.,   1,   1,  16, &
!----------------------------------------------------------------------
!   83 = TSHE/GASH-GP                         GBA: 317   p. 97
!   Western hemlock/salal (Gifford Pinchot)              R6 E 230A-86
!
   'CHS128  ','PSME  ', 820.,'DF  ', 123.,   1,   1,  16, &
!----------------------------------------------------------------------
!   84 = TSHE/GASH-MBS                        GBA: 286   p. 40
!   Western hemlock/salal (Mt Baker/Snoqual)             R6 E TP-028-91
!                                                        & Devlin memo
   'CHS129  ','PSME  ', 789.,'DF  ', 100.,   1,   1,  16, &
!---------------------------------------------------------------------
!   85 = TSHE/BENE-MBS                        GBA: 399   p. 36
!   Western hemlock/Oregon grape (Mt Baker/Snoq)         R6 E TP-028-91
!                                                        & Devlin memo
   'CHS130  ','PSME  ',1101.,'DF  ', 122.,   1,   1,  16, &
!--------------------------------------------------------------------
!   86 = TSHE/GASH-BENE                       GBA: 348   p. 42
!   Western hemlock/salal-Oregon grape                   R6 E TP-028-91
!
   'CHS135  ','PSME  ',1225.,'DF  ', 117.,   1,   1,  16, &
!--------------------------------------------------------------------
!   87 = TSHE/GASH-VAME                       GBA: 303   p. 44
!   Western hemlock/salal-big huckleberry                R6 E TP-028-91
!
   'CHS140  ','PSME  ', 890.,'DF  ',  89.,   1,   1,  16, &
!----------------------------------------------------------------------
!   88 = TSHE/BENE-CHME                       GBA: 273   p. 38
!   Western hemlock/Oregon grape-little prince's pine    R6 E TP-028-91
!
   'CHS141  ','PSME  ',1070.,'DF  ', 103.,   1,   1,  16, &
!---------------------------------------------------------------------
!   89 = TSHE/ACCI/ACTR                       GBA: 238   p. 56
!   Western hemlock/vine maple/vanilla leaf              R6 E 232A-86
!
   'CHS223  ','PSME  ', 880.,'DF  ', 141.,   1,   1,  16, &
!----------------------------------------------------------------------
!   90 = TSHE/CONU/ACTR                       GBA: 420   p. 100
!   Western hemlock/dogwood/vanilla leaf                 R6 E 230A-86
!
   'CHS224  ','PSME  ',1159.,'DF  ', 142.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=91,105) / &
!---------------------------------------------------------------------
!   91 = TSHE/ACCI-BENE                       GBA: 478   p. 34
!   Western hemlock/vine maple-Oregon grape              R6 E TP-028-91
!
   'CHS251  ','PSME  ', 955.,'DF  ', 136.,   1,   1,  16, &
!---------------------------------------------------------------------
!   92 = TSHE/RHMA/XETE-MTH                   GBA: 165   p. 83
!   Western hemlock/rhododendron/beargrass (Mt Hood)     R6 E 232A-86
!
   'CHS325  ','PSME  ', 845.,'DF  ',  97.,   1,   1,  16, &
!---------------------------------------------------------------------
!   93 = TSHE/RHMA-VAAL/COCA                  GBA: 229   p. 81
!   W hemlock/rhododendron-AK huckleberry/dogwood bunchb R6 E 232A-86
!
   'CHS326  ','PSME  ', 885.,'DF  ', 130.,   1,   1,  16, &
!-------------------------------------------------------------------
!   94 = TSHE/RHMA-GASH-MTH                   GBA: 299   p. 79
!   Western hemlock/rhododendron-salal (Mt Hood)         R6 E 232A-86
!
   'CHS327  ','TSHE  ', 700.,'WH  ',  77.,   1,   1,  19, &
!------------------------------------------------------------------
!   95 = TSHE/RHMA-BENE-MTH                   GBA: 388   p. 77
!   W hemlock/rhododendron-dwarf Oregon grape (Mt Hood)  R6 E 232A-86
!
   'CHS328  ','TSHE  ', 835.,'WH  ',  82.,   1,   1,  19, &
!----------------------------------------------------------------
!   96 = TSHE/RHMA-GASH-WILL                  GBA: 338   p. 222
!   Western hemlock/rhododendron-salal (Willamette)      R6 E 257-86
!
   'CHS351  ','PSME  ', 890.,'DF  ', 128.,   1,   1,  16, &
!--------------------------------------------------------------------
!   97 = TSHE/RHMA-BENE-WILL                  GBA: 367   p. 214
!    W hemlock/rhododendron-dwarf OR grape (Willamette)  R6 E 257-86
!
   'CHS352  ','PSME  ', 930.,'DF  ', 136.,   1,   1,  16, &
!---------------------------------------------------------------------
!   98 = TSHE/RHMA/XETE-WILL                  GBA: 298   p. 210
!   Western hemlock/rhododendron/beargrass (Willamette)  R6 E 257-86
!
   'CHS353  ','PSME  ', 970.,'DF  ', 122.,   1,   1,  16, &
!--------------------------------------------------------------------
!   99 = TSHE/RHMA/OXOR                       GBA: 495   p. 218
!   Western hemlock/rhododendron/Oregon oxalis           R6 E 257-86
!
   'CHS354  ','PSME  ', 670.,'DF  ', 135.,   1,   1,  16, &
!--------------------------------------------------------------------
!  100 = TSHE/RHMA/LIBO2                      GBA: 447   p. 226
!  Western hemlock/rhododendron/twinflower               R6 E 257-86
!
   'CHS355  ','PSME  ',1100.,'DF  ', 130.,   1,   1,  16, &
!--------------------------------------------------------------------
!  101 = TSHE/OPHO-WILL                       GBA: 413   p. 182
!  Western  hemlock/devil's club (Willamette)            R6 E 257-86
!
   'CHS511  ','PSME  ', 685.,'DF  ', 168.,   1,   1,  16, &
!---------------------------------------------------------------------
!  102 = TSHE/OPHO-ATFI                       GBA: 276   p. 50
!  Western  hemlock/devil's club-ladyfern                R6 E TP-028-91
!
   'CHS513  ','TSHE  ', 980.,'WH  ', 101.,   1,   1,  19, &
!-----------------------------------------------------------------------
!  103 = TSHE/OPHO/OXOR                       GBA: 288   p. 69
!  Western  hemlock/devil's club/Oregon oxalis           R6 E 232A-86
!
   'CHS522  ','TSHE  ', 815.,'WH  ',  93.,   1,   1,  19, &
!-----------------------------------------------------------------------
!  104 = TSHE/OPHO/SMST                       GBA: 212   p. 71
!  Western hemlock/devil's club/starry solomonseal       R6 E 232A-86
!
   'CHS523  ','PSME  ', 585.,'DF  ', 156.,   1,   1,  16, &
!----------------------------------------------------------------------
!  105 = TSHE/OPHO/POMU                       GBA: 579   p. 74
!  Western hemlock/devil's club/swordfern                R6 E 230A-86
!
   'CHS524  ','TSHE  ', 965.,'WH  ',  88.,   1,   1,  19/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=106,120) / &
!-----------------------------------------------------------------------
!  106 = TSHE/VAAL-OPHO                       GBA: 278   p. 90
!  Western hemlock/Alaska huckleberry-devil's club       R6 E 232A-86
!
   'CHS611  ','PSME  ', 767.,'DF  ', 165.,   1,   1,  16, &
!-----------------------------------------------------------------------
!  107 = TSHE/VAME/XETE                       GBA: 175   p. 93
!  Western hemlock/big huckleberry/beargrass             R6 E 232A-86
!
   'CHS612  ','PSME  ', 795.,'DF  ',  90.,   1,   1,  16, &
!---------------------------------------------------------------------
!  108 = TSHE/VAAL/OXOR                       GBA: 444   p. 78
!  Western hemlock/Alaska huckleberry/Oregon oxalis      R6 E 230A-86
!
   'CHS613  ','TSHE  ', 985.,'WH  ',  84.,   1,   1,  19, &
!----------------------------------------------------------------------
!  109 = TSHE/VAAL-GASH                       GBA: 295   p. 88
!  Western hemlock/Alaska huckleberry-salal              R6 E 230A-86
!
   'CHS614  ','TSHE  ', 710.,'WH  ',  81.,   1,   1,  19, &
!---------------------------------------------------------------------
!  110 = TSHE/VAAL/COCA                       GBA: 375   p. 86
!  Western hemlock/Alaska huckleberry/dogwood bunchberry R6 E 230A-86
!
   'CHS615  ','TSHE  ', 770.,'WH  ',  87.,   1,   1,  19, &
!---------------------------------------------------------------------
!  111 = TSHE/VAAL-POMU                       GBA: 842   p. 64
!  Western hemlock/Alaska huckleberry-swordfern          R6 E TP-028-91
!
   'CHS625  ','PSME  ',1050.,'DF  ', 154.,   1,   1,  16, &
!---------------------------------------------------------------------
!  112 = TSHE/VAAL-BENE                       GBA: 277   p. 62
!  Western hemlock/Alaska huckleberry-Oregon grape       R6 E TP-028-91
!
   'CHS626  ','PSME  ', 940.,'DF  ', 110.,   1,   1,  16, &
!--------------------------------------------------------------------
!  113 = TSME/TIUN-STRO                       GBA: 174   p. 162
!  M hemlock/foamflower-rosy twistedstalk                R6 E TP-028-91
!
   'CMF250  ','TSME  ', 820.,'MH  ',  36.,   1,   1,  20, &
!---------------------------------------------------------------------
!  114 = TSME/CABI                            GBA: 134   p. 150
!  Mountain hemlock/marshmarigold                        R6 E TP-028-91
!
   'CMF251  ','TSME  ', 795.,'MH  ',  14.,   1,   1,  20, &
!----------------------------------------------------------------------
!  115 = TSME/VASC                            GBA: 195   p. 73
!  Mountain hemlock/grouse huckleberry                   R6 E 08-95
!
   'CMS114  ','TSME  ', 925.,'MH  ',  16.,   1,   1,  20, &
!---------------------------------------------------------------------
!  116 = TSME/VAME-GP                         GBA: 221   p. 68
!  Mountain hemlock/big huckleberry (Gifford Pinchot)    R6 E 130-83
!
   'CMS210  ','TSME  ', 970.,'MH  ',  25.,   1,   1,  20, &
!-------------------------------------------------------------------
!  117 = TSME/VAME/XETE                       GBA: 278   p. 67
!  Mountain hemlock/big huckleberry/beargrass            R6 E 08-95
!
   'CMS216  ','TSME  ', 880.,'MH  ',  19.,   1,   1,  20, &
!---------------------------------------------------------------------
!  118 = TSME/VAME/CLUN                       GBA: 303   p. 61
!  Mountain hemlock/big huckleberry/queen's cup          R6 E 08-95
!
   'CMS218  ','TSME  ', 955.,'MH  ',  20.,   1,   1,  20, &
!---------------------------------------------------------------------
!  119 = TSME/MEFE                            GBA: 215   p. 39
!  Mountain hemlock/fool's huckleberry                   R6 E 08-95
!
   'CMS221  ','TSME  ',1005.,'MH  ',  22.,   1,   1,  20, &
!------------------------------------------------------------------
!  120 = TSME/RHAL                            GBA: 235   p. 51
!  Mountain hemlock/Cascades azalea                      R6 E 08-95
!
   'CMS223  ','TSME  ', 955.,'MH  ',  21.,   1,   1,  20/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=121,135) / &
!-------------------------------------------------------------------
!  121 = TSME/VAAL                            GBA: 185   p. 164
!  Mountain hemlock/Alaska huckleberry                   R6 E TP-028-91
!
   'CMS241  ','TSME  ',1015.,'MH  ',  34.,   1,   1,  20, &
!------------------------------------------------------------------
!  122 = TSME/VAME-VAAL                       GBA: 399   p. 178
!  Mountain hemlock/big huckleberry-Alaska huckleberry   R6 E TP-028-91
!
   'CMS244  ','TSME  ', 995.,'MH  ',  29.,   1,   1,  20, &
!-------------------------------------------------------------------
!  123 = TSME/VAME/XETE-WASH                  GBA: 308   p. 182
!  Mountain hemlock/big huckleberry/beargrass (MB/SQ)    R6 E TP-028-91
!
   'CMS245  ','TSME  ', 935.,'MH  ',  25.,   1,   1,  20, &
!-------------------------------------------------------------------
!  124 = TSME/VAME-MBS                        GBA: 363   p. 172
!  Mountain hemlock/big huckleberry (Mt Baker/Snoqual)   R6 E TP-028-91
!
   'CMS246  ','TSME  ',1075.,'MH  ',  25.,   1,   1,  20, &
!-------------------------------------------------------------------
!  125 = TSME/VAME-STRO                       GBA: 512   p. 176
!  Mountain hemlock/big huckleberry-rosy twistedstalk    R6 E TP-028-91
!
   'CMS250  ','TSME  ', 780.,'MH  ',  31.,   1,   1,  20, &
!-------------------------------------------------------------------
!  126 = TSME/VAME-VASI                       GBA: 238   p. 180
!  Mountain hemlock/big huckleberry-Sitka valerian       R6 E TP-028-91
!
   'CMS251  ','TSME  ', 770.,'MH  ',  25.,   1,   1,  20, &
!-------------------------------------------------------------------
!  127 = TSME/VAAL-STRO                       GBA: 402   p. 170
!  Mountain hemlock/Alaska huckleberry-rosy twistedstalk R6 E TP-028-91
!
   'CMS252  ','TSME  ', 960.,'MH  ',  35.,   1,   1,  20, &
!--------------------------------------------------------------------
!  128 = TSME/VAAL-CLUN                       GBA: 191   p. 166
!  Mountain hemlock/Alaska huckleberry-queen's cup       R6 E TP-028-91
!
   'CMS253  ','TSME  ',1090.,'MH  ',  29.,   1,   1,  20, &
!--------------------------------------------------------------------
!  129 = TSME/VAME-RULA                       GBA: 310   p. 174
!  Mountain hemlock/big huckleberry-trailing bramble     R6 E TP-028-91
!
   'CMS254  ','TSME  ',1155.,'MH  ',  28.,   1,   1,  20, &
!----------------------------------------------------------------------
!  130 = TSME/VAAL-MADI2                      GBA: 208   p. 168
!  M hemlock/Alaska huckleberry-false lily-of-the-valley R6 E TP-028-91
!
   'CMS255  ','TSME  ', 710.,'MH  ',  29.,   1,   1,  20, &
!---------------------------------------------------------------------
!  131 = TSME/PHEM-VADE                       GBA: 291   p. 156
!  M hemlock/red heather-blueleaf huckleberry            R6 E TP-028-91
!
   'CMS350  ','TSME  ', 750.,'MH  ',  20.,   1,   1,  20, &
!---------------------------------------------------------------------
!  132 = TSME/RHAL-VAAL                       GBA: 300   p. 158
!  M hemlock/white rhododendron-Alaska huckleberry       R6 E TP-028-91
!
   'CMS351  ','TSME  ', 820.,'MH  ',  23.,   1,   1,  20, &
!---------------------------------------------------------------------
!  133 = TSME/RHAL-VAME                       GBA: 210   p. 160
!  Mountain hemlock/white rhododendron-big huckleberry   R6 E TP-028-91
!
   'CMS352  ','TSME  ', 970.,'MH  ',  23.,   1,   1,  20, &
!-----------------------------------------------------------------------
!  134 = TSME/CLPY-RUPE                       GBA: 281   p. 152
!  Mountain hemlock/copperbush-five leaved bramble       R6 E TP-028-91
!
   'CMS353  ','TSME  ', 675.,'MH  ',  20.,   1,   1,  20, &
!----------------------------------------------------------------------
!  135 = TSME/OPHO-VAAL                       GBA: 291   p. 154
!  Mountain hemlock/devil's club-Alaska huckleberry      R6 E TP-028-91
!
   'CMS450  ','ABAM  ', 855.,'SF  ', 138.,   1,   1,   1/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=136,139) / &
!----------------------------------------------------------------------
!  136 = TSME/RHMA                            GBA: 249   p. 57
!  Mountain hemlock/rhododendron                         R6 E TP-08-95
!
   'CMS612  ','ABAM  ',1010.,'SF  ',  78.,   1,   1,   1, &
!---------------------------------------------------------------------
!  137 = ABGR/CHUM                            GBA: 475   p. 96
!  Grand fir/prince's pine                               R6 E 257-86
!
   'CWF211  ','PSME  ', 730.,'DF  ', 132.,   1,   1,  16, &
!---------------------------------------------------------------------
!  138 = ABGR/ARUV                            GBA: 213   p. 90
!  Grand fir/bearberry                                   R6 E 257-86
!
   'CWS521  ','PSME  ', 820.,'DF  ',  86.,   1,   1,  16, &
!---------------------------------------------------------------------
!  139 = ABGR/BENE                            GBA: 370   p. 92
!  Grand fir/dwarf Oregon grape                          R6 E 257-86
!
   'CWS522  ','PSME  ', 860.,'DF  ', 131.,   1,   1,  16/
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

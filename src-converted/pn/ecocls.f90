SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
IMPLICIT NONE
!----------
! PN $Id$
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
!  AN R6 ALL CVS DATA STUDY, JUNE 2008: 1-3,7-9,11,12,14-18,20-23,
!  25-31,34-39,41-47,49-66,68-75
!----------
INTEGER NENTRY
PARAMETER (NENTRY=75)
CHARACTER*4 SPC(NENTRY),ASPEC
CHARACTER*6 SCIEN(NENTRY)
CHARACTER*8 APASS,PA(NENTRY)
INTEGER FVSSEQ(NENTRY)
REAL SITE(NENTRY),SDIMX(NENTRY),RSI,RSDI
INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
INTEGER ISEQ,INDEX,NUM,ISFLAG,I,K
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=1,15) / &
!-----------------------------------------------------------------------
!      ALPHA     SCIEN          ALPHA       NUM  SITE FVS  PLANT
!       ECO      SITE      MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
!      CLASS     SPEC      SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
!-----------------------------------------------------------------------
!    1 = PSME/HODI-ROGY                       GBA: 265   p. 100
!    Douglas-fir/oceanspray-baldhip rose                 R6 E TP-001-88
!
   'CDS221  ','PSME  ', 750.,'DF  ',  54.,   1,   1,  16, &
!-----------------------------------------------------------------------
!    2 = PSME/GASH                            GBA: 221   p. 104
!    Douglas-fir/salal                                   R6 E TP-001-88
!
   'CDS255  ','PSME  ', 955.,'DF  ',  62.,   1,   1,  16, &
!----------------------------------------------------------------------
!    3 = PSME/ARUV                            GBA: 116   p. 96
!    Douglas-fir/kinnikinnick                            R6 E TP-001-88
!
   'CDS651  ','PSME  ', 600.,'DF  ',  33.,   1,   1,  16, &
!----------------------------------------------------------------------
!    4 = ABLA2/LULA                           GBA: 133   p. 268
!    Subalpine fir/subalpine lupine                      R6 E TP-001-88
!
   'CEF321  ','ABLA2 ', 367.,'AF  ',  50.,   1,   1,   4, &
!----------------------------------------------------------------------
!    5 = ABLA2/RHAL-OLY                       GBA: 194   p. 272
!    Subalpine fir/white rhododendron (Olympic)          R6 E TP-001-88
!
   'CES212  ','ABLA2 ', 535.,'AF  ',  65.,   1,   1,   4, &
!-----------------------------------------------------------------------
!    6 = ABLA2/VAME-OLY                       GBA: 346   p. 260
!    Subalpine fir/big huckleberry (Olympic)             R6 E TP-001-88
!
   'CES321  ','ABLA2 ', 955.,'AF  ',  91.,   1,   1,   4, &
!-----------------------------------------------------------------------
!    7 = ABLA2/JUCO4                          GBA: 167   p. 264
!    Subalpine fir/common juniper                        R6 E TP-001-88
!
   'CES621  ','ABLA2 ', 560.,'AF  ',  31.,   1,   1,   4, &
!-----------------------------------------------------------------------
!    8 = ABAM/OXOR-OLY                        GBA: 565   p. 202
!    Silver fir/oxalis (Olympic)                         R6 E TP-001-88
!
   'CFF111  ','ABAM  ',1050.,'SF  ', 150.,   1,   1,   1, &
!-----------------------------------------------------------------------
!    9 = ABAM/ACTR-TIUN                       GBA: 708   p. 238
!    Silver fir/vanillaleaf-foamflower                   R6 E TP-001-88
!
   'CFF211  ','PSME  ', 950.,'DF  ',  84.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   10 = ABAM/XETE                            GBA: 396   p. 186
!    Silver fir/beargrass                                R6 E TP-001-88
!
   'CFF311  ','ABAM  ',1093.,'SF  ',  83.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   11 = ABAM/POMU                            GBA: 660   p. 230
!    Silver fir/swordfern                                R6 E TP-001-88
!
   'CFF611  ','ABAM  ', 995.,'SF  ', 145.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   12 = ABAM/POMU-OXOR                       GBA: 383   p. 234
!    Silver fir/swordfern-oxalis                         R6 E TP-001-88
!
   'CFF612  ','ABAM  ', 845.,'SF  ', 154.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   13 = ABAM/Dep.                            GBA: 312   p. 194
!    Silver fir/depauperate                              R6 E TP-001-88
!
   'CFF911  ','PSME  ', 861.,'DF  ',  84.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   14 = ABAM/GASH/OXOR                       GBA: 577   p. 222
!    Silver fir/salal/oxalis                             R6 E TP-001-88
!
   'CFS156  ','ABAM  ',1015.,'SF  ', 149.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   15 = ABAM/VAME/XETE-OLY                   GBA: 308   p. 190
!    Silver fir/big huckleberry/beargrass (Olympic)      R6 E TP-001-88
!
   'CFS211  ','ABAM  ',1050.,'SF  ',  83.,   1,   1,   1/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=16,30) / &
!-----------------------------------------------------------------------
!   16 = ABAM/VAAL-OLY                        GBA: 528   p. 152
!    Silver fir/Alaska huckleberry (Olympic)             R6 E TP-001-88
!
   'CFS212  ','ABAM  ',1090.,'SF  ', 127.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   17 = ABAM/VAAL/ERMO                       GBA: 560   p. 156
!    Silver fir/Alaska huckleberry/avalanche lily        R6 E TP-001-88
!
   'CFS213  ','ABAM  ', 835.,'SF  ', 108.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   18 = ABAM/VAAL/XETE                       GBA: 442   p. 160
!    Silver fir/Alaska huckleberry/beargrass             R6 E TP-001-88
!
   'CFS214  ','PSME  ',1090.,'DF  ',  84.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   19 = ABAM/VAAL/TIUN                       GBA: 399   p. 164
!    Silver fir/Alaska huckleberry/foamflower            R6 E TP-001-88
!
   'CFS215  ','ABAM  ',1101.,'SF  ', 101.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   20 = ABAM/VAAL/OXOR                       GBA: 672   p. 172
!    Silver fir/Alaska huckleberry/oxalis                R6 E TP-001-88
!
   'CFS217  ','ABAM  ',1055.,'SF  ', 136.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   21 = ABAM/VAAL/CLUN                       GBA: 364   p. 176
!    Silver fir/Alaska huckleberry/queen's cup           R6 E TP-001-88
!
   'CFS218  ','ABAM  ',1080.,'SF  ', 111.,   1,   1,   1, &
!-----------------------------------------------------------------------
!   22 = ABAM/VAAL/LIBO2                      GBA: 380   p. 180
!    Silver fir/Alaska huckleberry/twinflower            R6 E TP-001-88
!
   'CFS219  ','ABAM  ', 955.,'SF  ', 115.,   1,   1,   1, &
!----------------------------------------------------------------------
!   23 = ABAM/OPHO-OLY                        GBA: 471   p. 198
!    Silver fir/devil's club (Olympic)                   R6 E TP-001-88
!
   'CFS311  ','ABAM  ', 920.,'SF  ', 118.,   1,   1,   1, &
!----------------------------------------------------------------------
!   24 = ABAM/RHMA-OLY                        GBA: 361   p. 206
!    Silver fir/rhododendron  (Olympic)                  R6 E TP-001-88
!
   'CFS611  ','ABAM  ', 996.,'SF  ', 107.,   1,   1,   1, &
!----------------------------------------------------------------------
!   25 = ABAM/RHMA-VAAL                       GBA: 356   p. 210
!    Silver fir/rhododendron-Alaska huckleberry          R6 E TP-001-88
!
   'CFS612  ','ABAM  ', 470.,'SF  ',  96.,   1,   1,   1, &
!---------------------------------------------------------------------
!   26 = TSHE/OXOR-OLY                        GBA: 793   p. 326
!    Western hemlock/oxalis (Olympic)                    R6 E TP-001-88
!
   'CHF112  ','TSHE  ', 780.,'WH  ', 104.,   1,   1,  19, &
!---------------------------------------------------------------------
!   27 = TSHE/OXOR-COAST                      GBA: 387   p. 83
!    Western hemlock/Oregon oxalis (Coast)               R6 E 220-86a
!
   'CHF121  ','TSHE  ', 960.,'WH  ', 110.,   1,   1,  19, &
!---------------------------------------------------------------------
!   28 = TSHE/POMU-COAST                      GBA: 674   p. 101
!    Western hemlock/swordfern (Coast)                   R6 E 220-86a
!
   'CHF122  ','TSHE  ', 925.,'WH  ', 114.,   1,   1,  19, &
!---------------------------------------------------------------------
!   29 = TSHE/POMU-OXOR-OLY                   GBA: 400   p. 386
!    Western hemlock/swordfern-oxalis (Olympic)          R6 E TP-001-88
!
   'CHF131  ','TSHE  ', 950.,'WH  ',  94.,   1,   1,  19, &
!---------------------------------------------------------------------
!   30 = TSHE/POMU-TITR                       GBA: 462   p. 382
!    Western hemlock/swordfern-foamflower                R6 E TP-001-88
!
   'CHF132  ','PSME  ',1010.,'DF  ', 116.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=31,45) / &
!---------------------------------------------------------------------
!   31 = TSHE/ACTR-OLY                        GBA: 492   p. 390
!    Western hemlock/vanillaleaf (Olympic)               R6 E TP-001-88
!
   'CHF211  ','PSME  ',1040.,'DF  ', 108.,   1,   1,  16, &
!---------------------------------------------------------------------
!   32 = TSHE/XETE-OLY                        GBA: 252   p. 306
!    Western hemlock/beargrass (Olympic)                 R6 E TP-001-88
!
   'CHF511  ','TSHE  ', 696.,'WH  ',  50.,   1,   1,  19, &
!---------------------------------------------------------------------
!   33 = TSHE/Dep.                            GBA: 422   p. 310
!    Western hemlock/depauperate                         R6 E TP-001-88
!
   'CHF911  ','TSHE  ',1165.,'WH  ', 105.,   1,   1,  19, &
!----------------------------------------------------------------------
!   34 = TSHE/LYAM-OLY                        GBA: 201   p. 378
!    Western hemlock/skunkcabbage  (Olympic)             R6 E TP-001-88
!
   'CHM111  ','ALRU  ', 760.,'RA  ',  52.,   1,   1,  22, &
!----------------------------------------------------------------------
!   35 = TSHE/BENE-COAST                      GBA: 538   p. 77
!    Western hemlock/dwarf Oregon grape  (Coast)         R6 E 220-86a
!
   'CHS121  ','PSME  ', 985.,'DF  ', 118.,   1,   1,  16, &
!----------------------------------------------------------------------
!   36 = TSHE/BENE-GASH-COAST                 GBA: 345   p. 79
!    Western hemlock/dwarf Oregon grape-salal (Coast)    R6 E 220-86a
!
   'CHS122  ','TSHE  ', 820.,'WH  ', 114.,   1,   1,  19, &
!----------------------------------------------------------------------
!   37 = TSHE/GASH-COAST                      GBA: 378   p. 93
!    Western hemlock/salal (Coast)                       R6 E 220-86a
!
   'CHS123  ','TSHE  ',1210.,'WH  ', 112.,   1,   1,  19, &
!----------------------------------------------------------------------
!   38 = TSHE/GASH-OLY                        GBA: 312   p. 350
!    Western hemlock/salal (Olympic)                     R6 E TP-001-88
!
   'CHS131  ','TSHE  ',1050.,'WH  ',  78.,   1,   1,  19, &
!---------------------------------------------------------------------
!   39 = TSHE/GASH/XETE                       GBA: 245   p. 354
!    Western hemlock/salal/beargrass                     R6 E TP-001-88
!
   'CHS132  ','PSME  ', 880.,'DF  ',  67.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   40 = TSHE/GASH-VAOV2                      GBA: 582   p. 358
!    Western hemlock/salal-evergreen huckleberry         R6 E TP-001-88
!
   'CHS133  ','PSME  ',1606.,'DF  ',  98.,   1,   1,  16, &
!-----------------------------------------------------------------------
!   41 = TSHE/GASH-HODI                       GBA: 232   p. 362
!    Western hemlock/salal-oceanspray                    R6 E TP-001-88
!
   'CHS134  ','PSME  ', 810.,'DF  ',  78.,   1,   1,  16, &
!----------------------------------------------------------------------
!   42 = TSHE/GASH/OXOR                       GBA: 500   p. 370
!    Western hemlock/salal/oxalis                        R6 E TP-001-88
!
   'CHS136  ','TSHE  ', 895.,'WH  ',  84.,   1,   1,  19, &
!---------------------------------------------------------------------
!   43 = TSHE/GASH/POMU                       GBA: 434   p. 374
!    Western hemlock/salal/swordfern                     R6 E TP-001-88
!
   'CHS137  ','PSME  ', 975.,'DF  ', 108.,   1,   1,  16, &
!----------------------------------------------------------------------
!   44 = TSHE/BENE-OLY                        GBA: 322   p. 318
!    Western hemlock/Oregongrape  (Olympic)              R6 E TP-001-88
!
   'CHS138  ','PSME  ',1095.,'DF  ',  71.,   1,   1,  16, &
!----------------------------------------------------------------------
!   45 = TSHE/BENE/POMU-OLY                   GBA: 454   p. 322
!    Western hemlock/Oregongrape/swordfern (Olympic)     R6 E TP-001-99
!
   'CHS139  ','PSME  ', 955.,'DF  ', 119.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=46,60) / &
!----------------------------------------------------------------------
!   46 = TSHE/ACCI-GASH-COAST                 GBA: 452   p. 103
!    Western hemlock/vine maple-salal (Coast)            R6 E 220-86a
!
   'CHS221  ','PSME  ', 825.,'DF  ', 122.,   1,   1,  16, &
!----------------------------------------------------------------------
!   47 = TSHE/ACCI/POMU-COAST                 GBA: 412   p. 105
!    Western hemlock/vine maple/swordfern (Coast)        R6 E 220-86a
!
   'CHS222  ','PSME  ', 875.,'DF  ', 128.,   1,   1,  16, &
!----------------------------------------------------------------------
!   48 = TSHE/RHMA-BENE-COAST                 GBA: 401   p. 85
!    Western hemlock/rhododendron-dwarf OR grape (Coast) R6 E 220-86a
!
   'CHS321  ','TSHE  ',1107.,'WH  ',  98.,   1,   1,  19, &
!----------------------------------------------------------------------
!   49 = TSHE/RHMA-GASH-COAST                 GBA: 429   p. 89
!    Western hemlock/rhododendron-salal                  R6 E 220-86a
!
   'CHS322  ','PSME  ', 840.,'DF  ', 114.,   1,   1,  16, &
!----------------------------------------------------------------------
!   50 = TSHE/RHMA/POMU-COAST                 GBA: 995   p. 91
!    Western hemlock/rhododendron/swordfern (Coast)      R6 E 220-86a
!
   'CHS323  ','TSHE  ', 945.,'WH  ',  84.,   1,   1,  19, &
!----------------------------------------------------------------------
!   51 = TSHE/RHMA/VAOV2-COAST                GBA: 518   p. 87
!    W.hemlock/rhododendron-evergreen huckleberry(Coast) R6 E 220-86a
!
   'CHS324  ','TSHE  ', 865.,'WH  ',  80.,   1,   1,  19, &
!----------------------------------------------------------------------
!   52 = TSHE/RHMA-OLY                        GBA: 384   p. 330
!    Western hemlock/rhododendron (Olympic)              R6 E TP-001-88
!
   'CHS331  ','TSHE  ',1145.,'WH  ',  56.,   1,   1,  19, &
!----------------------------------------------------------------------
!   53 = TSHE/RHMA/XETE-OLY                   GBA: 228   p. 334
!    Western hemlock/rhododendron/beargrass (Olympic)    R6 E TP-001-88
!
   'CHS332  ','PSME  ', 610.,'DF  ',  56.,   1,   1,  16, &
!----------------------------------------------------------------------
!   54 = TSHE/RHMA-BENE-OLY                   GBA: 152   p. 338
!    Western hemlock/rhododendron-Oregongrape (Olympic)  R6 E TP-001-88
!
   'CHS333  ','PSME  ',1065.,'DF  ',  80.,   1,   1,  16, &
!----------------------------------------------------------------------
!   55 = TSHE/RHMA-GASH-OLY                   GBA: 257   p. 342
!    Western hemlock/rhododendron-salal (Olympic)        R6 E TP-001-88
!
   'CHS334  ','PSME  ', 810.,'DF  ',  66.,   1,   1,  16, &
!----------------------------------------------------------------------
!   56 = TSHE/RHMA/POMU-OLY                   GBA: 286   p. 346
!    Western hemlock/rhododendron/swordfern (Olympic)    R6 E TP-001-88
!
   'CHS335  ','PSME  ', 845.,'DF  ',  88.,   1,   1,  16, &
!---------------------------------------------------------------------
!   57 = TSHE/RUSP-COAST                      GBA: 463   p. 95
!    Western hemlock/salmonberry  (Coast)                R6 E 220-86a
!
   'CHS421  ','TSHE  ', 675.,'WH  ', 110.,   1,   1,  19, &
!---------------------------------------------------------------------
!   58 = TSHE/RUSP-ACCI-COAST                 GBA: 999   p. 99
!    Western hemlock/salmonberry-vine maple (Coast)      R6 E 220-86a
!
   'CHS422  ','TSHE  ', 660.,'WH  ',  94.,   1,   1,  19, &
!---------------------------------------------------------------------
!   59 = TSHE/RUSP-GASH-COAST                 GBA: 341   p. 97
!    Western hemlock/salmonberry-salal (Coast)           R6 E 220-86a
!
   'CHS423  ','PSME  ', 600.,'DF  ', 119.,   1,   1,  16, &
!--------------------------------------------------------------------
!   60 = TSHE/OPHO-OLY                        GBA:1044   p. 314
!    Western hemlock/devil's club (Olympic)              R6 E TP-001-88
!
   'CHS512  ','PSME  ', 485.,'DF  ', 134.,   1,   1,  16/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=61,75) / &
!-------------------------------------------------------------------
!   61 = TSHE/OPHO-COAST                      GBA: 379   p. 75
!    Western hemlock/devil's club (Coast)                R6 E 220-86a
!
   'CHS521  ','TSHE  ', 375.,'WH  ', 114.,   1,   1,  19, &
!-----------------------------------------------------------------------
!   62 = TSHE/VAOV2-COAST                     GBA: 510   p. 81
!    Western hemlock/evergreen huckleberry (Coast)       R6 E 220-86a
!
   'CHS610  ','TSHE  ', 935.,'WH  ', 118.,   1,   1,  19, &
!-----------------------------------------------------------------------
!   63 = TSHE/VAAL                            GBA: 389   p. 290
!    Western hemlock/Alaska huckleberry                  R6 E TP-001-88
!
   'CHS621  ','TSHE  ',1025.,'WH  ',  98.,   1,   1,  19, &
!----------------------------------------------------------------------
!   64 = TSHE/VAAL/XETE                       GBA: 304   p. 294
!    Western hemlock/Alaska huckleberry/beargrass        R6 E TP-001-88
!
   'CHS622  ','PSME  ', 610.,'DF  ',  70.,   1,   1,  16, &
!----------------------------------------------------------------------
!   65 = TSHE/VAAL/OXOR-OLY                   GBA: 522   p. 298
!    Western hemlock/Alaska huckleberry/oxalis (Olympic) R6 E TP-001-88
!
   'CHS623  ','TSHE  ', 570.,'WH  ',  94.,   1,   1,  19, &
!---------------------------------------------------------------------
!   66 = TSHE/VAAL-GASH-OLY                   GBA: 548   p. 302
!    Western hemlock/Alaska huckleberry-salal (Olympic)  R6 E TP-001-88
!
   'CHS624  ','TSHE  ', 915.,'WH  ',  92.,   1,   1,  19, &
!---------------------------------------------------------------------
!   67 = TSME/VAAL/ERMO                       GBA: 370   p. 120
!    Mountain hemlock/Alaska huckleberry/avalanche lily  R6 E TP-001-88
!
   'CMS242  ','TSME  ',1021.,'MH  ',  14.,   1,   1,  20, &
!---------------------------------------------------------------------
!   68 = PISI/POMU-OXOR                       GBA:2040   p. 248
!    Sitka spruce/swordfern-oxalis                       R6 E TP-001-88
!
   'CSF111  ','PISI  ', 930.,'SS  ', 120.,   1,   1,   6, &
!----------------------------------------------------------------------
!   69 = PISI/POMU-COAST                      GBA: 913   p. 73
!    Sitka spruce/swordfern (Coast)                      R6 E 220-86a
!
   'CSF121  ','PISI  ', 930.,'SS  ', 115.,   1,   1,   6, &
!----------------------------------------------------------------------
!   70 = PISI/OXOR-COAST                      GBA: 875   p. 65
!    Sitka spruce/Oregon oxalis                          R6 E 220-86a
!
   'CSF321  ','PISI  ', 930.,'SS  ', 120.,   1,   1,   6, &
!---------------------------------------------------------------------
!   71 = PISI/MEFE-VAPA-COAST                 GBA: 747   p. 63
!    Sitka spruce/fool's huckleberry-red huckleb (Coast) R6 E 220-86a
!
   'CSS221  ','PISI  ',1000.,'SS  ', 125.,   1,   1,   6, &
!---------------------------------------------------------------------
!   72 = PISI/GASH-COAST                      GBA:  484  p. 67
!    Sitka spruce/salal (Coast)                          R6 E 220-86a
!
   'CSS321  ','PISI  ', 615.,'SS  ', 117.,   1,   1,   6, &
!---------------------------------------------------------------------
!   73 = PISI/RUSP-COAST                      GBA: 567   p. 69
!    Sitka spruce/salmonberry (Coast)                    R6 E 220-86a
!
   'CSS521  ','PISI  ', 545.,'SS  ', 123.,   1,   1,   6, &
!---------------------------------------------------------------------
!   74 = PISI/RUSP-GASH-COAST                 GBA: 632   p. 71
!    Sitka spruce/salmonberry-salal (Coast)              R6 E 220-86a
!
   'CSS522  ','PISI  ', 535.,'SS  ', 111.,   1,   1,   6, &
!--------------------------------------------------------------------
!   75 = PISI/OPHO-COAST                      GBA: 666   p. 61
!    Sitka spruce/devil's club (Coast)                   R6 E 220-86a
!
   'CSS621  ','PISI  ',1000.,'SS  ', 121.,   1,   1,   6/
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

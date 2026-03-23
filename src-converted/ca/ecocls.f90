SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
IMPLICIT NONE
!----------
! CA $Id$
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
!  AN R6 ALL CVS DATA STUDY, JUNE 2008: 3,4,6-8,10,11,13,14,16-24,
!  26-28,30-32,36,38,39,41,43-48,50,53,56,58,59,61,62,65-90
!----------
INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
REAL RSI,RSDI
PARAMETER (NENTRY=90)
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
!    1 =  CDC411 = PSME-ABCO-PIJE             GBA:       p.
!    Douglas-fir-white fir-Jeffrey pine
!
   'CDC411  ','PSME  ', 899.,'DF  ',  85.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    2 =  CDC412 = PSME-ABCO-PIPO             GBA:       p.
!    Douglas-fir-white fir-ponderosa pine
!
   'CDC412  ','PSME  ',1155.,'DF  ',  87.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    3 =  CDC421 = PSME-ABCO                  GBA:       p.
!    Douglas-fir-white fir
!
   'CDC421  ','PSME  ', 720.,'DF  ',  72.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    4 =  CDC431 = PSME-ABCO/HODI             GBA:       p.
!    Douglas-fir-white fir/creambush oceanspray
!
   'CDC431  ','PSME  ', 765.,'DF  ',  96.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    5 =  CDC432 = PSME-ABCO/BENE             GBA:       p.
!    Douglas-fir-white fir/dwarf Oregongrape
!
   'CDC432  ','PSME  ',1193.,'DF  ',  93.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    6 =  CDC511 = PSME-PIPO                  GBA:       p.
!    Douglas-fir-ponderosa pine
!
   'CDC511  ','PSME  ', 735.,'DF  ', 101.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    7 =  CDC521 = PSME-PIJE                  GBA:       p.
!    Douglas-fir-Jeffrey pine
!
   'CDC521  ','PSME  ', 595.,'DF  ',  71.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    8 =  CDF911 = PSME/DEPAUPERATE           GBA:       p.
!    Douglas-fir/depauperate
!
   'CDF911  ','PSME  ', 670.,'DF  ',  70.,   1,   1,   7, &
!-----------------------------------------------------------------------
!    9 =  CDH111 = PSME-LIDE3/GASH            GBA:       p.
!    Douglas-fir-tanoak/salal
!
   'CDH111  ','PSME  ', 845.,'DF  ',  86.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   10 =  CDH112 = PSME/RHMA                  GBA:       p.
!    Douglas-fir/Pacific rhododendron
!
   'CDH112  ','PSME  ', 800.,'DF  ',  92.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   11 =  CDH121 = PSME-LIDE3-PILA            GBA:       p.
!    Douglas-fir-tanoak-sugar pine
!
   'CDH121  ','PSME  ', 720.,'DF  ',  97.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   12 =  CDH131 = PSME-LIDE3                 GBA:       p.
!    Douglas-fir-tanoak
!
   'CDH131  ','PSME  ',1098.,'DF  ',  81.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   13 =  CDH141 = PSME-LIDE3-QUCH            GBA:       p.
!    Douglas-fir-tanoak-canyon live oak
!
   'CDH141  ','PSME  ', 780.,'DF  ',  86.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   14 =  CDH142 = PSME-LIDE3/RHDI            GBA:       p.
!    Douglas-fir-tanoak/poison oak
!
   'CDH142  ','PSME  ',1050.,'DF  ',  82.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   15 =  CDH511 = PSME-QUSA                  GBA:       p.
!    Douglas-fir-Sadler oak
!
   'CDH511  ','PSME  ',1087.,'DF  ',  95.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=16,30) / &
!-----------------------------------------------------------------------
!   16 =  CDS111 = PSME/RHDI-BEPI             GBA:       p.
!    Douglas-fir/poison oak-Piper's Oregongrape
!
   'CDS111  ','PSME  ', 655.,'DF  ',  77.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   17 =  CDS112 = PSME/RHDI                  GBA:       p.
!    Douglas-fir/poison oak
!
   'CDS112  ','PSME  ', 630.,'DF  ',  67.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   18 =  CDS511 = PSME/BENE                  GBA:       p.
!    Douglas-fir/dwarf Oregongrape
!
   'CDS511  ','PSME  ', 635.,'DF  ',  93.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   19 =  CDS521 = PSME/BERE                  GBA:       p.
!    Douglas-fir/creeping Oregongrape
!
   'CDS521  ','PSME  ', 670.,'DF  ',  85.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   20 =  CHC111 = TSHE-CHLA                  GBA:       p.
!    Western hemlock-Port-Orford-cedar
!
   'CHC111  ','PSME  ',1215.,'DF  ', 117.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   21 =  CHC412 = TSHE-THPL/HIGH ELEV        GBA:       p.
!    Western hemlock-western redcedar/high elevation
!
   'CHC412  ','PSME  ', 945.,'DF  ', 108.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   22 =  CHC461 = TSHE-THPL                  GBA:       p.
!    Western hemlock-western redcedar
!
   'CHC461  ','PSME  ',1105.,'DF  ', 146.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   23 =  CHC611 = TSHE-ABCO                  GBA:       p.
!    Western hemlock-white fir
!
   'CHC611  ','PSME  ', 890.,'DF  ', 119.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   24 =  CHH111 = TSHE-UMCA                  GBA:       p.
!    Western hemlock-California laurel
!
   'CHH111  ','PSME  ', 650.,'DF  ', 106.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   25 =  CHH511 = TSHE-QUSA                  GBA:       p.
!    Western hemlock-Sadler oak
!
   'CHH511  ','PSME  ',1152.,'DF  ', 108.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   26 =  CHS131 = TSHE/GASH (SWO)            GBA:       p.
!    Western hemlock/salal
!
   'CHS131  ','PSME  ',1050.,'DF  ',  61.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   27 =  CHS331 = TSHE/RHMA (SWO)            GBA:       p.
!    Western hemlock/Pacific rhododendron
!
   'CHS331  ','PSME  ',1145.,'DF  ', 102.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   28 =  CMF211 = TSME/POPU                  GBA:       p.
!    Mountain hemlock/skunkleaf polemonium
!
   'CMF211  ','ABMAS ', 555.,'SH  ',  74.,   1,   1,   6, &
!-----------------------------------------------------------------------
!   29 =  CPC411 = PIPO-PSME                  GBA:       p.
!    Ponderosa pine-Douglas-fir
!
   'CPC411  ','PSME  ', 720.,'DF  ',  76.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   30 =  CPC511 = PIJE-PIMO                  GBA:       p.
!    Jeffrey pine-western white pine
!
   'CPC511  ','PIJE  ', 420.,'JP  ',  52.,   1,   1,  15/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=31,45) / &
!-----------------------------------------------------------------------
!   31 =  CPG141 = PIJE/FEID                  GBA:       p.
!    Jeffrey pine/Idaho fescue
!
   'CPG141  ','PIJE  ', 200.,'JP  ',  57.,   1,   1,  15, &
!-----------------------------------------------------------------------
!   32 =  CPH411 = PIJE-QUVA                  GBA:       p.
!    Jeffrey pine-huckleberry oak
!
   'CPH411  ','PIJE  ', 470.,'JP  ',  60.,   1,   1,  15, &
!-----------------------------------------------------------------------
!   33 =  CPS321 = PIJE/CEPU                  GBA:       p.
!    Jeffrey pine/dwarf ceanothus
!
   'CPS321  ','PIJE  ', 364.,'JP  ',  58.,   1,   1,  15, &
!-----------------------------------------------------------------------
!   34 =  CPS611 = PIJE/GRASS                 GBA:       p.
!    Jeffrey pine/grass
!
   'CPS611  ','PIJE  ', 340.,'JP  ',  57.,   1,   1,  15, &
!-----------------------------------------------------------------------
!   35 =  CQF111 = PIMO/XETE                  GBA:       p.
!    Western white pine/beargrass
!
   'CQF111  ','ABCO  ', 436.,'WF  ',  33.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   36 =  CRF211 = ABMAS/POPU                 GBA:       p.
!    Shasta red fir/skunkleaf polemonium
!
   'CRF211  ','ABMAS ', 675.,'SH  ',  57.,   1,   1,   6, &
!-----------------------------------------------------------------------
!   37 =  CRF311 = ABMAS/SHEEP                GBA:       p.
!    Shasta red fir/sheep    (grazing destroyed understory plants)
!
   'CRF311  ','ABMAS ', 319.,'SH  ',  50.,   1,   1,   6, &
!-----------------------------------------------------------------------
!   38 =  CRH111 = ABMAS-QUSA                 GBA:       p.
!    Shasta red fir-Sadler oak
!
   'CRH111  ','ABMAS ', 470.,'SH  ',  81.,   1,   1,   6, &
!-----------------------------------------------------------------------
!   39 =  CRS211 = ABMAS/SYMO                 GBA:       p.
!    Shasta red fir/creeping snowberry
!
   'CRS211  ','ABMAS ', 755.,'SH  ',  91.,   1,   1,   6, &
!-----------------------------------------------------------------------
!   40 =  CTH111 = CHLA-QUVA                  GBA:       p.
!    Port-Orford-cedar-huckleberry oak
!
   'CTH111  ','PSME  ',1309.,'DF  ',  87.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   41 =  CTH211 = CHLA-ACMA                  GBA:       p.
!    Port-Orford-cedar-bigleaf maple
!
   'CTH211  ','PSME  ', 760.,'DF  ',  87.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   42 =  CTS111 = CHLA/BENE/ACTR             GBA:       p.
!    Port-Orford-cedar/dwarf Oregongrape/vanillaleaf
!
   'CTS111  ','PSME  ',1348.,'DF  ',  85.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   43 =  CTS112 = CHLA/BENE/LIBOL            GBA:       p.
!    Port-Orford-cedar/dwarf Oregongrape/western twinflower
!
   'CTS112  ','PSME  ', 370.,'DF  ',  92.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   44 =  CTS211 = CHLA/GASH                  GBA:       p.
!    Port-Orford-cedar/salal
!
   'CTS211  ','PSME  ', 990.,'DF  ',  83.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   45 =  CTS311 = CHLA/GABU                  GBA:       p.
!    Port-Orford-cedar/box-leaved silktassle
!
   'CTS311  ','PSME  ', 660.,'DF  ',  87.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=46,60) / &
!-----------------------------------------------------------------------
!   46 =  CWC221 = ABCO-PSME                  GBA:       p.
!    White fir-Douglas-fir
!
   'CWC221  ','PSME  ', 815.,'DF  ',  92.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   47 =  CWC231 = ABCO-PSME/BENE             GBA:       p.
!    White fir-Douglas-fir/dwarf Oregongrape
!
   'CWC231  ','PSME  ', 785.,'DF  ',  95.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   48 =  CWC232 = ABCO-PSME/HODI             GBA:       p.
!    White fir-Douglas-fir/creambush oceanspray
!
   'CWC232  ','PSME  ', 675.,'DF  ',  89.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   49 =  CWC233 = ABCO-PSME/DEPAUPERATE      GBA:       p.
!    White fir-Douglas-fir/depauperate
!
   'CWC233  ','PSME  ', 988.,'DF  ',  78.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   50 =  CWC241 = ABCO-PIPO                  GBA:       p.
!    White fir-ponderosa pine
!
   'CWC241  ','PSME  ', 930.,'DF  ',  84.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   51 =  CWC521 = ABCO-PIBR/VAME             GBA:       p.
!    White fir-Brewer spruce/thin-leaved huckleberry
!
   'CWC521  ','PSME  ', 899.,'DF  ',  57.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   52 =  CWC522 = ABCO-PIBR/GAOV             GBA:       p.
!    White fir-Brewer spruce/slender salal
!
   'CWC522  ','PSME  ', 874.,'DF  ',  95.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   53 =  CWC523 = ABCO-PIBR/CHUM             GBA:       p.
!    White fir-Brewer spruce/western prince's-pine
!
   'CWC523  ','PSME  ', 335.,'DF  ',  69.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   54 =  CWC611 = ABCO-CHLA                  GBA:       p.
!    White fir-Port-Orford-cedar
!
   'CWC611  ','PSME  ',1399.,'DF  ',  99.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   55 =  CWC612 = ABCO-CHLA/DEPAUPERATE      GBA:       p.
!    White fir-Port-Orford-cedar/depauperate
!
   'CWC612  ','PSME  ',1399.,'DF  ',  99.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   56 =  CWC721 = ABCO-ABMAS/RIBES           GBA:       p.
!    White fir-Shasta red fir/currant
!
   'CWC721  ','ABCO  ', 665.,'WF  ',  77.,   1,   1,   4, &
!-----------------------------------------------------------------------
!   57 =  CWC722 = ABCO-ABMAS/ROGY            GBA:       p.
!    White fir-Shasta red fir/baldhip rose
!
   'CWC722  ','PSME  ',1349.,'DF  ',  89.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   58 =  CWC723 = ABCO-ABMAS/SYMO            GBA:       p.
!    White fir-Shasta red fir/creeping snowberry
!
   'CWC723  ','PSME  ', 945.,'DF  ',  81.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   59 =  CWC811 = ABCO-TABR                  GBA:       p.
!    White fir-Pacific yew
!
   'CWC811  ','PSME  ', 695.,'DF  ',  96.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   60 =  CWC911 = ABCO-CHNO                  GBA:       p.
!    White fir-Alaska cedar
!
   'CWC911  ','ABCO  ',1641.,'WF  ',  65.,   1,   1,   4/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=61,75) / &
!-----------------------------------------------------------------------
!   61 =  CWF911 = ABCO/HERB                  GBA:       p.
!    White fir/herb
!
   'CWF911  ','PSME  ', 670.,'DF  ',  89.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   62 =  CWH312 = ABCO-LIDE3                 GBA:       p.
!    White fir-tanoak
!
   'CWH312  ','PSME  ', 815.,'DF  ',  93.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   63 =  CWH413 = ABCO-ACGL                  GBA:       p.
!    White fir-Rocky Mountain maple
!
   'CWH413  ','PSME  ', 654.,'DF  ', 108.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   64 =  CWH511 = ABCO-QUSA/CHUM             GBA:       p.
!    White fir-Sadler oak/western prince's-pine
!
   'CWH511  ','PSME  ',1337.,'DF  ',  93.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   65 =  CWH521 = ABCO-QUSA/BENE-PAMY        GBA:       p.
!    White fir-Sadler oak/dwarf Oregongrape-Oregon boxwood
!
   'CWH521  ','PSME  ', 470.,'DF  ',  96.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   66 =  CWH522 = ABCO-QUSA/BENE             GBA:       p.
!    White fir-Sadler oak/dwarf Oregongrape
!
   'CWH522  ','PSME  ', 560.,'DF  ', 105.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   67 =  CWH531 = ABCO-QUSA-CACH             GBA:       p.
!    White fir-Sadler oak-golden chinquapin
!
   'CWH531  ','PSME  ', 810.,'DF  ',  94.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   68 =  CWS331 = ABCO/SYMO                  GBA:       p.
!    White fir/creeping snowberry
!
   'CWS331  ','PSME  ', 695.,'DF  ',  92.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   69 =  CWS523 = ABCO/BENE                  GBA:       p.
!    White fir/dwarf Oregongrape
!
   'CWS523  ','PSME  ', 900.,'DF  ', 101.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   70 =  HTC111 = LIDE3-SESE2                GBA:       p.
!    Tanoak-coast redwood
!
   'HTC111  ','PSME  ', 820.,'DF  ', 125.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   71 =  HTC211 = LIDE3-TSHE                 GBA:       p.
!    Tanoak-western hemlock
!
   'HTC211  ','PSME  ', 870.,'DF  ', 103.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   72 =  HTC311 = LIDE3-CHLA                 GBA:       p.
!    Tanoak-Port-Orford-cedar
!
   'HTC311  ','PSME  ', 890.,'DF  ',  98.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   73 =  HTC411 = LIDE3-ABCO-ACCI            GBA:       p.
!    Tanoak-white fir-vine maple
!
   'HTC411  ','PSME  ', 865.,'DF  ',  90.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   74 =  HTC412 = LIDE3-ABCO                 GBA:       p.
!    Tanoak-white fir
!
   'HTC412  ','PSME  ', 970.,'DF  ',  99.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   75 =  HTH111 = LIDE3-QUCH                 GBA:       p.
!    Tanoak-canyon live oak
!
   'HTH111  ','PSME  ', 735.,'DF  ',  96.,   1,   1,   7/
!
DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I), &
        FVSSEQ(I),I=76,90) / &
!-----------------------------------------------------------------------
!   76 =  HTH112 = LIDE3-QUCH/BENE            GBA:       p.
!    Tanoak-canyon live oak/dwarf Oregongrape
!
   'HTH112  ','PSME  ', 650.,'DF  ',  83.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   77 =  HTH211 = LIDE3-UMCA                 GBA:       p.
!    Tanoak-California laurel
!
   'HTH211  ','PSME  ', 810.,'DF  ', 110.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   78 =  HTH311 = LIDE3-ACCI                 GBA:       p.
!    Tanoak-vine maple
!
   'HTH311  ','PSME  ', 595.,'DF  ', 104.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   79 =  HTS111 = LIDE3/VAOV2-GASH           GBA:       p.
!    Tanoak/evergreen huckleberry-salal
!
   'HTS111  ','PSME  ', 910.,'DF  ', 107.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   80 =  HTS112 = LIDE3/VAOV2                GBA:       p.
!    Tanoak/evergreen huckleberry
!
   'HTS112  ','PSME  ', 915.,'DF  ', 116.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   81 =  HTS221 = LIDE3/RHMA                 GBA:       p.
!    Tanoak/Pacific rhododendron
!
   'HTS221  ','PSME  ', 830.,'DF  ', 111.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   82 =  HTS222 = LIDE3/RHMA-VAOV2           GBA:       p.
!    Tanoak/Pacific rhododendron-evergreen huckleberry
!
   'HTS222  ','PSME  ', 815.,'DF  ',  93.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   83 =  HTS223 = LIDE3/RHMA-GASH            GBA:       p.
!    Tanoak/Pacific rhododendron-salal
!
   'HTS223  ','PSME  ', 840.,'DF  ',  68.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   84 =  HTS311 = LIDE3/BENE                 GBA:       p.
!    Tanoak/dwarf Oregongrape
!
   'HTS311  ','PSME  ', 805.,'DF  ',  95.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   85 =  HTS312 = LIDE3/BENE-RHDI            GBA:       p.
!    Tanoak/dwarf Oregongrape-poison oak
!
   'HTS312  ','PSME  ', 785.,'DF  ',  96.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   86 =  HTS321 = LIDE3/GASH                 GBA:       p.
!    Tanoak/salal
!
   'HTS321  ','PSME  ', 970.,'DF  ', 102.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   87 =  HTS331 = LIDE3/GASH-RHMA            GBA:       p.
!    Tanoak/salal-Pacific rhododendron
!
   'HTS331  ','PSME  ', 610.,'DF  ',  90.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   88 =  HTS341 = LIDE3/GASH-BENE            GBA:       p.
!    Tanoak/salal-dwarf Oregongrape
!
   'HTS341  ','PSME  ', 935.,'DF  ', 109.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   89 =  HTS411 = LIDE3/RHDI-LOHI            GBA:       p.
!    Tanoak/poison oak-hairy honeysuckle
!
   'HTS411  ','PSME  ', 730.,'DF  ',  79.,   1,   1,   7, &
!-----------------------------------------------------------------------
!   90 =  HTS511 = LIDE3/RHCA                 GBA:       p.
!    Tanoak/California coffeeberry
!
   'HTS511  ','PSME  ', 450.,'DF  ',  50.,   1,   1,   7/
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

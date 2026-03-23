BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! EC $Id$
!----------
!
!     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'COEFFS.f90'
!
!
INCLUDE 'ESPARM.f90'
!
!
INCLUDE 'ESCOMN.f90'
!
!
INCLUDE 'PDEN.f90'
!
!
INCLUDE 'ECON.f90'
!
!
INCLUDE 'HTCAL.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'RANCOM.f90'
!
!
INCLUDE 'SCREEN.f90'
!
!
INCLUDE 'VARCOM.f90'
!
!
INCLUDE 'FVSSTDCM.f90'
!
!
!OMMONS
!----------
INTEGER I,J
!----------
!  SPECIES LIST FOR EAST CASCADES VARIANT.
!
!   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
!   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
!   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
!   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
!   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
!   6 = GRAND FIR               (GF)    ABIES GRANDIS
!   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
!   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
!   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
!  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
!  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
!  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
!  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
!  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
!  15 = NOBLE FIR               (NF)    ABIES PROCERA
!  16 = WHITE FIR               (WF)    ABIES CONCOLOR
!  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
!  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
!  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
!  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
!  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
!  22 = RED ALDER               (RA)    ALNUS RUBRA
!  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
!  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA var.
!                                       CHRYSOPHYLLA
!  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
!  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
!  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var.
!                                       TRICHOCARPA
!  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
!  29 = PLUM                    (PL)    PRUNUS
!  30 = WILLOW                  (WI)    SALIX
!  31 = OTHER SOFTWOOD          (OS)
!  32 = OTHER HARDWOOD          (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE EC VARIANT:
!      USE 6(GF) FOR 16(WF)
!      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
!
!  FROM THE WC VARIANT:
!      USE 19(WH) FOR 11(WH)
!      USE 33(PY) FOR 13(PY)
!      USE 31(WB) FOR 14(WB)
!      USE  7(NF) FOR 15(NF)
!      USE 30(LL) FOR 17(LL)
!      USE  8(YC) FOR 18(YC)
!      USE 29(WJ) FOR 19(WJ)
!      USE 21(BM) FOR 20(BM) AND 21(VN)
!      USE 22(RA) FOR 22(RA)
!      USE 24(PB) FOR 23(PB)
!      USE 25(GC) FOR 24(GC)
!      USE 34(DG) FOR 25(DG)
!      USE 26(AS) FOR 26(AS) AND 32(OH)
!      USE 27(CW) FOR 27(CW)
!      USE 28(WO) FOR 28(WO)
!      USE 36(CH) FOR 29(PL)
!      USE 37(WI) FOR 30(WI)
!----------
!     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
!----------
DATA BKRAT/ &
       0.,    0.,    0.,    0.,    0., &
       0.,    0.,    0.,    0.,    0., &
       0.,    0.,    0.,    0.,    0., &
       0.,    0.,    0.,    0.,    0., &
       0.,    0.,    0.,    0.,    0., &
       0.,    0.,    0.,    0.,    0., &
       0.,    0./
!
DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
!
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /
!
DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
!
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
!----------
!   COMMON STATEMENT FOR ESCOMN VARIABLE
!----------
DATA XMIN/ &
    1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, &
    1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.5, 1.0, 1.0, 1.0, &
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
    0.5, 1.0/
!
DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
!
DATA ISPSPE/20,21,22,23,24,25,26,27,28,29,30/
!
DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325, &
     1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
!
DATA HHTMAX/ &
    23.0, 27.0, 21.0, 21.0, 22.0, 20.0, 24.0, 18.0, 18.0, 17.0, &
    20.0, 22.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
    20.0, 50.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, &
    22.0, 20.0/
!
DATA IFORCD/103,104,105,106,621,110,113,114,116,117, &
               118,109,111,112,412,402,108,102,115,  0/, &
        IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17, &
                 4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
!----------
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
!     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
!     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
!----------
DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0/
!----------
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
!     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
!     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
!----------
DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/ &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0/
!----------
!     COMMON STATEMENT FOR PLOT VARIABLES.
!----------
DATA JSP / &
    'WP ',   'WL ',   'DF ',   'SF ',   'RC ', &
    'GF ',   'LP ',   'ES ',   'AF ',   'PP ', &
    'WH ',   'MH ',   'PY ',   'WB ',   'NF ', &
    'WF ',   'LL ',   'YC ',   'WJ ',   'BM ', &
    'VN ',   'RA ',   'PB ',   'GC ',   'DG ', &
    'AS ',   'CW ',   'WO ',   'PL ',   'WI ', &
    'OS ',   'OH '/
!
!  NOTE: VINE MAPLE IS CONSIDERED A SHRUB BY FIA (CODE 324); THE FIA
!  CODE FOR ROCKY MTN MAPLE (321) WAS APPARENTLY USED IN THE INVENTORY
!  DATA FOR VINE MAPLE. HOWEVER, IN THIS VARIANT, FIA CODE 321 IS BEING
!  ASSOCIATED WITH VINE MAPLE IN THE SPECIES TRANSLATOR.
!
DATA FIAJSP / &
    '119',   '073',   '202',   '011',   '242', &
    '017',   '108',   '093',   '019',   '122', &
    '263',   '264',   '231',   '101',   '022', &
    '015',   '072',   '042',   '064',   '312', &
    '324',   '351',   '375',   '431',   '492', &
    '746',   '747',   '815',   '760',   '920', &
    '299',   '998'/
!
DATA PLNJSP / &
    'PIMO3 ','LAOC  ','PSME  ','ABAM  ','THPL  ', &
    'ABGR  ','PICO  ','PIEN  ','ABLA  ','PIPO  ', &
    'TSHE  ','TSME  ','TABR2 ','PIAL  ','ABPR  ', &
    'ABCO  ','LALY  ','CANO9 ','JUOC  ','ACMA3 ', &
    'ACCI  ','ALRU2 ','BEPA  ','CHCHC4','CONU4 ', &
    'POTR5 ','POBAT ','QUGA4 ','PRUNU ','SALIX ', &
    '2TN   ','2TB   '/
!
DATA JTYPE /130,170,250,260,280,290,310,320,330,420, &
               470,510,520,530,540,550,570,610,620,640, &
               660,670,680,690,710,720,730,830,850,999,92*0 /
!
DATA NSP / &
    'WP1','WL1','DF1','SF1','RC1','GF1','LP1','ES1','AF1','PP1', &
    'WH1','MH1','PY1','WB1','NF1','WF1','LL1','YC1','WJ1','BM1', &
    'VN1','RA1','PB1','GC1','DG1','AS1','CW1','WO1','PL1','WI1', &
    'OS1','OH1', &
    'WP2','WL2','DF2','SF2','RC2','GF2','LP2','ES2','AF2','PP2', &
    'WH2','MH2','PY2','WB2','NF2','WF2','LL2','YC2','WJ2','BM2', &
    'VN2','RA2','PB2','GC2','DG2','AS2','CW2','WO2','PL2','WI2', &
    'OS2','OH2', &
    'WP3','WL3','DF3','SF3','RC3','GF3','LP3','ES3','AF3','PP3', &
    'WH3','MH3','PY3','WB3','NF3','WF3','LL3','YC3','WJ3','BM3', &
    'VN3','RA3','PB3','GC3','DG3','AS3','CW3','WO3','PL3','WI3', &
    'OS3','OH3'/
!----------
!   COMMON STATEMENT FOR COEFFS VARIABLES
!----------
!   HT1 AND HT2 ARE HEIGHT DUBBING COEFFICIENTS FOR TREES 5.0" DBH
!   AND LARGER FOR SPECIES USING EQUATIONS FROM WC.
!----------
DATA HT1/ &
      5.035,    4.961,   4.920,   5.032,   4.896, &
      5.032,    4.854,   4.948,   4.834,   4.884, &
      5.298,   3.9715,   5.188,   5.188,   5.327, &
      5.032,    5.188,   5.143,   5.152,   4.700, &
      4.700,    4.886,   5.152,   5.152,   5.152, &
      5.152,    5.152,   5.152,   5.152,   5.152, &
     3.9715,    5.152/
!
DATA HT2/ &
    -10.674,   -8.247,  -9.003, -10.482,  -8.391, &
    -10.482,   -8.296,  -9.041,  -9.042,  -9.741, &
    -13.240,  -6.7145, -13.801, -13.801, -15.450, &
    -10.482,  -13.801, -13.497, -13.576,  -6.326, &
     -6.326,   -8.792, -13.576, -13.576, -13.576, &
    -13.576,  -13.576, -13.576, -13.576, -13.576, &
    -6.7145,  -13.576/
!
DATA SIGMAR/ &
     0.5086,   0.3340,  0.3420,  0.4320,  0.5500, &
     0.3890,   0.3060,  0.3970,  0.4690,  0.3660, &
     0.4104,   0.3220,  0.4842,  0.4842,  0.4275, &
     0.3890,   0.4842,  0.3931,  0.5357,  0.5107, &
     0.5107,   0.7487,  0.5357,  0.5357,  0.5357, &
     0.5357,   0.5357,   0.236,  0.5357,  0.5357, &
     0.3220,   0.5357/
!----------
!   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
!----------
!   HTT1 IS USED TO STORE THE HEIGHT DUBBING COEFFICIENTS FOR TREES
!   LESS THAN 5.0" DBH FOR SPECIES USING EQUATIONS FROM WC.
!   DIMENSIONED AT (MAXSP,9)
!
DATA HTT1/ &
    10*0.0, &
    1.3608,    0.0, 1.5907, 1.5907, 1.7100, &
       0.0, 1.5907, 1.5907, 0.0994, 0.0994, &
    0.0994, 0.0994, 0.0994, 0.0994, 0.0994, &
    0.0994, 0.0994, 0.0994, 0.0994, 0.0994, &
       0.0, 0.0994, &
!
!   HTT1(ISPC,2) IS USED TO STORE THE DBH COEFFICIENT.
!
    10*0.0, &
    0.6151,    0.0, 0.3040, 0.3040, 0.2943, &
       0.0, 0.3040, 0.3040, 4.9767, 4.9767, &
    4.9767, 4.9767, 4.9767, 4.9767, 4.9767, &
    4.9767, 4.9767, 4.9767, 4.9767, 4.9767, &
       0.0, 4.9767, &
!
!   HTT1(ISPC,3) IS USED TO STORE THE CR COEFFICIENT.
!
    32*0.0, &
!
!   HTT1(ISPC,4) IS USED TO STORE THE DBH SQUARED COEFFICIENT.
!
    10*0.0, &
   -0.0442,     0.,     0.,     0.,     0., &
    17*0.0, &
!
!   HTT1(ISPC,5) IS USED TO STORE THE DUMMY VARIABLE FOR
!   MANAGED/UNMANAGED STANDS.
!
    10*0.0, &
    0.0829,    0.0,    0.0,    0.0, 0.1054, &
    17*0.0, &
!
!   HTT1(ISPC,6) THRU HTT1(ISPC,9) ARE NOT USED. SET TO 0.0
!
    128*0.0/
!
DATA REGNBK/2.999/
!
DATA S0/55329D0/,SS/55329./
!
DATA LSCRN,JOSCRN/.FALSE.,6/
!
DATA JOSUME/13/
!
DATA KOLIST,FSTOPEN /27,.FALSE./
!
END

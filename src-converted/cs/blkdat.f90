BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! CS $Id$
!----------
!
!     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
!
!     COMMON STATEMENT FOR MODEL COEFFICIENTS WHICH ARE HABITAT
!     AND SITE DEPENDENT.
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
!
DATA BKRAT/2*.95,3*.93,4*.91,4*.95,11*.93,3*.95,.91,4*.95,8*.93, &
             .95,6*.93,3*.91,34*.93,12*.95/
!
DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/
!
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /
!
DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
!
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/15,2,3,16,4,8 /
!----------
!   COMMON STATEMENT FOR ESCOMN VARIABLE
!----------
!   HHTMAX, IFORST, OCURHT, AND OCURNF HAVE ALL BEEN SET TO ONE
!   VALUE FOR THE LS VARIANT.
!
DATA XMIN/ &
   0.33, 2.10, 0.25, 0.42, 0.25, 0.25, 0.33, 0.33, 0.33, 0.33, &
   3.59, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, &
   0.33, 0.33, 0.33, 0.25, 0.33, 0.42, 0.50, 0.42, 1.00, 0.33, &
   0.42, 0.42, 0.33, 0.33, 0.25, 0.50, 0.33, 0.50, 0.33, 0.50, &
   0.42, 0.33, 0.25, 0.42, 0.42, 0.42, 0.33, 0.42, 0.33, 0.33, &
   0.33, 2.80, 0.33, 0.33, 0.25, 0.33, 0.25, 2.80, 0.33, 1.40, &
   0.33, 0.25, 0.50, 0.33, 1.40, 0.25, 0.50, 1.40, 0.50, 0.55, &
   0.63, 0.25, 5.00, 0.42, 0.42, 0.42, 0.58, 1.40, 0.58, 1.40, &
   0.33, 0.33, 4.70, 1.00, 0.33, 0.42, 2.10, 0.25, 0.25, 0.50, &
   0.25, 0.33, 0.42, 2.10, 0.42, 0.33/
!
DATA HHTMAX/ &
   16., 27., 14., 14., 14., 16., 20., 20., 18., 16., &
   20., 20., 16., 14., 14., 14., 18., 14., 14., 14., &
   14., 14., 14., 14., 18., 28., 20., 24., 20., 16., &
   18., 26., 16., 14., 12., 20., 16., 20., 12., 20., &
   24., 16., 16., 24., 24., 24., 16., 20., 16., 16., &
   16., 20., 12., 16., 14., 12., 12., 20., 16., 20., &
   14., 14., 20., 16., 20., 14., 20., 20., 18., 20., &
   20., 12., 20., 24., 20., 20., 24., 20., 24., 20., &
   18., 18., 20., 32., 10., 20., 20., 18., 16., 20., &
   12., 20., 20., 20., 20., 16./
!
DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/, &
     BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232, &
     1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696, &
     1.742,1.789/,IFORST/20*1/, &
     IFORCD/905,908,911,17*0/, &
     ISPSPE/3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26, &
       27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45, &
       46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64, &
       65,66,67,69,70,71,72,73,74,75,76,77,79,80,81,82,83,84,86, &
       87,88,89,90,91,92,93,94,95,96/
!
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
!
DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/1536*1.0/
!
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
!
DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/1920*1.0/
!----------
!  COMMON STATEMENT FOR PLOT VARIABLES.
!----------
DATA JSP / &
    'RC ',   'JU ',   'SP ',   'VP ',   'LP ',   'OS ',   'WP ', &
    'WN ',   'BN ',   'TL ',   'TS ',   'WT ',   'BG ',   '   ', &
    'SH ',   'SL ',   'MH ',   'PH ',   'HI ',   'WH ',   'BH ', &
    'PE ',   'BI ',   'AB ',   'BA ',   'PA ',   'UA ',   'EC ', &
    'RM ',   'BE ',   'SV ',   'BC ',   'AE ',   'SG ',   'HK ', &
    'WE ',   'EL ',   'SI ',   'RL ',   'RE ',   'YP ',   'BW ', &
    'SM ',   'AS ',   'WA ',   'GA ',   'WO ',   'RO ',   'SK ', &
    'BO ',   'SO ',   'BJ ',   'CK ',   'SW ',   'BR ',   'SN ', &
    'PO ',   'DO ',   'CO ',   'PN ',   'CB ',   'QI ',   'OV ', &
    'WK ',   'NK ',   'WL ',   'QS ',   '   ',   'SS ',   'OB ', &
    'CA ',   'PS ',   'HL ',   'BP ',   'BT ',   'QA ',   'BK ', &
    '   ',   'SY ',   'BY ',   'RB ',   'SU ',   'WI ',   'BL ', &
    'OH ',   'AH ',   'RD ',   'DW ',   'HT ',   'KC ',   'OO ', &
    'CT ',   'MV ',   'MB ',   'HH ',   'SD '/
!
DATA FIAJSP / &
    '068',   '057',   '110',   '132',   '131',   '299',   '129', &
    '602',   '601',   '690',   '694',   '691',   '693',   '   ', &
    '407',   '405',   '409',   '403',   '400',   '401',   '402', &
    '404',   '408',   '531',   '543',   '545',   '546',   '742', &
    '316',   '313',   '317',   '762',   '972',   '461',   '462', &
    '971',   '970',   '974',   '975',   '977',   '621',   '951', &
    '318',   '540',   '541',   '544',   '802',   '833',   '812', &
    '837',   '806',   '824',   '826',   '804',   '823',   '825', &
    '835',   '836',   '832',   '830',   '813',   '817',   '822', &
    '827',   '828',   '831',   '834',   '   ',   '931',   '331', &
    '450',   '521',   '552',   '741',   '743',   '746',   '901', &
    '   ',   '731',   '221',   '373',   '611',   '920',   '922', &
    '998',   '391',   '471',   '491',   '500',   '571',   '641', &
    '651',   '653',   '680',   '701',   '711'/
!
DATA PLNJSP / &
   'JUVI  ','JUNIP ','PIEC2 ','PIVI2 ','PITA  ','2TN   ','PIST  ', &
   'JUNI  ','JUCI  ','NYSSA ','NYBI  ','NYAQ2 ','NYSY  ','      ', &
   'CAOV2 ','CALA21','CAAL27','CAGL8 ','CARYA ','CAAQ2 ','CACO15', &
   'CAIL2 ','CATE9 ','FAGR  ','FRNI  ','FRPR  ','FRQU  ','PODE3 ', &
   'ACRU  ','ACNE2 ','ACSA2 ','PRSE2 ','ULAM  ','CELA  ','CEOC  ', &
   'ULAL  ','ULMUS ','ULPU  ','ULRU  ','ULTH  ','LITU  ','TIAM  ', &
   'ACSA3 ','FRAXI ','FRAM2 ','FRPE  ','QUAL  ','QURU  ','QUFA  ', &
   'QUVE  ','QUCO2 ','QUMA3 ','QUMU  ','QUBI  ','QUMA2 ','QUMI  ', &
   'QUST  ','QUSI2 ','QUPR2 ','QUPA2 ','QUPA5 ','QUIM  ','QULY  ', &
   'QUNI  ','QUNU  ','QUPH  ','QUSH  ','      ','SAAL5 ','AEGL  ', &
   'CATAL ','DIVI5 ','GLTR  ','POBA2 ','POGR4 ','POTR5 ','ROPS  ', &
   '      ','PLOC  ','TADI2 ','BENI  ','LIST2 ','SALIX ','SANI  ', &
   '2TB   ','CACA18','CECA4 ','COFL2 ','CRATA ','GYDI  ','MAPO  ', &
   'MAAC  ','MAVI2 ','MORUS ','OSVI  ','OXAR  '/
!
DATA JTYPE /122*0/
!
DATA NSP /'RC1','JU1','SP1','VP1','LP1','OS1','WP1','WN1','BN1', &
    'TL1','TS1','WT1','BG1','__1','SH1','SL1','MH1','PH1','HI1', &
    'WH1','BH1','PE1','BI1','AB1','BA1','PA1','UA1','EC1','RM1','BE1' &
   ,'SV1','BC1','AE1','SG1','HK1','WE1','EL1','SI1','RL1','RE1', &
    'YP1','BW1','SM1','AS1','WA1','GA1','WO1','RO1','SK1','BO1', &
    'SO1','BJ1','CK1','SW1','BR1','SN1','PO1','DO1','CO1','PN1', &
    'CB1','QI1','OV1','WK1','NK1','WL1','QS1','__1','SS1','OB1', &
    'CA1','PS1','HL1','BP1','BT1','QA1','BK1','__1','SY1','BY1', &
    'RB1','SU1','WI1','BL1','OH1','AH1','RD1','DW1','HT1','KC1', &
    'OO1','CT1','MV1','MB1','HH1','SD1','RC2','JU2','SP2','VP2', &
    'LP2','OS2','WP2','WN2','BN2','TL2','TS2','WT2','BG2','__2', &
    'SH2','SL2','MH2','PH2','HI2','WH2','BH2','PE2','BI2','AB2', &
    'BA2','PA2','UA2','EC2','RM2','BE2','SV2','BC2','AE2','SG2', &
    'HK2','WE2','EL2','SI2','RL2','RE2','YP2','BW2','SM2','AS2', &
    'WA2','GA2','WO2','RO2','SK2','BO2','SO2','BJ2','CK2','SW2', &
    'BR2','SN2','PO2','DO2','CO2','PN2','CB2','QI2','OV2','WK2', &
    'NK2','WL2','QS2','__2','SS2','OB2','CA2','PS2','HL2','BP2', &
    'BT2','QA2','BK2','__2','SY2','BY2','RB2','SU2','WI2','BL2', &
    'OH2','AH2','RD2','DW2','HT2','KC2','OO2','CT2','MV2','MB2', &
    'HH2','SD2','RC3','JU3','SP3','VP3','LP3','OS3','WP3','WN3', &
    'BN3','TL3','TS3','WT3','BG3','__3','SH3','SL3','MH3','PH3', &
    'HI3','WH3','BH3','PE3','BI3','AB3','BA3','PA3','UA3','EC3', &
    'RM3','BE3','SV3','BC3','AE3','SG3','HK3','WE3','EL3','SI3', &
    'RL3','RE3','YP3','BW3','SM3','AS3','WA3','GA3','WO3','RO3', &
    'SK3','BO3','SO3','BJ3','CK3','SW3','BR3','SN3','PO3','DO3', &
    'CO3','PN3','CB3','QI3','OV3','WK3','NK3','WL3','QS3','__3', &
    'SS3','OB3','CA3','PS3','HL3','BP3','BT3','QA3','BK3','__3', &
    'SY3','BY3','RB3','SU3','WI3','BL3','OH3','AH3','RD3','DW3', &
    'HT3','KC3','OO3','CT3','MV3','MB3','HH3','SD3'/
!----------
!=====================================
!     SPECIES LIST FOR CENTRAL STATES
!=====================================
!     1 = EASTERN REDCEDAR (RC)
!     2 = JUNIPER (JU)
!     3 = SHORTLEAF PINE (SP)
!     4 = VIRGINIA PINE (VP)
!     5 = LOBLOLLY PINE (LP)
!     6 = OTHER SOFTWOOD (OS)
!     7 = EASTERN WHITE PINE (WP)
!     8 = BLACK WALNUT (WN)
!     9 = BUTTERNUT (BN)
!    10 = TUPELO (TL)
!    11 = SWAMP TUPELO (TS)
!    12 = WATER TUPELO (WT)
!    13 = BLACKGUM (BG)
!    14 = ---
!    15 = SHAGBARK HICKORY (SH)
!    16 = SHELLBARK HICKORY (SL)
!    17 = MOCKERNUT HICKORY (MH)
!    18 = PIGNUT HICKORY (PH)
!    19 = HICKORY (HI)
!    20 = WATER HICKORY (WH)
!    21 = BITTERNUT HICKORY (BH)
!    22 = PECAN (PE)
!    23 = BLACK HICKORY (BI)
!    24 = AMERICAN BEECH (AB)
!    25 = BLACK ASH (BA)
!    26 = PUMPKIN ASH (PA)
!    27 = BLUE ASH (UA)
!    28 = EASTERN COTTONWOOD (EC)
!    29 = RED MAPLE (RM)
!    30 = BOXELDER (BE)
!    31 = SILVER MAPLE (SV)
!    32 = BLACK CHERRY (BC)
!    33 = AMERICAN ELM (AE)
!    34 = SUGARBERRY (SG)
!    35 = COMMON HACKBERRY (HK)
!    36 = WINGED ELM (WE)
!    37 = ELM (EL)
!    38 = SIBERIAN ELM (SI)
!    39 = SLIPPERY (RED) ELM (RL)
!    40 = ROCK ELM (RE)
!    41 = TULIPTREE (YP)
!    42 = AMERICAN BASSWOOD (BW)
!    43 = SUGAR MAPLE (SM)
!    44 = ASH (AS)
!    45 = WHITE ASH (WA)
!    46 = GREEN ASH (GA)
!    47 = WHITE OAK (WO)
!    48 = NORTHERN RED OAK (RO)
!    49 = SOUTHERN RED OAK (SK)
!    50 = BLACK OAK (BO)
!    51 = SCARLET OAK (SO)
!    52 = BLACKJACK OAK (BJ)
!    53 = CHINKAPIN OAK (CK)
!    54 = SWAMP WHITE OAK (SW)
!    55 = BUR OAK (BR)
!    56 = SWAMP CHESTNUT OAK (SN)
!    57 = POST OAK (PO)
!    58 = BOTTOMLAND POST OAK (DO)
!    59 = CHESTNUT OAK (CO)
!    60 = PIN OAK (PN)
!    61 = CHERRYBARK OAK (CB)
!    62 = SHINGLE OAK (QI)
!    63 = OVERCUP OAK (OV)
!    64 = WATER OAK (WK)
!    65 = NUTTALL OAK (NK)
!    66 = WILLOW OAK (WL)
!    67 = SHUMARD'S OAK (QS)
!    68 = ---
!    69 = SASSAFRAS (SS)
!    70 = OHIO BUCKEYE (OB)
!    71 = CATALPA (CA)
!    72 = COMMON PERSIMMON (PS)
!    73 = HONEYLOCUST (HL)
!    74 = BALSAM POPLAR (BP)
!    75 = BIGTOOTH ASPEN (BT)
!    76 = QUAKING ASPEN (QA)
!    77 = BLACK LOCUST (BK)
!    78 = ---
!    79 = SYCAMORE (SY)
!    80 = BALD CYPRESS (BY)
!    81 = RIVER BIRCH (RB)
!    82 = SWEETGUM (SU)
!    83 = WILLOW (WI)
!    84 = BLACK WILLOW (BL)
!    85 = OTHER HARDWOOD (OH)
!    86 = AMERICAN HORNBEAM (AH)
!    87 = EASTERN REDBUD (RD)
!    88 = FLOWERING DOGWOOD (DW)
!    89 = HAWTHORN (HT)
!    90 = KENTUCKY COFFEETREE (KC)
!    91 = OSAGE-ORANGE (OO)
!    92 = CUCUMBER TREE (CT)
!    93 = SWEETBAY (MV)
!    94 = MULBERRY  (MB)
!    95 = HOPHORNBEAM (HH)
!    96 = SOURWOOD (SD)
!----------
!  SIGMAR ARE THE ERROR TERMS FOR DIAMETER GROWTH.  THESE COEFFICIENTS
!  ARE THE ROOT MEAN SQUARE ERROR FOR EACH EQUATION.
!----------
DATA SIGMAR/ &
     0.60928, 0.60928, 0.63787, 0.73192, 0.73192, &
     0.63965, 0.63965, 0.74594, 0.74594, 0.66815, &
     0.66815, 0.66815, 0.66815, 0.64614, 0.64614, &
     0.64614, 0.64614, 0.68908, 0.68908, 0.68908, &
     0.68908, 0.68908, 0.68908, 0.73976, 0.62169, &
     0.62169, 0.62169, 0.66929, 0.82798, 0.82798, &
     0.82798, 0.74808, 0.75819, 0.75819, 0.75819, &
     0.75819, 0.75819, 0.75819, 0.75819, 0.75819, &
     0.63415, 0.73467, 0.69750, 0.69474, 0.69474, &
     0.69474, 0.60740, 0.60699, 0.60699, 0.56874, &
     0.54465, 0.60603, 0.69062, 0.69062, 0.69062, &
     0.69062, 0.60748, 0.60748, 0.54283, 0.72515, &
     0.72515, 0.72515, 0.69062, 0.72515, 0.72515, &
     0.72515, 0.72515, 0.77412, 0.63500, 0.77412, &
     0.77412, 0.77412, 0.68493, 0.77412, 0.77412, &
     0.77412, 0.77412, 0.80347, 0.80347, 0.80347, &
     0.80347, 0.80347, 0.80347, 0.80347, 0.68013, &
     0.68013, 0.68013, 0.68013, 0.68013, 0.74594, &
     0.68111, 0.68013, 0.68013, 0.68013, 0.68013, &
     0.68013/
!----------
!     DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK IN THE
!     FOLLOWING DATA STATEMENTS, DF IS USED FOR OT, AND WF FOR RC
!----------
DATA HT1/ &
     4.4718,   4.0374,   4.6271,   4.4718,   4.6897,   4.0374, &
     4.6090,   4.5018,   4.5018,   4.3802,   4.4334,   4.4330, &
     4.3802,   4.5128,   4.5128,   4.5128,   4.5128,   4.5128, &
     4.5128,   4.5128,   4.5128,   4.5128,   4.5128,   4.4772, &
     4.6155,   4.4819,   4.4819,   4.9396,   4.3379,   4.5018, &
     4.5991,   4.3286,   4.6008,   4.5128,   4.4207,   4.5992, &
     4.3744,   4.3744,   4.6238,   4.3744,   4.6892,   4.5820, &
     4.4834,   4.4819,   4.5959,   4.6155,   4.5463,   4.5202, &
     4.5142,   4.4747,   4.5225,   3.9191,   4.3420,   4.7342, &
     4.5225,   4.6135,   4.2496,   4.2496,   4.4618,   4.5225, &
     4.7342,   4.4618,   4.5710,   4.5577,   4.5225,   4.9396, &
     4.6106,   4.5463,   4.3383,   4.5820,   4.9396,   4.4207, &
     4.3734,   4.5959,   4.5959,   4.5128,   4.4299,   4.3379, &
     4.6355,   4.6171,   4.4388,   4.5920,   4.4911,   4.4911, &
     4.4207,   4.0322,   3.7512,   3.7301,   4.4207,   4.4772, &
     4.0322,   4.6067,   4.3609,   3.9613,   4.0322,   4.1352/
!
DATA HT2/ &
    -5.0078,  -4.2964,  -6.4095,  -5.0078,  -6.8801,  -4.2964, &
    -6.1896,  -5.6123,  -5.6123,  -4.7903,  -4.5709,  -4.5383, &
    -4.7903,  -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.9918, &
    -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.9918,  -4.7206, &
    -6.2945,  -4.5314,  -4.5314,  -8.1838,  -3.8214,  -5.6123, &
    -6.6706,  -4.0922,  -7.2732,  -4.9918,  -5.1435,  -7.7428, &
    -4.5257,  -4.5257,  -7.4847,  -4.5257,  -4.9605,  -5.0903, &
    -4.5431,  -4.5314,  -6.4497,  -6.2945,  -5.2287,  -4.8896, &
    -5.2205,  -4.8698,  -4.9401,  -4.3503,  -5.1193,  -6.2674, &
    -4.9401,  -5.7613,  -4.8061,  -4.8061,  -4.8786,  -4.9401, &
    -6.2674,  -4.8786,  -6.0922,  -4.9595,  -4.9401,  -8.1838, &
    -5.4380,  -5.2287,  -4.5018,  -5.0903,  -8.1838,  -5.1435, &
    -5.3135,  -6.4497,  -6.4497,  -4.9918,  -4.9920,  -3.8214, &
    -5.2776,  -6.2684,  -4.0872,  -5.1719,  -5.7928,  -5.7928, &
    -5.1435,  -3.0833,  -2.5539,  -2.7758,  -5.1435,  -4.7206, &
    -3.0833,  -5.2030,  -4.1423,  -3.1993,  -3.0833,  -3.7450/
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


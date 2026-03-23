BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! CR $Id$
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
INCLUDE 'GGCOM.f90'
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
INCLUDE 'KEYCOM.f90'
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
!
!----------
INTEGER I,J
!----------
!  SPECIES ORDER:
!   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
!  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
!  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
!  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
!
!  SPECIES EXPANSION:
!  UJ,AJ,RM,OJ,ER USE CR JU
!  NC,PW USE CR CO
!  GO,AW,EM,BK,SO USE CR OA
!  PB USES CR AS
!  PM,PD,AZ USE CR PI
!  CI USES CR PP
!----------
!    AL COMMON                  FIA SCIENTIFIC
!  # CD NAME                    CD  NAME
! -- -- ---------------------   --- -----------------------------------
!  1 AF SUBALPINE FIR           019 ABIES LASIOCARPA
!  2 CB CORKBARK FIR            018 ABIES LASIOCARPA var. ARIZONICA
!  3 DF DOUGLAS-FIR             202 PSEUDOTSUGA MENZIESII
!  4 GF GRAND FIR               017 ABIES GRANDIS
!  5 WF WHITE FIR               015 ABIES CONCOLOR
!  6 MH MOUNTAIN HEMLOCK        264 TSUGA MERTENSIANA
!  7 RC WESTERN REDCEDAR        242 THUJA PLICATA
!  8 WL WESTERN LARCH           073 LARIX OCCIDENTALIS
!  9 BC BRISTLECONE PINE        102 PINUS ARISTATA
! 10 LM LIMBER PINE             113 PINUS FLEXILIS
! 11 LP LODGEPOLE PINE          108 PINUS CONTORTA
! 12 PI TWONEEDLE PINYON        106 PINUS EDULIS
! 13 PP PONDEROSA PINE          122 PINUS PONDEROSA
! 14 WB WHITEBARK PINE          101 PINUS ALBICAULIS
! 15 SW SOUTHWESTERN WHITE PINE 114 PINUS STROBIFORMUS
! 16 UJ UTAH JUNIPER            065 JUNIPERUS OSTEOSPERMA
! 17 BS BLUE SPRUCE             096 PICEA PUNGENS
! 18 ES ENGELMANN SPRUCE        093 PICEA ENGELMANNII
! 19 WS WHITE SPRUCE            094 PICEA GLAUCA
! 20 AS QUAKING ASPEN           746 POPULUS TREMULOIDES
! 21 NC NARROWLEAF COTTONWOOD   749 POPULUS ANGUSTIFOLIA
! 22 PW PLAINS COTTONWOOD       745 POPULUS DELTOIDES var. MONOLIFERA
! 23 GO GAMBEL OAK              814 QUERCUS GAMBELII
! 24 AW ARIZONA WHITE OAK       803 QUERCUS ARIZONICA
! 25 EM EMORY OAK               810 QUERCUS EMORYI
! 26 BK BUR OAK                 823 QUERCUS MACROCARPA
! 27 SO SILVERLEAF OAK          843 QUERCUS HYPOLEUCOIDES
! 28 PB PAPER BIRCH             375 BETULA PAPYRIFERA
! 29 AJ ALLIGATOR JUNIPER       063 JUNIPERUS DEPPEANA
! 30 RM ROCKY MOUNTAIN JUNIPER  066 JUNIPERUS SCOPULORUM
! 31 OJ ONESEED JUNIPER         069 JUNIPERUS MONOSPERMA
! 32 ER EASTERN REDCEDAR        068 JUNIPERUS VIRGINIANA
! 33 PM SINGLELEAF PINYON       133 PINUS MONOPHYLLA
! 34 PD BORDER PINYON           134 PINUS DISCOLOR
! 35 AZ ARIZONA PINYON PINE     143 PINUS MONOPHYLLA var. FALLAX
! 36 CI CHIHUAHUAN PINE         118 PINUS LEIOPHYLLA
! 37 OS OTHER SOFTWOOD          299
! 38 OH OTHER HARDWOOD          998
!----------
!
DATA COR2 /MAXSP*1./, HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/, &
        BKRAT/MAXSP*0./
!
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /
!
DATA YR / 10.0 /, IRECNT/ 0 /,ICCODE/0/
!
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
!----------
!     DATA STATEMENT FOR ESCOMN
!----------
DATA XMIN/ 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, &
              1.0, 0.5, 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 0.5, 3.0, &
              3.0, 3.0, 0.5, 0.5, 0.5, 0.5, 0.5, 3.0, 0.5, 0.5, &
              0.5, 0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 0.5 /
!
DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
!
DATA ISPSPE/20,21,22,23,24,25,26,27,28,29,36/
!
DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325, &
     1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
!
DATA HHTMAX/ 7.0, 7.0,10.0, 7.0, 7.0,10.0, 9.0,10.0, 9.0, 9.0, &
               10.0, 6.0,10.0, 9.0, 9.0, 6.0, 7.0, 7.0, 7.0,16.0, &
               16.0,16.0,10.0,10.0,10.0,10.0,10.0,16.0, 6.0, 6.0, &
                6.0, 6.0, 6.0, 6.0, 6.0,10.0, 9.0,12.0/
!
DATA IFORCD/ 202, 203, 204, 206, 207, 209, 210, 211, 212, 213, &
    214, 215, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 312/
!
DATA IFORST/  23*1 /
!
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
!     (DIMENSIONED (16,MAXSP) WITH THE 16 BEING THE HABITAT TYPE GROUP
!      AS SHOWN IN TABLE 3, PG 6, GTR INT-279) WHICH DOES NOT PERTAIN
!      TO THE CR VARIANT)
!
DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0/
!
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
!     (DIMENSIONED 23,MAXSP WITH THE 23 BEING 23 FOREST CODES SHOWN
!      IN ARRAY IFORCD ABOVE AND MAPPED AS SHOWN IN ARRAY IFORST)
!
DATA ((OCURNF(I,J),I=1,23),J=1,MAXSP)/ &
     23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, &
     23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, &
     23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, &
     23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, &
     23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0, 23*0.0/
!----------
!     COMMON STATEMENT FOR PLOT VARIABLES.
!----------
DATA JSP / &
    'AF ',   'CB ',   'DF ',   'GF ',   'WF ',   'MH ',   'RC ', &
    'WL ',   'BC ',   'LM ',   'LP ',   'PI ',   'PP ',   'WB ', &
    'SW ',   'UJ ',   'BS ',   'ES ',   'WS ',   'AS ',   'NC ', &
    'PW ',   'GO ',   'AW ',   'EM ',   'BK ',   'SO ',   'PB ', &
    'AJ ',   'RM ',   'OJ ',   'ER ',   'PM ',   'PD ',   'AZ ', &
    'CI ',   'OS ',   'OH '/
!
DATA FIAJSP / &
    '019',   '018',   '202',   '017',   '015',   '264',   '242', &
    '073',   '102',   '113',   '108',   '106',   '122',   '101', &
    '114',   '065',   '096',   '093',   '094',   '746',   '749', &
    '745',   '814',   '803',   '810',   '823',   '843',   '375', &
    '063',   '066',   '069',   '068',   '133',   '134',   '143', &
    '118',   '299',   '998'/
!
DATA PLNJSP / &
    'ABLA  ','ABLAA ','PSME  ','ABGR  ','ABCO  ','TSME  ','THPL  ', &
    'LAOC  ','PIAR  ','PIFL2 ','PICO  ','PIED  ','PIPO  ','PIAL  ', &
    'PIST3 ','JUOS  ','PIPU  ','PIEN  ','PIGL  ','POTR5 ','POAN3 ', &
    'PODEM ','QUGA  ','QUAR  ','QUEM  ','QUMA2 ','QUHY  ','BEPA  ', &
    'JUDE2 ','JUSC2 ','JUMO  ','JUVI  ','PIMO  ','PIDI3 ','PIMOF ', &
    'PILE  ','2TN   ','2TB   '/
!
DATA JTYPE /122*0/
!
DATA NSP    /'AF1','CB1','DF1','GF1','WF1','MH1','RC1','WL1', &
                'BC1','LM1','LP1','PI1','PP1','WB1','SW1','UJ1', &
                'BS1','ES1','WS1','AS1','NC1','PW1','GO1','AW1', &
                'EM1','BK1','SO1','PB1','AJ1','RM1','OJ1','ER1', &
                'PM1','PD1','AZ1','CI1','OS1','OH1', &
!
                'AF2','CB2','DF2','GF2','WF2','MH2','RC2','WL2', &
                'BC2','LM2','LP2','PI2','PP2','WB2','SW2','UJ2', &
                'BS2','ES2','WS2','AS2','NC2','PW2','GO2','AW2', &
                'EM2','BK2','SO2','PB2','AJ2','RM2','OJ2','ER2', &
                'PM2','PD2','AZ2','CI2','OS2','OH2', &
!
                'AF3','CB3','DF3','GF3','WF3','MH3','RC3','WL3', &
                'BC3','LM3','LP3','PI3','PP3','WB3','SW3','UJ3', &
                'BS3','ES3','WS3','AS3','NC3','PW3','GO3','AW3', &
                'EM3','BK3','SO3','PB3','AJ3','RM3','OJ3','ER3', &
                'PM3','PD3','AZ3','CI3','OS3','OH3'/
!----------
!   COMMON STATEMENT FOR COEFFS VARIABLES
!----------
DATA HT1/ &
      4.4717,   4.4717,   4.5879,   5.0271,   4.3008,   4.8740, &
      5.1631,   5.1631,   4.1920,   4.1920,   4.3767,   4.1920, &
      4.6024,   4.1920,   5.1999,   4.1920,   4.5293,   4.5293, &
      4.5293,   4.4421,   4.4421,   4.4421,   4.1920,   4.1920, &
      4.1920,   4.1920,   4.1920,   4.4421,   4.1920,   4.1920, &
      4.1920,   4.1920,   4.1920,   4.1920,   4.1920,   4.6024, &
      4.2597,   4.4421/
DATA HT2/ &
     -6.7387,  -6.7387,  -8.9277, -11.2168,  -6.8139, -10.4050, &
     -9.2566,  -9.2566,  -5.1651,  -5.1651,  -6.1281,  -5.1651, &
    -11.4693,  -5.1651,  -9.2672,  -5.1651,  -7.7725,  -7.7725, &
     -7.7725,  -6.5405,  -6.5405,  -6.5405,  -5.1651,  -5.1651, &
     -5.1651,  -5.1651,  -5.1651,  -6.5405,  -5.1651,  -5.1651, &
     -5.1651,  -5.1651,  -5.1651,  -5.1651,  -5.1651, -11.4693, &
     -9.3949,  -6.5405/
!
DATA REGNBK/0.499/
!
DATA S0/55329D0/,SS/55329./
!
DATA LSCRN,JOSCRN/.FALSE.,6/
!
DATA JOSUME/13/
!
DATA KOLIST,FSTOPEN /27,.FALSE./
!
!----------
!   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
!   VARIABLES B0ACCF B1ACCF B0BCCF B1BCCF B0ASTD B1BSTD
!   ARE ONLY USED IN THE EM & TT VARIANTS
!----------
DATA B0ACCF/ MAXSP*0.0 /
DATA B1ACCF/ MAXSP*0.0 /
DATA B0BCCF/ MAXSP*0.0 /
DATA B1BCCF/ MAXSP*0.0 /
DATA B0ASTD/ MAXSP*0.0 /
DATA B1BSTD/ MAXSP*0.0 /
!----------
! COMMON STATEMENT FOR GGCOM VARIABLES
!----------
DATA IGFOR/ 13 /
DATA BREAK/ &
      1.,   1.,   1.,   3.,   1.,   3.,   3.,   3.,  99.,   2., &
      1.,  99.,   1.,   3.,   1.,  99.,   1.,   1.,   1.,   1., &
      1.,   1.,  99.,  99.,  99.,  99.,  99.,   1.,  99.,  99., &
     99.,  99.,  99.,  99.,  99.,   1.,   1.,   1./
DATA SITELO/ &
     40.,  30.,  40.,  30.,  40.,  40.,  20.,  40.,  20.,  10., &
     30.,   6.,  30.,  20.,  30.,   6.,  30.,  40.,  30.,  20., &
     30.,  30.,   6.,   6.,   6.,   6.,   6.,  20.,   6.,   6., &
      6.,   6.,   6.,   6.,   6.,  30.,  30.,  20./
DATA SITEHI/ &
    105., 100., 120., 130., 105.,  70., 125., 120.,  60.,  60., &
     95.,  40., 100.,  60., 130.,  30., 110., 120.,  85., 100., &
    120., 120.,  40.,  40.,  40.,  40.,  40., 100.,  30.,  30., &
     30.,  30.,  40.,  40.,  40., 100.,  95., 100./
!
END

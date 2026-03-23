BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! EM $Id$
!----------
!
!     SEE **MAIN** FOR DICTIONARY OF VARIABLE NAMES.
!
!----------
!     COMMON STATEMENT FOR MODEL COEFFICIENTS WHICH ARE HABITAT
!     AND SITE DEPENDENT.
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'ESPARM.f90'
!
!
INCLUDE 'ESCOMN.f90'
!
!
INCLUDE 'COEFFS.f90'
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
INCLUDE 'OPCOM.f90'
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
!  SPECIES ORDER:
!   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
!  17=PB, 18=OS, 19=OH
!
!  SPECIES EXPANSION
!  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
!  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
!  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
!  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
!  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
!----------
!    AL COMMON                FIA SCIENTIFIC
!  # CD NAME                  CD  NAME
! -- -- --------------------- --- -----------------------------------
!  1 WB WHITEBARK PINE        101 PINUS ALBICALA (WB FROM EM11)
!  2 WL WESTERN LARCH         073 LARIX OCCIDENTALIS (WL FROM EM11)
!  3 DF DOUGLAS-FIR           202 PSEUDOTSUGA MENZIESII (DF FROM EM11)
!  4 LM LIMBER PINE           113 PINUS FLEXILIS (LM FROM TT)
!  5 LL SUBALPINE LARCH       072 LARIX LYALLII (AF FROM IE)
!  6 RM ROCKY MTN JUNIPER     066 JUNIPERUS SCOPULORUM (WJ FROM UT)
!  7 LP LODGEPOLE PINE        108 PINUS CONTORTA (LP FROM EM11)
!  8 ES ENGELMANN SPRUCE      093 PICEA ENGELMANNII (ES FROM EM11)
!  9 AF SUBALPINE FIR         019 ABIES LASIOCARPA (AF FROM EM11)
! 10 PP PONDEROSA PINE        122 PINUS PONDEROSA (PP FROM EM11)
! 11 GA GREEN ASH             544 FRAXINUS PENNSYLVANICA (CO FROM IE)
! 12 AS QUAKING ASPEN         746 POPULUS TREMULOIDES (AS FROM UT)
! 13 CW BLACK COTTONWOOD      747 POPULUS BALSAMIFERA SSP.
!                                 TRICHOCARPA (CO FROM CR)
! 14 BA BALSAM POPLAR         741 POPULUS BALSAMIFERA (CO FROM CR)
! 15 PW PLAINS COTTONWOOD     745 POPULUS DELTOIDES SSP. MONOLIFERA
!                                 (CO FROM CR)
! 16 NC NARROWLEAF COTTONWOOD 749 POPULUS ANGUSTIFOLIA (CO FROM CR)
! 17 PB PAPER BIRCH           375 BETULA PAPYRIFERA (AS FROM UT)
! 18 OS OTHER SOFTWOOD        299                   (WB FROM EM11)
! 19 OH OTHER HARDWOOD        998                   (CO FROM CR)
!----------
!
DATA  BKRAT/MAXSP*0./
!----------
!     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
!----------
DATA COR2/MAXSP*1./, HCOR2/MAXSP*1./, RCOR2/MAXSP*1./
!
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /
!
DATA YR/10.0/, IRECNT/0/, ICCODE/0/
!
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE /15,2,3,16,4,8/
!----------
!   COMMON STATEMENT FOR ESCOMN VARIABLE
!----------
DATA XMIN/1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, 3.0, &
   6.0, 3.0, 3.0, 3.0, 3.0, 6.0, 0.5, 3.0/
!
DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/, &
     ISPSPE/11, 12, 13, 14, 15, 16, 17/, &
     BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325, &
     1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/, &
     HHTMAX/23.0, 27.0, 21.0, 27.0, 18.0,  6.0, 24.0, 18.0, 18.0, &
      17.0, 16.0, 16.0, 16.0, 16.0, 16.0, 16.0, 16.0, 22.0, 16.0/, &
     IFORCD/103,104,105,106,621,110,113,114,116,117, &
            118,109,111,112,412,402,108,102,115,  0/, &
     IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17, &
              4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
!
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
!     (DIMENSIONED 16,MAXSP WITH THE 16 BEING THE HABITAT TYPE GROUP
!      AS SHOWN IN TABLE 3, PG 6, GTR INT-279)
!
DATA ((OCURHT(I,J),I=1,16),J=1,2)/ &
     0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 0., 1., 1., 0., 1., 0., &
     1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 0., 1., 0./
DATA ((OCURHT(I,J),I=1,16),J=3,4)/ &
     1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=5,6)/ &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=7,8)/ &
     1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., &
     0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1./
DATA ((OCURHT(I,J),I=1,16),J=9,10)/ &
     0., 0., 0., 0., 1., 1., 1., 0., 1., 1., 1., 1., 1., 1., 1., 1., &
     1., 1., 1., 1., 1., 1., 1., 1., 1., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=11,12)/ &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=13,14)/ &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=15,16)/ &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
DATA ((OCURHT(I,J),I=1,16),J=17,MAXSP)/ &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
     0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
!
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
!     (DIMENSIONED 20,MAXSP WITH THE 20 BEING 20 FOREST CODES SHOWN
!      IN ARRAY IFORCD ABOVE AND MAPPED AS SHOWN IN ARRAY IFORST)
!
DATA ((OCURNF(I,J),I=1,20),J=1,2)/ &
       0.,0.,0.,1.,1.,0.,1.,0.,0.,1.,0.,0.,0.,1.,0.,1.,0.,0.,0.,0., &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,1.,0.,0.,0.,1.,0.,1.,1.,0.,1.,0./
DATA ((OCURNF(I,J),I=1,20),J=3,4)/ &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,1.,1.,1.,0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
DATA ((OCURNF(I,J),I=1,20),J=5,6)/ &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
DATA ((OCURNF(I,J),I=1,20),J=7,8)/ &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,1.,1.,1.,0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,1.,1.,1.,0.,1.,0.,1.,1.,0.,1.,1./
DATA ((OCURNF(I,J),I=1,20),J=9,10)/ &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,1.,1.,1.,0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,1.,1.,1.,0.,1.,0.,1.,0.,0.,0.,0.,1.,0.,1.,1.,0.,1.,1./
DATA ((OCURNF(I,J),I=1,20),J=11,12)/ &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
DATA ((OCURNF(I,J),I=1,20),J=13,14)/ &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
DATA ((OCURNF(I,J),I=1,20),J=15,16)/ &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
DATA ((OCURNF(I,J),I=1,20),J=17,MAXSP)/ &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
!----------
!  COMMON STATEMENT FOR PLOT VARIABLES.
!----------
!
DATA JSP / &
    'WB ',   'WL ',   'DF ',   'LM ',   'LL ',   'RM ',   'LP ', &
    'ES ',   'AF ',   'PP ',   'GA ',   'AS ',   'CW ',   'BA ', &
    'PW ',   'NC ',   'PB ',   'OS ',   'OH '/
!
DATA FIAJSP / &
    '101',   '073',   '202',   '113',   '072',   '066',   '108', &
    '093',   '019',   '122',   '544',   '746',   '747',   '741', &
    '745',   '749',   '375',   '299',   '998'/
!
DATA PLNJSP / &
    'PIAL  ','LAOC  ','PSME  ','PIFL2 ','LALY  ','JUSC2 ','PICO  ', &
    'PIEN  ','ABLA  ','PIPO  ','FRPE  ','POTR5 ','POBAT ','POBA2 ', &
    'PODEM ','POAN3 ','BEPA  ','2TN   ','2TB   '/
!
DATA JTYPE / &
     10, 65, 70, 74, 79, 91, 92, 93, 95,100, &
    110,120,130,140,141,161,170,171,172,180, &
    181,182,200,210,220,221,230,250,260,261, &
    262,280,281,282,283,290,291,292,293,310, &
    311,312,313,315,320,321,322,323,330,331, &
    332,340,350,360,370,371,400,410,430,440, &
    450,460,461,470,480,591,610,620,624,625, &
    630,632,640,641,642,650,651,653,654,655, &
    660,661,662,663,670,674,690,691,692,700, &
    710,720,730,731,732,733,740,750,751,770, &
    780,790,791,792,810,820,830,832,850,860, &
    870,900,910,920,930,940,950,999,4*0 /
!
DATA NSP /'WB1','WL1','DF1','LM1','LL1','RM1','LP1','ES1','AF1', &
    'PP1','GA1','AS1','CW1','BA1','PW1','NC1','PB1','OS1','OH1', &
             'WB2','WL2','DF2','LM2','LL2','RM2','LP2','ES2','AF1', &
    'PP2','GA2','AS2','CW2','BA2','PW2','NC2','PB2','OS2','OH2', &
             'WB3','WL3','DF3','LM3','LL3','RM3','LP3','ES3','AF3', &
    'PP3','GA3','AS3','CW3','BA3','PW3','NC3','PB3','OS3','OH3'/
!----------
!  COMMON STATEMENT FOR COEFFS VARIABLES
!----------
DATA HT1/ &
    4.1539, 4.1539, 4.4161, 4.1920, 4.76537,    3.2, 4.5356, 4.7537, &
    4.5788, 4.414,  4.4421, 4.4421,  4.4421, 4.4421, 4.4421, 4.4421, &
    4.4421, 4.1539, 4.4421/
DATA HT2/ &
    -4.212, -4.212, -6.962,-5.1651,-7.61062,   -5.0, -5.692, -8.356, &
    -7.138, -8.907,-6.5405,-6.5405, -6.5405,-6.5405,-6.5405,-6.5405, &
   -6.5405, -4.212,-6.5405/
!
DATA SIGMAR/ &
    0.11645, 0.11645, 0.14465, 0.4671,  0.4345,  0.2,     0.14465, &
    0.15850, 0.14465, 0.13420, 0.2,     0.3750,  0.2,     0.2, &
    0.2,     0.2,     0.3750,  0.11645, 0.2   /
!----------
!   DATA STATEMENTS FOR VARIABLES IN VARCOM COMMON BLOCK
!----------
DATA B0ACCF/ &
     1.17527,1.17527,-4.35709,3*0.0,-0.90086,-0.55052, &
     -4.35709,0.405,7*0.0,1.17527,0.0/
DATA B1ACCF/ &
     -0.42124,-0.42124,0.67307,3*0.0,0.16996,-0.02858, &
     0.67307,0.,7*0.0,-0.42124,0.0/
DATA B0BCCF/ &
     -2.56002,-2.56002,-2.49682,3*0.0,-1.50963,-2.26007, &
    -2.49682,-1.50963,7*0.0,-2.56002,0.0/
DATA B1BCCF/ &
     -0.58642,-0.58642,-0.51938,3*0.0,-0.61825,-0.67115, &
    -0.51938,-0.61825,7*0.0,-0.58642,0.0/
DATA B0ASTD/ &
    1.08720,1.08720,1.13785,3*0.0,1.00749,1.09730,1.13785, &
    0.57707,7*0.0,1.08720,0.0/
DATA B1BSTD/ &
     -0.00230,-0.00230,-0.00185,3*0.0,-0.00435,-0.00130, &
    -0.00185,0.00055,7*0.0,-0.00230,0.0/
!
DATA REGNBK/2.999/
!
DATA S0/55329D0/,SS/55329./
!
DATA LSCRN,JOSCRN/.FALSE., 6/
!
DATA JOSUME/13/
!
DATA KOLIST,FSTOPEN /27,.FALSE./
!
END

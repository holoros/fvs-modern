BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! BM $Id$
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ESPARM.f90'
!
!
INCLUDE 'COEFFS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ECON.f90'
!
!
INCLUDE 'ESCOMN.f90'
!
!
INCLUDE 'FVSSTDCM.f90'
!
!
INCLUDE 'HTCAL.f90'
!
!
INCLUDE 'PDEN.f90'
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
!OMMONS
!
!----------
!  VARIABLE DECLARATIONS:
!----------
!
INTEGER I,J
!
!----------
!  DATA STATEMENTS.
!
!  SPECIES ORDER:
!   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
!   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
!  17=OS, 18=OH
!----------
DATA BKRAT/MAXSP*0./
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
DATA XMIN/0.9, 1.7, 1.0, 1.0, 0.5, 0.5, 1.3, 0.5, 0.5, 1.0, &
             1.0, 1.0, 1.0, 1.0, 6.0, 1.0, 1.0, 1.0/
DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/
DATA ISPSPE/13,15,16/
DATA BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232,1.278,1.325, &
     1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696,1.742,1.789/
DATA HHTMAX/23., 27., 21., 21., 22., 6.0, 24., 18., 18., 17., &
               23., 9.0, 20., 20., 16., 20., 17., 20./
DATA IFORCD/103, 104, 105, 106, 621, 110, 113, 114, 116, 117, &
               118, 109, 111, 112, 412, 402, 108, 102, 115,   0/
DATA IFORST/  3,   4,   5,   4,   7,  10,   4,  14,  16,  17, &
                 4,   9,  11,  12,  19,  20,  11,   9,  12,   4/
!
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
!     DIMENSIONED AT (16,MAXSP) WHERE THE FIRST INDEX IS A HABITAT TYPE.
!     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
!
DATA ((OCURHT(I,J),I=1,16),J=1,18)/ &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, 16*0.0, &
     16*0.0, 16*0.0/
!
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
!     DIMENSIONED AT (20,MAXSP) WHERE THE FIRST INDEX IS A NATIONAL FOREST.
!     NONE OF THE SPECIES IN THIS VARIANT HAVE NATURAL REGEN.
!
DATA ((OCURNF(I,J),I=1,20),J=1,18)/ &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, 20*0.0, &
     20*0.0, 20*0.0/
!----------
!     COMMON STATEMENT FOR PLOT VARIABLES.
!
!     SPECIES LIST FOR BLUE MOUNTAINS VARIANT.
!
!     1 = WESTERN WHITE PINE   119 WP  PIMO3  PINUS MONTICOLA
!     2 = WESTERN LARCH        073 WL  LAOC   LARIX OCCIDENTALIS
!     3 = DOUGLAS-FIR          202 DF  PSME   PSEUDOTSUGA MENZIESII
!     4 = GRAND FIR            017 GF  ABGR   ABIES GRANDIS
!     5 = MOUNTAIN HEMLOCK     264 MH  TSME   TSUGA MERTENSIANA
!     6 = WESTERN JUNIPER      064 WJ  JUOC   JUNIPERUS OCCIDENTALIS
!                                             (WJ FROM UT)
!     7 = LODGEPOLE PINE       108 LP  PICO   PINUS CONTORTA
!     8 = ENGLEMANN SPRUCE     093 ES  PIEN   PICEA ENGELMANNII
!     9 = SUBALPINE FIR        019 AF  ABLA   ABIES LASIOCARPA
!    10 = PONDEROSA PINE       122 PP  PIPO   PINUS PONDEROSA
!    11 = WHITEBARK PINE       101 WB  PIAL   PINUS ALBICAULIS
!                                             (WB FROM TT)
!    12 = LIMBER PINE          113 LM  PIFL2  PINUS FLEXILIS
!                                             (LM FROM UT)
!    13 = PACIFIC YEW          231 PY  TABR2  TAXUS BREVIFOLIA
!                                             (PY FROM WC)
!    14 = ALASKA CEDAR         042 YC  CANO9  CALLITROPSIS NOOTKATENSIS
!                                             (YC FROM WC)
!    15 = QUAKING ASPEN        746 AS  POTR5  POPULUS TREMULOIDES
!                                             (AS FROM UT)
!    16 = BLACK COTTONWOOD     747 CW  POBAT  POPULUS BALSAMIFERA var.
!                                             TRICHOCARPA (CW FROM WC)
!    17 = OTHER SOFTWOOD       299 OS  2TN    (PP FROM BM)
!    18 = OTHER HARDWOOD       998 OH  2TB    (OT FROM WC)
!----------
DATA JSP / &
    'WP ',   'WL ',   'DF ',   'GF ',   'MH ',   'WJ ',   'LP ', &
    'ES ',   'AF ',   'PP ',   'WB ',   'LM ',   'PY ',   'YC ', &
    'AS ',   'CW ',   'OS ',   'OH '/
!
DATA FIAJSP / &
    '119',   '073',   '202',   '017',   '264',   '064',   '108', &
    '093',   '019',   '122',   '101',   '113',   '231',   '042', &
    '746',   '747',   '299',   '998'/
!
DATA PLNJSP / &
    'PIMO3 ','LAOC  ','PSME  ','ABGR  ','TSME  ','JUOC  ','PICO  ', &
    'PIEN  ','ABLA  ','PIPO  ','PIAL  ','PIFL2 ','TABR2 ','CANO9 ', &
    'POTR5 ','POBAT ','2TN   ','2TB   '/
!
DATA JTYPE /130,170,250,260,280,290,310,320,330,420, &
               470,510,520,530,540,550,570,610,620,640, &
               660,670,680,690,710,720,730,830,850,999,92*0 /
!
DATA NSP /'WP1','WL1','DF1','GF1','MH1','WJ1','LP1','ES1','AF1', &
             'PP1','WB1','LM1','PY1','YC1','AS1','CW1','OS1','OH1', &
             'WP2','WL2','DF2','GF2','MH2','WJ2','LP2','ES2','AF2', &
             'PP2','WB2','LM2','PY2','YC2','AS2','CW2','OS2','OH2', &
             'WP3','WL3','DF3','GF3','MH3','WJ3','LP3','ES3','AF3', &
             'PP3','WB3','LM3','PY3','YC3','AS3','CW3','OS3','OH3'/
!----------
!   COMMON STATEMENT FOR COEFFS VARIABLES
!----------
DATA HT1/ &
      5.035,   5.043,   4.929,   4.874,   4.874, &
     3.2000,   4.954,   5.035,   4.875,   4.993, &
     4.1920,  4.1920,  5.1880,   5.143,  4.4421, &
     5.1520,   4.993,  5.1520/
DATA HT2/ &
    -10.674,  -9.123, -10.744, -10.405, -10.405, &
    -5.0000,  -9.177, -10.674,  -9.568, -12.430, &
    -5.1651, -5.1651,-13.8010, -13.497, -6.5405, &
   -13.5760, -12.430,-13.5760/
!
DATA SIGMAR/ &
    0.5670, 0.3383, 0.2580, 0.2548, 0.5571, &
   0.34663, 0.2820, 0.3348, 0.3249, 0.2745, &
   0.34663,0.46710, 0.4842, 0.3931,0.34663, &
    0.5357, 0.2745, 0.5357/
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

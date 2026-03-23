BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! AK $Id$
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
INCLUDE 'KEYCOM.f90'
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
! SPECIES LIST FOR ALASKA VARIANT.
!
! Number Code  Common Name         FIA  PLANTS Scientific Name
!   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
!   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
!   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
!   4     TA   tamarack            071  LALA   Larix laricina
!   5     WS   white spruce        094  PIGL   Picea glauca
!   6     LS   hybrid spruce            PILU   Picea lutzii
!   7     BE   black spruce        095  PIMA   Picea mariana
!   8     SS   Sitka spruce        098  PISI   Picea sitchensis
!   9     LP   lodgepole pine      108  PICO   Pinus contorta
!  10     RC   western redcedar    242  THPL   Thuja plicata
!  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
!  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
!  13     OS   other softwood      299  2TN
!  14     AD   alder               350  ALNUS  Alnus
!  15     RA   red alder           351  ALRU2  Alnus rubra
!  16     PB   paper birch         375  BEPA   Betula papyrifera
!  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
!  18     BA   balsam poplar       741  POBA2  Populus balsamifera
!  19     AS   quaking aspen       746  POTR5  Populus tremuloides
!  20     CW   black cottonwood    747  POBAT  Populus balsamifera
!                                              ssp. trichocarpa
!  21     WI   willow              920  SALIX  Salix
!  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
!  23     OH   other hardwood      998  2TB
!
!----------
!  VARIABLE DECLARATIONS:
!----------
INTEGER I,J
!----------
!  DATA STATEMENTS FOR COMMON CONCHR VARIABLES
!----------
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /

!----------
!  DATA STATEMENTS FOR COMMON CONTRL VARIABLES
!----------
DATA RCOR2 /MAXSP*1.0/
DATA ICCODE /0/, IRECNT /0/, YR /10.0/
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /

!----------
!  DATA STATEMENTS FOR COMMON COEFFS VARIABLES
!----------
DATA  BKRAT /MAXSP*0./, COR2 /MAXSP*1./
!----------
!  DATA STATEMENTS FOR COMMON HTCAL VARIABLES
!----------
DATA  HCOR2 /MAXSP*1./

!----------
!  DATA STATEMENTS FOR COMMON ESCOMN VARIABLES
!----------
DATA XMIN/ 1.00,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.00,0.50, &
              0.50,0.50,0.50,1.00,1.00,1.00,1.00,1.00,1.00,1.00, &
              1.00,1.00,1.00 /

DATA HHTMAX/ 20.0,20.0,12.0,13.0,13.0,13.0,13.0,20.0,9.0,15.0, &
   19.0,10.0,13.0,7.6,30.0,20.0,20.0,16.9,18.0,16.9,7.6,11.0,7.6 /
DATA DBHMID /1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0 /, &
     BNORML/ 3*1.0,1.046,1.093,1.139,1.186,1.232, &
     1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696, &
     1.742,1.789 /

!  REGENERATION MODELING NEEDS
!  SPROUTING SPECIES INDEX LIST
DATA ISPSPE/ 14,15,16,17,18,19,20,21,22 /
!
!  FULL ESTABLISHMENT MODEL VARIABLES
!
DATA IFORCD/ 20*0 /  ! NOT USED IN AK
DATA IFORST/ 20*0 /  ! NOT USED IN AK

!    OCURHT SETS SPECIES OCCURANCE BY HABITAT TYPE GROUP (0-NO, 1-YES)
!    OCURHT DIMENSIONED (16,MAXSP)
!
DATA ((OCURHT(I,J),I=1,16),J=1,MAXSP)/ 368 * 0.0 /  ! NOT USED IN AK

!    OCURNF SETS SPECIES OCCURANCE BY LOCATION (0-NO, 1-YES)
!    OCURNF DIMENSIONED (20,MAXSP)
!
DATA ((OCURNF(I,J),I=1,20),J=1,MAXSP)/ 460 * 0.0 /  ! NOT USED IN AK

!    OCURFT SETS SPECIES OCCURANCE BY FOREST TYPE (0-NO, 1-YES)
!    OCURFT DIMENSIONED (MAXSP (23),FOREST TYPE (14))
!
DATA ((OCURFT(I,J),I=1,MAXSP),J=1,14)/ &
    0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, &  ! 122 WHITE SPRUCE
    0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &  ! 125 BLACK SPRUCE
    0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, &  ! 270 MOUNTAIN HEMLOCK
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &  ! 271 ALASKA CEDAR
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &  ! 281 LODGEPOLE PINE
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &  ! 301 WESTERN HEMLOCK
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &  ! 304 WESTERN REDCEDAR
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, &  ! 305 SITKA SPRUCE
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, &  ! 703 COTTONWOOD
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &  ! 901 ASPEN
    0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, &  ! 902 PAPER BIRCH
    0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, &  ! 904 BALSAM POPLAR (MAPPED TO WHITE SPRUCE)
    0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, &  ! 911 RED ALDER
    0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
!
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &  ! OTHER F.T.
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /

!    DATA STATEMENTS FOR COMMON PDEN VARIABLES
!    REGNBK IS THE BREAKPOINT DIAMETER FOR TREES GOING INTO PDEN
!    VECTORS. SET TO 1.0 BASED ON FIA DATA.
DATA REGNBK/1.000/

!----------
!  DATA STATEMENTS FOR COMMONS PLTCHR AND PLOT VARIABLES
!----------
DATA JSP / &
    'SF ', 'AF ', 'YC ', 'TA ', 'WS ', 'LS ', 'BE ', 'SS ', &
    'LP ', 'RC ', 'WH ', 'MH ', 'OS ', 'AD ', 'RA ', 'PB ', &
    'AB ', 'BA ', 'AS ', 'CW ', 'WI ', 'SU ', 'OH '/

DATA FIAJSP / &
    '011', '019', '042', '071', '094', '   ', '095', '098', &
    '108', '242', '263', '264', '299', '350', '351', '375', &
    '376', '741', '746', '747', '920', '928', '998'/

DATA PLNJSP / &
    'ABAM  ', 'ABLA  ', 'CANO9 ', 'LALA  ', 'PIGL  ', 'PILU  ', &
    'PIMA  ', 'PISI  ', 'PICO  ', 'THPL  ', 'TSHE  ', 'TSME  ', &
    '2TN   ', 'ALNUS ', 'ALRU2 ', 'BEPA  ', 'BENE4 ', 'POBA2 ', &
    'POTR5 ', 'POBAT ', 'SALIX ', 'SASC  ', '2TB   '/

DATA JTYPE /130,170,250,260,280,290,310,320,330,420, &
               470,510,520,530,540,550,570,610,620,640, &
               660,670,680,690,710,720,730,830,850,999,92*0 /

DATA NSP / &
    'SF1', 'AF1', 'YC1', 'TA1', 'WS1', 'LS1', 'BE1', 'SS1', &
    'LP1', 'RC1', 'WH1', 'MH1', 'OS1', 'AD1', 'RA1', 'PB1', &
    'AB1', 'BA1', 'AS1', 'CW1', 'WI1', 'SU1', 'OH1', &
    'SF2', 'AF2', 'YC2', 'TA2', 'WS2', 'LS2', 'BE2', 'SS2', &
    'LP2', 'RC2', 'WH2', 'MH2', 'OS2', 'AD2', 'RA2', 'PB2', &
    'AB2', 'BA2', 'AS2', 'CW2', 'WI2', 'SU2', 'OH2', &
    'SF3', 'AF3', 'YC3', 'TA3', 'WS3', 'LS3', 'BE3', 'SS3', &
    'LP3', 'RC3', 'WH3', 'MH3', 'OS3', 'AD3', 'RA3', 'PB3', &
    'AB3', 'BA3', 'AS3', 'CW3', 'WI3', 'SU3', 'OH3'/

!----------
!  DATA STATEMENTS FOR COMMON COEFFS VARIABLES
!----------

!      INTERCEPT COEFFICIENTS FOR WYKOFF HT-DBH FORM
!
DATA HT1/ &
    5.047089, 5.047089, 4.683932, 4.350320,  4.633182, &
    4.633182, 4.350320, 5.047089, 4.513771, 4.716522, &
    5.007972, 4.701229, 4.633182, 4.622461, 4.542755, &
    4.393426, 4.393426, 4.622461, 4.479633, 4.622461, &
    4.622461, 4.622461, 4.622461/

!      SLOPE COEFFICIENTS FOR WYKOFF HT-DBH FORM
!
DATA HT2/ &
    -12.629014, -12.629014, -10.690737, -5.776563,  -6.81926, &
    -6.81926,   -5.776563,  -12.629014, -10.853785, -11.426736, &
    -12.085418, -12.133655, -6.81926,   -6.696442,  -6.654068, &
    -3.968868,  -3.968868, -6.696442,   -5.03023,   -6.696442, &
    -6.696442,  -6.696442,  -6.696442/

!  RESIDUAL ERROR ESTIMATES OF GROWTH MEASUREMENTS USED FOR CALIBRATION.
!  CALCULATED VALUES WERE MULTIPLIED BY .75 TO ACCOUNT FOR FIELD
!  MEASUREMENT ERROR. MARK CASTLE. 4/25/2020
!
DATA SIGMAR/ &
    0.5787,   0.5787,   0.52785,  0.616425, 0.715875, &
    0.715875, 0.616425, 0.5787,   0.45735,  0.510525, &
    0.530625, 0.519375, 0.715875, 0.61845,  0.58005, &
    0.746775, 0.746775, 0.61845,  0.641925, 0.61845, &
    0.61845,  0.61845,  0.61845/

!----------
!  DATA STATEMENTS FOR COMMON VARCOM VARIABLES
!----------

!   HTT1 AND HTT2, DIMENSIONED (MAXSP,9)
!   NOT USED IN NEW AK VARIANT.
!   MAY BE ABLE TO DELETE THESE INITIALIZE TO ZERO.
!
DATA HTT1/ 207*0.0 /
DATA HTT2/ 207*0.0 /

!      ASYMPTOTE PARAMETER COEFFICIENTS FOR CHAPMAN RICHARDS HT-DBH FORM
DATA HTT11/ &
    173.578068, 173.578068, 118.005962,  68.53457,  115.991446, &
    115.991446,  68.534570, 173.578068,  71.730424, 105.741099, &
    141.210402, 108.158839, 115.991446, 115.115222, 131.581959, &
     59.019982,  59.019982, 115.115222,  69.442183, 115.115222, &
    115.115222, 115.115222, 115.115222/

!      RATE PARAMTER COEFFICIENTS FOR CHAPMAN RICHARDS HT-DBH FORM
DATA HTT12/ &
    -0.034704, -0.034704, -0.04487,  -0.149647, -0.068079, &
    -0.068079, -0.149647, -0.034704, -0.096667, -0.051922, &
    -0.054102, -0.054874, -0.068079, -0.04711,  -0.03474, &
    -0.330044, -0.330044, -0.04711,  -0.181461, -0.04711, &
    -0.04711,  -0.04711,  -0.04711/

!      SHAPE PARAMTER COEFFICIENTS FOR CHAPMAN RICHARDS HT-DBH FORM
DATA HTT13/ &
    1.06697,  1.06697,  1.090783, 1.335243, 1.059224, &
    1.059224, 1.335243, 1.06697,  1.589409, 1.134463, &
    1.248358, 1.309823, 1.059224, 0.795143, 0.783043, &
    1.482009, 1.482009, 0.795143, 1.179203, 0.795143, &
    0.795143, 0.795143, 0.795143/

!----------
!  DATA STATEMENTS FOR COMMON RANCOM VARIABLES
!----------
DATA S0/55329D0/,SS/55329./

!----------
!  DATA STATEMENTS FOR COMMON SCREEN VARIABLES
!----------
DATA LSCRN,JOSCRN/.FALSE.,6/

!----------
!  DATA STATEMENTS FOR COMMON ECON VARIABLES
!----------
DATA JOSUME/13/

!----------
!  DATA STATEMENTS FOR COMMON FVSSTDCM VARIABLES
!----------
DATA KOLIST,FSTOPEN /27,.FALSE./

END

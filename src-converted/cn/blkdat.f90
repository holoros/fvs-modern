BLOCK DATA BLKDAT
IMPLICIT NONE
!----------
! NC $Id$
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
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ECON.f90'
!
!
INCLUDE 'ESPARM.f90'
!
!
INCLUDE 'ESCOMN.f90'
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
INCLUDE 'FVSSTDCM.f90'
!
!
!OMMONS
!----------
INTEGER I,J
!----------
!     TYPE DECLARATIONS AND COMMON STATEMENT FOR CONTROL VARIABLES.
!----------
DATA COR2 /MAXSP*1./,HCOR2 /MAXSP*1./,RCOR2/MAXSP*1.0/, &
        BKRAT/MAXSP*0./
!
DATA TREFMT / &
   '(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,3(I2,I2),2I1,I2,2I3, &
   2I1,F3.0)' /
!
DATA YR /  5.0 /, IRECNT/ 0 /,ICCODE/0/
!
DATA IREAD,ISTDAT,JOLIST,JOSTND,JOSUM,JOTREE/ 15,2,3,16,4,8 /
!----------
!     DATA STATEMENT FOR ESCOMN
!----------
DATA XMIN/ 1., 1., 1., .5, 1., .5, .5, 1., .5, 1., 1., 1./

DATA DBHMID/1.0,3.0,5.0,7.0,9.0,12.0,16.0,20.0,24.0,28.0/, &
     ISPSPE/5,7,8,12/, &
     BNORML/3*1.0,1.046,1.093,1.139,1.186,1.232, &
     1.278,1.325,1.371,1.418,1.464,1.510,1.557,1.603,1.649,1.696, &
     1.742,1.789/, &
     HHTMAX/ 27., 31., 25., 25., 26., 24., 28., 20., &
             20., 18., 26., 25./, &
     IFORCD/103,104,105,106,621,110,113,114,116,117, &
            118,109,111,112,412,402,108,102,115,  0/, &
     IFORST/  3,  4,  5,  4,  7, 10,  4, 14, 16, 17, &
              4,  9, 11, 12, 19, 20, 11,  9, 12,  4/
!
!     OCURHT ZEROES OUT PROBABILITIES WHICH CANNOT OCCUR BY DEFINITION.
DATA ((OCURHT(I,J),I=1,16),J=1,2)/ &
     0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,1.0,1.0,0.0,1.0,0.0, &
     13*1.0,                                             0.0,1.0,0.0/
DATA ((OCURHT(I,J),I=1,16),J=3,4)/ &
     15*1.0, 0.0, &
     0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0/
DATA ((OCURHT(I,J),I=1,16),J=5,6)/ &
     9*0.0,1.0,6*0.0, &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0/
DATA ((OCURHT(I,J),I=1,16),J=7,8)/ &
     16*1.0, &
     0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
DATA ((OCURHT(I,J),I=1,16),J=9,12)/ &
     0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
     1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,7*0.0, &
     16*0.0, &
     16*0.0/
!
!     OCURNF ZEROES OUT PROBABILITIES ON NATIONAL FORESTS BY SPECIES.
DATA ((OCURNF(I,J),I=1,20),J=1,2)/ &
       0.,0.,0.,1.,1.,0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,0.,0.,0.,0., &
       0.,0.,3*1.0,   0.,1.,0.,1.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,0./
DATA ((OCURNF(I,J),I=1,20),J=3,4)/ &
       0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,3*1.0,   0.,1.,0.,0.,1.,3*0.0,   1.,0.,1.,1.,0.,1.,1./
DATA ((OCURNF(I,J),I=1,20),J=5,6)/ &
       3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,4*0.0, &
       3*0.0,   1.,1.,0.,1.,6*0.0,            1.,0.,1.,1.,3*0.0/
DATA ((OCURNF(I,J),I=1,20),J=7,8)/ &
       0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1./
DATA ((OCURNF(I,J),I=1,20),J=9,12)/ &
       0.,0.,3*1.0,   0.,1.,0.,4*1.0,      0.,1.,0.,1.,1.,0.,1.,1., &
       0.,0.,3*1.0,0.,1.,0.,1.,4*0.0,1.,0.,1.,1.,0.,1.,1., &
       20*0.0, &
       20*0.0/
!----------
!     COMMON STATEMENT FOR PLOT VARIABLES.
!----------
!     SPECIES LIST FOR KLAMATH MOUNTAINS VARIANT.
!
!     1 = OTHER SOFTWOOD (OS)
!     2 = SUGAR PINE (SP)                PINUS LAMBERTIANA
!     3 = DOUGLAS-FIR (DF)               PSEUDOTSUGA MENZIESII
!     4 = WHITE FIR (WF)                 ABIES CONCOLOR
!     5 = MADRONE (MA)                   ARBUTUS MENZIESII
!     6 = INCENSE CEDAR (IC)             CALOCEDRUS DECURRENS
!     7 = CALIFORNIA BLACK OAK (BO)      QUERQUS KELLOGGII
!     8 = TANOAK (TO)                    LITHOCARPUS DENSIFLORUS
!     9 = RED FIR (RF)                   ABIES MAGNIFICA
!    10 = PONDEROSA PINE (PP)            PINUS PONDEROSA
!    11 = OTHER HARDWOOD (OH)
!    12 = REDWOOD (RW)                   SEQUIOA SEMPERVIRENS
!----------
DATA JSP / &
    'OS ',   'SP ',   'DF ',   'WF ',   'MA ',   'IC ',   'BO ', &
    'TO ',   'RF ',   'PP ',   'OH ',   'RW '/
!
DATA FIAJSP / &
    '299',   '117',   '202',   '015',   '361',   '081',   '818', &
    '631',   '020',   '122',   '998',   '211'/
!
DATA PLNJSP / &
    '2TN   ','PILA  ','PSME  ','ABCO  ','ARME  ','CADE27','QUKE  ', &
    'LIDE3 ','ABMA  ','PIPO  ','2TB   ','SESE3 '/
!
DATA JTYPE /130,170,250,260,280,290,310,320,330,420, &
               470,510,520,530,540,550,570,610,620,640, &
               660,670,680,690,710,720,730,830,850,999,92*0 /
!
DATA NSP /'OS1','SP1','DF1','WF1','MA1','IC1','BO1','TO1','RF1', &
             'PP1','OH1','RW1', &
             'OS2','SP2','DF2','WF2','MA2','IC2','BO2','TO2','RF2', &
             'PP2','OH2','RW2', &
             'OS3','SP3','DF3','WF3','MA3','IC3','BO3','TO3','RF3', &
             'PP3','OH3','RW3'/
!----------
!   COMMON STATEMENT FOR COEFFS VARIABLES
!----------
DATA HT1/ &
    4.78737,4.74961,4.78737, &
    4.80268,4.73881,4.89619, &
    4.80420,4.66181,4.83642, &
    4.23251,4.66181,5.3401/
DATA HT2/ &
    -7.31698,-7.19103,-7.31698,-8.40657,-9.44913, &
   -12.55873,-9.92422,-8.33117,-7.04795,-8.31711, &
    -8.33117,-15.9354/
!----------
!  RESIDUAL ERROR ESTIMATES MULTIPLIED BY 0.75 TO APPROXIMATE
!  CORRECTION FOR MEASUREMENT ERROR:  6/11/91 WRW.
!----------
DATA SIGMAR/ &
    0.3300,  0.2713,  0.3300,  0.3300,  0.3306,  0.3513, &
    0.3541,  0.3558,  0.3136,  0.1954,  0.3558,  0.6178 /
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

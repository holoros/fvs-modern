!----------
! ORGANON $Id$
!----------
!     ORGANON GROWTH AND YIELD MODEL
!     SUBROUTINES INCLUDED:
!        HDCALIB
!        PRDHT
!        A_HD_SWO
!        A_HD_RAP
!        CRCALIB
!        PRDCR
!        A_HCB_SWO
!        A_HCB_RAP
!        HS_H40
!        B_H40
!        F_H40
!C        NCH40
!        WHHLB_SI_UC
!        WHHLB_H40
!        SITECV_F
!        SITEF_C
!        SITEF_SI
!        DFORTY
!        SPMIX
!        HD40_SWO
!        HD40_NWO
!        HD40_SMC
!        HD40_RAP
!        GET_CCFL
!        CALTST
!
!
!  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
!               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
!               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
!               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
!
!  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - GET_CCFL TO GET_CCFL_EDIT
!  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITECV_F TO SITECV_F_EDIT
!  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_C TO SITEF_C_EDIT
!  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_SI TO SITEF_SI_EDIT
!  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - WHHLB_SI_UC TO WHHLB_SI_UC_EDIT
!
!*********************************************************************
SUBROUTINE HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,TAGE,BHAGE,TDATAI, &
              EVEN,SI_1,SI_2,PDEN,TDATAR,D40,HT40,PDF,PTF,PPP,PWH, &
              PRA,ENTHT,ENTDBH,PTRHT,CALIB)
IMPLICIT NONE
!
!     ROUTINE TO CALIBRATE HEIGHT/DIAMETER EQUATIONS
!
INTEGER*4    TAGE,BHAGE,ENTHT(18),ENTDBH(18),IB,NSPN,NTREES,NPTS, &
                TDATAI(2000,3),VERSION
REAL*4       CALIB(3,18),SI_1,SI_2,PTRHT(2000),TDATAR(2000,4)
LOGICAL*2    EVEN
INTEGER*4 I,ISPGRP
REAL*4    X,YXS(18),XSS(18),Y,WT,YSS(18),BETA,PDF,PTF,PPP,PWH, &
             PRA,HT40,D40,AGE1,DBH,PDEN,SI_UC,AGE2
!      INTEGER*4 IANS,IYN
!      IF (J .EQ. 1) THEN
!         WRITE(*,*) ' GROWTH = ',GROWTH(J),' PDG = ',PDG
!C         WRITE(*,*) ' BAL1(1) = ',BAL1(1)
!         WRITE(*,1600)
! 1600    FORMAT(1X,' '\)
!         IANS = IYN(2)
!      ENDIF
!
!     DETERMINE CALIBRATION RATIO FOR SPECIES GROUPS
!
DO I=1,18
  ENTHT(I)=0
  ENTDBH(I)=0
  CALIB(1,I)=1.0
  YXS(I)=0.0
  XSS(I)=0.0
  YSS(I)=0.0
ENDDO
D40=0.0
HT40=0.0
PDF=0.0
PTF=0.0
PPP=0.0
PWH=0.0
PRA=0.0
IF(EVEN) THEN
   AGE1=FLOAT(BHAGE)
   AGE2=FLOAT(TAGE)
   CALL SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
   CALL DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
   SELECT CASE(VERSION)
      CASE(1)
         IF(PDF .GE. 0.80) THEN
            CALL HS_H40(HT40,1,AGE1,SI_1)
         ELSEIF(PTF .GE. 0.80) THEN
            CALL HS_H40(HT40,1,AGE1,SI_1)
         ELSEIF(PPP .GE. 0.80) THEN
            CALL HS_H40(HT40,2,AGE1,SI_2)
         ENDIF
      CASE(2,3)
         IF(PDF .GE. 0.80) THEN
            CALL B_H40(HT40,AGE1,SI_1)
         ELSEIF(PWH .GE. 0.80) THEN
            CALL F_H40(HT40,AGE1,SI_2)
         ENDIF
      CASE(4)
         IF(PRA .GE. 0.80) THEN
            CALL WHHLB_SI_UC_EDIT(SI_1,PDEN,SI_UC)
            CALL WHHLB_H40_EDIT(SI_UC,20.0,AGE2,HT40)
         ENDIF
   ENDSELECT
ENDIF
DO I=1,NTREES
   ISPGRP=TDATAI(I,2)
   DBH=TDATAR(I,1)
   ENTDBH(ISPGRP)=ENTDBH(ISPGRP)+1
   IF(TDATAR(I,2) .LE. 0.0) CYCLE
!
!        CALCULATE TREE HEIGHT
!
   SELECT CASE(VERSION)
      CASE(1)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PTF .GE. 0.80 .AND. ISPGRP .EQ. 2) THEN
            CALL HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PPP .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL A_HD_SWO(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(2)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL HD_NWO(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(3)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL HD_SMC(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(4)
         IF(PRA .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL A_HD_RAP(ISPGRP,DBH,PTRHT(I))
         ENDIF
   ENDSELECT
   Y=TDATAR(I,2)-4.5
   X=PTRHT(I)-4.5
   WT=TDATAR(I,1)
   YXS(ISPGRP)=YXS(ISPGRP)+Y*X/WT
   XSS(ISPGRP)=XSS(ISPGRP)+X*X/WT
   YSS(ISPGRP)=YSS(ISPGRP)+Y*Y/WT
   ENTHT(ISPGRP)=ENTHT(ISPGRP)+1
ENDDO
DO I=1, NSPN
   IF(ENTDBH(I) .EQ. 0) CYCLE
   IF(ENTHT(I) .LT. 2) CYCLE
   CALL CALTST(YXS(I),XSS(I),YSS(I),ENTHT(I),BETA)
   CALIB(1,I)=BETA
ENDDO
RETURN
END
!*********************************************************************
SUBROUTINE PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40, &
                    PDF,PTF,PPP,PWH,PRA,CALIB,PTRHT)
IMPLICIT NONE
!
!     ROUTINE TO PREDICT MISSING TREE HEIGHTS
!
INTEGER*4    NTREES,TDATAI(2000,3),VERSION
REAL*4       CALIB(3,18),PTRHT(2000),TDATAR(2000,4)
INTEGER*4 ENT,I,ISPGRP
REAL*4    PDF,PTF,PPP,PWH,PRA,HT40,D40,DBH
ENT=0
DO I=1,NTREES
   ISPGRP=TDATAI(I,2)
   DBH=TDATAR(I,1)
   IF(DBH .LE. 0.0) CYCLE
   IF(TDATAR(I,2) .GT. 0.0) CYCLE
!
!        CALCULATE TREE HEIGHT
!
   SELECT CASE(VERSION)
      CASE(1)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PTF .GE. 0.80 .AND. ISPGRP .EQ. 2) THEN
            CALL HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PPP .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL A_HD_SWO(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(2)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL HD_NWO(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(3)
         IF(PDF .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
         ELSEIF(PWH .GE. 0.80 .AND. ISPGRP .EQ. 3) THEN
            CALL HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL HD_SMC(ISPGRP,DBH,PTRHT(I))
         ENDIF
      CASE(4)
         IF(PRA .GE. 0.80 .AND. ISPGRP .EQ. 1) THEN
            CALL HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
         ELSE
            CALL A_HD_RAP(ISPGRP,DBH,PTRHT(I))
         ENDIF
   ENDSELECT
   ENT=ENT+1
   TDATAR(I,2)=4.5+CALIB(1,ISPGRP)*(PTRHT(I)-4.5)
   IF(TDATAR(I,2) .LT. 4.6)TDATAR(I,2)=4.6
ENDDO
RETURN
END
!
!*********************************************************************
SUBROUTINE A_HD_SWO(ISPGRP,DBH,PRDHT)
IMPLICIT NONE
REAL*4 DBH,PRDHT,HDPAR(18,3),B0,B1,B2
INTEGER*4 ISPGRP
!
!  NEW HEIGHT/DIAMETER PARAMETERS FOR ALL (UNDAMAGED AND DAMAGED) TREES
!      (3 parameters - all species)
!
!     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     SP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     IC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     WH Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
!     PY Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     MD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     GC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     TA Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     CL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     BL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
!     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
!     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!
DATA HDPAR/ &
                7.153156143,  6.638003799, 7.181264435, 6.345116767, &  !  DF,GW,PP,SP
                8.776627288,  6.58804    , 6.14817441 , 6.402691396, &  !  IC,WH,RC,PY
                5.42457261 ,  9.21600278 , 7.398142262, 7.762149257, &  !  MD,GC,TA,CL
                5.02002617 ,  4.69753118 , 4.907340242, 5.59759126 , &  !  BL,WO,BO,RA
                5.252315215,  3.862132151, &  !  PD,WI
!
               -5.36900835 , -5.44399465 ,-5.90709219 ,-5.30026188 , &  !  DF,GW,PP,SP
               -7.4383668  , -5.35325461 ,-5.40092761 ,-4.79802411 , &  !  IC,WH,RC,PY
               -3.56317104 , -7.63409138 ,-5.5099273  ,-6.04759773 , &  !  MD,GC,TA,CL
               -2.51228202 , -3.51586969 ,-3.18017969 ,-3.19942952 , &  !  BL,WO,BO,RA
               -3.13509983 , -1.5294776  , &  !  PD,WI
!
               -0.25832512 , -0.33929196 ,-0.27533719 ,-0.35264183 , &  !  DF,GW,PP,SP
               -0.16906224 , -0.31897786 ,-0.38922036 ,-0.16317997 , &  !  IC,WH,RC,PY
               -0.36177689 , -0.15346440 ,-0.19080702 ,-0.16308399 , &  !  MD,GC,TA,CL
               -0.42256497 , -0.57665068 ,-0.46654227 ,-0.38783403 , &  !  BL,WO,BO,RA
               -0.26979750 , -0.62476287 /                           !  PD,WI
!
B0=HDPAR(ISPGRP,1)
B1=HDPAR(ISPGRP,2)
B2=HDPAR(ISPGRP,3)
PRDHT=4.5+EXP(B0+B1*DBH**B2)
RETURN
END
!*********************************************************************
SUBROUTINE A_HD_RAP(ISPGRP,DBH,PRDHT)
IMPLICIT NONE
REAL*4 DBH,PRDHT,HDPAR(7,3),B0,B1,B2
INTEGER*4 ISPGRP
!
!  HEIGHT/DIAMETER PARAMETERS (UNDAMAGED AND DAMAGED) TREES
!     (3 parameters - all species)
!
!     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
!     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
!     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
!     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
!     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
!
DATA HDPAR/ &
               6.76804202 ,  7.262195456,  6.555344622,  6.14817441, &  !  RA,DF,WH,RC
               5.21462    ,  4.49727    ,  4.88361    , &  !  BL,PD,WI
!
              -4.6370303  , -5.899759104, -5.137174162, -5.40092761, &  !  RA,DF,WH,RC
              -2.70252    , -2.07667    , -2.47605    , &  !  BL,PD,WI
!
              -0.23108894 , -0.287207389, -0.364550800, -0.38922036, &  !  RA,DF,WH,RC
              -0.354756   ,  -0.388650  , -0.309050  /              !  BL,PD,WI
!
B0=HDPAR(ISPGRP,1)
B1=HDPAR(ISPGRP,2)
B2=HDPAR(ISPGRP,3)
PRDHT=4.5+EXP(B0+B1*DBH**B2)
RETURN
END
!*********************************************************************
SUBROUTINE CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI, &
              ENTDBH,SI_1,SI_2,TDATAR,STDATAR,BAL,BALL,CCFL,CCFLL, &
              SBA,OG,ENTCR,PCR,CALIB)
IMPLICIT NONE
!
!     ROUTINE TO CALIBRATE CROWN RATIOS
!
INTEGER*4   VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3), &
               ENTDBH(18),ENTCR(18)
REAL*4      SI_1,SI_2,TDATAR(2000,4),STDATAR(2000,4),BAL(500), &
               BALL(51),CCFL(500),CCFLL(51),SBA,OG,PCR(2000), &
               CALIB(3,18)
INTEGER*4   I,ISPGRP
REAL*4      YSS(18),X,Y,XSS(18),YXS(18),DBH,HT,XSI_1,XSI_2, &
               SCCFL,PHCB,BETA
!      OPEN(44,FILE='TEMP.DAT',STATUS='UNKNOWN')
!
DO I=1,18
   ENTCR(I)=0
   CALIB(2,I)=1.0
   YSS(I)=0.0
   XSS(I)=0.0
   YXS(I)=0.0
ENDDO

CALL SSUM(2,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,SBA, &
             BAL,BALL,CCFL,CCFLL)
CALL OLDGROWTH(NPTS,NTREES,IB,TDATAI,TDATAR,OG)
DO I=1,NTREES
   IF(TDATAR(I,3) .LE. 0.0) CYCLE
   ISPGRP=TDATAI(I,2)
   DBH=TDATAR(I,1)
   HT=TDATAR(I,2)
   XSI_1=SI_1-4.5
   XSI_2=SI_2-4.5
   CALL GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
!         WRITE(44,2002) HT,DBH,SCCFL,SBA,SI_1
! 2002    FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
   SELECT CASE(VERSION)
      CASE(1)
         CALL A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(2)
         CALL HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(3)
         CALL HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(4)
         CALL A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
   ENDSELECT
   X=PCR(I)
   Y=TDATAR(I,3)
!         WRITE(44,2000) X,Y
! 2000    FORMAT(F8.4,1X,F8.4)
   YXS(ISPGRP)=YXS(ISPGRP)+X*Y
   XSS(ISPGRP)=XSS(ISPGRP)+X*X
   YSS(ISPGRP)=YSS(ISPGRP)+Y*Y
   ENTCR(ISPGRP)=ENTCR(ISPGRP)+1
ENDDO
DO I=1,NSPN
   IF(ENTDBH(I) .EQ. 0) CYCLE
   IF(ENTCR(I) .LT. 2)CYCLE
   CALL CALTST(YXS(I),XSS(I),YSS(I),ENTCR(I),BETA)
   CALIB(2,I)=BETA
ENDDO
!      WRITE(44,2001) YXS(1),XSS(1),YSS(1),BETA
! 2001 FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
!      CLOSE(44)
RETURN
END
!*********************************************************************
SUBROUTINE PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,SI_1, &
              SI_2,TDATAR,CCFL,CCFLL,SBA,OG,ENT,PCR,CALIB)
IMPLICIT NONE
!
!     ROUTINE TO CALCULATE MISSING CROWN RATIOS
!

INTEGER*4   VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3),ENT
REAL*4      SI_1,SI_2,TDATAR(2000,4),CCFL(500),CCFLL(51),SBA,OG, &
               PCR(2000),CALIB(3,18)
INTEGER*4   I,ISPGRP
REAL*4      DBH,HT,XSI_1,XSI_2,SCCFL,PHCB
INTEGER*4 IDANUW
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = IB
IDANUW = NPTS
IDANUW = NSPN
!
ENT=0
DO I=1,NTREES
   IF(TDATAR(I,3) .GT. 0.0) CYCLE
!
!        CALCULATE  CROWN RATIO
!
   ENT=ENT+1
   ISPGRP=TDATAI(I,2)
   DBH=TDATAR(I,1)
   IF(DBH .LE. 0.0) CYCLE
   HT=TDATAR(I,2)
   XSI_1=SI_1-4.5
   XSI_2=SI_2-4.5
   CALL GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
   SELECT CASE(VERSION)
      CASE(1)
         CALL A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(2)
         CALL HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(3)
         CALL HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
      CASE(4)
         CALL A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG, &
                         PHCB)
         PCR(I)=1.0-PHCB/HT
   ENDSELECT
   TDATAR(I,3)=PCR(I)*CALIB(2,TDATAI(I,2))
   IF(TDATAR(I,3) .GT. 1.0) TDATAR(I,3)=1.0
   IF(TDATAR(I,3) .LT. 0.05) TDATAR(I,3)=0.05
ENDDO
RETURN
END
!**********************************************************************
SUBROUTINE A_HCB_SWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
IMPLICIT NONE
INTEGER*4 ISPGRP
REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(18,7),B0,B1,B2,B3, &
          B4,B5,B6
!
!  NEW HEIGHT TO CROWN BASE FOR ALL (UNDAMAGED AND DAMAGED) TREES
!        (7 PARameters - all species)
!
!     DF Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     GW Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     PP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     SP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     IC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     WH Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
!     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     GC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     TA Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     CL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
!     BO Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
!     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!
DATA HCBPAR/ &
           1.990155033,  4.800089990,  2.024723585 ,  3.582314301, &  !  DF,GW,PP,SP
           3.127730861,  0.0        ,  4.49102006  ,  0.0        , &  !  IC,WH,RC,PY
           3.271130882,  0.387912505,  0.4488479442,  1.285465907, &  !  MD,GC,TA,CL
           1.000364090,  1.05786632 ,  2.672850866 ,  0.56713781 , &  !  BL,WO,BO,RA
           0.0        ,  0.0        , &  !  PD,WI
!
          -0.008180786,  0.0        , -0.001953589 , -0.003256792, &  !  DF,GW,PP,SP
          -0.004386780,  0.0        ,  0.0         ,  0.0        , &  !  IC,WH,RC,PY
           0.0        , -0.015000868, -0.009375810 , -0.024459278, &  !  MD,GC,TA,CL
          -0.010636441,  0.0        ,  0.0         , -0.010377976, &  !  BL,WO,BO,RA
           0.0        ,  0.0        , &  !  PD,WI
!
          -0.004696095, -0.003268539, -0.001837480 ,  0.0        , &  !  DF,GW,PP,SP
          -0.003557122,  0.0        , -0.00132412  ,  0.0        , &  !  IC,WH,RC,PY
           0.0        , -0.004098099, -0.001822050 , -0.003992574, &  !  MD,GC,TA,CL
          -0.005950398, -0.00183283 , -0.001400851 , -0.002066036, &  !  BL,WO,BO,RA
          -0.004842962, -0.004842962, &  !  PD,WI
!
          -0.392033240, -0.858744969, -0.568909853 , -0.765250973, &  !  DF,GW,PP,SP
          -0.637929879,  0.0        , -1.01460531  ,  0.0        , &  !  IC,WH,RC,PY
          -0.841331291,  0.0        ,  0.0         ,  0.0        , &  !  MD,GC,TA,CL
           0.0        , -0.28644547 , -0.605971926 ,  0.0        , &  !  BL,WO,BO,RA
          -0.567987126, -0.567987126, &  !  PD,WI
!
           1.945708371,  0.0        ,  4.831886553 ,  3.043845568, &  !  DF,GW,PP,SP
           0.977816058,  3.246352823,  0.0         ,  1.225564582, &  !  IC,WH,RC,PY
           1.791699815,  2.104871164,  0.0         ,  0.0        , &  !  MD,GC,TA,CL
           0.0        ,  0.0        ,  0.0         ,  1.39796223 , &  !  BL,WO,BO,RA
           0.0        ,  0.0        , &  !  PD,WI
!
           0.007854260,  0.0        ,  0.001653030 ,  0.0        , &  !  DF,GW,PP,SP
           0.005850321,  0.0        ,  0.01340624  ,  0.0        , &  !  IC,WH,RC,PY
           0.0        ,  0.0        ,  0.0         ,  0.0        , &  !  MD,GC,TA,CL
           0.0        ,  0.0        ,  0.0         ,  0.0        , &  !  BL,WO,BO,RA
           0.0281315332, 0.0281315332, &  !  PD,WI
!
           0.295593583,  0.275679490,  0.0         ,  0.0        , &  !  DF,GW,PP,SP
           0.257070387,  0.0        ,  0.0         ,  0.0        , &  !  IC,WH,RC,PY
           0.927163029,  0.352773356,  0.233233237 ,  0.0        , &  !  MD,GC,TA,CL
           0.310672769,  0.0        ,  0.430988703 ,  0.0        , &  !  BL,WO,BO,RA
           0.0        ,  0.0/                                       !  PD,WI
!
B0=HCBPAR(ISPGRP,1)
B1=HCBPAR(ISPGRP,2)
B2=HCBPAR(ISPGRP,3)
B3=HCBPAR(ISPGRP,4)
B4=HCBPAR(ISPGRP,5)
B5=HCBPAR(ISPGRP,6)
B6=HCBPAR(ISPGRP,7)
IF(ISPGRP .EQ. 3) THEN
   HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT) &
         +B5*SI_2+B6*OG**2))
ELSE
   HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT) &
         +B5*SI_1+B6*OG**2))
ENDIF
RETURN
END
!**********************************************************************
SUBROUTINE A_HCB_RAP(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
IMPLICIT NONE
INTEGER*4 ISPGRP
REAL*4 HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(7,8),B0,B1,B2,B3, &
          B4,B5,B6,K,SITE
!
!  HEIGHT TO CROWN BASE (UNDAMAGED AND DAMAGED) TREES
!     (7 parameters - all species)
!
!     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
!     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
!     WH Coefficients from Johnson (2002) Willamette Industries Report
!     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
!     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
!
DATA HCBPAR/ &  !
          3.98915507  ,  6.18464679 ,  1.92682    ,  4.49102006 , &  !  RA,DF,WH,RC
          0.9411395642,  0.0        ,  0.0        , &  !  BL,PD,WI
!
         -0.019280895 , -0.00328764 , -0.00280478 ,  0.0        , &  !  RA,DF,WH,RC
         -0.00768402  ,  0.0        ,  0.0        , &  !  BM,PD,WI
!
         -0.0017632543, -0.00136555 , -0.0011939  , -0.00132412 , &  !  RA,DF,WH,RC
         -0.005476131 , -0.005666559, -0.005666559, &  !  BL,PD,WI
!
         -1.1178816   , -1.19702220 , -0.513134   , -1.01460531 , &  !  RA,DF.WH,RC
          0.0         , -0.745540494, -0.745540494, &  !  BL,PD,WI
!
          7.12804469  ,  3.17028263 ,  3.68901    ,  0.0        , &  !  RA,DF,WH,RC
          0.0         ,  0.0        ,  0.0        , &  !  BL,PD,WI
!
          0.0240273988,  0.0        ,  0.00742219 ,  0.01340624 , &  !  RA,DF,WH,RC
          0.0        ,   0.038476613,  0.038476613, &  !  BL,PD,WI
!
          0.0        ,   0.0        ,  0.0        ,  0.0        , &  !  RA,DF,WH,RC
          0.0        ,   0.0        ,  0.0        , &  !  BL,PD,WI
!
          1.6        ,   0.0        ,  0.0        ,  0.0        , &  !  RA,DF,WH,RC
          0.0        ,   0.0        ,  0.0/                         !  BL,PD,WI
!
B0=HCBPAR(ISPGRP,1)
B1=HCBPAR(ISPGRP,2)
B2=HCBPAR(ISPGRP,3)
B3=HCBPAR(ISPGRP,4)
B4=HCBPAR(ISPGRP,5)
B5=HCBPAR(ISPGRP,6)
B6=HCBPAR(ISPGRP,7)
K=HCBPAR(ISPGRP,8)
IF(ISPGRP .EQ. 1) THEN
   HCB=(HT-K)/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT) &
         +B5*SI_1+B6*OG**2))+K
ELSE
   SITE=SI_2
   IF(ISPGRP .EQ. 3) THEN
      SITE=(0.480 +( 1.110 * (SI_2+4.5)))-4.5
   ENDIF
   HCB=HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT) &
         +B5*SITE+B6*OG**2))
ENDIF
RETURN
END
!*********************************************************************
SUBROUTINE HS_H40(HT40,ISP,AGE,SI)
!
!----------------------------------------------------------------------
!     Hann, D.W. and J.A. Scrivani.  1987.  Dominanat-height-growth and
!     site-index equations for Douglas-fir and ponderosa pine in
!     Southwest Oregon.  Oregon State University, Forest Research
!     Laboratory.  Research Bulletin 59.  13p.   Douglas-fir
!----------------------------------------------------------------------
!
IMPLICIT NONE
REAL*4 AGE,HT40,X1,X2,SI,S
INTEGER*4 ISP
!

  S=SI-4.5
  IF(ISP .EQ. 1) THEN
     X1 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S) &
             + 1.14354*ALOG(AGE)))
     X2 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S) &
             + 1.14354*ALOG(50.0)))
  ELSEIF(ISP .EQ. 2) THEN
     X1 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S) &
             + 1.21297*ALOG(AGE)))
     X2 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S) &
             + 1.21297*ALOG(50.0)))
  ENDIF
  HT40 = 4.5 + S*(X1/X2)
!
RETURN
END
!
!**********************************************************************
SUBROUTINE B_H40(HT40,AGE,SI)
!
!----------------------------------------------------------------------
!     Bruce, D.  1981.  Consistent height-growth and growth-rate
!     estimates for remeasured plots.  Forest Science 27:711-725.
!----------------------------------------------------------------------
!
IMPLICIT NONE
REAL*4 AGE,HT40,SI,A1,A2
!
  A2 = -0.447762 - 0.894427*SI/100.0 + 0.793548*(SI/100.0)**2 &
          -0.17166*(SI/100.0)**3
  A1 = LOG(4.5/SI)/((13.25 - (SI/20.0))**A2 - (63.25 - &
          (SI/20.0))**A2)
  HT40 = SI*EXP(A1*((AGE + 13.25 - (SI/20.0))**A2 &
          - (63.25 - (SI/20.0))**A2))
!
RETURN
END
!
!**********************************************************************
SUBROUTINE F_H40(HT40,AGE,SI)
!
!----------------------------------------------------------------------
!     Top height (option 2) and site curves (option 1) for western
!     hemlock by J. Flewelling (unpublished).  Note, these are metric
!     curves.  This subroutine requires the following subroutines:
!       SITECV_F   computes top height from site and age
!       SITEF_C    computes model parameters
!       SITEF_SI   calculates an approximate psi for a given site
!----------------------------------------------------------------------
!
IMPLICIT NONE
REAL*4 AGE,SI,HT40,HTOP,XSI
REAL*4 ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION
!
COMMON /INFO/ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION
!
  XSI = SI*0.3048
  CALL SITECV_F_EDIT(XSI,AGE,HTOP)
  HT40 = HTOP*3.2808
RETURN
END
!
!**********************************************************************
!      SUBROUTINE NCH40(H40,AGE,SI)
!C
!C     NIGH AND COURTIN (1998) RED ALDER
!C
!      IMPLICIT NONE
!      REAL*4 SI,AGE,H40,MSI,MH40
!      MSI=SI/3.28
!      MH40=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
!      H40=3.28*MH40
!      RETURN
!      END
!*******************************************************************************
SUBROUTINE WHHLB_SI_UC_EDIT(SI_C,PDEN,SI_UC)
!
!     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
!          SITE INDEX FOR RED ALDER
!
IMPLICIT NONE
REAL*4 SI_C,PDEN,SI_UC
!
!     SITE INDEX UNCORRECTED FOR DENSITY EFFECT
!
SI_UC=SI_C*(1.0-0.36979789*EXP(-0.00042264*PDEN**1.5))
RETURN
END
!***********************************************************************
SUBROUTINE WHHLB_H40_EDIT(H40M,TAGEM,TAGEP,PH40P)
!
!     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH EQUATION FOR
!     RED ALDER
!
IMPLICIT NONE
REAL*4 H40M,TAGEM,TAGEP,PH40P
REAL*4 B1,B2
B1=-4.481266
B2=-0.658884
PH40P=H40M*EXP(B1*(TAGEP**B2-TAGEM**B2))
RETURN
END
!**********************************************************************
SUBROUTINE SITECV_F_EDIT(si,age,htop)
!
!----------------------------------------------------------------------
!     Purpose:  Implements new height-increment methods (F)
!
!     Current Date: FEB 2, 1994    J. FLEWELLING
!
!     SI    IN     R*4    Site index (m) (basis is BH AGE 50)
!     AGE   IN     R*4    Breast height age (.= 1.0)
!     HTOP  OUT    R*4    Top height (== site height) (m)
!----------------------------------------------------------------------
!
REAL*4 SI,AGE,HTOP,OLD_SI,SI_2,yk,alpha,beta,c,b1,xk,psi,x,z
INTEGER*4 h1
!
SAVE OLD_SI

common /sitefprm/xk, b1, c , beta,alpha, h1, yk, SI_2
!
!                           determine if coefficients for this SI are
!                          already in siteprm. If not, get them.

if (SI.ne. old_si) then
       old_si = si
       call sitef_si_EDIT( si, psi)
       call sitef_C_EDIT ( psi)
     end if

!                                         apply height-age equation.
x=age-1
if (x.lt.xk) then
       htop  = h1 + SI_2*x + &
               (1-b1)*SI_2*xk/(c+1) * (((xk-x)/xk)**(c+1)-1)
    else
       z = x -xk
       htop = yk &
               +alpha*(1-exp(-beta*z))
    end if
return
end
!
!**********************************************************************
SUBROUTINE SITEF_C_EDIT(psi)
!
!----------------------------------------------------------------------
!     Purpose:  For a specified psi, calculates all of the height-age
!               model parameters, and stores them in /sitefprm/
!
!     Current Date: FEB 2, 1994    J. FLEWELLING
!
!     psi     input   REAL     productivity index (m/yr)
!----------------------------------------------------------------------
!
REAL*4 psi,fp,SI_2,yk,alpha,beta,c,b1,xk
INTEGER*4 i,h1
dimension fp(10)
common /sitefprm/xk, b1, c ,  beta,alpha, h1, yk, SI_2
!                                                   R24 Coefficients
data (fp(i),i=1,5),fp(9)  /    128.326    ,    -2.54871 , &
       5.33208, -9.00622 , 1.2, 52.7948 /

SI_2=psi
xk = fp(1) * exp( fp(2) * psi)
b1 = 0.2 + 0.8/ ( 1 + exp(fp(3) + fp(4)*psi))
c  =1.0 + fp(5)* psi
alpha=fp(9)*psi
h1 = INT(1.3 + (b1*psi)/2.)
yk = h1 + psi * xk* ( 1.0 - (1.-b1)/(c+1))

beta=psi/alpha

RETURN
 end
!
!**********************************************************************
SUBROUTINE SITEF_SI_EDIT( SI, PSI)
!
!----------------------------------------------------------------------
!     Purpose:  Calculates an approximate psi for a given site index
!               Ref 'Model Fitting: top height increment', Feb 2, 1994.
!
!     Current Date: FEB 2, 1994    J. FLEWELLING and A. ZUMRAWI
!
!          si      input  r*4    site index (top height at BH age 50)
!          psi     output r*4    site productivity parameter.
!----------------------------------------------------------------------
!
 REAL*4 SI,PSI,b,si_piv,x
 INTEGER*4 i,j
 dimension b(6,2)
 data si_piv  / 32.25953/
 data (b(i,1),i=1,6)/ .299720, .116875, .074866, .032348, &
                         .006984, .000339 /
 data (b(i,2),i=1,6)/ .290737, .129665, -.058777, &
                         -.000669, .006003, -.001060 /

  if (si.le. si_piv) then
      J=1
  else
      J=2
  end if

  x = (si - si_piv)/10.0

  psi = 0.75 + x*( b(1,j) + x*( b(2,j)  + x*( b(3,j) &
                + x*( b(4,j) + x*( b(5,j)  + x*b(6,j)   )))))
  return
  end
!
!**********************************************************************
SUBROUTINE DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
!     DETERMINE DIAMETER OF THE FORTY LARGEST BIG-6 TREES PER ACRE
!
!
IMPLICIT NONE
INTEGER*4 VERSION,NTREES,NPTS,IB,TDATAI(2000,3),I,ID,IIB
REAL*4    TDATAR(2000,4),TOTD,TOTTR,DCL(1000),TRCL(1000),D40, &
             TRDIFF,EXPAN
!
TOTD=0.0
TOTTR=0.0
DO I=1,1000
   DCL(I)=0.0
   TRCL(I)=0.0
ENDDO
IIB=IB
IF(VERSION .GE. 4) IIB=1
DO I=1,NTREES
   IF(TDATAR(I,1) .LE. 0.0) CYCLE
   IF(TDATAI(I,2) .LE. IIB) THEN
      ID=IFIX(TDATAR(I,1)*10.0)
      IF(ID.GT.1000) ID=1000
      EXPAN=TDATAR(I,4)/FLOAT(NPTS)
      DCL(ID)=DCL(ID)+TDATAR(I,1)*EXPAN
      TRCL(ID)=TRCL(ID)+EXPAN
   ENDIF
ENDDO
DO I=1000,1,-1
   TOTD=TOTD+DCL(I)
   TOTTR=TOTTR+TRCL(I)
   IF(TOTTR.GT.40.0) THEN
      TRDIFF=TRCL(I)-(TOTTR-40.0)
      TOTD=TOTD-DCL(I)+((DCL(I)/TRCL(I))*TRDIFF)
      TOTTR=40.0
      EXIT
   ENDIF
ENDDO
IF(TOTTR.GT.0.0) THEN
   D40=TOTD/TOTTR
ELSE
   D40=0.0
ENDIF
RETURN
END
!
!**********************************************************************
SUBROUTINE SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
IMPLICIT NONE
INTEGER*4  NTREES,NPTS,TDATAI(2000,3),I
REAL*4     TDATAR(2000,4),BA,BATOT,BADF,BATF,BAPP,BAWH,BARA,PDF, &
              PTF,PPP,PWH,PRA
BATOT=0.0
BADF=0.0
BATF=0.0
BAPP=0.0
BAWH=0.0
BARA=0.0
PDF=0.0
PTF=0.0
PPP=0.0
PWH=0.0
PRA=0.0
DO I=1,NTREES
   BA=(TDATAR(I,1)**2*TDATAR(I,4))*0.005454154/NPTS
   BATOT=BATOT+BA
   IF(TDATAI(I,1) .EQ. 15 .OR. TDATAI(I,1) .EQ. 17) THEN
      BATF=BATF+BA
   ELSEIF(TDATAI(I,1) .EQ. 122) THEN
      BAPP=BAPP+BA
   ELSEIF(TDATAI(I,1) .EQ. 202) THEN
      BADF=BADF+BA
   ELSEIF(TDATAI(I,1) .EQ. 263) THEN
      BAWH=BAWH+BA
   ELSEIF(TDATAI(I,1) .EQ. 351) THEN
      BARA=BARA+BA
   ENDIF
ENDDO
IF(BATOT .GT. 0.0) THEN
   PDF=BADF/BATOT
   PTF=BATF/BATOT
   PPP=BAPP/BATOT
   PWH=BAWH/BATOT
   PRA=BARA/BATOT
ENDIF
RETURN
END
!**********************************************************************
SUBROUTINE HD40_SWO(IEQ,HT40,D40,DBH,PTRHT)
IMPLICIT NONE
INTEGER*4  IEQ
REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)
!
!    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir, true firs,
!                               ponderosa pine)
!     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
!C
DATA HD40PAR/ &
                -3.485635287, -4.376160718, -4.047994965, &  !  DF,GW,PP
                -0.255712209, -0.231693907, -0.135864020, &  !  DF,GW,PP
                -0.001555149, -0.001334070, -0.005647510/           !  DF,GW,PP
!
B0=HD40PAR(IEQ,1)
B1=HD40PAR(IEQ,2)
B2=HD40PAR(IEQ,3)
EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
RETURN
END
!**********************************************************************
SUBROUTINE HD40_NWO(IEQ,HT40,D40,DBH,PTRHT)
IMPLICIT NONE
INTEGER*4  IEQ
REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)
!
!    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and Western Hemlock)
!
!     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!
DATA HD40PAR/ &
                -2.857232223, -2.790360488, &  !  DF,WH
                -0.393885195, -0.235470605, &  !  DF,WH
                -0.000521583, -0.002374673/                         !  DF,WH
!
B0=HD40PAR(IEQ,1)
B1=HD40PAR(IEQ,2)
B2=HD40PAR(IEQ,3)
EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
RETURN
END
!**********************************************************************
SUBROUTINE HD40_SMC(IEQ,HT40,D40,DBH,PTRHT)
IMPLICIT NONE
INTEGER*4  IEQ
REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)

!
!    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and western hemlock)
!
!     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!
DATA HD40PAR/ &
                -2.857232223, -2.790360488, &  !  DF,WH
                -0.393885195, -0.235470605, &  !  DF,WH
                -0.000521583, -0.002374673/                         !  DF,WH
!
B0=HD40PAR(IEQ,1)
B1=HD40PAR(IEQ,2)
B2=HD40PAR(IEQ,3)
EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
RETURN
END
!******************************************************************************
SUBROUTINE HD40_RAP(IEQ,HT40,D40,DBH,PTRHT)
IMPLICIT NONE
INTEGER*4  IEQ
REAL*4     HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)

!
!     HEIGHT/DIAMETER USING D40 (3PARameters - Red Alder, Douglas-fir and
!     western hemlock)
!
!     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
!     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
!
DATA HD40PAR/ &
                -1.7477875  , -2.857232223, -2.790360488, &  !  RA,DF,WH
                -0.40004105 , -0.393885195, -0.235470605, &  !  RA,DF,WH
                -0.00497111 , -0.000521583, -0.002374673/        !  RA,DF,WH
!
B0=HD40PAR(IEQ,1)
B1=HD40PAR(IEQ,2)
B2=HD40PAR(IEQ,3)
EXD=EXP(B0*DBH**(B1+B2*(HT40-4.5)))
EXD40=EXP(B0*D40**(B1+B2*(HT40-4.5)))
PTRHT=4.5+(HT40-4.5)*(EXD/EXD40)
RETURN
END
!******************************************************************************
SUBROUTINE GET_CCFL_EDIT(DBH,CCFLL1,CCFL1,CCFL)
IMPLICIT NONE
INTEGER*4 K
REAL*4 DBH,CCFLL1(51),CCFL1(500),CCFL
IF(DBH .GT. 100.0) THEN
   CCFL=0.0
ELSEIF(DBH .GT. 50.0)THEN
   K=INT(DBH-49.0)
   CCFL=CCFLL1(K)
ELSE
   K=INT(DBH*10.0+0.5)
   CCFL=CCFL1(K)
ENDIF
RETURN
END
!*********************************************************************
SUBROUTINE CALTST(YXS,XSS,YSS,N,BETA)
!
!     COMPUTE CALIBRATION BETA AND DETERMINE IF IT IS SIGNIFICANT FROM
!     ONE
!
IMPLICIT NONE
INTEGER*4 N,IDEGF
REAL*4 BETA,VARBETA,DEGF,MSE,TTEST,YXS,XSS,YSS,TVAL(34),CRITVAL
DATA TVAL/63.657, 9.925, 5.841, 4.604, 4.032, 3.707, 3.499, 3.355, &
              3.250, 3.169, 3.106, 3.055, 3.012, 2.977, 2.947, 2.921, &
              2.898, 2.878, 2.861, 2.845, 2.831, 2.819, 2.807, 2.797, &
              2.787, 2.779, 2.771, 2.763, 2.756, 2.750, 2.704, 2.660, &
              2.617, 2.576/
BETA=YXS/XSS
IDEGF=N-1
DEGF=FLOAT(IDEGF)
MSE=(YSS-2.0*BETA*YXS+BETA**2*XSS)/DEGF
VARBETA=MSE/XSS
IF(VARBETA.GT.0.0) THEN
   TTEST=ABS(BETA-1.0)/SQRT(VARBETA)
   IF(IDEGF.LE.30) THEN
      CRITVAL=TVAL(IDEGF)
   ELSE IF(IDEGF.LE.40) THEN
      CRITVAL=TVAL(30)+(TVAL(31)-TVAL(30))*((DEGF-30.0)/10.0)
   ELSE IF(IDEGF.LE.60) THEN
      CRITVAL=TVAL(31)+(TVAL(32)-TVAL(31))*((DEGF-40.0)/20.0)
   ELSE IF(IDEGF.LE.120) THEN
      CRITVAL=TVAL(32)+(TVAL(33)-TVAL(32))*((DEGF-60.0)/60.0)
   ELSE IF(IDEGF.LE.1000) THEN
      CRITVAL=TVAL(33)+(TVAL(34)-TVAL(33))*((DEGF-120.0)/880.0)
   ELSE
      CRITVAL=TVAL(34)
   ENDIF
   IF(TTEST.LT.CRITVAL) THEN
      BETA=1.0
   ELSE
      IF(BETA .GT. 2.0)THEN
         BETA=2.0
      ELSE IF(BETA .LT. 0.5)THEN
         BETA=0.5
      ENDIF
   ENDIF
ELSE
   IF(BETA .GT. 2.0)THEN
      BETA=2.0
   ELSE IF(BETA .LT. 0.5)THEN
      BETA=0.5
   ENDIF
ENDIF
RETURN
END

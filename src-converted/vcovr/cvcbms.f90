SUBROUTINE CVCBMS (LTHIN)
IMPLICIT NONE
!----------
! VCOVR $Id$
!----------
!  **CVCBMS** COMPUTES FOLIAGE BIOMASS FOR INDIVIDUAL TREES.
!
!  FOLIAGE BIOMASS RELATIONSHIPS ARE DOCUMENTED IN :
!  MOEUR, MELINDA. 1981. CROWN WIDTH AND FOLIAGE WEIGHT OF NORTHERN
!       ROCKY MOUNTAIN CONIFERS.  USDA FOR. SERV. RES. PAP. INT-283.
!
!  MODEL OPTIONS CONTROLLED BY PARAMETER 'COVOPT' ON THE COVER
!  KEYWORD ARE FOR DATA SETS WITH INDIVIDUAL TREE AGE
!  RECORDED (COVOPT=1), OR WITHOUT AGE (COVOPT=2).
!  DEFAULT IS COVOPT = 2.
!----------
!  VARIABLE DEFINITIONS
!----------
!  AGE         -- TREE AGE
!  BINT11(MAXSP)-- ARRAY OF INTERCEPTS FOR FOLIAGE BIOMASS FUNCTION
!                 FOR TREES LESS THAN 3.5 INCHES, MODEL 1
!  BCL11(MAXSP)-- ARRAY OF COEFFICIENTS FOR CROWN LENGTH TERM FOR TREES
!                 LESS THAN 3.5 INCHES, MODEL 1
!  BINT12(MAXSP)-- ARRAY OF INTERCEPTS FOR TREES LESS THAN 3.5 INCHES,
!                 MODEL 2
!  BCL12(MAXSP)-- ARRAY OF COEFFICIENTS FOR CROWN LENGTH TERM FOR
!                 TREES .LT. 3.5 INCHES, MODEL 2
!  BINT2(MAXSP)-- ARRAY OF INTERCEPTS FOR FOLIAGE BIOMASS FUNCTION
!                 FOR TREES 3.5 INCHES AND LARGER, MODELS 1 AND 2
!  CL          -- CROWN LENGTH
!  COVOPT      -- EQUATION OPTION NUMBER
!  D           -- TREE DBH
!  DDS         -- DELTA DIAMETER SQUARED (CHANGE IN SQUARED DIAMETER)
!  H           -- TREE HEIGHT
!  RD          -- RELATIVE DIAMETER
!  RMSD        -- ROOT MEAN SQUARE DIAMETER
!  TPA         -- TREES PER ACRE
!  TRFBMS(MAXTRE)-- PREDICTED FOLIAGE BIOMASS IN POUNDS
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'CVCOM.f90'
!
!OMMONS
!----------
LOGICAL LTHIN,DEBUG
INTEGER I,ISPI,IICR
INTEGER MAPISP(49),MAPAK(49),MAPCA(49),MAPBM(49),MAPCI(49), &
            MAPCR(49),MAPEC(49),MAPEM(49),MAPIE(49),MAPNI(49), &
            MAPSO(49),MAPTT(49),MAPUT(49),MAPWC(49),MAPWS(49)
REAL RMSD,TPA,ALNTPA,D,H,CL,RD,DDS,AGE
REAL BINT11(11), BCL11(11)
REAL BINT12(11), BCL12(11)
REAL BINT2(11)
!----------
!  DATA STATEMENTS
!----------
DATA BINT11 /-2.15894, -5.02156, -2.30430, -2.78090, &
                -4.22701, -2.64034, -3.38394, -3.30673, &
                -2.03919, -3.02050, -2.81317 /, &
        BCL11 /  1.48969, 2.31835, 1.52896, 1.90272, &
                 2.22534, 1.69973, 1.96060, 2.27613, &
                 1.64942, 1.88712, 1.47513 /
DATA BINT12 /-1.94951, -4.73762, -2.05828, -2.43200, &
                -4.17456, -2.24876, -3.13488, -2.93508, &
                -1.60998, -2.74410, -2.63387 /, &
        BCL12 /  1.22023, 1.98479, 1.25837, 1.60270, &
                 2.00749, 1.37600, 1.62368, 1.96125, &
                 1.32649, 1.58171, 1.35092 /
DATA BINT2 /2.666072, 1.756537, 2.705866, 3.115084, &
               2.654572, 3.059351, 2.622505, 3.300852, &
               3.060169, 2.452492, 2.622505 /
!----------
!  MAP VARIANT SPECIES = ORIGINAL 11 SPECIES
!  IF VARIANT SPECIES NUMBER OR ORDER CHANGES
!  THE FOLLOWING MAPPING SHOULD BE ADJUSTED HERE AND IN **CVSHAP**
!----------
DATA MAPNI / &
       1,    2,    3,    4,    5,    6,    7,    8,    9,   10, &
!     MH=MH RW=RC
      11,    6, 37*0/
!
!
DATA MAPAK / &
       8,    6,    4,   11,    5,    6,    7,    8,    9,    2, &
!     CW=WL,OH=WL,OS=MH
       2,    2,   11, 36*0/
!
!
DATA MAPBM / &
       1,    2,    3,    4,   11,    6,    7,    8,    9,   10, &
!     WB=WP,LM=WP,PY=WH,YC=RC,AS=WL,CW=WL,OS=MH,OH=WL
       1,    1,    5,    6,    2,    2,   11,    2, 31*0/
!
!
DATA MAPCA / &
       6,    6,    6,    4,    9,    9,    3,    5,   11,    1, &
!     KP=PP,LP=LP,CP=PP,LM=WP,JP=PP,SP=WP,WP=WP,PP=PP,MP=PP,GP=PP
      10,    7,   10,    1,   10,    1,    1,   10,   10,   10, &
!     WJ=RC,BR=ES,GS=RC,PY=WH,OS=MH,LO=WL,CY=WL,BL=WL,EO=WL,WO=WL
       6,    8,    6,    5,   11,    2,    2,    2,    2,    2, &
!     BO=WL,VO=WL,IO=WL,BM=WL,BU=WL,RA=WL,MA=WL,GC=WL,DG=WL,FL=WL
       2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
!     WN=WL,TO=WL,SY=WL,AS=WL,CW=WL,WI=WL,CN=WL,CL=WL,OH=WL
       2,    2,    2,    2,    2,    2,    2,    2,    2/
!
!
DATA MAPCI / &
       1,    2,    3,    4,    5,    6,    7,    8,    9,   10, &
!     WB=WP,PY=WH,AS=WL,WJ=RC,MC=WL,LM=WP,CW=WL,OS=MH,OH=WL
       1,    5,    2,    6,    2,    1,    2,   11,    2, 30*0/
!
!
DATA MAPCR / &
       9,    9,    3,    4,    4,   11,    6,    2,    1,    1, &
!     LP=LP,PI=PP,PP=PP,WB=WP,SW=WP,UJ=RC,BS=ES,ES=ES,WS=ES,AS=WL
       7,   10,   10,    1,    1,    6,    8,    8,    8,    2, &
!     NC=WL,PW=WL,GO=WL,AW=WL,EM=WL,BK=WL,SO=WL,PB=WL,AJ=RC,RM=RC
       2,    2,    2,    2,    2,    2,    2,    2,    6,    6, &
!     OJ=RC,ER=RC,PM=PP,PD=PP,AZ=PP,CI=LP,OS=MH,OH=WL
       6,    6,   10,   10,   10,    7,   11,    2, 11*0/
!
!
DATA MAPEC / &
       1,    2,    3,    4,    6,    4,    7,    8,    9,   10, &
!     WH=WH,MH=MH,PY=WH,WB=WP,NF=GF,WF=GF,LL=WL,YC=RC,WJ=RC,BM=WL
       5,   11,    5,    1,    4,    4,    2,    6,    6,    2, &
!     VN=WL,RA=WL,PB=WL,GC=WL,DG=WL,AS=WL,CW=WL,WO=WL,PL=WL,WI=WL
       2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
!     OS=MH,OH=WL
      11,    2, 17*0/
!
!
DATA MAPEM / &
       1,    2,    3,    1,    2,    6,    7,    8,    9,   10, &
!     GA=WL,AS=WL,CW=WL,BA=WL,PW=WL,NC=WL,PB=WL,OS=MH,OH=WL
       2,    2,    2,    2,    2,    2,    2,   11,    2, 30*0/
!
!
DATA MAPIE / &
       1,    2,    3,    4,    5,    6,    7,    8,    9,   10, &
!     MH=MH,WB=WP,LM=WP,LL=WL,PI=PP,RM=RC,PY=WH,AS=WL,CO=WL,MM=WL
      11,    1,    1,    2,   10,    6,    5,    2,    2,    2, &
!     PB=WL,OH=WL,OS=MH
       2,    2,   11, 26*0/
!
!
 DATA MAPSO / &
       1,    1,    3,    4,   11,    6,    7,    8,    9,   10, &
!     WJ=RC,GF=GF,AF=AF,SF=GF,NF=GF,WB=WP,WL=WL,RC=RC,WH=WH,PY=WH
       6,    4,    9,    4,    4,    1,    2,    6,    5,    5, &
!     WA=WL,RA=WL,BM=WL,AS=WL,CW=WL,CH=WL,WO=WL,WI=WL,GC=WL,MC=WL
       2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
!     MB=WL,OS=MH,OH=WL
       2,   11,    2, 16*0/
!
!
DATA MAPTT / &
       1,    1,    3,   10,    8,    2,    7,    8,    9,  10, &
!     UJ=RC,RM=RC,BI=WL,MM=WL,NC=WL,MC=WL,OS=MH,OH=WL
       6,    6,    2,    2,    2,    2,   11,    2, 31*0/
!
!
DATA MAPUT / &
       1,    1,    3,    4,    8,    2,    7,    8,    9,   10, &
!     PI=PP,WJ=RC,GO=WL,PM=PP,RM=RC,UJ=RC,GB=WP,NC=WL,FC=WL,MC=WL,
      10,    6,    2,   10,    6,    6,    1,    2,    2,    2, &
!    &BI=WL,BE=WL,OS=MH,OH=WL
       2,    2,   11,    2, 25*0/
!
!
DATA MAPWC / &
       4,    4,    4,    9,    9,   11,    4,    6,    6,    8, &
!     LP=LP,JP=PP,SP=WP,WP=WP,PP=PP,DF=DF,RW=RC,RC=RC,WH=WH,MH=MH
       7,   10,    1,    1,   10,    3,    6,    6,    5,   11, &
!     BM=WL,RA=WL,WA=WL,PB=WL,GC=WL,AS=WL,CW=WL,WO=WL,WJ=RC,LL=WL
       2,    2,    2,    2,    2,    2,    2,    2,    6,    2, &
!     WB=WP,KP=PP,PY=WH,DG=WL,HT=WL,CH=WL,WI=WL,  =MH,OT=MH
       1,   10,    5,    2,    2,    2,    2,   11,   11, 10*0/
!
!
DATA MAPWS / &
       1,    3,    4,    6,    6,   10,    9,   10,    7,    1, &
!     WP=WP,PM=PP,SF=GF,KP=PP,FP=PP,CP=PP,LM=WP,MP=PP,GP=PP,WE=PP,
       1,   10,    4,   10,   10,   10,    1,   10,   10,   10, &
!     GB=WP,BD=DF,RW=RC,MH=MH,WJ=RC,UJ=RC,CJ=RC,LO=WL,CY=WL,BL=WL,
       1,    3,    6,   11,    6,    6,    6,    2,    2,    2, &
!     BO=WL,VO=WL,IO=WL,TO=WL,GC=WL,AS=WL,CL=WL,MA=WL,DG=WL,BM=WL,
       2,    2,    2,    2,    2,    2,    2,    2,    2,    2, &
!     MC=WL,OS=MH,OH=WL
       2,   11,    2,  6*0/
!
!----------
!
SELECT CASE (VARACD)
!  ORIGINAL 11 SPECIES VARIANTS
CASE('KT','NC')
  MAPISP=MAPNI
CASE('AK')
  MAPISP=MAPAK
CASE('BM')
  MAPISP=MAPBM
!
!  SPECIES 4=GF IN OC VARIANT SO USING CA MAPPING IS OKAY FOR OC
!
CASE('CA','OC')
  MAPISP=MAPCA
CASE('CI')
  MAPISP=MAPCI
CASE('CR')
  MAPISP=MAPCR
CASE('EC')
  MAPISP=MAPEC
CASE('EM')
  MAPISP=MAPEM
CASE('IE')
  MAPISP=MAPIE
CASE('SO')
  MAPISP=MAPSO
CASE('TT')
  MAPISP=MAPTT
CASE('UT')
  MAPISP=MAPUT
!
!  SPECIES 23=MA, 24=TO, 25=GC IN OP VARIANT; ALL MAP TO WL. SO USING
!  WC MAPPING IS OKAY FOR OP
!
CASE('WC','PN','OP')
  MAPISP=MAPWC
CASE('WS')
  MAPISP=MAPWS
CASE DEFAULT
  MAPISP=1
END SELECT
!----------
!  CHECK FOR DEBUG.
!----------
CALL DBCHK(DEBUG,'CVCBMS',6,ICYC)
IF (DEBUG) WRITE (JOSTND,9000) ICYC
9000 FORMAT (/'**IN CVCBMS - CYCLE = ',I2/'        I      ISPI', &
   '         D         H        CL       AGE       TPA', &
    '        RD       DDS    TRFBMS')
!----------
!  RETURN IF NOTREES OPTION IN EFFECT.
!----------
IF (ITRN .GT. 0) GO TO 2
IF (DEBUG) WRITE (JOSTND,9001) ITRN
9001 FORMAT ('ITRN =', I5,' : NOTREES : RETURN TO **CVCNOP**')
RETURN
2 CONTINUE
!----------
!  USE PRE-THIN/FIRE DENSITY STATISTICS IF A THINNING OR FIRE
!  HAS JUST OCCURRED.
!----------
RMSD = RMSQD
TPA = TPROB
IF ((LTHIN.OR.LFIRE).AND.(ORMSQD.GT.0.))RMSD = ORMSQD
IF ((LTHIN.OR.LFIRE).AND.(OLDTPA.GT.0.))TPA = OLDTPA
ALNTPA = ALOG(TPA)
!----------
!  ENTER TREE LOOP
!----------
DO 110 I = 1,ITRN
ISPI = MAPISP(ISP(I))
D = DBH(I)
H = HT(I)
IICR = ICR(I)
CL = FLOAT(IICR)*H/100.
RD = D/RMSD
DDS = (2*D*DG(I) + DG(I)**2)/FINT
IF (DDS .LT. .0001) DDS = .0001
!----------
!  COMPUTE TREE AGE
!----------
AGE = FLOAT (ITRE(I) + IY(ICYC+1) - IY(1))
!----------
!  BRANCH ON MODEL OPTION.  (THIS OPTION NOT CURRENTLY USED, BECAUSE
!  PROGNOSIS DOES NOT CARRY TREE AGES.)
!----------
!  COVOPT = 1  GO TO FOLIAGE WEIGHT FUNCTIONS CONTAINING AGE
!              AS AN INDEPENDENT VARIABLE
!  COVOPT = 2  GO TO FOLIAGE WEIGHT FUNCTIONS CONTAINING DELTA
!              DIAMETER SQUARED (DDS) FOR TREES .GE. 3.5 INCHES,
!              AND HEIGHT FOR TREES .LT. 3.5 INCHES IN PLACE OF
!              AGE AS INDEPENDENT VARIABLES
!----------
GO TO (5, 15),COVOPT
!===============================================================
!          MODEL OPTION 1     (USES TREE AGE)
!===============================================================
!  BRANCH ON DBH
!----------
5 IF (D .LT. 3.5) GO TO 10
!----------
!  COMPUTE FOLIAGE BIOMASS FOR TREES 3.5 INCHES AND LARGER
!----------
TRFBMS(I) = EXP (BINT2(ISPI) + 2.086241*ALOG(D) - 1.077047*ALOG(H) &
            + 0.690825*ALOG(CL) - 0.308847*ALOG(AGE) - 0.142069 &
            *ALNTPA + 0.399244*ALOG(RD))
!----------
!  CORRECT ESTIMATE FOR NEGATIVE BIAS.
!  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP(.5*.13301)
!----------
TRFBMS(I) = TRFBMS(I)*1.06877
GO TO 100
!----------
!  COMPUTE FOLIAGE BIOMASS FOR TREES LESS THAN 3.5 INCHES
!----------
10 TRFBMS(I) = EXP (BINT11(ISPI) + 0.22823*ALOG(AGE) &
            + BCL11(ISPI)*ALOG(CL) - 0.13550*ALNTPA)
!----------
!  CORRECT ESTIMATE FOR NEGATIVE BIAS
!  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP(.5*.23751)
!----------
TRFBMS(I) = TRFBMS(I)*1.12609
GO TO 100
!===============================================================
!          MODEL OPTION 2     (USES DDS)
!===============================================================
!  BRANCH ON DBH
!----------
15 IF (D .LT. 3.5) GO TO 20
!----------
!  COMPUTE FOLIAGE BIOMASS FOR TREES 3.5 INCHES AND LARGER
!----------
TRFBMS(I) = EXP (BINT2(ISPI) + 1.468547*ALOG(D) + &
            0.308847*ALOG(DDS) - 1.077047*ALOG(H) &
            + 0.690825*ALOG(CL) - 0.142096*ALNTPA &
            + 0.399244*ALOG(RD))
!----------
!  NO NEGATIVE BIAS CORRECTION APPLIED, BECAUSE THERE IS
!  NO ESTIMATE OF MSE FOR THIS EQUATION.
!----------
GO TO 100
!----------
!  COMPUTE FOLIAGE BIOMASS FOR TREES LESS THAN 3.5 INCHES
!----------
20 TRFBMS(I) = EXP (BINT12(ISPI) + BCL12(ISPI)*ALOG(CL) &
             - 0.12975*ALNTPA + 0.40350*ALOG(H))
!----------
!  CORRECT FOR NEGATIVE BIAS.
!  BIAS ADJUSTMENT IS EXP(.5*MSE) = EXP (.5*.24759)
!----------
TRFBMS(I) = TRFBMS(I) * 1.13178
!
100 CONTINUE
IF (DEBUG) WRITE (JOSTND,9002) I,ISPI,D,H,CL,AGE,TPA,RD,DDS, &
                                  TRFBMS(I)
9002 FORMAT (2I10,8F10.2)
110 CONTINUE
RETURN
END

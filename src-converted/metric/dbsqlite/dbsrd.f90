SUBROUTINE DBSRD1(IYEAR,CNPLT,IAGE,DTYPE,NCENT,DAREA,SRATE, &
     STUTPA,STUBA,MRTTPA,MRTCUFT,UNTPA,INTPA,PCTROOT, &
     MRCUFT,DABA,NEWIN,NEWEXP,NEWTOT)

IMPLICIT NONE
!
! METRIC-DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST ROOT DISEASE MODEL REPORT,
!              SUMMARY STATISTICS FOR ROOT DISEASE AREAS (PER ACRE BASED ON
!              DISEASE AREA ONLY).
!     AUTH: L. DAVID -- FMSC (METI) -- 12/30/2014
!
!     ARGUMENT LIST
!      1: IYEAR   -- YEAR
!      2: NPLT    -- STAND ID
!      3: IAGE    -- STAND AGE
!      4: DTYPE   -- DISEASE TYPE
!      5: NCENT   -- NUMBER OF DISEASE CENTERS
!      6: DAREA   -- DISEASE AREA (ACRES)
!      7: SRATE   -- SPREAD RATE FT/YR
!      8: STUTPA  -- STUMPS/ACRE, DISEASE AREA ONLY
!      9: STUBA   -- STUMP BA/ACRE, DISEASE AREA ONLY
!     10: MRTTPA  -- TPA KILLED, DISEASE AREA ONLY
!     11: MRTCUFT -- CUFT/ACRE KILLED, DISEASE AREA ONLY
!     12: UNTPA   -- UNINFECTED LIVE TPA IN DISEASE AREA
!     13: INTPA   -- INFECTED LIVE TPA IN DISEASE AREA
!     14: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED
!     15: MRCUFT  -- TOTAL MERCH CUFT IN DISEASE AREA
!     16: DABA    -- LIVE BA/ACRE IN DISEASE AREA
!     17: NEWIN   -- NEWLY INFECTED PROPORTION WITHIN DISEASE AREA
!     18: NEWEXP  -- NEWLY INFECTED PROPORTION DUE TO AREA EXPANSION
!     19: NEWTOT  -- NEWLY INFECTED PROPORTION OF TOTAL STAND

INCLUDE 'DBSCOM.f90'
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_RESET &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_STEP &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS &
   _WIN64) &
   $ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE &
   $ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG &
   $ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC &
   $ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE &
   $ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN &
   $ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE &
   $ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET &
   $ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP &
   $ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS

!     ARGUMENT LIST

INTEGER IYEAR, IAGE, NCENT
REAL    DAREA, SRATE, STUTPA, STUBA, MRTTPA, MRTCUFT, UNTPA, &
           INTPA, PCTROOT, MRCUFT, DABA, NEWIN, NEWEXP, NEWTOT
CHARACTER(LEN=1)  DTYPE
CHARACTER(LEN=26) CNPLT
INTEGER iRet

!     LOCAL VARIABLES

INTEGER ColNumber

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

CHARACTER*2000    SQLStmtStr

!     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
!     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
!
DOUBLE PRECISION &
     DAREAD, SRATED, STUTPAD, STUBAD, MRTTPAD, MRTCUFTD, UNTPAD, &
     INTPAD, PCTROOTD, MRCUFTD, DABAD, NEWIND, NEWEXPD, NEWTOTD

!     If RD Summary not selected for DB output, return.
!      WRITE(23,*) "IN DBSRD1: IRD1 = ",IRD1               ! DEBUG

IF(IRD1 .EQ. 0) RETURN

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_RD_Sum_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_RD_Sum_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year Int null,'// &
         'Age Int null,'// &
         'RD_Type char(1) null,'// &
         'Num_Centers Int null,'// &
         'RD_Area real null,'// &
         'Spread_M_per_Year real null,'// &
         'Stumps_per_Ha real null,'// &
         'Stumps_BA real null,'// &
         'Mort_TPH real null,'// &
         'Mort_CuM real null,'// &
         'UnInf_TPH real null,'// &
         'Inf_TPH real null,'// &
         'Ave_Pct_Root_Inf real null,'// &
         'Live_Merch_CuM real null,'// &
         'Live_BA real null,'// &
         'New_Inf_Prp_Ins real null,'// &
         'New_Inf_Prp_Exp real null,'// &
         'New_Inf_Prp_Tot real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    IRD1 = 0
    RETURN
  ENDIF
ENDIF

!     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
!     WRITE RECORD TO DATABASE

DAREAD   = DAREA
SRATED   = SRATE
STUTPAD  = STUTPA
STUBAD   = STUBA
MRTTPAD  = MRTTPA
MRTCUFTD = MRTCUFT
UNTPAD   = UNTPA
INTPAD   = INTPA
PCTROOTD = PCTROOT
MRCUFTD  = MRCUFT
DABAD    = DABA
NEWIND   = NEWIN
NEWEXPD  = NEWEXP
NEWTOTD  = NEWTOT

!     DEFINE DATABASE INSERT STATEMENT

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Sum_Metric', &
     ' (CaseID,StandID,Year,Age,RD_Type,Num_Centers,RD_Area,', &
     'Spread_M_per_Year,Stumps_per_Ha,Stumps_BA,', &
     'Mort_TPH,Mort_CuM,UnInf_TPH,Inf_TPH,Ave_Pct_Root_Inf,', &
     'Live_Merch_CuM,Live_BA,', &
     'New_Inf_Prp_Ins,New_Inf_Prp_Exp,New_Inf_Prp_Tot) ', &
     ' VALUES (''',CASEID,''',''',TRIM(CNPLT),''', &
     ?,?,''',DTYPE,''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IRD1 = 0
   RETURN
ENDIF

ColNumber=1                 ! YEAR
iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1       ! AGE
iRet=fsql3_bind_int(IoutDBref,ColNumber,IAGE)

ColNumber=ColNumber+1       ! NUMBER OF DISEASE CENTERS
iRet=fsql3_bind_int(IoutDBref,ColNumber,NCENT)

ColNumber=ColNumber+1       ! DISEASE AREA IN ACRES
iRet=fsql3_bind_double(IoutDBref,ColNumber,DAREAD)

ColNumber=ColNumber+1       ! SPREAD RATE FEET PER YEAR
iRet=fsql3_bind_double(IoutDBref,ColNumber,SRATED)

ColNumber=ColNumber+1       ! DEAD STUMPS PER ACRE
iRet=fsql3_bind_double(IoutDBref,ColNumber,STUTPAD)

ColNumber=ColNumber+1       ! STUMPS BA SQFT PER ACRE
iRet=fsql3_bind_double(IoutDBref,ColNumber,STUBAD)

ColNumber=ColNumber+1       ! TREES KILLED TPA
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTTPAD)

ColNumber=ColNumber+1       ! TREES KILLED CUFT PER ACRE
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTCUFTD)

ColNumber=ColNumber+1       ! UNINFECTED LIVE TREES TPA
iRet=fsql3_bind_double(IoutDBref,ColNumber,UNTPAD)

ColNumber=ColNumber+1       ! INFECTED LIVE TREES TPA
iRet=fsql3_bind_double(IoutDBref,ColNumber,INTPAD)

ColNumber=ColNumber+1       ! AVERAGE % ROOTS INFECTED
iRet=fsql3_bind_double(IoutDBref,ColNumber,PCTROOTD)

ColNumber=ColNumber+1       ! LIVE MERCH CUFT PER ACRE
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRCUFTD)

ColNumber=ColNumber+1       ! LIVE BASAL AREA PER ACRE
iRet=fsql3_bind_double(IoutDBref,ColNumber,DABAD)

ColNumber=ColNumber+1       ! NEWLY INFECTED PROP INSIDE DIS AREA
iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWIND)

ColNumber=ColNumber+1       ! NEWLY INFECTED PROP DUE TO EXPANSION
iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWEXPD)

ColNumber=ColNumber+1       ! NEWLY INFECTED PROPORTION TOTAL
iRet=fsql3_bind_double(IoutDBref,ColNumber,NEWTOTD)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IRD1 = 0
ENDIF
RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSRD2(IYEAR,CNPLT,DTYPE,DAREA,CSP,DDBHCL, &
     DTPA,LDBHCL,LUNTPA,LINTPA,PCTROOT)

IMPLICIT NONE
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
!              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
!              DISEASE AREA ONLY).
!     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
!
!     ARGUMENT LIST
!      1: IYEAR   -- YEAR
!      2: NPLT    -- STAND ID
!      3: DTYPE   -- DISEASE TYPE
!      4: DAREA   -- DISEASE AREA (ACRES)
!      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
!      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
!      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
!      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
!      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
!     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
!     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED

INCLUDE 'PRGPRM.f90'

INCLUDE 'PLOT.f90'

INCLUDE 'DBSCOM.f90'
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_RESET &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_STEP &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS &
   _WIN64) &
   $ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE &
   $ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG &
   $ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC &
   $ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE &
   $ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN &
   $ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE &
   $ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET &
   $ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP &
   $ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS

!     ARGUMENT LIST

INTEGER IYEAR
REAL    DAREA, DDBHCL(6), DTPA, LDBHCL(6), LUNTPA, LINTPA, &
           PCTROOT
CHARACTER(LEN=1)  DTYPE
CHARACTER(LEN=2)  CSP
CHARACTER(LEN=26) CNPLT
INTEGER iRet

!     LOCAL VARIABLES

CHARACTER(LEN=8)  CSP1,CSP2,CSP3
INTEGER ColNumber,I,I2

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

CHARACTER*2000    SQLStmtStr

!     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
!     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
!
DOUBLE PRECISION &
     DAREAD, DDBHCLD(6), DTPAD, LDBHCLD(6), LUNTPAD, LINTPAD, &
     PCTROOTD

!     If RD Detail not selected for DB output, return.

IF(IRD2 .EQ.0) RETURN

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_RD_Det_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_RD_Det_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year Int null,'// &
         'RD_Type char(1) not null,'// &
         'RD_Area real null,'// &
         'SpeciesFVS text not null,'// &
         'SpeciesPLANTS text not null,'// &
         'SpeciesFIA text not null,'// &
         'Mort_10Pctile_DBH real null,'// &
         'Mort_30Pctile_DBH real null,'// &
         'Mort_50Pctile_DBH real null,'// &
         'Mort_70Pctile_DBH real null,'// &
         'Mort_90Pctile_DBH real null,'// &
         'Mort_100Pctile_DBH real null,'// &
         'Mort_TPH_Total real null,'// &
         'Live_10Pctile_DBH real null,'// &
         'Live_30Pctile_DBH real null,'// &
         'Live_50Pctile_DBH real null,'// &
         'Live_70Pctile_DBH real null,'// &
         'Live_90Pctile_DBH real null,'// &
         'Live_100Pctile_DBH real null,'// &
         'UnInf_TPH_Total real null,'// &
         'Inf_TPH_Total real null,'// &
         'Pct_Roots_Inf real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IRD2 = 0
     RETURN
   ENDIF
ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
!
DO I2 = 1,MAXSP
  IF (CSP .EQ. JSP(I2)) THEN
    CSP1 = JSP(I2)
    CSP2 = PLNJSP(I2)
    CSP3 = FIAJSP(I2)
  ENDIF
ENDDO

!     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
!     WRITE RECORD TO DATABASE

DAREAD   = DAREA
DTPAD    = DTPA
LUNTPAD  = LUNTPA
LINTPAD  = LINTPA
PCTROOTD = PCTROOT
DO I=1,6
  DDBHCLD(I) = DDBHCL(I)
  LDBHCLD(I) = LDBHCL(I)
END DO

!     DEFINE DATABASE INSERT STATEMENT

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Det_Metric', &
     ' (CaseID,StandID,Year,RD_Type,RD_Area,', &
     'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
     'Mort_10Pctile_DBH,Mort_30Pctile_DBH,Mort_50Pctile_DBH,', &
     'Mort_70Pctile_DBH,Mort_90Pctile_DBH,Mort_100Pctile_DBH,', &
     'Mort_TPH_Total,Live_10Pctile_DBH,Live_30Pctile_DBH,', &
     'Live_50Pctile_DBH,Live_70Pctile_DBH,Live_90Pctile_DBH,', &
     'Live_100Pctile_DBH,UnInf_TPH_Total,Inf_TPH_Total,', &
     'Pct_Roots_Inf) ', &
     " VALUES ('",CASEID,"','",TRIM(CNPLT),"',?,'",DTYPE,"'", &
     ",?,'",TRIM(CSP1),"','",TRIM(CSP2),"','",TRIM(CSP3),"'", &
     ",?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IRD2 = 0
   RETURN
ENDIF

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1                 ! YEAR
iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1       ! DISEASE AREA IN ACRES
iRet=fsql3_bind_double(IoutDBref,ColNumber,DAREAD)

ColNumber=ColNumber+1       ! Mortality 10 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(1))

ColNumber=ColNumber+1       ! Mortality 30 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(2))

ColNumber=ColNumber+1       ! Mortality 50 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(3))

ColNumber=ColNumber+1       ! Mortality 70 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(4))

ColNumber=ColNumber+1       ! Mortality 90 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(5))

ColNumber=ColNumber+1       ! Mortality 100 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,DDBHCLD(6))

ColNumber=ColNumber+1       ! Mortality TPA total
iRet=fsql3_bind_double(IoutDBref,ColNumber,DtpaD)

ColNumber=ColNumber+1       ! Live trees 10 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(1))

ColNumber=ColNumber+1       ! Live trees 30 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(2))

ColNumber=ColNumber+1       ! Live trees 50 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(3))

ColNumber=ColNumber+1       ! Live trees 70 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(4))

ColNumber=ColNumber+1       ! Live trees 90 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(5))

ColNumber=ColNumber+1       ! Live trees 100 percentile DBH
iRet=fsql3_bind_double(IoutDBref,ColNumber,LDBHCLD(6))

ColNumber=ColNumber+1       ! Live uninfected TPA total
iRet=fsql3_bind_double(IoutDBref,ColNumber,LUNTPAD)

ColNumber=ColNumber+1       ! Live infected TPA total
iRet=fsql3_bind_double(IoutDBref,ColNumber,LINTPAD)

ColNumber=ColNumber+1       ! AVERAGE % ROOTS INFECTED
iRet=fsql3_bind_double(IoutDBref,ColNumber,PCTROOTD)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IRD2 = 0
ENDIF
RETURN

END


!-------------------------------------------------------------------------------

SUBROUTINE DBSRD3(IYEAR, CNPLT, CSP, MRTININ, ININTOT, ININLIV, &
     MRTINUN, INUNTOT, INUNLIV, MRTOUT, OUTTOT, OUTLIV, STDMRT)

IMPLICIT NONE
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND ROOT DISEASE MODEL REPORT,
!              DBH PERCENTILE DETAIL FOR ROOT DISEASE AREAS (PER ACRE BASED ON
!              DISEASE AREA ONLY).
!     AUTH: L. DAVID -- FMSC (METI) -- 01/06/2015
!
!     ARGUMENT LIST
!      1: IYEAR   -- YEAR
!      2: NPLT    -- STAND ID
!      3: DTYPE   -- DISEASE TYPE
!      4: DAREA   -- DISEASE AREA (ACRES)
!      5: CSP     -- TREE SPECIES CHARACTER ABBREVIATION
!      6: DDBHCL  -- DEAD %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
!      7: DTPA    -- DEAD TREES/ACRE TOTAL, DISEASE AREA ONLY
!      8: LDBHCL  -- LIVE %TILE CLASSES 1-6 (ARRAY), DISEASE AREA ONLY
!      9: LUNTPA  -- LIVE UNINFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
!     10: LINTPA  -- LIVE INFECTED TREES/ACRE TOTAL, DISEASE AREA ONLY
!     11: PCTROOT -- AVERAGE PERCENT OF ROOTS INFECTED

INCLUDE 'PRGPRM.f90'

INCLUDE 'PLOT.f90'

INCLUDE 'DBSCOM.f90'
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_RESET &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_STEP &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS &
   _WIN64) &
   $ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE &
   $ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG &
   $ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC &
   $ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE &
   $ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN &
   $ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE &
   $ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET &
   $ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP &
   $ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS


!     ARGUMENT LIST

INTEGER IYEAR
REAL    MRTININ(7), MRTINUN(7), MRTOUT(7), &
           ININTOT, INUNTOT, OUTTOT, STDMRT, &
           ININLIV, INUNLIV, OUTLIV
CHARACTER(LEN=2)  CSP
CHARACTER(LEN=26) CNPLT
INTEGER iRet

!     LOCAL VARIABLES

CHARACTER(LEN=8)  CSP1,CSP2,CSP3
INTEGER ColNumber,I,I2

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

CHARACTER*2000    SQLStmtStr

!     DOUBLE PRECISION VARIABLES FOR REASSIGNMENT OF INCOMING REAL
!     VARIABLES TO BE WRITTEN TO OUTPUT DATABASE.
!
DOUBLE PRECISION &
     MRTININD(7), MRTINUND(7), MRTOUTD(7), &
     ININTOTD, INUNTOTD, OUTTOTD, STDMRTD, &
     ININLIVD, INUNLIVD, OUTLIVD

!     If RD Bark Beetle not selected for DB output, return.

IF(IRD3 .EQ. 0) RETURN

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_RD_Beetle_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_RD_Beetle_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year Int null,'// &
         'SpeciesFVS         text not null,'// &
         'SpeciesPLANTS      text not null,'// &
         'SpeciesFIA         text not null,'// &
         'In_Inf_0_127_DBH       real null,'// &
         'In_Inf_127_254_DBH     real null,'// &
         'In_Inf_254_381_DBH     real null,'// &
         'In_Inf_381_508_DBH     real null,'// &
         'In_Inf_508_635_DBH     real null,'// &
         'In_Inf_635_762_DBH     real null,'// &
         'In_Inf_762_DBH         real null,'// &
         'In_Inf_Mort            real null,'// &
         'In_Inf_Live_before     real null,'// &
         'In_UnInf_0_127_DBH     real null,'// &
         'In_UnInf_127_254_DBH   real null,'// &
         'In_UnInf_254_381_DBH   real null,'// &
         'In_UnInf_381_508_DBH   real null,'// &
         'In_UnInf_508_635_DBH   real null,'// &
         'In_UnInf_635_762_DBH   real null,'// &
         'In_UnInf_762_DBH       real null,'// &
         'In_UnInf_Mort          real null,'// &
         'In_UnInf_Live_Before   real null,'// &
         'Outside_0_127_DBH      real null,'// &
         'Outside_127_254_DBH    real null,'// &
         'Outside_254_381_DBH    real null,'// &
         'Outside_381_508_DBH    real null,'// &
         'Outside_508_635_DBH    real null,'// &
         'Outside_635_762_DBH    real null,'// &
         'Outside_762_DBH        real null,'// &
         'Outside_Mort           real null,'// &
         'Outside_Live_Before    real null,'// &
         'Stand_Mort_Total       real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    IRD3 = 0
    RETURN
  ENDIF
ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

DO I2 = 1,MAXSP
  IF (CSP .EQ. JSP(I2)) THEN
    CSP1 = JSP(I2)
    CSP2 = PLNJSP(I2)
    CSP3 = FIAJSP(I2)
  ENDIF
ENDDO

!     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
!     WRITE RECORD TO DATABASE

DO I=1,7
  MRTININD(I) = MRTININ(I)
  MRTINUND(I) = MRTINUN(I)
  MRTOUTD(I)  = MRTOUT(I)
END DO

ININTOTD = ININTOT
ININLIVD = ININLIV
INUNTOTD = INUNTOT
INUNLIVD = INUNLIV
OUTTOTD  = OUTTOT
OUTLIVD  = OUTLIV
STDMRTD  = STDMRT

!     DEFINE DATABASE INSERT STATEMENT

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_RD_Beetle_Metric', &
     ' (CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
     'In_Inf_0_127_DBH,In_Inf_127_254_DBH,In_Inf_254_381_DBH,', &
     'In_Inf_381_508_DBH,In_Inf_508_635_DBH,In_Inf_635_762_DBH,', &
     'In_Inf_762_DBH,In_Inf_Mort,In_Inf_Live_before,', &
     'In_UnInf_0_127_DBH,In_UnInf_127_254_DBH,In_UnInf_254_381_DBH,', &
     'In_UnInf_381_508_DBH,In_UnInf_508_635_DBH,', &
     'In_UnInf_635_762_DBH,', &
     'In_UnInf_762_DBH,In_UnInf_Mort,In_UnInf_Live_Before,', &
     'Outside_0_127_DBH,Outside_127_254_DBH,Outside_254_381_DBH,', &
     'Outside_381_508_DBH,Outside_508_635_DBH,Outside_635_762_DBH,', &
     'Outside_762_DBH,Outside_Mort,Outside_Live_Before,', &
     'Stand_Mort_Total) ', &
     " VALUES (","'",CASEID,"','",TRIM(CNPLT),"',?", &
     ",'",TRIM(CSP1),"','",TRIM(CSP2),"','",TRIM(CSP3),"',", &
     "?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IRD3= 0
   RETURN
ENDIF

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1                 ! YEAR
iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1       ! INSIDE INFECTED 0-5
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(1))

ColNumber=ColNumber+1       ! INSIDE INFECTED 5-10
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(2))

ColNumber=ColNumber+1       ! INSIDE INFECTED 10-15
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(3))

ColNumber=ColNumber+1       ! INSIDE INFECTED 15-20
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(4))

ColNumber=ColNumber+1       ! INSIDE INFECTED 20-25
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(5))

ColNumber=ColNumber+1       ! INSIDE INFECTED 25-30
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(6))

ColNumber=ColNumber+1       ! INSIDE INFECTED 30+
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTININD(7))

ColNumber=ColNumber+1       ! INSIDE INFECTED TOTAL
iRet=fsql3_bind_double(IoutDBref,ColNumber,ININTOTD)

ColNumber=ColNumber+1       ! INSIDE INFECTED LIVE BEFORE ATTACK
iRet=fsql3_bind_double(IoutDBref,ColNumber,ININLIVD)

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 0-5
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(1))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 5-10
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(2))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 10-15
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(3))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 15-20
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(4))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 20-25
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(5))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 25-30
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(6))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED 30+
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTINUND(7))

ColNumber=ColNumber+1       ! INSIDE UNINFECTED TOTAL
iRet=fsql3_bind_double(IoutDBref,ColNumber,INUNTOTD)

ColNumber=ColNumber+1       ! INSIDE UNINFECTED LIVE BEFORE ATTACK
iRet=fsql3_bind_double(IoutDBref,ColNumber,INUNLIVD)

ColNumber=ColNumber+1       ! OUTSIDE MORT 0-5
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(1))

ColNumber=ColNumber+1       ! OUTSIDE MORT 5-10
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(2))

ColNumber=ColNumber+1       ! OUTSIDE MORT 10-15
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(3))

ColNumber=ColNumber+1       ! OUTSIDE MORT 15-20
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(4))

ColNumber=ColNumber+1       ! OUTSIDE MORT 20-25
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(5))

ColNumber=ColNumber+1       ! OUTSIDE MORT 25-30
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(6))

ColNumber=ColNumber+1       ! OUTSIDE MORT 30+
iRet=fsql3_bind_double(IoutDBref,ColNumber,MRTOUTD(7))

ColNumber=ColNumber+1       ! OUTSIDE MORT TOTAL
iRet=fsql3_bind_double(IoutDBref,ColNumber,OUTTOTD)

ColNumber=ColNumber+1       ! OUTSIDE LIVE BEFORE ATTACK
iRet=fsql3_bind_double(IoutDBref,ColNumber,OUTLIVD)

ColNumber=ColNumber+1       ! STAND MORT TOTAL
iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMRTD)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IRD3 = 0
ENDIF
RETURN
END

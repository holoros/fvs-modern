SUBROUTINE DBSMIS1(IYEAR,NPLT,CSP, &
     SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4, &
     KODE)
!----------
! METRIC-DBS $Id$
!----------
!     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
!
!     1:     Year
!     2:     Mean DMR for species - SPDMR4
!     3:     Mean DMI for species - SPDMI4
!     4:     Infected trees/acre for species - SPINF4
!     5:     Mortality trees/acre for species - SPMRT4
!     6:     Infected trees/acre % for species - SPPIN4
!     7:     Mortality trees/acre % for species - SPPMR4
!     8:     Stand trees/acre % for species - SPPOC4

use f90SQLConstants
use f90SQLStructures
use f90SQL
IMPLICIT NONE

INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR
REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
INTEGER KODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber
 INTEGER           ID,I

DOUBLE PRECISION  SPDMR4B, SPDMI4B
INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

CHARACTER*2000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME
CHARACTER(LEN=26) NPLT
CHARACTER(LEN=2)  CSP(4)

!     ICASE - CASE NUMBER FROM THE FVSRUN TABLE

!     INITIALIZE VARIABLES

IF(IDM1.EQ.0) RETURN
IF(IDM1.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut, &
     iRet)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  IDM1 = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSMIS1:DSN Connection')
  GOTO 100
ENDIF

!     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_DM_Spp_Sum$]'
ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
  TABLENAME = '"FVS_DM_Spp_Sum"'
ELSE
  TABLENAME = 'FVS_DM_Spp_Sum'
ENDIF
SQLStmtStr= 'SELECT * FROM ' // TABLENAME

CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR. &
     iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID text null,'// &
         'Year Int null,'// &
         'Spp Text null,'// &
         'Mean_DMR double null,'// &
         'Mean_DMI double null,'// &
         'Inf_TPH int null,'// &
         'Mort_TPH int null,'// &
         'Inf_TPH_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Stnd_TPH_Pct int null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'ID Int,'// &
         'CaseID Int,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Spp Text,'// &
         'Mean_DMR Number,'// &
         'Mean_DMI Number,'// &
         'Inf_TPH Int,'// &
         'Mort_TPH Int,'// &
         'Inf_TPH_Pct Int,'// &
         'Mort_TPH_Pct Int,'// &
         'Stnd_TPH_Pct Int)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID char(26) not null,'// &
         'Year Int null,'// &
         'Spp char(2) null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null,'// &
         'Inf_TPH int null,'// &
         'Mort_TPH int  null,'// &
         'Inf_TPH_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Stnd_TPH_Pct int null)'
  ENDIF

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
  CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS1:Creating Table: '//trim(SQLStmtStr))
  DM1ID = 0
ENDIF

!     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

IF(DM1ID .EQ. -1) THEN
  CALL DBSGETID(TABLENAME,'Id',ID)
  DM1ID = ID
ENDIF

!     LOOP OVER 4 TOP SPECIES

DO I = 1,4

!       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

  DM1ID = DM1ID + 1
  IF(DM1ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

!       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
!       INTEGER COPIES OF REAL VECTOR INPUTS

  SPDMR4B = SPDMR4(I)
  SPDMI4B = SPDMI4(I)

  ISPINF4 = NINT(SPINF4(I))
  ISPMRT4 = NINT(SPMRT4(I))
  ISPPIN4 = NINT(SPPIN4(I))
  ISPPMR4 = NINT(SPPMR4(I))
  ISPPOC4 = NINT(SPPOC4(I))

  WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID, &
       StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPH, &
       Mort_TPH,Inf_TPH_Pct,Mort_TPH_Pct,Stnd_TPH_Pct) VALUES &
       (?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?, &
       ',CHAR(39),TRIM(CSP(I)),CHAR(39),',?,?,?,?,?,?,?)'

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

!       PREPARE QUERY

  CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 ID
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),DM1ID,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 2 CASEID
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 3 YEAR
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 4 MEAN DMR
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
       INT(5,SQLSMALLINT_KIND),SPDMR4B,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 5 MEAN DMI
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
       INT(5,SQLSMALLINT_KIND),SPDMI4B,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 6 TPH INFECTED
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPINF4,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 7 TPH MORTALITY
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPMRT4,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 8 % TPH INFECTED
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPIN4,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 9 % TPH MORTALITY
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPMR4,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 10 % STAND TPH
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPOC4,f90SQL_NULL_PTR,iRet)

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
  CALL f90SQLExecute(StmtHndlOut,iRet)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS1:Inserting Row')

  ENDDO

!     RELEASE STATEMENT HANDLE

100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

  RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE, &
     ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM, &
     ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
!----------
!  **DBSMIS2--DBS  DATE OF LAST REVISION:  04/20/04
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
!
!     1:     Year - IYEAR
!     2:     Age - NAGE
!     3:     Stand trees/acre - ISTTPAT
!     4:     Stand basal area - IBA
!     5:     Stand volume - ISTVOL
!     6:     Infected trees/acre - ISTTPAI
!     7:     Infected basal area - ISTBAI
!     8:     Infected volume - ISTVOLI
!     9:     Mortality trees/acre - ISTTPAM
!     10:    Mortality basal area - ISTBAM
!     11:    Mortality volume - ISTVOLM
!     12:    Infected trees/acre % - ISTPIT
!     13:    Infected volume % - ISTPIV
!     14:    Mortality trees/acre % - ISTPMT
!     15:    Mortality volume % - ISTPMV
!     16:    Mean DMR - STDMR
!     17:    Mean DMI - STDMI

use f90SQLConstants
use f90SQLStructures
use f90SQL
IMPLICIT NONE

INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR,NAGE
CHARACTER(LEN=26) NPLT
INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
REAL    STDMR,STDMI
INTEGER KODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber
INTEGER           ID

DOUBLE PRECISION  STDMRB, STDMIB

CHARACTER*2000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME

!     INITIALIZE VARIABLES

IF(IDM2.EQ.0) RETURN
IF(IDM2.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut, &
     iRet)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  IDM2 = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSMIS2:DSN Connection')
  GOTO 100
ENDIF

!     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_DM_Stnd_Sum$]'
ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
  TABLENAME = '"FVS_DM_Stnd_Sum"'
ELSE
  TABLENAME = 'FVS_DM_Stnd_Sum'
ENDIF
SQLStmtStr= 'SELECT * FROM ' // TABLENAME

CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR. &
     iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID text null,'// &
         'Year int null,'// &
         'Age int null,'// &
         'Stnd_TPH int null,'// &
         'Stnd_BA int null,'// &
         'Stnd_Vol int null,'// &
         'Inf_TPH int null,'// &
         'Inf_BA int null,'// &
         'Inf_Vol int null,'// &
         'Mort_TPH int null,'// &
         'Mort_BA int null,'// &
         'Mort_Vol int null,'// &
         'Inf_TPH_Pct int null,'// &
         'Inf_Vol_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Mort_Vol_Pct int null,'// &
         'Mean_DMR double null,'// &
         'Mean_DMI double null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'ID Int,'// &
         'CaseID Int,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Age Int,'// &
         'Stnd_TPH Int,'// &
         'Stnd_BA Int,'// &
         'Stnd_Vol Int,'// &
         'Inf_TPH Int,'// &
         'Inf_BA Int,'// &
         'Inf_Vol Int,'// &
         'Mort_TPH Int,'// &
         'Mort_BA Int,'// &
         'Mort_Vol Int,'// &
         'Inf_TPH_Pct Int,'// &
         'Inf_Vol_Pct Int,'// &
         'Mort_TPH_Pct Int,'// &
         'Mort_Vol_Pct Int,'// &
         'Mean_DMR Number,'// &
         'Mean_DMI Number)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID char(26) not null,'// &
         'Year int null,'// &
         'Age int null,'// &
         'Stnd_TPH int null,'// &
         'Stnd_BA int null,'// &
         'Stnd_Vol int null,'// &
         'Inf_TPH int null,'// &
         'Inf_BA int null,'// &
         'Inf_Vol int null,'// &
         'Mort_TPH int null,'// &
         'Mort_BA int null,'// &
         'Mort_Vol int null,'// &
         'Inf_TPH_Pct int null,'// &
         'Inf_Vol_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Mort_Vol_Pct int null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null)'
  ENDIF

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
  CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS2:Creating Table: '//trim(SQLStmtStr))
  DM2ID = 0
ENDIF

!     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

IF(DM2ID .EQ. -1) THEN
  CALL DBSGETID(TABLENAME,'Id',ID)
  DM2ID = ID
ENDIF

!     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

DM2ID = DM2ID + 1
IF(DM2ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

!     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

STDMRB = STDMR
STDMIB = STDMI

WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID, &
     StandID,Year,Age,Stnd_TPH,Stnd_BA,Stnd_Vol,Inf_TPH,Inf_BA, &
     Inf_Vol,Mort_TPH,Mort_BA,Mort_Vol,Inf_TPH_Pct,Inf_Vol_Pct, &
     Mort_TPH_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI) &
     VALUES (?,?,',CHAR(39),TRIM(NPLT),CHAR(39), &
     ',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

!     CLOSE CURSOR

CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

!     PREPARE QUERY

CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1                 ! 1 ID
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),DM2ID,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 2 CASEID
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 3 YEAR
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 4 AGE
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),NAGE,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 5 STAND TPH
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAT,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 6 STAND BA
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),IBA,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 7 STAND VOL
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOL,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 8 INF TPA
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAI,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 9 INF BA
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTBAI,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 10 INF VOL
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOLI,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 11 MORT TPA
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAM,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 12 MORT BA
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTBAM,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 13 MORT VOL
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOLM,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 14 INF TPH %
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPIT,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 15 INF VOL %
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPIV,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 16 MORT TPH %
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPMT,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 17 MORT VOL %
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPMV,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 18 MEAN DMR
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
     INT(5,SQLSMALLINT_KIND),STDMRB,f90SQL_NULL_PTR,iRet)

ColNumber=ColNumber+1       ! 19 MEAN DMI
CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
     INT(5,SQLSMALLINT_KIND),STDMIB,f90SQL_NULL_PTR,iRet)

!     CLOSE CURSOR

CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
CALL f90SQLExecute(StmtHndlOut,iRet)
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
     'DBSMIS2:Inserting Row')

!     RELEASE STATEMENT HANDLE

100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

  RETURN
END
!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS, &
     DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)

!----------
!  **DBSMIS3--DBS  DATE OF LAST REVISION:  04/20/04
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
!
!     1:     Year - IYEAR
!     2:     Age - NAGE
!     3:     Stand trees/acre - ISTTPAT
!     4:     Stand basal area - IBA
!     5:     Stand volume - ISTVOL
!     6:     Infected trees/acre - ISTTPAI
!     7:     Infected basal area - ISTBAI
!     8:     Infected volume - ISTVOLI
!     9:     Mortality trees/acre - ISTTPAM
!     10:    Mortality basal area - ISTBAM
!     11:    Mortality volume - ISTVOLM
!     12:    Infected trees/acre % - ISTPIT
!     13:    Infected volume % - ISTPIV
!     14:    Mortality trees/acre % - ISTPMT
!     15:    Mortality volume % - ISTPMV
!     16:    Mean DMR - STDMR
!     17:    Mean DMI - STDMI

use f90SQLConstants
use f90SQLStructures
use f90SQL
IMPLICIT NONE

INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR
CHARACTER(LEN=26) NPLT
  CHARACTER(LEN=3)  NLABS(5)
  REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
  INTEGER KODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber
  INTEGER           ID,I,J

DOUBLE PRECISION  X(10)

CHARACTER*2000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME

!     INITIALIZE VARIABLES

IF(IDM3.EQ.0) RETURN
IF(IDM3.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

CALL f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut, &
     iRet)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  IDM3 = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSMIS3:DSN Connection')
  GOTO 100
ENDIF

!     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_DM_Sz_Sum$]'
ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
  TABLENAME = '"FVS_DM_Sz_Sum"'
ELSE
  TABLENAME = 'FVS_DM_Sz_Sum'
ENDIF
SQLStmtStr= 'SELECT * FROM ' // TABLENAME

CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)

IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR. &
     iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID text null,'// &
         'Year int null,'// &
         'Type text null,'// &
         char(39)//'0-5cm'  //char(39)//' double null,'// &
         char(39)//'5-10cm' //char(39)//' double null,'// &
         char(39)//'10-15cm'//char(39)//' double null,'// &
         char(39)//'15-20cm'//char(39)//' double null,'// &
         char(39)//'20-25cm'//char(39)//' double null,'// &
         char(39)//'25-30cm'//char(39)//' double null,'// &
         char(39)//'30-35cm'//char(39)//' double null,'// &
         char(39)//'35-40cm'//char(39)//' double null,'// &
         char(39)//'40-45cm'//char(39)//' double null,'// &
         char(39)//'gt45cm' //char(39)//' double null)'

  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'ID Int,'// &
         'CaseID Int,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Type Text,'// &
         char(39)//'0-5cm'   //char(39)//' Number,'// &
         char(39)//'5-10cm'  //char(39)//' Number,'// &
         char(39)//'10-15cm' //char(39)//' Number,'// &
         char(39)//'15-20cm' //char(39)//' Number,'// &
         char(39)//'20-25cm' //char(39)//' Number,'// &
         char(39)//'25-30cm' //char(39)//' Number,'// &
         char(39)//'30-35cm' //char(39)//' Number,'// &
         char(39)//'35-40cm' //char(39)//' Number,'// &
         char(39)//'40-45cm' //char(39)//' Number,'// &
         char(39)//'gt45cm'  //char(39)//' Number)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'Id int primary key,'// &
         'CaseID int not null,'// &
         'StandID char(26) not null,'// &
         'Year int null,'// &
         'Type char(3) not null,'// &
         char(39)//'0-5cm'   //char(39)//' real null,'// &
         char(39)//'5-10cm'  //char(39)//' real null,'// &
         char(39)//'10-15cm' //char(39)//' real null,'// &
         char(39)//'15-20cm' //char(39)//' real null,'// &
         char(39)//'20-25cm' //char(39)//' real null,'// &
         char(39)//'25-30cm' //char(39)//' real null,'// &
         char(39)//'30-35cm' //char(39)//' real null,'// &
         char(39)//'35-40cm' //char(39)//' real null,'// &
         char(39)//'40-45cm' //char(39)//' real null,'// &
         char(39)//'gt45cm'  //char(39)//' real null)'
  ENDIF

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE, iRet)
  CALL f90SQLExecDirect(StmtHndlOut,trim(SQLStmtStr),iRet)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS3:Creating Table: '//trim(SQLStmtStr))
  DM3ID = 0
ENDIF

!     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

IF(DM3ID .EQ. -1) THEN
  CALL DBSGETID(TABLENAME,'Id',ID)
  DM3ID = ID
ENDIF

!     LOOP OVER 5 TYPES

DO I = 1,5

!       MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

  DM3ID = DM3ID + 1
  IF(DM3ID .GE. 65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

!     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

  SELECT CASE (I)
      CASE (1)
        DO J = 1,10
          X(J) = DCTPA(J)
        ENDDO
      CASE (2)
        DO J = 1,10
          X(J) = DCINF(J)
        ENDDO
      CASE (3)
        DO J = 1,10
          X(J) = DCMRT(J)
        ENDDO
      CASE (4)
        DO J = 1,10
          X(J) = DCDMR(J)
        ENDDO
      CASE (5)
        DO J = 1,10
          X(J) = DCDMI(J)
        ENDDO
  END SELECT

  WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (Id,CaseID, &
       StandID,Year,Type,'// &
       char(39),'0-5cm',  char(39),','// &
       char(39),'5-10cm', char(39),','// &
       char(39),'10-15cm',char(39),','// &
       char(39),'15-20cm',char(39),','// &
       char(39),'20-25cm',char(39),','// &
       char(39),'25-30cm',char(39),','// &
       char(39),'30-35cm',char(39),','// &
       char(39),'35-40cm',char(39),','// &
       char(39),'40-45cm',char(39),','// &
       char(39),'gt45cm', char(39),') VALUES ', &
       '(?,?,',char(39),TRIM(NPLT),char(39), &
       ',?,',char(39),TRIM(NLABS(I)),char(39), &
       ',?,?,?,?,?,?,?,?,?,?)'

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)

!       PREPARE QUERY

  CALL f90SQLPrepare(StmtHndlOut, SQLStmtStr, iRet)

!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 ID
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),DM3ID,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 2 CASEID
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ICASE,f90SQL_NULL_PTR,iRet)

  ColNumber=ColNumber+1       ! 3 YEAR
  CALL f90SQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),IYEAR,f90SQL_NULL_PTR,iRet)

  DO J = 1,10
    ColNumber=ColNumber+1     ! SIZE CLASSES 1-10
    CALL f90SQLBindParameter(StmtHndlOut,ColNumber, &
         SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE, &
         INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND), &
         X(J),f90SQL_NULL_PTR,iRet)
  ENDDO

!       CLOSE CURSOR

  CALL f90SQLFreeStmt(StmtHndlOut,SQL_CLOSE,iRet)
  CALL f90SQLExecute(StmtHndlOut,iRet)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS3:Inserting Row')

  ENDDO

!     RELEASE STATEMENT HANDLE

100 CALL f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut,iRet)

  RETURN
END

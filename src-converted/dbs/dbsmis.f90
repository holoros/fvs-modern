SUBROUTINE DBSMIS1(IYEAR,NPLT,CSP, &
     SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4, &
     KODE)
IMPLICIT NONE
!
! DBS $Id$
!
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


INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR
REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
INTEGER KODE,IRCODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber
INTEGER           I

DOUBLE PRECISION  SPDMR4B, SPDMI4B
INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

CHARACTER*2000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME
CHARACTER(LEN=26) NPLT
CHARACTER(LEN=2)  CSP(4)

!     INITIALIZE VARIABLES

IF(IDM1.EQ.0) RETURN
IF(IDM1.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)

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

CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')

IF(IRCODE.EQ.2) THEN
  IDM1 = 0
  RETURN
ENDIF
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'CaseID text not null,'// &
         'StandID text null,'// &
         'Year Int null,'// &
         'Spp Text null,'// &
         'Mean_DMR double null,'// &
         'Mean_DMI double null,'// &
         'Inf_TPA int null,'// &
         'Mort_TPA int null,'// &
         'Inf_TPA_Pct int null,'// &
         'Mort_TPA_Pct int null,'// &
         'Stnd_TPA_Pct int null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'CaseID Text,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Spp Text,'// &
         'Mean_DMR Number,'// &
         'Mean_DMI Number,'// &
         'Inf_TPA Int,'// &
         'Mort_TPA Int,'// &
         'Inf_TPA_Pct Int,'// &
         'Mort_TPA_Pct Int,'// &
         'Stnd_TPA_Pct Int)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('// &
         'CaseID char(36) not null,'// &
         'StandID char(26) not null,'// &
         'Year Int null,'// &
         'Spp char(2) null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null,'// &
         'Inf_TPA int null,'// &
         'Mort_TPA int  null,'// &
         'Inf_TPA_Pct int null,'// &
         'Mort_TPA_Pct int null,'// &
         'Stnd_TPA_Pct int null)'
  ENDIF

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)

  iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
             int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS1:Creating Table: '//trim(SQLStmtStr))
ENDIF

!     LOOP OVER 4 TOP SPECIES

DO I = 1,4

!       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
!       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE.


  IF (CSP(I) .EQ. '**') GOTO 100

!       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
!       INTEGER COPIES OF REAL VECTOR INPUTS

  SPDMR4B = SPDMR4(I)
  SPDMI4B = SPDMI4(I)

  ISPINF4 = NINT(SPINF4(I))
  ISPMRT4 = NINT(SPMRT4(I))
  ISPPIN4 = NINT(SPPIN4(I))
  ISPPMR4 = NINT(SPPMR4(I))
  ISPPOC4 = NINT(SPPOC4(I))

  WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME), &
       ' (CaseID,StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPA,', &
       'Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) ', &
       ' VALUES (''',CASEID,''',''',TRIM(NPLT), &
       ''',?,''',TRIM(CSP(I)),''',?,?,?,?,?,?,?)'

  iRet=fvsSQLCloseCursor(StmtHndlOut)

  iRet=fvsSQLPrepare(StmtHndlOut, SQLStmtStr, &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 YEAR
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1     ! MEAN DMR
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
       INT(5,SQLSMALLINT_KIND),SPDMR4B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! MEAN DMI
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
       INT(5,SQLSMALLINT_KIND),SPDMI4B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! TPA INFECTED
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPINF4,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! TPA MORTALITY
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPMRT4,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! % TPA INFECTED
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPIN4,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! % TPA MORTALITY
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPMR4,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  ColNumber=ColNumber+1       ! % STAND TPA
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),ISPPOC4,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)

  iRet=fvsSQLExecute(StmtHndlOut)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS1:Inserting Row')

ENDDO

!     RELEASE STATEMENT HANDLE

100 CONTINUE
iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE, &
     ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM, &
     ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
IMPLICIT NONE
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

INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR,NAGE
CHARACTER(LEN=26) NPLT
INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
REAL    STDMR,STDMI
INTEGER KODE,IRCODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber

DOUBLE PRECISION  STDMRB, STDMIB

CHARACTER*1000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME

!     INITIALIZE VARIABLES

IF(IDM2.EQ.0) RETURN
IF(IDM2.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
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
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) THEN
  IDM2 = 0
  RETURN
ENDIF
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'CaseID text not null,'// &
         'StandID text null,'// &
         'Year int null,'// &
         'Age int null,'// &
         'Stnd_TPA int null,'// &
         'Stnd_BA int null,'// &
         'Stnd_Vol int null,'// &
         'Inf_TPA int null,'// &
         'Inf_BA int null,'// &
         'Inf_Vol int null,'// &
         'Mort_TPA int null,'// &
         'Mort_BA int null,'// &
         'Mort_Vol int null,'// &
         'Inf_TPA_Pct int null,'// &
         'Inf_Vol_Pct int null,'// &
         'Mort_TPA_Pct int null,'// &
         'Mort_Vol_Pct int null,'// &
         'Mean_DMR double null,'// &
         'Mean_DMI double null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'CaseID Text,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Age Int,'// &
         'Stnd_TPA Int,'// &
         'Stnd_BA Int,'// &
         'Stnd_Vol Int,'// &
         'Inf_TPA Int,'// &
         'Inf_BA Int,'// &
         'Inf_Vol Int,'// &
         'Mort_TPA Int,'// &
         'Mort_BA Int,'// &
         'Mort_Vol Int,'// &
         'Inf_TPA_Pct Int,'// &
         'Inf_Vol_Pct Int,'// &
         'Mort_TPA_Pct Int,'// &
         'Mort_Vol_Pct Int,'// &
         'Mean_DMR Number,'// &
         'Mean_DMI Number)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('// &
         'CaseID char(36) not null,'// &
         'StandID char(26) not null,'// &
         'Year int null,'// &
         'Age int null,'// &
         'Stnd_TPA int null,'// &
         'Stnd_BA int null,'// &
         'Stnd_Vol int null,'// &
         'Inf_TPA int null,'// &
         'Inf_BA int null,'// &
         'Inf_Vol int null,'// &
         'Mort_TPA int null,'// &
         'Mort_BA int null,'// &
         'Mort_Vol int null,'// &
         'Inf_TPA_Pct int null,'// &
         'Inf_Vol_Pct int null,'// &
         'Mort_TPA_Pct int null,'// &
         'Mort_Vol_Pct int null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null)'
  ENDIF

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)
  iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
             int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS2:Creating Table: '//trim(SQLStmtStr))
ENDIF

!     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

STDMRB = STDMR
STDMIB = STDMI

WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (CaseID,', &
     'StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,', &
     'Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,', &
     'Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)', &
     'VALUES (''',CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

iRet=fvsSQLCloseCursor(StmtHndlOut)
iRet=fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1                 ! YEAR
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! AGE
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),NAGE,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! STAND TPA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! STAND BA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),IBA,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! STAND VOL
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOL,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! INF TPA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAI,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! INF BA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTBAI,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1      ! INF VOL
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOLI,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MORT TPA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTTPAM,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MORT BA
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTBAM,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MORT VOL
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTVOLM,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! INF TPA %
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPIT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! INF VOL %
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPIV,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! 16 MORT TPA %
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPMT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MORT VOL %
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
     INT(0,SQLSMALLINT_KIND),ISTPMV,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MEAN DMR
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
     INT(5,SQLSMALLINT_KIND),STDMRB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1       ! MEAN DMI
iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
     SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
     INT(5,SQLSMALLINT_KIND),STDMIB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

!     CLOSE CURSOR

iRet=fvsSQLCloseCursor(StmtHndlOut)
iRet=fvsSQLExecute(StmtHndlOut)
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
     'DBSMIS2:Inserting Row')

!     RELEASE STATEMENT HANDLE

100 iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

RETURN
END
!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS, &
     DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)
IMPLICIT NONE

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


INCLUDE 'DBSCOM.f90'

!     ARGUMENT LIST

INTEGER IYEAR
CHARACTER(LEN=26) NPLT
CHARACTER(LEN=3)  NLABS(5)
REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
INTEGER KODE,IRCODE

!     LOCAL VARIABLES

INTEGER(SQLSMALLINT_KIND)::ColNumber
INTEGER           I,J

DOUBLE PRECISION  X(10)

CHARACTER*1000    SQLStmtStr
CHARACTER(LEN=20) TABLENAME

!     INITIALIZE VARIABLES

IF(IDM3.EQ.0) RETURN
IF(IDM3.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     ALLOCATE A STATEMENT HANDLE

iRet=fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
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
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) THEN
  IDM3 = 0
  RETURN
ENDIF
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'CaseID text not null,'// &
         'StandID text null,'// &
         'Year int null,'// &
         'Type text null,'// &
         char(39)//'0-3in'  //char(39)//' double null,'// &
         char(39)//'3-5in' //char(39)//' double null,'// &
         char(39)//'5-7in'//char(39)//' double null,'// &
         char(39)//'7-9in'//char(39)//' double null,'// &
         char(39)//'9-11in'//char(39)//' double null,'// &
         char(39)//'11-13in'//char(39)//' double null,'// &
         char(39)//'13-15in'//char(39)//' double null,'// &
         char(39)//'15-17in'//char(39)//' double null,'// &
         char(39)//'17-19in'//char(39)//' double null,'// &
         char(39)//'gt19in' //char(39)//' double null)'

  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'CaseID Text,'// &
         'StandID Text,'// &
         'Year Int,'// &
         'Type Text,'// &
         char(39)//'0-3in'   //char(39)//' Number,'// &
         char(39)//'3-5in'  //char(39)//' Number,'// &
         char(39)//'5-7in' //char(39)//' Number,'// &
         char(39)//'7-9in' //char(39)//' Number,'// &
         char(39)//'9-11in' //char(39)//' Number,'// &
         char(39)//'11-13in' //char(39)//' Number,'// &
         char(39)//'13-15in' //char(39)//' Number,'// &
         char(39)//'15-17in' //char(39)//' Number,'// &
         char(39)//'17-19in' //char(39)//' Number,'// &
         char(39)//'gt19in'  //char(39)//' Number)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('// &
         'CaseID char(36) not null,'// &
         'StandID char(26) not null,'// &
         'Year int null,'// &
         'Type char(3) not null,'// &
         char(39)//'0-3in'   //char(39)//' real null,'// &
         char(39)//'3-5in'  //char(39)//' real null,'// &
         char(39)//'5-7in' //char(39)//' real null,'// &
         char(39)//'7-9in' //char(39)//' real null,'// &
         char(39)//'9-11in' //char(39)//' real null,'// &
         char(39)//'11-13in' //char(39)//' real null,'// &
         char(39)//'13-15in' //char(39)//' real null,'// &
         char(39)//'15-17in' //char(39)//' real null,'// &
         char(39)//'17-19in' //char(39)//' real null,'// &
         char(39)//'gt19in'  //char(39)//' real null)'
  ENDIF

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)
  iRet=fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
             int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS3:Creating Table: '//trim(SQLStmtStr))
ENDIF

!     LOOP OVER 5 TYPES

DO I = 1,5

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

  WRITE(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME), &
       ' (CaseID,StandID,Year,Type,', &
       char(39),'0-3in',  char(39),',', &
       char(39),'3-5in', char(39),',', &
       char(39),'5-7in',char(39),',', &
       char(39),'7-9in',char(39),',', &
       char(39),'9-11in',char(39),',', &
       char(39),'11-13in',char(39),',', &
       char(39),'13-15in',char(39),',', &
       char(39),'15-17in',char(39),',', &
       char(39),'17-19in',char(39),',', &
       char(39),'gt19in', char(39),') VALUES ', &
       '(''',CASEID,''',''',TRIM(NPLT), &
       ''',?,''',TRIM(NLABS(I)), &
       ''',?,?,?,?,?,?,?,?,?,?)'

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)

!       PREPARE QUERY

  iRet=fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 YEAR
  iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
       SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
       INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

  DO J = 1,10
    ColNumber=ColNumber+1     ! SIZE CLASSES 2-11
    iRet=fvsSQLBindParameter(StmtHndlOut,ColNumber, &
         SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE, &
         INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND), &
         X(J),int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)
  ENDDO

!       CLOSE CURSOR

  iRet=fvsSQLCloseCursor(StmtHndlOut)
  iRet=fvsSQLExecute(StmtHndlOut)
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
       'DBSMIS3:Inserting Row')

ENDDO

!     RELEASE STATEMENT HANDLE

100 iRet=fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

RETURN
END

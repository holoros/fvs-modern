SUBROUTINE DBSSTRCLASS(IYEAR,NPLT,RCODE,S1DBH,S1NHT,S1LHT,S1SHT, &
     S1CB,S1CC,S1MS1,S1MS2,S1SC,S2DBH,S2NHT,S2LHT,S2SHT,S2CB,S2CC, &
     S2MS1,S2MS2,S2SC,S3DBH,S3NHT,S3LHT,S3SHT,S3CB,S3CC,S3MS1,S3MS2, &
     S3SC,NS,TOTCOV,SCLASS,KODE,NTREES)
IMPLICIT NONE
!
! DBS $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE STRUCTURE CLASS OUTPUT.
!     AUTH: S. REBAIN -- FMSC -- AUGUST 2005
!     INPUT:
!            THE STRUCTURE CLASS OUTPUT.
!              1: REMOVAL CODE
!              2: STRATUM 1 DBH
!              3: STRATUM 1 NOMINAL HEIGHT
!              4: STRATUM 1 LARGEST HEIGHT
!              5: STRATUM 1 SMALLEST HEIGHT
!              6: STRATUM 1 CROWN BASE
!              7: STRATUM 1 CROWN COVER
!              8: STRATUM 1 MAJOR SPECIES 1
!              9: STRATUM 1 MAJOR SPECIES 2
!             10: STRATUM 1 STATUS CODE
!             11: STRATUM 2 DBH
!             12: STRATUM 2 NOMINAL HEIGHT
!             13: STRATUM 2 LARGEST HEIGHT
!             14: STRATUM 2 SMALLEST HEIGHT
!             15: STRATUM 2 CROWN BASE
!             16: STRATUM 2 CROWN COVER
!             17: STRATUM 2 MAJOR SPECIES 1
!             18: STRATUM 2 MAJOR SPECIES 2
!             19: STRATUM 2 STATUS CODE
!             20: STRATUM 3 DBH
!             21: STRATUM 3 NOMINAL HEIGHT
!             22: STRATUM 3 LARGEST HEIGHT
!             23: STRATUM 3 SMALLEST HEIGHT
!             24: STRATUM 3 CROWN BASE
!             25: STRATUM 3 CROWN COVER
!             26: STRATUM 3 MAJOR SPECIES 1
!             27: STRATUM 3 MAJOR SPECIES 2
!             28: STRATUM 3 STATUS CODE
!             29: NUMBER OF STRATA
!             30: TOTAL COVER
!             31: STRUCTURE CLASS
!             32: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE
!             33: THE NUMBER OF TREE RECORDS
!
!OMMONS
!
INCLUDE 'DBSCOM.f90'
!
!OMMONS
!

INTEGER IYEAR,KODE,RCODE,S1NHT,S1LHT,S1SHT,S1CB,S1CC,S1SC,IRCODE
INTEGER S2NHT,S2LHT,S2SHT,S2CB,S2CC,S3NHT,S3LHT,S3SHT,S3CB,S3CC
INTEGER S2SC, S3SC, TOTCOV,NS, NTREES
INTEGER(SQLSMALLINT_KIND)::ColNumber
REAL S1DBH,S2DBH,S3DBH
CHARACTER*2000 SQLStmtStr
CHARACTER*3 S1MS1,S1MS2,S2MS1,S2MS2,S3MS1,S3MS2
CHARACTER*4 SCLASS
CHARACTER(len=20) TABLENAME
CHARACTER(len=26) NPLT
!
!
!OMMONS END

!---
!     Initialize variables
!
IF (ISTRCLAS .EQ. 0) RETURN
IF (ISTRCLAS .EQ. 2) KODE = 0
IF (NTREES .EQ. 0) RETURN
!---------
!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
!---------
CALL DBSCASE(1)

!---------
!     ALLOCATE A STATEMENT HANDLE
!---------
iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  ISTRCLAS = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSSTRCLASS:DSN Connection')
  GOTO 200
ENDIF
!---------
!     CHECK TO SEE IF THE STRUCTURE CLASS TABLE EXISTS IN DATBASE
!---------
IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_StrClass$]'
ELSE
  TABLENAME = 'FVS_StrClass'
ENDIF
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) THEN
  ISTRCLAS = 0
  RETURN
ENDIF
IF(IRCODE.EQ.1) THEN

  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_StrClass('// &
                 'CaseID Text not null,'// &
                 'StandID Text null,'// &
                 'Year int null,'// &
                 'Removal_Code double null,'// &
                 'Stratum_1_DBH double null,'// &
                 'Stratum_1_Nom_Ht double null,'// &
                 'Stratum_1_Lg_Ht double null,'// &
                 'Stratum_1_Sm_Ht double null,'// &
                 'Stratum_1_Crown_Base double null,'// &
                 'Stratum_1_Crown_Cover double null,'// &
                 'Stratum_1_Species_1 text null,'// &
                 'Stratum_1_Species_2 text null,'// &
                 'Stratum_1_Status_Code double null,'// &
                 'Stratum_2_DBH double null,'// &
                 'Stratum_2_Nom_Ht double null,'// &
                 'Stratum_2_Lg_Ht double null,'// &
                 'Stratum_2_Sm_Ht double null,'// &
                 'Stratum_2_Crown_Base double null,'// &
                 'Stratum_2_Crown_Cover double null,'// &
                 'Stratum_2_Species_1 text null,'// &
                 'Stratum_2_Species_2 text null,'// &
                 'Stratum_2_Status_Code double null,'// &
                 'Stratum_3_DBH double null,'// &
                 'Stratum_3_Nom_Ht double null,'// &
                 'Stratum_3_Lg_Ht double null,'// &
                 'Stratum_3_Sm_Ht double null,'// &
                 'Stratum_3_Crown_Base double null,'// &
                 'Stratum_3_Crown_Cover double null,'// &
                 'Stratum_3_Species_1 text null,'// &
                 'Stratum_3_Species_2 text null,'// &
                 'Stratum_3_Status_Code double null,'// &
                 'Number_of_Strata double null,'// &
                 'Total_Cover double null,'// &
                 'Structure_Class text null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_StrClass('// &
                 'CaseID Text,'// &
                 'StandID Text,'// &
                 'Year int,'// &
                 'Removal_Code Number,'// &
                 'Stratum_1_DBH Number,'// &
                 'Stratum_1_Nom_Ht Number,'// &
                 'Stratum_1_Lg_Ht Number,'// &
                 'Stratum_1_Sm_Ht Number,'// &
                 'Stratum_1_Crown_Base Number,'// &
                 'Stratum_1_Crown_Cover Number,'// &
                 'Stratum_1_Species_1 Text,'// &
                 'Stratum_1_Species_2 Text,'// &
                 'Stratum_1_Status_Code Number,'// &
                 'Stratum_2_DBH Number,'// &
                 'Stratum_2_Nom_Ht Number,'// &
                 'Stratum_2_Lg_Ht Number,'// &
                 'Stratum_2_Sm_Ht Number,'// &
                 'Stratum_2_Crown_Base Number,'// &
                 'Stratum_2_Crown_Cover Number,'// &
                 'Stratum_2_Species_1 Text,'// &
                 'Stratum_2_Species_2 Text,'// &
                 'Stratum_2_Status_Code Number,'// &
                 'Stratum_3_DBH Number,'// &
                 'Stratum_3_Nom_Ht Number,'// &
                 'Stratum_3_Lg_Ht Number,'// &
                 'Stratum_3_Sm_Ht Number,'// &
                 'Stratum_3_Crown_Base Number,'// &
                 'Stratum_3_Crown_Cover Number,'// &
                 'Stratum_3_Species_1 Text,'// &
                 'Stratum_3_Species_2 Text,'// &
                 'Stratum_3_Status_Code Number,'// &
                 'Number_of_Strata Number,'// &
                 'Total_Cover Number,'// &
                 'Structure_Class Text)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_StrClass('// &
                 'CaseID char(36) not null,'// &
                 'StandID char(26) null,'// &
                 'Year int null,'// &
                 'Removal_Code int null,'// &
                 'Stratum_1_DBH real null,'// &
                 'Stratum_1_Nom_Ht int null,'// &
                 'Stratum_1_Lg_Ht int null,'// &
                 'Stratum_1_Sm_Ht int null,'// &
                 'Stratum_1_Crown_Base int null,'// &
                 'Stratum_1_Crown_Cover int null,'// &
                 'Stratum_1_Species_1 char(3) null,'// &
                 'Stratum_1_Species_2 char(3) null,'// &
                 'Stratum_1_Status_Code int null,'// &
                 'Stratum_2_DBH real null,'// &
                 'Stratum_2_Nom_Ht int null,'// &
                 'Stratum_2_Lg_Ht int null,'// &
                 'Stratum_2_Sm_Ht int null,'// &
                 'Stratum_2_Crown_Base int null,'// &
                 'Stratum_2_Crown_Cover int null,'// &
                 'Stratum_2_Species_1 char(3) null,'// &
                 'Stratum_2_Species_2 char(3) null,'// &
                 'Stratum_2_Status_Code int null,'// &
                 'Stratum_3_DBH real null,'// &
                 'Stratum_3_Nom_Ht int null,'// &
                 'Stratum_3_Lg_Ht int null,'// &
                 'Stratum_3_Sm_Ht int null,'// &
                 'Stratum_3_Crown_Base int null,'// &
                 'Stratum_3_Crown_Cover int null,'// &
                 'Stratum_3_Species_1 char(3) null,'// &
                 'Stratum_3_Species_2 char(3) null,'// &
                 'Stratum_3_Status_Code int null,'// &
                 'Number_of_Strata int null,'// &
                 'Total_Cover int null,'// &
                 'Structure_Class char(4) null)'
  ENDIF

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
          'DBSSTRCLASS:Creating Table: '//trim(SQLStmtStr))
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(CaseID,', &
       'StandID,Year,Removal_Code,Stratum_1_DBH,Stratum_1_Nom_Ht,', &
       'Stratum_1_Lg_Ht,Stratum_1_Sm_Ht,Stratum_1_Crown_Base,', &
       'Stratum_1_Crown_Cover,Stratum_1_Species_1,', &
       'Stratum_1_Species_2,Stratum_1_Status_Code,Stratum_2_DBH,', &
       'Stratum_2_Nom_Ht,Stratum_2_Lg_Ht,Stratum_2_Sm_Ht,', &
       'Stratum_2_Crown_Base,Stratum_2_Crown_Cover,', &
       'Stratum_2_Species_1,Stratum_2_Species_2,', &
       'Stratum_2_Status_Code,Stratum_3_DBH,Stratum_3_Nom_Ht,', &
       'Stratum_3_Lg_Ht,Stratum_3_Sm_Ht,Stratum_3_Crown_Base,', &
       'Stratum_3_Crown_Cover,Stratum_3_Species_1,', &
       'Stratum_3_Species_2,Stratum_3_Status_Code,Number_of_Strata,', &
       'Total_Cover,Structure_Class) VALUES(''',CASEID, &
       ''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,''',TRIM(S1MS1), &
       ''',''',TRIM(S1MS2),''',?,?,?,?,?,?,?,''',TRIM(S2MS1), &
       ''',''',TRIM(S2MS2),''',?,?,?,?,?,?,?,''',TRIM(S3MS1), &
       ''',''',TRIM(S3MS2),''',?,?,?,''',TRIM(SCLASS),''')'
!
!     CLOSE CURSOR
!
iRet = fvsSQLCloseCursor(StmtHndlOut)
!
!     PREPARE THE SQL QUERY
!
iRet = fvsSQLPrepare(StmtHndlOut, trim (SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
!
!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
!
ColNumber=1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),RCODE,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_FLOAT, SQL_REAL,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1DBH,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1NHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1LHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1SHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1CB,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1CC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S1SC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_FLOAT, SQL_REAL,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2DBH,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2NHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2LHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2SHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2CB,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2CC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S2SC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_FLOAT, SQL_REAL,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3DBH,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3NHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3LHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3SHT,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3CB,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3CC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),S3SC,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),NS,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
             SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
             INT(5,SQLSMALLINT_KIND),TOTCOV,int(4,SQLLEN_KIND), &
             SQL_NULL_PTR)

iRet = fvsSQLCloseCursor(StmtHndlOut)
iRet = fvsSQLExecute(StmtHndlOut)
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                 'DBSSTRCLASS:Inserting Row')

200 CONTINUE
!Release statement handle
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

END



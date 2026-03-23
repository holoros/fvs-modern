SUBROUTINE DBSFMSSNAG(IYEAR,NPLT,HCL1,HCL2,HCL3,HCL4,HCL5,HCL6, &
     HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF,KODE)
IMPLICIT NONE
!
! DBS $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE SUMMARY SNAG REPORT
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
!     INPUT:
!              THE SUMMARY SNAG OUTPUT FROM THE FIRE MODEL.
!              1: HARD SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
!              2: HARD SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
!              3: HARD SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
!              4: HARD SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
!              5: HARD SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
!              6: HARD SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
!              7: HARD SNAGS - TOTAL       (> 0)
!              8: SOFT SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
!              9: SOFT SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
!             10: SOFT SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
!             11: SOFT SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
!             12: SOFT SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
!             13: SOFT SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
!             14: SOFT SNAGS - TOTAL       (> 0)
!             15: HARD+SOFT  - TOTAL       (> 0)
!             16: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
!
!     NOTE: THE DBH CLASS BREAKS CAN BE CHANGED BY THE SNAGCLAS KEYWORD
!
!OMMONS
!
INCLUDE 'DBSCOM.f90'
!
!OMMONS

INTEGER IYEAR,KODE,IRCODE
INTEGER(SQLSMALLINT_KIND)::ColNumber
REAL HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,HCL7,SCL1,SCL2,SCL3,SCL4,SCL5, &
     SCL6,SCL7,HDSF
DOUBLE PRECISION HCL1B,HCL2B,HCL3B,HCL4B,HCL5B,HCL6B,HCL7B, &
     SCL1B,SCL2B,SCL3B,SCL4B,SCL5B,SCL6B,SCL7B,HDSFB
CHARACTER*2000 SQLStmtStr
CHARACTER(len=20) TABLENAME
CHARACTER(len=26) NPLT

!     Initialize variables

IF(ISSUM.EQ.0) RETURN
IF(ISSUM.EQ.2) KODE = 0

!---------
!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
!---------
CALL DBSCASE(1)

!---------
!     ALLOCATE A STATEMENT HANDLE
!---------
iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  ISSUM = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSFMSSNAG:DSN Connection')
  GOTO 200
ENDIF
!---------
!     CHECK TO SEE IF THE SUMMARY SNAG TABLE EXISTS IN DATBASE
!---------
IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_SnagSum$]'
ELSE
  TABLENAME = 'FVS_SnagSum'
ENDIF
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) THEN
  ISSUM = 0
  RETURN
ENDIF
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_SnagSum('// &
                 'CaseID Text not null,'// &
                 'StandID Text null,'// &
                 'Year Int null,'// &
                 'Hard_snags_class1 double null,'// &
                 'Hard_snags_class2 double null,'// &
                 'Hard_snags_class3 double null,'// &
                 'Hard_snags_class4 double null,'// &
                 'Hard_snags_class5 double null,'// &
                 'Hard_snags_class6 double null,'// &
                 'Hard_snags_total  double null,'// &
                 'Soft_snags_class1 double null,'// &
                 'Soft_snags_class2 double null,'// &
                 'Soft_snags_class3 double null,'// &
                 'Soft_snags_class4 double null,'// &
                 'Soft_snags_class5 double null,'// &
                 'Soft_snags_class6 double null,'// &
                 'Soft_snags_total  double null,'// &
                 'Hard_soft_snags_total double null)'

  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_SnagSum('// &
                 'CaseID Text,'// &
                 'StandID Text,'// &
                 'Year Int,'// &
                 'Hard_snags_class1 Number,'// &
                 'Hard_snags_class2 Number,'// &
                 'Hard_snags_class3 Number,'// &
                 'Hard_snags_class4 Number,'// &
                 'Hard_snags_class5 Number,'// &
                 'Hard_snags_class6 Number,'// &
                 'Hard_snags_total  Number,'// &
                 'Soft_snags_class1 Number,'// &
                 'Soft_snags_class2 Number,'// &
                 'Soft_snags_class3 Number,'// &
                 'Soft_snags_class4 Number,'// &
                 'Soft_snags_class5 Number,'// &
                 'Soft_snags_class6 Number,'// &
                 'Soft_snags_total  Number,'// &
                 'Hard_soft_snags_total Number)'

  ELSE
    SQLStmtStr='CREATE TABLE FVS_SnagSum('// &
                 'CaseID char(36) not null,'// &
                 'StandID char(26) not null,'// &
                 'Year Int null,'// &
                 'Hard_snags_class1 real null,'// &
                 'Hard_snags_class2 real null,'// &
                 'Hard_snags_class3 real null,'// &
                 'Hard_snags_class4 real null,'// &
                 'Hard_snags_class5 real null,'// &
                 'Hard_snags_class6 real null,'// &
                 'Hard_snags_total  real null,'// &
                 'Soft_snags_class1 real null,'// &
                 'Soft_snags_class2 real null,'// &
                 'Soft_snags_class3 real null,'// &
                 'Soft_snags_class4 real null,'// &
                 'Soft_snags_class5 real null,'// &
                 'Soft_snags_class6 real null,'// &
                 'Soft_snags_total  real null,'// &
                 'Hard_soft_snags_total real null)'
  ENDIF

      iRet = fvsSQLCloseCursor(StmtHndlOut)
      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
               int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
              'DBSFMSSNAG:Creating Table: '//trim(SQLStmtStr))
ENDIF

!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
HCL1B = HCL1
HCL2B = HCL2
HCL3B = HCL3
HCL4B = HCL4
HCL5B = HCL5
HCL6B = HCL6
HCL7B = HCL7
SCL1B = SCL1
SCL2B = SCL2
SCL3B = SCL3
SCL4B = SCL4
SCL5B = SCL5
SCL6B = SCL6
SCL7B = SCL7
HDSFB = HDSF

WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,', &
     'StandID,Year,Hard_snags_class1,Hard_snags_class2,', &
     'Hard_snags_class3,Hard_snags_class4,Hard_snags_class5,', &
     'Hard_snags_class6,Hard_snags_total,Soft_snags_class1,', &
     'Soft_snags_class2,Soft_snags_class3,Soft_snags_class4,', &
     'Soft_snags_class5,Soft_snags_class6,Soft_snags_total,', &
     'Hard_soft_snags_total) VALUES(''',CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

!
!     CLOSE CURSOR
!
iRet = fvsSQLCloseCursor(StmtHndlOut)
!
!     PREPARE THE SQL QUERY
!
iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr), &
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
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),HCL1B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),HCL2B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),HCL3B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),HCL4B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),HCL5B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),HCL6B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),HCL7B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL1B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL2B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL3B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL4B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL5B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL6B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SCL7B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),HDSFB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

!Close Cursor
iRet = fvsSQLCloseCursor(StmtHndlOut)

iRet = fvsSQLExecute(StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS) ISSUM=0

CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                 'DBSFMSSNAG:Inserting Row')

200 CONTINUE
!Release statement handle
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

END



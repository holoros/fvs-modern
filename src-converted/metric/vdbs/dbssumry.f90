SUBROUTINE DBSSUMRY(IYEAR,IAGE,NPLT,ITPA,IBA,ISDI,ICCF, &
     TOPHT,FQMD,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT, &
     IATBA,IATSDI,IATCCF,ATTOPHT,FATQMD,IPRDLEN,IACC,IMORT,YMAI, &
     IFORTP,ISZCL,ISTCL)
IMPLICIT NONE
!----------
! METRIC-VDBS $Id$
!----------
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              OUTPUT.
!     AUTH: D. GAMMEL -- SEM -- JUNE 2002
!     INPUT:
!     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
!              1: YEAR
!              2: AGE
!              3: TREES/HA
!              4: TOTAL CU M
!     *        4: MERCH CU M (PULP AND SAWLOG)
!              5: MERCH CU M
!     *        5: MERCH CU M (SAWLOG)
!              6: MERCH BD FT
!     *        6: MERCH BD FT (SAWLOG)
!              7: REMOVED TREES/HA
!              8: REMOVED TOTAL CU M
!     *        8: REMOVED MERCH CU M (PULP AND SAWLOG)
!              9: REMOVED MERCH CU M
!     *        9: REMOVED MERCH CU M (SAWLOG)
!             10: REMOVED MERCH BD FT
!     *       10: REMOVED MERCH BD FT (SAWLOG)
!             11: BASAL AREA/HA
!             12: CCF
!             13: AVERAGE DOMINATE HEIGHT
!             14: PERIOD LENGTH (YEARS)
!             15: ACCRETION (ANNUAL IN CU M/HA)
!             16: MORTALITY  (ANNUAL IN CU M/HA)
!             17: SAMPLE WEIGHT
!
!     ICASE - CASE NUMBER FROM THE FVSRUN TABLE
!OMMONS

!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
!
!OMMONS
!
INTEGER IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF, &
           ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT, &
           IATBA,IATSDI,IATCCF,ID,IFORTP,ISZCL,ISTCL
INTEGER(SQLSMALLINT_KIND)::ColNumber
REAL FQMD,FATQMD,YMAI,TOPHT,ATTOPHT
DOUBLE PRECISION FQMDB,FATQMDB,YMAIB,TOPHTB,ATTOPHTB

CHARACTER*2000 SQLStmtStr
CHARACTER(len=20) TABLENAME
CHARACTER(len=*) NPLT
!
!OMMONS END
!---
!     Initialize variables
!
IF(ISUMARY.EQ.0) RETURN
!
!---------
!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
!---------
CALL DBSCASE(1)

!---------
!     ALLOCATE A STATEMENT HANDLE
!---------
iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  ISUMARY = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSSUMRY:DSN Connection')
  GOTO 200
ENDIF
!---------
!     CHECK TO SEE IF THE SUMMARY STATS TABLE EXISTS IN DATBASE
!---------
IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR. &
         (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR. &
         (VARACD .EQ. 'ON')) THEN

      TABLENAME = '[FVS_Summary_East$]'
  ELSE
      TABLENAME = '[FVS_Summary$]'
  ENDIF
ELSE
  IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR. &
         (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR. &
         (VARACD .EQ. 'ON')) THEN

      TABLENAME = 'FVS_Summary_East'
  ELSE
      TABLENAME = 'FVS_Summary'
  ENDIF
ENDIF
SQLStmtStr= 'SELECT * FROM ' // TABLENAME

iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
!
IF(.NOT.(iRet.EQ.SQL_SUCCESS .OR. &
       iRet.EQ.SQL_SUCCESS_WITH_INFO)) THEN
!
!  EASTERN VARIANT VOLUME NOMENCLATURE
!
  IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR. &
         (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR. &
         (VARACD .EQ. 'ON')) THEN
!
    IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
      SQLStmtStr='CREATE TABLE FVS_Summary_East('// &
                    'Id int primary key,'// &
                    'CaseID int not null,'// &
                    'StandID text not null,'// &
                    'Year int null,'// &
                    'Age int null,'// &
                    'TPH double null,'// &
                    'BA double null,'// &
                    'SDI double null,'// &
                    'CCF double null,'// &
                    'TopHt double null,'// &
                    'QMD double null,'// &
                    'GTV double null,'// &
                    'GMV double null,'// &
                    'NMV double null,'// &
                    'RTPH double null,'// &
                    'RGTV double null,'// &
                    'RGMV double null,'// &
                    'RNMV double null,'// &
                    'ATBA double null,'// &
                    'ATSDI double null,'// &
                    'ATCCF double null,'// &
                    'ATTopHt double null,'// &
                    'ATQMD double null,'// &
                    'PrdLen int null,'// &
                    'Acc double null,'// &
                    'Mort double null,'// &
                    'MAI double null,'// &
                    'ForTyp int null,'// &
                    'SizeCls int null,'// &
                    'StkCls int null)'
    ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
      SQLStmtStr="CREATE TABLE FVS_Summary_East("// &
                    'Id int,'// &
                    'CaseID int,'// &
                    'StandID Text,'// &
                    'Year int,'// &
                    'Age int,'// &
                    'TPH NUMBER,'// &
                    'BA NUMBER,'// &
                    'SDI NUMBER,'// &
                    'CCF NUMBER,'// &
                    'TopHt NUMBER,'// &
                    'QMD NUMBER,'// &
                    'GTV NUMBER,'// &
                    'GMV NUMBER,'// &
                    'NMV NUMBER,'// &
                    'RTPH NUMBER,'// &
                    'RGTV NUMBER,'// &
                    'RGMV NUMBER,'// &
                    'RNMV NUMBER,'// &
                    'ATBA NUMBER,'// &
                    'ATSDI NUMBER,'// &
                    'ATCCF NUMBER,'// &
                    'ATTopHt NUMBER,'// &
                    'ATQMD NUMBER,'// &
                    'PrdLen int,'// &
                    'Acc NUMBER,'// &
                    'Mort NUMBER,'// &
                    'MAI NUMBER,'// &
                    'ForTyp int,'// &
                    'SizeCls int,'// &
                    'StkCls int)'
!
    ELSE
      SQLStmtStr='CREATE TABLE FVS_Summary_East('// &
                    'Id int primary key,'// &
                    'CaseID int not null,'// &
                    'StandID Char(26) null,'// &
                    'Year int null,'// &
                    'Age int null,'// &
                    'TPH real null,'// &
                    'BA real null,'// &
                    'SDI real null,'// &
                    'CCF real null,'// &
                    'TopHt real null,'// &
                    'QMD real null,'// &
                    'GTV real null,'// &
                    'GMV real null,'// &
                    'NMV real null,'// &
                    'RTPH real null,'// &
                    'RGTV real null,'// &
                    'RGMV real null,'// &
                    'RNMV real null,'// &
                    'ATBA real null,'// &
                    'ATSDI real null,'// &
                    'ATCCF real null,'// &
                    'ATTopHt real null,'// &
                    'ATQMD real null,'// &
                    'PrdLen int null,'// &
                    'Acc real null,'// &
                    'Mort real null,'// &
                    'MAI real null,'// &
                    'ForTyp int null,'// &
                    'SizeCls int null,'// &
                    'StkCls int null)'
    ENDIF
  ELSE
!----------
!  WESTERN VARIANT VOLUME NOMENCLATURE
!----------
    IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
      SQLStmtStr='CREATE TABLE FVS_Summary('// &
                    'Id int primary key,'// &
                    'CaseID int not null,'// &
                    'StandID text not null,'// &
                    'Year int null,'// &
                    'Age int null,'// &
                    'TPH double null,'// &
                    'BA double null,'// &
                    'SDI double null,'// &
                    'CCF double null,'// &
                    'TopHt double null,'// &
                    'QMD double null,'// &
                    'TCuM double null,'// &
                    'MCuM double null,'// &
                    'BdFt double null,'// &
                    'RTPH double null,'// &
                    'RTCuM double null,'// &
                    'RMCuM double null,'// &
                    'RBdFt double null,'// &
                    'ATBA double null,'// &
                    'ATSDI double null,'// &
                    'ATCCF double null,'// &
                    'ATTopHt double null,'// &
                    'ATQMD double null,'// &
                    'PrdLen int null,'// &
                    'Acc double null,'// &
                    'Mort double null,'// &
                    'MAI double null,'// &
                    'ForTyp int null,'// &
                    'SizeCls int null,'// &
                    'StkCls int null)'
    ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
      SQLStmtStr="CREATE TABLE FVS_Summary("// &
                    'Id int,'// &
                    'CaseID int,'// &
                    'StandID Text,'// &
                    'Year int,'// &
                    'Age int,'// &
                    'TPH NUMBER,'// &
                    'BA NUMBER,'// &
                    'SDI NUMBER,'// &
                    'CCF NUMBER,'// &
                    'TopHt NUMBER,'// &
                    'QMD NUMBER,'// &
                    'TCuM NUMBER,'// &
                    'MCuM NUMBER,'// &
                    'BdFt NUMBER,'// &
                    'RTPH NUMBER,'// &
                    'RTCuM NUMBER,'// &
                    'RMCuM NUMBER,'// &
                    'RBdFt NUMBER,'// &
                    'ATBA NUMBER,'// &
                    'ATSDI NUMBER,'// &
                    'ATCCF NUMBER,'// &
                    'ATTopHt NUMBER,'// &
                    'ATQMD NUMBER,'// &
                    'PrdLen int,'// &
                    'Acc NUMBER,'// &
                    'Mort NUMBER,'// &
                    'MAI NUMBER,'// &
                    'ForTyp int,'// &
                    'SizeCls int,'// &
                    'StkCls int)'
    ELSE
      SQLStmtStr='CREATE TABLE FVS_Summary('// &
                    'Id int primary key,'// &
                    'CaseID int not null,'// &
                    'StandID Char(26) null,'// &
                    'Year int null,'// &
                    'Age int null,'// &
                    'TPH real null,'// &
                    'BA real null,'// &
                    'SDI real null,'// &
                    'CCF real null,'// &
                    'TopHt real null,'// &
                    'QMD real null,'// &
                    'TCuM real null,'// &
                    'MCuM real null,'// &
                    'BdFt real null,'// &
                    'RTPH real null,'// &
                    'RTCuM real null,'// &
                    'RMCuM real null,'// &
                    'RBdFt real null,'// &
                    'ATBA real null,'// &
                    'ATSDI real null,'// &
                    'ATCCF real null,'// &
                    'ATTopHt real null,'// &
                    'ATQMD real null,'// &
                    'PrdLen int null,'// &
                    'Acc real null,'// &
                    'Mort real null,'// &
                    'MAI real null,'// &
                    'ForTyp int null,'// &
                    'SizeCls int null,'// &
                    'StkCls int null)'
    ENDIF
  ENDIF
  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
               int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                        'DBSSumry:Creating Table')
  SUMRYID = 0
ENDIF

!---------
!     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE
!---------
IF(SUMRYID.EQ.-1) THEN
  CALL DBSGETID(TABLENAME,'Id',ID)
  SUMRYID = ID
ENDIF
SUMRYID = SUMRYID + 1
!----------
!     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
!----------
IF(SUMRYID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
FQMDB=FQMD
FATQMDB=FATQMD
YMAIB=YMAI
TOPHTB=TOPHT
ATTOPHTB=ATTOPHT

IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR. &
       (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR. &
       (VARACD .EQ. 'ON')) THEN
!
  WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID, &
                 Year,Age,TPH,BA,SDI,CCF,TopHt,QMD,GTV,GMV,NMV, &
                 RTPH,RGTV,RGMV,RNMV,ATBA,ATSDI,ATCCF,ATTopHt, &
                 ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls) &
                 VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?, &
                 ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
ELSE
  WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(Id,CaseID,StandID, &
                 Year,Age,TPH,BA,SDI,CCF,TopHt,QMD,TCuM,MCuM,BdFt, &
                 RTPH,RTCuM,RMCuM,RBdFt,ATBA,ATSDI,ATCCF,ATTopHt, &
                 ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls) &
                 VALUES(?,?,',CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?, &
                 ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
ENDIF
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
              INT(0,SQLSMALLINT_KIND),SUMRYID,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IAGE,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ITPA,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IBA,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ISDI,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ICCF,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),TOPHTB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

 ColNumber=ColNumber+1
 iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),FQMDB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ITCUFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IMCUFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IBDFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IRTPA,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IRTCUFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IRMCUFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IRBDFT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IATBA,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IATSDI,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IATCCF,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),ATTOPHTB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

 ColNumber=ColNumber+1
 iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),FATQMDB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IPRDLEN,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IACC,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IMORT,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

 ColNumber=ColNumber+1
 iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),YMAIB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IFORTP,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ISZCL,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),ISTCL,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)


100 CONTINUE
iRet = fvsSQLCloseCursor(StmtHndlOut)
iRet = fvsSQLExecute(StmtHndlOut)
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                 'DBSSumry:Inserting Row')

200 CONTINUE
!Release statement handle
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

END



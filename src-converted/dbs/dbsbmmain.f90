SUBROUTINE DBSBMMAIN(NPLT,IYEAR,PREDBKP,POSTDBKP,RV,STD_BA,BAH, &
              BAK,TPA,TPAH,TPAK,VOL,VOLH,VOLK,BA_SP,SPCL_TPA,IPSLSH, &
              SANBAREM,SANTREM1,SANTREM2,VOLREMSAN,VOLREMSAL, &
              REMBKP,CID)
!
! DBS $Id$
!
!     PURPOSE: POPULATES A DATABASE TABLE WITH "MAIN" WWPBM OUTPUT VARS.
!     AUTH: AJ MCMAHAN -- ITX, INC. SEPT. 2005
!     INPUT:
!     PREDBKP    BKP PER ACRE AFTER REPRODUCTION BUT BEFORE DISPERSAL
!     POSTDBKP   BKP PER ACRE AFER DISPERSAL (& AFTER IN-FLIGHT REDUCTIONS)
!     RV         STAND RATING VALUE
!     STD_BA     TOTAL BASAL AREA (SQ FT PER ACRE)
!     BAH        BASAL AREA OF HOST (SQ FT / ACRE)
!     BAK        BASAL AREA BEETLE-KILLED (SQ FT PER ACRE)
!     TPA        TOTAL TPA IN THE STAND
!     TPAH       TPA HOST
!     TPAK,      TPA BEETLE-KILLED THIS YEAR
!     VOL        STAND VOLUME BEGINNING OF YEAR (CU FT / ACRE) WWPBM-ESTIMATED
!     VOLH       VOLUME OF HOST CU FT/ACRE; WWPBM-ESTIMATED
!     VOLK       VOLUME BEETLE-KILLED THIS YEAR
!     BA_SP      BASAL AREA OF "SPECIAL" TREES AT BEGINNING OF YEAR
!                  =AFTER THIS YRs LIGHTNING, ATTRACT PHER, FIRE SCORCH ETC.
!                  & INCLUDING LAST YRs (BUT NOT THIS YRs) PITCHOUTS/STRPKILS
!     SPCL_TPA   TPA SPECIAL TREES
!     IPSLSH     IPS SLASH (RECENT DEAD FUEL OF QUALIFYING SIZE) TONS PER ACRE
!     SANBAREM   BASAL AREA SANITIZED THIS YEAR (LIVE-TREE SANITIATIONS ONLY)
!     SANTREM1   TPA SANITIZED (LIVE-TREE SANITATIONS ONLY)
!     SANTREM2   TPA SANITIZED --LIVE PLUS DEAD TREE SANITATIONS
!     VOLREMSAN  VOLUME REMOVED SANITATION (ALL TREES, LIVE + DEAD)
!     VOLREMSAL  VOLUME REMOVED SALVAGE
!     REMBKP     BKP PER ACRE REMOVED VIA SANITATION CUTS
!
!---
!OMMONS
!
INCLUDE 'DBSCOM.f90'
!
!OMMONS END
!
!     DECLARATIONS
!---
INTEGER IYEAR,IRCODE
INTEGER(SQLSMALLINT_KIND)::ColNumber
REAL PREDBKP,POSTDBKP,RV,STD_BA,BAH,BAK,TPA,TPAH,TPAK, &
        VOL,VOLH,VOLK,BA_SP,SPCL_TPA,IPSLSH,REMBKP, &
        SANBAREM,SANTREM1,SANTREM2,VOLREMSAN,VOLREMSAL
DOUBLE PRECISION PREDBKPb,POSTDBKPb,RVb,STD_BAb,BAHb,BAKb,TPAb, &
         TPAHb,TPAKb, VOLb,VOLHb,VOLKb,BA_SPb,SPCL_TPAb,IPSLSHb, &
         SANBAREMb,SANTREM1b,SANTREM2b,VOLREMSANb,VOLREMSALb,REMBKPB
CHARACTER*2000 SQLStmtStr
CHARACTER(len=20) TABLENAME
CHARACTER(len=26) NPLT
!
!---------
!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
!---------
CALL DBSCASE(1)
!---------
!     ALLOCATE A STATEMENT HANDLE
!---------
iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                     'DBSSUMRY:DSN Connection')
  GOTO 200
ENDIF
!---------
!     CHECK TO SEE IF THE "MAINOUT" WWPBM TABLE EXISTS IN DATBASE
!---------
IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_BM_Main$]'
ELSE
  TABLENAME = 'FVS_BM_Main'
ENDIF
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) RETURN
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_BM_Main('// &
                 'CaseID Text not null,'// &
                 'StandID Text null,'// &
                 'Year Int null,'// &
                 'PreDispBKP  double null,'// &
                 'PostDispBKP double null,'// &
                 'StandRV double null,'// &
                 'StandBA double null,'// &
                 'BAH double null,'// &
                 'BA_BtlKld double null,'// &
                 'TPA double null,'// &
                 'TPAH double null,'// &
                 'TPA_BtlKld double null,'// &
                 'StandVol double null,'// &
                 'VolHost double null,'// &
                 'VolBtlKld double null,'// &
                 'BA_Special double null,'// &
                 'Ips_Slash double null,'// &
                 'BA_San_Remv double null,'// &
                 'BKP_San_Remv double null,'// &
                 'TPA_SanRemvLv double null,'// &
                 'TPA_SanRemLvDd double null,'// &
                 'VolRemSan double null,'// &
                 'VolRemSalv double null)'

  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_BM_Main('// &
                 'CaseID Text,'// &
                 'StandID Text,'// &
                 'Year Int,'// &
                 'PreDispBKP  Number,'// &
                 'PostDispBKP Number,'// &
                 'StandRV Number,'// &
                 'StandBA Number,'// &
                 'BAH Number,'// &
                 'BA_BtlKld Number,'// &
                 'TPA Number,'// &
                 'TPAH Number,'// &
                 'TPA_BtlKld Number,'// &
                 'StandVol Number,'// &
                 'VolHost Number,'// &
                 'VolBtlKld Number,'// &
                 'BA_Special Number,'// &
                 'Ips_Slash Number,'// &
                 'BA_San_Remv Number,'// &
                 'BKP_San_Remv Number,'// &
                 'TPA_SanRemvLv Number,'// &
                 'TPA_SanRemLvDd Number,'// &
                 'VolRemSan Number,'// &
                 'VolRemSalv Number)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_BM_Main('// &
                 'CaseID char(36) not null,'// &
                 'StandID char(26) not null,'// &
                 'Year Int null,'// &
                 'PreDispBKP real null,'// &
                 'PostDispBKP real null,'// &
                 'StandRV real null,'// &
                 'StandBA real null,'// &
                 'BAH real null,'// &
                 'BA_BtlKld real null,'// &
                 'TPA real null,'// &
                 'TPAH real null,'// &
                 'TPA_BtlKld real null,'// &
                 'StandVol real null,'// &
                 'VolHost real null,'// &
                 'VolBtlKld real null,'// &
                 'BA_Special real null,'// &
                 'Ips_Slash real null,'// &
                 'BA_San_Remv real null,'// &
                 'BKP_San_Remv real null,'// &
                 'TPA_SanRemvLv real null,'// &
                 'TPA_SanRemLvDd real null,'// &
                 'VolRemSan real null,'// &
                 'VolRemSalv real null)'
  ENDIF
  !PRINT*, SQLStmtStr

      !Close Cursor
      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
              'DBSBMMAIN:Creating Table: '//trim(SQLStmtStr))
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
PREDBKPb=    PREDBKP
POSTDBKPb=   POSTDBKP
RVb=         RV
STD_BAb=     STD_BA
BAHb=        BAH
BAKb=        BAK
TPAb=        TPA
TPAHb=       TPAH
TPAKb=       TPAK
VOLb=        VOL
VOLHb=       VOLH
VOLKb=       VOLK
BA_SPb=      BA_SP
REMBKPB=     REMBKP
SPCL_TPAb=   SPCL_TPA
IPSLSHb=     IPSLSH
SANBAREMb=   SANBAREM
SANTREM1b=   SANTREM1
SANTREM2b=   SANTREM2
VOLREMSANb=  VOLREMSAN
VOLREMSALb=  VOLREMSAL


WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,', &
     'StandID,Year,PreDispBKP,PostDispBKP,StandRV,StandBA,', &
     'BAH,BA_BtlKld,TPA,TPAH,TPA_BtlKld,StandVol,VolHost,', &
     'VolBtlKld,BA_Special,Ips_Slash,BA_San_Remv,BKP_San_Remv,', &
     'TPA_SanRemvLv,TPA_SanRemLvDd,VolRemSan,VolRemSalv)', &
     'VALUES(''',CID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,' &
     '?,?,?,?,?,?,?,?,?,?,?,?)'

!PRINT*, SQLStmtStr
iRet = fvsSQLCloseCursor(StmtHndlOut)
iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
!
!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
!
ColNumber=1
iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber, SQL_PARAM_INPUT, &
              SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND), &
              INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),PREDBKPB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
              SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
              INT(5,SQLSMALLINT_KIND),POSTDBKPB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),RVb,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),STD_BAB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),BAHB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),BAKB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),TPAB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),TPAHB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),TPAKB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),VOLB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),VOLHB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),VOLKB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),BA_SPB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),IPSLSHB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SANBAREMB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),REMBKPB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SANTREM1B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),SANTREM2B,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),VOLREMSANB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

ColNumber=ColNumber+1
iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT, &
            SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND), &
            INT(5,SQLSMALLINT_KIND),VOLREMSALB,int(4,SQLLEN_KIND), &
              SQL_NULL_PTR)

100 CONTINUE
!Close Cursor
iRet = fvsSQLCloseCursor(StmtHndlOut)

iRet = fvsSQLExecute(StmtHndlOut)
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                 'DBSBMMAIN:Inserting Row')

200 CONTINUE
!Release statement handle
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

END


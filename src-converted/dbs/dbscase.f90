SUBROUTINE DBSCASE(IFORSURE)
IMPLICIT NONE
!
! DBS $Id$
!
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              OUTPUT.
!
!     INPUT: IFORSURE - 1 NEED CONNECTION, 0 CHECK IF CONNECTION IS
!                       NEEDED.
!
!---
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'DBSCOM.f90'
!
!
INCLUDE 'KEYCOM.f90'
!
!
INCLUDE 'OPCOM.f90'
!
!
INCLUDE 'OUTCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
!OMMONS
!---

INTEGER(SQLINTEGER_KIND),parameter:: MaxStringLen=255

CHARACTER*2000 SQLStmtStr
CHARACTER*10  DATO, REV
CHARACTER*8   TIM, SVN
CHARACTER*(*) CFN
CHARACTER(len=MaxStringLen) TIMESTAMP
INTEGER IFORSURE, IFORSR, I, KODE, IRCODE
CHARACTER(len=36) CID

CHARACTER*20 TABLENAME

CHARACTER*2 VAR

INCLUDE 'INCLUDESVN.f90'


CALL REVISE(VARACD,REV)
VAR=VARACD

!-----
!     CHECK TO SEE IF WE ARE NEEDING TO CONTINUE
!-----
IFORSR=IFORSURE
IF(IFORSR.EQ.0) THEN
  IF(ISUMARY.GE.1.OR. &
        ICOMPUTE.GE.1.OR. &
        IATRTLIST.GE.1.OR. &
        ITREELIST.GE.1.OR. &
        ICUTLIST.GE.1.OR. &
        IDM1.GE.1.OR.IDM2.GE.1.OR.IDM3.GE.1.OR. &
        IDM5.GE.1.OR.IDM6.GE.1.OR. &
        IPOTFIRE.GE.1.OR. &
        IFUELS.GE.1.OR. &
        ICMRPT.GE.1.OR. &
        ICHRPT.GE.1.OR. &
        ISTRCLAS.GE.1.OR. &
        IFUELC.GE.1.OR. &
        IBURN.GE.1.OR. &
        IMORTF.GE.1.OR. &
        ISSUM.GE.1.OR. &
        ISDET.GE.1.OR. &
        ICANPR.GE.1.OR. &
        IDWDVOL.GE.1.OR. &
        IDWDCOV.GE.1.OR. &
        IBMMAIN.GE.1.OR. &
        IBMBKP.GE.1.OR. &
        IBMTREE.GE.1.OR. &
        IBMVOL.GE.1) IFORSR = 1
 ENDIF
 IF(IFORSR.EQ.0) RETURN

!---------
!     IF ALREADY HAVE A CURRENT CASE NUMBER, JUST BAIL
!---------
IF (CASEID.NE."") RETURN
!---
!     Initialize variables
!     CREATE DATETIME
CALL GRDTIM(DATO,TIM)
TIMESTAMP = DATO(7:10)//'-'//DATO(1:5)//'-'//TIM
!
!     STRIP 3-CHARACTER EXTENSION (IF PRESENT) FROM KEYFNAME
!
KEYFNAME = TRIM(KEYFNAME)
I = INDEX(".KEY",KEYFNAME)
IF (I.GT.0 .AND. I.EQ.LEN_TRIM(KEYFNAME)-3) THEN
    KEYFNAME = KEYFNAME(1:I-4)
ENDIF

!---------
!     MAKE SURE WE HAVE AN OPEN CONNECTION
!---------
IF(ConnHndlOut.EQ.-1) CALL DBSOPEN(DSNOUT,EnvHndlOut, &
                         ConnHndlOut,DBMSOUT,0,.FALSE.,KODE)
!---------
!     ALLOCATE A STATEMENT HANDLE
!---------
iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  ICOMPUTE  = 0
  ISUMARY   = 0
  IATRTLIST = 0
  ITREELIST = 0
  ICUTLIST  = 0
  IDM1      = 0
  IDM2      = 0
  IDM3      = 0
  IDM5      = 0
  IDM6      = 0
  IPOTFIRE  = 0
  IFUELS    = 0
  ICMRPT    = 0
  ICHRPT    = 0
  ISTRCLAS  = 0
  IFUELC    = 0
  IBURN     = 0
  IMORTF    = 0
  ISSUM     = 0
  ISDET     = 0
  ICANPR    = 0
  IDWDVOL   = 0
  IDWDCOV   = 0
  IBMMAIN   = 0
  IBMBKP    = 0
  IBMTREE   = 0
  IBMVOL    = 0
!
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                    'DBSRun:Connecting to DSN')
  GOTO 200
ENDIF

!---------
!     MAKE SURE WE HAVE AN OPEN CONNECTION
!---------
IF(ConnHndlOut.EQ.-1) CALL DBSOPEN(DSNOUT,EnvHndlOut, &
                         ConnHndlOut,DBMSOUT,0,.FALSE.,KODE)
!---------
!     CHECK TO SEE IF THE FVS_Cases TABLE EXISTS IN DATBASE
!---------
IF(TRIM(DBMSOUT).EQ."EXCEL") THEN
  TABLENAME = '[FVS_Cases$]'
ELSEIF(TRIM(DBMSOUT).EQ."ORACLE") THEN
  TABLENAME = '"FVS_Cases"'
ELSE
  TABLENAME = 'FVS_Cases'
ENDIF
CALL DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).EQ.'EXCEL')
IF(IRCODE.EQ.2) RETURN
IF(IRCODE.EQ.1) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr="CREATE TABLE FVS_Cases("// &
                 "CaseID Text primary key,"// &
                 "Stand_CN Text null,"// &
                 "StandID Text null,"// &
                 "MgmtID Text null,"// &
                 "RunTitle Text null,"// &
                 "KeywordFile Text null,"// &
                 "SamplingWt double null,"// &
                 "Variant Text null,"// &
                 "Version Text null,"// &
                 "RV Text null,"// &
                 "Groups Text null,"// &
                 "RunDateTime Text null)"
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr="CREATE TABLE FVS_Cases("// &
                 "CaseID Text,"// &
                 "Stand_CN Text,"// &
                 "StandID Text,"// &
                 "MgmtID Text,"// &
                 "RunTitle Text,"// &
                 "KeywordFile Text,"// &
                 "SamplingWt Number,"// &
                 "Variant Text,"// &
                 "Version Text,"// &
                 "RV Text,"// &
                 "Groups Text,"// &
                 "RunDateTime Text);"
  ELSE
    SQLStmtStr="CREATE TABLE "//TABLENAME// &
                 " (CaseID char(36) primary key,"// &
                 "Stand_CN char(40),"// &
                 "StandID char(26),"// &
                 "MgmtID char(4),"// &
                 "RunTitle char(72),"// &
                 "KeywordFile char(50),"// &
                 "SamplingWt real,"// &
                 "Variant char(2),"// &
                 "Version char(10),"// &
                 "RV char(8),"// &
                 "Groups char(250),"// &
                 "RunDateTime char(19))"
  ENDIF

  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
          'DBSCase:Creating Table: '//trim(SQLStmtStr))
  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)

ENDIF
!---------
!     CREATE ENTRY FROM DATA FOR FVSRUN TABLE
!---------
CALL UUIDGEN(CASEID)
!----------
!           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL
!----------
if (LENSLS.EQ.-1) SLSET =""
IF (KEYFNAME.EQ.' ') KEYFNAME='Unknown'
WRITE(SQLStmtStr,*)"INSERT INTO ",trim(TABLENAME), &
    " (CaseID,Stand_CN,StandID,MgmtID,RunTitle,KeywordFile,", &
    "SamplingWt,Variant,Version,RV,Groups,RunDateTime) ", &
    "VALUES('",CASEID,"','", &
    TRIM(ADJUSTL(DBCN)),"','",TRIM(ADJUSTL(NPLT)),"','", &
    TRIM(ADJUSTL(MGMID)),"','",TRIM(ADJUSTL(ITITLE)),"','", &
    TRIM(ADJUSTL(KEYFNAME)),"',",SAMWT,",'",VAR,"',", &
    "'",SVN,"','",REV,"','", &
    TRIM(ADJUSTL(SLSET)),"','",TRIM(ADJUSTL(TIMESTAMP)),"')"
!Close Cursor
iRet = fvsSQLCloseCursor(StmtHndlOut)

!Execute Query
iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                 'DBSCase:Inserting Row: '//trim(SQLStmtStr))

!Release statement handle
200 CONTINUE
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
RETURN

!
!     CALLED BY FILOPN: ENTRY TO SAVE THE KEYWORD FILE NAME AND TO SET
!                       THE DEFAULT DBS CONNECTIONS
!
ENTRY DBSVKFN (CFN)
I=LEN_TRIM(CFN)
IF (I.GT.LEN(KEYFNAME)) THEN
   KEYFNAME=CFN(1:4)//'...'//CFN(I+8-LEN(KEYFNAME):)
ELSE
   KEYFNAME = CFN
ENDIF
RETURN
!
!======================================================================
!     ENTRY FOR WWPBM, FETCHING CASEID
!    (NOTE: THE WWPBM NEEDS TO KNOW AND SAVE (INTERNALLY) CASEID, BECAUSE
!     IT IS DOING ITS DB-WRITING FROM WITHIN ITS OWN INTERNAL STAND LOOP
!
ENTRY DBSWW2(CID)
CID=CASEID
RETURN
!======================================================================
END


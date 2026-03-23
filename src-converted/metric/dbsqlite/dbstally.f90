SUBROUTINE DBSTALLY(PRBSTK,NTALLY,CSP,SUMTPA,SUMPCT,BESTTPA, &
    BESTPCT,AVEHT,PASTPA,PASPCT,IYEAR,WTAVEHT,TOT,TOTTTPAA, &
    TOTTTPAB,TOTTTPA3)
IMPLICIT NONE
!----------
! DBSQLITE-METRIC $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE REGENERATION TALLY
!              SUMMARY BY SPECIES OF THE TREES PER ACRE, PERCENTAGE OF TOTAL,
!              AND AVERAGE HEIGHT FOR ALL TREES, BEST TREES,
!              AND TREES LESS THAN 3 INCHES IN DIAMETER THAT REGENERATED
!              DURING A GIVEN TALLY.
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'PLOT.f90'
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
!OMMONS
!
INTEGER ColNumber,iret,NTALLY,SUMTPA1,SUMPCT1,BESTTPA1
INTEGER I,PASTPA1,PASPCT1,BESTPCT1,IYEAR,TOT,IYEAR1
REAL PRBSTK,AVEHT,WTAVEHT,TOTTTPAA,TOTTTPAB,TOTTTPA3
REAL SUMTPA,SUMPCT,BESTTPA,PASTPA,PASPCT,BESTPCT
DOUBLE PRECISION PRBSTK1,AVEHT1
CHARACTER*2000 SQLStmtStr
CHARACTER*3 CSP
CHARACTER*8 CSP1, CSP2, CSP3

INTEGER fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text
IF(IREG4.NE.1) RETURN
!
CALL DBSCASE(1)

IF(PRBSTK.GT.1) GOTO 100

!     DEFINE TAABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Tally_Metric' &
     //CHAR(0))
!
!       TALLY SUMMARY
!
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_Tally_Metric('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'ProbStock real null,'// &
                 'TallyNumber int null,'// &
                 'SpeciesFVS text null,'// &
                 'SpeciesPLANTS text null,'// &
                 'SpeciesFIA text null,'// &
                 'TphAll int null,'// &
                 'PctOfTotalAll int null,'// &
                 'TphBest int null,'// &
                 'PctOfTotalBest int null,'// &
                 'AverageHt real null,'// &
                 'TphLt76cm int null,'// &
                 'PctOfTotalLt76cm int null);'//CHAR(0)

iRet = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet .NE. 0) THEN
  IREG4 = 0
  RETURN
ENDIF
ENDIF

  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally_Metric', &
            ' (CaseID,StandID,Year,ProbStock,TallyNumber,', &
            'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
            'TphAll,PctOfTotalAll,TphBest,', &
            'PctOfTotalBest,AverageHt,TphLt76cm,PctOfTotalLt76cm)', &
            'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,', &
            '?,?,?,?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
    IF (iRet .NE. 0) THEN
      IREG4 = 0
      RETURN
    ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
!
IF (CSP .EQ. "ALL") THEN
  CSP1 = "ALL"
  CSP2 = "ALL"
  CSP3 = "ALL"
ELSE
  DO I = 1,MAXSP
    IF (CSP(1:2) .EQ. JSP(I)) THEN
      CSP1 = JSP(I)
      CSP2 = PLNJSP(I)
      CSP3 = FIAJSP(I)
    ENDIF
  ENDDO
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  PRBSTK1=PRBSTK
  IF(TOT.EQ.1)THEN
  AVEHT1=AVEHT
  ELSE
  AVEHT1=WTAVEHT
  ENDIF
!
!     ASSIGN INTEGER VALUES TO REAL VARS
!
  IF(TOT.EQ.1)THEN
  SUMTPA1=NINT(SUMTPA)
  BESTTPA1=NINT(BESTTPA)
  PASTPA1=NINT(PASTPA)
  PASPCT1=NINT(PASPCT)
  BESTPCT1=NINT(BESTPCT)
  SUMPCT1=NINT(SUMPCT)
  ELSE
  SUMTPA1=NINT(TOTTTPAA)
  BESTTPA1=NINT(TOTTTPAB)
  PASTPA1=NINT(TOTTTPA3)
  PASPCT1=100
  BESTPCT1=100
  SUMPCT1=100
  ENDIF
  IYEAR1=IYEAR+1

  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,PRBSTK1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                     LEN_TRIM(CSP1))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                     LEN_TRIM(CSP2))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                     LEN_TRIM(CSP3))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  IF (iRet.ne.0) THEN
    IREG4 = 0
  ENDIF
  RETURN
100 CONTINUE

!     DEFINE TABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Tally_Metric' &
    //CHAR(0))
!
!       SITE PREP SUMMARY
!
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_Tally_Metric('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'TallyNumber int null,'// &
                 'SpeciesFVS text null,'// &
                 'SpeciesPLANTS text null,'// &
                 'SpeciesFIA text null,'// &
                 'TphAll int null,'// &
                 'PctOfTotalAll int null,'// &
                 'TphBest int null,'// &
                 'PctOfTotalBest int null,'// &
                 'AverageHt real null,'// &
                 'TphLt76cm int null,'// &
                 'PctOfTotalLt76cm int null);'//CHAR(0)

iRet = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet .NE. 0) THEN
  IREG4 = 0
  RETURN
ENDIF
ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
!
IF (CSP .EQ. "ALL") THEN
  CSP1 = "ALL"
  CSP2 = "ALL"
  CSP3 = "ALL"
ELSE
  DO I = 1,MAXSP
    IF (CSP(1:2) .EQ. JSP(I)) THEN
      CSP1 = JSP(I)
      CSP2 = PLNJSP(I)
      CSP3 = FIAJSP(I)
    ENDIF
  ENDDO
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally_Metric', &
            ' (CaseID,StandID,Year,TallyNumber,', &
            'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
            'TphAll,PctOfTotalAll,TphBest,', &
            'PctOfTotalBest,AverageHt,TphLt76cm,PctOfTotalLt76cm)', &
            'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,', &
            '?,?,?,?,?,?,?,?,?);'

iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
    IF (iRet .NE. 0) THEN
      IREG4 = 0
      RETURN
    ENDIF

!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
  IF(TOT.EQ.1)THEN
  AVEHT1=AVEHT
  ELSE
  AVEHT1=WTAVEHT
  ENDIF
!
!     ASSIGN INTEGER VALUES TO REAL VARS
!
  IF(TOT.EQ.1)THEN
  SUMTPA1=NINT(SUMTPA)
  BESTTPA1=NINT(BESTTPA)
  PASTPA1=NINT(PASTPA)
  PASPCT1=NINT(PASPCT)
  BESTPCT1=NINT(BESTPCT)
  SUMPCT1=NINT(SUMPCT)
  ELSE
  SUMTPA1=NINT(TOTTTPAA)
  BESTTPA1=NINT(TOTTTPAB)
  PASTPA1=NINT(TOTTTPA3)
  PASPCT1=100
  BESTPCT1=100
  SUMPCT1=100
  ENDIF
  IYEAR1=IYEAR+1

  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                     LEN_TRIM(CSP1))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                     LEN_TRIM(CSP2))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                     LEN_TRIM(CSP3))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  IF (iRet.ne.0) THEN
    IREG4 = 0
  ENDIF

END

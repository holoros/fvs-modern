SUBROUTINE DBSINGROW(IYEAR,INGROW,CSP,TPAIN)
IMPLICIT NONE
!----------
! METRIC-DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              PLOT HABITAT TYPE SUMMARY OUTPUT
!     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'METRIC.f90'
!
!OMMONS
!
INTEGER ColNumber,I,iret,IYEAR,IYEAR1
REAL INGROW,TPAIN
DOUBLE PRECISION INGROW1,TPAIN1
CHARACTER*2000 SQLStmtStr
CHARACTER*2 CSP
CHARACTER*8 CSP1,CSP2,CSP3 &
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

!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text

IF(IREG5.NE.1) RETURN
!
CALL DBSCASE(1)

!     DEFINE TAABLENAME

iRet=fsql3_tableexists(IoutDBref, &
     'FVS_Regen_Ingrow_Metric'//CHAR(0))
!
!       SITE PREP SUMMARY
!
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_Ingrow_Metric('// &
                 'CaseID           text not null,'// &
                 'StandID          text not null,'// &
                 'Year             int  null,'// &
                 'IngrowthTotalTph real null,'// &
                 'SpeciesFVS       text null,'// &
                 'SpeciesPLANTS    text null,'// &
                 'SpeciesFIA       text null,'// &
                 'IngrowthTph      int  null);'//CHAR(0)

iRet = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet .NE. 0) THEN
  IREG5 = 0
  RETURN
ENDIF
ENDIF

  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow_Metric', &
        ' (CaseID,StandID,Year,IngrowthTotalTph,', &
        ' SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTph)', &
        ' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'
    IF (iRet .NE. 0) THEN
      IREG5 = 0
      RETURN
    ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
!
DO I = 1,MAXSP
  IF (CSP .EQ. JSP(I)) THEN
    CSP1 = JSP(I)
    CSP2 = PLNJSP(I)
    CSP3 = FIAJSP(I)
  ENDIF
ENDDO
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!

  INGROW1=INGROW
  TPAIN1=TPAIN
  IYEAR1=IYEAR

  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,INGROW1)
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
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  IF (iRet.ne.0) then
    IREG5 = 0
  ENDIF
  RETURN
END

SUBROUTINE DBSSITEPREP(NOYEAR,MECHYEAR,BURNYEAR,PCTNONE, &
    PCTMECH,PCTBURN)
IMPLICIT NONE
!----------
! DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              SITE PREP SUMMARY OUTPUT
!     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019
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
INTEGER ColNumber,iret,NOYEAR,MECHYEAR,BURNYEAR
INTEGER PCTNONE,PCTMECH,PCTBURN
CHARACTER*2000 SQLStmtStr
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_finalize

IF(IREG2.NE.1) RETURN
!
CALL DBSCASE(1)
!     DEFINE TAABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_SitePrep'//CHAR(0))
!
!       SITE PREP SUMMARY
!
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_SitePrep('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'YearNone int null,'// &
                 'YearMech int null,'// &
                 'YearBurn int null,'// &
                 'PcntNone int null,'// &
                 'PcntMech int null,'// &
                 'PcntBurn int null);'//CHAR(0)

iRet = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet .NE. 0) THEN
  IREG2 = 0
  RETURN
ENDIF
ENDIF

  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_SitePrep', &
            ' (CaseID,StandID,YearNone,YearMech,YearBurn,', &
            'PcntNone,PcntMech,PcntBurn)', &
            'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'


  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
    IF (iRet .NE. 0) THEN
      IREG2 = 0
      RETURN
    ENDIF

  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NOYEAR)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,MECHYEAR)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BURNYEAR)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTNONE)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTMECH)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PCTBURN)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  IF (iRet.ne.0) then
    IREG2 = 0
  ENDIF
  RETURN
END

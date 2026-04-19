SUBROUTINE DBSFMCANPR(IYEAR,CRFILL,NPLT)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH CANOPY PROFILE
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- JUNE 2006
!     INPUT:
!              THE CANOPY PROFILE INFO FROM THE FIRE MODEL.
!              1: BIOMASS OF CANOPY FUELS AT VARIOUS HEIGHTS
!                  ABOVE THE GROUND IN LBS/ACRE/FOOT.
!
!     THIS TABLE IS UNIQUE IN THAT THERE IS NOT A CORRESPONDING
!     TEXT FILE TABLE.
!
!OMMONS
!
!
INCLUDE 'DBSCOM.f90'
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP &
   !$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS &
   !_WIN64) &
   !$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT &
   !$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG &
   !$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC &
   !$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN &
   !$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE &
   !$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET &
   !$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP &
   !$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS

!
!OMMONS
!---
INTEGER IYEAR,iRet,I,ColNumber
REAL CRFILL
DOUBLE PRECISION CRFILLB, CRFILLKG, HTFT, HTM
DIMENSION CRFILL(400)
CHARACTER*2000 SQLStmtStr
CHARACTER(len=26) NPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset

IF(ICANPR.EQ.0) RETURN

iRet = 0
DO I = 1,400
  IF (CRFILL(I).GE.0) THEN
    iRet=1
    EXIT
  ENDIF
ENDDO
if (iRet.eq.0) RETURN

CALL DBSCASE(1)
iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_CanProfile"//CHAR(0))
IF(iRet.EQ.0) THEN
  SQLStmtStr='CREATE TABLE FVS_CanProfile ('// &
            'CaseID text not null,'// &
            'StandID text not null,'// &
            'Year Int null,'// &
            'Height_m real null,'// &
            'Canopy_Fuel_kg_m3 real null,'// &
            'Height_ft real null,'// &
            'Canopy_Fuel_lbs_acre_ft real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ICANPR = 0
    RETURN
  ENDIF
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_CanProfile (CaseID,', &
       'StandID,Year,Height_m,Canopy_Fuel_kg_m3,Height_ft,', &
       'Canopy_Fuel_lbs_acre_ft) VALUES (''',CASEID, &
       ''',''',TRIM(NPLT),''',?,?,?,?,?);'
iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  ICANPR = 0
  RETURN
ENDIF

DO I = 1,400
  IF (CRFILL(I).LE.0) CYCLE
!
!       ASSIGN VALUES TO DOUBLE PRECISION VARS
!
  CRFILLB = CRFILL(I)
  CRFILLKG = CRFILL(I)*0.45359237 / (4046.856422 * 0.3048)
  HTFT = I
  HTM = I*0.3048

  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HTM)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLKG)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HTFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLB)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)
enddo
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ICANPR = 0
ENDIF
iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
RETURN
END



SUBROUTINE DBSFMHRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE CARBON HARVEST
!              PRODUCTS REPORT INFORMATION
!     AUTH: D. ROBINSON - ESSA
!     INPUT:
!       IYEAR  - CALENDAR YEAR
!       NPLT   - CASE NUMBER
!       VAR    - ARRAY WITH VARIABLES TO REPORT
!       VARDIM - LENGTH OF VAR ARRAY
!         1 = PRODUCTS
!         2 = LANDFILL
!         3 = ENERGY
!         4 = EMISSIONS
!         5 = MERCH CARBON STORED
!         6 = MERCH CARBON REMOVED
!OMMONS
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

!OMMONS

INTEGER IYEAR,KODE,VARDIM,iRet
CHARACTER(len=26) NPLT
REAL      VAR
DIMENSION VAR(VARDIM)

DOUBLE PRECISION  VARD(VARDIM)
INTEGER           I,ColNumber
CHARACTER*2000    SQLStmtStr

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

IF(ICHRPT.EQ.0) RETURN
IF(ICHRPT.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)
iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_Hrv_Carbon"//CHAR(0))
IF(iRet.EQ.0) THEN
  SQLStmtStr='CREATE TABLE FVS_Hrv_Carbon('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year int null,' // &
         'Products real null,' // &
         'Landfill real null,' // &
         'Energy real null,' // &
         'Emissions real null,' // &
         'Merch_Carbon_Stored real null,' // &
         'Merch_Carbon_Removed real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ICHRPT = 0
    RETURN
  ENDIF
ENDIF

VARD = VAR

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Hrv_Carbon (CaseID,', &
     'StandID,Year,Products,Landfill,Energy,Emissions,', &
     'Merch_Carbon_Stored,Merch_Carbon_Removed) VALUES(''', &
     CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?)'
iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  ICHRPT = 0
  RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

DO I=1,VARDIM
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
ENDDO
iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ICHRPT = 0
ENDIF
iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
RETURN
END


SUBROUTINE DBSFMCRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE MAIN CARBON REPORT
!              INFORMATION
!     AUTH: D. ROBINSON - ESSA
!     INPUT:
!       IYEAR  - CALENDAR YEAR
!       NPLT   - CASE NUMBER
!       VAR    - ARRAY WITH VARIABLES TO REPORT
!       VARDIM - LENGTH OF VAR ARRAY
!       UNITS ARE TONS/ACRE IF USER HAS REQUESTED IMPERIAL UNITS; TONNES/HA IF METRIC
!         1 = ABOVEGROUND TOTAL LIVE
!         2 = ABOVEGOURND MERCH LIVE
!         3 = BELOWGROUND LIVE
!         4 = BELOWGROUND DEAD (WHICH DECAYS)
!         5 = STANDING DEAD
!         6 = FOREST DOWN DEAD WOOD
!         7 = FOREST FLOOR (SOILS?)
!         8 = FOREST SHRUB+HERB
!         9 = TOTAL STAND CARBON
!        10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
!        11 = CARBON RELEASED THRU FIRE
!       KODE   - RETURN CODE
!
!OMMONS
!
INCLUDE 'DBSCOM.f90'
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

INTEGER IYEAR, KODE,VARDIM
CHARACTER(len=26) NPLT
REAL      VAR
DIMENSION VAR(VARDIM)

DOUBLE PRECISION  VARD(VARDIM)
INTEGER           iRet,I,ColNumber
CHARACTER*2000    SQLStmtStr

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

!     Initialize variables

IF(ICMRPT.EQ.0) RETURN
IF(ICMRPT.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

!     INSURE MAIN CARBON TABLE EXISTS IN DATBASE

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_Carbon"//CHAR(0))
IF(iRet.EQ.0) THEN
  SQLStmtStr='CREATE TABLE FVS_Carbon('// &
            'CaseID text not null,'// &
            'StandID text not null,'// &
            'Year Int null,'// &
            'Aboveground_Total_Live real null,'// &
            'Aboveground_Merch_Live real null,'// &
            'Belowground_Live real null,'// &
            'Belowground_Dead real null,'// &
            'Standing_Dead real null,'// &
            'Forest_Down_Dead_Wood real null,'// &
            'Forest_Floor real null,'// &
            'Forest_Shrub_Herb real null,'// &
            'Total_Stand_Carbon real null,'// &
            'Total_Removed_Carbon real null,'// &
            'Carbon_Released_From_Fire real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ICMRPT = 0
    RETURN
  ENDIF
ENDIF

!     COPY INPUT VECTOR TO DOUBLE-PRECISION

VARD = VAR

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Carbon (CaseID,', &
     'StandID,Year,Aboveground_Total_Live,Aboveground_Merch_Live,', &
     'Belowground_Live,Belowground_Dead,Standing_Dead,', &
     'Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,', &
     'Total_Stand_Carbon,Total_Removed_Carbon,', &
     'Carbon_Released_From_Fire) VALUES (''', &
     CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

!     PREPARE THE SQL QUERY

iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
if (iRet.ne.0) then
   ICMRPT = 0
   RETURN
ENDIF

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

DO I=1,VARDIM
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
ENDDO
iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ICMRPT = 0
ENDIF

END

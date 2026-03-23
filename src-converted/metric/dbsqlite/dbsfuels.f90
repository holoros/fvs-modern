SUBROUTINE DBSFUELS(IYEAR,NPLT,LITTER,DUFF,SDEADLT3,SDEADGT3, &
     SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3, &
     SNAGSGT3,FOLIAGE,STANDLT3,STANDGT3,STANDTOTAL,BIOMASS,CONSUMED, &
     REMOVED,KODE)

IMPLICIT NONE
!
! METRIC-DBSQLITE $Id: dbsfuels.f 2477 2018-08-30 15:16:07Z lancedavid $
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS REPORT INFORMATION
!     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
!     INPUT:
!              THE ALL FUELS OUTPUT FROM THE FIRE MODEL.
!              1: SURFACE LITTER
!              2: SURFACE DUFF
!              3: SURFACE DEAD FUEL LESS THAN 3 INCHES
!              4: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 3 INCHES
!              5: SURFACE DEAD FUEL BETWEEN 3 AND 6
!              6: SURFACE DEAD FUEL BETWEEN 6 AND 12
!              7: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 12
!              8: SURFACE HERB FUEL
!              9: SURFACE SHRUB FUEL
!             10: SURFACE TOTAL FUEL
!             11: STANDING SNAGS LESS THAN 3
!             12: STANDING SNAGS GREATER THAN OR EQUAL TO 3
!             13: STANDING FOLIAGE
!             14: STANDING LIVE LESS THAN 3
!             15: STANDING LIVE GREATER THAN OR EQUAL TO 3
!             16: STANDING TOTAL
!             17: TOTAL BIOMASS
!             18: TOTAL CONSUMED
!             19: BIOMASS REMOVED
!             20: KODE FOR WHETHER REPORT ALSO DUMPS TO FILE
!
!OMMONS
!
!     ******************************************************************
!     NOTE: The variables written out in this METRIC version are already
!     computed in metric units by the calling subroutine FMFOUT. So
!     there is no need to convert them here. Some label names are
!     tweaked: e.g., TPA -> TPH for "Trees/acre" to "Trees/hectare"
!     ******************************************************************
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

!
!OMMONS
!

INTEGER IYEAR,BIOMASS,CONSUMED,REMOVED,iRet,KODE,STANDGT3, &
           STANDTOTAL,ColNumber
REAL LITTER,DUFF,SDEADLT3,SDEADGT3,SDEAD3TO6, &
     SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,SNAGSGT3, &
     FOLIAGE,STANDLT3
DOUBLE PRECISION LITTERB,DUFFB,SDEADLT3B,SDEADGT3B,SDEAD3TO6B, &
     SDEAD6TO12B,SDEADGT12B,HERBB,SHRUBB,SURFTOTALB,SNAGSLT3B, &
     SNAGSGT3B,FOLIAGEB,STANDLT3B
CHARACTER*2000 SQLStmtStr
CHARACTER(len=26) NPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

IF(IFUELS.EQ.0) RETURN
IF(IFUELS.EQ.2) KODE = 0

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_Fuels_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Fuels_Metric('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year Int null,'// &
                 'Surface_Litter real null,'// &
                 'Surface_Duff real null,'// &
                 'Surface_lt76 real null,'// &
                 'Surface_ge76 real null,'// &
                 'Surface_76to152 real null,'// &
                 'Surface_152to305 real null,'// &
                 'Surface_ge305 real null,'// &
                 'Surface_Herb real null,'// &
                 'Surface_Shrub real null,'// &
                 'Surface_Total real null,'// &
                 'Standing_Snag_lt76 real null,'// &
                 'Standing_Snag_ge76 real null,'// &
                 'Standing_Foliage real null,'// &
                 'Standing_Live_lt76 real null,'// &
                 'Standing_Live_ge76 real null,'// &
                 'Standing_Total real null,'// &
                 'Total_Biomass Int null,'// &
                 'Total_Consumed Int null,'// &
                 'Biomass_Removed Int null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IFUELS = 0
     RETURN
   ENDIF
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
LITTERB=LITTER
DUFFB=DUFF
SDEADLT3B=SDEADLT3
SDEADGT3B=SDEADGT3
SDEAD3TO6B=SDEAD3TO6
SDEAD6TO12B=SDEAD6TO12
SDEADGT12B=SDEADGT12
HERBB=HERB
SHRUBB=SHRUB
SURFTOTALB=SURFTOTAL
SNAGSLT3B=SNAGSLT3
SNAGSGT3B=SNAGSGT3
FOLIAGEB=FOLIAGE
STANDLT3B=STANDLT3

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Fuels_Metric (CaseID,', &
     'StandID,Year,Surface_Litter,Surface_Duff,Surface_lt76,', &
     'Surface_ge76,Surface_76to152,Surface_152to305,Surface_ge305,', &
     'Surface_Herb,Surface_Shrub,Surface_Total,Standing_Snag_lt76,', &
     'Standing_Snag_ge76,Standing_Foliage,Standing_Live_lt76,', &
     'Standing_Live_ge76,Standing_Total,Total_Biomass,', &
     'Total_Consumed,Biomass_Removed) VALUES(''',CASEID,''',''', &
     TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
iRet = fsql3_prepare(IoutDBref, SQLStmtStr)
IF (iRet .NE. 0) THEN
   IFUELS = 0
   RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,LITTERB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADLT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADGT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEAD3TO6B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEAD6TO12B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADGT12B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HERBB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SHRUBB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SURFTOTALB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SNAGSLT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SNAGSGT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,FOLIAGEB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,STANDLT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,STANDGT3)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,STANDTOTAL)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,BIOMASS)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,CONSUMED)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,REMOVED)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IFUELS = 0
ENDIF
RETURN

END



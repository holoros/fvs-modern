SUBROUTINE DBSFUELS(IYEAR,NPLT,LITTER,DUFF,SDEADLT3,SDEADGT3, &
     SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3, &
     SNAGSGT3,FOLIAGE,STANDLT3,STANDGT3,STANDTOTAL,BIOMASS,CONSUMED, &
     REMOVED,KODE)

IMPLICIT NONE
!
! DBSQLITE $Id$
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
INCLUDE 'DBSCOM.f90'
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
          "FVS_Fuels"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Fuels('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year Int null,'// &
                 'Surface_Litter real null,'// &
                 'Surface_Duff real null,'// &
                 'Surface_lt3 real null,'// &
                 'Surface_ge3 real null,'// &
                 'Surface_3to6 real null,'// &
                 'Surface_6to12 real null,'// &
                 'Surface_ge12 real null,'// &
                 'Surface_Herb real null,'// &
                 'Surface_Shrub real null,'// &
                 'Surface_Total real null,'// &
                 'Standing_Snag_lt3 real null,'// &
                 'Standing_Snag_ge3 real null,'// &
                 'Standing_Foliage real null,'// &
                 'Standing_Live_lt3 real null,'// &
                 'Standing_Live_ge3 real null,'// &
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

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Fuels (CaseID,', &
     'StandID,Year,Surface_Litter,Surface_Duff,Surface_lt3,', &
     'Surface_ge3,Surface_3to6,Surface_6to12,Surface_ge12,', &
     'Surface_Herb,Surface_Shrub,Surface_Total,Standing_Snag_lt3,', &
     'Standing_Snag_ge3,Standing_Foliage,Standing_Live_lt3,', &
     'Standing_Live_ge3,Standing_Total,Total_Biomass,', &
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



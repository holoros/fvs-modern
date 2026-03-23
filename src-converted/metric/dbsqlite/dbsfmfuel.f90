SUBROUTINE DBSFMFUEL(IYEAR,NPLT,MSE,LITTER,DUFF,CLT3,CGT3, &
     C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3, &
     PERTRCR,SM25,SM10,KODE)
IMPLICIT NONE
!----------
! METRIC-DBSQLITE $Id: dbsfmfuel.f 2477 2018-08-30 15:16:07Z lancedavid $
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS CONSUMPTION REPORT
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
!     INPUT:
!              THE FUEL CONSUMPTION OUTPUT FROM THE FIRE MODEL.
!              1: MINERAL SOIL EXPOSURE
!              2: LITTER CONSUMPTION
!              3: DUFF CONSUMPTION
!              4: CONSUMPTION 0 - 7.6cm
!              5: CONSUMPTION >= 7.6cm
!              6: CONSUMPTION 7.6 - 15.2cm
!              7: CONSUMPTION 15.2 - 30.6cm
!              8: CONSUMPTION > 30.5cm
!              9: HERB / SHRUB CONSUMPTION
!             10: CROWN CONSUMPTION
!             11: TOTAL CONSUMPTION
!             12: % CONSUMPTION DUFF
!             13: % CONSUMPTION >= 7.6cm
!             14: % TREES WITH CROWNING
!             15: SMOKE PRODUCTION < 2.5 micrometers
!             16: SMOKE PRODUCTION < 10  micrometers
!             17: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
!
!     ******************************************************************
!     NOTE: The variables written out in this METRIC version are already
!     computed in metric units by the calling subroutine FMFOUT. So
!     there is no need to convert them here. Some label names are
!     tweaked: e.g., TPA -> TPH for "Trees/acre" to "Trees/hectare"
!     ******************************************************************
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

INTEGER IYEAR,iRet,KODE,PERTRCR,ColNumber
REAL MSE,LITTER,DUFF,CLT3,CGT3,C3TO6,C6TO12,CGT12,HERB,CROWN, &
        CTOTAL,PERCDUFF,PERCGT3,SM25,SM10
DOUBLE PRECISION MSEB,LITTERB,DUFFB,CLT3B,CGT3B,C3TO6B,C6TO12B, &
        CGT12B,HERBB,CROWNB,CTOTALB,PERCDUFFB,PERCGT3B, &
        SM25B,SM10B
CHARACTER*2000 SQLStmtStr
CHARACTER(len=26) NPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

IF(IFUELC.EQ.0) RETURN
IF(IFUELC.EQ.2) KODE = 0

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_Consumption_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
   SQLStmtStr='CREATE TABLE FVS_Consumption_Metric('// &
          'CaseID text not null,'// &
          'StandID text not null,'// &
          'Year Int null,'// &
          'Min_Soil_Exp real null,'// &
          'Litter_Consumption real null,'// &
          'Duff_Consumption real null,'// &
          'Consumption_lt76 real null,'// &
          'Consumption_ge76 real null,'// &
          'Consumption_76to152 real null,'// &
          'Consumption_152to305 real null,'// &
          'Consumption_ge305 real null,'// &
          'Consumption_Herb_Shrub real null,'// &
          'Consumption_Crowns real null,'// &
          'Total_Consumption real null,'// &
          'Percent_Consumption_Duff real null,'// &
          'Percent_Consumption_ge76 real null,'// &
          'Percent_Trees_Crowning int null,'// &
          'Smoke_Production_25 real null,'// &
          'Smoke_Production_10 real null);'//CHAR(0)
  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    IFUELC = 0
    RETURN
  ENDIF
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
MSEB=MSE
LITTERB=LITTER
CLT3B=CLT3
DUFFB=DUFF
CGT3B=CGT3
C3TO6B=C3TO6
C6TO12B=C6TO12
CGT12B=CGT12
HERBB=HERB
CROWNB=CROWN
CTOTALB=CTOTAL
PERCDUFFB=PERCDUFF
PERCGT3B=PERCGT3
SM25B=SM25
SM10B=SM10

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Consumption_Metric (CaseID,', &
     'StandID,Year,Min_Soil_Exp,Litter_Consumption,', &
     'Duff_Consumption,', &
     'Consumption_lt76,Consumption_ge76,Consumption_76to152,', &
     'Consumption_152to305,Consumption_ge305,', &
     'Consumption_Herb_Shrub,', &
     'Consumption_Crowns,Total_Consumption,', &
     'Percent_Consumption_Duff,', &
     'Percent_Consumption_ge76,Percent_Trees_Crowning,', &
     'Smoke_Production_25,Smoke_Production_10) VALUES (''',CASEID, &
     ''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'

iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
!
!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
!
ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,MSEB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,LITTERB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,CLT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,C3TO6B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,C6TO12B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT12B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HERBB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,CROWNB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,CTOTALB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCDUFFB)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCGT3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,PERTRCR)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SM25B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SM10B)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IFUELC = 0
ENDIF
RETURN
END


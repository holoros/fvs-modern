SUBROUTINE DBSMIS1(IYEAR,CNPLT,CSP, &
     SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4, &
     KODE)

IMPLICIT NONE
!
! DBSQLITE-METRIC $Id: dbsmis.f 2477 2018-08-30 15:16:07Z lancedavid $
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
!
!     1:     Year
!     2:     Stand ID
!     3:     Species
!     4:     Mean DMR for species - SPDMR4
!     5:     Mean DMI for species - SPDMI4
!     6:     Infected trees/ha for species    - SPINF4
!     7:     Mortality trees/ha for species   - SPMRT4
!     8:     Infected trees/ha % for species  - SPPIN4
!     9:     Mortality trees/ha % for species - SPPMR4
!    10:     Stand trees/ha % for species     - SPPOC4
!
!     ******************************************************************
!     NOTE: The variables written out in this METRIC version are already
!     computed in metric units by the calling subroutine MISPRT. So
!     there is no need to convert them here. Some label names are
!     tweaked: e.g., TPA -> TPH for "Trees/acre" to "Trees/hectare"
!     ******************************************************************
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
!
INCLUDE 'PLOT.f90'
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

!     ARGUMENT LIST

INTEGER IYEAR
REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
INTEGER KODE,iRet

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset,fsql3_bind_text

INTEGER ColNumber,I,I2

DOUBLE PRECISION  SPDMR4B, SPDMI4B
INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

CHARACTER*2000    SQLStmtStr
CHARACTER(LEN=26) CNPLT
CHARACTER(LEN=2)  CSP(4)
CHARACTER(LEN=8)  CSP1,CSP2,CSP3

!     INITIALIZE VARIABLES

IF(IDM1.EQ.0) RETURN
IF(IDM1.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_DM_Spp_Sum_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year Int null,'// &
         'SpeciesFVS text null,'// &
         'SpeciesPLANTS text null,'// &
         'SpeciesFIA text null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null,'// &
         'Inf_TPH int null,'// &
         'Mort_TPH int  null,'// &
         'Inf_TPH_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Stnd_TPH_Pct int null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IDM1 = 0
     RETURN
   ENDIF
ENDIF

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_Spp_Sum_Metric', &
     ' (CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
     'Mean_DMR,Mean_DMI,Inf_TPH,', &
     'Mort_TPH,Inf_TPH_Pct,Mort_TPH_Pct,Stnd_TPH_Pct) ', &
     ' VALUES (''',CASEID,''',''',TRIM(CNPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?);'
iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IDM1 = 0
   RETURN
ENDIF

!     LOOP OVER 4 TOP SPECIES

DO I = 1,4

!       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
!       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE.

  IF (CSP(I) .EQ. '**') CYCLE

!       ASSIGN FVS, PLANTS AND FIA SPECIES CODES

  DO I2 = 1,MAXSP
    IF (CSP(I) .EQ. JSP(I2)) THEN
      CSP1 = JSP(I2)
      CSP2 = PLNJSP(I2)
      CSP3 = FIAJSP(I2)
    ENDIF
  ENDDO

!       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
!       INTEGER COPIES OF REAL VECTOR INPUTS

  SPDMR4B = SPDMR4(I)
  SPDMI4B = SPDMI4(I)

  ISPINF4 = NINT(SPINF4(I))
  ISPMRT4 = NINT(SPMRT4(I))
  ISPPIN4 = NINT(SPPIN4(I))
  ISPPMR4 = NINT(SPPMR4(I))
  ISPPOC4 = NINT(SPPOC4(I))


!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 YEAR
  iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber=ColNumber+1       ! SpeciesFVS
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                       LEN_TRIM(CSP1))

  ColNumber=ColNumber+1       ! SpeciesPLANTS
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                       LEN_TRIM(CSP2))

  ColNumber=ColNumber+1       ! SpeciesFIA
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                       LEN_TRIM(CSP3))

  ColNumber=ColNumber+1       ! MEAN DMR
  iRet=fsql3_bind_double(IoutDBref,ColNumber,SPDMR4B)

  ColNumber=ColNumber+1       ! MEAN DMI
  iRet=fsql3_bind_double(IoutDBref,ColNumber,SPDMI4B)

  ColNumber=ColNumber+1       ! TPA INFECTED
  iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPINF4)

  ColNumber=ColNumber+1       ! TPA MORTALITY
  iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPMRT4)

  ColNumber=ColNumber+1       ! % TPA INFECTED
  iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPIN4)

  ColNumber=ColNumber+1       ! % TPA MORTALITY
  iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPMR4)

  ColNumber=ColNumber+1       ! % STAND TPA
  iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPOC4)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)
ENDDO

iRet = fsql3_finalize(IoutDBref)
iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))

if (iRet.ne.0) then
   IDM1 = 0
   RETURN
ENDIF

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE, &
     ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM, &
     ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)

IMPLICIT NONE
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

!     ******************************************************************
!     NOTE: The variables written out in this METRIC version are already
!     computed in metric units by the calling subroutine MISPRT. So
!     there is no need to convert them here. Some label names are
!     tweaked: e.g., TPA -> TPH for "Trees/acre" to "Trees/hectare"
!     ******************************************************************

!     1:     Year - IYEAR
!     2:     Age - NAGE
!     3:     Stand trees/acre - ISTTPAT
!     4:     Stand basal area - IBA
!     5:     Stand volume - ISTVOL
!     6:     Infected trees/ha - ISTTPAI
!     7:     Infected basal area - ISTBAI
!     8:     Infected volume - ISTVOLI
!     9:     Mortality trees/ha - ISTTPAM
!     10:    Mortality basal area - ISTBAM
!     11:    Mortality volume - ISTVOLM
!     12:    Infected trees/ha % - ISTPIT
!     13:    Infected volume % - ISTPIV
!     14:    Mortality trees/ha % - ISTPMT
!     15:    Mortality volume % - ISTPMV
!     16:    Mean DMR - STDMR
!     17:    Mean DMI - STDMI

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

!     ARGUMENT LIST

INTEGER IYEAR,NAGE
CHARACTER(LEN=26) NPLT
INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
REAL    STDMR,STDMI
INTEGER KODE,iRet

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

!     LOCAL VARIABLES

INTEGER ColNumber

DOUBLE PRECISION  STDMRB, STDMIB

CHARACTER*1000    SQLStmtStr

!     INITIALIZE VARIABLES

IF(IDM2.EQ.0) RETURN
IF(IDM2.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_DM_Stnd_Sum_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year int null,'// &
         'Age int null,'// &
         'Stnd_TPH int null,'// &
         'Stnd_BA int null,'// &
         'Stnd_Vol int null,'// &
         'Inf_TPH int null,'// &
         'Inf_BA int null,'// &
         'Inf_Vol int null,'// &
         'Mort_TPH int null,'// &
         'Mort_BA int null,'// &
         'Mort_Vol int null,'// &
         'Inf_TPH_Pct int null,'// &
         'Inf_Vol_Pct int null,'// &
         'Mort_TPH_Pct int null,'// &
         'Mort_Vol_Pct int null,'// &
         'Mean_DMR real null,'// &
         'Mean_DMI real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IDM2 = 0
     RETURN
   ENDIF
ENDIF

!     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

STDMRB = STDMR
STDMIB = STDMI

WRITE(SQLStmtStr,*)'INSERT INTO FVS_DM_Stnd_Sum_Metric(CaseID,', &
     'StandID,Year,Age,Stnd_TPH,Stnd_BA,Stnd_Vol,Inf_TPH,Inf_BA,', &
     'Inf_Vol,Mort_TPH,Mort_BA,Mort_Vol,Inf_TPH_Pct,Inf_Vol_Pct,', &
     'Mort_TPH_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)', &
     'VALUES (''',CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'

iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IDM2 = 0
   RETURN
ENDIF

!     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

ColNumber=1                 ! YEAR
iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1       ! AGE
iRet=fsql3_bind_int(IoutDBref,ColNumber,NAGE)

ColNumber=ColNumber+1       ! STAND TPA
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAT)

ColNumber=ColNumber+1       ! STAND BA
iRet=fsql3_bind_int(IoutDBref,ColNumber,IBA)

ColNumber=ColNumber+1       ! STAND VOL
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOL)

ColNumber=ColNumber+1       ! INF TPA
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAI)

ColNumber=ColNumber+1       ! INF BA
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTBAI)

ColNumber=ColNumber+1       ! INF VOL
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOLI)

ColNumber=ColNumber+1       ! MORT TPA
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAM)

ColNumber=ColNumber+1       ! MORT BA
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTBAM)

ColNumber=ColNumber+1       ! MORT VOL
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOLM)

ColNumber=ColNumber+1       ! INF TPA %
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPIT)

ColNumber=ColNumber+1       ! INF VOL %
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPIV)

ColNumber=ColNumber+1       ! 16 MORT TPA %
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPMT)

ColNumber=ColNumber+1       ! MORT VOL %
iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPMV)

ColNumber=ColNumber+1       ! MEAN DMR
iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMRB)

ColNumber=ColNumber+1       ! MEAN DMI
iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMIB)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))

if (iRet.ne.0) then
   IDM2 = 0
ENDIF

RETURN
END
!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS, &
     DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)

IMPLICIT NONE

!     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
!     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

!     ******************************************************************
!     NOTE: The variables written out in this METRIC version are already
!     computed in metric units by the calling subroutine MISPRT. So
!     there is no need to convert them here. Some label names are
!     tweaked: e.g., TPA -> TPH for "Trees/acre" to "Trees/hectare"
!     ******************************************************************

!     1:     Year - IYEAR
!     2:     Age - NAGE
!     3:     Stand trees/ha - ISTTPAT
!     4:     Stand basal area - IBA
!     5:     Stand volume - ISTVOL
!     6:     Infected trees/ha - ISTTPAI
!     7:     Infected basal area - ISTBAI
!     8:     Infected volume - ISTVOLI
!     9:     Mortality trees/ha - ISTTPAM
!     10:    Mortality basal area - ISTBAM
!     11:    Mortality volume - ISTVOLM
!     12:    Infected trees/ha % - ISTPIT
!     13:    Infected volume % - ISTPIV
!     14:    Mortality trees/ha % - ISTPMT
!     15:    Mortality volume % - ISTPMV
!     16:    Mean DMR - STDMR
!     17:    Mean DMI - STDMI

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

!     ARGUMENT LIST

INTEGER IYEAR
CHARACTER(LEN=26) NPLT
CHARACTER(LEN=3)  NLABS(5)
REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
INTEGER KODE,iRet

!     LOCAL VARIABLES

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset,fsql3_bind_text

INTEGER ColNumber,I,J

DOUBLE PRECISION  X(10)

CHARACTER*1000    SQLStmtStr

!     INITIALIZE VARIABLES

IF(IDM3.EQ.0) RETURN
IF(IDM3.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_DM_Sz_Sum_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year int null,'// &
         'Type text null,'// &
         char(39)//'0-5cm'   //char(39)//' real null,'// &
         char(39)//'5-10cm'  //char(39)//' real null,'// &
         char(39)//'10-15cm' //char(39)//' real null,'// &
         char(39)//'15-20cm' //char(39)//' real null,'// &
         char(39)//'20-25cm' //char(39)//' real null,'// &
         char(39)//'25-30cm' //char(39)//' real null,'// &
         char(39)//'30-35cm' //char(39)//' real null,'// &
         char(39)//'35-40cm' //char(39)//' real null,'// &
         char(39)//'40-45cm' //char(39)//' real null,'// &
         char(39)//'gt45cm'  //char(39)//' real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IDM3 = 0
     RETURN
   ENDIF
ENDIF

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_Sz_Sum_Metric', &
       ' (CaseID,StandID,Year,Type,', &
       char(39),'0-5cm',   char(39),',', &
       char(39),'5-10cm',  char(39),',', &
       char(39),'10-15cm', char(39),',', &
       char(39),'15-20cm', char(39),',', &
       char(39),'20-25cm', char(39),',', &
       char(39),'25-30cm', char(39),',', &
       char(39),'30-35cm', char(39),',', &
       char(39),'35-40cm', char(39),',', &
       char(39),'40-45cm', char(39),',', &
       char(39),'gt45cm',  char(39),') VALUES ', &
       '(''',CASEID,''',''',TRIM(NPLT), &
       ''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IDM3 = 0
   RETURN
ENDIF

!     LOOP OVER 5 TYPES

DO I = 1,5

!     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

  SELECT CASE (I)
    CASE (1)
      DO J = 1,10
        X(J) = DCTPA(J)
      ENDDO
    CASE (2)
      DO J = 1,10
        X(J) = DCINF(J)
      ENDDO
    CASE (3)
      DO J = 1,10
        X(J) = DCMRT(J)
      ENDDO
    CASE (4)
      DO J = 1,10
        X(J) = DCDMR(J)
      ENDDO
    CASE (5)
      DO J = 1,10
        X(J) = DCDMI(J)
      ENDDO
  END SELECT

  iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

!       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber=1                 ! 1 YEAR
  iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber=ColNumber+1       ! Label
  iRet = fsql3_bind_text(IoutDBref,ColNumber,NLABS(I), &
            len_trim(NLABS(I)))

  DO J = 1,10
    ColNumber=ColNumber+1     ! SIZE CLASSES 2-11
    iRet=fsql3_bind_double(IoutDBref,ColNumber,X(J))
  ENDDO

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)

ENDDO

iRet = fsql3_finalize(IoutDBref)
iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))

if (iRet.ne.0) then
   IDM3 = 0
ENDIF

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS5(IYEAR,KODE)
IMPLICIT NONE

!----------
!     PURPOSE: TO OUTPUT THE TREELIST DM DATA TO THE DATABASE
!
!     AUTH: D. ROBINSON, WITH INSPIRATION FROM
!           D. GAMMEL -- SEM -- JULY 2002
!
!            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
!                     REDIRECT OF THE FLAT FILE REPORT OR IN
!                     ADDITION TO
!
!----------

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'ESTREE.f90'
INCLUDE 'VARCOM.f90'
INCLUDE 'WORKCM.f90'
INCLUDE 'MISCOM.f90'
INCLUDE 'DMCOM.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'METRIC.f90'
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

INTEGER IYEAR, KODE, iRet

!     LOCAL VARIABLES

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset,fsql3_bind_text

INTEGER ColNumber,I,J,K,L,I1,I2,I3,ISPC
INTEGER IDCMP1,IDCMP2
DATA    IDCMP1,IDCMP2/10000000,20000000/

DOUBLE PRECISION PB,DBHB,HTB,CWB
DOUBLE PRECISION BRKPNTB(BPCNT),NEWSPRB(3),NEWINTB(3)
DOUBLE PRECISION DMINFB(CRTHRD,DEAD_BC),DMINFB_BC(CRTHRD,ACTIVE)

CHARACTER*8      TID,CSPECIE1
CHARACTER*1000   SQLStmtStr

!     INITIALIZE VARIABLES

IF(IDM5.EQ.0) RETURN
IF(IDM5.EQ.2) KODE = 0

!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

CALL DBSCASE(1)

iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_DM_TreeList_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_DM_TreeList_Metric ('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year int null,'// &
         'TreeID text null,'// &
         'TreeIndex int null,'// &
         'SpeciesFVS text null,'// &
         'TPH real null,'// &
         'DBH real null,'// &
         'Ht real null,'// &
         'CrWidth real null,'// &
         'BrkPnt1 real null,'// &
         'BrkPnt2 real null,'// &
         'BrkPnt3 real null,'// &
         'BrkPnt4 real null,'// &
         'DMR int null,'// &
         'NewSpr1 real null,'// &
         'NewSpr2 real null,'// &
         'NewSpr3 real null,'// &
         'NewInt1 real null,'// &
         'NewInt2 real null,'// &
         'NewInt3 real null,'// &
         'Immtr1 real null,'// &
         'Immtr2 real null,'// &
         'Immtr3 real null,'// &
         'Latnt1 real null,'// &
         'Latnt2 real null,'// &
         'Latnt3 real null,'// &
         'Suppr1 real null,'// &
         'Suppr2 real null,'// &
         'Suppr3 real null,'// &
         'Actv1 real null,'// &
         'Actv2 real null,'// &
         'Actv3 real null,'// &
         'BCImmtr1 real null,'// &
         'BCImmtr2 real null,'// &
         'BCImmtr3 real null,'// &
         'BCLatnt1 real null,'// &
         'BCLatnt2 real null,'// &
         'BCLatnt3 real null,'// &
         'BCSuppr1 real null,'// &
         'BCSuppr2 real null,'// &
         'BCSuppr3 real null,'// &
         'BCActv1 real null,'// &
         'BCActv2 real null,'// &
         'BCActv3 real null,'// &
         'BCKill1 real null,'// &
         'BCKill2 real null,'// &
         'BCKill3 real null);'//CHAR(0)

   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IDM5 = 0
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     RETURN
   ENDIF
ENDIF

WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_TreeList_Metric ', &
     '(CaseID,StandID,Year,', &
     'TreeID,TreeIndex,SpeciesFVS,TPH,DBH,Ht,CrWidth,', &
     'BrkPnt1,BrkPnt2,BrkPnt3,BrkPnt4,', &
     'DMR,', &
     'NewSpr1,NewSpr2,NewSpr3,', &
     'NewInt1,NewInt2,NewInt3,', &
     'Immtr1,Immtr2,Immtr3,', &
     'Latnt1,Latnt2,Latnt3,', &
     'Suppr1,Suppr2,Suppr3,', &
     'Actv1,Actv2,Actv3,', &
     'BCImmtr1,BCImmtr2,BCImmtr3,', &
     'BCLatnt1,BCLatnt2,BCLatnt3,', &
     'BCSuppr1,BCSuppr2,BCSuppr3,', &
     'BCActv1,BCActv2,BCActv3,', &
     'BCKill1,BCKill2,BCKill3) ', &
     'VALUES (''',CASEID,''',''',TRIM(NPLT),''',', &
     '?,?,?,?,', &
     '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,', &
     '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,', &
     '?,?,?,?,?,?,?,?,?,?,?,?);'

iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
 IDM5 = 0
 iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
 RETURN
ENDIF

DO ISPC = 1,MAXSP
  I1 = ISCT(ISPC,1)
  IF (MISFIT(ISPC) .NE. 0 .AND. I1 .NE. 0) THEN
    I2 = ISCT(ISPC,2)
    DO I3 = I1,I2
      I = IND1(I3)

      PB = (PROB(I) / GROSPC)/ACRtoHA

!           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN
!           COMPRESSED OR GENERATED THROUGH THE ESTAB SYSTEM.

      IF (IDTREE(I) .GT. IDCMP1) THEN
        IF (IDTREE(I) .GT. IDCMP2) THEN
          WRITE(TID,'(''CM'',I6.6)') IDTREE(I) - IDCMP2
        ELSE
          WRITE(TID,'(''ES'',I6.6)') IDTREE(I) - IDCMP1
        ENDIF
      ELSE
        WRITE(TID,'(I8)') IDTREE(I)
      ENDIF

!           DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS
!           CONVERT BREAKPOINTS FROM MESH-UNITS TO METERS

      DO J = 1,BPCNT
        BRKPNTB(J) = BRKPNT(I,J) * REAL(MESH)
      ENDDO
      DO J = 1,CRTHRD
        NEWSPRB(J) = NEWSPR(I,J)
        NEWINTB(J) = NEWINT(I,J)
      ENDDO

      DO J = 1,CRTHRD
        DO K = 1,DEAD_BC
          DMINFB(J,K) = DMINF(I,J,K)
        ENDDO
        DO K = 1,ACTIVE
          DMINFB_BC(J,K) = 0.0
          DO L = 1,MAXBC
            DMINFB_BC(J,K) = DMINFB_BC(J,K) + DMINF_BC(I,J,K,L)
          ENDDO
        ENDDO
      ENDDO

      iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

!           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

      CSPECIE1 = JSP(ISP(I))

!           BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! Year
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1       ! TreeID
      iRet = fsql3_bind_text(IoutDBref,ColNumber,TID, &
           LEN_TRIM(TID))

      ColNumber=ColNumber+1       ! TreeIndex
      iRet=fsql3_bind_int(IoutDBref,ColNumber,I)

      ColNumber=ColNumber+1       ! FVSSpecies
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1, &
           LEN_TRIM(CSPECIE1))

      ColNumber=ColNumber+1       ! TPH
      iRet = fsql3_bind_double(IoutDBref,ColNumber,PB)

      ColNumber=ColNumber+1       ! DBH
      DBHB=DBH(I)*INtoCM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DBHB)

      ColNumber=ColNumber+1       ! Ht
      HTB=HT(I)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HTB)

      ColNumber=ColNumber+1       ! CrWidth
      CWB=CRWDTH(I)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CWB)

      DO J = 1,BPCNT
        ColNumber=ColNumber+1     ! BRKPNT
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BRKPNTB(J))
      ENDDO

      ColNumber=ColNumber+1       ! DMR
      iRet=fsql3_bind_int(IoutDBref,ColNumber,DMRATE(I))

      DO J = 1,CRTHRD
         ColNumber=ColNumber+1     ! NEWSPR
         iRet = fsql3_bind_double(IoutDBref,ColNumber,NEWSPRB(J))
      ENDDO

      DO J = 1,CRTHRD
         ColNumber=ColNumber+1     ! NEWINT
         iRet = fsql3_bind_double(IoutDBref,ColNumber,NEWINTB(J))
      ENDDO

      DO K = 1,ACTIVE
        DO J = 1,CRTHRD
          ColNumber=ColNumber+1    ! DMINF
          iRet = fsql3_bind_double(IoutDBref,ColNumber, &
               DMINFB(J,K))
        ENDDO
      ENDDO

      DO K = 1,ACTIVE
        DO J = 1,CRTHRD
          ColNumber=ColNumber+1    ! DMINF_BC
          iRet = fsql3_bind_double(IoutDBref,ColNumber, &
               DMINFB_BC(J,K))
        ENDDO
      ENDDO

      K = DEAD_BC
      DO J = 1,CRTHRD
        ColNumber=ColNumber+1      ! DMINF-DEAD
        iRet = fsql3_bind_double(IoutDBref,ColNumber, &
             DMINFB_BC(J,K))
      ENDDO

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_reset(IoutDBref)
    ENDDO
  ENDIF
ENDDO

iRet = fsql3_finalize(IoutDBref)
iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

if (iRet.ne.0) then
   IDM5 = 0
ENDIF

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE DBSMIS6(IYEAR,KODE)
IMPLICIT NONE

INCLUDE 'DBSCOM.f90'

INTEGER IYEAR, KODE

IDM6 = 0
!
!     code for 6 is here:
!     https://sourceforge.net/p/open-fvs/code/HEAD/tree/branches/ESSAdev/newmist/src/dbsmis.f
!
RETURN
END

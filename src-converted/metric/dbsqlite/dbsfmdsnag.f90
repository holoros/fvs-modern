SUBROUTINE DBSFMDSNAG(IYEAR,SDBH,SHTH,SHTS,SVLH,SVLS, &
     SDH,SDS,YRLAST,KODE)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE DETAILED SNAG REPORT
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
!     INPUT:
!              THE DETAILED SNAG OUTPUT FROM THE FIRE MODEL.
!              1: AVERAGE SNAG DIAMETER FOR THE RECORD
!              2: CURRENT HEIGHT OF HARD SNAGS
!              3: CURRENT HEIGHT OF SOFT SNAGS
!              4: CURRENT VOLUME OF HARD SNAGS
!              5: CURRENT VOLUME OF SOFT SNAGS
!              6: DENSITY OF HARD SNAGS
!              7: DENSITY OF SOFT SNAGS
!              8: YRLAST
!              9: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'DBSCOM.f90'
!
!
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

INTEGER IYEAR,iRet,KODE,YRDEAD,YRLAST,JYR,IDC,JCL
INTEGER ColNumber
REAL SDBH, SHTH, SHTS, SDH, SDS, SVLH, SVLS
REAL*8 SDBHB, SHTHB, SHTSB, SDHB, SDSB, SDTB, SVLHB, SVLSB, SVLTB
DIMENSION SVLH(MAXSP,100,6), SVLS(MAXSP,100,6), &
     SDBH(MAXSP,100,6), SHTH(MAXSP,100,6),SHTS(MAXSP,100,6), &
     SDH(MAXSP,100,6), SDS(MAXSP,100,6)
CHARACTER*2000 SQLStmtStr
CHARACTER(LEN=8) CSP1,CSP2,CSP3

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset,fsql3_bind_text

IF(ISDET.EQ.0) RETURN
IF(ISDET.EQ.2) KODE = 0

CALL DBSCASE(1)

iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_SnagDet_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_SnagDet_Metric ('// &
             'CaseID text not null,'// &
             'StandID text not null,'// &
             'Year Int null,'// &
             'SpeciesFVS    text null,'// &
             'SpeciesPLANTS text null,'// &
             'SpeciesFIA    text null,'// &
             'DBH_Class int null,'// &
             'Death_DBH real null,'// &
             'Current_Ht_Hard real null,'// &
             'Current_Ht_Soft real null,'// &
             'Current_Vol_Hard real null,'// &
             'Current_Vol_Soft real null,'// &
             'Total_Volume real null,'// &
             'Year_Died int null,'// &
             'Density_Hard real null,'// &
             'Density_Soft real null,'// &
             'Density_Total real null);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ISDET = 0
    RETURN
  ENDIF
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_SnagDet_Metric ', &
     '(CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
     'DBH_Class,Death_DBH,', &
     'Current_Ht_Hard,Current_Ht_Soft,Current_Vol_Hard,', &
     'Current_Vol_Soft,Total_Volume,Year_Died,Density_Hard,', &
     'Density_Soft,Density_Total) VALUES (''', &
     CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
IF (iRet .NE. 0) THEN
  ISDET = 0
  RETURN
ENDIF

DO JYR= 1,YRLAST
   DO IDC= 1,MAXSP
      DO JCL= 1,6

        SDTB   = SDH(IDC,JYR,JCL) + SDS(IDC,JYR,JCL)
        IF (SDTB .LE. 0) CYCLE
        YRDEAD = IYEAR - JYR + 1
        SDBHB = SDBH(IDC,JYR,JCL)
        SHTHB = SHTH(IDC,JYR,JCL)
        SHTSB = SHTS(IDC,JYR,JCL)
        SDHB  = SDH (IDC,JYR,JCL)
        SDSB  = SDS (IDC,JYR,JCL)
        SVLHB = SVLH(IDC,JYR,JCL)
        SVLSB = SVLS(IDC,JYR,JCL)
        SVLTB = SVLHB + SVLSB

!             ASSIGN FVS, PLANTS AND FIA SPECIES CODES

        CSP1 = JSP(IDC)
        CSP2 = PLNJSP(IDC)
        CSP3 = FIAJSP(IDC)

        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                             len_trim(CSP1))

        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                             len_trim(CSP2))

        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                             len_trim(CSP3))

        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,JCL)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SDBHB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SHTHB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SHTSB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLHB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLSB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLTB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,YRDEAD)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SDHB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SDSB)

        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,SDTB)

        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref)

      ENDDO
   ENDDO
ENDDO
iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ISDET = 0
ENDIF

RETURN

END



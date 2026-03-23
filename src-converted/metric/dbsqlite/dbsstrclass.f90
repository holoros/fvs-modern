SUBROUTINE DBSSTRCLASS(IYEAR,CNPLT,RCODE,S1DBH,S1NHT,S1LHT,S1SHT, &
     S1CB,S1CC,S1MS1,S1MS2,S1SC,S2DBH,S2NHT,S2LHT,S2SHT,S2CB,S2CC, &
     S2MS1,S2MS2,S2SC,S3DBH,S3NHT,S3LHT,S3SHT,S3CB,S3CC,S3MS1,S3MS2, &
     S3SC,NS,TOTCOV,SCLASS,KODE,NTREES)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE STRUCTURE CLASS OUTPUT.
!     AUTH: S. REBAIN -- FMSC -- AUGUST 2005
!     INPUT:
!            THE STRUCTURE CLASS OUTPUT.
!              1: REMOVAL CODE
!              2: STRATUM 1 DBH
!              3: STRATUM 1 NOMINAL HEIGHT
!              4: STRATUM 1 LARGEST HEIGHT
!              5: STRATUM 1 SMALLEST HEIGHT
!              6: STRATUM 1 CROWN BASE
!              7: STRATUM 1 CROWN COVER
!              8: STRATUM 1 MAJOR SPECIES 1
!              9: STRATUM 1 MAJOR SPECIES 2
!             10: STRATUM 1 STATUS CODE
!             11: STRATUM 2 DBH
!             12: STRATUM 2 NOMINAL HEIGHT
!             13: STRATUM 2 LARGEST HEIGHT
!             14: STRATUM 2 SMALLEST HEIGHT
!             15: STRATUM 2 CROWN BASE
!             16: STRATUM 2 CROWN COVER
!             17: STRATUM 2 MAJOR SPECIES 1
!             18: STRATUM 2 MAJOR SPECIES 2
!             19: STRATUM 2 STATUS CODE
!             20: STRATUM 3 DBH
!             21: STRATUM 3 NOMINAL HEIGHT
!             22: STRATUM 3 LARGEST HEIGHT
!             23: STRATUM 3 SMALLEST HEIGHT
!             24: STRATUM 3 CROWN BASE
!             25: STRATUM 3 CROWN COVER
!             26: STRATUM 3 MAJOR SPECIES 1
!             27: STRATUM 3 MAJOR SPECIES 2
!             28: STRATUM 3 STATUS CODE
!             29: NUMBER OF STRATA
!             30: TOTAL COVER
!             31: STRUCTURE CLASS
!             32: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE
!             33: THE NUMBER OF TREE RECORDS
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'

INCLUDE 'PLOT.f90'

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
!

INTEGER I,IYEAR,KODE,RCODE,S1NHT,S1LHT,S1SHT,S1CB,S1CC,S1SC
INTEGER S2NHT,S2LHT,S2SHT,S2CB,S2CC,S3NHT,S3LHT,S3SHT,S3CB,S3CC
INTEGER S2SC, S3SC, TOTCOV,NS, NTREES
INTEGER ColNumber,iRet
REAL S1DBH,S2DBH,S3DBH
DOUBLE PRECISION BS1DBH,BS2DBH,BS3DBH
CHARACTER*2000 SQLStmtStr
CHARACTER*3 S1MS1,S1MS2,S2MS1,S2MS2,S3MS1,S3MS2
CHARACTER*8 S1CSP11,S1CSP12,S1CSP13,S1CSP21,S1CSP22,S1CSP23, &
               S2CSP11,S2CSP12,S2CSP13,S2CSP21,S2CSP22,S2CSP23, &
               S3CSP11,S3CSP12,S3CSP13,S3CSP21,S3CSP22,S3CSP23
CHARACTER*4 SCLASS
CHARACTER(len=26) CNPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

IF (ISTRCLAS .EQ. 0) RETURN
IF (ISTRCLAS .EQ. 2) KODE = 0
IF (NTREES .EQ. 0) RETURN

CALL DBSCASE(1)
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_StrClass_Metric"//CHAR(0))
IF(iRet.EQ.0) THEN
  SQLStmtStr='CREATE TABLE FVS_StrClass_Metric ('// &
       'CaseID text not null,'// &
       'StandID text not null,'// &
       'Year int null,'// &
       'Removal_Code int null,'// &
       'Stratum_1_DBH real null,'// &
       'Stratum_1_Nom_Ht int null,'// &
       'Stratum_1_Lg_Ht int null,'// &
       'Stratum_1_Sm_Ht int null,'// &
       'Stratum_1_Crown_Base int null,'// &
       'Stratum_1_Crown_Cover int null,'// &
       'Stratum_1_SpeciesFVS_1    text null,'// &
       'Stratum_1_SpeciesFVS_2    text null,'// &
       'Stratum_1_SpeciesPLANTS_1 text null,'// &
       'Stratum_1_SpeciesPLANTS_2 text null,'// &
       'Stratum_1_SpeciesFIA_1    text null,'// &
       'Stratum_1_SpeciesFIA_2    text null,'// &
       'Stratum_1_Status_Code int null,'// &
       'Stratum_2_DBH real null,'// &
       'Stratum_2_Nom_Ht int null,'// &
       'Stratum_2_Lg_Ht int null,'// &
       'Stratum_2_Sm_Ht int null,'// &
       'Stratum_2_Crown_Base int null,'// &
       'Stratum_2_Crown_Cover int null,'// &
       'Stratum_2_SpeciesFVS_1    text null,'// &
       'Stratum_2_SpeciesFVS_2    text null,'// &
       'Stratum_2_SpeciesPLANTS_1 text null,'// &
       'Stratum_2_SpeciesPLANTS_2 text null,'// &
       'Stratum_2_SpeciesFIA_1    text null,'// &
       'Stratum_2_SpeciesFIA_2    text null,'// &
       'Stratum_2_Status_Code int null,'// &
       'Stratum_3_DBH real null,'// &
       'Stratum_3_Nom_Ht int null,'// &
       'Stratum_3_Lg_Ht int null,'// &
       'Stratum_3_Sm_Ht int null,'// &
       'Stratum_3_Crown_Base int null,'// &
       'Stratum_3_Crown_Cover int null,'// &
       'Stratum_3_SpeciesFVS_1    text null,'// &
       'Stratum_3_SpeciesFVS_2    text null,'// &
       'Stratum_3_SpeciesPLANTS_1 text null,'// &
       'Stratum_3_SpeciesPLANTS_2 text null,'// &
       'Stratum_3_SpeciesFIA_1    text null,'// &
       'Stratum_3_SpeciesFIA_2    text null,'// &
       'Stratum_3_Status_Code int null,'// &
       'Number_of_Strata int null,'// &
       'Total_Cover int null,'// &
       'Structure_Class text null);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ISTRCLAS = 0
    RETURN
  ENDIF
ENDIF
!
!     INITIALIZE AND ASSIGN FVS, PLANTS AND FIA SPECIES CODES
!
S1CSP11 = '--'
S1CSP12 = '--'
S1CSP13 = '--'
S1CSP21 = '--'
S1CSP22 = '--'
S1CSP23 = '--'
S2CSP11 = '--'
S2CSP12 = '--'
S2CSP13 = '--'
S2CSP21 = '--'
S2CSP22 = '--'
S2CSP23 = '--'
S3CSP11 = '--'
S3CSP12 = '--'
S3CSP13 = '--'
S3CSP21 = '--'
S3CSP22 = '--'
S3CSP23 = '--'

DO I = 1,MAXSP
  IF (S1MS1(1:2) .EQ. JSP(I)) THEN
    S1CSP11 = JSP(I)
    S1CSP12 = PLNJSP(I)
    S1CSP13 = FIAJSP(I)
  ENDIF
  IF (S1MS2(1:2) .EQ. JSP(I)) THEN
    S1CSP21 = JSP(I)
    S1CSP22 = PLNJSP(I)
    S1CSP23 = FIAJSP(I)
  ENDIF
  IF (S2MS1(1:2) .EQ. JSP(I)) THEN
    S2CSP11 = JSP(I)
    S2CSP12 = PLNJSP(I)
    S2CSP13 = FIAJSP(I)
  ENDIF
  IF (S2MS2(1:2) .EQ. JSP(I)) THEN
    S2CSP21 = JSP(I)
    S2CSP22 = PLNJSP(I)
    S2CSP23 = FIAJSP(I)
  ENDIF
  IF (S3MS1(1:2) .EQ. JSP(I)) THEN
    S3CSP11 = JSP(I)
    S3CSP12 = PLNJSP(I)
    S3CSP13 = FIAJSP(I)
  ENDIF
  IF (S3MS2(1:2) .EQ. JSP(I)) THEN
    S3CSP21 = JSP(I)
    S3CSP22 = PLNJSP(I)
    S3CSP23 = FIAJSP(I)
  ENDIF
ENDDO

!     LOAD REAL VARIABLES INTO DOUBLE PRECISION AND
!     WRITE RECORD TO DATABASE

BS1DBH=S1DBH
BS2DBH=S2DBH
BS3DBH=S3DBH

WRITE(SQLStmtStr,*)'INSERT INTO FVS_StrClass_Metric (CaseID,', &
     'StandID,Year,Removal_Code,Stratum_1_DBH,Stratum_1_Nom_Ht,', &
     'Stratum_1_Lg_Ht,Stratum_1_Sm_Ht,Stratum_1_Crown_Base,', &
     'Stratum_1_Crown_Cover,', &
     'Stratum_1_SpeciesFVS_1,Stratum_1_SpeciesFVS_2,', &
     'Stratum_1_SpeciesPLANTS_1,Stratum_1_SpeciesPLANTS_2,', &
     'Stratum_1_SpeciesFIA_1,Stratum_1_SpeciesFIA_2,', &
     'Stratum_1_Status_Code,Stratum_2_DBH,', &
     'Stratum_2_Nom_Ht,Stratum_2_Lg_Ht,Stratum_2_Sm_Ht,', &
     'Stratum_2_Crown_Base,Stratum_2_Crown_Cover,', &
     'Stratum_2_SpeciesFVS_1,Stratum_2_SpeciesFVS_2,', &
     'Stratum_2_SpeciesPLANTS_1,Stratum_2_SpeciesPLANTS_2,', &
     'Stratum_2_SpeciesFIA_1,Stratum_2_SpeciesFIA_2,', &
     'Stratum_2_Status_Code,Stratum_3_DBH,Stratum_3_Nom_Ht,', &
     'Stratum_3_Lg_Ht,Stratum_3_Sm_Ht,Stratum_3_Crown_Base,', &
     'Stratum_3_Crown_Cover,', &
     'Stratum_3_SpeciesFVS_1,Stratum_3_SpeciesFVS_2,', &
     'Stratum_3_SpeciesPLANTS_1,Stratum_3_SpeciesPLANTS_2,', &
     'Stratum_3_SpeciesFIA_1,Stratum_3_SpeciesFIA_2,', &
     'Stratum_3_Status_Code,Number_of_Strata,', &
     "Total_Cover,Structure_Class) VALUES('",CASEID,"',", &
      "'",TRIM(CNPLT),"',?,?,?,?,?,?,?,?,", &
     "'",TRIM(S1CSP11),"','",TRIM(S1CSP21),"',", &
     "'",TRIM(S1CSP12),"','",TRIM(S1CSP22),"',", &
     "'",TRIM(S1CSP13),"','",TRIM(S1CSP23),"',", &
     "?,?,?,?,?,?,?,", &
     "'",TRIM(S2CSP11),"','",TRIM(S2CSP21),"',", &
     "'",TRIM(S2CSP12),"','",TRIM(S2CSP22),"',", &
     "'",TRIM(S2CSP13),"','",TRIM(S2CSP23),"',", &
     "?,?,?,?,?,?,?,", &
     "'",TRIM(S3CSP11),"','",TRIM(S3CSP21),"',", &
     "'",TRIM(S3CSP12),"','",TRIM(S3CSP22),"',", &
     "'",TRIM(S3CSP13),"','",TRIM(S3CSP23),"',", &
     "?,?,?,'",TRIM(SCLASS),"');"
iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  ISTRCLAS = 0
  RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,RCODE)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BS1DBH)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1NHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1LHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1SHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1CB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1CC)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S1SC)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BS2DBH)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2NHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2LHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2SHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2CB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2CC)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S2SC)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BS3DBH)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3NHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3LHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3SHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3CB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3CC)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,S3SC)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,NS)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,TOTCOV)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ISTRCLAS = 0
ENDIF
RETURN
END


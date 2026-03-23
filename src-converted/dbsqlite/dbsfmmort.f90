SUBROUTINE DBSFMMORT(IYEAR,KILLED,TOTAL,BAKILL, &
     VOKILL,KODE)
IMPLICIT NONE
!
! DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE MORTALITY REPORT
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
!     INPUT:
!              THE MORTALITY OUTPUT FROM THE FIRE MODEL.
!              1: KILLED TREES PER ACRE FOR EACH SIZE CLASS
!              2: TOTAL TREES PER ACRE FOR EACH SIZE CLASS
!              3: MORTALITY IN TERMS OF BASAL AREA
!              4: MORTALITY IN TERMS OF CUFT VOLUME
!              5: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
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
!---
INTEGER MXSP1
PARAMETER (MXSP1 = MAXSP + 1)
INTEGER IYEAR,iRet,KODE,I,J,ColNumber
REAL KILLED,TOTAL,BAKILL,VOKILL
DOUBLE PRECISION KILLEDB,TOTALB,BAKILLB,VOKILLB
DIMENSION KILLED(MXSP1,8),TOTAL(MXSP1,8),KILLEDB(MXSP1,8), &
             TOTALB(MXSP1,8)
DIMENSION BAKILL(MXSP1),VOKILL(MXSP1),BAKILLB(MXSP1), &
             VOKILLB(MXSP1)
CHARACTER*2000 SQLStmtStr
CHARACTER(LEN=8) CSP1,CSP2,CSP3

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_step,fsql3_reset,fsql3_bind_text


IF(IMORTF.EQ.0) RETURN
IF(IMORTF.EQ.2) KODE = 0

CALL DBSCASE(1)
iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
iRet = fsql3_tableexists(IoutDBref, &
          "FVS_Mortality"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Mortality ('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year Int null,'// &
                 'SpeciesFVS text null,'// &
                 'SpeciesPLANTS text null,'// &
                 'SpeciesFIA text null,'// &
                 'Killed_class1 real null,'// &
                 'Total_class1 real null,'// &
                 'Killed_class2 real null,'// &
                 'Total_class2 real null,'// &
                 'Killed_class3 real null,'// &
                 'Total_class3 real null,'// &
                 'Killed_class4 real null,'// &
                 'Total_class4 real null,'// &
                 'Killed_class5 real null,'// &
                 'Total_class5 real null,'// &
                 'Killed_class6 real null,'// &
                 'Total_class6 real null,'// &
                 'Killed_class7 real null,'// &
                 'Total_class7 real null,'// &
                 'Bakill real null,'// &
                 'Volkill real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     IMORTF = 0
     RETURN
   ENDIF
ENDIF
  SQLStmtStr ='INSERT INTO FVS_Mortality (CaseID,'// &
       'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'// &
       'Killed_class1,Total_class1,'// &
       'Killed_class2,'// &
       'Total_class2,Killed_class3,Total_class3,Killed_class4,'// &
       'Total_class4,Killed_class5,Total_class5,Killed_class6,'// &
       'Total_class6,Killed_class7,Total_class7,Bakill,Volkill)'// &
       " VALUES ('"//CASEID//"','"//TRIM(NPLT)// &
       "',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"//CHAR(0)

iRet = fsql3_prepare(IoutDBref, SQLStmtStr)
IF (iRet .NE. 0) THEN
   IMORTF = 0
   RETURN
ENDIF

DO J = 1,MXSP1
  IF (TOTAL(J,8) .LE. 0) CYCLE
  IF (J.EQ.MXSP1) THEN
    CSP1='ALL'
    CSP2='ALL'
    CSP3='ALL'
  ELSE
!
!     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
!
    CSP1 = JSP(J)
    CSP2 = PLNJSP(J)
    CSP3 = FIAJSP(J)

  ENDIF
!
!       ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
  BAKILLB(J) = BAKILL(J)
  VOKILLB(J) = VOKILL(J)

  DO I = 1,8
    KILLEDB(J,I) = KILLED(J,I)
    TOTALB(J,I) = TOTAL(J,I)
  ENDDO

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
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,1))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,1))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,2))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,2))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,3))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,3))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,4))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,4))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,5))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,5))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,6))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,6))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,7))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,7))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BAKILLB(J))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,VOKILLB(J))

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)

ENDDO
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   IMORTF = 0
ENDIF
iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
RETURN
END



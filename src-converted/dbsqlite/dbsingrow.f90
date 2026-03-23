SUBROUTINE DBSINGROW(IYEAR,INGROW,CSP,TPAIN)
IMPLICIT NONE
!----------
! DBSQLITE $Id$
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH REGENERATION INGROWTH
!              SPECIES AND TPA INFORMATION.
!     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'PLOT.f90'
!
!OMMONS
!
INTEGER ColNumber,I,iret,IYEAR,IYEAR1
REAL INGROW,TPAIN
DOUBLE PRECISION INGROW1,TPAIN1
CHARACTER*2000 SQLStmtStr
CHARACTER*2 CSP
CHARACTER*8 CSP1,CSP2,CSP3
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text

IF(IREG5.NE.1) RETURN
!
CALL DBSCASE(1)

!     DEFINE TAABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Ingrow'//CHAR(0))
!
!     SITE PREP SUMMARY
!
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_Ingrow('// &
                 'CaseID           text not null,'// &
                 'StandID          text not null,'// &
                 'Year             int  null,'// &
                 'IngrowthTotalTpa real null,'// &
                 'SpeciesFVS       text null,'// &
                 'SpeciesPLANTS    text null,'// &
                 'SpeciesFIA       text null,'// &
                 'IngrowthTpa      int  null);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    IREG5 = 0
    RETURN
  ENDIF
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow', &
      ' (CaseID,StandID,Year,IngrowthTotalTpa,', &
      'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTpa)', &
      ' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'

iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
   IREG5 = 0
   RETURN
ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
!
DO I = 1,MAXSP
  IF (CSP .EQ. JSP(I)) THEN
    CSP1 = JSP(I)
    CSP2 = PLNJSP(I)
    CSP3 = FIAJSP(I)
  ENDIF
ENDDO

!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!

INGROW1=INGROW
TPAIN1=TPAIN
IYEAR1=IYEAR

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,INGROW1)
ColNumber=ColNumber+1
iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                     LEN_TRIM(CSP1))
ColNumber=ColNumber+1
iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                     LEN_TRIM(CSP2))
ColNumber=ColNumber+1
iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                     LEN_TRIM(CSP3))
ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)
iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
IF (iRet.ne.0) then
  IREG5 = 0
ENDIF
RETURN
END




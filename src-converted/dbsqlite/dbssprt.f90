SUBROUTINE DBSSPRT(CSP,TPASUM,HTAVE,TPATOT, &
     WTHTAVE,TOT,IYEAR)
IMPLICIT NONE
!----------
! DBSQLITE $Id$
!----------C
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              SPROUTING OUTPUT
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
INTEGER ColNumber,I,iret,TOT,IYEAR,TPASUM1,IYEAR1
REAL TPASUM,HTAVE,TPATOT,WTHTAVE
DOUBLE PRECISION HTAVE1
CHARACTER*2000 SQLStmtStr
CHARACTER*3 CSP
CHARACTER*8 CSP1,CSP2,CSP3
!
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text

IF(IREG1.NE.1) RETURN

CALL DBSCASE(1)

!     DEFINE TABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Sprouts'//CHAR(0))
!
!       REGENERATION FROM STUMP & ROOT SPROUTS
!

IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Regen_Sprouts('// &
         'CaseID text not null,'// &
         'StandID text not null,'// &
         'Year int null,'// &
         'SpeciesFVS    text null,'// &
         'SpeciesPLANTS text null,'// &
         'SpeciesFIA    text null,'// &
         'SprtTpa int,'// &
         'SprtAveHt real);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    IREG1 = 0
    RETURN
  ENDIF
ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
IF (CSP .EQ. "ALL") THEN
  CSP1="ALL"
  CSP2="ALL"
  CSP3="ALL"
ELSE
  DO I = 1,MAXSP
    IF (CSP(1:2) .EQ. JSP(I)) THEN
      CSP1 = JSP(I)
      CSP2 = PLNJSP(I)
      CSP3 = FIAJSP(I)
    ENDIF
  ENDDO
ENDIF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Sprouts', &
     ' (CaseID,StandID,Year,', &
     'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,SprtTpa,SprtAveHt)', &
     'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'

iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  IREG1 = 0
  RETURN
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
IF(TOT.EQ.1)THEN
  TPASUM1=NINT(TPASUM)
  HTAVE1=HTAVE
ELSE
  TPASUM1=NINT(TPATOT)
  HTAVE1=WTHTAVE
ENDIF
IYEAR1=IYEAR+1

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
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
iRet = fsql3_bind_int(IoutDBref,ColNumber,TPASUM1)
ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HTAVE1)
iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
IF (iRet.ne.0) then
  IREG1 = 0
ENDIF
RETURN
END




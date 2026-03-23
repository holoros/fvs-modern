SUBROUTINE DBSERROR(NPLT,CMSG)
IMPLICIT NONE
!----------
! DBSQLITE
!----------
!
!     PURPOSE: CREATE AND POPULATE A DATABASE TABLE WITH WARNING AND
!              ERROR MESSAGES.
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
!
!OMMONS
!
INTEGER iRet
CHARACTER*2000 SQLStmtStr
CHARACTER(len=*) NPLT,CMSG
!
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_step, &
           fsql3_prepare,fsql3_finalize

CALL DBSCASE(1)

!     DEFINE TABLENAME

iRet=fsql3_tableexists(IoutDBref,'FVS_Error'//CHAR(0))

IF(iRet.EQ.0) THEN
!
!       TABLE CREATION
!
  SQLStmtStr='CREATE TABLE FVS_Error('// &
                'CaseID text not null,'// &
                'StandID text not null,'// &
                'Message text);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
ENDIF
!
!     INSERT RECORD
!
 WRITE(SQLStmtStr,*)'INSERT INTO FVS_Error', &
         ' (CaseID,StandID,Message)', &
         'VALUES(''',CASEID,''',''',TRIM(NPLT),''',', &
         '''',CMSG,''');'
iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
RETURN
END

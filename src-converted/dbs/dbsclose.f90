SUBROUTINE DBSCLOSE(LCOUT,LCIN)
IMPLICIT NONE
!
! DBS $Id$
!
!     PURPOSE: TO CLOSE DATABASE CONNECTION
!
!OMMONS
!
!
INCLUDE 'DBSCOM.f90'
!
!OMMONS
!
LOGICAL LCOUT,LCIN
IF(ConnHndlOut.NE.-1 .AND. LCOUT) THEN
  !make sure transactions are committed
  iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlOut, SQL_COMMIT)

  !disconnect
  iRet = fvsSQLDisconnect(ConnHndlOut)

  !release connection handles
  iRet = fvsSQLFreeHandle(SQL_HANDLE_DBC, ConnHndlOut)
  ConnHndlOut = -1

  !release environment handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndlOut)
  EnvHndlOut = -1
ENDIF
IF(ConnHndlIn.NE.-1 .AND. LCIN) THEN
  !make sure transactions are committed
  iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlIn, SQL_COMMIT)

  !disconnect
  iRet = fvsSQLDisconnect(ConnHndlIn)

  !release connection handles
  iRet = fvsSQLFreeHandle(SQL_HANDLE_DBC, ConnHndlIn)
  ConnHndlIn = -1

  !release environment handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndlIn)
  EnvHndlIn = -1
ENDIF

END

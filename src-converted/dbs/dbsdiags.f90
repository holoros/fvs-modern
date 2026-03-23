SUBROUTINE DBSDIAGS(HndlType,Hndl,CallFrom)
IMPLICIT NONE
!
! DBS $Id$
!
!      THIS SUBROUTINE PRINTS ERROR DIAGNOSTICS FOR THE DBS
!      CALLS TO THE SQL ODBC API
!
!----
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'DBSCOM.f90'
!
!
!----

INTEGER(SQLHANDLE_KIND)::Hndl
INTEGER(SQLSMALLINT_KIND)::HndlType

CHARACTER(LEN=6):: SqlState
CHARACTER(LEN=*):: CallFrom
CHARACTER(LEN=SQL_MAX_MESSAGE_LENGTH)::Msg
INTEGER(SQLINTEGER_KIND)::NativeError
INTEGER(SQLSMALLINT_KIND):: iDiag, MsgLen
INTEGER(SQLRETURN_KIND):: DiagRet

iDiag = 1
DO WHILE (.true. .and. iDiag.le.5)
   DiagRet = fvsSQLGetDiagRec(loc(HndlType), Hndl, iDiag, &
                SqlState, NativeError, Msg, &
                SQL_MAX_MESSAGE_LENGTH, &
                MsgLen)
   IF (DiagRet.EQ.SQL_NO_DATA) EXIT
   IF (DiagRet.NE.SQL_SUCCESS.AND.DiagRet.NE. &
          SQL_SUCCESS_WITH_INFO) THEN

   EXIT
   ENDIF
   WRITE(JOSTND,100)TRIM(CallFrom),NativeError,Msg(1:MsgLen)
100    FORMAT(/'DBS Error: ',A/'Native error number= ',I16/ &
              'Message text: ',A)

   CALL RCDSET (2,.TRUE.)

   iDiag=iDiag+1

ENDDO
RETURN
END

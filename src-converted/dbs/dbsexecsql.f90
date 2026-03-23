SUBROUTINE DBSEXECSQL (SQLCMD,ConnHndl,LSCHED,IRC)
IMPLICIT NONE
!
! DBS $Id$
!
!
!     AUTH: D. GAMMEL -- RMRS -- MAY 2003
!     PURPOSE: EXECUTES USER DEFINED QUERIES UPON OPENED
!              DATABASE CONNECTIONS
!     INPUT: SQLCMD   - SQL COMMAND QUERY TO BE EXECUTED
!            ConnHndl - THE CONNECTION HANDLE TO THE DBMS
!            LSCHED   - SPECIFIES WHETHER THIS IS COMING FROM THE EVENT
!                       MONITOR
!OMMONS
!
!
INCLUDE  'PRGPRM.f90'
!
!
INCLUDE  'CONTRL.f90'
!
!
INCLUDE  'OPCOM.f90'
!
!
INCLUDE  'PLOT.f90'
!
!
INCLUDE  'DBSCOM.f90'
!
!OMMONS

CHARACTER*(*) SQLCMD
INTEGER(SQLSMALLINT_KIND) ColumnCount,MxNameLen,ColNumber, &
      NameLen
PARAMETER (MxNameLen=20)
CHARACTER(LEN=MxNameLen) ColName
INTEGER(SQLLEN_KIND):: NumAttrPtr
INTEGER(SQLINTEGER_KIND)::LI(MXTST5)
INTEGER(SQLHDBC_KIND):: ConnHndl
INTEGER(SQLHSTMT_KIND):: StmtHndl
INTEGER KODE,I,INDX(MXTST5),IBound,IOPKD
REAL SQLDATA(MXTST5)
LOGICAL      LSCHED
INTEGER IRC

IRC = 1

!     MAKE SURE WE HAVE AN OPEN CONNECTION
IF(ConnHndl.EQ.-1) RETURN

!     PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
CALL DBSPRSSQL(SQLCMD,LSCHED,KODE)
IF(KODE.EQ.0) THEN
!       THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
  WRITE (JOSTND,110) TRIM(SQLCMD)
110   FORMAT (/'********   ERROR: SQLOUT/SQLIN PARSING FAILED. ' &
               'SQL STMT: ',A)
  CALL RCDSET(2,.TRUE.)
  RETURN
ENDIF

!     ALLOCATE A STATEMENT HANDLE

iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, &
                          StmtHndl)
IF (iRet.NE.SQL_SUCCESS .AND. &
       iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndl, &
            'DBSEXEC:DSN Connection')
ELSE
!       EXECUTE QUERY
  iRet = fvsSQLExecDirect(StmtHndl,trim(SQLCMD), &
               int(len_trim(SQLCMD),SQLINTEGER_KIND))
  IF (iRet.NE.SQL_SUCCESS .AND. &
         iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
    CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndl, &
           'DBSEXEC:Executing SQL KeyWrd: '//trim(SQLCMD))
    WRITE (JOSTND,120) trim(SQLCMD)
120     FORMAT ('SQL STMT: ',A)
    GOTO 500
  ENDIF
!
!       IF THIS IS A SELECT QUERY THEN MAKE NEEDED UPDATES TO VARIABLES
!
  iRet = fvsSQLNumResultCols(StmtHndl,ColumnCount)

  IF(ColumnCount.GT.0) THEN
!
!         DETERMINE THE KEWORD AND EDIT THE APPROPRIATE VARIABLE
!
    IBound = 0
    DO ColNumber = 1,ColumnCount
!           GET COLUMN NAME
      iRet = fvsSQLColAttribute(StmtHndl, ColNumber, &
                           SQL_DESC_NAME, ColName, &
                           MxNameLen,NameLen,NumAttrPtr)
!
!          UPPER CASE THE COLUMN NAME FOR EASIER COMPARISONS
!
      DO I = 1, NameLen
        CALL UPCASE(ColName(I:I))
      END DO
      IF (NameLen .LT. MxNameLen) ColName(NameLen+1:) = ""
      IOPKD = 0
      DO I=1,ITST5
        IF (ColName(:8).EQ.CTSTV5(I)) THEN
          IOPKD = I
          EXIT
        ENDIF
      ENDDO
      IF (IOPKD.EQ.0) THEN
        IF(ITST5.LT.MXTST5) THEN
          ITST5 = ITST5+1
          CTSTV5(ITST5) = ColName(:8)
          LTSTV5(ITST5) = .FALSE.
          IOPKD = ITST5
        ENDIF
      ENDIF

      IF(IOPKD.GT.0) THEN
        IBound = IBound + 1
        LI(IBound)=SQL_NULL_DATA
!             FOUND THE EVMON VARIABLE INDEX
        INDX(IBound) = IOPKD
!             THE VALUE IS DEFINED SO BIND VALUE TO SQL DATA ARRAY
        iRet = fvsSQLBindCol (StmtHndl,ColNumber,SQL_F_FLOAT, &
                SQLDATA(IBound),int(4,SQLLEN_KIND),LI(IBound))
      ENDIF
    ENDDO
!         FETCH THE FIRST ROW OF DATA FROM THE QUERY
    iRet = fvsSQLFetch (StmtHndl)
    IF (iRet.EQ.SQL_SUCCESS .OR. &
           iRet.EQ.SQL_SUCCESS_WITH_INFO) THEN
       DO I = 1, IBound
!              IF THE FETCH RETURNED NULL THEN SET EVMON VAR TO UNDEFINED
         IF(LI(I).EQ.SQL_NULL_DATA) THEN
           TSTV5(INDX(I)) = 0
           LTSTV5(INDX(I)) = .FALSE.
         ELSE
           TSTV5(INDX(I)) = SQLDATA(I)
           LTSTV5(INDX(I)) = .TRUE.
         ENDIF
         IRC=0
       ENDDO
    ENDIF
  ELSE
    IRC=0
  ENDIF
ENDIF
!make sure transactions are committed
iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndl, SQL_COMMIT)

500 CONTINUE
!     FREE HANDLE BEFORE LEAVING
iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndl)
RETURN
END

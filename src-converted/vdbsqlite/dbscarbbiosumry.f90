SUBROUTINE DBSCARBBIOSUMRY
IMPLICIT NONE

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'OUTCOM.f90'
INCLUDE 'OPCOM.f90'
INCLUDE 'PLOT.f90'

INTEGER iRet, ColNumber, IHRVC, IYEAR, I
DOUBLE PRECISION DPTCUFT,DPMCUFT,DPSCUFT, &
                    DPRTCUFT,DPRMCUFT,DPRSCUFT, &
                    DPAGBIO,DPMRBIO,DPCSBIO,DPFLBIO, &
                    DPABCRB,DPMRCRB,DPCSCRB,DPFOLCRB, &
                    DPRAGBIO,DPRMRBIO,DPRCSBIO,DPRFOLBIO, &
                    DPRAGCRB,DPRMRCRB,DPRCSCRB,DPRFOLCRB
CHARACTER*2000 SQLStmtStr
CHARACTER*20   TABLENAME

INTEGER fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize

IF(IVBCSUM.EQ.0) RETURN
IF(.NOT.LFIANVB) THEN
  CALL ERRGRO(.TRUE.,52)
  RETURN
END IF

TABLENAME = 'FVS_FIAVBC_Summary'

IYEAR    = IY(ICYC)
IHRVC    = 0
IF (ICYC.GT.NCYC) THEN
  DPTCUFT  = OCVCUR(7)/GROSPC
  DPMCUFT  = OMCCUR(7)/GROSPC
  DPSCUFT  = OSCCUR(7)/GROSPC
  DPAGBIO  = OAGBIOCUR(7)/GROSPC/2000
  DPMRBIO  = OMERBIOCUR(7)/GROSPC/2000
  DPCSBIO  = OCSAWBIOCUR(7)/GROSPC/2000
  DPFLBIO  = OFOLIBIO(7)/GROSPC/2000
  DPABCRB  = OAGCARBCUR(7)/GROSPC/2000
  DPMRCRB  = OMERCARBCUR(7)/GROSPC/2000
  DPCSCRB  = OCSAWCARBCUR(7)/GROSPC/2000
  DPFOLCRB = OFOLICARB(7)/GROSPC/2000

ELSE
  DPTCUFT  = TSTV1(4)
  DPMCUFT  = TSTV1(5)
  DPSCUFT  = TSTV1(20)
  DPAGBIO  = TSTV1(51)
  DPMRBIO  = TSTV1(52)
  DPCSBIO  = TSTV1(53)
  DPFLBIO  = TSTV1(54)
  DPABCRB  = TSTV1(55)
  DPMRCRB  = TSTV1(56)
  DPCSCRB  = TSTV1(57)
  DPFOLCRB = TSTV1(58)

ENDIF

DPRTCUFT = 0.
DPRMCUFT = 0.
DPRSCUFT = 0.
DPRAGBIO = 0.
DPRMRBIO = 0.
DPRCSBIO = 0.
DPRAGCRB = 0.
DPRMRCRB = 0.
DPRCSCRB = 0.
DPRFOLBIO  = 0.
DPRFOLCRB = 0.

IF (ICYC.LE.NCYC) THEN
  DPRTCUFT = OCVREM(7)/GROSPC
  DPRMCUFT = OMCREM(7)/GROSPC
  DPRSCUFT = OSCREM(7)/GROSPC
  DPRAGBIO = OAGBIOREM(7)/GROSPC/2000
  DPRMRBIO = OMERBIOREM(7)/GROSPC/2000
  DPRCSBIO = OCSAWBIOREM(7)/GROSPC/2000
  DPRFOLBIO= OFOLIBIOREM(7)/GROSPC/2000
  DPRAGCRB = OAGCARBREM(7)/GROSPC/2000
  DPRMRCRB = OMERCARBREM(7)/GROSPC/2000
  DPRCSCRB = OCSAWCARBREM(7)/GROSPC/2000
  DPRFOLCRB= OFOLICARBREM(7)/GROSPC/2000
  IF (DPRTCUFT.GT.0.) IHRVC= 1
ENDIF

iRet=fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
IF(iRet.EQ.0) THEN

  SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)// &
                ' (CaseID text not null, '// &
                'StandID text not null, '// &
                'Year int, '// &
                'RmvCode int, '// &
                'TCuFt real, '// &
                'MCuFt real, '// &
                'SCuFt real, '// &
                'AbvGrdBio real, '// &
                'MerchBio real, '// &
                'SawBio real, '// &
                'FoliBio real,'// &
                'AbvGrdCarb real, '// &
                'MerchCarb real, '// &
                'SawCarb real, '// &
                'FoliCarb real, '// &
                'RTCuFt real, '// &
                'RMCuFt real, '// &
                'RSCuFt real, '// &
                'RAbvGrdBio real, '// &
                'RMerchBio real, '// &
                'RSawBio real, '// &
                'RFoliBio real,'// &
                'RAbvGrdCarb real, '// &
                'RMerchCarb real, '// &
                'RSawCarb real,'// &
                'RFoliCarb real);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref, SQLStmtStr)
  IF(iRet .NE. 0) RETURN
ENDIF
DO I=1,2
  SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)// &
      ' (CaseID,StandID,Year,RmvCode,'// &
      'TCuFt,MCuFt,SCuFt,'// &
      'AbvGrdBio,MerchBio,SawBio,FoliBio,'// &
      'AbvGrdCarb,MerchCarb,SawCarb,FoliCarb,'// &
      'RTCuFt,RMCuFt,RSCuFt,'// &
      'RAbvGrdBio,RMerchBio,RSawBio,RFoliBio,'// &
      'RAbvGrdCarb,RMerchCarb,RSawCarb,RFoliCarb)'// &
      " VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,"// &
      '?,?,?,'// &
      '?,?,?,?,'// &
      '?,?,?,?,'// &
      '?,?,?,'// &
      '?,?,?,?,'// &
      '?,?,?,?);'//CHAR(0)

  iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) RETURN

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IHRVC)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPSCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPAGBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMRBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCSBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPFLBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPABCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMRCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCSCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPFOLCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRSCUFT)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRAGBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMRBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRCSBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRFOLBIO)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRAGCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMRCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRCSCRB)

  ColNumber = ColNumber + 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRFOLCRB)

  iRet = fsql3_step(IoutDBref)
  IF (IHRVC.EQ.0) EXIT
  IHRVC     = 2
  DPTCUFT  = MAX(0.,DPTCUFT - DPRTCUFT)
  DPMCUFT  = MAX(0.,DPMCUFT - DPRMCUFT)
  DPSCUFT  = MAX(0.,DPSCUFT - DPRSCUFT)
  DPAGBIO  = MAX(0.,DPAGBIO - DPRAGBIO)
  DPMRBIO  = MAX(0.,DPMRBIO - DPRMRBIO)
  DPCSBIO  = MAX(0.,DPCSBIO - DPRCSBIO)
  DPABCRB  = MAX(0.,DPABCRB - DPRAGCRB)
  DPMRCRB  = MAX(0.,DPMRCRB - DPRMRCRB)
  DPCSCRB  = MAX(0.,DPCSCRB - DPRCSCRB)
  DPFLBIO  = MAX(0.,DPFLBIO - DPRFOLBIO)
  DPFOLCRB  = MAX(0.,DPFOLCRB - DPRFOLCRB)
  DPRTCUFT = 0
  DPRMCUFT = 0
  DPRSCUFT = 0
  DPRAGBIO = 0
  DPRMRBIO = 0
  DPRCSBIO = 0
  DPRAGCRB = 0
  DPRMRCRB = 0
  DPRCSCRB = 0
  DPRFOLBIO = 0
  DPRFOLCRB = 0
ENDDO
iRet = fsql3_finalize(IoutDBref)

IF(iRet.NE.0) RETURN
END

SUBROUTINE DBSSUMRY(IYEAR, IAGE, NPLT, ITPA, IBA, &
      ISDI, ICCF, ITOPHT,FQMD, ITCUFT, &
      IMCUFT, ISCUFT, IBDFT, IRTPA, IRTCUFT, &
      IRMCUFT, IRSCUFT, IRBDFT, IATBA, IATSDI, &
      IATCCF, IATTOPHT, FATQMD, IPRDLEN, IACC, &
      IMORT, YMAI, IFORTP, ISZCL, ISTCL)
IMPLICIT NONE
!----------
! VDBSQLITE $Id$
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              OUTPUT.
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
!
!OMMONS
!
INTEGER IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF, &
      ITOPHT,ITCUFT,IMCUFT,ISCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT, &
      IRSCUFT,IRBDFT, &
      IATBA,IATSDI,IATCCF,IATTOPHT,IFORTP,ISZCL,ISTCL
INTEGER ColNumber,iRet
DOUBLE PRECISION FQMDB,FATQMDB,YMAIB
REAL FQMD,FATQMD,YMAI
CHARACTER*2000 SQLStmtStr
CHARACTER(len=*) NPLT
!
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
      fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
      fsql3_addcolifabsent

IF(ISUMARY.NE.1) RETURN
!
CALL DBSCASE(1)

!     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Summary'//CHAR(0))

IF(iRet.EQ.0) THEN

      SQLStmtStr='CREATE TABLE FVS_Summary('// &
      'CaseID text not null,'// &
      'StandID text not null,'// &
      'Year int,'// &
      'Age int,'// &
      'Tpa int,'// &
      'BA int,'// &
      'SDI int,'// &
      'CCF int,'// &
      'TopHt int,'// &
      'QMD real,'// &
      'TCuFt int,'// &
      'MCuFt int,'// &
      'SCuFt int,'// &
      'BdFt int,'// &
      'RTpa int,'// &
      'RTCuFt int,'// &
      'RMCuFt int,'// &
      'RSCuFt int,'// &
      'RBdFt int,'// &
      'ATBA int,'// &
      'ATSDI int,'// &
      'ATCCF int,'// &
      'ATTopHt int,'// &
      'ATQMD real,'// &
      'PrdLen int,'// &
      'Acc int,'// &
      'Mort int,'// &
      'MAI real,'// &
      'ForTyp int,'// &
      'SizeCls int,'// &
      'StkCls int);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ISUMARY = 0
    RETURN
  ENDIF
ENDIF

!--------
!     CHECK TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
!     `SCuFt`,
!     `RSCuFt`,
!     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE
!--------
iRet = fsql3_addcolifabsent(IoutDBref,"FVS_Summary"//CHAR(0), &
      "SCuFt"//CHAR(0),"int"//CHAR(0))

iRet = fsql3_addcolifabsent(IoutDBref,"FVS_Summary"//CHAR(0), &
      "RSCuFt"//CHAR(0),"int"//CHAR(0))

!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
FQMDB=FQMD
FATQMDB=FATQMD
YMAIB=YMAI

  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Summary', &
      ' (CaseID,StandID,Year,Age,Tpa,', &
      'BA,SDI,CCF,TopHt,QMD,', &
      'TCuFt,MCuFt,SCuFt,BdFt,', &
      'RTpa,RTCuFt,RMCuFt,RSCuFt,RBdFt,', &
      'ATBA,ATSDI,ATCCF,ATTopHt,ATQMD,', &
      'PrdLen,Acc,Mort,MAI,ForTyp,', &
      'SizeCls,StkCls)', &
      'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,', &
      '?,?,?,?,?,', &
      '?,?,?,?,', &
      '?,?,?,?,?,', &
      '?,?,?,?,?,', &
      '?,?,?,?,?,', &
      '?,?);'

iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  ISUMARY = 0
  RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGE)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ITPA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IBA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ISDI)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,FQMDB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ITCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IMCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ISCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTPA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IRMCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IRSCUFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IRBDFT)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IATBA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IATSDI)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IATCCF)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IATTOPHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,FATQMDB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IACC)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IMORT)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,YMAIB)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IFORTP)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ISUMARY = 0
ENDIF
RETURN
END


SUBROUTINE DBSSUMRY2
IMPLICIT NONE
!----------
! VDBSQLITE $Id$
!----------
!     PURPOSE: TO POPULATE A DATABASE WITH SUMMARY STATISTICS
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'OPCOM.f90'
INCLUDE 'OUTCOM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'SUMTAB.f90'
!
!OMMONS
!
INTEGER IYEAR,ICCF,ITOPHT,IOSDI,IPRDLEN,IHRVC,IAGEOUT,IFRTP,SDIX, &
      IZSDI, IRSDI
DOUBLE PRECISION DPTPA,DPTPTPA,DPBA,DPQMD,DPTCUFT,DPTPTCUFT, &
      DPMCUFT,DPTPMCUFT,DPBDFT,DPTPBDFT,DPACC,DPMORT,DPMAI,DPRTPA, &
      DPRTCUFT,DPRMCUFT,DPRBDFT,DPSCUFT,DPTPSCUFT,DPRSCUFT,DPRELDEN, &
      DPDR016
INTEGER ColNumber,iRet,I
CHARACTER*2000 SQLStmtStr
CHARACTER*20 TABLENAME
!
integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
      fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
      fsql3_addcolifabsent
!

IF(ISUMARY.NE.2) RETURN
!
IYEAR    = IY(ICYC)
IAGEOUT  = IOSUM(2,ICYC)
ICCF     = IBTCCF(ICYC)
ITOPHT   = IBTAVH(ICYC)
IOSDI    = ISDI(ICYC)
IZSDI    = NINT(SDIBC2)
IRSDI    = NINT(SDIBC)
DPTPA    = OLDTPA/GROSPC
DPBA     = OLDBA/GROSPC
DPQMD    = ORMSQD
DPDR016  = ODR016
IHRVC    = 0
IF (ICYC.GT.NCYC) THEN
  DPTCUFT  = OCVCUR(7)/GROSPC
  DPMCUFT  = OMCCUR(7)/GROSPC
  DPSCUFT  = OSCCUR(7)/GROSPC
  DPBDFT   = OBFCUR(7)/GROSPC
ELSE
  DPTCUFT  = TSTV1(4)
  DPMCUFT  = TSTV1(5)
  DPSCUFT  = TSTV1(20)
  DPBDFT   = TSTV1(6)
ENDIF
DPTPTPA  = DPTPA   + (TRTPA/GROSPC)
DPTPTCUFT= DPTCUFT + (TRTCUFT/GROSPC)
DPTPMCUFT= DPMCUFT + (TRMCUFT/GROSPC)
DPTPSCUFT= DPSCUFT + (TRSCUFT/GROSPC)
DPTPBDFT = DPBDFT  + (TRBDFT/GROSPC)
DPRTPA   = 0.
DPRTCUFT = 0.
DPRMCUFT = 0.
DPRSCUFT = 0.
DPRBDFT  = 0.
IPRDLEN  = 0
DPACC    = 0.
DPMORT   = 0.
IPRDLEN  = IOSUM(14,ICYC)
SDIX     = NINT(BTSDIX)
DPRELDEN = REAL(IOSDI)/BTSDIX
IF (ICYC.LE.NCYC) THEN
!       NO ACCCRETION OR MORTALITY ON LAST RECORD OF SUMMARY (END OF PROJECTION)
  DPACC    = OACC(7)/GROSPC
  DPMORT   = OMORT(7)/GROSPC
ENDIF
DPMAI    = BCYMAI(ICYC)
IFRTP    = IOSUM(18,ICYC)           ! added 1/18/2014
IF (ICYC.LE.NCYC) THEN
  DPRTPA   = ONTREM(7)/GROSPC
  DPRTCUFT = OCVREM(7)/GROSPC
  DPRMCUFT = OMCREM(7)/GROSPC
  DPRSCUFT = OSCREM(7)/GROSPC
  DPRBDFT  = OBFREM(7)/GROSPC
  IF (DPRTPA.LE.0.) THEN
    IPRDLEN  = IOSUM(14,ICYC)
    DPACC    = OACC(7)/GROSPC
    DPMORT   = OMORT(7)/GROSPC
    DPMAI    = BCYMAI(ICYC)
  ELSE
    IHRVC = 1
  ENDIF
ENDIF

CALL DBSCASE(1)

!     DEFINE TABLENAME

  TABLENAME='FVS_Summary2'

iRet=fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
IF(iRet.EQ.0) THEN

    SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)// &
      ' (CaseID text not null,'// &
      'StandID text not null,'// &
      'Year int,'// &
      'RmvCode int,'// &
      'Age int,'// &
      'Tpa real,'// &
      'TPrdTpa real,'// &
      'BA real,'// &
      'SDI int,'// &
      'ZeideSDI int,'// &
      'ReinekeSDI int,'// &
      'SDIMax int,'// &
      'RDSDI real,'// &
      'CCF int,'// &
      'TopHt int,'// &
      'QMD real,'// &
      'GMD real,' // &
      'TCuFt real,'// &
      'TPrdTCuFt real,'// &
      'MCuFt real,'// &
      'TPrdMCuFt real,'// &
      'SCuFt real,'// &
      'TPrdSCuFt real,'// &
      'BdFt real,'// &
      'TPrdBdFt real,'// &
      'RTpa real,'// &
      'RTCuFt real,'// &
      'RMCuFt real,'// &
      'RSCuFt real,'// &
      'RBdFt real,'// &
      'PrdLen int,'// &
      'Acc real,'// &
      'Mort real,'// &
      'MAI real,'// &
      'ForTyp int,'// &
      'SizeCls int,'// &
      'StkCls int);'//CHAR(0)

  iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ISUMARY = 0
    RETURN
  ENDIF
ENDIF

!--------
!     CHECK EXISTING TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
!     `SCuFt`,
!     `TPrdSCuFt`,
!     `RSCuFt`,
!     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE
!--------
iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "SCuFt"//CHAR(0),"real"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "TPrdSCuFt"//CHAR(0),"real"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "RSCuFt"//CHAR(0),"real"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "SDIMax"//CHAR(0),"int"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "RDSDI"//CHAR(0),"real"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "GMD"//CHAR(0),"real"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "ZeideSDI"//CHAR(0),"int"//CHAR(0))

iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0), &
      "ReinekeSDI"//CHAR(0),"int"//CHAR(0))

DO I=1,2
  IF (IHRVC.EQ.1) THEN
    DPTPTPA   = DPTPTPA   - DPRTPA
    DPTPTCUFT = DPTPTCUFT - DPRTCUFT
    DPTPMCUFT = DPTPMCUFT - DPRMCUFT
    DPTPSCUFT = DPTPSCUFT - DPRSCUFT
    DPTPBDFT  = DPTPBDFT  - DPRBDFT
  ENDIF

   SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)// &
      ' (CaseID,StandID,Year,RmvCode,Age,Tpa,TPrdTpa,BA,SDI,'// &
      'ZeideSDI,ReinekeSDI,SDIMax,RDSDI,'// &
      'CCF,TopHt,QMD,GMD,TCuFt,TPrdTCuFt,MCuFt,TPrdMCuFt,'// &
      'SCuFt,TPrdSCuFt,BdFt,'// &
      'TPrdBdFt,RTpa,RTCuFt,RMCuFt,RSCuFt,RBdFt,'// &
      'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'// &
      ")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,?,?,"// &
      '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' &
      //CHAR(0)

  iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
  IF (iRet .NE. 0) THEN
    ISUMARY = 0
    RETURN
  ENDIF
  ColNumber=1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IHRVC)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGEOUT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPA)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTPA)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBA)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IOSDI)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IZSDI)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IRSDI)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SDIX)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRELDEN)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPQMD)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPDR016)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPMCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPSCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPSCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBDFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPBDFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTPA)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRSCUFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRBDFT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPACC)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMORT)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMAI)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IFRTP)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)
  iRet = fsql3_step(IoutDBref)
  IF (IHRVC.EQ.0) exit
  IHRVC    = 2
  IOSDI    = ISDIAT(ICYC)
  IZSDI    = NINT(SDIAC2)
  IRSDI    = NINT(SDIAC)
  ICCF     = NINT(ATCCF/GROSPC)
  ITOPHT   = NINT(ATAVH)
  DPQMD    = ATAVD
  DPDR016  = ATDR016
  DPBA     = ATBA/GROSPC
  DPTPA    = ATTPA/GROSPC
  DPTCUFT  = MAX(0.,DPTCUFT-DPRTCUFT)
  DPMCUFT  = MAX(0.,DPMCUFT-DPRMCUFT)
  DPSCUFT  = MAX(0.,DPSCUFT-DPRSCUFT)
  DPBDFT   = MAX(0.,DPBDFT -DPRBDFT)
  DPRTPA   = 0.
  DPRTCUFT = 0.
  DPRMCUFT = 0.
  DPRSCUFT = 0.
  DPRBDFT  = 0.
  SDIX     = NINT(ATSDIX)
  DPRELDEN = REAL(IOSDI)/BTSDIX
ENDDO
iRet = fsql3_finalize(IoutDBref)

if (iRet.ne.0) then
   ISUMARY = 0
ENDIF
RETURN
END


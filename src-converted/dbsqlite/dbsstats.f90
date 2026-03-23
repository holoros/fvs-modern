SUBROUTINE DBSSTATS(SPECCD,TPA,BAREA,CFVOL,BFVOL,AGBIO,AGCARB, &
    STDIST1,STDIST2,STDIST3,STDIST4,STDIST5,STDIST6,STDIST7,STDIST8, &
    STDIST9,LABEL,TBL,IYEAR)
IMPLICIT NONE
!----------
! DBSQLITE $Id$
!----------
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
!              CRUISE STATISTICS OUTPUT
!     AUTH: M. SHETTLES -- FMSC -- JULY 2019
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
INTEGER ColNumber,I,iret1,iret2,TBL,STDIST41,IYEAR,STDIST51
REAL STDIST1,STDIST2,STDIST3,STDIST4,STDIST5
REAL STDIST6,STDIST7,STDIST8,STDIST9
REAL TPA,BAREA,CFVOL,BFVOL,AGBIO,AGCARB
DOUBLE PRECISION TPA1,BAREA1,CFVOL1,BFVOL1,BIOMASS,CARBON
DOUBLE PRECISION STDIST11,STDIST21,STDIST31
DOUBLE PRECISION STDIST61,STDIST71,STDIST81,STDIST91
CHARACTER*2000 SQLStmtStr
CHARACTER*4 SPECCD
CHARACTER*8 CSP1,CSP2,CSP3
CHARACTER*19 LABEL
!
!
!OMMONS END

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text

IF(ISTATS1.NE.1) RETURN
!
CALL DBSCASE(1)

IF(TBL.EQ.2)GOTO 110

!     DEFINE TABLENAME

iRet1=fsql3_tableexists(IoutDBref,'FVS_Stats_Species'//CHAR(0))
!
!       GENERAL SPECIES SUMMARY FOR THE CRUISE (PER ACRE)
!

IF(iRet1.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Stats_Species('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'SpeciesFVS    text null,'// &
                 'SpeciesPLANTS text null,'// &
                 'SpeciesFIA    text null,'// &
                 'TreesPerAcre real,'// &
                 'BasalArea real,'// &
                 'CubicFeet real,'// &
                 'BoardFeet real,'// &
                 'FIA_AbvGrdBio real,'// &
                 'FIA_AbvGrdCarb real);'//CHAR(0)

iRet1 = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet1 .NE. 0) THEN
  ISTATS1 = 0
  RETURN
ENDIF
ENDIF

  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Stats_Species', &
       ' (CaseID,StandID,Year,', &
       'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
       'TreesPerAcre,BasalArea,', &
       'CubicFeet,BoardFeet,', &
       'FIA_AbvGrdBio,FIA_AbvGrdCarb)', &
       'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,', &
       '?,?,?,?,?,?,?,?,?);'

  iRet1 = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
    IF (iRet1 .NE. 0) THEN
      ISTATS1 = 0
      RETURN
    ENDIF

!     ASSIGN FVS, PLANTS AND FIA SPECIES CODE

DO I = 1,MAXSP
  IF (SPECCD(1:2) .EQ. JSP(I)) THEN
    CSP1 = JSP(I)
    CSP2 = PLNJSP(I)
    CSP3 = FIAJSP(I)
  ENDIF
ENDDO
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  BFVOL1=BFVOL
  CFVOL1=CFVOL
  TPA1=TPA
  BAREA1=BAREA
  CARBON=AGCARB
  BIOMASS=AGBIO

  ColNumber=1
  iRet1 = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP1, &
                                        LEN_TRIM(CSP1))
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP2, &
                                        LEN_TRIM(CSP2))
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP3, &
                                        LEN_TRIM(CSP3))
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,TPA1)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,BAREA1)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,CFVOL1)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,BFVOL1)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,BIOMASS)
  ColNumber=ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,CARBON)
  iRet1 = fsql3_step(IoutDBref)
  iRet1 = fsql3_finalize(IoutDBref)
  IF (iRet1.ne.0) then
    ISTATS1 = 0
  ENDIF
  RETURN

!
!     DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE POINTS
!
!     DEFINE TABLENAME

!     DEFINE TABLENAME

110 CONTINUE

    iRet2=fsql3_tableexists(IoutDBref,'FVS_Stats_Stand'//CHAR(0))
IF(iRet2.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_Stats_Stand('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'Characteristic text null,'// &
                 'Average real,'// &
                 'Standard_Dev real,'// &
                 'Coeff_of_Var real,'// &
                 'Sample_Size int,'// &
                 'Conf_Level_Percent int,'// &
                 'CI_LB real,'// &
                 'CI_UB real,'// &
                 'Samp_Error_Percent real,'// &
                 'Samp_Error_Units real);'//CHAR(0)

iRet2 = fsql3_exec(IoutDBref,SQLStmtStr)
IF (iRet2 .NE. 0) THEN
  ISTATS2 = 0
  RETURN
ENDIF
ENDIF
  WRITE(SQLStmtStr,*)'INSERT INTO FVS_Stats_Stand', &
           ' (CaseID,StandID,Year,', &
           'Characteristic,Average,Standard_Dev,Coeff_of_Var,', &
           'Sample_Size,Conf_Level_Percent,CI_LB,CI_UB,', &
           'Samp_Error_Percent,Samp_Error_Units) VALUES(''',CASEID, &
           ''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?);'

iRet2 = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet2 .NE. 0) THEN
  ISTATS2 = 0
  RETURN
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
STDIST11=STDIST1
STDIST21=STDIST2
STDIST31=STDIST3
STDIST61=STDIST6
STDIST71=STDIST7
STDIST81=STDIST8
STDIST91=STDIST9
!
!     ASSIGN INTEGER VALUE TO REAL VAR FOR SAMPLE SIZE
!
STDIST41=NINT(STDIST4)
STDIST51=NINT(STDIST5)

ColNumber=1
iRet1 = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_text(IoutDBref,ColNumber,LABEL, &
                                     LEN_TRIM(LABEL))
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST11)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST21)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST31)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_int(IoutDBref,ColNumber,STDIST41)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_int(IoutDBref,ColNumber,STDIST51)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST61)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST71)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST81)
ColNumber=ColNumber+1
iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST91)
iRet2 = fsql3_step(IoutDBref)
iRet2 = fsql3_finalize(IoutDBref)
IF (iRet2.ne.0) then
  ISTATS2 = 0
ENDIF
RETURN
END




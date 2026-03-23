SUBROUTINE DBSTRLS(IWHO,KODE,TEM)
IMPLICIT NONE
!----------
! METRIC-DBS $Id$
!----------
!     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE
!
!     AUTH: D. GAMMEL -- SEM -- JULY 2002
!
!     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
!                     INORDER FOR US TO CONTINUE
!            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
!                     REDIRECT OF THE FLAT FILE REPORT OR IN
!                     ADDITION TO
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'ESTREE.f90'
INCLUDE 'VARCOM.f90'
INCLUDE 'WORKCM.f90'
INCLUDE 'DBSCOM.f90'
INCLUDE 'METRIC.f90'
!
!OMMONS
!
CHARACTER*8 TID,CSPECIES
CHARACTER*2000 SQLStmtStr
CHARACTER*20 TABLENAME,DTYPE
INTEGER IWHO,I,JYR,IP,ITPLAB,ID,IDMR,ICDF,IBDF,IPTBAL,KODE
INTEGER ISPC,I1,I2,I3
INTEGER*4 IDCMP1,IDCMP2
DATA IDCMP1,IDCMP2/10000000,20000000/
REAL CW,P,DGI,DP,TEM,ESTHT

!     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
!     THEN JUST RETURN

IF(ITREELIST.EQ.0.OR.IWHO.NE.1) RETURN

!     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

IF(ITREELIST.EQ.2) KODE = 0


!     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

CALL DBSCASE(1)

IF(TRIM(DBMSOUT).EQ.'EXCEL') THEN
  TABLENAME = '[FVS_TreeList$]'
  DTYPE = 'Number'
ELSEIF(TRIM(DBMSOUT).EQ.'ACCESS') THEN
  TABLENAME = 'FVS_TreeList'
  DTYPE = 'Double'
ELSE
  TABLENAME = 'FVS_TreeList'
  DTYPE = 'real'
ENDIF

!     ALLOCATE A STATEMENT HANDLE

iret = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
IF (iRet.NE.SQL_SUCCESS .AND. iRet.NE. SQL_SUCCESS_WITH_INFO) THEN
  ITREELIST = 0
  PRINT *,'Error connecting to data source'
  CALL  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut, &
                    'DBSTRLS:Connecting to DSN')
  GOTO 100
ENDIF

!     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
!     IF IT DOESN'T THEN WE NEED TO CREATE IT

SQLStmtStr= 'SELECT * FROM '//TABLENAME

iret = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

IF(iRet.NE.SQL_SUCCESS.AND. &
       iRet.NE.SQL_SUCCESS_WITH_INFO) THEN
  IF(TRIM(DBMSOUT).EQ."ACCESS") THEN
    SQLStmtStr='CREATE TABLE FVS_TreeList('// &
                'Id int primary key,'// &
                'CaseID int not null,'// &
                'StandID Text null,'// &
                'Year int null,'// &
                'PrdLen int null,'// &
                'TreeId Text null,'// &
                'TreeIndex int null,'// &
                'Species Text null,'// &
                'TreeVal int null,'// &
                'SSCD int null,'// &
                'PtIndex int null,'// &
                'TPH double null,'// &
                'MortPH double null,'// &
                'DBH double null,'// &
                'DG double null,'// &
                'Ht double null,'// &
                'HtG double null,'// &
                'PctCr int null,'// &
                'CrWidth double null,'// &
                'MistCD int null,'// &
                'BAPctile double null,'// &
                'PtBAL double null,'// &
                'TCuM double null,'// &
                'MCuM double null,'// &
                'NCuM double null,'// &
                'MDefect int null,'// &
                'BDefect int null,'// &
                'TruncHt int null,'// &
                'EstHt double null,'// &
                'ActPt int null)'
  ELSEIF(TRIM(DBMSOUT).EQ."EXCEL") THEN
    SQLStmtStr='CREATE TABLE FVS_TreeList('// &
                'Id INT null,'// &
                'CaseID INT null,'// &
                'StandID Text null,'// &
                'Year INT null,'// &
                'PrdLen int null,'// &
                'TreeId Text null,'// &
                'TreeIndex int null,'// &
                'Species Text null,'// &
                'TreeVal int null,'// &
                'SSCD int null,'// &
                'PtIndex int null,'// &
                'TPH Number null,'// &
                'MortPH Number null,'// &
                'DBH Number null,'// &
                'DG Number null,'// &
                'Ht Number null,'// &
                'HtG Number null,'// &
                'PctCr int null,'// &
                'CrWidth Number null,'// &
                'MistCD int null,'// &
                'BAPctile Number null,'// &
                'PtBAL Number null,'// &
                'TCuM Number null,'// &
                'MCuM Number null,'// &
                'NCuM Number null,'// &
                'MDefect int null,'// &
                'BDefect int null,'// &
                'TruncHt int null,'// &
                'EstHt Number null,'// &
                'ActPt int null)'
  ELSE
    SQLStmtStr='CREATE TABLE FVS_TreeList('// &
                'Id int primary key,'// &
                'CaseID int null,'// &
                'StandID char(26) null,'// &
                'Year int null,'// &
                'PrdLen int null,'// &
                'TreeId char(8) null,'// &
                'TreeIndex int null,'// &
                'Species char(3) null,'// &
                'TreeVal int null,'// &
                'SSCD int null,'// &
                'PtIndex int null,'// &
                'TPH real null,'// &
                'MortPH real null,'// &
                'DBH real null,'// &
                'DG real null,'// &
                'Ht real null,'// &
                'HtG real null,'// &
                'PctCr int null,'// &
                'CrWidth real null,'// &
                'MistCD int null,'// &
                'BAPctile real null,'// &
                'PtBAL real null,'// &
                'TCuM real null,'// &
                'MCuM real null,'// &
                'NCuM real null,'// &
                'MDefect int null,'// &
                'BDefect int null,'// &
                'TruncHt int null,'// &
                'EstHt real null,'// &
                'ActPt int null)'
  ENDIF
  iret = fvsSQLFreeStmt(StmtHndlOut)
  iret = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
          'DBSTRLS:Creating Table: '//trim(SQLStmtStr))
  TREEOUTID = 0
ENDIF

!     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
!     AND THE OUTPUT REPORTING YEAR.

DO ISPC=1,MAXSP
  I1=ISCT(ISPC,1)
  IF(I1.NE.0) THEN
    I2=ISCT(ISPC,2)
    DO I3=I1,I2
      I=IND1(I3)

      JYR=IY(ICYC+1)
      IP=ITRN
      ITPLAB=1
      P = PROB(I) / GROSPC
      IF (ICYC.GT.0) THEN
        DP = WK2(I)/ GROSPC
      ELSE
        DP = 0.0
      ENDIF

!           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
!           GENERATED THROUGH THE ESTAB SYSTEM.

      IF (IDTREE(I) .GT. IDCMP1) THEN
        IF (IDTREE(I) .GT. IDCMP2) THEN
          WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
        ELSE
          WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
        ENDIF
      ELSE
        WRITE(TID,'(I8)') IDTREE(I)
      ENDIF

!           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

      CALL MISGET(I,IDMR)

!           SET CROWN WIDTH.

      CW=CRWDTH(I)

!           DECODE DEFECT AND ROUND OFF POINT BAL.

      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      IPTBAL=NINT(PTBALT(I)*FT2pACRtoM2pHA)

!           DETERMINE ESTIMATED HEIGHT
!           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HASN'T
!           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

      IF (NORMHT(I) .NE. 0) THEN
        ESTHT = (REAL(NORMHT(I))+5)/100
      ELSE
        ESTHT = HT(I)
      ENDIF

!           GET DG INPUT

      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

!
!           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
!           KEYWORD OVER RIDES
!
      IF(JSPIN(ISP(I)).EQ.1)THEN
        CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
      ELSEIF(JSPIN(ISP(I)).EQ.2)THEN
        CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
      ELSEIF(JSPIN(ISP(I)).EQ.3)THEN
        CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
      ELSE
        CSPECIES=ADJUSTL(PLNJSP(ISP(I)))
      ENDIF
!
      IF(ISPOUT6.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
      IF(ISPOUT6.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
      IF(ISPOUT6.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
!
!           CREATE ENTRY FROM DATA FOR TREELIST TABLE

      IF(TREEOUTID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        TREEOUTID = ID
      ENDIF
      TREEOUTID = TREEOUTID + 1

!           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

      IF(TREEOUTID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'( &
              Id,CaseID,StandID,Year,PrdLen, &
              TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPH, &
              MortPH,DBH,DG, &
              HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,TCuM, &
              MCuM,NCuM,MDefect,BDefect,TruncHt,EstHt,ActPt) &
              VALUES(', &
              TREEOUTID,',',ICASE,',',CHAR(39),TRIM(NPLT),CHAR(39), &
              ',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,",'", &
              CSPECIES,"',",IMC(I),',',ISPECL(I), &
              ',',ITRE(I), &
              ',',P/ACRtoHA,',',DP/ACRtoHA,',',DBH(I)*INtoCM, &
              ',',DGI*INtoCM,',',HT(I)*FTtoM,',',HTG(I)*FTtoM, &
              ',',ICR(I),',',CW*FTtoM,',',IDMR,',',PCT(I),',',IPTBAL, &
              ',',CFV(I)*FT3toM3,',',WK1(I)*FT3toM3, &
              ',',BFV(I)*FT3toM3, &
              ',',ICDF,',',IBDF,',', &
              NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),',',ESTHT*FTtoM, &
              ',',IPVEC(ITRE(I)),')'

      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                     'DBSTRLS:Inserting Row: '//trim(SQLStmtStr))
    ENDDO
  ENDIF
ENDDO

!     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
!     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
!
IF((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR.(ICYC.GE.1)) GO TO 100
DO 150 I=IREC2,MAXTRE
  P =(PROB(I) / GROSPC) / (FINT/FINTM)
  WRITE(TID,'(I8)') IDTREE(I)

!       GET MISTLETOE RATING FOR CURRENT TREE RECORD.
  CALL MISGET(I,IDMR)

!       SET CROWN WIDTH.
  CW=CRWDTH(I)

!       DECODE DEFECT AND ROUND OFF POINT BAL.
  ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
  IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
  IPTBAL=NINT(PTBALT(I)*FT2pACRtoM2pHA)

!       CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
  DGI=DG(I)
  IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

!       PUT PROB IN MORTALITY COLUMN
  DP = P
  P = 0.
!
!           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
!           KEYWORD OVER RIDES
!
      IF(JSPIN(ISP(I)).EQ.1)THEN
        CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
      ELSEIF(JSPIN(ISP(I)).EQ.2)THEN
        CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
      ELSEIF(JSPIN(ISP(I)).EQ.3)THEN
        CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
      ELSE
        CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
      ENDIF
!
      IF(ISPOUT6.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
      IF(ISPOUT6.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
      IF(ISPOUT6.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
!
!           CREATE ENTRY FROM DATA FOR TREELIST TABLE

      IF(TREEOUTID.EQ.-1) THEN
        CALL DBSGETID(TABLENAME,'Id',ID)
        TREEOUTID = ID
      ENDIF
      TREEOUTID = TREEOUTID + 1

!           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

      IF(TREEOUTID.GE.65535.AND.TRIM(DBMSOUT).EQ.'EXCEL') GOTO 100

      WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'( &
              Id,CaseID,StandID,Year,PrdLen, &
              TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPH, &
              MortPH,DBH,DG, &
              HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,TCuM, &
              MCuM,BdFt,MDefect,BDefect,TruncHt,EstHt,ActPt) &
              VALUES(', &
              TREEOUTID,',',ICASE,',',CHAR(39),TRIM(NPLT),CHAR(39), &
              ',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,",'", &
              CSPECIES,"',",IMC(I),',',ISPECL(I), &
              ',',ITRE(I), &
              ',',P/ACRtoHA,',',DP/ACRtoHA,',',DBH(I)*INtoCM, &
              ',',DGI*INtoCM,',',HT(I)*FTtoM,',',HTG(I)*FTtoM, &
              ',',ICR(I),',',CW*FTtoM,',',IDMR,',',PCT(I), &
              ',',IPTBAL,',', CFV(I)*FT3toM3,',',WK1(I)*FT3toM3, &
              ',',BFV(I)*0.0,',',ICDF,',',IBDF,',', &
              NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),',',ESTHT*FTtoM, &
              ',',IPVEC(ITRE(I)),')'

      iRet = fvsSQLCloseCursor(StmtHndlOut)

      iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr), &
                   int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
      CALL DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut, &
                     'DBSTRLS:Inserting Row: '//trim(SQLStmtStr))

150 CONTINUE

100 CONTINUE

iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

END

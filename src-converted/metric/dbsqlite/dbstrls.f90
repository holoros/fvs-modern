SUBROUTINE DBSTRLS(IWHO,KODE,TEM)
IMPLICIT NONE
!----------
! DBSQLITE-METRIC $Id: dbstrls.f 2910 2020-01-03 17:39:57Z ckeyser01 $
!----------
!     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE
!
!     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
!                     INORDER FOR US TO CONTINUE
!            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
!                     REDIRECT OF THE FLAT FILE REPORT OR IN
!                     ADDITION TO IT
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'ESTREE.f90'
!
!
INCLUDE 'VARCOM.f90'
!
!
INCLUDE 'WORKCM.f90'
!
!
INCLUDE 'DBSCOM.f90'
!
!
INCLUDE 'METRIC.f90'
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_RESET &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_STEP &
   $ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS &
   _WIN64) &
   $ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT &
   $ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT &
   $ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE &
   $ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG &
   $ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC &
   $ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE &
   $ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN &
   $ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE &
   $ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET &
   $ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP &
   $ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS

!OMMONS
!
CHARACTER*8 TID,CSPECIE1,CSPECIE2,CSPECIE3
CHARACTER*24 TBLNAME
CHARACTER*5 NTCUFT,NMCUFT,NBDFT
CHARACTER*8 NAMDCF,NAMDBF
CHARACTER*2000 SQLStmtStr
INTEGER IWHO,I,IP,ITPLAB,iRet,IDMR,ICDF,IBDF,IPTBAL,KODE
INTEGER ISPC,I1,I2,I3,ColNumber
INTEGER IDCMP1,IDCMP2,ITRNK
DATA IDCMP1,IDCMP2/10000000,20000000/
REAL TEM
REAL*8 CW,P,DGI,DP,ESTHT,TREAGE
DOUBLE PRECISION XB

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize, &
           fsql3_bind_text,fsql3_reset

!     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
!     THEN JUST RETURN

IF(ITREELIST.EQ.0.OR.IWHO.NE.1) RETURN

!     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

IF(ITREELIST.EQ.2) KODE = 0

CALL DBSCASE(1)

!     For CS, LS, NE and SN, the table name is FVS_TreeList_East and the following
!     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR. &
       VARACD.EQ.'NE' .OR. VARACD.EQ.'ON') THEN
  TBLNAME = 'FVS_TreeList_East_Metric'
  NTCUFT  = 'TCuM'
  NMCUFT  = 'MCuM'
  NBDFT   = 'CCum'
  NAMDCF  = 'Ht2TDMCM'
  NAMDBF  = 'Ht2TDSCM'
ELSE
  TBLNAME = 'FVS_TreeList_Metric'
  NTCUFT  = 'TCuM'
  NMCUFT  = 'MCuM'
  NBDFT   = 'CCum'
  NAMDCF  = 'Ht2TDCM '
  NAMDBF  = 'Ht2TDBM '
ENDIF

iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
iRet = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE ' // TRIM(TBLNAME) // &
                ' (CaseID text not null,'// &
                'StandID text not null,'// &
                'Year int null,'// &
                'PrdLen int null,'// &
                'TreeId text null,'// &
                'TreeIndex int null,'// &
                'SpeciesFVS text null,'// &
                'SpeciesPLANTS text null,'// &
                'SpeciesFIA text null,'// &
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
                NTCUFT // ' real null,'// &
                NMCUFT // ' real null,'// &
                NBDFT  // ' real null,'// &
                'MDefect int null,'// &
                'BDefect int null,'// &
                'TruncHt int null,'// &
                'EstHt real null,'// &
                'ActPt int null,'// &
                NAMDCF // ' real null,'// &
                NAMDBF // ' real null,'// &
                'TreeAge real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     ITREELIST = 0
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     RETURN
   ENDIF
ENDIF
WRITE(SQLStmtStr,*)'INSERT INTO ',TBLNAME, &
     ' (CaseID,StandID,Year,PrdLen,', &
     'TreeId,TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,', &
     'TreeVal,SSCD,PtIndex,TPH,', &
     'MortPH,DBH,DG,', &
     'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',NTCUFT,',', &
     NMCUFT,',',NBDFT,',MDefect,BDefect,TruncHt,', &
     'EstHt,ActPt,',NAMDCF,',',NAMDBF,',','TreeAge) VALUES (''', &
     CASEID,''',''',TRIM(NPLT),''',',IY(ICYC+1),',',IFINT, &
    ',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
IF (iRet .NE. 0) THEN
  ITREELIST = 0
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  RETURN
ENDIF

!     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
!     AND THE OUTPUT REPORTING YEAR.

DO ISPC=1,MAXSP
  I1=ISCT(ISPC,1)
  IF(I1.NE.0) THEN
    I2=ISCT(ISPC,2)
    DO I3=I1,I2
      I=IND1(I3)

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
        TID=ADJUSTL(TID)
      ENDIF

!           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

      CALL MISGET(I,IDMR)

!           DECODE DEFECT AND ROUND OFF POINT BAL.

      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      IPTBAL=NINT(PTBALT(I))

!           SET TRUNCATED (TOPKILL) HEIGHT
!
      ITRNK = INT(REAL((ITRUNC(I)+5)*.01*FTtoM))

!           DETERMINE ESTIMATED HEIGHT
!           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
!           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

      IF (NORMHT(I) .NE. 0) THEN
        ESTHT = (REAL(NORMHT(I))+5)/100
      ELSE
        ESTHT = HT(I)
      ENDIF

!           DETERMINE TREE AGE

      IF (LBIRTH(I)) THEN
        TREAGE = ABIRTH(I)
      ELSE
        TREAGE = 0
      ENDIF

!           GET DG INPUT

      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

!           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.
!
      CSPECIE1 = JSP(ISP(I))
      CSPECIE2 = PLNJSP(ISP(I))
      CSPECIE3 = FIAJSP(ISP(I))

      ColNumber=1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,TID, &
                            LEN_TRIM(TID))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,I)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1, &
                                LEN_TRIM(CSPECIE1))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2, &
                                LEN_TRIM(CSPECIE2))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3, &
                                LEN_TRIM(CSPECIE3))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

      ColNumber=ColNumber+1
      XB = P/ACRtoHA
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = DP/ACRtoHA
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = DBH(I)*INtoCM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = DGI*INtoCM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = HT(I)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = HTG(I)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))

      ColNumber=ColNumber+1
      XB = CRWDTH(I)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)

      ColNumber=ColNumber+1
      XB = PCT(I)
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)

      ColNumber=ColNumber+1
      XB = CFV(I)*FT3toM3
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = WK1(I)*FT3toM3
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = BFV(I)*FT3toM3
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)

      ColNumber=ColNumber+1
      XB = ESTHT*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

      ColNumber=ColNumber+1
      XB = HT2TD(I,2)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = HT2TD(I,1)*FTtoM
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      ColNumber=ColNumber+1
      XB = TREAGE
      iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_reset(IoutDBref)
    ENDDO
  ENDIF
ENDDO

!     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
!     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
!
IF (ITREELIST .EQ. 0) RETURN
IF ((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR. &
            (ICYC.GE.1)) THEN
  iRet = fsql3_finalize(IoutDBref)
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  RETURN
ENDIF

DO I=IREC2,MAXTRE

  P =(PROB(I) / GROSPC) / (FINT/FINTM)
  WRITE(TID,'(I8)') IDTREE(I)
  TID=ADJUSTL(TID)

!       GET MISTLETOE RATING FOR CURRENT TREE RECORD.
  CALL MISGET(I,IDMR)

!       SET CROWN WIDTH.
  CW=CRWDTH(I)

!       DECODE DEFECT AND ROUND OFF POINT BAL.

  ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
  IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
  IPTBAL=NINT(PTBALT(I))

!       SET TRUNCATED (TOPKILL) HEIGHT
!
  ITRNK = INT(REAL((ITRUNC(I)+5)*.01*FTtoM))

!       DETERMINE ESTIMATED HEIGHT
!       ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
!       BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

  IF (NORMHT(I) .NE. 0) THEN
    ESTHT = ((REAL(NORMHT(I))+5)/100)
  ELSE
    ESTHT = HT(I)
  ENDIF

!       DETERMINE TREE AGE

  IF (LBIRTH(I)) THEN
    TREAGE = ABIRTH(I)
  ELSE
    TREAGE = 0
  ENDIF

!       CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.

  DGI=DG(I)
  IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

!       PUT PROB IN MORTALITY COLUMN
  DP = P
  P = 0.

!       LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.
!
  CSPECIE1 = JSP(ISP(I))
  CSPECIE2 = PLNJSP(ISP(I))
  CSPECIE3 = FIAJSP(ISP(I))

  ColNumber=1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,TID, &
                            LEN_TRIM(TID))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,I)
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1, &
                            LEN_TRIM(CSPECIE1))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2, &
                            LEN_TRIM(CSPECIE2))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3, &
                            LEN_TRIM(CSPECIE3))
  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

  ColNumber=ColNumber+1
  XB = P/ACRtoHA
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = DP/ACRtoHA
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = DBH(I)*INtoCM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = DGI*INtoCM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = HT(I)*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = HTG(I)*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))

  ColNumber=ColNumber+1
  XB = CRWDTH(I)*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)

  ColNumber=ColNumber+1
  XB = PCT(I)
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)

  ColNumber=ColNumber+1
  XB = CFV(I)*FT3toM3
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = WK1(I)*FT3toM3
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = BFV(I)*FT3toM3
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)

  ColNumber=ColNumber+1
  XB = ESTHT*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

  ColNumber=ColNumber+1
  XB = HT2TD(I,2)*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = HT2TD(I,1)*FTtoM
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  ColNumber=ColNumber+1
  XB = TREAGE
  iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)
ENDDO
iRet = fsql3_finalize(IoutDBref)
iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

RETURN
END

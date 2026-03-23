SUBROUTINE DBSATRTLS(IWHO,KODE,TEM)
!
! DBSQLITE $Id$
!
!     PURPOSE: TO OUTPUT THE ATRTLIST DATA TO THE DATABASE
!
!     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 3
!                     INORDER FOR US TO CONTINUE
!            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
!                     REDIRECT OF THE FLAT FILE REPORT OR IN
!                     ADDITION TO
!
IMPLICIT NONE
!
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
INCLUDE 'wdbkwtdata.inc'
!
!
!OMMONS
!
CHARACTER*8 TID,CSPECIE1,CSPECIE2,CSPECIE3
CHARACTER*17 TBLNAME
CHARACTER*5 NTCUFT,NMCUFT,NSCUFT,NBDFT
CHARACTER*8 NAMDCF,NAMDBF
CHARACTER*2000 SQLStmtStr
INTEGER IWHO,I,JYR,IP,ITPLAB,IRCODE,IDMR,ICDF,IBDF,IPTBAL,KODE
INTEGER ISPC,I1,I2,I3, IFIASPP
INTEGER*4 IDCMP1,IDCMP2
DATA IDCMP1,IDCMP2/10000000,20000000/
REAL CW,P,DGI,DP,TEM,ESTHT,TREAGE

INTEGER fsql3_tableexists,fsql3_exec,fsql3_addcolifabsent,iRet
!---------
!     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
!     THEN JUST RETURN

IF(IATRTLIST.EQ.0.OR.IWHO.NE.3) RETURN

!     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

IF(IATRTLIST.EQ.2) KODE = 0

!     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

CALL DBSCASE(1)

!     UPDATED TO CONSOLIDATE TABLES.  DISTINCT EAST/WEST TABLES NO LONGER NEEDED (2024 DW)

  TBLNAME = 'FVS_ATRTList'
  NTCUFT  = 'TCuFt'
  NMCUFT  = 'MCuFt'
  NSCUFT  = 'SCuFt'
  NBDFT   = 'BdFt'
  NAMDCF  = 'Ht2TDCF '
  NAMDBF  = 'Ht2TDBF '

IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))

!     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
!     IF IT DOESNT THEN WE NEED TO CREATE IT

IRCODE = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
IF(IRCODE.EQ.0) THEN
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
                'TPA real null,'// &
                'MortPA real null,'// &
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
                NSCUFT // ' real null,'// &
                NBDFT  // ' real null,'// &
                'MDefect int null,'// &
                'BDefect int null,'// &
                'TruncHt int null,'// &
                'EstHt real null,'// &
                'ActPt int null,'// &
                NAMDCF // ' real null,'// &
                NAMDBF // ' real null,'// &
                'TreeAge real null);' // CHAR(0)
  IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
  IF (IRCODE .NE. 0) THEN
    IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
    IATRTLIST = 0
    RETURN
  ENDIF
ENDIF

!--------
!     CHECK TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
!     `SCuFt`,
!     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE
!--------
iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TBLNAME)//CHAR(0), &
           "SCuFt"//CHAR(0),"real"//CHAR(0))

!     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
!     AND THE OUTPUT REPORTING YEAR.
!
JYR=IY(ICYC)
DO ISPC=1,MAXSP
  I1=ISCT(ISPC,1)
  IF(I1.NE.0) THEN
    I2=ISCT(ISPC,2)
    DO I3=I1,I2
      IP=0
      DO I=1,ITRN
      IF (PROB(I).GT.0) IP=IP+1
      ENDDO
      I=IND1(I3)
      ITPLAB=4
      P=PROB(I)/GROSPC
      DP = 0.0
!           SKIP OUTPUT IF P <= 0
      IF (P.LE.0.0) CYCLE

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
      IPTBAL=NINT(PTBALT(I))

!           DETERMINE ESTIMATED HEIGHT
!           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HAS NOT
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

      WRITE(SQLStmtStr,*)'INSERT INTO ',TBLNAME, &
           ' (CaseID,StandID,Year,PrdLen,TreeId,', &
           'TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,TreeVal,', &
           'SSCD,PtIndex,TPA,MortPA,DBH,', &
           'DG,HT,HTG,PctCr,CrWidth,', &
           'MistCD,BAPctile,PtBAL,',NTCUFT,',',NMCUFT,',', &
           NSCUFT,',',NBDFT,',', &
           'MDefect,BDefect,TruncHt,EstHt,ActPt,', &
           NAMDCF,',',NAMDBF,',','TreeAge)', &
           ' VALUES (''', &
           CASEID,''',''',TRIM(NPLT),''',', &
           JYR,',',IFINT,",'",TRIM(ADJUSTL(TID)),"',",I, &
           ",'",TRIM(CSPECIE1),"'", &
           ",'",TRIM(CSPECIE2),"'", &
           ",'",TRIM(CSPECIE3),"',", &
           IMC(I),',',ISPECL(I),',',ITRE(I), &
           ',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I), &
           ',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',', &
           CFV(I),',',MCFV(I),',',SCFV(I),',',BFV(I),',', &
           ICDF,',',IBDF,',', &
           ((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)), &
           ',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,');'

     IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
     IF (IRCODE .NE. 0) THEN
       IATRTLIST = 0
       IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
       RETURN
     ENDIF
    ENDDO
  ENDIF
ENDDO
IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
RETURN
END

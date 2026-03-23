      SUBROUTINE DBS_FIAVBC_CUTLST()
C
C DBSQLITE $Id$
C
C     PURPOSE: TO OUTPUT THE FIAVBC CUTS LIST DATA TO THE DATABASE
C
      IMPLICIT NONE
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ESTREE.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'wdbkwtdata.inc'
COMMONS
C
      CHARACTER*8 TID,CSPECIE1,CSPECIE2,CSPECIE3
      CHARACTER*18 TBLNAME
      CHARACTER*2000 SQLStmtStr
      INTEGER I,IP,IRCODE,ITRNK
      INTEGER ISPC,I1,I2,I3,iRet
      INTEGER*4 IDCMP1,IDCMP2
      DATA IDCMP1,IDCMP2/10000000,20000000/

      INTEGER ColNumber
      REAL*8 P, DDBH, DP, DHT, ESTHT,
     >       DCFV,DMCFV,DSCFV,DCULL,DCRBFRC,
     >       DAGBIO, DMERCHBIO, DSAWBIO, DFOLIBIO,
     >       DAGCARB, DMERCHCARB, DSAWCARB, DFOLICARB

      INTEGER fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,
     >        fsql3_bind_text,fsql3_reset
C---------
C     IF IVBCCUTLST IS NOT TURNED ON OR THE FIAVBC NOT ACTIVE
C     THEN JUST RETURN
C---------
      IF(IVBCCUTLST.EQ.0) RETURN
      IF(.NOT.LFIANVB) THEN
        CALL ERRGRO(.TRUE.,52)
        RETURN
      END IF
C---------
C     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER
C---------
      CALL DBSCASE(1)
      
        TBLNAME = 'FVS_FIAVBC_CutList'

C     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
C     IF IT DOESNT THEN WE NEED TO CREATE IT

      IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))
      IF (IRCODE .NE. 0) THEN
        IVBCCUTLST = 0
        RETURN
      ENDIF
      IRCODE = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
      IF(IRCODE.EQ.0) THEN
          SQLStmtStr='CREATE TABLE ' // TRIM(TBLNAME) //
     -             ' (CaseID text not null,'//
     -             'StandID text not null,'//
     -             'PtIndex int null,'//
     -             'ActPt int null,'//
     -             'Year int null,'//
     -             'TreeId text null,'//
     -             'TreeIndex int null,'//
     -             'SpeciesFVS text null,'//
     -             'SpeciesPLANTS text null,'//
     -             'SpeciesFIA text null,'//
     -             'TPA real null,'//
     -             'MortTPA real null,'//
     -             'DBH real null,'//
     -             'Ht real null,'//
     -             'EstHt real null,' //
     -             'TruncHt int null,'//
     -             'PctCr int null,'//
     -             'Cull real null,' //
     -             'WdldStem int null,' //
     -             'DecayCd int null,' //
     -             'CarbFrac real null,' //
     -             'TCuFt real null,'//
     -             'MCuFt real null,'//
     -             'SCuFt real null,'//
     -             'AbvGrdBio real null,'//
     -             'MerchBio real null,'//
     -             'SawBio real null,'//
     -             'FoliBio real null,' //
     -             'AbvGrdCarb real null,'//
     -             'MerchCarb real null,'//
     -             'SawCarb real null,'//
     -             'FoliCarb real null);'//CHAR(0)

        IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (IRCODE .NE. 0) THEN
          IVBCCUTLST = 0
          iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
          RETURN
        ENDIF
      ENDIF

      WRITE(SQLStmtStr,*)'INSERT INTO ',TBLNAME,
     -         ' (CaseID,StandID,PtIndex,ActPt,Year,TreeId,TreeIndex,', !1
     -         'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',                  !2
     -         'TPA,MortTPA,DBH,Ht,EstHt,TruncHt,PctCr,',               !3
     -         'Cull,WdldStem,DecayCd,CarbFrac,',                       !4
     -         'TCuFt,MCuFt,SCuFt,',                                    !5
     -         'AbvGrdBio,MerchBio,SawBio,FoliBio,',                    !6 
     -         'AbvGrdCarb,MerchCarb,SawCarb,FoliCarb) ',               !7
     -         ' VALUES (''',
     -           CASEID,''',''',TRIM(NPLT),''',?,?,',IY(ICYC),',?,?,',  !1
     -           '?,?,?,',                                              !2  
     -           '?,?,?,?,?,?,?,',                                      !3
     -           '?,?,?,?,',                                            !4
     -           '?,?,?,',                                              !5
     -           '?,?,?,?,',                                            !6
     -           '?,?,?,?);'                                            !7 
 
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
        IVBCCUTLST = 0
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        RETURN
      ENDIF
C---------
C     SET THE CUTS LIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C---------
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            IP=0
            DO I=1,ITRN
              IF (WK3(I).GT.0) IP=IP+1
            ENDDO
            I=IND1(I3)
            P = WK3(I)/GROSPC
            DP = 0.0
C           SKIP OUTPUT IF P <= 0
            IF (P.LE.0.0) CYCLE

C----------
C           TRANSLATE TREES IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C           GENERATED THROUGH THE ESTAB SYSTEM.
C----------
            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
            ENDIF

C           DETERMINE ESTIMATED HEIGHT
C           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HAS NOT
C           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT
            ! IF (NORMHT(I) .NE. 0) THEN
            !   ESTHT = (REAL(NORMHT(I))+5)/100
            ! ELSE
            !   ESTHT = HT(I)
            ! ENDIF

C           SET TRUNCATED (TOPKILL) HEIGHT
C
            ITRNK = INT((ITRUNC(I)+5)/100)

C           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

            CSPECIE1 = JSP(ISP(I))
            CSPECIE2 = PLNJSP(ISP(I))
            CSPECIE3 = FIAJSP(ISP(I))

            ColNumber=1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I))          !PtIndex (Plot or point index)

            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))   !ActPt 

            ColNumber=ColNumber+1
            iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,             
     >                         LEN_TRIM(TID))                           !TreeID
     
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,I)                !TreeIndex

            ColNumber=ColNumber+1
            iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1,        
     >                             LEN_TRIM(CSPECIE1))                  !SpeciesFVS 

            ColNumber=ColNumber+1
            iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2,        
     >                             LEN_TRIM(CSPECIE2))                  !SpeciesPLANTS

            ColNumber=ColNumber+1
            iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3,        
     >                             LEN_TRIM(CSPECIE3))                  !SpeciesFIA

            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,P)             !TPA

            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DP)            !MortTPA

            DDBH=DBH(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH)          !DBH

            DHT = HT(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT)         !Ht

            IF (NORMHT(I) .GT. 0) THEN
              ESTHT=(REAL(NORMHT(I))+5)/100
            ELSE
              ESTHT=0
            ENDIF
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT)         !NormHt

            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)            !TruncHt

            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))           !PctCr (Percent live crown)

C           Add FIA Cull, Number WoodlandStems, Decay Code, Carbon fraction
            ColNumber=ColNumber+1
            DCULL = CULL(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DCULL)         !Cull

            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,WDLDSTEM(I))      !WdldStem

            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,DECAYCD(I))      !DecayCd

            ColNumber=ColNumber+1
            DCRBFRC = CARB_FRAC(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DCRBFRC)       !CarbFrac

C           Add cubic volume outputs
            ColNumber=ColNumber+1
            DCFV = CFV(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV)          !TCuFt

            ColNumber=ColNumber+1
            DMCFV = MCFV(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DMCFV)         !MCuFt

            ColNumber=ColNumber+1
            DSCFV = SCFV(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DSCFV)         !SCuFt
            
C           Add biomass and carbon outputs
            ColNumber=ColNumber+1
            DAGBIO = ABVGRD_BIO(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DAGBIO)        !AbvGrdBio

            ColNumber=ColNumber+1
            DMERCHBIO = MERCH_BIO(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DMERCHBIO)     !MerchBio

            ColNumber=ColNumber+1
            DSAWBIO = CUBSAW_BIO(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DSAWBIO)       !SawBio

            ColNumber=ColNumber+1
            DFOLIBIO = FOLI_BIO(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DFOLIBIO)      !FoliBio

            ColNumber=ColNumber+1
            DAGCARB = ABVGRD_CARB(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DAGCARB)       !AbvGrdCarb

            ColNumber=ColNumber+1
            DMERCHCARB = MERCH_CARB(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DMERCHCARB)    !MerchCarb

            ColNumber=ColNumber+1
            DSAWCARB = CUBSAW_CARB(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DSAWCARB)      !SawCarb

            ColNumber=ColNumber+1
            DFOLICARB = FOLI_CARB(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DFOLICARB)     !FoliCarb

            iRet = fsql3_step(IoutDBref)
            iRet = fsql3_reset(IoutDBref)
          ENDDO
        ENDIF
      ENDDO
      IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
      RETURN
      END

SUBROUTINE DBSFMPF(IYEAR,NPLT,SFLMSU,MFLMSU,SFLMTO,MFLMTO,SFTYPE, &
     MFTYPE,SPTRCH,MPTRCH,TORCHI,CROWNI,CNPYHT,CNPYDNST,SMORTBA, &
     MMORTBA,SMORTVOL,MMORTVOL,SPSMOKE,MPSMOKE,SFUELMOD,SFUELWT, &
     FUELMOD,FUELWT,KODE)

IMPLICIT NONE
!----------
! VDBSQLITE $Id$
!----------
!     PURPOSE: TO POPULATE A DATABASE WITH THE FIRE MODELS POTENTIAL FIRE
!              OUTPUT.
!     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
!     INPUT:
!            THE POTFIRE OUTPUT FROM THE FIRE MODEL.
!              1: SURFACE FLAME LENGTH SEVERE
!              2: SURFACE FLAME LENGHT MODERATE
!              3: TOTAL FLAME LENGTH SEVERE
!              4: TOTAL FLAME LENGHT MODERATE
!              5: FIRE TYPE SEVERE
!              6: FIRE TYPE MODERATE
!              7: P-TORCH SEVERE
!              8: P-TORCH MODERATE
!              9: TORCH INDEX
!             10: CROWN INDEX
!             11: CANOPY HEIGHT
!             12: CANOPY DENSITY
!             13: MORTALITY BA SEVERE
!             14: MORTALITY BA MODERATE
!             15: MORTALITY VOLUME SEVERE
!             16: MORTALITY VOLUME MODERATE
!             17: POTENTIAL SMOKE SEVERE
!             18: POTENTIAL SMOKE MODERATE
!             19: SEVERE FUEL MODEL
!             20: SEVERE FUEL WEIGHT
!             21: FUEL MODEL
!             22: FUEL WEIGHT
!             23: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'DBSCOM.f90'
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

INTEGER, PARAMETER :: MxMsg = 500
CHARACTER(LEN=MxMsg) Msg
INTEGER IYEAR,CNPYHT,SMORTBA,MMORTBA,SMORTVOL,MMORTVOL,KODE,iRet
INTEGER FUELMOD,SFUELMOD,ColNumber
REAL SFLMTO,MFLMTO,TORCHI,CROWNI,CNPYDNST,SPSMOKE,MPSMOKE
REAL SFLMSU,MFLMSU,SPTRCH,MPTRCH
DOUBLE PRECISION BSFLMTO,BMFLMTO,BTORCHI,BCROWNI
DOUBLE PRECISION BSFLMSU,BMFLMSU,BSPTRCH,BMPTRCH
DOUBLE PRECISION BCNPYDNST,BSPSMOKE,BMPSMOKE
REAL FUELWT,SFUELWT
DOUBLE PRECISION,DIMENSION(4)::BFUELWT,BSFUELWT
DIMENSION FUELMOD(4),FUELWT(4),SFUELMOD(4),SFUELWT(4)
CHARACTER*2000 SQLStmtStr
CHARACTER*8 SFTYPE,MFTYPE
CHARACTER(len=26) NPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
       fsql3_prepare,fsql3_bind_double,fsql3_finalize,fsql3_errmsg

!     Initialize variables

IF(IPOTFIRE.EQ.0) RETURN
IF(IPOTFIRE.EQ.2) KODE = 0

!---------
!     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
!---------
CALL DBSCASE(1)

IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
  iRet = fsql3_tableexists(IoutDBref, &
          "FVS_PotFire_East"//CHAR(0))
  IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_PotFire_East('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'Flame_Len_Sev real null,'// &
                 'Flame_Len_Mod real null,'// &
                 'Canopy_Ht int null,'// &
                 'Canopy_Density real null,'// &
                 'Mortality_BA_Sev real null,'// &
                 'Mortality_BA_Mod real null,'// &
                 'Mortality_VOL_Sev real null,'// &
                 'Mortality_VOL_Mod real null,'// &
                 'Pot_Smoke_Sev real null,'// &
                 'Pot_Smoke_Mod real null,'// &
                 'Fuel_Mod1_Sev int null,'// &
                 'Fuel_Mod2_Sev int null,'// &
                 'Fuel_Mod3_Sev int null,'// &
                 'Fuel_Mod4_Sev int null,'// &
                 'Fuel_Wt1_Sev real null,'// &
                 'Fuel_Wt2_Sev real null,'// &
                 'Fuel_Wt3_Sev real null,'// &
                 'Fuel_Wt4_Sev real null,'// &
                 'Fuel_Mod1_Mod int null,'// &
                 'Fuel_Mod2_Mod int null,'// &
                 'Fuel_Mod3_Mod int null,'// &
                 'Fuel_Mod4_Mod int null,'// &
                 'Fuel_Wt1_Mod real null,'// &
                 'Fuel_Wt2_Mod real null,'// &
                 'Fuel_Wt3_Mod real null,'// &
                 'Fuel_Wt4_Mod real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     IF (iRet.NE.0) THEN
       iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
       print *,"FVS_PotFire_East exec direct east error:", &
            Msg(:iRet)
       IPOTFIRE = 0
       RETURN
     ENDIF
   ENDIF
ELSE !NOT SN VARIANT
  iRet = fsql3_tableexists(IoutDBref, &
          "FVS_PotFire"//CHAR(0))
  IF(iRet.EQ.0) THEN
     SQLStmtStr='CREATE TABLE FVS_PotFire ('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year int null,'// &
                 'Surf_Flame_Sev real null,'// &
                 'Surf_Flame_Mod real null,'// &
                 'Tot_Flame_Sev real null,'// &
                 'Tot_Flame_Mod real null,'// &
                 'Fire_Type_Sev text null,'// &
                 'Fire_Type_Mod text null,'// &
                 'PTorch_Sev real null,'// &
                 'PTorch_Mod real null,'// &
                 'Torch_Index real null,'// &
                 'Crown_Index real null,'// &
                 'Canopy_Ht int null,'// &
                 'Canopy_Density real null,'// &
                 'Mortality_BA_Sev real null,'// &
                 'Mortality_BA_Mod real null,'// &
                 'Mortality_VOL_Sev real null,'// &
                 'Mortality_VOL_Mod real null,'// &
                 'Pot_Smoke_Sev real null,'// &
                 'Pot_Smoke_Mod real null,'// &
                 'Fuel_Mod1 int null,'// &
                 'Fuel_Mod2 int null,'// &
                 'Fuel_Mod3 int null,'// &
                 'Fuel_Mod4 int null,'// &
                 'Fuel_Wt1 real null,'// &
                 'Fuel_Wt2 real null,'// &
                 'Fuel_Wt3 real null,'// &
                 'Fuel_Wt4 real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     IF (iRet.NE.0) THEN
       iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
       print *,"FVS_PotFire west exec direct error:",Msg(:iRet)
       IPOTFIRE = 0
       RETURN
     ENDIF
   ENDIF
ENDIF

BSFLMTO=0D0
BMFLMTO=0D0
BSFLMSU=0D0
BMFLMSU=0D0
BSPTRCH=0D0
BMPTRCH=0D0
BTORCHI=0D0
BCROWNI=0D0
BCNPYDNST=0D0
BSPSMOKE=0D0
BMPSMOKE=0D0
BSFLMTO=SFLMTO
BMFLMTO=MFLMTO
BSFLMSU=SFLMSU
BMFLMSU=MFLMSU
BSPTRCH=SPTRCH
BMPTRCH=MPTRCH
BTORCHI=TORCHI
BCROWNI=CROWNI
BCNPYDNST=CNPYDNST
BSPSMOKE=SPSMOKE
BMPSMOKE=MPSMOKE

BFUELWT(1)=DBLE(INT((FUELWT(1)*100.)+0.5))
BFUELWT(2)=DBLE(INT((FUELWT(2)*100.)+0.5))
BFUELWT(3)=DBLE(INT((FUELWT(3)*100.)+0.5))
BFUELWT(4)=DBLE(INT((FUELWT(4)*100.)+0.5))

IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
  BSFUELWT(1)=DBLE(INT((SFUELWT(1)*100.)+0.5))
  BSFUELWT(2)=DBLE(INT((SFUELWT(2)*100.)+0.5))
  BSFUELWT(3)=DBLE(INT((SFUELWT(3)*100.)+0.5))
  BSFUELWT(4)=DBLE(INT((SFUELWT(4)*100.)+0.5))
  WRITE(SQLStmtStr,*)'INSERT INTO FVS_PotFire_East (CaseID,', &
        'StandID,Year,Flame_Len_Sev,Flame_Len_Mod,', &
        'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,', &
        'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,', &
        'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1_mod,Fuel_Mod2_mod,', &
        'Fuel_Mod3_mod,Fuel_Mod4_mod,Fuel_Wt1_mod,Fuel_Wt2_mod,', &
        'Fuel_Wt3_mod,Fuel_Wt4_mod,Fuel_Mod1_Sev,', &
        'Fuel_Mod2_Sev,Fuel_Mod3_Sev,Fuel_Mod4_Sev,', &
        'Fuel_Wt1_Sev,Fuel_Wt2_Sev,Fuel_Wt3_Sev,', &
        'Fuel_Wt4_Sev) VALUES (''',CASEID,''',''',TRIM(NPLT), &
        ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,? ', &
        ',?,?,?,?);'
ELSE
  WRITE(SQLStmtStr,*)'INSERT INTO FVS_PotFire (CaseID,', &
        'StandID,Year,Surf_Flame_Sev,Surf_Flame_Mod,', &
        'Tot_Flame_Sev,Tot_Flame_Mod,Fire_Type_Sev,Fire_Type_Mod,', &
        'PTorch_Sev,PTorch_Mod,Torch_Index,Crown_Index,', &
        'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,', &
        'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,', &
        'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1,Fuel_Mod2,', &
        'Fuel_Mod3,Fuel_Mod4,Fuel_Wt1,Fuel_Wt2,', &
        'Fuel_Wt3,Fuel_Wt4) VALUES (''',CASEID,''',''',TRIM(NPLT), &
        ''',?,?,?,?,?,''',TRIM(SFTYPE),''',''',TRIM(MFTYPE), &
        ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
ENDIF

iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))

IF (iRet.NE.0) THEN
   iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
   print *,"FVS_PotFire prepare error:",Msg(:iRet)
   IPOTFIRE = 0
   RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMSU)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMSU)

IF ((VARACD .NE. 'SN') .AND. (VARACD .NE. 'CS')) THEN
  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMTO)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMTO)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPTRCH)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPTRCH)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BTORCHI)

  ColNumber=ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BCROWNI)
ENDIF

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref, ColNumber,CNPYHT)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BCNPYDNST)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTBA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTBA)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTVOL)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTVOL)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPSMOKE)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPSMOKE)

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(1))

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(2))

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(3))

ColNumber=ColNumber+1
iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(4))

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(1))

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(2))

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(3))

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(4))

IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
 ColNumber=ColNumber+1
 iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(1))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(2))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(3))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(4))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(1))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(2))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(3))

 ColNumber=ColNumber+1
 iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(4))

ENDIF

iRet = fsql3_step(IoutDBref)
if (iRet.ne.0) then
   iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
   print *,"FVS_PotFire step error:",Msg(:iRet)
   IPOTFIRE = 0
ENDIF
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
   print *,"FVS_PotFire finalize error:",Msg(:iRet)
   IPOTFIRE = 0
ENDIF
RETURN

END



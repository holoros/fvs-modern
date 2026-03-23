SUBROUTINE DBSFMSSNAG(IYEAR,NPLT,HCL1,HCL2,HCL3,HCL4,HCL5,HCL6, &
     HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF,KODE)

IMPLICIT NONE
!
! DBSQLITE $Id$
!
!     PURPOSE: TO POPULATE A DATABASE WITH THE SUMMARY SNAG REPORT
!              INFORMATION
!     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
!     INPUT:
!              THE SUMMARY SNAG OUTPUT FROM THE FIRE MODEL.
!              1: HARD SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
!              2: HARD SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
!              3: HARD SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
!              4: HARD SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
!              5: HARD SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
!              6: HARD SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
!              7: HARD SNAGS - TOTAL       (> 0)
!              8: SOFT SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
!              9: SOFT SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
!             10: SOFT SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
!             11: SOFT SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
!             12: SOFT SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
!             13: SOFT SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
!             14: SOFT SNAGS - TOTAL       (> 0)
!             15: HARD+SOFT  - TOTAL       (> 0)
!             16: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
!
!     NOTE: THE DBH CLASS BREAKS CAN BE CHANGED BY THE SNAGCLAS KEYWORD
!
!OMMONS
!
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

INTEGER IYEAR,KODE,iRet,ColNumber
REAL HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,HCL7,SCL1,SCL2,SCL3,SCL4,SCL5, &
     SCL6,SCL7,HDSF
DOUBLE PRECISION HCL1B,HCL2B,HCL3B,HCL4B,HCL5B,HCL6B,HCL7B, &
     SCL1B,SCL2B,SCL3B,SCL4B,SCL5B,SCL6B,SCL7B,HDSFB
CHARACTER*2000 SQLStmtStr
CHARACTER(len=26) NPLT

integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step, &
           fsql3_prepare,fsql3_bind_double,fsql3_finalize


IF(ISSUM.EQ.0) RETURN
IF(ISSUM.EQ.2) KODE = 0

CALL DBSCASE(1)

iRet = fsql3_tableexists(IoutDBref, &
          "FVS_SnagSum"//CHAR(0))
IF(iRet.EQ.0) THEN
    SQLStmtStr='CREATE TABLE FVS_SnagSum('// &
                 'CaseID text not null,'// &
                 'StandID text not null,'// &
                 'Year Int null,'// &
                 'Hard_snags_class1 real null,'// &
                 'Hard_snags_class2 real null,'// &
                 'Hard_snags_class3 real null,'// &
                 'Hard_snags_class4 real null,'// &
                 'Hard_snags_class5 real null,'// &
                 'Hard_snags_class6 real null,'// &
                 'Hard_snags_total  real null,'// &
                 'Soft_snags_class1 real null,'// &
                 'Soft_snags_class2 real null,'// &
                 'Soft_snags_class3 real null,'// &
                 'Soft_snags_class4 real null,'// &
                 'Soft_snags_class5 real null,'// &
                 'Soft_snags_class6 real null,'// &
                 'Soft_snags_total  real null,'// &
                 'Hard_soft_snags_total real null);'//CHAR(0)
   iRet = fsql3_exec(IoutDBref,SQLStmtStr)
   IF (iRet .NE. 0) THEN
     ISSUM = 0
     RETURN
   ENDIF
ENDIF
!
!     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
!
HCL1B = HCL1
HCL2B = HCL2
HCL3B = HCL3
HCL4B = HCL4
HCL5B = HCL5
HCL6B = HCL6
HCL7B = HCL7
SCL1B = SCL1
SCL2B = SCL2
SCL3B = SCL3
SCL4B = SCL4
SCL5B = SCL5
SCL6B = SCL6
SCL7B = SCL7
HDSFB = HDSF

WRITE(SQLStmtStr,*)'INSERT INTO FVS_SnagSum (CaseID,', &
     'StandID,Year,Hard_snags_class1,Hard_snags_class2,', &
     'Hard_snags_class3,Hard_snags_class4,Hard_snags_class5,', &
     'Hard_snags_class6,Hard_snags_total,Soft_snags_class1,', &
     'Soft_snags_class2,Soft_snags_class3,Soft_snags_class4,', &
     'Soft_snags_class5,Soft_snags_class6,Soft_snags_total,', &
     'Hard_soft_snags_total) VALUES(''',CASEID,''',''',TRIM(NPLT), &
     ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

iRet = fsql3_prepare(IoutDBref, SQLStmtStr)

IF (iRet .NE. 0) THEN
   ISSUM = 0
   RETURN
ENDIF

ColNumber=1
iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL1B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL2B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL4B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL5B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL6B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL7B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL1B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL2B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL3B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL4B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL5B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL6B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL7B)

ColNumber=ColNumber+1
iRet = fsql3_bind_double(IoutDBref,ColNumber,HDSFB)

iRet = fsql3_step(IoutDBref)
iRet = fsql3_finalize(IoutDBref)
if (iRet.ne.0) then
   ISSUM = 0
ENDIF
RETURN

END


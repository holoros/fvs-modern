SUBROUTINE SUMHED
IMPLICIT NONE
!----------
! METRIC-VBASE $Id: sumhed.f 2494 2018-09-12 20:34:17Z lancedavid $
!----------
!  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
!  SCREEN.
!----------
!
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'OUTCOM.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
INCLUDE 'SCREEN.f90'
!
!
!OMMONS
!
!----------
!  VARIABLE DECLARATIONS:
!----------
!
CHARACTER*7 FMT
CHARACTER REV*10
!
CHARACTER*2 CRMT(5)
!
INTEGER I1,I2,ISTFNB,ISTLNB
!
!----------
!  DEFINITION OF LOCAL VARIABLES:
!----------
!     CRMT -- CENTRAL ROCKIES ALPHABETIC MODEL TYPE DESIGNATOR
!----------
!  DATA STATEMENTS
!----------
DATA CRMT/'SM','SP','BP','SF','LP'/
!----------
IF (.NOT.LSCRN) GOTO 1000
!----------
!  GET VARIANT REVISION DATE.
!----------
CALL REVISE (VARACD,REV)
!
SELECT CASE (VARACD)
!
CASE ('CR')
  WRITE(JOSCRN,1) CRMT(IMODTY),REV
1   FORMAT(/T19,'CR-',A2,' FVS METRIC VARIANT -- RV:',A10/)
!
CASE DEFAULT
  WRITE(JOSCRN,2) VARACD,REV
2   FORMAT(/T19,A2,' FVS METRIC VARIANT -- RV:',A10/)
!
END SELECT
!
WRITE(JOSCRN,5) NPLT,MGMID
5 FORMAT(/T9,'STAND = ',A26,'  MANAGEMENT CODE = ',A4/)
I1=ISTFNB(ITITLE)
IF (I1.GT.0) THEN
   I2=ISTLNB(ITITLE)
   WRITE (FMT,'(''(T'',I2.2,'',A)'')') (81-I2+I1)/2
   WRITE (JOSCRN,FMT) ITITLE(I1:I2)
ENDIF
!
SELECT CASE (VARACD)
!----------
!  EASTERN VARIANTS
!----------
CASE ('CS','LS','NE','SN')
  WRITE (JOSCRN,10)
10   FORMAT (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)' &
     /,76('-')/ &
     T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',5X, &
     'AFTER TREATMENT  GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'), &
     '  CU M',/, T6,'TREES',9X,'TOP',6X,'MERCH TREES MERCH SAWLG', &
     9X,'TOP',6X,'PER YR',/,'YEAR  /HA  BA  SDI HT  QMD', &
     'CU  M  /HA  CU  M BD  M  BA SDI  HT  QMD ACC MOR',/, &
     '---- ----- --- --- --- ---- ----- ----- ----- ----- ', &
     '--- --- --- ---- --- ---')
!----------
!  CANADIAN VARIANTS
!----------
CASE ('BC','ON')
  WRITE (JOSCRN,15)
15   FORMAT (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)' &
     /,76('-')/ &
     T7,'START OF SIMULATION PERIOD    REMOVALS/HA  ',3X, &
     'AFTER TREATMENT   GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'), &
     '    CU M',/, T6,'TREES',9X,'TOP',6X,'  GTV TREES   GTV   GMV', &
     9X,'TOP',6X,' PER YR',/,'YEAR   /HA  BA SDI  HT  QMD ', &
     ' CU M   /HA  CU M  CU M  BA SDI  HT  QMD ACC MOR',/, &
     '---- ----- --- --- --- ---- ----- ----- ----- ----- ', &
     '--- --- --- ---- --- ---')
!----------
!  WESTERN AND SOUTHERN VARIANTS
!----------
CASE DEFAULT
  WRITE (JOSCRN,20)
20   FORMAT (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)' &
     /,76('-')/ &
     T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',4X, &
     'AFTER TREATMENT GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'), &
     '  CU M',/, T6,'TREES',9X,'TOP',6X,'TOTAL TREES TOTAL MERCH', &
     9X,'TOP',6X,'PER YR',/,'YEAR /HA    BA SDI  HT  QMD ', &
     '  CU M/HA    CU M  CU M  BA SDI  HT  QMD ACC MOR',/, &
     '---- ----- --- --- --- ---- ----- ----- ----- ----- ', &
     '--- --- --- ---- --- ---')
!
END SELECT
!
1000 CONTINUE
!
RETURN
END

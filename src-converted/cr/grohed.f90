SUBROUTINE GROHED (IUNIT)
IMPLICIT NONE
!----------
! CR $Id$
!----------
!     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM
!----------
!OMMONS
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'INCLUDESVN.f90'
!
!
INCLUDE 'PLOT.f90'
!
! COMMONS
!----------
INTEGER IUNIT
CHARACTER DAT*10,TIM*8,REV*10,SVN*8
!----------
!     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.
!----------
CALL REVISE (VARACD,REV)
!----------
!     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.
!----------
CALL GRDTIM (DAT,TIM)
!----------
! BRANCH TO APPROPRIATE MODEL TYPE
!----------
SELECT CASE (IMODTY)
!
!********************************************
! SOUTHWEST MIXED CONIFER TYPE             **
!********************************************
!
  CASE (1)
  WRITE (IUNIT,140) SVN,REV,DAT,TIM
140   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CEN. ROCKIES SW MIXED CONIFERS GENGYM', &
     T97,'RV:',A,T112,A,2X,A)
!
!**********************************************
! SOUTHWEST PONDEROSA PINE TYPE              **
!**********************************************
!
  CASE (2)
  WRITE (IUNIT,240) SVN,REV,DAT,TIM
240   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CEN. ROCKIES SW PONDEROSA PINE GENGYM', &
     T97,'RV:',A,T112,A,2X,A)
!
!*********************************************
! BLACK HILLS PONDEROSA PINE TYPE           **
!*********************************************
!
  CASE (3)
  WRITE (IUNIT,340) SVN,REV,DAT,TIM
340   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CEN. ROCKIES BLACK HILLS/NEBR GENGYM', &
     T97,'RV:',A,T112,A,2X,A)
!
!*******************************************
! SPRUCE-FIR TYPE                         **
!*******************************************
!
  CASE (4)
  WRITE (IUNIT,440) SVN,REV,DAT,TIM
440   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CEN. ROCKIES SPRUCE-FIR GENGYM        ', &
     T97,'RV:',A,T112,A,2X,A)
!
!*******************************************
! LODGEPOLE PINE TYPE                     **
!*******************************************
!
  CASE (5)
  WRITE (IUNIT,540) SVN,REV,DAT,TIM
540   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CEN. ROCKIES LODGEPOLE PINE GENGYM    ', &
     T97,'RV:',A,T112,A,2X,A)
!
!****************************************************
! MODEL TYPE 0 -- HASN'T BEEN SET YET (AT BEGINNING OF KEYWORD LISTING)
! ALSO FOR FUTURE MODEL TYPES INCLUDING ASPEN     **
!****************************************************
!
  CASE DEFAULT
  WRITE (IUNIT,50) SVN,REV,DAT,TIM
50   FORMAT (//T6,'FOREST VEGETATION SIMULATOR', &
     5X,'VERSION ',A,' -- CENTRAL ROCKIES                         ', &
     T97,'RV:',A,T112,A,2X,A)
!
END SELECT
!
RETURN
END

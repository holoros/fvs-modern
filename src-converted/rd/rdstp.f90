SUBROUTINE RDSTP (ISL,IISP,DEN,DIAM,RTD)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  ADD INSIDE INFECTED STUMPS TO STUMP ARRAYS. ONLY ADD THE INFECTED PORTION
!     OF THE ROOT SYSTEM TO THE ROOT ARRAYS.
!
!  CALLED BY :
!     RDPRIN  [ROOT DISEASE]
!     RDEND   [ROOT DISEASE]
!     RDIN    [ROOT DISEASE]
!     RDSADD  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  PARAMETERS :
!     ISL    - (I ) Stump size class.
!     IISP   - (I ) Tree species of current tree record.
!     DEN    - (I ) Number of infected trees in current tree record
!                   that were cut.
!     DIAM   - (I ) Diameter of stump.
!     RTD    - (I ) Root radius of the live tree
!     ROTD   - (I ) Infected root radius of the stump
!
!  Revision History:
!     28-JUN-2002 Lance R. David (FHTET)
!        Previous revision date noted was March 2, 1995.
!        Changed (TST + 1E-6) to just TST in equations below because it
!        is not possible for TST to be zero at this point.
!   09/03/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... PARAMETER INCLUDE FILES
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
!
!.... COMMON INCLUDE FILES
!
INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDADD.f90'
!
!.... Local variables
!
LOGICAL DEBUG
INTEGER IDI, IISP, IS, ISL, IST
REAL    DEN, DIAM, RTD, TST, ROTD
!
!.... Check for DEBUG.
!
CALL DBCHK(DEBUG,'RDSTP',5,ICYC)
IF (DEBUG) WRITE (JOSTND,100) ICYC,ISL,IISP,DEN,DIAM,RTD
100 FORMAT (' Begin RDSTP : ICYC, ISL, IISP, DEN, DIAM, RTD = ', &
             I5, I5, I5, F8.3, F8.3, F8.3)

!     EXIT ROUTINE IF HAVE NO INFECTED STUMPS TO ADD

IF (DEN .LE. 0.0) RETURN

IDI = MAXRR
IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(IISP))
!
!     ALLOW DISEASE TYPE TO BE USER SET UPON INITIALIZATION
!
IF (ISTFLG .EQ. 1) IDI = IRRSP

IST = INT(AMAX0(1,ISTEP))
IS  = ISPS(IRTSPC(IISP))
ROTD = RTD * PCOLO(IRTSPC(IISP),IDI)
TST = PROBDA(IDI,IS,ISL,IST) + DEN

IF (DEBUG) WRITE (JOSTND,200) IST,IS,ROTD,TST
200 FORMAT (' In RDSTP : IST, IS, ROTD, TST = ', &
             2(' ',I5), 3(' ', F8.3))

!     changed (TST + 1E-6) to just TST in equations below because it
!     is not possible for TST to be zero at this point. lrd 28-JUN-02
!
!     DBHDA and ROOTDA is being accumulated as a weighted average
!     of what is already in the class and what is being added.

DBHDA(IDI,IS,ISL,IST) = ((DBHDA(IDI,IS,ISL,IST) * &
           PROBDA(IDI,IS,ISL,IST)) + (DIAM * DEN)) / TST
ROOTDA(IDI,IS,ISL,IST) = ((ROOTDA(IDI,IS,ISL,IST) * &
           PROBDA(IDI,IS,ISL,IST)) + (ROTD * DEN)) / TST
!     JRAGED(IDI,IS,ISL,IST) = IYEAR
PROBDA(IDI,IS,ISL,IST) = PROBDA(IDI,IS,ISL,IST) + DEN

IF (DEBUG) WRITE (JOSTND,900) IDI,IS,ISL,IST
900 FORMAT (' In RDSTP : IDI, IS, ISL, IST = ', 4I5)
IF (DEBUG) WRITE (JOSTND,901) DBHDA(IDI,IS,ISL,IST), &
            ROOTDA(IDI,IS,ISL,IST), PROBDA(IDI,IS,ISL,IST)
901 FORMAT (' End RDSTP : DBHDA, ROOTDA, PROBDA = ', 3(' ',F10.5))

RETURN
END

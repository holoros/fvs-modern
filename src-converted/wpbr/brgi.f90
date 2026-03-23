SUBROUTINE BRGI(IIAG,HHT,GIBR,TBSUM)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRGI calculates the growth index and sum target for individual white
!  pine trees (currently used for all species).
!  Minimum age of tree is set at 2 years for GI calculation.
!----------------------------------------------------------------------
!
!  IIAG - Tree age
!  HHT  - Tree height in meters
!  GIBR - Growth Index function result
!  TBSUM - Target area
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  14-SEP-2000 Lance David (FHTET)
!     No changes actually made to this routine that calculates growth
!     index for white pine trees. The July, 2000 update to expand the
!     model to function on additional pine species did not address the
!     differences between species; therefore, it will likely be
!     necessary to develop species-specific sections and add species to
!     the argument list. FVS variant-specific versions of this may also
!     be necessary or desirable for future development.
!  28-FEB-2001 Lance R. David (FHTET)
!     Minimum tree age used in GI calculation set at 2 years.
!     Explicitly defined variables.
!  22-MAR-2001 Lance R. David (FHTET)
!     Added debug statment. Removed "0.0" preceding negative values in
!     GI functions. Don't know why they were there in the first place,
!     but may have been needed for some earlier version compiler.
!  03-MAY-2001 Lance R. David (FHTET)
!     Disabled stand target area summing because it was already being
!     done in BRTREG and BRTARG.
!  07-NOV-2002 Lance R. David (FHTET)
!     Added max and min limits of 50 and 125 on growth index GIBR.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'

!.... Local variable declarations.

LOGICAL DEBUG
INTEGER IIAG, GIAGE, K
REAL    HHT, GIBR, GIBRK, HITE, SVAL, TBSUM

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRGI',4,ICYC)
IF(DEBUG) WRITE(JOSTND,111) ICYC,IIAG,HHT
111 FORMAT('Entering subroutine BRGI: cycle = ',I2,/, &
          27X,'IIAG=',I3,' HHT=',F6.2)

TBSUM=0.0

!.... Calculate growth index.  This equation taken from the abstract:
!.... "Measuring Early Performance of Second Generation Resistance to
!.... Blister Rust in White Pine"; Geral McDonald, et.al.
!.... Per Geral's recommendation, minimum age is set at 2 years.

IF(IIAG .LE. 2) THEN
  GIAGE = 2
ELSE
  GIAGE = IIAG
ENDIF

GIBR=0.466*(HHT-0.05)* &
        ((1-(1.024494*EXP((-0.024202)*GIAGE)))**(-2.071822))

!.... Min (15.24m = 50ft*.3048) and max 38.10m = 125ft limits imposed
!.... on growth index. feet to meter conversion is m=ft*0.3048.

IF(GIBR .LT. 15.24) GIBR = 15.24
IF(GIBR .GT. 38.10) GIBR = 38.10

IF(DEBUG) WRITE(JOSTND,*) ' IN BRGI: GIBR=',GIBR
!.... Calculate stand target area.

DO 40 K=1,IIAG
   GIBRK=0.466*((1-(1.024494*EXP((-0.024202)*K)))** &
         (-2.071822))
   HITE=0.05+GIBR/GIBRK

   CALL BRSTAR(HITE,SVAL)
!****    STSUM=STSUM+SVAL        !!! already occuring in BRTREG and BRTARG
   TBSUM=TBSUM+SVAL
40 CONTINUE

!.... Common return.

IF(DEBUG) WRITE(JOSTND,113) ICYC
113 FORMAT('Leaving subroutine BRGI: cycle = ',I2)
RETURN
END

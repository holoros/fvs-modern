SUBROUTINE BRCINI(IBRNO,HITE)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRCINI uses the canker counts read from the canker data list
!  (if the CANKDATA keyword is used - there is a required record that
!  contains the canker count for the tree and has 0's for up, out, and
!  girdle) to initialize canker and infection conditions for each tree.
!  This routine randomly generates up, out, and %girdle measurements
!  for any number of cankers up to 10 or up to the canker count,
!  whichever is less, minus the number of cankers already loaded from
!  the actual canker data.
!----------------------------------------------------------------------
!  Parameters
!     IBRNO  - index for current tree
!     HITE   - current tree height (in meters)
!----------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.
INTEGER IBRNO, ITEMPC, J, NUMCNK
REAL    HITE, HITEBC, PBOLE, TOUT, TUP, XRAN, YRAN, ZRAN, CRLEN
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRCINI',6,ICYC)
IF(DEBUG) WRITE(JOSTND,23) ICYC
23 FORMAT('Entering subroutine BRCINI: cycle = ',I2)

!.... Calculate crown length in centimeters.
!.... Height to base of crown is already in centimeters but HITE
!.... is in meters.

HITEBC=BRHTBC(IBRNO)
CRLEN=(HITE*100.0)-HITEBC

!.... Determine the number of cankers to be generated.  Take the total
!.... count, which has been stored already in ITCAN, subtract the lethal
!.... count, stored already in ILCAN (in subroutine BRCANK) and then
!.... add that many cankers, up to 10 total.  Note: ITCAN already has
!.... the correct total canker count; we may add cankers to ILCAN if
!.... it's not already full and if ITCAN is greater that ILCAN.

NUMCNK=ITCAN(IBRNO)-ILCAN(IBRNO)

!.... Loop through for number of cankers, if any, and stop at 10.

IF(NUMCNK.GT.0) THEN
   DO 300 J=1,NUMCNK

!....       Check to see if the lethal canker array is already full.

      IF(ILCAN(IBRNO).LT.10) THEN
         ILCAN(IBRNO)=ILCAN(IBRNO)+1
         ITEMPC=ILCAN(IBRNO)

!....          Generate up and out positions for added canker.
!....          Up position can be anywhere on the tree, including
!....          below the base on the crown, whereas out will only be
!....          generated for cankers above the base of the crown.

         CALL BRANN(XRAN)
         TUP=100*HITE*XRAN
         IF(TUP.LT.HITEBC) THEN
            TOUT=0.0
         ELSE
            TOUT=(35*SQRT(HITE)*(100*HITE-TUP)/CRLEN)*XRAN
         ENDIF

!....          If out < 50 then the canker is possibly lethal--different
!....          probability than if the canker is farther out than 50 cm.
!....          This stuff came from BRECAN.

         IF(TOUT.LT.50.0) THEN
            PBOLE=0.97-0.0158*TOUT
         ELSE
            PBOLE=35.4/TOUT**(1+(0.35*TOUT/50))
         ENDIF
         IF(PBOLE.LT.0.0) PBOLE=0.0

!....          Call random number generator. If PBOLE >= number then
!....          create a bole canker, otherwise create a branch canker.

         CALL BRANN(YRAN)
         IF(PBOLE.GE.YRAN) THEN

!....             Bole canker.

            DOUT(ITEMPC,IBRNO)=0.0

!....             Call random number generator again and generate
!....             a % girdle measurement.  Don't want to kill trees
!....             in the first cycle so make sure girdling is never
!....             more than 50%.

            CALL BRANN(ZRAN)
            GIRDL(ITEMPC,IBRNO)=ZRAN*50.0
         ELSE

!....             Branch canker.

            DOUT(ITEMPC,IBRNO)=TOUT
            GIRDL(ITEMPC,IBRNO)=0.0
         ENDIF

!....          Set distance up and canker status.  Canker status will
!....          be reset in routine BRCSTA.

         DUP(ITEMPC,IBRNO)=TUP
         ISTCAN(ITEMPC,IBRNO)=0
      ENDIF
300    CONTINUE
ENDIF

!.... Common return.

IF(DEBUG) WRITE(JOSTND,330) ICYC
330 FORMAT('Leaving subroutine BRCINI: cycle = ',I2)
RETURN
END

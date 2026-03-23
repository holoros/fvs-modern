SUBROUTINE RDMREC (ITYP,I,KSP,OAMOVE)
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  Purpose :
!     This subroutine re-arranges other agent mortality so that when
!     uninfected trees become infected or just inside a center, the
!     appropriate other agent mortality will still be applied.
!
!  Called By :
!     RDINF   [ROOT DISEASE]
!     RDINSD  [ROOT DISEASE]
!
!  Calls :
!     RDSSIZ   (SUBROUTINE)   [ROOT DISEASE]
!     RDSTP    (SUBROUTINE)   [ROOT DISEASE]
!
!  Revision History :
!     06/12/96 - Matthew K. Thompson
!                Moved the declaration of DSO, DSII, and DSIU to the
!                parameter include file RDPARM.
!     10/07/97 - Matthew K. Thompson
!                Commented out the line of code that sets
!                PROBI = PROBI - OAMOVE(DSII)
!                The other agent mortality has already been deleted
!                from PROBI.
!   08/29/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!


!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'

!.... Coomon include fules.

INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDARRY.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'RDADD.f90'

!.... Local variable declaraions.

INTEGER  I, ISL, ITYP, KSP
REAL     OAMOVE(3)
INTEGER IDANUW

!.... Make sure that OAKL continues to contain the right number of
!.... killed trees. Assume that some proportion of those trees that
!.... were outside killed are now inside killed (either infected or
!.... uninfected) (eg. If 10% of the outside trees were killed from
!.... bark beetles or windthrow then 10% of those trees just becoming
!.... inside trees were killed from bark beetles or windthrow).
!----------
!  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
!----------
IDANUW = ITYP
!
OAKL(DSO,I) = OAKL(DSO,I) - OAMOVE(DSO)
OAKL(DSII,I) = OAKL(DSII,I) + OAMOVE(DSII)
OAKL(DSIU,I) = OAKL(DSIU,I) + OAMOVE(DSIU)

IF (OAMOVE(DSIU) .LT. 0.0 .AND. OAKL(DSIU,I) .GT. 0.0) THEN

!....    New OAKL goes from DSIU to DSII.

   BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) * &
                                    (BBKILL(DSIU,I) / OAKL(DSIU,I))
   BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) * &
                                    (BBKILL(DSIU,I) / OAKL(DSIU,I))

ELSEIF (OAMOVE(DSIU) .GE. 0.0 .AND. OAKL(DSO,I) .GT. 0.0) THEN

!....    New OAKL goes from DSO to DSII and DSIU.

   BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) * &
                                      (BBKILL(DSO,I) / OAKL(DSO,I))
   BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) * &
                                      (BBKILL(DSO,I) / OAKL(DSO,I))
   BBKILL(DSO,I) = BBKILL(DSO,I) - (BBKILL(DSO,I) / OAKL(DSO,I)) * &
                                      OAMOVE(DSO)
ENDIF

!.... Bark beetle and windthrown trees are already removed from PROBI.
!.... This line of code is double counting the removal.
!     PROBI(I,ISTEP,ITYP) = PROBI(I,ISTEP,ITYP) - OAMOVE(DSII)

RRKILL(I) = RRKILL(I) + OAMOVE(DSII)

CALL RDSSIZ(KSP,DBH(I),STCUT,ISL,ISPS,IRTSPC)
CALL RDSTP (ISL,KSP,OAMOVE(DSII),DBH(I),ROOTL(I))


RETURN
END

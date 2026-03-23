SUBROUTINE BRSOR
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!   This subroutine is called to sort and match tree IDs to insure
!   proper mapping of Blister Rust Model canker data and FVS Model
!   tree data.  This subroutine is called by TRESOR which is called
!   from the the PROCESS KEYWORD section in INITRE.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  09-APR-2001 Lance R. David
!     Update for stock type that can now be supplied through canker file.
!  09-MAY-2006 Lance R. David
!     Added exit if blister rust not active.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'BRCOM.f90'

!.... Local variable declarations.

INTEGER IDTR, IID, STK, IC1, IC2, IERR, IPOS, J, K, L, N
REAL    AGE, DO1, DU1, GI1
LOGICAL DEBUG

!.... Exit if Blister Rust not active
IF(.NOT. BRYES) GOTO 300

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRSOR',5,ICYC)
IF(DEBUG) WRITE(JOSTND,11) ICYC
11 FORMAT('Entering subroutine BRSOR: cycle = ',I2)

!.... Sort the FVS list of tree IDs.

CALL I4PSRT(IREC1,IDTREE,IND1,.TRUE.)

!.... Initializations.

IERR=0
K=1

!.... Top of BRUST tree loop. If the tree count is greater than the
!.... number of unique BRUST tree IDs, then skip out.

100 CONTINUE
IF(K.GT.INCAN) GO TO 200
IDTR=IBRTID(K)

!.... Invalid BRUST tree ID.

IF(IDTR.LE.0) THEN
   K=K+1
   GO TO 100
ENDIF

!.... Call binary search routine to find the position of the BRUST
!.... tree ID that matches the FVS tree ID.

CALL I4PBSR(IREC1,IDTREE,IND1,IDTR,IPOS)

!.... If tree ID cannot be found in the canker file that matches the
!.... tree ID in the tree data file, then delete the canker info.

IF(IPOS.LE.0) THEN
   IERR=IERR+1
   DO 123 J=1,10
      DUP(J,K)=0.0
      DOUT(J,K)=0.0
      GIRDL(J,K)=0.0
123    CONTINUE
   ILCAN(K)=0
   ITCAN(K)=0
   IBRTID(K)=0
   BRAGE(K)=0.0
   ISTOTY(K)=0
   K=K+1
   GO TO 100
ENDIF

!.... If cankers are already in their proper position based on the
!.... input data (a chance occurrance) then go on to next canker.

IF(IPOS.EQ.K) THEN
   K=K+1
   GO TO 100
ENDIF

!.... Swap canker data.

N=MAX(ILCAN(K),ILCAN(IPOS))
DO 150 L=1,N

!....    Store contents of identified position in temporary variables.

   DU1=DUP(L,IPOS)
   DO1=DOUT(L,IPOS)
   GI1=GIRDL(L,IPOS)

!....    Load BRUST values into FVS position.

   DUP(L,IPOS)=DUP(L,K)
   DOUT(L,IPOS)=DOUT(L,K)
   GIRDL(L,IPOS)=GIRDL(L,K)

!....    Load temporary variables into position vacated by
!....    "Load BRUST values . . "

   DUP(L,K)=DU1
   DOUT(L,K)=DO1
   GIRDL(L,K)=GI1
150 CONTINUE

!.... Free up indicated position by saving the values.

IC1=ILCAN(IPOS)
IC2=ITCAN(IPOS)
IID=IBRTID(IPOS)
AGE=BRAGE(IPOS)
STK=ISTOTY(IPOS)

!.... Load appropriate values into indicated position.

ILCAN(IPOS)=ILCAN(K)
ITCAN(IPOS)=ITCAN(K)
IBRTID(IPOS)=IDTR
BRAGE(IPOS)=BRAGE(K)
ISTOTY(IPOS)=ISTOTY(K)

!.... Store values that used to be in position IPOS

ILCAN(K)=IC1
ITCAN(K)=IC2
IBRTID(K)=IID
BRAGE(K)=AGE
ISTOTY(K)=STK

!.... Process next tree ID.

GO TO 100

!.... Common return.

200 CONTINUE

!.... If there are IDs that have no match then print error message.

IF(IERR.GT.0) WRITE (JOSTND,250) IERR
250 FORMAT(/,'**** WARNING ****', I5,' Canker IDs have been found', &
         ' that have no match in the tree ID list.  These cankers' &
         ' will be ignored.')

IF(DEBUG) WRITE(JOSTND,255) ICYC
255 FORMAT('Leaving subroutine BRSOR: cycle = ',I2)
300 CONTINUE
RETURN
END

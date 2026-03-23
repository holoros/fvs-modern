SUBROUTINE BRSETP
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRSETP initializes the Blister Rust Model tree-specific variables
!  after FVS tree data and blister rust canker data has been read.
!  It is called from MAIN.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  14-SEP-2000 Lance David (FHTET)
!     Transfered Glen Brink's July, 2000 modifications from older version
!     of blister rust source code:
!     Modified to allow blister rust on other species using ISPBR array.
!     Species loop (label 110) and species temp index variable (I3)
!     are new.
!  02-APR-2001 Lance R. David (FHTET)
!     Deactivated initialization of stock type (ISTOTY) because it is
!     now a canker input file variable.
!  02-MAY-2001 Lance R. David (FHTET)
!     Added species dimension to BRNTRECS variable.
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM. Instead of just being and indicator of a
!     species being a host, BRSPM holds the array index value for that
!     species and is used to access all species-specific BR arrays.
!**********************************************************************

!.... Common include files

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'ARRAYS.f90'
INCLUDE 'PLOT.f90'

!.... Local variable declarations

INTEGER I1, I2, I3, I4, J, K
REAL    BRDBH, BRHT
LOGICAL DEBUG,BRGO

!.... See if we need to do some debug

CALL DBCHK (DEBUG,'BRSETP',6,ICYC)
IF(DEBUG) WRITE(JOSTND,111) ICYC
111 FORMAT('Entering subroutine BRSETP: cycle = ',I2)

!.... Call BRATV to see if the Blister Rust model is being used in this
!.... simulation. Return if not active or there are no trees.

CALL BRATV(BRGO)
IF(.NOT. BRGO .OR. ITRN .EQ. 0) GO TO 115

!.... Process host pines. If none then return.
!.... Start species loop

DO 110 I3 = 1, MAXSP
IF (BRSPM(I3) .EQ. 0) GO TO 110
I1=ISCT(I3,1)
IF(I1.EQ.0) GO TO 110
I2=ISCT(I3,2)

!.... Set blister rust species index
I4=BRSPM(I3)
BRNTRECS(I4)=I2-I1+1

DO 100 J=I1,I2
   K=IND1(J)

!....    Change height to meters, DBH to centimeters, calculate ground
!....    level diameter, and calculate height to base of crown in cm.
!....    If tree age not recorded, then assign stand age as tree age.
!....
!....    Assign stock type of existing inventory trees to 5 so they
!....    will be treated as planted trees during the assignment of
!....    stock types in subroutine BRSTYP.
!....
!....    Stock type is now a variable in the canker list input file.
!....    but still must be initialized here for the host trees that
!....    do not have a corresponding record in the canker file.
!....    (LRD 04/02/01)

   BRHT=HT(K)*0.3048
   BRDBH=DBH(K)*2.54
   BRGD(K)=(100*BRHT*BRDBH)/(100*(BRHT-1.14))
   IF(BRGD(K).LT.BRDBH) BRGD(K)=BRDBH
   IF(BRAGE(K).EQ.0.0) BRAGE(K)=IAGE
   IF(ISTOTY(K).EQ.0) ISTOTY(K)=5
   BRHTBC(K)=(BRHT-(BRHT*(FLOAT(ICR(K))/100.0)))*100.0

!....    Initialize canker and infection conditions for each tree
!....    based on canker counts read from canker data if requested
!....    on the CANKDATA keyword.

   IF(CKINIT) CALL BRCINI(K,BRHT)
100 CONTINUE

!.... End of species loop
110 CONTINUE

!.... Assign stock types to trees.
!.... Any trees that were not assigned a stock type via the canker
!.... input file will be rectified.

CALL BRSTYP

!.... Assign status to cankers.

CALL BRCSTA

!.... Calculate statistics.

CALL BRSTAT

!.... Reduce crowns of trees that have been girdled by cankers.

CALL BRCRED

!.... Calculate STAND RUST INDEX using basal area if specified by
!.... the rust index assignment method.

IF(RIMETH.NE.0) CALL BRIBA

!.... Calculate STAND TARGET, individual tree GROWTH INDEX,
!.... individual tree RUST INDEX, and STAND DEVIATION FACTOR.

CALL BRTARG

!.... Common return.

115 CONTINUE
IF(DEBUG) WRITE(JOSTND,120) ICYC
120 FORMAT('Leaving subroutine BRSETP: cycle = ',I2)
RETURN
END

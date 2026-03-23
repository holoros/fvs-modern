SUBROUTINE BRTDEL(IVAC,IREC)
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  This Blister Rust model subroutine is called from the FVS routine
!  TREDEL.  It deletes and packs the blister rust tree lists.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'CONTRL.f90'

!.... Local variable declarations.

INTEGER IVAC, IREC, J
LOGICAL LGO, DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRTDEL',6,ICYC)
IF(DEBUG) WRITE(JOSTND,15) ICYC
15 FORMAT('Entering subroutine BRTDEL: cycle = ',I2)

!.... Call BRATV to see if the Blister Rust Model is active for this
!.... simulation.  If not, then return.

CALL BRATV(LGO)
IF(.NOT.LGO) GO TO 1000

!.... Move the Blister Rust records.

UPMARK(IVAC)=UPMARK(IREC)
BRHTBC(IVAC)=BRHTBC(IREC)
GI(IVAC)=GI(IREC)
RI(IVAC)=RI(IREC)
BRGD(IVAC)=BRGD(IREC)
BRPB(IVAC)=BRPB(IREC)
ESTCAN(IVAC)=ESTCAN(IREC)
TSTARG(IVAC)=TSTARG(IREC)
ITCAN(IVAC)=ITCAN(IREC)
ILCAN(IVAC)=ILCAN(IREC)
BRAGE(IVAC)=BRAGE(IREC)
ISTOTY(IVAC)=ISTOTY(IREC)
IBRTID(IVAC)=IBRTID(IREC)
ICRED(IVAC)=ICRED(IREC)
IBRSTAT(IVAC)=IBRSTAT(IREC)

DO 999 J=1,10
   ISTCAN(J,IVAC)=ISTCAN(J,IREC)
   GIRDL(J,IVAC)=GIRDL(J,IREC)
   DUP(J,IVAC)=DUP(J,IREC)
   DOUT(J,IVAC)=DOUT(J,IREC)
999 CONTINUE

!.... Common return.

1000 CONTINUE
IF(DEBUG) WRITE(JOSTND,1010) ICYC
1010 FORMAT('Leaving subroutine BRTDEL: cycle = ',I2)
RETURN
END

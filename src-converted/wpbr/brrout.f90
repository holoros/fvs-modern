SUBROUTINE BRROUT
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRROUT writes a Blister Rust Model Stand Summary Statistics
!  table at the end of the FVS listing.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!  14-SEP-2000 Lance David (FHTET)
!     Modified table heading. Changed "White Pine" to "Pine", because
!     the model now recognizes other pine host species and all are
!     represented in this table.
!     Changed version number from 1.0 to 1.1
!  01-MAR-2001 Lance David (FHTET)
!     Change summary record format to allow additional decimals for
!     GI, RI, and Deviation Factor columns.
!  04-MAY-2001 Lance R. David (FHTET)
!     Added species loop to process and species dimension to arrays.
!     Changed version number from 1.1 to 1.2
!  09-MAY-2001 Lance R. David (FHTET)
!     Changed ISPBR to BRSPM. Instead of just being and indicator of a
!     species being a host, BRSPM holds the array index value for that
!     species and is used to access all species-specific BR arrays.
!  21-MAY-2001 Lance R. David (FHTET)
!     Added stand id line to heading and FVS common OUTCOM.F77
!  15-MAY-2006 Lance R. David (FHTET)
!     Added control from BROUT keyword.
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'OUTCOM.f90'

CHARACTER*3 BRVER
!.... Local variable declarations.
INTEGER I3, I4, J, K, ISTLNB
LOGICAL BRGO

!.... See if Blister Rust Model is active.  If not, then return.

CALL BRATV(BRGO)
IF(.NOT. BRGO .OR. ITRN .EQ. 0) GO TO 1000
IF(.NOT. LBRSUM) GO TO 855
BRVER='1.2'

!.... Write heading information.

WRITE(JOSTND,100)
WRITE(JOSTND,102) BRVER
WRITE(JOSTND,103)
WRITE (JOSTND,15) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
15 FORMAT('STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A)

!.... Begin species loop....................

DO 850 I3 = 1, MAXSP

IF (BRSPM(I3) .EQ. 0) GO TO 850
I4=BRSPM(I3)

!.... Do not print table that is all zeros.
IF(.NOT. LBROUT(I4)) GO TO 850

WRITE(JOSTND,104)
WRITE(JOSTND,105) BRSPC(I4)
WRITE(JOSTND,106)
WRITE(JOSTND,107)
WRITE(JOSTND,108)
WRITE(JOSTND,109)
WRITE(JOSTND,110)
WRITE(JOSTND,104)

100 FORMAT(//)
102 FORMAT(T36,'*  *  *  WHITE PINE BLISTER RUST MODEL:' &
      ' Version ',A3,'  *  *  *')
103 FORMAT(T42,'Stand Summary Statistics for Blister Rust Hosts')
104 FORMAT(131('-'))
! 105 FORMAT(T64,'PROPORTION')
105 FORMAT('WPBR HOST: ',A4,T64,'PROPORTION')
106 FORMAT(T48,'AVERAGE',9X,'TREES/ACRE')
107 FORMAT(T16,'AVERAGE',22X,'CANKERS/TREE',7X,'INF W/CANKS', &
      13X,'NUMBER OF TREES/ACRE (HOST PINE ONLY)')
108 FORMAT(7X,24('-'),2X,'STAND',2X,20('-'),3X,11('-'),2X,55('-'))
109 FORMAT(7X,'GROWTH',4X,'RUST',6X,'SUM',4X,'DEV',47X,'NON',19X, &
      'NON')
110 FORMAT('YEAR',4X,'INDEX',3X,'INDEX',4X,'TARGET',2X,'FACTOR', &
      4X,'ALL',3X,'LETH',4X,'EXP',4X,'ALL',2X,'LETH',3X,'CLEAN', &
      2X,'LETH',2X,'PRUNE',2X,'EXCISE',3X,'SALV',2X,'TOPKILL', &
      2X,'DEAD',2X,'TOTAL')

!.... Write summary data.

DO 800 K=1,NCYC+1
   WRITE(JOSTND,777) INT(BROUT(I4,1,K)),(BROUT(I4,J,K),J=2,18)
777    FORMAT(I4,1X,F8.4,F9.6,F9.2,F8.4,F7.2,2F7.2,F7.2, &
         F6.2,F8.0,F6.0,F7.0,F8.0,F7.0,F9.0,F6.0,F7.0)

!        format replaced 03/01/01
!        WRITE(JOSTND,777) (BROUT(J,K),J=1,18)
! 777    FORMAT(F6.0,F8.2,F8.4,F10.2,F6.2,F9.2,2F7.2,F7.2,F6.2,F8.0,
!    &      F6.0,F7.0,F8.0,F7.0,F9.0,F6.0,F7.0)
800 CONTINUE

!.... End of species loop.......................
850 CONTINUE

!.... Call the routine that writes the 2" DBH class table, if this
!.... needs to be an option to turn off and on someday, can do that
!.... here then.  Right now it will always write this table to the
!.... end of the FVS output listing - right after the summary table.
855 CONTINUE
IF(LBRDBH) CALL BRDOUT

!.... Common return.

1000 CONTINUE
RETURN
END

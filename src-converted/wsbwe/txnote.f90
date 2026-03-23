SUBROUTINE TXNOTE (LXNOTE)
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!
!     This module sets the logical flag LXNOTE to true when the
!     ??X.exe executables are being linked.  This flag causes,
!     the following warning to be written in the disease model output
!     (RNH May98). LXN_ON is set to true after the warning has been
!     written once during a model execution to prevent multiple messages
!     in one output file
!
!     TXNOTE is called by:
!     - BWIN  where LXNOTE is passed by BWCNTL.F77
!     - DFBIN where LXNOTE is passed by DFBCOM.F77
!     - MPBIN where LXNOTE is passed by MPBCOM.F77
!     - RRIN  where LXNOTE is passed by RRCOM.F77
!
LOGICAL LXNOTE, LXN_ON

IF (LXN_ON) THEN
   LXNOTE = .FALSE.
ELSE
   LXNOTE = .TRUE.
   LXN_ON = .TRUE.
ENDIF

RETURN
END

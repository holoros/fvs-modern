SUBROUTINE FXNOTE
IMPLICIT NONE
!----------
! WSBWE $Id$
!----------
!
!     This module satisfies the external reference to the module
!     TXNOTE when the disease extensions. other than the ??X.exe
!     executables are being linked (RNH May98)
!     TXNOTE is called by:
!     - BWIN  where LXNOTE is passed by BWCNTL.F77
!     - DFBIN where LXNOTE is passed by DFBCOM.F77
!     - MPBIN where LXNOTE is passed by MPBCOM.F77
!     - RRIN  where LXNOTE is passed by RRCOM.F77
!
LOGICAL LXNOTE

ENTRY TXNOTE (LXNOTE)
LXNOTE = .FALSE.
RETURN
END

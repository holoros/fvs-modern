SUBROUTINE BMPPPT (IPNT, ILIMIT)
!----------
! WWPB $Id$
!----------
!  Purpose:
!     Write the damage code information for the given stand.
!  This is part of the Parallel Processing Extension, and freely
!  adapted from MSPPPT.FOR. In this case, damage code information is
!  only useful from the initialization. Therefore, if the master cycle
!  is more than 2, the variable is not saved or restored.
!----------------------------------------------------------------------
!
!  Call list definitions:
!     ILIMIT: (I)  Size of print buffer WK3.
!     IPNT:   (IO) Pointer to curent element in print buffer WK3.
!
!  Local variable definitions:
!
!  Common block variables and parameters:
!     LBMDAM:   From BMCOM; TRUE if damage code in inventory
!     WK3:      FVS work array used as a print buffer.
!
!**********************************************************************

!.... Parameter statements.

PARAMETER (MXL=1, MXR=0, MXI=0)

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!.... Common include files.

INCLUDE 'ARRAYS.f90'
INCLUDE 'PPCNTL.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'BMCOM.f90'

!.... Variable declarations.

LOGICAL LOGICS(MXL), LX

!     WRITE THE LOGICAL SCALAR. THIS IS .TRUE. IF THE MASTER
!     CYCLE IS 2 OR LESS, .FALSE. OTHERWISE.

LX = (MICYC .LE. 2)

LOGICS (1) = LX
CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

!     WRITE THE LOGICAL ARRAYS.

IF (LX) CALL LFWRIT (WK3,IPNT,ILIMIT,LBMDAM,ITRN, 2)

RETURN
END

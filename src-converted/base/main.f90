PROGRAM MAIN
IMPLICIT NONE
!----------
! BASE $Id$
!----------
INTEGER rtnCode,lenCL,i
!
!     PROCSS THE COMMAND LINE. Passing an empty string signals that the
!     real command line arguments will be fetched.
!
lenCl = 0
CALL fvsSetCmdLine(' ',lenCL,rtnCode)
IF (rtnCode.NE.0) GOTO 10

!     RUN ALL THE CYCLES and STANDS--unless there is a stop point!

DO
  CALL FVS(rtnCode)
  IF (rtnCode .NE. 0) exit
ENDDO

10 CONTINUE

call fvsGetICCode(i)

SELECT CASE (i)
  CASE (0)
    STOP
  CASE (1)
    STOP 10
  CASE (2)
    STOP 20
  CASE (3)
    STOP 30
  CASE (4)
    STOP 40
  CASE (5)
    STOP 50
  CASE DEFAULT
    STOP
END SELECT
END


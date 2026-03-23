SUBROUTINE  RDINUP
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  THIS SUBROUTINE FORMS A WEIGHTED AVERAGE OF INFECTED ROOT
!  CHARACTERISTICS FOR THE ROOT DISEASE MODEL
!
!  CALLED BY :
!     RDCNTL  [ROOT DISEASE]
!     RDSPOR  [ROOT DISEASE]
!
!  CALLS     :
!     NONE
!
!  Revision History :
!   11/06/89 - Last revision date.
!   08/28/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!OMMONS
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'PLOT.f90'
!
!OMMONS
!
INTEGER  I, IDI, J, K

!*    WRITE(6,1)
!*  1 FORMAT(' STARTING RDINUP')

IDI=IRRSP

DO 500 I=1, 2
   DO 400 J=1,5
      PROBD(IDI,I,J)  = 0.0
      DBHD(IDI,I,J)   = 0.0
      ROOTD(IDI,I,J)  = 0.0

      DO 300 K=1, ISTEP
         IF (PROBDA(IDI,I,J,K) .LE. 0) GOTO 300
         PROBD(IDI,I,J)  = PROBD(IDI,I,J) + PROBDA(IDI,I,J,K)
         DBHD(IDI,I,J)   = DBHD(IDI,I,J) + DBHDA(IDI,I,J,K) * &
                              PROBDA(IDI,I,J,K)
         ROOTD(IDI,I,J)  = ROOTD(IDI,I,J) + ROOTDA(IDI,I,J,K) * &
                              PROBDA(IDI,I,J,K)
300       CONTINUE

      DBHD(IDI,I,J)   = DBHD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
      ROOTD(IDI,I,J)  = ROOTD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
400    CONTINUE
500 CONTINUE

!*    WRITE(6,2)
!*  2 FORMAT(' ENDING RDINUP')

RETURN
END

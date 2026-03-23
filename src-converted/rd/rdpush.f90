SUBROUTINE RDPUSH
IMPLICIT NONE
!----------
! RD $Id$
!----------
!
!  PROCESSES THE PSTUMP KEYWORD.
!
!  CALLED BY :
!     RDTREG  [ROOT DISEASE]
!
!  CALLS     :
!     OPFIND  (SUBROUTINE)   [PROGNOSIS]
!     OPGET   (SUBROUTINE)   [PROGNOSIS]
!     OPDONE  (SUBROUTINE)   [PROGNOSIS]
!
!  PARAMETERS :
!     NONE
!
!  COMMON BLOCK VARIABLES :
!     xxxxx:   From ANCOM;
!
!
!  LOCAL VARIABLES :
!
!  Revision History :
!   03/24/93 - Last revision date.
!   09/02/14 Lance R. David (FMSC)
!     Added implicit none and declared variables.
!
!----------------------------------------------------------------------
!
!.... PARAMETER INCLUDE FILES
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'RDPARM.f90'
!
!.... COMMON INCLUDE FILES
!
INCLUDE 'CONTRL.f90'
INCLUDE 'RDCOM.f90'
INCLUDE 'RDADD.f90'
!
!.... DATA statements
!
INTEGER  I, IACTK, IDI, J, K, KDT, MYACT(1), NPS, NTODO
REAL     DIAM, PROP, PRMS(2)

DATA MYACT /2403/

CALL OPFIND (1,MYACT,NTODO)
IF (NTODO .LE. 0) RETURN

CALL OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
CALL OPDONE (NTODO,IY(ICYC))
PROP = PRMS(1)
DIAM = PRMS(2)

DO 95 IDI=MINRR,MAXRR
   DO 100 I = 1,2
      DO 110 J = 1,5
         DO 115 K = 1,ISTEP
!
!                 PUSH STUMPS
!
!                 WRITE (IRUNIT,999) IDI, I, J, K, PROBDA(IDI,I,J,K),
!    &                  DBHDA(IDI,I,J,K),DIAM,PROP
! 999             FORMAT ('PUSHSTUMPS: ',4I5,2F10.5,2F10.5)
!
            IF (DBHDA(IDI,I,J,K) .GE. DIAM) &
                  PROBDA(IDI,I,J,K) = PROBDA(IDI,I,J,K) * (1 - PROP)
            IF (PROBDA(IDI,I,J,K) .NE. 0.0) GOTO 115
!
!                 IF ALL STUMPS WERE REMOVED, ZERO ARRAYS ASSOCIATED
!                 WITH STUMPS
!
            DBHDA(IDI,I,J,K)  = 0.0
            ROOTDA(IDI,I,J,K) = 0.0
            JRAGED(IDI,I,J,K) = 0

!*                DO 1010 J = 1,NCENTS(IDI)
!*                   PCENTS(IDI,J,3) = 0.0
!1010             CONTINUE
!*                NCENTS(IDI)=0

115          CONTINUE
110       CONTINUE
100    CONTINUE
95 CONTINUE

RETURN
END

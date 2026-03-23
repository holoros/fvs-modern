SUBROUTINE RDDBUG (ICODE)
IMPLICIT NONE
!----------
! RD $Id$
!---------
!
!  PRINT DEBUG INFORMATION BASED ON PARAMETER ICODE.
!
!  CALLED BY :
!     VARIOUS [ROOT DISEASE]
!
!  CALLS :
!     NONE
!
!  PARAMETERS :
!     ICODE  - (I ) which debug information to print; if 0, nothing
!                   is printed.
!
!  COMMON BLOCK VARIABLES :
!     ICYC:   From CONTRL; cycle index number.
!     IRUNIT: From ANCOM; unit number of annosus output.
!
!  LOCAL VARIABLES:
!     IDI:    Disease type (1, P type; 2, S type).
!     IS:     Tree type (1, heartwood; 2, non-heartwood).
!     ISL:    Stump class (1-5).
!     IST:    Time step (1-40).
!     PRSNT:  Array (IDI,IS): true if stumps of that type are
!             present in this cycle; controls printing.
!     HEADR:  Array (IDI,IS): headers for output tables.
!
!  Revision History :
!   03/18/93 - Last revision date.
!   08/28/14 Lance R. David (FMSC)
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
!
!.... Local variables
!
INTEGER ICODE, I, IDI, IS, ISL, IST, J
LOGICAL PRSNT(2,2)
CHARACTER*20 HEADR(2,2)
!
!.... DATA statements
!
DATA ((HEADR(I,J),J=1,2),I=1,1) &
              /'P-type Heartwood', &
               'P-type Non-Heartwood'/
DATA ((HEADR(I,J),J=1,2),I=2,2) &
              /'S-type Heartwood', &
               'S-type Non-Heartwood'/
!
!.... Clear, then set PRSNT, based on current cycle PROBDA
!
DO IDI=1,2
   DO IS=1,2
      PRSNT(IDI,IS)=.FALSE.
      DO ISL=1,5
         IF (PROBDA(IDI,IS,ISL,1).GT.0.0) &
               PRSNT(IDI,IS)=.TRUE.
      END DO
   END DO
END DO
!
!.... Check ICODE
!
IF (ICODE.EQ.0) GOTO 9000
IF (ICODE.EQ.1) THEN
   WRITE (IRUNIT,800)

   WRITE (IRUNIT,810)
   DO IDI=1,2
      DO IS=1,2
         IF (PRSNT(IDI,IS)) THEN
            WRITE (IRUNIT,850) HEADR(IDI,IS)
            WRITE (IRUNIT,860)(ISL,ISL=1,5)
            DO IST=1,ICYC
               WRITE (IRUNIT,870) IST, &
                         (PROBDA(IDI,IS,ISL,IST),ISL=1,5)
            END DO
         END IF
      END DO
   END DO

   WRITE (IRUNIT,820)
   DO  IDI=1,2
      DO IS=1,2
         IF (PRSNT(IDI,IS)) THEN
            WRITE (IRUNIT,850) HEADR(IDI,IS)
            WRITE (IRUNIT,860)(ISL,ISL=1,5)
            DO IST=1,ICYC
               WRITE (IRUNIT,870) IST, &
                         (DBHDA(IDI,IS,ISL,IST),ISL=1,5)
            END DO
         END IF
      END DO
   END DO

   WRITE (IRUNIT,830)
   DO IDI=1,2
      DO IS=1,2
         IF (PRSNT(IDI,IS)) THEN
            WRITE (IRUNIT,850) HEADR(IDI,IS)
            WRITE (IRUNIT,860)(ISL,ISL=1,5)
            DO IST=1,ICYC
               WRITE (IRUNIT,870) IST, &
                         (ROOTDA(IDI,IS,ISL,IST),ISL=1,5)
            END DO
         END IF
      END DO
   END DO

   WRITE (IRUNIT,840)
   DO IDI=1,2
      DO IS=1,2
         IF (PRSNT(IDI,IS)) THEN
            WRITE (IRUNIT,850) HEADR(IDI,IS)
            WRITE (IRUNIT,860)(ISL,ISL=1,5)
            DO IST=1,ICYC
               WRITE (IRUNIT,880) IST, &
                         (JRAGED(IDI,IS,ISL,IST),ISL=1,5)
            END DO
         END IF
      END DO
   END DO

   WRITE (IRUNIT,890)

800    FORMAT (' RDDBUG output:')
810    FORMAT (/' PROBDA array:')
820    FORMAT (/' DBHDA array:')
830    FORMAT (/' ROOTDA array:')
840    FORMAT (/' JRAGED array:')
850    FORMAT (1X, A20)
860    FORMAT (' Cycle/Stump ', 5I10)
870    FORMAT (10X, I5, 5F10.4)
880    FORMAT (10X, I5, 5I10)
890    FORMAT (/' RDDBUG end.'/)

ENDIF
GOTO 9000
!
!.... Common exit
!
9000 CONTINUE
RETURN
END

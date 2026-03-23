SUBROUTINE BMDFOL (ISTD,IYR)
!----------
! WWPB $Id$
!----------
!     CALLED FROM BMDRV
!**********************************************************************
! **BMDFOL    Date of last revision:  June 14, 1994
!
!  Definitions:
!     ITYP:   Type of tree: 1=host (PB host), 2=non-host
!
!  Common block variables and parameters:
!**********************************************************************

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!.... Common include files.
INCLUDE 'PPCNTL.f90'
INCLUDE 'BMCOM.f90'

!.... Variable declarations.

INTEGER ISIZ, MINSIZ, MAXSIZ
INTEGER DUM(1)
LOGICAL LOK
REAL    PATTCK
REAL    PRMS(4)

SAVE

IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
10 FORMAT(' Begin BMDFOL: Year= ',I5, 'Stand= ', I6)

!     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

IF (ICNT .GE. BMEND) ICNT = 0
ICNT = ICNT + 1

!     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
!     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
!     SET OF PARAMETERS.

IF (ICNT .EQ. 1) THEN

  IYR1= IYR
  NPRMS= 4

  CALL GPGET2 (314, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

  IF (LOK) THEN

    IYR2= IYR1 + IFIX(PRMS(1)) - 1

    PATTCK = PRMS(2)
    MINSIZ = INT(PRMS(3))
    MAXSIZ = INT(PRMS(4))

    IF (LBMDEB) WRITE (JBMBPR,101) IYR1,PATTCK,MINSIZ,MAXSIZ
101     FORMAT (/' IN BMDFOL: IYR1=',I5,' PATTCK=',F5.3, &
          ' MINSIZE=',I4,' MAXSIZE=',I4)

    IF (IYR2.GE.MIY(MICYC)) THEN
      PRMS(1) = IYR2 - MIY(MICYC) + 1
      CALL GPADD (KODE, MIY(MICYC), 314, NPRMS, PRMS, 1, DUM)
      IYR2 = MIY(MICYC) - 1
      IF (LBMDEB) WRITE (JBMBPR,103) PRMS(1),IYR2
103       FORMAT (/' IN BMDFOL: DEFOLIATORS ARE SCHEDULED FOR ', &
           'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0, &
           '  NEW IYR2=',I5)
    ENDIF
  ELSE
!         Zero out the attact rate to signify that defoliators are not active this year.
    IF (IYR .GT. IYR2) PATTCK= 0.0
  ENDIF
ENDIF

DO 30 ISIZ= 1,NSCL
  RVDFOL(ISTD,ISIZ) = 1.0
30 CONTINUE

!     LEAVE ROUTINE IF THERE ARE NO TREES ATTACKED.

IF (PATTCK .LE. 0.0) RETURN


!     RV FROM DEFOLIATORS IS SIMPLY 1 - PROP. TREES > 70% ATTACKED (user defined)

DO 50 ISIZ= MINSIZ,MAXSIZ
  RVDFOL(ISTD,ISIZ) = 1.0 - PATTCK
50 CONTINUE


IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
99 FORMAT(' End BMDFOL: Year= ',I5, 'Stand= ', I6)

RETURN
END

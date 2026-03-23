SUBROUTINE GRDTIM (DAT,TIM)
IMPLICIT NONE
!----------
! BASE $Id$
!----------
!
!     THE TIME AND DATE STAMPS FOR THE PROGNOSIS HEADING.
!----------
CHARACTER DAT*10,TIM*8
!***  CHARACTER TIME1*10
!**   CHARACTER UDAT*11,UTIM*9
!**   CHARACTER CFCDAT*8,CFCTIM*8
!**   CHARACTER LAHDAT*8
CHARACTER IBMDAT*8,IBMTIM*10,IBMZ*5
INTEGER IBMDT(8)
INTEGER IDATIM(8),IDAT(3),ITIM(3)
EQUIVALENCE (IDAT,IDATIM),(ITIM,IDATIM(4))

!----------
!     DUMMY DATE AND TIME RETURNS BLANK FIELDS.
!----------
DAT=' '
TIM=' '
!----------
!     CALL THE DATE AND TIME ROUTINE.  IBM FORTRAN VERSION 2.
!----------
!*    CALL DATIM (IDATIM)
!*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
!*   >             IDATIM(7),IDATIM(6),IDATIM(8)
!*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)')
!*   >             IDATIM(5),IDATIM(4),IDATIM(3)
!----------
!     CALL THE DATE AND TIME ROUTINES.  DG VERSION.
!----------
!*    CALL DATE (IDAT)
!*    CALL TIME (ITIM)
!*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
!*   >             IDAT(2),  IDAT(3),   IDAT(1)
!*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)') ITIM
!----------
!  CALL DATE AND TIME ROUTINES--RYAN MCFARLAND PC VERSION.
!----------
!*    CALL GETDAT (IYEAR,IMONTH,IDAY)
!*    CALL GETTIM (IHOUR,IMIN,ISEC,IHUND)
!*    WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)')
!*   >             IMONTH,    IDAY,      IYEAR
!*    WRITE (TIM,'(I2.2,'':'',I2.2,'':'',I2.2)')
!*   >             IHOUR,     IMIN,      ISEC
!----------
!     CALL THE DATE AND TIME ROUTINES.  LAHEY F77L VERSION.
!----------
!**   CALL DATE (LAHDAT)
!**   IF (LAHDAT(7:8).GE.'98') THEN
!**      DAT = LAHDAT(1:2)//'-'//LAHDAT(4:5)//'-19'//LAHDAT(7:8)
!**   ELSE
!**      DAT = LAHDAT(1:2)//'-'//LAHDAT(4:5)//'-20'//LAHDAT(7:8)
!**   ENDIF
!**   CALL TIME (TIM)
!
!----------
!     CALL THE DATE AND TIME ROUTINE.  LAHEY LF90 AND LF95 VERSIONS.
!----------
!***  CALL DATE_AND_TIME (LAHDAT,TIME1)
!***  IF (LAHDAT(3:4).GE.'98') THEN
!***  DAT = LAHDAT(5:6)//'-'//LAHDAT(7:8)//'-19'//LAHDAT(3:4)
!***  ELSE
!***  DAT = LAHDAT(5:6)//'-'//LAHDAT(7:8)//'-20'//LAHDAT(3:4)
!***  ENDIF
!***  TIM= TIME1(1:2)//':'//TIME1(3:4)//':'//TIME1(5:6)
!
!----------
!  CALL DATE AND TIME ROUTINES--MICROSOFT PC VERSION.
!----------
!     CALL GETDAT (IYEAR,IMONTH,IDAY)
!     CALL GETTIM (IHOUR,IMIN,ISEC,IHUND)
!     WRITE (DAT, '(I2,''-'',I2,''-'',I4)')
!    >              IMONTH, IDAY, IYEAR
!     WRITE (TIM, '(I2,'':'',I2,'':'',I2)')
!    >               IHOUR, IMIN,  ISEC
!----------
!  CALL DATE AND TIME ROUTINES -- xlf (IBM rs6000) VERSION
!----------
CALL DATE_AND_TIME (IBMDAT,IBMTIM,IBMZ,IBMDT)
WRITE (DAT,'(I2.2,''-'',I2.2,''-'',I4.4)') &
                IBMDT(2),IBMDT(3),IBMDT(1)
WRITE (TIM, '(I2.2,'':'',I2.2,'':'',I2.2)') &
                  IBMDT(5),IBMDT(6),IBMDT(7)
RETURN
END

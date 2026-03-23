SUBROUTINE BMSALV (IYR)
!----------
! WWPB $Id$
!----------
!     CALLED FROM BMDRV
!     CALLS:  BMSLSH
!**********************************************************************
! **BMSALV    Date of last revision:  09/28/05
!
!     Modified input data fields to coresspond with SANITISE keyword,
!     and modified SALVAGE keyword to consider minimum volume to
!     conduct Salvage operation (RNH May98)
!
!
!  Definitions:
!     IAG:    Loop counter over age classes
!     IPC:    Loop counter over pool types (fast, med, slow)
!     ISC:    Loop counter over size classes
!     MINSC:  Minimum size class for cut
!     MAXSC:  Maximum size class for cut
!     REMOVE: Amount of standing dead volume removed for one age & size
!     SUM:    Total amount of standing dead volume (all classes)
!     THRVOL -min. Vol. to perform salvage (RNH MAy98)
!     VREMOV: Amount of standing dead volume removed over age and size classes
!
!  Common block variables and parameters:
!**********************************************************************

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'PPCNTL.f90'
INCLUDE 'BMPRM.f90'
INCLUDE 'BMCOM.f90'
INCLUDE 'BMPCOM.f90'

!.... Variable declarations.

INTEGER ISC
INTEGER MINSC, MAXSC, MAXAGE
INTEGER SCNT, MYLST(MXSTND)
INTEGER MINLIV, MAXLIV
LOGICAL LOK
REAL    PRMS(7)
REAL    REMOVE
REAL    VREMOV, EFFC, THRVOL

SAVE

IF(LBMDEB) WRITE(JBMBPR,10) IYR
10 FORMAT(' Begin BMSALV: Year= ',I5)

!     Initializations

IYR1 = 0
!      NPRMS = 4
!
!     Added one parameter so new total in 5 parameters (RNH May98)
NPRMS = 5
IYR1 = IYR
MAXPRM = 7
MAXLIV = -1
MINLIV = -1
CALL GPGET2 (318,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
IF (LOK) THEN
  MINSC = INT(PRMS(1))
  MAXSC = INT(PRMS(2))
  MAXAGE = INT(PRMS(3))
  THRVOL = PRMS(4)       !  Min. Vol. to perform Salvage (RNH May98)
!        EFFC = PRMS(4)
!
!     Efficiency is now t 5th parameter in Field 7 of Keyword (RNH MAy98)
  EFFC = PRMS(5)
  IF (EFFC .GT. 1.0) EFFC = 1.0
  IF (EFFC .LT. 0.0) EFFC = 0.0

!       DON'T BOTHER DOING THE REST OF THE LOOP IF EFFICIENCY = 0
  IF (EFFC .EQ. 0.0) RETURN

  DO 22 ISIZ = 1,NSCL
    IF (L2D(ISIZ) .EQ. MINSC .AND. MINLIV .EQ. -1) MINLIV = ISIZ
    IF (L2D(ISIZ) .EQ. MAXSC .AND. MAXLIV .EQ. -1) MAXLIV = ISIZ
22   CONTINUE
!
!     Beginning of main stand loop
!
  DO 200 I = 1, SCNT

    ISTD = MYLST(I)
    IF (.NOT.STOCK(ISTD).OR.ISTD.LE.0) GOTO 200
!
!     Check for Min. volume for salvage cut.  This routine was lifted
!     from an ESSA commented-out r0outine at the end of this module
!     (RNH May98)
!
!
SUM = 0.0
!     Total standing dead volume available for cut
DO 30 IPC= 1,MXDWPC
  DO 25 ISC= MINSC,MAXSC
!          DO 20 IAG= 1,MXDWAG   Fixed 10/01 ajm.  Only dead trees
!                                less than user-defined threshold should be
!                                added to eligibility pool!  User-entered age
!                                {PRMS(3)=[MAXAGE]} is set to a class (1-5) in BMPPIN.
    DO 20 IAG= 1,MAXAGE
        SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
20     CONTINUE
25   CONTINUE
30 CONTINUE
SUM = SUM * EFFC

!     If not enough volume to warrent a salvage cut then return
IF (SUM .LT. THRVOL) GO TO 200

!         Remove some standing dead wood from various classes
    DO 50 IPC= 1,MXDWPC
      VREMOV = 0.0
      DO 45 ISC= MINSC,MAXSC
        DO 40 IAG= 1,MAXAGE
          REMOVE = SDWP(ISTD,IPC,ISC,IAG) * EFFC

          VREMOV = VREMOV + REMOVE
          SDWP(ISTD,IPC,ISC,IAG) = SDWP(ISTD,IPC,ISC,IAG) - REMOVE
40         CONTINUE
45       CONTINUE

!           Calculate amount of slash produced from salvage

!            CALL BMSLSH (IPC,1,VREMOV,ISTD)
!           **** Integer argument "1" caused fortran error (arithmetic underflow...
!                passed 1E-45 into BMSLSH via TTREE). Changed to a real number in line below.
!                MJO March 1998

      CALL BMSLSH (IPC,1.0,VREMOV,ISTD)

      VOLREM(ISTD,2) = VOLREM(ISTD,2) + VREMOV
50     CONTINUE

!         For bookkeeping purposes, make sure to remove some PBKILL and ALLKILL
!         since they would have been part of the removals in the youngest age class.

    DO 52 ISIZ= MINLIV,MAXLIV
       PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) * (1 - EFFC)
       ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * (1 - EFFC)
52     CONTINUE

200   CONTINUE
ENDIF

!      SUM = 0.0
!     Total standing dead volume available for cut
!      DO 30 IPC= 1,MXDWPC
!        DO 25 ISC= MINSC,MAXSC
!          DO 20 IAG= 1,MXDWAG
!              SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
!    20     CONTINUE
!   25   CONTINUE
!   30 CONTINUE
!      SUM = SUM * EFFC

!     If not enough volume to warrent a salvage cut then return
!      IF (SUM .LT. THRVOL) RETURN


IF(LBMDEB) WRITE(JBMBPR,99) IYR
99 FORMAT(' End BMSALV: Year= ',I5)


RETURN
END

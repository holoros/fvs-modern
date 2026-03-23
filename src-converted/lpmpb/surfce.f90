SUBROUTINE SURFCE
IMPLICIT NONE
!----------
! LPMPB $Id$
!----------
!
!     SURFACE AREA CALCUTLATION ROUTINE
!     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
!     NICK CROOKSTON; PROGRAMMER
!
!      THIS ROUTINE IS CALLED FROM MPBCUP WHEN MPBMOD WILL BE CALLED
!
!      EQUATIONS ARE FROM A. STAGE
!
!          SNOHST= TOTAL NON-LPP SURFACE AREA TO 5 INCH TOPS
!          SURF  = SURFACE OF LPP BY CLASS TO 5 INCH TOPS
!          SUR   = TEMPORARY.  SURFACE TO THE TOP
!          SUR5  = TEMPORARY.  SURFACE TO 5 INCH TOP
!
!
! Revision History:
!   06/05/00  Glen Brink (FHTET)
!     Added variables IDXWP,IDXWL,IDXDF,IDXLP and IDXPP, array indices of
!       White Pine, Western Larch, Douglas Fir, Lodgepole Pine and
!       Ponderosa Pine respectively.  Added to common block in file
!       MPBCOM.F77.
!     Added array MPBSPM to govern the computations by species using an
!       IF block as opposed to the old COMPUTED GO TO, since the array
!       allows the definition to be made in mpblkd.f, instead of always
!       having to change the COMPUTED GO TO. Added to common block in
!       file MPBCOM.F77.
!   11/10/00  Lance David (FHTET)
!     Basic cleanup of code and comments.
!     Removed old & inactive code.
!   07/02/10 Lance R. David (FMSC)
!     Added IMPLICIT NONE.
!---------------------------------------------------------------------
!
!OMMONS
!

INCLUDE 'PRGPRM.f90'

INCLUDE 'ARRAYS.f90'

INCLUDE 'CONTRL.f90'

INCLUDE 'PLOT.f90'

INCLUDE 'MPBCOM.f90'
!
!OMMONS
!
INTEGER I, I1, I2, II, IS
REAL SUR, SUR5, SURFLP

SNOHST = 0.0

!     SPECIES LOOP.
!
DO 200 IS = 1,MAXSP
  IF (IS .EQ. IDXLP) GO TO 200
  I1 = ISCT(IS,1)
  IF (I1 .EQ. 0) GO TO 200
  I2 = ISCT(IS,2)

!       TREE RECORD LOOP WITHIN SPECIES
!
  DO 100 II = I1, I2
    I = IND1(II)

    IF(MPBSPM(IS) .EQ. IDXWP) THEN
!
!           WHITE PINE
!
      SUR = -1.8673+0.86*ALOG(DBH(I))+1.07854*ALOG(HT(I))
      SUR5 = 19.6794 + 0.2345*HT(I) + 0.1363*CFV(I) + 0.8841*SUR

    ELSE IF(MPBSPM(IS) .EQ. IDXWL) THEN
!
!           WESTERN LARCH
!
      SUR = -3.0779+0.062565*ALOG(DBH(I))+1.46018*ALOG(HT(I))
      SUR5 = 31.90062 - 0.12195784*HT(I) + 0.071106*CFV(I) + &
                0.972312*SUR

    ELSE IF(MPBSPM(IS) .EQ. IDXDF) THEN
!
!           DOUGLAS-FIR
!
      SUR = -1.73648+0.912291*ALOG(DBH(I))+1.010666*ALOG(HT(I))
      SUR5 = 19.4183 + 0.2631*HT(I) + 0.2593*CFV(I) + 0.8274*SUR

    ELSE IF(MPBSPM(IS) .EQ. IDXPP) THEN
!
!           PONDEROSA PINE
!
      SUR = -1.51556+0.99399*ALOG(DBH(I))+0.93723*ALOG(HT(I))
      SUR5 = 9.1767 + 0.4531*HT(I) + 0.3719*CFV(I) + 0.7589*SUR

    END IF

    IF (SUR5 .GE. 0.0)  SNOHST = SNOHST + SUR5 * PROB(I)

100   CONTINUE
200 CONTINUE
!
!     LODGEPOLE PINE   (BY CLASS)
!
DO 250 I = 1,NACLAS
  SURF(I) = SURFLP(CLASS(I,2))
250 CONTINUE

IF (DEBUIN) THEN
  WRITE (JOMPB,400) SNOHST,(SURF(I),I=1,NACLAS)
400   FORMAT (/,'SNOHST (TOTAL NON-LPP SURFACE AREA TO', &
             ' 5 INCH TOPS) ',E14.7,//, &
             'LPP SURFACE AREA BY CLASS . . .',//, &
             3(10E13.3,/))
END IF

RETURN
END

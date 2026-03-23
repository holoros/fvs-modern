 SUBROUTINE NVBEQDEF (SPCD, VOLEQ)
 IMPLICIT NONE

 INCLUDE 'PRGPRM.f90'
 INCLUDE 'PLOT.f90'
 INCLUDE 'CONTRL.f90'
 INCLUDE 'VARCOM.f90'

 CHARACTER(LEN=4) SppID, DivSplit, DIVISION
 CHARACTER(LEN=10) WoodlandEqs(16)
 CHARACTER (LEN=11) VOLEQ
 INTEGER I, SPCD

 INTEGER WoodlandSpp(16)

 DATA (WoodlandSpp(I), I=1,16) / &
     62,  63,  65,  66, &
     69, 106, 133, 134, &
    143, 321, 322, 475, &
    803, 810, 814, 843/

DATA (WoodlandEqs(I), I=1,16) / &
    "R03CHO0065", "R03CHO0066", "R03CHO0065", "R03CHO0066", &
    "R03CHO0065", "R03CHO0106", "400DVEW133", "R03CHO0106", &
    "R03CHO0106", "200DVEW475", "200DVEW814", "200DVEW475", &
    "300DVEW800", "300DVEW800", "200DVEW814", "300DVEW800" /

DivSplit = '0000'
DIVISION = ECOREG

SELECT CASE (SPCD)
  CASE (62, 63, 65, 66, 69, 106, 133, 134, &
           143, 321, 322, 475, 803, 810, 814, 843)
    IF (ISTATE.eq.0) CALL ERRGRO(.TRUE., 44)
      DO I= 1, size(WoodlandSpp)
        IF (WoodlandSpp(I).eq.SPCD) THEN
          VOLEQ=WoodlandEqs(I)
          EXIT
        END IF
      END DO
    IF (ISTATE.eq.6 .or. ISTATE.eq.41 .or. ISTATE.eq.53) THEN
      IF (SPCD.eq.62 .or. SPCD.eq.65)   VOLEQ='400DVEW065'
      IF (SPCD.eq.66)                   VOLEQ='200DVEW066'
      IF (SPCD.eq.322)                  VOLEQ='200DVEW475'
      ELSE IF (ISTATE.eq.4 .or. ISTATE.eq.35) THEN
        IF (SPCD.eq.332 .or. SPCD.eq.814) VOLEQ='300DVEW800'
    END IF

  CASE DEFAULT
    ! Species is not woodland and uses NVB species
    DIVISION = ADJUSTL(DIVISION)
    IF (LEN_TRIM(DIVISION) .LT. 4) DIVISION ='0'//TRIM(DIVISION)
    DivSplit = TRIM(DIVISION)
    WRITE(SppID, FMT='(I4)') SPCD
    SppID = ADJUSTL(SppID)

    DO WHILE (LEN_TRIM(SppID) .LT. 3)
      SppID = '0' // TRIM(SppID)
    END DO

    VOLEQ = 'NVB' // DivSplit // TRIM(SppID)

!       Check for managed plantation
  IF((DivSplit .EQ.'0000' .OR. DivSplit .EQ. '0230').AND. &
         (SPCD .EQ. 111 .OR. SPCD .EQ. 131) &
          .AND. ISTDORG .EQ. 1) THEN
   VOLEQ(11:11) = 'P'
  END IF
END SELECT

RETURN
END

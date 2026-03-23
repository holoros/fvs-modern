SUBROUTINE SETLEAFN(I)
!--------------------------------------------------
!  THIS SUBROUTINE SETS LEAF_ON & FROOT_ON
!--------------------------------------------------
INTEGER C
INCLUDE 'ENTITY.f90'
! Set tree on-off
  ITON=0
  ITOFF=365
! Set shrub on-off
  ISON=120
  ISOFF=260
! Set grass on-off
  IGON=120
  IGOFF=210
! Set LEAF_ON and LEAF_OFF
  IF(ID(I).EQ.'T' .AND. (JD.GE.ITON .AND. JD.LE.ITOFF)) THEN
     LEAF_ON(I)=.TRUE.
     FROOT_ON(I)=.TRUE.
  ELSE IF(ID(I).EQ.'S' .AND. (JD.GE.ISON .AND. JD.LE.ISOFF)) THEN
     LEAF_ON(I)=.TRUE.
     FROOT_ON(I)=.TRUE.
  ELSE IF(ID(I).EQ.'G' .AND. (JD.GE.IGON .AND. JD.LE.IGOFF)) THEN
     LEAF_ON(I)=.TRUE.
     FROOT_ON(I)=.TRUE.
!!!!shutting down grass based on limited soil water not yet implemented
!!!!in model -- DWC,10/28/94.
!       ELSE IF(ID(I).EQ.'G' .AND. VOLCONT.LT.(H2OMAX*.1)) THEN
!          LEAF_ON(I)=.FALSE.
!          FROOT_ON(I)=.FALSE.
  ELSE
     LEAF_ON(I)=.FALSE.
     FROOT_ON(I)=.TRUE.
  ENDIF
!       WRITE(*,*) I,ID(I),JD,LEAF_ON(I)
! Count the number of days LEAF_ON=TRUE
IF(LEAF_ON(I)) THEN
   C=1
ELSE
   C=0
ENDIF
TLEAFON(I)=TLEAFON(I) + C
RETURN
END

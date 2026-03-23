SUBROUTINE BSITELOAD(FVS_SLOPE,FVS_ASPECT,FVS_ELEV,FVS_TLAT, &
                       FVS_KODTYP,FVS_KODFOR)
!----------
!  **SITELOAD  BGC--DATE OF LAST REVISION: 10/16/99
!----------
!
!     LOADS FVS SITE ATTRIBUTES INTO BGC MEMORY
!
!     CALLED FROM: BGCGROW
!
!OMMONS
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'
!OMMONS
INTEGER FVS_KODTYP, FVS_KODFOR
!
!      print *,'in siteload'
!------------------------------------------------------------------------
! GET FVS SITE VARIABLES
!------------------------------------------------------------------------
!  FVS_SLOPE =   slope in % scaled to 0-1
!  FVS_ASPEC =   aspect in degrees
!  FVS_ELEV   =  elevation in 100's feet
!  FVS_TLAT =    latitude
!  FVS_KODTYP =  habitat type code
!  FVS_KODFOR =  Nat. Forest code
!------------------------------------------------------------------------
SLOPE=FVS_SLOPE
ASPECT=FVS_ASPECT*(360./6.28)     !convert radians to degrees
LAT=FVS_TLAT
HTYPE=FVS_KODTYP
NFLOC=FVS_KODFOR
ELEV=INT((FVS_ELEV*100.)/3.28)  ! Converts to meters
!
!
  WRITE(*,*)
  WRITE(*,*) 'HTYPE= ',HTYPE
  WRITE(*,*) 'SLOPE= ',SLOPE
  WRITE(*,*) 'ASPECT= ',ASPECT
  WRITE(*,*) 'ELEV= ',ELEV
  WRITE(*,*) 'NFLOC= ',NFLOC
  WRITE(*,*) 'FVS_TLAT= ',FVS_TLAT
  WRITE(*,*) 'LAT= ',LAT
RETURN
END


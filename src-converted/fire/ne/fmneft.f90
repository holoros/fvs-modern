SUBROUTINE FMNEFT(IFFEFT)
IMPLICIT NONE
!----------
! FIRE-NE $Id$
!----------
!  SINGLE-STAND VERSION
!  CALLED FROM: FMCBA
!  PURPOSE:
!     THIS SUBROUTINES CALCULATES A CATEGORICAL FOREST TYPE BASED ON
!     FIA FOREST TYPE.  THIS FOREST TYPE IS USED IN SETTING DEFAULT
!     SURFACE FUEL LEVELS.
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'FMPARM.f90'
!
!
INCLUDE 'FMCOM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'ARRAYS.f90'
!
!
INCLUDE 'PLOT.f90'
!
!
!OMMONS
!----------
!     LOCAL VARIABLE DECLARATIONS
!----------
INTEGER IFFEFT
!----------
!  ROUTINE BEGINS.
!  DETERMINE FFE FOREST TYPE (1 OF 10 CATEGORIES) FROM FIA FOR. TYPE
!----------
IFFEFT = 0
SELECT CASE (IFORTP)
CASE (101:105)
  IFFEFT = 1 ! white / red / jack pine (includes eastern hemlock)
CASE (121:127)
  IFFEFT = 2 ! spruce fir, (includes tamarck, n. white cedar)
CASE (161:168)
  IFFEFT = 3 ! loblolly - shortleaf pine
CASE (381:383)
  IFFEFT = 4 ! exotic softwoods
CASE (401:409)
  IFFEFT = 5 ! oak - pine
CASE (501:520)
  IFFEFT = 6 ! oak - hickory
CASE (601:608)
  IFFEFT = 7  ! oak - gum - cypress
CASE (701:709)
  IFFEFT = 8 ! elm - ash - cottonwood
 CASE (801:809)
  IFFEFT = 9 ! maple-beech-birch
CASE (901:904)
  IFFEFT = 10 ! aspen-birch
CASE (999)
  IFFEFT = 11 ! nonstocked
CASE DEFAULT
  IFFEFT = 2 ! spruce - fir
END SELECT
!
RETURN
END

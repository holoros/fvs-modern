SUBROUTINE FMLSFT(IFFEFT)
IMPLICIT NONE
!----------
! FIRE-LS $Id$
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
CASE (102:105,381)
  IFFEFT = 1 ! white / red pine (includes eastern hemlock)
CASE (101)
  IFFEFT = 2 ! jack pine
CASE (121:127)
  IFFEFT = 3 ! spruce fir, (includes tamarck, n. white cedar)
CASE (181)
  IFFEFT = 4 ! eastern redcedar
CASE (401:409)
  IFFEFT = 5 ! oak - pine
CASE (501:520)
  IFFEFT = 6 ! oak - hickory
CASE (701:709)
  IFFEFT = 7 ! elm - ash - cottonwood
 CASE (801:809)
  IFFEFT = 8 ! maple-beech-birch
CASE (901:904)
  IFFEFT = 9 ! aspen-birch
CASE (999)
  IFFEFT = 10 ! nonstocked
CASE DEFAULT
  IFFEFT = 1 ! white / red pine
END SELECT
!
RETURN
END

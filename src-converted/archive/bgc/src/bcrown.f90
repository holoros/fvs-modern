SUBROUTINE BCROWN(I,NEW_CR)
!------------------------------
! Subroutine to adjust crown ratios for entities. Called
! from TRNOVR.FOR -- DWC,5/13/94.
!   Note: updates to comments made on 8/24/94,DWC.
! Re-written 7/13/95 -- KSM.
!-------------------------------

REAL HINC, CL, LOSS, NEW_CR, NEW_CL
INCLUDE 'ENTITY.f90'

! calculate current crown length and height increment
CL=O_H(I) * CR(I)
HINC=H(I) - O_H(I)
! check for undefined variables or conditions
IF(HINC.LE.0.0) HINC=0.0
! if previous years' leaf carbon > current years' leaf carbon
! reduce CL by an amount such that the reduction in crown volume
! just contains the leaf carbon loss. Carbon is removed from the
! base of the crown. If leaf carbon has increased, then
! crown length is increased by HINC.
IF(O_LEAF(I).GT.LEAF(I)) THEN
  LOSS=O_LEAF(I) - LEAF(I)
ELSE
  LOSS=0.0
ENDIF
! calculate new CL based on start of period values for foliage density
! (FOLDENS (kg/m3) - in STRUCT), crown angle (BETA(I) in STRUCT), CL, and
! equation for calculating volume of a cone frustrum (in STRUCT)
FRUSTVOL=LOSS*1/FOLDENS(I)   !gives m3 of crown volume in leafcarbon
PART=(3.145*BETA(I)**2.)/3.  ! coeff. on Beta(i) changed from 3 to 2
C1=CL**3.                    ! 3/16/96. KSM.
C2=FRUSTVOL/PART
DIF=C1-C2
IF(DIF.LE.0.0) THEN
   NEW_CL=0. ! can't have negative CL
ELSE
   NEW_CL=(DIF**(1./3.))+HINC
ENDIF
! control crown length by holding FOLDENS(I) constant
!     CRWNVOL=LEAF(I)/FOLDENS(I)
!     NEW_CL=(CRWNVOL/PART)**(1./3.)+HINC
!
NEW_CR=NEW_CL/H(I)
!     WRITE(*,100) I, FOLDENS(I), CL, NEW_CL, CR(I), NEW_CR
! 100 FORMAT(I4,1X,F6.4,1X,2(F8.2,1X),2(F8.3,1X))
RETURN
END

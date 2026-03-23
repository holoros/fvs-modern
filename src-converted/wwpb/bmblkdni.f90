BLOCK DATA BMBLKD
!----------
! WWPB $Id$
!----------
!**********************************************************************
! The following Westwide Pine Beetle Model variable initializations
! were originally within the subroutine BMINIT code.
! It was moved to this BLOCK DATA subprogram to satisfy the FORTRAN
! standards enforced by Lahey FORTRAN compiler.
! 08/16/94 Lance R. David
!
! Changed code so that there are NO default beetle host tree species.
! --ajm 3/15/00
!**********************************************************************

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'PPEPRM.f90'
INCLUDE 'BMPRM.f90'

!.... Common include files.

INCLUDE 'BMCOM.f90'
INCLUDE 'BMRCOM.f90'

!.... Data statements

!     Logical variable to control initialization of beetle model. True
!     means that subroutine BMINIT needs to be called.
!
DATA LBMINT/.TRUE./

! Defaults: PBSPEC= 1 : Mountain Pine Beetle is simulated
!           NBGEN=  1 : 1 generation of PBSPEC per year
!           NIBGEN= 2 : 2 generation of Ips per year
!           Keywords can modify

DATA  NBGEN/1/, NIBGEN/2/
!      DATA PBSPEC/1/, NBGEN/1/, NIBGEN/2/

! Defaults: IPSON= false  :Ips is NOT a driving variable
!           IPSMIN= 2     :smallest size class Ips will attack
!           IPSMAX= 5     :biggest size class that Ips will kill tree in
!                          (but will attack larger classes)
!           PFSLSH=.9     :prop slash to fill before attacking trees
!           Keywords can modify

DATA IPSON/.FALSE./, IPSMIN/2/, IPSMAX/5/
DATA PFSLSH/0.9/

! Defaults: Host species for MPB/WPB/I. For now, 7 (LP) is the only
!           host in town, except for WPB, which is 10 (PP)
!  Inland Empire FVS species are:   WP L  DF GF WH C  LP S  AF PP OTH
!
!
DATA (HSPEC(1,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
DATA (HSPEC(2,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
DATA (HSPEC(3,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
!      DATA (HSPEC(1,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/
!      DATA (HSPEC(2,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/
!      DATA (HSPEC(3,I), I=1,MAXSP) /0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/

! Defaults: breakpoints for the DBH size classes. There can be no more than
!           NSCL of these. The computation which follows computes a rough
!           value for the basal area in each size class. "Rough" since it
!           has no treelist as a basis.

DATA UPSIZ/3, 6, 9, 12, 15, 18, 21, 25, 30, 50/

! Defaults: DEAD WOODY POOL DBH SIZE CLASSES

DATA WPSIZ/10, 20, 60/

! Defaults: Species falldown rates (1=fast,2=medium,3=slow) for standing dead
!  Inland Empire FVS species are:
!                  WP L  DF GF WH C  LP S  AF PP OTH
DATA ISPFLL /2, 3, 2, 1, 2, 3, 2, 1, 2, 1, 2/

! Defaults: Falldown rates (standing -> dead) for the different pool qualities
!           (fast, medium, slow)

DATA FALLRT /0.2, 0.1, 0.05/

! Defaults: The smallest attractive size class varies with species.
!           MPB & WPB wont go into stands with only trees < 6 inches (sc<3),
!           and Ips won't see stands with trees less than 3 inches only.
!           Keyword can modify

DATA ISCMIN/3,3,2/

! Defaults: Seeds for the random number generator BMRANN.

DATA BMS0/55329D0/, BMSS/55329./

END

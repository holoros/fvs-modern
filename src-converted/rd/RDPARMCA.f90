!ODE SEGMENT RDPARM
!----------
! RD $Id$
!----------
!
!  Parameters for the root disease model are :
!
INTEGER DSO, DSII, DSIU

PARAMETER (DSO=1)
PARAMETER (DSII=2)
PARAMETER (DSIU=3)

PARAMETER (IRRTRE=500)
PARAMETER (IRRTP1=IRRTRE+1)
PARAMETER (ITOTSP=46)
PARAMETER (ITOTRR=4)
!
!     DSO    - Code for Dead Standing Outside (includes non-standing)
!     DSII   - Code for Dead Standing Inside Infected (includes
!              non-standing trees)
!     DSIU   - Code for Dead Standing Inside Uninfected (includes
!              non-standing)
!     IRRTRE - The max number of tree records that root disease can
!              process
!     IRRTP1 - The max number of tree records plus 1.
!     ITOTSP - Total number of species used by all variants.
!     ITOTRR - Total possible number of root diseases represented by
!              the model
!                 1 = P-TYPE ANNOSUS
!                 2 = S-TYPE ANNOSUS
!                 3 = ARMILLARIA
!                 4 = PHELLINUS
!
!            **If this is changed, the data statements in ANBLK1 must
!              also change.
!
!-----END SEGMENT


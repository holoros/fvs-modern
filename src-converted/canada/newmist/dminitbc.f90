SUBROUTINE DMINIT
IMPLICIT NONE
!----------
! CANADA-NEWMIST $Id$
!----------
! **DMINIT--NI  DATE OF LAST REVISION: 02/22/96
!----------
! Purpose:
!   This routine initializes all the common variables for the model.
! Explanations and definitions of the variables can be found in
! DMCOM.
!
!
! Called by:
!
!     MISIN0
!
! Other routines called:
!
!     DMRNSD
!
! Argument list definitions:
!
!     [none]
!
! Local parameter and variable definitions:
!
!     INTEGER i          General loop counter
!     INTEGER j          General loop counter
!     INTEGER TPDMR      Dummy array used to load DMDMR
!     REAL    x          product of pi * sq-meter-to-acre conversion
!     REAL    y          radius of disc in circle area calculation
!     REAL    tmp        Temporary storage for calculation of ring
!                        scaling factor 'SF()' terms using DMALPH
!                        and DMBETA.
!     REAL    TPOPAQ     Dummy array used to load DMOPAQ
!     REAL    TSEED      Dummy variable used as a parameter to call DMRNSD.
!                        Never used.
!
!
! Common block variables and parameters:
!
!     MXTHRX  DMCOM
!     MAXTRE  PRGPRM
!     DMRATE  DMCOM
!     NTDn    DMCOM
!     DCDn    DMCOM
!     ZeroPDn DMCOM
!     PIE     DMCOM
!     SQM2AC  DMCOM
!     MESH    DMCOM
!     CrArea  DMCOM
!     Dstnce  DMCOM
!     CLUMP   DMCOM
!     DMALPH  DMCOM
!     DMBETA  DMCOM
!     SF      DMCOM
!     DMRDFF  DMCOM
!     DMDMR   DMCOM
!     DMOPQM  DMCOM
!     DMOPAQ  DMCOM [values are variant-specific]
!     DMLtnp  DMCOM
!     DMLtRx  DMCOM
!     DMKTUN  DMCOM
!     DMETUN  DMCOM
!     DMSTUN  DMCOM
!     DMITUN  DMCOM
!     DMFLWR  DMCOM
!     DMCAP   DMCOM
!     DMDETH  DMCOM
!     DMS0    DMCOM
!     DMSS    DMCOM
!

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'DMCOM.f90'

!.... Local variable declarations.

INTEGER i, j, k, l
INTEGER TPDMR(0:6, CRTHRD)
REAL    x, y, tmp(MXTHRX)
REAL    TSEED
REAL    TPOPAQ(MAXSP)

!.... Data statements

NEWMOD = .FALSE.

!.... TPDMR is the dummy array used to load the array DMDMR.
!.... This is initialized this way so that if multiple stands are run through
!.... the model then the array DMDMR will get re-initialized properly for each
!.... stand.

DATA TPDMR  / 0, 0, 0, 0, 0, 1, 2, &
                 0, 0, 0, 1, 2, 2, 2, &
                 0, 1, 2, 2, 2, 2, 2 /

!.... TPOPAQ is the dummy array used to load the array DMOPAQ.
!.... This is initialized this way so that if multiple stands are run through
!.... the model then the array DMOPAQ will get re-initialized properly for each
!.... stand.
!....
!.... The RELATIVE opacities of DMOPAQ() are taken from the April 1993
!.... Model Review Workshop Report, Table 4.1 (p.29).
!.... These values can be changed by the keyword DMOPQ.
!.... The 15 (MAXSP) spcecies codes for the BC variant are:
!....
!.... 'PW','LW','FD','BG','HW','CW','PL','SE','BL','PY',
!.... 'EP','AT','AC','OC','OH'


DATA TPOPAQ / &
     1.2, &  !PW
     0.9, &  !LW
     1.5, &  !FD
     1.8, &  !BG
     2.0, &  !HW
     1.8, &  !CW
     1.0, &  !PL
     1.7, &  !SE
     1.8, &  !BL
     1.0, &  !PY
     2.0, &  !EP
     2.0, &  !AT
     2.0, &  !AC
     1.5, &  !OC=FD
     2.0/ !OH=EP

!.... Zero 0-6 DM rating and all DM pools

DO i = 1, MAXTRE
   DMRATE(i) = 0
   DO j = 1, CRTHRD
     DO k = 1, DEAD_BC
       DMINF(i,j,k) = 0.0
     ENDDO
  ENDDO
ENDDO

DO i = 1, MAXTRE
   DO j = 1, CRTHRD
     DO k = 1, ACTIVE
        DO L = 1,MAXBC
          DMINF_BC(i,j,k,l) = 0.0
        ENDDO
     ENDDO
  ENDDO
ENDDO

!.... Initialize logical triggers to .FALSE. These are responsible for
!.... notifying whether the initial crown third assignments have been
!.... made; whether the damage codes have been assigned to the model's
!.... array of DM ratings; and whether the life history pools have been
!.... filled.

NTDn = .FALSE.
DCDn = .FALSE.
ZPDn = .FALSE.

!.... Compute areas in circles; a necessary step for subsequent sampling
!.... calculations. Then assign the distance to the midpoint of each
!.... sampling ring.

x = PIE * SQM2AC

DO 70 i = 1, MXTHRX
   y = FLOAT(MESH * i)
   CrArea(i) = x * y**2
70 CONTINUE

DO 80 i = 1, MXTHRX
   Dstnce(i) = FLOAT(MESH) * (FLOAT(i) - .5)
80 CONTINUE

!.... Initialize the mean/variance ration to "random" (Poisson). Then
!.... initialize the autocorellation terms for the ring quadrats. These
!.... values are arbitrary, but could be measured in the field and made
!.... into sensible defaults at a later date.

DMCLMP =  1.00
DMALPH = -0.50
DMBETA =  0.0

!.... Compute the default scaling weights for the autocorrelation
!.... function.

DO 90 j = 1, MXTHRX
   tmp(j) = EXP(Dstnce(j) * DMBETA)
90 CONTINUE

DO 100 i = 0, 6
   x = FLOAT(i) * DMALPH

   DO 95 j = 1, MXTHRX
      SF(i, j) = EXP(x * tmp(j))
95    CONTINUE
100 CONTINUE

!.... Assign the matrix which defines the difference in DMR between any
!.... two DMR categories. Although the matrix is symmetric, 'i' indexes
!.... the *source* DMR and 'j' the *target* DMR when it is actually
!.... used.

DO 210 i = 0, 6
   DO 200 j = 0, 6
      DMRDFF(i, j) = ABS(j-i)
200    CONTINUE
210 CONTINUE

!.... Initialize default values for crown thirds DMR.

DO 230 i = 0, 6
   DO 220 j = 1, CRTHRD
      DMDMR(i,j) = TPDMR(i,j)
220    CONTINUE
230 CONTINUE

!.... Initialize default values for the relative opacities.

DO 250 i = 1, MAXSP
   DMOPAQ(i) = TPOPAQ(i)
250 CONTINUE

!.... The value of DMOPQM is arbitrarily chosen to be consistent with
!.... the guess used for the Phase 1 model.  This value can be changed
!.... by the keyword DMOPQM.

DMOPQM = 0.20

DO 300 i = 1, MAXSP

!....    Default coefficients for the light-driven forward and backward
!....    transitions.
!....    These correspond to 0% latent -> immature at 0% light
!....    and 100% latent -> immature at 100% light for DMLT().
!....    The reverse: (immature -> latent) transition coefficients are:
!....    100% immature -> C latent at 0% light and 0% immature -> latent
!....    at 100% light.
!....    Coefficients are the same for all species, and are modified
!....    by the DMLIGHT keyword.

   DMLtnp(i, 1) = 2
   DMLtRx(i, 1, 1, 1) = 0.
   DMLtRx(i, 1, 2, 1) = 0.
   DMLtRx(i, 1, 1, 2) = 1.
   DMLtRx(i, 1, 2, 2) = 1.

   DMLtnp(i, 2) = 2
   DMLtRx(i, 2, 1, 1) = 0.
   DMLtRx(i, 2, 2, 1) = 1.
   DMLtRx(i, 2, 1, 2) = 1.
   DMLtRx(i, 2, 2, 2) = 0.

!....    Spread and fecundity tuning parameters.

   DMKTUN(i) =  0
   DMETUN(i) =  1.00
   DMSTUN(i) =  1.00
   DMITUN(i) =  1.00

   DMFLWR(i) =  4
   DMCAP(i)  =  3.00

   DMDETH(i) =  0.08

300 CONTINUE

!.... Initial values for the random number generator.

DMS0 = 55329.0D0
DMSS = 55329.0

!.... Reset the random number generator. Value of 'i' is a dummy.

CALL DMRNSD (.FALSE., TSEED)

RETURN
END

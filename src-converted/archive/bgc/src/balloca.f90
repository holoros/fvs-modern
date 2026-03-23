SUBROUTINE BALLOCA(I)

!-------------------------------------------------------------------
!   THIS SUBROUTINE CALCULATES THE LEAF, STEM, AND ROOT ALLOCATION
!   FRACTIONS -- DWC, 8/11/93;12/1/93.  CALLED BY ENTITY
!   NOTE: The fractions are initialized in the RESPIRE subroutine for year 1.
!         The dynamic allocation fraction algorithm has been disabled
!         because it does not perform well; static allocation fractions
!         are being used.
!
!   Changing leaf allocation fractions for grass back to static "defaults";
!   For now, hard-coded at 50% each to leaf and root.
!   ALlocation before this change seemed to dis-favor leaf, hence the grasses
!   always dying.  AJM 11/03
!   I think there is a conceptual model problem.  If PSN is zero, all three
!   allocation fractions get set to zero.  This would not be a problem if the
!   allocation fractions were used in this year (of zero PSN).  However, the
!   fractions calculated here are used NEXT YEAR.  PSN might not be zero.  Yet
!   since the fractions are set here to zero, even if PSN is net positive
!   nedxt year, nothing will get allocated to leaf,stem,root, because in
!   UPDATE, the PSN is multiplied by allocation fraction to get net increment
!   to pool.
!   As initial temporary fix, am setting allocation fractions to 63%root,&
!   37% leaf if PSN is zero. ajm 1/22/04
!-------------------------------------------------------------------

REAL SWFINDX1,SWFINDX, NO2INDX, LRATIO, CLIMIT, WLIMIT
REAL PSN, FINALLC,CLIMIT1
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'


IB=IBLB(I)
!-----------------------------------------------------------
! CALCULATE ALLOCATION FRACTIONS FOR LEAF, STEM, & ROOTS
! Subtract maintenance respiration from photosynthesis
PSN=AFPSN(I) - AFMRESP(I)
! Zero-out PSN and growth respiration if PSN <= 0
IF(PSN.LE.0.0) THEN
 AFGRESP(I)=0.0
 PSN=0.0
ENDIF
! Annual soil water fractional average index. SUMWP sums daily soil
! water potential. SUMPSI(I) summed leafwp over layers (in CONDUCTANCE)
! and thus overestimated cummulative stress. SWFINDX1 approaches zero
! when there are many days of high water stress, and approaches one if
! there are few days with water stress. B2(3) is water potential at
! stomatal closure. SWFINDX is constrained to be no greater than 0.66.
!     SWFINDX1=1.0 - SUMPSI(I) / (1.01*TLEAFON(I)*B2(3))

! Note! 11/03.  I am not changing this here and now.  But the code below does
! not work for grass!  SWFINDX1 gets calculated as a negative number--I think
! because SUMWP is derived for ALL days, while denominator is for only those
! days that grass leaf is ON.  For current algorithm to work, SUMWP(1) will
! need to be re-calculated separately for grasses shrubs and trees.  Note that
! this error only presents errors if the SWFINDX that SHOULD be calculated
! is greater than 0.66, because if it is less than 0.66 it gets readjusted TO
! 0.66 further below.  So now, with the error, grasses typically get assigned
! SWFINDX values of 0.66 regardless of the water status, because of this
! mathematical error resulting in negative SWFINDX1 values. ajm
!
IF (ID(I).EQ.'G') THEN
   SWFINDX1=1.0 - SUMWP(1) / (1.01*TLEAFON(I)*B2(3))      !*!  for grass
!         SWFINDX1=1.0 - SUMWP(1) / (1.01*365*B2(3))    !*!  for grass
ELSE IF ((ID(I).EQ.'S').OR. &  !added shrubs ajm 10/99
    ((ID(I).EQ.'T').AND.(H(I).LT.HTLIMIT))) THEN
   SWFINDX1=1.0 - SUMWP(1) / (1.01*TLEAFON(I)*B2(3))      !*!for small trees [and shrub (ajm)]
ELSE
   SWFINDX1=1.0 - SUMWP(2) / (1.01*TLEAFON(I)*B2(3))      !*!  for large trees
ENDIF
!
SWFINDX=MAX(SWFINDX1,0.66)
! Nitrogen availability index
!!!!note: nitrogen use has NOT been incorporated yet.DWC,1/94.
NO2INDX=0.0
NO2INDX=MIN(NO2INDX,1.0)
! Leaf/(Leaf+Root) allocation ratio. B2(19) is the maximum ratio(.66) allowed.
! If a year has little water stress, SWFINDX is close to one, and
! LRATIO approaches B2(19). In dry years, LRATIO will be smaller, thus
! more allocation to roots.
!      LRATIO=(B2(19) / 2.) * (SWFINDX + NO2INDX)
 LRATIO=B2(19) * SWFINDX  !!! nitrogen not operative
! Carbon, nitrogen, and water limits for leaf area.
!======================================================================
! Carbon limit. Calculation based on net PSN minus growth respiration.
! If all carbon went to either leaves or roots, the maximum that could
! go to leaves would be determined by LRATIO.
CLIMIT1=PSN * LRATIO*(1-B2(7))
CLIMIT=MAX(CLIMIT1,0.0)
!======================================================================
! nitrogen limit
!     NLIMIT=1.0
!======================================================================
! water limit, uses Myers' water stress integral. B1(9,IB) is leaf
! turnover %. B2(20) appears to be a fudge factor Hunt threw in - called the
! water stress integral fraction. B2(7) is the growth respiration
! fraction. If there is no water stress, SWFINDX is 1, and WLIMIT
! gives the carbon needed to replace leaf turnover plus growth
! respiration costs, plus the fudge factor. As water stress increases
! WLIMIT is reduced by SWFINDX.
WLIMIT=SWFINDX*((B1(9,IB)/100.)+B2(20)) * LEAF(I)/(1.0-B2(7))
!     WLIMIT=((B1(9,IB)/100.)+B2(20)) * LEAF(I)/(1.0-B2(7))
!======================================================================
! final leaf carbon limit
!     FINALLC=MIN(CLIMIT,NLIMIT)  !DON'T NEED NEXT 2 LINES UNTIL N IS USED
!     FINALLC=MIN(WLIMIT,FINALLC)
FINALLC=MIN(CLIMIT,WLIMIT)
!======================================================================
! leaf carbon fraction of total psn
! test new algorithm.  reduce PSN by (1-grwoth respir fraction for leaf)
! in the calculation of LEAFCF.  ajm 10.03
IF(PSN.GT.0.0) THEN
   LEAFCF(I)=FINALLC/(PSN * (1.0-B2(7)))
!         LEAFCF(I)=FINALLC/PSN ! commented out ajm 10/03)
!        LEAFCF(I)=B2(11)
ELSE
!        LEAFCF(I)=MIN((LEAF(I)*(B1(9,IB)/100.))
!    +                  / (PSN + 0.000001),0.0)
!         LEAFCF(I)=0.0  !! COMMENT OUT 1/04 AJM
   LEAFCF(I)=0.37
!        LEAFCF(I)=B2(11)
ENDIF
!======================================================================
! Fine root fraction of total psn. The first term in the MIN
! expression solves for what the root allocation fraction would
! have to be for LRATIO to be maintained. The second term in the MIN
! expression assumes stems get nothing.
FROOTCF(I)=MIN(LEAFCF(I)*(1./LRATIO-1.),1.-LEAFCF(I))
!
! Calculate based on RLIMIT for root turnover as with WLIMIT for leaves, except
! increase according to SWFINDX. Change=(1+(1-SWFINDX)).
!      RLIMIT=(2.0-SWFINDX)*(B1(11,IB)/100.)*ROOT(I)/(1.0-B2(10))
!      RFRACT=RLIMIT/PSN
!      FROOTCF(I)=MIN(RFRACT,1.0-LEAFCF(I))
!      FROOTCF(I)=B2(14)
!=======================================================================
! stem carbon  !!!B2(21) REPLACES 1 IF KEEPING TRACK OF CROOT.
!     X1=B2(21)
X1=1.0
IF(PSN.GT.0.0) THEN
  STEMCF(I)=X1 * (1. - LEAFCF(I) - FROOTCF(I))
ELSE
  STEMCF(I)=0.0
ENDIF
!     STEMCF(I)=B2(12)
!=======================================================================
! coarse root carbon
!     CROOTCF= 1. - LEAFCF - FROOTCF - STEMCF
!=======================================================================
! assign stem allocation fraction for grass to be 0.0. MODIFIED 11/03 AJM
IF(ID(I).EQ.'G') THEN
   LEAFCF(I) = 0.5
   FROOTCF(I)=1-LEAFCF(I)
   STEMCF(I)=0.0
!        LEAFCF(I)=B2(11)
!        FROOTCF(I)=B2(14)
!        STEMCF(I)=B2(12)
ENDIF
! original below: ADDED ABOVE 11/03 AJM**************************************
!      IF(ID(I).EQ.'G') THEN
!         FROOTCF(I)=1-LEAFCF(I)
!         STEMCF(I)=0.0
!C        LEAFCF(I)=B2(11)
!C        FROOTCF(I)=B2(14)
!C        STEMCF(I)=B2(12)
!      ENDIF
!***************************************************************************
! Commenting out the writing of "FRACTIONS.out" for release version. ajm 11/02
!         WRITE(75,*) 'LIMITS',I,PSN,CLIMIT,WLIMIT,FINALLC,SWFINDX
 WRITE(75,100)I,ID(I),SWFINDX1,SWFINDX,CLIMIT1,CLIMIT,WLIMIT, &
                 FINALLC,LEAFCF(I),STEMCF(I),FROOTCF(I),PSN
100  FORMAT(I3, A2, 6(1X,F8.4),3(1X,F6.4),1X,F7.3)
!       WRITE(*,*) 'WATER STRESS',I,SUMWP,TLEAFON(I),SWFINDX1
!       WRITE(*,*) 'WLIMIT',I,SWFINDX,LEAF(I),B1(9,IB)
RETURN
END


SUBROUTINE BGROWN(I,FVS_CYC)
!------------------------------
! Subroutine to grow entities, zone by zone, then accumulate. Called
! daily from GSV.
! Added FVS_CYC as an argument passed from BSTNDBGC to here, and from here to RESPIRE.  ajm 2/16/01
! Still having problems with buckets going negative!  Changed "Leaf_off" condition in SOILPSI,
! Now is turning leaf off if volumetric content at 20% of volmax.  The entire logic of this process
! combined with logic between call to SOILPSI and call to CONDUCTN should be revisited.  ajm 2/28/01
!
! Another modification...Changing LWP to be equal to SWP plus 0.5 (i.e. more negative, under more
! tension [water potentials in the model are dealt with as if they are positive numbers]) Line 269, in
! CONDUCTN.   AJM 3/01.
!
! redo LWP calculation again. 6/03 make LWP = f(SWP and canopy layer Height)AJM
!
! Make respiring stemwood amount a function of crown length--more specifically,
! substitute CR for the 0.67 coefficient in the eqn CS=EXP(0.67*LOG(STEM(I)))
! circa line 493.  AJM 7/28/03
!
! This version 3R writes maintenance respiration to new local variables in RESPIRE
! Accumulate maintenance respiration by tissue pool, by entity, for the year
! Hence 3 new variables indexed by entity.  ajm 8/03
! YRMRLF(I), YRMRST(I), YRMRRT(I) [For:YeaRly MaintResp, by LeaF or STem, or RooT]
!
! Modified 11/18/03.  Recent changes for Flagstaff data involved my changing
! the way grasses are modeled.  original code had grasses on days 120-210.  For
! Flagstaff, i had changed grasses to OFF days 120-210.  This resulted in an
! explosion of grass biomass.
! I suppose grasses ought to be modeled as being respiring in summer, even
! if "off"  So, I am turning grasses back on all year.
! In the subroutines herein, I'm removing the "leaf_off" and "root_off" logic
! built into SOILPSI (which doesn't really do anything anyway), and am
! changing the logic that immediately follows the call to SOILPSI whereby the
! entity bypasses the processes if the soil is dry. Earlier, I had made the
! entity bypass not only conduction, PSN, and transpiration, but RESPIRE as
! well.  Now, if soil is dry, entity will still respire.
!
! TEST: AJM 11/21/03 The scaling up of conductance prior to call to
! Penmon-Montieth routine seems incorrect. Changing TRANSPIRATION so that Gs
! (only) is fed to Penmon (rather than Gs*LAI).  (This is how it is
! documented to happen in FOrestBGC--Running and Coughlan 1988).
! Then upon return from penmon, I will inflate the Penmon transpiration
! number by LAI(I,J) and Base(I,J).  Initial inspection suggests the
! pre-today version under-estimates transpiration consistently, relative to how
! it "should" be done. (where "should" -->documented way)
! Post-compile note: this made very little difference.  surprisingly looks
! like transpiration is actually lower with this change
!
! For Version6, added (in PHOTOSYN) the reduction of DAYPSN by ratio of all-
! to 1-sided LAI.
! For version7, reduced incident ratiation ZQ2, by a "correction factor"
! which corresponds to a ratio of silohuette LA : projected LA.
! Using Barclay's ratio of 0.23 (SilLA : Total LA) * Kolb's 2.4
! (all:one-sided), the conversion factor is 0.553.  This effectively reduces
! the PSN "efficiency" of leaves due to their orientation.  Without this
! correction, modeled leaves are all unrealistically oriented normal to
! radiation. AJM 12/03
! For version8: Am making identical change to conduction routine as made to
! PSN routine re: adjustment to PAR.  Reduce incoming PAR density by ratio of
! silhouette LA to projected LA. ajm 12/16/03
! For version9 (executable crb15) am also adding silhouette LA : projected LA
! "attenuation" to transpiration as well (on the ZQ1 radiation term)12/16/03
!-------------------------------

INTEGER FVS_CYC            !ADDED 2/01 AJM
REAL LEAFPSI
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

IB=IBLB(I)                 !to distinguish T,S,G

!--- modified for 2-soil layers  ----------------------------------------------------
SWC2(1)=SWC(1)*((AREA-BARE)/AREA)                         !*! set SWC for each
SWC2(2)=SWC(2)*((AREA-BARE)/AREA)                         !*! of 2 layers
                                                          !*!
BUCKET(1)=SWC2(1)*RATIO(I)                                !*!
BUCKET(2)=SWC2(2)*RATIO(I)                                !*!
!------------------------------------------------------------------------------------

IF(.NOT. LEAF_ON(I)) THEN  ! skip entity if leaf_on = false
   XPSN=0.0
   CALL RESPIRE(I,FVS_CYC)
   RETURN
ENDIF
!
!****************************************************
!
!    Troubleshoot again. ajm 12/00
!
!      WRITE(77,*)'I=',I,'SWC2(1)=',SWC2(1),'SWC2(2)=',SWC2(2),
!     +              'RATIO(I)=',RATIO(I)
!
!*******************************************************
!
! begin crown zone loop, from bottom to top
DO 20 L=1,NUM2
! define index variables
   I1=INDEX(L)
   J=I1
! skip looping if no leaves in layer
   IF(LAI(I,J).LE.0.0) THEN
!         WRITE(72,*) 'ENTITY NO.= ',I,' HAS LAI= ',LAI(I,J)
   GOTO 20
   ENDIF
! calculate soil water potential for entities bucket.
   CALL SOILPSI(I)
!
!*******************************************************
! Adding condition that if grass leaf is turned off in SOILPSI, then bypass the rest
! of this subroutine.  Return to BSTNDBGC.  Do not transpire, photosynthesize, or
! respire.  This change implemented as a result of earlier change, whereby grass and
! shrub entities are now can never die (their covers may get extremely small).
! Consequently, the "background" transpiration that occurs on these albiet small LAs is
! apparently enough to make water buckets go negative, thus crashing the simulation.
! Now, [(First try) if VWHC becomes <10%] [(SECOND TRY)]if SOILWP is less than wilting point (leaf
! water potential at stomatal closure), the plant shuts down (until next day), no more water
! removed via transpiration from entity's bucket.  This change is only being
! implemented for grasses (though a similar fix may in the future need to be done for
! shrubs.)
! Note: 2/5/01: This method may still have problems: (1) perhaps we should still respire. (2) Under
! current scenarios, the grass is not "shut down" for very long: the available soil water soon
! returns.  If, however, there is an extended drought, this no-maintenence-respiration problem might
! become more pronounced.
! Note that the leaf still gets turn off in SOILPSI if the bucket is less than 0.1*VOLMAX , and
! therefore, in lines 42-44 above, entity goes to 20.
! The problem with that method, however, is that turning off the leaf does not manifest until
! the NEXT DAY.  Before my change, if covers are very small, buckets are very small, and amount
! of water keeping water potentials less negative than stomatal closure threshold is small enough
! that more water than is present gets transpired.
! Perhaps this fix below should go to line 20 (therefore will respire) instead of to line 30.
! Perhaps: change the condition in SOILPSI to (IF SOILWP<B2(3), THEN LEAF_OFF), then here do: IF
! LEAF_OFF, GO TO 20.
!
   IF((ID(I).EQ.'G' .OR. ID(I).EQ.'S') .AND. SOILWP.GT.B2(3)) &
          GO TO 20
!     &       GO TO 30
!        IF(.NOT.LEAF_ON(I)) GO TO 30
!**********************************************************
! calculate conductance
   CALL CONDUCTN(I,J,LEAFPSI)
! calculate transpiration
   CALL TRANSPIR(I,J)
! calculate photosynthesis
   CALL PHOTOSYN(I,J,LEAFPSI)

! update soil water available to plant (transpiration removed)
! This feeds back into SOILPSI.
! Note: bucket is initialized for an entity, then drawn down as each layer
!  of an entity uses the water. It is zeroed before the next entity
!  is called (in GSV.FOR) -- 12/9/93, DWC.
! !*!         BUCKET=BUCKET-TRANSP   !*! old single layer version

!--- modified for 2-soil layers  ----------------------------------------------------
   IF ((ID(I).EQ.'G') .OR. (ID(I) .EQ. 'S')) THEN         !*!  added shrubs.  AJM 10/00
         BUCKET(1)=BUCKET(1) -TRANSP                      !*!  if grass [or shrub (ajm)]
   ELSE IF ( (ID(I).EQ.'T').AND.(H(I).LT.HTLIMIT) ) THEN  !*!
         BUCKET(1)=BUCKET(1) -TRANSP                      !*!  if small tree
   ELSE                                                   !*!
         IF (MAXSL.EQ.1)  THEN                            !*!  if large tree, take
             BUCKET(1)=BUCKET(1) -TRANSP                  !*!  water from bucket in
         ELSE                                             !*!  layer with 'best' SWP
             BUCKET(2)=BUCKET(2) -TRANSP                  !*!
         ENDIF                                            !*!
   ENDIF                                                  !*!
!------------------------------------------------------------------------------------

! accumulate transpiration costs and production across layers. S(16) is a multiplier
! from SITE.DAT for calibrating production. Default is 1.0.
   XPSN=XPSN+DAYPSN*S(16)
   TRANS=TRANS+TRANSP             !    total trans for all entities

!--- modified------------------------------------------------------------------------
   IF (ID(I).EQ.'G') THEN                                 !*!
     TRANS_GR = TRANS_GR + TRANSP                         !*!  grass trans
   ELSE IF (ID(I) .EQ.'S') THEN                           ! added 10/00 ajm
     TRANS_SH = TRANS_SH + TRANSP                         !     shrub transp
   ELSE IF ((ID(I).EQ.'T') .AND. (H(I).LT.HTLIMIT)) THEN  !*!
     TRANS_SM = TRANS_SM + TRANSP                         !*!  small tree trans
   ELSE                                                   !*!
     TRANS_LG = TRANS_LG + TRANSP                         !*!  large tree trans
     IF (MAXSL.EQ.1) THEN
        TRANS_LG1=TRANS_LG1 + TRANSP      !*!
     ELSE
        TRANS_LG2=TRANS_LG2 + TRANSP              !*! TRANS_LG1 = transp based on swp1 and
     ENDIF                                                !*! TRANS_LG2 = transp based on swp2
   ENDIF                                                  !*!
!------------------------------------------------------------------------------------
! Commenting out writing of "layers" output file for release version. ajm 11/02
! output entity-layer variables.
!        IF(JD.EQ.180 .AND. I.EQ.1) THEN
!        WRITE(67,*) 'NUMBER OF LAYERS = ', NUM2
!        WRITE(67,*) 'CURRENT LAYER IS = ', J
!        WRITE(67,996)
!  996   FORMAT(T2,'YR',T7,'JD',T12,'ETY',T16,'LAYER',T23,'LA(m2)',
!     +         T35,'ZQ2',T45,'SOILWP',T55,'DAYPSN')
!        ENDIF
!        IF(JD.EQ.180) THEN
!        WRITE(67,997) Z1(1), JD, I, J, LLA(I,J), ZQ2(J),SOILWP,DAYPSN
!  997   FORMAT(F3.0,1X,3(I4,1X),F10.4,F12.2,2(F10.4,1X))
!        ENDIF
!---------------------------------------------------------------------------
20 CONTINUE
!
! calculate respiration for whole entity
!      CALL RESPIRE(I)                  !ADDED FVS_CYC (See RESPIRE) ajm 2/01
CALL RESPIRE(I,FVS_CYC)
30 CONTINUE !ADDED 1/12/01 AJM
RETURN
END


SUBROUTINE SOILPSI(I)
!----------------------------
!  Calculates soil water potential(SOILWP), for water bucket of each
!  entity, from soil water content (SWC) and soil depth.
!  Maximum volumetric content is input directly or calculated from
!  input texture data in subroutine SOILH2O.
!    BUCKET=water available to entity (m3).                 ENTITY
!    S(2)=soil depth (m)                                    SITE
!    OCPNCY(I)=area occupied by entity=I (m2). From SITOC   ENTITY
!    VOLMAX=max volumetric water content. S(3) in BIOME-BGC SITE
!    H2OMAX=max water content for entity (m3);used w/ grass ENTITY
!    AA and BB are texture coefficients                     SITE
!    VOLMAX (m3/m3), AA, BB are from SOILH20 Subroutine
!----------------------------

REAL VCONT(10)
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'
!
!************************************************
!--- modified for 2-soil layers  ----------------------------------------------------
 H2OMAX(1) = S(3) * SD(1) * OCPNCY(I)                             !*!
 VCONT(1) = BUCKET(1) / (SD(1) * OCPNCY(I))                       !*!
 VCONT(1) = MAX(VCONT(1),0.01)     !ADDING NEW FIX HERE (TO PREVENT NEGATIVE WATER BUCKETS) AJM 2/28/01
!******************************************************
!
!...Troubleshoot...ajm12/00
!
!      IF(ID(I).EQ.'G') THEN
!      WRITE(81,*)'IN SOILPSI','YR=',Z1(1),'DAY=',JD,'ID(I)=',ID(I),
!     & 'VCONT=',VCONT(1),'BUCKET=',BUCKET(1),'OCPNCY=',OCPNCY(I),
!     & 'DEPTH=',SD(1),'A=',AA,'B=',BB
!      END IF
!
!*******************************************************************
!
 SWP(1) = 0.001 * AA * VCONT(1) ** BB                             !*!
                                                                  !*!
 H2OMAX(2) = S(3) * SD(2) * OCPNCY(I)                             !*!
 VCONT(2) = BUCKET(2) / (SD(2) * OCPNCY(I))                       !*!
 VCONT(2) = MAX(VCONT(2),0.01)   !LIKE FIX ABOVE; ACCOUNTS FOR NEGATIVE BUCKETS AJM 2/01
 SWP(2) = 0.001 * AA * VCONT(2) ** BB                             !*!
                                                                  !*!
   !*! next, if h20 bucket for grass empty, grass dies.           !*!
!       IF ((ID(I).EQ.'G').AND.(BUCKET(1).LT.(H2OMAX(1)*0.1)))  ! CHANGED 2/28/01 AJM
! AM COMMENTING OUT NEXT 7 LINES.  THESE HAVE BEEN OBSOLETE.  GRASSES WITH
! DRY SOIL ARE HANDLED VIA SWP TEST IMMEDIATELY FOLLOWING BGROWN'S CALL TO
! SOILPSI.
!       IF ((ID(I).EQ.'G' .OR. ID(I).EQ.'S').AND.               ! THERE ARE STILL PROBLEMS!
!     +               (BUCKET(1).LT.(H2OMAX(1)*0.2)))               !*!
!     +          THEN                                                    !*!
!          IF (LEAF_ON(I)) WRITE (*,*) 'LEAF_OFF AT JD = ', JD      !*!
!               LEAF_ON(I)=.FALSE.       !!! grass is turned off         !*!
!               FROOT_ON(I)=.FALSE.                                      !*!
!            ENDIF                                                       !*!

!***********************************************************************!*!
!
!...TROUBLESHOOT AJM 12/00
!
!      IF ((L .GE. NUM2-10) .AND. (I .LE.10)) THEN
!      WRITE(76,10)I,BUCKET(1),OCPNCY(I),VCONT(1),VCONT(2),SWP(1),SWP(2),
!     +            AA,BB
!   10 FORMAT(I2,2X,8F9.3)
!      ENDIF
! **********************************************************************!*!
!--- large trees get to choose layer with best swp, sm trees and grasses!*!
!--- are restricted to top layer.                                       !*!
IF (SWP(1).LT.SWP(2)) THEN                                        !*! check which layer
   MAXSL=1                                                        !*!   has highest SWP
ELSE                                                              !*!
   MAXSL=2                                                        !*! and set MAXSL
ENDIF                                                             !*!   to that layer
                                                                  !*!
IF ((ID(I).EQ.'G') .OR. (ID(I) .EQ. 'S')) THEN                    !*! added shrubs 10/00 ajm
   SOILWP = SWP(1)                                                !*! SWP for grass and shrub
ELSE IF ( (ID(I).EQ.'T').AND.(H(I).LT.HTLIMIT) ) THEN             !*!
   SOILWP = SWP(1)                                                !*! SWP for small trees
ELSE                                                              !*!
   SOILWP = SWP(MAXSL)                                            !*! SWP for large trees
ENDIF                                                             !*!

! -----------------------------------------------------------------------------------

RETURN
END



SUBROUTINE CONDUCTN(I,J,LEAFPSI)
!------------------
! Subroutine determines CO2 conductance rates (GSFINAL) for each
! layer of an entity's crown. Called daily.
!------------------

REAL LEAFPSI, INTRCPT, VPDSLP, DAYCONST, VPDMOD, CO2MOD
REAL TMINMOD, PSIMOD, TEMPMOD, CONDUCT, GSMAX
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

! CALCULATE MODIFIERS APPLIED TO MAXIMUM CONDUCTANCE
!    Calculate temperature modifier. Optimum temperature
!    for XPSN=B2(5). Maximum XPSN temperature=B2(6)

TEMPMOD=TEMPCRVE(Z1(14), 0.2, B2(5), B2(6))

!       IF (I.eq.18 .and. JD.eq.230)
!     +     write (*,*) I, soilwp

!    Calculate leaf water potential modifier. Soil psi is calculated
!    in subroutine SOILPSI.
!      write (*,*) ID(I),MAXSL,SOILWP,SWP(1),SWP(2)
IF(FROOT_ON(I)) THEN
!        LEAFPSI=SOILWP
!        LEAFPSI=SOILWP + 0.3   !Changed 3/01 ajm
!
! Am again changing this calculation of LWP.  Now it will be a function of
! canopy layer height.  Height to base of layer J is in common block variable
! ZONE(J). Top of canopy is CTOP(I).  For Height of leaves in canopy zone,
! I will average the heights (from ground [which is the value provided by
! variable ZONES]) of canopy layer base and the base of the canopy layer above
! (or CTOP if the layer in question is the top layer--i.e. when J=1 [remember,
! layers are defined from the top down]).
! Algorithm of LWP = SWP  minus 0.01MPa per meter in height comes from Kolb,
! personal communication.   ajm 6/03
  IF(J.EQ.1)LEAFPSI=SOILWP + (0.01*((ZONE(J)+CTOP(I))/2.))
  IF(J.GT.1)LEAFPSI=SOILWP + (0.01*((ZONE(J)+ZONE(J-1))/2.))
ELSE
  LEAFPSI=2.*SOILWP
ENDIF
IF(LEAFPSI.LT.B1(2,IB)) LEAFPSI=B1(2,IB)
IF(LEAFPSI.GT.B2(3)) LEAFPSI=B2(3)
PSIMOD=(B2(3)-LEAFPSI)/(B2(3)-B1(2,IB))
PSIMOD=MAX(PSIMOD,0.00001)
! Move the following calculation to WATER.FOR. Here it loops through crown
! layers and thus incorrectly sums daily stress through the year.
!     Calculate water stress integral (for use in carbon allocation)
!     IF(LAI(I,J).GT.0.0001 .AND. LEAF_ON(I))
!    +          SUMPSI(I)=SUMPSI(I) + LEAFPSI

!     Calculate CO2 modifier. 350 ppm has CO2MOD=1.0. After Hunt, 1993
IF(Z1(11).LT.200.) THEN
  CO2MOD=2.0
ELSE
  CO2MOD=2.0 - (1.5*(Z1(11)-200.)) / (75.+(Z1(11)-200.))
ENDIF

!     Calculate night minimum temperature modifier
IF(Z1(5).GE.0.) THEN
  TMINMOD=1.0
ELSE
  TMINMOD=1.0 + 0.125 * Z1(5)
ENDIF
TMINMOD=MAX(TMINMOD,0.000001)

!     Calculate VPD modifier.
IF(ID(I).EQ.'G') THEN
  VPDMOD=1.0
ELSE
  IF(Z1(16).GE.B2(4)) THEN
    VPDMOD=0.000001
  ELSE IF(Z1(16).LE.7.5) THEN
    VPDMOD=1.0
  ELSE
    VPDSLP=-1./(B2(4)-7.5)
    INTRCPT=1.-VPDSLP*7.5
    VPDMOD=INTRCPT+VPDSLP*Z1(16)
  ENDIF
ENDIF
! Calculate final leaf conductance (in m/sec).
! ZQ2(J) is instantaneous PAR at solar noon incident at top of canopy
! zone J (in umol/m2/sec). Defined in subroutine ABSRAD. DAYCONST is a
! constant determined from the integral of the sine curve approximating
! the fraction of a day where XPSN is light saturated.
DAYCONST=0.85
GSMAX=B1(1,IB)*1000.    ! convert to mm/sec
!     IF(LEAF_ON(I) .AND. LAI(I,J).NE.0.0) THEN  !ADDED LAI.NE.0,10/28/93
!************************************************************************
! Adjusting incoming PAR by ratio of silhouette LA to Projected LA.
! See notes in PSN, where identical change was made last week.

XZQ2= ZQ2(J)*(B2(21)*B2(15))
  CONDUCT=(GSMAX/(-B2(2)*LAI(I,J)/B2(15)))* &
             LOG((GSMAX+B2(16)*XZQ2)/(GSMAX+B2(16)* &
             XZQ2*EXP(B2(2)*LAI(I,J)/B2(15))))
!
! Original code commented out below.
!        CONDUCT=(GSMAX/(-B2(2)*LAI(I,J)/B2(15)))*
!    +          LOG((GSMAX+B2(16)*ZQ2(J))/(GSMAX+B2(16)*
!    +          ZQ2(J)*EXP(B2(2)*LAI(I,J)/B2(15))))
!**********END OF MODIFICATION   AJM 12/03********************************
!     ELSE
!       CONDUCT=0.000001
!     ENDIF
GSFINAL=CONDUCT*CO2MOD*TEMPMOD*PSIMOD*TMINMOD*VPDMOD &
           *DAYCONST / 1000.    ! 1/1000 to convert back to m/sec
GSFINAL=MAX(GSFINAL,0.000001)
GSFINAL=MIN(GSFINAL,GSMAX)
!       IF(JD.EQ.180) WRITE(*,*) 'COND--> ',I,GSFINAL
RETURN
END



SUBROUTINE TRANSPIR(I,J)
! ---------------------------
! From Hunt. BIOME-BGC. 1993
! calculate transpiration for a sub canopy
! given LAI(I,J), conductance (GSFINAL), daylight average air
! temperature (Z1(14)), vapor pressure deficit (Z1(16)), canopy
! daily absorbed radiation (ZQ1 sum in ABSRAD subroutine), boundary
! layer conductance, (B(11) in BGC), daylength (Z1(18) from BGC)
! and DAYCONST, which is the fraction of a day (.85) when
! photosynthesis at top of canopy is about light saturated.
! Output is for a sub-canopy of an entity. Expansion to per ha
! basis takes place later.
!----------------------------

REAL GSAVG, PENMON, TEMP
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

! expand conductance of sub-canopy (m/s) by LAI
!      GSAVG=LAI(I,J)*GSFINAL
! NO! do not expand! ajm 11/03
GSAVG=GSFINAL
!     IF(LEAF_ON(I) .AND. LAI(I,J).NE.0.0) THEN  !ADDED LAI.NE.0,10/28/93
! use Penman-Monteith estimation procedure for transpiration.
!
! Modifying 12/16/03 ajm (LOGIC DESCRIBED IN psn BELOW)
!
! "attenuate" radiation by ratio silhouette LA:Projected LA
  XZQ1=ZQ1(J)*(B2(21)*B2(15))
!
  CALL BPENMAN(Z1(14),Z1(16),XZQ1,B1(3,IB), &
                 GSAVG,Z1(18),PENMON)
! ORIGINAL CODE COMMENTED OUT BELOW
!
!        CALL BPENMAN(Z1(14),Z1(16),ZQ1(J),B1(3,IB),
!     +              GSAVG,Z1(18),PENMON)
!
!************** END OF MODIFICATION 12/03 AJM *****************************

  TEMP=PENMON
! convert to entity-subcanopy level and day
!        TRANSP=TEMP*BASE(I,J)*Z1(18)
! So, now we need to properly inflate: ajm 11/03
  TRANSP=TEMP*LAI(I,J)*BASE(I,J)*Z1(18)
!     ELSE
!       TRANSP=0.0
!     ENDIF
RETURN
END


SUBROUTINE PHOTOSYN(I,J,LEAFPSI)
!----------------------------
! subroutine calculates photosynthesis using the "C3 maximum rate"
! logic from BIOME-BGC, Hunt, 1993.
! (equation of Rastetter et al. 1992, Ecol. Appl. 2:55-70).
!----------------------------
! define some variables
!     B1(8,IB)=AMAX = max psn rate (umol/m2/s), B(20) in BIOME-BGC
!     NCONC     = N concentration, G(21) of B(51) in BIOME-BGC
!     B2(3)  = lwp at stomatal closure(-MPa). B(9) in BIOME-BGC
!     B1(2,IB)  = minimum lwp. B(8) in Biome BGC
!     B1(13,IB) = specific leaf area by T,S,G. B(2) in Biome BGC
!     B2(18)    = fraction of carbon in dry matter. B(38)
!     B2(5) = optimum temperature XPSN. B(21) in BIOME BGC
!     B2(6) = max temp XPSN. B(22) in BIOME-BGC
!     B2(15)  = ratio all sided to one sided LA
!     B2(2)  = extinction coeffeicient. B(5) in BIOME BGC
!----------------------------------------

REAL AMAX, LEAFPSI, DAYCONST, EPSILON, O2, TAU25, CA
REAL Q10TAU, TAU, GAMMA, C, ALPHA, PHOTO, NCONC
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

! do photosynthesis if leaves are 'on'.
!     IF(LEAF_ON(I) .AND. LAI(I,J).NE.0.0) THEN  !ADDED LAI.NE.0,10/28/93
! Get maximum rate from input (MAX PSN RATE) or from leaf nitrogen (NCONC)
! NOTE: NCONC=0.01 SHOULD BE MOVED TO A B2(n) VARIABLE-10/29/93,DWC
IF(ANINT(B1(8,IB)*100.).GT.0.0) THEN
  AMAX=B1(8,IB)
ELSE
  NCONC=0.01      !(fraction dry-weight)
  AMAX=((NCONC*1000./14.)*105.-33.1)/(B1(13,IB)*B2(18))
ENDIF
! Adjust AMAX by water stress
AMAX=AMAX*(B2(3)-LEAFPSI)/(B2(3)-B1(2,IB))
! Adjust AMAX by temperature
AMAX=AMAX*TEMPCRVE(Z1(14),0.2,B2(5),B2(6))
! Adjust ALPHA (quantum yield) by temperature and CO2
DAYCONST=0.85
EPSILON=0.08
O2=0.2
TAU25=2360.
CA=Z1(11)*Z1(12)/100000.
Q10TAU=0.74
TAU=TAU25*EXP(((Z1(14)-25.)/10.)*LOG(Q10TAU))
GAMMA=0.5 * 1000000. * O2 / TAU
C=0.7*CA
ALPHA=(EPSILON-0.001*Z1(14))*(C-GAMMA)/(C-2.*GAMMA)
! ***************************************************************************
! AJM 12/03
! COMMENTING OUT ORIGINAL CODE.  REPLACING ZQ2 WITH NEW VARIABLE XZQ2.
! XZQ2 REPRESENTS ZQ2, ADJUSTED FOR THE INCLINATION OF LEAVES (AWAY FROM
! HORIZONTAL).
! USING BARCLAY'S SILOHUETTE LA : TOTAL LA OF 0.23, AND KOLB'S ALL:ONE SIDED
! LA OF 2.4, WE GET A CORRECTION FACTOR OF 0.553, REPRESENTING A "CORRECTION"
! FROM PROJECTED LEAF AREA TO SILOUETTE LA.  THE PSN EQUATIONS AND BEER'S LAW
! STILL APPLY; i.e. WE'RE STILL ASSUMING VERTICAL LIGHT PENETRATION THROUGH A
!ANOPY OF HORIZONTAL LEAVES INSOFAR AS AMAX, LIGHT EXTICTION COEFFICIENT (K)
! AND LAI ARE CONCERNED.  THIS CORRECTION REPRESENTS AN ALTERNATIVE METHOD OF
! ADDRESSING THE ISSUE OF THE LEAVES NOT REALLY EXISTING AS MODELED. THIS
! ADDRESSES, IN AN ALTERNATIVE FASHION, WHAT COULD BE ACCOMPLISHED BY
! SIMULTANEOUSLY MEASURING AMAX, K, AND SILOHUETTE LA ON A "NATURAL
! ORIENTATION" BASIS.  HERE, WE SIMULATE THE "TILT THE LEAVES" BY "TILTING
! THE LIGHTSOURCE".  WE'RE LOWERING THE PHOTON FLUX DENSITY BY AN AMOUT
! PROPORTIONAL TO THE RATIO OF SILOUETTE LA : PROJECTED LA.

! Calculate photosynthesis for each entity/layer (umol CO2).
! Ajust (downaward) ZQ2 to simulate the effect of the "natural orientation"
! of leaves (they are not all horizontal and normal).
! NOTE!! Am "co-opting" unused parameter B2(21).  Now it is ratio of
! silhouette leaf area to all sided LA.

XZQ2= ZQ2(J)*(B2(21)*B2(15))
IF(XZQ2.LT.0.1) XZQ2=0.0
PHOTO=(AMAX/(-B2(2)*LAI(I,J)/B2(15))) &
    *LOG((AMAX+ALPHA*XZQ2) / &
    (AMAX+ALPHA*XZQ2*EXP(B2(2)*LAI(I,J)/B2(15))))
!
! ORIGINAL CODE BELOW
!
!C Calculate photosynthesis for each entity/layer (umol CO2).
!      IF(ZQ2(J).LT.0.1) ZQ2(J)=0.0
!      PHOTO=(AMAX/(-B2(2)*LAI(I,J)/B2(15)))
!     + *LOG((AMAX+ALPHA*ZQ2(J)) /
!     + (AMAX+ALPHA*ZQ2(J)*EXP(B2(2)*LAI(I,J)/B2(15))))

! END OF 12/03 XZQ2 MODIFICATION AJM.
!***************************************************************************
! Convert umol CO2 for the entity/layer to kg C/m2 layer area/day
! (with 12/1e9, BASE & LAI, Z1(18)-daylength in sec & DAYCONST).
DAYPSN=PHOTO*Z1(18)*LAI(I,J)*BASE(I,J)*DAYCONST*(12./1.0E9)
!
! Don't know who made the comment below, but they were right on.
! Yes, we need to reduce PSN.  "PHOTO" is on a per projected m2 basis
! when we calculate daypsn above, and inflate by entity-layer LA, we
! should be doing so on a projected LA basis, not on a total (all-sided) LA
! basis.
!
DAYPSN = DAYPSN/B2(15)
! Tried changing LAI all sided to 1 sided
!     DAYPSN=DAYPSN/2.3
!     ELSE      !Goes with the LEAF_ON IF-THEN-ELSE loop
!       PHOTO=0.0
!       DAYPSN=0.0
!     ENDIF
RETURN
END


SUBROUTINE RESPIRE(I,FVS_CYC)
!----------------------------
! Calculates maintenence and growth respiration for leaves,
! stems, coarse roots and fine roots. Subroutine called
! daily for each entity, after photosynthesis has been calculated.
! Units are in kgC/entity/day.
!----------------------------

INTEGER FVS_CYC                              !ADDED 2/01 AJM
REAL DARK, CS, XLEAFR, STEMR, FROOTR, XXPSN
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

! Initialize the carbon allocation fractions for year 1
! Added the new condition below so that the C allocation fractions are only initialized once per
! simulation, and NOT at every cycle boundary.  ajm 2/16/01
!      IF(Z1(1).EQ.1) THEN
IF(FVS_CYC .EQ. 1 .AND. Z1(1).EQ.1) THEN
   IF(ID(I).EQ.'G') THEN
     LEAFCF(I)=0.5
     STEMCF(I)=0.0
     FROOTCF(I)=0.5
   ELSE
     LEAFCF(I)=B2(11)
     STEMCF(I)=B2(12)
     FROOTCF(I)=B2(14)
   ENDIF
END IF
!
! calculate night maintenence respiration for leaves.
IF(LEAF_ON(I)) THEN
  DARK=1.-Z1(18)/86400.
  XLEAFR=B1(4,IB)*EXP(B2(17)*Z1(15))*DARK*LEAF(I)
ELSE
  XLEAFR=0.0
ENDIF
! Calculate maintenence respiration of stems. Use logic from BGC.
! Need to change this portion to a surface area or sapwood basis.
IF(ID(I).EQ.'T') THEN
   CS=EXP(CR(I)*LOG(STEM(I))) !from p139 in Running & Coughlan
!!!!ADDED SAPWOOD RESPIRATION CALCULATION--5/17/94,DWC
!!!!NOT YET WORKING CORRECTLY.
!        CALL SAPWOOD(I,SAPC)
!        CS=SAPC
ELSE
   CS=STEM(I)       !Do not calculate CS for shrubs & grasses
ENDIF
STEMR=B1(5,IB)*EXP(B2(17)*Z1(7))*CS
! Now do coarse roots
!     CROOTR=B1(6,IB)*EXP(B2(17)*Z1(20))*CROOT(I)
!     CROOTR=MAX(CROOTR,0.0)
! now do fine roots (as of 10/20/93 froots is total)
IF(FROOT_ON(I)) THEN
  FROOTR=B1(7,IB)*EXP(B2(17)*Z1(20))*ROOT(I)
ELSE
  FROOTR=0.0
ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Adding new accumulators for yearly maintResp by tissue pool, by entity.
! AJM 8/03  Added to commonblock ENTITY.F77
!
YRMRLF(I)=YRMRLF(I)+XLEAFR
YRMRST(I)=YRMRST(I)+STEMR
YRMRRT(I)=YRMRRT(I)+FROOTR
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! sum across components for total maintenance respiration
TOTMRESP=XLEAFR+STEMR+FROOTR
! SUBTRACT MAIN. RESP. FROM PSN--SEE PROCEDURE PLANT_RESPIR IN
! BIOME'S BIO_DAY.PAS--2/1/94, DWC.
XXPSN=XPSN-TOTMRESP
! calculate growth respiration. Must have photosynthesis, XPSN,
! minus total maintenance respiration for each entity.
IF(XXPSN.GT.0.0) THEN
   GRLEAF=XXPSN*LEAFCF(I)*B2(7)
   GRSTEM=XXPSN*STEMCF(I)*B2(8)
   GRFROOTS=XXPSN*FROOTCF(I)*B2(10)
!        GRCROOTS=XXPSN*B2(13)*B2(9)
ELSE
   GRLEAF=0.0
   GRSTEM=0.0
   GRFROOTS=0.0
!        GRCROOTS=0.0
ENDIF
! total growth respiration (kgC/entity/day)
TOTGRESP=GRLEAF+GRSTEM+GRFROOTS
! total respiration (kgC/entity/day)
TOTRESP=TOTMRESP + TOTGRESP
! total above ground (leaves and stems) respiration
ABGRESP=GRLEAF+GRSTEM+STEMR+XLEAFR
RETURN
END


!-------------------------------------------
FUNCTION TEMPCRVE(T,A,TOPT,TMAXP)
!-------------------------------------------
!  T=daylight average temperature = Z1(14)
!  A=.2 for C3 plants
!  TOPT=optimum temperature for photosynthesis = B2(5)
!  TMAXP=maximum temperature for photosynthesis = B2(6)

REAL A, T, X, Y, W, TMINP, TMAXP, TOPT, TEMPCRVE
TMINP=0.0
IF(T.GT.TMINP .AND. T.LT.TMAXP) THEN
  X=TMAXP-TOPT
  Y=EXP(LOG((TMAXP-T)/X)*A*X)
  W=Y*EXP(A*(T-TOPT))
ELSE
  W=0.0
ENDIF
TEMPCRVE=W
RETURN
END




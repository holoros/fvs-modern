SUBROUTINE BZCOMP(FVS_CYC)
!-------------------------------------------------
! THIS SUBROUTINE CALCULATES THE Z VARIABLES
! NECESSARY FOR THE GROWTH SUBMODEL
!   NOTE: ADD LAI EFFECT ON SOIL TEMP AT FUTURE TIME-10/28/93,DWC
!-------------------------------------------------
!  Z1(1)  =  YEAR
!  Z1(2)  =  JD
!  Z1(3)  =  PRECIPITATION (m)
!  Z1(4)  =  MAX. AIR TEMP (degree C)
!  Z1(5)  =  MIN. AIR TEMP (degree C)
!  Z1(6)  =  RELATIVE HUMIDITY (%)
!  Z1(7)  =  AVG. 24-hr TEMP (degree C)
!  Z1(8)  =  SHORTWAVE RADIATION (kJ/m2/day)
!  Z1(9)  =  MAX PAR (noon, umol/m2/s)
!  Z1(10) =  LAI (all-sided)
!  Z1(11) =  ATMOSPHERIC CO2 CONCENTRATION (ppm)
!  Z1(12) =  ATMOSPHERIC PRESSURE (Pa)
!  Z1(13) =  AVERAGE WIND SPEED (m/s)
!  Z1(14) =  AVERAGE DAYTIME AIR TEMPERATURE (degree C)
!  Z1(15) =  AVERAGE NIGHT MIN. TEMPERATURE (degree C)
!  Z1(16) =  VAPOR PRESSURE DEFICIT (mbar)
!  Z1(17) =  ABSOLUTE HUMIDITY DEFICIT (ug/m3)
!  Z1(18) =  DAYLENGTH (s)
!  Z1(19) =  CANOPY DAILY ABSORBED RADIATION (kJ/m2/day)
!  Z1(20) =  SOIL TEMPERATURE (degree C)
!-------------------------------------------------

INTEGER FVS_CYC                         !ADDED 2/01 AJM
REAL XD, AMPL, DAY, ESD, ES, VPD, PTAIR
INCLUDE 'ENTITY.f90'
INCLUDE 'SITE.f90'

! Calculate year
IF(YRFLAG.EQ.1) THEN
  Z1(1)=INT(S(11))
  YRFLAG=0
ELSE IF(JD.EQ.1) THEN
  Z1(1)=Z1(1) + 1
ENDIF

! Calculate daylength
Z1(2)=JD*1.
XD=Z1(2) - 79.
IF(XD.LT.0.0) XD=286. + Z1(2)
AMPL=EXP(7.42+0.045*LAT) / 3600.
DAY=AMPL * (SIN(XD*0.01721)) + 12.
Z1(18)=DAY * 3600.

! Convert PPT in mm to PPT in meters & assign variables
Z1(3)=PPT / 1000. * S(15)
Z1(4)=TMAX
Z1(5)=TMIN
Z1(6)=RH
Z1(7)=(Z1(4)+Z1(5)) / 2.
Z1(8)=XRAD * (1.-S(6))    !(kJ/m2/day)

! CO2 concentration
Z1(11)=350.0

! Average wind speed (m/s)
Z1(13)=1.0

! Average daytime and nightime temperatures
Z1(14)=0.212 * (Z1(4)-Z1(7)) + Z1(7)
Z1(15)=(Z1(14) + Z1(5)) / 2.

! Atmospheric pressure in Pa from elevation in meters
Z1(12)=101300. / EXP( ELEV/(29.3*(Z1(14)+273.)) )

! Compute absolute humidity deficit
ESD=6.1078 * EXP( (17.269*Z1(14)) / (237.3+Z1(14)) )
ES=Z1(6)/100. * ESD
VPD=ESD - ES
Z1(16)=MAX(VPD,0.0)
Z1(17)=217.0E-6 * Z1(16)/(Z1(14)+273.16)

! Compute soil temperature
! Added condition so that this only done in first cycle
!      IF(Z1(1).EQ.1 .AND. JD.EQ.1) THEN  !Initialize 2 variables
IF(FVS_CYC .EQ. 1 .AND. Z1(1).EQ.1 .AND. JD.EQ.1) THEN  !Initialize 2 variables
   PTAIR=TMIN
   SNOWPACK=S(4)
ENDIF
IF(SNOWPACK.GT.0.0 .OR. PTAIR.LT.0.0) THEN
   Z1(20)=0.1 * (Z1(7) - PTAIR) + PTAIR
ELSE
   Z1(20)=0.25 * (Z1(7) - PTAIR) + PTAIR
ENDIF
PTAIR=Z1(7)

! Zero-out unused Z variables
Z1(9)=0.0
Z1(10)=0.0
Z1(19)=0.0
RETURN
END

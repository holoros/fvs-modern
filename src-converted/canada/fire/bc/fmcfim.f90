SUBROUTINE FMCFIM (IYR, FMD, UWIND, IBYRAM, FLAMEHT, CANBURN, ROS)
IMPLICIT NONE
!----------
! CANADA-FIRE-BC $Id$
!----------
!
!     CALLED FROM: FMBURN
!                  FMPOFL
!     CALLS cfim.cpp
!
!  PURPOSE:
!     THIS SUBROUTINE IS THE INTERFACE BETWEEN FFE AND THE CFIM FIRE
!     CALCULATIONS
!
!  CALL LIST DEFINITIONS:
!     IYR:  CURRENT YEAR
!     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
!
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
INCLUDE 'FMPARM.f90'
INCLUDE 'FMCOM.f90'
INCLUDE 'FMFCOM.f90'
INCLUDE 'CONTRL.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'METRIC.f90'
   $ ATTRIBUTES DLLIMPORT :: CFIM_DRIVER &
   _WIN64) &
   $ ATTRIBUTES ALIAS:'_CFIM_DRIVER' :: CFIM_DRIVER

!
!OMMONS
!
integer iyr, RC
integer FMD
real CFIM_OUTPUT(10)
REAL FMINFO(13), LHV
logical debug
integer I,J
REAL MWIND, KTEMP, UWIND
REAL IBYRAM, FLAMEHT, ROS
INTEGER CANBURN, INB

integer cfim_driver

!     check for debug

call dbchk (debug,'FMCFIM',6,icyc)
if (debug) write(jostnd,7) icyc, iyr
7 format(' FMCFIM cycle=',i2,' iyr=',i5)

!     ALL CFIM_INPUT VARIABLES THAT ARE NOT SET HERE
!     ARE SET BY KEYWORD IN FMIN

!     CONSTANTS
!     ignition_temperature(K)---igtemp              600.0
CFIM_INPUT(6) = 600.0

!     heat_content(J/kg)---hc                       18600.0
CFIM_INPUT(18) = 18600.0

!     DEPENDS ON STAND CONDITION OR USER-DEFINED FIRE VARIABLES
!     10m_wind(m/s)---10   (test=7)
MWIND = UWIND * MItoKM * 1000. / (60. * 60.)
CFIM_INPUT(1) = MWIND

!     slope - percent (test=0)
CFIM_INPUT(2) = FMSLOP

!     ambient_temperature(K)---Ta (test = 300)
KTEMP = ATEMP*FtoC1+FtoC2 + 273.15
CFIM_INPUT(3) = KTEMP

!     stand_height(m)---sh                          13.0
CFIM_INPUT(4) = AVH * FTtoM

!     wind_attenuation_coefficient---alpha          1.2
!     'HOW TO CALCULATE??
!       CURRENTLY SET TO 1, BECAUSE USING A WIND FACTOR THAT HAS ALREADY BEEN REDUCED
CFIM_INPUT(5) = 1

!     SURFACE_INPUTS
!     fuel_model_number---FuelModelNumber                   2
CFIM_INPUT(10) = FMD

!     1_hour_fuel_moisture(fraction)---FuelMoisture[0]          0.05
!     10_hour_fuel_moisture(fraction)---FuelMoisture[1]         0.06
!     100_hour_fuel_moisture(fraction)---FuelMoisture[2]        0.07
!     live_herbaceous(fraction)---FuelMoisture[3]               0.3
!     live_woody(fraction)---FuelMoisture[4]                    1.0
I = 10
DO J = 1, 3
    I = I + 1
    CFIM_INPUT(I) = MOIS(1,J)
ENDDO
CFIM_INPUT(14) = MOIS(2,1)
CFIM_INPUT(15) = MOIS(2,2)

!     canopy_base_height(m)---canbaseht                 5.0
CFIM_INPUT(20) = ACTCBH * FTtoM

!     foliar_moisture_content---FMC                     1.0
!       NOTE THAT FOLMC IS ON SCALE 0-100, AND WE NEED 0-1
CFIM_INPUT(22) = FOLMC / 100.0

!     canopy_fuel_density(kg/m^3)---rho_can             398.0
!     DO NOT USE CROWN BULK DENSITY (already in kg/m3)
!       CBD IS NOT THE SAME AS CANOPY FUEL DENSITY
!       FOR NOW, KEEP CANOPY FUEL DENSITY AS A CONSTANT
CFIM_INPUT(24) = 398.0


!       PREDICTED CONSUMPTION OF DUFF, LITTER
!       THIS IS NOW DONE IN FMCONS, INCLUDING LOADING THE CFIM_INPUT ARRAY
!      CFIM_INPUT(25) = FFC + WFC

FLAMEHT = 0.0
IBYRAM = 0.0
ROS = 0.0

!     The CFIM model does not have access to FM common blocks.
!     Therefore, load Fuel model information into an array to pass
!     to it.
DO 800 INB = 1,MXFMOD
    IF (FMOD(INB).EQ.0 .OR. FWT(INB) .LE. 0.0) GOTO 800

!       LOAD UP THE FUEL MODEL VARIABLES
!        FOR NOW, JUST USE THE MAIN FUEL MODEL - WE WILL NEED TO CHANGE THIS LATER
!        CALL FMGFMV(IYR, FMD)
    CALL FMGFMV(IYR, FMOD(INB))

!       FUEL LOADING (convert from LB/FT2 to T/A)
    FMINFO(1) = FWG(1,1) * 43560./2000.
    FMINFO(2) = FWG(1,2) * 43560./2000.
    FMINFO(3) = FWG(1,3) * 43560./2000.
    FMINFO(4) = FWG(1,4) * 43560./2000.
    FMINFO(5) = FWG(2,1) * 43560./2000.

!       SAV RATIO (1/FT)
    FMINFO(6) = MPS(1,1)
    FMINFO(7) = MPS(2,1)
    FMINFO(8) = MPS(2,2)

!       HEAT CONTENT (LOGIC COPIED FROM FMFINT)
      IF (FMD .EQ. 106) THEN
        LHV = 9000.0
      ELSEIF (IFLOGIC .EQ. 2) THEN
        LHV = ULHV
      ELSE
        LHV = 8000.0
      ENDIF
    FMINFO(9) = LHV
    FMINFO(10) = LHV
    FMINFO(11) = LHV

!       DEPTH AND MOIS OF EXTINCTION
    FMINFO(12) = DEPTH
    FMINFO(13) = MEXT(1)

  RC = 0
  RC = cfim_driver(CFIM_INPUT,CFIM_OUTPUT,FMINFO)

!       CFIM_output[0] = maxparttemp;
!       CFIM_output[1] = flag;0.

!       CFIM_output[2] = iByram;
!       CFIM_output[3] = taur;
!       CFIM_output[4] = ROS;
!       CFIM_output[5] = flamedepth;
!       CFIM_output[6] = flameheight;
!       CFIM_output[7] = FlameLength;

!       CHECK UNITS. IS THIS BTU\M\MIN?
!          IBYRAM = CFIM_OUTPUT(3) / MtoFT
!          FLAMEHT = CFIM_OUTPUT(7) * MtoFT
!       FLAG > 0 MEANS THAT THE CANOPY INGITED
!         DON'T USE WEIGHTING FOR THIS ONE - IF ANY FUEL
!         MODEL INGITES THE CANOPY, THEN SET THAT.
    IF (CANBURN .EQ. 0) CANBURN = INT(CFIM_OUTPUT(2))

    FLAMEHT = FLAMEHT + CFIM_OUTPUT(7) * FWT(INB)
    IBYRAM = IBYRAM + CFIM_OUTPUT(3) * FWT(INB)
    ROS = ROS + CFIM_OUTPUT(5) * FWT(INB)

800  CONTINUE
!       CHECK UNITS. IS THIS BTU\M\MIN?
    IBYRAM = IBYRAM / MtoFT
    FLAMEHT = FLAMEHT * MtoFT
!       CHECK UNITS. ROS IS IN M/S FROM CFIM. CHANGE TO FT/MIN (MtoFT / (1/60) = .00508)
    ROS = ROS / 0.00508

return
end

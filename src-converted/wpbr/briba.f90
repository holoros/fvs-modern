SUBROUTINE BRIBA
IMPLICIT NONE
!----------
! WPBR $Id$
!----------
!  Purpose:
!  BRIBA calculates the Rust Index based on Basal Area.
!
!  The functions used to determine the rust index from basal area were
!  provided by Geral McDonald, Intermountain Research Station, Moscow,
!  ID. Additional documentation can be found in Research Paper INT-258,
!  Computer Simulation of White Pine Blister Rust Epidemics, pp. 81-83.
!  Julie Williams-Cipriani provided assistance with the implementation
!  of these functions in the model code.
!
!  Rust Index is calculated once per cycle. The calculation is a three
!  step process.  Proportion Full Sunlight (PFS) is calculated first,
!  then Ribes Denity (RD) is calculated from PFS, and finally the
!  Rust Index (RI) is calculated.
!
!  BRIBA is called from BRSETP at initialization (cycle 0) and from
!  BRTREG each cycle depending on RI assignment method.
!----------------------------------------------------------------------
!
!  Revision History:
!
!  dd-MMM-YYYY programmer_name
!     description of change or update.
!
!**********************************************************************

!.... Common include files.

INCLUDE 'PRGPRM.f90'
INCLUDE 'BRCOM.f90'
INCLUDE 'PLOT.f90'
INCLUDE 'CONTRL.f90'

!.... Local variable declarations.
!....    BRI    - Blister Rust Index.
!....    RD     - Array of 3. Ribes Density (Hud, Lac, Vis)
!....    RDP    - Ribes density multiplied by proportion of total pop.
!....    PFS    - Proportion Full sunlight
!....    I      - Counter for ribes species index.

REAL PFS,RD(3),BRI,RDP
INTEGER I
LOGICAL DEBUG

!.... See if we need to do some debug.

CALL DBCHK(DEBUG,'BRIBA',5,ICYC)
IF(DEBUG) WRITE(JOSTND,10) ICYC
10 FORMAT('Entering subroutine BRIBA: cycle = ',I2)

!.... Calculate Proportion Full Sunlight using the FVS current
!.... basal area (BA from common block PLOT);
!.... BA is stored in FVS as feet**2/acre --  the original calculation
!.... was based on meters**2/hectare.  The .00459 constant comes from
!.... dividing the original .02 by 4.36 which is the conversion factor
!.... from m2/ha to f2/ac.

PFS=EXP(-(0.00459*BA))

!.... Calculate Ribes Density for all three Ribes species.
!.... The original functions are hectares; therefore, they are
!.... divided by 2.47 for conversion to acres.
!.... (bushes/hectare divided by acres/hectare = bushes/acre)

RD(1)=(0.05+(2.15*(PFS**16.38)))/2.47
RD(2)=(40.0+(190.0*(PFS**10.96)))/2.47
RD(3)=(40.0+(660.0*(PFS**27.03)))/2.47

!.... Calculate Rust Index value. Weighted average is applied to
!.... ribes density before calculation of rust index.

RIDEF=0.0
DO 50 I=1,3

!....    The ribes population values (RIBPRP) are the proportions listed
!....    in fields 4, 5, and 6 of the RUSTINDX keyword.
!....    If RIBPRP is 0 for any of the ribes species, don't do the
!....    calculation for that species (even when a 0 is entered for RDP
!....    you get a value anyway for BRI and it skews the rust index RI).

   IF(RIBPRP(I).GT.0) THEN
      RDP=RD(I)*RIBPRP(I)
      BRI=(0.499675+0.4*ATAN((RDP/150.0)-3))*RSF(I)
      RIDEF=RIDEF+BRI
   ENDIF
50 CONTINUE

!.... Common return.

IF(DEBUG) WRITE(JOSTND,60) RIDEF,ICYC
60 FORMAT(27X,'RIDEF = ',F10.8,/, &
          ' Leaving subroutine BRIBA: cycle = ',I2)
RETURN
END

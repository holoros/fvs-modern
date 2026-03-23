SUBROUTINE MISINT
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Mistletoe parameter initialization routine. This routine is
!  variant dependent and sets the variant dependent variables for other
!  mistletoe routines. This is the Blue Mountain version.
!----------------------------------------------------------------------
!
!  Call list definitions:
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on and off.
!     AFIT:   Array of MISFIT data.
!     ACSP:   Array of CSPARR data.
!     ADGP:   Array of DGPDMR data.
!     APMC:   Array of PMCSP data.
!
!  Common block variables and parameters:
!     CSPARR: From MISCOM; 2-char. representations of all species.
!     DGPDMR: From MISCOM; diameter growth potentials based on species
!                and DMR (0-6).
!     ICYC:   From CONTRL; cycle index number.
!     JOSTND: From CONTRL; logical unit number for stand output.
!     MISFIT: From MISCOM; tells which species are affected by DM.
!     PMCSP:  From MISCOM; percent mortality coefficients by species.
!
!  Revision History :
!     03/01/95 - Lance R. David (MAG)
!                Exception values for growth modification replaced by
!                values described in the Interim Dwarf Mistletoe Impact
!                Modeling System Users Guide and Reference Manual.
!  12-JUL-2011 Lance R. David (FMSC)
!    Added arrays for height growth impacts.
!    Impact values must be supplied by MistHMod keyword.
!
!**********************************************************************
IMPLICIT NONE

!.... Parameter statements.

!.... Parameter include files.

INCLUDE 'PRGPRM.f90'

!.... Common include files.

INCLUDE 'CONTRL.f90'
INCLUDE 'MISCOM.f90'

!.... Variable declarations.

LOGICAL DEBUG
REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
CHARACTER*2 ACSP(MAXSP)
INTEGER I,J,AFIT(MAXSP)

!.... Data statements.

!.... Species character representations

DATA (ACSP(I),I=1,MAXSP) &
      /'WP','WL','DF','GF','MH','WJ','LP','ES','AF','PP', &
       'WB','LM','PY','YC','AS','CW','OS','OH'/

!.... Species affected by mistletoe

DATA (AFIT(I),I=1,MAXSP) &
      /  0,   1,   1,   0,   0,   0,   1,   0,   0,   1, &
         1,   1,   0,   0,   0,   0,   0,   0/

!.... Diameter growth rates
!.... 03/01/95 - Lance R. David (MAG)
!....    Exception values replaced by values described in
!....    Interim Dwarf Mistletoe Impact Modeling System
!....    Users Guide and Reference Manual, February, 1993
!....    Pages 20, 24 and 28 for species DF, LP, PP.
!....    The original values found on page 31 were:
!....    &   1.0,.83,.83,.83,.69,.68,.63,
!....    &   1.0,1.0,1.0,.93,.93,.92,.92,
!....    &   1.0,.94,.94,.94,.86,.72,.69,

DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)/ &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !WP
      1.0, .94, .92, .88, .84, .58, .54, &  !WL
      1.0, .98, .97, .85, .80, .52, .44, &  !DF
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !GF
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !MH
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !WJ
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !LP
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !ES
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !AF
      1.0, 1.0, 1.0, .98, .86, .73, .50, &  !PP
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !WB
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !LM
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !PY
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !YC
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !AS
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !CW
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &  !OS
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/     !OH

!.... Height growth potential rates
!....
!.... Using Douglas-fir height growth impact values described in:
!....
!.... Marshall, Katy 2007. Permanent plots for measuring spread and
!.... impact of Douglas-fir dwarf mistletoe in the Southern Oregon
!.... Cascades, Pacific Northwest Region: Results of the ten year
!.... remeasurement. USDA Forest Service, Pacific Northwest Region,
!.... Southwest Oregon Forest Insect and Disease Service Center,
!.... Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
!....
!.... Default values for DF in this table would be:
!.... &   1.0,1.0,1.0,.95,.65,.50,.10,
!.... So that impacts are not unknowingly applied to projections,
!.... the values must be supplied with the MistHMod keyword.
!.... when appropriat default values are developed, they will be
!.... set here.

DATA ((AHGP(I,J),J=1,7),I=1,MAXSP) &
     /1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/

!.... Mortality coefficients

DATA ((APMC(I,J),J=1,3),I=1,MAXSP)/ &
          0.0,      0.0,      0.0, &  !WP
      0.01319, -0.01627,  0.00822, &  !WL
      0.01319, -0.01627,  0.00822, &  !DF
          0.0,      0.0,      0.0, &  !GF
          0.0,      0.0,      0.0, &  !MH
          0.0,      0.0,      0.0, &  !WJ
      0.00112,  0.02170, -0.00171, &  !LP
          0.0,      0.0,      0.0, &  !ES
          0.0,      0.0,      0.0, &  !AF
      0.00681, -0.00580,  0.00935, &  !PP
      0.00112,  0.02170, -0.00171, &  !WB
      0.00112,  0.02170, -0.00171, &  !LM
          0.0,      0.0,      0.0, &  !PY
          0.0,      0.0,      0.0, &  !YC
          0.0,      0.0,      0.0, &  !AS
          0.0,      0.0,      0.0, &  !CW
          0.0,      0.0,      0.0, &  !OS
          0.0,      0.0,      0.0/     !OH

!.... Check for debug.

CALL DBCHK(DEBUG,'MISINT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,10)ICYC
10 FORMAT(' Begin/end MISINTBM: Cycle = ',I5)

!.... Mistletoe model initializations.

DO 200 I=1,MAXSP
   MISFIT(I)=AFIT(I)
   CSPARR(I)=ACSP(I)
   DO 100 J=1,7
      DGPDMR(I,J)=ADGP(I,J)
      HGPDMR(I,J)=AHGP(I,J)
100    CONTINUE
   DO 150 J=1,3
      PMCSP(I,J)=APMC(I,J)
150    CONTINUE
200 CONTINUE

RETURN
END

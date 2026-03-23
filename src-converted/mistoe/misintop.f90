SUBROUTINE MISINT
!**********************************************************************
! MISTOE $Id$
!----------------------------------------------------------------------
!  Purpose:
!     Mistletoe parameter initialization routine. This routine is
!  variant dependent and sets the variant dependent variables for other
!  mistletoe routines. This is the Pacific Northwest Coast version.
!----------------------------------------------------------------------
!
!  Call list definitions:
!
!  Local variable definitions:
!     DEBUG:  Logical flag to turn debug on and off.
!     AFIT:   Array of MISFIT data.
!     ACSP:   Array of CSPARR data.
!     ADGP:   Array of DGPMDR data.
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
!     19-OCT-2005 Lance R. David (FHTET)
!       Corrected specie codes to match specie codes in FVS.
!  21-APR-2009 Lance R David (FMSC)
!    Changed species codes CO to CW (Black Cottonwood),
!    J to WJ (Western Juniper), BC to CH (Bitter Cherry).
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
      /'SF','WF','GF','AF','RF','SS','NF','YC','IC','ES', &
       'LP','JP','SP','WP','PP','DF','RW','RC','WH','MH', &
       'BM','RA','MA','TO','GC','AS','CW','WO','WJ','LL', &
       'WB','KP','PY','DG','HT','CH','WI','**','OT'/

!.... Species affected by mistletoe

DATA (AFIT(I),I=1,MAXSP) &
      /  0,   0,   0,   1,   1,   0,   0,   0,   0,   0, &
         1,   0,   0,   0,   0,   1,   0,   0,   1,   1, &
         0,   0,   0,   0,   0,   0,   0,   0,   0,   0, &
         0,   0,   0,   0,   0,   0,   0,   0,   0/

!.... Diameter growth rates

DATA ((ADGP(I,J),J=1,7),I=1,MAXSP) &
      /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, .98, .95, .70, .50, &
      1.0, 1.0, 1.0, .98, .95, .70, .50, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, .98, .97, .85, .80, .52, .44, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, .82, .82, .82, &
      1.0, 1.0, 1.0, .98, .86, .73, .50, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/

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
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
      1.0,1.0,1.0,1.0,1.0,1.0,1.0/

!.... Mortality coefficients

DATA ((APMC(I,J),J=1,3),I=1,39) &
      /0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.00159,  0.00508, &
      0.0,  0.00159,  0.00508, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.00112,  0.02170,  -0.00171, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.01319,  -0.01627,  0.00822, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.00681,  -0.00580,  0.00935, &
      0.00681,  -0.00580,  0.00935, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0, &
      0.0,  0.0,  0.0/

!.... Check for debug.

CALL DBCHK(DEBUG,'MISINT',6,ICYC)

IF(DEBUG) WRITE(JOSTND,10)ICYC
10 FORMAT(' Begin/end MISINTWC: Cycle = ',I5)

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

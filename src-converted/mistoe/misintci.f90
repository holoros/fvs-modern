SUBROUTINE MISINT
IMPLICIT NONE
!----------
! MISTOE $Id$
!----------
!  Purpose:
!  Mistletoe parameter initialization routine. This routine is
!  variant dependent and sets the variant dependent variables for other
!  mistletoe routines. This is the Central Idaho version.
!----------
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
!  12-JUL-2011 Lance R. David (FMSC)
!    Added arrays for height growth impacts.
!    Impact values must be supplied by MistHMod keyword.
!
!----------
!OMMONS
!
!
INCLUDE 'PRGPRM.f90'
!
!
INCLUDE 'CONTRL.f90'
!
!
INCLUDE 'MISCOM.f90'
!
!OMMONS
!----------
!  Variable declarations.
!
LOGICAL DEBUG
CHARACTER*2 ACSP(MAXSP)
INTEGER I,J,AFIT(MAXSP)
REAL ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
!----------
!     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
!
!     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
!     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
!     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
!     4 = GRAND FIR (GF)                   ABIES GRANDIS
!     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
!     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
!     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
!     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
!     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
!    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
!    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
!    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
!    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
!    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
!    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
!    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
!    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
!    18 = OTHER SOFTWOODS (OS)
!    19 = OTHER HARDWOODS (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE IE VARIANT:
!      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
!      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
!      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
!      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
!
!  FROM THE UT VARIANT:
!      USE 12(WJ) FOR 14(WJ)
!      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
!                                                  REALLY WC39=OT)
!----------
!  Data statements.
!
!  Species character representations
!----------
DATA (ACSP(I),I=1,MAXSP)/ &
    'WP','WL','DF','GF','WH','RC','LP','ES','AF','PP', &
    'WB','PY','AS','WJ','MC','LM','CW','OS','OH'/
!----------
!  Species affected by mistletoe
!----------
DATA (AFIT(I),I=1,MAXSP)/ &
       1,   1,   1,   1,   0,   0,   1,   0,   1,   1, &
       1,   0,   0,   0,   0,   1,   0,   0,   0/
!----------
!  Diameter growth rates
!----------
DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)/ &
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &
      1.0, .94, .92, .88, .84, .58, .54, &
      1.0, .98, .97, .85, .80, .52, .44, &
      1.0, 1.0, 1.0, .98, .95, .70, .50, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, .98, .95, .70, .50, &
      1.0, 1.0, 1.0, .98, .86, .73, .50, &
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !LP (for WB)
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, .94, .80, .59, &  !LP (for LM)
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/
!----------
!  Mortality coefficients
!----------
DATA ((APMC(I,J),J=1,3),I=1,MAXSP)/ &
      0.00112,  0.02170, -0.00171, &
      0.01319, -0.01627,  0.00822, &
      0.01319, -0.01627,  0.00822, &
          0.0,  0.00159,  0.00508, &
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0, &
      0.00112,  0.02170, -0.00171, &
          0.0,      0.0,      0.0, &
          0.0,  0.00159,  0.00508, &
      0.00681, -0.00580,  0.00935, &
      0.00112,  0.02170,  -0.00171, &  ! LP (for WB)
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0, &
      0.00112,  0.02170,  -0.00171, &  ! LP (for LM)
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0, &
          0.0,      0.0,      0.0/
!----------
!  Height growth potential rates
!
!  Using Douglas-fir height growth impact values described in:
!
!  Marshall, Katy 2007. Permanent plots for measuring spread and
!  impact of Douglas-fir dwarf mistletoe in the Southern Oregon
!  Cascades, Pacific Northwest Region: Results of the ten year
!  remeasurement. USDA Forest Service, Pacific Northwest Region,
!  Southwest Oregon Forest Insect and Disease Service Center,
!  Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
!
!  Default values for DF in this table would be:
!  &   1.0,1.0,1.0,.95,.65,.50,.10,
!  So that impacts are not unknowingly applied to projections,
!  the values must be supplied with the MistHMod keyword.
!  when appropriat default values are developed, they will be
!  set here.
!----------
DATA ((AHGP(I,J),J=1,7),I=1,MAXSP) &
     /1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
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
!----------
!  Check for debug.
!----------
CALL DBCHK(DEBUG,'MISINT',6,ICYC)
!
IF(DEBUG) WRITE(JOSTND,10)ICYC
10 FORMAT(' Begin/end MISINTCI: Cycle = ',I5)
!----------
!  Mistletoe model initializations.
!----------
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
!
RETURN
END
